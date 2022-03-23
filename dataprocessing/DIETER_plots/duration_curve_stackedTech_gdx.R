# plot hourly energy mix for several years from results of DIETER

# file = "full_DIETER_y10.gdx"
file = "full_DIETER.gdx"

outputdir = "output/hydro945"
dir = "/home/chengong/remind-coupling-dieter/"
mypath =paste0(dir,outputdir)

mydatapath = paste0(mypath, "/", file)
source("~/remind-coupling-dieter/dataprocessing/DIETER_plots/library_import.R")
library(readr)

dieter.tech.mapping <- c(coal = "Coal",
                         nuc = "Nuclear",
                         OCGT_eff = "OCGT",
                         CCGT = "CCGT",
                         bio = "Biomass",
                         ror = "Hydro",
                         Wind_on = "Wind Onshore",
                         Wind_off = "Wind Offshore",
                         Solar = "Solar",
                         elh2 = "Electrolyzers",
                         el = "Electricity",
                         `all Tech` = "All Tech",
                         vregrid = "VRE grid",
                         lith = "Lithium-ion Battery",
                         PSH = "Pumped Storage Hydro",
                         hydrogen = "Hydrogen Storage",
                         caes = "Compressed Air Energy Storage",
                         NULL)

dieter.variables <- c( 
  "generation (GWh)",
  "curtailment renewable (GWh)",
  "storage generation (GWh)",
  "storage loading (GWh)",
  "consumption (GWh)",
  NULL)

color.mapping <- c("CCGT" = "#999959", "Coal" = "#0c0c0c", 
                   "Solar" = "#ffcc00", "Wind" = "#337fff", "Biomass" = "#005900",
                   "OCGT" = "#e51900", "Nuclear" = "#ff33ff","Hydro" = "#191999", 
                   "Wind Onshore" = "#337fff", 
                   "Wind Offshore" = "#334cff",
                   "Electrolyzer" = "#66cccc", "Electricity" = "red", 
                   "Lithium-ion Battery" ="cyan",
                   "Pumped Storage Hydro" ="#D55E00",
                   "Hydrogen Storage" = "#56B4E9",
                   "Compressed Air Energy Storage" =  "#CC79A7",
                   NULL)

#####################################################
# specify output file
dieter.runningcost.variables = c("fuel cost - divided by eta ($/MWh)","CO2 cost ($/MWh)","O&M var cost ($/MWh)")

dieter.dispatch.tech = c("CCGT", "coal","bio", "OCGT_eff", "nuc")
dieter.dispatch.tech.whyd = c("CCGT", "coal","bio", "OCGT_eff", "nuc","ror")

#####################################################
#plot load duration curve of residual loads once production is being accounted for for various tech., one by one, ordered by their capacity factor
year_toplot_list <- c(2020,2025,2030,2035,2040,2045,2050,2055,2060,2070)

for(year_toplot in year_toplot_list){

  # year_toplot = 2070
  hr_data <- file.path(mydatapath) %>%
    read.gdx("report_tech_hours", factors = FALSE, squeeze = FALSE) %>% 
    # select(model = X., period = X..1, variable = X..3, tech = X..4, hour = X..5, value) %>%
    select(filename = X., period = X..2, variable = X..4, tech = X..5, hour = X..6, value) %>%
    revalue.levels(tech = dieter.tech.mapping) %>% 
    mutate(hour = as.numeric(str_extract(hour, "[0-9]+"))) %>% 
    mutate(tech = factor(tech, levels = rev(unique(dieter.tech.mapping)))) %>%
    filter(variable%in% dieter.variables) %>% 
    filter(period == year_toplot) %>%
    select(period, tech, variable, value,hour) %>% 
    mutate(period = as.numeric(period)) 
  
  LDC <- hr_data %>% 
    filter(variable == "consumption (GWh)") %>% 
    filter(tech == "Electricity") %>% 
    select(hour, value) %>% 
    dplyr::group_by(hour) %>%
    dplyr::summarise( value = sum(value) , .groups = 'keep' ) %>% 
    dplyr::ungroup(hour) %>% 
    dplyr::rename(load = value)%>% 
    complete(hour = 1:8760, fill = list(value = 0)) 
  
  LDC0 <- LDC %>% arrange(desc(load)) %>% 
    mutate(hour = as.numeric(hour)) %>% 
    select(load, hour) 
  
  LDC0$sorted_x <- seq(1, 8760)  
  LDC0$te <- "Solar"
  
  PV<- hr_data %>% 
    filter(variable == "generation (GWh)") %>% 
    filter(tech == "Solar") %>% 
    select(hour, value) %>%  
    mutate(value = value)%>% 
    dplyr::rename(solgen = value) %>% 
    complete(hour = 1:8760, fill = list(solgen = 0)) 
  
  CU_VRE_solar <- hr_data %>% 
    filter(variable == "curtailment renewable (GWh)") %>% 
    filter(tech == "Solar")%>% 
    select(hour, value) %>% 
    mutate(value = value)%>% 
    dplyr::rename(curt_s = value) %>% 
    complete(hour = 1:8760, fill = list(curt_s = 0)) 
  
  RLDC_all = list(LDC, PV, CU_VRE_solar) %>% 
    reduce(full_join) %>% 
    replace(is.na(.), 0)
  
  RLDC_all$rldc1<- RLDC_all$load - PV$solgen - RLDC_all$curt_s
  
  RLDC1 <- RLDC_all %>% arrange(desc(rldc1)) %>% 
    select(rldc1) 
  
  RLDC1$sorted_x <- seq(1, 8760)  
  RLDC1$te <- "Wind"
  #=================================================================================================
  #=================================================================================================
  
  Wind<- hr_data %>% 
    filter(variable == "generation (GWh)") %>% 
    filter(tech %in% c("Wind Onshore","Wind Offshore"))%>% 
    select(hour, value, tech) %>% 
    dplyr::group_by(hour) %>%
    dplyr::summarise(value = sum(value), .groups = "keep") %>%
    dplyr::ungroup(hour) %>% 
    select(hour, value) %>% 
    dplyr::rename(windgen = value) %>% 
    complete(hour = 1:8760, fill = list(windgen = 0)) 
  
  RLDC_all = list(RLDC_all,Wind) %>% 
    reduce(full_join) %>% 
    replace(is.na(.), 0)
  
  CU_VRE_wind <- hr_data %>% 
    filter(variable == "curtailment renewable (GWh)") %>% 
    filter(tech %in% c("Wind Onshore","Wind Offshore"))%>% 
    select(hour, value, tech) %>% 
    dplyr::group_by(hour) %>%
    dplyr::summarise(value = sum(value), .groups = "keep") %>%
    dplyr::ungroup(hour) %>% 
    select(hour, value) %>% 
    dplyr::rename(curt_w = value) %>% 
    complete(hour = 1:8760, fill = list(curt_w = 0)) 
  
  RLDC_all = list(RLDC_all,CU_VRE_wind) %>% 
    reduce(full_join) %>% 
    replace(is.na(.), 0)

    RLDC_all$rldc2 <- RLDC_all$rldc1 - Wind$windgen - RLDC_all$curt_w
  
  RLDC2 <- RLDC_all %>% arrange(desc(rldc2)) %>% 
    select(rldc2)
  
  RLDC2$sorted_x <- seq(1, 8760)  
  RLDC2$te <- "Lithium-ion Battery"
  
  #=================================================================================================
  
  Battery_Out <- hr_data %>% 
    filter(variable == "storage generation (GWh)") %>% 
    filter(tech %in% c("Lithium-ion Battery")) %>% 
    select(hour, value, tech) %>% 
    dplyr::group_by(hour) %>%
    dplyr::summarise(value = sum(value), .groups = "keep") %>%
    dplyr::ungroup(hour) %>% 
    select(hour, value) %>% 
    dplyr::rename(battgen = value)%>% 
    complete(hour = 1:8760, fill = list(battgen = 0)) 
  
  RLDC_checkpeak = list(LDC, PV, Wind,Battery_Out) %>%
    reduce(full_join) %>%
    mutate(residueLoad = load- solgen-windgen-battgen)
  
  Battery_In <- hr_data %>% 
    filter(variable == "storage loading (GWh)") %>% 
    filter(tech %in% c("Lithium-ion Battery")) %>% 
    select(hour, value, tech) %>% 
    dplyr::group_by(hour) %>%
    dplyr::summarise(value = sum(value), .groups = "keep") %>%
    dplyr::ungroup(hour) %>% 
    select(hour, value) %>% 
    dplyr::rename(battcharge = value)%>% 
    complete(hour = 1:8760, fill = list(battcharge = 0)) 
  
  RLDC_all = list(RLDC_all,Battery_Out,Battery_In) %>% 
    reduce(full_join) %>% 
    replace(is.na(.), 0)
  
  RLDC_all$rldc3 <- RLDC_all$rldc2 - Battery_Out$battgen + Battery_In$battcharge
  
  RLDC3 <- RLDC_all %>% arrange(desc(rldc3)) %>% 
    select(rldc3)
  
  RLDC3$sorted_x <- seq(1, 8760)
  #=================================================================================================
  #=================================================================================================
  # order dispatchables (excluding hydro) based on lowest running cost 
  
  running_cost <- file.path(mydatapath) %>%
    read.gdx("report_tech", factors = FALSE, squeeze = FALSE) %>% 
    # select(model = X., period = X..1, variable = X..3, tech = X..4,  value) %>% 
    select(filename = X., model = X..1, period = X..2, variable = X..4, tech = X..5, value) %>%
    filter(model == "DIETER") %>% 
    filter(tech %in% dieter.dispatch.tech) %>% 
    revalue.levels(tech = dieter.tech.mapping) %>% 
    mutate(tech = factor(tech, levels = rev(unique(dieter.tech.mapping)))) %>%
    filter(period == year_toplot) %>% 
    filter(variable%in% dieter.runningcost.variables) %>% 
    select(tech, variable, value) %>% 
    dplyr::group_by(tech) %>%
    dplyr::summarise(value = sum(value), .groups = "keep") %>%
    dplyr::ungroup(tech) %>% 
    arrange(desc(value))
  
  low_running_tech_list = as.vector(running_cost$tech)
  
  #=================================================================================================
  #=================================================================================================
  # order dispatchables (excluding hydro) based on lowest running cost 
  
  capfac <- file.path(mydatapath) %>%
    read.gdx("report_tech", factors = FALSE, squeeze = FALSE) %>% 
    select(filename = X., model = X..1, period = X..2, variable = X..4, tech = X..5, value) %>%
    filter(model == "REMIND") %>% 
    filter(tech %in% dieter.dispatch.tech.whyd) %>% 
    revalue.levels(tech = dieter.tech.mapping) %>% 
    mutate(tech = factor(tech, levels = rev(unique(dieter.tech.mapping)))) %>%
    filter(period == year_toplot) %>% 
    filter(variable == "REMIND real CapFac (%)") %>% 
    select(tech, variable, value) %>% 
    arrange(value)
  
  highcf_tech_list = as.vector(capfac$tech)
  
  # techranking = low_running_tech_list
  techranking = highcf_tech_list
  
  #--------------------------------------------------------------
  RLDC3$te <- techranking[[1]]
  #--------------------------------------------------------------
  
  # first dispatchable tech hourly generation
  hr_disp_gen1 <- hr_data %>% 
    filter(variable == "generation (GWh)") %>% 
    filter(tech == techranking[[1]]) %>% 
    select(hour, gen1 = value)%>% 
    complete(hour = 1:8760, fill = list(gen1 = 0)) 
  
  RLDC_all = list(RLDC_all, hr_disp_gen1) %>% 
    reduce(full_join)  %>% 
    replace(is.na(.), 0)
  
  RLDC_all$rldc4 <- RLDC_all$rldc3 - hr_disp_gen1$gen1
  
  RLDC4 <- RLDC_all %>% 
    arrange(desc(rldc4)) %>% 
    select(rldc4)
  
  RLDC4$sorted_x <- seq(1, 8760)
  RLDC4$te <- techranking[[2]]
  #=================================================================================================
  #=================================================================================================
  
  hr_disp_gen2<- hr_data %>% 
    filter(variable == "generation (GWh)") %>% 
    filter(tech == techranking[[2]])%>% 
    select(hour, value) %>% 
    dplyr::rename(gen2 = value)%>% 
    complete(hour = 1:8760, fill = list(gen2 = 0)) 
  
  RLDC_all = list(RLDC_all, hr_disp_gen2) %>% 
    reduce(full_join) %>% 
    replace(is.na(.), 0)
  
  RLDC_all$rldc5 <- RLDC_all$rldc4 - hr_disp_gen2$gen2 
  
  RLDC5 <- RLDC_all %>% 
    arrange(desc(rldc5)) %>% 
    select(rldc5)
  
  RLDC5$sorted_x <- seq(1, 8760)
  RLDC5$te <- techranking[[3]]
  #=================================================================================================
  #=================================================================================================
  
  hr_disp_gen3<- hr_data %>% 
    filter(variable == "generation (GWh)") %>% 
    filter(tech == techranking[[3]])%>% 
    select(hour, value) %>% 
    dplyr::rename(gen3 = value)%>% 
    complete(hour = 1:8760, fill = list(gen3 = 0)) 
  
  RLDC_all$rldc6 <- RLDC_all$rldc5 - hr_disp_gen3$gen3
  
  RLDC6 <- RLDC_all %>% 
    arrange(desc(rldc6)) %>% 
    select(rldc6)
  
  RLDC6$sorted_x <- seq(1, 8760)
  RLDC6$te <- techranking[[4]]
  #=================================================================================================
  #=================================================================================================
  
  hr_disp_gen4 <- hr_data %>% 
    filter(variable == "generation (GWh)") %>% 
    filter(tech == techranking[[4]])%>% 
    mutate(value = value)%>% 
    select(hour, value) %>% 
    dplyr::rename(gen4 = value)%>% 
    complete(hour = 1:8760, fill = list(gen4 = 0)) 
  
  RLDC_all$rldc7 <- RLDC_all$rldc6 - hr_disp_gen4$gen4
  
  RLDC7 <- RLDC_all %>% arrange(desc(rldc7)) %>% 
    select(rldc7)
  
  RLDC7$sorted_x <- seq(1, 8760)
  RLDC7$te <- techranking[[5]]
  
  #=================================================================================================
  #=================================================================================================
  hr_disp_gen5 <- hr_data %>% 
    filter(variable == "generation (GWh)") %>% 
    filter(tech == techranking[[5]])%>% 
    mutate(value = value)%>% 
    select(hour, value) %>% 
    dplyr::rename(gen5 = value)%>% 
    complete(hour = 1:8760, fill = list(gen5 = 0)) 
  
  RLDC_all$rldc8 <- RLDC_all$rldc7 - hr_disp_gen5$gen5
  
  RLDC8 <- RLDC_all %>% arrange(desc(rldc8)) %>% 
    select(rldc8)
  
  RLDC8$sorted_x <- seq(1, 8760)
  RLDC8$te <- "Nuclear"
  # 
  # #=================================================================================================
  
  p1<-ggplot() +
    geom_area(data = LDC0 %>% filter(sorted_x %in% c(seq(1,8760,20),8760)), aes(x = sorted_x, y = load, fill = te), size = 1.2, alpha = 1) +
    geom_area(data = RLDC1%>% filter(sorted_x %in% c(seq(1,8760,20),8760)), aes(x = sorted_x, y = rldc1, fill = te), size = 1.2, alpha = 1) +
    geom_area(data = RLDC2%>% filter(sorted_x %in% c(seq(1,8760,20),8760)), aes(x = sorted_x, y = rldc2, fill = te), size = 1.2, alpha = 1) +
    geom_area(data = RLDC3%>% filter(sorted_x %in% c(seq(1,8760,20),8760)), aes(x = sorted_x, y = rldc3, fill = te), size = 1.2, alpha = 1) +
    geom_area(data = RLDC4%>% filter(sorted_x %in% c(seq(1,8760,20),8760)), aes(x = sorted_x, y = rldc4, fill = te), size = 1.2, alpha = 1) +
    geom_area(data = RLDC5%>% filter(sorted_x %in% c(seq(1,8760,20),8760)), aes(x = sorted_x, y = rldc5, fill = te), size = 1.2, alpha = 1) +
    geom_area(data = RLDC6%>% filter(sorted_x %in% c(seq(1,8760,20),8760)), aes(x = sorted_x, y = rldc6, fill = te), size = 1.2, alpha = 1) +
    geom_area(data = RLDC7%>% filter(sorted_x %in% c(seq(1,8760,20),8760)), aes(x = sorted_x, y = rldc7, fill = te), size = 1.2, alpha = 1) +
    geom_area(data = RLDC8%>% filter(sorted_x %in% c(seq(1,8760,20),8760)), aes(x = sorted_x, y = rldc8, fill = te), size = 1.2, alpha = 1) +
    coord_cartesian(ylim = c(-120,170))+
    scale_fill_manual(name = "Technology", values = color.mapping)+
    xlab("hour") + ylab("residual load (GW)")+
    ggtitle(paste0("DIETER ", year_toplot))
  
  ggsave(filename = paste0(mypath, "/DIETER/DIETER_RLDC_yr=", year_toplot, ".png"),  width = 8, height =8, units = "in", dpi = 120)
  
}
