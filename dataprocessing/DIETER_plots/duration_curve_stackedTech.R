mypath = "~/remind-coupling-dieter/dataprocessing/DIETER_plots/"
runnumber = "hydro601"

mydatapath = paste0("~/remind-coupling-dieter/output/", runnumber, "/")

filenames <- list.files(mydatapath, pattern="fulldata_[0-9]+\\.gdx")
maxiter = length(filenames)-1
# import library

source(paste0(mypath, "library_import.R"))
source(paste0(mypath, "GDXtoQuitte.R"))
library(readr)

igdx("/opt/gams/gams30.2_linux_x64_64_sfx")
# specify output file
file1 = paste0("report_DIETER_i",maxiter,".gdx")

# gdxToQuitte_annual(mydatapath, file1,runnumber)
# gdxToQuitte_hourly(mydatapath, file1,runnumber)

annual_reportCSV = read.csv(paste0(myDIETERPLOT_path, runnumber, "_i", maxiter, "_annualreport.csv"), sep = ';', header = T, stringsAsFactors = F)
VAR_report_key_DT = c("fuel cost - divided by eta ($/MWh)","CO2 cost ($/MWh)")

# TECH_DISPATCH_DT = c("CCGT", "lig","bio", "OCGT_eff", "nuc", "hc")
TECH_DISPATCH_DT = c("CCGT", "lig","bio", "OCGT_eff", "nuc")
dieter.tech.mapping <- c(hc = "Hard coal",
                         lig = "Coal",
                         coal = "Coal (Lig + HC)",
                         nuc = "Nuclear",
                         OCGT_eff = "OCGT",
                         CCGT = "CCGT",
                         bio = "Biomass",
                         ror = "Hydro",
                         Wind_on = "Wind",
                         Solar = "Solar",
                         NULL)

color.mapping <- c("CCGT" = "#999959", "Coal" = "#0c0c0c", 
                   # "Coal (Lig + HC)" = "#0c0c0c",
                   "Solar" = "#ffcc00", "Wind" = "#337fff", "Biomass" = "#005900",
                   "OCGT" = "#e51900", "Hydro" = "#191999", "Nuclear" = "#ff33ff"
                   # ,
                   # "Hard coal" = "#808080"
                   # , "peak demand" = "#0c0c0c"
                   )

#####################################################
#plot load duration curve of residual loads once production is being accounted for for various tech., one by one, ordered by their capacity factor
year_toplot_list <- c(2020,2025,2030,2035,2040,2045,2050,2055,2060,2070)
# year_toplot_list <- c(2030,2050,2070)
# year_toplot_list <- c(2055)

mycolors <- c("combined cycle gas" = "#999959", "lignite" = "#0c0c0c", "solar" = "#ffcc00", "wind" = "#337fff", "biomass" = "#005900", "open cycle gas turbine" = "#e51900", "hydro" =  "#191999", "nuclear" =  "#ff33ff", "hard coal" = "#808080")

hourly_reportCSV = read.csv(paste0(mypath, "/", runnumber, "_i",maxiter,"_hourlyreport.csv"), sep = ';', header = T, stringsAsFactors = F)

hourly_reportQUITT <- as.quitte(hourly_reportCSV)

for(year_toplot in year_toplot_list){
# 
# year_toplot = 2030

QUITTobj<-hourly_reportQUITT %>% 
  filter(period == year_toplot) %>% 
  revalue.levels(tech = dieter.tech.mapping) %>%
  mutate( tech = factor(tech, levels=rev(unique(dieter.tech.mapping))) )

LDC <- QUITTobj %>% 
  filter(variable == "consumption (GWh)") %>% 
  select(hour, value) %>% 
  dplyr::group_by(hour) %>%
  dplyr::summarise( value = sum(value) , .groups = 'keep' ) %>% 
  dplyr::ungroup(hour) %>% 
  dplyr::rename(load = value)
  
LDC0 <- LDC %>% arrange(desc(load)) %>% 
  mutate(hour = as.numeric(hour)) %>% 
  select(load, hour) 
LDC0$sorted_x <- seq(1, 8760)  
LDC0$te <- "Solar"

PV<- QUITTobj %>% 
  filter(variable == "generation (GWh)") %>% 
  filter(tech == "Solar") %>% 
  select(hour, value) %>%  
  mutate(value = value)%>% 
  dplyr::rename(solgen = value)

CU_VRE_solar <- QUITTobj %>% 
  filter(variable == "curtailment renewable (GWh)") %>% 
  filter(tech == "Solar")%>% 
  select(hour, value) %>% 
  mutate(value = value)%>% 
  dplyr::rename(curt_s = value)

RLDC_all = list(LDC, PV, CU_VRE_solar) %>% 
  reduce(full_join) 
RLDC_all[is.na(RLDC_all)] <- 0

RLDC_all$rldc1<- RLDC_all$load - PV$solgen - RLDC_all$curt_s

RLDC1 <- RLDC_all %>% arrange(desc(rldc1)) %>% 
  select(rldc1) 

RLDC1$sorted_x <- seq(1, 8760)  
RLDC1$te <- "Wind"
#=================================================================================================
#=================================================================================================

Wind<- QUITTobj %>% 
  filter(variable == "generation (GWh)") %>% 
  filter(tech == "Wind")%>% 
  select(hour, value) %>% 
  mutate(value = value)%>% 
  dplyr::rename(windgen = value)

# RLDC_checkpeak = list(LDC, PV, Wind) %>% 
#   reduce(full_join) %>% 
#   mutate(residueLoad = load- solgen-windgen)

CU_VRE_wind <- QUITTobj %>% 
  filter(variable == "curtailment renewable (GWh)") %>% 
  filter(tech == "Wind")%>% 
  mutate(value = value)%>% 
  select(hour, value) %>% 
  dplyr::rename(curt_w = value)

RLDC_all = list(RLDC_all, CU_VRE_wind) %>% 
  reduce(full_join) 
RLDC_all[is.na(RLDC_all)] <- 0

RLDC_all$rldc2 <- RLDC_all$rldc1 - Wind$windgen - RLDC_all$curt_w
  
RLDC2 <- RLDC_all %>% arrange(desc(rldc2)) %>% 
  select(rldc2)

RLDC2$sorted_x <- seq(1, 8760)  

#=================================================================================================
#=================================================================================================
#order based on lowest running cost (repeated script from price_duration_curve_w_varCost)
annual_reportQUITT <- as.quitte(annual_reportCSV) 

running_cost <- annual_reportQUITT %>% 
  filter(period %in% year_toplot) %>% 
  filter(tech %in% TECH_DISPATCH_DT) %>% 
  filter(variable %in% VAR_report_key_DT) %>% 
  select(tech, value) %>% 
  revalue.levels(tech = dieter.tech.mapping) %>%
  mutate(tech = factor(tech, levels=rev(unique(dieter.tech.mapping)))) %>% 
  dplyr::group_by(tech) %>%
  dplyr::summarise( value = sum(value), .groups = "keep" ) %>% 
  dplyr::ungroup(tech) %>% 
  arrange(desc(value))

low_running_tech_list = as.vector(running_cost$tech)

RLDC2$te <- low_running_tech_list[[1]]

#--------------------------------------------------------------

hr_gen3 <- QUITTobj %>% 
  filter(variable == "generation (GWh)") %>% 
  filter(tech == low_running_tech_list[[1]]) %>% 
  select(hour, gen3 = value)

RLDC_all = list(RLDC_all, hr_gen3) %>% 
  reduce(full_join) 

RLDC_all[is.na(RLDC_all)] <- 0

RLDC_all$rldc3 <- RLDC_all$rldc2 - RLDC_all$gen3

RLDC3 <- RLDC_all %>% 
  arrange(desc(rldc3)) %>% 
  select(rldc3)

RLDC3$sorted_x <- seq(1, 8760)
RLDC3$te <- low_running_tech_list[[2]]
#=================================================================================================
#=================================================================================================

hr_gen4<- QUITTobj %>% 
  filter(variable == "generation (GWh)") %>% 
  filter(tech == low_running_tech_list[[2]])%>% 
  select(hour, value) %>% 
  dplyr::rename(gen4 = value)

RLDC_all$rldc4 <- RLDC_all$rldc3 - hr_gen4$gen4 

RLDC4 <- RLDC_all %>% arrange(desc(rldc4)) %>% 
  select(rldc4)

RLDC4$sorted_x <- seq(1, 8760)
RLDC4$te <- low_running_tech_list[[3]]
#=================================================================================================
#=================================================================================================

hr_gen5<- QUITTobj %>% 
  filter(variable == "generation (GWh)") %>% 
  filter(tech == low_running_tech_list[[3]])%>% 
  select(hour, value) %>% 
  dplyr::rename(gen5 = value)

RLDC_all$rldc5 <- RLDC_all$rldc4 - hr_gen5$gen5

RLDC5 <- RLDC_all %>% arrange(desc(rldc5)) %>% 
  select(rldc5)

RLDC5$sorted_x <- seq(1, 8760)
RLDC5$te <- low_running_tech_list[[4]]
#=================================================================================================
#=================================================================================================

hr_gen6 <- QUITTobj %>% 
  filter(variable == "generation (GWh)") %>% 
  filter(tech == low_running_tech_list[[4]])%>% 
  mutate(value = value)%>% 
  select(hour, value) %>% 
  dplyr::rename(gen6 = value)

RLDC_all$rldc6 <- RLDC_all$rldc5 - hr_gen6$gen6

RLDC6 <- RLDC_all %>% arrange(desc(rldc6)) %>% 
  select(rldc6)

RLDC6$sorted_x <- seq(1, 8760)
RLDC6$te <- low_running_tech_list[[5]]
#=================================================================================================
#=================================================================================================
# 
# hr_gen7<- QUITTobj %>% 
#   filter(variable == "generation (GWh)") %>% 
#   filter(tech == low_running_tech_list[[5]])%>% 
#   select(hour, value) %>% 
#   dplyr::rename(gen7 = value)
# 
# RLDC_all$rldc7 <- RLDC_all$rldc6 - hr_gen7$gen7
# 
# RLDC7 <- RLDC_all %>% arrange(desc(rldc7)) %>% 
#   select(rldc7)
# 
# RLDC7$sorted_x <- seq(1, 8760)
# RLDC7$te <- low_running_tech_list[[6]]

#=================================================================================================
#=================================================================================================

hr_gen7<- QUITTobj %>% 
  filter(variable == "generation (GWh)") %>% 
  filter(tech == low_running_tech_list[[5]])%>% 
  select(hour, value) %>% 
  dplyr::rename(gen7 = value)

RLDC_all$rldc7 <- RLDC_all$rldc6 - hr_gen7$gen7

RLDC7 <- RLDC_all %>% arrange(desc(rldc7)) %>% 
  select(rldc7)

RLDC7$sorted_x <- seq(1, 8760)
RLDC7$te <- "Hydro"

#=================================================================================================
#=================================================================================================

# ROR<- QUITTobj %>% 
#   filter(variable == "generation (GWh)") %>% 
#   filter(tech == "Hydro")%>% 
#   select(hour, value) %>% 
#   dplyr::rename(rorgen = value)
# 
# RLDC_all$rldc8 <- RLDC_all$rldc7 - ROR$rorgen 
# 
# RLDC8 <- RLDC_all %>% arrange(desc(rldc8)) %>% 
#   select(rldc8)
# 
# RLDC8$sorted_x <- seq(1, 8760)

p1<-ggplot() +
    geom_area(data = LDC0, aes(x = sorted_x, y = load, fill = te), size = 1.2, alpha = 1) +
    geom_area(data = RLDC1, aes(x = sorted_x, y = rldc1, fill = te), size = 1.2, alpha = 1) +
    geom_area(data = RLDC2, aes(x = sorted_x, y = rldc2, fill = te), size = 1.2, alpha = 1) +
    geom_area(data = RLDC3, aes(x = sorted_x, y = rldc3, fill = te), size = 1.2, alpha = 1) +
    geom_area(data = RLDC4, aes(x = sorted_x, y = rldc4, fill = te), size = 1.2, alpha = 1) +
    geom_area(data = RLDC5, aes(x = sorted_x, y = rldc5, fill = te), size = 1.2, alpha = 1) +
    geom_area(data = RLDC6, aes(x = sorted_x, y = rldc6, fill = te), size = 1.2, alpha = 1) +
    geom_area(data = RLDC7, aes(x = sorted_x, y = rldc7, fill = te), size = 1.2, alpha = 1) +
    coord_cartesian(ylim = c(-120,270))+
    # geom_area(data = RLDC8, aes(x = sorted_x, y = rldc8, fill = te), size = 1.2, alpha = 1) +
    # geom_area(data = RLDC9, aes(x = sorted_x, y = rldc9), size = 1.2, alpha = 0.8) +
    scale_fill_manual(name = "Technology", values = color.mapping)+
    xlab("hour") + ylab("residual load (GW)")+
    ggtitle(paste0("DIETER ", year_toplot))
  
  ggsave(filename = paste0(mypath, "RLDC_xNte_", runnumber, "_iter=", iteration, "_yr=", year_toplot, ".png"),  width = 8, height =8, units = "in", dpi = 120)
  
  }
  
  