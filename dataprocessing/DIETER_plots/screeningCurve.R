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
iteration =maxiter
file1 = paste0("report_DIETER_i", iteration, ".gdx")

# gdxToQuitte_annual(mydatapath, file1,runnumber)
# gdxToQuitte_hourly(mydatapath, file1,runnumber)

#####################################################
#plot screening curves

mycolors <- c("CCGT" = "#999959", "lig" = "#0c0c0c", "bio" = "#005900", "OCGT_eff" = "#e51900", "nuc" =  "#ff33ff", "hc" = "#808080")

annual_reportCSV = read.csv(paste0(mypath, "/", runnumber, "_i", iteration, "_annualreport.csv"), sep = ';', header = T, stringsAsFactors = F)
annual_reportQUITT <- as.quitte(annual_reportCSV) %>% 
  filter(model=="DIETER")

hourly_reportCSV = read.csv(paste0(mypath, "/", runnumber, "_i", iteration, "_hourlyreport.csv"), sep = ';', header = T, stringsAsFactors = F)
hourly_reportQUITT <- as.quitte(hourly_reportCSV) %>% 
  filter(model=="DIETER")

te_toplot = c("lig", "OCGT_eff", "CCGT", "nuc", "bio")
IC <- annual_reportQUITT %>% 
  filter(variable == "annualized investment cost ($/kW)") %>% 
  filter(tech %in% te_toplot) %>% 
  select(period, tech, value) %>% 
  dplyr::rename(IC = value)

OM <- annual_reportQUITT %>% 
  filter(variable == "O&M cost ($/kW)") %>% 
  filter(tech %in% te_toplot) %>% 
  select(period, tech, value) %>% 
  dplyr::rename(OM = value)

CO2 <- annual_reportQUITT %>% 
  filter(variable == "CO2 cost ($/MWh)") %>% 
  filter(tech %in% te_toplot) %>% 
  select(period, tech, value) %>% 
  dplyr::rename(CO2 = value)

FC <- annual_reportQUITT %>% 
  filter(variable == "fuel cost - divided by eta ($/MWh)") %>% 
  filter(tech %in% te_toplot) %>% 
  select(period, tech, value) %>% 
  dplyr::rename(FC = value)

fixedCosts <- list(IC, OM, FC, CO2) %>% 
  reduce(full_join) %>% 
  replace(is.na(.), 0) 

FLHseq0 <- hourly_reportQUITT %>% 
  filter(variable == "generation (GWh)") %>% 
  filter(tech %in% te_toplot) %>% 
  select(period, tech, value) %>% 
  mutate(flh = 1*(value !=0)) %>% 
  # filter(period == 2080) %>%
  # filter(tech =="bio") %>%
  dplyr::group_by(period, tech) %>% 
  dplyr::arrange(period, tech, desc(flh) )%>% 
  dplyr::ungroup(period, tech)
  
FLHseq0$sorted_x <- rep(seq(1, 8760),length(FLHseq0$value)/8760)

FLHseq <- FLHseq0 %>% 
  filter(flh == 1)  %>% 
  select(period, tech, sorted_x, value)

ScreenCurve = list(FLHseq, fixedCosts) %>% 
  reduce(full_join) %>% 
  mutate(value = sorted_x * (FC + CO2)/1e3 + (IC + OM) )
# %>% 
  # filter(period == 2080) %>%
  # filter(tech =="bio")
  
p1 <- ggplot() +
    geom_line(data = ScreenCurve, aes(x = sorted_x, y = value, color = tech), size = 1.2, alpha = 0.8) +
    scale_color_manual(name = "Technology", values = mycolors) +
    xlab("full load hours") + ylab("Annualized full cost ($/kW)") +
    theme(axis.text=element_text(size = 10), axis.title = element_text(size = 10,face = "bold")) +
    facet_wrap(~period, nrow = 3)
  
  ggsave(filename = paste0(mypath, "screening_curve_", runnumber, "_", iteration, ".png"),  width = 12, height =8, units = "in", dpi = 120)
  
p2<-ggplot() +
  geom_line(data = ScreenCurve, aes(x = sorted_x, y = value/sorted_x * 1e3, color = tech), size = 1.2, alpha = 0.8) +
    scale_color_manual(name = "Technology", values = mycolors)+
    xlab("full load hours") + ylab("Annualized full cost ($/kW)")+
    theme(axis.text=element_text(size = 10), axis.title=element_text(size = 10,face = "bold")) +
     coord_cartesian(ylim = c(0,400)) +
    facet_wrap(~period, nrow = 3)
  
  ggsave(filename = paste0(mypath, "inversescreening_curve_", runnumber, "_", iteration, ".png"),  width = 12, height =8, units = "in", dpi = 120)
  
  FLHseq2 <- FLHseq0 %>% 
    # filter(flh == 1)  %>% 
    select(period, tech, sorted_x, value)
  
  ScreenCurve2 = list(FLHseq2, fixedCosts) %>% 
    reduce(full_join) %>% 
    replace(is.na(.), 0) %>% 
    mutate(value = sorted_x * (FC + CO2)/1e3 + (IC + OM))
  # %>% 
  # filter(period == 2090) %>%
  # filter(tech =="bio")

  p3<-ggplot() +
    geom_line(data = ScreenCurve2, aes(x = sorted_x, y = value/sorted_x * 1e3, color = tech), size = 1.2, alpha = 0.8) +
    scale_color_manual(name = "Technology", values = mycolors)+
    xlab("hours") + ylab("Annualized full cost ($/kW)")+
    theme(axis.text=element_text(size = 10), axis.title=element_text(size = 10,face = "bold")) +
    coord_cartesian(ylim = c(0,200)) +
    facet_wrap(~period, nrow = 3)
  ggsave(filename = paste0(mypath, "inversescreening_curve_fullyr_", runnumber, "_", iteration, ".png"),  width = 12, height =8, units = "in", dpi = 120)
  
  
  ScreenCurve_min_envelop <- ScreenCurve2 %>% 
    select(period, tech, sorted_x, value) %>% 
    dplyr::group_by(period,sorted_x) %>%  
    dplyr::summarize(minvalue= min(value), minTech = tech[which.min(value)], .groups = "keep" )%>% 
    dplyr::ungroup(period,sorted_x) 
  # %>% 
    # filter(period == "2025") 
  