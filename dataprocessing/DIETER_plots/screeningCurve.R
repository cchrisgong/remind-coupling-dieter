mypath = "~/remind/dataprocessing/DIETER_plots/"
runnumber = 18
mydatapath = paste0("~/remind/output/capfac", runnumber, "/")


# import library

source(paste0(mypath, "library_import.R"))
source(paste0(mypath, "GDXtoQuitte.R"))
library(readr)

igdx("/opt/gams/gams30.2_linux_x64_64_sfx")

# specify output file
file1 = "report_DIETER_i30.gdx"

# gdxToQuitte_annual(mydatapath, file1)
# gdxToQuitte_hourly(mydatapath, file1)

#####################################################
#plot screening curves

mycolors <- c("CCGT" = "#999959", "Lignite" = "#0c0c0c", "Biomass" = "#005900", "OCGT_eff" = "#e51900", "Nuclear" =  "#ff33ff", "Hard coal" = "#808080")

annual_reportCSV = read.csv(paste0(mypath, "capfac", runnumber, "_i30_annualreport.csv"), sep = ';', header = T, stringsAsFactors = F)
annual_reportQUITT <- as.quitte(annual_reportCSV) 

hourly_reportCSV = read.csv(paste0(mypath, "capfac", runnumber, "_i30_hourlyreport.csv"), sep = ';', header = T, stringsAsFactors = F)
hourly_reportQUITT <- as.quitte(hourly_reportCSV) 

te_toplot = c("Lignite", "OCGT_eff", "CCGT", "Hard coal", "Nuclear", "Biomass")
IC <- annual_reportQUITT %>% 
  filter(variable == "Annualized investment cost") %>% 
  filter(tech %in% te_toplot) %>% 
  select(period, tech, value) %>% 
  dplyr::rename(IC = value)

OM <- annual_reportQUITT %>% 
  filter(variable == "O&M cost") %>% 
  filter(tech %in% te_toplot) %>% 
  select(period, tech, value) %>% 
  dplyr::rename(OM = value)

FC <- annual_reportQUITT %>% 
  filter(variable == "primary energy price") %>% 
  filter(tech %in% te_toplot) %>% 
  select(period, tech, value) %>% 
  dplyr::rename(FC = value)

eta <- annual_reportQUITT %>% 
  filter(variable == "fuel efficiency") %>% 
  filter(tech %in% te_toplot) %>% 
  select(period, tech, value) %>% 
  mutate(value = value/1e2) %>%  
  dplyr::rename(eta = value)

FLH <- hourly_reportQUITT %>% 
  filter(variable == "Hourly generation") %>% 
  filter(tech %in% te_toplot) %>% 
  select(period, hour, tech, value) %>% 
  dplyr::rename(gen = value) %>% 
  dplyr::group_by(period, tech) %>% 
  dplyr::summarise(FLH= sum(gen != 0)) %>% 
  dplyr::ungroup(period, tech)

fixedCosts = list(IC, OM, FC) %>% 
  reduce(full_join)

FLHseq0 <- hourly_reportQUITT %>% 
  filter(variable == "Hourly generation") %>% 
  filter(tech %in% te_toplot) %>% 
  select(period, tech, value) %>% 
  mutate(flh = 1*(value !=0)) %>% 
  # filter(period == 2025) %>%
  dplyr::group_by(period, tech) %>% 
  dplyr::arrange(period, tech, desc(flh) )%>% 
  dplyr::ungroup(period, tech)
  
FLHseq0$sorted_x <- seq(1, 8760)  

FLHseq <- FLHseq0 %>% 
  filter(flh == 1)  %>% 
  select(period, tech, sorted_x, value)

ScreenCurve = list(FLHseq, IC, OM, FC, eta) %>% 
  reduce(full_join) %>% 
  mutate(value = sorted_x * FC / 1e3 / eta + IC + OM)
  
p1 <- ggplot() +
    geom_line(data = ScreenCurve, aes(x = sorted_x, y = value, color = tech), size = 1.2, alpha = 0.8) +
    scale_color_manual(name = "Technology", values = mycolors) +
    xlab("full load hours") + ylab("Annualized full cost ($/kW)") +
    theme(axis.text=element_text(size = 10), axis.title = element_text(size = 10,face = "bold")) +
    facet_wrap(~period, nrow = 3)
  
  ggsave(filename = paste0(mypath, "screening_curve_capfac", runnumber, ".png"),  width = 12, height =8, units = "in", dpi = 120)
  
p2<-ggplot() +
  geom_line(data = ScreenCurve, aes(x = sorted_x, y = value/sorted_x * 1e3, color = tech), size = 1.2, alpha = 0.8) +
    scale_color_manual(name = "Technology", values = mycolors)+
    xlab("full load hours") + ylab("Annualized full cost ($/MWh)")+
    theme(axis.text=element_text(size = 10), axis.title=element_text(size = 10,face = "bold")) +
     coord_cartesian(ylim = c(0,200)) +
    facet_wrap(~period, nrow = 3)
  
  ggsave(filename = paste0(mypath, "inversescreening_curve_capfac", runnumber, ".png"),  width = 12, height =8, units = "in", dpi = 120)
  
  
  