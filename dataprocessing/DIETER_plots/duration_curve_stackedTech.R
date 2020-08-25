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
#plot load duration curve of residual loads once production is being accounted for for various tech., one by one, ordered by their capacity factor
year_toplot_list <- c(2010, 2015, 2020, 2025, 2030, 2040, 2050, 2070)

for(year_toplot in year_toplot_list){
  
# year_toplot = 2015

mycolors <- c("combined cycle gas" = "#999959", "lignite" = "#0c0c0c", "solar" = "#ffcc00", "wind" = "#337fff", "biomass" = "#005900", "open cycle gas turbine" = "#e51900", "hydro" =  "#191999", "nuclear" =  "#ff33ff", "hard coal" = "#808080")

hourly_reportCSV = read.csv(paste0(mypath, "capfac", runnumber, "_i30_hourlyreport.csv"), sep = ';', header = T, stringsAsFactors = F)
hourly_reportQUITT <- as.quitte(hourly_reportCSV) 
QUITTobj<-hourly_reportQUITT %>% 
  filter(period == year_toplot)

LDC <- QUITTobj %>% 
  filter(variable == "Net fixed electricity demand") %>% 
  select(hour, value) %>% 
  dplyr::rename(load = value)
  
LDC0 <- LDC %>% arrange(desc(load)) %>% 
  mutate(hour = str_replace(hour, hour, str_sub(hour, 2, -1))) %>% 
  mutate(hour = as.numeric(hour)) %>% 
  select(load, hour) %>% 
  mutate(load = load/1e3)
LDC0$sorted_x <- seq(1, 8760)  
LDC0$te <- "solar"

PV<- QUITTobj %>% 
  filter(variable == "Hourly generation") %>% 
  filter(tech == "Solar PV") %>% 
  select(hour, value) %>% 
  dplyr::rename(solgen = value)

CU_VRE_solar <- QUITTobj %>% 
  filter(variable == "Hourly VRE curtailment") %>% 
  filter(tech == "Solar PV")%>% 
  select(hour, value) %>% 
  dplyr::rename(curt_s = value)

RLDC_all = list(LDC, PV, CU_VRE_solar) %>% 
  reduce(full_join) 
RLDC_all[is.na(RLDC_all)] <- 0

RLDC_all$rldc1<- RLDC_all$load - PV$solgen - RLDC_all$curt_s

RLDC1 <- RLDC_all %>% arrange(desc(rldc1)) %>% 
  mutate(hour = str_replace(hour, hour, str_sub(hour, 2, -1))) %>% 
  mutate(hour = as.numeric(hour)) %>% 
  select(rldc1) %>% 
  mutate(rldc1 = rldc1/1e3)

RLDC1$sorted_x <- seq(1, 8760)  
RLDC1$te <- "wind"
#=================================================================================================
#=================================================================================================

Wind<- QUITTobj %>% 
  filter(variable == "Hourly generation") %>% 
  filter(tech == "Wind_on")%>% 
  select(hour, value) %>% 
  dplyr::rename(windgen = value)

CU_VRE_wind <- QUITTobj %>% 
  filter(variable == "Hourly VRE curtailment") %>% 
  filter(tech == "Wind_on")%>% 
  select(hour, value) %>% 
  dplyr::rename(curt_w = value)

RLDC_all = list(RLDC_all, CU_VRE_wind) %>% 
  reduce(full_join) 
RLDC_all[is.na(RLDC_all)] <- 0

RLDC_all$rldc2 <- RLDC_all$rldc1 - Wind$windgen - RLDC_all$curt_w
  
RLDC2 <- RLDC_all %>% arrange(desc(rldc2)) %>% 
  mutate(hour = str_replace(hour, hour, str_sub(hour, 2, -1))) %>% 
  mutate(hour = as.numeric(hour))%>% 
  select(rldc2)%>% 
  mutate(rldc2 = rldc2/1e3)

RLDC2$sorted_x <- seq(1, 8760)  
RLDC2$te <- "open cycle gas turbine"

#=================================================================================================
#=================================================================================================

OCGT<- QUITTobj %>% 
  filter(variable == "Hourly generation") %>% 
  filter(tech == "OCGT_eff")%>% 
  select(hour, value) %>% 
  dplyr::rename(ocgtgen = value)

RLDC_all = list(RLDC_all, OCGT) %>% 
  reduce(full_join) 
RLDC_all[is.na(RLDC_all)] <- 0
  
RLDC_all$rldc3 <- RLDC_all$rldc2 - RLDC_all$ocgtgen 

RLDC3 <- RLDC_all %>% arrange(desc(rldc3)) %>% 
  mutate(hour = str_replace(hour, hour, str_sub(hour, 2, -1))) %>% 
  mutate(hour = as.numeric(hour))%>% 
  select(rldc3)%>% 
  mutate(rldc3 = rldc3/1e3)

RLDC3$sorted_x <- seq(1, 8760)
RLDC3$te <- "combined cycle gas"

#=================================================================================================
#=================================================================================================

CCGT<- QUITTobj %>% 
  filter(variable == "Hourly generation") %>% 
  filter(tech == "CCGT")%>% 
  select(hour, value) %>% 
  dplyr::rename(ccgtgen = value)

RLDC_all$rldc4 <- RLDC_all$rldc3 - CCGT$ccgtgen 

RLDC4 <- RLDC_all %>% arrange(desc(rldc4)) %>% 
  mutate(hour = str_replace(hour, hour, str_sub(hour, 2, -1))) %>% 
  mutate(hour = as.numeric(hour))%>% 
  select(rldc4)%>% 
  mutate(rldc4 = rldc4/1e3)

RLDC4$sorted_x <- seq(1, 8760)
RLDC4$te <- "hard coal"
#=================================================================================================
#=================================================================================================

HCoal<- QUITTobj %>% 
  filter(variable == "Hourly generation") %>% 
  filter(tech == "Hard coal")%>% 
  select(hour, value) %>% 
  dplyr::rename(hcgen = value)

RLDC_all$rldc5 <- RLDC_all$rldc4 - HCoal$hcgen 

RLDC5 <- RLDC_all %>% arrange(desc(rldc5)) %>% 
  mutate(hour = str_replace(hour, hour, str_sub(hour, 2, -1))) %>% 
  mutate(hour = as.numeric(hour))%>% 
  select(rldc5)%>% 
  mutate(rldc5 = rldc5/1e3)

RLDC5$sorted_x <- seq(1, 8760)
RLDC5$te <- "lignite"
#=================================================================================================
#=================================================================================================

Lignite<- QUITTobj %>% 
  filter(variable == "Hourly generation") %>% 
  filter(tech == "Lignite")%>% 
  select(hour, value) %>% 
  dplyr::rename(liggen = value)

RLDC_all$rldc6 <- RLDC_all$rldc5 - Lignite$liggen 

RLDC6 <- RLDC_all %>% arrange(desc(rldc6)) %>% 
  mutate(hour = str_replace(hour, hour, str_sub(hour, 2, -1))) %>% 
  mutate(hour = as.numeric(hour))%>% 
  select(rldc6)%>% 
  mutate(rldc6 = rldc6/1e3)

RLDC6$sorted_x <- seq(1, 8760)
RLDC6$te <- "biomass"
#=================================================================================================
#=================================================================================================

Biomass<- QUITTobj %>% 
  filter(variable == "Hourly generation") %>% 
  filter(tech == "Biomass")%>% 
  select(hour, value) %>% 
  dplyr::rename(biogen = value)

RLDC_all$rldc7 <- RLDC_all$rldc6 - Biomass$biogen 

RLDC7 <- RLDC_all %>% arrange(desc(rldc7)) %>% 
  mutate(hour = str_replace(hour, hour, str_sub(hour, 2, -1))) %>% 
  mutate(hour = as.numeric(hour))%>% 
  select(rldc7)%>% 
  mutate(rldc7 = rldc7/1e3)

RLDC7$sorted_x <- seq(1, 8760)
RLDC7$te <- "hydro"

#=================================================================================================
#=================================================================================================

hydro<- QUITTobj %>% 
  filter(variable == "Hourly generation") %>% 
  filter(tech == "Run-of-River")%>% 
  select(hour, value) %>% 
  dplyr::rename(rorgen = value)

RLDC_all$rldc8 <- RLDC_all$rldc7 - hydro$rorgen 

RLDC8 <- RLDC_all %>% arrange(desc(rldc8)) %>% 
  mutate(hour = str_replace(hour, hour, str_sub(hour, 2, -1))) %>% 
  mutate(hour = as.numeric(hour))%>% 
  select(rldc8)%>% 
  mutate(rldc8 = rldc8/1e3)

RLDC8$sorted_x <- seq(1, 8760)
RLDC8$te <- "nuclear"

#=================================================================================================
#=================================================================================================

Nuc<- QUITTobj %>% 
  filter(variable == "Hourly generation") %>% 
  filter(tech == "Nuclear")%>% 
  select(hour, value) %>% 
  dplyr::rename(nucgen = value)

RLDC_all$rldc9 <- RLDC_all$rldc8 - Nuc$nucgen 

RLDC9 <- RLDC_all %>% arrange(desc(rldc9)) %>% 
  mutate(hour = str_replace(hour, hour, str_sub(hour, 2, -1))) %>% 
  mutate(hour = as.numeric(hour))%>% 
  select(rldc9)%>% 
  mutate(rldc9 = rldc9/1e3)

RLDC9$sorted_x <- seq(1, 8760)



# RLDC_test = list(LDC, PV, CU_VRE_solar, Wind, CU_VRE_wind, hydro, CCGT, OCGT, Biomass, Lignite, HCoal, Nuc) %>% 
#   reduce(full_join) 
# 
# RLDC_test$RLDC <- RLDC_test$LDC- RLDC_test$PV- RLDC_test$CU_VRE_solar- RLDC_test$Wind- RLDC_test$CU_VRE_wind- RLDC_test$hydro- RLDC_test$CCGT- RLDC_test$OCGT- RLDC_test$Biomass- RLDC_test$Lignite- RLDC_test$HCoal - RLDC_test$Nuc

p1<-ggplot() +
    geom_area(data = LDC0, aes(x = sorted_x, y = load, fill = te), size = 1.2, alpha = 0.8) +
    geom_area(data = RLDC1, aes(x = sorted_x, y = rldc1, fill = te), size = 1.2, alpha = 0.8) +
    geom_area(data = RLDC2, aes(x = sorted_x, y = rldc2, fill = te), size = 1.2, alpha = 0.8) +
    geom_area(data = RLDC3, aes(x = sorted_x, y = rldc3, fill = te), size = 1.2, alpha = 0.8) +
    geom_area(data = RLDC4, aes(x = sorted_x, y = rldc4, fill = te), size = 1.2, alpha = 0.8) +
    geom_area(data = RLDC5, aes(x = sorted_x, y = rldc5, fill = te), size = 1.2, alpha = 0.8) +
    geom_area(data = RLDC6, aes(x = sorted_x, y = rldc6, fill = te), size = 1.2, alpha = 0.8) +
    geom_area(data = RLDC7, aes(x = sorted_x, y = rldc7, fill = te), size = 1.2, alpha = 0.8) +
    geom_area(data = RLDC8, aes(x = sorted_x, y = rldc8, fill = te), size = 1.2, alpha = 0.8) +
    geom_area(data = RLDC9, aes(x = sorted_x, y = rldc9), size = 1.2, alpha = 0.8) +
    scale_fill_manual(name = "Technology", values = mycolors)+
    xlab("hour") + ylab("residual load (GW)")
  
  ggsave(filename = paste0(mypath, "RLCD_xNte_capfac", runnumber, "_", year_toplot, ".png"),  width = 8, height =8, units = "in", dpi = 120)
  
  }
  
  