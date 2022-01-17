
mypath = "~/DIETER/myFirstDIETER/dataprocessing/"
mydatapath = "~/DIETER/myFirstDIETER/DIETER/"

# import library

source(paste0(mypath, "library_import.R"))
source(paste0(mypath, "GDXtoQuitte.R"))
library(readr)
# specify output file

#===========================================

# hourly_reportCSV = read.csv(paste0(mypath, "results70_hourlyreport.csv"), sep = ';', header = T, stringsAsFactors = F)
hourly_reportCSV = read.csv(paste0(mypath, "results70_flex_hourlyreport.csv"), sep = ';', header = T, stringsAsFactors = F)
hourly_reportQUITT <- as.quitte(hourly_reportCSV) 
QUITTobj2 = hourly_reportQUITT 

load_2 <- QUITTobj2 %>% 
  filter(variable == "Net electricity demand") %>% 
select(hour, value) %>% 
  dplyr::rename(load = value)

CU_VRE_2_wind <- QUITTobj2 %>% 
  filter(variable == "Hourly VRE curtailment") %>% 
  filter(tech == "Wind_on")%>% 
  select(hour, value) %>% 
  dplyr::rename(curt_w = value)

CU_VRE_2_solar <- QUITTobj2 %>% 
  filter(variable == "Hourly VRE curtailment") %>% 
  filter(tech == "Solar PV")%>% 
  select(hour, value) %>% 
  dplyr::rename(curt_s = value)

PV_2<- QUITTobj2 %>% 
  filter(variable == "Hourly renewable generation") %>% 
  filter(tech == "Solar PV") %>% 
  select(hour, value) %>% 
  dplyr::rename(solgen = value)
  
  Wind_2<- QUITTobj2 %>% 
  filter(variable == "Hourly renewable generation") %>% 
  filter(tech == "Wind_on")%>% 
  select(hour, value) %>% 
  dplyr::rename(windgen = value)
  
  LDC =list(load_2, PV_2, Wind_2,CU_VRE_2_wind, CU_VRE_2_solar) %>% 
    reduce(full_join) 
  LDC[is.na(LDC)] <- 0
  
  LDC$ldc <-  LDC$load -  LDC$solgen -  LDC$windgen - LDC$curt_w - LDC$curt_s
  
  LDC2 <- LDC %>% arrange(desc(ldc)) %>% 
    mutate(hour = str_replace(hour, hour, str_sub(hour, 2, -1))) %>% 
    mutate(hour = as.numeric(hour))
  LDC2$sorted_x <- seq(1, 8760)  

#===========================================
# hourly_reportCSV = read.csv(paste0(mypath, "results50_hourlyreport.csv"), sep = ';', header = T, stringsAsFactors = F)
hourly_reportCSV = read.csv(paste0(mypath, "results50_flex_hourlyreport.csv"), sep = ';', header = T, stringsAsFactors = F)
hourly_reportQUITT <- as.quitte(hourly_reportCSV) 
QUITTobj3 = hourly_reportQUITT

load_3 <- QUITTobj3 %>% 
  filter(variable == "Net electricity demand") %>% 
  select(hour, value) %>% 
  dplyr::rename(load = value)

CU_VRE_3_wind <- QUITTobj3 %>% 
  filter(variable == "Hourly VRE curtailment") %>% 
  filter(tech == "Wind_on")%>% 
  select(hour, value) %>% 
  dplyr::rename(curt_w = value)

CU_VRE_3_solar <- QUITTobj3 %>% 
  filter(variable == "Hourly VRE curtailment") %>% 
  filter(tech == "Solar PV")%>% 
  select(hour, value) %>% 
  dplyr::rename(curt_s = value) 

PV_3<- QUITTobj3 %>% 
  filter(variable == "Hourly renewable generation") %>% 
  filter(tech == "Solar PV") %>% 
  select(hour, value) %>% 
  dplyr::rename(solgen = value)

Wind_3<- QUITTobj3 %>% 
  filter(variable == "Hourly renewable generation") %>% 
  filter(tech == "Wind_on")%>% 
  select(hour, value) %>% 
  dplyr::rename(windgen = value)

LDC =list(load_3, PV_3, Wind_3,CU_VRE_3_wind, CU_VRE_3_solar) %>% 
  reduce(full_join) 

LDC[is.na(LDC)] <- 0

LDC$ldc <-  LDC$load -  LDC$solgen -  LDC$windgen- LDC$curt_w - LDC$curt_s

LDC3 <- LDC %>% arrange(desc(ldc)) %>% 
  mutate(hour = str_replace(hour, hour, str_sub(hour, 2, -1))) %>% 
  mutate(hour = as.numeric(hour))

LDC3$sorted_x <- seq(1, 8760)

#===========================================
# hourly_reportCSV = read.csv(paste0(mypath, "results25_hourlyreport.csv"), sep = ';', header = T, stringsAsFactors = F)
hourly_reportCSV = read.csv(paste0(mypath, "results25_flex_hourlyreport.csv"), sep = ';', header = T, stringsAsFactors = F)
hourly_reportQUITT <- as.quitte(hourly_reportCSV) 
QUITTobj4 = hourly_reportQUITT

load_4 <- QUITTobj4 %>% 
  filter(variable == "Net electricity demand") %>% 
  select(hour, value) %>% 
  dplyr::rename(load = value)

CU_VRE_4 <- QUITTobj4 %>% 
  filter(variable == "Hourly VRE curtailment") %>% 
  select(hour, value) %>% 
  dplyr::rename(curt = value)

PV_4<- QUITTobj4 %>% 
  filter(variable == "Hourly renewable generation") %>% 
  filter(tech == "Solar PV") %>% 
  select(hour, value) %>% 
  dplyr::rename(solgen = value)

Wind_4<- QUITTobj4 %>% 
  filter(variable == "Hourly renewable generation") %>% 
  filter(tech == "Wind_on")%>% 
  select(hour, value) %>% 
  dplyr::rename(windgen = value)

LDC =list(load_4, PV_4, Wind_4,CU_VRE_4) %>% 
  reduce(full_join) 

LDC[is.na(LDC)] <- 0

LDC$ldc <-  LDC$load -  LDC$solgen -  LDC$windgen - LDC$curt

LDC4 <- LDC %>% arrange(desc(ldc)) %>% 
  mutate(hour = str_replace(hour, hour, str_sub(hour, 2, -1))) %>% 
  mutate(hour = as.numeric(hour))

LDC4$sorted_x <- seq(1, 8760)
#======================================================

  rainbow.pal <- rev(rainbow(100, start = 0.63, end = 0.62))
  
  ylim <- c(-600000,500000)

  LDC4$share <- "VRE25%"
  LDC3$share <- "VRE50%"
  LDC2$share <- "VRE70%"
  
    p <- ggplot() +
      geom_line(data = LDC4, aes(x = sorted_x, y = ldc, color = share), size = 3.2, alpha = 0.8) +
      geom_col(data = LDC4, aes(x = sorted_x, y = ldc, fill = hour), position = "identity", alpha = 0.8, width = 0.3) +
      scale_fill_gradientn(colours  = rainbow.pal,
                           limits = c(1, 8760),
                           breaks = (c(1,7,13,19,23)+.3) * 8760 / 24,
                           labels = c("Jan", "Apr", "Jul", "Oct", "Dec"),
                           name = "") +
      theme(plot.title =element_text(size=42), axis.text=element_text(size=40), axis.title=element_text(size= 40,face="bold"), 
            legend.text = element_text(size = 35),
            legend.key.size = unit(1.5, "cm"),legend.title = element_blank(),legend.position = c(.85, .85)) +
      xlab("sorted hour") + ylab("Residual load (MWh)")+ggtitle("Residual Load Duration Curves for various shares of VRE")+
      coord_cartesian(ylim = ylim,  xlim = c(0, 8760))
      
  ggsave(filename = paste0(mypath, "LDC_season_wstor1.png"),  width = 20, height =20, units = "in", dpi = 120)

  p <- ggplot() +
    geom_line(data = LDC4, aes(x = sorted_x, y = ldc, color = share), size = 3.2, alpha = 0.2) +
    geom_col(data = LDC4, aes(x = sorted_x, y = ldc, fill = hour), position = "identity", alpha = 0.2, width = 0.3) +
    geom_line(data = LDC3, aes(x = sorted_x, y = ldc, color = share), size = 3.2, alpha = 0.8) +
    geom_col(data = LDC3, aes(x = sorted_x, y = ldc, fill = hour), position = "identity", alpha = 0.8, width = 0.3) +
    scale_fill_gradientn(colours  = rainbow.pal,
                         limits = c(1, 8760),
                         breaks = (c(1,7,13,19,23)+.3) * 8760 / 24,
                         labels = c("Jan", "Apr", "Jul", "Oct", "Dec"),
                         name = "") +
    theme(plot.title =element_text(size=42), axis.text=element_text(size=40), axis.title=element_text(size= 40,face="bold"), 
          legend.text = element_text(size = 35),
          legend.key.size = unit(1.5, "cm"),legend.title = element_blank(),legend.position = c(.85, .85)) +
    xlab("sorted hour") + ylab("Residual load (MWh)")+ggtitle("Residual Load Duration Curves for various shares of VRE")+
    coord_cartesian( ylim = ylim, xlim = c(0, 8760))
  
  ggsave(filename = paste0(mypath, "LDC_season_wstor2.png"),  width = 20, height =20, units = "in", dpi = 120)
  
  p <- ggplot() +
    geom_line(data = LDC4, aes(x = sorted_x, y = ldc, color = share), size = 3.2, alpha = 0.2) +
    geom_col(data = LDC4, aes(x = sorted_x, y = ldc, fill = hour), position = "identity", alpha = 0.2, width = 0.3) +
    geom_line(data = LDC3, aes(x = sorted_x, y = ldc, color = share), size = 3.2, alpha = 0.2) +
    geom_col(data = LDC3, aes(x = sorted_x, y = ldc, fill = hour), position = "identity", alpha = 0.2, width = 0.3) +
    geom_line(data = LDC2, aes(x = sorted_x, y = ldc, color = share), size = 3.2, alpha = 0.8) +
    geom_col(data = LDC2, aes(x = sorted_x, y = ldc, fill = hour), position = "identity", alpha = 0.8, width = 0.3) +
    scale_fill_gradientn(colours  = rainbow.pal,
                         limits = c(1, 8760),
                         breaks = (c(1,7,13,19,23)+.3) * 8760 / 24,
                         labels = c("Jan", "Apr", "Jul", "Oct", "Dec"),
                         name = "") +
    theme(plot.title =element_text(size=42), axis.text=element_text(size=40), axis.title=element_text(size= 40,face="bold"), 
          legend.text = element_text(size = 35),
          legend.key.size = unit(1.5, "cm"),legend.title = element_blank(),legend.position = c(.85, .85)) +
    xlab("sorted hour") + ylab("Residual load (MWh)")+ggtitle("Residual Load Duration Curves for various shares of VRE")+
    coord_cartesian( ylim = ylim, xlim = c(0, 8760))
  
  ggsave(filename = paste0(mypath, "LDC_season_wstor3.png"),  width = 20, height =20, units = "in", dpi = 120)

#   p <- ggplot() +
#     geom_line(data = LDC4, aes(x = sorted_x, y = value), size = 3.2, alpha = 0.8, color = "purple") +
#     geom_col(data = LDC4, aes(x = sorted_x, y = value, fill = DayNight ), alpha = 0.8, position = "identity", width = 0.3) +
#     annotate(geom = "text", x = 7000, y = 120, label = "25%VRE", color = "purple",  size=20)+
  #     # geom_line(data = LDC3, aes(x = sorted_x, y = value), size = 3.2, alpha = 0.8, color = "dark green") +
#     # geom_col(data = LDC3, aes(x = sorted_x, y = value, fill = DayNight ), alpha = 0.8, position = "identity", width = 0.3) +
#     # annotate(geom = "text", x = 7000, y = 120, label = "50%VRE", color = "dark green",  size=20)+
#     # geom_line(data = LDC2, aes(x = sorted_x, y = value), size = 3.2, alpha = 0.8, color = "blue") +
#     # geom_col(data = LDC2, aes(x = sorted_x, y = value, fill = DayNight ), alpha = 0.8, position = "identity", width = 0.3) +
#     # annotate(geom = "text", x = 7000, y = 120, label = "70%VRE", color = "blue",  size=20)+
#     # geom_line(data = LDC1, aes(x = sorted_x, y = value), size = 3.2, alpha = 0.8, color = "orange") +
#     # geom_col(data = LDC1, aes(x = sorted_x, y = value, fill = DayNight ), alpha = 0.8, position = "identity", width = 0.3) +
#     # annotate(geom = "text", x = 7000, y = 120, label = "90%VRE", color = "orange",  size=20)+
#     
#     scale_fill_brewer(palette="Set1")+
#     theme(plot.title =element_text(size=45), axis.text=element_text(size=40), axis.title=element_text(size= 40,face="bold"), 
#           legend.text = element_text(size = 28),
#           legend.title = element_text(size = 28),
#           legend.key.size = unit(1.5, "cm")) +
#     xlab("sorted hour") + ylab("Residual load (MWh)")+ggtitle("Residual Load Duration Curves for various shares of VRE")+
#     coord_cartesian( ylim = ylim, xlim = c(0, 8760))
#   
# ggsave(filename =paste0(mypath, "Duration_curve_DAYNIGHT_1.png"),  width = 25, height =25, units = "in", dpi = 120)

# p <- ggplot() +
#   geom_line(data = LDC4, aes(x = sorted_x, y = value), size = 3.2, alpha = 0.8, color = "purple") +
#   annotate(geom = "text", x = 8000, y = 120, label = "25%VRE", color = "purple",  size=10)+
#   geom_line(data = LDC3, aes(x = sorted_x, y = value), size = 3.2, alpha = 0.8, color = "dark green") +
#   annotate(geom = "text", x = 6000, y = 120, label = "50%VRE", color = "dark green",  size=10)+
#   geom_line(data = LDC2, aes(x = sorted_x, y = value), size = 3.2, alpha = 0.8, color = "blue") +
#   annotate(geom = "text", x = 4000, y = 120, label = "70%VRE", color = "blue",  size=10)+
#   geom_line(data = LDC1, aes(x = sorted_x, y = value), size = 3.2, alpha = 0.8, color = "orange") +
#   annotate(geom = "text", x = 2000, y = 120, label = "90%VRE", color = "orange",  size=10)+
#   
#   theme(plot.title =element_text(size=22), axis.text=element_text(size=25), axis.title=element_text(size= 25,face="bold"), 
#         legend.text = element_text(size = 25), legend.title = element_text(size = 25)) +
#   xlab("sorted hour") + ylab("Residual load (MWh)")+ggtitle("Residual Load Duration Curves for various shares of VRE")+
#   coord_cartesian( ylim = ylim, xlim = c(0, 8760))
# 
# ggsave(filename =paste0(mypath, "Duration_curve_lines.png"),  width = 10, height =10, units = "in", dpi = 120)
