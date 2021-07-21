mypath = "~/remind/dataprocessing/DIETER_plots"
mydatapath = "~/remind/output/capfac14/"

igdx("/opt/gams/gams30.2_linux_x64_64_sfx")
# import library

source(paste0(mypath, "library_import.R"))
source(paste0(mypath, "GDXtoQuitte.R"))
library(readr)
# specify output file
file = paste0(mydatapath, "report_DIETER.gdx")

gdxToQuitte(testfile)

hourly_reportCSV = read.csv(paste0(mypath, "results_DIETER_hourlyreport.csv"), sep = ';', header = T, stringsAsFactors = F)
hourly_reportQUITT <- as.quitte(hourly_reportCSV) 

QUITTobj = hourly_reportQUITT

#####################################################
#plot hourly price duration curve, along with the sorted variable (VARkey) curve of one technology "TECH" in quitte object "QUITTobj", for PV share, Wind On share, Wind Off share, and other parameters
# duration_curve_Production_curve <- function(QUITTobj, VARkey, TECHkey){
  
  VARkey = "Net fixed electricity demand"
  # VARkey = "Net flexible electricity demand"
  # select price
  price_hr <- QUITTobj %>% 
    filter(variable == "Wholesale electricity price") %>% 
    mutate(hour = str_replace(hour, hour, str_sub(hour, 2, -1))) %>% 
    mutate(hour = as.numeric(hour)) %>%
    select("variable", "period", "hour", "unit", "tech", "value") 
  
  # select production variable and technology
  techProd_hr <- QUITTobj %>% 
    filter(variable == VARkey) %>%
    # filter(tech == TECHkey) %>%
    mutate(hour = str_replace(hour, hour, str_sub(hour, 2, -1))) %>% 
    mutate(hour = as.numeric(hour)) %>%
    select("variable", "period", "hour", "unit", "tech", "value")
  
  price_hr$DayNight <- ifelse(and(price_hr$hour%%24 >=7, price_hr$hour%%24 <19), "DAY", "NIGHT")
  
  price_hr$tech_Prod <- techProd_hr$value
  
  price_Hr_plot <- price_hr  %>% arrange(desc(value)) 
  price_Hr_plot$sorted_x <- seq(1, 8760)
  max_price <- max(price_Hr_plot$value)
  mean_price= mean(price_Hr_plot$value)
  
  max_techProd= max(techProd_hr$value)
  
  rainbow.pal <- rev(rainbow(100, start = 0.63, end = 0.62))
  max_price_plot = 140
  
  p1<-ggplot(data = price_Hr_plot) +
    geom_line(aes(x = sorted_x, y = value ), size = 1.2, alpha = 0.5, color = "red") +
    coord_cartesian(expand = FALSE, ylim = c(0, max_price_plot)) +
    geom_col(aes(x = sorted_x, y = value, fill = hour ), position = "identity", width = 0.2) +
    scale_fill_gradientn(colours  = rainbow.pal,
                         limits = c(1, 8760),
                         breaks = (c(1,7,13,19,23)+.3) * 8760 / 24,
                         labels = c("Jan", "Apr", "Jul", "Oct", "Dec"),
                         name = "") +
    geom_point(aes(x = sorted_x, y = 0.9 * max_price_plot * tech_Prod/max_techProd), color = "green", size = 5) +
    annotate(geom = "text", x = 2000, y = 0.7 * max_price_plot, label = paste0("mean price = ",round(mean_price)), color = "purple",  size=15) +
    scale_y_continuous(sec.axis = sec_axis(~.*max_techProd/(0.9 * max_price_plot), name = "GW")) +
    theme(axis.text=element_text(size=10), axis.title=element_text(size= 10,face="bold")) +
    xlab("hour") + ylab("electricity price")
  
  ggsave(filename = paste0(mypath, "Duration_curve.png"),  width = 25, height =25, units = "in", dpi = 120)
  
  
  p2<-ggplot(data = price_Hr_plot) + 
    geom_line(aes(x = sorted_x, y = value ), size = 1.2, alpha = 0.5, color = "red") +
    coord_cartesian(expand = FALSE,  ylim = c(0, max_price_plot)) +
    geom_col(aes(x = sorted_x, y = value, fill = DayNight,), position = "identity", width = 0.2) +
    scale_fill_brewer(palette="Set1")+
    geom_point(aes(x = sorted_x, y = 0.9 * max_price_plot * tech_Prod/max_techProd), color = "green", size = 5) +
    annotate(geom = "text", x = 2000, y = 0.7 * max_price_plot, label = paste0("mean price = ", round(mean_price)), color = "purple",  size=15) +
    annotate(geom = "text", x = 2000, y = 0.8 * max_price_plot, label = paste0("max", VARkey," =", round(max_techProd)), color = "blue",  size=15) +
    scale_y_continuous(sec.axis = sec_axis(~.*max_techProd/(0.9*max_price_plot), name = "GW")) +
    theme(axis.text=element_text(size=10), axis.title=element_text(size= 10, face="bold")) +
    xlab("hour") + ylab("electricity price")
  
  ggsave(filename =paste0(mypath, "Duration_curve_DAYNIGHT.png"),  width = 25, height =25, units = "in", dpi = 120)
  
# }
