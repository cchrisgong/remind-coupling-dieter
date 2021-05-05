mypath = "~/remind-coupling-dieter/dataprocessing/DIETER_plots/"

runnumber = "mrkup106"
mydatapath = paste0("~/remind-coupling-dieter/output/", runnumber, "/")


igdx("/opt/gams/gams30.2_linux_x64_64_sfx")
# import library

source(paste0(mypath, "library_import.R"))
source(paste0(mypath, "GDXtoQuitte.R"))
library(readr)
# specify output file
iteration = 35
file = paste0("report_DIETER_i", iteration, ".gdx")


# gdxToQuitte_hourly(mydatapath, file,runnumber)

hourly_reportCSV = read.csv(paste0(mypath, runnumber, "_i", iteration, "_hourlyreport.csv"), sep = ';', header = T, stringsAsFactors = F)

hourly_reportQUITT <- as.quitte(hourly_reportCSV) 

QUITTobj = hourly_reportQUITT

#####################################################
#plot hourly price duration curve

year_toplot_list <- c(2030,2040,2050,2055,2070,2080,2090,2100) 

for(year_toplot in year_toplot_list){

  # year_toplot = 2055
  # select price
  price_hr <- QUITTobj %>% 
    filter(variable == "hourly wholesale price ($/MWh)") %>% 
    mutate(hour = as.numeric(hour)) %>% 
    filter(period == year_toplot) %>% 
    mutate(unit = "$/MWh", tech, value) %>% 
    select(variable, period, hour, tech, value)

  # select production variable and technology
  # techProd_hr <- QUITTobj %>% 
  #   filter(variable == VARkey) %>%
  #   # filter(tech == TECHkey) %>%
  #   mutate(hour = str_replace(hour, hour, str_sub(hour, 2, -1))) %>% 
  #   mutate(hour = as.numeric(hour)) %>%
  #   select("variable", "period", "hour", "unit", "tech", "value")
  # 
  # # price_hr$DayNight <- ifelse(and(price_hr$hour%%24 >= 7, price_hr$hour%%24 < 19), "DAY", "NIGHT")
  # 
  # price_hr$tech_Prod <- techProd_hr$value
  # 
  price_Hr_plot <- price_hr %>% arrange(desc(value))
  price_Hr_plot$sorted_x <- seq(1, 8760)
  max_price <- max(price_Hr_plot$value)
  
  # price_Hr_plot <- price_Hr_plot %>% 
  #   filter(sorted_x < 10)
  
  # mean_price = mean(price_Hr_plot$value)
  # 
  # max_techProd= max(techProd_hr$value)
  
  # rainbow.pal <- rev(rainbow(100, start = 0.63, end = 0.62))
  # max_price_plot = 140
  
  # p1<-ggplot(data = price_Hr_plot) +
  #   geom_line(aes(x = sorted_x, y = value ), size = 1.2, alpha = 0.5, color = "red") +
  #   coord_cartesian(expand = FALSE, ylim = c(0, max_price_plot)) +
  #   geom_col(aes(x = sorted_x, y = value, fill = hour ), position = "identity", width = 0.2) +
  #   scale_fill_gradientn(colours  = rainbow.pal,
  #                        limits = c(1, 8760),
  #                        breaks = (c(1,7,13,19,23)+.3) * 8760 / 24,
  #                        labels = c("Jan", "Apr", "Jul", "Oct", "Dec"),
  #                        name = "") +
  #   geom_point(aes(x = sorted_x, y = 0.9 * max_price_plot * tech_Prod/max_techProd), color = "green", size = 5) +
  #   annotate(geom = "text", x = 2000, y = 0.7 * max_price_plot, label = paste0("mean price = ",round(mean_price)), color = "purple",  size=15) +
  #   scale_y_continuous(sec.axis = sec_axis(~.*max_techProd/(0.9 * max_price_plot), name = "GW")) +
  #   theme(axis.text=element_text(size=10), axis.title=element_text(size= 10,face="bold")) +
  #   xlab("hour") + ylab("electricity price")
  # 
  # ggsave(filename = paste0(mypath, "Duration_curve.png"),  width = 25, height =25, units = "in", dpi = 120)
  # 
  
  p1<-ggplot( data = price_Hr_plot ) +
    geom_line(aes(x = sorted_x, y = value ), size = 1.2, alpha = 0.5, color = "red") +
    coord_cartesian(expand = FALSE, ylim = c(0.1, max_price*100)) +
    # geom_col( aes(x = sorted_x, y = value, fill = hour ), position = "identity", width = 0.2) +
    # geom_point(aes(x = sorted_x, y = 0.9 * max_price_plot * tech_Prod/max_techProd), color = "green", size = 5) +
    # annotate(geom = "text", x = 2000, y = 0.7 * max_price_plot, label = paste0("mean price = ",round(mean_price)), color = "purple",  size=15) +
    # scale_y_continuous(sec.axis = sec_axis(~.*max_techProd/(0.9 * max_price_plot), name = "GW")) +
    theme(axis.text = element_text(size=10), axis.title = element_text(size= 10, face="bold")) +
    scale_y_continuous(trans = 'log10')+
    ggtitle(paste0("DIETER ", year_toplot))+
    xlab("hour") + ylab("electricity price (with scarcity price) ($/MWh)")
  
  ggsave(filename = paste0(mypath, "Price_duration_curve_yr", year_toplot, "_", runnumber, "_iter_", iteration,".png"),  width = 5, height =5, units = "in", dpi = 120)
  
}
