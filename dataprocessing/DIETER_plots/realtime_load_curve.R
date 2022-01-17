
mypath = "~/DIETER/myFirstParallelDIETER/dataprocessing/"
igdx("/opt/gams/gams30.2_linux_x64_64_sfx")
# import library

source(paste0(mypath, "library_import.R"))
source(paste0(mypath, "GDXtoQuitte.R"))
library(readr)
# specify output file
testfile = "~/DIETER/myFirstParallelDIETER/DIETER/results_DIETER_1.gdx"
gdxToQuitte(testfile)

hourly_reportCSV = read.csv(paste0(mypath, "results_DIETER_1_hourlyreport.csv"), sep = ';', header = T, stringsAsFactors = F)
hourly_reportQUITT <- as.quitte(hourly_reportCSV) 

QUITTobj = hourly_reportQUITT

#####################################################
#plot a few days of load curve and sector coupling (flexible) load curve
  
  VARkey1 = "Net fixed electricity demand"
  VARkey2 = "Net flexible electricity demand"

  VARkey3 = "Hourly generation"
  TECHkey1 = "Wind_on"
  TECHkey2 = "Solar PV"
  
  # select production variable and technology
  dem_hr1 <- QUITTobj %>% 
    filter(variable == VARkey1) %>%
    mutate(hour = str_replace(hour, hour, str_sub(hour, 2, -1))) %>% 
    mutate(hour = as.numeric(hour)) %>%
    select("variable", "period", "hour", "unit", "tech", "value")%>% 
    filter(hour %in% c(1800:1900))

  dem_hr2 <- QUITTobj %>% 
    filter(variable == VARkey2) %>%
    mutate(hour = str_replace(hour, hour, str_sub(hour, 2, -1))) %>% 
    mutate(hour = as.numeric(hour)) %>%
    select("variable", "period", "hour", "unit", "tech", "value") %>% 
    filter(hour %in% c(1800:1900))
  
  dem_hr =list(dem_hr1, dem_hr2) %>% 
    reduce(full_join)
 
  prod_hr1 <- QUITTobj %>% 
    filter(variable == VARkey3) %>%
    filter(tech == TECHkey1) %>%
    mutate(hour = str_replace(hour, hour, str_sub(hour, 2, -1))) %>% 
    mutate(hour = as.numeric(hour)) %>%
    select("variable", "period", "hour", "unit", "tech", "value") %>% 
    filter(hour %in% c(1800:1900))
 
  prod_hr2 <- QUITTobj %>% 
    filter(variable == VARkey3) %>%
    filter(tech == TECHkey2) %>%
    mutate(hour = str_replace(hour, hour, str_sub(hour, 2, -1))) %>% 
    mutate(hour = as.numeric(hour)) %>%
    select("variable", "period", "hour", "unit", "tech", "value") %>% 
    filter(hour %in% c(1800:1900)) 

  prod_hr =list(prod_hr1, prod_hr2) %>% 
    reduce(full_join) 

  dem_tot_hr =dem_hr1 %>% 
    mutate(value = value + dem_hr2$value) %>% 
    mutate(variable = "Total electricity demand")
  
  p1<-ggplot() +
    geom_area(data = dem_hr, aes(x = hour, y = value,fill=variable ), size = 1.2, alpha = 0.5, position = 'stack') +
    theme(axis.text=element_text(size=10), axis.title=element_text(size= 10,face="bold")) +
    xlab("hour") + ylab("MW")
 
  p2<-ggplot() +
    geom_area(data = prod_hr, aes(x = hour, y = value ,fill=tech ), size = 1.2, alpha = 0.5, position = 'stack') +
    geom_line(data = dem_tot_hr, aes(x = hour, y = value, color = variable), size = 1.2, alpha = 0.5) +
    geom_line(data = dem_hr2, aes(x = hour, y = value, color = variable),linetype="dotted", size = 1.2, alpha = 0.5) +
    theme(axis.text=element_text(size=10), axis.title=element_text(size= 10,face="bold")) +
    xlab("hour") + ylab("MW")+ scale_fill_manual(values=c("yellow", "blue"))+
    scale_color_manual(values=c("black","black"))
  
  library(grid)
  grid.newpage()
  p <- arrangeGrob(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))
  grid.draw(p)
  
  ggsave(file=paste0(mypath, "real_time_demProd.png"), p,  width = 16, height =10, units = "in", dpi = 120)
   
  