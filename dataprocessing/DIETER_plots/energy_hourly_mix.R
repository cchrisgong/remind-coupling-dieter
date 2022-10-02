# plot hourly energy mix for several years from results of DIETER

mypath = "~/remind-coupling-dieter/dataprocessing/DIETER_plots/"
runnumber = "hydro910"
mydatapath = paste0("~/remind-coupling-dieter/output/", runnumber, "/")

# import library

source(paste0(mypath, "library_import.R"))
library(readr)

# specify output file
iteration =maxiter
file = paste0("report_DIETER_i", iteration, ".gdx")

dieter.tech.mapping <- c(hc = "Hard coal",
                         lig = "Lignite",
                         coal = "Coal (Lig + HC)",
                         nuc = "Nuclear",
                         OCGT_eff = "OCGT",
                         CCGT = "CCGT",
                         bio = "Biomass",
                         ror = "Hydro",
                         Wind_on = "Wind",
                         Solar = "Solar",
                         Electrolyzer = "elh2",
                         Electricity = "el",
                         NULL)

color.mapping <- c("CCGT" = "#999959", "Lignite" = "#0c0c0c", "Coal (Lig + HC)" = "#0c0c0c",
                   "Solar" = "#ffcc00", "Wind" = "#337fff", "Biomass" = "#005900",
                   "OCGT" = "#e51900", "Hydro" = "#191999", "Nuclear" = "#ff33ff",
                   "Hard coal" = "#808080", 
                   # "Sto2" = "#e51900", 
                   "Electrolyzer" = "#40E0D0")

#####################################################
year_toplot_list <- c(2030,2050,2070)
# 
for(year_toplot in year_toplot_list){
  
  # year_toplot = 2030
  
   hourly_reportCSV = read.csv(paste0(mypath, runnumber, "_i",iteration,"_hourlyreport.csv"), sep = ';', header = T, stringsAsFactors = F)

  hourly_reportQUITT <- as.quitte(hourly_reportCSV) 
  
  QUITTobj<-hourly_reportQUITT %>% 
    filter(period == year_toplot) %>% 
    revalue.levels(tech = dieter.tech.mapping) %>%
    mutate( tech = factor(tech, levels=rev(unique(dieter.tech.mapping))) ) %>% 
    filter((hour > 5500) & (hour < 6000) )
  
  H2_demand <- QUITTobj %>% 
    filter(variable == "consumption (GWh)") %>% 
    filter(tech == "elh2") %>% 
    select(hour, value) 
  
  el_demand <- QUITTobj %>% 
    filter(variable == "consumption (GWh)") %>% 
    filter(tech == "el") %>% 
    select(hour, value) 
  
 generation<- QUITTobj %>% 
    filter(variable == "generation (GWh)") %>% 
    select(tech, hour, value) 
 
 # storage <- QUITTobj %>% 
 #   filter(variable == "generation storage (MWh)") %>% 
 #   select(tech, hour, value) %>% 
 #   mutate(value = value / 1e3) %>% 
 #   mutate(tech = "Sto2")
 # 
 # generationANDstorage = list(generation, storage) %>%
 #   reduce(full_join) 
   
 price <- QUITTobj %>% 
   filter(variable == "hourly wholesale price ($/MWh)") %>% 
   mutate(value = value / 1e3) %>% 
   select(hour, value)
 
 plot_scale = 800
 
  p <- ggplot() +
    geom_col(generation , mapping = aes(x=hour, y=value, fill=tech), alpha = 0.7)  +
    # geom_col(generationANDstorage , mapping = aes(x=hour, y=value, fill=tech), alpha = 0.7)  + 
    geom_point(data=price, mapping = aes(x=hour, y=value*plot_scale), color = "blue", group = 2)+
    geom_point(data=el_demand, mapping = aes(x=hour, y=value), color = "red", group = 1)+
    # geom_point(data=H2_demand, mapping = aes(x=hour, y=value), color = "purple", group = 1)+
    scale_y_continuous("Generation (GWh)") + xlab("") +
    scale_fill_manual(name = "hourly generation (GWh)", values = color.mapping)
  
  p <- p + geom_line(data=el_demand, mapping = aes(x=hour, y=value, group = 1))
  # p <- p + geom_line(data=H2_demand, mapping = aes(x=hour, y=value, group = 2))
  p <- p + geom_line(data=price, mapping = aes(x=hour, y=value*plot_scale, group = 3), alpha = 0.5)
  
  p <- p + scale_y_continuous(sec.axis = sec_axis(~./plot_scale, name = "Hourly price ($/kWh) (blue)"))
  
  # modifying colours and theme options
  p <- p + scale_colour_manual(values = c("blue", "red"))
  p <- p + labs(y = "demand (GWh) (red)",
                x = "Hour of the Day",
                colour = "")
  p <- p + theme(legend.position = c(0.9, 0.9))
  
  
  ggsave(paste0(mypath, "/",runnumber,"_yr=", year_toplot, "_generation_hourly_mix.png"), width = 26,  height = 12)
}
  #############################################################################################
  
