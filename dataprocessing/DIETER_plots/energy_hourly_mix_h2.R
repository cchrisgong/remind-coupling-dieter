# plot hourly energy mix for several years from results of DIETER

mypath = "~/remind-coupling-dieter/dataprocessing/DIETER_plots/"
runnumber = "hydro601"
mydatapath = paste0("~/remind-coupling-dieter/output/", runnumber, "/")

# specify output file
filenames <- list.files(mydatapath, pattern="fulldata_[0-9]+\\.gdx")
maxiter = length(filenames)-1
iteration = maxiter

# import library
source(paste0(mypath, "library_import.R"))
source(paste0(mypath, "GDXtoQuitte.R"))
library(readr)

igdx("/opt/gams/gams30.2_linux_x64_64_sfx")


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
                         el = "Electricity",
                         elh2 = "Electrolyzer",
                         NULL)

color.mapping <- c("CCGT" = "#999959", "Lignite" = "#0c0c0c", 
                   # "Coal (Lig + HC)" = "#0c0c0c",
                   "Solar" = "#ffcc00", "Wind" = "#337fff", "Biomass" = "#005900",
                   "OCGT" = "#e51900", "Hydro" = "#191999", "Nuclear" = "#ff33ff",
                   # "Hard coal" = "#808080",
                   # "peak demand" = "#0c0c0c", "Sto2" = "#e51900",  
                   "Electrolyzer" = "#66cccc", "Electricity" = "red")

#####################################################
year_toplot_list <- c(2020,2030,2045,2050,2070)

for(season in c("summer", "winter")){
  
for(year_toplot in year_toplot_list){
  
  # year_toplot = 2030
  
  hourly_reportCSV = read.csv(paste0(mypath, "/", runnumber, "_i",iteration,"_hourlyreport.csv"), sep = ';', header = T, stringsAsFactors = F)

  hourly_reportQUITT <- as.quitte(hourly_reportCSV) 
  
  QUITTobj0<-hourly_reportQUITT %>% 
    filter(period == year_toplot) %>% 
    revalue.levels(tech = dieter.tech.mapping) %>%
    mutate( tech = factor(tech, levels=rev(unique(dieter.tech.mapping))) )
  
  if (season == "summer"){
  QUITTobj <- QUITTobj0 %>% 
    filter((hour > 5500) & (hour < 6000) )
  }
  if (season == "winter"){
    QUITTobj <- QUITTobj0 %>% 
      filter((hour > 1500) & (hour < 2000) )
  }

  
 generation<- QUITTobj %>% 
    filter(variable == "generation (GWh)") %>% 
    select(tech, hour, value) 
 
 consumption<- QUITTobj %>% 
   filter(variable == "consumption (GWh)") %>% 
   select(tech, hour, value)%>% 
   mutate(value = -value)
 
 # storage <- QUITTobj %>%
   # filter(variable == "generation storage (MWh)") %>% 
   # select(tech, hour, value) %>% 
   # mutate(value = value / 1e3) %>% 
   # mutate(tech = "Sto2")
 
 # generationANDstorage = list(generation, storage) %>%
   # reduce(full_join) 
   
 price <- QUITTobj %>% 
   filter(variable == "hourly wholesale price ($/MWh)") %>% 
   mutate(value = value / 1e3) %>% 
   select(hour, value)
 
 plot_scale = 800
 
  p <- ggplot() +
    geom_col(generation , mapping = aes(x=hour, y=value, fill=tech), alpha = 0.7)  +
    # geom_col(generationANDstorage , mapping = aes(x=hour, y=value, fill=tech), alpha = 0.7)  + 
    geom_point(data=price, mapping = aes(x=hour, y=value*plot_scale), color = "blue", group = 2)+
    # geom_point(data=demand, mapping = aes(x=hour, y=value), color = "red", group = 1)+
    geom_col(consumption, mapping = aes(x=hour, y=value, fill=tech), alpha = 0.7)  + 
    # scale_y_continuous("Generation vs Consumption (GWh)") + xlab("") +
    scale_fill_manual(name = "(GWh)", values = color.mapping)
  
  # p <- p + geom_line(data=demand, mapping = aes(x=hour, y=value, group = 1))
  # p <- p + geom_line(data=price, mapping = aes(x=hour, y=value*plot_scale, group = 2), alpha = 0.5)
  
  p <- p + scale_y_continuous(sec.axis = sec_axis(~./plot_scale, name = "Hourly price ($/kWh) (blue)"))
  
  # modifying colours and theme options
  # p <- p + scale_colour_manual(values = c("blue", "red"))
  p <- p + labs(y = "Generation vs Consumption (GWh)",
                x = "Hour of the Day",
                colour = "")
  p <- p + theme(legend.position = c(0.95, 0.8))
  
  
  # ggsave(paste0(mypath, "/",runnumber,"_yr=", year_toplot, "_generation_hourly_mix_winter.png"), width = 20,  height = 10)
  ggsave(paste0(mypath, "/",runnumber,"_", season, "_yr=", year_toplot, "_generation_hourly_mix.png"), width = 20,  height = 10)
}
}
  #############################################################################################
  