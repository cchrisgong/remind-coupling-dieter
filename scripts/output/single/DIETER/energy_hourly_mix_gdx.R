# plot hourly energy mix for several years from results of DIETER

mypath = "~/DIETER/dieter-coupling-remind/DIETER"
file = "full_DIETER_y10.gdx"
file = "full_DIETER.gdx"
mydatapath = paste0(mypath, "/", file)
source("~/remind-coupling-dieter/dataprocessing/DIETER_plots/library_import.R")
library(readr)

outputdir = "output/hydro1180"
dir = "/home/chengong/remind-coupling-dieter/"
mypath =paste0(dir,outputdir)
mydatapath = paste0(mypath, "/", file)

dieter.tech.mapping <- c(coal = "Coal",
                         nuc = "Nuclear",
                         OCGT_eff = "OCGT",
                         CCGT = "CCGT",
                         bio = "Biomass",
                         ror = "Hydro",
                         Wind_on = "Wind Onshore",
                         Wind_off = "Wind Offshore",
                         Solar = "Solar",
                         elh2 = "Flexible electrolyzers (PtG)",
                         el = "Electricity",
                         `all Tech` = "All Tech",
                         vregrid = "VRE grid",
                         lith = "Lithium-ion battery",
                         PSH = "Pumped Storage Hydro",
                         hydrogen = "Electrolyzers for long-term storage",
                         caes = "Compressed Air Energy Storage",
                         NULL)



# label mapping for plots
dieter.variables <- c( 
  "generation (GWh)",
  "curtailment renewable (GWh)",
  "storage generation (GWh)",
  "storage loading (GWh)",
  "consumption (GWh)",
  NULL)


supply.color.mapping <- c("Solar" = "#ffcc00", 
                          "Wind Onshore" = "#337fff", 
                          "Wind Offshore" = "#334cff",
                          "Lithium-ion battery" ="cyan",
                          "Pumped Storage Hydro" ="#D55E00",
                          "Electrolyzers for long-term storage" = "#56B4E9",
                          "Compressed Air Energy Storage" =  "#CC79A7",
                          "Wind" = "#337fff", "OCGT" = "#e51900", 
                          "Biomass" = "#005900",
                           "CCGT" = "#999959", "Coal" = "#0c0c0c", 
                           "Hydro" = "#191999", "Nuclear" = "#ff33ff",
                           NULL)

demand.color.mapping <- c("Flexible electrolyzers (PtG)" = "#66cccc", "Electricity" = "red", 
                          NULL)

color.mapping <- c(supply.color.mapping, demand.color.mapping)

#####################################################
year_toplot_list <- c(2025,2030,2035,2040,2045,2050)

for(season in c("summer", "winter")){
  
  for(year_toplot in year_toplot_list){
    
    # year_toplot = 2045
    
  hr_data <- file.path(mydatapath) %>%
    read.gdx("report_tech_hours", factors = FALSE, squeeze = FALSE) %>% 
    # select(model = X., period = X..1, variable = X..3, tech = X..4, hour = X..5, value) %>% 
    select(filename = X., period = X..2, variable = X..4, tech = X..5, hour = X..6, value) %>% 
    revalue.levels(tech = dieter.tech.mapping) %>% 
    mutate(hour = as.numeric(str_extract(hour, "[0-9]+"))) %>% 
    mutate(tech = factor(tech, levels = rev(unique(dieter.tech.mapping)))) %>%
    filter(variable%in% dieter.variables) %>%
    filter(period == year_toplot) %>% 
    select(period, tech, variable, value,hour) %>% 
    mutate(period = as.numeric(period)) 
  
    if (season == "summer"){
      hr_data <- hr_data %>% 
        filter((hour > 5500) & (hour < 5670) )
      # filter((hour > 4500) & (hour < 6000) )
    }
    if (season == "winter"){
      hr_data <- hr_data %>% 
        filter((hour > 1500) & (hour < 1670) )
      # filter((hour > 500) & (hour < 2000) )
    }
    
    generation <- hr_data %>% 
      filter(variable == "generation (GWh)") %>% 
      select(tech, hour, value) 
    
    consumption <- hr_data %>% 
      filter(variable == "consumption (GWh)") %>% 
      select(tech, hour, value) %>% 
      mutate(value = -value) 
    
    storage_out <- hr_data %>%
      filter(variable == "storage generation (GWh)") %>%
      select(tech, hour, value)
   
    storage_in <- hr_data %>% 
      filter(variable == "storage loading (GWh)") %>% 
      select(tech, hour, value) %>% 
      mutate(value = -value)
    
    hr.data.plot = list(generation, consumption, storage_out, storage_in) %>%
      reduce(full_join)%>% 
      order.levels(tech = names(color.mapping)) 
      
    plot_scale = 800
    
    p <- ggplot() +
      geom_col(hr.data.plot , mapping = aes(x=hour, y=value, fill=tech), alpha = 0.7)  +
      scale_fill_manual(name = "Technology", values = color.mapping)
    
    p <- p + labs(y = "Hourly generation/storage outflow (positive) (GWh) \n Hourly consumption/storage incharge (negative) (GWh)",
                  x = "Hour of the Day",
                  colour = "")
    # p <- p + theme(legend.position = c(0.85, 0.75))
    p<-p +theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(size=8))
    
    
    ggsave(paste0(mypath, "/DIETER/year=", year_toplot, season,"_generation_hourly_mix.png"), width = 6.5,  height = 6)
  }
  
}
