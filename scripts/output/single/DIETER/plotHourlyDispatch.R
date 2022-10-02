# plot hourly energy mix for several years from results of DIETER


# setwd("/home/chengong/remind-coupling-dieter/")
# outputdir <- "./output/hydro1186"


#####################################################
year_toplot_list <- model.periods.till2045

for(season in c("summer", "winter")){
  
  for(year_toplot in year_toplot_list){
    
    # year_toplot = 2045
    
  hr_data <-  file.path(outputdir, dieter.files.report[length(dieter.files.report)]) %>%
    read.gdx("report_tech_hours", factors = FALSE, squeeze = FALSE) %>% 
    # select(model = X., period = X..1, variable = X..3, tech = X..4, hour = X..5, value) %>% 
    select(filename = X., period = X..2, variable = X..4, tech = X..5, hour = X..6, value) %>% 
    revalue.levels(tech = dieter.tech.mapping) %>% 
    mutate(hour = as.numeric(str_extract(hour, "[0-9]+"))) %>% 
    mutate(tech = factor(tech, levels = rev(unique(dieter.tech.mapping)))) %>%
    filter(variable%in% dieter.variables.hrly.mix) %>%
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
      order.levels(tech = names(color.mapping.hrly.mix)) 
      
    plot_scale = 800
    
    p.hrly <- ggplot() +
      geom_col(hr.data.plot , mapping = aes(x=hour, y=value, fill=tech), alpha = 0.7)  +
      scale_fill_manual(name = "Technology", values = color.mapping.hrly.mix) +
      labs(y = "Hourly generation/storage outflow (positive) (GWh) \n Hourly consumption/storage incharge (negative) (GWh)",  x = "Hour of the Day", colour = "") +
      theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(size=8))
    
    swfigure(sw,print,p.hrly)
    
    if (save_png == 1){
      ggsave(filename = paste0(outputdir, "/DIETER/year=", year_toplot, season,"_generation_hourly_mix.png"), p.hrly, width = 6.5, height =6, units = "in", dpi = 120)
    }
    
  }
  
  if (season == "summer"){
    p.hrly.summer <- p.hrly
  }
  if (season == "winter"){
    p.hrly.winter <- p.hrly
  }
  
}
