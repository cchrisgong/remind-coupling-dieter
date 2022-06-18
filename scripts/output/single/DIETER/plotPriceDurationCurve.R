#####################################################
#plot hourly price duration curve
dieter.runningcost.variables.PDC = c("fuel cost - divided by eta ($/MWh)","CO2 cost ($/MWh)","O&M var cost ($/MWh)")

dieter.capture.price.variables.PDC <- c("DIETER Market value ($/MWh)")

dieter.dispatch.tech.PDC = c("CCGT", "coal","bio", "OCGT_eff", "nuc")
dieter.demand.tech.PDC <- c("elh2")

year_toplot_list <- model.periods.RLDC

for(year_toplot in year_toplot_list){
  
  # Data preparation --------------------------------------------------------
  # year_toplot = 2050
  price_hr <- file.path(outputdir, dieter.files.report[length(dieter.files.report)]) %>%
    read.gdx("report_hours", factors = FALSE, squeeze = FALSE) %>% 
    select(filename = X., period = X..2, variable = X..4, hour = X..5, value) %>%
    mutate(hour = as.numeric(str_extract(hour, "[0-9]+"))) %>% 
    filter(variable == "hourly wholesale price ($/MWh)") %>% 
    filter(period == year_toplot) %>%
    select(period, variable, value,hour) %>% 
    mutate(period = as.numeric(period)) 
  
  running_cost <- file.path(outputdir, dieter.files.report[length(dieter.files.report)]) %>%
    read.gdx("report_tech", factors = FALSE, squeeze = FALSE) %>% 
    select(filename = X., model = X..1, period = X..2, variable = X..4, tech = X..5, value) %>%
    filter(model == "DIETER") %>% 
    filter(tech %in% dieter.dispatch.tech.PDC) %>% 
    revalue.levels(tech = dieter.tech.mapping) %>% 
    mutate(tech = factor(tech, levels = rev(unique(dieter.tech.mapping)))) %>%
    filter(period == year_toplot) %>% 
    filter(variable%in% dieter.runningcost.variables.PDC) %>% 
    select(tech, variable, value) %>% 
    dplyr::group_by(tech) %>%
    dplyr::summarise(value = sum(value), .groups = "keep") %>%
    dplyr::ungroup(tech) 
  
  running_cost$maxT <-8760
  
  expanded_running_cost <- data.frame(tech = rep(running_cost$tech, running_cost$maxT),
                                      value = rep(running_cost$value, running_cost$maxT),
                                      hour = seq(1,8760))
  
  capture_price <- file.path(outputdir, dieter.files.report[length(dieter.files.report)]) %>%
    read.gdx("report_tech", factors = FALSE, squeeze = FALSE) %>% 
    select(filename = X., model = X..1, period = X..2, variable = X..4, tech = X..5, value) %>%
    filter(model == "DIETER") %>% 
    filter(tech %in% dieter.demand.tech.PDC) %>% 
    revalue.levels(tech = dieter.tech.mapping) %>% 
    mutate(tech = factor(tech, levels = rev(unique(dieter.tech.mapping)))) %>%
    filter(period == year_toplot) %>% 
    filter(variable%in% dieter.capture.price.variables.PDC) %>% 
    select(tech, variable, value)
  
  capture_price$maxT <-8760
  
  expanded_capture_price <- data.frame(tech = rep(capture_price$tech, capture_price$maxT),
                                      value = rep(capture_price$value, capture_price$maxT),
                                      hour = seq(1,8760))
  
  price_Hr_plot <- price_hr %>% arrange(desc(value))
  price_Hr_plot$sorted_x <- seq(1, 8760)
  max_price <- max(price_Hr_plot$value)
  
  if (h2switch == "on"){
  cost.plot <- list(expanded_running_cost, expanded_capture_price) %>%
    reduce(full_join)
  }
  
  if (h2switch == "off"){
    cost.plot <- list(expanded_running_cost) %>%
      reduce(full_join)
  }
  
  p.PDC<-ggplot() +
    geom_line(data = price_Hr_plot, aes(x = sorted_x, y = value ), size = 1.2, alpha = 1, color = "blue") +
    geom_line(data = cost.plot, aes(x = hour, y = value, color = tech ), size = 0.8, alpha = 0.8) +
    coord_cartesian(expand = FALSE, ylim = c(0.1, 200)) +
    scale_color_manual(name = "Running costs ($/MWh)", values = color.mapping.PDC) +
    theme(axis.text = element_text(size=15), axis.title = element_text(size= 15, face="bold")) +
    ggtitle(paste0("DIETER ", year_toplot))+
    theme(legend.position="bottom", legend.direction="horizontal")+
    xlab("hour") + ylab("electricity price (with scarcity price) ($/MWh)")
  
  swfigure(sw, grid.draw, p.PDC)
  if (save_png == 1){
    ggsave(filename = paste0(outputdir, "/DIETER/DIETER_PDC_yr=", year_toplot, ".png"),  p.PDC,width = 6.5, height =6.5, units = "in", dpi = 120)
  }

}
