# plot hourly energy mix for several years from results of DIETER

file = "full_DIETER.gdx"

outputdir = "output/hydro958"
dir = "/home/chengong/remind-coupling-dieter/"
mypath =paste0(dir,outputdir)

mydatapath = paste0(mypath, "/", file)
source("~/remind-coupling-dieter/dataprocessing/DIETER_plots/library_import.R")
library(readr)

dieter.runningcost.variables = c("fuel cost - divided by eta ($/MWh)","CO2 cost ($/MWh)","O&M var cost ($/MWh)")

dieter.dispatch.tech = c("CCGT", "coal","bio", "OCGT_eff", "nuc")

color.mapping <- c("CCGT" = "#999959", 
                   "Coal" = "#0c0c0c",
                   "Solar" = "#ffcc00", "Wind" = "#337fff", "Biomass" = "#005900",
                   "OCGT" = "#e51900", "Hydro" = "#191999", "Nuclear" = "#ff33ff",
                   "Electrolyzers" = "#48D1CC")

#####################################################
#plot hourly price duration curve

year_toplot_list <- c(2020,2025,2030,2035,2040,2045,2050,2055,2060,2070)

for(year_toplot in year_toplot_list){
  
  # year_toplot = 2050
  price_hr <- file.path(mydatapath) %>%
    read.gdx("report_hours", factors = FALSE, squeeze = FALSE) %>% 
    select(filename = X., period = X..2, variable = X..4, hour = X..5, value) %>%
    mutate(hour = as.numeric(str_extract(hour, "[0-9]+"))) %>% 
    filter(variable == "hourly wholesale price ($/MWh)") %>% 
    filter(period == year_toplot) %>%
    select(period, variable, value,hour) %>% 
    mutate(period = as.numeric(period)) 
  
  running_cost <- file.path(mydatapath) %>%
    read.gdx("report_tech", factors = FALSE, squeeze = FALSE) %>% 
    select(filename = X., model = X..1, period = X..2, variable = X..4, tech = X..5, value) %>%
    filter(model == "DIETER") %>% 
    filter(tech %in% dieter.dispatch.tech) %>% 
    revalue.levels(tech = dieter.tech.mapping) %>% 
    mutate(tech = factor(tech, levels = rev(unique(dieter.tech.mapping)))) %>%
    filter(period == year_toplot) %>% 
    filter(variable%in% dieter.runningcost.variables) %>% 
    select(tech, variable, value) %>% 
    dplyr::group_by(tech) %>%
    dplyr::summarise(value = sum(value), .groups = "keep") %>%
    dplyr::ungroup(tech) %>% 
    arrange(desc(value))
  
  running_cost$maxT <-8760
  
  expanded_running_cost <- data.frame(tech = rep(running_cost$tech, running_cost$maxT),
                                      value = rep(running_cost$value, running_cost$maxT),
                                      hour = seq(1,8760))
  
  price_Hr_plot <- price_hr %>% arrange(desc(value))
  price_Hr_plot$sorted_x <- seq(1, 8760)
  max_price <- max(price_Hr_plot$value)
  
  p<-ggplot() +
    geom_line(data = price_Hr_plot, aes(x = sorted_x, y = value ), size = 1.2, alpha = 0.8, color = "blue") +
    geom_line(data = expanded_running_cost, aes(x = hour, y = value, color = tech ), size = 0.8, alpha = 0.5) +
    coord_cartesian(expand = FALSE, ylim = c(0.1, 200)) +
    scale_color_manual(name = "running costs ($/MWh)", values = color.mapping) +
    theme(axis.text = element_text(size=10), axis.title = element_text(size= 10, face="bold")) +
    # scale_y_continuous(trans = 'log10')+
    ggtitle(paste0("DIETER ", year_toplot))+
    xlab("hour") + ylab("electricity price (with scarcity price) ($/MWh)")
  
  ggsave(filename = paste0(mypath, "/DIETER/DIETER_PDC_yr=", year_toplot, ".png"),  width = 8, height =8, units = "in", dpi = 120)
  
}
