# plot hourly energy mix for several years from results of DIETER

# file = "full_DIETER_y10.gdx"
file = "full_DIETER.gdx"

outputdir = "output/hydro945"
dir = "/home/chengong/remind-coupling-dieter/"
mypath =paste0(dir,outputdir)

mydatapath = paste0(mypath, "/", file)
source("~/remind-coupling-dieter/dataprocessing/DIETER_plots/library_import.R")
library(readr)

VAR_report_key_DT = c("fuel cost - divided by eta ($/MWh)","CO2 cost ($/MWh)","O&M var cost ($/MWh)")

TECH_DISPATCH_DT = c("CCGT", "lig","bio", "OCGT_eff", "nuc", "hc")

color.mapping <- c("CCGT" = "#999959", 
                   "Coal" = "#0c0c0c",
                   "Solar" = "#ffcc00", "Wind" = "#337fff", "Biomass" = "#005900",
                   "OCGT" = "#e51900", "Hydro" = "#191999", "Nuclear" = "#ff33ff",
                   "Electrolyzers" = "#48D1CC")

#####################################################
#plot hourly price duration curve

# year_toplot_list <- c(2030,2050,2070) 
year_toplot_list <- c(2020,2025,2030,2035,2040,2045,2050,2055,2060,2070)

for(year_toplot in year_toplot_list){
  # year_toplot = 2030
  
  annual_reportQUITT <- as.quitte(annual_reportCSV) 
  
  running_cost <- annual_reportQUITT %>% 
    filter(period %in% year_toplot) %>% 
    filter(tech %in% TECH_DISPATCH_DT) %>% 
    filter(model == "DIETER") %>% 
    filter(variable %in% VAR_report_key_DT) %>% 
    revalue.levels(tech = dieter.tech.mapping) %>%
    mutate(tech = factor(tech, levels=rev(unique(dieter.tech.mapping)))) %>% 
    select(tech, value) %>% 
    dplyr::group_by(tech) %>%
    dplyr::summarise( value = sum(value), .groups = "keep" ) %>% 
    dplyr::ungroup(tech)
  
  running_cost$maxT <-8760
  
  expanded_running_cost <- data.frame(tech = rep(running_cost$tech, running_cost$maxT),
                                      value = rep(running_cost$value, running_cost$maxT),
                                      hour = seq(1,8760))
  
  price_hr <- QUITTobj %>% 
    filter(variable == "hourly wholesale price ($/MWh)") %>% 
    mutate(hour = as.numeric(hour)) %>% 
    filter(period == year_toplot) %>% 
    mutate(unit = "$/MWh", tech, value) %>% 
    select(variable, period, hour, tech, value)
  
  price_Hr_plot <- price_hr %>% arrange(desc(value))
  price_Hr_plot$sorted_x <- seq(1, 8760)
  max_price <- max(price_Hr_plot$value)
  
  p1<-ggplot() +
    geom_line(data = price_Hr_plot, aes(x = sorted_x, y = value ), size = 1.2, alpha = 0.8, color = "blue") +
    geom_line(data = expanded_running_cost, aes(x = hour, y = value, color = tech ), size = 0.8, alpha = 0.5) +
    coord_cartesian(expand = FALSE, ylim = c(0.1, 200)) +
    scale_color_manual(name = "running costs ($/MWh)", values = color.mapping) +
    theme(axis.text = element_text(size=10), axis.title = element_text(size= 10, face="bold")) +
    # scale_y_continuous(trans = 'log10')+
    ggtitle(paste0("DIETER ", year_toplot))+
    xlab("hour") + ylab("electricity price (with scarcity price) ($/MWh)")
  
  ggsave(filename = paste0(myDIETERPLOT_path, "Price_duration_curve_wRunningCost_", runnumber,"_yr", year_toplot, "_iter_", iteration,".png"),  width = 7, height =7, units = "in", dpi = 120)
  
}
