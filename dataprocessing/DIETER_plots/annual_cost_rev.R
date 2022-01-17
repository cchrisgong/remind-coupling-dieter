# hourly cost and revenue


year_toplot_list <- c(2030,2050,2070)
# 
for(year_toplot in year_toplot_list){
  
# year_toplot = 2030

files_DT_rep <- list.files(mydatapath, pattern = "report_DIETER_i[0-9]+\\.gdx")
sorted_annual_report_DT <- paste0(myDIETERPLOT_path, runnumber, "_i", seq(from = 2, to = length(files_DT_rep), by = 1), "_annualreport.csv")
fixedcost_report_key_DT = c("annualized investment cost ($/kW)", "O&M cost ($/kW)")
runningcost_report_key_DT = c("fuel cost - divided by eta ($/MWh)","CO2 cost ($/MWh)")
cap_report_key_DT = c("DIETER post-investment capacities (GW)")

cvs = sorted_annual_report_DT[[iteration - 1]]
annual_reportCSV = read.csv(cvs, sep = ";", header = T, stringsAsFactors = F)
annual_reportQUITT <- as.quitte(annual_reportCSV) 

hourly_reportCSV = read.csv(paste0(mypath, "/", runnumber, "_i",iteration,"_hourlyreport.csv"), sep = ';', header = T, stringsAsFactors = F)
hourly_reportQUITT <- as.quitte(hourly_reportCSV) 

QUITTobj<-hourly_reportQUITT %>% 
  filter(period == year_toplot) %>% 
  revalue.levels(tech = dieter.tech.mapping) %>%
  mutate( tech = factor(tech, levels=rev(unique(dieter.tech.mapping))) ) 

generation<- QUITTobj %>% 
  filter(variable == "generation (GWh)") %>% 
  select(tech, hour, value) 

price <- QUITTobj %>% 
  filter(variable == "hourly wholesale price ($/MWh)") %>% 
  mutate(value = value) %>% 
  select(hour, price = value)

rep_DT <- annual_reportQUITT %>% 
  filter(period %in% year_toplot) %>% 
  filter(tech %in% names(dieter.tech.mapping)) %>% 
  revalue.levels(tech = dieter.tech.mapping) %>%
  mutate(tech = factor(tech, levels=rev(unique(dieter.tech.mapping)))) %>% 
  filter(variable %in% c(fixedcost_report_key_DT, runningcost_report_key_DT, cap_report_key_DT)) %>% 
  select(tech, variable, value)

FixedCost <- rep_DT %>% 
  filter(variable %in% fixedcost_report_key_DT ) 

RunningCost <- rep_DT %>% 
  filter(variable %in% runningcost_report_key_DT) 
RunningCost.spread = spread(RunningCost, variable, value)

cap <- rep_DT %>% 
  filter(variable %in% cap_report_key_DT) 
cap.spread <- spread(cap, variable, value) %>%
  dplyr::rename(cap = "DIETER post-investment capacities (GW)")

RunningCost.hourly <- list(RunningCost.spread, generation, cap.spread) %>% 
  reduce(full_join) %>% 
  replace(is.na(.), 0) %>% 
  filter(!tech %in% c("Solar", "Wind", "Hydro"))%>% 
  dplyr::rename(FC = "fuel cost - divided by eta ($/MWh)") %>% 
  dplyr::rename(CO2 = "CO2 cost ($/MWh)") %>%
  mutate(CO2_hourly = value * CO2*1e3 / (cap*1e6)) %>% 
  mutate(FC_hourly = value * FC *1e3/ (cap*1e6)) %>% 
  select(hour,tech,CO2_hourly,FC_hourly) %>% 
  dplyr::rename("fuel cost - divided by eta ($/MWh)" = FC_hourly) %>% 
  dplyr::rename("CO2 cost ($/MWh)" = CO2_hourly) %>%
  gather(variable,value,3:4)

RunningCost.hourly$Xlabel <- "Cost"

RunningCost.annual <- RunningCost.hourly %>% 
  select(-hour) %>% 
    dplyr::group_by(tech, variable) %>%
    dplyr::summarise( value = sum(value), .groups = "keep" ) %>% 
    dplyr::ungroup(tech, variable) %>% 
    replace(is.na(.), 0) 

RunningCost.annual$Xlabel <- "Cost"
FixedCost$Xlabel <- "Cost"

COST <- list(RunningCost.annual, FixedCost) %>% 
  reduce(full_join)

REVENUE <- list(generation, price, cap.spread ) %>% 
  reduce(full_join) %>% 
  mutate(value = value * price * 1e3/(cap*1e6)) %>% 
  dplyr::group_by(tech) %>%
  dplyr::summarise( value = sum(value), .groups = "keep" ) %>% 
  dplyr::ungroup(tech)

REVENUE$Xlabel <- "Revenue"
REVENUE$variable <- "Revenue"

df.plot <- list(COST, REVENUE) %>% 
  reduce(full_join) 

n <- 5
qual_col_pals = brewer.pal.info[ brewer.pal.info$category == 'qual', ]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

p <- ggplot() +
  geom_bar(data = subset(df.plot, Xlabel == "Cost"), aes(x = Xlabel, y = value, fill = variable), position = "stack", stat = "identity") +  
  geom_bar(data = subset(df.plot, Xlabel == "Revenue"), aes(x = Xlabel, y = value, fill = variable), position = "stack", stat = "identity") +
  xlab("") + ylab("Euro/kW") +
  # guides(fill=guide_legend(ncol=4))+
  scale_fill_manual(values=col_vector)+
  # scale_fill_hue(c = 40) +
  # geom_text(data=df.plot, aes(x = Xlabel, y = pos, label = costType))+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank())+
  facet_wrap( ~ tech, scales = "free", nrow = 1)

ggsave(paste0(mypath, "/",runnumber,"_yr=", year_toplot, "_cost_rev.png"), width = 16,  height = 6)

}

