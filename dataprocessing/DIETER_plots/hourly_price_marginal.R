year_toplot = 2050

FixedCost.hourly <- FixedCost %>% 
expand(FixedCost, hour = seq(1, 8760)) 
  
COST.hourly <- list(RunningCost.hourly, FixedCost.hourly) %>% 
  reduce(full_join) %>% 
  select(-Xlabel) %>% 
  mutate(value = value)

QUITTobj<-hourly_reportQUITT %>% 
  filter(period == year_toplot) %>% 
  revalue.levels(tech = dieter.tech.mapping) %>%
  mutate( tech = factor(tech, levels=rev(unique(dieter.tech.mapping))) ) 
  
price.hourly <- QUITTobj %>% 
  filter(variable == "hourly wholesale price ($/MWh)") %>% 
  mutate(value = value/1e3) %>% 
  select(hour,variable,value) %>% 
  mutate(tech="price")

df.plot <- list(COST.hourly, price.hourly) %>% 
  reduce(full_join)%>%
  filter(hour < 575) %>% 
  filter(hour > 565) 

fill_var = c("hourly wholesale price ($/MWh)", "annualized investment cost ($/kW)", "O&M cost ($/kW)","fuel cost - divided by eta ($/MWh)","CO2 cost ($/MWh)")
  
p <- ggplot() +
  geom_bar(data = df.plot, aes(x = tech, y = value, fill = factor(variable,fill_var)), position = "stack", stat = "identity") + 
  coord_cartesian(ylim=c(0, 150)) +
  xlab("") + ylab("Euro/kWh") +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank()) +
  theme(aspect.ratio = 1.2)+
  scale_fill_manual(values = col_vector) +
  facet_wrap( ~ hour, scales = "free_x", nrow =2)

ggsave(paste0(mypath, "/",runnumber,"_yr=", year_toplot, "_marg_cost_fixedcost.png"), width = 26,  height = 12)
  

p <- ggplot() +
  geom_bar(data = df.plot, aes(x = tech, y = value, fill = factor(variable,fill_var)), position = "stack", stat = "identity") + 
  coord_cartesian(ylim=c(0, 0.2)) +
  xlab("") + ylab("Euro/kWh") +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank()) +
  theme(aspect.ratio = 1.2)+
  scale_fill_manual(values = col_vector) +
  facet_wrap( ~ hour, scales = "free_x", nrow =2)

ggsave(paste0(mypath, "/",runnumber,"_yr=", year_toplot, "_marg_cost_runningcost.png"), width = 26,  height = 12)



