
cat("Plot REMIND RLDC \n")

# plot REMIND RLDC ordered by their capacity factor

if (h2switch == "off"){
  year_toplot_list <- model.periods.RLDC
}

if (h2switch == "on"){
  # year_toplot_list <- model.periods.till2070
  year_toplot_list <- model.periods.till2045
}

remind.genshare.vre <- out.remind.genshare %>% 
  filter(iteration == maxiter-1) %>% 
  filter(tech %in% remind.vre.mapping) %>% 
  select(period,genshare) %>%
  dplyr::group_by(period) %>%
  dplyr::summarise( genshare = sum(genshare), .groups = "keep" ) %>% 
  dplyr::ungroup(period) 

# for(year_toplot in year_toplot_list){

year_toplot = 2045
print(year_toplot)
plot.remind.cf <- out.remind.capfac %>% 
  filter(tech %in% remind.nonvre.mapping.whyd) %>% 
  filter(period == year_toplot) %>% 
  filter(iteration == maxiter-1) %>% 
  select(tech,cf=value)

plot.cap <- out.remind.capacity  %>% 
  filter(tech %in% remind.nonvre.mapping.whyd)%>% 
  filter(period == year_toplot) %>% 
  filter(iteration == maxiter-1) %>% 
  select(tech,cap=value)

plot.rldc <- plot.cap %>% 
  full_join(plot.remind.cf) %>% 
  mutate(flh = round(cf/1e2 * 8760,0)) %>% 
  arrange(desc(flh)) %>% 
  select(tech,flh,cap)

highcfranking = as.vector(plot.rldc$tech)

plot.rldc.hr <- plot.rldc %>% 
  expand(plot.rldc, hour = seq(1,8760,10)) %>% 
  dplyr::group_by(cap, tech) %>%
  arrange(desc(flh)) %>% 
  filter(hour <= flh) %>% 
  dplyr::ungroup(cap, tech) %>% 
  select(tech,value=cap,hour) %>% 
  mutate(tech = factor(tech, levels=rev(unique(highcfranking))))

#---------------------------
# hourly average demand is total demand /8760 (REMIND demand is in Twa)
avg.remind.tot.demand <- file.path(outputdir, remind.files[maxiter]) %>%  
  read.gdx("v32_usableSeDisp", field="l", factor = FALSE) %>% 
  filter(all_regi == reg) %>%
  filter(entySe == "seel") %>%
  select(period=ttot,value) %>% 
  filter(period == year_toplot) %>% 
  mutate(value = value * 1e3)

h2consum <- file.path(outputdir, remind.files[maxiter]) %>%
  read.gdx("vm_demSe", field="l", factors = FALSE,squeeze = FALSE) %>% 
  filter(all_regi == reg) %>% 
  filter(all_te == "elh2") %>% 
  mutate(value = value * 1e3) %>%
  select(period = ttot, h2dem = value) %>% 
  filter(period == year_toplot) 

avg.dem <- as.numeric(avg.remind.tot.demand$value) - as.numeric(h2consum$h2dem)

## depending on VRE share
df.vreShare <- remind.genshare.vre %>% 
  filter(period == year_toplot)

vreShare <- as.numeric(df.vreShare$genshare)

if (vreShare < 85){
# baseload as defined by CF > 33% for medium VRE share (<70%)
  baseload_cf_limit = 33
}

if (vreShare >= 85){
  # baseload as defined by CF > 0% for medium VRE share (<70%)
  baseload_cf_limit = 0
}

baseload.cap <- plot.remind.cf %>% 
  left_join(plot.cap) %>% 
  filter(cf > baseload_cf_limit) %>% 
  select(- cf) %>% 
  dplyr::summarise(cap = sum(cap)) %>% 
  select(value = cap)

# dispatchable 
disp.cap <- plot.cap %>% 
  dplyr::summarise( cap = sum(cap)) %>% 
  select(value = cap)

baseload.dem <- baseload.cap$value

if (vreShare > 80){
  # for high VRE share, raise "baseload" capacity (right edge of outer most trapezoid) to make plot prettier
  baseload.dem = baseload.dem * 1.3
}

# peak demand as calculated from average demand and baseload demand (GW)
peak.dem <- avg.dem * 2 - baseload.dem

#residual peak demand calculated from peak demand and sum of dispatchable capacities, it should be positive
if (policyMode == 1){
  residual = 0
}

if (policyMode == 9){
  residual <- peak.dem*0.2 #start drawing solar from 80% of peak demand
}

delta_value = (peak.dem-baseload.dem)/876

plot.remind.peak.demand <- out.remind.peak.demand %>% 
  filter(period == year_toplot) %>% 
  filter(iteration == maxiter-1) %>% 
  select(value)

# make a trapezoid out of peak demand and baseload demand, paint it in wind color
total.demand <- plot.remind.peak.demand %>% 
  expand(plot.remind.peak.demand, hour = seq(1,8760,10)) %>% 
  mutate(value = seq(peak.dem,baseload.dem + delta_value, -delta_value)) %>% 
  mutate(tech="Wind")
  
# get wind production (wind is plotted on the outside)
remind.wind.prod <- file.path(outputdir, remind.files[maxiter]) %>%  
  read.gdx("v32_usableSeTeDisp", field="l", factor = FALSE) %>% 
  filter(all_regi == reg, entySe == "seel", ttot == year_toplot) %>%
  filter(all_te %in% c("wind","windoff")) %>% 
  select(value) %>% 
  dplyr::summarise( value = sum(value), .groups = "keep" ) %>% 
  mutate(value = value * 1e3)

avg.wind.gen <- remind.wind.prod$value

# generation WithOut wind (solar and dispatchables), i.e. the area that should be painted in solar
gen.wowind <- avg.dem - avg.wind.gen

width.solar = gen.wowind*2/(peak.dem-residual)

# if the wowind patch width is smaller than 8760
if (width.solar < 1){
delta_value2 = (peak.dem - residual) / (round(876 * width.solar))

residual.solar.demand <- plot.remind.peak.demand %>%  
  expand(plot.remind.peak.demand, hour = seq(1, 8760,10)) %>% 
  filter(hour <= round(width.solar * 8760)) %>% 
  mutate(value = seq((peak.dem-residual), +delta_value2, -delta_value2)) %>% 
  mutate(tech="Solar")
}

# if the wowind patch width is larger than 8760, then wowind patch also needs to be a trapezoid
if (width.solar > 1){
  
delta_value2 = (peak.dem - residual - (baseload.dem - 2 * avg.wind.gen)) / 876

residual.solar.demand <- plot.remind.peak.demand %>% 
  expand(plot.remind.peak.demand, hour = seq(1,8760,10)) %>% 
  mutate(value = seq(peak.dem-residual,baseload.dem-2*avg.wind.gen+delta_value2,-delta_value2)) %>% 
  mutate(tech="Solar")
}

if (policyMode == 1){
largest.cf <- max(plot.remind.cf$cf)/1e2 + 0.07 #add 7% to largest CF for aesthetic reasons
}

if (policyMode == 9){
  largest.cf <- max(plot.remind.cf$cf)/1e2 + 0.25 #add 25% to largest CF for aesthetic reasons
}

# take the GWa curtailment value, times 8760 hours, divided by (width/2), where width is the difference
# between 8760 and the highest full load hour plant
remind.total.curt <- file.path(outputdir, remind.files[maxiter]) %>%
  read.gdx("p32_curtLoss", factor = FALSE) %>%
  filter(all_regi == reg, ttot == year_toplot) %>%
  select(value) %>%
  dplyr::summarise( value = sum(value)) %>%
  mutate(value = value * 1e3)

curt.value = remind.total.curt$value

peak.curt = 2*8760*curt.value/(8760*(1-largest.cf))

delta_value3 = peak.curt/(876*(1-largest.cf))

total.curt.rldc <- remind.total.curt %>%
  expand(remind.total.curt, hour = seq(8760*largest.cf,8760,10)) %>%
  mutate(value = - seq(0,peak.curt,delta_value3)) %>%
  mutate(tech="Solar") %>%
  select(hour,tech,value)

remind.wind.curt <- file.path(outputdir, remind.files[maxiter]) %>%
  read.gdx("v32_storloss", field="l", factor = FALSE) %>%
  filter(all_regi == reg, all_te%in%c("wind","windoff"), ttot == year_toplot) %>%
  select(value) %>%
  dplyr::summarise( value = sum(value)) %>%
  mutate(value = value * 1e3)

wind.curt.value <- remind.wind.curt$value

wind.curt.width = 2*8760*wind.curt.value/(peak.curt)

delta_value4 = peak.curt/(wind.curt.width/10)

remind.wind.curt.rldc <- remind.total.curt %>%
  expand(remind.total.curt, hour = seq(round(8760-wind.curt.width),8760,10)) %>%
  mutate(value = - seq(0, peak.curt,delta_value4)) %>%
  mutate(tech="Wind") %>%
  select(hour,tech,value)

RLDC.VRE <- list(residual.solar.demand,total.demand) %>% 
    reduce(full_join) %>% 
    mutate(tech = factor(tech, levels=rev(c("Solar","Wind"))))

RLDC.VREcurt <- list(remind.wind.curt.rldc,total.curt.rldc) %>% 
  reduce(full_join) %>% 
  mutate(tech = factor(tech, levels=rev(c("Wind","Solar"))))
# =================================================================================================

p.RM.rldc <-ggplot() +
    geom_area(data = RLDC.VRE, aes(x = hour, y = value, fill = tech), size = 1.2, alpha = 1, position = "identity") + 
    geom_area(data = plot.rldc.hr, aes(x = hour, y = value, fill = tech), size = 1.2, alpha = 1) +
  geom_area(data = RLDC.VREcurt, aes(x = hour, y = value, fill = tech), size = 1.2, alpha = 1, position = "identity") + 
    coord_cartesian(ylim = c(-50,210),xlim = c(0,8760))+
    scale_fill_manual(name = "Technology", values = color.mapping.RLDC.basic)+
    xlab("Hour") + ylab("Residual load (GWh)")+
    ggtitle(paste0("REMIND ", year_toplot))
  
  swfigure(sw, grid.draw, p.RM.rldc)
  if (save_png == 1){
    ggsave(filename = paste0(outputdir, "/DIETER/REMIND_RLDC_yr=", year_toplot, ".png"),  p.RM.rldc,  width = 8, height =8, units = "in", dpi = 120)
  }
# }
