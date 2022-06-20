
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


for(year_toplot in year_toplot_list){

# year_toplot = 2045
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
  # for high VRE share, raise baseload cap to make plot prettier
  baseload.dem = baseload.dem * 1.3
}


# peak demand as calculated from average demand and baseload demand (GW)
peak.dem <- avg.dem * 2 - baseload.dem
#residual peak demand calculated from peak demand and sum of dispatchable capacities, it should be positive
# residual <- max(0,peak.dem- disp.cap$value)
residual = 0
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

# generation without wind (solar and dispatchables), i.e. the area that should be painted in solar
gen.wowind <- avg.dem - avg.wind.gen

width.solar = gen.wowind*2/(peak.dem-residual)

if (width.solar < 1){
delta_value2 = (peak.dem - residual) / (round(8760 * width.solar))

residual.solar.demand <- plot.remind.peak.demand %>%  
  expand(plot.remind.peak.demand, hour = seq(1, 8760)) %>% 
  filter(hour <= round(width.solar * 8760)) %>% 
  mutate(value = seq((peak.dem-residual), +delta_value2, -delta_value2)) %>% 
  mutate(tech="Solar")
}

if (width.solar > 1){

delta_value2 = (peak.dem - residual - (baseload.dem - 2 * avg.wind.gen)) / 876

residual.solar.demand <- plot.remind.peak.demand %>% 
  expand(plot.remind.peak.demand, hour = seq(1,8760,10)) %>% 
  mutate(value = seq(peak.dem-residual,baseload.dem-2*avg.wind.gen+delta_value2,-delta_value2)) %>% 
  mutate(tech="Solar")
}

# make a triangle out of total curtailment
# curtailment in GWa
# remind.total.curt <- file.path(outputdir, remind.files[maxiter]) %>%  
#   read.gdx("p32_seelCurt", factor = FALSE) %>% 
#   filter(all_regi == reg, ttot == year_toplot) %>%
#   select(value) %>% 
#   mutate(value = value * 1e3)
# 
# curt.value <- remind.total.curt$value
# largest.cf <- max(plot.remind.cf$cf)/1e2
# 
# peak.curt = 2*8760*curt.value/(8760*(1-largest.cf))
# 
# delta_value3 = peak.curt/(876*(1-largest.cf))
  
# total.curt <- remind.total.curt %>% 
#   expand(remind.total.curt, hour = seq(8760*largest.cf,8760,10)) %>% 
#   mutate(value = - seq(0,peak.curt,delta_value3)) %>% 
#   mutate(tech="Wind") %>% 
#   select(hour,tech,curt=value)
# 
# remind.solar.curt <- file.path(outputdir, remind.files[maxiter]) %>%  
#   read.gdx("v32_storloss", field="l", factor = FALSE) %>% 
#   filter(all_regi == reg, all_te == "spv", ttot == year_toplot) %>%
#   select(value) %>% 
#   mutate(value = value * 1e3)
# 
# solar.curt.value <- remind.solar.curt$value
# 
# solar.curt.width = solar.curt.value*8760*2/(peak.curt)
# 
# delta_value4 = peak.curt/(solar.curt.width/10)
# 
# solar.curt <- remind.total.curt %>% 
#   expand(remind.total.curt, hour = seq(round(8760-solar.curt.width),8760,10)) %>% 
#   mutate(value = - seq(0, peak.curt,delta_value4)) %>% 
#   mutate(tech="Solar") %>% 
#   select(hour,tech,curt=value)

RLDC.VRE <- list(residual.solar.demand,total.demand) %>% 
    reduce(full_join) %>% 
    mutate(tech = factor(tech, levels=rev(c("Solar","Wind"))))

# =================================================================================================
 
p.RM.rldc <-ggplot() +
    geom_area(data = RLDC.VRE, aes(x = hour, y = value, fill = tech), size = 1.2, alpha = 1, position = "identity") +
    geom_area(data = plot.rldc.hr, aes(x = hour, y = value, fill = tech), size = 1.2, alpha = 1) +
    coord_cartesian(ylim = c(-50,210),xlim = c(0,8760))+
    scale_fill_manual(name = "Technology", values = color.mapping.RLDC.basic)+
    xlab("Hour") + ylab("Residual load (GWh)")+
    ggtitle(paste0("REMIND ", year_toplot))
  
  swfigure(sw, grid.draw, p.RM.rldc)
  if (save_png == 1){
    ggsave(filename = paste0(outputdir, "/DIETER/REMIND_RLDC_yr=", year_toplot, ".png"),  p.RM.rldc,  width = 8, height =8, units = "in", dpi = 120)
  }
}
