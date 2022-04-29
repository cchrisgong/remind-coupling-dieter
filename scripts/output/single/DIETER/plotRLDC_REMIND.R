
cat("Plot REMIND RLDC \n")

# plot REMIND RLDC ordered by their capacity factor

for(year_toplot in model.periods.till2100){

# year_toplot = 2045
  
plot.remind.cf <- out.remind.capfac %>% 
  filter(tech %in% remind.nonvre.mapping.whyd) %>% 
  filter(period == year_toplot) %>% 
  filter(iteration == maxiter-1)%>% 
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
# average demand is total demand /8760 (REMIND demand is in Twa)
remind.demand <- file.path(outputdir, remind.files[maxiter]) %>%  
  read.gdx("v32_usableSeDisp", field="l", factor = FALSE) %>% 
  filter(all_regi == reg) %>%
  filter(entySe == "seel") %>%
  select(period=ttot,value) %>% 
  filter(period == year_toplot) %>% 
  mutate(value = value * 1e3)
  
avg.dem <- remind.demand$value

# baseload as defined by CF > 50%
baseload.cap <- plot.remind.cf %>% 
  left_join(plot.cap) %>% 
  filter(cf > 33) %>% 
  select(-cf) %>% 
  dplyr::summarise( cap = sum(cap)) %>% 
  select(value = cap)

baseload.dem <- baseload.cap$value

# peak demand as calculated from average demand and baseload demand (GW)
peak.dem <- avg.dem*2-baseload.dem

delta_value = (peak.dem-baseload.dem)/876

plot.remind.peak.demand <- out.remind.peak.demand %>% 
  filter(period == year_toplot) %>% 
  filter(iteration == maxiter-1) 

# make a trapezoid out of peak demand and baseload demand, paint it in solar
total.demand <- plot.remind.peak.demand %>% 
  expand(plot.remind.peak.demand, hour = seq(1,8760,10)) %>% 
  mutate(value = seq(peak.dem,baseload.dem+delta_value,-delta_value)) %>% 
  mutate(tech="Solar")
  
remind.solar.prod <- file.path(outputdir, remind.files[maxiter]) %>%  
  read.gdx("v32_usableSeTeDisp", field="l", factor = FALSE) %>% 
  filter(all_regi == reg, entySe == "seel", ttot == year_toplot) %>%
  filter(all_te %in% c("spv")) %>% 
  select(value) %>% 
  mutate(value = value * 1e3)

solar.cap <- remind.solar.prod$value

delta_value2 = (peak.dem-(baseload.dem-2*solar.cap))/876

residual.solar.demand <- plot.remind.peak.demand %>% 
  expand(plot.remind.peak.demand, hour = seq(1,8760,10)) %>% 
  mutate(value = seq(peak.dem,baseload.dem-2*solar.cap+delta_value2,-delta_value2)) %>% 
  mutate(tech="Wind")

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

RLDC.VRE <- list(total.demand, residual.solar.demand) %>% 
    reduce(full_join)

# =================================================================================================
 
  p<-ggplot() +
    geom_area(data = RLDC.VRE, aes(x = hour, y = value, fill = tech), size = 1.2, alpha = 1, position = "identity") +
    geom_area(data = plot.rldc.hr, aes(x = hour, y = value, fill = tech), size = 1.2, alpha = 1) +
    coord_cartesian(ylim = c(-50,210),xlim = c(0,8760))+
    scale_fill_manual(name = "Technology", values = color.mapping.RLDC.basic)+
    xlab("hour") + ylab("residual load (GW)")+
    ggtitle(paste0("REMIND ", year_toplot))
  
  swfigure(sw, grid.draw, p)
  if (save_png == 1){
    ggsave(filename = paste0(outputdir, "/DIETER/REMIND_RLDC_yr=", year_toplot, ".png"),  p,  width = 8, height =8, units = "in", dpi = 120)
  }
}
