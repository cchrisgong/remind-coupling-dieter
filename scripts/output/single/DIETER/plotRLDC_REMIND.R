# Data preparation (REMIND) -----------------------------------------------

cat("Plot REMIND RLDC \n")

# ordered by their capacity factor
year_toplot_list <- c(2020,2025,2030,2035,2040,2045,2050,2055,2060,2070,2080,2090,2100)

for(year_toplot in year_toplot_list){

  # year_toplot = 2035
  
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
  select(tech,cap,hour) %>% 
  mutate(tech = factor(tech, levels=rev(unique(highcfranking))))
  
# #=================================================================================================

p<-ggplot() +
  geom_area(data = plot.rldc.hr, aes(x = hour, y = cap, fill = tech), size = 1.2, alpha = 1) +
  coord_cartesian(ylim = c(0,170),xlim = c(0,8760))+
  scale_fill_manual(name = "Technology", values = color.mapping)+
  xlab("hour") + ylab("residual load (GW)")+
  ggtitle(paste0("REMIND ", year_toplot))

swfigure(sw, grid.draw, p)
if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/REMIND_RLDC_yr=", year_toplot, ".png"),  p,  width = 8, height =8, units = "in", dpi = 120)
}

}
