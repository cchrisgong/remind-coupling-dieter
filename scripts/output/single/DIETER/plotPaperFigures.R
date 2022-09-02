# Plots for paper

library(cowplot)  # Useful for themes and for arranging plots
# library(ggnewscale)  # Useful for multiple legends (not yet required)
library(metR)  # Useful for contour_fill
# library(svglite)
library(scales)

font.size = 8

# Plotting style ----------------------------------------------------------
theme_set(theme_cowplot(font_size = 8))  # Use simple theme and set font size

# Figure 2: Electricity price convergence ---------------------------------

# calculate shadow price in remind due to capacity constraint - no, not needed since markup should add up to zero at convergence,
# and these shadow prices are not part of the markup

# Panel 1: Surface plot
# with interpolation
# p.surface <- ggplot() +
#   # With REMIND rolling mean over three periods
#   geom_contour_fill(data = diff.price.rollmean,
#                     mapping = aes(x = iteration, y = period, z = value),
#                     breaks = seq(-20, 40, 5)) +
#   scale_fill_divergent(name = "$/MWh",
#                        limits = c(-20, 20),
#                        #breaks = seq(-20,20,10),
#                        labels = c(-20, -10, 0, 10, ">20"),
#                        oob = scales::squish) +
#   scale_x_continuous(name = "Iteration") +
#   scale_y_continuous(name = "Time",
#                      breaks = seq(2020, 2100, 10)) +
#   ggtitle("Electricity price difference (REMIND - DIETER)") +
#   theme(axis.text = element_text(size = font.size),
#         axis.title = element_text(size = font.size)) +
#   theme(legend.text = element_text(size = font.size))

# without interpolation
p.surface <- ggplot() +
  # With REMIND rolling mean over three periods
  geom_raster(data = diff.price.rollmean,
                    mapping = aes(x = iteration, y = period, fill = value))+
  # scale_fill_continuous(name = "$/MWh",
  #                       limits = c(-20, 20),
  #                       breaks = seq(-20,20,5),
  #                       labels = c(-20,-15,-10,-5, 0, 5, 10, 15, ">20"),
  #                       oob = scales::squish,
  #                       guide = guide_legend(reverse=TRUE)
  #                       )+
  scale_fill_gradient2(name = "$/MWh",
                       limits = c(-20, 20),
                       breaks = seq(-20,20,5),
                       low = "#0000FFFF", mid = "white", high = "#FF0000FF", midpoint = .0,
                       labels = c(-20,-15,-10,-5, 0, 5, 10, 15, ">20"),
                       oob = scales::squish,
                       guide = guide_legend(reverse=TRUE))+
  scale_x_continuous(name = "Iteration") +
  scale_y_continuous(name = "Time",
                     breaks = seq(2020, 2100, 10)) +
  ggtitle("Electricity price difference (REMIND - DIETER)") +
  theme(axis.text = element_text(size = font.size),
        axis.title = element_text(size = font.size)) +
  theme(legend.text = element_text(size = font.size)) 

remind.price.avg <- out.RMprice %>% 
  filter(period %in% model.periods) %>% 
  filter(!value == 0) %>% 
  select(period, iteration, rmprice=value) %>% 
  dplyr::group_by(iteration) %>% 
  mutate( rmprice = frollmean(rmprice, 3, align = "left", fill = 0) ) %>%
  dplyr::ungroup(iteration) %>% 
  dplyr::group_by(iteration) %>%
  dplyr::summarise( rmprice = mean(rmprice), .groups = "keep" ) %>% 
  dplyr::ungroup(iteration) 
  
diff.price.rollmean.avg.yr.rel <- diff.price.rollmean.avg.yr %>% 
  select(iteration, value) %>% 
  full_join(remind.price.avg) %>% 
  mutate(value = value / rmprice*100)

# Panel 2: Time averaged line plot
p.line <- ggplot() + 
  geom_line(
            # data = diff.price.avg.yr,
            data = diff.price.rollmean.avg.yr.rel,
            mapping = aes(x = iteration,
                          y = value,
                          color = "Time-averaged (%)")) + 
  scale_color_manual(name = element_blank(),
                     values = c("Time-averaged (%)" = "black")) +
  scale_x_continuous(name = "Iteration") + 
  scale_y_continuous(name = "%",
                     limits = c(-0.05, max(diff.price.rollmean.avg.yr.rel$value)))+
  theme_minimal_grid(12) +
  theme(axis.text = element_text(size = font.size),
        axis.title = element_text(size =font.size))+
  theme(legend.text = element_text(size=font.size))
  
# Arrange both plots
p <- plot_grid(p.surface,
               p.line,
               ncol = 1,
               rel_heights = c(1, 0.5),
               labels = c("(a)", "(b)"),
               label_size = 1.2*font.size,
               align = "v")

# Save as png
ggsave(filename = paste0(outputdir, "/DIETER/FIGURE02.png"),
       bg = "white",
       width = 14,  # Vary width according to how many panels plot has
       height = 10,  # Vary height according to how many panels plot has
       units = "cm")

# Save as svg
# ggsave(filename = paste0(outputdir, "/DIETER/_FIGURE02.svg"),
#        bg = "white",
#        width = 14,  # Vary width according to how many panels plot has
#        height = 10,  # Vary height according to how many panels plot has
#        units = "cm")

# Figure 3: Generation convergence ------------------------------------------
if (coupMode == "none"){
  diff_breaks_cap = seq(-30,30,15)
  diff_breaks_gen = seq(-30,30,15)
}

if (coupMode == "validation" | coupMode == "dispatch"){
  diff_breaks_cap = seq(-15,15,5)
  diff_breaks_gen = seq(-10,10,2)
}


# With manual colour scales, we need to order this vector 
color.mapping.gen.order <- color.mapping.cap[levels(plot.dieter.gen2$tech)]
color.mapping.gen.order <- color.mapping.gen.order[!is.na(color.mapping.gen.order)]

# Make additional tibble for capacity sum
# This way we can draw a border around the entire stacked bar
plot.dieter.gen2.group <- plot.dieter.gen2 %>% 
  dplyr::group_by(period,model) %>% 
  dplyr::summarise( sum = sum(value), .groups = "keep" ) %>% 
  dplyr::ungroup(period,model)

plot.remind.gen2.group <- plot.remind.gen2 %>% 
  dplyr::group_by(period,model) %>% 
  dplyr::summarise( sum = sum(value), .groups = "keep" ) %>% 
  dplyr::ungroup(period,model)

plot.remind.gen.total <- out.remind.generation %>% 
  filter(iteration == maxiter - 1) %>% 
  filter(period < 2110) %>% 
  mutate(period = as.numeric(period)) %>% 
  select(period,tech,value) %>% 
  dplyr::group_by(period) %>% 
  dplyr::summarise( remind_gen_total = sum(value), .groups = "keep" ) %>% 
  dplyr::ungroup(period)

p.gen.doublebar <- ggplot() +
  # Stacked bar for DIETER
  geom_bar(data = plot.dieter.gen2,
           mapping = aes(x = period, y = value, fill = tech),
           stat = "identity",
           position = "stack",
           width = 1.5) +
  # Border around stacked bar
  geom_bar(data = plot.dieter.gen2.group,
           mapping = aes(x = period, y = sum, linetype = model),
           lwd = 0.5,
           colour = "black",
           stat = "identity",
           fill = "transparent",
           width = 1.5) + 
  # stacked bar for REMIND
  geom_bar(data = plot.remind.gen2,
           mapping = aes(x = period, y = value, fill = tech),
           stat = "identity",
           position = "stack",
           width = 1.5) +
  # Border around stacked bar
  geom_bar(data = plot.remind.gen2.group,
           mapping = aes(x = period, y = sum, linetype = model),
           lwd = 0.5,
           colour = "black",
           stat = "identity",
           fill = "transparent",
           width = 1.5) + 
  theme_minimal_grid(12)+
  scale_fill_manual(name = "Technology", values = color.mapping.gen.order) +
  scale_linetype_manual(name = "Model", values = linetype.map[c("REMIND", "DIETER")]) +
  xlab("Time") +
  ylab("Generation (TWh)") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10,face="bold"), plot.title = element_text(size = 12, face = "bold")) +
  ggtitle("Annual generation in REMIND and DIETER") 

plot.gen.relDiff <- list(plot.remind.gen.snap, plot.dieter.gen.snap, plot.remind.gen.total) %>%
  reduce(full_join) %>%
  filter(!tech %in% remind.storage.mapping.narrow) %>% 
  mutate(delta_gen = (remind_gen - dieter_gen),
         delta_gen.rel = 100 * (remind_gen - dieter_gen) / remind_gen_total)

p.gen.diff <- ggplot() +
  geom_bar(data = plot.gen.relDiff ,
           mapping = aes(x = period, y = delta_gen.rel, fill = tech),
           stat = "identity") +
  theme_minimal_grid(12)+ 
  scale_fill_manual(name = "Technology", values = color.mapping.gen.order) +
  scale_y_continuous(name = "Difference (%)", breaks = diff_breaks_gen) + 
  xlab("Time") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10,face="bold"), plot.title = element_text(size = 10, face = "bold")) +
  ggtitle("Annual generation diff. (REMIND - DIETER) / annual REMIND generation ") 

# Arrange both plots
p.plots <- plot_grid(
  p.gen.doublebar + theme(legend.position = "none"),  # Without legend
  p.gen.diff + theme(legend.position = "none"),  # Without legend
  ncol = 1,
  rel_heights = c(1, 0.6),
  labels = c("(a)", "(b)"),
  label_size = 1.7*font.size,
  align = "v"
)

# Extract legend of doublebar plot for 
p.legend <- get_legend(p.gen.doublebar + theme(legend.box.margin = margin(0, 0, 0, 12)))

# Put together plot and legend
p <- plot_grid(p.plots,
               p.legend,
               ncol = 2,
               rel_widths = c(1, 0.45))

# Save as png
ggsave(filename = paste0(outputdir, "/DIETER/FIGURE03.png"),
       bg = "white",
       width = 22,  # Vary width according to how many panels plot has
       height = 12,  # Vary height according to how many panels plot has
       units = "cm")

# Save as svg
# ggsave(filename = paste0(outputdir, "/DIETER/FIGURE03.svg"),
#        bg = "white",
#        width = 18,  # Vary width according to how many panels plot has
#        height = 12,  # Vary height according to how many panels plot has
#        units = "cm")


# Figure 4: Capacity convergence ------------------------------------------
# With manual colour scales, we need to order this vector 
color.mapping.cap.order <- color.mapping.cap[levels(plot.dieter.capacity2$tech)]
color.mapping.cap.order <- color.mapping.cap.order[!is.na(color.mapping.cap.order)]

# Make additional tibble for capacity sum
# This way we can draw a border around the entire stacked bar
plot.dieter.capacity2.group <- plot.dieter.capacity2 %>% 
  dplyr::group_by(period,model) %>% 
  dplyr::summarise( sum = sum(value), .groups = "keep" ) %>% 
  dplyr::ungroup(period,model)

plot.remind.capacity2.group <- plot.remind.capacity.wDIETERstorage2 %>% 
  dplyr::group_by(period,model) %>% 
  dplyr::summarise( sum = sum(value), .groups = "keep" ) %>% 
  dplyr::ungroup(period,model)

p.cap.doublebar <- ggplot() +
  # Stacked bar for DIETER
  geom_bar(data = plot.dieter.capacity2,
           mapping = aes(x = period, y = value, fill = tech),
           stat = "identity",
           position = "stack",
           width = 1.5) +
  # Border around stacked bar
  geom_bar(data = plot.dieter.capacity2.group,
           mapping = aes(x = period, y = sum, linetype = model),
           lwd = 0.5,
           colour = "black",
           stat = "identity",
           fill = "transparent",
           width = 1.5) + 
  # stacked bar for REMIND
  geom_bar(data = plot.remind.capacity.wDIETERstorage2,
           mapping = aes(x = period, y = value, fill = tech),
           stat = "identity",
           position = "stack",
           width = 1.5) +
  # Border around stacked bar
  geom_bar(data = plot.remind.capacity2.group,
           mapping = aes(x = period, y = sum, linetype = model),
           lwd = 0.5,
           colour = "black",
           stat = "identity",
           fill = "transparent",
           width = 1.5) + 
  theme_minimal_grid(12)+
  scale_fill_manual(name = "Technology", values = color.mapping.cap.order) +
  scale_linetype_manual(name = "Model", values = linetype.map[c("REMIND", "DIETER")]) +
  xlab("Time") +
  ylab("Capacity (GW)") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10,face="bold"), plot.title = element_text(size = 12, face = "bold")) +
  ggtitle("Capacity in REMIND and DIETER") 

plot.remind.cap.total <- out.remind.capacity %>% 
  filter(iteration == maxiter - 1) %>% 
  filter(period < 2110) %>%
  mutate(period = as.numeric(period)) %>% 
  select(period,tech,value) %>% 
  dplyr::group_by(period) %>%
  dplyr::summarise( remind_cap_total = sum(value), .groups = "keep" ) %>% 
  dplyr::ungroup(period)

plot.cap.relDiff <- list(plot.remind.cap.snap, plot.dieter.cap.snap, plot.remind.cap.total) %>%
  reduce(full_join) %>% 
  mutate(delta_cap = (remind_cap - dieter_cap), 
         delta_cap.rel = 100 * (remind_cap - dieter_cap) / remind_cap_total)

p.cap.diff <-ggplot() +
  geom_bar(data = plot.cap.relDiff ,
           mapping = aes(x = period, y = delta_cap.rel, fill = tech),
           stat = "identity") +
  theme_minimal_grid(12)+
  scale_fill_manual(name = "Technology", values = color.mapping.cap.order) +
  scale_y_continuous(name = "Difference (%)", breaks = diff_breaks_cap) + 
  xlab("Time") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10,face="bold"), plot.title = element_text(size = 12, face = "bold")) +
  ggtitle("Capacity diff. (REMIND - DIETER) / total REMIND capacity ") 

# Arrange both plots
p.plots <- plot_grid(
  p.cap.doublebar + theme(legend.position = "none"),  # Without legend
  p.cap.diff + theme(legend.position = "none"),  # Without legend
  ncol = 1,
  rel_heights = c(1, 0.6),
  labels = c("(a)", "(b)"),
  label_size = 1.7*font.size,
  align = "v"
)

# Extract legend of doublebar plot for 
p.legend <- get_legend(p.cap.doublebar + theme(legend.box.margin = margin(0, 0, 0, 12)))

# Put together plot and legend
p <- plot_grid(p.plots,
               p.legend,
               ncol = 2,
               rel_widths = c(1, 0.45))

# Save as png
ggsave(filename = paste0(outputdir, "/DIETER/FIGURE04.png"),
       bg = "white",
       width = 22,  # Vary width according to how many panels plot has
       height = 12,  # Vary height according to how many panels plot has
       units = "cm")
# Save as svg
# ggsave(filename = paste0(outputdir, "/DIETER/FIGURE04.svg"),
#        bg = "white",
#        width = 18,  # Vary width according to how many panels plot has
#        height = 12,  # Vary height according to how many panels plot has
#        units = "cm")

# ============ 0-profit plots - system =============================================================================
ymax = max(DT.prices.lines$value) * 1.1
ymin = min(sys_avgLCOE_compare$value) * 1.1

p.sysLCOE_RM <- ggplot() + 
  geom_col( data = sys_avgLCOE_compare %>% filter(model=="REMIND"), 
            aes(period, value, fill=variable)) +
  geom_line(data = prices_RM.movingavg %>% filter(period %in% model.periods.till2100) ,
            aes(period, value, color=variable), alpha = 0.7, size=1.5) +  
  geom_line(data = prices_w2Shad_RM %>% filter(period %in% model.periods.till2100) ,
            aes(period, value, color=variable), alpha = 0.7, size=1.5)+
  scale_y_continuous("LCOE and electricity price\n(USD2015/MWh)") +
  scale_x_continuous(breaks = seq(2010,2100,10)) +
  scale_color_manual(name = "Price", values = price.colors.RM.fancy) +
  coord_cartesian(ylim = c(ymin,ymax))+
  scale_fill_manual(name = "Costs", values = cost.colors,guide = "none") +
  guides(fill="none", color=guide_legend(nrow=2,byrow=TRUE))+
  ggtitle("REMIND")+
  theme(legend.position="bottom", legend.direction="horizontal", legend.text = element_text(size=font.size)) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10,face="bold"), plot.title = element_text(size = 12, face = "bold")) 

elec_prices_DT_wShadPrice_laIter_forPaper <- elec_prices_DT_wShadPrice_laIter %>% 
  mutate(variable = "DIETER annual avg. electricity price + capacity shadow price")

elec_prices_DT_laIter_forPaper <- elec_prices_DT_laIter %>% 
  mutate(variable = "DIETER annual avg. electricity price")

DT.prices.lines_forPaper <- list(elec_prices_DT_wShadPrice_laIter_forPaper, elec_prices_DT_laIter_forPaper) %>%
  reduce(full_join)

p.sysLCOE_DT <- ggplot() + 
  geom_col( data = sys_avgLCOE_compare %>% filter(model=="DIETER"), 
            aes(period, value, fill=variable)) +
  geom_line(data = DT.prices.lines_forPaper %>% filter(period %in% model.periods.till2100) ,
            aes(period, value, color=variable), alpha = 0.7, size=1.5) +  
  scale_y_continuous("LCOE and electricity price\n(USD2015/MWh)") +
  scale_x_continuous(breaks = seq(2010,2100,10)) +
  scale_color_manual(name = "Price", values = price.colors.DT.fancy.forPaper) +
  coord_cartesian(ylim = c(ymin,ymax))+
  scale_fill_manual(name = "Costs",values = cost.colors) +
  guides(fill="none", color = guide_legend(nrow=2,byrow=TRUE)) +
  ggtitle("DIETER") +
  theme(legend.position="bottom", legend.direction="horizontal", legend.text = element_text(size=font.size)) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10,face="bold"), plot.title = element_text(size = 12, face = "bold")) 

p.dummy4legend <- ggplot() + 
  geom_col( data = sys_avgLCOE_compare %>% filter(model=="DIETER"), 
            aes(period, value, fill=variable)) +
  scale_fill_manual(name = "Costs",values = cost.colors)+
  guides(fill=guide_legend(nrow=1, byrow=TRUE))+
  theme(legend.position="bottom", legend.direction="horizontal", legend.text = element_text(size=7.2)) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10,face="bold"), plot.title = element_text(size = 12, face = "bold"))
  
# Arrange both plots
p.plots <- plot_grid(
  p.sysLCOE_RM + theme(legend.position = "bottom"),  # Without legend
  p.sysLCOE_DT + theme(legend.position = "bottom"),  # Without legend
  ncol = 2,
  rel_heights = c(1, 1),
  labels = c("(a)", "(b)"),
  label_size = 1.5*font.size,
  align = "v"
)

p.legend <- get_legend(p.dummy4legend + theme(legend.box.margin = margin(0, 0, 0, 47)))

# Put together plot and legend
p <- plot_grid(p.plots,
               p.legend,
               ncol = 1,
               rel_heights = c(1, 0.1),
               align = "v")

# Save as png
ggsave(filename = paste0(outputdir, "/DIETER/FIGURE_sysZPR.png"),
       bg = "white",
       width = 21.5,  # Vary width according to how many panels plot has
       height = 12.5,  # Vary height according to how many panels plot has
       units = "cm")

# ============ 0-profit plots - tech =============================================================================
## Arrange both plots
mv.plus.sp.agg_forPaper <- mv.plus.sp.agg %>% 
  mutate(cost = "Market value + peak demand capacity shadow price (&other)")

df.telcoe_mv.plot_forPaper <- list(
  mv.plus.sp.agg_forPaper, mv.agg) %>% 
  reduce(full_join) %>% 
  filter(!tech %in% c("VRE grid", "Electrolyzers for PtG")) %>% 
  filter(period %in% model.periods.till2100)

p.teLCOE.REMIND <- ggplot() + 
  geom_col( 
    data = df.lcoe.teAgg.wAdj %>% 
      filter(period %in% model.periods.till2100, period > 2020) %>% 
      filter(value < 1e4) %>% 
      mutate(tech = factor(tech, levels=rev(unique(dieter.tech.mapping)))),
    aes(period, value, fill=cost), stat='identity', size = 1.2, alpha = 0.65) +
  geom_line(data = df.telcoe_mv.plot_forPaper %>% 
              filter(period > 2020) %>% 
              filter(value < 1e4) %>% 
              select(period,tech,value,variable=cost) %>% 
              mutate(tech = factor(tech, levels=rev(unique(dieter.tech.mapping)))),
            aes(period, value, color=variable, linetype=variable), size=1.2) +
  geom_line(data=prices_RM.movingavg %>% 
              filter(period %in% model.periods.till2100)%>% 
              filter(period >2020),
               aes(period, value, color=variable, linetype=variable), alpha = 0.7, size=1.5) +
  scale_linetype_manual(name = "Price", values = linetype.map.techZPR.RM) +
  scale_color_manual(name = "Price", values = color.map.techZPR.RM) +
  theme_bw()+
  guides(linetype=guide_legend(nrow=1,byrow=TRUE)) +
  xlab("Period") + ylab(paste0("REMIND LCOE and prices (2015$/MWh)")) +
  scale_x_continuous(breaks = seq(2020,2100,20)) +
  coord_cartesian(xlim = c(2020,2100))+
  scale_fill_manual(values = cost.colors,guide = "none") +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20, face="bold"), strip.text = element_text(size = 20)) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), legend.text = element_text(size=20), plot.margin = unit(c(2,1,0,0), "cm")) +
  facet_wrap(~tech, nrow = 3, scales = "free") 

plot.dieter.price.wscar_forPaper <- plot.dieter.price.wscar %>%
  mutate(variable = "Market value + standing capacity shadow price (&other)")

plot.dieter.price_forPaper <- plot.dieter.price %>%
  filter(variable !="Shadow Price")%>% 
  mutate(variable = "Market value") 

plot.dieter.price.total_forPaper <- list(plot.dieter.price.wscar_forPaper, plot.dieter.price_forPaper) %>% 
  reduce(full_join) 

p.teLCOE_avg.DIETER <- ggplot() +
  # costs
  geom_bar(data = plot.dieter.telcoe_avg %>% 
             filter(period >2020) %>% 
             filter(!value ==0) %>% 
             mutate(tech = factor(tech, levels=rev(unique(dieter.tech.mapping)))), aes(x = period, y = value, fill = variable), stat='identity', size = 1.2, alpha = 0.65)+
  # MVs
  geom_line(data = plot.dieter.price.total_forPaper%>% 
              filter(period >2020)%>% 
              filter(!value ==0) %>% 
              mutate(tech = factor(tech, levels=rev(unique(dieter.tech.mapping)))), aes(period, value, color=variable, linetype=variable), size=1.2) +
  # elec prices
  geom_line(data = elec_prices_DT_laIter_forPaper %>% 
              filter(period %in% model.periods.till2100) %>% 
              filter(period >2020),
                          aes(period, value, color=variable, linetype=variable), alpha = 0.7, size=1.5) +
  scale_linetype_manual(name = "Price", values = linetype.map.techZPR.DT) +
  scale_color_manual(name = "Price", values = color.map.techZPR.DT) +
  theme_bw()+
  guides(linetype=guide_legend(nrow=1,byrow=TRUE)) +
  xlab("Period") + ylab(paste0("DIETER LCOE and prices (2015$/MWh)")) +
  scale_x_continuous(breaks = seq(2020,2100,20)) +
  coord_cartesian(xlim = c(2020,2100))+
  scale_fill_manual(values = cost.colors_DT.line,guide = "none") +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20, face="bold"), strip.text = element_text(size = 20)) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(size=20), plot.margin = unit(c(2,1,0,0), "cm")) +
  facet_wrap(~tech, nrow = 3, scales = "free") 

p.dummy4legend <- ggplot() + 
  geom_col( data = df.lcoe.teAgg.wAdj, 
            aes(period, value, fill=cost)) +
  scale_fill_manual(name = "Costs",values = cost.colors_DT.line)+
  guides(fill=guide_legend(nrow=1, byrow=TRUE)) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(size=18))

p.legend <- get_legend(p.dummy4legend + theme(legend.box.margin = margin(0, 0, 0, 50)))

# Put together plot and legend
p <- plot_grid(p.teLCOE.REMIND,
               p.teLCOE_avg.DIETER,
               p.legend,
               ncol = 1,
               rel_heights = c(1, 1, 0.08),
               labels = c("(a) REMIND", "(b) DIETER"),
               label_size = 3*font.size,
               align = "v",
               label_x = 0, label_y = 0.98
               )

# Save as png
ggsave(filename = paste0(outputdir, "/DIETER/FIGURE_techZPR.png"),
       bg = "white",
       width = 37,  # Vary width according to how many panels plot has
       height = 42,  # Vary height according to how many panels plot has
       units = "cm")


# ============SCENARIO plots =============================================================================
# Figure: Long-term development ------------------------------------------

p.cap1 <- ggplot() +
  geom_area(data = plot.remind.capacity.wDIETERstorage %>% 
              filter(period %in% model.periods.from2020.till2100) %>% 
              filter(period < 2065)
            , aes(x = period, y = value, fill = tech), size = 1.2, alpha = 0.7) +
  scale_fill_manual(name = "Technology", values = color.mapping.cap.paper) +
  theme(legend.position="right", legend.direction="vertical", legend.title = element_blank(),legend.text = element_text(size=12))+
  theme_minimal_grid(12) +
  xlab("Time") + ylab("Capacity (GW)")+
  theme(axis.text=element_text(size=12), axis.title=element_text(size= 12, face="bold"))

# if (outputdir == "./output/hydro1186"){
#   # 2, sector coupling in GWa
#   remind.total.secCoup <- as.numeric(h2consum$h2dem)
#   
#   # 3, storing out in GWa
#   # 3a, storage loss
#   # for 1186 :
#   remind.total.storloss.ratio <- file.path(outputdir, remind.files[maxiter]) %>%
#     read.gdx("p32_storLoss", factor = FALSE) %>%
#     filter(all_regi == reg) %>%
#     select(period=ttot, value) %>%
#     dplyr::group_by(period) %>%
#     dplyr::summarise( value = sum(value), .groups = "keep") %>%
#     dplyr::ungroup(period) %>% 
#     mutate(value = value * 1e3)
#   
#   #(full_DIETER.gdx storloss_ratio)
#   
#   dieter.total.storloss.ratio <- file.path(outputdir, "full_DIETER.gdx") %>%
#     read.gdx("storloss_ratio", factor = FALSE) %>%
#     filter(all_regi == reg) %>%
#     select(period=ttot, value) %>%
#     dplyr::group_by(period) %>%
#     dplyr::summarise( value = sum(value), .groups = "keep") %>%
#     dplyr::ungroup(period) %>% 
#     mutate(value = value * 1e3/avg.remind.tot.demand$value)
#   
#   dieter.total.storloss.ratio <- 0.0771693
#   
#   # stor_in_h2 / stor_in_batt = 0.347
#   # stor_out_h2 / stor_out_batt = 0.161
#   # TSL_RM2DT <- 1.164
#   TSL_RM2DT <- remind.total.storloss.ratio$value/dieter.total.storloss.ratio
#   H2_batt_ratio_storout <- 0.161
#   
#   # infer storage out in REMIND from this ratio
#   remind.total.storout <- (209451+33841.5)/8760 * TSL_RM2DT
#   remind.hydrogen.storout <- H2_batt_ratio_storout * remind.total.storout
#   remind.batt.storout <- remind.total.storout-H2_batt_ratio_storout * remind.total.storout
#   
#   plot.remind.generation <- plot.remind.generation %>% 
#   
#   
# }
  
# if (outputdir == "./output/hydro1186"){
#  
# }

p.genwConsump1 <- ggplot() +
  geom_area(
    data = plot.remind.generation %>% filter(period <2065),
    aes(x = period, y = value, fill = tech),
    size = 1.2,
    alpha = 0.7,
    stat = "identity"
  ) +
  geom_area(
    data = plot.remind.generation.withCurt%>% filter(period <2065),
    aes(x = period, y = value, color = tech),
    size = 1,
    alpha = 0,
    linetype = "dotted"
  ) +
  theme_minimal_grid(12) +
  scale_fill_manual(name = "Technology", values = color.mapping.paper) +
  scale_color_manual(name = "Technology", values = color.mapping_vre) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold")) +
  xlab("Time") + ylab("Generation (TWh)") +
  theme(legend.position="none")

# Arrange both plots
p <- plot_grid(p.genwConsump1,
               p.cap1,
               ncol = 2,
               rel_widths = c(1, 1.53),
               labels = c("(a)", "(b)"),
               align = "h") 
# Save as png
ggsave(filename = paste0(outputdir, "/DIETER/FIGURE_LONG_GENCAP.png"),
       bg = "white",
       width = 35,  # Vary width according to how many panels plot has
       height = 12,  # Vary height according to how many panels plot has
       units = "cm")

# ============SCENARIO plots =============================================================================
# Figure ?: RLDC ------------------------------------------
year_toplot = 2045
######################################################################################
#### baseline (hydro1098, it uses older reporting variables)#########################
######################################################################################
# make a triangle out of total curtailment
# curtailment in GWa
if (outputdir == "./output/hydro1098"){
remind.total.curt <- file.path(outputdir, remind.files[maxiter]) %>%
  read.gdx("p32_seelCurt", factor = FALSE) %>%
  filter(all_regi == reg, ttot == year_toplot) %>%
  select(value) %>%
  mutate(value = value * 1e3)

curt.value <- remind.total.curt$value
largest.cf <- max(plot.remind.cf$cf)/1e2 + 0.07 #add 7% to largest CF for aesthetic reasons

# take the GWa curtailment value, times 8760 hours, divided by (width/2), where width is the difference
# between 8760 and the highest full load hour plant
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

p.RM.rldc <-ggplot() +
  geom_area(data = RLDC.VRE, aes(x = hour, y = value, fill = tech), size = 1.2, alpha = 1, position = "identity") +
  geom_area(data = plot.rldc.hr, aes(x = hour, y = value, fill = tech), size = 1.2, alpha = 1) +
  geom_area(data = RLDC.VREcurt, aes(x = hour, y = value, fill = tech), size = 1.2, alpha = 1, position = "identity") +
  coord_cartesian(ylim = c(min(CU_VRE_Solar.plot$Solar.RLDC2) * 1.1,max(LDC0$load) * 1.1 )) +
  scale_fill_manual(name = "Technology", values = color.mapping.RLDC.basic)+
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size= 10, face="bold"))+
  xlab("Sorted hours of a year") + ylab("Residual load (GW)")+
  ggtitle(paste0("REMIND ", year_toplot))

# Extract legend 
p.legend <- get_legend(p.DT.rldc + theme(legend.box.margin = margin(0, 0, 0, 12)))

# Arrange both plots
p.plots <- plot_grid(
               p.RM.rldc,
               p.DT.rldc + theme(legend.position = "none"),
               ncol = 2,
               rel_widths = c(1, 1),
               labels = c("(a)", "(b)"),
               label_size = 1.2*font.size,
               align = "h")

# Put together plot and legend
p <- plot_grid(p.plots,
               p.legend,
               ncol = 2,
               rel_widths = c(1, 0.33))

# Save as png
ggsave(filename = paste0(outputdir, "/DIETER/FIGURE_RLDC.png"),
       bg = "white",
       width = 20,  # Vary width according to how many panels plot has
       height = 12,  # Vary height according to how many panels plot has
       units = "cm")
}
######################################################################################
#### baseline (hydro1186, it uses older reporting variables)#########################
######################################################################################
if (outputdir == "./output/hydro1186"){
# make a triangle out of total curtailment + sector coupling demand + storage in
# 1, curtailment in GWa
remind.total.curt <- file.path(outputdir, remind.files[maxiter]) %>%
  read.gdx("p32_curtLoss", factor = FALSE) %>%
  filter(all_regi == reg, ttot == year_toplot) %>%
  select(value) %>%
  dplyr::summarise( value = sum(value)) %>%
  mutate(value = value * 1e3)
curt.value <- remind.total.curt$value

# 2, sector coupling in GWa
remind.total.secCoup <- as.numeric(h2consum$h2dem)

# 3, storing in in GWa
# 3a, storage loss
# for 1186 :
remind.total.storloss.ratio <- file.path(outputdir, remind.files[maxiter]) %>%
  read.gdx("p32_storLoss", factor = FALSE) %>%
  filter(all_regi == reg, ttot == year_toplot) %>%
  select(value) %>%
  dplyr::summarise( value = sum(value)) %>%
  mutate(value = value * 1e3/avg.remind.tot.demand$value)
#(full_DIETER.gdx storloss_ratio)
dieter.total.storloss.ratio <- 0.0771693

# stor_in_h2 / stor_in_batt = 0.347
# stor_out_h2 / stor_out_batt = 0.161
# TSL_RM2DT <- 1.164
TSL_RM2DT <- remind.total.storloss.ratio$value/dieter.total.storloss.ratio
H2_batt_ratio_storin <- 0.347
H2_batt_ratio_storout <- 0.161

# infer storage in and out in REMIND from this ratio
remind.total.storin <- (243547+84603.7)/8760 * TSL_RM2DT
remind.hydrogen.storin <- H2_batt_ratio_storin * remind.total.storin
remind.batt.storin <- remind.total.storin- H2_batt_ratio_storin * remind.total.storin
remind.total.storout <- (209451+33841.5)/8760 * TSL_RM2DT
remind.hydrogen.storout <- H2_batt_ratio_storout * remind.total.storout
remind.batt.storout <- remind.total.storout-H2_batt_ratio_storout * remind.total.storout

# total negative triangle
remind.total.negative.num <- curt.value + remind.total.secCoup + remind.total.storin

remind.total.negative <- data.frame(
  value = remind.total.negative.num
)

largest.cf <- max(plot.remind.cf$cf)/1e2 + 0.25 #add 25% to largest CF for aesthetic reasons

total.peak = 2 * 8760 * (remind.total.negative.num)/(8760 * (1-largest.cf))

delta_value3 = total.peak/(876*(1-largest.cf))

total.neg.rldc <- remind.total.negative %>%
  expand(remind.total.negative, hour = seq(8760*largest.cf,8760,10)) %>%
  mutate(value = - seq(0,total.peak,delta_value3)) %>%
  mutate(tech="Lithium-ion battery") %>%
  select(hour,tech,value)

#-----------------------------------
#plot triangle
# take the GWa total negative value, subtract the battery stor in
# peak.batt = 2 * 8760 * (remind.total.negative.num-remind.batt.storin)/(8760 * (1-largest.cf))
# 
# delta_value4 = peak.batt/(876 * (1-largest.cf))
# 
# remind.total.negative <- data.frame(
#   value = remind.total.negative.num
# )
# 
# batt.storin.rldc <- remind.total.negative %>%
#   expand(remind.total.negative, hour = seq(8760 * largest.cf, 8760, 10)) %>%
#   mutate(value = - seq(0, peak.batt, delta_value4)) %>%
#   mutate(tech="Electrolyzers for long-term storage") %>%
#   select(hour,tech,value)

#plot rectangle
# batt = remind.total.negative.num-remind.batt.storin
batt = remind.hydrogen.storin+1/3*curt.value
cap.batt = 36.6214 #(battery + Ptg Electrolyzers)
cap.elh2 = 63935.2/1e3
cap.total = cap.batt + cap.elh2

batt.storin.rldc <- remind.total.negative %>%
  expand(remind.total.negative, hour = seq(8760* (1-batt/cap.batt), 8760, 10)) %>%
  mutate(value = -cap.total) %>%
  mutate(tech="Electrolyzers for long-term storage") %>%
  select(hour,tech,value)

#-----------------------------------
#plot triangle
# take the GWa total negative value, subtract the battery stor in and h2 stor in
# peak.h2 = 2 * 8760 * (remind.total.negative.num-remind.batt.storin-remind.hydrogen.storin)/(8760 * (1-largest.cf))
# 
# delta_value5 = peak.h2/(876*(1-largest.cf))
# 
# remind.h2.storin.df <- data.frame(
#   value = remind.total.negative.num
# )
# 
# h2.storin.rldc <- remind.h2.storin.df %>%
#   expand(remind.h2.storin.df, hour = seq(8760*largest.cf,8760,10)) %>%
#   mutate(value = - seq(0,peak.h2,delta_value5)) %>%
#   mutate(tech = "Electrolyzers for PtG") %>%
#   select(hour,tech,value)

#plot rectangle
elh2 = remind.total.secCoup+1/2*curt.value
cap.elh2 = 63935.2/1e3 #(Ptg Electrolyzers)

h2.storin.rldc <- remind.total.negative %>%
  expand(remind.total.negative, hour = seq(8760 * (1-elh2/cap.elh2), 8760, 10)) %>%
  mutate(value = -cap.elh2) %>%
  mutate(tech="Electrolyzers for PtG") %>%
  select(hour,tech,value)
#-----------------------------------
# take the GWa total negative value, subtract the battery stor in and h2 stor in and PtG electrolyzers
largest.cf = 1- 0.14
peak.PtG_H2 = 2 * 8760 * (remind.total.negative.num-remind.batt.storin-remind.hydrogen.storin-remind.total.secCoup)/(8760 * (1-largest.cf))

delta_value6 = peak.PtG_H2/(876*(1-largest.cf))

remind.secCoup.df <- data.frame(
  value = remind.total.negative.num
)

secCoup.rldc <- remind.secCoup.df %>%
  expand(remind.secCoup.df, hour = seq(8760*largest.cf,8760,10)) %>%
  mutate(value = - seq(0,peak.PtG_H2,delta_value6)) %>%
  mutate(tech = "Solar") %>%
  select(hour,tech,value)
#-----------------------------------
# wind curtailment
largest.cf = 1- 0.01
remind.wind.curt <- file.path(outputdir, remind.files[maxiter]) %>%
  read.gdx("p32_curtLoss", factor = FALSE) %>%
  filter(all_regi == reg, all_te%in%c("wind","windoff"), ttot == year_toplot) %>%
  select(value) %>%
  dplyr::summarise( value = sum(value)) %>%
  mutate(value = value * 1e3)

wind.curt.value <- remind.wind.curt$value

delta_value7 = wind.curt.value/(876*(1-largest.cf))

remind.wind.curt.rldc <- remind.wind.curt %>%
  expand(remind.wind.curt, hour = seq(8760*largest.cf,8760,10)) %>%
  mutate(value = - seq(0, wind.curt.value,delta_value7)) %>%
  mutate(tech="Wind") %>%
  select(hour,tech,value)

RLDC.neg <- list(remind.wind.curt.rldc, secCoup.rldc, h2.storin.rldc,batt.storin.rldc, total.neg.rldc) %>% 
  reduce(full_join) %>% 
  mutate(tech = factor(tech, levels=rev(c("Wind","Solar","Electrolyzers for PtG","Electrolyzers for long-term storage", "Lithium-ion battery"))))

########## positive storage (storage discharge)
# total positive (storage) triangle
remind.total.storout <- (209451+33841.5)/8760 * TSL_RM2DT

remind.total.storout.df <- data.frame(
  value = remind.total.storout
)

largest.cf <- max(plot.remind.cf$cf)/1e2 + 0.26 # add 26% to largest CF for aesthetic reasons
total.storout.peak = 2 * remind.total.storout/largest.cf

delta_value8 = total.storout.peak/(876*largest.cf)

total.pos.rldc <- remind.total.storout.df %>%
  expand(remind.total.storout.df, hour = seq(0,8760*largest.cf,10)) %>%
  mutate(value = seq(total.storout.peak,0,-delta_value8)) %>%
  mutate(tech="Lithium-ion battery") %>% 
  select(hour,tech,value)

RLDC.pos <- list(total.pos.rldc) %>% 
  reduce(full_join) %>% 
  mutate(tech = factor(tech, levels=rev(c("Electrolyzers for long-term storage", "Lithium-ion battery"))))
# --------------------------------------
remind.hydrogen.storout <- H2_batt_ratio_storout * remind.total.storout + (33.8*223 + 14.9*56)/8760

remind.h2.storout.df <- data.frame(
  value = remind.hydrogen.storout
)

h2.cf.dieter = 10.549/100

h2.value = remind.hydrogen.storout/h2.cf.dieter+0.055+9.2+12.6 #(stacked on top of hydro, biomass plants)

total.h2.rldc <- remind.h2.storout.df %>%
  expand(remind.h2.storout.df, hour = seq(0,8760*h2.cf.dieter,10)) %>%
  mutate(value =h2.value) %>%
  mutate(tech="Hydrogen turbine") %>%
  select(hour,tech,value)

RLDC.pos <- list(total.h2.rldc, total.pos.rldc) %>% 
  reduce(full_join) %>% 
  mutate(tech = factor(tech, levels=rev(c("Hydrogen turbine", "Lithium-ion battery"))))


p.RM.rldc <-ggplot() +
  geom_area(data = RLDC.VRE, aes(x = hour, y = value, fill = tech), size = 1.2, alpha = 1, position = "identity") +
  geom_area(data = RLDC.pos, aes(x = hour, y = value, fill = tech), size = 1.2, alpha = 1, position = "identity") +
  geom_area(data = plot.rldc.hr, aes(x = hour, y = value, fill = tech), size = 1.2, alpha = 1) +
  geom_area(data = RLDC.neg, aes(x = hour, y = value, fill = tech), size = 1.2, alpha = 1, position = "identity") +
  coord_cartesian(ylim = c(min(CU_VRE_Solar.plot$Solar.RLDC2) * 1.1,max(LDC0$load) * 1.1 )) +
  scale_fill_manual(name = "Technology", values = color.mapping.RLDC.fancy) +
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size= 10, face="bold")) +
  xlab("Sorted hours of a year") + ylab("Residual load (GW)") +
  ggtitle(paste0("REMIND ", year_toplot))

# Extract legend 
p.legend <- get_legend(p.DT.rldc + theme(legend.box.margin = margin(0, 0, 0, 12)))

# Arrange both plots
p.plots <- plot_grid(
  p.RM.rldc,
  p.DT.rldc +
    theme(legend.position = "none"),
  ncol = 2,
  rel_widths = c(1, 1),
  labels = c("(a)", "(b)"),
  label_size = 1.2*font.size,
  align = "h")

# Put together plot and legend
p <- plot_grid(p.plots,
               p.legend,
               ncol = 2,
               rel_widths = c(1, 0.33))

# Save as png
ggsave(filename = paste0(outputdir, "/DIETER/FIGURE_RLDC.png"),
       bg = "white",
       width = 20,  # Vary width according to how many panels plot has
       height = 12,  # Vary height according to how many panels plot has
       units = "cm")
}
##########################################################
#plot hourly price duration curve
setwd("/home/chengong/remind-coupling-dieter/")
base_nostor_outputdir = "./output/hydro1098"
policy_outputdir = "./output/hydro1186"
# scen_outputdir_lst <- c(base_nostor_outputdir, policy_outputdir)

dieter.runningcost.variables.PDC = c("fuel cost - divided by eta ($/MWh)","CO2 cost ($/MWh)","O&M var cost ($/MWh)") 

dieter.capture.price.variables.PDC <- c("DIETER Market value ($/MWh)")

dieter.dispatch.tech.PDC = c("CCGT", "coal","bio", "OCGT_eff", "nuc")
dieter.demand.tech.PDC <- c("elh2")

year_toplot_list <- c(seq(2025, 2045, 5))

for(year_toplot in year_toplot_list){
  
# year_toplot = 2030

get_PDC <- function(outputdir){
  
  dieter.files.report <- list.files(outputdir, pattern = "report_DIETER_i[0-9]+\\.gdx") %>%
    str_sort(numeric = TRUE)
  
  # Data preparation --------------------------------------------------------
  price_hr <- file.path(outputdir, dieter.files.report[length(dieter.files.report)]) %>%
    read.gdx("report_hours", factors = FALSE, squeeze = FALSE) %>% 
    select(filename = X., period = X..2, variable = X..4, hour = X..5, value) %>%
    mutate(hour = as.numeric(str_extract(hour, "[0-9]+"))) %>% 
    filter(variable == "hourly wholesale price ($/MWh)") %>% 
    filter(period == year_toplot) %>%
    select(period, variable, value,hour) %>% 
    mutate(period = as.numeric(period)) 
  
  price_Hr_plot <- price_hr %>% arrange(desc(value))
  price_Hr_plot$sorted_x <- seq(1, 8760)
  
  return(price_Hr_plot)
}

get_runningcost <- function(outputdir){
  
  load(file.path(outputdir, "config.Rdata"))
  h2switch <- cfg$gms$cm_DT_elh2_coup
  
  dieter.files.report <- list.files(outputdir, pattern = "report_DIETER_i[0-9]+\\.gdx") %>%
    str_sort(numeric = TRUE)
  
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
  # max_price <- max(price_Hr_plot$value)
  
  if (h2switch == "on"){
    cost.plot <- list(expanded_running_cost, expanded_capture_price) %>%
      reduce(full_join)
  }
  
  if (h2switch == "off"){
    cost.plot <- list(expanded_running_cost) %>%
      reduce(full_join)
  }
  
  return(cost.plot)
}

base_price_Hr_plot <- lapply(base_nostor_outputdir, get_PDC)
pol_price_Hr_plot <- lapply(policy_outputdir, get_PDC)

base_price_Hr_plot <- as.data.frame(base_price_Hr_plot)
pol_price_Hr_plot <- as.data.frame(pol_price_Hr_plot)

base_cost.plot <- lapply(base_nostor_outputdir, get_runningcost)
pol_cost.plot <- lapply(policy_outputdir, get_runningcost)

base_cost.plot <- as.data.frame(base_cost.plot)
pol_cost.plot <- as.data.frame(pol_cost.plot)

color.mapping.PDC <- c("CCGT" = "#999959", 
                       "Coal" = "#0c0c0c",
                       "Biomass" = "#005900",
                       "OCGT" = "#e51900",
                       "Nuclear" = "#ff33ff",
                       NULL)

color.mapping.PDC <- c(color.mapping.PDC,
                         "Electrolyzers for PtG" = "#48D1CC")

  p.PDC.base <- ggplot() +
    geom_line(data = base_price_Hr_plot, aes(x = sorted_x, y = value ), size = 1.2, alpha = 1, color = "blue") +
    geom_line(data = base_cost.plot, aes(x = hour, y = value, color = tech ), size = 0.8, alpha = 0.8) +
    coord_cartesian(expand = FALSE, ylim = c(0.1, 400)) +
    scale_color_manual(name = "Running costs ($/MWh)", values = color.mapping.PDC) +
    theme(axis.text = element_text(size=10), axis.title = element_text(size= 10, face="bold")) +
    ggtitle(paste0("Baseline ", year_toplot))+
    theme(legend.position = "none")+
    # theme(legend.position="bottom", legend.direction="horizontal")+
    xlab("Hour") + ylab("Sorted hourly electricity price ($/MWh)")

  p.PDC.policy <- ggplot() +
    geom_line(data = pol_price_Hr_plot, aes(x = sorted_x, y = value ), size = 1.2, alpha = 1, color = "blue") +
    geom_line(data = pol_cost.plot, aes(x = hour, y = value, color = tech ), size = 0.8, alpha = 0.8) +
    coord_cartesian(expand = FALSE, ylim = c(0.1, 400)) +
    scale_color_manual(name = "Running costs ($/MWh)", values = color.mapping.PDC) +
    theme(axis.text = element_text(size=10), axis.title = element_text(size= 10, face="bold")) +
    ggtitle(paste0("Net-zero policy ", year_toplot))+
    theme(legend.position="right", legend.direction="vertical")+
    xlab("Hour") + ylab("Sorted hourly electricity price ($/MWh)")
  
# Arrange both plots
p <- plot_grid(
  p.PDC.base,
  p.PDC.policy,
  ncol = 2,
  rel_widths = c(1, 1.5),
  labels = c("(a)", "(b)"),
  label_size = 1.4*font.size,
  align = "h")

# Save as png
ggsave(filename = paste0(outputdir, "/DIETER/FIGURE_PDC",year_toplot,".png"),
       bg = "white",
       width = 20,  # Vary width according to how many panels plot has
       height = 12,  # Vary height according to how many panels plot has
       units = "cm")
}
# -------------------------------------------------------------------------
# Figure: hourly dispatch
# plot hourly price duration curve
setwd("/home/chengong/remind-coupling-dieter/")
base_nostor_outputdir = "./output/hydro1098"
policy_outputdir = "./output/hydro1186"
year_toplot = 2045

get_hrly <- function(outputdir,season){
  
dieter.files.report <- list.files(outputdir, pattern = "report_DIETER_i[0-9]+\\.gdx") %>%
    str_sort(numeric = TRUE)

# season = "winter"
# outputdir = policy_outputdir

hr_data <-  file.path(outputdir, dieter.files.report[length(dieter.files.report)]) %>%
  read.gdx("report_tech_hours", factors = FALSE, squeeze = FALSE) %>% 
  select(filename = X., period = X..2, variable = X..4, tech = X..5, hour = X..6, value) %>% 
  revalue.levels(tech = dieter.tech.mapping) %>% 
  mutate(hour = as.numeric(str_extract(hour, "[0-9]+"))) %>% 
  mutate(tech = factor(tech, levels = rev(unique(dieter.tech.mapping)))) %>% 
  filter(period == year_toplot) %>% 
  filter(variable %in% dieter.variables.hrly.mix) %>% 
  select(period, tech, variable, value,hour) %>% 
  mutate(period = as.numeric(period)) 

if (season == "summer"){
  hr_data <- hr_data %>% 
    filter((hour > 5500) & (hour < 5670) )
  # filter((hour > 4500) & (hour < 6000) )
}
if (season == "winter"){
  hr_data <- hr_data %>% 
    filter((hour > 480) & (hour < 650) )
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

storage_out_h2 <- storage_out %>% 
  filter(tech == "Electrolyzers for long-term storage") %>% 
  mutate(tech = "Hydrogen turbine")

storage_out_nonh2 <- storage_out %>% 
  filter(tech != "Electrolyzers for long-term storage")

storage_in <- hr_data %>% 
  filter(variable == "storage loading (GWh)") %>% 
  select(tech, hour, value) %>% 
  mutate(value = -value)

curtailment <- hr_data %>% 
  filter(variable == "curtailment renewable (GWh)") %>% 
  select(tech, hour, value) %>% 
  revalue.levels(tech = dieter.tech.curt.mapping)

hr.data.plot = list(generation, consumption, storage_out_h2, storage_out_nonh2, storage_in, curtailment) %>%
  reduce(full_join)%>% 
  order.levels(tech = names(color.mapping.hrly.mix)) 

return(hr.data.plot)
}

plot_scale = 800

base_summer_plot <- lapply(base_nostor_outputdir, get_hrly, season = "summer")
base_winter_plot <- lapply(base_nostor_outputdir, get_hrly, season = "winter")
pol_summer_plot <- lapply(policy_outputdir, get_hrly, season = "summer")
pol_winter_plot <- lapply(policy_outputdir, get_hrly, season = "winter")

base_summer_plot <- as.data.frame(base_summer_plot)
base_winter_plot <- as.data.frame(base_winter_plot)
pol_summer_plot <- as.data.frame(pol_summer_plot)
pol_winter_plot <- as.data.frame(pol_winter_plot)

p.summer.hrly1 <- ggplot() +
  geom_col(base_summer_plot , mapping = aes(x=hour, y=value, fill=tech), alpha = 0.7, width = 0.9)  +
  scale_fill_manual(name = "Technology", values = color.mapping.hrly.mix) +
  labs(y = "Consumption /\ Generation (GWh)",  x = "Hour of the year", colour = "") +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(size=7), plot.margin = unit(c(1,0,0,0), "cm"))+
  guides(fill=guide_legend(nrow=6, byrow=TRUE)) 

p.winter.hrly1 <- ggplot() +
  geom_col(base_winter_plot , mapping = aes(x=hour, y=value, fill=tech), alpha = 0.7, width = 0.9)  +
  scale_fill_manual(name = "Technology", values = color.mapping.hrly.mix) +
  labs(y = "Consumption /\ Generation (GWh)",  x = "Hour of the year", colour = "") +
  theme(plot.margin = unit(c(1,0,0,0), "cm"))

p.summer.hrly2 <- ggplot() +
  geom_col(pol_summer_plot , mapping = aes(x=hour, y=value, fill=tech), alpha = 0.7, width = 0.9)  +
  scale_fill_manual(name = "Technology", values = color.mapping.hrly.mix) +
  labs(y = "Consumption /\ Generation (GWh)",  x = "Hour of the year", colour = "") +
  theme(plot.margin = unit(c(1,0,0,0), "cm"))


p.winter.hrly2 <- ggplot() +
  geom_col(pol_winter_plot , mapping = aes(x=hour, y=value, fill=tech), alpha = 0.7, width = 0.9)  +
  scale_fill_manual(name = "Technology", values = color.mapping.hrly.mix) +
  labs(y = "Consumption /\ Generation (GWh)",  x = "Hour of the year", colour = "") +
  theme(plot.margin = unit(c(1,0,0,0), "cm"))

p.legend <- get_legend(p.summer.hrly1 + theme(legend.box.margin = margin(0, 0, 0, 2)))

# Arrange both plots
p.plots <- plot_grid(
  p.summer.hrly1 + theme(legend.position = "none"),  # Without legend,
  p.winter.hrly1 + theme(legend.position = "none"),  # Without legend,
  p.summer.hrly2 + theme(legend.position = "none"),  # Without legend,
  p.winter.hrly2 + theme(legend.position = "none"),  # Without legend,
  ncol = 1,
  nrow = 4,
  labels = c("(a) Summer (baseline w/o storage or demand flexibility)", "(b) Winter (baseline w/o storage or demand flexibility)","(c) Summer (net-zero w/ storage and demand flexibility)", "(d) Winter (net-zero w/ storage and demand flexibility)"),
  label_size = 1*font.size,
  label_x = -0.2, label_y = 0.98,
  align = "h",
  rel_heights = c(1,1,1.5,1.5))

# Put together plot and legend
p <- plot_grid(p.plots,
               p.legend,
               nrow = 2,
               rel_heights = c(1, 0.2))

# Save as png
ggsave(filename = paste0(policy_outputdir, "/DIETER/FIGURE_Hrly2.png"),
       bg = "white",
       width = 12,  # Vary width according to how many panels plot has
       height = 22,  # Vary height according to how many panels plot has
       units = "cm")

# Figure ?: 3-panel coupled vs. uncoupled comparison  ------------------------------------------
# baseline run comparison 
setwd("/home/chengong/remind-coupling-dieter/")
coupledStor_outputdir = "./output/hydro1241"
uncoupStor_outputdir = "./output/hydro1265"
coupledNoStor_outputdir = "./output/hydro1240"
uncoupNoStor_outputdir = "./output/hydro1267"

baseline_outputdir_lst <- c(coupledStor_outputdir, uncoupStor_outputdir, coupledNoStor_outputdir, uncoupNoStor_outputdir)

# policy run comparison - 2C
coupledStor_outputdir2 = "./output/hydro1251"
uncoupStor_outputdir2 = "./output/hydro1266"
coupledNoStor_outputdir2 = "./output/hydro1247"
uncoupNoStor_outputdir2 = "./output/hydro1263" #("ngt" has to early retire, otherwise infes)

policy_outputdir_lst <- c(coupledStor_outputdir2, uncoupStor_outputdir2)

# policy_outputdir_lst <- c(coupledStor_outputdir2, uncoupStor_outputdir2, coupledNoStor_outputdir2, uncoupNoStor_outputdir2)

# run_name_lst <- c("Coupled with storage", "Uncoupled with storage parametrization", "Coupled without storage", "Uncoupled without storage parametrization")
# x_positions <- c(-2.24, -1.12,0,1.12)

run_name_lst <- c("Coupled with storage", "Uncoupled with storage parametrization")

x_positions <- c(-1,1)

# outputdir_lst = baseline_outputdir_lst
# fig_title = "Baseline"
outputdir_lst = policy_outputdir_lst
fig_title = "Policy"

font.size = 8

df.capacity <- NULL
for (i in c(1:length(outputdir_lst))){
  
  outputdir = outputdir_lst[[i]]
  print(outputdir)
  # outputdir = "./output/hydro1217"
  run_name = run_name_lst[[i]]
  
remind.files <- list.files(outputdir, pattern = "fulldata.gdx")

remind.capacity <- file.path(outputdir, remind.files[length(remind.files)]) %>%  
    read.gdx("vm_cap", factors = FALSE, squeeze = FALSE) %>% 
    filter(tall %in% model.periods) %>%
    filter(all_regi == reg) %>%
    filter(rlf == "1") %>% 
    filter(all_te %in% names(remind.tech.mapping.narrow.wh2turb)) %>%
    mutate(value = value * 1e3) %>% #TW->GW
    select(period = tall, tech = all_te, rlf, value) %>% 
    revalue.levels(tech = remind.tech.mapping.narrow.wh2turb) %>%
    dplyr::group_by(period, tech, rlf) %>%
    dplyr::summarise( value = sum(value) , .groups = 'keep' ) %>% 
    dplyr::ungroup(period, tech, rlf) %>% 
    mutate(tech = factor(tech, levels=rev(unique(remind.tech.mapping.narrow.wh2turb))))%>% 
    filter(period %in% model.periods.till2100) %>% 
    mutate(period = as.numeric(as.character(period)) + x_positions[[i]]) %>% 
    mutate(runname = run_name)
  
df.capacity <- rbind(df.capacity, remind.capacity)
}

df.capacity <- df.capacity%>% 
  mutate(runname = factor(runname, levels=rev(unique(run_name_lst)))) %>%
  mutate(tech = factor(tech, levels = rev(unique(remind.tech.mapping.narrow.wh2turb)))) 

df.capacity.sum.box <- df.capacity %>% 
  dplyr::group_by(period, rlf,runname) %>%
  dplyr::summarise( value = sum(value) , .groups = 'keep' ) %>% 
  dplyr::ungroup(period, rlf,runname)
  
outputdir = coupledStor_outputdir2
  print(outputdir)
  # outputdir = "./output/hydro1217"
  run_name = run_name_lst[[1]]
  
  remind.files <- list.files(outputdir, pattern = "fulldata.gdx")

  remind.peak.demand <- file.path(outputdir, remind.files[length(remind.files)]) %>%  
    read.gdx("p32_peakDemand", factor = FALSE) %>% 
    filter(ttot %in% model.periods.till2100) %>% 
    filter(all_regi == reg) %>%
    select(period=ttot,value) %>% 
    mutate(value = value * 1e3) 

p.cap.compare <-ggplot() +
    geom_bar(data = df.capacity.sum.box, aes(x=period, y=value, linetype=runname), stat="identity",colour = "black", width=2, linewidth=0.2, fill = "transparent") +
  geom_bar(data = df.capacity, aes(x=period, y=value, fill=tech, linetype=runname), stat="identity", position="stack", width=2, linewidth=0.2, alpha=0.7) +
    geom_point(data = remind.peak.demand, aes(x=period-1, y=value), shape = 17,size=5,colour = "black") + 
    scale_fill_manual(name = "Technology", values = color.mapping.cap.wh2turb) +
    xlab("Time") + ylab(paste0("Capacity (GW)")) +
    theme(legend.title = element_blank()) +
    scale_linetype_discrete(breaks=run_name_lst) +
    # theme(legend.position="bottom", legend.direction="horizontal", legend.text = element_text(size=8)) +
    # guides(fill=guide_legend(nrow=4,byrow=TRUE), linetype=guide_legend(nrow=4,byrow=TRUE))+
    theme(axis.text = element_text(size=14), axis.title = element_text(size= 14, face="bold")) +
    theme(legend.position = "none")
  
  # swfigure(sw,print,p)
  # if (save_png == 1){
  #   ggsave(filename = paste0(outputdir_lst[[1]], "/DIETER/Figure_uncoup_", fig_title, "_CAP.png"), p.cap.compare, width = 7, height = 4.5, units = "in", dpi = 120)
  # }

  df.generation <- NULL
  df.loss <- NULL
  for (i in c(1:length(outputdir_lst))){
    
    outputdir_toplot = outputdir_lst[[i]]
    print(outputdir_toplot)
    run_name = run_name_lst[[i]]
    # outputdir = coupled_outputdir
    remind.files <- list.files(outputdir_toplot, pattern = "fulldata.gdx") 
    
    # usable energy for VRE (excluding curtailment)
    vmUsableSeTe <- file.path(outputdir_toplot, remind.files[length(remind.files)]) %>%
      read.gdx("vm_usableSeTe", factors = FALSE, squeeze = FALSE) %>%
      filter(ttot %in% model.periods.till2100) %>%
      filter(all_regi == reg) %>%
      filter(entySe == "seel") %>%
      filter(all_te %in% names(remind.vre.mapping)) %>%
      select(period = ttot, tech=all_te, value) %>%
      revalue.levels(tech = remind.vre.mapping) %>% 
      mutate(value = value * sm_TWa_2_MWh / 1e6) %>%
      dplyr::group_by(period, tech) %>%
      dplyr::summarise(value = sum(value) , .groups = 'keep') %>%
      dplyr::ungroup(period, tech) %>% 
      mutate(tech = factor(tech, levels = rev(unique(remind.tech.mapping))))%>% 
      mutate(period = as.numeric(as.character(period)) + x_positions[[i]]) %>% 
      mutate(runname = run_name)
    
    # for non-VRE
    vmprodSe <- file.path(outputdir_toplot, remind.files[length(remind.files)]) %>%
      read.gdx("vm_prodSe", factors = FALSE, squeeze = FALSE) %>%
      filter(tall %in% model.periods.till2100) %>%
      filter(all_regi == reg) %>%
      filter(all_enty.1 == "seel") %>%
      filter(all_te %in% names(remind.nonvre.mapping.whyd)) %>%
      select(period = tall, tech=all_te, value) %>%
      revalue.levels(tech = remind.nonvre.mapping.whyd) %>%
      mutate(value = value * sm_TWa_2_MWh / 1e6) %>%
      dplyr::group_by(period, tech) %>%
      dplyr::summarise(value = sum(value) , .groups = 'keep') %>%
      dplyr::ungroup(period, tech) %>% 
      mutate(tech = factor(tech, levels = rev(unique(remind.tech.mapping))))%>% 
      mutate(period = as.numeric(as.character(period)) + x_positions[[i]]) %>% 
      mutate(runname = run_name)
    
    loss <- file.path(outputdir_toplot, remind.files[length(remind.files)]) %>%
      read.gdx("v32_storloss", factors = FALSE, squeeze = FALSE) %>% 
      filter(ttot %in% model.periods.till2100) %>%
      filter(all_regi == reg) %>%
      filter(all_te %in% names(remind.vre.mapping)) %>% 
      select(period = ttot, value, tech=all_te) %>% 
      revalue.levels(tech = remind.tech.storloss.mapping) %>% 
      mutate(value = value * sm_TWa_2_MWh/1e6) %>%
      mutate(tech = factor(tech, levels=rev(unique(remind.tech.storloss.mapping)))) %>% 
      mutate(period = as.numeric(as.character(period)) + x_positions[[i]]) %>% 
      mutate(runname = run_name)
    
    df.generation <- rbind(df.generation,loss,vmUsableSeTe,vmprodSe)
  }
  
  df.generation <- df.generation%>% 
    mutate(runname = factor(runname, levels=rev(unique(run_name_lst)))) 
  
  p.gen.compare <- ggplot() +
    geom_bar(data = df.generation, aes(x=period, y=value, fill=tech, linetype=runname), colour = "black", stat="identity",position="stack", width=2,linewidth=0.2) +
    scale_fill_manual(name = "Technology", values = color.mapping.wloss) +
    theme(legend.title = element_blank()) +
    theme(legend.position="bottom", legend.direction="horizontal", legend.text = element_text(size=14)) +
    guides(fill=guide_legend(nrow=4,byrow=TRUE), linetype=guide_legend(nrow=4,byrow=TRUE))+
    scale_linetype_discrete(breaks=run_name_lst)+
    theme(axis.text = element_text(size=14), axis.title = element_text(size= 14, face="bold")) +
    xlab("Time") + ylab(paste0("Generation (TWh)")) 
  
  swfigure(sw,print,p)
  
  p <- plot_grid(p.cap.compare,
                 p.gen.compare,
                 labels = c("(a)", "(b)"),
                 label_size = 1.7*font.size,
                 nrow = 2,
                 rel_heights = c(1, 1))
  
  if (save_png == 1){
    ggsave(filename = paste0(outputdir_lst[[1]], "/DIETER/Figure_uncoup_", fig_title, ".png"),  p,  width = 11.5, height = 12, units = "in", dpi = 120)
  }
    


