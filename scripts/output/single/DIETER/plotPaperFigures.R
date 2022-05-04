# Plots for paper

library(cowplot)  # Useful for themes and for arranging plots
# library(ggnewscale)  # Useful for multiple legends (not yet required)
library(metR)  # Useful for contour_fill
# library(svglite)

font.size = 8

# Plotting style ----------------------------------------------------------
theme_set(theme_cowplot(font_size = 8))  # Use simple theme and set font size

# Figure 2: Electricity price convergence ---------------------------------

# calculate shadow price in remind due to capacity constraint - no, not needed since markup should add up to zero at convergence,
# and these shadow prices are not part of the markup

# diff.price.rollmean <- out.RMprice %>% 
#   select(-variable) %>% 
#   dplyr::group_by(iteration) %>%
#   mutate(value = frollmean(value, 3, align = "left", fill = NA)) %>%
#   dplyr::ungroup(iteration) %>% 
#   replace(is.na(.), 0) %>% 
#   select(iteration,period,price=value) %>%
#   full_join(out.remind.sys.sp.capcon %>% select(iteration,period,sp.capcon=value) %>% filter(sp.capcon <20) ) %>% 
#   replace(is.na(.), 0) %>% 
#   filter(period %in% model.periods) %>% 
#   filter(!price == 0) %>% 
#   mutate(rmprice=price+sp.capcon) %>% 
#   select(period,iteration,rmprice) %>% 
#   left_join(out.DTprice) %>% 
#   filter(period %in% model.periods.till2100) %>% 
#   select(period,iteration,rmprice, value) %>% 
#   mutate(value = rmprice - value) %>% 
#   select(-rmprice)%>% 
#   filter(iteration > start_i-1)
# 
# diff.price.rollmean.avg.yr <- diff.price.rollmean %>% 
#   # filter(period > 2030) %>% 
#   dplyr::group_by(iteration) %>%
#   dplyr::summarise( value = mean(value), .groups = "keep" ) %>% 
#   dplyr::ungroup(iteration) %>% 
#   mutate(variable = "Difference of electricity price (REMIND rolling avg)")

# Panel 1: Surface plot
p.surface <- ggplot() +
  # With REMIND rolling mean over three periods
  geom_contour_fill(data = diff.price.rollmean,
                    mapping = aes(x = iteration, y = period, z = value),
                    breaks = seq(-20, 400, 5)) +
  scale_fill_divergent(name = "$/MWh",
                       limits = c(-20, 20),
                       #breaks = seq(-20,20,10),
                       labels = c(-20, -10, 0, 10, ">20"),
                       oob = scales::squish) + 
  scale_x_continuous(name = "Iteration") + 
  scale_y_continuous(name = "Time",
                     breaks = seq(2020, 2100, 10)) + 
  ggtitle("Electricity price difference (REMIND - DIETER)") +
  theme(axis.text = element_text(size = font.size),
        axis.title = element_text(size = font.size))+
  theme(legend.text = element_text(size = font.size))

# Panel 2: Time averaged line plot
p.line <- ggplot() + 
  geom_line(
            # data = diff.price.avg.yr,
            data = diff.price.rollmean.avg.yr,
            mapping = aes(x = iteration,
                          y = value,
                          color = "Time-averaged")) + 
  scale_color_manual(name = element_blank(),
                     values = c("Time-averaged" = "black")) +
  scale_x_continuous(name = "Iteration") + 
  scale_y_continuous(name = "$/MWh",
                     limits = c(-1, max(diff.price.rollmean.avg.yr$value)))+
  theme_minimal_grid(12) +
  theme(axis.text = element_text(size = font.size),
        axis.title = element_text(size =font.size))+
  theme(legend.text = element_text(size=font.size))
  
# Arrange both plots
p <- plot_grid(p.surface,
               p.line,
               ncol = 1,
               rel_heights = c(1, 0.5),
               labels = "auto",
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
  scale_fill_manual(name = "Technology", values = color.mapping.gen.order) +
  scale_linetype_manual(name = "Model", values = linetype.map[c("REMIND", "DIETER")]) +
  #guides(linetype = guide_legend(override.aes = list(fill = NA, col = "black"))) +
  xlab("Time") +
  ylab("Generation (TWh)") +
  ggtitle("Generation in REMIND and DIETER") +
  theme_minimal_grid(12)

plot.gen.relDiff <- list(plot.remind.gen.snap, plot.dieter.gen.snap, plot.remind.gen.total) %>%
  reduce(full_join) %>%
  mutate(delta_gen = (remind_gen - dieter_gen),
         delta_gen.rel = 100 * (remind_gen - dieter_gen) / remind_gen_total)

p.gen.diff <- ggplot() +
  geom_bar(data = plot.gen.relDiff ,
           mapping = aes(x = period, y = delta_gen.rel, fill = tech),
           stat = "identity") + 
  scale_fill_manual(name = "Technology", values = color.mapping.gen.order) +
  scale_y_continuous(name = "Difference (%)", breaks = seq(-10,10,3)) + 
  xlab("Time") +
  ggtitle("Generation difference (REMIND - DIETER) / total REMIND generation ") +
  theme_minimal_grid(12)

# Arrange both plots
p.plots <- plot_grid(
  p.gen.doublebar + theme(legend.position = "none"),  # Without legend
  p.gen.diff + theme(legend.position = "none"),  # Without legend
  ncol = 1,
  rel_heights = c(1, 0.6),
  labels = "auto",
  label_size = 1.5*font.size,
  align = "v"
)

# Extract legend of doublebar plot for 
p.legend <- get_legend(p.gen.doublebar + theme(legend.box.margin = margin(0, 0, 0, 12)))

# Put together plot and legend
p <- plot_grid(p.plots,
               p.legend,
               ncol = 2,
               rel_widths = c(1, 0.2))

# Save as png
ggsave(filename = paste0(outputdir, "/DIETER/FIGURE03.png"),
       bg = "white",
       width = 22,  # Vary width according to how many panels plot has
       height = 12,  # Vary height according to how many panels plot has
       units = "cm")

# Save as svg
# ggsave(filename = paste0(outputdir, "/DIETER/FIGURE02.svg"),
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


plot.remind.capacity2.group <- plot.remind.capacity2 %>% 
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
  geom_bar(data = plot.remind.capacity2,
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
  scale_fill_manual(name = "Technology", values = color.mapping.cap.order) +
  scale_linetype_manual(name = "Model", values = linetype.map[c("REMIND", "DIETER")]) +
  #guides(linetype = guide_legend(override.aes = list(fill = NA, col = "black"))) +
  xlab("Time") +
  ylab("Capacity (GW)") +
  ggtitle("Capacity in REMIND and DIETER") +
  theme_minimal_grid(12)

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
  scale_fill_manual(name = "Technology", values = color.mapping.cap.order) +
  scale_y_continuous(name = "Difference (%)", breaks = seq(-20,20,5)) + 
  xlab("Time") +
  ggtitle("Capacity difference (REMIND - DIETER) / total REMIND capacity ") +
  theme_minimal_grid(12)

# Arrange both plots
p.plots <- plot_grid(
  p.cap.doublebar + theme(legend.position = "none"),  # Without legend
  p.cap.diff + theme(legend.position = "none"),  # Without legend
  ncol = 1,
  rel_heights = c(1, 0.6),
  labels = "auto",
  label_size = 1.5*font.size,
  align = "v"
)

# Extract legend of doublebar plot for 
p.legend <- get_legend(p.cap.doublebar + theme(legend.box.margin = margin(0, 0, 0, 12)))

# Put together plot and legend
p <- plot_grid(p.plots,
               p.legend,
               ncol = 2,
               rel_widths = c(1, 0.2))

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
p.sysLCOE_RM <- ggplot() + 
  geom_col( data = sys_avgLCOE_compare %>% filter(model=="REMIND"), 
            aes(period, value, fill=variable)) +
  geom_line(data = prices_RM.movingavg %>% filter(period %in% model.periods.till2100) ,
            aes(period, value, color=variable), alpha = 0.5, size=1.5) +  
  geom_line(data = prices_w2Shad_RM %>% filter(period %in% model.periods.till2100) ,
            aes(period, value, color=variable), alpha = 0.5, size=1.5)+
  scale_y_continuous("LCOE and electricity price\n(USD2015/MWh)") +
  scale_x_continuous(breaks = seq(2010,2100,10)) +
  scale_color_manual(name = "Price", values = price.colors) +
  coord_cartesian(ylim = c(ymin,ymax))+
  scale_fill_manual(name = "Costs", values = cost.colors) +
  guides(fill=guide_legend(nrow=4,byrow=TRUE), color=guide_legend(nrow=4,byrow=TRUE))+
  ggtitle("REMIND")+
  theme(legend.position="bottom", legend.direction="horizontal", legend.text = element_text(size=font.size)) +
  theme(axis.text=element_text(size=font.size), axis.title=element_text(size=font.size, face="bold"),strip.text = element_text(size=font.size)) 

p.sysLCOE_DT <- ggplot() + 
  geom_col( data = sys_avgLCOE_compare %>% filter(model=="DIETER"), 
            aes(period, value, fill=variable)) +
  geom_line(data = DT.prices.lines %>% filter(period %in% model.periods.till2100) ,
            aes(period, value, color=variable), alpha = 0.7, size=1.5) +  
  scale_y_continuous("LCOE and electricity price\n(USD2015/MWh)") +
  scale_x_continuous(breaks = seq(2010,2100,10)) +
  scale_color_manual(name = "Price", values = price.colors) +
  coord_cartesian(ylim = c(ymin,ymax))+
  scale_fill_manual(name = "Costs",values = cost.colors) +
  guides(fill=guide_legend(nrow=4,byrow=TRUE), color=guide_legend(nrow=4,byrow=TRUE))+
  ggtitle("DIETER")+
  theme(legend.position="bottom", legend.direction="horizontal", legend.text = element_text(size=font.size)) +
  theme(axis.text=element_text(size=font.size), axis.title=element_text(size=font.size, face="bold"),strip.text = element_text(size=font.size)) 

# Arrange both plots
p.plots <- plot_grid(
  p.sysLCOE_RM + theme(legend.position = "none"),  # Without legend
  p.sysLCOE_DT + theme(legend.position = "none"),  # Without legend
  ncol = 2,
  rel_heights = c(1, 1),
  labels = "auto",
  label_size = 1.5*font.size,
  align = "v"
)

p.legend <- get_legend(p.sysLCOE_RM + theme(legend.box.margin = margin(0, 0, 0, 12)))

# Put together plot and legend
p <- plot_grid(p.plots,
               p.legend,
               ncol = 1,
               rel_heights = c(1, 0.2))

# Save as png
ggsave(filename = paste0(outputdir, "/DIETER/FIGURE_sysZPC.png"),
       bg = "white",
       width = 22,  # Vary width according to how many panels plot has
       height = 14,  # Vary height according to how many panels plot has
       units = "cm")

# ============ 0-profit plots - tech =============================================================================
# Arrange both plots
p.plots <- plot_grid(
  p.teLCOE_avg.DIETER + theme(legend.position = "none"),  # Without legend
  p.teLCOE.REMIND + theme(legend.position = "none"),  # Without legend
  ncol = 1,
  rel_heights = c(1, 1),
  labels = "auto",
  label_size = 3*font.size,
  align = "v"
)

p.legend <- get_legend(p.teLCOE.REMIND + theme(legend.box.margin = margin(0, 0, 0, 12)))

# Put together plot and legend
p <- plot_grid(p.plots,
               p.legend,
               ncol = 1,
               rel_heights = c(1, 0.05))

# Save as png
ggsave(filename = paste0(outputdir, "/DIETER/FIGURE_techZPC.png"),
       bg = "white",
       width = 37,  # Vary width according to how many panels plot has
       height = 38,  # Vary height according to how many panels plot has
       units = "cm")

# ============SCENARIO plots =============================================================================
# Figure ?: Long-term development ------------------------------------------

# Figure ?: RLDC ------------------------------------------
year_toplot = 2045

p.RM.rldc <-ggplot() +
  geom_area(data = RLDC.VRE, aes(x = hour, y = value, fill = tech), size = 1.2, alpha = 1, position = "identity") +
  geom_area(data = plot.rldc.hr, aes(x = hour, y = value, fill = tech), size = 1.2, alpha = 1) +
  coord_cartesian(ylim = c(-80,140),xlim = c(0,8760))+
  scale_fill_manual(name = "Technology", values = color.mapping.RLDC.basic)+
  theme(legend.position = "none")+
  xlab("hour") + ylab("residual load (GW)")+
  ggtitle(paste0("REMIND ", year_toplot))

# Arrange both plots
p <- plot_grid(p.RM.rldc,
               p.DT.rldc,
               ncol = 2,
               rel_widths = c(1, 1.2),
               labels = "auto",
               label_size = 1.2*font.size,
               align = "h")

# Save as png
ggsave(filename = paste0(outputdir, "/DIETER/FIGURE0n.png"),
       bg = "white",
       width = 20,  # Vary width according to how many panels plot has
       height = 12,  # Vary height according to how many panels plot has
       units = "cm")

