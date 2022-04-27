# Plots for paper

library(cowplot)  # Useful for themes and for arranging plots
# library(ggnewscale)  # Useful for multiple legends (not yet required)
library(metR)  # Useful for contour_fill
# library(svglite)

font.size = 8

# Plotting style ----------------------------------------------------------
theme_set(theme_cowplot(font_size = 8))  # Use simply theme and set font size

# Figure 1: Electricity price convergence ---------------------------------

# diff.price.rollmeaaan <-diff.price.rollmean  %>% 
#   filter(iteration == 36) %>% 
#   dplyr::group_by(iteration) %>%
#   dplyr::summarise( value = mean(value), .groups = "keep" ) %>% 
#   dplyr::ungroup(iteration) 

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
  ggtitle("Electricity price difference (REMIND - DIETER)")

# Panel 2: Time averaged line plot
p.line <- ggplot() + 
  geom_line(
            # data = diff.price.avg.yr,
            data = diff.price.rollmean.avg.yr,
            mapping = aes(x = iteration,
                          y = value,
                          color = "Time averaged")) + 
  scale_color_manual(name = element_blank(),
                     values = c("Time averaged" = "black")) +
  scale_x_continuous(name = "Iteration") + 
  scale_y_continuous(name = "$/MWh",
                     limits = c(-1, max(diff.price.avg.yr$value)))+
  theme_minimal_grid(12)
  
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

# Figure 2: Generation convergence ------------------------------------------
# With manual colour scales, we need to order this vector 
color.mapping.gen.order <- color.mapping.cap[levels(plot.dieter.gen2$tech)]
color.mapping.gen.order <- color.mapping.gen.order[!is.na(color.mapping.gen.order)]

# Make additional tibble for capacity sum
# This way we can draw a border around the entire stacked bar
plot.dieter.gen2.group <- plot.dieter.gen2 %>% 
  group_by(period, model) %>% 
  summarise(sum = sum(value))

plot.remind.gen2.group <- plot.remind.gen2 %>% 
  group_by(period, model) %>% 
  summarise(sum = sum(value))

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


# Figure 3: Capacity convergence ------------------------------------------
# With manual colour scales, we need to order this vector 
color.mapping.cap.order <- color.mapping.cap[levels(plot.dieter.capacity2$tech)]
color.mapping.cap.order <- color.mapping.cap.order[!is.na(color.mapping.cap.order)]

# Make additional tibble for capacity sum
# This way we can draw a border around the entire stacked bar
plot.dieter.capacity2.group <- plot.dieter.capacity2 %>% 
  group_by(period, model) %>% 
  summarise(sum = sum(value))

plot.remind.capacity2.group <- plot.remind.capacity2 %>% 
  group_by(period, model) %>% 
  summarise(sum = sum(value))


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