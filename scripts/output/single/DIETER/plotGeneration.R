# Data preparation (REMIND) -----------------------------------------------

cat("Plot generation \n")

out.remind.generation <- NULL
remind.generation.withCurt <- NULL

for (i in 1:length(remind.files)) {
  
  # usable energy for VRE (excluding curtailment)
  vmUsableSeTe <- file.path(outputdir, remind.files[i]) %>%
    read.gdx("vm_usableSeTe", factors = FALSE, squeeze = FALSE) %>%
    filter(ttot %in% model.periods) %>%
    filter(all_regi == reg) %>%
    filter(entySe == "seel") %>%
    filter(all_te %in% names(remind.vre.mapping)) %>%
    select(period = ttot, tech=all_te, value) %>%
    revalue.levels(tech = remind.vre.mapping) %>% 
    mutate(value = value * sm_TWa_2_MWh / 1e6) %>%
    dplyr::group_by(period, tech) %>%
    dplyr::summarise(value = sum(value) , .groups = 'keep') %>%
    dplyr::ungroup(period, tech) %>%
    mutate(iteration = i-1)%>%
    mutate(tech = factor(tech, levels = rev(unique(remind.tech.mapping))))
  
  # for non-VRE
  vmprodSe <- file.path(outputdir, remind.files[i]) %>%
    read.gdx("vm_prodSe", factors = FALSE, squeeze = FALSE) %>%
    filter(tall %in% model.periods) %>%
    filter(all_regi == reg) %>%
    filter(all_enty.1 == "seel") %>%
    filter(all_te %in% names(remind.nonvre.mapping.whyd)) %>%
    select(period = tall, tech=all_te, value) %>%
    revalue.levels(tech = remind.nonvre.mapping.whyd) %>%
    mutate(value = value * sm_TWa_2_MWh / 1e6) %>%
    dplyr::group_by(period, tech) %>%
    dplyr::summarise(value = sum(value) , .groups = 'keep') %>%
    dplyr::ungroup(period, tech) %>%
    mutate(iteration = i-1)%>%
    mutate(tech = factor(tech, levels = rev(unique(remind.tech.mapping))))
  
  out.remind.generation <- rbind(out.remind.generation, vmUsableSeTe,vmprodSe)
  
  generation.withCurt<- file.path(outputdir, remind.files[i]) %>%
    read.gdx("vm_prodSe", factors = FALSE, squeeze = FALSE) %>%
    filter(tall %in% model.periods) %>%
    filter(all_regi == reg) %>%
    filter(all_te %in% names(remind.tech.mapping)) %>%
    filter(all_enty.1 == "seel")  %>%
    select(period = tall, value, tech=all_te) %>%
    revalue.levels(tech = remind.tech.mapping) %>%
    dplyr::group_by(period, tech) %>%
    dplyr::summarise( value = sum(value) , .groups = 'keep' ) %>%
    dplyr::ungroup(period, tech) %>%
    mutate(value = value * sm_TWa_2_MWh/1e6) %>%
    mutate(tech = factor(tech, levels=rev(unique(remind.tech.mapping)))) %>%
    mutate(iteration = i-1)

  remind.generation.withCurt <- rbind(remind.generation.withCurt,generation.withCurt)
  
}

remind.consumption <- NULL

for (i in 2:length(remind.files)) {
  
  h2consum <- file.path(outputdir, remind.files[i]) %>%
    read.gdx("p32_seh2elh2Dem",
             factors = FALSE,
             squeeze = FALSE) %>%
    filter(ttot %in% model.periods) %>%
    filter(all_regi == reg) %>%
    mutate(value = value * sm_TWa_2_MWh / 1e6) %>%
    select(period = ttot, h2dem = value) 
  
  totalConsum <- file.path(outputdir, remind.files[i]) %>%
    read.gdx("p32_usableSeDisp",
             factors = FALSE,
             squeeze = FALSE) %>%
    filter(ttot %in% model.periods) %>%
    filter(all_regi == reg) %>%
    mutate(value = value * sm_TWa_2_MWh / 1e6) %>%
    mutate(tech="seel") %>% 
    select(tech, period = ttot, value) %>% 
    full_join(h2consum) %>% 
    replace(is.na(.), 0) %>% 
    mutate(value = value -h2dem) %>% 
    select(-h2dem)
  
  h2consum2 <- h2consum %>% 
    mutate(tech="elh2") %>% 
    select(period, tech,value=h2dem)
  
  consumption.data <- list(h2consum2, totalConsum) %>%
    reduce(full_join) %>%
    revalue.levels(tech = remind.tech.mapping) %>%
    mutate(iteration = i-1) 
  
  remind.consumption <- rbind(remind.consumption, consumption.data)
  
}

remind.consumption <- remind.consumption %>%
  mutate(tech = fct_relevel(tech, table_ordered_name_dem))

generation.withCurt.disp <- remind.generation.withCurt %>% 
  filter(!tech %in% c("Solar", "Wind Onshore", "Wind Offshore")) %>% 
  dplyr::group_by(period, iteration) %>%
  dplyr::summarise( disp = sum(value) , .groups = 'keep' ) %>%
  dplyr::ungroup(period, iteration)

generation.withCurt.wind <- remind.generation.withCurt %>% 
  filter(tech %in% c("Wind Onshore")) %>% 
  left_join(generation.withCurt.disp)  %>% 
  mutate(value = value + disp) %>% 
  select(iteration,period,tech,value)

generation.withCurt.vre <- remind.generation.withCurt %>% 
  filter(tech %in% c("Solar","Wind Offshore")) %>% 
  select(iteration,period,tech,value)%>% 
  full_join(generation.withCurt.wind) 

generation.withCurt.vre$variable <- "total generation w/ curtailment"   

remind.generation.withCurt <- generation.withCurt.vre

# Data preparation (DIETER) -----------------------------------------------

out.dieter <- NULL
for (i in 1:length(dieter.files)) {
  it <- as.numeric(str_extract(dieter.files[i], "[0-9]+"))
  
  dieter.data <- file.path(outputdir, dieter.files[i]) %>%
    read.gdx("p32_report4RM", factors = FALSE, squeeze = FALSE) %>%
    select(
      period = X..1,
      regi = X..2,
      tech = X..3,
      variable = X..4,
      value
    ) %>%
    filter(variable %in% c("total_generation", "usable_generation", "total_consumption")) %>%
    mutate(value = value / 1e6) %>%
    filter(tech %in% names(dieter.tech.mapping)) %>%
    revalue.levels(tech = dieter.tech.mapping) %>%
    mutate(tech = factor(tech, levels = rev(unique(dieter.tech.mapping))))  %>%
    mutate(iteration = it) %>%
    mutate(period = as.numeric(period))
  
  out.dieter <- rbind(out.dieter, dieter.data)
}

dieter.gen.wCurt <- out.dieter %>%
  filter(variable == "total_generation") %>%
  select(period, iteration, tech, value)

dieter.consumption <- out.dieter %>%
  filter(variable == "total_consumption") %>%
  select(period, iteration, tech, value) %>% 
  mutate(tech = fct_relevel(tech, table_ordered_name_dem))

dieter.gen.wCurt.sum <- out.dieter %>%
  filter(variable == "total_generation") %>%
  select(period, iteration, tech, value) %>%
  dplyr::group_by(period, iteration) %>%
  dplyr::summarise(value = sum(value) , .groups = 'keep') %>%
  dplyr::ungroup(period, iteration)

dieter.gen.wCurt.disp <- dieter.gen.wCurt %>% 
  filter(!tech %in% c("Solar", "Wind Onshore", "Wind Offshore")) %>% 
  dplyr::group_by(period, iteration) %>%
  dplyr::summarise( disp = sum(value) , .groups = 'keep' ) %>%
  dplyr::ungroup(period, iteration)

dieter.gen.wCurt.wind <- dieter.gen.wCurt %>% 
  filter(tech %in% c("Wind Onshore")) %>% 
  left_join(dieter.gen.wCurt.disp) %>% 
  mutate(value = value +disp) %>% 
  select(iteration,period,tech,value)

dieter.gen.wCurt.vre <- dieter.gen.wCurt %>% 
  filter(tech %in% c("Wind Offshore","Solar")) %>% 
  select(iteration,period,tech,value)%>% 
  full_join(dieter.gen.wCurt.wind) 

dieter.gen.wCurt.vre$variable <- "total generation w/ curtailment"  

dieter.gen.wCurt <- dieter.gen.wCurt.vre
# Plotting ----------------------------------------------------------------

swlatex(sw, paste0("\\section{Generation}"))

for (year_toplot in model.periods) {
    
  plot.remind.generation <- out.remind.generation %>%
    filter(period == year_toplot)
  
  plot.remind.generation.withCurt <- remind.generation.withCurt %>%
    filter(period == year_toplot)
  
  plot.remind.consumption <- remind.consumption %>%
    filter(period == year_toplot)
  
  
  if (length(dieter.files) != 0) {
    plot.dieter.usableGen <-
      out.dieter %>% filter(as.numeric(period) == year_toplot,
                            variable == "usable_generation")
    plot.dieter.gen.wCurt <-
      dieter.gen.wCurt %>% filter(period == year_toplot)
    plot.dieter.gen.wCurt.sum <-
      dieter.gen.wCurt.sum %>% filter(period == year_toplot)
    plot.dieter.consumption <-
      dieter.consumption %>% filter(period == year_toplot)
    
  
  }
  #####################################################################################################
  swlatex(sw, paste0("\\subsection{Generation in ", year_toplot, "}"))
  
  p1 <- ggplot() +
    geom_area(
      data = plot.remind.generation,
      aes(x = iteration, y = value, fill = tech),
      size = 1.2,
      alpha = 0.5,
      stat = "identity"
    ) +
    geom_area(
      data = plot.remind.generation.withCurt,
      aes(x = iteration, y = value, color = tech),
      size = 1,
      alpha = 0,
      linetype = "dotted"
    ) +
    scale_fill_manual(name = "Technology", values = color.mapping) +
    scale_color_manual(name = "Technology", values = color.mapping_vre) +
    theme(axis.text = element_text(size = 10),
          axis.title = element_text(size = 10, face = "bold")) +
    xlab("iteration") + ylab(paste0("Generation (TWh)")) +
    ggtitle(paste0("REMIND: ", reg, year_toplot)) +
    theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank())
  
  if (h2switch == "on"){
    p1 <- p1 +
      geom_area(
        data = plot.remind.consumption ,
        aes(x = iteration, y = -value, fill = tech),
        size = 1.2,
        alpha = 0.5,
        stat = "identity"
      )
  }
  
  if (length(dieter.files) != 0) {
    p2 <- ggplot() +
      geom_area(
        data = plot.dieter.usableGen,
        aes(x = iteration, y = value, fill = tech),
        size = 1.2,
        alpha = 0.5
      ) +
      geom_area(
        data = plot.dieter.gen.wCurt,
        aes(x = iteration, y = value, color = tech),
        size = 1,
        alpha = 0,
        linetype = "dotted"
      ) +
      scale_fill_manual(name = "Technology", values = color.mapping) +
      scale_color_manual(name = "Technology", values = color.mapping_vre) +
      theme(axis.text = element_text(size = 10),
            axis.title = element_text(size = 10, face = "bold")) +
      xlab("iteration") + ylab(paste0("Generation (TWh)")) +
      ggtitle(paste0("DIETER: ", reg, year_toplot)) +
      theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank())
    
    if (h2switch == "on"){
    p2 <- p2 +
    geom_area(
      data = plot.dieter.consumption,
      aes(
        x = iteration,
        y = -value,
        fill = tech
      ),
      size = 1.2,
      alpha = 0.5,
      stat = "identity"
    ) 
    }
    
  }
  
  
  grid.newpage()
  if (length(dieter.files) != 0) {
    p <- arrangeGrob(rbind(ggplotGrob(p1), ggplotGrob(p2)))
  } else {
    p <- p1
  }
  
  swfigure(sw, grid.draw, p)
  if (save_png == 1){
    ggsave(filename = paste0(outputdir, "/DIETER/Generation_", year_toplot, ".png"),  p,  width = 9, height =12, units = "in", dpi = 120)
  }
}

#####################################################################################################
swlatex(sw, "\\subsection{Generation over time (last iteration)}")

plot.remind.generation <-
  out.remind.generation  %>% filter(iteration == max(out.remind.generation$iteration))

plot.remind.generation.withCurt <- remind.generation.withCurt %>%
  filter(iteration == max(remind.generation.withCurt$iteration))

plot.remind.consumption <- remind.consumption %>%
  filter(iteration == max(remind.consumption$iteration))

plot.dieter.generation <-
  out.dieter %>% filter(iteration == max(out.dieter$iteration),
                        variable == "usable_generation")

plot.dieter.gen.wCurt <-
  dieter.gen.wCurt %>% filter(iteration == max(dieter.gen.wCurt$iteration))
plot.dieter.gen.wCurt.sum <-
  dieter.gen.wCurt.sum %>% filter(iteration == max(dieter.gen.wCurt.sum$iteration))
plot.dieter.consumption <-
  dieter.consumption %>% filter(iteration == max(dieter.consumption$iteration))

p1 <- ggplot() +
  geom_area(
    data = plot.remind.generation %>% filter(period %in% model.periods.till2100),
    aes(x = period, y = value, fill = tech),
    size = 1.2,
    alpha = 0.5,
    stat = "identity"
  ) +
  geom_area(
    data = plot.remind.generation.withCurt%>% filter(period %in% model.periods.till2100),
    aes(x = period, y = value, color = tech),
    size = 1,
    alpha = 0,
    linetype = "dotted"
  ) +

  scale_fill_manual(name = "Technology", values = color.mapping) +
  scale_color_manual(name = "Technology", values = color.mapping_vre) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 15, face = "bold")) +
  xlab("Year") + ylab("Generation (TWh)") +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(size=13)) +
  theme(axis.text=element_text(size=15), axis.title=element_text(size= 13, face="bold"),strip.text = element_text(size=13)) +
  ggtitle(paste0("REMIND: ", reg, " (last iteration)")) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank())



if (length(dieter.files) != 0) {
  p2 <- ggplot() +
    geom_area(
      data = plot.dieter.generation %>% filter(period %in% model.periods.till2100),
      aes(
        x = as.numeric(period),
        y = value,
        fill = tech
      ),
      size = 1.2,
      alpha = 0.5
    ) +
    geom_area(
      data = plot.dieter.gen.wCurt%>% filter(period %in% model.periods.till2100),
      aes(
        x = as.numeric(period),
        y = value,
        color = tech
      ),
      size = 1,
      alpha = 0,
      linetype = "dotted"
    ) +

    scale_fill_manual(name = "Technology", values = color.mapping) +
    scale_color_manual(name = "Technology", values = color.mapping_vre) +
    theme(axis.text = element_text(size = 10),
          axis.title = element_text(size = 15, face = "bold")) +
    xlab("Year") + ylab("Generation (TWh)") +
    theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(size=13)) +
    theme(axis.text=element_text(size=15), axis.title=element_text(size= 13, face="bold"),strip.text = element_text(size=13)) +
    ggtitle(paste0("DIETER: ", reg, " (last iteration)")) +
    theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank())
  
}

grid.newpage()
if (length(dieter.files) != 0) {
  p <- arrangeGrob(rbind(ggplotGrob(p1), ggplotGrob(p2)))
} else {
  p <- p1
}

swfigure(sw, grid.draw, p)
if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/Generation_time.png"),  p,  width = 12, height =14, units = "in", dpi = 120)
}

#####################################################################################################
swlatex(sw, "\\subsection{Generation and consumption over time (last iteration)}")

p.genwConsump1 <- ggplot() +
  geom_area(
    data = plot.remind.generation %>% filter(period %in% model.periods.till2100),
    aes(x = period, y = value, fill = tech),
    size = 1.2,
    alpha = 0.5,
    stat = "identity"
  ) +
  geom_area(
    data = plot.remind.generation.withCurt%>% filter(period %in% model.periods.till2100),
    aes(x = period, y = value, color = tech),
    size = 1,
    alpha = 0,
    linetype = "dotted"
  ) +
  scale_fill_manual(name = "Technology", values = color.mapping) +
  scale_color_manual(name = "Technology", values = color.mapping_vre) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 15, face = "bold")) +
  xlab("Year") + ylab("Generation (TWh)") +
  theme(legend.position="none")+
  theme(axis.text=element_text(size=15), axis.title=element_text(size= 13, face="bold"),strip.text = element_text(size=13)) +
  ggtitle(paste0("REMIND: ", reg, " (last iteration)")) 
# +
  # theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank())



if (length(dieter.files) != 0) {
  p.genwConsump2 <- ggplot() +
    geom_area(
      data = plot.dieter.generation%>% filter(period %in% model.periods.till2100),
      aes(
        x = as.numeric(period),
        y = value,
        fill = tech
      ),
      size = 1.2,
      alpha = 0.5
    ) +
    geom_area(
      data = plot.dieter.gen.wCurt %>% filter(period %in% model.periods.till2100),
      aes(
        x = as.numeric(period),
        y = value,
        color = tech
      ),
      size = 1,
      alpha = 0,
      linetype = "dotted"
    ) +
    scale_fill_manual(name = "Technology", values = color.mapping) +
    scale_color_manual(name = "Technology", values = color.mapping_vre) +
    theme(axis.text = element_text(size = 10),
          axis.title = element_text(size = 15, face = "bold")) +
    xlab("Year") + ylab("Generation (TWh)") +
    theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(size=13)) +
    theme(axis.text=element_text(size=15), axis.title=element_text(size= 13, face="bold"),strip.text = element_text(size=13)) +
    ggtitle(paste0("DIETER: ", reg, " (last iteration)")) +
    theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank())
  
}

if (h2switch == "on"){
  p.genwConsump1 <- p.genwConsump1 + geom_area(
  data = plot.dieter.consumption%>% filter(period %in% model.periods.till2100) %>% mutate(value = -value),
  aes(
    x = as.numeric(period),
    y = value,
    fill = tech
  ),
  size = 1.2,
  alpha = 0.5
)
  
  
  p.genwConsump2 <- p.genwConsump2 +  geom_area(
    data = plot.remind.consumption%>% filter(period %in% model.periods.till2100)%>% mutate(value = -value),
    aes(x = period, y = value, fill = tech),
    size = 1.2,
    alpha = 0.5,
    stat = "identity"
  ) 
} 
  
grid.newpage()
if (length(dieter.files) != 0) {
  p <- arrangeGrob(rbind(ggplotGrob(p.genwConsump1), ggplotGrob(p.genwConsump2)))
} else {
  p <- p.genwConsump1
}

swfigure(sw, grid.draw, p)
if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/Generation_wConsumption_time.png"),  p,  width = 12, height =14, units = "in", dpi = 120)
}
##################################################################################################
swlatex(sw, "\\subsection{Generation last iteration - double bar plot}")

if (length(dieter.files) != 0) {
  plot.dieter.gen2 <- plot.dieter.generation %>% 
    filter(period %in% model.periods.till2100) %>% 
    mutate(period = as.numeric(as.character(period)) + 1) %>% 
    mutate(model = "DIETER")
  # %>% 
    # filter(iteration == maxiter-1) 
  
  plot.remind.gen2 <- plot.remind.generation %>% 
    filter(period %in% model.periods.till2100) %>% 
    mutate(period = as.numeric(as.character(period)) - 1) %>% 
    mutate(model = "REMIND") %>% 
    filter(iteration == maxiter-1)
  
  p<-ggplot() +
    geom_bar(data = plot.dieter.gen2, aes(x=period, y=value, fill=tech, linetype=model), colour = "black", stat="identity",position="stack", width=1.5) + 
    geom_bar(data = plot.remind.gen2, aes(x=period, y=value, fill=tech, linetype=model), colour = "black", stat="identity",position="stack", width=1.5) + 
    scale_fill_manual(name = "Technology", values = color.mapping) +
    scale_linetype_manual(name = "model", values = linetype.map) +
    guides(linetype = guide_legend(override.aes = list(fill = NA, col = "black"))) +
    xlab("period") + ylab(paste0("Generation (TWh)")) +
    ggtitle(paste0(reg)) +
    theme(legend.title = element_blank()) 
  
  swfigure(sw,print,p)
  if (save_png == 1){
    ggsave(filename = paste0(outputdir, "/DIETER/Generation_doublebar_time.png"),  p,  width = 9, height = 6, units = "in", dpi = 120)
  }
  
}
#####################################################################################################
if (length(dieter.files) != 0) {
  for (i in c(start_i+1,start_i+5,start_i+10,maxiter-1)){
   # i = 27
    plot.remind.gen.snap <- out.remind.generation %>% 
    filter(iteration == i) %>% 
    filter(period %in% model.periods.till2100)%>%
    mutate(period = as.numeric(period))%>% 
    dplyr::rename(remind_gen = value)
    
    plot.dieter.gen.snap <- out.dieter %>%
    filter(iteration == i) %>%
    filter(variable == "usable_generation") %>%
    filter(period %in% model.periods.till2100)%>%
    mutate(period = as.numeric(period)) %>%
    dplyr::rename(dieter_gen = value)
    
    plot.gen.diff <- list(plot.remind.gen.snap, plot.dieter.gen.snap) %>%
      reduce(full_join) %>%
      mutate(delta_gen = remind_gen - dieter_gen) 
  
  p <-ggplot() +
    geom_bar(data = plot.gen.diff, aes(x = period, y = delta_gen, fill = tech, label = delta_gen),  alpha = 0.5, stat = "identity") +
    geom_label(size = 3, position = position_stack(vjust = 0.5)) +
    scale_fill_manual(name = "Technology", values = color.mapping)+
    theme(axis.text=element_text(size=10), axis.title=element_text(size= 10,face="bold")) +
    xlab("period") + ylab(paste0("Generation (TWh)")) +
    ggtitle(paste0("Generation difference REMIND - DIETER"))+
    theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank()) +
    theme(aspect.ratio = .5) 
  
  swfigure(sw, grid.draw, p)
  if (save_png == 1){
    ggsave(filename = paste0(outputdir, "/DIETER/deltaGeneration_time_i", i, ".png"),  p,  width = 8, height =6, units = "in", dpi = 120)
  }
  
}
}


#####################################################################################################

if (length(dieter.files) != 0) {
  
  for (i in c(start_i,start_i+1,start_i+2,start_i+3,start_i+5,start_i+10,maxiter-2)){
    
    plot.remind.i0 <- out.remind.generation %>% 
      filter(iteration == 0+i) %>% 
      filter(period %in% model.periods.till2100) %>%
      select(-iteration) %>% 
      mutate(period = as.numeric(period)) %>% 
      dplyr::rename(remind_gen_0 = value)
    
    plot.remind.i1 <- out.remind.generation %>% 
      filter(iteration == 1+i) %>% 
      filter(period %in% model.periods.till2100) %>%
      select(-iteration) %>% 
      mutate(period = as.numeric(period)) %>% 
      dplyr::rename(remind_gen_1 = value)
  
    plot.remind.iter.diff <- list(plot.remind.i0, plot.remind.i1) %>%
      reduce(full_join) %>%
      mutate(delta_gen = remind_gen_1 - remind_gen_0) 
    
    p1 <-ggplot() +
      geom_bar(data = plot.remind.iter.diff , aes(x = period, y = delta_gen, fill = tech),  alpha = 0.5, stat = "identity") +
      geom_label(size = 3, position = position_stack(vjust = 0.5)) +
      scale_fill_manual(name = "Technology", values = color.mapping)+
      theme(axis.text=element_text(size=10), axis.title=element_text(size= 10,face="bold")) +
      xlab("period") + ylab(paste0("Generation (TWh)")) +
      ggtitle(paste0("REMIND Generation (i=", i+1,") - REMIND Generation (i=", i,")"))+
      theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank()) +
      theme(aspect.ratio = .5) 
    
    grid.newpage()
    p <- p1
    
    swfigure(sw, grid.draw, p)
    if (save_png == 1){
      ggsave(filename = paste0(outputdir, "/DIETER/deltaGeneration_iteration_", i, ".png"),  p,  width = 8, height =6, units = "in", dpi = 120)
    }
  }
}

