# Data preparation (REMIND) -----------------------------------------------

cat("Plot generation \n")

out.remind <- NULL
remind.generation.withCurt <- NULL

for (i in 1:length(remind.files)) {
  
  # usable energy for VRE (excluding curtailment)
  vmUsableSeTe <- file.path(outputdir, remind.files[i]) %>%
    read.gdx("vm_usableSeTe", factors = FALSE, squeeze = FALSE) %>%
    filter(ttot %in% model.periods) %>%
    filter(all_regi == reg) %>%
    filter(entySe == "seel") %>%
    filter(all_te %in% names(remind.vre.mapping)) %>%
    select(period = ttot, all_te, value) %>%
    revalue.levels(all_te = remind.vre.mapping) %>% 
    mutate(value = value * sm_TWa_2_MWh / 1e6) %>%
    dplyr::group_by(period, all_te) %>%
    dplyr::summarise(value = sum(value) , .groups = 'keep') %>%
    dplyr::ungroup(period, all_te) %>%
    mutate(iteration = i-1)%>%
    mutate(all_te = factor(all_te, levels = rev(unique(remind.tech.mapping))))
  
  # for non-VRE
  vmprodSe <- file.path(outputdir, remind.files[i]) %>%
    read.gdx("vm_prodSe", factors = FALSE, squeeze = FALSE) %>%
    filter(tall %in% model.periods) %>%
    filter(all_regi == reg) %>%
    filter(all_enty.1 == "seel") %>%
    filter(all_te %in% names(remind.nonvre.mapping.whyd)) %>%
    select(period = tall, all_te, value) %>%
    revalue.levels(all_te = remind.nonvre.mapping.whyd) %>%
    mutate(value = value * sm_TWa_2_MWh / 1e6) %>%
    dplyr::group_by(period, all_te) %>%
    dplyr::summarise(value = sum(value) , .groups = 'keep') %>%
    dplyr::ungroup(period, all_te) %>%
    mutate(iteration = i-1)%>%
    mutate(all_te = factor(all_te, levels = rev(unique(remind.tech.mapping))))
  
  out.remind <- rbind(out.remind, vmUsableSeTe,vmprodSe)
  
  generation.withCurt<- file.path(outputdir, remind.files[i]) %>%
    read.gdx("vm_prodSe", factors = FALSE, squeeze = FALSE) %>%
    filter(tall %in% model.periods) %>%
    filter(all_regi == reg) %>%
    filter(all_te %in% names(remind.tech.mapping)) %>%
    filter(all_enty.1 == "seel")  %>%
    select(period = tall, value,all_te) %>%
    revalue.levels(all_te = remind.tech.mapping) %>%
    dplyr::group_by(period, all_te) %>%
    dplyr::summarise( value = sum(value) , .groups = 'keep' ) %>%
    dplyr::ungroup(period, all_te) %>%
    mutate(value = value * sm_TWa_2_MWh/1e6) %>%
    mutate(all_te = factor(all_te, levels=rev(unique(remind.tech.mapping)))) %>%
    mutate(iteration = i-1)

  remind.generation.withCurt <- rbind(remind.generation.withCurt,generation.withCurt)
  
}

remind.consumption <- NULL

for (i in 2:length(remind.files)) {
  
  h2consum <- file.path(outputdir, remind.files[i]) %>%
    read.gdx("vm_demSe",
             factors = FALSE,
             field = "l",
             squeeze = FALSE) %>%
    filter(ttot %in% model.periods) %>%
    filter(all_regi == reg) %>%
    filter(all_te == "elh2") %>%
    mutate(value = value * sm_TWa_2_MWh / 1e6) %>%
    select(all_te,period = ttot, value) 
  
  totalConsum <- file.path(outputdir, remind.files[i]) %>%
    read.gdx("p32_usableSeDisp",
             factors = FALSE,
             squeeze = FALSE) %>%
    filter(ttot %in% model.periods) %>%
    filter(all_regi == reg) %>%
    mutate(value = value * sm_TWa_2_MWh / 1e6) %>%
    mutate(all_te="seel") %>% 
    select(all_te,period = ttot, value)
  
  consumption.data <- list(h2consum, totalConsum) %>%
    reduce(full_join) %>%
    revalue.levels(all_te = remind.tech.mapping) %>%
    mutate(iteration = i-1)
  
  remind.consumption <- rbind(remind.consumption, consumption.data)
  
}

remind.consumption <- remind.consumption %>%
  mutate(all_te = fct_relevel(all_te, table_ordered_name_dem))

generation.withCurt.disp <- remind.generation.withCurt %>% 
  filter(!all_te %in% c("Solar", "Wind Onshore", "Wind Offshore")) %>% 
  dplyr::group_by(period, iteration) %>%
  dplyr::summarise( disp = sum(value) , .groups = 'keep' ) %>%
  dplyr::ungroup(period, iteration)

generation.withCurt.wind <- remind.generation.withCurt %>% 
  filter(all_te %in% c("Wind Onshore")) %>% 
  left_join(generation.withCurt.disp)  %>% 
  mutate(value = value + disp) %>% 
  select(iteration,period,all_te,value)

generation.withCurt.vre <- remind.generation.withCurt %>% 
  filter(all_te %in% c("Solar","Wind Offshore")) %>% 
  select(iteration,period,all_te,value)%>% 
  full_join(generation.withCurt.wind) 

generation.withCurt.vre$tech <- "total generation w/ curtailment"   

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
      all_te = X..3,
      variable = X..4,
      value
    ) %>%
    filter(variable %in% c("total_generation", "usable_generation", "total_consumption")) %>%
    mutate(value = value / 1e6) %>%
    filter(all_te %in% names(dieter.tech.mapping)) %>%
    revalue.levels(all_te = dieter.tech.mapping) %>%
    mutate(all_te = factor(all_te, levels = rev(unique(dieter.tech.mapping))))  %>%
    mutate(iteration = it) %>%
    mutate(period = as.numeric(period))
  
  out.dieter <- rbind(out.dieter, dieter.data)
}

dieter.gen.wCurt <- out.dieter %>%
  filter(variable == "total_generation") %>%
  select(period, iteration, all_te, value)

dieter.consumption <- out.dieter %>%
  filter(variable == "total_consumption") %>%
  select(period, iteration, all_te, value) %>% 
  mutate(all_te = fct_relevel(all_te, table_ordered_name_dem))

dieter.gen.wCurt.sum <- out.dieter %>%
  filter(variable == "total_generation") %>%
  select(period, iteration, all_te, value) %>%
  dplyr::group_by(period, iteration) %>%
  dplyr::summarise(value = sum(value) , .groups = 'keep') %>%
  dplyr::ungroup(period, iteration)

dieter.gen.wCurt$tech <- "total generation w/ curtailment"

dieter.gen.wCurt.disp <- dieter.gen.wCurt %>% 
  filter(!all_te %in% c("Solar", "Wind Onshore", "Wind Offshore")) %>% 
  dplyr::group_by(period, iteration) %>%
  dplyr::summarise( disp = sum(value) , .groups = 'keep' ) %>%
  dplyr::ungroup(period, iteration)

dieter.gen.wCurt.wind <- dieter.gen.wCurt %>% 
  filter(all_te %in% c("Wind Onshore")) %>% 
  left_join(dieter.gen.wCurt.disp) %>% 
  mutate(value = value +disp) %>% 
  select(iteration,period,all_te,value)

dieter.gen.wCurt.vre <- dieter.gen.wCurt %>% 
  filter(all_te %in% c("Wind Offshore","Solar")) %>% 
  select(iteration,period,all_te,value)%>% 
  full_join(dieter.gen.wCurt.wind) 

dieter.gen.wCurt.vre$tech <- "total generation w/ curtailment"  

dieter.gen.wCurt <- dieter.gen.wCurt.vre
# Plotting ----------------------------------------------------------------

swlatex(sw, paste0("\\section{Generation}"))

for (year_toplot in model.periods) {
    
  plot.remind <- out.remind %>%
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
      data = plot.remind,
      aes(x = iteration, y = value, fill = all_te),
      size = 1.2,
      alpha = 0.5,
      stat = "identity"
    ) +
    geom_area(
      data = plot.remind.generation.withCurt,
      aes(x = iteration, y = value, color = all_te),
      size = 1,
      alpha = 0,
      linetype = "dotted"
    ) +
    scale_fill_manual(name = "Technology", values = color.mapping) +
    scale_color_manual(name = "Technology", values = color.mapping_vre) +
    theme(axis.text = element_text(size = 10),
          axis.title = element_text(size = 10, face = "bold")) +
    xlab("iteration") + ylab(paste0("Usable generation (TWh)")) +
    ggtitle(paste0("REMIND: ", reg, year_toplot)) +
    theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank())
  
  if (h2switch == "on"){
    p1 <- p1 +
      geom_area(
        data = plot.remind.consumption ,
        aes(x = iteration, y = -value, fill = all_te),
        size = 1.2,
        alpha = 0.5,
        stat = "identity"
      )
  }
  
  if (length(dieter.files) != 0) {
    p2 <- ggplot() +
      geom_area(
        data = plot.dieter.usableGen,
        aes(x = iteration, y = value, fill = all_te),
        size = 1.2,
        alpha = 0.5
      ) +
      geom_area(
        data = plot.dieter.gen.wCurt,
        aes(x = iteration, y = value, color = all_te),
        size = 1,
        alpha = 0,
        linetype = "dotted"
      ) +
      scale_fill_manual(name = "Technology", values = color.mapping) +
      scale_color_manual(name = "Technology", values = color.mapping_vre) +
      theme(axis.text = element_text(size = 10),
            axis.title = element_text(size = 10, face = "bold")) +
      xlab("iteration") + ylab(paste0("Usable generation (TWh)")) +
      ggtitle(paste0("DIETER: ", reg, year_toplot)) +
      theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank())
    
    if (h2switch == "on"){
    p2 <- p2 +
    geom_area(
      data = plot.dieter.consumption,
      aes(
        x = iteration,
        y = -value,
        fill = all_te
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

plot.remind <-
  out.remind  %>% filter(iteration == max(out.remind$iteration))

plot.remind.generation.withCurt <- remind.generation.withCurt %>%
  filter(iteration == max(remind.generation.withCurt$iteration))

plot.remind.consumption <- remind.consumption %>%
  filter(iteration == max(remind.consumption$iteration))

plot.dieter <-
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
    data = plot.remind %>% filter(period <2110),
    aes(x = period, y = value, fill = all_te),
    size = 1.2,
    alpha = 0.5,
    stat = "identity"
  ) +
  geom_area(
    data = plot.remind.generation.withCurt%>% filter(period <2110),
    aes(x = period, y = value, color = all_te),
    size = 1,
    alpha = 0,
    linetype = "dotted"
  ) +

  scale_fill_manual(name = "Technology", values = color.mapping) +
  scale_color_manual(name = "Technology", values = color.mapping_vre) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10, face = "bold")) +
  xlab("Year") + ylab(paste0("Usable generation (TWh)")) +
  ggtitle(paste0("REMIND: ", reg, " (last iteration)")) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank())



if (length(dieter.files) != 0) {
  p2 <- ggplot() +
    geom_area(
      data = plot.dieter%>% filter(period <2110),
      aes(
        x = as.numeric(period),
        y = value,
        fill = all_te
      ),
      size = 1.2,
      alpha = 0.5
    ) +
    geom_area(
      data = plot.dieter.gen.wCurt%>% filter(period <2110),
      aes(
        x = as.numeric(period),
        y = value,
        color = all_te
      ),
      size = 1,
      alpha = 0,
      linetype = "dotted"
    ) +

    scale_fill_manual(name = "Technology", values = color.mapping) +
    scale_color_manual(name = "Technology", values = color.mapping_vre) +
    theme(axis.text = element_text(size = 10),
          axis.title = element_text(size = 10, face = "bold")) +
    xlab("Year") + ylab(paste0(c("total_generation", "usable_generation", "total_consumption"), "(TWh)")) +
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
  ggsave(filename = paste0(outputdir, "/DIETER/Generation_time.png"),  p,  width = 8, height =12, units = "in", dpi = 120)
}


#####################################################################################################
if (length(dieter.files) != 0) {
for (i in c(5,10,20,27,maxiter-1)){
   # i = 27
    plot.remind.snap <- out.remind %>% 
    filter(iteration == i) %>% 
    filter(period <2110)%>%
    mutate(period = as.numeric(period))%>% 
    dplyr::rename(remind_gen = value)
    
    plot.dieter.snap <- out.dieter %>%
    filter(iteration == i) %>%
    filter(variable == "usable_generation") %>%
    filter(period <2110)%>%
    mutate(period = as.numeric(period)) %>%
    dplyr::rename(dieter_gen = value)
    
    plot.diff <- list(plot.remind.snap, plot.dieter.snap) %>%
      reduce(full_join) %>%
      mutate(delta_gen = remind_gen - dieter_gen) 
  
  p <-ggplot() +
    geom_bar(data = plot.diff , aes(x = period, y = delta_gen, fill = all_te, label = delta_gen),  alpha = 0.5, stat = "identity") +
    geom_label(size = 3, position = position_stack(vjust = 0.5)) +
    scale_fill_manual(name = "Technology", values = color.mapping)+
    theme(axis.text=element_text(size=10), axis.title=element_text(size= 10,face="bold")) +
    xlab("period") + ylab(paste0("Usable generation (TWh)")) +
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
  
  for (i in c(0,1,2,3,5,maxiter-2)){
    
    plot.remind.i0 <- out.remind %>% 
      filter(iteration == 0+i) %>% 
      filter(period <2110) %>%
      select(-iteration) %>% 
      mutate(period = as.numeric(period)) %>% 
      dplyr::rename(remind_gen_0 = value)
    
    plot.remind.i1 <- out.remind %>% 
      filter(iteration == 1+i) %>% 
      filter(period <2110) %>%
      select(-iteration) %>% 
      mutate(period = as.numeric(period)) %>% 
      dplyr::rename(remind_gen_1 = value)
  
    plot.remind.iter.diff <- list(plot.remind.i0, plot.remind.i1) %>%
      reduce(full_join) %>%
      mutate(delta_gen = remind_gen_1 - remind_gen_0) 
    
    p1 <-ggplot() +
      geom_bar(data = plot.remind.iter.diff , aes(x = period, y = delta_gen, fill = all_te),  alpha = 0.5, stat = "identity") +
      geom_label(size = 3, position = position_stack(vjust = 0.5)) +
      scale_fill_manual(name = "Technology", values = color.mapping)+
      theme(axis.text=element_text(size=10), axis.title=element_text(size= 10,face="bold")) +
      xlab("period") + ylab(paste0("Usable generation (TWh)")) +
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

