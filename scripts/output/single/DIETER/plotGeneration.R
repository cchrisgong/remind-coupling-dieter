# Data preparation (REMIND) -----------------------------------------------

cat("Plot generation \n")

out.remind <- NULL
remind.generation.withCurt <- NULL
remind.consumption <- NULL

for (i in 1:length(remind.files)) {
  vmProdSe <-   file.path(outputdir, remind.files[i]) %>%
    read.gdx("vm_prodSe", factors = FALSE, squeeze = FALSE) %>%
    filter(tall %in% report.periods) %>%
    filter(all_regi == "DEU") %>%
    filter(all_te %in% names(remind.nonvre.mapping2)) %>%
    filter(all_enty.1 == "seel") %>%
    select(period = tall, all_te, value) %>%
    revalue.levels(all_te = remind.nonvre.mapping2) %>%
    mutate(value = value * sm_TWa_2_MWh / 1e6) %>%
    dplyr::group_by(period, all_te) %>%
    dplyr::summarise(value = sum(value) , .groups = 'keep') %>%
    dplyr::ungroup(period, all_te) %>%
    mutate(iteration = i)
  
  vmUsableSeTe <- file.path(outputdir, remind.files[i]) %>%
    read.gdx("vm_usableSeTe", factors = FALSE, squeeze = FALSE) %>%
    filter(ttot %in% report.periods) %>%
    filter(all_regi == "DEU") %>%
    filter(all_te %in% names(remind.vre.mapping)) %>%
    filter(entySe == "seel")  %>%
    select(period = ttot, all_te, value) %>%
    revalue.levels(all_te = remind.vre.mapping) %>%
    mutate(value = value * sm_TWa_2_MWh / 1e6) %>%
    dplyr::group_by(period, all_te) %>%
    dplyr::summarise(value = sum(value) , .groups = 'keep') %>%
    dplyr::ungroup(period, all_te) %>%
    mutate(iteration = i)
  
  remind.vm_prodSe  <- list(vmProdSe, vmUsableSeTe) %>%
    reduce(full_join) %>%
    mutate(all_te = factor(all_te, levels = rev(unique(
      remind.tech.mapping
    ))))
  
  out.remind <- rbind(out.remind, remind.vm_prodSe)
  
  generation.withCurt<- file.path(outputdir, remind.files[i]) %>%
    read.gdx("vm_prodSe", factors = FALSE, squeeze = FALSE) %>%
    filter(tall %in% report.periods) %>%
    filter(all_regi == "DEU") %>%
    filter(all_te %in% names(remind.tech.mapping)) %>%
    filter(all_enty.1 == "seel")  %>%
    select(period = tall, value,all_te) %>%
    revalue.levels(all_te = remind.tech.mapping) %>%
    dplyr::group_by(period, all_te) %>%
    dplyr::summarise( value = sum(value) , .groups = 'keep' ) %>%
    dplyr::ungroup(period, all_te) %>%
    mutate(value = value * sm_TWa_2_MWh/1e6) %>%
    mutate(all_te = factor(all_te, levels=rev(unique(remind.tech.mapping)))) %>%
    mutate(iteration = i)

  remind.generation.withCurt <- rbind(remind.generation.withCurt,generation.withCurt)
  
  h2consum <- file.path(outputdir, remind.files[i]) %>%
    read.gdx("vm_demSe",
             factors = FALSE,
             field = "l",
             squeeze = FALSE) %>%
    filter(ttot %in% report.periods) %>%
    filter(all_regi == "DEU") %>%
    filter(all_te == "elh2") %>%
    mutate(value = value * sm_TWa_2_MWh / 1e6) %>%
    select(period = ttot, value) %>%
    mutate(iteration = i)
  
  h2consum2 <- h2consum %>%
    mutate(all_te = "elh2") %>%
    mutate(iteration = i)
  
  totalConsum <- file.path(outputdir, remind.files[i]) %>%
    read.gdx("p32_seelUsableDem",
             factors = FALSE,
             squeeze = FALSE) %>%
    filter(ttot %in% report.periods) %>%
    filter(all_regi == "DEU") %>%
    mutate(value = value * sm_TWa_2_MWh / 1e6) %>%
    select(period = ttot, totalconsum = value) %>%
    mutate(iteration = i)
  
  consumption.data <- list(h2consum, totalConsum) %>%
    reduce(full_join) %>%
    mutate(value = totalconsum - value)  %>%
    mutate(all_te = "seel") %>%
    select(period, all_te, value) %>%
    full_join(h2consum2) %>%
    revalue.levels(all_te = dieter.tech.mapping) %>%
    mutate(iteration = i)
  
 
  remind.consumption <- rbind(remind.consumption, consumption.data)
  
}


remind.generation.withCurt$tech <- "total generation w/ curtailment"

remind.consumption <- remind.consumption %>%
  mutate(all_te = fct_relevel(all_te, table_ordered_name_dem))

# Data preparation (DIETER) -----------------------------------------------

out.dieter <- NULL
for (i in 1:length(sorted_files_DT)) {
  dieter.data <- file.path(outputdir, sorted_files_DT[i]) %>%
    read.gdx("p32_report4RM", factors = FALSE, squeeze = FALSE) %>%
    select(
      period = X..1,
      regi = X..2,
      all_te = X..3,
      variable = X..4,
      value
    ) %>%
    filter(all_te %in% TECHkeylst_DT) %>%
    filter(variable %in% c("total_generation", "usable_generation", "total_consumption")) %>%
    mutate(value = value / 1e6) %>%
    filter(all_te %in% names(dieter.tech.mapping)) %>%
    revalue.levels(all_te = dieter.tech.mapping) %>%
    mutate(all_te = factor(all_te, levels = rev(unique(
      dieter.tech.mapping
    ))))  %>%
    mutate(iteration = id[i]) %>%
    mutate(period = as.integer(period))
  
  out.dieter <- rbind(out.dieter, dieter.data)
}

dieter.gen.wCurt <- out.dieter %>%
  filter(variable == "total_generation") %>%
  select(period, iteration, all_te, value)


dieter.consumption <- out.dieter %>%
  filter(variable == "total_consumption") %>%
  select(period, iteration, all_te, value)

dieter.gen.wCurt.sum <- out.dieter %>%
  filter(variable == "total_generation") %>%
  select(period, iteration, all_te, value) %>%
  dplyr::group_by(period, iteration) %>%
  dplyr::summarise(value = sum(value) , .groups = 'keep') %>%
  dplyr::ungroup(period, iteration)

dieter.gen.wCurt$tech <- "total generation w/ curtailment"

# Plotting ----------------------------------------------------------------

swlatex(sw, paste0("\\section{Generation}"))

for (year_toplot in report.periods) {
  if(year_toplot >= 2020){
  plot.remind <- out.remind %>%
    filter(period == year_toplot)
  
  plot.remind.generation.withCurt <- remind.generation.withCurt %>%
    filter(period == year_toplot)
  
  plot.remind.consumption <- remind.consumption %>%
    filter(period == year_toplot)
  
  ymax = max(remind.consumption$value) * 2
  ymin = -ymax
  
  if (length(sorted_files_DT) != 0) {
    plot.dieter.usableGen <-
      out.dieter %>% filter(as.integer(period) == year_toplot,
                            variable == "usable_generation")
    plot.dieter.gen.wCurt <-
      dieter.gen.wCurt %>% filter(period == year_toplot)
    plot.dieter.gen.wCurt.sum <-
      dieter.gen.wCurt.sum %>% filter(period == year_toplot)
    plot.dieter.consumption <-
      dieter.consumption %>% filter(period == year_toplot)
    ymax = max(plot.dieter.gen.wCurt.sum$value) * 1.3
    if (max(dieter.consumption$value) == 0) {
      ymin = 0
    } else {
      ymin = -max(plot.dieter.consumption$value) * 1.3
    }
  }
  
  
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
      size = 1.2,
      alpha = 0,
      linetype = "dotted"
    ) +
    geom_area(
      data = plot.remind.consumption ,
      aes(x = iteration, y = -value, fill = all_te),
      size = 1.2,
      alpha = 0.5,
      stat = "identity"
    ) +
    scale_fill_manual(name = "Technology", values = color.mapping1) +
    scale_color_manual(name = "Technology", values = color.mapping1) +
    theme(axis.text = element_text(size = 10),
          axis.title = element_text(size = 10, face = "bold")) +
    xlab("iteration") + ylab(paste0("Usable generation (TWh)")) +
    ggtitle(paste0("REMIND", year_toplot)) +
    coord_cartesian(ylim = c(ymin, ymax), xlim = c(0, max(out.remind$iteration))) +
    theme(
      legend.title = element_blank()
    ) 
  
  
  if (length(sorted_files_DT) != 0) {
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
        size = 1.2,
        alpha = 0,
        linetype = "dotted"
      ) +
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
      ) +
      scale_fill_manual(name = "Technology", values = color.mapping2) +
      scale_color_manual(name = "Technology", values = color.mapping2) +
      theme(axis.text = element_text(size = 10),
            axis.title = element_text(size = 10, face = "bold")) +
      xlab("iteration") + ylab(paste0(c("total_generation", "usable_generation", "total_consumption"), "(TWh)")) +
      coord_cartesian(ylim = c(ymin, ymax), xlim = c(0, max(out.remind$iteration))) +
      ggtitle(paste0("DIETER", year_toplot)) +
      theme(
        legend.title = element_blank()
      ) 
    
  }
  
  
  grid.newpage()
  if (length(sorted_files_DT) != 0) {
    p <- arrangeGrob(rbind(ggplotGrob(p1), ggplotGrob(p2)))
  } else {
    p <- p1
  }
  
  swfigure(sw, grid.draw, p)
}
}

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
    data = plot.remind ,
    aes(x = period, y = value, fill = all_te),
    size = 1.2,
    alpha = 0.5,
    stat = "identity"
  ) +
  geom_area(
    data = plot.remind.generation.withCurt,
    aes(x = period, y = value, color = all_te),
    size = 1.2,
    alpha = 0,
    linetype = "dotted"
  ) +
  geom_area(
    data = plot.remind.consumption,
    aes(x = period, y = -value, fill = all_te),
    size = 1.2,
    alpha = 0.5,
    stat = "identity"
  ) +
  scale_fill_manual(name = "Technology", values = color.mapping1) +
  scale_color_manual(name = "Technology", values = color.mapping1) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10, face = "bold")) +
  xlab("Year") + ylab(paste0("Usable generation (TWh)")) +
  ggtitle(paste0("REMIND (last iteration)")) +
  # coord_cartesian(ylim = c(ymin,ymax), xlim = c(0, max(out.remind$period)))+
  theme(
    legend.title = element_blank()
  ) 


if (length(sorted_files_DT) != 0) {
  p2 <- ggplot() +
    geom_area(
      data = plot.dieter,
      aes(
        x = as.integer(period),
        y = value,
        fill = all_te
      ),
      size = 1.2,
      alpha = 0.5
    ) +
    geom_area(
      data = plot.dieter.gen.wCurt,
      aes(
        x = as.integer(period),
        y = value,
        color = all_te
      ),
      size = 1.2,
      alpha = 0,
      linetype = "dotted"
    ) +
    geom_area(
      data = plot.dieter.consumption,
      aes(
        x = as.integer(period),
        y = -value,
        fill = all_te
      ),
      size = 1.2,
      alpha = 0.5,
      stat = "identity"
    ) +
    scale_fill_manual(name = "Technology", values = color.mapping2) +
    scale_color_manual(name = "Technology", values = color.mapping2) +
    theme(axis.text = element_text(size = 10),
          axis.title = element_text(size = 10, face = "bold")) +
    xlab("Year") + ylab(paste0(c("total_generation", "usable_generation", "total_consumption"), "(TWh)")) +
    #coord_cartesian( xlim = c(min(out.dieter$period), max(out.dieter$period)))+
    ggtitle(paste0("DIETER(last iteration)")) +
    theme(
      legend.title = element_blank()
    ) 
  
}

grid.newpage()
if (length(sorted_files_DT) != 0) {
  p <- arrangeGrob(rbind(ggplotGrob(p1), ggplotGrob(p2)))
} else {
  p <- p1
}

swfigure(sw, grid.draw, p)
