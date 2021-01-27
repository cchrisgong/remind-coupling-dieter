# Data preparation (REMIND) -----------------------------------------------

out.remind <- NULL
for (i in 1:length(remind.files)){
  
  # Read in vm_prodSe (generation)
  remind.vm_prodSe <- file.path(outputdir, remind.files[i]) %>%  
    read.gdx("vm_prodSe", field="l", squeeze=F) %>% 
    filter(all_enty.1 == "seel") %>% 
    filter(all_regi == "DEU") %>% 
    filter(tall %in% report.periods) %>% 
    filter(all_te %in% names(remind.tech.mapping)) %>% 
    revalue.levels(all_te = remind.tech.mapping) %>%
    mutate(all_te = factor(all_te, levels=rev(unique(remind.tech.mapping)))) %>%
    group_by(tall, all_te) %>% 
    summarise(generation = 8760*sum(value)) %>% # TWa ->TWh 
    ungroup() %>% 
    mutate(iteration = i)
  
  out.remind <- rbind(out.remind, remind.vm_prodSe)
}


# Data preparation (DIETER) -----------------------------------------------

out.dieter <- NULL
for (i in 1:length(dieter.files)){
  dieter.data <- file.path(outputdir, dieter.files[i]) %>% 
    read.gdx("report4RM", squeeze=F) %>% 
    select(X..1, X..3, X..4, value) %>% 
    rename(tall = X..1, technology=X..3, var=X..4) %>%
    mutate(tall = as.numeric(tall)) %>% 
    filter(tall %in% report.periods) %>% 
    filter(var == "generation") %>% 
    mutate(generation = value/1e6) %>% # MWh -> TWh
    select(-var) %>%
    filter(!technology %in% dieter.tech.exclude) %>% 
    revalue.levels(technology = dieter.tech.mapping) %>% 
    mutate(technology = factor(technology, levels=rev(unique(dieter.tech.mapping)))) %>%
    mutate(iteration = dieter.iter.step*i)
  
  out.dieter <- rbind(out.dieter, dieter.data)
}

# Plotting ----------------------------------------------------------------

swlatex(sw, paste0("\\section{Generation}"))

for(t.rep in report.periods){
  plot.remind <- out.remind %>% 
    filter(tall == t.rep)
  
  plot.dieter <- out.dieter %>% 
    filter(tall == t.rep)
  
  swlatex(sw, paste0("\\subsection{Generation in ", t.rep, "}"))
  
  p1 <- ggplot() + 
    geom_area(data=plot.remind, aes(x=iteration,y=generation, fill=all_te), alpha=0.5) + 
    scale_fill_manual(name = "Technology", values = color.mapping) + 
    coord_cartesian(xlim = c(0, max(plot.remind$iteration))) +
    xlab("Iteration") + 
    ylab("Generation [TWh]") + 
    ggtitle("REMIND")
  
  p2 <- ggplot() +
    geom_area(data=plot.dieter, aes(x=iteration,y=generation, fill=technology), alpha=0.5) + 
    scale_fill_manual(name = "Technology", values = color.mapping) + 
    coord_cartesian(xlim = c(0, max(plot.remind$iteration))) +
    xlab("Iteration") + 
    ylab("Generation [TWh]") + 
    ggtitle("DIETER")
  
  grid.newpage()
  p <- arrangeGrob(rbind(ggplotGrob(p1), ggplotGrob(p2)))
  
  swfigure(sw,grid.draw,p)
}


swlatex(sw, "\\subsection{Generation over time (last iteration)}")

plot.remind <- out.remind %>% 
  filter(iteration == max(out.remind$iteration))

plot.dieter <- out.dieter %>% 
  filter(iteration == max(out.dieter$iteration))

p1 <- ggplot() + 
  geom_area(data=plot.remind, aes(x=tall, y=generation, fill=all_te), alpha=0.5) + 
  scale_fill_manual(name = "Technology", values = color.mapping) +
  xlab("Time") + 
  ylab("Generation [TWh]") + 
  ggtitle("REMIND")
  
p2 <- ggplot() + 
  geom_area(data=plot.dieter, aes(x=tall, y=generation, fill=technology), alpha=0.5) +  
  scale_fill_manual(name = "Technology", values = color.mapping) + 
  xlab("Time") + 
  ylab("Generation [TWh]") + 
  ggtitle("DIETER")

grid.newpage()
p <- arrangeGrob(rbind(ggplotGrob(p1), ggplotGrob(p2)))

swfigure(sw,grid.draw,p)

