# Data preparation (REMIND) -----------------------------------------------

out.remind.cap <- NULL
out.remind.dem <- NULL
for (i in 1:length(remind.files)){
  
  # Read in vm_cap (capacity)
  remind.vm_cap <- file.path(outputdir, remind.files[i]) %>%  
    read.gdx("vm_cap", field="l", squeeze=F) %>% 
    filter(rlf == 1) %>% 
    select(-rlf) %>%
    filter(all_regi == "DEU") %>% 
    filter(tall %in% report.periods) %>% 
    filter(all_te %in% names(remind.tech.mapping)) %>% 
    revalue.levels(all_te = remind.tech.mapping) %>%
    mutate(all_te = factor(all_te, levels=rev(unique(remind.tech.mapping)))) %>%
    group_by(tall, all_te) %>% 
    summarise(capacity = 1e3*sum(value)) %>% # REMIND capacity is in TW
    ungroup() %>% 
    mutate(iteration = i)
  
  # Read in v32_seelDem (total secondary electricity demand)
  remind.v32_seelDem <- file.path(outputdir, remind.files[i]) %>% 
    read.gdx("v32_seelDem", field="l", squeeze=F) %>% 
    filter(all_regi == "DEU") %>%
    filter(tall %in% report.periods) %>% 
    mutate(demand = 0.000155891 * 8760 * 1e3 * value) %>% 
    mutate(iteration = i)
  
  out.remind.cap <- rbind(out.remind.cap, remind.vm_cap)
  out.remind.dem <- rbind(out.remind.dem, remind.v32_seelDem)
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
    filter(var == "capacity") %>% 
    mutate(capacity = value/1e3) %>% # DIETER capacity is in MW
    select(-var) %>%
    filter(!technology %in% dieter.tech.exclude) %>% 
    revalue.levels(technology = dieter.tech.mapping) %>% 
    mutate(technology = factor(technology, levels=rev(unique(dieter.tech.mapping)))) %>%
    mutate(iteration = dieter.iter.step*i)
  
  out.dieter <- rbind(out.dieter, dieter.data)
}

# Plotting ----------------------------------------------------------------

swlatex(sw, paste0("\\section{Capacities}"))

for(t.rep in report.periods){
  plot.remind.cap <- out.remind.cap %>% 
    filter(tall == t.rep)
  
  plot.remind.dem <- out.remind.dem %>% 
    filter(tall == t.rep)
  
  plot.dieter <- out.dieter %>% 
    filter(tall == t.rep)
  
  swlatex(sw, paste0("\\subsection{Capacities in ", t.rep, "}"))
  
  p1 <- ggplot() + 
    geom_area(data=plot.remind.cap, aes(x=iteration,y=capacity, fill=all_te), alpha=0.5) + 
    scale_fill_manual(name = "Technology", values = color.mapping) + 
    geom_line(data=plot.remind.dem, aes(x=iteration, y=demand, colour="Peak demand"), linetype="dotted") +
    scale_colour_manual(name = "Demand", values=c("Peak demand" = "black")) + 
    coord_cartesian(xlim = c(0, max(plot.remind.cap$iteration))) +
    xlab("Iteration") + 
    ylab("Capacity [GW]") + 
    ggtitle("REMIND")
  
  p2 <- ggplot() +
    geom_area(data=plot.dieter, aes(x=iteration,y=capacity, fill=technology), alpha=0.5) + 
    scale_fill_manual(name = "Technology", values = color.mapping) + 
    geom_line(data=plot.remind.dem, aes(x=iteration, y=demand, colour="Peak demand"), linetype="dotted") +
    scale_colour_manual(name = "Demand", values=c("Peak demand" = "black")) + 
    coord_cartesian(xlim = c(0, max(plot.remind.cap$iteration))) +
    xlab("Iteration") + 
    ylab("Capacity [GW]") + 
    ggtitle("DIETER")
  
  grid.newpage()
  p <- arrangeGrob(rbind(ggplotGrob(p1), ggplotGrob(p2)))
  
  swfigure(sw,grid.draw,p)
}


swlatex(sw, "\\subsection{Capacities over time (last iteration)}")

plot.remind.cap <- out.remind.cap %>% 
  filter(iteration == max(out.remind.cap$iteration))

plot.remind.dem <- out.remind.dem %>% 
  filter(iteration == max(out.remind.dem$iteration))

plot.dieter <- out.dieter %>% 
  filter(iteration == max(out.dieter$iteration))

p1 <- ggplot() + 
  geom_area(data=plot.remind.cap, aes(x=tall, y=capacity, fill=all_te), alpha=0.5) + 
  scale_fill_manual(name = "Technology", values = color.mapping) +
  geom_line(data=plot.remind.dem, aes(x=tall, y=demand, colour="Peak demand"), linetype="dotted") +
  scale_colour_manual(name = "Demand", values=c("Peak demand" = "black")) +
  xlab("Time") + 
  ylab("Capacity [GW]") + 
  ggtitle("REMIND")
  
p2 <- ggplot() + 
  geom_area(data=plot.dieter, aes(x=tall, y=capacity, fill=technology), alpha=0.5) +  
  scale_fill_manual(name = "Technology", values = color.mapping) + 
  geom_line(data=plot.remind.dem, aes(x=tall, y=demand, colour="Peak demand"), linetype="dotted") +
  scale_colour_manual(name = "Demand", values=c("Peak demand" = "black")) +
  xlab("Time") + 
  ylab("Capacity [GW]") + 
  ggtitle("DIETER")

grid.newpage()
p <- arrangeGrob(rbind(ggplotGrob(p1), ggplotGrob(p2)))

swfigure(sw,grid.draw,p)

