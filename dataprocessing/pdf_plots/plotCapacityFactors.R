# Data preparation (REMIND) -----------------------------------------------

out.remind <- NULL
for (i in 1:length(remind.files)){
  
  # Capacity factor for non-VRE ---------------------------------------------
  
  # Read in vm_capFac
  remind.vm_capFac <- paste0(remind.dieter.path, scenario.name, remind.files[i]) %>% 
    read.gdx("vm_capFac", field="l", squeeze=F) %>% 
    rename(tall = ttot) %>% 
    mutate(variable = "vm_capFac")
  
  # Read in vm_cap
  remind.vm_cap <- paste0(remind.dieter.path, scenario.name, remind.files[i]) %>%  
    read.gdx("vm_cap", field="l", squeeze=F) %>% 
    filter(rlf == 1) %>% 
    select(-rlf) %>% 
    mutate(variable = "vm_cap")
  
  # Join both vm_capFac and vm_cap
  remind.data.nonVRE <- rbind(remind.vm_capFac, remind.vm_cap) %>% 
    spread(variable, value) %>%
    filter(all_regi == "DEU") %>% 
    filter(tall %in% report.periods) %>% 
    filter(all_te %in% names(remind.nonvre.mapping)) %>% 
    mutate(technology = all_te) %>% 
    revalue.levels(technology = remind.nonvre.mapping) %>% 
    mutate(generation = vm_cap * vm_capFac) %>% 
    group_by(tall, technology) %>% 
    summarise(capfac = sum(generation)/sum(vm_cap)) %>% 
    mutate(iteration = i)
  
  # Capacity factor for VRE -------------------------------------------------
  
  # Read in pm_dataren with VRE capacity factors over grades
  remind.pm_dataren <- paste0(remind.dieter.path, scenario.name, remind.files[i]) %>% 
    read.gdx("pm_dataren", squeeze=F) %>% 
    filter(all_te %in% names(remind.vre.mapping)) %>% 
    filter(char == "nur") %>% 
    select(-char) %>% 
    rename(capfac = value)
  
  # Read in vm_capDistr with VRE capacity distribution over grades
  remind.vm_capDistr <- paste0(remind.dieter.path, scenario.name, remind.files[i]) %>% 
    read.gdx("vm_capDistr", field="l", squeeze=F) %>% 
    rename(cap = value)
  
  # Join pm_dataren with vm_capDistr and calculate VRE CFs
  remind.data.VRE <- left_join(remind.pm_dataren, remind.vm_capDistr) %>% 
    filter(all_regi == "DEU") %>% 
    filter(tall %in% report.periods) %>%
    rename(technology = all_te) %>% 
    revalue.levels(technology = remind.vre.mapping) %>% 
    mutate(generation = cap * capfac) %>% 
    group_by(tall, technology) %>% 
    summarise(capfac = sum(generation)/sum(cap)) %>% 
    mutate(iteration = i)
  
  out.remind <- rbind(out.remind, remind.data.nonVRE, remind.data.VRE) %>% 
    mutate(model = "REMIND")
}


# Data preparation (DIETER) -----------------------------------------------

dieter.iter.step <- floor(length(remind.files)/length(dieter.files))

out.dieter <- NULL
for (i in 1:length(dieter.files)){
  dieter.data <- paste0(remind.dieter.path, scenario.name, dieter.files[i]) %>% 
    read.gdx("report4RM", squeeze=F) %>% 
    select(X..1, X..3, X..4, value) %>% 
    rename(tall = X..1, technology=X..3, var=X..4, capfac=value) %>%
    mutate(tall = as.numeric(tall)) %>% 
    filter(var == "capfac") %>% 
    select(-var) %>%
    filter(!technology %in% dieter.tech.exclude) %>% 
    revalue.levels(technology = dieter.tech.mapping) %>% 
    mutate(iteration = dieter.iter.step*i) %>% 
    mutate(model = "DIETER")
  
  out.dieter <- rbind(out.dieter, dieter.data)
}


# Plotting ----------------------------------------------------------------

swlatex(sw, paste0("\\section{Capacity factors}"))

for(t.rep in report.periods){
  plot.remind <- out.remind %>% 
    filter(tall == t.rep)
  
  plot.dieter <- out.dieter %>% 
    filter(tall == t.rep)
  
  swlatex(sw, paste0("\\subsection{Capacity factors in ", t.rep, "}"))
  
  p <- ggplot() + 
    geom_line(data=plot.remind, aes(x=iteration, y=capfac, color=model)) + 
    geom_point(data=plot.dieter, aes(x=iteration, y=capfac, color=model)) +
    facet_wrap(~technology, nrow=3)
  
  swfigure(sw,print,p)
}


swlatex(sw, "\\subsection{Capacity factors over time (last iteration)}")

plot.remind <- out.remind %>% 
  filter(iteration == length(remind.files))

p <- ggplot(plot.remind, aes(x=tall, y=capfac)) +
  geom_line() + facet_wrap(~technology, nrow=3)

swfigure(sw,print,p)

