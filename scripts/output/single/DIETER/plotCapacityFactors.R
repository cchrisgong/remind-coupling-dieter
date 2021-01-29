# Data preparation (REMIND) -----------------------------------------------

cat("Plot capacity factors \n")

out.remind.capfac <- NULL
for (i in 1:length(remind.files)){
  
  # Capacity factor for non-VRE ---------------------------------------------
  
  # Read in vm_capFac
  remind.vm_capFac <- file.path(outputdir, remind.files[i]) %>% 
    read.gdx("vm_capFac", field="l", squeeze=F) %>% 
    rename(tall = ttot) %>% 
    mutate(variable = "vm_capFac")
  
  # Read in vm_cap
  remind.vm_cap <- file.path(outputdir, remind.files[i]) %>%  
    read.gdx("vm_cap", field="l", squeeze=F) %>% 
    filter(rlf == 1) %>% 
    select(-rlf) %>% 
    mutate(variable = "vm_cap")
  
  # Join both vm_capFac and vm_cap
  remind.data.nonVRE <- rbind(remind.vm_capFac, remind.vm_cap) %>% 
    spread(variable, value) %>%
    filter(all_regi == "DEU") %>% 
    filter(tall %in% report.periods.long) %>% 
    filter(all_te %in% names(remind.nonvre.mapping)) %>% 
    mutate(technology = all_te) %>% 
    revalue.levels(technology = remind.nonvre.mapping) %>% 
    mutate(generation = vm_cap * vm_capFac) %>% 
    group_by(tall, technology) %>% 
    summarise(capfac = sum(generation)/sum(vm_cap)) %>% 
    mutate(iteration = i)
  
  # Capacity factor for VRE -------------------------------------------------
  
  # Read in pm_dataren with VRE capacity factors over grades
  remind.pm_dataren <- file.path(outputdir, remind.files[i]) %>% 
    read.gdx("pm_dataren", squeeze=F) %>% 
    filter(all_te %in% names(remind.vre.mapping)) %>% 
    filter(char == "nur") %>% 
    select(-char) %>% 
    rename(capfac = value)
  
  # Read in vm_capDistr with VRE capacity distribution over grades
  remind.vm_capDistr <- file.path(outputdir, remind.files[i]) %>% 
    read.gdx("vm_capDistr", field="l", squeeze=F) %>% 
    rename(cap = value)
  
  # Join pm_dataren with vm_capDistr and calculate VRE CFs
  remind.data.VRE <- left_join(remind.pm_dataren, remind.vm_capDistr) %>% 
    filter(all_regi == "DEU") %>% 
    filter(tall %in% report.periods.long) %>%
    rename(technology = all_te) %>% 
    revalue.levels(technology = remind.vre.mapping) %>% 
    mutate(generation = cap * capfac) %>% 
    group_by(tall, technology) %>% 
    summarise(capfac = sum(generation)/sum(cap)) %>% 
    mutate(iteration = i)
  
  out.remind.capfac <- rbind(out.remind.capfac, remind.data.nonVRE, remind.data.VRE) %>% 
    mutate(model = "REMIND")
}


# Data preparation (DIETER) -----------------------------------------------

out.dieter.capfac <- NULL
for (i in 1:length(dieter.files)){
  dieter.data <- file.path(outputdir, dieter.files[i]) %>% 
    read.gdx("report4RM", squeeze=F, colNames=c("file", "tall", "all_regi", "technology", "var", "value")) %>% 
    select(!c(file, all_regi)) %>% 
    filter(tall %in% report.periods.long) %>% 
    mutate(tall = as.numeric(as.character(tall))) %>%
    filter(var == "capfac") %>% 
    filter(!technology %in% dieter.tech.exclude) %>% 
    revalue.levels(technology = dieter.tech.mapping) %>% 
    mutate(iteration = dieter.iter.step*i) %>% 
    mutate(model = "DIETER")
  
  out.dieter.capfac <- rbind(out.dieter.capfac, dieter.data)
}


# Plotting ----------------------------------------------------------------

swlatex(sw, paste0("\\section{Capacity factors}"))

for(t.rep in report.periods){
  plot.remind <- out.remind.capfac %>% 
    filter(tall == t.rep)
  
  plot.dieter <- out.dieter.capfac %>% 
    filter(tall == t.rep) %>% 
    filter(!technology %in% c("Lignite", "Hard coal"))
  
  swlatex(sw, paste0("\\subsection{Capacity factors in ", t.rep, "}"))
  
  p <- ggplot() + 
    geom_line(data=plot.remind, aes(x=iteration, y=capfac, color=model)) + 
    geom_point(data=plot.dieter, aes(x=iteration, y=value, color=model)) +
    xlab("Iteration") + 
    ylab("Capacity factor") + 
    facet_wrap(~technology, nrow=3)
  
  swfigure(sw,print,p)
}


swlatex(sw, "\\subsection{Capacity factors over time (last iteration)}")

plot.remind <- out.remind.capfac %>% 
  filter(iteration == max(iteration))

plot.dieter <- out.dieter.capfac %>% 
  filter(iteration == max(iteration)) %>% 
  filter(!technology %in% c("Lignite", "Hard coal"))

p <- ggplot() + 
  geom_line(data=plot.remind, aes(x=tall, y=capfac, color=model)) +
  geom_line(data=plot.dieter, aes(x=tall, y=value, color=model)) +
  facet_wrap(~technology, nrow=3) +
  xlab("Time") + 
  ylab("Capacity factor")

swfigure(sw,print,p)

