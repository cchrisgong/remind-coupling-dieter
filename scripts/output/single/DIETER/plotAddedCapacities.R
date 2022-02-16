# Data preparation --------------------------------------------------------

cat("Plot added capacities \n")

dieter.report.cap <- c("DIETER pre-investment capacities (GW)","REMIND pre-investment capacities (GW)")
dieter.report.addcap <- c("DIETER added capacities (GW)", "REMIND added capacities (GW)")
dieter.report.divest <- c("REMIND divestment (GW)")

dieter.report.vars <- c(dieter.report.cap, dieter.report.addcap, dieter.report.divest)

out.dieter.report <- NULL
for (i in 1:length(dieter.files.report)){
  dieter.data <- file.path(outputdir, dieter.files.report[i]) %>% 
    read.gdx("report_tech", squeeze=F) %>% 
    select(model=X..1, tall = X..2, var=X..4, tech=X..5, value) %>%
    filter(var %in% dieter.report.vars) %>% 
    revalue.levels(tech = dieter.tech.mapping) %>%
    mutate(variable = case_when(var %in% dieter.report.cap ~ "Pre-inv. cap.",
                                var %in% dieter.report.addcap ~ "Added cap.",
                                var %in% dieter.report.divest ~ "Divestment")) %>% 
    mutate(variable = factor(variable, levels=rev(c("Pre-inv. cap.", "Added cap.", "Divestment")))) %>% 
    mutate(iteration = i)
  
  out.dieter.report <- rbind(out.dieter.report, dieter.data)
}


# Plotting ----------------------------------------------------------------

swlatex(sw,"\\onecolumn")
swlatex(sw, paste0("\\section{Added capacities}"))

for(iter.rep in 1:round(length(dieter.files.report)/dieter.iter.step-1, 0)){

  swlatex(sw, paste0("\\subsection{Added capacities in iteration ", iter.rep*dieter.iter.step, "}"))
  
  plot.dieter <- out.dieter.report %>%
    filter(model == "DIETER") %>% 
    filter(tall %in% model.periods.till2100) %>%
    filter(iteration == iter.rep*dieter.iter.step) %>% 
    mutate(tall = as.numeric(as.character(tall)) - 1)   # Shift for dodged plot
    
  
  plot.remind <- out.dieter.report %>% 
    filter(model == "REMIND") %>% 
    filter(tall %in%model.periods.till2100) %>%
    filter(iteration == iter.rep*dieter.iter.step) %>% 
    mutate(tall = as.numeric(as.character(tall)) + 1) %>%  # Shift for dodged plot
    mutate(value = ifelse(variable == "Divestment", -value, value))  # Divestment has negative value
  
  p <- ggplot() +
    geom_bar(data = plot.dieter, aes(x=tall, y=value, fill=model, alpha=variable), colour="black", stat="identity", position="stack", width=2) + 
    geom_bar(data = plot.remind, aes(x=tall, y=value, fill=model, alpha=variable), colour="black", stat="identity", position="stack", width=2) +
    scale_alpha_manual(values=c("Pre-inv. cap."= 1, "Added cap."=0.5, "Divestment"=0.2), limits=c("Pre-inv. cap.", "Added cap.", "Divestment")) +
    facet_wrap(~tech, scales="free") +
    coord_cartesian(xlim = c(2010, 2100)) +
    theme(legend.position="bottom") + 
    xlab("Time") + 
    ylab("Capacity [GW]")
  
  swfigure(sw,print,p, sw_option="width=20, height=10")
  
  if (save_cfg == 1){
    ggsave(filename = paste0(outputdir, "/AddedCapacity_compare_i", iter.rep*dieter.iter.step, ".png"),  p,  width = 17, height =10, units = "in", dpi = 120)
  }
}
swlatex(sw,"\\twocolumn")