# Data preparation --------------------------------------------------------

# Initialise output files
out.dieter <- NULL

# Loop over DIETER iterations
for (i in 1:length(dieter.files.report)){
  # Read in demand(hour)
  dieter.report_hours <-
    file.path(outputdir, dieter.files.report[i]) %>%
    read.gdx("report_hours", squeeze = F) %>%
    rename(tall = X..2, var = X..4, hour = X..5) %>%
    select(!c(X., X..1, X..3)) %>%
    filter(var == "price") %>%
    mutate(hour = as.numeric(substring(hour, 2))) %>%
    group_by(tall) %>%
    arrange(desc(value),group_by=T) %>%  # Descending order
    mutate(hour.sorted = seq(1, 8760)) %>%  # Descending hour (sorted)
    mutate(iteration = dieter.iter.step*i) %>%
    ungroup()
  
  out.dieter <- rbind(out.dieter, dieter.report_hours)
}

# Plotting ----------------------------------------------------------------

swlatex(sw,"\\onecolumn")
swlatex(sw, paste0("\\section{Price duration curves}"))

for(t.rep in report.periods){
  plot.dieter <- out.dieter %>% 
    filter(tall==t.rep)
  
  swlatex(sw, paste0("\\subsection{Price duration curve in ", t.rep, " over iterations}"))
  
  p <- ggplot() +
    geom_line(data = plot.dieter, aes(x = hour.sorted, y = value)) +
    xlab("Hours (sorted)") + 
    ylab("Electricity price [$/MWh]") +
    scale_y_continuous(trans = 'log10') +
    facet_wrap(~iteration,ncol=4)
  
  swfigure(sw,print,p,sw_option="width=20, height=10")
}
