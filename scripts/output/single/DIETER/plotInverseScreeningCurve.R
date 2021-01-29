# Data preparation --------------------------------------------------------

cat("Plot inverse screening curve \n")

dieter.report.lcoe.kW <- c("annualized investment cost", "O&M cost")
dieter.report.lcoe.MWh <- c("fuel cost (divided by eta)", "CO2 cost")
dieter.report.vars <- c(dieter.report.lcoe.kW, dieter.report.lcoe.MWh)

report.tech <- c("CCGT", "OCGT", "Lignite", "Hard coal", "Biomass")

# Initialise output files
out.dieter <- NULL
# Loop over DIETER iterations
for (i in 1:length(dieter.files.report)){
  dieter.report_tech <-
    file.path(outputdir, dieter.files.report[i]) %>%
    read.gdx("report_tech", squeeze = F) %>%
    rename(tall = X..2, var = X..4, technology = X..5) %>%
    select(!c(X., X..1, X..3)) %>%
    filter(var %in% dieter.report.vars) %>%
    revalue.levels(technology = dieter.tech.mapping) %>%
    filter(technology %in% report.tech) %>% 
    spread(var, value) %>% 
    rename(IC = `annualized investment cost`, OM = `O&M cost`, FC = `fuel cost (divided by eta)`, CO2 = `CO2 cost`) %>% 
    replace_na(list(CO2=0, FC=0))

  dieter.report_tech_hours <-
    file.path(outputdir, dieter.files.report[i]) %>%
    read.gdx("report_tech_hours", squeeze = F) %>%
    rename(tall = X..2, var = X..4, technology = X..5, hour = X..6) %>%
    filter(var=="generation") %>% 
    select(!c(X., X..1, X..3,var)) %>%
    revalue.levels(technology = dieter.tech.mapping) %>%
    mutate(hour = as.numeric(substring(hour, 2))) %>% 
    filter(technology %in% report.tech) %>% 
    complete(technology, tall, hour = 1:8760, fill = list(value = NA)) %>%   # Fill up missing hours with 0
    group_by(tall,technology) %>% 
    arrange(desc(value)) %>% 
    mutate(hour.sorted = 1:8760) %>% 
    filter(!is.na(value))
  
  dieter.report <- full_join(dieter.report_tech, dieter.report_tech_hours) %>% 
    mutate(screening = hour.sorted*(FC + CO2)/1e3 + (IC + OM)) %>%  # $/kW
    mutate(inv.screening = (FC + CO2) + (IC + OM)/hour.sorted*1e3) %>%  # $/MWh
    mutate(iteration=dieter.iter.step*i)
  
  out.dieter <- rbind(out.dieter, dieter.report)
}

# Plotting ----------------------------------------------------------------

swlatex(sw,"\\onecolumn")
swlatex(sw, paste0("\\section{Screening curves}"))

for(i in unique(out.dieter$iteration)){
  plot.dieter <- out.dieter %>% 
    filter(iteration==i) %>% 
    filter(hour.sorted %in% c(seq(1,8760,20),8760))  # Only plot every 20-th hour to decrease file size 

  swlatex(sw, paste0("\\subsection{Screening curves in iteration ", i, "}"))
  
  p <- ggplot() + 
    geom_line(data=plot.dieter, aes(x=hour.sorted, y=screening, colour=technology),size=1) +
    scale_colour_manual(name = "Technology", values = color.mapping) +
    xlab("Hours (sorted)") + 
    ylab("Total cost [$/kW]") +
    facet_wrap(~tall)
  
  swfigure(sw,print,p,sw_option="width=20, height=10")
}

swlatex(sw, paste0("\\section{Inverse screening curves}"))

for(i in unique(out.dieter$iteration)){
  plot.dieter <- out.dieter %>% 
    filter(iteration==i) %>% 
    filter(hour.sorted %in% c(seq(1,8760,20),8760))  # Only plot every 20-th hour to decrease file size 
  
  swlatex(sw, paste0("\\subsection{Inverse screening curves in iteration ", i, "}"))
  
  p <- ggplot() + 
    geom_line(data=plot.dieter, aes(x=hour.sorted, y=inv.screening, colour=technology),size=1) +
    scale_colour_manual(name = "Technology", values = color.mapping) +
    scale_y_continuous(limits=c(0,200)) +     
    xlab("Hours (sorted)") + 
    ylab("Total cost [$/MWh]") +
    facet_wrap(~tall)
  
  swfigure(sw,print,p,sw_option="width=20, height=10")
}