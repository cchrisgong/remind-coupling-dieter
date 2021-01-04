# Data preparation --------------------------------------------------------

dieter.report.lcoe.kW <- c("annualized investment cost", "O&M cost")
dieter.report.lcoe.MWh <- c("fuel cost (divided by eta)", "CO2 cost")
dieter.report.mv <- "DIETER Market value ($/MWh)"

dieter.report.vars <- c(dieter.report.lcoe.kW, dieter.report.lcoe.MWh, dieter.report.mv)

out.dieter.report <- NULL
for (i in 1:length(dieter.files.report)){
  dieter.data <- paste0(remind.dieter.path, scenario.name, dieter.files.report[i]) %>% 
    read.gdx("report_tech", squeeze=F) %>% 
    rename(model=X..1, tall = X..2, var=X..4, technology=X..5) %>%
    filter(var %in% dieter.report.vars) %>% 
    filter(tall %in% report.periods) %>% 
    mutate(var = factor(var, levels=rev(dieter.report.vars))) %>% 
    filter(!technology %in% c(dieter.tech.exclude, "coal")) %>% 
    revalue.levels(technology = dieter.tech.mapping) %>% 
    mutate(iteration = dieter.iter.step*i)
  
  out.dieter.report <- rbind(out.dieter.report, dieter.data)
}

out.dieter.lcoe <- out.dieter.report %>% 
  filter(var %in% c(dieter.report.lcoe.kW, dieter.report.lcoe.MWh)) %>%
  select(!c(X., X..3)) %>% 
  rbind(out.dieter.capfac) %>% 
  spread(var,value) %>%
  mutate(IC = 1e3 * `annualized investment cost`/(capfac*8760)) %>% 
  mutate(OM = 1e3 * `O&M cost`/(capfac*8760)) %>% 
  mutate(FC = `fuel cost (divided by eta)`) %>% 
  mutate(CO2 = `CO2 cost`) %>% 
  gather(c("IC", "OM", "FC", "CO2"), key="var", value="cost") %>% 
  select(c(model, tall, technology, iteration, var, cost)) %>% 
  filter(!technology=="Coal (Lig + HC)") %>% 
  mutate(var = factor(var, levels=rev(c("IC", "OM", "FC", "CO2"))))
  
out.dieter.mv <- out.dieter.report %>% 
  filter(var %in% dieter.report.mv)


# Plotting ----------------------------------------------------------------

swlatex(sw,"\\onecolumn")
swlatex(sw, paste0("\\section{Levelised cost of electricity (LCOE)}"))

for(t.rep in report.periods){

  swlatex(sw, paste0("\\subsection{LCOEs in ", t.rep, "}"))
  
  plot.dieter.lcoe <- out.dieter.lcoe %>%
    filter(tall == t.rep)

  plot.dieter.mv <- out.dieter.mv %>%
    filter(tall == t.rep)
  
  plot.dieter.capfac <- out.dieter.capfac %>% 
    filter(tall == t.rep) %>% 
    filter(!technology=="Coal (Lig + HC)")
  
  p <- ggplot() + 
    geom_bar(data=plot.dieter.lcoe, aes(x=iteration, y=cost, fill=var), stat="identity", position="stack") + 
    scale_fill_discrete(name="LCOE components", labels=c("Annualised investment", "O&M", "Fuel", "CO2"), limits=c("IC", "OM", "FC", "CO2")) + 
    geom_point(data=plot.dieter.mv, aes(x=iteration, y=value, colour="Market value")) + 
    scale_colour_manual(values="black", name=NULL) +
    geom_text(data=plot.dieter.capfac, aes(x=iteration, y=350, label=paste0(100*round(value,2),"%"))) +
    theme(legend.position="bottom") +
    xlab("Iteration") + 
    ylab("LCOE [$/MWh]") +
    coord_cartesian(ylim = c(0, 400)) +
    facet_wrap(~technology)
  
  swfigure(sw,print,p, sw_option="width=20, height=10")
}

swlatex(sw, paste0("\\subsection{LCOEs over time (last iteration)}"))

plot.dieter.lcoe <- out.dieter.lcoe %>% 
  filter(iteration == max(iteration)) %>% 
  mutate(tall = as.numeric(tall))

plot.dieter.mv <- out.dieter.mv %>% 
  filter(iteration == max(iteration)) %>% 
  mutate(tall = as.numeric(tall))

plot.dieter.capfac <- out.dieter.capfac %>% 
  filter(iteration == max(iteration)) %>% 
  filter(tall %in% report.periods) %>% 
  filter(!technology == "Coal (Lig + HC)")

p <- ggplot() + 
  geom_bar(data=plot.dieter.lcoe, aes(x=tall, y=cost, fill=var), stat="identity", position="stack") + 
  scale_fill_discrete(name="LCOE components", labels=c("Annualised investment", "O&M", "Fuel", "CO2"), limits=c("IC", "OM", "FC", "CO2")) + 
  geom_point(data=plot.dieter.mv, aes(x=tall, y=value, colour="Market value")) + 
  scale_colour_manual(values="black", name=NULL) +
  geom_text(data=plot.dieter.capfac, aes(x=tall, y=350, label=paste0(100*round(value,2),"%"))) +
  theme(legend.position="bottom") +
  xlab("Time") + 
  ylab("LCOE [$/MWh]") +
  coord_cartesian(ylim = c(0, 400)) +
  facet_wrap(~technology, scale="free")

swfigure(sw,print,p, sw_option="width=20, height=10")

swlatex(sw,"\\twocolumn")