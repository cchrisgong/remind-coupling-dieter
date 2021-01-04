# Data preparation --------------------------------------------------------

out.remind.peakdem <- NULL
for (i in 1:length(remind.files)){
  remind.q32_peakDemand_DT <- paste0(remind.dieter.path, scenario.name, remind.files[i]) %>% 
    read.gdx("q32_peakDemand_DT", field="m", squeeze=F) %>% 
    select(!all_enty) %>% 
    filter(tall %in% report.periods.long) %>% 
    rename(q32_peakDemand_DT.m = m)

  remind.qm_budget <- paste0(remind.dieter.path, scenario.name, remind.files[i]) %>% 
    read.gdx("qm_budget", field="m", squeeze=F) %>% 
    filter(all_regi == "DEU") %>%
    select(!all_regi) %>%  
    filter(ttot %in% report.periods.long) %>% 
    rename(tall = ttot) %>% 
    rename(qm_budget.m = m)
  
  remind.peakdem <- left_join(remind.q32_peakDemand_DT, remind.qm_budget) %>% 
    mutate(peakdem.price = 1.2 * 1e3 * q32_peakDemand_DT.m / qm_budget.m) %>%  # (10^12 2005$)/TW-> 2015$/kW
    mutate(iteration = i)
  
  out.remind.peakdem <- rbind(out.remind.peakdem, remind.peakdem)
}


# Plotting ----------------------------------------------------------------

swlatex(sw,"\\onecolumn")
swlatex(sw, paste0("\\section{Peak demand (capital constraint) shadow price}"))

swlatex(sw, paste0("\\subsection{Peak demand shadow price over iterations}"))

p <- ggplot() +
  geom_line(data=out.remind.peakdem, aes(x=iteration, y=peakdem.price, size="Peak demand (capital constraint)")) + 
  scale_size_manual(name="Shadow price", values=1) + 
  theme(legend.position = "bottom") +
  xlab("Iteration") + 
  ylab("Shadow price [$/kW]") +
  facet_wrap(~tall, ncol=4)

swfigure(sw,print,p,sw_option="width=20, height=12")

swlatex(sw,"\\twocolumn")
swlatex(sw, paste0("\\subsection{Peak demand shadow price over time (last iteration)}"))

plot.remind.peakdem <- out.remind.peakdem %>% 
  filter(iteration == max(iteration))

p <- ggplot() + 
  geom_line(data=plot.remind.peakdem, aes(x=tall, y=peakdem.price, size="Peak demand (capital constraint)")) +
  scale_size_manual(name="Shadow price", values=1) + 
  theme(legend.position = "bottom") +
  xlab("Time") + 
  ylab("Shadow price [$/kW]")

p

swfigure(sw,print,p)
