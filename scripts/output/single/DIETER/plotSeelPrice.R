# Data preparation --------------------------------------------------------

out.remind.seel <- NULL
for (i in 1:length(remind.files)){
  remind.q32_balSe <- file.path(outputdir, remind.files[i]) %>% 
    read.gdx("q32_balSe", field="m", squeeze=F) %>% 
    filter(all_regi == "DEU") %>%
    select(!all_regi) %>% 
    filter(ttot %in% report.periods.long) %>% 
    select(!all_enty) %>%
    rename(tall = ttot) %>% 
    rename(q32_balSe.m = m)
  
  remind.qm_budget <- file.path(outputdir, remind.files[i]) %>% 
    read.gdx("qm_budget", field="m", squeeze=F) %>% 
    filter(all_regi == "DEU") %>%
    select(!all_regi) %>%  
    filter(ttot %in% report.periods.long) %>% 
    rename(tall = ttot) %>% 
    rename(qm_budget.m = m)
  
  remind.seel <- left_join(remind.q32_balSe, remind.qm_budget) %>% 
    mutate(seel.price = 1.2 * 1e12 / sm_TWa_2_MWh * q32_balSe.m / qm_budget.m) %>%  # (10^12 2005$)/TWa -> 2015$/MWh
    mutate(iteration = i)
  
  out.remind.seel <- rbind(out.remind.seel, remind.seel)
}


# Plotting ----------------------------------------------------------------

swlatex(sw,"\\onecolumn")
swlatex(sw, paste0("\\section{Secondary electricity (seel) price + capacity factors}"))

swlatex(sw, paste0("\\subsection{Seel price and capacity factor over iterations}"))

p <- ggplot() +
  geom_line(data=out.remind.seel, aes(x=iteration, y=seel.price, size="Seel")) + 
  scale_size_manual(name="Shadow price", values=1) + 
  geom_line(data=out.remind.capfac, aes(x=iteration, y=2.5*100*capfac, colour=technology)) +
  scale_colour_manual(name = "Capacity factor", values = color.mapping) +
  scale_y_continuous(name="Seel price [$/MWh]", limits=c(0,250), sec.axis = sec_axis(~./2.5, name = paste0("CF", "(%)"))) +
  theme(legend.position = "bottom") +
  xlab("Iteration") + 
  facet_wrap(~tall, ncol=4)

swfigure(sw,print,p,sw_option="width=20, height=12")

swlatex(sw,"\\twocolumn")
swlatex(sw, paste0("\\subsection{Seel price and capacity factor over time (last iteration)}"))

plot.remind.seel <- out.remind.seel %>% 
  filter(iteration == max(iteration))

plot.remind.capfac <- out.remind.capfac %>% 
  filter(iteration == max(iteration))

p <- ggplot() + 
  geom_line(data=plot.remind.seel, aes(x=tall, y=seel.price, size="Seel")) +
  scale_size_manual(name="Shadow price", values=1) + 
  geom_line(data=plot.remind.capfac, aes(x=tall, y=2.5*100*capfac, colour=technology)) +
  scale_colour_manual(name = "Capacity factor", values = color.mapping) +
  scale_y_continuous(name="Seel price [$/MWh]", limits=c(0,250), sec.axis = sec_axis(~./2.5, name = paste0("CF", "(%)"))) +
  theme(legend.position = "bottom") +
  xlab("Time")

swfigure(sw,print,p)

