# Data preparation --------------------------------------------------------

cat("Plot convergence variables \n")

# REMIND -------------------------------------------------
# generation shares

# REMIND total system markup from DIETER - difference to last iteration, this should go to 0 
# as iterative coupling continues
out.remind.total.sys.markup.diff <- NULL
out.remind.qm_budget.marg <- NULL

for (i in 2:length(remind.files)){

  remind.qm_budget.marg <- file.path(outputdir, remind.files[i]) %>%
    read.gdx("qm_budget", field="m", squeeze=F) %>%
    filter(all_regi == reg) %>%
    filter(ttot %in% model.periods) %>%
    select(period = ttot, qm_budget.m = m) %>%
    mutate(iteration = i)

  remind.total.sys.markup.diff <- file.path(outputdir, remind.files[i]) %>%
    read.gdx("v21_taxrevMrkup", field="l", squeeze=F)  %>%
    select(period = ttot, value) %>%
    filter(period %in% model.periods) %>%
    left_join(remind.qm_budget.marg) %>%
    mutate(value = value/qm_budget.m * 1e12 / 1e9 * 1.2) %>% # (10^12 2005$)/TW-> 2015$/kW
    replace(is.na(.), 0) %>%
    select(period,value) %>%
    mutate(iteration = i)

  out.remind.total.sys.markup.diff <- rbind(out.remind.total.sys.markup.diff, remind.total.sys.markup.diff)
  out.remind.qm_budget.marg <- rbind(out.remind.qm_budget.marg, remind.qm_budget.marg)
}

# DIETER --------------------------------------------------

dieter.report.gensh <- c("genshares (%)",
                      NULL)

out.dieter.report.gensh <- NULL

for (i in 1:length(dieter.files.report)){
  it <- as.numeric(str_extract(dieter.files[i], "[0-9]+"))
  dieter.gensh <- file.path(outputdir, dieter.files.report[i]) %>% 
    read.gdx("report_tech", squeeze=F) %>% 
    select(model=X..1, period = X..2, var=X..4, tech=X..5, value) %>% 
    filter(model == "DIETER") %>% 
    filter(var %in% dieter.report.gensh) %>% 
    filter(period %in% model.periods) %>% 
    revalue.levels(tech = dieter.tech.mapping) %>%
    mutate(iteration = it) 
  
  out.dieter.report.gensh <- rbind(out.dieter.report.gensh, dieter.gensh)
}


# Plotting ----------------------------------------------------------------

swlatex(sw,"\\onecolumn")
swlatex(sw, paste0("\\section{Convergence variables - iteration series}"))
##################################################################################################
swlatex(sw, paste0("\\subsection{Generation share over iterations}"))

plot.remind.genshare <- out.remind.genshare %>% 
  mutate(model = "REMIND")

p<-ggplot() +
  geom_line(data = plot.remind.genshare, aes(x = iteration, y = genshare, color = tech,linetype=model), size = 1.2, alpha = 0.5) +
  geom_line(data = out.dieter.report.gensh, aes(x = iteration, y = value, color = tech,linetype=model), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size= 20,face="bold")) +
  xlab("iteration") + ylab(paste0("generation share (%)"))  +
  scale_color_manual(name = "tech", values = color.mapping)+
  scale_linetype_manual(name = "model", values = linetype.map)+
  theme(legend.title = element_text(size=25),legend.text = element_text(size=25))+
  coord_cartesian(ylim = c(0,60))+
  theme(legend.text = element_text(size=20), strip.text = element_text(size = 20))+
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  facet_wrap(~period, nrow = 3)

swfigure(sw,print,p,sw_option="width=20, height=12")
if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/Generation_share_convergence_iteration.png"),  p,  width = 24, height =12, units = "in", dpi = 120)
}

##################################################################################################
swlatex(sw, paste0("\\subsection{Generation share difference over iterations}"))

remind.gensh <- plot.remind.genshare %>% 
  mutate(period = as.factor(period)) %>% 
  select(period, tech, genshareRM = genshare, iteration)

diff.gensh <- out.dieter.report.gensh %>% 
  select(period, tech, genshareDT = value, iteration) %>% 
  # mutate(period = as.numeric(period)) %>% 
  left_join(remind.gensh) %>% 
  mutate(delGenshare = genshareRM - genshareDT)

p <-ggplot() +
  geom_line(data = diff.gensh, aes(x = iteration, y = delGenshare, color = tech), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size= 20,face="bold")) +
  xlab("iteration") + ylab(paste0("Difference of generation share (REMIND-DIETER) (%)"))  +
  scale_color_manual(name = "tech", values = color.mapping) +
  theme(legend.title = element_text(size=25),legend.text = element_text(size=25)) +
  theme(legend.text = element_text(size=20), strip.text = element_text(size = 20)) +
  coord_cartesian(ylim = c(-10,10)) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  facet_wrap(~period, nrow = 3)

swfigure(sw,print,p,sw_option="width=20, height=12")
if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/Diff_generation_share_convergence_iteration.png"),  p,  width = 24, height =12, units = "in", dpi = 120)
}

##################################################################################################
swlatex(sw, paste0("\\subsection{Electricity price difference over iterations}"))

diff.price <- out.RMprice %>% 
  filter(period %in% model.periods) %>% 
  filter(!value == 0) %>% 
  select(period,iteration,rmprice=value) %>% 
  left_join(out.DTprice) %>% 
  select(period,iteration,rmprice, value) %>% 
  mutate(value = rmprice - value)

p <-ggplot() +
  geom_line(data = diff.price, aes(x = iteration, y = value,), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size= 20,face="bold")) +
  xlab("iteration") + ylab(paste0("Difference of electricity price (REMIND-DIETER)\n($/MWh)"))  +
  theme(legend.title = element_text(size=25),legend.text = element_text(size=25)) +
  theme(legend.text = element_text(size=20), strip.text = element_text(size = 20)) +
  # coord_cartesian(ylim = c(-10,10)) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  facet_wrap(~period, nrow = 3)

swfigure(sw,print,p,sw_option="width=20, height=12")
if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/Diff_elec_price_convergence_iteration.png"),  p,  width = 24, height =12, units = "in", dpi = 120)
}

##################################################################################################
swlatex(sw, paste0("\\subsection{Electricity price difference (time average) over iterations}"))

diff.price.avg.yr <- diff.price %>% 
  filter(period %in% model.periods.till2100) %>% 
  dplyr::group_by(iteration) %>%
  dplyr::summarise( value = mean(value), .groups = "keep" ) %>% 
  dplyr::ungroup(iteration) %>% 
  mutate(variable = "Difference of electricity price")

# moving average
diff.price.avg.yr.movingavg <-diff.price.avg.yr %>% 
  mutate( value = frollmean(value, 3, align = "center", fill = NA)) %>% 
  mutate(variable = "Moving average")

ymax = max(diff.price.avg.yr$value) * 1.1

p <-ggplot() +
  geom_line(data = diff.price.avg.yr, aes(x = iteration, y = value, color = variable), size = 1.2, alpha = 0.5) +
  geom_line(data = diff.price.avg.yr.movingavg, aes(x = iteration, y = value, color = variable), size = 2.5, alpha = 0.5) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size= 10,face="bold")) +
  xlab("iteration") + ylab(paste0("Difference of electricity price (REMIND-DIETER)\n($/MWh)"))  +
  coord_cartesian(ylim = c(0,ymax)) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

swfigure(sw,print,p,sw_option="width=20, height=12")
if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/Diff_avg_elec_price_convergence_iteration.png"),  p,  width = 7, height =4.5, units = "in", dpi = 120)
}

##################################################################################################
swlatex(sw, paste0("\\subsection{Market value difference over iterations}"))
diff.mv <- out.remind.mv %>% 
  # filter(tech == dieter.supply.tech.mapping) 
  filter(period %in% model.periods) %>% 
  select(period,tech,iteration,rm.mv=value) %>% 
  filter(iteration > 0) %>% 
  left_join(out.RMprice) %>% 
  filter(!value == 0) %>% 
  select(-value,-variable) %>% 
  filter(!rm.mv == 0) %>% 
  left_join(out.dieter.mv.woscar%>% 
              mutate(period = as.numeric(period)) %>% 
              select(period,iteration,tech,value)) %>% 
  filter(!tech %in% remind.sector.coupling.mapping) %>% 
  replace(is.na(.), 0) %>%
  select(period,iteration,tech,rm.mv, value) %>% 
  mutate(value = rm.mv - value) %>% 
  select(-rm.mv) 

p <-ggplot() +
  geom_line(data = diff.mv, aes(x = iteration, y = value, color = tech), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size= 20,face="bold")) +
  xlab("iteration") + ylab(paste0("Difference of market value (REMIND-DIETER) by tech\n($/MWh)"))  +
  theme(legend.title = element_text(size=25),legend.text = element_text(size=25)) +
  theme(legend.text = element_text(size=20), strip.text = element_text(size = 20)) +
  scale_color_manual(name = "tech", values = color.mapping)+
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  facet_wrap(~period, nrow = 3)

swfigure(sw,print,p,sw_option="width=20, height=12")
if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/Diff_MV_convergence_iteration.png"),  p,  width = 24, height =12, units = "in", dpi = 120)
}
##################################################################################################
swlatex(sw, paste0("\\subsection{Market value difference (time average) over iterations}"))

diff.mv.avg.yr <- diff.mv %>% 
  # filter(iteration == 12)
  filter(period %in% model.periods.till2100) %>% 
  dplyr::group_by(period,iteration) %>%
  dplyr::summarise( value = mean(value), .groups = "keep" ) %>% 
  dplyr::ungroup(period,iteration) %>% 
  dplyr::group_by(iteration) %>%
  dplyr::summarise( value = mean(value), .groups = "keep" ) %>% 
  dplyr::ungroup(iteration) %>% 
  filter(value>0)%>% 
  mutate(variable = "Difference of electricity price")

# moving average
diff.mv.avg.yr.movingavg <-diff.mv.avg.yr %>% 
  mutate( value = frollmean(value, 3, align = "center", fill = NA)) %>% 
  mutate(variable = "Moving average")

ymax = max(diff.mv.avg.yr$value) * 1.1

p <-ggplot() +
  geom_line(data = diff.mv.avg.yr, aes(x = iteration, y = value, color = variable), size = 1.2, alpha = 0.5) +
  geom_line(data = diff.mv.avg.yr.movingavg, aes(x = iteration, y = value, color = variable), size = 2.5, alpha = 0.5) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size= 10,face="bold")) +
  xlab("iteration") + ylab(paste0("Difference of time-averaged market value (REMIND-DIETER)\n($/MWh)"))  +
  coord_cartesian(ylim = c(0,ymax)) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

swfigure(sw,print,p,sw_option="width=20, height=12")
if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/Diff_t_avg_MV_convergence_iteration.png"),  p,  width = 6, height =5, units = "in", dpi = 120)
}


##################################################################################################
swlatex(sw, paste0("\\subsection{Total system markup (REMIND) - difference to last iteration}"))

p <-ggplot() +
  geom_line(data = out.remind.total.sys.markup.diff, aes(x = iteration, y = value), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size= 20,face="bold")) +
  xlab("iteration") + ylab(paste0("Total tax markup (REMIND) = markup - reference markup from last iteration\n($/MWh)"))  +
  # coord_cartesian(ylim = c(-10,10)) +
  ggtitle(paste0("Total system markup (REMIND) - difference to last iteration (this should go to zero as coupled model reaches convergence"))+
  theme(plot.title = element_text(size = 26, face = "bold"))+
  facet_wrap(~period, nrow = 3, scale="free")

swfigure(sw,print,p,sw_option="width=20, height=12")
if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/REMIND_total_sys_markup_diff_iteration.png"),  p,  width = 24, height =12, units = "in", dpi = 120)
}

##################################################################################################
swlatex(sw, paste0("\\subsection{REMIND budget time series (to check stability)}"))

p <-ggplot() +
  geom_line(data = out.remind.qm_budget.marg, aes(x = period, y = -qm_budget.m), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size= 12,face="bold")) +
  xlab("Time") + ylab(paste0("Budget equation marginal"))  +
  # coord_cartesian(ylim = c(-10,10)) +
  ggtitle(paste0("Budget equation marginal (this shows if for some iterations budget becomes 0, i.e the forcing from the coupling relieves the bound on budget, the more there are these iterations the less stable the coupling usually is)"))+
  theme(plot.title = element_text(size = 15, face = "bold"))+
  facet_wrap(~iteration, nrow = 5, scale="free")

swfigure(sw,print,p,sw_option="width=20, height=12")
if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/REMIND_budget_time.png"),  p,  width = 24, height =12, units = "in", dpi = 120)
}


