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


# fitCubic <- function(input){
#   # iter_tofit = 5
#   input = test
#   
#   x = unlist(input$t)
#   y = unlist(input$price)
#   
#   # fit to a cubic function
#   model <- lm(y ~ 1 + x + I(x^2) + I(x^3)) 
#   coeff <- summary(model)$coefficients[, "Estimate"]
#   
#   output <- input %>% 
#     mutate(fittedValue = (coeff[[1]] + coeff[[2]]*t + coeff[[3]] * t^2 +coeff[[4]] * t^3 )) %>% 
#     select(t,value=fittedValue)
#   
#   price_fitted <- list(input, output) %>% 
#     reduce(full_join) %>% 
#     mutate(value = round(as.numeric(value), digits = 3))
#   
#   return(price_fitted)
# }

# test <- out.RMprice %>%
#   filter(iteration == 5) %>%
#   select(t=period,price=value)
# 
# test0 <- lapply(test, fitCubic)
# test1 <- rbindlist(test0)
# 
# out.fitted.df <- NULL
# 
# # for (i in 2:length(out.RMprice)){
# #   
# #   fitted_iter <- out.RMprice %>% 
# #     filter(iteration
# #   
# #     out.fitted.df <- rbind(out.fitted.df, remind.qm_budget.marg)
# # }
# 
# 
# p<-ggplot() +
#   geom_line(data = test1, aes(x = t, y = value), size = 1.2, alpha = 1) +
#   # scale_color_manual(name = "label", values = mycolors)+
#   coord_cartesian(ylim = c(0, 150), xlim = c(2020,2100))+
#   theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))
# 
# ggsave(filename = paste0(outputdir, "/DIETER/checkFittedCubic.png"), p, width = 8, height = 5, units = "in", dpi = 120)

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
  xlab("iteration") + ylab(paste0("generation share (%)")) +
  scale_color_manual(name = "tech", values = color.mapping) +
  scale_linetype_manual(name = "model", values = linetype.map) +
  theme(legend.title = element_text(size=25),legend.text = element_text(size=25))+
  coord_cartesian(ylim = c(0,60)) +
  theme(legend.text = element_text(size=20), strip.text = element_text(size = 20))+
  theme(legend.position = "bottom") +
  theme(legend.title=element_blank())+
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  facet_wrap(~period, nrow = 3)

swfigure(sw,print,p,sw_option="width=20, height=12")
if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/Generation_share_convergence_iteration.png"),  p,  width = 24, height =12, units = "in", dpi = 120)
}

##################################################################################################
for (tech_plot in c("CCGT", "Wind Onshore", "Solar")){
swlatex(sw, paste0("\\subsection{Generation share ", tech_plot, " over iterations - contour plot}"))

plot.tech.remind.genshare <- out.remind.genshare %>% 
  mutate(model = "REMIND") %>% 
  filter(period %in% model.periods.till2100) %>% 
  filter(tech == tech_plot)

p<- ggplot() +
  geom_contour_filled(aes(iteration, period, z = genshare), breaks = c(seq(0, 70, 1)), show.legend = TRUE, plot.tech.remind.genshare)+
  ggtitle(paste0("Generation share (REMIND) ", tech_plot, " ($/MWh)") )+
  xlab("Iteration") + ylab("Period")+
  scale_y_continuous(breaks = seq(2020,2100,10)) +
  theme(axis.text = element_text(size = 12),
        title = element_text(size = 12,face="bold"),
        panel.border= element_rect(size=2,color="black",fill=NA))

swfigure(sw,print,p,sw_option="width=20, height=12")
if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/Generation_share_", gsub(" ", "_", tech_plot), "_convergence_iteration_surface.png"),  p,  width = 14, height =7, units = "in", dpi = 120)
}
}
##################################################################################################
swlatex(sw, paste0("\\subsection{Generation share difference over iterations}"))

remind.gensh <- plot.remind.genshare %>% 
  mutate(period = as.factor(period)) %>% 
  select(period, tech, genshareRM = genshare, iteration)

diff.gensh <- out.dieter.report.gensh %>% 
  select(period, tech, genshareDT = value, iteration) %>% 
  left_join(remind.gensh) %>% 
  mutate(delGenshare = genshareRM - genshareDT) %>% 
  filter(iteration >0)

p <-ggplot() +
  geom_line(data = diff.gensh, aes(x = iteration, y = delGenshare, color = tech), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size= 20,face="bold")) +
  xlab("iteration") + ylab(paste0("Difference of generation share (REMIND-DIETER) (%)"))  +
  scale_color_manual(name = "tech", values = color.mapping) +
  theme(legend.title = element_text(size=25),legend.text = element_text(size=25)) +
  theme(legend.text = element_text(size=20), strip.text = element_text(size = 20)) +
  coord_cartesian(ylim = c(-10,10)) +
  theme(legend.position = "bottom") +
  theme(legend.title=element_blank())+
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  facet_wrap(~period, nrow = 3)

swfigure(sw,print,p,sw_option="width=20, height=12")
if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/Diff_generation_share_convergence_iteration.png"),  p,  width = 24, height =12, units = "in", dpi = 120)
}

##################################################################################################

###### comparison of electricity price should be without scarcity price hour in DIETER and the normal price in REMIND (which is without scarcity price hour) ################################################################

swlatex(sw, paste0("\\subsection{Electricity price difference over iterations}"))

diff.price <- out.RMprice %>% 
  filter(period %in% model.periods) %>% 
  filter(!value == 0) %>% 
  select(period,iteration,rmprice=value) %>% 
  left_join(out.DTprice) %>% 
  filter(period %in% model.periods.till2100) %>% 
  select(period,iteration,rmprice, value) %>% 
  mutate(value = rmprice - value) %>% 
  select(-rmprice) %>% 
  filter(iteration > start_i-1)

p <-ggplot() +
  geom_line(data = diff.price, aes(x = iteration, y = value,), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size= 20,face="bold")) +
  xlab("iteration") + ylab(paste0("Difference of electricity price (REMIND-DIETER)\n($/MWh)"))  +
  theme(legend.title = element_text(size=25),legend.text = element_text(size=25)) +
  theme(legend.text = element_text(size=20), strip.text = element_text(size = 20)) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  facet_wrap(~period, nrow = 3)

swfigure(sw,print,p,sw_option="width=20, height=12")
if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/Diff_elec_price_convergence_iteration.png"),  p,  width = 24, height =12, units = "in", dpi = 120)
}

##################################################################################################

swlatex(sw, paste0("\\subsection{Electricity price difference over iterations (with rolling mean REMIND price over 3 periods)}"))

diff.price.rollmean <- out.RMprice %>% 
  filter(period %in% model.periods) %>% 
  filter(!value == 0) %>% 
  select(period,iteration,rmprice=value) %>% 
  dplyr::group_by(iteration) %>%
  mutate( rmprice = frollmean(rmprice, 3, align = "left", fill = 0)) %>%
  dplyr::ungroup(iteration) %>% 
  left_join(out.DTprice) %>% 
  filter(period %in% model.periods.till2100) %>% 
  select(period,iteration,rmprice, value) %>% 
  mutate(value = rmprice - value) %>% 
  select(-rmprice)%>% 
  filter(iteration > start_i-1)

p <- ggplot() +
  geom_line(data = diff.price.rollmean, aes(x = iteration, y = value,), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size= 20,face="bold")) +
  xlab("iteration") + ylab(paste0("Difference of electricity price (REMIND-DIETER) (2020-2100) \n($/MWh)"))  +
  theme(legend.title = element_text(size=25),legend.text = element_text(size=25)) +
  theme(legend.text = element_text(size=20), strip.text = element_text(size = 20)) +
  # coord_cartesian(ylim = c(-10,10)) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  facet_wrap(~period, nrow = 3)

swfigure(sw,print,p,sw_option="width=20, height=12")
if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/Diff_RMrollmean_elec_price_convergence_iteration.png"),  p,  width = 24, height =12, units = "in", dpi = 120)
}

##################################################################################################

swlatex(sw, paste0("\\subsection{Electricity price difference - surface plot}"))

p<- ggplot() +
  geom_contour_filled(aes(iteration, period, z = value), breaks = c(seq(-80, 150, 10)), show.legend = TRUE, diff.price)+
  ggtitle("Electricity price difference (REMIND-DIETER) ($/MWh)") +
  xlab("Iteration") + ylab("Period")+
  scale_y_continuous(breaks = seq(2020,2100,10)) +
  theme(axis.text = element_text(size = 12),
        title = element_text(size = 12,face="bold"),
        panel.border= element_rect(size=2,color="black",fill=NA))

swfigure(sw,print,p,sw_option="width=20, height=12")
if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/Diff_elec_price_convergence_iteration_surface.png"),  p,  width = 14, height =7, units = "in", dpi = 120)
}


##################################################################################################

swlatex(sw, paste0("\\subsection{Electricity price difference (with rolling mean REMIND price over 3 periods) - surface plot}"))

p<- ggplot() +
  geom_contour_filled(aes(iteration, period, z = value), breaks = c(seq(-80, 200, 5)), show.legend = TRUE, diff.price.rollmean)+
  ggtitle("Electricity price difference (REMIND-DIETER) (with rolling mean REMIND price over 3 periods) ($/MWh)- convergence surface") +
  xlab("Iteration") + ylab("Period") +
  scale_y_continuous(breaks = seq(2020,2100,10)) +
  theme(axis.text = element_text(size = 12),
        title = element_text(size = 12,face="bold"),
        panel.border= element_rect(size=2,color="black",fill=NA))

swfigure(sw,print,p,sw_option="width=20, height=12")
if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/Diff_RMrollmean_elec_price_convergence_iteration_surface.png"),  p,  width = 14, height =7, units = "in", dpi = 120)
}


##################################################################################################

swlatex(sw, paste0("\\subsection{Electricity price difference (time average) over iterations}"))

diff.price.avg.yr <- diff.price %>% 
  # filter(period > 2030) %>% 
  dplyr::group_by(iteration) %>%
  dplyr::summarise( value = mean(value), .groups = "keep" ) %>% 
  dplyr::ungroup(iteration) %>% 
  mutate(variable = "Difference of electricity price")

diff.price.rollmean.avg.yr <- diff.price.rollmean %>% 
  # filter(period > 2030) %>% 
  dplyr::group_by(iteration) %>%
  dplyr::summarise( value = mean(value), .groups = "keep" ) %>% 
  dplyr::ungroup(iteration) %>% 
  mutate(variable = "Difference of electricity price (REMIND rolling avg)")

# moving average
diff.price.avg.yr.movingavg <- diff.price.avg.yr %>% 
  mutate( value = frollmean(value, 3, align = "left", fill = NA)) %>% 
  mutate(variable = "Moving average of difference of electricity price")

ymax = max(diff.price.avg.yr$value) * 1.1
ymin = min(diff.price.avg.yr$value) * 1.1

p <-ggplot() +
  geom_line(data = diff.price.avg.yr, aes(x = iteration, y = value, color = variable), size = 1.2, alpha = 0.5) +
  geom_line(data = diff.price.rollmean.avg.yr, aes(x = iteration, y = value, color = variable), size = 2.5, alpha = 0.5) +
  geom_line(data = diff.price.avg.yr.movingavg, aes(x = iteration, y = value, color = variable), size = 2.5, alpha = 0.5) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size= 10,face="bold")) +
  xlab("iteration") + ylab(paste0("Difference of time-averaged electricity price (REMIND-DIETER) (2020-2100) ($/MWh) \n($/MWh)"))  +
  # coord_cartesian(ylim = c(-30,60)) +
  coord_cartesian(ylim = c(0,ymax)) +
  theme(legend.position = "bottom") +
  theme(legend.title=element_blank())+
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

swfigure(sw,print,p,sw_option="width=20, height=12")
if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/Diff_avg_elec_price_convergence_iteration.png"),  p,  width = 7, height =4.5, units = "in", dpi = 120)
}

##################################################################################################
swlatex(sw, paste0("\\subsection{Market value over iterations}"))

plot.remind.mv <- out.remind.mv %>% 
  filter(tech %in% dieter.supply.tech.mapping) 

p <-ggplot() +
  geom_line(data = plot.remind.mv, aes(x = iteration, y = value, color = tech), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size= 20,face="bold")) +
  xlab("iteration") + ylab(paste0("Market value (REMIND) by tech\n($/MWh)"))  +
  theme(legend.title = element_text(size=25),legend.text = element_text(size=25)) +
  theme(legend.text = element_text(size=20), strip.text = element_text(size = 20)) +
  scale_color_manual(name = "tech", values = color.mapping)+
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  facet_wrap(~period, nrow = 3)

swfigure(sw,print,p,sw_option="width=20, height=12")
if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/MV_convergence_iteration.png"),  p,  width = 24, height =12, units = "in", dpi = 120)
}

##################################################################################################
for (tech_plot in c("OCGT", "Wind Onshore", "Solar")){

swlatex(sw, paste0("\\subsection{Markup and Market value for ", tech_plot, " over iterations}"))
  # tech_plot = "CCGT"

plot.remind.mv <- out.remind.mv %>% 
  filter(tech == tech_plot) %>% 
  mutate( value = frollmean(value, 4, align = "left", fill = NA)) %>% 
  filter(period %in% model.periods.till2100) %>% 
  filter(period >2015)

plot.remind.mk <- out.remind.mrkup %>% 
  filter(tech == tech_plot) %>% 
  filter(period %in% model.periods.till2100) %>% 
  filter(period >2015) %>% 
  mutate(value = -value)

p1 <- ggplot() +
  geom_contour_filled(aes(iteration, period, z = value), plot.remind.mk)+
  ggtitle(paste0("MK ", tech_plot, " (REMIND) ($/MWh) - convergence surface") ) +
  xlab("Iteration") + ylab("Period")  +
  theme(axis.text = element_text(size = 12),
        title = element_text(size = 12,face="bold"),
        panel.border= element_rect(size=2,color="black",fill=NA))+
  scale_color_continuous(breaks = c(seq(-200, 200, 5)))


p2 <- ggplot() +
  geom_contour_filled(aes(iteration, period, z = value), plot.remind.mv)+
  ggtitle(paste0("MV (rolling avg over 4 periods)", tech_plot, " (REMIND) ($/MWh) - convergence surface") )+
  xlab("Iteration") + ylab("Period")  +
  theme(axis.text = element_text(size = 12),
        title = element_text(size = 12,face="bold"),
        panel.border= element_rect(size=2,color="black",fill=NA))+
  scale_color_continuous(breaks = c(seq(-200, 200, 5)))

grid.newpage()
  p <- arrangeGrob(rbind(ggplotGrob(p1), ggplotGrob(p2)))

  swfigure(sw,print,p,sw_option="width=20, height=12")
if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/MKMV_", gsub(" ", "_", tech_plot), "_convergence_iteration.png"),  p,  width = 12, height =10, units = "in", dpi = 120)
}

}
##################################################################################################
swlatex(sw, paste0("\\subsection{Market value difference over iterations}"))
diff.mv <- out.remind.mv %>% 
  filter(period %in% model.periods) %>% 
  select(period,tech,iteration,rm.mv=value) %>% 
  filter(iteration > 0) %>% 
  left_join(out.RMprice) %>% 
  filter(!value == 0) %>% 
  select(-value, -variable) %>% 
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
# swlatex(sw, paste0("\\subsection{Market value difference (time and technology average) over iterations}"))
# 
# # weighted market value by share
# rm.mv <- out.remind.mv %>% 
#   # filter(iteration == 12) %>% 
#   filter(tech %in% dieter.supply.tech.mapping) %>% 
#   filter(period %in% model.periods) %>% 
#   select(period,tech,iteration,rm.mv=value) %>% 
#   filter(iteration > 0) %>% 
#   left_join(out.RMprice) %>% 
#   filter(!value == 0) %>% 
#   select(-value,-variable) %>% 
#   filter(!rm.mv == 0)  %>% 
#   left_join( out.remind.genshare) %>% 
#   filter(period %in% model.periods.till2100) %>% 
#   mutate(rm.mv = rm.mv * genshare / 1e2) %>% 
#   dplyr::group_by(period,iteration) %>%
#   dplyr::summarise( rm.mv = sum(rm.mv), .groups = "keep" ) %>% 
#   dplyr::ungroup(period,iteration) 
#   
# rm.mv.avg.yr <- rm.mv %>% 
#   dplyr::group_by(iteration) %>%
#   dplyr::summarise( rm.mv = mean(rm.mv), .groups = "keep" ) %>% 
#   dplyr::ungroup(iteration)  
#   
# dt.mv <- out.dieter.mv.woscar%>% 
#   filter(iteration > 0) %>% 
#   mutate(period = as.numeric(period)) %>% 
#   select(period,iteration,tech,dt.mv = value) %>% 
#   filter(!tech %in% remind.sector.coupling.mapping) %>% 
#   left_join( out.dieter.report.gensh %>% 
#              mutate(period = as.numeric(period)) %>% 
#              select(period,iteration,tech,genshare = value)) %>% 
#   filter(period %in% model.periods.till2100) %>% 
#   mutate(dt.mv = dt.mv * genshare / 1e2) %>% 
#   dplyr::group_by(period,iteration) %>%
#   dplyr::summarise( dt.mv = sum(dt.mv), .groups = "keep" ) %>% 
#   dplyr::ungroup(period,iteration) 
#   
# dt.mv.avg.yr <- dt.mv %>% 
#   dplyr::group_by(iteration) %>%
#   dplyr::summarise( dt.mv = mean(dt.mv), .groups = "keep" ) %>% 
#   dplyr::ungroup(iteration) 
# 
# diff.mv.avg.yr <- list(rm.mv.avg.yr, dt.mv.avg.yr) %>%
#   reduce(full_join) %>% 
#   mutate(value = rm.mv - dt.mv) %>% 
#   mutate(variable = "Difference of weighted-average market value") %>% 
#   select(-rm.mv, -dt.mv)
# 
# # moving average
# diff.mv.avg.yr.movingavg <-diff.mv.avg.yr %>% 
#   mutate(variable = "Moving average")
# 
# p <-ggplot() +
#   geom_line(data = diff.mv.avg.yr, aes(x = iteration, y = value, color = variable), size = 1.2, alpha = 0.5) +
#   geom_line(data = diff.mv.avg.yr.movingavg, aes(x = iteration, y = value, color = variable), size = 2.5, alpha = 0.5) +
#   theme(axis.text=element_text(size=10), axis.title=element_text(size= 10,face="bold")) +
#   xlab("iteration") + ylab("Diff. of generation-share-weighted-average time-average market values (REMIND - DIETER)\n (2020-2100) ($/MWh)") +
#   coord_cartesian(ylim = c(0,ymax)) +
#   theme(legend.title=element_blank())+
#   theme(legend.position = "bottom") +
#   guides(color = guide_legend(nrow = 2, byrow = TRUE))
# 
# swfigure(sw,print,p,sw_option="width=20, height=12")
# if (save_png == 1){
#   ggsave(filename = paste0(outputdir, "/DIETER/Diff_t_avg_MV_convergence_iteration.png"),  p,  width = 6, height =5.5, units = "in", dpi = 120)
# }

# ##################################################################################################
# swlatex(sw, paste0("\\subsection{Market value difference (time average) over iterations - surface}"))
# 
# diff.mv.tech.avg <- list(rm.mv, dt.mv) %>%
#   reduce(full_join) %>% 
#   mutate(value = rm.mv - dt.mv) %>% 
#   mutate(variable = "Difference of generation-share weighted-average market value")
# 
# p<- ggplot() +
#   geom_contour_filled(aes(iteration, period, z = value), breaks = c(seq(-80, 150, 15)), show.legend = TRUE, diff.mv.tech.avg)+
#   ggtitle("Difference of generation-share weighted-average market value - convergence surface") +
#   xlab("Iteration") + ylab("Period") +
#   scale_y_continuous(breaks = seq(2020,2100,10)) +
#   theme(axis.text = element_text(size = 12),
#         title = element_text(size = 12,face="bold"),
#         panel.border= element_rect(size=2,color="black",fill=NA))
# 
# swfigure(sw,print,p,sw_option="width=20, height=12")
# if (save_png == 1){
#   ggsave(filename = paste0(outputdir, "/DIETER/Diff_tech_wavg_MV_convergence_surface.png"),  p,  width = 10, height =5.5, units = "in", dpi = 120)
# }

##################################################################################################
# DIETER shadow price is also an indicator of convergence

out.shad_prices_DT <- NULL

for (i in 1:length(dieter.files.report)){
  
  iter = as.numeric(str_extract(dieter.files.report[i], "[0-9]+"))
  
  shad_prices_DT <- file.path(outputdir, dieter.files.report[i]) %>% 
      read.gdx("report", squeeze=F) %>% 
      select(model = X..1, period = X..2, variable = X..4, value) %>%
      filter(variable %in% report_DT_prices) %>% 
      filter(period %in% model.periods) %>% 
      filter(model == "DIETER")%>% 
      revalue.levels(variable = dieter.variable.mapping) %>%
      mutate(variable = factor(variable, levels=rev(unique(dieter.variable.mapping)))) %>% 
      mutate(period = as.numeric(period)) %>% 
      mutate(iteration = iter)%>% 
      select(iteration,period, variable, value) %>% 
      filter(variable == 'DIETER shadow price due to capacity constraint from REMIND (with grid)') 
    
  out.shad_prices_DT <-  rbind(out.shad_prices_DT, shad_prices_DT)
}

p<- ggplot() +
  geom_contour_filled(aes(iteration, period, z = value), breaks = c(seq(-80, 150, 5)), show.legend = TRUE, out.shad_prices_DT%>% filter(period <2110))+
  ggtitle("DIETER shadow price due to capacity constraint from REMIND - convergence surface") +
  xlab("Iteration") + ylab("Period")  +
  scale_y_continuous(breaks = seq(2020,2100,10))+
  theme(axis.text = element_text(size = 12),
        title = element_text(size = 12,face="bold"),
        panel.border= element_rect(size=2,color="black",fill=NA))

swfigure(sw,print,p,sw_option="width=20, height=12")
if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/DT_shadprice_convergence_surface.png"),  p,  width = 10, height =5.5, units = "in", dpi = 120)
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


