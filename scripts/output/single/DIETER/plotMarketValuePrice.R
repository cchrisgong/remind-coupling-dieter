# Data preparation --------------------------------------------------------

cat("Plot market value, markup, capacity shadow price \n")

# REMIND -------------------------------------------------
# shadow price

out.remind.peakdem.shadowprice <- NULL
for (i in 2:(length(remind.files))){

  remind.qm_budget <- file.path(outputdir, remind.files[i]) %>% 
    read.gdx("qm_budget", field="m", squeeze=F) %>% 
    filter(all_regi == reg) %>%
    filter(ttot %in% report.periods) %>% 
    select(period = ttot, qm_budget.m = m)
    
  remind.peakdem.shadowprice <- file.path(outputdir, remind.files[i]) %>% 
    read.gdx("q32_peakDemandDT", field="m", squeeze=F)  %>% 
    select(period = ttot, q32_peakDemandDT.m = m) %>% 
    filter(period %in% model.periods) %>% 
    left_join(remind.qm_budget) %>% 
    mutate(q32_peakDemandDT.m = q32_peakDemandDT.m / qm_budget.m * 1e12 / 1e9 * 1.2) %>% # (10^12 2005$)/TW-> 2015$/kW
    replace(is.na(.), 0) %>%
    select(period,value=q32_peakDemandDT.m) %>% 
    mutate(iteration = i-1)
  
  out.remind.peakdem.shadowprice <- rbind(out.remind.peakdem.shadowprice, remind.peakdem.shadowprice)
}

# markup and market values

out.remind.mrkup <- NULL
out.remind.flexadj <- NULL
out.remind.mv <- NULL
out.remind.genshare <-NULL
out.remind.sys.mrkup <-NULL
out.remind.sys.sp.capcon <- NULL

for (i in 2:(length(remind.files))){
  # i = 7
  # print(i)
  
  # load generation share
  remind.genshare <- file.path(outputdir, remind.files[i]) %>% 
    read.gdx("v32_shSeElDisp", squeeze = F)  %>% 
    filter(all_regi == reg) %>% 
    filter(all_te %in% names(remind.tech.mapping)) %>% 
    select(period=ttot, tech=all_te, genshare = value) %>% 
    mutate(period = as.numeric(period)) %>% 
    mutate(iteration = i-1)
  
  remind.genshare.agg <- remind.genshare %>% 
    revalue.levels(tech = remind.tech.mapping) %>% 
    dplyr::group_by(iteration,period, tech) %>%
    dplyr::summarise( genshare = sum(genshare), .groups = "keep" ) %>% 
    dplyr::ungroup(iteration,period, tech) %>% 
    mutate(period = as.numeric(period))

  ### markups  
  # supply side: mean value is okay, since in remind model it is already upscaled 
  remind.mrkup <- file.path(outputdir, remind.files[i]) %>% 
    read.gdx("p32_mrkupUpscaled", squeeze=F) %>% 
    filter(all_regi == reg) %>%
    select(period = ttot, tech = all_te, value) %>% 
    filter(tech %in% names(remind.tech.mapping)) %>% 
    revalue.levels(tech = remind.tech.mapping) %>%
    filter(period %in% model.periods) %>% 
    mutate(period = as.numeric(period)) %>% 
    dplyr::group_by(period,tech) %>%
    dplyr::summarise(value = mean(value), .groups = "keep" ) %>%  
    dplyr::ungroup(period,tech) %>%
    # negative sign here needed since a positive markup is equivalent to lower cost
    mutate(value = - value * 1e12 / sm_TWa_2_MWh * 1.2) %>% # mrkup already divides by budget in the model
    mutate(iteration = i-1)
  
  # only report those tech for which there are generations
  remind.mrkup.non0gen <- remind.mrkup %>% 
    left_join(remind.genshare.agg) %>% 
    replace(is.na(.), 0) %>% 
    filter(!genshare ==0) %>% 
    select(-genshare)
  
  remind.sys.mrkup <- remind.mrkup %>% 
    left_join(remind.genshare.agg) %>% 
    mutate(value = value * genshare/1e2) %>% 
    dplyr::group_by(period) %>%
    dplyr::summarise( value = sum(value), .groups = "keep" ) %>%
    dplyr::ungroup(period) %>% 
    mutate(iteration = i-1)
  
  # demand side
  remind.flexadj <- file.path(outputdir, remind.files[i]) %>% 
    read.gdx("vm_flexAdj", field="l", squeeze=F)  %>% 
    filter(all_regi == reg) %>%
    select(period = tall, tech = all_te, value) %>% 
    revalue.levels(tech = remind.sector.coupling.mapping) %>%
    filter(period %in% model.periods) %>% 
    mutate(period = as.numeric(period)) %>% 
    dplyr::group_by(period,tech) %>%
    dplyr::summarise( value = mean(value), .groups = "keep" ) %>%
    dplyr::ungroup(period,tech) %>%
    mutate(value = -value * 1e12 / sm_TWa_2_MWh * 1.2) %>% # flexadj already divides by budget in the model
    mutate(iteration = i-1)
  
  # calculate shadow price in remind due to capacity constraint
  remind.sp.capcon <- file.path(outputdir, remind.files[i]) %>% 
    read.gdx("p32_capConShadowPrice", factors = FALSE,squeeze=F) %>% 
    filter(all_regi == reg) %>%
    filter(all_te %in% names(remind.nonvre.mapping))  %>% 
    select(period = ttot, tech = all_te, value) %>% 
    mutate(value = value * 1e12 / sm_TWa_2_MWh * 1.2) %>%
    dplyr::group_by(tech) %>%
    complete(period = report.periods, fill = list(load = 0)) %>%
    dplyr::ungroup(tech) %>%
    replace(is.na(.), 0)  %>% 
    dplyr::group_by(tech) %>%
    mutate(value = frollmean(value, 3, align = "center", fill = NA)) %>%
    dplyr::ungroup(tech)%>% 
    mutate(iteration = i-1)
  
  remind.sys.sp.capcon <- list(remind.genshare, remind.sp.capcon) %>% 
    reduce(full_join) %>% 
    replace(is.na(.), 0) %>% 
    filter(genshare > 1e-6)  %>% 
    mutate_all(function(x) ifelse(is.infinite(x), 0, x)) %>% 
    select(iteration,period, genshare, value) %>% 
    mutate(value = genshare * value/1e2) %>% 
    dplyr::group_by(iteration,period) %>%
    dplyr::summarise( value = sum(value), .groups = "keep" ) %>% 
    dplyr::ungroup(iteration,period)
  
  #### market values
  remind.marketvalue <- file.path(outputdir, remind.files[i]) %>% 
    read.gdx("p32_MVupscaled", squeeze=F) %>% 
    filter(all_regi == reg) %>%
    select(period = ttot, tech = all_te, value) %>%
    revalue.levels(tech = remind.tech.mapping) %>%
    filter(period %in% report.periods) %>% 
    mutate(period = as.numeric(period)) %>% 
    dplyr::group_by(period,tech) %>%
    dplyr::summarise( value = mean(value), .groups = "keep" ) %>%
    dplyr::ungroup(period,tech) %>%
    mutate(value = value * 1e12 / sm_TWa_2_MWh * 1.2) %>% # mrkup already divides by budget in the model
    mutate(iteration = i-1)

  # only report those tech for which there are generations
  remind.marketvalue.non0gen <- remind.marketvalue %>% 
    left_join(remind.genshare.agg) %>% 
    replace(is.na(.), 0) %>% 
    filter(!genshare ==0) %>% 
    select(-genshare)
  
  remind.marketprice <- file.path(outputdir, remind.files[i]) %>% 
    read.gdx("p32_marketPrice", squeeze = F) %>% 
    filter(all_regi == reg) %>%
    filter(all_te %in% names(remind.sector.coupling.mapping)) %>%
    revalue.levels(all_te = remind.tech.mapping) %>% 
    mutate(value = value * 1e12 / sm_TWa_2_MWh * 1.2) %>%
    select(period= ttot, tech=all_te, value) %>% 
    mutate(period = as.numeric(period)) %>% 
    mutate(iteration = i-1)
  
    out.remind.mrkup <- rbind(out.remind.mrkup, remind.mrkup.non0gen, remind.flexadj)
    out.remind.sys.mrkup <- rbind(out.remind.sys.mrkup, remind.sys.mrkup)
    out.remind.flexadj <- rbind(out.remind.flexadj, remind.flexadj)
    out.remind.sys.sp.capcon <- rbind(out.remind.sys.sp.capcon, remind.sys.sp.capcon)
    out.remind.mv <- rbind(out.remind.mv, remind.marketvalue.non0gen, remind.marketprice)
    out.remind.genshare <- rbind(out.remind.genshare, remind.genshare.agg)
}

if (h2switch == "off"){
  out.remind.mv <- out.remind.mv %>% 
    filter(!tech %in% names(remind.sector.coupling.mapping))
  out.remind.mrkup <- out.remind.mrkup %>% 
    filter(!tech %in% names(remind.sector.coupling.mapping))
}
# DIETER --------------------------------------------------


out.dieter.report.mv <- NULL
for (i in 1:length(dieter.files.report)){
  it <- as.numeric(str_extract(dieter.files[i], "[0-9]+"))
  
  dieter.mv <- file.path(outputdir, dieter.files.report[i]) %>% 
    read.gdx("report_tech", squeeze = F) %>% 
    select(model=X..1, period = X..2, var=X..4, tech=X..5, value) %>%
    filter(var %in% dieter.report.mv) %>% 
    filter(period %in% model.periods) %>% 
    revalue.levels(tech = dieter.tech.mapping) %>%
    mutate(iteration = it)
  
  out.dieter.report.mv <- rbind(out.dieter.report.mv, dieter.mv)
}

# with scarcity price hour price
out.dieter.mv.wscar <- out.dieter.report.mv %>% 
  filter(var == "DIETER Market value with scarcity price ($/MWh)") 

# without scarcity price hour price
out.dieter.mv.woscar <- out.dieter.report.mv %>% 
  filter(var == "DIETER Market value ($/MWh)")

if (h2switch == "off"){
  out.dieter.mv.wscar <- out.dieter.mv.wscar %>%
    filter(!tech %in% dieter.demand.tech.mapping)%>%
    filter(!tech %in% names(dieter.demand.tech.mapping))
  
  out.dieter.mv.woscar <- out.dieter.mv.woscar %>%
    filter(!tech %in% dieter.demand.tech.mapping)%>%
    filter(!tech %in% names(dieter.demand.tech.mapping))
}
# Plotting ----------------------------------------------------------------

swlatex(sw,"\\onecolumn")
swlatex(sw, paste0("\\section{Peak demand (capital constraint) shadow price}"))

swlatex(sw, paste0("\\subsection{Peak demand shadow price over iterations}"))

p <- ggplot() +
  geom_line(data=out.remind.peakdem.shadowprice, aes(x=iteration, y=value)) + 
  theme(legend.position = "bottom") +
  xlab("Iteration") + 
  ggtitle(paste0("REMIND: ", reg))+
  ylab("Capacity constraint shadow price ($/kW)") +
  facet_wrap(~period, ncol=4)

swfigure(sw,print,p,sw_option="width=20, height=12")
if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/CapacityConstraint_shadowPrice_iteration.png"),  p,  width = 15, height =8, units = "in", dpi = 120)
}
#########################################################################################################
swlatex(sw,"\\twocolumn")
swlatex(sw, paste0("\\subsection{Peak demand shadow price over time (a few iterations)}"))

p <- ggplot() + 
  geom_line(data=out.remind.peakdem.shadowprice %>% filter(iteration %in% c(10,20,maxiter-1)), aes(x=period, y=value)) +
  theme(legend.position = "bottom") +
  xlab("Time") + 
  ggtitle(paste0("REMIND: ", reg)) +
  ylab("Capacity constraint shadow price ($/kW)")+
  facet_wrap(~iteration, nrow = 3,labeller = label_both)

swfigure(sw,print,p)

if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/CapacityConstraint_shadowPrice_time.png"),  p,  width = 6, height =12, units = "in", dpi = 120)
}

########################################################################################################
########################################################################################################
swlatex(sw,"\\onecolumn")
swlatex(sw, paste0("\\section{Market value and markups}"))
########################################################################################################
swlatex(sw, paste0("\\subsection{Markup time series (a few iterations)}"))

p <- ggplot() + 
  geom_line(data=out.remind.mrkup %>% filter(iteration %in% c(1,2,maxiter-1), period <2100), aes(x=period, y=-value, color = tech)) +
  # theme(legend.position = "bottom") +
  scale_color_manual(name = "Technology", values = color.mapping) +
  xlab("Time") + 
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(size=13)) +
  theme(axis.text=element_text(size=15), axis.title=element_text(size= 13, face="bold"),strip.text = element_text(size=13)) +
  ggtitle(paste0("REMIND: ", reg))+
  ylab(paste0("Markup (USD/MWh)"))+
  facet_wrap(~iteration, nrow = 3, labeller = label_both)

swfigure(sw,print,p)

if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/Markup_time_iters.png"),  p,  width = 8, height =12, units = "in", dpi = 120)
}
########################################################################################################
swlatex(sw, paste0("\\subsection{Markup time series (last iteration)}"))

p <- ggplot() + 
  geom_line(data=out.remind.mrkup %>% filter(iteration %in% c(maxiter-1), period <2100), aes(x=period, y=-value, color = tech)) +
  theme(legend.position = "bottom") +
  scale_color_manual(name = "Technology", values = color.mapping) +
  xlab("Time") + 
  ggtitle(paste0("REMIND: ", reg))+
  ylab(paste0("Markup (USD/MWh)"))

swfigure(sw,print,p)

if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/Markup_time_lastiter.png"),  p,  width = 8, height =6, units = "in", dpi = 120)
}

########################################################################################################
swlatex(sw, paste0("\\subsection{Market value time series (a few iterations)}"))

p <- ggplot() + 
  geom_line(data=out.remind.mv %>% filter(iteration %in% c(5,20,maxiter-1), period <2100), aes(x=period, y=value, color = tech)) +
  theme(legend.position = "bottom") +
  scale_color_manual(name = "Technology", values = color.mapping) +
  xlab("Time") + 
  ggtitle(paste0("REMIND: ", reg))+
  ylab(paste0("Market Value (moving average) (USD/MWh)"))+
  facet_wrap(~iteration, nrow = 3,labeller = label_both)

swfigure(sw,print,p)

if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/MarketValue_time_iters.png"),  p,  width = 8, height =12, units = "in", dpi = 120)
}

########################################################################################################
swlatex(sw, paste0("\\subsection{Market value time series (last iteration)}"))

p <- ggplot() + 
  geom_line(data=out.remind.mv %>% filter(iteration %in% c(maxiter-1), period <2100), aes(x=period, y=value, color = tech)) +
  theme(legend.position = "bottom") +
  scale_color_manual(name = "Technology", values = color.mapping) +
  xlab("Time") + 
  ggtitle(paste0("REMIND: ", reg))+
  ylab(paste0("Market Value (USD/MWh)"))

swfigure(sw,print,p)

if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/MarketValue_time_lastiter.png"),  p,  width = 8, height =6, units = "in", dpi = 120)
}

########################################################################################################
swlatex(sw, paste0("\\subsection{Market value time series - rolling average (a few iterations)}"))

remind.mv.rollmean <- out.remind.mv %>% 
  dplyr::group_by(tech,iteration) %>%
  mutate( value = frollmean(value, 7, align = "center", fill = NA)) %>% 
  dplyr::ungroup(tech,iteration)

p <- ggplot() + 
  geom_line(data=remind.mv.rollmean %>% filter(iteration %in% c(5,20,maxiter-1), period %in% model.periods.till2100), aes(x=period, y=value, color = tech)) +
  theme(legend.position = "bottom") +
  scale_color_manual(name = "Technology", values = color.mapping) +
  xlab("Time") + 
  ggtitle(paste0("REMIND: ", reg))+
  ylab(paste0("Market Value (moving average) (USD/MWh)"))+
  facet_wrap(~iteration, nrow = 3,labeller = label_both)

swfigure(sw,print,p)

if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/MarketValue_rollmean_time_iters.png"),  p,  width = 8, height =12, units = "in", dpi = 120)
}

########################################################################################################
swlatex(sw, paste0("\\subsection{Market value time series - rolling average (last iteration)}"))

p <- ggplot() + 
  geom_line(data=remind.mv.rollmean %>% filter(iteration %in% c(maxiter-1), period %in% model.periods.till2100), aes(x=period, y=value, color = tech)) +
  theme(legend.position = "bottom") +
  scale_color_manual(name = "Technology", values = color.mapping) +
  xlab("Time") + 
  ggtitle(paste0("REMIND: ", reg))+
  ylab(paste0("Market Value (USD/MWh)"))

swfigure(sw,print,p)

if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/MarketValue_rollmean_time_lastiter.png"),  p,  width = 8, height =6, units = "in", dpi = 120)
}

########################################################################################################
swlatex(sw, paste0("\\section{Market value model comparison}"))
swlatex(sw, paste0("\\subsection{Market value model comparison - over iteration for a few years}"))

for (yr in c(2025,2030,2040,2050)){
  
  plot.remind.mv <- out.remind.mv %>% 
    mutate(var = "REMIND market value ($/MWh)")
  
  p <- ggplot()+
    geom_line(data = plot.remind.mv %>% filter(period == yr), aes(x = iteration, y = value, color = var), size = 1.2, alpha = 1.5) +
    geom_line(data = out.dieter.mv.wscar %>% filter(period == yr), aes(x = iteration, y = value, color = var), size = 1.2, alpha = 0.5) +
    geom_line(data = out.dieter.mv.woscar %>% filter(period == yr), aes(x = iteration, y = value, color = var), size = 1.2, alpha = 0.5) +
    theme(axis.text=element_text(size=20), axis.title=element_text(size=20, face="bold")) +
    theme(legend.text = element_text(size=20), strip.text = element_text(size = 20))+
    xlab("iteration") + ylab(paste0("Market Value ", "(USD/MWh)"))  +
    ggtitle(paste0("Market value iteration series - both models: ", reg))+
    theme(legend.position = "bottom") +
    theme(aspect.ratio = .6) +
    theme(plot.title = element_text(size = 26, face = "bold"))+
    guides(color = guide_legend(nrow = 2, byrow = TRUE))+
    theme(legend.title = element_blank()) +
    coord_cartesian(ylim = c(-40,250))+
    facet_wrap(~tech, nrow = 3, scale="free")
  
  swfigure(sw,print,p)
  
  if (save_png == 1){
    ggsave(filename = paste0(outputdir, "/DIETER/MarketValue_compare_", yr, "_iteration.png"),  p,  width = 18, height =12, units = "in", dpi = 120)
  }
  
}
########################################################################################################
swlatex(sw, paste0("\\subsection{Market value model comparison - time series (last iteration)}"))

plot.dieter.mv.woscar <- out.dieter.mv.woscar %>%
  filter(iteration == maxiter-1, period %in% model.periods.till2100) %>% 
  mutate(period = as.numeric(period)) %>% 
  replace(is.na(.), 0) 

plot.dieter.mv.wscar <- out.dieter.mv.wscar %>% 
  filter(iteration == maxiter-1, period %in% model.periods.till2100) %>% 
  mutate(period = as.numeric(period)) %>% 
  replace(is.na(.), 0) 

plot.remind.mv.laiter <- plot.remind.mv %>% 
  filter(iteration == maxiter-1, period %in% model.periods.till2100) %>% 
  replace(is.na(.), 0) 

p <- ggplot()+
  geom_line(data = plot.remind.mv.laiter, 
            aes(x = period , y = value, color = var), size = 1.2, alpha = 1.5) +
  geom_line(data = plot.dieter.mv.wscar,
            aes(x = period , y = value, color = var), size = 1.2, alpha = 0.5) +
  geom_line(data = plot.dieter.mv.woscar,
            aes(x = period , y = value, color = var), size = 1.2, alpha = 0.5) +
  theme(axis.text = element_text(size=15), axis.title=element_text(size=15, face="bold")) +
  xlab("Time") + ylab(paste0("Market Value ", "(USD/MWh)"))  +
  scale_color_manual(name = "var", values = color.mapping.mv.var)+
  ggtitle(paste0("Market value time series - both models: ", reg))+
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=20), strip.text = element_text(size = 20))+
  coord_cartesian(ylim = c(-40, 300))+
  theme(legend.position = "bottom") +
  theme(aspect.ratio = .6) +
  theme(plot.title = element_text(size = 26, face = "bold"))+
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  facet_wrap(~tech, nrow = 3, scale="free")

swfigure(sw,print,p)

if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/MarketValue_compare_time.png"),  p,  width = 17, height =10, units = "in", dpi = 120)
}

