# Data preparation --------------------------------------------------------

cat("Plot Seel price \n")

out.SEPrice <- NULL
out.SEPrice.rollmean <- NULL
out.RMprice <- NULL
out.RMprice_wSP <- NULL
out.DTprice_woscar <- NULL
out.DTprice_wscar <- NULL
out.totalDem <- NULL

for (i in 2:(length(remind.files))){
  
  RMprice <- file.path(outputdir, remind.files[i]) %>% 
    read.gdx( "pm_SEPrice", squeeze = FALSE) %>% 
    filter(all_enty == "seel") %>%
    filter(all_regi == reg) %>% 
    mutate(value = value * 1e12 / sm_TWa_2_MWh * 1.2)  %>%  # (10^12 2005$)/TWa -> 2015$/MWh
    select(period=ttot, value) %>%
    mutate(iteration = i-1) %>%
    mutate(variable= "REMIND secondary electricity price ($/MWh)")
  
  DTprice_wscar <- file.path(outputdir, remind.files[i]) %>% 
    read.gdx( "p32_DIETER_elecprice_wscar", squeeze = FALSE) %>%
    select(period=ttot, value) %>%
    mutate(iteration = i-1) %>%
    mutate(variable = "DIETER secondary electricity price ($/MWh)") %>% 
    mutate(value = value *1.2) # convert to 2015 dollar
  
  DTprice_woscar <- file.path(outputdir, remind.files[i]) %>% 
    read.gdx( "p32_DIETER_elecprice", squeeze = FALSE) %>%
    select(period=ttot, value) %>%
    mutate(iteration = i-1) %>%
    mutate(variable = "DIETER secondary electricity price ($/MWh)") %>% 
    mutate(value = value *1.2) # convert to 2015 dollar
  
 RM_CF <- file.path(outputdir, remind.files[i]) %>% 
    read.gdx("vm_capFac", field = "l", squeeze=F)  %>% 
    filter(all_regi == reg) %>% 
    filter(all_te %in% names(remind.tech.mapping))%>% 
    filter(!all_te %in% names(remind.sector.coupling.mapping.exclude)) %>% 
    filter(!all_te %in% names(remind.storage.mapping.narrow)) %>% 
    select(period = ttot, tech = all_te, cf=value) %>%
    filter(cf >1e-3)
  
  # capacity constraint shadow price
  RMprice_capcon <- file.path(outputdir, remind.files[i]) %>% 
    read.gdx("p32_capConShadowPrice", squeeze=F) %>% 
    filter(all_regi == reg) %>%
    filter(all_te %in% names(remind.nonvre.mapping))  %>% 
    select(period = ttot, tech = all_te, value) %>% 
    right_join(RM_CF) %>%
    select(-cf) %>%
    replace(is.na(.), 0) %>% 
    mutate(value = value * 1e12 / sm_TWa_2_MWh * 1.2) %>%
    dplyr::group_by(tech) %>%
    complete(period = report.periods, fill = list(load = 0)) %>% 
    dplyr::ungroup(tech) %>% 
    replace(is.na(.), 0)  %>% 
    dplyr::group_by(tech) %>%
    mutate(value = frollmean(value, 3, align = "center", fill = NA)) %>%
    dplyr::ungroup(tech)
  
  prod_share.broad <- file.path(outputdir, remind.files[i]) %>% 
    read.gdx("v32_shSeElDisp", squeeze = F)  %>% 
    filter(all_regi == reg) %>% 
    filter(all_te %in% names(remind.tech.mapping)) %>% 
    select(period=ttot, tech=all_te, genshare = value) %>% 
    mutate(genshare = genshare / 1e2) %>% 
    mutate(period = as.numeric(period))
  
  prod_share <- prod_share.broad %>% 
    filter(period %in% model.periods.till2100) 
  
  prod_share <- prod_share %>% 
    filter(!tech %in% names(remind.storage.mapping.narrow))
  
  RM_capcon.sys <- list(prod_share, RMprice_capcon) %>% 
    reduce(full_join) %>% 
    replace(is.na(.), 0) %>% 
    filter(genshare >1e-6) %>% 
    mutate_all(function(x) ifelse(is.infinite(x), 0, x)) %>% 
    select(period, genshare, value) %>% 
    mutate(value = genshare * value) %>% 
    dplyr::group_by(period) %>%
    dplyr::summarise( value = sum(value), .groups = "keep" ) %>% 
    dplyr::ungroup(period) 
  
  RMprice_sp <- file.path(outputdir, remind.files[i]) %>% 
    read.gdx("p32_shadowPrice", squeeze=F) %>% 
    filter(all_regi == reg) %>%
    filter(all_te %in% names(remind.tech.mapping)) %>% 
    select(period = ttot, tech = all_te, value) %>%
    right_join(RM_CF) %>% 
    select(-cf) %>% 
    replace(is.na(.), 0) %>% 
    mutate(value = -value * 1e12 / sm_TWa_2_MWh * 1.2)
  
  RM_sp.sys <- list(prod_share, RMprice_sp) %>% 
    reduce(full_join) %>% 
    replace(is.na(.), 0) %>% 
    filter(genshare >1e-6) %>% 
    mutate_all(function(x) ifelse(is.infinite(x), 0, x)) %>% 
    select(period, genshare, value) %>% 
    mutate(value = genshare * value) %>% 
    dplyr::group_by(period) %>%
    dplyr::summarise( value = sum(value), .groups = "keep" ) %>% 
    dplyr::ungroup(period) 
  
  RMprice_wSP <- list(RMprice %>% 
                            select(-variable,-iteration,rmprice=value) %>% 
                            mutate(rmprice = frollmean(rmprice, 3, align = "left", fill = NA)), 
                          RM_capcon.sys, RM_sp.sys %>% select(period,sp=value) ) %>%
    reduce(full_join) %>% 
    mutate(value = rmprice+value+sp) %>% 
    mutate(iteration = i-1)
    
  p32_seelTotDem <- file.path(outputdir, remind.files[i]) %>% 
    read.gdx( "p32_seelTotDem", factor = FALSE)  %>% 
    filter(all_regi == reg) %>%
    filter(all_enty == "seel") %>%
    select(period=ttot,value) %>% 
    mutate(value = value *sm_TWa_2_MWh/1e6)  %>%
    mutate(iteration = i-1)  %>%
    mutate(variable = "Total demand (Twh)")
  
  RMprice.rollmean <- RMprice  %>% 
    mutate(value = frollmean(value, 3, align = "center", fill = NA))
  
  out.RMprice <- rbind(out.RMprice, RMprice)
  out.RMprice_wSP <- rbind(out.RMprice_wSP, RMprice_wSP)
  out.DTprice_woscar <- rbind(out.DTprice_woscar, DTprice_woscar)
  out.DTprice_wscar <- rbind(out.DTprice_wscar, DTprice_wscar)
  out.SEPrice <- rbind(out.SEPrice, RMprice, DTprice_woscar)
  out.SEPrice.rollmean <- rbind(out.SEPrice.rollmean, RMprice.rollmean, DTprice_woscar)
  out.totalDem <- rbind(out.totalDem, p32_seelTotDem)
}

RM.SEprice <- out.RMprice %>% 
  select(period, value,iteration,tech=variable)

out.prices_DT<- NULL
for (i in c(length(dieter.files.report))){
  
  iter = as.numeric(str_extract(dieter.files.report[i], "[0-9]+"))
  
  prices_DT <- file.path(outputdir, dieter.files.report[i]) %>% 
    read.gdx("report", squeeze=F) %>% 
    select(model = X..1, period = X..2, variable = X..4, value) %>%
    filter(variable %in% report_DT_prices)  %>% 
    filter(period %in% model.periods) %>% 
    filter(model == "DIETER")%>% 
    revalue.levels(variable = dieter.variable.mapping) %>%
    mutate(variable = factor(variable, levels=rev(unique(dieter.variable.mapping)))) %>% 
    mutate(period = as.numeric(period)) %>% 
    mutate(iteration = iter)%>% 
    select(model,iteration,period, variable, value)
  
  out.prices_DT <-  rbind(out.prices_DT, prices_DT)
}

shadow_prices_DT <- out.prices_DT %>% 
  # filter(variable == 'DIETER shadow price due to capacity constraint from REMIND (with grid)') %>% 
  filter(variable == 'DIETER shadow price due to capacity constraint from REMIND') %>% 
  select(period, shad=value,iteration)

elec_prices_DT <- out.prices_DT %>% 
  filter(variable == "DIETER annual average electricity price with scarcity price") 

elec_prices_DT_woscar <- out.prices_DT %>% 
  filter(variable == 'DIETER annual average electricity price') 

elec_prices_DT_wShadPrice <- elec_prices_DT %>% 
  left_join(shadow_prices_DT) %>% 
  mutate(value = value + shad) %>% 
  mutate(variable = "DIETER annual average electricity price with scarcity price + shadow price")

# Plotting ----------------------------------------------------------------

swlatex(sw,"\\onecolumn")
swlatex(sw, paste0("\\section{REMIND secondary electricity price}"))

swlatex(sw, paste0("\\subsection{Secondary electricity (seel) price }"))

p<-ggplot() +
  geom_line(data = RM.SEprice, aes(x = iteration, y = value, color = "red"), show.legend = FALSE,size = 1.2, alpha = 0.5) +
  xlab("iteration") + ylab(paste0("REMIND secondary electricity price ($/MWh)"))  +
  coord_cartesian(ylim = c(-20,200))+
  facet_wrap(~period, nrow = 3)

swfigure(sw,print,p,sw_option="width=20, height=12")

if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/SE_elec_price_RM_iteration.png"),  p,  width = 12, height =7, units = "in", dpi = 120)
}

swlatex(sw,"\\onecolumn")
swlatex(sw, paste0("\\subsection{Secondary electricity (seel) price + total demand }"))

secAxisScale = 0.4

p<-ggplot() +
  geom_line(data = out.RMprice, aes(x = iteration, y = value, color = variable), size = 1.2, alpha = 0.5) +
  geom_line(data = out.totalDem, aes(x = iteration, y = value * secAxisScale , color =variable), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(), axis.title=element_text()) +
  scale_y_continuous(sec.axis = sec_axis(~./secAxisScale, name = paste0("total demand ", "(TWh)")))+
  xlab("iteration") + ylab(paste0("REMIND secondary electricity price ($/MWh)"))  +
  coord_cartesian(ylim = c(-20,500))+
  facet_wrap(~period, nrow = 3)

swfigure(sw,print,p,sw_option="width=20, height=12")

if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/SE_elec_price_w_demand_RM_iteration.png"),  p,  width = 12, height =7, units = "in", dpi = 120)
}

swlatex(sw,"\\onecolumn")
swlatex(sw, paste0("\\subsection{Secondary electricity (seel) price + capacity factors}"))


secAxisScale = 1

p<-ggplot() +
  geom_line(data = RM.SEprice, aes(x = iteration, y = value, color = tech), linetype = "dashed", size = 1, alpha = 0.8) +
  geom_line(data = out.remind.capfac, aes(x = iteration, y = value, color = tech), size = 1, alpha = 0.5) +
  scale_y_continuous(sec.axis = sec_axis(~./secAxisScale, name = paste0("CF", "(%)")))+
 scale_color_manual(name = "tech", values = color.mapping.seel.line) +
  theme(axis.text=element_text(), axis.title=element_text()) +
  xlab("iteration") + ylab(paste0("REMIND secondary electricity price ($/MWh) with capacity factors"))  +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(size=15)) +
  coord_cartesian(ylim = c(-20,200))+
  facet_wrap(~period, nrow = 3)

swfigure(sw,print,p2,sw_option="width=20, height=12")
if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/SE_elec_price_w_CF_RM_iteration.png"),  p,  width = 16, height =9, units = "in", dpi = 120)
}

swlatex(sw, paste0("\\subsection{secondary electricity price over time for both models}"))

plot.seelprice <- out.SEPrice %>% 
  filter(iteration %in% c(start_i,start_i+1,start_i+2,maxiter-1))%>% 
  filter(period %in% model.periods.till2100) 

p <- ggplot() + 
  geom_line(data=plot.seelprice, aes(x=as.numeric(period), y=value, color = variable), size = 2, alpha = 0.5) +
  xlab("Period")+ylab("")+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(size=13)) +
  theme(axis.text=element_text(size=15), axis.title=element_text(size= 18, face="bold"),strip.text = element_text(size=15)) +
  coord_cartesian(ylim = c(0,150))+
  facet_wrap(~iteration, nrow = 3,labeller = label_both)

swfigure(sw,print,p,sw_option="width=20, height=12")
if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/SE_elec_price_RMDT_time.png"),  p,  width = 12, height =7, units = "in", dpi = 120)
}

swlatex(sw, paste0("\\subsection{secondary electricity price over time for both models (REMIND price rolling mean)}"))

plot.seelprice <- out.SEPrice.rollmean %>% 
  filter(iteration %in% c(1,2,3,maxiter-1)) %>% 
  filter(period %in% model.periods.till2100) 

p <- ggplot() + 
  geom_line(data=plot.seelprice, aes(x=as.numeric(period), y=value, color = variable), size = 2, alpha = 0.5) +
  xlab("Period")+ylab("")+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(size=13)) +
  theme(axis.text=element_text(size=15), axis.title=element_text(size= 18, face="bold"),strip.text = element_text(size=15)) +
  coord_cartesian(ylim = c(0,150))+
  facet_wrap(~iteration, nrow = 3,labeller = label_both)

swfigure(sw,print,p,sw_option="width=20, height=12")
if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/SE_elec_price_RMDT_RMrollmean_time.png"),  p,  width = 12, height =7, units = "in", dpi = 120)
}
