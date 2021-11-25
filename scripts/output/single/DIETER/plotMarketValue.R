# Data preparation --------------------------------------------------------

cat("Plot Market Value \n")

out.markup.data <- NULL
out.cap.constraint <- NULL
out.marketValue.data<- NULL
out.remind.genshare <- NULL
out.SE.price <- NULL
out.tax.rev <- NULL
out.p32.DIETER.VF <- NULL
out.p32.DIETER.MV <- NULL
out.DIETER.elecprice <- NULL
out.budget <- NULL
out.tax.rev.lastiter <- NULL

for (i in 1:length(remind.files)){
  
  budget.data <- file.path(outputdir, remind.files[i]) %>%
    read.gdx("qm_budget", field="m", squeeze = FALSE) %>%
    filter(all_regi == "DEU") %>%
    mutate(m = -m) %>%
    dplyr::rename(budget = m)  %>%
    select(period= ttot, budget)
  
  budget.data$iter <- i
  budget.data$model <- "budget"
  
  out.budget <- rbind(out.budget,budget.data)
  
  capcon.initial <- file.path(outputdir, remind.files[i]) %>%
    read.gdx( "q32_peakDemandDT", field="m") %>%
    mutate(m = m) %>%
    select(capcon = m, period = ttot)
  
  # transform from tr$2005/TW to $2015/kW
  capcon.data = list(capcon.initial, budget.data) %>%
    reduce(full_join) %>%
    select(period, capcon, budget) %>%
    replace(is.na(.), 0) %>%
    mutate(capcon = capcon/ budget * 1e12 / 1e9 * 1.2) %>%
    select(period, capcon)
  
  capcon.data$iter <- i
  capcon.data$model <- "capacity price"
  
  out.cap.constraint <- rbind(out.cap.constraint,capcon.data)
  
  vm.Mrkup.data <- file.path(outputdir, remind.files[i]) %>%
    read.gdx("vm_Mrkup", field="l", squeeze = FALSE)  %>% 
    filter(all_regi == "DEU") %>% 
    filter(all_te %in% FLEX_tech) %>% 
    dplyr::rename(ttot = tall)
  
  vm.flexAdj.data <- file.path(outputdir, remind.files[i]) %>%
    read.gdx("vm_flexAdj", field="l", squeeze = FALSE)  %>% 
    filter(all_regi == "DEU") %>% 
    filter(all_te %in% FLEX_tech2) %>% 
    dplyr::rename(ttot = tall) %>% 
    mutate(value = value)
  
  budgetdata <-budget.data %>% 
    dplyr::rename(ttot = period)
  
  markup.data <- list(vm.Mrkup.data, vm.flexAdj.data) %>%
    reduce(full_join) %>% 
    left_join(budgetdata) %>% 
    mutate(value = value * 1e12 / sm_TWa_2_MWh * 1.2) %>%  # markup is proportional to pm_seeprice, which is already divided by budget
    filter(all_te %in% names(remind.tech.mapping)) %>% 
    revalue.levels(all_te = remind.tech.mapping) %>% 
    select(ttot,all_te,value) %>% 
    dplyr::group_by(ttot,all_te) %>%
    dplyr::summarise( value = mean(value), .groups = "keep" ) %>% 
    dplyr::ungroup(ttot,all_te) %>% 
    select(tech=all_te,ttot,value)
  
  markup.data$iter <- i
  markup.data$model <- "REMIND markup ($/MWh)"
  out.markup.data <- rbind(out.markup.data, markup.data)
  
  p32.marketValue <- file.path(outputdir, remind.files[i]) %>%
    read.gdx("p32_marketValue") %>%
    mutate(marketvalue = value) %>%
    filter(all_te %in% FLEX_tech) %>%
    revalue.levels(all_te = remind.tech.mapping) %>%
    mutate(marketvalue = marketvalue * 1e12 / sm_TWa_2_MWh * 1.2) %>%
    dplyr::group_by(ttot,all_te) %>%
    dplyr::summarise( marketvalue = mean(marketvalue), .groups = "keep" ) %>%
    dplyr::ungroup(ttot,all_te) %>%
    select(period= ttot, tech=all_te, marketvalue)
  
  p32.marketPrice <- file.path(outputdir, remind.files[i]) %>%
    read.gdx("p32_marketPrice") %>%
    mutate(marketvalue = value) %>%
    filter(all_te %in% FLEX_tech2) %>%
    revalue.levels(all_te = remind.tech.mapping) %>%
    mutate(marketvalue = marketvalue * 1e12 / sm_TWa_2_MWh * 1.2) %>%
    select(period= ttot, tech=all_te, marketvalue)
  
  marketValue.data<- list(p32.marketValue, p32.marketPrice) %>%
    reduce(full_join)
  
  marketValue.data$iter <- i
  marketValue.data$model <- "REMIND market value ($/MWh)"
  out.marketValue.data <- rbind(out.marketValue.data,marketValue.data)
  
  
  remind.genshare <-file.path(outputdir, remind.files[i]) %>%
    read.gdx("v32_shSeEl")  %>% 
    filter(all_regi == "DEU") %>% 
    filter(all_te %in% FLEX_tech) %>% 
    mutate(genshare = value) %>% 
    revalue.levels(all_te = remind.tech.mapping) %>%
    dplyr::group_by(ttot, all_te) %>%
    dplyr::summarise( genshare = sum(genshare), .groups = "keep" ) %>% 
    dplyr::ungroup(ttot, all_te) %>% 
    select(period=ttot, tech=all_te, genshare) 
  
  remind.genshare$iter <- i
  remind.genshare$model <- "REMIND"
  out.remind.genshare <- rbind(out.remind.genshare,remind.genshare)
  
  
  SE.price <- file.path(outputdir, remind.files[i]) %>%
    read.gdx("q32_balSe", field="m") %>%
    filter(all_regi == "DEU") %>% 
    mutate(m = -m) %>% 
    dplyr::rename(SEprice = m) %>%
    select(period= ttot,SEprice) %>% 
    left_join(budget.data) %>% 
    mutate(SEprice = SEprice/budget * 1e12 / sm_TWa_2_MWh * 1.2) %>% 
    select(period,SEprice)
  
  SE.price$iter <- i
  out.SE.price <- rbind(out.SE.price,SE.price)
  
  budgetdata <- budgetdata %>% 
    filter(ttot > 2005)
  
  v21.taxrevMrkup <- file.path(outputdir, remind.files[i]) %>%
    read.gdx("v21_taxrevMrkup") %>% 
    filter(all_regi == "DEU") %>% 
    filter(ttot > 2005)
  
  tax.rev <- list(v21.taxrevMrkup, budgetdata) %>%
    reduce(full_join) %>%
    mutate(value = value / budget * 1e12 / sm_TWa_2_MWh * 1.2) 
  
  tax.rev$iter <- i
  tax.rev$model <- "total markup tax (REMIND)"
  out.tax.rev <- rbind(out.tax.rev,tax.rev)
  
  
  p21.taxrevMrkup <- file.path(outputdir, remind.files[i]) %>%
    read.gdx("p21_taxrevMrkup0", squeeze = F) %>% 
    filter(all_regi == "DEU") 
  
  tax.rev.lastiter <- list(p21.taxrevMrkup, budgetdata) %>%
    reduce(full_join) %>%
   # replace(is.na(.), 0) %>%
    mutate(value = value / budget * 1e12 / sm_TWa_2_MWh * 1.2)
  
  tax.rev.lastiter$iter <- i
  tax.rev.lastiter$model <- "reference markup from last iteration (REMIND)"
  out.tax.rev.lastiter <- rbind(out.tax.rev.lastiter,tax.rev.lastiter)
  
  
  p32.DIETER.VF <- file.path(outputdir, remind.files[i]) %>%
    read.gdx("p32_DIETER_VF", squeeze = F) %>% 
    filter(all_te %in% FLEX_tech) %>% 
    revalue.levels(all_te = remind.tech.mapping) %>%
    dplyr::group_by(ttot, all_te) %>%
    dplyr::summarise( value = mean(value), .groups = "keep" ) %>% 
    dplyr::ungroup(ttot, all_te) %>% 
    select(period=ttot, tech=all_te, value)
  
  p32.DIETER.VF$iter <- i
  p32.DIETER.VF$model <- "DIETER value factor"
  out.p32.DIETER.VF <- rbind(out.p32.DIETER.VF,p32.DIETER.VF)
  
  p32.DIETER.MV <- file.path(outputdir, remind.files[i]) %>%
    read.gdx( "p32_DIETER_MV", squeeze = F) %>% 
    filter(all_te %in% FLEX_tech) %>% 
    revalue.levels(all_te = remind.tech.mapping) %>%
    dplyr::group_by(ttot, all_te) %>%
    dplyr::summarise( value = mean(value), .groups = "keep" ) %>% 
    dplyr::ungroup(ttot, all_te) %>% 
    select(period=ttot, tech=all_te, value)
  
  p32.DIETER.MV$iter <- i
  out.p32.DIETER.MV <- rbind(out.p32.DIETER.MV,p32.DIETER.MV)
  
  
  DIETER.elecprice <- file.path(outputdir, remind.files[i]) %>%
    read.gdx( "p32_DIETER_elecprice", squeeze = F) %>% 
    select(period=ttot,DT_price=value)
  
  DIETER.elecprice$iter <- i
  out.DIETER.elecprice <- rbind(out.DIETER.elecprice,DIETER.elecprice)
}

out.markup.data2 <- out.markup.data %>% 
  dplyr::rename(markup = value) %>% 
  select(period=ttot, iter,tech, markup,model)

dieter.markup <- list(out.p32.DIETER.MV, DIETER.elecprice) %>%
  reduce(full_join) %>% 
  mutate(dieter_mrkup = value - DT_price) %>% 
  mutate(model = "DIETER markup ($/MWh)")



# Plotting ----------------------------------------------------------------

swlatex(sw,"\\onecolumn")
swlatex(sw, paste0("\\section{Market Value}"))


swlatex(sw, paste0("\\subsection{REMIND capconShadow (USD/MWh) }"))

secAxisScale = 1/8.76

p1<-ggplot() +
  geom_line(data = out.cap.constraint, aes(x = iter, y = -capcon, color = model), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(), axis.title=element_text()) +
  xlab("iteration") + scale_y_continuous(name = paste0("q32_peakDemandDT (USD/kW)"))+
  # xlab("iteration") + scale_y_continuous(trans='log10', name = paste0(CapConstraintKey, "(USD/kW)"))+
  coord_cartesian(ylim = c(-1000,1000))+
  facet_wrap(~period, nrow = 3)

swfigure(sw,print,p1,sw_option="width=20, height=12")

swlatex(sw,"\\onecolumn")
swlatex(sw, paste0("\\subsection{REMIND absolute markup (USD/MWh) }"))

p2<-ggplot() +
  geom_line(data = out.markup.data2, aes(x = iter, y = markup, color=tech), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(), axis.title=element_text()) +
  xlab("iteration") + ylab(paste0("absolute markup",  "(USD/MWh)"))  +
  scale_color_manual(name = "tech", values = mycolors)+
  coord_cartesian(ylim = c(-70,150))+
  facet_wrap(~period, nrow = 3)

swfigure(sw,print,p2,sw_option="width=20, height=12")

swlatex(sw,"\\onecolumn")
swlatex(sw, paste0("\\subsection{REMIND absolute markup(10,20) (USD/MWh)}"))



p3<-ggplot() +
  geom_line(data = out.markup.data %>% filter(iter %in% c(10,20,maxiter-1)), aes(x = ttot, y = value, color=tech), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(), axis.title=element_text(), strip.text = element_text()) +
  xlab("year") + ylab(paste0("absolute markup",  "(USD/MWh)"))  +
  scale_color_manual(name = "tech", values = mycolors2)+
  # ggtitle(paste0("REMIND (100$/tCO2)"))+
  # theme(plot.title = element_text(size = 19, face = "bold"))+
  coord_cartesian(ylim = c(-70,150))+
  facet_wrap(~iter, nrow = 3)

swfigure(sw,print,p3,sw_option="width=20, height=12")

swlatex(sw,"\\onecolumn")
swlatex(sw, paste0("\\subsection{REMIND absolute markup Time Series (USD/MWh)}"))


p4<-ggplot() +
  geom_line(data = out.markup.data %>% filter(iter %in% c(maxiter-1)), aes(x = ttot, y = value, color=tech), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"), strip.text = element_text(size = 20)) +
  xlab("year") + ylab(paste0("absolute markup",  "(USD/MWh)"))  +
  scale_color_manual(name = "tech", values = mycolors2)+
  coord_cartesian(ylim = c(-70,150))+
  facet_wrap(~iter, nrow = 3)

swfigure(sw,print,p4,sw_option="width=20, height=12")

swlatex(sw,"\\onecolumn")
swlatex(sw, paste0("\\subsection{Market Value (REMIND) (USD/MWh)}"))


p5<-ggplot() +
  geom_line(data = out.marketValue.data, aes(x = iter, y = marketvalue, color = tech), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20, face="bold")) +
  xlab("iteration") + ylab(paste0("Market Value (REMIND)", "(USD/MWh)"))  +
  scale_color_manual(name = "tech", values = mycolors2)+
  coord_cartesian(ylim = c(-40,250))+
  facet_wrap(~period, nrow = 3)

swfigure(sw,print,p5,sw_option="width=20, height=12")

swlatex(sw,"\\onecolumn")
swlatex(sw, paste0("\\subsection{Total tax markup (REMIND) (USD/MWh)}"))

p6<-ggplot() +
  geom_line(data = out.tax.rev, aes(x = iter, y = value, color = model), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size= 20,face="bold")) +
  xlab("iteration") + ylab(paste0("Total tax markup (REMIND) = markup - reference markup from last iteration"))  +
  coord_cartesian(ylim = c(-50,50))+
  #scale_y_continuous(trans=ssqrt_trans)+
  facet_wrap(~ttot, nrow = 3)

swfigure(sw,print,p6,sw_option="width=20, height=12")

swlatex(sw,"\\onecolumn")
swlatex(sw, paste0("\\subsection{reference markup from last iteration (REMIND)}"))

p7<-ggplot() +
  geom_line(data = out.tax.rev.lastiter, aes(x = iter, y = value, color = model), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size= 20,face="bold")) +
  xlab("iteration") + ylab(paste0("reference markup from last iteration (REMIND)"))  +
  coord_cartesian(ylim = c(-5,5))+
  # scale_y_continuous(trans=ssqrt_trans)+
  facet_wrap(~ttot, nrow = 3)

swfigure(sw,print,p7,sw_option="width=20, height=12")

# swlatex(sw,"\\onecolumn")
# swlatex(sw, paste0("\\subsection{generation share (%)}"))
# 
# p8<-ggplot() +
#   geom_line(data = out.remind.genshare, aes(x = iter, y = genshare, color = tech,linetype=model), size = 1.2, alpha = 0.5) +
#   geom_line(data = dieter.data.genshare, aes(x = iter, y = VALUE, color = TECH,linetype=model), size = 1.2, alpha = 0.5) +
#   theme(axis.text=element_text(size=20), axis.title=element_text(size= 20,face="bold")) +
#   xlab("iteration") + ylab(paste0("generation share (%)"))  +
#   scale_color_manual(name = "tech", values = mycolors)+
#   scale_linetype_manual(name = "model", values = linetypemap)+
#   theme(legend.title = element_text(size=25),legend.text = element_text(size=25))+
#   coord_cartesian(ylim = c(0,80))+
#   facet_wrap(~period, nrow = 3)
# 
# swfigure(sw,print,p8,sw_option="width=20, height=12")

swlatex(sw,"\\onecolumn")
swlatex(sw, paste0("\\subsection{budget (USD/MWh)}"))

p9<-ggplot() +
  geom_line(data = out.budget, aes(x = period, y = budget), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(), axis.title=element_text()) +
  xlab("iteration") + ylab(paste0("budget", "(USD/MWh)"))  +
  coord_cartesian(ylim = c(0,.26))+
  facet_wrap(~iter, nrow = 3)

swfigure(sw,print,p9,sw_option="width=20, height=12")


swlatex(sw,"\\onecolumn")
#swlatex(sw, paste0("\\section{Market Value (USD/MWh)}"))
secAxisScale = .5

for(te in FLEX_tech_names){

  swlatex(sw, paste0("\\subsection{Market Value (USD/MWh) -",te,"}"))
  p10<-ggplot() +
    geom_line(data = out.markup.data2%>% filter(tech == te), aes(x = iter, y = markup, color = model), size = 1.2, alpha = 0.5) +
    geom_line(data = out.marketValue.data%>% filter(tech == te), aes(x = iter, y = marketvalue, color = model), size = 1.2, alpha = 0.5) +
    geom_line(data = out.pm.SEPrice, aes(x = iteration, y = value, color = model), size = 1.2, alpha = 0.5) +
    geom_line(data = dieter.data.marketValue%>% filter(TECH == te), aes(x = iter, y = VALUE, color = model), size = 1.2, alpha = 0.5) +
    geom_line(data = dieter.data.seelprice, aes(x = iter, y = VALUE, color = model), size = 1.2, alpha = 0.5) +
    theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20, face = "bold")) +
    theme(legend.text = element_text(size=20))+
    theme(legend.title = element_blank()) +
    xlab("iteration") + ylab(paste0("USD/MWh")) +
    ggtitle(paste0("TECH: ", te))+
    theme(legend.position = "bottom") +
    guides(color = guide_legend(nrow = 2, byrow = TRUE))+
    theme(plot.title = element_text(size = 26, face = "bold"))+
    coord_cartesian(ylim = c(-100,200)) +
    facet_wrap(~period, nrow = 3)

  swfigure(sw,print,p10,sw_option="width=20, height=12")
}

remind.generation.withCurt <- remind.generation.withCurt %>%
  select(tech=all_te,period,value,iteration)  %>%
  rename(iter = iteration)
for (iterp in c(14,19,24,maxiter-1)){

  vrN_mv_onlygen0 <- list(remind.generation.withCurt,out.marketValue.data) %>%
    reduce(full_join)%>% filter(iter == iterp) %>%
    filter(value >1e-4) %>%
    filter((period >2015) & (period < 2110))

  vrN_demandTech_mv <- out.marketValue.data %>%
    filter(tech %in% c("Electricity (stationary)",
                       "Electrolyzers"))%>% filter(iter == iterp) %>%
    filter((period >2015) & (period < 2110))

  plot.mv.onlygen<- list(vrN_mv_onlygen0,vrN_demandTech_mv) %>%
    reduce(full_join)
  swlatex(sw,"\\onecolumn")
  swlatex(sw, paste0("\\subsection{Market (REMIND) (USD/MWh)- ",iterp,"}"))

  p11<-ggplot() +
    geom_line(data = plot.mv.onlygen, aes(x = period, y = marketvalue, color = tech), size = 1.2, alpha = 0.5) +
    theme(axis.text=element_text(size=20), axis.title=element_text(size=20, face="bold")) +
    xlab("year") + ylab(paste0("Market Value (REMIND)", "(USD/MWh)"))  +
    scale_color_manual(name = "tech", values = mycolors2)+
    coord_cartesian(ylim = c(-40,250))

  swfigure(sw,print,p11,sw_option="width=20, height=12")

  vrN_mk_onlygen0 <- list(remind.generation.withCurt, out.markup.data2) %>%
    reduce(full_join)%>% filter(iter == iterp) %>%
    filter(value >1e-4) %>%
    filter((period >2015) & (period < 2110))

  vrN_demandTech_mk <- out.markup.data2 %>%
    filter(tech %in% c("Electricity (stationary)",
                       "Electrolyzers")) %>% filter(iter == iterp) %>%
    filter((period >2015) & (period < 2110))

  plot.mk.onlygen<- list(vrN_mk_onlygen0, vrN_demandTech_mk) %>%
    reduce(full_join)

  swlatex(sw,"\\onecolumn")
  swlatex(sw, paste0("\\subsection{Markup (REMIND) (USD/MWh)- ",iterp,"}"))

  p12<-ggplot() +
    geom_line(data = plot.mk.onlygen, aes(x = period, y = markup, color = tech), size = 1.2, alpha = 0.5) +
    theme(axis.text=element_text(size=20), axis.title=element_text(size=20, face="bold")) +
    xlab("year") + ylab(paste0("Markup (REMIND)", "(USD/MWh)"))  +
    scale_color_manual(name = "tech", values = mycolors2)+
    coord_cartesian(ylim = c(-40,80))

  swfigure(sw,print,p12,sw_option="width=20, height=12")

  swlatex(sw,"\\onecolumn")
  swlatex(sw, paste0("\\subsection{market value and markup time series (USD/MWh)- ",iterp,"}"))

  p13<-ggplot() +
    geom_line(data = plot.mk.onlygen, aes(x = period, y = markup, color = model), size = 1.2, alpha = 0.5) +
    geom_line(data = plot.mv.onlygen, aes(x = period, y = marketvalue, color = model), size = 1.2, alpha = 0.5) +
    geom_line(data = dieter.data.marketValue %>% 
                filter(iter == iterp) %>%
                filter((as.integer(period) >2015) & (as.integer(period) < 2110))%>% 
                filter(!TECH %in% c("Hard coal", "Lignite")), 
              aes(x = as.integer(period), y = VALUE, color = model), size = 1.2, alpha = 0.5) +
    theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20, face = "bold")) +
    theme(legend.text = element_text(size=20), strip.text = element_text(size = 20))+
    theme(legend.title = element_blank()) +
    theme(panel.spacing = unit(1, "lines")) +
    xlab("year") + ylab(paste0("USD/MWh")) +
    ggtitle(paste0("market value and markup time series"))+
    scale_color_manual(name = "model", values = color.mapping.var)+
    theme(aspect.ratio = .6) +
    theme(legend.position = "bottom") +
    guides(color = guide_legend(nrow = 2, byrow = TRUE))+
    theme(plot.title = element_text(size = 26, face = "bold"))+
    coord_cartesian(xlim = c(2020,2100), ylim = c(-30,220)) +
    facet_wrap(~tech, nrow = 3)

  swfigure(sw,print,p13,sw_option="width=20, height=12")

}



  
 
  
  
 
  


