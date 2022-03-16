# Data preparation --------------------------------------------------------

cat("Plot Seel price \n")

out.SEPrice <- NULL
out.SEPrice.rollmean <- NULL
out.RMprice <- NULL
out.DTprice <- NULL
out.totalDem <- NULL

for (i in 2:length(remind.files)){
  
  RMprice <- file.path(outputdir, remind.files[i]) %>% 
    read.gdx( "pm_SEPrice", squeeze = FALSE) %>% 
    filter(all_enty == "seel") %>%
    filter(all_regi == reg) %>% 
    mutate(value = value * 1e12 / sm_TWa_2_MWh * 1.2)  %>%  # (10^12 2005$)/TWa -> 2015$/MWh
    select(period=ttot, value) %>%
    mutate(iteration = i-1) %>%
    mutate(variable= "REMIND secondary electricity price ($/MWh)")
  
  DTprice  <- file.path(outputdir, remind.files[i]) %>% 
    # read.gdx( "p32_DIETER_elecprice_wscar", squeeze = FALSE) %>% 
    read.gdx( "p32_DIETER_elecprice", squeeze = FALSE) %>% 
    select(period=ttot, value) %>%
    mutate(iteration = i-1) %>%
    mutate(variable = "DIETER secondary electricity price ($/MWh)") %>% 
    mutate(value = value *1.2) # convert to 2015 dollar
  
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
  out.DTprice <- rbind(out.DTprice, DTprice)
  out.SEPrice <- rbind(out.SEPrice, RMprice, DTprice)
  out.SEPrice.rollmean <- rbind(out.SEPrice.rollmean, RMprice.rollmean, DTprice)
  out.totalDem <- rbind(out.totalDem, p32_seelTotDem)
}

RM.SEprice <- out.RMprice %>% 
  select(period, value,iteration,tech=variable)
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
  geom_line(data = out.remind.capfac, aes(x = iter, y = value, color = tech), size = 1, alpha = 0.5) +
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
  filter(iteration %in% c(1,2,3,maxiter-1))%>% 
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
