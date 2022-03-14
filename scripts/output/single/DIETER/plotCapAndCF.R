cat("Plot capacities \n")

# Data preparation (REMIND) -----------------------------------------------
out.remind.capacity <- NULL
out.remind.demand <- NULL
if (length(dieter.files) != 0) {
  for (i in 2:(length(remind.files))){
    
    peak.demand.relfac <- file.path(outputdir, remind.files[i]) %>%  
      read.gdx("p32_peakDemand_relFac", factor = FALSE) %>% 
      filter(ttot %in% model.periods) %>% 
      filter(all_regi == reg) %>%
      select(period=ttot,resfrac = value) 
    
    h2.demand <- file.path(outputdir, remind.files[i]) %>%  
      read.gdx("p32_seh2elh2Dem", factor = FALSE) %>% 
      filter(ttot %in% model.periods) %>% 
      filter(all_regi == reg) %>%
      select(period=ttot,h2dem = value) 
    
    remind.data <- file.path(outputdir, remind.files[i]) %>%  
      read.gdx("v32_usableSeDisp", field="l", factor = FALSE) %>% 
      filter(ttot %in% model.periods) %>% 
      filter(all_regi == reg) %>%
      filter(entySe == "seel") %>%
      select(period=ttot,value) %>% 
      right_join(peak.demand.relfac) %>% 
      full_join(h2.demand) %>% 
      replace(is.na(.), 0) %>% 
      mutate(value = (value - h2dem) * resfrac * 8760 * 1e3) 
    
    it <- as.numeric(str_extract(remind.files[i], "[0-9]+"))
    remind.data$iter <- it
    remind.data$model <- "REMIND"
    out.remind.demand <- rbind(out.remind.demand, remind.data)
  }
}
  for (i in 1:(length(remind.files))){
 
    data.capacity <- file.path(outputdir, remind.files[i]) %>%  
      read.gdx("vm_cap", factors = FALSE, squeeze = FALSE) %>% 
      filter(tall %in% model.periods) %>%
      filter(all_regi == reg) %>%
      filter(all_te %in% names(remind.tech.mapping.narrow)) %>%
      filter(rlf == "1") %>% 
      mutate(value = value * 1e3) %>% #TW->GW
      select(period = tall, tech = all_te, rlf, value) %>% 
      revalue.levels(tech = remind.tech.mapping.narrow) %>%
      dplyr::group_by(period, tech, rlf) %>%
      dplyr::summarise( value = sum(value) , .groups = 'keep' ) %>% 
      dplyr::ungroup(period, tech, rlf) %>% 
      mutate(tech = factor(tech, levels=rev(unique(remind.tech.mapping.narrow))))
    
    it <- as.numeric(str_extract(remind.files[i], "[0-9]+"))
    data.capacity$iter <- it
    data.capacity$model <- "REMIND"
    
    out.remind.capacity <- rbind(out.remind.capacity, data.capacity)
  }
  
  
# Data preparation (DIETER) -----------------------------------------------
out.dieter.data <- NULL
out.remind.capfac <- NULL

if (length(dieter.files) != 0) {
  
  for (i in 1:length(dieter.files)){
    # i=22
    it <- as.numeric(str_extract(dieter.files[i], "[0-9]+"))
    data.real.capfac <-
      file.path(outputdir, dieter.files.report[i]) %>% 
      read.gdx("report_tech", squeeze = F) %>% 
      select(model = X..1, period = X..2, variable = X..4, tech = X..5, value) %>% 
      filter(variable %in% c("REMIND real CapFac (%)")) %>% 
      revalue.levels(tech = dieter.tech.mapping) %>%
      mutate(tech = factor(tech, levels=rev(unique(dieter.tech.mapping))))
    
    data.real.capfac$iter <- it
    data.real.capfac$model <- "REMIND"
    
    out.remind.capfac <- rbind(out.remind.capfac, data.real.capfac)
  
    dieter.data <- file.path(outputdir, dieter.files[i]) %>% 
      read.gdx("p32_report4RM", factor = FALSE, squeeze = FALSE) %>%
      select(period = X..1, tech = X..3, variable=X..4, value)  %>%
      filter(period %in% model.periods) %>%
      filter(tech %in% names(dieter.tech.mapping)) %>%
      filter(variable %in% c("capacity")) %>%
      revalue.levels(tech = dieter.tech.mapping) %>%
      mutate(tech = factor(tech, levels=rev(unique(dieter.tech.mapping)))) 
    
    dieter.data$iter <- it
    dieter.data$model <- "DIETER"
    
    out.dieter.data <- rbind(out.dieter.data, dieter.data)
}
  
  
  out.dieter.capacity <- out.dieter.data %>%
    filter(variable == "capacity") %>%
    mutate(value = value/1e3) %>% #MW->GW
    select(period, tech, value, iter)
  
  out.dieter.capfac <- NULL
  for (i in 1:(length(dieter.files))){
    it <- as.numeric(str_extract(dieter.files[i], "[0-9]+"))
    data.real.capfac <-
      file.path(outputdir, dieter.files.report[i]) %>% 
      read.gdx("report_tech", squeeze = F) %>% 
      select(model = X..1, period = X..2, variable = X..4, tech = X..5, value) %>% 
      filter(variable %in% c("DIETER real avg CapFac (%)")) %>% 
      revalue.levels(tech = dieter.tech.mapping) %>%
      mutate(tech = factor(tech, levels=rev(unique(dieter.tech.mapping))))
    
    data.real.capfac$iter <- it
    data.real.capfac$model <- "DIETER"
    out.dieter.capfac <- rbind(out.dieter.capfac,data.real.capfac)
  }
  
  }

# Plotting ----------------------------------------------------------------
##################################################################################################
swlatex(sw, paste0("\\section{Capacities}"))

for(year_toplot in model.periods){
  if(year_toplot >= 2020){
    
  plot.remind.capacity <- out.remind.capacity %>% 
    filter(period == year_toplot)
  
  if (length(dieter.files) != 0) {
    plot.remind.demand <- out.remind.demand %>% 
      filter(period == year_toplot)
    plot.remind.demand$tech <- "peak demand"

    plot.remind.capfac <- out.remind.capfac %>% 
      filter(period == year_toplot) 
  }
  
  secAxisScale1 = max(plot.remind.capacity$value) / 100
  #get max value
  df.maxval<- plot.remind.capacity %>% 
    dplyr::group_by(period, rlf, iter, model) %>%
    dplyr::summarise( value = sum(value) , .groups = 'keep' ) %>% 
    dplyr::ungroup(period, rlf, iter, model) 
  
  ymax = max(df.maxval$value) * 1.1
  
  swlatex(sw, paste0("\\subsection{Capacities in ", year_toplot, "}"))
  
  p1 <- ggplot() +
    geom_area(data = plot.remind.capacity, aes(x = iter, y = value, fill = tech), size = 1.2, alpha = 0.5) +
    scale_y_continuous(sec.axis = sec_axis(~./secAxisScale1, name = paste0("CF", "(%)")))+
    scale_fill_manual(name = "Technology", values = color.mapping.cap) +
    scale_color_manual(name = "Technology", values = color.mapping.capfac.line) +
    xlab("iteration") + ylab(paste0("capacity", "(GW)")) +
    ggtitle(paste0("REMIND: ", reg, " ", year_toplot))+
    coord_cartesian(xlim = c(0, max(plot.remind.capacity$iter)+1),ylim = c(0, ymax)) +
    theme(legend.title = element_blank()) 
  
  if (length(dieter.files) != 0) {
    p1 <- p1 + 
      geom_line(data = plot.remind.demand, aes(x = iter, y = value, color = tech), size = 1.2, alpha = 2,linetype="dotted")  + 
      geom_line(data = plot.remind.capfac, aes(x = iter, y = value*secAxisScale1, color = tech), size = 1.2, alpha = 1)
  }
  
  if (length(dieter.files) != 0) {
  plot.dieter.capacity <- out.dieter.capacity %>%
    filter(period == year_toplot)
  plot.dieter.capfac <- out.dieter.capfac %>%
      filter(period == year_toplot)

  secAxisScale2 = max(plot.dieter.capacity$value) / 100
  
    p2<-ggplot() +
      geom_area(data = plot.dieter.capacity, aes(x = iter, y = value, fill = tech), size = 1.2, alpha = 0.5) +
      geom_line(data = plot.remind.demand, aes(x = iter, y = value, color = tech), size = 1.2, alpha = 2, linetype="dotted") +
      scale_y_continuous(sec.axis = sec_axis(~./secAxisScale2, name = paste0("CF", "(%)")))+
      scale_fill_manual(name = "Technology", values = color.mapping.cap) +
      scale_color_manual(name = "Technology", values = color.mapping.capfac.line)+
      xlab("iteration") + ylab(paste0("Capacity (GW)")) +
      coord_cartesian(xlim = c(0, max(plot.dieter.capacity$iter)),ylim = c(0, ymax))+
      ggtitle(paste0("DIETER: ", reg, " ", year_toplot)) +
      theme(legend.title = element_blank()) 
  }
  
  grid.newpage()
  if (length(dieter.files) != 0) {
    p2 <- p2 + geom_line(data = plot.dieter.capfac, aes(x = iter, y = value*secAxisScale2, color = tech), size = 1.2, alpha = 1)
    p <- arrangeGrob(rbind(ggplotGrob(p1), ggplotGrob(p2)))
  } else { p<-p1 }
  
  swfigure(sw,grid.draw,p)
  if (save_png == 1){
  if (length(dieter.files) != 0) {  
  
  ggsave(filename = paste0(outputdir, "/DIETER/CAP_", year_toplot, "wCF.png"),  p,  width = 12, height =15, units = "in", dpi = 120)
  }
  else { 
  ggsave(filename = paste0(outputdir, "/DIETER/CAP_", year_toplot, "wCF.png"),  p,  width = 12, height =6, units = "in", dpi = 120) }
  }
}
}
##################################################################################################
swlatex(sw, "\\subsection{Capacities over time (last iteration)}")

plot.remind.capacity <- out.remind.capacity %>% 
  filter(iter == max(out.remind.capacity$iter))

p1<-ggplot() +
  geom_area(data = plot.remind.capacity%>% filter(period %in% model.periods.till2100) , aes(x = period, y = value, fill = tech), size = 1.2, alpha = 0.5) +
  scale_fill_manual(name = "Technology", values = color.mapping.cap) +
  theme(legend.position="none")+
  theme(axis.text=element_text(size=15), axis.title=element_text(size= 20, face="bold"),strip.text = element_text(size=13)) +
  xlab("period") + ylab(paste0("Capacity (GW)")) +
  ggtitle(paste0("REMIND last iteration: ", reg))+
  theme(plot.title = element_text(size = 20, face = "bold"))

if (length(dieter.files) != 0) {
plot.dieter.capacity <- out.dieter.capacity %>%
  filter(iter == max(out.dieter.capacity$iter))

p2<-ggplot() +
    geom_area(data = plot.dieter.capacity%>% filter(period %in% model.periods.till2100), aes(x = as.numeric(period), y = value, fill = tech), size = 1.2, alpha = 0.5) +
    scale_fill_manual(name = "Technology", values = color.mapping.cap) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(size=13)) +
  theme(axis.text=element_text(size=15), axis.title=element_text(size= 20, face="bold"),strip.text = element_text(size=13)) +
    xlab("period") + ylab(paste0("Capacity (GW)")) +
    ggtitle(paste0("DIETER last iteration: ", reg))+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(size=20))+
  theme(plot.title = element_text(size = 20, face = "bold"))

}

grid.newpage()
if (length(dieter.files) != 0) {
  p <- arrangeGrob(rbind(ggplotGrob(p1), ggplotGrob(p2)))
} else { p<-p1 }

swfigure(sw,grid.draw,p)

if (save_png == 1){
  if (length(dieter.files) != 0) {
  ggsave(filename = paste0(outputdir, "/DIETER/CAP_time.png"),  p,  width = 12, height =15, units = "in", dpi = 120)
  }else { 
  ggsave(filename = paste0(outputdir, "/DIETER/CAP_time.png"),  p,  width = 12, height =6, units = "in", dpi = 120) }
}
  
##################################################################################################
if (length(dieter.files) != 0) {
swlatex(sw, paste0("\\section{Capacity factors}"))

for(year_toplot in model.periods){
  plot.remind <- out.remind.capfac %>% 
    filter(period == year_toplot)
  
  plot.dieter <- out.dieter.capfac %>% 
    filter(period == year_toplot) 
  
  swlatex(sw, paste0("\\subsection{Capacity factors in ", year_toplot, "}"))
  
  p <- ggplot() + 
    geom_line(data=plot.remind, aes(x=iter, y=value, color=variable, linetype = model)) + 
    geom_line(data=plot.dieter, aes(x=iter, y=value, color=variable, linetype = model)) +
    scale_color_manual(name = "variable", values = color.mapping.cf)+
    theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(size=7)) +
    xlab("Iteration") + 
    ylab("Capacity factor") + 
    facet_wrap(~tech, nrow=3)
  
  swfigure(sw,print,p)
  if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/CF_", year_toplot, ".png"),  p,  width = 6, height =5, units = "in", dpi = 120)
  }
}

##################################################################################################
swlatex(sw, "\\subsection{Capacity factors over time (last iteration): detailed}")

data.capfac <-
  file.path(outputdir, dieter.files.report[length(dieter.files.report)]) %>% 
  read.gdx("report_tech", squeeze = F) %>% 
  select(model = X..1, period = X..2, variable = X..4, tech = X..5, value) %>% 
  filter(variable %in% capfac.detail.report.dieter) %>% 
  revalue.levels(tech = dieter.tech.mapping) %>%
  mutate(tech = factor(tech, levels=rev(unique(dieter.tech.mapping))))
  
p <- ggplot() +
  geom_line(data = data.capfac, aes(x = as.numeric(period), y = value, color = variable, linetype = model), size = 1.2, alpha = 1) +
  scale_color_manual(name = "variable", values = color.mapping.cf.detail) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10,face="bold")) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(size=10)) +
  facet_wrap(~tech, nrow = 3)+
  xlab("Time") +
  ylab("Capacity factor")

swfigure(sw,print,p)

if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/CF_compare_time.png"),  p,  width = 15, height =8, units = "in", dpi = 120)
}

if (length(dieter.files) != 0) {
for (i in c(5,10,20,27,maxiter-1)){
  # i = 27
  plot.remind.snap <- out.remind.capacity %>% 
    filter(iter == i) %>% 
    filter(period <2110)%>%
    mutate(period = as.numeric(period))%>% 
    dplyr::rename(remind_cap = value)
  
  plot.dieter.snap <- out.dieter.capacity %>%
    filter(iter == i) %>%
    filter(period <2110)%>%
    mutate(period = as.numeric(period)) %>%
    dplyr::rename(dieter_cap = value)
  
  plot.diff <- list(plot.remind.snap, plot.dieter.snap) %>%
    reduce(full_join) %>%
    mutate(delta_cap = remind_cap - dieter_cap) 
  
  p <-ggplot() +
    geom_bar(data = plot.diff , aes(x = period, y = delta_cap, fill = tech, label = delta_cap),  alpha = 0.5, stat = "identity") +
    geom_label(size = 3, position = position_stack(vjust = 0.5)) +
    scale_fill_manual(name = "Technology", values = color.mapping)+
    theme(axis.text=element_text(size=10), axis.title=element_text(size= 10,face="bold")) +
    xlab("period") + ylab(paste0("Capacity (GW)")) +
    ggtitle(paste0("Capacity difference REMIND - DIETER"))+
    theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank()) +
    theme(aspect.ratio = .5) 
  
  swfigure(sw, grid.draw, p)
  if (save_png == 1){
    ggsave(filename = paste0(outputdir, "/DIETER/deltaCapacity_time_i", i, ".png"),  p,  width = 8, height =10, units = "in", dpi = 120)
  }

}
}
}
