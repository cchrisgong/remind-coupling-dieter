

cat("Plot capacities \n")


# Data preparation (REMIND) -----------------------------------------------
out.remind.capacity <- NULL
out.remind.demand <- NULL
out.remind.capfac <- NULL

if (length(dieter.files) != 0) {
  for (i in 1:(length(remind.files)-1)){
    
    
    peak.demand.relfac <-  file.path(outputdir, remind.files[i]) %>%  
      read.gdx("p32_peakDemand_relFac", factor = FALSE) %>% 
      filter(ttot %in% report.periods) %>% 
      filter(all_regi == "DEU") %>%
      select(period=ttot,resfrac = value) 
    
    h2.demand <- file.path(outputdir, remind.files[i]) %>%  
      read.gdx("p32_seh2elh2Dem", factor = FALSE) %>% 
      filter(ttot %in% report.periods) %>% 
      filter(all_regi == "DEU") %>%
      select(period=ttot,h2dem = value) 
    
    remind.data <- file.path(outputdir, remind.files[i]) %>%  
      read.gdx("p32_seelUsableDem", field="l", factor = FALSE) %>% 
      filter(ttot %in% report.periods) %>% 
      filter(all_regi == "DEU") %>%
      filter(all_enty == "seel") %>%
      select(period=ttot,value) %>% 
      right_join(peak.demand.relfac) %>% 
      right_join(h2.demand) %>% 
      mutate(value = (value - h2dem) * resfrac * 8760 * 1e3) 
    
    remind.data$iter <- i
    remind.data$model <- "REMIND"
    out.remind.demand <- rbind(out.remind.demand, remind.data)
    
    data.capacity <- file.path(outputdir, remind.files[i]) %>%  
      read.gdx("vm_cap", factors = FALSE, squeeze = FALSE) %>% 
      filter(tall %in% report.periods) %>%
      filter(all_regi == "DEU") %>%
      filter(all_te %in% names(remind.tech.mapping)) %>%
      filter(rlf == "1") %>% 
      mutate(value = value * 1e3) %>% #TW->GW
      select(period = tall, all_te, rlf, value) %>% 
      revalue.levels(all_te = remind.tech.mapping) %>%
      dplyr::group_by(period, all_te, rlf) %>%
      dplyr::summarise( value = sum(value) , .groups = 'keep' ) %>% 
      dplyr::ungroup(period, all_te, rlf) %>% 
      mutate(all_te = factor(all_te, levels=rev(unique(remind.tech.mapping))))
    
    data.capacity$iter <- i
    data.capacity$model <- "REMIND"
    
    out.remind.capacity <- rbind(out.remind.capacity, data.capacity)
    
    
    # first the dispatchable
    data.capfac <- file.path(outputdir, remind.files[i]) %>%  
      read.gdx("vm_capFac", field="l", squeeze = FALSE) %>% 
      filter(ttot %in% report.periods) %>%
      filter(all_te %in% names(remind.tech.mapping)) %>% 
      filter(all_regi == "DEU") %>%
      revalue.levels(all_te = remind.tech.mapping) %>%
      filter(!(all_te %in% remind.vre.mapping)) %>% 
      select(period = ttot, all_te, value) %>% 
      dplyr::group_by(period, all_te) %>%
      dplyr::summarise(value = mean(value), .groups = "keep") %>% 
      dplyr::ungroup(period, all_te)
    
    # second the VRE
    # pm_dataren("nur") = capacity factor at different grade levels (quality of wind resources), 
    #pm_dataren is used to represent different cost levels necessary to create the same amount of energy if you have to build your wind farm in places with lower wind. The grades are ordered from 1 to 10. 
    #1 is closer to the wind always blowing (highest nur capacity factor), 10 is the worst place to install wind.
    dataren <- file.path(outputdir, remind.files[i]) %>%  
      read.gdx("pm_dataren") %>% 
      #filter(ttot %in% report.periods) %>%
      filter(char == "nur") %>%
      filter(all_regi == "DEU") %>%
      filter(all_te %in% names(remind.vre.mapping)) %>% 
      revalue.levels(all_te = remind.vre.mapping) %>%
      select(all_te, rlf, ren_nur= value)
    
    #vm_capDistr is a "helper" variable that stores the amount of capacity that REMIND assign to each grade. 
    #Therefore it can have values from any grade. vm_cap is the total capacity (sum of vm_capDistr). 
    #It does not have grade infor anymore, everything is assigned to "1". 
    #vm_capDistr will retain the information of the optimal grades used in the REMIND decision
    
    percentage_cap_distr <- file.path(outputdir, remind.files[i]) %>%  
      read.gdx("vm_capDistr") %>% 
      filter(tall %in% report.periods) %>%
      filter(all_regi == "DEU") %>% 
      filter(all_te %in% names(remind.vre.mapping)) %>% 
      revalue.levels(all_te = remind.vre.mapping) %>% 
      select(period=tall, all_te, rlf, cap_distr = value) %>% 
      dplyr::group_by(period, all_te) %>% 
      transmute(rlf, percentage_cap_distr = cap_distr/sum(cap_distr))
    
    # vrdata2_0 = list(dataren, percentage_cap_distr) %>%
    #   reduce(right_join)
    
    data.remind.capfac <- list(dataren, percentage_cap_distr) %>%
      reduce(right_join) %>% 
      select(period, all_te, ren_nur, percentage_cap_distr) %>% 
      replace(is.na(.), 0) %>%
      dplyr::group_by(period, all_te) %>%
      dplyr::summarise(value = sum(ren_nur * percentage_cap_distr), .groups = "keep" ) %>% 
      dplyr::ungroup(period, all_te)
    
    vrdata_tot <- list(data.capfac, data.remind.capfac) %>% 
      reduce(full_join) %>% 
      dplyr::rename(tech = all_te) %>% 
      mutate(value = value * 100)
    
    vrdata_tot$iter <- i
    
    out.remind.capfac <- rbind(out.remind.capfac,vrdata_tot)
    
  }
}

# Data preparation (DIETER) -----------------------------------------------
out.dieter.data <- NULL
if (length(dieter.files) != 0) {
  for (i in 1:length(sorted_files_DT)){
    dieter.data <- file.path(outputdir, sorted_files_DT[i]) %>% 
      read.gdx("p32_report4RM", factor = FALSE, squeeze = FALSE) %>%
      select(period = X..1, all_te = X..3,variable=X..4,value)  %>%
      filter(period %in% report.periods) %>%
      filter(all_te %in% names(dieter.tech.mapping)) %>%
      filter(variable %in% c("capacity","capfac")) %>%
      revalue.levels(all_te = dieter.tech.mapping) %>%
      mutate(all_te = factor(all_te, levels=rev(unique(dieter.tech.mapping)))) 
    
    dieter.data$iter <- id[i]
    dieter.data$model <- "DIETER"
    
    out.dieter.data <- rbind(out.dieter.data, dieter.data)
  }
  out.dieter.capfac <- out.dieter.data %>%
    filter(variable == "capfac") %>%
    mutate(value = value * 100) %>%
    select(period, tech=all_te, value, iter)
  
  out.dieter.capacity <- out.dieter.data %>%
    filter(variable == "capacity") %>%
    mutate(value = value/1e3) %>% #MW->GW
    select(period, all_te, value, iter)
}
# Plotting ----------------------------------------------------------------

swlatex(sw, paste0("\\section{Capacities}"))

for(year_toplot in report.periods){
  plot.remind.capacity <- out.remind.capacity %>% 
    filter(period == year_toplot)
  
  if (length(dieter.files) != 0) {
    plot.remind.demand <- out.remind.demand %>% 
      filter(period == year_toplot)
    plot.remind.demand$tech <- "peak demand"
  }
  
  plot.remind.capfac <- out.remind.capfac %>% 
    filter(period == year_toplot)
  
  secAxisScale1 = max(plot.remind.capacity$value) / 100
  #get max value
  df.maxval<- plot.remind.capacity %>% 
    dplyr::group_by(period, rlf, iter, model) %>%
    dplyr::summarise( value = sum(value) , .groups = 'keep' ) %>% 
    dplyr::ungroup(period, rlf, iter, model) 
  
  ymax = max(df.maxval$value) * 1.1
  
  swlatex(sw, paste0("\\subsection{Capacities in ", year_toplot, "}"))
  
  p0<-ggplot() +
    geom_area(data = plot.remind.capacity, aes(x = iter, y = value, fill = all_te), size = 1.2, alpha = 0.5) +
    geom_line(data = plot.remind.capfac, aes(x = iter, y = value*secAxisScale1, color = tech), size = 1.2, alpha = 0.5) + 
    scale_y_continuous(sec.axis = sec_axis(~./secAxisScale1, name = paste0("CF", "(%)")))+
    scale_fill_manual(name = "Technology", values = color.mapping2) +
    scale_color_manual(name = "Technology", values = color.mapping2) +
    xlab("iteration") + ylab(paste0("capacity", "(GW)")) +
    ggtitle(paste0("REMIND ", year_toplot))+
    coord_cartesian(xlim = c(0, max(plot.remind.capacity$iter)+1),ylim = c(0, ymax)) +
    theme(legend.title = element_blank()) 
  
  if (length(dieter.files) != 0) {
    p1 <- p0 + 
      geom_line(data = plot.remind.demand, aes(x = iter+1, y = value, color = tech), size = 1.2, alpha = 0.5,linetype="dotted")
  }
  
  plot.dieter.capacity <- out.dieter.capacity %>%
    filter(period == year_toplot)
  
  if (length(dieter.files) != 0) {
    plot.dieter.capfac <- out.dieter.capfac %>%
      filter(period == year_toplot)
  }
  secAxisScale2 = max(plot.dieter.capacity$value) / 100
  
  if (length(dieter.files) != 0) {
    p2<-ggplot() +
      geom_area(data = plot.dieter.capacity, aes(x = iter + 1, y = value, fill = all_te), size = 1.2, alpha = 0.5) +
      geom_line(data = plot.remind.demand, aes(x = iter + 1, y = value, color = tech), size = 1.2, alpha = 1,linetype="dotted") +
      geom_line(data = plot.dieter.capfac, aes(x = iter + 1, y = value*secAxisScale2, color = tech), size = 1.2, alpha = 0.5) +
      scale_y_continuous(sec.axis = sec_axis(~./secAxisScale2, name = paste0("CF", "(%)")))+
      scale_fill_manual(name = "Technology", values = color.mapping2) +
      scale_color_manual(name = "Technology", values = color.mapping2)+
      xlab("iteration") + ylab(paste0("capacity (GW)")) +
      coord_cartesian(xlim = c(0, max(plot.dieter.capacity$iter)),ylim = c(0, ymax))+
      ggtitle(paste0("DIETER ", year_toplot))+
      theme(legend.title = element_blank()) 
  }
  
  grid.newpage()
  if (length(dieter.files) != 0) {
    p <- arrangeGrob(rbind(ggplotGrob(p1), ggplotGrob(p2)))
  } else { p<-p1 }
  
  swfigure(sw,grid.draw,p)
}


swlatex(sw, "\\subsection{Capacities over time (last iteration)}")


plot.remind.capacity <- out.remind.capacity %>% 
  filter(iter == max(out.remind.capacity$iter))

if (length(dieter.files) != 0) {
  plot.remind.demand <- out.remind.demand %>% 
    filter(iter == max(out.remind.demand$iter))
  plot.remind.demand$tech <- "peak demand"
}

plot.remind.capfac <- out.remind.capfac %>% 
  filter(iter == max(out.remind.capfac$iter))

secAxisScale1 = max(plot.remind.capacity$value) / 100
#get max value
df.maxval<- plot.remind.capacity %>% 
  dplyr::group_by(period, rlf, iter, model) %>%
  dplyr::summarise( value = sum(value) , .groups = 'keep' ) %>% 
  dplyr::ungroup(period, rlf, iter, model) 

ymax = max(df.maxval$value) * 1.1


swlatex(sw, paste0("\\subsection{Capacities in ", year_toplot, "}"))

p0<-ggplot() +
  geom_area(data = plot.remind.capacity, aes(x = period, y = value, fill = all_te), size = 1.2, alpha = 0.5) +
  geom_line(data = plot.remind.capfac, aes(x = period, y = value*secAxisScale1, color = tech), size = 1.2, alpha = 0.5) + 
  scale_y_continuous(sec.axis = sec_axis(~./secAxisScale1, name = paste0("CF", "(%)")))+
  scale_fill_manual(name = "Technology", values = color.mapping2) +
  scale_color_manual(name = "Technology", values = color.mapping2) +
  xlab("period") + ylab(paste0("vm_cap", "(GW)")) +
  ggtitle(paste0("REMIND Last iteration"))+
  theme(legend.title = element_blank()) 

if (length(dieter.files) != 0) {
  p1 <- p0 + 
    geom_line(data = plot.remind.demand, aes(x = period, y = value, color = tech), size = 1.2, alpha = 0.5,linetype="dotted")
  
}

plot.dieter.capacity <- out.dieter.capacity %>%
  filter(iter == max(out.dieter.capacity$iter))

if (length(dieter.files) != 0) {
  plot.dieter.capfac <- out.dieter.capfac %>%
    filter(iter == max(out.dieter.capfac$iter))
}
secAxisScale2 = max(plot.dieter.capacity$value) / 100



if (length(dieter.files) != 0) {
  p2<-ggplot() +
    geom_area(data = plot.dieter.capacity, aes(x = as.integer(period), y = value, fill = all_te), size = 1.2, alpha = 0.5) +
    geom_line(data = plot.remind.demand, aes(x = as.integer(period), y = value, color = tech), size = 1.2, alpha = 1,linetype="dotted") +
    geom_line(data = plot.dieter.capfac, aes(x = as.integer(period), y = value*secAxisScale2, color = tech), size = 1.2, alpha = 0.5) +
    scale_y_continuous(sec.axis = sec_axis(~./secAxisScale2, name = paste0("CF", "(%)")))+
    scale_fill_manual(name = "Technology", values = color.mapping2) +
    scale_color_manual(name = "Technology", values = color.mapping2)+
    xlab("period") + ylab(paste0("capacity (GW)")) +
    ggtitle(paste0("DIETER Last iteration "))+
    
    theme( legend.title = element_blank()) 
 
}

grid.newpage()
p <- arrangeGrob(rbind(ggplotGrob(p1), ggplotGrob(p2)))

swfigure(sw,grid.draw,p)

