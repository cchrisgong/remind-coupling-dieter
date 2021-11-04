# Data preparation (REMIND) -----------------------------------------------

cat("Plot capacities \n")
year_toplot_list <- c(2025,2030,2035,2040,2045,2050,2055,2060,2070,2080,2090,2100,2110,2130)


remind.sector.coupling.mapping <- c(elh2 = "Electrolyzers",
                                    NULL)

remind.vre.mapping <- c(hydro = "Hydro",
                        wind = "Wind",
                        spv = "Solar",
                        NULL)

vre.names <- c("Hydro","Wind","Solar")
nonvre.names <- c("Lignite", "Hard coal","Coal (Lig + HC)", "Nuclear","OCGT","CCGT","Biomass","Electrolyzers")
table_ordered_name = c("Coal (Lig + HC)", "Lignite", "Hard coal","CCGT", "Solar", "Wind", "Biomass", "OCGT", "Hydro", "Nuclear","Electrolyzers")

remind.tech.mapping <- c(remind.nonvre.mapping, remind.vre.mapping, remind.sector.coupling.mapping)

#dieter.tech.exclude <- c("OCGT_ineff", "Wind_off")

dieter.tech.mapping <- c(hc = "Hard coal",
                         lig = "Lignite",
                         coal = "Coal (Lig + HC)",
                         nuc = "Nuclear",
                         OCGT_eff = "OCGT",
                         CCGT = "CCGT",
                         bio = "Biomass",
                         ror = "Hydro",
                         Wind_on = "Wind",
                         Solar = "Solar",
                         elh2 = "Electrolyzers",
                         NULL)

# year_toplot = 2050

VARkey1 = "vm_cap"
REGIkey1 = "DEU"
# REGIkey1 = "USA"
grade = "1"

VARkey1_DT = "p32_report4RM"
VARsubkey1_DT = "capacity"
VARsubkey2_DT = "capfac"

VARsubkey1_RM = "p32_peakDemand_relFac"
VARsubkey2_RM = "p32_seelUsableDem"
VARsubkey3_RM = "p32_seh2elh2Dem"

CFkey1 = "vm_capFac"
CFkey2 = "pm_dataren"
CFkey3 = "vm_capDistr"

color.mapping <- c("CCGT" = "#999959", "Lignite" = "#0c0c0c", "Coal (Lig + HC)" = "#0c0c0c",
                   "Solar" = "#ffcc00", "Wind" = "#337fff", "Biomass" = "#005900",
                   "OCGT" = "#e51900", "Hydro" = "#191999", "Nuclear" = "#ff33ff",
                   "Hard coal" = "#808080", "peak demand" = "#0c0c0c", "Electrolyzers" = "#48D1CC")

vrN_RM_CAP <- NULL
vr1_DEM <- NULL
vrN_RM_CF <- NULL
if (length(dieter.files) != 0) {
for (i in 1:(length(remind.files)-1)){
  
  # Read in vm_cap (capacity)
  resfrac <-  file.path(outputdir, remind.files[i]) %>%  
    read.gdx(VARsubkey1_RM, factor = FALSE) %>% 
    filter(ttot %in% year_toplot_list) %>% 
    filter(all_regi == REGIkey1) %>%
    select(period=ttot,resfrac = value) 
  
  h2dem <- file.path(outputdir, remind.files[i]) %>%  
    read.gdx(VARsubkey3_RM, factor = FALSE) %>% 
    filter(ttot %in% year_toplot_list) %>% 
    filter(all_regi == REGIkey1) %>%
    select(period=ttot,h2dem = value) 
  
  vrdata <- file.path(outputdir, remind.files[i]) %>%  
    read.gdx(VARsubkey2_RM, field="l", factor = FALSE) %>% 
    filter(ttot %in% year_toplot_list) %>% 
    filter(all_regi == REGIkey1) %>%
    filter(all_enty == "seel") %>%
    select(period=ttot,value) %>% 
    right_join(resfrac) %>% 
    right_join(h2dem) %>% 
    mutate(value = (value - h2dem) * resfrac * 8760 * 1e3) 
  
  vrdata$iter <- i
  vrdata$model <- "REMIND"
  vr1_DEM <- rbind(vr1_DEM, vrdata)
  
  vrdataCap <- file.path(outputdir, remind.files[i]) %>%  
    read.gdx(VARkey1, factors = FALSE, squeeze = FALSE) %>% 
    filter(tall %in% year_toplot_list) %>%
    filter(all_regi == REGIkey1) %>%
    filter(all_te %in% names(remind.tech.mapping)) %>%
    filter(rlf == grade) %>% 
    mutate(value = value * 1e3) %>% #TW->GW
    select(period = tall, all_te, rlf, value) %>% 
    revalue.levels(all_te = remind.tech.mapping) %>%
    dplyr::group_by(period, all_te, rlf) %>%
    dplyr::summarise( value = sum(value) , .groups = 'keep' ) %>% 
    dplyr::ungroup(period, all_te, rlf) %>% 
    mutate(all_te = factor(all_te, levels=rev(unique(remind.tech.mapping))))
  
  vrdataCap$iter <- i
  vrdataCap$model <- "REMIND"
  
  vrN_RM_CAP <- rbind(vrN_RM_CAP, vrdataCap)
  
  
  # first the dispatchable
  vrdata <- file.path(outputdir, remind.files[i]) %>%  
    read.gdx(CFkey1, field="l", squeeze = FALSE) %>% 
    filter(ttot %in% year_toplot_list) %>%
    filter(all_te %in% names(remind.tech.mapping)) %>% 
    filter(all_regi == REGIkey1) %>%
    revalue.levels(all_te = remind.tech.mapping) %>%
    filter(!(all_te %in% remind.vre.mapping)) %>% 
    select(period = ttot, all_te, value) %>% 
    dplyr::group_by(period, all_te) %>%
    dplyr::summarise(value = mean(value), .groups = "keep") %>% 
    dplyr::ungroup(period, all_te)
  
  # second the VRE
  # pm_dataren("nur") = capacity factor at different grade levels (quality of wind resources), pm_dataren is used to represent different cost levels necessary to create the same amount of energy if you have to build your wind farm in places with lower wind. The grades are ordered from 1 to 10. 1 is closer to the wind always blowing (highest nur capacity factor), 10 is the worst place to install wind.
  dataren <- file.path(outputdir, remind.files[i]) %>%  
    read.gdx(CFkey2) %>% 
    #filter(ttot %in% year_toplot_list) %>%
    filter(char == "nur") %>%
    filter(all_regi == REGIkey1) %>%
    filter(all_te %in% names(remind.vre.mapping)) %>% 
    revalue.levels(all_te = remind.vre.mapping) %>%
    select(all_te, rlf, ren_nur= value)
  
  #vm_capDistr is a "helper" variable that stores the amount of capacity that REMIND assign to each grade. Therefore it can have values from any grade. vm_cap is the total capacity (sum of vm_capDistr). It does not have grade infor anymore, everything is assigned to "1". vm_capDistr will retain the information of the optimal grades used in the REMIND decision
  
  percentage_cap_distr <- file.path(outputdir, remind.files[i]) %>%  
    read.gdx(CFkey3) %>% 
    filter(tall %in% year_toplot_list) %>%
    filter(all_regi == REGIkey1) %>% 
    filter(all_te %in% names(remind.vre.mapping)) %>% 
    revalue.levels(all_te = remind.vre.mapping) %>% 
    select(period=tall, all_te, rlf, cap_distr = value) %>% 
    dplyr::group_by(period, all_te) %>% 
    transmute(rlf, percentage_cap_distr = cap_distr/sum(cap_distr))
  
  vrdata2_0 = list(dataren, percentage_cap_distr) %>%
    reduce(right_join)
  
  vrdata2 <- vrdata2_0 %>% 
    select(period, all_te, ren_nur, percentage_cap_distr) %>% 
    replace(is.na(.), 0) %>%
    dplyr::group_by(period, all_te) %>%
    dplyr::summarise(value = sum(ren_nur * percentage_cap_distr), .groups = "keep" ) %>% 
    dplyr::ungroup(period, all_te)
  
  vrdata_tot <- list(vrdata, vrdata2) %>% 
    reduce(full_join) %>% 
    dplyr::rename(tech = all_te) %>% 
    mutate(value = value * 100)
  
  vrdata_tot$iter <- i
  
  vrN_RM_CF <- rbind(vrN_RM_CF,vrdata_tot)

}
}

# Data preparation (DIETER) -----------------------------------------------
vrN_DT <- NULL
if (length(dieter.files) != 0) {
for (i in 1:length(sorted_files_DT)){
  dieter.data <- file.path(outputdir, sorted_files_DT[i]) %>% 
    read.gdx( VARkey1_DT, factor = FALSE, squeeze = FALSE) %>%
    select(period = X..1, all_te = X..3,variable=X..4,value)  %>%
    filter(period %in% year_toplot_list) %>%
    filter(all_te %in% names(dieter.tech.mapping)) %>%
    filter(variable %in% c(VARsubkey1_DT,VARsubkey2_DT)) %>%
    revalue.levels(all_te = dieter.tech.mapping) %>%
    mutate(all_te = factor(all_te, levels=rev(unique(dieter.tech.mapping)))) 
  
  dieter.data$iter <- id[i]
  dieter.data$model <- "DIETER"
  
  vrN_DT <- rbind(vrN_DT, dieter.data)
}
vrN_DT_CF <- vrN_DT %>%
  filter(variable == "capfac") %>%
  mutate(value = value * 100) %>%
  select(period, tech=all_te, value, iter)

vrN_DT_CAP<- vrN_DT %>%
  filter(variable == "capacity") %>%
  mutate(value = value/1e3) %>% #MW->GW
  select(period, all_te, value, iter)
}
# Plotting ----------------------------------------------------------------

swlatex(sw, paste0("\\section{Capacities}"))

for(year_toplot in year_toplot_list){
  vrN_RM_CAP_plot <- vrN_RM_CAP %>% 
    filter(period == year_toplot)
  
  if (length(dieter.files) != 0) {
    vr1_DEM_plot <- vr1_DEM %>% 
      filter(period == year_toplot)
    vr1_DEM_plot$tech <- "peak demand"
  }
  
  vrN_RM_CF_plot <- vrN_RM_CF %>% 
    filter(period == year_toplot)
  
  secAxisScale1 = max(vrN_RM_CAP_plot$value) / 100
  
  vrN_RM_CAP_plot2<- vrN_RM_CAP_plot %>% 
    dplyr::group_by(period, rlf, iter, model) %>%
    dplyr::summarise( value = sum(value) , .groups = 'keep' ) %>% 
    dplyr::ungroup(period, rlf, iter, model) 
  
  ymax = max(vrN_RM_CAP_plot2$value) * 1.1
  
  swlatex(sw, paste0("\\subsection{Capacities in ", year_toplot, "}"))
  
  p0<-ggplot() +
    geom_area(data = vrN_RM_CAP_plot, aes(x = iter, y = value, fill = all_te), size = 1.2, alpha = 0.5) +
    geom_line(data = vrN_RM_CF_plot, aes(x = iter, y = value*secAxisScale1, color = tech), size = 1.2, alpha = 0.5) + 
    scale_y_continuous(sec.axis = sec_axis(~./secAxisScale1, name = paste0("CF", "(%)")))+
    scale_fill_manual(name = "Technology", values = color.mapping) +
    scale_color_manual(name = "tech", values = color.mapping) +
    theme(axis.text=element_text(size=14), axis.title=element_text(size = 14,face="bold")) + 
    xlab("iteration") + ylab(paste0(VARkey1, "(GW)")) +
    # ggtitle(paste0("REMIND ", year_toplot, " (100$/tCO2)"))+
    ggtitle(paste0("REMIND ", year_toplot))+
    coord_cartesian(xlim = c(0, max(vrN_RM_CAP_plot$iter)+1),ylim = c(0, ymax)) +
    theme(plot.title = element_text(size = 16, face = "bold")) +
    theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text=element_text(size=14)) +
    theme(aspect.ratio = .5)
  
  if (length(dieter.files) != 0) {
    p1 <- p0 + 
      geom_line(data = vr1_DEM_plot, aes(x = iter+1, y = value, color = tech), size = 1.2, alpha = 0.5,linetype="dotted")
  }
  
  vrN_DT_CAP_plot <- vrN_DT_CAP %>%
    filter(period == year_toplot)
  
  if (length(dieter.files) != 0) {
    vrN_DT_CF_plot <- vrN_DT_CF %>%
      filter(period == year_toplot)
  }
  secAxisScale2 = max(vrN_RM_CAP_plot$value) / 100
  
  if (length(dieter.files) != 0) {
    p2<-ggplot() +
      geom_area(data = vrN_DT_CAP_plot, aes(x = iter + 1, y = value, fill = all_te), size = 1.2, alpha = 0.5) +
      geom_line(data = vr1_DEM_plot, aes(x = iter + 1, y = value, color = tech), size = 1.2, alpha = 1,linetype="dotted") +
      geom_line(data = vrN_DT_CF_plot, aes(x = iter + 1, y = value*secAxisScale2, color = tech), size = 1.2, alpha = 0.5) +
      scale_y_continuous(sec.axis = sec_axis(~./secAxisScale2, name = paste0("CF", "(%)")))+
      scale_fill_manual(name = "Technology", values = color.mapping) +
      scale_color_manual(name = "tech", values = color.mapping)+
      theme(axis.text=element_text(size=14), axis.title=element_text(size= 14,face="bold")) +
      xlab("iteration") + ylab(paste0("capacity (GW)")) +
      coord_cartesian(xlim = c(0, max(vrN_DT_CAP_plot$iter)),ylim = c(0, ymax))+
      # ggtitle(paste0("DIETER ", year_toplot, " (100$/tCO2)"))+
      ggtitle(paste0("DIETER ", year_toplot))+
      theme(plot.title = element_text(size = 16, face = "bold"))+
      theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text=element_text(size=14)) +
      theme(aspect.ratio = .5)
  }
  
  grid.newpage()
  if (length(dieter.files) != 0) {
    p <- arrangeGrob(rbind(ggplotGrob(p1), ggplotGrob(p2)))
  } else { p<-p1 }
  
  swfigure(sw,grid.draw,p)
}


swlatex(sw, "\\subsection{Capacities over time (last iteration)}")



vrN_RM_CAP_plot <- vrN_RM_CAP %>% 
  filter(iter == max(vrN_RM_CAP$iter))

if (length(dieter.files) != 0) {
  vr1_DEM_plot <- vr1_DEM %>% 
    filter(iter == max(vr1_DEM$iter))
  vr1_DEM_plot$tech <- "peak demand"
}

vrN_RM_CF_plot <- vrN_RM_CF %>% 
  filter(iter == max(vrN_RM_CF$iter))

secAxisScale1 = max(vrN_RM_CAP_plot$value) / 100

vrN_RM_CAP_plot2<- vrN_RM_CAP_plot %>% 
  dplyr::group_by(period, rlf, iter, model) %>%
  dplyr::summarise( value = sum(value) , .groups = 'keep' ) %>% 
  dplyr::ungroup(period, rlf, iter, model) 

ymax = max(vrN_RM_CAP_plot2$value) * 1.1

swlatex(sw, paste0("\\subsection{Capacities in ", year_toplot, "}"))

p0<-ggplot() +
  geom_area(data = vrN_RM_CAP_plot, aes(x = period, y = value, fill = all_te), size = 1.2, alpha = 0.5) +
  geom_line(data = vrN_RM_CF_plot, aes(x = period, y = value*secAxisScale1, color = tech), size = 1.2, alpha = 0.5) + 
  scale_y_continuous(sec.axis = sec_axis(~./secAxisScale1, name = paste0("CF", "(%)")))+
  scale_fill_manual(name = "Technology", values = color.mapping) +
  scale_color_manual(name = "tech", values = color.mapping) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size = 14,face="bold")) + 
  xlab("period") + ylab(paste0(VARkey1, "(GW)")) +
  # ggtitle(paste0("REMIND ", year_toplot, " (100$/tCO2)"))+
  ggtitle(paste0("REMIND Last iteration"))+
  #coord_cartesian(xlim = c(0, max(vrN_RM_CAP_plot$iter)+1),ylim = c(0, ymax)) +
  theme(plot.title = element_text(size = 16, face = "bold")) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text=element_text(size=14)) +
  theme(aspect.ratio = .5)

if (length(dieter.files) != 0) {
  p1 <- p0 + 
    geom_line(data = vr1_DEM_plot, aes(x = period, y = value, color = tech), size = 1.2, alpha = 0.5,linetype="dotted")
}

vrN_DT_CAP_plot <- vrN_DT_CAP %>%
  filter(iter == max(vrN_DT_CAP$iter))

if (length(dieter.files) != 0) {
  vrN_DT_CF_plot <- vrN_DT_CF %>%
    filter(iter == max(vrN_DT_CF$iter))
}
secAxisScale2 = max(vrN_RM_CAP_plot$value) / 100

if (length(dieter.files) != 0) {
  p2<-ggplot() +
    geom_area(data = vrN_DT_CAP_plot, aes(x = as.integer(period), y = value, fill = all_te), size = 1.2, alpha = 0.5) +
    geom_line(data = vr1_DEM_plot, aes(x = as.integer(period), y = value, color = tech), size = 1.2, alpha = 1,linetype="dotted") +
    geom_line(data = vrN_DT_CF_plot, aes(x = as.integer(period), y = value*secAxisScale2, color = tech), size = 1.2, alpha = 0.5) +
    scale_y_continuous(sec.axis = sec_axis(~./secAxisScale2, name = paste0("CF", "(%)")))+
    scale_fill_manual(name = "Technology", values = color.mapping) +
    scale_color_manual(name = "tech", values = color.mapping)+
    theme(axis.text=element_text(size=14), axis.title=element_text(size= 14,face="bold")) +
    xlab("period") + ylab(paste0("capacity (GW)")) +
  # coord_cartesian(xlim = c(0, max(vrN_DT_CAP_plot$iter)),ylim = c(0, ymax))+
    # ggtitle(paste0("DIETER ", year_toplot, " (100$/tCO2)"))+
    ggtitle(paste0("DIETER Last iteration "))+
    theme(plot.title = element_text(size = 16, face = "bold"))+
    theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text=element_text(size=14)) +
    theme(aspect.ratio = .5)
}

grid.newpage()
p <- arrangeGrob(rbind(ggplotGrob(p1), ggplotGrob(p2)))

swfigure(sw,grid.draw,p)

