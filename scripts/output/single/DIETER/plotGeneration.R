# Data preparation (REMIND) -----------------------------------------------

cat("Plot generation \n")

VARkey1 = "vm_prodSe"
VARkey2 = "vm_usableSeTe"

VARkey4 = "p32_seelUsableDem" # normal electricity demand
VARkey5 = "vm_demSe" 


REGIkey1 = "DEU"
ProdKEY1 = "seel"
ProdKEY2 = "seh2"
TECHkeylst_peakGas = c("ngt")
TECHkeylst_nonPeakGas = c("ngccc","ngcc",  "gaschp") 
TECHkeylst_coal = c("coalchp", "igccc", "igcc", "pcc", "pco","pc")
TECHkeylst_solar = c("spv")
TECHkeylst_wind = c("wind")
TECHkeylst_hydro = c("hydro")
TECHkeylst_nuclear = c("tnrs")
TECHkeylst_biomass = c("biochp", "bioigccc", "bioigcc")

TECHkeylst <- c(TECHkeylst_peakGas, TECHkeylst_nonPeakGas, TECHkeylst_coal, TECHkeylst_solar, TECHkeylst_wind, TECHkeylst_hydro, TECHkeylst_biomass, TECHkeylst_nuclear)

out.remind <- NULL
RM_GEN_wCurt <- NULL
RM_CONSM <- NULL
for (i in 1:length(remind.files)){
  
  
  vrdata_disp<-   file.path(outputdir, remind.files[i]) %>%  
    read.gdx(VARkey1, factors = FALSE, squeeze = FALSE) %>% 
    filter(tall %in% report.periods) %>%
    filter(all_regi == REGIkey1) %>%
    filter(all_te %in% names(remind.nonvre.mapping2)) %>% 
    filter(all_enty.1 == ProdKEY1) %>% 
    select(period = tall, all_te, value) %>% 
    revalue.levels(all_te = remind.nonvre.mapping2) %>%
    mutate(value = value*sm_TWa_2_MWh/1e6) %>% 
    dplyr::group_by(period, all_te) %>%
    dplyr::summarise( value = sum(value) , .groups = 'keep' ) %>% 
    dplyr::ungroup(period, all_te) %>% 
    mutate(iteration = i)
  
  vrdata_vre<- file.path(outputdir, remind.files[i]) %>%  
    read.gdx(VARkey2, factors = FALSE, squeeze = FALSE) %>% 
    filter(ttot %in% report.periods) %>%
    filter(all_regi == REGIkey1) %>%
    filter(all_te %in% names(remind.vre.mapping)) %>% 
    filter(entySe == ProdKEY1)  %>% 
    select(period = ttot, all_te, value) %>% 
    revalue.levels(all_te = remind.vre.mapping) %>% 
    mutate(value = value*sm_TWa_2_MWh/1e6) %>% 
    dplyr::group_by(period, all_te) %>%
    dplyr::summarise( value = sum(value) , .groups = 'keep' ) %>% 
    dplyr::ungroup(period, all_te)%>% 
    mutate(iteration = i)
  
  remind.vm_prodSe  <- list(vrdata_disp, vrdata_vre) %>% 
    reduce(full_join) %>% 
    mutate(all_te = factor(all_te, levels=rev(unique(remind.tech.mapping)))) 
  
  out.remind <- rbind(out.remind, remind.vm_prodSe )
  
  vrdata<- file.path(outputdir, remind.files[i]) %>%
    read.gdx(VARkey1, factors = FALSE, squeeze = FALSE) %>% 
    filter(tall %in% report.periods) %>%
    filter(all_regi == REGIkey1) %>%
    filter(all_te %in% names(remind.tech.mapping)) %>% 
    filter(all_enty.1 == ProdKEY1)  %>% 
    select(period = tall, value,all_te) %>% 
    revalue.levels(all_te = remind.tech.mapping) %>%
    dplyr::group_by(period, all_te) %>%
    dplyr::summarise( value = sum(value) , .groups = 'keep' ) %>% 
    dplyr::ungroup(period, all_te) %>% 
    mutate(value = value * sm_TWa_2_MWh/1e6) %>% 
    mutate(all_te = factor(all_te, levels=rev(unique(remind.tech.mapping)))) %>% 
    mutate(iteration = i)
  
  RM_GEN_wCurt <- rbind(RM_GEN_wCurt,vrdata)
  
  h2consum <- file.path(outputdir, remind.files[i]) %>%
    read.gdx(VARkey5, factors = FALSE, field = "l", squeeze = FALSE) %>% 
    filter(ttot %in% report.periods) %>%
    filter(all_regi == REGIkey1) %>%
    filter(all_te == "elh2") %>% 
    mutate(value = value * sm_TWa_2_MWh / 1e6) %>% 
    select(period = ttot,value) %>% 
    mutate(iteration = i)
  
  h2consum2 <- h2consum %>% 
    mutate(all_te = "elh2") %>% 
    mutate(iteration = i)
  
  totelconsum <- file.path(outputdir, remind.files[i]) %>%
    read.gdx(VARkey4, factors = FALSE, squeeze = FALSE) %>% 
    filter(ttot %in% report.periods) %>%
    filter(all_regi == REGIkey1) %>%
    mutate(value = value * sm_TWa_2_MWh / 1e6) %>% 
    select(period = ttot,totelcon=value)%>% 
    mutate(iteration = i)
  
  vrdata <-list(h2consum, totelconsum) %>%
    reduce(full_join) %>% 
    mutate(value = totelcon - value)  %>% 
    mutate(all_te = "seel") %>% 
    select(period,all_te,value) %>% 
    full_join(h2consum2) %>% 
    revalue.levels(all_te = dieter.demand.tech.mapping)%>% 
    mutate(iteration = i)
  
  RM_CONSM <- rbind(RM_CONSM,vrdata)
  
}
RM_GEN_wCurt$tech <- "total generation w/ curtailment"

RM_CONSM <- RM_CONSM %>%
  mutate(all_te = fct_relevel(all_te,table_ordered_name_dem))

# Data preparation (DIETER) -----------------------------------------------
VARkey1_DT = "p32_report4RM"
VARsubkey1_DT = c("total generation", "usable generation", "total consumption")
# VARsubkey1_DT = c("total generation", "usable generation")
TECHkeylst_DT = c("CCGT", "lig", "Solar", "Wind_on", "bio", "OCGT_eff", "ror", "nuc", "hc","elh2","seel")


out.dieter <- NULL
for (i in 1:length(sorted_files_DT)){
  dieter.data <- file.path(outputdir, sorted_files_DT[i]) %>% 
    read.gdx(VARkey1_DT, factors = FALSE, squeeze = FALSE) %>% 
    select(period = X..1, regi=X..2, all_te=X..3, variable = X..4, value) %>% 
    # filter(period %in% year_toplot_list) %>%
    filter(all_te %in% TECHkeylst_DT) %>% 
    filter(variable %in% VARsubkey1_DT) %>% 
    mutate(value = value/1e6) %>% 
    filter(all_te %in% names(dieter.tech.mapping)) %>% 
    revalue.levels(all_te = dieter.tech.mapping) %>%
    mutate(all_te = factor(all_te, levels=rev(unique(dieter.tech.mapping))))  %>%
    mutate(iteration = id[i])
  
  out.dieter <- rbind(out.dieter, dieter.data)
}
vr1_DT_GEN_wCurt <- out.dieter %>%
  filter(variable == "total generation") %>%
  select(period,iteration,all_te,value) 

vr1_DT_CONSUMP <- out.dieter %>%
  filter(variable == "total consumption") %>%
  select(period,iteration,all_te,value) 


vr1_DT_GEN_wCurt_tot <- out.dieter %>%
  filter(variable == "total generation") %>%
  select(period, iteration, all_te,value) %>%
  dplyr::group_by(period, iteration) %>%
  dplyr::summarise( value = sum(value) , .groups = 'keep' ) %>%
  dplyr::ungroup(period, iteration)

vr1_DT_GEN_wCurt$tech <- "total generation w/ curtailment"

# Plotting ----------------------------------------------------------------

swlatex(sw, paste0("\\section{Generation}"))

for(year_toplot in report.periods){
  
  vr1_GEN_wCurt_yr = RM_GEN_wCurt %>% filter(period == year_toplot)
  ymax = max(RM_CONSM$value)*2
  ymin = -ymax
  
  if (length(sorted_files_DT) != 0) {
    vr1_DT_GEN_wCurt_yr = vr1_DT_GEN_wCurt %>% filter(period == year_toplot)
    vr1_DT_GEN_wCurt_tot_yr = vr1_DT_GEN_wCurt_tot%>% filter(period == year_toplot) 
    vr1_DT_CONSUMP_yr = vr1_DT_CONSUMP%>% filter(period == year_toplot) 
    
    ymax = max(vr1_DT_GEN_wCurt_tot_yr$value)*1.3
    
    
    if (max(vr1_DT_CONSUMP$value) == 0) {
      ymin = 0
    } else {ymin = -max(vr1_DT_CONSUMP_yr$value)*1.7}
  }
  
  
  swlatex(sw, paste0("\\subsection{Generation in ", year_toplot, "}"))
  
  p1<-ggplot() +
    geom_area(data = out.remind  %>% filter(period == year_toplot), aes(x = iteration, y = value, fill = all_te), size = 1.2, alpha = 0.5, stat = "identity") +
    geom_area(data = vr1_GEN_wCurt_yr, aes(x = iteration, y = value, color = all_te), size = 1.2, alpha = 0,linetype="dotted") +
    geom_area(data = RM_CONSM %>% filter(period == year_toplot), aes(x = iteration, y = -value, fill = all_te), size = 1.2, alpha = 0.5, stat = "identity") +
    scale_fill_manual(name = "Technology", values = color.mapping1)+
    scale_color_manual(name = "Technology", values = color.mapping1)+
    theme(axis.text=element_text(size=10), axis.title=element_text(size= 10,face="bold")) +
    xlab("iteration") + ylab(paste0("Usable generation (TWh)")) +
    ggtitle(paste0("REMIND", year_toplot))+
    coord_cartesian(ylim = c(ymin,ymax), xlim = c(0, max(out.remind$iteration)))+
    theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank()) +
    theme(aspect.ratio = .5)
  
  if (length(sorted_files_DT) != 0) {
    p2<-ggplot() +
      geom_area(data = out.dieter %>% filter(period == year_toplot, variable == "usable generation"), aes(x = iteration, y = value, fill = all_te), size = 1.2, alpha = 0.5) +
      geom_area(data = vr1_DT_GEN_wCurt_yr, aes(x = iteration, y = value, color = all_te), size = 1.2, alpha = 0,linetype="dotted") +
      geom_area(data = vr1_DT_CONSUMP %>% filter(period == year_toplot), aes(x = iteration, y = -value, fill = all_te), size = 1.2, alpha = 0.5, stat = "identity") +
      scale_fill_manual(name = "Technology", values = color.mapping2)+
      scale_color_manual(name = "Technology", values = color.mapping2)+
      theme(axis.text=element_text(size=10), axis.title=element_text(size= 10,face="bold")) +
      xlab("iteration") + ylab(paste0(VARsubkey1_DT, "(TWh)")) +
      coord_cartesian(ylim = c(ymin,ymax), xlim = c(0, max(out.remind$iteration)))+
      ggtitle(paste0("DIETER", year_toplot))+
      theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank()) +
      theme(aspect.ratio = .5)
  }
  
  
  grid.newpage()
  if (length(sorted_files_DT) != 0) {
    p <- arrangeGrob(rbind(ggplotGrob(p1), ggplotGrob(p2)))
  } else {p <- p1}
  
 
  
  swfigure(sw,grid.draw,p)
}


swlatex(sw, "\\subsection{Generation over time (last iteration)}")



p1<-ggplot() +
  geom_area(data = out.remind  %>% filter(iteration == max(out.remind$iteration)), aes(x = period, y = value, fill = all_te), size = 1.2, alpha = 0.5, stat = "identity") +
  geom_area(data = RM_GEN_wCurt %>% filter(iteration == max(RM_GEN_wCurt$iteration)), aes(x = period, y = value, color = all_te), size = 1.2, alpha = 0,linetype="dotted") +
  geom_area(data = RM_CONSM %>% filter(iteration == max(RM_CONSM$iteration)), aes(x = period, y = -value, fill = all_te), size = 1.2, alpha = 0.5, stat = "identity") +
  scale_fill_manual(name = "Technology", values = color.mapping1)+
  scale_color_manual(name = "Technology", values = color.mapping1)+
  theme(axis.text=element_text(size=10), axis.title=element_text(size= 10,face="bold")) +
  xlab("Year") + ylab(paste0("Usable generation (TWh)")) +
  ggtitle(paste0("REMIND (last iteration)"))+
 # coord_cartesian(ylim = c(ymin,ymax), xlim = c(0, max(out.remind$period)))+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank()) +
  theme(aspect.ratio = .5)


if (length(sorted_files_DT) != 0) {
  p2<-ggplot() +
    geom_area(data = out.dieter %>% filter(iteration == max(out.dieter$iteration), variable == "usable generation"), aes(x =as.integer(period), y = value, fill = all_te), size = 1.2, alpha = 0.5) +
    geom_area(data = vr1_DT_GEN_wCurt %>% filter(iteration == max(vr1_DT_GEN_wCurt$iteration)), aes(x = as.integer(period), y = value, color = all_te), size = 1.2, alpha = 0,linetype="dotted") +
    geom_area(data = vr1_DT_CONSUMP %>% filter(iteration == max(vr1_DT_CONSUMP$iteration)), aes(x = as.integer(period), y = -value, fill = all_te), size = 1.2, alpha = 0.5, stat = "identity") +
    scale_fill_manual(name = "Technology", values = color.mapping2)+
    scale_color_manual(name = "Technology", values = color.mapping2)+
    theme(axis.text=element_text(size=10), axis.title=element_text(size= 10,face="bold")) +
    xlab("Year") + ylab(paste0(VARsubkey1_DT, "(TWh)")) +
    #coord_cartesian( xlim = c(min(out.dieter$period), max(out.dieter$period)))+
    ggtitle(paste0("DIETER(last iteration)"))+
    theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank()) +
    theme(aspect.ratio = .5)
  
}



grid.newpage()
if (length(sorted_files_DT) != 0) {
  p <- arrangeGrob(rbind(ggplotGrob(p1), ggplotGrob(p2)))
} else {p <- p1}

swfigure(sw,grid.draw,p)

