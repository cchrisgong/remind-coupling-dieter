# plot residual load duration curve for DIETER 

#####################################################
#plot load duration curve of residual loads once production is being accounted for for various tech., one by one, ordered by their capacity factor

if (h2switch == "off"){
  year_toplot_list <- model.periods.RLDC
}

if (h2switch == "on"){
  # year_toplot_list <- model.periods.till2070
  year_toplot_list <- model.periods.till2045
}

# 
# for(year_toplot in year_toplot_list){

  year_toplot = 2045
  hr_data <- file.path(outputdir, dieter.files.report[length(dieter.files.report)]) %>%
    read.gdx("report_tech_hours", factors = FALSE, squeeze = FALSE) %>% 
    # select(model = X., period = X..1, variable = X..3, tech = X..4, hour = X..5, value) %>%
    select(filename = X., period = X..2, variable = X..4, tech = X..5, hour = X..6, value) %>%
    revalue.levels(tech = dieter.tech.mapping.hrly) %>% 
    mutate(hour = as.numeric(str_extract(hour, "[0-9]+"))) %>% 
    mutate(tech = factor(tech, levels = rev(unique(dieter.tech.mapping.hrly)))) %>%
    filter(variable%in% dieter.RLDC.variables) %>% 
    filter(period == year_toplot) %>%
    select(period, tech, variable, value,hour) %>% 
    mutate(period = as.numeric(period)) 
  
  LDC <- hr_data %>% 
    filter(variable == "consumption (GWh)") %>% 
    filter(tech == "Electricity demand") %>%
    select(hour, value) %>%
    dplyr::rename(load = value)%>% 
    complete(hour = 1:8760, fill = list(load = 0)) 
  
  LDC0 <- LDC %>% arrange(desc(load)) %>% 
    mutate(hour = as.numeric(hour)) %>% 
    select(load, hour)  %>%
    mutate(hour.sorted = seq(1, 8760)) %>% 
    mutate(te = "Wind")
  
  Wind <- hr_data %>% 
    filter(variable == "generation (GWh)") %>% 
    filter(tech %in% c("Wind_Onshore","Wind_Offshore"))%>% 
    select(hour, value, tech) %>% 
    dplyr::group_by(hour) %>%
    dplyr::summarise(value = sum(value), .groups = "keep") %>%
    dplyr::ungroup(hour) %>% 
    select(hour, value) %>% 
    dplyr::rename(windgen = value) %>% 
    complete(hour = 1:8760, fill = list(windgen = 0)) 
  
  CU_VRE_Wind <- hr_data %>% 
    filter(variable == "curtailment renewable (GWh)") %>% 
    filter(tech %in% c("Wind_Onshore","Wind_Offshore"))%>% 
    select(hour, value, tech) %>% 
    dplyr::group_by(hour) %>%
    dplyr::summarise(value = sum(value), .groups = "keep") %>%
    dplyr::ungroup(hour) %>% 
    select(hour, value) %>% 
    dplyr::rename(curt_w = value) %>% 
    complete(hour = 1:8760, fill = list(curt_w = 0)) 
  
  PV <- hr_data %>% 
    filter(variable == "generation (GWh)") %>% 
    filter(tech == "Solar") %>% 
    select(hour, value) %>%  
    mutate(value = value)%>% 
    dplyr::rename(solgen = value) %>% 
    complete(hour = 1:8760, fill = list(solgen = 0)) 
  
  CU_VRE_Solar <- hr_data %>% 
    filter(variable == "curtailment renewable (GWh)") %>% 
    filter(tech == "Solar")%>% 
    select(hour, value) %>% 
    dplyr::rename(curt_s = value) %>% 
    complete(hour = 1:8760, fill = list(curt_s = 0)) 
  
  #=================================================================================================
  #=================================================================================================
  
  RLDC_all = list(LDC,Wind,CU_VRE_Wind) %>% 
    reduce(full_join) %>% 
    replace(is.na(.), 0) %>% 
    mutate(Wind.RLDC = load - windgen - curt_w)
  
  RLDC.Wind <- RLDC_all %>% arrange(desc(Wind.RLDC)) %>% 
    select(Wind.RLDC) %>%
    mutate(hour.sorted = seq(1, 8760)) %>% 
    mutate(te = "Solar")
  
  RLDC_all <- list(RLDC_all, PV, CU_VRE_Solar) %>% 
    reduce(full_join) %>% 
    replace(is.na(.), 0) %>% 
    mutate(Solar.RLDC = Wind.RLDC - solgen - curt_s)
  
  RLDC.Solar <- RLDC_all %>% arrange(desc(Solar.RLDC)) %>% 
    select(Solar.RLDC)%>%
    mutate(hour.sorted = seq(1, 8760)) %>% 
    mutate(te = "Lithium-ion battery")
  
  #=================================================================================================
  Battery_Out_Lith <- hr_data %>% 
    filter(variable == "storage generation (GWh)") %>% 
    filter(tech %in% c("Lithium-ion battery")) %>% 
    select(hour, value, tech) %>% 
    dplyr::group_by(hour) %>%
    dplyr::summarise(value = sum(value), .groups = "keep") %>%
    dplyr::ungroup(hour) %>% 
    select(hour, value) %>% 
    dplyr::rename(battgen.li = value) %>% 
    complete(hour = 1:8760, fill = list(battgen.li = 0)) 
  
  Battery_In_Lith <- hr_data %>% 
    filter(variable == "storage loading (GWh)") %>% 
    filter(tech %in% c("Lithium-ion battery")) %>% 
    select(hour, value, tech) %>% 
    dplyr::group_by(hour) %>%
    dplyr::summarise(value = sum(value), .groups = "keep") %>%
    dplyr::ungroup(hour) %>% 
    select(hour, value) %>% 
    dplyr::rename(battcharge.li = value)%>% 
    complete(hour = 1:8760, fill = list(battcharge.li = 0)) 

  RLDC_all <- list(RLDC_all, Battery_Out_Lith, Battery_In_Lith) %>% 
    reduce(full_join) %>% 
    replace(is.na(.), 0) %>% 
    mutate(Batt.Li.RLDC = Solar.RLDC - battgen.li + battcharge.li) 
  
  RLDC.Batt_Li <- RLDC_all %>% arrange(desc(Batt.Li.RLDC)) %>%
    select(Batt.Li.RLDC) %>% 
    mutate(hour.sorted = seq(1, 8760)) %>% 
    mutate(te = "Hydrogen turbine")
  
  #=================================================================================================
  Battery_Out_H2 <- hr_data %>% 
    filter(variable == "storage generation (GWh)") %>% 
    filter(tech %in% c("Electrolyzers for long-term storage")) %>% 
    select(hour, value, tech) %>% 
    dplyr::group_by(hour) %>%
    dplyr::summarise(value = sum(value), .groups = "keep") %>%
    dplyr::ungroup(hour) %>% 
    select(hour, value) %>% 
    dplyr::rename(battgen.h2 = value) %>% 
    complete(hour = 1:8760, fill = list(battgen.h2 = 0)) 
  
  Battery_In_H2 <- hr_data %>% 
    filter(variable == "storage loading (GWh)") %>% 
    filter(tech %in% c("Electrolyzers for long-term storage")) %>% 
    select(hour, value, tech) %>% 
    dplyr::group_by(hour) %>%
    dplyr::summarise(value = sum(value), .groups = "keep") %>%
    dplyr::ungroup(hour) %>% 
    select(hour, value) %>% 
    dplyr::rename(battcharge.h2 = value) %>% 
    complete(hour = 1:8760, fill = list(battcharge.h2 = 0)) 
  
  RLDC_checkpeak = list(LDC, PV, Wind, Battery_Out_Lith, Battery_Out_H2) %>%
    reduce(full_join) %>%
    mutate(residueLoad = load - solgen - windgen - battgen.li - battgen.h2)
  
  RLDC_all <- list(RLDC_all, Battery_Out_H2) %>% 
    reduce(full_join) %>% 
    replace(is.na(.), 0) %>% 
    mutate(Batt.H2.RLDC = Batt.Li.RLDC - battgen.h2) 
  
  RLDC.Batt_H2 <- RLDC_all %>% arrange(desc(Batt.H2.RLDC)) %>%
    select(Batt.H2.RLDC) %>% 
    mutate(hour.sorted = seq(1, 8760)) %>% 
    mutate(te = "Electrolyzers for long-term storage")
  
  RLDC_all <- list(RLDC_all, Battery_In_H2) %>% 
    reduce(full_join) %>% 
    replace(is.na(.), 0) %>% 
    mutate(Batt.H2.RLDC2 = Batt.H2.RLDC + battcharge.h2) 
  
  RLDC.Batt2_H2 <- RLDC_all %>% arrange(desc(Batt.H2.RLDC2)) %>%
    select(Batt.H2.RLDC2) %>% 
    mutate(hour.sorted = seq(1, 8760)) %>% 
    mutate(te = "Electrolyzers for PtG")
  
  #=================================================================================================
  #--------------------------------------------------------------
  running_cost <- file.path(outputdir, dieter.files.report[length(dieter.files.report)]) %>%
    read.gdx("report_tech", factors = FALSE, squeeze = FALSE) %>% 
    # select(model = X., period = X..1, variable = X..3, tech = X..4,  value) %>% 
    select(filename = X., model = X..1, period = X..2, variable = X..4, tech = X..5, value) %>%
    filter(model == "DIETER") %>% 
    filter(tech %in% dieter.dispatch.tech) %>% 
    revalue.levels(tech = dieter.tech.mapping.hrly) %>% 
    mutate(tech = factor(tech, levels = rev(unique(dieter.tech.mapping.hrly)))) %>%
    filter(period == year_toplot) %>% 
    filter(variable%in% dieter.runningcost.variables) %>% 
    select(tech, variable, value) %>% 
    dplyr::group_by(tech) %>%
    dplyr::summarise(value = sum(value), .groups = "keep") %>%
    dplyr::ungroup(tech) %>% 
    arrange(desc(value))
  
  low_running_tech_list = as.vector(running_cost$tech)
  
  #=================================================================================================
  #=================================================================================================
  # order dispatchables (excluding hydro) based on lowest running cost 
  
  capfac <- file.path(outputdir, dieter.files.report[length(dieter.files.report)]) %>%
    read.gdx("report_tech", factors = FALSE, squeeze = FALSE) %>% 
    select(filename = X., model = X..1, period = X..2, variable = X..4, tech = X..5, value) %>%
    filter(model == "REMIND") %>% 
    filter(tech %in% dieter.dispatch.tech.whyd) %>% 
    revalue.levels(tech = dieter.tech.mapping.hrly) %>% 
    mutate(tech = factor(tech, levels = rev(unique(dieter.tech.mapping.hrly)))) %>%
    filter(period == year_toplot) %>% 
    filter(variable == "REMIND real CapFac (%)") %>% 
    select(tech, variable, value) %>% 
    arrange(value) %>% 
    filter(value > 1e-3)
  
  highcf_tech_list = as.vector(capfac$tech)
  
  techranking = highcf_tech_list
  
  LDC_h2 <- hr_data %>% 
    filter(variable == "consumption (GWh)") %>% 
    filter(tech == "Electrolyzers for PtG") %>% 
    select(hour, value) %>% 
    dplyr::rename(h2 = value) %>% 
    complete(hour = 1:8760, fill = list(h2 = 0)) 
  
  RLDC_all <- list(RLDC_all, LDC_h2) %>% 
    reduce(full_join) %>% 
    replace(is.na(.), 0) %>% 
    mutate(H2.RLDC = Batt.H2.RLDC - h2) 
  
  RLDC.H2 <- RLDC_all %>% arrange(desc(H2.RLDC)) %>%
    select(H2.RLDC) %>% 
    mutate(hour.sorted = seq(1, 8760)) %>% 
    mutate(te = techranking[[1]]) %>%
    mutate_all(function(H2.RLDC) ifelse(H2.RLDC < 0, 0, H2.RLDC))
  
  #=================================================================================================
  #=================================================================================================
  # order dispatchables based on capacity factor
  
  
  # first dispatchable tech hourly generation
  hr_disp_gen1 <- hr_data %>% 
    filter(variable == "generation (GWh)") %>% 
    filter(tech == techranking[[1]]) %>% 
    select(hour, gen1 = value)%>% 
    complete(hour = 1:8760, fill = list(gen1 = 0)) 
  
  RLDC_all = list(RLDC_all, hr_disp_gen1) %>% 
    reduce(full_join) %>% 
    replace(is.na(.), 0)%>%
    mutate(RLDC1 = H2.RLDC - gen1)
  
  RLDC1 <- RLDC_all %>% arrange(desc(RLDC1)) %>%
    select(RLDC1) %>% 
    mutate(hour.sorted = seq(1, 8760)) %>% 
    mutate(te = techranking[[2]]) %>% 
    mutate_all(function(RLDC1) ifelse(RLDC1 <0, 0, RLDC1)) 
    
  #=================================================================================================
  if (length(techranking) > 2){
  hr_disp_gen2<- hr_data %>% 
    filter(variable == "generation (GWh)") %>% 
    filter(tech == techranking[[2]])%>% 
    select(hour, value) %>% 
    dplyr::rename(gen2 = value)%>% 
    complete(hour = 1:8760, fill = list(gen2 = 0)) 
  
  RLDC_all = list(RLDC_all, hr_disp_gen2) %>% 
    reduce(full_join) %>% 
    replace(is.na(.), 0)%>%
    mutate(RLDC2 = RLDC1 - gen2)
  
  RLDC2 <- RLDC_all %>% arrange(desc(RLDC2)) %>%
    select(RLDC2) %>% 
    mutate(hour.sorted = seq(1, 8760)) %>% 
    mutate(te = techranking[[3]]) %>% 
    mutate_all(function(RLDC2) ifelse(RLDC2 <0, 0, RLDC2))
  }
  #=================================================================================================
  if (length(techranking) > 3){
  hr_disp_gen3<- hr_data %>% 
    filter(variable == "generation (GWh)") %>% 
    filter(tech == techranking[[3]])%>% 
    select(hour, value) %>% 
    dplyr::rename(gen3 = value)%>% 
    complete(hour = 1:8760, fill = list(gen3 = 0)) 
  
  RLDC_all = list(RLDC_all, hr_disp_gen3) %>% 
    reduce(full_join) %>% 
    replace(is.na(.), 0)%>%
    mutate(RLDC3 = RLDC2 - gen3)
  
  RLDC3 <- RLDC_all %>% arrange(desc(RLDC3)) %>%
    select(RLDC3) %>% 
    mutate(hour.sorted = seq(1, 8760)) %>% 
    mutate(te = techranking[[4]])%>% 
    mutate_all(function(RLDC3) ifelse(RLDC3 <0, 0, RLDC3))
  }
  #=================================================================================================
  if (length(techranking) > 4){
  hr_disp_gen4<- hr_data %>% 
    filter(variable == "generation (GWh)") %>% 
    filter(tech == techranking[[4]])%>% 
    select(hour, value) %>% 
    dplyr::rename(gen4 = value)%>% 
    complete(hour = 1:8760, fill = list(gen4 = 0)) 
  
  RLDC_all = list(RLDC_all, hr_disp_gen4) %>% 
    reduce(full_join) %>% 
    replace(is.na(.), 0)%>%
    mutate(RLDC4 = RLDC3 - gen4)
  
  RLDC4 <- RLDC_all %>% arrange(desc(RLDC4)) %>%
    select(RLDC4) %>% 
    mutate(hour.sorted = seq(1, 8760)) %>% 
    mutate(te = techranking[[5]])%>% 
    mutate_all(function(RLDC4) ifelse(RLDC4 <0, 0, RLDC4))
  }
  
  #=================================================================================================
  if (length(techranking) > 5){
  hr_disp_gen5 <- hr_data %>% 
    filter(variable == "generation (GWh)") %>% 
    filter(tech == techranking[[5]])%>% 
    select(hour, value) %>% 
    dplyr::rename(gen5 = value)%>% 
    complete(hour = 1:8760, fill = list(gen5 = 0)) 
  
  RLDC_all = list(RLDC_all, hr_disp_gen5) %>% 
    reduce(full_join) %>% 
    replace(is.na(.), 0)%>%
    mutate(RLDC5 = RLDC4 - gen5)
  
  RLDC5 <- RLDC_all %>% arrange(desc(RLDC5)) %>%
    select(RLDC5) %>% 
    mutate(hour.sorted = seq(1, 8760)) %>% 
    mutate(te = techranking[[6]])%>% 
    mutate_all(function(RLDC5) ifelse(RLDC5 < 0, 0, RLDC5))
  }
  
  ### plot negative curtailment
  CU_VRE_Solar.plot <- list(CU_VRE_Solar, CU_VRE_Wind) %>% 
    reduce(full_join) %>% 
    mutate(Solar.RLDC2 = -(curt_w + curt_s)) %>% 
    select(-curt_s, -curt_w) %>% 
    arrange(desc(Solar.RLDC2)) %>% 
    mutate(hour= seq(1, 8760)) %>% 
    mutate(te = "Solar" )
  
  CU_VRE_Wind.plot = list(LDC, Wind, CU_VRE_Wind,Battery_In_Lith,Battery_In_H2,LDC_h2) %>% 
    reduce(full_join) %>% 
    replace(is.na(.), 0) %>% 
    mutate(Wind.RLDC2 = load - windgen - curt_w + battcharge.li + battcharge.h2 + h2) %>%
    # mutate(Wind.RLDC2 = - curt_w + battcharge + h2) %>%
    arrange(desc(Wind.RLDC2)) %>% 
    mutate(hour= seq(1, 8760)) %>% 
    select(Wind.RLDC2,hour) %>% 
    filter(Wind.RLDC2 <0) %>% 
    complete(hour = 1:8760, fill = list(Wind.RLDC2 = 0)) %>% 
    mutate(te = "Wind")
 
  # if flipping order of solar and wind curtailment: (deprecated)
  # CU_VRE_Wind.plot <- list(CU_VRE_Solar, CU_VRE_Wind) %>% 
  #   reduce(full_join) %>% 
  #   mutate(Wind.RLDC2 = -(curt_w + curt_s)) %>% 
  #   select(-curt_s, -curt_w) %>% 
  #   arrange(desc(Wind.RLDC2)) %>% 
  #   mutate(hour= seq(1, 8760)) %>% 
  #   mutate(te = "Wind" )
  # 
  # CU_VRE_Solarplot = list(LDC, PV, CU_VRE_Solar,Battery_In_Lith,Battery_In_H2,LDC_h2) %>% 
  #   reduce(full_join) %>% 
  #   replace(is.na(.), 0) %>% 
  #   mutate(Solar.RLDC2 = load - solgen - curt_s + battcharge.li + battcharge.h2 + h2) %>%
  #   # mutate(Wind.RLDC2 = - curt_w + battcharge + h2) %>%
  #   arrange(desc(Solar.RLDC2)) %>% 
  #   mutate(hour= seq(1, 8760)) %>% 
  #   select(Solar.RLDC2,hour) %>% 
  #   filter(Solar.RLDC2 <0) %>% 
  #   complete(hour = 1:8760, fill = list(Solar.RLDC2 = 0)) %>% 
  #   mutate(te = "Solar")
  
  # =================================================================================================
  
  p.DT.rldc<-ggplot() +
    geom_area(data = LDC0 %>% filter(hour.sorted %in% c(seq(1,8760,20),8760)), aes(x = hour.sorted, y = load, fill = te), size = 1.2, alpha = 1) +
    geom_area(data = RLDC.Wind%>% filter(hour.sorted %in% c(seq(1,8760,20),8760)), aes(x = hour.sorted, y = Wind.RLDC, fill = te), size = 1.2, alpha = 1) +
    geom_area(data = RLDC.Solar%>% filter(hour.sorted %in% c(seq(1,8760,20),8760)), aes(x = hour.sorted, y = Solar.RLDC, fill = te), size = 1.2, alpha = 1) +
    geom_area(data = RLDC.Batt_Li%>% filter(hour.sorted %in% c(seq(1,8760,20),8760)), aes(x = hour.sorted, y = Batt.Li.RLDC, fill = te), size = 1.2, alpha = 1) +
    geom_area(data = RLDC.Batt_H2%>% filter(hour.sorted %in% c(seq(1,8760,20),8760)), aes(x = hour.sorted, y = Batt.H2.RLDC, fill = te), size = 1.2, alpha = 1) +
    geom_area(data = RLDC.Batt2_H2%>% filter(hour.sorted %in% c(seq(1,8760,20),8760)), aes(x = hour.sorted, y = Batt.H2.RLDC2, fill = te), size = 1.2, alpha = 1) +
    geom_area(data = RLDC.H2%>% filter(hour.sorted %in% c(seq(1,8760,20),8760)), aes(x = hour.sorted, y = H2.RLDC, fill = te), size = 1.2, alpha = 1) +
    geom_area(data = RLDC1%>% filter(hour.sorted %in% c(seq(1,8760,20),8760)), aes(x = hour.sorted, y = RLDC1, fill = te), size = 1.2, alpha = 1) +
    xlab("Sorted hours of a year") + ylab("Residual load (GW)")+
    ggtitle(paste0("DIETER ", year_toplot))
  
  if (h2switch == "off"){
    p.DT.rldc <- p.DT.rldc + scale_fill_manual(name = "Technology", values = color.mapping.RLDC.basic)+
      coord_cartesian(ylim = c(min(CU_VRE_Solar.plot$Solar.RLDC2)*1.1,max(LDC0$load)*1.1 ))
  }
  
  if (h2switch == "on" | storswitch == "on"){
    p.DT.rldc <- p.DT.rldc + scale_fill_manual(name = "Technology", values = color.mapping.RLDC.fancy) + coord_cartesian(ylim = c(min(CU_VRE_Solar.plot$Solar.RLDC2)*1.1,max(LDC0$load)*1.1 ))
  }
  
  if (length(techranking) > 2){
    p.DT.rldc <- p.DT.rldc+
      geom_area(data = RLDC2%>% filter(hour.sorted %in% c(seq(1,8760,20),8760)), aes(x = hour.sorted, y = RLDC2, fill = te), size = 1.2, alpha = 1)
  }

  if (length(techranking) > 3){
    p.DT.rldc <- p.DT.rldc+
    geom_area(data = RLDC3%>% filter(hour.sorted %in% c(seq(1,8760,20),8760)), aes(x = hour.sorted, y = RLDC3, fill = te), size = 1.2, alpha = 1)
  }

  if (length(techranking) > 4){
    p.DT.rldc<-  p.DT.rldc+
      geom_area(data = RLDC4%>% filter(hour.sorted %in% c(seq(1,8760,20),8760)), aes(x = hour.sorted, y = RLDC4, fill = te), size = 1.2, alpha = 1)
  }

  if (length(techranking) > 5){
    p.DT.rldc <- p.DT.rldc +
      geom_area(data = RLDC5%>% filter(hour.sorted %in% c(seq(1,8760,20),8760)), aes(x = hour.sorted, y = RLDC5, fill = te), size = 1.2, alpha = 1)
  }
  
  p.DT.rldc <- p.DT.rldc +
    geom_area(data = CU_VRE_Solar.plot %>% filter(hour %in% c(seq(1,8760,20),8760)), aes(x = hour, y = Solar.RLDC2, fill = te), size = 1.2, alpha = 1)  +
    geom_area(data = CU_VRE_Wind.plot %>% filter(hour %in% c(seq(1,8760,20),8760)), aes(x = hour, y = Wind.RLDC2, fill = te), size = 1.2, alpha = 1)
  
  p.DT.rldc <- p.DT.rldc + 
    theme(axis.text=element_text(size=10), axis.title=element_text(size= 10, face="bold"))

  swfigure(sw, grid.draw, p.DT.rldc)
  
  if (save_png == 1){
    ggsave(filename = paste0(outputdir, "/DIETER/DIETER_RLDC_yr=", year_toplot, ".png"), p.DT.rldc, width = 8, height =8, units = "in", dpi = 120)
  }

# }

