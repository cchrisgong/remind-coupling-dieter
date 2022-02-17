# Data preparation --------------------------------------------------------

cat("Plot PE price, production and trade \n")

# get PE marginals
out.PE.marginals <- NULL
for (i in 1:length(remind.files)){
  remind.qm_budget <- file.path(outputdir, remind.files[i]) %>% 
    read.gdx("qm_budget", field="m", squeeze=F) %>% 
    filter(all_regi == reg) %>%
    filter(ttot %in% report.periods) %>% 
    select(period = ttot, qm_budget.m = m)
  
  PE.marginals <- file.path(outputdir, remind.files[i]) %>% 
    read.gdx("q_balPe", field="m", squeeze = F) %>%
    filter(all_regi == REGIkey1) %>% 
    filter(all_enty %in% c("pegas","pecoal","pebiolc")) %>% 
    select(period = ttot, fuel = all_enty, m) %>% 
    left_join(remind.qm_budget) %>% 
    mutate(m = m/qm_budget.m * 1e12 / sm_TWa_2_MWh * 1.2) %>% 
    replace(is.na(.), 0) %>%
    select(period,fuel,value=m) %>% 
    mutate(variable = "marginal")%>% 
    mutate(iter = i)
  
  out.PE.marginals <- rbind(out.PE.marginals, PE.marginals)
}

out.PE.price <- NULL
out.PE.avg.price <- NULL
out.PE.prod <- NULL

# get PE production and prices
for (i in 2:length(remind.files)){
  
  PE.price <- file.path(outputdir, remind.files[i]) %>% 
    read.gdx("pm_PEPrice", squeeze = F) %>%
    filter(all_regi == REGIkey1) %>% 
    filter(all_enty %in% c("pegas","pecoal","pebiolc")) %>% 
    mutate(value = value * 1e12 / sm_TWa_2_MWh * 1.2) %>% 
    select(period = ttot, fuel = all_enty, value) %>% 
    mutate(variable = "pricePe")%>% 
    mutate(iter = i-1)
  
  # in REMIND PE prices can be averaged over 3 iterations before being passed to DIETER
  PE.avg.price  <- file.path(outputdir, remind.files[i]) %>% 
    read.gdx("p32_fuelprice_avgiter", squeeze = F) %>%
    filter(all_regi == REGIkey1) %>% 
    filter(all_enty %in% c("pegas","pecoal","pebiolc")) %>% 
    mutate(value = value * 1e12 / sm_TWa_2_MWh * 1.2) %>% 
    select(period = ttot, fuel = all_enty, value) %>% 
    mutate(variable = "avg_pricePe")%>% 
    mutate(iter = i-1)
  
  PE.prod <- file.path(outputdir, remind.files[i]) %>% 
    read.gdx("vm_prodPe", field="l", squeeze = F) %>%
    filter(all_regi == REGIkey1) %>% 
    filter(all_enty %in% c("pegas","pecoal","pebiolc")) %>% 
    mutate(value = value *sm_TWa_2_MWh/1e6) %>% 
    select(period = ttot, fuel = all_enty, value) %>% 
    mutate(variable = "vm_prodPe")%>% 
    mutate(iter = i-1)
  
  out.PE.price <- rbind(out.PE.price, PE.price)
  out.PE.avg.price  <- rbind(out.PE.price, PE.avg.price)
  out.PE.prod <- rbind(out.PE.prod, PE.prod)
}

# get PE trade variables
out.PE.trade <- NULL
for (i in 2:length(remind.files)){
  PE.export <- file.path(outputdir, remind.files[i]) %>% 
    read.gdx("vm_Xport", field="l", squeeze = FALSE) %>% 
    filter(tall %in% report.periods) %>% 
    filter(all_regi == REGIkey1) %>% 
    filter(all_enty %in% c("pecoal", "pegas","pebiolc")) %>% 
    mutate(value = value * sm_TWa_2_MWh/1e6) %>% 
    select(period = tall,fuel = all_enty, export = value)
  
  PE.mport <- file.path(outputdir, remind.files[i]) %>% 
    read.gdx("vm_Mport", field="l", squeeze = FALSE) %>% 
    filter(tall %in% report.periods) %>% 
    filter(all_regi == REGIkey1) %>% 
    filter(all_enty %in% c("pecoal", "pegas","pebiolc")) %>% 
    mutate(value = value * sm_TWa_2_MWh/1e6) %>% 
    select(period = tall,fuel = all_enty, import = value)
  
  PE.importcost  <- file.path(outputdir, remind.files[i]) %>% 
    read.gdx("pm_costsPEtradeMp", squeeze = FALSE)  %>% 
    filter(all_regi == REGIkey1) %>% 
    filter(all_enty %in% c("pecoal", "pegas","pebiolc")) %>% 
    select(fuel = all_enty, mportcost = value)
  
  PE.import = list(PE.mport, PE.importcost) %>% 
    reduce(left_join) %>% 
    replace(is.na(.), 0) %>%
    mutate(import = (1 - mportcost) * import) %>% 
    select(-mportcost)
  
  PE.extr <- file.path(outputdir, remind.files[i]) %>% 
    read.gdx("vm_fuExtr", field="l", squeeze = FALSE)  %>% 
    filter(ttot %in% report.periods) %>% 
    filter(all_regi == REGIkey1) %>% 
    filter(all_enty %in% c("pecoal", "pegas","pebiolc"))  %>% 
    filter(rlf == "1") %>% 
    mutate(value = -value * sm_TWa_2_MWh/1e6) %>% 
    select(period = ttot,fuel = all_enty, extr = value)
  
  PE.trade = list(PE.export, PE.import, PE.extr) %>%
    reduce(full_join) %>% 
    mutate(iter = i-1)
  
  out.PE.trade <- rbind(out.PE.trade, PE.trade)

  }
  
out.PE.trade <-out.PE.trade %>% 
    gather(key = "variable", value = "value", -period,-fuel,-iter)

# fuel price DIETER sees
out.dieter.PEprice <- NULL
for (i in 1:(length(dieter.files.report))){
  
  dieter.PEprice <- file.path(outputdir, dieter.files.report[i]) %>% 
  read.gdx("report_tech", squeeze = F) %>% 
  select(model = X..1, period = X..2, variable = X..4, tech = X..5, value) %>% 
  filter(variable %in% c("primary energy price ($/MWh)")) %>% 
  revalue.levels(tech = dieter.tech.mapping) %>% 
  filter(tech %in% names(dieter.supply.fuel.mapping)) %>% 
  revalue.levels(tech = dieter.supply.fuel.mapping) %>% 
  select(period,variable,fuel=tech,value) %>% 
  mutate(variable = "DIETER PE price ($/MWh)")

  dieter.PEprice$iter <- i
  
out.dieter.PEprice<- rbind(out.dieter.PEprice, dieter.PEprice)
}
### plot PE price and production/demand relation

swlatex(sw, paste0("\\section{PE price, production and trade}"))
#####################################################################################################
swlatex(sw, paste0("\\subsection{Price and production for main PE fuels - REMIND"))

secAxisScale= 1/20
  p<-ggplot() +
    geom_line(data = out.PE.marginals, aes(x = iter, y = value, color = fuel, linetype=variable), size = 1.2, alpha = 0.5) +
    geom_line(data = out.PE.price , aes(x = iter, y = value, color = fuel, linetype=variable), size = 1.2, alpha = 0.5) +
    geom_line(data = out.PE.avg.price, aes(x = iter, y = value, color = fuel, linetype=variable), size = 1.2, alpha = 0.5) +
    geom_line(data = out.PE.prod, aes(x = iter, y = value * secAxisScale, color =fuel,linetype=variable), size = 1.2, alpha = 0.5) +
    theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold")) +
    scale_y_continuous(sec.axis = sec_axis(~./secAxisScale, name = paste0("PE production (DIETER)", "(TWh)")))+
    xlab("iteration") + ylab(paste0("PE price (DIETER)",  "(USD/MWh)"))  +
    coord_cartesian(ylim = c(-0.5,80))+
    facet_wrap(~period, nrow = 3)
  
  swfigure(sw, grid.draw, p)
  if (save_png == 1){
    ggsave(filename = paste0(outputdir, "/PE_price_prod_RM.png"),  p,  width = 20, height =13, units = "in", dpi = 120)
  }
  
#####################################################################################################  
  swlatex(sw, paste0("\\subsection{Price of main PE fuels - DIETER"))
  
  p<-ggplot() +
    geom_line(data = out.dieter.PEprice, aes(x = iter, y = value, color = fuel), size = 1.2, alpha = 0.5) +
    theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold")) +
    xlab("iteration") + ylab(paste0("PE price (DIETER)",  "(USD/MWh)"))  +
    coord_cartesian(ylim = c(-0.5,80))+
    facet_wrap(~period, nrow = 3)

  swfigure(sw, grid.draw, p)
  if (save_png == 1){
    ggsave(filename = paste0(outputdir, "/PE_price_DT.png"),  p,  width = 20, height =13, units = "in", dpi = 120)
  }
  #####################################################################################################
  swlatex(sw, paste0("\\subsection{Price, production and trade of coal - REMIND"))
  
  secAxisScale= 1/20
  p<-ggplot() +
    geom_line(data = out.PE.price%>% filter(fuel == "pecoal") , aes(x = iter, y = value, linetype=variable), size = 1.2, alpha = 0.5) +
    geom_area(data = out.PE.trade%>% filter(fuel == "pecoal"), aes(x = iter, y = value * secAxisScale, fill = variable), size = 1.2, alpha = 0.5) +
    theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold")) +
    scale_y_continuous(sec.axis = sec_axis(~./secAxisScale, name = paste0("Coal extraction and trade (REMIND)", " (TWh)")))+
    xlab("iteration") + ylab(paste0("Coal price (REMIND)",  " USD/MWh)"))  +
    facet_wrap(~period, nrow = 3)

  swfigure(sw, grid.draw, p)
  if (save_png == 1){
    ggsave(filename = paste0(outputdir, "/COAL_price_quantity_RM.png"),  p,  width = 20, height =13, units = "in", dpi = 120)
  }
  #####################################################################################################
  swlatex(sw, paste0("\\subsection{Price, production and trade of gas - REMIND"))

  p <-ggplot() +
    geom_line(data = out.PE.price%>% filter(fuel == "pegas") , aes(x = iter, y = value, linetype=variable), size = 1.2, alpha = 0.5) +
    geom_area(data = out.PE.trade%>% filter(fuel == "pegas"), aes(x = iter, y = value * secAxisScale, fill = variable), size = 1.2, alpha = 0.5) +
    theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold")) +
    scale_y_continuous(sec.axis = sec_axis(~./secAxisScale, name = paste0("Gas extraction and trade (REMIND)", " (TWh)")))+
    xlab("iteration") + ylab(paste0("Gas price (REMIND) ",  "(USD/MWh)"))  +
    facet_wrap(~period, nrow = 3)

  swfigure(sw, grid.draw, p)
  if (save_png == 1){
    ggsave(filename = paste0(outputdir, "/GAS_price_quantity_RM.png"),  p,  width = 20, height =13, units = "in", dpi = 120)
  }
  
  # 
  secAxisScale= 1/5
  p <-ggplot() +
    geom_line(data = out.PE.price%>% filter(fuel == "pebiolc") , aes(x = iter, y = value, linetype=variable), size = 1.2, alpha = 0.5) +
    geom_area(data = out.PE.trade%>% filter(fuel == "pebiolc"), aes(x = iter, y = value * secAxisScale, fill = variable), size = 1.2, alpha = 0.5) +
    theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold")) +
    scale_y_continuous(sec.axis = sec_axis(~./secAxisScale, name = paste0("Biomass extraction and trade (REMIND)", "(TWh)")))+
    xlab("iteration") + ylab(paste0("Biomass price (REMIND) ",  "(USD/MWh)"))  +
    facet_wrap(~period, nrow = 3)

  swfigure(sw, grid.draw, p)
  if (save_png == 1){
    ggsave(filename = paste0(outputdir, "/BIOMASS_price_quantity_RM.png"),  p,  width = 20, height =13, units = "in", dpi = 120)
  }
  
  