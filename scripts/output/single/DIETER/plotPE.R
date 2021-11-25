# Data preparation --------------------------------------------------------

cat("Plot PE price \n")

out.remind.PEMarginals <- NULL
out.pm.PEPrice <- NULL
out.remind.TradeVariable<- NULL

for (i in 1:length(remind.files)){
  
  budget.data <- file.path(outputdir, remind.files[i]) %>%
    read.gdx("qm_budget", field="m", squeeze = FALSE) %>%
    filter(all_regi == "DEU") %>%
    mutate(m = -m) %>%
    dplyr::rename(budget = m)  %>%
    select(period= ttot, budget)
  
  PE.price <- file.path(outputdir, remind.files[i]) %>%
    read.gdx("q_balPe", field="m", squeeze = FALSE) %>%
    filter(all_regi == "DEU") %>% 
    filter(all_enty %in% c("pegas","pecoal","pebiolc")) %>% 
    mutate(m = -m) %>% 
    dplyr::rename(value = m) %>% 
    select(period = ttot,fuel = all_enty, value) %>% 
    left_join(budget.data) %>% 
    mutate(value = value/budget * 1e12 / sm_TWa_2_MWh * 1.2) %>% 
    replace(is.na(.), 0) %>%
    select(period,fuel,value) %>% 
    mutate(variable = "marginal")
  
  PE.price$iter <- i
  
  out.remind.PEMarginals <- rbind(out.remind.PEMarginals,PE.price)
  
  p.PEPrice <- file.path(outputdir, remind.files[i]) %>%
    read.gdx("p_PEPrice", squeeze = FALSE) %>% 
    filter(all_regi == "DEU") %>% 
    filter(all_enty %in% c("pecoal", "pegas","pebiolc")) %>% 
    mutate(value = value * 1e12 / sm_TWa_2_MWh * 1.2) %>% 
    select(period = ttot,fuel = all_enty, value)%>% 
    mutate( variable = "price")
  
  vm.prodPe <- file.path(outputdir, remind.files[i]) %>%
    read.gdx("vm_prodPe", field="l", squeeze = FALSE)  %>% 
    filter(all_regi == "DEU") %>% 
    filter(all_enty %in% c("pecoal", "pegas","pebiolc"))  %>% 
    mutate(value = value *sm_TWa_2_MWh/1e6) %>% 
    select(period = ttot,fuel = all_enty, value) %>% 
    mutate(variable = "production")
  
  fuelprice.avgiter <- file.path(outputdir, remind.files[i]) %>%
    read.gdx("p32_fuelprice_avgiter", squeeze = FALSE)  %>% 
    filter(all_regi == "DEU") %>% 
    filter(all_enty %in% c("pecoal", "pegas","pebiolc"))  %>% 
    mutate(value = -value * 1e12 / sm_TWa_2_MWh * 1.2) %>% 
    select(period = ttot,fuel = all_enty, value)%>% 
    mutate( variable = "price4DT")
  
  pm.PEPrice <- list(p.PEPrice, vm.prodPe,fuelprice.avgiter) %>%
    reduce(full_join)
  
  pm.PEPrice$iter <- i
  out.pm.PEPrice <- rbind(out.pm.PEPrice, pm.PEPrice)
  
  remind.xport <- file.path(outputdir, remind.files[i]) %>%
    read.gdx("vm_Xport", field="l", squeeze = FALSE) %>% 
    filter(all_regi == "DEU") %>% 
    filter(all_enty %in% c("pecoal", "pegas","pebiolc")) %>% 
    mutate(value = value * sm_TWa_2_MWh/1e6) %>% 
    select(period = tall,fuel = all_enty, export = value)
  
  remind.mport.initial <- file.path(outputdir, remind.files[i]) %>%
    read.gdx("vm_Mport", field="l", squeeze = FALSE)  %>% 
    filter(all_regi == "DEU") %>% 
    filter(all_enty %in% c("pecoal", "pegas","pebiolc"))  %>% 
    mutate(value = value * sm_TWa_2_MWh/1e6) %>% 
    select(period = tall,fuel = all_enty, value)
  
  remind.mportcost <- file.path(outputdir, remind.files[i]) %>%
    read.gdx("pm_costsPEtradeMp", squeeze = FALSE)  %>% 
    filter(all_regi == "DEU") %>% 
    filter(all_enty %in% c("pecoal", "pegas","pebiolc")) %>% 
    select(fuel = all_enty, mportcost = value)
  
  remind.mport <- list(remind.mport.initial, remind.mportcost) %>% 
    reduce(left_join) %>%
    replace(is.na(.), 0) %>%
    mutate(value = (1-mportcost) * value) %>% 
    select(period,fuel, import = value)
  
  remind.vm_fuExtr <- file.path(outputdir, remind.files[i]) %>%
    read.gdx("vm_fuExtr", field="l", squeeze = FALSE)  %>% 
    filter(all_regi == "DEU") %>% 
    filter(all_enty %in% c("pecoal", "pegas","pebiolc"))  %>% 
    filter(rlf == "1") %>% 
    mutate(value = value * sm_TWa_2_MWh/1e6) %>% 
    select(period = ttot,fuel = all_enty, prod = value)
  
  remind.TradeVariable <- list(remind.xport, remind.mport,remind.vm_fuExtr) %>%
    reduce(full_join)
  
  remind.TradeVariable$iter <- i
  out.remind.TradeVariable <- rbind(out.remind.TradeVariable,remind.TradeVariable)
}


out.pe_price <- out.pm.PEPrice %>% 
  filter(variable == "price")

out.pe_prod <- out.pm.PEPrice %>% 
  filter(variable == "production")

out.pe_price_4DT <- out.pm.PEPrice %>% 
  filter(variable == "price4DT")

out.remind.TradeVariable <-out.remind.TradeVariable %>% 
  gather(key = "variable", value = "value", -period,-fuel,-iter)



# Data preparation (DIETER) -----------------------------------------------
VAR_report_key_DT = c("DIETER Market value w/ scarcity price shaved ($/MWh)","genshares (%)", "price w/ scarcity price shaved ($/MWh)","primary energy price ($/MWh)")
out.dieter.data <- NULL
if (length(dieter.files) != 0) {
  for (i in 1:length(sorted_files_DT_report)){
    
    dieter.data <- NULL
    rep = read.gdx(gdxName = file.path(outputdir, sorted_files_DT_report[i]), requestList = 'report', factors = FALSE)
    
    names(rep) <- c("gdxfile", "model","year", "country","variable", "value")
    out <- rep %>% 
      mutate(MODEL = model, SCENARIO = paste0("baseline"), 
             REGION = country, YEAR = year, VALUE = round(value, digits = 4), 
             TECH = "all Tech",
             VARIABLE = variable,
             PERIOD = "annual") %>%
      arrange(YEAR) %>% 
      select(MODEL, SCENARIO, YEAR, REGION, PERIOD, VARIABLE, TECH, VALUE)
    
    #################################################################
    rep_Tech = read.gdx(gdxName = file.path(outputdir, sorted_files_DT_report[i]), requestList = 'report_tech', factors = FALSE)
    
    names(rep_Tech) <- c("gdxfile", "model","year", "country","variable", "tech", "value")
  
    out_t <- rep_Tech %>% 
      select(model, year, tech,variable, country,value) %>%
      group_by(model, tech, variable, country) %>%
      mutate(year = as.numeric(year)) %>%
      complete(year = c(2010,2015,2020,2025,2030,2035,2040,2045,2050,2055,2060,2070,2080,2090,2100)) %>%
      replace(is.na(.), 0) %>%
      ungroup(model, tech, variable, country) %>% 
      mutate(MODEL = model, SCENARIO = paste0("baseline"), 
             REGION = country, YEAR = year, VALUE = round(value, digits = 4), 
             TECH = tech,
             VARIABLE = variable,
             PERIOD = "annual"
      ) %>%
      arrange(YEAR) %>%
      select(MODEL, SCENARIO, YEAR, REGION, PERIOD, VARIABLE, TECH, VALUE)
    
    #################################################################
    dieter.data <- rbind(dieter.data, out)
    dieter.data <- rbind(dieter.data, out_t)
    
    dieter.data.PE <- dieter.data %>% 
      filter(YEAR >2015) %>% 
      filter(VARIABLE %in% VAR_report_key_DT) %>% 
      filter(MODEL == "DIETER") %>% 
      select(YEAR, TECH, VARIABLE, VALUE) %>% 
      filter(!TECH %in% dieter.tech.exclude) %>% 
      revalue.levels(TECH = dieter.tech.mapping) %>% 
      replace(is.na(.), 0) 
    
    dieter.data.PE$iter <- id[i]
    dieter.data.PE$period <- dieter.data.PE$YEAR
    out.dieter.data <- rbind(out.dieter.data, dieter.data.PE)
  
  }
}

dieter.data.marketValue <- out.dieter.data %>% 
  filter(VARIABLE == "DIETER Market value w/ scarcity price shaved ($/MWh)") %>% 
  select(iter,period=YEAR,TECH,VALUE,model=VARIABLE) 

dieter.data.seelprice <- out.dieter.data %>% 
  filter(VARIABLE == "price w/ scarcity price shaved ($/MWh)") %>% 
  mutate(VARIABLE = "DIETER seel price ($/MWh)") %>% 
  select(iter,period=YEAR,VALUE,model=VARIABLE) 

dieter.data.genshare <- out.dieter.data %>% 
  filter(VARIABLE == "genshares (%)") %>% 
  select(iter,period=YEAR,TECH,VALUE) %>% 
  mutate(model = "DIETER")

dieter.data.peprice <- out.dieter.data %>% 
  filter(VARIABLE == "primary energy price ($/MWh)") %>%
  revalue.levels(TECH = dieter.supply.fuel.mapping) %>% 
  filter(TECH %in% dieter.supply.fuel.mapping) %>% 
  select(iter,period=YEAR,VARIABLE,fuel=TECH,VALUE) %>% 
  mutate(variable = "DIETER PE price ($/MWh)")


# Plotting ----------------------------------------------------------------

swlatex(sw,"\\onecolumn")
swlatex(sw, paste0("\\section{PE price}"))


swlatex(sw, paste0("\\subsection{REMIND PE price (USD/MWh) }"))

secAxisScale1= 1/20
p1a<-ggplot() +
  geom_line(data = out.remind.PEMarginals, aes(x = iter, y = value, color = fuel, linetype=variable), size = 1.2, alpha = 0.5) +
  geom_line(data = out.pe_price, aes(x = iter, y = value, color = fuel, linetype=variable), size = 1.2, alpha = 0.5) +
  geom_line(data = out.pe_prod, aes(x = iter, y = value * secAxisScale1, color =fuel,linetype=variable), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(), axis.title=element_text()) +
  scale_y_continuous(sec.axis = sec_axis(~./secAxisScale1, name = paste0("PEprod", "(TWh)")))+
  xlab("iteration") + ylab(paste0("PE price",  "(USD/MWh)"))  +
  coord_cartesian(ylim = c(-0.5,80))+
  facet_wrap(~period, nrow = 3)

swfigure(sw,print,p1a,sw_option="width=20, height=12")

swlatex(sw,"\\onecolumn")
swlatex(sw, paste0("\\subsection{DIETER PE price (USD/MWh) }"))

p1b<-ggplot() +
  geom_line(data = dieter.data.peprice, aes(x = iter, y = VALUE, color = fuel, linetype=variable), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(), axis.title=element_text()) +
  scale_y_continuous(sec.axis = sec_axis(~./secAxisScale1, name = paste0("PEprod", "(TWh)")))+
  xlab("iteration") + ylab(paste0("PE price",  "(USD/MWh)"))  +
  coord_cartesian(ylim = c(-0.5,80))+
  facet_wrap(~period, nrow = 3)

swfigure(sw,print,p1b,sw_option="width=20, height=12")

swlatex(sw,"\\onecolumn")
swlatex(sw, paste0("\\subsection{Coal price (USD/MWh)}"))



p1c<-ggplot() +
  geom_line(data = out.pe_price%>% filter(fuel == "pecoal") , aes(x = iter, y = value, linetype=variable), size = 1.2, alpha = 0.5) +
  geom_area(data = out.remind.TradeVariable %>% filter(fuel == "pecoal"), aes(x = iter, y = value * secAxisScale1, fill = variable), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(), axis.title=element_text()) +
  scale_y_continuous(sec.axis = sec_axis(~./secAxisScale1, name = paste0("PE breakdown", "(TWh)")))+
  xlab("iteration") + ylab(paste0("Coal price",  "(USD/MWh)"))  +
  coord_cartesian(ylim = c(-0.5,80))+
  facet_wrap(~period, nrow = 3)

swfigure(sw,print,p1c,sw_option="width=20, height=12")

swlatex(sw,"\\onecolumn")
swlatex(sw, paste0("\\subsection{Gas price (USD/MWh)}"))


p1d<-ggplot() +
  geom_line(data = out.pe_price%>% filter(fuel == "pegas") , aes(x = iter, y = value, linetype=variable), size = 1.2, alpha = 0.5) +
  geom_area(data = out.remind.TradeVariable %>% filter(fuel == "pegas"), aes(x = iter, y = value * secAxisScale1, fill = variable), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(), axis.title=element_text()) +
  scale_y_continuous(sec.axis = sec_axis(~./secAxisScale1, name = paste0("PE breakdown", "(TWh)")))+
  xlab("iteration") + ylab(paste0("Gas price",  "(USD/MWh)"))  +
  coord_cartesian(ylim = c(-0.5,80))+
  facet_wrap(~period, nrow = 3)

swfigure(sw,print,p1d,sw_option="width=20, height=12")

swlatex(sw,"\\onecolumn")
swlatex(sw, paste0("\\subsection{Biomass price (USD/MWh)}"))


secAxisScale= 1/5
p1e<-ggplot() +
  geom_line(data = out.pe_price%>% filter(fuel == "pebiolc") , aes(x = iter, y = value, linetype=variable), size = 1.2, alpha = 0.5) +
  geom_area(data = out.remind.TradeVariable %>% filter(fuel == "pebiolc"), aes(x = iter, y = value * secAxisScale, fill = variable), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(), axis.title=element_text()) +
  scale_y_continuous(sec.axis = sec_axis(~./secAxisScale, name = paste0("PE breakdown", "(TWh)")))+
  xlab("iteration") + ylab(paste0("Biomass price",  "(USD/MWh)"))  +
  coord_cartesian(ylim = c(-0.5,70))+
  facet_wrap(~period, nrow = 3)

swfigure(sw,print,p1e,sw_option="width=20, height=12")


# swlatex(sw, paste0("\\subsection{Seel price and capacity factor over time (last iteration)}"))
# 
# plot.remind.seel <- out.pm.SEPrice %>% 
#   filter(iteration == max(iteration))
# 
# plot.remind.capfac <- out.remind.capfac %>% 
#   filter(iter == max(iter))
# 
# p <- ggplot() + 
#   geom_line(data=plot.remind.seel, aes(x=period, y=value, color = tech), size = 2, alpha = 0.5) +
#  scale_size_manual(name="Shadow price", values=1) + 
#   geom_line(data=plot.remind.capfac, aes(x=period, y=value, colour=tech)) +
#   scale_colour_manual(name = "tech", values = custColors) +
#   scale_y_continuous(name="REMIND secondary electricity price [$/MWh]", limits=c(-120,250), sec.axis = sec_axis(~./1, name = paste0("CF", "(%)"))) +
#     xlab("Time")
# 
# swfigure(sw,print,p,sw_option="width=20, height=12")