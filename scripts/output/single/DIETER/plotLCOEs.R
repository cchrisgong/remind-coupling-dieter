# Data preparation --------------------------------------------------------
# the script that generates LCOE is from Felix: https://github.com/pik-piam/remind2/blob/master/R/reportLCOE.R

cat("Plot LCOEs \n")
### read mif

plot.tech <- c("elh2","system")
plot.sector <- c("supply-side")
iter_toplot = maxiter

# read mif and LCOE data
mifpath <- grep("REMIND_generic.*.mif", list.files(outputdir, full.names = T), value=T)

dat.mod <- read.report(mifpath, as.list=FALSE)
df.mod <- read.quitte(mifpath)

lcoe.file <- grep("REMIND_LCOE.*", list.files(outputdir, full.names = T), value=T)
df.lcoe <- read.csv(lcoe.file, sep = ";", header = T, colClasses = c(rep("factor", 3), "numeric", rep("factor", 6), "numeric") )
  
#system markup: out.remind.sys.mrkup
#te markup: out.remind.mrkup
#te flexadj: out.remind.flexadj
# generation share:　out.remind.genshare

  # load generation share
prod_share <- file.path(outputdir, remind.files[iter_toplot]) %>% 
    read.gdx("v32_shSeElDisp", squeeze = F)  %>% 
    filter(all_regi == reg) %>% 
    filter(all_te %in% names(remind.tech.mapping)) %>% 
    select(period=ttot, tech=all_te, genshare = value) %>% 
    filter(period %in% model.periods.till2100) %>% 
    mutate(genshare = genshare/1e2) %>% 
    mutate(period = as.numeric(period))
  
mv <- file.path(outputdir, remind.files[iter_toplot]) %>% 
  read.gdx("p32_marketValue", squeeze=F)  %>% 
  filter(all_regi == reg) %>%
  filter(all_te %in% names(remind.tech.mapping)) %>% 
  select(period = ttot, tech = all_te, value) %>%
  filter(period %in% model.periods.till2100) %>% 
  mutate(cost = "Market value") %>% 
  mutate(value = value * 1e12 / sm_TWa_2_MWh * 1.2)

sp <- file.path(outputdir, remind.files[iter_toplot]) %>% 
  read.gdx("p32_shadowPrice", squeeze=F)  %>% 
  filter(all_regi == reg) %>%
  filter(all_te %in% names(remind.tech.mapping)) %>% 
  select(period = ttot, tech = all_te, value) %>%
  filter(period %in% model.periods.till2100) %>% 
  mutate(value = value * 1e12 / sm_TWa_2_MWh * 1.2)

sp.capcon <- file.path(outputdir, remind.files[iter_toplot]) %>% 
  read.gdx("p32_capConShadowPrice", squeeze=F)  %>% 
  filter(all_regi == reg) %>%
  filter(all_te %in% names(remind.nonvre.mapping))  %>% 
  select(period = ttot, tech = all_te, value) %>%
  filter(period %in% model.periods.till2100) %>% 
  mutate(value = value * 1e12 / sm_TWa_2_MWh * 1.2)

mrkup <- out.remind.mrkup %>% 
  mutate(cost = "Markup subsidy/tax") %>% 
  filter(period %in% model.periods.till2100) %>% 
  filter(iteration == iter_toplot -1) %>% 
  select(-iteration)

mv.agg <- out.remind.mv %>% 
  mutate(cost = "Market value") %>% 
  filter(period %in% model.periods.till2100) %>% 
  filter(iteration == iter_toplot -1) %>% 
  select(-iteration)

if (h2switch == "off"){
  mv.agg <- mv.agg %>% 
    filter(!tech %in% remind.sector.coupling.mapping)
}

df.markup.sys <- out.remind.sys.mrkup %>% 
  filter(iteration == iter_toplot -1) %>% 
  select(-iteration) %>% 
  filter(period %in% model.periods.till2100) %>% 
  mutate(tech = "System") %>% 
  mutate(sector = "supply-side") %>%  
  mutate(cost = "Total Markup")

flexadj <- out.remind.flexadj %>% 
  filter(iteration == iter_toplot -1) %>% 
  select(-iteration) %>% 
  filter(period %in% model.periods.till2100) %>% 
  mutate(cost = "flexibility subsidy") %>% 
  mutate(sector = "supply-side") 

# aggregated production share per upscaled technology
prod_aggShare_RM <- prod_share %>% 
  revalue.levels(tech = remind.tech.mapping) %>% 
  dplyr::group_by(period,tech) %>%
  dplyr::summarise( genshare = sum(genshare), .groups = "keep" ) %>% 
  dplyr::ungroup(period,tech) %>% 
  select(period,tech,aggshare = genshare)

prod_share_RM <- prod_share %>%
  mutate(tech2 = tech) %>%
  revalue.levels(tech = remind.tech.mapping)

#share of REMIND tech production within a DIETER type
prod_shareType_RM <- list(prod_aggShare_RM, prod_share_RM) %>%
  reduce(full_join) %>%
  mutate(share = genshare / aggshare) %>%
  select(period,tech=tech2,share) %>% 
  mutate(period = as.numeric(period))

#####################################################################################################
#REMIND marginal LCOE component for each tech (marginal in the sense of one additional added unit of generation of each tech)
df.lcoe.te.total <- df.lcoe %>% 
  filter(region == reg,
         period %in% model.periods, type == "marginal",
         tech %in% map.price.tech$tech, sector %in% plot.sector) %>% 
  filter( cost == c("Total LCOE")) %>% 
  filter( output == "seel") %>% 
  select(period, tech, value) %>% 
  dplyr::rename( totalLCOE = value )

df.lcoe.grid<- df.lcoe %>% 
  filter(region == reg,
         period %in% model.periods, type == "average",
         tech %in% map.price.tech$tech, sector %in% plot.sector) %>% 
  filter(cost == "Grid Cost") %>% 
  filter( output %in% c("seel","seh2")) %>% 
  replace(is.na(.), 0) %>% 
  select(period, tech, cost, lcoe=value)

#component (marginal) LCOE per tech
df.lcoe.te.components <- df.lcoe %>% 
  filter(region == reg,
         period %in% model.periods, type == "marginal",
         tech %in% map.price.tech$tech, sector %in% plot.sector) %>% 
  filter(! cost == c("Total LCOE")) %>% 
  filter(period %in% model.periods.till2100) %>% 
  filter(tech %in% names(remind.tech.mapping)) %>% 
  filter( output %in% c("seel","seh2")) %>% 
  replace(is.na(.), 0) %>% 
  select(period, tech, cost, lcoe=value) %>% 
  full_join(df.lcoe.grid) %>% 
  mutate(period = as.numeric(period))

df.lcoe.te.components[mapply(is.infinite, df.lcoe.te.components)] <- 0

if (h2switch == "off"){
  df.lcoe.te.components <- df.lcoe.te.components %>% 
    filter(!tech %in% names(remind.sector.coupling.mapping))
}

#weighted total system (marginal) LCOE (with cost breakdown)
df.lcoe.sys.components <- list(prod_share, df.lcoe.te.components) %>% 
  reduce(left_join) %>% 
  replace(is.na(.), 0) %>%
  filter(period %in% model.periods.till2100) %>% 
  select(period,genshare, cost,lcoe) %>% 
  mutate(sysLCOECOMP = genshare * lcoe) %>% 
  select(period,cost,value = sysLCOECOMP) %>%
  dplyr::group_by(period,cost) %>%
  dplyr::summarise( value = sum(value), .groups = "keep" ) %>% 
  dplyr::ungroup(period,cost) %>% 
  mutate(tech = "System") %>% 
  mutate(sector = "supply-side") %>%  
  mutate(variable = "Total LCOE")

#weighted total system (marginal) LCOE (no cost breakdown)
df.lcoe.sys <- list(prod_share, df.lcoe.te.total) %>% 
  reduce(full_join)  %>% 
  replace(is.na(.), 0) %>% 
  mutate_all(function(x) ifelse(is.infinite(x), 0, x)) %>% 
  select(period,genshare, totalLCOE) %>% 
  mutate(sysLCOE = genshare * totalLCOE) %>% 
  select(period,value = sysLCOE) %>% 
  dplyr::group_by(period) %>%
  dplyr::summarise( value = sum(value), .groups = "keep" ) %>% 
  dplyr::ungroup(period) %>% 
  mutate(tech = "System") %>% 
  mutate(sector = "supply-side") %>%  
  mutate(variable = "Total LCOE")

### electrolyzers LCOE
df.lcoe.elh2.components <- df.lcoe %>% 
  filter(region == reg, 
         period %in% model.periods, type == "marginal",
         tech %in% plot.tech, sector %in% plot.sector,
         value != 0) %>% 
  filter( ! cost %in% c("Total LCOE")) %>% 
  order.levels(tech = plot.tech, cost = names(cost.colors.te)) %>% 
  revalue.levels(tech = tech.label) %>% 
  select(period, tech, cost, sector,value) %>% 
  mutate(period = as.numeric(period))

# filter for total LCOE
df.lcoe.elh2.total <- df.lcoe %>% 
  filter(region == reg,
         period %in% model.periods, type == "marginal",
         tech %in% plot.tech, sector %in% plot.sector) %>% 
  filter( cost == c("Total LCOE")) %>%
  select(-unit, -cost) %>% 
  order.levels(tech = plot.tech) %>% 
  revalue.levels(tech = tech.label) %>% 
  mutate(variable = "Total (marginal) LCOE") %>% 
  mutate(cost = "Total (marginal) LCOE") %>% 
  select(period, tech, variable, cost, sector,value)

# prepare REMIND prices for plot, calculate 15-year moving average and convert to USD2015/MWh, join with total LCOE for lineplot
df.price0 <- df.mod %>% 
  filter(region == reg, variable %in% map.price.tech$variable) %>% 
  # convert from USD2005/GJ to USD2015/MWh
  mutate( value = value * 1.2 * 3.66) %>% 
  left_join(map.price.tech) %>% 
  select(-unit, -variable) %>% 
  mutate( variable = "REMIND Price") %>% 
  filter(tech %in% plot.tech) %>% 
  revalue.levels(tech = tech.label) %>% 
  select(period, tech, variable, sector,value) %>% 
  distinct()

df.price <- df.price0 %>% 
  filter(period %in% model.periods)

df.lcoe.teAgg <- list(prod_shareType_RM, df.lcoe.te.components) %>% 
  reduce(full_join) %>% 
  mutate(period = as.numeric(period)) %>%
  replace(is.na(.), 0) %>% 
  mutate(value = share * lcoe) %>% 
  revalue.levels(tech = remind.tech.mapping) %>% 
  dplyr::group_by(period,tech,cost) %>%
  dplyr::summarise( value = sum(value), .groups = "keep" ) %>% 
  dplyr::ungroup(period,tech,cost) %>% 
  mutate(period = as.numeric(period)) %>% 
  filter(period %in% model.periods.till2100)

df.mrkup.plot <- remind.mrkup.non0gen %>% 
  filter(iteration == iter_toplot-1) %>% 
  select(-iteration) %>% 
  mutate(cost = "Markup subsidy/tax")

df.lcoe.te.plot <- df.lcoe.teAgg %>% 
  full_join(df.mrkup.plot)
  
df.total.lcoe.teAgg <- df.lcoe.teAgg %>% 
  dplyr::group_by(period, tech) %>%
  dplyr::summarise( value = sum(value), .groups = "keep" ) %>% 
  dplyr::ungroup(period,tech) %>% 
  select(period,tech,totalLCOE=value)
  
# net LCOE for generating tech
df.lcoe_minus_markup.te <- df.total.lcoe.teAgg %>%
                  full_join(df.mrkup.plot) %>% 
                  mutate(totalLCOE = totalLCOE + value) %>%
                  mutate(cost = "Total (marginal) LCOE + Markup")

df.lcoe_minus_markup.te.plot <- df.lcoe_minus_markup.te %>% 
  select(-value) %>% 
  dplyr::rename( value = totalLCOE )

df.telcoe_mv.plot <- list(df.lcoe_minus_markup.te.plot, mv.agg) %>% 
  reduce(full_join) %>%
  filter(period %in% model.periods.till2100)

# weighted total system (average) shadow price
df.sp.sys <- list(prod_share, sp) %>% 
  reduce(full_join) %>% 
  replace(is.na(.), 0) %>% 
  mutate_all(function(x) ifelse(is.infinite(x), 0, x)) %>% 
  select(period, genshare, value) %>% 
  mutate(value = genshare * value) %>% 
  dplyr::group_by(period) %>%
  dplyr::summarise( value = sum(value), .groups = "keep" ) %>% 
  dplyr::ungroup(period) 

# weighted total system (average) shadow price
df.sp.capcon.sys <- list(prod_share, sp.capcon) %>% 
  reduce(full_join) %>% 
  replace(is.na(.), 0) %>% 
  mutate_all(function(x) ifelse(is.infinite(x), 0, x)) %>% 
  select(period, genshare, value) %>% 
  mutate(value = genshare * value) %>% 
  dplyr::group_by(period) %>%
  dplyr::summarise( value = sum(value), .groups = "keep" ) %>% 
  dplyr::ungroup(period) 


########################################################################################################
########################################################################################################
swlatex(sw,"\\onecolumn")

# ########################################################################################################
swlatex(sw, paste0("\\section{Technology LCOEs}"))
swlatex(sw, paste0("\\subsection{Technology LCOE - DIETER}"))

#DIETER's marginal LCOE components for each technology, market value, shadow price, etc (marginal in the sense of one additional MWh added to the year by the marginal plant (corresponding to capfac of the highest level empty grade for VRE))

cost_bkdw_avg <- file.path(outputdir, dieter.files.report[iter_toplot]) %>%
    read.gdx("report_tech", factors = FALSE, squeeze=FALSE) %>%
    select(model=X..1, period = X..2, variable=X..4, tech=X..5, value) %>%
    filter(variable %in% reportLCOEcomponents_DT_avg) %>%
    filter(period %in% model.periods) %>%
    filter(tech %in% names(dieter.tech.mapping)) %>%
    filter(model == "DIETER")%>%
    revalue.levels(tech = dieter.tech.mapping) %>%
    mutate(tech = factor(tech, levels=rev(unique(dieter.tech.mapping)))) %>%
    revalue.levels(variable = dieter.variable.mapping) %>%
    mutate(variable = factor(variable, levels=rev(unique(dieter.variable.mapping)))) %>%
    select(period, tech, variable, value)%>% 
    mutate(period = as.numeric(period))

# cost for REMIND but reported in DIETER (this should be properly reported in remind2/reportLCOE in the future)
cost_bkdw_avg_4RM <- file.path(outputdir, dieter.files.report[iter_toplot]) %>%
  read.gdx("report_tech", factors = FALSE, squeeze=FALSE) %>%
  select(model=X..1, period = X..2, variable=X..4, tech=X..5, value) %>%
  filter(variable %in% reportLCOEcomponents_REMIND_avg) %>%
  filter(period %in% model.periods) %>%
  filter(tech %in% names(dieter.tech.mapping)) %>%
  filter(model == "REMIND")%>%
  revalue.levels(tech = dieter.tech.mapping) %>%
  mutate(tech = factor(tech, levels=rev(unique(dieter.tech.mapping)))) %>%
  revalue.levels(variable = dieter.variable.mapping) %>%
  mutate(variable = factor(variable, levels=rev(unique(dieter.variable.mapping)))) %>%
  select(period, tech, variable, value)%>% 
  mutate(period = as.numeric(period))

if (h2switch == "off"){
    cost_bkdw_avg <- cost_bkdw_avg %>%
      filter(!tech %in% c(remind.sector.coupling.mapping))
  }

cost_bkdw_marg <- file.path(outputdir, dieter.files.report[iter_toplot]) %>%
  read.gdx("report_tech", factors = FALSE, squeeze=FALSE) %>%
  select(model=X..1, period = X..2, variable=X..4, tech=X..5, value) %>%
  filter(variable %in% reportLCOEcomponents_DT_marg) %>%
  filter(period %in% model.periods) %>%
  filter(tech %in% names(dieter.tech.mapping)) %>%
  filter(model == "DIETER")%>%
  revalue.levels(tech = dieter.tech.mapping) %>%
  mutate(tech = factor(tech, levels=rev(unique(dieter.tech.mapping)))) %>%
  revalue.levels(variable = dieter.variable.mapping) %>%
  mutate(variable = factor(variable, levels=rev(unique(dieter.variable.mapping)))) %>%
  select(period, tech, variable, value) %>% 
  mutate(period = as.numeric(period))

if (h2switch == "off"){
  cost_bkdw_marg <- cost_bkdw_marg %>%
    filter(!tech %in% c(remind.sector.coupling.mapping))
}


# cost for REMIND but reported in DIETER (this should be properly reported in remind2/reportLCOE in the future)
cost_bkdw_marg_4RM <- file.path(outputdir, dieter.files.report[iter_toplot]) %>%
  read.gdx("report_tech", factors = FALSE, squeeze=FALSE) %>%
  select(model=X..1, period = X..2, variable=X..4, tech=X..5, value) %>%
  filter(variable %in% reportLCOEcomponents_REMIND_marg) %>%
  filter(period %in% model.periods) %>%
  filter(tech %in% names(dieter.tech.mapping)) %>%
  filter(model == "REMIND")%>%
  revalue.levels(tech = dieter.tech.mapping) %>%
  mutate(tech = factor(tech, levels=rev(unique(dieter.tech.mapping)))) %>%
  revalue.levels(variable = dieter.variable.mapping) %>%
  mutate(variable = factor(variable, levels=rev(unique(dieter.variable.mapping)))) %>%
  select(period, tech, variable, value)%>% 
  mutate(period = as.numeric(period))

cost_bkdw_marg_DT <- cost_bkdw_marg %>%
  mutate(variable = factor(variable, levels=rev(unique(dieter.variable.mapping))))

cost_bkdw_avg_DT <- cost_bkdw_avg %>%
  mutate(variable = factor(variable, levels=rev(unique(dieter.variable.mapping))))

# grid cost (diff from but equivalent to VRE-attributed cost)
gridcost <- cost_bkdw_avg_DT %>%
  filter(tech == "VRE grid") %>%
  select(period,variable,value)

# REMIND marginal adjustment cost (note: only approx, since this is reported in DIETER and uses marginal capfac in dieter, so it is the same as DIETER adjustment cost)
adjcost_marg <- cost_bkdw_marg_4RM %>%
  filter(variable == "Adjustment Cost") %>%
  filter(!tech == "VRE grid") %>%
  select(period,tech,cost=variable,value)

if (h2switch == "off"){
  adjcost_marg <- adjcost_marg %>%
    filter(!tech %in% c(remind.sector.coupling.mapping))
}

# only look at MV with scarcity hour revenue
dieter.price <- cost_bkdw_avg_DT %>%
  filter(variable %in% label.price) %>%
  mutate(value = -value) %>% 
  filter(!variable == "Market Value")

################## DIETER average LCOE plot ########################

dieter.telcoe_avg <- cost_bkdw_avg_DT %>%
  filter(!variable %in% label.price)%>%
  filter(period %in% model.periods.till2100)%>%
  filter(!tech == "VRE grid")

dieter.teloceprice_avg <- list(dieter.price, dieter.telcoe_avg) %>%
  reduce(full_join) %>% 
  filter(period %in% model.periods.till2100) %>%
  mutate(period = as.numeric(period))%>%
  filter(!tech == "VRE grid")

# 2020 has very high LCOE due to shadow price for biomass and OCGT, exclude from plotting
p <- ggplot() +
  geom_bar(data = dieter.teloceprice_avg %>% filter(period >2020) , aes(x = period, y = value,  fill = variable), stat='identity', size = 1.2, alpha = 0.5)+
  labs(color = "LCOE") +
  labs(linetype = "") +
  labs(fill = "") +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20, face="bold"), strip.text = element_text(size = 20)) +
  theme(legend.text = element_text(size=20), strip.text = element_text(size = 20))+
  xlab("year") + ylab(paste0("LCOE ($/MWh)")) +
  scale_fill_manual(values = cost.colors_DT) +
  # coord_cartesian(ylim = c(-150,350))+
  facet_wrap(~tech, nrow = 3, scales = "free")

swfigure(sw,print,p)

if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/teLCOE_avg_DIETER_bar.png"),  p,  width = 20, height =10, units = "in", dpi = 120)
}
################## DIETER average LCOE plot ########################
plot.dieter.telcoe_avg <- dieter.telcoe_avg %>%
  mutate(period = as.numeric(period)) %>%
  filter(period %in% model.periods.till2100) %>%
  filter(!tech == "VRE grid")
# 
plot.dieter.price0 <- dieter.price %>%
  mutate(value = - value) %>%
  mutate(period = as.numeric(period)) %>%
  filter(period %in% model.periods.till2100) %>%
  filter(!tech == "VRE grid")

# fill non existing values with 0s by spreading then gathering
plot.dieter.price.spread <- spread(plot.dieter.price0, period,value) %>% 
  replace(is.na(.), 0) 

plot.dieter.price <- plot.dieter.price.spread %>% 
  gather(period,value,-tech,-variable)%>%
  mutate(period = as.numeric(period))

plot.dieter.price.total <- plot.dieter.price %>% 
  dplyr::group_by(tech,period) %>%
  dplyr::summarise( value = sum(value), .groups = "keep" ) %>% 
  dplyr::ungroup(tech,period) %>% 
  mutate(variable = "Market Value with scarcity price + shadow price")

# 2020 has very high LCOE due to shadow price for biomass and OCGT, exclude from plotting
p <- ggplot() +
  geom_bar(data = plot.dieter.telcoe_avg %>% filter(period >2020), aes(x = period, y = value, fill = variable), stat='identity', size = 1.2, alpha = 0.5)+
  geom_line(data = plot.dieter.price %>% filter(period >2020, variable !="Shadow Price"), aes(period, value, linetype=variable), size=1.2) +
  geom_line(data = plot.dieter.price.total%>% filter(period >2020), aes(period, value, linetype=variable), size=1.2) +
  labs(linetype = "") +
  labs(fill = "") +
  theme(legend.text = element_text(size=20), strip.text = element_text(size = 20))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20, face="bold"), strip.text = element_text(size = 20)) +
  xlab("year") + ylab(paste0("LCOE ($/MWh)"))  +
  scale_fill_manual(values = cost.colors_DT) +
  facet_wrap(~tech, nrow = 3, scales = "free")

swfigure(sw,print,p)

if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/teLCOE_avg_DIETER_line.png"),  p,  width = 20, height =10, units = "in", dpi = 120)
}

################## DIETER marginal LCOE plot ########################
dieter.telcoe_marg <- cost_bkdw_marg_DT %>%
  filter(!variable %in% label.price)%>%
  filter(period %in% model.periods.till2100)%>% 
  filter(!tech == "VRE grid")

dieter.teloceprice_marg <- list(dieter.price, dieter.telcoe_marg) %>%
  reduce(full_join) %>%
  filter(period %in% model.periods.till2100) %>%
  mutate(period = as.numeric(period))%>% 
  filter(!tech == "VRE grid")

# 2020 has very high LCOE due to shadow price for biomass and OCGT, exclude from plotting
p <- ggplot() +
  geom_bar(data = dieter.teloceprice_marg %>% filter(period >2020) , aes(x = period, y = value,  fill = variable), stat='identity', size = 1.2, alpha = 0.5) +
  labs(linetype = "") +
  labs(fill = "") +
  theme(legend.text = element_text(size=20), strip.text = element_text(size = 20)) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20, face="bold"), strip.text = element_text(size = 20)) +
  xlab("year") + ylab(paste0("LCOE ($/MWh)")) +
  scale_fill_manual(values = cost.colors_DT) +
  facet_wrap(~tech, nrow = 3, scales = "free")

swfigure(sw,print,p)

if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/teLCOE_marg_DIETER_bar.png"),  p,  width = 20, height =10, units = "in", dpi = 120)
}

plot.dieter.telcoe_marg <- dieter.telcoe_marg %>%
  mutate(period = as.numeric(period)) %>%
  filter(period %in% model.periods.till2100)
# 2020 has very high LCOE due to shadow price for biomass and OCGT, exclude from plotting
p <- ggplot() +
  geom_bar(data = plot.dieter.telcoe_marg %>% filter(period >2020) , aes(x = period, y = value,  fill = variable), stat='identity', size = 1.2, alpha = 0.5)+
  geom_line(data = plot.dieter.price %>% filter(period >2020, variable !="Shadow Price"), aes(period, value, linetype=variable), size=1.2) +
  geom_line(data = plot.dieter.price.total%>% filter(period >2020), aes(period, value, linetype=variable), size=1.2) +
  labs(linetype = "") +
  labs(fill = "") +
  theme(legend.text = element_text(size=20), strip.text = element_text(size = 20)) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20, face="bold"), strip.text = element_text(size = 20)) +
  xlab("year") + ylab(paste0("LCOE ($/MWh)"))  +
  scale_fill_manual(values = cost.colors_DT) +
  facet_wrap(~tech, nrow = 3, scales = "free")

swfigure(sw,print,p)

if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/teLCOE_marg_DIETER_line.png"),  p,  width = 20, height =10, units = "in", dpi = 120)
}

########################################################################################################
swlatex(sw, paste0("\\subsection{Technology LCOE - REMIND}"))


# 2020 has very high LCOE for biomass and OCGT, exclude from plotting
p.teLCOE <- ggplot() +
  geom_col( data = df.lcoe.te.plot %>% filter(period %in% model.periods.till2100, period >2020),
            aes(period, value, fill=cost)) +
  geom_line(data = df.telcoe_mv.plot %>% filter(period >2020),
            aes(period, value, linetype=cost), size=1.2) +
  facet_wrap(~tech, scales = "free_y") +
  scale_y_continuous("LCOE and REMIND Price\n(USD2015/MWh)") +
  scale_x_continuous(breaks = seq(2020,2100,10)) +
  scale_fill_manual(values = cost.colors.te) +
  # coord_cartesian(ylim = c(ymin,ymax))+
  theme_bw() +
  theme( axis.text.x = element_text(angle = 90),
         strip.background = element_blank())

swfigure(sw,print,p.teLCOE)

if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/teLCOE_REMIND.png"),  p.teLCOE,  width = 17, height =7, units = "in", dpi = 120)
}


########################################################################################################
swlatex(sw, paste0("\\subsection{Technology LCOE (DIETER average LCOE, REMIND marginal LCOE) - Comparison}"))

df.lcoe.avg.dieter <- cost_bkdw_avg_DT %>% 
  filter(!variable %in% label.price) %>% 
  mutate(period = as.numeric(period)) %>% 
  filter(period %in% model.periods.till2100)

barwidth = 1.5
df.lcoe.teAgg.wAdj <- list(df.lcoe.teAgg, adjcost_marg) %>%
  reduce(full_join) %>% 
  filter(period %in% model.periods.till2100) %>% 
  mutate(cost = factor(cost, levels=rev(unique(c(dieter.variable.mapping,"Curtailment Cost")))))
  
p.techLCOE_compare<-ggplot() +
  geom_col(data = df.lcoe.teAgg.wAdj %>% filter(period >2020), aes(x = period-barwidth/2-0.1, y = value, fill = cost), colour="black", position='stack', size = 1, width = barwidth) +
  geom_col(data = df.lcoe.avg.dieter %>% 
             filter(!tech == "VRE grid", period >2020), aes(x = period+barwidth/2+0.1, y = value, fill = variable), colour="black", position='stack', size = 1,
           width = barwidth) +
  scale_alpha_discrete(range = c(0.4,1)) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size= 20, face="bold"),strip.text = element_text(size=25),plot.title = element_text(size = 30, face = "bold")) +
  xlab("year") + ylab(paste0("LCOE ($/MWh)")) +
  ggtitle("Tech LCOE comparison (last iteration) - left REMIND (marginal), right DIETER (average)")+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(size=20)) +
  theme(aspect.ratio = .5) +
  scale_fill_manual(name = "model", values =cost.colors)+
  facet_wrap(~tech, nrow = 3, scales = "free") 

swfigure(sw,print,p.techLCOE_compare)

if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/teLCOE_avg_compare.png"),  p.techLCOE_compare,  width = 25, height =15, units = "in", dpi = 120)
}
########################################################################################################
swlatex(sw, paste0("\\subsection{Technology LCOE (DIETER marginal LCOE, REMIND marginal LCOE) - Comparison}"))
df.lcoe.marg.dieter <- cost_bkdw_marg_DT %>% 
  filter(!variable %in% label.price) %>% 
  mutate(period = as.numeric(period))

barwidth = 1.5

p.techmargLCOE_compare <-ggplot() +
  geom_col(data = df.lcoe.teAgg.wAdj %>% 
             filter(period %in% model.periods.till2100, period > 2020), 
           aes(x = period-barwidth/2-0.1, y = value, fill = cost), colour="black", position='stack', size = 1, width = barwidth) +
  geom_col(data = df.lcoe.marg.dieter %>% 
             filter(!tech == "VRE grid") %>% 
             filter(period %in% model.periods.till2100, period > 2020), 
           aes(x = period+barwidth/2+0.1, y = value, fill = variable), colour="black", position='stack', size = 1, width = barwidth) +
  scale_alpha_discrete(range = c(0.4,1)) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size= 20, face="bold"),strip.text = element_text(size=25),plot.title = element_text(size = 30, face = "bold")) +
  xlab("year") + ylab(paste0("LCOE ($/MWh)")) +
  ggtitle("Tech LCOE comparison (last iteration) - left REMIND (marginal), right DIETER (marginal)")+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(size=20)) +
  theme(aspect.ratio = .5) +
  scale_fill_manual(name = "model", values =cost.colors)+
  facet_wrap(~tech, nrow = 3, scales = "free") 

swfigure(sw,print,p.techmargLCOE_compare)

if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/teLCOE_marg_compare.png"),  p.techmargLCOE_compare,  width = 25, height =15, units = "in", dpi = 120)
}

swlatex(sw, paste0("\\section{System LCOEs}"))
########################################################################################################
swlatex(sw, paste0("\\subsection{System LCOE - REMIND - with curtailment cost}"))
# REMIND marginal LCOE component for the entire power system (marginal in the sense of one additional added unit of generation in the system)

df.lcoe.components <- list(df.lcoe.elh2.components,df.lcoe.sys.components,df.markup.sys) %>%
  reduce(full_join)

df.lcoe.components.nocurt <- df.lcoe.components %>%
  filter(!cost =="Curtailment Cost")

df.lcoe_minus_tax.plot <- list(df.lcoe.components, df.markup.sys, flexadj) %>%
  reduce(full_join)%>%
  dplyr::group_by(period,tech,sector) %>%
  dplyr::summarise( value = sum(value), .groups = "keep" ) %>%
  dplyr::ungroup(period,tech,sector) %>%
  mutate(variable = "Total (marginal) LCOE - Flex Tax")

df.lcoe_minus_tax.plot.nocurt <- list(df.lcoe.components.nocurt, df.markup.sys, flexadj) %>%
  reduce(full_join)%>%
  dplyr::group_by(period,tech,sector) %>%
  dplyr::summarise( value = sum(value), .groups = "keep" ) %>%
  dplyr::ungroup(period,tech,sector) %>%
  mutate(variable = "Total (marginal) LCOE - Flex Tax")

df.pricelcoe_minus_tax.plot <- list(df.lcoe_minus_tax.plot,df.price) %>%
  reduce(full_join)

df.pricelcoe_minus_tax.plot.nocurt <- list(df.lcoe_minus_tax.plot.nocurt,df.price) %>%
  reduce(full_join)

df.lcoe.components.nocurt.plot <- list(df.lcoe.components.nocurt, flexadj) %>%
  reduce(full_join)

df.lcoe.components.plot <- list(df.lcoe.components, flexadj) %>%
  reduce(full_join)

if (h2switch == "off"){
  df.lcoe.components.plot <- df.lcoe.components.plot %>%
    filter(!tech %in% remind.sector.coupling.mapping)
  df.lcoe.components.nocurt.plot <- df.lcoe.components.nocurt.plot %>%
    filter(!tech %in% remind.sector.coupling.mapping)
  df.pricelcoe_minus_tax.plot <- df.pricelcoe_minus_tax.plot %>%
    filter(!tech %in% remind.sector.coupling.mapping)
  df.pricelcoe_minus_tax.plot.nocurt <- df.pricelcoe_minus_tax.plot.nocurt %>%
    filter(!tech %in% remind.sector.coupling.mapping)
}

ymax = max(df.pricelcoe_minus_tax.plot$value) * 1.1
ymin = min(df.lcoe.components.plot$value) * 1.1

# capfac being divided by IC in LCOE routine is pre-curtailment capfac, so curtailment cost is prob still needed
p.sysLCOE_wmarkup <- ggplot() +
  geom_col( data = df.lcoe.components.plot,
            aes(period, value, fill=cost)) +
  geom_line(data = df.pricelcoe_minus_tax.plot %>% filter(period %in% model.periods.till2100),
            aes(period, value, linetype=variable), size=1.2) +
  facet_wrap(~tech~sector, scales = "free_y") +
  scale_y_continuous("LCOE and REMIND Price\n(USD2015/MWh)") +
  scale_x_continuous(breaks = seq(2010,2100,10)) +
  scale_fill_manual(values = cost.colors.sys.wh2) +
  coord_cartesian(ylim = c(ymin,ymax))+
  theme_bw() +
  ggtitle(paste0("System LCOE for 1 MWh of generated power: ", reg))+
  theme(plot.title = element_text(size = 16, face = "bold"))+
  theme(aspect.ratio = 1) +
  theme( axis.text.x = element_text(angle = 90),
         strip.background = element_blank())

swfigure(sw,print,p.sysLCOE_wmarkup)

if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/sysLCOE_w_markup_REMIND.png"),  p.sysLCOE_wmarkup,  width = 17, height =7, units = "in", dpi = 120)
}
########################################################################################################
swlatex(sw, paste0("\\subsection{System LCOE - REMIND - without curtailment cost}"))

ymax = max(df.pricelcoe_minus_tax.plot.nocurt$value) * 1.1
ymin = min(df.lcoe.components.nocurt.plot$value) * 1.1

p.sysLCOE_wmarkup <- ggplot() +
  geom_col( data = df.lcoe.components.nocurt.plot,
            aes(period, value, fill=cost)) +
  geom_line(data = df.pricelcoe_minus_tax.plot.nocurt %>% filter(period %in% model.periods.till2100),
            aes(period, value, linetype=variable), size=1.2) +
  facet_wrap(~tech~sector, scales = "free_y") +
  scale_y_continuous("LCOE and REMIND Price\n(USD2015/MWh)") +
  scale_x_continuous(breaks = seq(2010,2100,10)) +
  scale_fill_manual(values = cost.colors.sys.wh2) +
  coord_cartesian(ylim = c(ymin,ymax))+
  theme_bw() +
  ggtitle(paste0("System LCOE for 1 MWh of usable power: ", reg))+
  theme(plot.title = element_text(size = 16, face = "bold"))+
  theme(aspect.ratio = 1) +
  theme( axis.text.x = element_text(angle = 90),
         strip.background = element_blank())


swfigure(sw,print,p.sysLCOE_wmarkup)

if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/sysLCOE_w_markup_REMIND_nocurt.png"),  p.sysLCOE_wmarkup,  width = 17, height =7, units = "in", dpi = 120)
}

########################################################################################################
swlatex(sw, paste0("\\subsection{Marginal and average DIETER system LCOE vs. marginal REMIND system LCOE}"))
#DIETER's marginal and average system LCOE and price, compared with REMIND side-by-side

prices_DT <- file.path(outputdir, dieter.files.report[iter_toplot]) %>% 
  read.gdx("report", squeeze=F) %>% 
  select(model=X..1, period = X..2, variable=X..4, value) %>%
  filter(variable %in% report_DT_prices) %>% 
  filter(period %in% model.periods) %>% 
  filter(model == "DIETER")%>% 
  revalue.levels(variable = dieter.variable.mapping) %>%
  mutate(variable = factor(variable, levels=rev(unique(dieter.variable.mapping)))) %>% 
  mutate(period = as.numeric(period)) %>% 
  select(model,period, variable, value)

shadow_prices_DT <- prices_DT %>% 
  filter(variable == 'DIETER shadow price due to capacity constraint from REMIND') %>% 
  select(period, shad=value)

elec_prices_DT <- prices_DT %>% 
  filter(variable == "DIETER annual average electricity price with scarcity price") 

elec_prices_DT_wShadPrice <- elec_prices_DT %>% 
  left_join(shadow_prices_DT) %>% 
  mutate(value = value + shad) %>% 
  mutate(variable = "DIETER annual average electricity price with scarcity price + shadow price from REMIND")

prices_RM <- df.pricelcoe_minus_tax.plot %>%
# prices_RM <- df.pricelcoe_minus_tax.plot.nocurt %>%
  filter(tech == "System") %>%
  filter(variable %in% c("REMIND Price", "Total (marginal) LCOE - Flex Tax")) %>%
  select(period, variable, value)%>%
  mutate(model = "REMIND")

# REMIND price with capacity shadow price
prices_wShad_RM <- prices_RM %>%
  filter(variable %in% c("REMIND Price")) %>% 
  select(period,price =value) %>% 
  full_join(df.sp.sys) %>% 
  replace(is.na(.), 0) %>% 
  mutate(value = value +price) %>% 
  mutate(variable = "REMIND price + shadow price (historical bound on capacities)")

prices_w2Shad_RM <- prices_wShad_RM %>%
  select(period,price=value) %>% 
  full_join(df.sp.capcon.sys) %>% 
  replace(is.na(.), 0) %>% 
  mutate(value = value + price) %>% 
  mutate(variable = "REMIND price + shadow price (historical and peak load bound on cap.)")%>%
  mutate(model = "REMIND")

prices_RM.movingavg <- df.price0 %>%
  filter(tech == "System") %>% 
  select(period, variable, value)%>%
  mutate(model = "REMIND") %>% 
  mutate(value = frollmean(value, 6, align = "center", fill = NA)) %>% 
  mutate(variable = "REMIND price moving average")

prices_lines <- list(elec_prices_DT_wShadPrice, elec_prices_DT, prices_RM) %>%
  reduce(full_join)

## filter out the electricity price in DIETER which has scarcity price shaved off, since it does
# not match well with LCOE
prices_bar <- prices_DT %>% 
  filter(!variable == 'DIETER annual average electricity price') %>%
  mutate(value = -value)

genshare.dieter <- file.path(outputdir, dieter.files.report[iter_toplot]) %>% 
  read.gdx("report_tech", squeeze=F) %>% 
  select(model=X..1, period = X..2, variable=X..4, tech=X..5, value) %>%
  filter(variable %in% report_DT_genshare) %>% 
  filter(period %in% model.periods) %>% 
  filter(tech %in% names(dieter.tech.mapping)) %>% 
  filter(model == "DIETER")%>% 
  revalue.levels(tech = dieter.tech.mapping) %>%
  mutate(tech = factor(tech, levels=rev(unique(dieter.tech.mapping)))) %>% 
  revalue.levels(variable = dieter.variable.mapping) %>%
  mutate(variable = factor(variable, levels=rev(unique(dieter.variable.mapping)))) %>% 
  select(period, tech, variable, value) %>% 
  mutate(period = as.numeric(period))

genshare1 <- genshare.dieter %>%
  select(period, tech, genshare=value) %>%
  mutate(genshare = genshare/100)

gridcost_p <- gridcost %>% 
  filter(variable == "Shadow Cost")

sysLCOE_avg_DT <- dieter.telcoe_avg %>% 
  select(period,tech,variable,value) %>% 
  filter(!tech %in% c("VRE grid", "Electrolyzers")) %>% 
  left_join(genshare1) %>% 
  mutate(value = value * genshare) %>% 
  select(period,tech,variable,value) %>% 
  full_join(gridcost_p) %>%
  replace(is.na(.), 0) %>% 
  dplyr::group_by(period,variable) %>%
  dplyr::summarise( value = sum(value), .groups = "keep" ) %>% 
  dplyr::ungroup(period,variable) %>% 
  mutate(variable = factor(variable, levels=rev(unique(dieter.variable.mapping)))) %>% 
  mutate(period = as.numeric(period)) 

sysLCOE_avg_DT$type <- "Average"
sysLCOE_avg_DT$model <- "DIETER"

AdjCost <- sysLCOE_avg_DT

sysLCOE_marg_RM <- df.lcoe.components %>%
# sysLCOE_marg_RM <- df.lcoe.components.nocurt %>%
  filter(tech == "System") %>%
  select(!variable) %>%
  select(period, variable= cost, value) %>%
  mutate(model = "REMIND")

sys_avgLCOE_compare <- list(sysLCOE_avg_DT, sysLCOE_marg_RM) %>% 
  reduce(full_join)  

ymax = max(prices_lines$value) * 1.1
ymin = min(sys_avgLCOE_compare$value) * 1.1

p.sysLCOE_compare <- ggplot() + 
  geom_col( data = sys_avgLCOE_compare, 
            aes(period, value, fill=variable)) +
  geom_line(data = prices_lines %>% filter(period %in% model.periods.till2100) ,
            aes(period, value, color=variable), size=1.2) +  
  geom_line(data = prices_RM.movingavg %>% filter(period %in% model.periods.till2100) ,
            aes(period, value, color=variable), alpha = 0.5, size=2) +  
  geom_line(data = prices_w2Shad_RM %>% filter(period %in% model.periods.till2100) ,
            aes(period, value, color=variable), size=1.2) +   
  # geom_polygon(data = prices_lines, aes(x = period, y = value), alpha = 0.3) +
  facet_wrap(~model, scales = "free_y") +
  scale_y_continuous("LCOE and power price\n(USD2015/MWh)") +
  scale_x_continuous(breaks = seq(2010,2100,10)) +
  scale_color_manual(name = "variable", values = price.colors) +
  coord_cartesian(ylim = c(ymin,ymax))+
  scale_fill_manual(values = cost.colors.sys) +
  theme_bw() +
  theme( axis.text.x = element_text(angle = 90),
         strip.background = element_blank())

swfigure(sw,print,p.sysLCOE_compare)

if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/avgLCOE_price_compare_line.png"),  p.sysLCOE_compare,  width = 17, height =7, units = "in", dpi = 120)
}


DIETER_bar_avg <- list(prices_bar, sysLCOE_avg_DT) %>%
  reduce(full_join) 

p.sysLCOEprice_DIETER <- ggplot() + 
  geom_col( data = DIETER_bar_avg %>% filter(period %in% model.periods.till2100) ,
            aes(period, value, fill=variable)) +
  facet_wrap(~model, scales = "free_y") +
  scale_y_continuous("LCOE and DIETER Price\n(USD2015/MWh)") +
  scale_x_continuous(breaks = seq(2010,2100,10)) +
  scale_color_manual(name = "variable", values = price.colors) +
  # coord_cartesian(ylim = c(-115,115))+
  scale_fill_manual(values = cost.colors.dieter) +
  theme_bw() +
  theme( axis.text.x = element_text(angle = 90),
         strip.background = element_blank())

swfigure(sw,print,p.sysLCOEprice_DIETER)

if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/avgLCOE_price_compare_bar.png"),  p.sysLCOEprice_DIETER,  width = 10, height =7, units = "in", dpi = 120)
}



# DIETER system marginal LCOE
# 
# sysLCOE_marg_DT <- dieter.telcoe_marg %>%
#   select(period,tech,variable,value) %>%
#   filter(!tech %in% c("VRE grid", "Electrolyzers")) %>%
#   left_join(genshare1) %>%
#   mutate(value = value * genshare) %>%
#   select(period,tech,variable,value) %>%
#   full_join(gridcost_p) %>%
#   replace(is.na(.), 0) %>%
#   dplyr::group_by(period,variable) %>%
#   dplyr::summarise( value = sum(value), .groups = "keep" ) %>%
#   dplyr::ungroup(period,variable) %>%
#   mutate(variable = factor(variable, levels=rev(unique(dieter.variable.mapping)))) %>% 
#   mutate(period = as.numeric(period))
# 
# sysLCOE_marg_DT$type <- "Marginal"
# sysLCOE_marg_DT$model <- "DIETER"
# 
# sys_margLCOE_compare <- list(sysLCOE_marg_DT, sysLCOE_marg_RM) %>%
#   reduce(full_join)
# 
# p <- ggplot() +
#   geom_col( data = sys_margLCOE_compare,
#             aes(period, value, fill=variable)) +
#   geom_line(data = prices_lines %>% filter(period %in% model.periods.till2100) ,
#             aes(period, value, color=variable), size=1.2) +
#   facet_wrap(~model, scales = "free_y") +
#   scale_y_continuous("LCOE and DIETER Price\n(USD2015/MWh)") +
#   scale_x_continuous(breaks = seq(2010,2100,10)) +
#   scale_color_manual(name = "variable", values = price.colors) +
#   coord_cartesian(ylim = c(-5,115))+
#   scale_fill_manual(values = cost.colors.sys) +
#   theme_bw() +
#   theme( axis.text.x = element_text(angle = 90),
#          strip.background = element_blank())
# 
# swfigure(sw,print,p)
# 
# if (save_png == 1){
#   ggsave(filename = paste0(outputdir, "/DIETER/margLCOE_price_compare_line.png"),  p,  width = 17, height =7, units = "in", dpi = 120)
# }

# 
# DIETER_bar_marg <- list(prices_bar, sysLCOE_marg_DT) %>%
#   reduce(full_join) 
# 
# p.sysLCOEprice_DIETER <- ggplot() + 
#   geom_col( data = DIETER_bar_marg, 
#             aes(period, value, fill=variable)) +
#   facet_wrap(~model, scales = "free_y") +
#   scale_y_continuous("LCOE and DIETER Price\n(USD2015/MWh)") +
#   scale_x_continuous(breaks = seq(2010,2100,10)) +
#   scale_color_manual(name = "variable", values = price.colors) +
#   # coord_cartesian(ylim = c(-115,115))+
#   scale_fill_manual(values = cost.colors.dieter) +
#   theme_bw() +
#   theme( axis.text.x = element_text(angle = 90),
#          strip.background = element_blank())
# 
# swfigure(sw,print,p)
# 
# if (save_png == 1){
#   ggsave(filename = paste0(outputdir, "/DIETER/margLCOE_price_compare_bar.png"),  p,  width = 17, height =7, units = "in", dpi = 120)
# }
########################################################################################################
swlatex(sw, paste0("\\subsectionDIETER's VRE total LCOEs compared to fossil fuel plants running cost}"))

dieter.telcoe_avg_ffr <- dieter.telcoe_avg %>% 
    filter(variable %in% running_lcoe_components) %>% 
    filter(tech %in% conventionals) %>% 
    mutate(tech = factor(tech, ordered=TRUE))

dieter.telcoe_avg_vre <- dieter.telcoe_avg %>% 
    filter(tech %in% renewables) %>% 
    mutate(tech = factor(tech, ordered=TRUE))

dieter.telcoe_marg_ffr <- dieter.telcoe_marg %>% 
    filter(variable %in% running_lcoe_components) %>% 
    filter(tech %in% conventionals)

### avg LCOE
p.techLCOE_compare<-ggplot() +
  geom_col(data = dieter.telcoe_avg_vre %>% filter(period %in% model.periods.till2100), aes(x = tech, y = value, fill = variable), colour="black", position='stack', size = 1) +
  geom_col(data = dieter.telcoe_avg_ffr %>% filter(period %in% model.periods.till2100), aes(x = tech, y = value, fill = variable), colour="black", position='stack', size = 1) +
  scale_alpha_discrete(range = c(0.4,1)) +
  theme(axis.text=element_text(size=15), axis.title=element_text(size= 18, face="bold"),strip.text = element_text(size=25),plot.title = element_text(size = 30, face = "bold")) +
  xlab("") + ylab(paste0("LCOE ($/MWh)")) +
  ggtitle("Tech average LCOE DIETER (last iteration) - left: running cost of conventionals, right: total cost of VRE")+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(size=20)) +
  theme(aspect.ratio = .4) +
  scale_fill_manual(name = "model", values =cost.colors_DT_running)+
  facet_wrap(~period, nrow = 4, scales = "free") 

swfigure(sw,print,p.techLCOE_compare)

if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/teLCOE_avg_ffrunningVRE_compare.png"),  p.techLCOE_compare,  width = 30, height =18, units = "in", dpi = 120)
}

