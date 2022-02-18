# Data preparation --------------------------------------------------------

cat("Plot LCOEs \n")
### read mif

plot.tech <- c("elh2","system")
plot.sector <- c("supply-side")
iter_toplot = maxiter - 1

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
prod_share <- file.path(outputdir, remind.files[maxiter-1]) %>% 
    read.gdx("v32_shSeElDisp", squeeze = F)  %>% 
    filter(all_regi == reg) %>% 
    filter(all_te %in% names(remind.tech.mapping)) %>% 
    select(period=ttot, tech=all_te, genshare = value) %>% 
    filter(period %in% model.periods.till2100) %>% 
    mutate(genshare = genshare/1e2)
  
mv <- file.path(outputdir, remind.files[maxiter-1]) %>% 
  read.gdx("p32_marketValue", squeeze=F)  %>% 
  filter(all_regi == reg) %>%
  select(period = ttot, tech = all_te, value) %>%
  filter(period %in% model.periods.till2100) %>% 
  mutate(cost = "Market value") %>% 
  mutate(value = value * 1e12 / sm_TWa_2_MWh * 1.2)

mrkup <- out.remind.mrkup %>% 
  mutate(cost = "Markup subsidy/tax") %>% 
  filter(period %in% model.periods.till2100) %>% 
  filter(iteration == maxiter-1) %>% 
  select(-iteration)

mv.agg <- out.remind.mv %>% 
  mutate(cost = "Market value") %>% 
  filter(period %in% model.periods.till2100) %>% 
  filter(iteration == maxiter-1) %>% 
  select(-iteration)

df.markup.sys <- out.remind.sys.mrkup %>% 
  filter(iteration == maxiter-1) %>% 
  select(-iteration) %>% 
  filter(period %in% model.periods.till2100) %>% 
  mutate(tech = "System") %>% 
  mutate(sector = "supply-side") %>%  
  mutate(cost = "Total Markup")

flexadj <- out.remind.flexadj %>% 
  filter(iteration == maxiter-1) %>% 
  select(-iteration) %>% 
  filter(period %in% model.periods.till2100) %>% 
  mutate(cost = "flexibility subsidy") %>% 
  mutate(sector = "supply-side") 

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
  filter( output %in% c("seel","seh2")) %>% 
  replace(is.na(.), 0) %>% 
  select(period, tech, cost, lcoe=value) %>% 
  full_join(df.lcoe.grid)

df.lcoe.te.components[mapply(is.infinite, df.lcoe.te.components)] <- 0

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

### electrolysers LCOE
df.lcoe.elh2.components <- df.lcoe %>% 
  # filter(output == "seh2",region == reg, tech == "elh2")
  filter(region == reg, 
         period %in% model.periods, type == "marginal",
         tech %in% plot.tech, sector %in% plot.sector,
         value != 0) %>% 
  filter( ! cost %in% c("Total LCOE")) %>% 
  order.levels(tech = plot.tech, cost = names(cost.colors.te)) %>% 
  revalue.levels(tech = tech.label) %>% 
  select(period, tech, cost, sector,value)

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
  filter(region == reg,variable %in% map.price.tech$variable) %>% 
  # convert from USD2005/GJ to USD2015/MWh
  mutate( value = value * 1.2 * 3.66) %>% 
  left_join(map.price.tech) %>% 
  select(-unit, -variable) %>% 
  mutate( variable = "REMIND Price") %>% 
  filter(tech %in% plot.tech) %>% 
  revalue.levels(tech = tech.label) %>% 
  select(period, tech, variable, sector,value)

df.price <- df.price0 %>% 
  filter(period %in% model.periods)
  
df.lcoe.te.components.plot <- df.lcoe.te.components %>% 
  dplyr::rename( value = lcoe )

df.lcoe.te.plot <- df.lcoe.te.components.plot

if (h2switch == "off"){
  df.lcoe.te.plot <- df.lcoe.te.plot %>% 
    filter(!tech =="elh2")
}


df.lcoe_minus_markup.te <- df.lcoe.te.total%>% 
  mutate(cost = "Total (marginal) LCOE")

df.lcoe_minus_markup.te.plot <- df.lcoe_minus_markup.te %>% 
  dplyr::rename( value = totalLCOE )

df.telcoe_mv.plot <- list(df.lcoe_minus_markup.te.plot, mv) %>% 
  reduce(full_join)


########################################################################################################
########################################################################################################
swlatex(sw,"\\onecolumn")

# ########################################################################################################
swlatex(sw, paste0("\\section{Technology LCOEs}"))
swlatex(sw, paste0("\\subsection{Technology LCOE - DIETER}"))

#DIETER's marginal LCOE components for each technology, market value, shadow price, etc (marginal in the sense of one additional MWh added to the year by the marginal plant (corresponding to capfac of the highest level empty grade for VRE))

cost_bkdw_avg <- file.path(outputdir, dieter.files.report[maxiter-1]) %>%
    read.gdx("report_tech", squeeze=F) %>%
    select(model=X..1, period = X..2, variable=X..4, tech=X..5, value) %>%
    filter(variable %in% reportLCOEcomponents_DT_avg) %>%
    filter(period %in% model.periods) %>%
    filter(tech %in% names(dieter.tech.mapping)) %>%
    filter(model == "DIETER")%>%
    revalue.levels(tech = dieter.tech.mapping) %>%
    mutate(tech = factor(tech, levels=rev(unique(dieter.tech.mapping)))) %>%
    revalue.levels(variable = dieter.variable.mapping) %>%
    mutate(variable = factor(variable, levels=rev(unique(dieter.variable.mapping)))) %>%
    select(period, tech, variable, value)

  if (h2switch == "off"){
    cost_bkdw_avg <- cost_bkdw_avg %>%
      filter(!tech =="Electrolyzers")
  }

cost_bkdw_marg <- file.path(outputdir, dieter.files.report[maxiter-1]) %>%
  read.gdx("report_tech", squeeze=F) %>%
  select(model=X..1, period = X..2, variable=X..4, tech=X..5, value) %>%
  filter(variable %in% reportLCOEcomponents_DT_avg) %>%
  filter(period %in% model.periods) %>%
  filter(tech %in% names(dieter.tech.mapping)) %>%
  filter(model == "DIETER")%>%
  revalue.levels(tech = dieter.tech.mapping) %>%
  mutate(tech = factor(tech, levels=rev(unique(dieter.tech.mapping)))) %>%
  revalue.levels(variable = dieter.variable.mapping) %>%
  mutate(variable = factor(variable, levels=rev(unique(dieter.variable.mapping)))) %>%
  select(period, tech, variable, value)

if (h2switch == "off"){
  cost_bkdw_marg <- cost_bkdw_marg %>%
    filter(!tech =="Electrolyzers")
}

cost_bkdw_marg_DT <- cost_bkdw_marg %>%
  mutate(variable = factor(variable, levels=rev(unique(dieter.variable.mapping))))

cost_bkdw_avg_DT <- cost_bkdw_avg %>%
  mutate(variable = factor(variable, levels=rev(unique(dieter.variable.mapping))))

gridcost <- cost_bkdw_avg_DT %>%
  filter(tech == "VRE grid") %>%
  select(period,variable,value)

dieter.price <- cost_bkdw_avg_DT %>%
  filter(variable %in% label.price) %>%
  mutate(value = -value)

################## DIETER average LCOE plot ########################

dieter.telcoe_avg <- cost_bkdw_avg_DT %>%
  filter(!variable %in% label.price)%>%
  filter(period %in% model.periods.till2100)

dieter.teloceprice_avg <- list(dieter.price, dieter.telcoe_avg) %>%
  reduce(full_join) %>%
  filter(period %in% model.periods.till2100) %>%
  mutate(period = as.integer(period))

p <- ggplot() +
  geom_bar(data = dieter.teloceprice_avg , aes(x = period, y = value,  fill = variable), stat='identity', size = 1.2, alpha = 0.5)+
  labs(color = "LCOE") +
  labs(linetype = "") +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20, face="bold"), strip.text = element_text(size = 20)) +
  theme(legend.text = element_text(size=20), strip.text = element_text(size = 20))+
  xlab("year") + ylab(paste0("LCOE ($/MWh)"))  +
  scale_fill_manual(values = cost.colors_DT) +
  facet_wrap(~tech, nrow = 3, scales = "free")

swfigure(sw,print,p)

if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/teLCOE_avg_DIETER_bar.png"),  p,  width = 20, height =10, units = "in", dpi = 120)
}
################## DIETER average LCOE plot ########################
plot.dieter.telcoe_avg <- dieter.telcoe_avg %>%
  mutate(period = as.integer(period)) %>%
  filter(period %in% model.periods.till2100)

plot.dieter.price <-dieter.price %>%
  mutate(value = -value) %>%
  mutate(period = as.integer(period)) %>%
  filter(period %in% model.periods.till2100)

p <- ggplot() +
  geom_bar(data = plot.dieter.telcoe_avg, aes(x = period, y = value, fill = variable), stat='identity', size = 1.2, alpha = 0.5)+
  geom_line(data = plot.dieter.price, aes(period, value, linetype=variable), size=1.2) +
  labs(color = "LCOE") +
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
  filter(period %in% model.periods.till2100)

dieter.teloceprice_marg <- list(dieter.price, dieter.telcoe_marg) %>%
  reduce(full_join) %>%
  filter(period %in% model.periods.till2100) %>%
  mutate(period = as.integer(period))

p <- ggplot() +
  geom_bar(data = dieter.teloceprice_marg, aes(x = period, y = value,  fill = variable), stat='identity', size = 1.2, alpha = 0.5)+
  labs(color = "LCOE") +
  labs(linetype = "") +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20, face="bold"), strip.text = element_text(size = 20)) +
  xlab("year") + ylab(paste0("LCOE ($/MWh)"))  +
  # coord_cartesian(ylim = c(0,430))+
  scale_fill_manual(values = cost.colors_DT) +
  facet_wrap(~tech, nrow = 3, scales = "free")

swfigure(sw,print,p)

if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/teLCOE_marg_DIETER_bar.png"),  p,  width = 20, height =10, units = "in", dpi = 120)
}

plot.dieter.telcoe_marg <- dieter.telcoe_marg %>%
  mutate(period = as.integer(period)) %>%
  filter(period %in% model.periods.till2100)

p <- ggplot() +
  geom_bar(data = plot.dieter.telcoe_marg, aes(x = period, y = value,  fill = variable), stat='identity', size = 1.2, alpha = 0.5)+
  geom_line(data = plot.dieter.price, aes(period, value, linetype=variable), size=1.2) +
  labs(color = "LCOE") +
  labs(linetype = "") +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20, face="bold"), strip.text = element_text(size = 20)) +
  xlab("year") + ylab(paste0("LCOE ($/MWh)"))  +
  # coord_cartesian(ylim = c(0,430))+
  scale_fill_manual(values = cost.colors_DT) +
  facet_wrap(~tech, nrow = 3, scales = "free")

swfigure(sw,print,p)

if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/teLCOE_marg_DIETER_line.png"),  p,  width = 20, height =10, units = "in", dpi = 120)
}

########################################################################################################
swlatex(sw, paste0("\\subsection{Technology LCOE - REMIND}"))

p.teLCOE <- ggplot() +
  geom_col( data = df.lcoe.te.plot%>% filter(period %in% model.periods.till2100),
            aes(period, value, fill=cost)) +
  geom_line(data = df.telcoe_mv.plot,
            aes(period, value, linetype=cost), size=1.2) +
  facet_wrap(~tech, scales = "free_y") +
  scale_y_continuous("LCOE and REMIND Price\n(USD2015/MWh)") +
  scale_x_continuous(breaks = seq(2010,2100,10)) +
  scale_fill_manual(values = cost.colors.te) +
  theme_bw() +
  theme( axis.text.x = element_text(angle = 90),
         strip.background = element_blank())

swfigure(sw,print,p.teLCOE)

if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/teLCOE_REMIND.png"),  p.teLCOE,  width = 17, height =7, units = "in", dpi = 120)
}


########################################################################################################
swlatex(sw, paste0("\\subsection{Technology LCOE (DIETER average LCOE) - Comparison}"))

#remind tech marginal costs
df.lcoe.te.RM <- df.lcoe.te.plot %>% 
  select(period,tech,cost,lcoe = value)

# aggregated production share per dieter technology
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
  mutate(share = genshare/aggshare) %>% 
  select(period,tech=tech2,share)

df.lcoe.RM2DTte.RM <- list(prod_shareType_RM, df.lcoe.te.RM) %>% 
  reduce(full_join) %>% 
  replace(is.na(.), 0) %>% 
  mutate(agg_lcoe = share * lcoe) %>% 
  select(period,cost,tech,value = agg_lcoe) %>% 
  revalue.levels(tech = remind.tech.mapping) %>% 
  dplyr::group_by(period,tech,cost) %>%
  dplyr::summarise( value = sum(value), .groups = "keep" ) %>% 
  dplyr::ungroup(period,tech,cost) %>% 
  # filter(!cost =="Curtailment Cost") %>% 
  filter(!cost =="Markup subsidy/tax") %>% 
  mutate(period = as.integer(period)) %>% 
  filter(period %in% model.periods.till2100)

df.lcoe.avg.dieter <- cost_bkdw_avg_DT %>% 
  filter(!variable == "Market Value") %>% 
  filter(!variable == "Shadow Price") %>% 
  mutate(period = as.integer(period)) %>% 
  filter(period %in% model.periods.till2100)

barwidth = 1.5

p.techLCOE_compare<-ggplot() +
  geom_col(data = df.lcoe.RM2DTte.RM , aes(x = period-barwidth/2-0.1, y = value, fill = cost), colour="black", position='stack', size = 1, width = barwidth) +
  geom_col(data = df.lcoe.avg.dieter , aes(x = period+barwidth/2+0.1, y = value, fill = variable), colour="black", position='stack', size = 1,
           width = barwidth) +
  scale_alpha_discrete(range = c(0.4,1)) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size= 20, face="bold"),strip.text = element_text(size=25),plot.title = element_text(size = 30, face = "bold")) +
  xlab("year") + ylab(paste0("LCOE ($/MWh)")) +
  ggtitle("Tech LCOE comparison (last iteration) - left REMIND (marginal), right DIETER (average)")+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(size=20)) +
  theme(aspect.ratio = .5) +
  scale_fill_manual(name = "model", values =cost.colors.te)+
  facet_wrap(~tech, nrow = 3, scales = "free") 

swfigure(sw,print,p.techLCOE_compare)

if (save_png == 1){
  ggsave(filename = paste0(outputdir, "/DIETER/teLCOE_avg_compare.png"),  p.techLCOE_compare,  width = 25, height =15, units = "in", dpi = 120)
}
########################################################################################################
swlatex(sw, paste0("\\subsection{Technology LCOE (DIETER marginal LCOE) - Comparison}"))
df.lcoe.marg.dieter <- cost_bkdw_marg_DT %>% 
  filter(!variable == "Market Value") %>% 
  filter(!variable == "Shadow Price") %>%
  mutate(period = as.integer(period))

barwidth = 1.5

p.techmargLCOE_compare <-ggplot() +
  geom_col(data = df.lcoe.RM2DTte.RM %>% filter(period > 2015 & period <2110), aes(x = period-barwidth/2-0.1, y = value, fill = cost), colour="black", position='stack', size = 1, width = barwidth) +
  geom_col(data = df.lcoe.marg.dieter %>% filter(period > 2015 & period <2110), aes(x = period+barwidth/2+0.1, y = value, fill = variable), colour="black", position='stack', size = 1,
           width = barwidth) +
  scale_alpha_discrete(range = c(0.4,1)) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size= 20, face="bold"),strip.text = element_text(size=25),plot.title = element_text(size = 30, face = "bold")) +
  xlab("year") + ylab(paste0("LCOE ($/MWh)")) +
  ggtitle("Tech LCOE comparison (last iteration) - left REMIND (marginal), right DIETER (marginal)")+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(size=20)) +
  theme(aspect.ratio = .5) +
  scale_fill_manual(name = "model", values =cost.colors.te)+
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
  coord_cartesian(ylim = c(-30,160))+
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

p.sysLCOE_wmarkup <- ggplot() +
  geom_col( data = df.lcoe.components.nocurt.plot,
            aes(period, value, fill=cost)) +
  geom_line(data = df.pricelcoe_minus_tax.plot.nocurt %>% filter(period %in% model.periods.till2100),
            aes(period, value, linetype=variable), size=1.2) +
  facet_wrap(~tech~sector, scales = "free_y") +
  scale_y_continuous("LCOE and REMIND Price\n(USD2015/MWh)") +
  scale_x_continuous(breaks = seq(2010,2100,10)) +
  scale_fill_manual(values = cost.colors.sys.wh2) +
  coord_cartesian(ylim = c(-30,160))+
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
swlatex(sw, paste0("\\subsection{Marginal and average system LCOE - DIETER vs. REMIND}"))
#DIETER's marginal and average system LCOE and price, compared with REMIND side-by-side


prices_DT <- file.path(outputdir, dieter.files.report[maxiter-1]) %>% 
  read.gdx("report", squeeze=F) %>% 
  select(model=X..1, period = X..2, variable=X..4, value) %>%
  filter(variable %in% report_DT_prices) %>% 
  filter(period %in% model.periods) %>% 
  filter(model == "DIETER")%>% 
  revalue.levels(variable = dieter.variable.mapping) %>%
  mutate(variable = factor(variable, levels=rev(unique(dieter.variable.mapping)))) %>% 
  mutate(period = as.integer(period)) %>% 
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

# prices_RM <- df.pricelcoe_minus_tax.plot %>%
prices_RM <- df.pricelcoe_minus_tax.plot.nocurt %>%
  filter(tech == "System") %>%
  filter(variable %in% c("REMIND Price", "Total (marginal) LCOE - Flex Tax")) %>%
  select(period, variable, value)%>%
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


genshare.dieter <- file.path(outputdir, dieter.files.report[maxiter-1]) %>% 
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
  select(period, tech, variable, value)

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
  mutate(period = as.integer(period)) 

sysLCOE_avg_DT$type <- "Average"
sysLCOE_avg_DT$model <- "DIETER"

# sysLCOE_marg_RM <- df.lcoe.components %>%
sysLCOE_marg_RM <- df.lcoe.components.nocurt %>%
  filter(tech == "System") %>%
  select(!variable) %>%
  select(period, variable= cost, value) %>%
  mutate(model = "REMIND")

sys_avgLCOE_compare <- list(sysLCOE_avg_DT, sysLCOE_marg_RM) %>% 
  reduce(full_join)  

p.sysLCOE_compare <- ggplot() + 
  geom_col( data = sys_avgLCOE_compare, 
            aes(period, value, fill=variable)) +
  geom_line(data = prices_lines %>% filter(period %in% model.periods.till2100) ,
            aes(period, value, color=variable), size=1.2) +  
  geom_line(data = prices_RM.movingavg %>% filter(period %in% model.periods.till2100) ,
            aes(period, value, color=variable), alpha = 0.5, size=2) +  
  # geom_polygon(data = prices_lines, aes(x = period, y = value), alpha = 0.3) +
  facet_wrap(~model, scales = "free_y") +
  scale_y_continuous("LCOE and power price\n(USD2015/MWh)") +
  scale_x_continuous(breaks = seq(2010,2100,10)) +
  scale_color_manual(name = "variable", values = price.colors) +
  coord_cartesian(ylim = c(-5,115))+
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
#   mutate(period = as.integer(period))
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
  geom_col(data = dieter.telcoe_avg_vre %>% filter(period > 2015 & period <2110), aes(x = tech, y = value, fill = variable), colour="black", position='stack', size = 1) +
  geom_col(data = dieter.telcoe_avg_ffr %>% filter(period > 2015 & period <2110), aes(x = tech, y = value, fill = variable), colour="black", position='stack', size = 1) +
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
