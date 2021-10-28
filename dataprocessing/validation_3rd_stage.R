#for revenue and market value, produces cvs table of marginal and annual average revenue and market value

mypath = "~/remind-coupling-dieter/dataprocessing/"
myDIETERPLOT_path = "~/remind-coupling-dieter/dataprocessing/DIETER_plots/"

#fully coupled REMIND run (in validation mode
run_number_full = "hydro355"

# import library
source(paste0(mypath, "library_import.R"))
source(paste0(myDIETERPLOT_path, "GDXtoQuitte.R"))

# !!!Note:run order is remind_1 -> remind_2 (not coupled) -> dieter_1 (coupled to remind_1) -> remind_3 (coupled to dieter_1) -> dieter_2 (coupled to remind_2) ...
## gdx in order produced: fulldata_1.gdx -> DIETER_i1.gdx -> fulldata_2.gdx -> DIETER_i2.gdx -> fulldata_3.gdx ...



run_number = run_number_full
mydatapath =  paste0("~/remind-coupling-dieter/output/", run_number, "/") 

#remind output iteration gdx files
files <- list.files(mydatapath, pattern="fulldata_[0-9]+\\.gdx")
sorted_files <- paste0(mydatapath, "fulldata_", 1:length(files), ".gdx")

maxiter = length(files)

# to compare runs between the 2 models, we should use dieter1 to compare with remind2, because they have
# the same CF, however, they will see different LCOE
iter_tovalid_RM = c(28,29)
iter_tovalid_DT_4RM = c(28,29) # the iteration of DIETER needed for reporting iteration=iter_tovalid_RM of remind
iter_tovalid_DT = c(27,28) # the iteration of DIETER needed for reporting the iteration of DIETER that passes
#CF to REMIND iteration iter_tovalid_RM

#dieter output iteration gdx files
files_DT_rep <- list.files(mydatapath, pattern="report_DIETER_i[0-9]+\\.gdx") 
# for(fname in files_DT_rep){
# gdxToQuitte_annual(mydatapath, fname, run_number)
# }
startyear = 2020
endyear = 2100

id <- NULL
for(fname in files_DT_rep){
  idx <- as.numeric(str_extract(fname, "[0-9]+"))
  id = c(id, idx)
}

if (length(files_DT_rep) != 0) {
  sorted_annual_report_DT <- paste0(myDIETERPLOT_path, run_number, "_i", sort(id), "_annualreport.csv")
}

remind.nonvre.mapping <- c(coalchp = "Coal (Lig + HC)",
                           igcc = "Coal (Lig + HC)",
                           igccc = "Coal (Lig + HC)",
                           pcc = "Coal (Lig + HC)",
                           pco = "Coal (Lig + HC)",
                           pc = "Coal (Lig + HC)",
                           tnrs = "Nuclear",
                           ngt = "OCGT",
                           ngcc = "CCGT",
                           ngccc = "CCGT",
                           gaschp = "CCGT",
                           biochp = "Biomass",
                           bioigcc = "Biomass",
                           bioigccc = "Biomass",
                           NULL)

remind.vre.mapping <- c(hydro = "Hydro",
                        wind = "Wind",
                        spv = "Solar",
                        NULL)

remind.dem.mapping <- c(elh2 = "Electrolyser",
                        el = "Electricity",
                        NULL)

vre.names <- c("Hydro","Wind","Solar")
nonvre.names <- c("Lignite", "Hard coal","Coal (Lig + HC)", "Nuclear","OCGT","CCGT","Biomass")
table_ordered_name = c("Coal (Lig + HC)", "Lignite", "Hard coal","CCGT", "Solar", "Wind", "Biomass", "OCGT", "Hydro", "Nuclear")

remind.tech.mapping <- c(remind.nonvre.mapping, remind.vre.mapping,remind.dem.mapping)

dieter.tech.exclude <- c("OCGT_ineff", "Wind_off")

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
                         elh2 = "Electrolyser",
                         el = "Secondary Electricity (stationary or T&D)",
                         "all Tech" = "Secondary Electricity (total)",
                         NULL)

table_tech_order <-  c(Wind_on = "Wind",
                       Solar = "Solar",
                       hc = "Hard coal",
                       lig = "Lignite",
                       coal = "Coal (Lig + HC)",
                       nuc = "Nuclear",
                       OCGT_eff = "OCGT",
                       CCGT = "CCGT",
                       bio = "Biomass",
                       ror = "Hydro",
                       el = "Secondary Electricity (stationary or T&D)",
                       "all Tech" = "Secondary Electricity (total)",
                       elh2 = "Electrolyser",
                       NULL)
  
  
VAR_report_key1_DT = c("DIETER avg CapFac (%)","DIETER LCOE_avg ($/MWh)","DIETER LCOE_marg ($/MWh)","DIETER marg CapFac (%)","DIETER LCOE_marg ($/kWh)","DIETER Market value ($/MWh)","DIETER Marginal market value ($/MWh)","DIETER Revenue (billionUSD)","DIETER Revenue marginal plant (millionUSD)", "DIETER added capacities (GW)", "REMIND pre-investment capacities (GW)","REMIND LCOE ($/MWh)", "REMIND CapFac (%)","REMIND added capacities (GW)","DIETER LCOE_marg ($/MWh)", "load-weighted price for fixed demand ($/MWh)", "price w/ scarcity price shaved ($/MWh)","DIETER Market value w/ scarcity price shaved ($/MWh)", "DIETER Value factor (%)", "genshares (%)")

# relevant cost parameters to be loaded from DIETER reporting
VAR_report_key2_DT = c("annualized investment cost ($/kW)", "O&M cost ($/kW)")
VAR_report_key4_DT = c("annualized investment cost ($/kW)", "O&M cost ($/kW)", "fuel cost - divided by eta ($/MWh)", "CO2 cost ($/MWh)")
VAR_report_key_DT = c(VAR_report_key1_DT, VAR_report_key4_DT)

# =======================================
report.periods <- c(seq(2020, 2060, 5), seq(2070, 2100, 10))
year_tovalid = report.periods
year_tovalid_before = c(seq(2015, 2060, 5), seq(2070, 2090, 10))
year_tovalid_after  = c(seq(2025, 2060, 5), seq(2070, 2110, 10))

# =======================================

BUDGETkey1 = "qm_budget"
SEELPRICEkey = "pm_SEPrice"
CFkey1 = "vm_capFac"
CFkey2 = "pm_dataren"
CFkey3 = "vm_capDistr"
grade = "1"

InvAdjKEY = "o_margAdjCostInv"

CapConstraintKey = "q32_peakDemand_DT"
DEMkey1 = "vm_demSe"
GENkey1 = "vm_prodSe"
GENkey2 = "vm_usableSeTe"
GENSHAREkey = "v32_shSeEl"
CAPkey1 = "vm_cap"
CAPkey2 = "vm_deltaCap"
PM_TS_Key = "pm_ts"
ProdKEY = "seel"
ProdKEY2 = "seh2"
MARKUP = "vm_Mrkup"
MARKETPRICE = "vm_flexAdj"

REGIkey1 = "DEU"
sm_TWa_2_MWh = 8760000000

TECHkeylst_peakGas = c("ngt")
TECHkeylst_nonPeakGas = c("ngccc","ngcc",  "gaschp") 
TECHkeylst_coal = c("coalchp", "igccc", "igcc", "pcc", "pco","pc")
TECHkeylst_solar = c("spv")
TECHkeylst_wind = c("wind")
TECHkeylst_hydro = c("hydro")
TECHkeylst_nuclear = c("tnrs")
TECHkeylst_biomass = c("biochp", "bioigccc", "bioigcc")

TECHkeylst <- c(TECHkeylst_peakGas, TECHkeylst_nonPeakGas, TECHkeylst_coal, TECHkeylst_solar, TECHkeylst_wind, TECHkeylst_hydro, TECHkeylst_biomass, TECHkeylst_nuclear)

TECHkeylst_dem <- c("elh2","tdels")

TECHVREkeylst <- c(TECHkeylst_solar, TECHkeylst_wind,TECHkeylst_hydro)
TECH_NONVRE_keylst <- c(TECHkeylst_peakGas, TECHkeylst_nonPeakGas, TECHkeylst_coal, TECHkeylst_biomass, TECHkeylst_nuclear)

#=========================================================================================================
#=========================================================================================================
#=========================================================================================================

######################################################
########## unpack reporting from DIETER ##############

out.dieter.report_DT <- NULL
for (i in iter_tovalid_DT){
  # i = 29
  csv = sorted_annual_report_DT[[i+1]] # since DIETER files start from 0
  annual_reportCSV = read.csv(csv, sep = ";", header = T, stringsAsFactors = F)
  annual_reportQUITT <- as.quitte(annual_reportCSV) 
  
  dieter.data <- annual_reportQUITT %>% 
    filter(period %in% year_tovalid) %>% 
    filter(variable %in% VAR_report_key_DT) %>% 
    filter(model == "DIETER") %>% 
    select(period, tech, variable, value) %>% 
    filter(!tech %in% dieter.tech.exclude) %>% 
    revalue.levels(tech = dieter.tech.mapping) %>% 
    replace(is.na(.), 0) 
  
  # dieter.data2 <- annual_reportQUITT %>% 
  #   filter(period %in% year_tovalid) %>% 
  #   filter(variable == "REMIND CapFac (%)") %>% #remind VRE capfac reported at DIETER's current iteration
  #   select(period, tech, variable, value) %>% 
  #   revalue.levels(tech = dieter.tech.mapping) %>% 
  #   filter(tech %in% vre.names) %>% 
  #   replace(is.na(.), 0) 
  
  # dieter.data <- list(dieter.data1, dieter.data2) %>%
  #   reduce(full_join) 
  
  # this should match iter_tovalid_DT above
  dieter.data$iter = i+1
  
  out.dieter.report_DT <- rbind(out.dieter.report_DT, dieter.data)
}

dieter.data.spread_DT = spread(out.dieter.report_DT, variable, value)

# parsing the tables into LCOE reporting: shared LCOE data, DIETER reporting: DT_rep, and VRE CF: RM_capfac_VRE
DT_LCOE <-out.dieter.report_DT %>% 
  filter(variable %in% VAR_report_key4_DT)

DT_rep <- out.dieter.report_DT %>% 
  filter(!variable %in% VAR_report_key4_DT) 
# %>% 
  # filter(!variable %in% c( "REMIND CapFac (%)"))

DT_rep_spread = spread(DT_rep, variable, value) %>% 
  replace(is.na(.), 0) 

# RM_capfac_VRE <- out.dieter.report_DT %>% 
  # filter(variable %in% c( "REMIND CapFac (%)") )

#===================================================
out.dieter.report_RM <- NULL
for (i in iter_tovalid_DT_4RM){
  # i = 2
  cvs = sorted_annual_report_DT[[i+1]] # since DIETER files start from 0
  annual_reportCSV = read.csv(cvs, sep = ";", header = T, stringsAsFactors = F)
  annual_reportQUITT <- as.quitte(annual_reportCSV) 
  
  dieter.data1 <- annual_reportQUITT %>% 
    filter(period %in% year_tovalid) %>% 
    filter(variable %in% VAR_report_key_DT) %>% 
    filter(!variable %in% c("REMIND CapFac (%)" )) %>% 
    filter(model == "REMIND") %>% 
    select(period, tech, variable, value) %>% 
    revalue.levels(tech = dieter.tech.mapping) %>% 
    filter(!tech %in% c("Hard coal", "Lignite")) %>% 
    replace(is.na(.), 0) 
  
  dieter.data2 <- annual_reportQUITT %>% 
    filter(period %in% year_tovalid) %>% 
    filter(variable == "REMIND CapFac (%)") %>% #remind CF is reported at next DIETER iteration
    select(period, tech, variable, value) %>% 
    revalue.levels(tech = dieter.tech.mapping) %>% 
    # filter(tech %in% nonvre.names) %>% 
    replace(is.na(.), 0) 
  
  dieter.data <- list(dieter.data1, dieter.data2) %>%
    reduce(full_join) 
  
  # this should match iter_tovalid_DT, iter_tovalid_RM
  dieter.data$iter = i
  
  out.dieter.report_RM <- rbind(out.dieter.report_RM, dieter.data)
}

# parsing the tables into REMIND reporting: dieter.data.spread_RM, 
# REMIND dispatchable CF: RM_capfac_disp

out.dieter.report_RM1 <-out.dieter.report_RM %>%
  filter(!variable%in%c( "REMIND CapFac (%)") ) 

dieter.data.spread_RM = spread(out.dieter.report_RM1, variable, value)

RM_capfac_disp <-out.dieter.report_RM %>%
  filter(variable%in%c( "REMIND CapFac (%)") ) %>%
  filter(tech %in% nonvre.names) 

RM_capfac_VRE <- out.dieter.report_DT %>%
  filter(variable %in% c( "REMIND CapFac (%)") ) %>% 
  filter(tech %in% vre.names) 

# combine REMIND VRE and nonVRE CF: RM_capfac_spread

RM_capfac<- list(RM_capfac_disp,RM_capfac_VRE) %>%
  reduce(full_join) 

RM_capfac_spread = spread(RM_capfac, variable, value)

#===================================================
get_CAPCON <- function(iteration){
  # iteration = 19
  gdx = sorted_files[[iteration]]
  
  budgetdata <- read.gdx(gdx, BUDGETkey1, field="m") %>%
    dplyr::rename(period = ttot)  %>%
    filter(period %in% year_tovalid) %>%
    filter(all_regi == REGIkey1) %>%
    mutate(m = -m) %>%
    dplyr::rename(budget = m) %>%
    select(period, budget)
  # 
  capcondata1 <- read.gdx(gdx, CapConstraintKey, field="m") %>%
  mutate(m = -m) %>%
  select(period = ttot, cap_ShadowPrice = m) %>% 
  filter(period %in% year_tovalid) 
    
    # capcondata1 <- read.gdx(gdx, CapConstraintKey, field="l") %>% 
    #   filter(tall %in% year_tovalid) %>%
    #   mutate(value = -value) %>% 
    #   select(period = tall, cap_ShadowPrice = value) 
    # 
  #transform from tr$2005/TW to $2015/kW
  capcondata = list(capcondata1, budgetdata) %>%
    reduce(full_join) %>%
    select(period, cap_ShadowPrice, budget) %>%
    replace(is.na(.), 0) %>%
    mutate(cap_ShadowPrice = cap_ShadowPrice/ budget * 1e12 / 1e9 * 1.2) %>%
    select(period, cap_ShadowPrice)
  
  capcondata$iter = iteration
  
  return(capcondata)
}

# capfac----------------------------
RM_capfac_disp <-RM_capfac %>% 
  filter(tech %in% nonvre.names) %>% 
  select(period,tech,value,iter) %>% 
  dplyr::rename(CapFac = value) %>% 
  filter(!tech %in% c("Hard coal", "Lignite")) %>% 
  filter(CapFac>0)

vr1_capcon_list <- lapply(iter_tovalid_RM, get_CAPCON)
vr1_capcon <- rbindlist(vr1_capcon_list)

# #2015$/kW -> 2015$/Mwh, $/kW/FLH = $/kW / ( 8760h * CF) * 1e3 = $/MWh, CF in unit %, CF% -> CF /1e2

ShadowPrice <- list(vr1_capcon, RM_capfac_disp) %>%
  reduce(full_join) %>% 
  mutate(cap_ShadowPrice = cap_ShadowPrice / (8760 * CapFac / 1e2) * 1e3) %>% 
  select(period, iter, tech, cap_ShadowPrice) %>% 
  filter(!tech %in% c("Hard coal", "Lignite")) %>% 
  dplyr::rename("REMIND Capacity Shadow Price ($/MWh)" = cap_ShadowPrice)%>% 
  replace(is.na(.), 0) 


#==========================REMIND seel price and revenue============================================
#======================================================================
get_PRICEvariable <- function(iteration){
  # iteration =29
  gdx = sorted_files[[iteration]]
  
  vrdata <- read.gdx(gdx, SEELPRICEkey, squeeze = FALSE) %>%  
    filter(all_enty == ProdKEY) %>%
    filter(ttot %in% year_tovalid) %>%
    filter(all_regi == REGIkey1) %>% 
    mutate(value = value * 1e12 / sm_TWa_2_MWh * 1.2) %>% 
    select(period = ttot, seelprice = value) 
  
  vrdata$iter <- iteration
  return(vrdata)
}

get_GENvariable <- function(iteration, key, te_keylst){
  # iteration = 4
  gdx = sorted_files[[iteration]]
  vrdata <- read.gdx(gdx, key, field="l", factors = FALSE) %>% 
    filter(tall%in% year_tovalid) %>% 
    filter(all_regi == REGIkey1) %>%
    filter(all_enty.1 == ProdKEY)  %>% 
    filter(all_te %in% te_keylst) %>% 
    mutate(value = value * sm_TWa_2_MWh) %>% 
    select(tall,all_te, value) %>% 
    filter(all_te %in% names(remind.tech.mapping)) %>% 
    revalue.levels(all_te = remind.tech.mapping) %>% 
    mutate(all_te = factor(all_te, levels=rev(unique(remind.tech.mapping)))) %>%
    select(period = tall,tech=all_te, gen = value) %>% 
    dplyr::group_by(period,tech) %>%
    dplyr::summarise( gen = sum(gen), .groups = "keep" ) %>% 
    dplyr::ungroup(period,tech)
  
  vrdata$iter <- iteration
  
  return(vrdata)
}

get_DEMvariable <- function(iteration, key, te_keylst){
  # iteration = 4
  # key = DEMkey1
  # te_keylst = TECHkeylst_dem
  
  gdx = sorted_files[[iteration]]
  vrdata <- read.gdx(gdx, key, field="l", factors = FALSE) %>% 
    filter(ttot%in% year_tovalid) %>% 
    filter(all_regi == REGIkey1) %>%
    filter(all_enty.1 == ProdKEY2)  %>% 
    filter(all_te %in% te_keylst) %>% 
    mutate(value = value * sm_TWa_2_MWh) %>% 
    select(ttot, all_te, value) %>% 
    filter(all_te %in% names(remind.tech.mapping)) %>% 
    revalue.levels(all_te = remind.tech.mapping) %>% 
    mutate(all_te = factor(all_te, levels=rev(unique(remind.tech.mapping)))) %>%
    select(period = ttot,tech=all_te, gen = value) %>% 
    dplyr::group_by(period,tech) %>%
    dplyr::summarise( gen = sum(gen), .groups = "keep" ) %>% 
    dplyr::ungroup(period,tech)
  
  vrdata$iter <- iteration
  
  return(vrdata)
}

vrN_GEN0 <- lapply(iter_tovalid_RM, get_GENvariable, key = GENkey1, te_keylst = TECHkeylst)
vrN_DEM0 <- lapply(iter_tovalid_RM, get_DEMvariable, key = DEMkey1, te_keylst = TECHkeylst_dem)

vrN_PRICE0 <- lapply(iter_tovalid_RM, get_PRICEvariable)
vrN_GEN <- rbindlist(vrN_GEN0)
vrN_DEM <- rbindlist(vrN_DEM0)
vrN_PRICE<- rbindlist(vrN_PRICE0) 

# -----------------------------------  generation share --------------------------------------------

get_GEN_SHARE <- function(iteration){
  # gdx = sorted_files[[5]]
  gdx = sorted_files[[iteration]]
  
  vrdata <- read.gdx(gdx, GENSHAREkey)  %>% 
    filter(all_regi == REGIkey1) %>% 
    filter(all_te %in% TECHkeylst) %>% 
    filter(ttot >= startyear) %>% 
    filter(ttot <= endyear) %>% 
    mutate(genshare = value) %>% 
    revalue.levels(all_te = remind.tech.mapping) %>%
    dplyr::group_by(ttot, all_te) %>%
    dplyr::summarise( genshare = sum(genshare), .groups = "keep" ) %>% 
    dplyr::ungroup(ttot, all_te) %>% 
    select(period=ttot, tech=all_te, genshare) 
  
  vrdata$iter <- iteration
  return(vrdata)
}

genshare0 <- lapply(iter_tovalid_RM, get_GEN_SHARE)
genshare <- rbindlist(genshare0)

# -----------------------------------  markup --------------------------------------------
get_MARKUPvariable <- function(iteration, MARKUPkey){
  # MARKUPkey=MARKUP
  # iteration = 32
  gdx = sorted_files[[iteration]]

  vrdata <- read.gdx(gdx, MARKUPkey, field="l") %>%
    filter(tall %in% year_tovalid) %>%
    filter(all_regi == REGIkey1) %>%
    filter(all_te %in% names(remind.tech.mapping)) %>%
    revalue.levels(all_te = remind.tech.mapping) %>%
    dplyr::group_by(tall, all_te) %>%
    dplyr::summarise( value = mean(value) , .groups = "keep" ) %>%
    dplyr::ungroup(tall, all_te) %>%
    dplyr::rename(period = tall) %>%
    mutate(value = value * 1e12 / sm_TWa_2_MWh * 1.2) %>%
    select(period,markup=value, tech = all_te)

  vrdata$iter <- iteration
  return(vrdata)
}

mp0 <- lapply(iter_tovalid_RM, get_MARKUPvariable, MARKUPkey = MARKETPRICE)
mp <- rbindlist(mp0)

markup0 <- lapply(iter_tovalid_RM, get_MARKUPvariable, MARKUPkey = MARKUP)
markup <- rbindlist(markup0)

vrN_REV <- list(vrN_GEN,vrN_PRICE,markup) %>%
  reduce(full_join) %>% 
  replace(is.na(.), 0) %>% 
  mutate(MarketValue_RM = seelprice + markup) %>% 
  filter(!tech %in% c("Hard coal", "Lignite")) %>% 
  mutate(Revenue_RM_billUSD = gen * MarketValue_RM / 1e9) %>% 
  select(period,tech,iter,Revenue_RM_billUSD,seelprice,MarketValue_RM, markup)%>% 
  dplyr::rename("REMIND Market value ($/MWh)" = MarketValue_RM) %>% 
  dplyr::rename("REMIND whole sale electricity price ($/MWh)" = seelprice) %>% 
  dplyr::rename("REMIND Revenue (billionUSD)" = Revenue_RM_billUSD) %>% 
  dplyr::rename("REMIND Markup ($/MWh)" = markup)

vrN_COST <- list(vrN_DEM,vrN_PRICE,mp) %>%
  reduce(full_join) %>% 
  replace(is.na(.), 0) %>% 
  mutate(MarketPrice_RM = seelprice - markup) %>% 
  mutate(Cost_RM_billUSD = gen * MarketPrice_RM / 1e9) %>% 
  select(period,tech,iter,seelprice,MarketPrice_RM, markup) %>% 
  dplyr::rename("REMIND Market price ($/MWh)" = MarketPrice_RM) %>% 
  dplyr::rename("REMIND whole sale electricity price ($/MWh)" = seelprice) %>% 
  dplyr::rename("REMIND Subsidy ($/MWh)" = markup)

vrN_REV <- list(vrN_REV,vrN_COST) %>%
  reduce(full_join) %>% 
  filter(period >2020)%>% 
  replace(is.na(.), 0)

#=================================================
get_ADJ_COST2 <- function(iteration, years){
  years = year_tovalid
  # iteration = 5
  gdx = sorted_files[[iteration]]
  # ajustment cost
  adjcost <- read.gdx(gdx, InvAdjKEY) %>% 
    filter(ttot %in% years) %>% 
    filter(all_te %in% names(remind.tech.mapping)) %>% 
    filter(all_regi == REGIkey1) %>%
    filter(all_te %in% names(remind.tech.mapping)) %>% 
    revalue.levels(all_te = remind.tech.mapping) %>%
    select(period = ttot, tech = all_te, adjCost = value) %>% 
    dplyr::group_by(period, tech) %>%
    dplyr::summarise( adjCost = mean(adjCost), .groups = "keep" ) %>% 
    dplyr::ungroup(period, tech)
  
  adjcost$iter <- iteration
  return(adjcost)
}

vrN_InvAdjcost <- lapply(iter_tovalid_RM, get_ADJ_COST2, years = year_tovalid)
vrN_InvAdjcost <- rbindlist(vrN_InvAdjcost)

# Trillion 2015$/TW -> 2015$/MWh, Tr$/TW/FLH = $/MW / ( 8760h * CF) * 1e6 = $/MWh, CF in unit %, CF% -> CF /1e2

Adjcost_disp <- list(vrN_InvAdjcost, RM_capfac_disp) %>%
  reduce(right_join) %>% 
  mutate(adjCost = adjCost / (8760 * CapFac / 1e2) * 1e6 * 1.2) %>% 
  select(period, iter, tech, "Marginal adjustment cost ($/MWh)" = adjCost) %>% 
  replace(is.na(.), 0) 

# renewable grade related CF, LCOE
get_dataren <- function(iteration){
  # iteration = 15
  gdx = sorted_files[[iteration]]
  
  # CapFac is not always 1 for VRE in remind!!!!!!!!
  remind.vm_capFac <- read.gdx(gdx, "vm_capFac", field="l", squeeze=F) %>% 
    filter(all_regi == REGIkey1) %>%
    filter(all_te %in% names(remind.vre.mapping)) %>% 
    revalue.levels(all_te = remind.vre.mapping) %>%
    filter(ttot %in% year_tovalid) %>% 
    select(period = ttot,tech = all_te,vm_capfac=value)
  
  # pm_dataren("nur") = capacity factor at different grade levels (quality of wind resources), pm_dataren is used to represent different cost levels necessary to create the same amount of energy if you have to build your wind farm in places with lower wind. The grades are ordered from 1 to 10. 1 is closer to the wind always blowing (highest nur capacity factor), 10 is the worst place to install wind.
  dataren <- read.gdx(gdx, CFkey2) %>% 
    filter(char == "nur") %>%
    filter(all_regi == REGIkey1) %>%
    filter(all_te %in% names(remind.vre.mapping)) %>% 
    revalue.levels(all_te = remind.vre.mapping) %>% 
    dplyr::rename(ren_nur = value) %>% 
    dplyr::rename(tech = all_te) %>% 
    select(tech, rlf, ren_nur)
  
  vrdata <- list(remind.vm_capFac, dataren) %>%
    reduce(right_join) %>% 
    mutate(VRE_capfac = ren_nur * vm_capfac) %>% 
    select(period, tech, rlf, VRE_capfac)
  
  vrdata$iter <- iteration
  
  return(vrdata)
}

vrN_CFgrade <- lapply(iter_tovalid_RM, get_dataren)
vrN_CFgrade <- rbindlist(vrN_CFgrade)

Adjcost_VRE <- list(vrN_InvAdjcost, vrN_CFgrade) %>%
  reduce(right_join) %>% 
  mutate(adjCost = adjCost / (8760 * VRE_capfac ) * 1e6 * 1.2) %>% 
  select(period, iter, tech, rlf, "Marginal adjustment cost ($/MWh)" = adjCost) %>% 
  replace(is.na(.), 0) 

DT_LCOE_sprd =spread(DT_LCOE, variable, value) %>% 
  replace(is.na(.), 0) 

# RM and DT share the same IC and OM
RM_LCOE_grade <- list(DT_LCOE_sprd, vrN_CFgrade) %>% 
  reduce(full_join) %>% 
  filter(tech %in% vre.names) %>% 
  replace(is.na(.), 0) %>% 
  dplyr::rename(IC = "annualized investment cost ($/kW)") %>% 
  dplyr::rename(OM = "O&M cost ($/kW)") %>% 
  mutate(LCOE_grade = (IC + OM) * 1e3 / (8760 * VRE_capfac) ) %>%  #IC & OM $/kW -> $/MW, cap / gen = 1 / (gen / cap) = (1/8760) / (gen / 8760* cap) = (1/8760) / ren_nur 
  mutate(VRE_capfac = VRE_capfac * 1e2) %>% 
  select(period, tech, iter, rlf, LCOE_grade, VRE_capfac) %>% 
  dplyr::rename("REMIND LCOE grade ($/MWh)" = LCOE_grade) %>% 
  dplyr::rename("REMIND CapFac grade (%)" = VRE_capfac) 

get_capDistr <- function(iteration){
  # iteration = 15
  gdx = sorted_files[[iteration]]
  #vm_capDistr is a "helper" variable that stores the amount of capacity that REMIND assign to each grade. Therefore it can have values from any grade. vm_cap is the total capacity (sum of vm_capDistr). It does not have grade infor anymore, everything is assigned to "1". vm_capDistr will retain the information of the optimal grades used in the REMIND decision
  capDistr <- read.gdx(gdx, CFkey3) %>% 
    filter(tall %in% year_tovalid) %>%
    filter(all_regi == REGIkey1) %>%
    filter(all_te %in% names(remind.vre.mapping)) %>% 
    revalue.levels(all_te = remind.vre.mapping) %>% 
    select(tall,all_te, rlf, cap_distr = value) %>% 
    dplyr::group_by(all_te) %>%
    mutate(cap_distr = cap_distr * 1e3) %>% 
    dplyr::ungroup(all_te) %>% 
    dplyr::rename(period = tall)  %>% 
    dplyr::rename(tech = all_te) %>% 
    filter(cap_distr >1e-5)
  
  capDistr$iter <- iteration
  return(capDistr)
}

vrN_capgrade <- lapply(iter_tovalid_RM, get_capDistr)
vrN_capgrade <- rbindlist(vrN_capgrade)

vrN_capDistr <- vrN_capgrade %>% 
  dplyr::rename("REMIND VRE distributed capacities (GW)"= cap_distr) 

vrN_grade <- right_join(RM_LCOE_grade, vrN_capDistr)

Adjcost_VRE0 <- right_join(Adjcost_VRE, vrN_capDistr)

Adjcost <- full_join(Adjcost_disp, Adjcost_VRE0)

#============ minimum screening curve envelop and whether a tech belongs to it (or whether a tech  ============
#============ is the cheapest to disptach at certain hours of a year) =========================================
ScreenCurve0 <- DT_LCOE_sprd %>% 
  replace(is.na(.), 0) %>% 
  dplyr::rename(IC = "annualized investment cost ($/kW)") %>% 
  dplyr::rename(OM = "O&M cost ($/kW)") %>% 
  dplyr::rename(FC = "fuel cost - divided by eta ($/MWh)") %>% 
  dplyr::rename(CO2 = "CO2 cost ($/MWh)") %>% 
  filter(tech %in% nonvre.names)

ScreenCurve <-ScreenCurve0 %>% 
  expand(ScreenCurve0, sorted_x = seq(1, 8760)) %>% 
  mutate(value = sorted_x * (FC + CO2) / 1e3 + (IC + OM))

ScreenCurve_min_envelop <- ScreenCurve %>% 
  select(period, iter, tech, sorted_x, value) %>% 
  dplyr::group_by(period, iter, sorted_x) %>%  
  dplyr::summarize(minvalue= min(value), minTech = tech[which.min(value)], .groups = "keep" )%>% 
  dplyr::ungroup(period, iter, sorted_x) %>% 
  select(period, iter, minTech) %>% 
  dplyr::group_by(period, iter) %>% 
  dplyr::summarize(minTech= unique(minTech), .groups = "keep" ) %>% 
  dplyr::rename(tech = minTech)

ScreenCurve_min_envelop$minTech_yesno <- "1"
# whether a tech belongs to the minumum inverse screening curve

ScreenCurve_min_tech_binary <-list(ScreenCurve0, ScreenCurve_min_envelop ) %>% 
  reduce(full_join) %>% 
  select(period, iter, tech, minTech_yesno) %>% 
  replace(is.na(.), 0) 

# ===================================== join all tables ========================================
#  get shadow price to be 0 for VRE, all REMIND tech without hc, lig
  vrN0 <- list(vrN_REV,ShadowPrice,dieter.data.spread_RM)%>%
    reduce(full_join) %>% 
    replace(is.na(.), 0)

  vrN1 <- list(ScreenCurve_min_tech_binary,DT_rep_spread)%>%
    reduce(full_join) %>% 
    replace(is.na(.), 0)
  
  vrN<- list(vrN0,RM_capfac_spread, Adjcost, vrN_grade,vrN1,genshare) %>%
    reduce(full_join) %>% 
    mutate(tech = fct_relevel(tech, table_ordered_name)) 

# for nonVRE, if REMIND MV is larger than REMIND LCOE
RM_MV_ge_LCOE <- (vrN$"REMIND Market value ($/MWh)"[vrN$tech %in% nonvre.names] >= vrN$"REMIND LCOE ($/MWh)"[vrN$tech %in% nonvre.names])
vrN$RM_MV_ge_LCOE[vrN$tech %in% nonvre.names] <- RM_MV_ge_LCOE
#for VRE,  if REMIND MV is larger than REMIND LCOE_grade
RM_MV_ge_LCOE_VRE <- (vrN$"REMIND Market value ($/MWh)"[vrN$tech %in% vre.names] >= vrN$"REMIND LCOE grade ($/MWh)"[vrN$tech %in% vre.names])
vrN$RM_MV_ge_LCOE[vrN$tech %in% vre.names] <- RM_MV_ge_LCOE_VRE

# if REMIND MV plus capacity constraint is larger than REMIND LCOE plus adjustment cost
# for dispatchable
total_disp_MV = vrN$"REMIND Market value ($/MWh)"[vrN$tech %in% nonvre.names] + vrN$"REMIND Capacity Shadow Price ($/MWh)"[vrN$tech %in% nonvre.names] 
total_disp_cost =  vrN$"REMIND LCOE ($/MWh)"[vrN$tech %in% nonvre.names] + vrN$"Marginal adjustment cost ($/MWh)"[vrN$tech %in% nonvre.names]
RM_disp_value_ge_cost <- (total_disp_MV>= total_disp_cost)

# for VRE
total_VRE_MV = vrN$"REMIND Market value ($/MWh)"[vrN$tech %in% vre.names] + vrN$"REMIND Capacity Shadow Price ($/MWh)"[vrN$tech %in% vre.names]
total_VRE_cost =  vrN$"REMIND LCOE grade ($/MWh)"[vrN$tech %in% vre.names] + vrN$"Marginal adjustment cost ($/MWh)"[vrN$tech %in% vre.names]
RM_VRE_value_ge_cost <- (total_VRE_MV >=total_VRE_cost)

vrN$RM_value_ge_cost[vrN$tech %in% nonvre.names] <- RM_disp_value_ge_cost
vrN$total_MV[vrN$tech %in% nonvre.names] <- total_disp_MV
vrN$total_cost[vrN$tech %in% nonvre.names] <- total_disp_cost

vrN$RM_value_ge_cost[vrN$tech %in% vre.names] <- RM_VRE_value_ge_cost
vrN$total_MV[vrN$tech %in% vre.names] <- total_VRE_MV
vrN$total_cost[vrN$tech %in% vre.names] <- total_VRE_cost

# vrN2 <- vrN %>%
#   dplyr::rename(DT_eprice = "load-weighted price for fixed demand ($/MWh)") %>% 
#   dplyr::group_by(across(c(-DT_eprice))) %>% 
#   dplyr::mutate(DT_eprice[tech != "all Tech"] = DT_eprice[tech == "all Tech"], .groups = "keep" )
#   dplyr::ungroup(across(c(-DT_eprice)))
# 
# # value factors
# vrN$VF_DIETER_NonShaved <- vrN$"DIETER Market value ($/MWh)"/(vrN$"load-weighted price for fixed demand ($/MWh)"[vrN$tech == "all Tech"])
# vrN$VF_DIETER_Shaved <- vrN$"DIETER Market value w/ scarcity price shaved ($/MWh)"/vrN$"price w/ scarcity price shaved ($/MWh)"
vrN$VF_REMIND <- vrN$"REMIND Market value ($/MWh)"/vrN$"REMIND whole sale electricity price ($/MWh)"

# if DIETER MV is larger than DIETER avg LCOE
DT_MV_ge_LCOE <- (vrN$"DIETER Market value ($/MWh)" >= vrN$"DIETER LCOE_avg ($/MWh)")
vrN$DT_MV_ge_LCOE <- DT_MV_ge_LCOE

# if (run_number == run_number_uncoupl){
#   vrN$run <- "uncoupled run"
# }

# if (run_number == run_number_full){
  vrN$run <- "full coupled run"
# }

vrN2 <- vrN[, c("iter",	"period",	"tech", "rlf", "REMIND CapFac (%)",	"REMIND CapFac grade (%)", "REMIND Capacity Shadow Price ($/MWh)","REMIND Revenue (billionUSD)","REMIND whole sale electricity price ($/MWh)" ,"REMIND Market value ($/MWh)","REMIND Market price ($/MWh)",	"REMIND Markup ($/MWh)", "REMIND Subsidy ($/MWh)", 
                "genshare",
                "genshares (%)",
                # "DIETER Value factor (%)", "VF_REMIND", 
                "REMIND LCOE ($/MWh)","Marginal adjustment cost ($/MWh)",
                "REMIND LCOE grade ($/MWh)", "REMIND VRE distributed capacities (GW)", "total_MV","total_cost", "RM_MV_ge_LCOE","RM_value_ge_cost", "REMIND added capacities (GW)", "REMIND pre-investment capacities (GW)","DIETER avg CapFac (%)",	"DIETER marg CapFac (%)","DIETER LCOE_avg ($/MWh)",	
                "DIETER LCOE_marg ($/MWh)",
                "DIETER Revenue marginal plant (millionUSD)", 
                "load-weighted price for fixed demand ($/MWh)", 
                "price w/ scarcity price shaved ($/MWh)",
                "DIETER Market value ($/MWh)",
                "DIETER Market value w/ scarcity price shaved ($/MWh)",
                # "VF_DIETER_NonShaved",
                # "VF_DIETER_Shaved",
                # "DIETER Marginal market value ($/MWh)",	"DIETER Revenue (billionUSD)",
                "DT_MV_ge_LCOE","minTech_yesno",	"DIETER added capacities (GW)","run")]

vrN_final <- vrN2 %>% 
  dplyr::rename("REMIND market value >= LCOE" = "RM_MV_ge_LCOE") %>% 
  dplyr::rename("REMIND total value >= total cost" = "RM_value_ge_cost") %>% 
  dplyr::rename("REMIND total value (wholesale+cap)" = "total_MV") %>% 
  dplyr::rename("REMIND total cost (LCOE+adjCost)" = "total_cost") %>% 
  dplyr::rename("DIETER market value >= average LCOE" = "DT_MV_ge_LCOE") %>% 
  dplyr::rename("tech part of min invers screening curve" = "minTech_yesno") %>% 
  dplyr::rename("REMIND Generation share (%)" = "genshare") %>% 
  dplyr::rename("DIETER Generation share (%)" = "genshares (%)") 
# %>% 
  # dplyr::rename("DIETER value factor passed to REMIND" = "DIETER Value factor (%)") %>% 
  # dplyr::rename("REMIND value factor" = "VF_REMIND") %>% 
  # dplyr::rename("DIETER value factor (not shaved)" = "VF_DIETER_NonShaved") %>% 
  # dplyr::rename("DIETER value factor (shaved)" = "VF_DIETER_Shaved") 
  

vrN_final[is.na(vrN_final)] <- ""

  A <- function(col){
    # col = vrN_final$tech
    # col = vrN_final$"REMIND LCOE ($/MWh)"
    # col = vrN_final$"REMIND market value >= LCOE"
    if (length(col[[1]])>0 & str_detect(col, "[0-9]+")){
    col <- ifelse(as.numeric(col)>10, round(as.numeric(col), digits = 1), col)
    col <- ifelse(as.numeric(col)>1 & as.numeric(col)<10, round(as.numeric(col), digits = 1), col)
    col <- ifelse(as.numeric(col)<1, round(as.numeric(col), digits = 2), col)

    }
    
    col[is.na(col)] <- ""
    
    return(col)
  }
Ncol = ncol(vrN_final)

vrN_final[,4:Ncol] <- data.frame(lapply(vrN_final[,4:Ncol], A) )

vrN_final <- vrN_final[order(vrN_final$period),]
vrN_final <- vrN_final[order(vrN_final$iter),]
write.table(vrN_final, paste0(mypath, "validation_table_", run_number, "_iter=", iter_tovalid_RM[[1]],".xls"), sep = ";", row.names = F)

