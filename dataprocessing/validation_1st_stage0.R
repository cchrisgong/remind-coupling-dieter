#for revenue and market value, produces cvs table of marginal and annual average revenue and market value

mypath = "~/remind/dataprocessing/"

#uncoupled REMIND run
run_number_uncoupl = "21_uncoupl"
#fully coupled REMIND run (in validation mode, with capfac & capacity constraint)
run_number_full = "32"
#partially coupled REMIND run (only with capfac, no capacity constraint)
run_number_partial = "31"

# import library
source(paste0(mypath, "library_import.R"))
myDIETERPLOT_path = "~/remind/dataprocessing/DIETER_plots/"
source(paste0(myDIETERPLOT_path, "GDXtoQuitte.R"))
library(readr)
require(ggplot2)
require(lusweave)
require(rmndt)
igdx("/opt/gams/gams30.2_linux_x64_64_sfx")

run_number_list = c(run_number_uncoupl, run_number_full,run_number_partial)

for(run_number in run_number_list){
  # run_number = "32" 
  mydatapath =  paste0("~/remind/output/capfac", run_number, "/") 
  #dieter output iteration gdx files
  files_DT_rep <- list.files(mydatapath, pattern="report_DIETER_i[0-9]+\\.gdx")
  sorted_files_DT_rep <- paste0(mydatapath, "report_DIETER_i", seq(from = 5, to = length(files_DT_rep)*5, by = 5), ".gdx")
  #remind output iteration gdx files
  files <- list.files(mydatapath, pattern="fulldata_[0-9]+\\.gdx")
  sorted_files <- paste0(mydatapath, "fulldata_", 1:length(files), ".gdx")
  
  for(fname in files_DT_rep){
    gdxToQuitte_annual(mydatapath, fname, run_number)
  }

sorted_annual_report_DT <- paste0(myDIETERPLOT_path, "capfac", run_number, "_i", seq(from = 5, to = length(files_DT_rep)*5, by = 5), "_annualreport.csv")

TECH_report_keylst_DT = c("CCGT", "Lignite", "Solar PV", "Wind_on", "Biomass", "OCGT_eff", "Run-of-River", "Nuclear", "Hard coal")
plot_DTte_names = c("combined cycle gas", "lignite", "solar", "wind", "biomass", "open cycle gas turbine", "hydro", "nuclear", "hard coal")
plot_RMte_names = c("combined cycle gas", "coal", "solar", "wind", "biomass", "open cycle gas turbine", "hydro", "nuclear")

VAR_report_key1_DT = c("Market value", "marginal capacity factor","avg capacity factor", "DIETER LCOE_avg","Revenue marginal plant per MW")

# |year|iter|tech|CF|LCOE_REMIND|MV_RM|LCOE_avg_DIETER|LCOE_marg_DIETER|MV_DT|Revenue_REMIND|Revenue_avg_DIETER|Revenue_marg_DIETER
# |Shadow_price|Inv_REMIND|Inv_DIETER|run

year_toplot = c(2025,2035,2050)
iter_toplot = c(6,11,31)
iter_toplot_DT = c(5,10,30)

BUDGETkey1 = "qm_budget"
VARkey1 = "q32_balSe"
CFkey1 = "vm_capFac"
CFkey2 = "pm_dataren"
CFkey3 = "vm_capDistr"

CapConstraintKey = "q32_peakDemand_DT"
GENkey1 = "vm_prodSe"
ProdKEY = "seel"
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
TECHVREkeylst <- c(TECHkeylst_solar, TECHkeylst_wind,TECHkeylst_hydro)
TECH_NONVRE_keylst <- c(TECHkeylst_peakGas, TECHkeylst_nonPeakGas, TECHkeylst_coal, TECHkeylst_biomass, TECHkeylst_nuclear)
#=========================================================================================================
#=========================================================================================================
#=========================================================================================================

get_PRICEvariable <- function(iteration){
  gdx = sorted_files[[iteration]]
  budgetdata <- read.gdx(gdx, BUDGETkey1,field="m") %>% 
    filter(ttot%in% year_toplot) %>%
    filter(all_regi == REGIkey1) %>% 
    mutate(m = -m) %>% 
    dplyr::rename(budget = m)
  
  vrdata0 <- read.gdx(gdx, VARkey1,field="m") %>% 
    filter(ttot%in% year_toplot) %>%
    filter(all_regi == REGIkey1) 
  
  vrdata = list(vrdata0, budgetdata) %>%
    reduce(full_join) %>%
    mutate(m = -m/ budget * 1e12 / sm_TWa_2_MWh * 1.2) %>% 
    dplyr::rename(value = m) %>% 
    select(ttot, all_regi,value) %>% 
    dplyr::rename(period = ttot)
    
  return(vrdata)
}

#=========================================================================================================

get_SHADOWPRICE <- function(iteration){
  # iteration = 10
  gdx = sorted_files[[iteration]]
  
  budgetdata <- read.gdx(gdx, BUDGETkey1,field="m") %>% 
    dplyr::rename(period = ttot)  %>% 
    filter(period %in%year_toplot) %>%
    filter(all_regi == REGIkey1) %>% 
    mutate(m = -m) %>% 
    dplyr::rename(budget = m)  %>% 
    select(period, budget)
  
  capcondata1 <- read.gdx(gdx, CapConstraintKey,field="m") %>% 
    dplyr::rename(period = tall)  %>% 
    filter(period %in%year_toplot) %>%
    mutate(m = -m) %>% 
    dplyr::rename(cap_ShadowPrice = m)
  
  # transform from tr$2005/TW to $2015/kW
  capcondata = list(capcondata1, budgetdata) %>%
    reduce(full_join) %>%
    select(period, cap_ShadowPrice, budget) %>% 
    replace(is.na(.), 0) %>% 
    mutate(cap_ShadowPrice = cap_ShadowPrice/ budget * 1e12 / 1e9 * 1.2) %>% 
    select(period, cap_ShadowPrice)
    
  CFdata <- read.gdx(gdx, CFkey1,field="l") %>% 
    dplyr::rename(period = ttot)  %>% 
    filter(period %in%year_toplot) %>% 
    filter(all_regi == REGIkey1) %>% 
    filter(all_te %in% TECH_NONVRE_keylst) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_biomass[[1]], plot_RMte_names[[5]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_biomass[[2]], plot_RMte_names[[5]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_biomass[[3]], plot_RMte_names[[5]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_nonPeakGas[[1]], plot_RMte_names[[1]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_nonPeakGas[[2]], plot_RMte_names[[1]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_nonPeakGas[[3]], plot_RMte_names[[1]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_coal[[1]], plot_RMte_names[[2]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_coal[[2]], plot_RMte_names[[2]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_coal[[3]], plot_RMte_names[[2]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_coal[[4]], plot_RMte_names[[2]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_coal[[5]], plot_RMte_names[[2]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_coal[[6]], plot_RMte_names[[2]])) %>% 
    # mutate(all_te = str_replace(all_te, TECHkeylst_solar[[1]], plot_RMte_names[[3]])) %>% 
    # mutate(all_te = str_replace(all_te, TECHkeylst_wind[[1]], plot_RMte_names[[4]])) %>%
    mutate(all_te = str_replace(all_te, TECHkeylst_nuclear[[1]], plot_RMte_names[[8]])) %>%
    # mutate(all_te = str_replace(all_te, TECHkeylst_hydro[[1]], plot_RMte_names[[7]])) %>%
    mutate(all_te = str_replace(all_te, TECHkeylst_peakGas[[1]], plot_RMte_names[[6]])) %>% 
    # mutate(all_te = str_replace(all_te, TECHkeylst_placeholder[[1]], plot_RMte_names[[9]])) %>% 
    dplyr::group_by(period, all_te) %>%
    dplyr::summarise( value = mean(value), .groups = 'keep' ) %>% 
    dplyr::ungroup(period, all_te) %>% 
    dplyr::rename(CapFac = value)
  
  # CFdata$CapFac[CFdata$all_te == "hard coal"] <- CFdata$CapFac[CFdata$all_te == "lignite"]
  
  #2015$/kW -> 2015$/Mwh, $/kW/FLH = $/kW /( 8760h * CF) * 1e3 = $/MWh
  ShadowPrice <- list(capcondata, CFdata) %>%
    reduce(full_join) %>%
    mutate(cap_ShadowPrice = cap_ShadowPrice / (8760 * CapFac) * 1e3)
  
  # ShadowPrice$all_te <- factor(ShadowPrice$all_te, levels= c("combined cycle gas", "biomass", "open cycle gas turbine", "nuclear", "coal"))
  
  return(ShadowPrice)
}

get_GENvariable <- function(iteration){
  gdx = sorted_files[[iteration]]
  vrdata <- read.gdx(gdx, GENkey1, factors = FALSE) %>% 
    filter(tall%in% year_toplot) %>%
    filter(all_regi == REGIkey1) %>%
    filter(all_enty.1 == ProdKEY)  %>% 
    filter(all_te %in% TECHkeylst) %>% 
    mutate(value = value * sm_TWa_2_MWh) %>% 
    select(all_te, value) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_biomass[[1]], plot_RMte_names[[5]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_biomass[[2]], plot_RMte_names[[5]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_biomass[[3]], plot_RMte_names[[5]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_nonPeakGas[[1]], plot_RMte_names[[1]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_nonPeakGas[[2]], plot_RMte_names[[1]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_nonPeakGas[[3]], plot_RMte_names[[1]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_coal[[1]], plot_RMte_names[[2]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_coal[[2]], plot_RMte_names[[2]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_coal[[3]], plot_RMte_names[[2]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_coal[[4]], plot_RMte_names[[2]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_coal[[5]], plot_RMte_names[[2]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_coal[[6]], plot_RMte_names[[2]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_solar[[1]], plot_RMte_names[[3]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_nuclear[[1]], plot_RMte_names[[8]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_wind[[1]], plot_RMte_names[[4]])) %>%
    mutate(all_te = str_replace(all_te, TECHkeylst_hydro[[1]], plot_RMte_names[[7]])) %>%
    mutate(all_te = str_replace(all_te, TECHkeylst_peakGas[[1]], plot_RMte_names[[6]])) %>% 
    # mutate(all_te = str_replace(all_te, TECHkeylst_placeholder[[1]], plot_RMte_names[[9]])) %>% 
    dplyr::group_by(all_te) %>%
    dplyr::summarise( value = sum(value), .groups = 'keep' ) %>% 
    dplyr::ungroup(all_te)
  
  # vrdata$value[vrdata$all_te == "hard coal"] <- vrdata$value[vrdata$all_te == "lignite"]
  
  return(vrdata)
}

vrN_GEN0 <- lapply(iter_toplot, get_GENvariable)
vrN_PRICE0 <- lapply(iter_toplot, get_PRICEvariable)
vr1_SP0 <- lapply(iter_toplot, get_SHADOWPRICE)

idx_toplot <- 1:length(iter_toplot)

for(idx in idx_toplot){
  vrN_GEN0[[idx]]$iter <- iter_toplot[[idx]]
  vrN_PRICE0[[idx]]$iter <- iter_toplot[[idx]]
  vr1_SP0[[idx]]$iter <- iter_toplot[[idx]]
}

vr1_SP1 <- rbindlist(vr1_SP0)
vrN_GEN <- rbindlist(vrN_GEN0)
vrN_PRICE1<- rbindlist(vrN_PRICE0) 

vrN_PRICE<- vrN_PRICE1 %>% 
  select(period,value,iter) %>% 
  dplyr::rename(MarketValue_RM = value)

vrN_REV0 = list(vrN_GEN, vrN_PRICE) %>%
  reduce(full_join)

vrN_REV <- vrN_REV0 %>%
  mutate(value=value*MarketValue_RM/1e9) %>%
  dplyr::rename(Revenue_RM_billUSD = value) %>% 
  select(period,all_te,iter,Revenue_RM_billUSD,MarketValue_RM)

vrN_RM0 = list(vrN_REV, vr1_SP1) %>%
  reduce(full_join)

vrN_RM <- vrN_RM0%>% 
  mutate(CapFac=CapFac*1e2) %>%
  select(period,tech=all_te,iter,CapFac,Revenue_RM_billUSD,MarketValue_RM,cap_ShadowPrice)

##########################################
########## market value ##################

get_report_variable_DT <- function(cvs){
  # cvs = sorted_annual_report_DT[[1]]
  annual_reportCSV = read.csv(cvs, sep = ';', header = T, stringsAsFactors = F)
  annual_reportQUITT <- as.quitte(annual_reportCSV) 
  
  vrdata <- annual_reportQUITT %>% 
    filter(period%in% year_toplot) %>%
    filter(tech %in% TECH_report_keylst_DT)%>% 
    filter(variable %in% VAR_report_key1_DT) %>% 
    select(period, tech, variable, value, unit) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_DT[[1]], plot_DTte_names[[1]])) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_DT[[2]], plot_DTte_names[[2]])) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_DT[[3]], plot_DTte_names[[3]])) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_DT[[4]], plot_DTte_names[[4]])) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_DT[[5]], plot_DTte_names[[5]])) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_DT[[6]], plot_DTte_names[[6]])) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_DT[[7]], plot_DTte_names[[7]])) %>%  
    mutate(tech = str_replace(tech, TECH_report_keylst_DT[[8]], plot_DTte_names[[8]])) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_DT[[9]], plot_DTte_names[[9]])) 
  
}

vrN_rep_DTlist  <- lapply(sorted_annual_report_DT, get_report_variable_DT)
idx_toplot <- 1:length(iter_toplot)

for(idx in idx_toplot){
  vrN_rep_DTlist[[idx]]$iter <- iter_toplot[[idx]]
  vrN_PRICE0[[idx]]$iter <- iter_toplot[[idx]]
  vr1_SP0[[idx]]$iter <- iter_toplot[[idx]]
}
#===================================================
idx_DT <- 1:length(files_DT_rep)
for(id in idx_DT){
  vrN_rep_DTlist[[id]]$iter <- id*5
  vrN_rep_DTlist[[id]]$model <- "DIETER"  
}

vrN_rep_DT0 <- rbindlist(vrN_rep_DTlist)
vrN_rep_DT <- vrN_rep_DT0 %>% 
  filter(iter %in% iter_toplot_DT) %>% 
  # filter(tech %in% tech_list)  %>% 
  dplyr::rename(DIETER = value) %>% 
  select(period,tech, variable,DIETER,unit,iter)


get_LCOE <- function(iteration){
  annual_reportCSV = read.csv(paste0(myDIETERPLOT_path, "capfac", run_number, "_i", iteration, "_annualreport.csv"), sep = ';', header = T, stringsAsFactors = F)
  annual_reportQUITT <- as.quitte(annual_reportCSV)
  
  FC0 <- annual_reportQUITT %>% 
    filter(period%in% year_toplot) %>% 
    filter(variable %in% c(cost_toplot[[1]],cost_toplot[[4]])) %>% 
    filter(tech %in% TECHFCkeylst_DT) %>% 
    select(period, variable, tech, value) %>% 
    mutate(variable = str_replace(variable, " \\(divided by eta\\)", ""))
  
  gdx = sorted_files[[iteration]]
  vrdata <- read.gdx(gdx, GENkey1, factors = FALSE) %>% 
    filter(tall%in% year_toplot) %>%
    filter(all_regi == REGIkey1) %>%
    filter(all_enty.1 == ProdKEY)  %>% 
    filter(all_te %in% TECHkeylst) %>% 
    mutate(value = value * sm_TWa_2_MWh) %>% 
    select(all_te, value) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_biomass[[1]], plot_RMte_names[[5]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_biomass[[2]], plot_RMte_names[[5]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_biomass[[3]], plot_RMte_names[[5]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_nonPeakGas[[1]], plot_RMte_names[[1]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_nonPeakGas[[2]], plot_RMte_names[[1]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_nonPeakGas[[3]], plot_RMte_names[[1]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_coal[[1]], plot_RMte_names[[2]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_coal[[2]], plot_RMte_names[[2]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_coal[[3]], plot_RMte_names[[2]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_coal[[4]], plot_RMte_names[[2]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_coal[[5]], plot_RMte_names[[2]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_coal[[6]], plot_RMte_names[[2]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_solar[[1]], plot_RMte_names[[3]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_nuclear[[1]], plot_RMte_names[[8]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_wind[[1]], plot_RMte_names[[4]])) %>%
    mutate(all_te = str_replace(all_te, TECHkeylst_hydro[[1]], plot_RMte_names[[7]])) %>%
    mutate(all_te = str_replace(all_te, TECHkeylst_peakGas[[1]], plot_RMte_names[[6]])) %>% 
    dplyr::group_by(all_te) %>%
    dplyr::summarise( value = sum(value), .groups = 'keep' ) %>% 
    dplyr::ungroup(all_te)
  
  #=====================================================

  CAP <- annual_reportQUITT %>% 
    filter(period%in% year_toplot) %>% 
    filter(variable == "Installed capacity") %>% 
    select(period, value, tech)%>% 
    filter(tech %in% TECH_report_keylst_DT) %>% 
    mutate(value = value * 1e6)%>% #GW->kW
    dplyr::rename(cap = value) 
  
  OM0 <- annual_reportQUITT %>% 
    filter(period%in% year_toplot) %>% 
    filter(variable == cost_toplot[[2]]) %>% 
    filter(tech %in% TECH_report_keylst_DT) %>% 
    select(period, variable, tech, value)
  
  OM <- OM0 %>% 
    left_join(CAP) %>% 
    left_join(GEN) %>% 
    mutate(value = value * cap /gen ) %>% 
    replace(is.na(.), 0) %>% 
    select(period, variable, tech, value)
  
  IC0 <- annual_reportQUITT %>% 
    filter(period%in% year_toplot) %>% 
    filter(tech %in% TECH_report_keylst_DT) %>% 
    filter(variable == cost_toplot[[3]]) %>% 
    select(period, variable, tech, value)
  
  IC <- IC0 %>% 
    left_join(CAP) %>% 
    left_join(GEN) %>% 
    mutate(value = value * cap / gen) %>%
    replace(is.na(.), 0) %>% 
    select(period, variable, tech, value)
  
  COST = list(FC, OM, IC, CO2) %>% 
    reduce(full_join)
  
  COST$variable <- factor(COST$variable, levels= c("Annualized investment cost","O&M cost","fuel cost", "CO2 cost"))
  
  # COST$iter <- iteration
  
  COST_total <- COST %>% 
    select(period, tech, value) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_DT[[1]], plot_DTte_names[[1]])) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_DT[[2]], plot_DTte_names[[2]])) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_DT[[3]], plot_DTte_names[[3]])) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_DT[[4]], plot_DTte_names[[4]])) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_DT[[5]], plot_DTte_names[[5]])) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_DT[[6]], plot_DTte_names[[6]])) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_DT[[7]], plot_DTte_names[[7]])) %>%  
    mutate(tech = str_replace(tech, TECH_report_keylst_DT[[8]], plot_DTte_names[[8]])) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_DT[[9]], plot_DTte_names[[9]])) %>% 
    dplyr::group_by(period, tech) %>%
    dplyr::summarise( value = sum(value), .groups = 'keep' ) %>% 
    dplyr::ungroup(period, tech) %>% 
    dplyr::rename(LCOE = value) 
  
  # any( sapply(COST_total, is.infinite) )
  # COST_total$unit <- "USD/MWh"
  # COST_total$variable <- "LCOE"
  COST_total$iter <- iteration
  
  
  return(COST_total)
  
}

COST_list <- lapply(iter_toplot, get_LCOE)

COST_output <- rbindlist(COST_list)

vrN = list(vrN_RM, COST_output) %>%
  reduce(full_join)  


vrN$variable <- factor(vrN$variable, levels= c("Annualized Cost", "Revenue", "Market value", "avg capacity factor", "Marginal Revenue", "Revenue marginal plant per MW", "marginal market value", "marginal capacity factor"))

write.table(vrN , paste0(mypath, run_number, "_revenue_mv_cf_dieter.xls"), sep = ";", row.names = F)

