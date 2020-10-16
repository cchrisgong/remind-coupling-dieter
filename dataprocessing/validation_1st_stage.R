#for revenue and market value, produces cvs table of marginal and annual average revenue and market value

mypath = "~/remind/dataprocessing/"

#fully coupled REMIND run (in validation mode, with capfac & capacity constraint)
run_number_full = "oldbranch"
# run_number_full = "32_valid1"
#partially coupled REMIND run (only with capfac, no capacity constraint)
run_number_partial = "32_valid2"
#uncoupled REMIND run
run_number_uncoupl = "32_valid3"

# import library
source(paste0(mypath, "library_import.R"))
myDIETERPLOT_path = "~/remind/dataprocessing/DIETER_plots/"
source(paste0(myDIETERPLOT_path, "GDXtoQuitte.R"))
library(readr)
require(ggplot2)
require(lusweave)
require(plyr)
require(rmndt)
igdx("/opt/gams/gams30.2_linux_x64_64_sfx")

# run_number_list = c(run_number_full,run_number_partial, run_number_uncoupl)
run_number_list = c(run_number_full)

# runtable <- function(run_number){
run_number = run_number_full
# run_number = run_number_partial
# run_number = run_number_uncoupl

mydatapath =  paste0("~/remind/output/", run_number, "/") 
#dieter output iteration gdx files
files_DT_rep <- list.files(mydatapath, pattern="report_DIETER_i[0-9]+\\.gdx")
sorted_files_DT_rep <- paste0(mydatapath, "report_DIETER_i", seq(from = 5, to = length(files_DT_rep)*5, by = 5), ".gdx")
# sorted_files_DT <- paste0(mydatapath, "results_DIETER_i", seq(from = 2, to = length(files_DT), by = 1), ".gdx")

#remind output iteration gdx files
files <- list.files(mydatapath, pattern="fulldata_[0-9]+\\.gdx")
sorted_files <- paste0(mydatapath, "fulldata_", 1:length(files), ".gdx")
# 
# for(fname in files_DT_rep){
# gdxToQuitte_annual(mydatapath, fname, run_number)
# }

sorted_annual_report_DT <- paste0(myDIETERPLOT_path, "capfac", run_number, "_i", seq(from = 5, to = length(files_DT_rep)*5, by = 5), "_annualreport.csv")
# sorted_annual_report_DT <- paste0(myDIETERPLOT_path, "capfac", run_number, "_i", seq(from = 2, to = length(files_DT_rep), by = 1), "_annualreport.csv")

TECH_report_keylst_DT = c("coal", "CCGT", "Lignite", "Solar PV", "Wind_on", "Biomass", "OCGT_eff", "Run-of-River", "Nuclear", "Hard coal")
plot_DTte_names = c("coal","combined cycle gas", "lignite", "solar", "wind", "biomass", "open cycle gas turbine", "hydro", "nuclear", "hard coal")
table_ordered_name = c("coal", "lignite", "hard coal","combined cycle gas", "solar", "wind", "biomass", "open cycle gas turbine", "hydro", "nuclear")
plot_RMte_names = c("combined cycle gas", "coal", "solar", "wind", "biomass", "open cycle gas turbine", "hydro", "nuclear")
plot_RMLCOEte_names = c("combined cycle gas", "lignite", "solar", "wind", "biomass", "open cycle gas turbine", "hydro", "nuclear")
Dispatch_RMte_names = c("combined cycle gas", "coal", "biomass", "open cycle gas turbine", "nuclear")
Dispatch_DTte_names = c("combined cycle gas", "lignite", "biomass", "open cycle gas turbine", "nuclear", "hard coal")
VRE_RMte_names = c("solar", "wind", "hydro")

VAR_report_key1_DT = c("DIETER avg CapFac (%)","DIETER LCOE_avg ($/MWh)","DIETER marg CapFac (%)","DIETER LCOE_marg ($/kWh)","DIETER Market value ($/MWh)","Marginal market value ($/MWh)","DIETER Revenue (billionUSD)","DIETER Revenue marginal plant ($/MW)", "DIETER added capacities (GW)", "REMIND pre-investment capacities")

#this is the same as the REMIND capfac from iter 5N-1
VAR_report_key2_DT = c("REMIND CapFac (%)")

#this is the same as the REMIND capfac from iter 5N, which is needed for cap.con. shadow price calculation
VAR_report_key3_DT = c("DIETER avg CapFac (%)")

# relevant cost parameters to be loaded from DIETER reporting
VAR_report_key4_DT = c("Annualized investment cost","O&M cost","fuel cost (divided by eta)", "CO2 cost")

# |year|iter|tech|^ CF_REMIND|x LCOE_REMIND|^ MV_RM|x CF_DIETER|x LCOE_avg_DIETER|x LCOE_marg_DIETER|x MV_DT|^ Revenue_REMIND|x Revenue_avg_DIETER|
# |x Revenue_marg_DIETER |^ Shadow_price| x Inv_REMIND| xInv_DIETER|run

year_total = c(2010,2015,2020,2025,2030,2035,2040,2045,2050,2055,2060,2070,2080,2090,2100)
# =======================================
# group I validation
# year_toplot = c(2040,2045,2050,2055)
# year_toplot_before = c(2035,2040,2045,2050)
# year_toplot_after = c(2045,2050,2055,2060)
year_toplot = year_total
year_toplot_before = c(2000,2005,2010,2015,2020,2025,2030,2035,2040,2045,2050,2055,2060,2070,2080)
year_toplot_before = c(2005,2010,2015,2020,2025,2030,2035,2040,2045,2050,2055,2060,2070,2080,2090)
year_toplot_after = c(2015,2020,2025,2030,2035,2040,2045,2050,2055,2060,2070,2080,2090,2100,2110)

iter_toplot_RM = c(5)
#5N-1 iter
# iter_toplot_RM4 = c(14,29)
#5N iter
# iter_toplot_RM5 = c(15,30)

iter_toplot_DT = c(5)

# =======================================
# # group II validation
# year_toplot = c(2020,2025,2030,2035,2040,2045,2050,2055)
# iter_toplot_RM = c(14,15,29,30)
# #5N-1 iter
# iter_toplot_RM4 = c(14,29)
# #5N iter
# iter_toplot_RM5 = c(15,30)
# 
# iter_toplot_DT = c(15,30)

BUDGETkey1 = "qm_budget"
VARkey1 = "q32_balSe"
CFkey1 = "vm_capFac"
CFkey2 = "pm_dataren"
CFkey3 = "vm_capDistr"
grade = "1"

InvAdjKEY = "v_costInvTeAdj"

CapConstraintKey = "q32_peakDemand_DT"
GENkey1 = "vm_prodSe"
CAPkey1 = "vm_cap"
CAPkey2 = "vm_deltaCap"
PM_TS_Key = "pm_ts"
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

######################################################
########## unpack reporting from DIETER ##############


get_report_variable_DT <- function(iteration, varlist){
  # iteration = 15
  cvs = sorted_annual_report_DT[[iteration/5]]
  # varlist =VAR_report_key1_DT
  annual_reportCSV = read.csv(cvs, sep = ";", header = T, stringsAsFactors = F)
  annual_reportQUITT <- as.quitte(annual_reportCSV) 
  
  vrdata <- annual_reportQUITT %>% 
    filter(period %in% year_toplot) %>% 
    filter(tech %in% TECH_report_keylst_DT) %>% 
    filter(variable %in% varlist) %>% 
    select(period, tech, variable, value, unit) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_DT[[1]], plot_DTte_names[[1]])) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_DT[[2]], plot_DTte_names[[2]])) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_DT[[3]], plot_DTte_names[[3]])) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_DT[[4]], plot_DTte_names[[4]])) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_DT[[5]], plot_DTte_names[[5]])) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_DT[[6]], plot_DTte_names[[6]])) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_DT[[7]], plot_DTte_names[[7]])) %>%  
    mutate(tech = str_replace(tech, TECH_report_keylst_DT[[8]], plot_DTte_names[[8]])) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_DT[[9]], plot_DTte_names[[9]])) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_DT[[10]], plot_DTte_names[[10]])) 

  vrdata$iter = iteration
  
  return(vrdata)  
}

vrN_rep_DTlist  <- lapply(iter_toplot_RM, get_report_variable_DT, varlist=VAR_report_key1_DT)

#===================================================

vrN_rep_DT0 <- rbindlist(vrN_rep_DTlist)
vrN_rep_DT <- vrN_rep_DT0 %>% 
  select(period,tech, variable,value,iter)

vrN_rep = spread(vrN_rep_DT, variable, value)

vrN_rep$"DIETER added capacities (GW)"[is.na(vrN_rep$"DIETER added capacities (GW)")] <- 0
vrN_rep$"DIETER Revenue marginal plant ($/MW)"[is.na(vrN_rep$"DIETER Revenue marginal plant ($/MW)")] <- 0

#============REMIND capacity constraint shadow price ===========================================================

get_CAPCON <- function(iteration){
  # iteration = 15
  gdx = sorted_files[[iteration]]
  
  budgetdata <- read.gdx(gdx, BUDGETkey1,field="m") %>% 
    dplyr::rename(period = ttot)  %>% 
    filter(period %in%year_toplot) %>%
    filter(all_regi == REGIkey1) %>% 
    mutate(m = -m) %>% 
    dplyr::rename(budget = m) %>% 
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
  
  capcondata$iter = iteration
  
  return(capcondata)
}

vrN_RMcapfac_list  <- lapply(iter_toplot_RM, get_report_variable_DT, varlist=VAR_report_key2_DT)

#===================================================

# capfac----------------------------
RM_capfac0 <- rbindlist(vrN_RMcapfac_list)
RM_capfac1 <- RM_capfac0 %>% 
  select(period,tech,variable,value,iter) %>% 
  dplyr::rename(CapFac = value)

RM_capfac2 = spread(RM_capfac1, variable, CapFac)

#dispacthable CF
RM_capfac_disp <- RM_capfac2 %>% 
  mutate(tech = str_replace(tech, "hard coal", "coal")) %>% 
  filter(tech %in% Dispatch_RMte_names)

#------------------calculate shadow price using 5th iter capfac -----------------------
if (run_number != run_number_uncoupl){
vr1_capcon_list <- lapply(iter_toplot_RM, get_CAPCON)
vr1_capcon <- rbindlist(vr1_capcon_list)

RM_capfac4ShadowPrice <- RM_capfac_disp %>% 
  dplyr::rename(CapFac = "REMIND CapFac (%)")

# #2015$/kW -> 2015$/Mwh, $/kW/FLH = $/kW /( 8760h * CF) * 1e3 = $/MWh, CF in unit %, CF% -> CF /1e2

ShadowPrice <- list(vr1_capcon, RM_capfac4ShadowPrice) %>%
  reduce(full_join) %>% 
  mutate(cap_ShadowPrice = cap_ShadowPrice / (8760 * CapFac/1e2) * 1e3) %>% 
  select(period, iter, tech, cap_ShadowPrice) %>% 
  dplyr::rename("REMIND Capacity Shadow Price ($/MWh)" = cap_ShadowPrice)
}
#========================default REMIND VRE capfac =====================================

get_CAPFAC_variable <- function(iteration){
  # iteration = 15
  gdx = sorted_files[[iteration]]
  
  vrdata <- read.gdx(gdx, CFkey1,field="l") %>% 
    filter(ttot%in%year_toplot) %>% 
    filter(all_regi == REGIkey1) %>%
    filter(all_te %in% TECHkeylst) %>% 
    filter(!(all_te %in% TECHVREkeylst)) %>% 
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
    mutate(all_te = str_replace(all_te, TECHkeylst_nuclear[[1]], plot_RMte_names[[8]])) %>%
    mutate(all_te = str_replace(all_te, TECHkeylst_peakGas[[1]], plot_RMte_names[[6]])) %>% 
    dplyr::group_by(ttot, all_te) %>%
    dplyr::summarise( value = mean(value), .groups = "keep" ) %>% 
    dplyr::ungroup(ttot, all_te) %>% 
    dplyr::rename(tall = ttot) 
    
    
  # pm_dataren("nur") = capacity factor at different grade levels (quality of wind resources), pm_dataren is used to represent different cost levels necessary to create the same amount of energy if you have to build your wind farm in places with lower wind. The grades are ordered from 1 to 10. 1 is closer to the wind always blowing (highest nur capacity factor), 10 is the worst place to install wind.
  dataren <- read.gdx(gdx, CFkey2) %>% 
    filter(char == "nur") %>%
    filter(all_regi == REGIkey1) %>%
    filter(all_te %in% TECHVREkeylst) %>% 
    dplyr::rename(ren_nur = value) %>% 
    select(all_te, rlf, ren_nur)
  
  #vm_capDistr is a "helper" variable that stores the amount of capacity that REMIND assign to each grade. Therefore it can have values from any grade. vm_cap is the total capacity (sum of vm_capDistr). It does not have grade infor anymore, everything is assigned to "1". vm_capDistr will retain the information of the optimal grades used in the REMIND decision
  percentage_cap_distr <- read.gdx(gdx, CFkey3) %>% 
    filter(tall %in% year_toplot) %>%
    filter(all_regi == REGIkey1) %>%
    filter(all_te %in% TECHVREkeylst) %>% 
    dplyr::rename(cap_distr = value)  %>% 
    select(tall, all_te, rlf, cap_distr) %>% 
    dplyr::group_by(tall, all_te) %>%
    transmute(rlf, percentage_cap_distr = cap_distr/sum(cap_distr))
    
  vrdata2_0 = list(dataren, percentage_cap_distr) %>%
    reduce(right_join)
  
  vrdata2 <- vrdata2_0 %>% 
    select(tall, all_te, ren_nur,percentage_cap_distr) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_solar[[1]], plot_RMte_names[[3]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_wind[[1]], plot_RMte_names[[4]])) %>%
    mutate(all_te = str_replace(all_te, TECHkeylst_hydro[[1]], plot_RMte_names[[7]])) %>%
    dplyr::group_by(tall, all_te) %>%
    dplyr::summarise(value = sum(ren_nur * percentage_cap_distr), .groups = "keep" ) %>% 
    dplyr::ungroup(tall, all_te)
  
  vrdata_tot <- list(vrdata, vrdata2) %>% 
    reduce(full_join) %>% 
    dplyr::rename(period = tall) %>% 
    dplyr::rename(tech = all_te)
    
  vrdata_tot$iter <- iteration
  
  return(vrdata_tot)
}

vr1_capfac_RMdef0 <- lapply(iter_toplot_RM, get_CAPFAC_variable)
vr1_capfac_RMdef0 <- rbindlist(vr1_capfac_RMdef0)
  
RM_capfac <- vr1_capfac_RMdef0 %>% 
  mutate(value = round(value * 1e2, 2)) %>% 
  dplyr::rename("REMIND CapFac (%)" = value)

# vr1_capfac_RMVRE <- vr1_capfac_RMdef %>% 
#   filter(tech %in% VRE_RMte_names)
# 
# period <- RM_capfac_disp_iter4$period
# 
# RM_capfac_aux <- vr1_capfac_RMVRE %>% 
#   expand(vr1_capfac_RMVRE, period)
# 
# RM_capfac_disp_iter5 <-  RM_capfac_disp %>% 
#   dplyr::rename("REMIND CapFac (%)" = "DIETER avg CapFac (%)")

# RM_capfac <- list(RM_capfac_aux, RM_capfac_disp_iter4, RM_capfac_disp_iter5) %>%
  # reduce(full_join)
#===================end of REMIND iter 4 and 5 capfac ===================================
  

#==========================REMIND market value and revenue============================================
#======================================================================
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
  
  vrdata$iter <- iteration
  
  return(vrdata)
}

get_GENvariable <- function(iteration){
  # iteration = 4
  gdx = sorted_files[[iteration]]
  vrdata <- read.gdx(gdx, GENkey1, factors = FALSE) %>% 
    filter(tall%in% year_toplot) %>% 
    filter(all_regi == REGIkey1) %>%
    filter(all_enty.1 == ProdKEY)  %>% 
    filter(all_te %in% TECHkeylst) %>% 
    mutate(value = value * sm_TWa_2_MWh) %>% 
    dplyr::rename(period = tall) %>% 
    select(period,all_te, value) %>% 
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
    dplyr::rename(tech = all_te) %>% 
    dplyr::group_by(period,tech) %>%
    dplyr::summarise( value = sum(value), .groups = "keep" ) %>% 
    dplyr::ungroup(period,tech)
  
  vrdata$iter <- iteration
  
  return(vrdata)
}

vrN_GEN0 <- lapply(iter_toplot_RM, get_GENvariable)
vrN_PRICE0 <- lapply(iter_toplot_RM, get_PRICEvariable)
vrN_GEN <- rbindlist(vrN_GEN0)
vrN_PRICE1<- rbindlist(vrN_PRICE0) 

vrN_PRICE <- vrN_PRICE1 %>% 
  select(period,value,iter) %>% 
  dplyr::rename(MarketValue_RM = value)

vrN_REV <- list(vrN_GEN, vrN_PRICE) %>%
  reduce(full_join) %>%
  mutate(value=round(value*MarketValue_RM/1e9,2)) %>%
  mutate(MarketValue_RM=round(MarketValue_RM,2)) %>%
  dplyr::rename(Revenue_RM_billUSD = value) %>% 
  select(period,tech,iter,Revenue_RM_billUSD,MarketValue_RM)%>% 
  dplyr::rename("REMIND Market value ($/MWh)" = MarketValue_RM) %>% 
  dplyr::rename("REMIND Revenue (billionUSD)" = Revenue_RM_billUSD)

#==========================REMIND LCOE =============================================
#first load cost breakdown (parameters) from DIETER
vrN_RMcost_bkdw0  <- lapply(iter_toplot_RM, get_report_variable_DT, varlist=VAR_report_key4_DT)
vrN_RMcost_bkdw1 <- rbindlist(vrN_RMcost_bkdw0)

RMcost_bkdw1 <- vrN_RMcost_bkdw1 %>% 
  select(period, tech, variable, value,iter)

RMcost_bkdw2 = spread(RMcost_bkdw1, variable, value)

RMcost_bkdw3 <- RMcost_bkdw2 %>% 
  replace(is.na(.), 0) %>% 
  filter(tech %in% plot_RMLCOEte_names) %>% 
  mutate(tech = str_replace(tech, "lignite", "coal"))

# then load capacity from REMIND fulldata

get_CAPvariable <- function(iteration, CAPkey){
  # iteration = 4
  # CAPkey = CAPkey1
  gdx = sorted_files[[iteration]]
  
  vrdata <- read.gdx(gdx, CAPkey, factors = FALSE) %>% 
    filter(tall %in% year_toplot) %>%
    filter(all_regi == REGIkey1) %>%
    filter(all_te %in% TECHkeylst) %>%
    filter(rlf == grade) %>% 
    mutate(value = value * 1e6) %>%  # cap: TW -> MW
    select(tall, all_te, value) %>% 
    dplyr::rename(period = tall) %>% 
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
    mutate(all_te = str_replace(all_te, TECHkeylst_wind[[1]], plot_RMte_names[[4]])) %>%
    mutate(all_te = str_replace(all_te, TECHkeylst_nuclear[[1]], plot_RMte_names[[8]])) %>%
    mutate(all_te = str_replace(all_te, TECHkeylst_hydro[[1]], plot_RMte_names[[7]])) %>%
    mutate(all_te = str_replace(all_te, TECHkeylst_peakGas[[1]], plot_RMte_names[[6]])) %>% 
    dplyr::rename(tech = all_te) %>% 
    dplyr::group_by(period, tech) %>%
    dplyr::summarise( value = sum(value) , .groups = "keep" ) %>% 
    dplyr::ungroup(period, tech) %>% 
    dplyr::rename(cap = value) 
  
  vrdata$iter <- iteration
  return(vrdata)
}

vrN_CAP0 <- lapply(iter_toplot_RM, get_CAPvariable, CAPkey = CAPkey1)
vrN_CAP0 <- rbindlist(vrN_CAP0)

vrN_GEN_4LCOE <- vrN_GEN %>% 
  select(period, tech, value,iter) %>% 
  dplyr::rename(gen = value) 

# investment adjustment cost
get_InvAdjvariable <- function(iteration, years){
  # years = year_total
  # iteration = 5
  
  gdx = sorted_files[[iteration]]
  # ajustment cost
  AdjInvdata <- read.gdx(gdx, InvAdjKEY, field = "l",squeeze = FALSE) %>% 
    filter(tall %in% years) %>%
    filter(all_regi == REGIkey1) %>% 
    filter(all_te %in% TECHkeylst) %>% 
    mutate(value = value * 1e6 * 1.2) %>% # conversion from tr USD 2010/TW to USD2015/MW
    dplyr::rename(C_AdjInv = value) %>% 
    select(tall,all_te,C_AdjInv)

  deltacap <- read.gdx(gdx, "vm_deltaCap", field = "l",squeeze = FALSE) %>% 
    filter(tall %in% c(2005,years)) %>%
    filter(rlf =="1") %>% 
    filter(all_regi == REGIkey1) %>% 
    filter(all_te %in% TECHkeylst) %>% 
    mutate(value = value * 1e6) %>% # conversion from TW to MW 
    select(tall,all_te,value) %>% 
    group_by(tall) %>%
    arrange(all_te) %>%
    mutate(diff = value - lag(value, default = first(value))) %>% 
    dplyr::rename(diff_deltacap = diff) 
  
  Cost_Adj <- list(deltacap, AdjInvdata) %>% 
    reduce(right_join) %>% 
    filter(tall %in% years) %>% 
    mutate(d_C_InvAdj_d_deltacap = 2 *C_AdjInv /diff_deltacap) %>% 
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
    mutate(all_te = str_replace(all_te, TECHkeylst_wind[[1]], plot_RMte_names[[4]])) %>%
    mutate(all_te = str_replace(all_te, TECHkeylst_nuclear[[1]], plot_RMte_names[[8]])) %>%
    mutate(all_te = str_replace(all_te, TECHkeylst_hydro[[1]], plot_RMte_names[[7]])) %>%
    mutate(all_te = str_replace(all_te, TECHkeylst_peakGas[[1]], plot_RMte_names[[6]])) %>% 
    select(tall, all_te, d_C_InvAdj_d_deltacap) %>% 
    dplyr::group_by(tall, all_te) %>%
    dplyr::summarise( d_C_InvAdj_d_deltacap = sum(d_C_InvAdj_d_deltacap) , .groups = "keep" ) %>% 
    dplyr::ungroup(tall, all_te) %>% 
    dplyr::rename(tech = all_te) %>% 
    dplyr::rename(period = tall) %>% 
    dplyr::rename(Adjcost = d_C_InvAdj_d_deltacap) 
  
  # Cost_Adj[is.nan(Cost_Adj)] <- 0  
  
  Cost_Adj$iter <- iteration
  return(Cost_Adj)
}

vrN_InvAdjcost <- lapply(iter_toplot_RM, get_InvAdjvariable, years = year_toplot)
vrN_InvAdjcost <- rbindlist(vrN_InvAdjcost)

#adjustment cost from the year before
vrN_InvAdjcost_yrbefore0 <- lapply(iter_toplot_RM, get_InvAdjvariable, years = year_toplot_before)
vrN_InvAdjcost_yrbefore0 <- rbindlist(vrN_InvAdjcost_yrbefore0)

vrN_InvAdjcost_yrbefore <- vrN_InvAdjcost_yrbefore0 %>% 
  dplyr::rename(Adjcost_yrbefore = Adjcost) %>% 
  select(tech, Adjcost_yrbefore, iter)
vrN_InvAdjcost_yrbefore$period <- vrN_InvAdjcost$period

#adjustment cost from the year after
vrN_InvAdjcost_yrafter0 <- lapply(iter_toplot_RM, get_InvAdjvariable, years = year_toplot_after)
vrN_InvAdjcost_yrafter0 <- rbindlist(vrN_InvAdjcost_yrafter0)

vrN_InvAdjcost_yrafter <- vrN_InvAdjcost_yrafter0 %>% 
  dplyr::rename(Adjcost_yrafter = Adjcost) %>% 
  select(tech, Adjcost_yrafter, iter)

vrN_InvAdjcost_yrafter$period <- vrN_InvAdjcost$period

RM_LCOE <- list(vrN_GEN_4LCOE, RMcost_bkdw3, vrN_InvAdjcost, vrN_InvAdjcost_yrbefore, vrN_InvAdjcost_yrafter, vrN_CAP0) %>% 
  reduce(full_join) %>% 
  dplyr::rename(IC = "Annualized investment cost") %>% 
  dplyr::rename(OM = "O&M cost") %>% 
  dplyr::rename(FC = "fuel cost (divided by eta)") %>% 
  dplyr::rename(CO2 = "CO2 cost") %>% 
  mutate(LCOE = FC + CO2 + (IC + OM) * 1e3 * cap / gen) %>%  #IC & OM $/kW -> $/MW
  mutate(Adjcost = Adjcost * cap / gen) %>%
  mutate(Adjcost_yrbefore = Adjcost_yrbefore * cap / gen) %>%
  mutate(Adjcost_yrafter = Adjcost_yrafter * cap / gen) %>%
  select(period, tech, iter, LCOE, Adjcost, Adjcost_yrbefore, Adjcost_yrafter) %>% 
  dplyr::rename("REMIND LCOE ($/MWh)" = LCOE) %>% 
  dplyr::rename("Adjustment cost ($/MWh)" = Adjcost) %>% 
  dplyr::rename("Adjustment cost year before ($/MWh)" = Adjcost_yrbefore) %>% 
  dplyr::rename("Adjustment cost year after ($/MWh)" = Adjcost_yrafter) 

# renewable grade related CF, LCOE
get_dataren <- function(iteration){
  # iteration = 15
  gdx = sorted_files[[iteration]]
  # pm_dataren("nur") = capacity factor at different grade levels (quality of wind resources), pm_dataren is used to represent different cost levels necessary to create the same amount of energy if you have to build your wind farm in places with lower wind. The grades are ordered from 1 to 10. 1 is closer to the wind always blowing (highest nur capacity factor), 10 is the worst place to install wind.
  dataren <- read.gdx(gdx, CFkey2) %>% 
    filter(char == "nur") %>%
    filter(all_regi == REGIkey1) %>%
    filter(all_te %in% TECHVREkeylst) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_solar[[1]], plot_RMte_names[[3]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_wind[[1]], plot_RMte_names[[4]])) %>%
    mutate(all_te = str_replace(all_te, TECHkeylst_hydro[[1]], plot_RMte_names[[7]])) %>% 
    dplyr::rename(ren_nur = value) %>% 
    dplyr::rename(tech = all_te) %>% 
    select(tech, rlf, ren_nur)
  
  dataren$iter <- iteration
  return(dataren)
}

vrN_CFgrade <- lapply(iter_toplot_RM, get_dataren)
vrN_CFgrade <- rbindlist(vrN_CFgrade)

RM_LCOE_grade <- list(RMcost_bkdw3, vrN_CFgrade) %>% 
  reduce(full_join) %>% 
  replace(is.na(.), 0) %>% 
  dplyr::rename(IC = "Annualized investment cost") %>% 
  dplyr::rename(OM = "O&M cost") %>% 
  dplyr::rename(FC = "fuel cost (divided by eta)") %>% 
  dplyr::rename(CO2 = "CO2 cost") %>% 
  filter(tech %in% VRE_RMte_names) %>% 
  mutate(LCOE_grade = FC + CO2 + (IC + OM) * 1e3 / (8760 * ren_nur) ) %>%  #IC & OM $/kW -> $/MW, cap / gen = 1 / (gen / cap) = (1/8760) / (gen / 8760* cap) = (1/8760) / ren_nur 
  mutate(ren_nur = ren_nur* 1e2) %>% 
  select(period, tech, iter, rlf, LCOE_grade, ren_nur) %>% 
  dplyr::rename("REMIND LCOE grade ($/MWh)" = LCOE_grade) %>% 
  dplyr::rename("REMIND CapFac grade (%)" = ren_nur) 


get_capDistr <- function(iteration){
  # iteration = 15
  gdx = sorted_files[[iteration]]
  #vm_capDistr is a "helper" variable that stores the amount of capacity that REMIND assign to each grade. Therefore it can have values from any grade. vm_cap is the total capacity (sum of vm_capDistr). It does not have grade infor anymore, everything is assigned to "1". vm_capDistr will retain the information of the optimal grades used in the REMIND decision
  capDistr <- read.gdx(gdx, CFkey3) %>% 
    filter(tall %in% year_toplot) %>%
    filter(all_regi == REGIkey1) %>%
    filter(all_te %in% TECHVREkeylst) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_solar[[1]], plot_RMte_names[[3]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_wind[[1]], plot_RMte_names[[4]])) %>%
    mutate(all_te = str_replace(all_te, TECHkeylst_hydro[[1]], plot_RMte_names[[7]])) %>%     
    dplyr::rename(cap_distr = value)  %>% 
    select(tall,all_te, rlf, cap_distr) %>% 
    dplyr::group_by(all_te) %>%
    mutate(cap_distr = cap_distr * 1e3) %>% 
    dplyr::ungroup(all_te) %>% 
    dplyr::rename(period = tall)  %>% 
    dplyr::rename(tech = all_te)
  
  capDistr$iter <- iteration
  return(capDistr)
}

vrN_capgrade <- lapply(iter_toplot_RM, get_capDistr)
vrN_capgrade <- rbindlist(vrN_capgrade)

vrN_capDistr <- vrN_capgrade %>% 
  dplyr::rename("REMIND VRE distributed capacities (GW)"= cap_distr) 

vrN_grade<- list(RM_LCOE_grade, vrN_capDistr) %>%
reduce(right_join)
  
# RM_totalCOST <- list(vrN_GEN_4LCOE, RMcost_bkdw3, vrN_CAP0) %>% 
#   reduce(full_join) %>% 
#   replace(is.na(.), 0) %>% 
#   dplyr::rename(IC = "Annualized investment cost") %>% 
#   dplyr::rename(OM = "O&M cost") %>% 
#   dplyr::rename(FC = "fuel cost (divided by eta)") %>% 
#   dplyr::rename(CO2 = "CO2 cost") %>% 
#   mutate(cost = (((FC + CO2) * gen + (IC + OM) * 1e3 * cap )) / 1e9) %>% # IC & OM $/kW -> $/MW
#   select(period, tech, iter, cost) %>% 
#   mutate(cost = round(cost, 2)) %>% 
#   dplyr::rename("REMIND annual cost (billionUSD)" = cost) 

#============ minimum screening curve envelop and whether a tech belongs to it (or whether a tech  ============
#============ is the cheapest to disptach at certain hours of a year) =========================================
ScreenCurve0 <- RMcost_bkdw3 %>% 
  filter(iter %in% iter_toplot_RM) %>% 
  replace(is.na(.), 0) %>% 
  dplyr::rename(IC = "Annualized investment cost") %>% 
  dplyr::rename(OM = "O&M cost") %>% 
  dplyr::rename(FC = "fuel cost (divided by eta)") %>% 
  dplyr::rename(CO2 = "CO2 cost") %>% 
  filter(tech %in% Dispatch_RMte_names)

ScreenCurve <- ScreenCurve0 %>% 
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

ScreenCurve_min_envelop$ minTech_yesno <- "1"
# whether a tech belongs to the minumum inverse screening curve

ScreenCurve_min_tech_binary <-list(ScreenCurve0, ScreenCurve_min_envelop ) %>% 
  reduce(full_join) %>% 
  select(period, iter, tech, minTech_yesno) %>% 
  replace(is.na(.), 0) 
  
# ================================ REMIND added capacity =============================================

vrN_deltaCAP0 <- lapply(iter_toplot_RM, get_CAPvariable, CAPkey = CAPkey2)
idx_toplot <- 1:length(iter_toplot_RM)
for(idx in idx_toplot){
  vrN_deltaCAP0[[idx]]$iter <- iter_toplot_RM[[idx]]
}  
vrN_deltaCAP0 <- rbindlist(vrN_deltaCAP0)

get_PM_TS <- function(iteration){
  # iteration = 4
  gdx = sorted_files[[iteration]]
  vrdata <- read.gdx(gdx, PM_TS_Key, factors = FALSE) %>% 
    filter(tall %in% year_toplot) %>%
    dplyr::rename(period = tall) %>% 
    dplyr::rename(pm_ts = value)
  vrdata$iter <- iteration
  return(vrdata)
}

vrN_pm_ts <- lapply(iter_toplot_RM, get_PM_TS)
vrN_pm_ts <- rbindlist(vrN_pm_ts)

vrN_deltaCAP <- list(vrN_pm_ts, vrN_deltaCAP0) %>% 
  reduce(full_join) %>% 
  mutate(cap = round(cap / 1e3 * pm_ts / 2 , 2)) %>% #cap: MW -> GW
  select(period, iter, tech, cap) %>% 
  dplyr::rename("REMIND added capacities (GW)"= cap) 

# ===================================== join all tables ========================================
if (run_number == run_number_uncoupl){
vrN<- list(vrN_REV, RM_capfac, vrN_rep, RM_LCOE, vrN_grade, vrN_deltaCAP,ScreenCurve_min_tech_binary) %>%
# vrN<- list(vrN_REV, RM_capfac, vrN_rep, RM_LCOE, vrN_deltaCAP,ScreenCurve_min_tech_binary) %>%
  reduce(full_join) %>% 
  mutate(tech = fct_relevel(tech, table_ordered_name)) 
}

if (run_number != run_number_uncoupl){
# vrN<- list(vrN_REV, RM_capfac, vrN_rep, RM_LCOE, ShadowPrice, vrN_deltaCAP,ScreenCurve_min_tech_binary) %>%
  vrN<- list(vrN_REV, RM_capfac, vrN_rep, RM_LCOE, vrN_grade, ShadowPrice, vrN_deltaCAP, ScreenCurve_min_tech_binary) %>%
  reduce(full_join) %>% 
  mutate(tech = fct_relevel(tech, table_ordered_name)) 
 
}

vrN$"REMIND Capacity Shadow Price ($/MWh)" [is.na(vrN$"REMIND Capacity Shadow Price ($/MWh)" )] <- 0
vrN$"REMIND added capacities (GW)" [is.na(vrN$"REMIND added capacities (GW)" )] <- 0

# if REMIND MV is larger than REMIND LCOE
RM_MV_ge_LCOE <- (vrN$"REMIND Market value ($/MWh)" >= vrN$"REMIND LCOE ($/MWh)")
vrN$RM_MV_ge_LCOE <- RM_MV_ge_LCOE
# if REMIND MV plus capacity constraint is larger than REMIND LCOE
RM_MVplusCC_ge_LCOE <- (vrN$"REMIND Market value ($/MWh)" + vrN$"REMIND Capacity Shadow Price ($/MWh)" >= vrN$"REMIND LCOE ($/MWh)")
vrN$RM_MVplusCC_ge_LCOE <- RM_MVplusCC_ge_LCOE

# if DIETER MV is larger than DIETER avg LCOE
DT_MV_ge_LCOE <- (vrN$"DIETER Market value ($/MWh)" >= vrN$"DIETER LCOE_avg ($/MWh)")
vrN$DT_MV_ge_LCOE <- DT_MV_ge_LCOE

if (run_number == run_number_uncoupl){
  vrN$"REMIND Capacity Shadow Price ($/MWh)" <- 0
  vrN$run <- "uncoupled run"
}

if (run_number == run_number_full){
  vrN$run <- "full coupled run"
}

if (run_number == run_number_partial){
  vrN$run <- "coupled run w/o cap. constraint"
}

vrN_new <- vrN[, c("iter",	"period",	"tech", "rlf", "REMIND CapFac (%)",	"REMIND CapFac grade (%)", "REMIND Capacity Shadow Price ($/MWh)","REMIND Revenue (billionUSD)","REMIND Market value ($/MWh)",	"REMIND LCOE ($/MWh)","Adjustment cost ($/MWh)","Adjustment cost year before ($/MWh)", "Adjustment cost year after ($/MWh)", "REMIND LCOE grade ($/MWh)", "REMIND VRE distributed capacities (GW)", "RM_MV_ge_LCOE", "RM_MVplusCC_ge_LCOE","minTech_yesno", "REMIND added capacities (GW)", "REMIND pre-investment capacities","DIETER avg CapFac (%)",	"DIETER marg CapFac (%)","DIETER LCOE_avg ($/MWh)",	"DIETER LCOE_marg ($/kWh)","DIETER Revenue marginal plant ($/MW)", "DIETER Market value ($/MWh)","Marginal market value ($/MWh)",	"DIETER Revenue (billionUSD)","DT_MV_ge_LCOE",	"DIETER added capacities (GW)","run")]

# vrN_new <- vrN[, c("iter",	"period",	"tech", "REMIND CapFac (%)","REMIND Capacity Shadow Price ($/MWh)","REMIND Revenue (billionUSD)","REMIND Market value ($/MWh)",	"REMIND LCOE ($/MWh)","RM_MV_ge_LCOE", "RM_MVplusCC_ge_LCOE", "REMIND added capacities (GW)", "REMIND pre-investment capacities","DIETER avg CapFac (%)",	"DIETER marg CapFac (%)","DIETER LCOE_avg ($/MWh)",	"DIETER LCOE_marg ($/MWh)","DIETER Revenue marginal plant ($/MW)", "DIETER Market value ($/MWh)","Marginal market value ($/MWh)",	"DIETER Revenue (billionUSD)","DT_MV_ge_LCOE","minTech_yesno",	"DIETER added capacities (GW)","run")]

vrN_final <- vrN_new %>% 
  dplyr::rename("REMIND market value >= LCOE" = "RM_MV_ge_LCOE") %>% 
  dplyr::rename("REMIND market value + cap.con. shadow price >= LCOE" = "RM_MVplusCC_ge_LCOE") %>% 
  dplyr::rename("DIETER market value >= average LCOE" = "DT_MV_ge_LCOE") %>% 
  dplyr::rename("tech part of min invers screening curve" = "minTech_yesno") %>% 
  dplyr::rename(LCOE_marg = "DIETER LCOE_marg ($/kWh)") %>% 
  mutate(LCOE_marg = round(LCOE_marg / 1e3, 2)) %>%  
  dplyr::rename("DIETER LCOE_marg ($/MWh)" = LCOE_marg) %>%
  dplyr::rename("pre-investment capacities (GW)" = "REMIND pre-investment capacities")

  
vrN_final[is.na(vrN_final)] <- ""

  A <- function(col){
    # col = vrN_final$"REMIND LCOE ($/MWh)"
    col <- ifelse(as.numeric(col)>10, round(as.numeric(col), digits = 0), col)
    col <- ifelse(as.numeric(col)>1 & as.numeric(col)<10, round(as.numeric(col), digits = 1), col)
    col <- ifelse(as.numeric(col)<1, round(as.numeric(col), digits = 2), col)
    
    col[is.na(col)] <- ""
    
    return(col)
  }
  
vrN_final[,5:15] <- data.frame(lapply(vrN_final[,5:15], A) )
vrN_final[,19:28] <- data.frame(lapply(vrN_final[,19:28], A) )
vrN_final[,30] <- data.frame(lapply(vrN_final[,30], A) )

# vrN_final[,4:8] <- data.frame(lapply(vrN_final[,4:8], A) )
# vrN_final[,11:20] <- data.frame(lapply(vrN_final[,11:20], A) )
# vrN_final[,23] <- data.frame(lapply(vrN_final[,23], A) )
  
# return(vrN_final)
# }

# validationTABLE_lst = lapply(run_number_list, runtable)
# validationTABLE <- rbindlist(validationTABLE_lst)

# validationTABLE <- validationTABLE[order(validationTABLE$period),]
# validationTABLE <- validationTABLE[order(validationTABLE$iter),]


vrN_final <- vrN_final[order(vrN_final$period),]
vrN_final <- vrN_final[order(vrN_final$iter),]
write.table(vrN_final, paste0(mypath, "validation_table_oldbranch_ocgt_allyears.xls"), sep = ";", row.names = F)

