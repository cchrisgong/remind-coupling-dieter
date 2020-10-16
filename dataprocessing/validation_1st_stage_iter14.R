#for revenue and market value, produces cvs table of marginal and annual average revenue and market value

mypath = "~/remind/dataprocessing/"

#fully coupled REMIND run (in validation mode, with capfac & capacity constraint)
run_number_full = "32_valid1_DIETERperIter"
iteration = 15
#partially coupled REMIND run (only with capfac, no capacity constraint)
run_number_partial = "32_valid2_DIETERperIter"
#uncoupled REMIND run
run_number_uncoupl = "32_valid3_DIETERperIter"

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

run_number_list = c(run_number_full)

runtable <- function(run_number){
run_number = run_number_full
# run_number = run_number_partial
# run_number = run_number_uncoupl

mydatapath =  paste0("~/remind/output/capfac", run_number, "/iter14/fx_ocgt/") 
#dieter output iteration gdx files
files_DT_rep <- paste0("report_DIETER_i", iteration, ".gdx")

#remind output iteration gdx files

gdxToQuitte_annual(mydatapath, files_DT_rep, run_number)
  #   write.table(out_annual, paste0(myDIETERPLOT_path, "capfac", run_number, "_i", idx, "_annualreport.csv"), sep = ";", row.names = F)
  

sorted_annual_report_DT <- paste0(myDIETERPLOT_path, "capfac", run_number, "_i", iteration, "_annualreport.csv")

TECH_report_keylst_DT = c("coal", "CCGT", "Lignite", "Solar PV", "Wind_on", "Biomass", "OCGT_eff", "Run-of-River", "Nuclear", "Hard coal")
plot_DTte_names = c("coal","combined cycle gas", "lignite", "solar", "wind", "biomass", "open cycle gas turbine", "hydro", "nuclear", "hard coal")
table_ordered_name = c("lignite", "hard coal","combined cycle gas", "solar", "wind", "biomass", "open cycle gas turbine", "hydro", "nuclear")
plot_RMte_names = c("combined cycle gas", "coal", "solar", "wind", "biomass", "open cycle gas turbine", "hydro", "nuclear")
plot_RMLCOEte_names = c("combined cycle gas", "lignite", "solar", "wind", "biomass", "open cycle gas turbine", "hydro", "nuclear")
Dispatch_RMte_names = c("combined cycle gas", "coal", "biomass", "open cycle gas turbine", "nuclear")
Dispatch_DTte_names = c("combined cycle gas", "lignite", "biomass", "open cycle gas turbine", "nuclear", "hard coal")
VRE_RMte_names = c("solar", "wind", "hydro")

VAR_report_key1_DT = c("DIETER avg CapFac (%)","DIETER LCOE_avg ($/MWh)","DIETER marg CapFac (%)","DIETER LCOE_marg ($/MWh)","DIETER Market value ($/MWh)","Marginal market value ($/MWh)","DIETER Revenue (billionUSD)","DIETER Revenue marginal plant ($/MW)", "DIETER added capacities (GW)", "REMIND pre-investment capacities")

#this is the same as the REMIND capfac from iter 5N-1
VAR_report_key2_DT = c("REMIND CapFac (%)")

#this is the same as the REMIND capfac from iter 5N, which is needed for cap.con. shadow price calculation
VAR_report_key3_DT = c("DIETER avg CapFac (%)")

# relevant cost parameters to be loaded from DIETER reporting
VAR_report_key4_DT = c("Annualized investment cost","O&M cost","fuel cost (divided by eta)", "CO2 cost")

# |year|iter|tech|^ CF_REMIND|x LCOE_REMIND|^ MV_RM|x CF_DIETER|x LCOE_avg_DIETER|x LCOE_marg_DIETER|x MV_DT|^ Revenue_REMIND|x Revenue_avg_DIETER|
# |x Revenue_marg_DIETER |^ Shadow_price| x Inv_REMIND| xInv_DIETER|run

# =======================================
#group I validation
year_toplot = c(2025,2035,2050)


######################################################
########## unpack reporting from DIETER ##############


get_report_variable_DT <- function(cvs, varlist){
  # cvs = sorted_annual_report_DT[[1]]
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
  
}

vrN_rep_DTlist  <- lapply(sorted_annual_report_DT, get_report_variable_DT, varlist=VAR_report_key1_DT)

#===================================================
idx_DT <- 1:length(files_DT_rep)
for(id in idx_DT){
  vrN_rep_DTlist[[id]]$iter <- iteration
}

vrN_rep_DT0 <- rbindlist(vrN_rep_DTlist)
vrN_rep_DT <- vrN_rep_DT0 %>% 
  select(period,tech, variable,value,iter)

vrN_rep = spread(vrN_rep_DT, variable, value)

vrN_rep$"DIETER added capacities (GW)"[is.na(vrN_rep$"DIETER added capacities (GW)")] <- 0
vrN_rep$"DIETER Revenue marginal plant ($/MW)"[is.na(vrN_rep$"DIETER Revenue marginal plant ($/MW)")] <- 0

#==========================REMIND LCOE =============================================
#first load cost breakdown (parameters) from DIETER
vrN_RMcost_bkdw  <- lapply(sorted_annual_report_DT, get_report_variable_DT, varlist=VAR_report_key4_DT)

idx_DT <- 1:length(files_DT_rep)
for(id in idx_DT){
  vrN_RMcost_bkdw[[id]]$iter <- iteration
}

vrN_RMcost_bkdw <- rbindlist(vrN_RMcost_bkdw)

#make a copy
RMcost_bkdw1 <- vrN_RMcost_bkdw %>% 
  select(period, tech, variable, value,iter)

RMcost_bkdw2 = spread(RMcost_bkdw1, variable, value)

RMcost_bkdw3 <- RMcost_bkdw2 %>% 
  replace(is.na(.), 0) %>% 
  filter(tech %in% plot_RMLCOEte_names) %>% 
  mutate(tech = str_replace(tech, "lignite", "coal"))

#============ minimum screening curve envelop and whether a tech belongs to it (or whether a tech  ============
#============ is the cheapest to disptach at certain hours of a year) =========================================
ScreenCurve0 <- RMcost_bkdw2 %>% 
  replace(is.na(.), 0) %>% 
  dplyr::rename(IC = "Annualized investment cost") %>% 
  dplyr::rename(OM = "O&M cost") %>% 
  dplyr::rename(FC = "fuel cost (divided by eta)") %>% 
  dplyr::rename(CO2 = "CO2 cost") %>% 
  filter(tech %in% Dispatch_DTte_names)

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

# ===================================== join all tables ========================================
if (run_number == run_number_uncoupl){
  vrN<- list(vrN_rep,ScreenCurve_min_tech_binary) %>%
  reduce(full_join) %>% 
  mutate(tech = fct_relevel(tech, table_ordered_name)) %>%
  arrange(iter) %>% 
  arrange(tech)
}

if (run_number != run_number_uncoupl){
vrN<- list(vrN_rep,ScreenCurve_min_tech_binary) %>%
  reduce(full_join) %>% 
  mutate(tech = fct_relevel(tech, table_ordered_name)) %>%
  arrange(iter) %>% 
  arrange(tech)
}

# if REMIND MV is larger than REMIND LCOE
DT_MV_ge_LCOE <- (vrN$"DIETER Market value ($/MWh)" >= vrN$"DIETER LCOE_avg ($/MWh)")
vrN$DT_MV_ge_LCOE <- DT_MV_ge_LCOE

if (run_number == run_number_uncoupl){
  vrN$run <- "uncoupled run"
}

# col_order <- c(iter	period	tech	REMIND CapFac (%)	REMIND Capacity Shadow Price ($/MWh)	REMIND Revenue (billionUSD)	REMIND Market value ($/MWh)	REMIND LCOE ($/MWh)	REMIND market value > LCOE	REMIND market value + cap.con. shadow price > LCOE	REMIND added capacities (GW)	DIETER avg CapFac (%)	DIETER LCOE_avg ($/MWh)	DIETER Market value ($/MWh)	DIETER LCOE_marg ($/MWh)	DIETER Revenue marginal plant ($/MW)	DIETER Revenue (billionUSD)	DIETER added capacities (GW)	run)

if (run_number == run_number_full){
  vrN$run <- "full coupled run"
}

if (run_number == run_number_partial){
  vrN$run <- "coupled run w/o cap. constraint"
}

# vrN_new <- vrN[, c("iter",	"period",	"tech", "rlf", "REMIND CapFac (%)",	"REMIND CapFac grade (%)", "REMIND Capacity Shadow Price ($/MWh)","REMIND Revenue (billionUSD)","REMIND Market value ($/MWh)",	"REMIND LCOE ($/MWh)",	"REMIND LCOE grade ($/MWh)","REMIND VRE distributed capacities (GW)", "RM_MV_ge_LCOE", "RM_MVplusCC_ge_LCOE", "REMIND added capacities (GW)", "REMIND pre-investment capacities","DIETER avg CapFac (%)",	"DIETER marg CapFac (%)","DIETER LCOE_avg ($/MWh)",	"DIETER LCOE_marg ($/MWh)","DIETER Revenue marginal plant ($/MW)", "DIETER Market value ($/MWh)","Marginal market value ($/MWh)",	"DIETER Revenue (billionUSD)","DT_MV_ge_LCOE","minTech_yesno",	"DIETER added capacities (GW)","run")]

vrN_new <- vrN[, c("iter",	"period",	"tech", "REMIND pre-investment capacities","DIETER avg CapFac (%)",	"DIETER marg CapFac (%)","DIETER LCOE_avg ($/MWh)","DIETER Revenue marginal plant ($/MW)", "DIETER Market value ($/MWh)","Marginal market value ($/MWh)",	"DIETER Revenue (billionUSD)","DT_MV_ge_LCOE","minTech_yesno",	"DIETER added capacities (GW)","run")]

vrN_final <- vrN_new %>% 
  dplyr::rename("DIETER market value >= average LCOE" = "DT_MV_ge_LCOE") %>% 
  dplyr::rename("tech part of min invers screening curve" = "minTech_yesno") %>% 
  dplyr::rename("pre-investment capacities (GW)" = "REMIND pre-investment capacities")

  
vrN_final[is.na(vrN_final)] <- ""

#   A <- function(col){
#     # col = vrN_final$"REMIND LCOE ($/MWh)"
#     col <- ifelse(as.numeric(col)>10, round(as.numeric(col), digits = 0), col)
#     col <- ifelse(as.numeric(col)>1 & as.numeric(col)<10, round(as.numeric(col), digits = 1), col)
#     col <- ifelse(as.numeric(col)<1, round(as.numeric(col), digits = 2), col)
#     
#     col[is.na(col)] <- ""
#     
#     return(col)
#   }
#   
# # vrN_final[,5:12] <- data.frame(lapply(vrN_final[,5:12], A) )
# # vrN_final[,15:24] <- data.frame(lapply(vrN_final[,15:24], A) )
# # vrN_final[,27] <- data.frame(lapply(vrN_final[,27], A) )
# 
# vrN_final[,4:8] <- data.frame(lapply(vrN_final[,4:8], A) )
# vrN_final[,11:20] <- data.frame(lapply(vrN_final[,11:20], A) )
# vrN_final[,23] <- data.frame(lapply(vrN_final[,23], A) )
  
return(vrN_final)
}

validationTABLE_lst = lapply(run_number_list, runtable)
validationTABLE <- rbindlist(validationTABLE_lst)

validationTABLE <- validationTABLE[order(validationTABLE$iter),]
validationTABLE <- validationTABLE[order(validationTABLE$period),]

write.table(validationTABLE, paste0(mypath, "validation_table_raw_iter15_fxlig.xls"), sep = ";", row.names = F)

