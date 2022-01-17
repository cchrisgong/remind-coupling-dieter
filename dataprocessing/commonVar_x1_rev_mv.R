#for revenue and market value

mypath = "~/remind/dataprocessing/"
mydatapath = "~/remind/output/capfac20/"
run_number = 20
# import library
source(paste0(mypath, "library_import.R"))
library(readr)
require(ggplot2)
require(lusweave)
require(rmndt)
igdx("/opt/gams/gams30.2_linux_x64_64_sfx")


#dieter output iteration gdx files
files_DT_rep <- list.files(mydatapath, pattern="report_DIETER_i[0-9]+\\.gdx")
sorted_files_DT_rep <- paste0(mydatapath, "report_DIETER_i", seq(from = 5, to = length(files_DT_rep)*5, by = 5), ".gdx")

#remind output iteration gdx files
files <- list.files(mydatapath, pattern="fulldata_[0-9]+\\.gdx")
sorted_files <- paste0(mydatapath, "fulldata_", 1:length(files), ".gdx")

for(fname in files_DT_rep){
  gdxToQuitte_annual(mydatapath, fname)
}
#dieter output iteration gdx files
sorted_annual_report_DT <- paste0(myDIETERPLOT_path, "capfac", run_number, "_i", seq(from = 5, to = length(files_DT_rep)*5, by = 5), "_annualreport.csv")

# files_full <- list.files(mydatapath, pattern="fulldata_[0-9]+\\.gdx")
# sorted_files_full <- paste0(mydatapath, "fulldata_", 1:length(files_full), ".gdx")
# files_nonopt <- list.files(mydatapath, pattern="non_optimal_[0-9]+\\.gdx")
# sorted_files_nonopt <- paste0(mydatapath, "non_optimal_", (length(files_full)+1):(length(files_full)+length(files_nonopt)), ".gdx")
# sorted_files = c(sorted_files_full, sorted_files_nonopt)
# files = c(files_full, files_nonopt)

TECHkeylst_DT = c("CCGT", "lig", "Solar", "Wind_on", "bio", "OCGT_eff", "ror", "nuc", "hc")
plot_DTte_names = c("combined cycle gas", "lignite", "solar", "wind", "biomass", "open cycle gas turbine", "hydro", "nuclear", "hard coal")
plot_RMte_names = c("combined cycle gas", "coal", "solar", "wind", "biomass", "open cycle gas turbine", "hydro", "nuclear")

tech_list= c("combined cycle gas", "solar","open cycle gas turbine")

year_toplot = 2025
iter_toplot = 20
maxiter = 100

BUDGETkey1 = "qm_budget"
VARkey1 = "q32_balSe"
GENkey1 = "vm_prodSe"
ProdKEY = "seel"
REGIkey1 = "DEU"
sm_TWa_2_MWh = 8760000000

VAR_report_key1_DT = c("Market value full load","Market value", "full load capacity factor","avg capacity factor", "Revenue full load","Revenue","Revenue marginal plant per MW")
TECH_report_keylst_DT = c("CCGT", "Lignite", "Solar PV", "Wind_on", "Biomass", "OCGT_eff", "Run-of-River", "Nuclear", "Hard coal")

get_PRICEvariable <- function(gdx){
  # gdx = sorted_files[[1]]
  budgetdata <- read.gdx(gdx, BUDGETkey1,field="m") %>% 
    filter(ttot == year_toplot) %>%
    filter(all_regi == REGIkey1) %>% 
    mutate(m = -m) %>% 
    dplyr::rename(budget = m)
  
  vrdata0 <- read.gdx(gdx, VARkey1,field="m") %>% 
    filter(ttot == year_toplot) %>%
    filter(all_regi == REGIkey1) 
  
  vrdata = list(vrdata0, budgetdata) %>%
    reduce(full_join) %>%
    mutate(m = -m/ budget * 1e12 / sm_TWa_2_MWh * 1.2) %>% 
    dplyr::rename(value = m)
    
  return(vrdata)
}

get_GENvariable <- function(gdx){
  # gdx = sorted_files[[1]]
  vrdata <- read.gdx(gdx, GENkey1, factors = FALSE) %>% 
    filter(tall == year_toplot) %>%
    filter(all_regi == REGIkey1) %>%
    filter(all_te %in% TECHkeylst) %>% 
    filter(all_enty.1 == ProdKEY)  %>% 
    mutate(value = value*sm_TWa_2_MWh) %>% 
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
    dplyr::summarise( value = sum(value) , .groups = 'keep' ) %>% 
    dplyr::ungroup(all_te)
  
  vrdata$all_te <- factor(vrdata$all_te, levels= c("solar", "wind", "hydro", "combined cycle gas", "biomass", "open cycle gas turbine",  "nuclear", "coal"))
  
  return(vrdata)
}

vrN_GEN <- lapply(sorted_files, get_GENvariable)
vrN_PRICE <- lapply(sorted_files, get_PRICEvariable)
# print(vrN[[1]])

for(fname in files){
  idx <- as.numeric(str_extract(fname, "[0-9]+"))
  vrN_GEN[[idx]]$iter <- idx
  vrN_GEN[[idx]]$model <- "REMIND"
  vrN_PRICE[[idx]]$iter <- idx
  vrN_PRICE[[idx]]$model <- "REMIND"  
}

vrN_GEN <- rbindlist(vrN_GEN)
vrN_PRICE0<- rbindlist(vrN_PRICE) 

vrN_PRICE<- vrN_PRICE0 %>% 
  mutate(price = value) %>% 
  select(price,iter,model,ttot) 

vrN_REV0 = list(vrN_GEN, vrN_PRICE) %>%
  reduce(full_join)

vrN_REV <- vrN_REV0 %>%
  mutate(value=value*price/1e9) %>%
  filter(iter == iter_toplot) %>%
  filter(all_te %in% tech_list) %>% 
  mutate(market_value = price)

##########################################
##########market value ##################

get_report_variable_DT <- function(cvs){
  cvs = sorted_annual_report_DT[[1]]
  annual_reportCSV = read.csv(cvs, sep = ';', header = T, stringsAsFactors = F)
  annual_reportQUITT <- as.quitte(annual_reportCSV) 
  
  vrdata <- annual_reportQUITT %>% 
    filter(period == year_toplot) %>%
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

#===================================================
idx_DT <- 1:length(files_DT_rep)
for(id in idx_DT){
  vrN_rep_DTlist[[id]]$iter <- id*5
  vrN_rep_DTlist[[id]]$model <- "DIETER"  
}

vrN_rep_DT0 <- rbindlist(vrN_rep_DTlist)
vrN_rep_DT <- vrN_rep_DT0 %>% 
  filter(iter == iter_toplot) %>% 
  filter(tech %in% tech_list) 
  
write.table(vrN_rep_DT, paste0(mypath, run_number, "_revenue_mv_cf_dieter.csv"), sep = ";", row.names = F)
