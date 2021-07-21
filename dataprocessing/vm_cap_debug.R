mypath = "~/remind-coupling-dieter/dataprocessing/"
run_number = "debug_recreate"
# run_number = "capfac32_valid1"
mydatapath = paste0("~/remind-coupling-dieter/output/", run_number, "/")

# import library
source(paste0(mypath, "library_import.R"))
library(readr)
require(rmndt)

igdx("/opt/gams/gams30.2_linux_x64_64_sfx")

# remind output iteration gdx files
files <- list.files(mydatapath, pattern="fulldata_[0-9]+\\.gdx")
sorted_files <- paste0(mydatapath, "fulldata_", 1:length(files), ".gdx")

ttot = c(2005,2010,2015,2020,2025,2030,2035,2040,2045,2050,2055,2060,2070,2080,2090,2100,2110,2130,2150)

year_toplot_list <- ttot

# for(year_toplot in year_toplot_list){
# year_toplot = 2035

VARkey1 = "vm_cap"
VARkey2 = "vm_deltacap"
VARkey3 = "vm_capEarlyReti"
PM_TS_Key = "pm_ts"

REGIkey1 = "DEU"
TECHkeylst_peakGas = c("ngt")
TECHkeylst_nonPeakGas = c("ngccc","ngcc",  "gaschp") 
TECHkeylst_coal = c("coalchp", "igccc", "igcc", "pcc", "pco","pc")
TECHkeylst_solar = c("spv")
TECHkeylst_wind = c("wind")
TECHkeylst_hydro = c("hydro")
TECHkeylst_nuclear = c("tnrs")
TECHkeylst_biomass = c("biochp", "bioigccc", "bioigcc")
grade = "1"

TECHkeylst <- c(TECHkeylst_peakGas, TECHkeylst_nonPeakGas, TECHkeylst_coal, TECHkeylst_solar, TECHkeylst_wind, TECHkeylst_hydro,TECHkeylst_biomass, TECHkeylst_nuclear)

get_variable <- function(gdx, VARkey){
  # gdx = sorted_files[[9]]
  # VARkey = VARkey1
  
  vrdata <- read.gdx(gdx, VARkey, factors = FALSE,squeeze = FALSE) %>% 
    filter(tall %in% year_toplot_list) %>%
    filter(all_regi == REGIkey1) %>% 
    # filter(all_te %in% TECHkeylst_coal) %>%
    # filter(all_te %in% TECHkeylst) %>%
    filter(all_te %in% c("pc")) %>%
    # filter(rlf == "1" ) %>% 
    # filter(rlf == grade) %>% 
    # mutate(value = value) %>% 
    filter(tall < 2110) 

    return(vrdata)
}

get_PM_TS <- function(gdx){
  vrdata <- read.gdx(gdx, PM_TS_Key, factors = FALSE,squeeze = FALSE) %>% 
    filter(tall %in% year_toplot_list) %>%
    dplyr::rename(pm_ts = value) %>% 
    filter(tall < 2110) 
  
  return(vrdata)
}


vm_cap <- lapply(sorted_files, get_variable, VARkey = VARkey1)
vm_deltacap <- lapply(sorted_files, get_variable, VARkey = VARkey2)
ER0 <- lapply(sorted_files, get_variable, VARkey = VARkey3)
vrN_pm_ts <- lapply(sorted_files, get_PM_TS)

for(fname in files){
  idx <- as.numeric(str_extract(fname, "[0-9]+"))
  vm_cap[[idx]]$iter <- idx
  vm_deltacap[[idx]]$iter <- idx
  ER0[[idx]]$iter <- idx
  vrN_pm_ts[[idx]]$iter <- idx
}

# ER0[[1]]

vm_cap <- rbindlist(vm_cap)
vm_cap1 <- vm_cap %>% 
  filter(rlf == "1" ) %>% 
  select(tall, all_te, value, iter) %>% 
  dplyr::rename(vm_cap = value)  %>% 
  mutate(vm_cap = vm_cap*1e3)

vm_deltacap <- rbindlist(vm_deltacap)
vm_deltacap1 <- vm_deltacap %>% 
  filter(rlf == "1" ) %>% 
  select(tall, all_te, value, iter) %>% 
  dplyr::rename(vm_deltacap = value)  %>% 
  mutate(vm_deltacap = vm_deltacap*1e3)

vrN_pm_ts <- rbindlist(vrN_pm_ts)

addedCap <- list(vrN_pm_ts, vm_deltacap1) %>% 
  reduce(full_join) %>% 
  mutate(cap = round(vm_deltacap * pm_ts / 2 , 2)) %>% 
  select(tall, iter, all_te, cap) %>% 
  dplyr::rename("REMIND added capacities (GW)"= cap)
# %>% 
  # filter(all_te %in% TECHkeylst_coal)

ER <- rbindlist(ER0)

ER1 <- ER %>% 
  dplyr::rename(ER = value) %>% 
  # filter(all_te %in% TECHkeylst_coal) %>%
  # filter(all_te %in% c("pc")) %>% 
  group_by(tall) %>% 
  arrange(all_te) %>% 
  mutate(diff = ER - lag(ER, default = first(ER))) %>% 
  ungroup(tall) %>% 
  filter(tall > 2010)
  
Divestment = list(vm_cap1, ER1) %>%
  reduce(full_join) %>% 
  mutate(diff = diff * vm_cap / (1 - ER)) %>% 
  dplyr::rename(divestment = diff) %>% 
  filter(tall > 2010) %>% 
  select(tall, all_te, divestment,iter)

# earlyRetiCap_reporting("2010", reg, te_remind) = (remind_capEarlyReti("2010", reg, te_remind) - remind_capEarlyReti2("2005", reg, te_remind) ) * remind_cap("2010", reg, te_remind, "1") / (1 - remind_capEarlyReti("2010", reg, te_remind)) ;

cap_table = list(vm_cap1, addedCap, Divestment,ER) %>%
  reduce(full_join) %>% 
  filter(tall > 2010) %>% 
  dplyr::rename(ER = value)

# test_table = list(vm_cap1, addedCap, ER) %>%
#   reduce(full_join) %>% 
#   filter(tall > 2010) %>% 
#   dplyr::rename(ER = value)

cap_table <- cap_table[order(cap_table$all_te),]

write.table(cap_table, paste0(mypath, "debug_table",run_number,".csv"), sep = ";", row.names = F)

