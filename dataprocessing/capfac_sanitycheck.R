mypath = "~/remind-coupling-dieter/dataprocessing/"
run_number = "mrkup52"
mydatapath = paste0("~/remind-coupling-dieter/output/", run_number,"/")

# import library
source(paste0(mypath, "library_import.R"))
library(readr)
require(ggplot2)
require(lusweave)
require(rmndt)
igdx("/opt/gams/gams30.2_linux_x64_64_sfx")


#remind output iteration gdx files
files <- list.files(mydatapath, pattern="fulldata_[0-9]+\\.gdx")
sorted_files0 <- paste0(mydatapath, "fulldata_", 1:length(files), ".gdx")
filenames0 <- paste0("fulldata_", 1:length(files), ".gdx")

iter_start = 0
sorted_files <- sorted_files0[1:7]
filenames <- filenames0[1:7]

# files_full <- list.files(mydatapath, pattern="fulldata_[0-9]+\\.gdx")
# sorted_files_full <- paste0(mydatapath, "fulldata_", 1:length(files_full), ".gdx")
# files_nonopt <- list.files(mydatapath, pattern="non_optimal_[0-9]+\\.gdx")
# sorted_files_nonopt <- paste0(mydatapath, "non_optimal_", (length(files_full)+1):(length(files_full)+length(files_nonopt)), ".gdx")
# sorted_files = c(sorted_files_full, sorted_files_nonopt)
# files = c(files_full, files_nonopt)

year_toplot = 2040

BUDGETkey1 = "qm_budget"
SEELp_iter = "q32_balSe"

SEELp_iterb4 = "p32_priceSeel"

MARKUP = "vm_Mrkup"
CAP = "vm_cap"
CAPFAC = "vm_capFac"
SEELdem = "p32_seelDem"
PRODSE = "vm_prodSe"
PM_CF = "pm_cf"
FLEX_TAX_conv = "v21_taxrevFlex"

FLEX_TAX_iterb4 = "p21_taxrevFlex0"
Total_tax = "vm_taxrev"

mult_MARKUP = "p32_DIETER_VF"

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

FLEX_tech = c(TECHkeylst_peakGas, TECHkeylst_solar, TECHkeylst_nonPeakGas,TECHkeylst_coal,TECHkeylst_wind,TECHkeylst_hydro,TECHkeylst_nuclear,TECHkeylst_biomass)


readPAR3 <- function(gdx, key){
  # gdx = sorted_files[[5]]
  vrdata <- read.gdx(gdx, key) %>% 
    filter(tall == year_toplot) %>%
    filter(all_regi == REGIkey1)
  return(vrdata)
}


readVAR2 <- function(gdx, key){
  # gdx = sorted_files[[5]]
  vrdata <- read.gdx(gdx, key,field="l") %>% 
    filter(ttot == year_toplot) %>%
    filter(all_regi == REGIkey1) %>% 
    select(tall = ttot, all_te, value) %>% 
    filter(all_te %in% FLEX_tech) 
  
  return(vrdata)
}

readVAR3 <- function(gdx, key){
  # gdx = sorted_files[[5]]
  vrdata <- read.gdx(gdx, key,field="l") %>% 
    filter(tall == year_toplot) %>%
    filter(all_regi == REGIkey1) %>% 
    filter(all_te %in% FLEX_tech) 
  return(vrdata)
}

readVAR4 <- function(gdx, key){
  # gdx = sorted_files[[5]]
  vrdata <- read.gdx(gdx, key,field="l") %>% 
    filter(ttot == year_toplot) %>%
    filter(all_regi == REGIkey1)
  
  return(vrdata)
}


pm_cf0 <- lapply(sorted_files, readPAR3, key = PM_CF)
prodse0 <- lapply(sorted_files, readVAR3, key = PRODSE)
vm_capfac0 <- lapply(sorted_files, readVAR2, key = CAPFAC)
vm_cap0 <- lapply(sorted_files, readVAR3, key = CAP)

for(fname in filenames){
  idx <- as.numeric(str_extract(fname, "[0-9]+"))
  prodse0[[idx- iter_start]]$iter <- idx
  vm_cap0[[idx- iter_start]]$iter <- idx
  vm_capfac0[[idx- iter_start]]$iter <- idx
  pm_cf0[[idx- iter_start]]$iter <- idx+1
}

prodse1 <- rbindlist(prodse0)
prodse <-  prodse1 %>% 
  select(tall,all_te,prodse = value, iter)

cap1 <- rbindlist(vm_cap0)
cap <- cap1 %>% 
  select(tall,all_te,cap = value, iter)

vm_capfac <- rbindlist(vm_capfac0)

pm_cf <- rbindlist(pm_cf0)

capfac <- list(cap, prodse,vm_capfac) %>%
  reduce(full_join) %>% 
  mutate(capfac = prodse/cap) %>% 
  mutate(vm_capfac = value) %>% 
  mutate(vm_capfac = round(vm_capfac,4)) %>% 
  mutate(capfac = round(capfac,4)) %>% 
  mutate(prodse = round(prodse,4)) %>% 
  mutate(cap = round(cap,4)) %>% 
  select(iter,tall, all_te, vm_capfac, capfac, prodse, cap) %>% 
  filter(!all_te %in% c("spv","wind","hydro"))

capfac2 <- list(capfac,pm_cf) %>% 
  reduce(full_join) %>% 
  dplyr::rename(pm_cf = value)%>% 
  mutate(pm_cf = round(pm_cf,4)) %>%
  select(iter, tall, all_te, capfac, vm_capfac, pm_cf, prodse, cap )%>% 
  filter(all_te %in% FLEX_tech) %>% 
  filter(!all_te %in% c("spv","wind","hydro"))%>% 
  filter(iter <39)

capfac_eq_vmcapfac <- (capfac2$capfac == capfac2$vm_capfac)
capfac2$"capfac=vmcapfac" <- capfac_eq_vmcapfac

pmcf_eq_vmcapfac <- (capfac2$pm_cf == capfac2$vm_capfac)
capfac2$"vmcapfac=pm_cf" <- pmcf_eq_vmcapfac

write.table(capfac2, paste0(mypath, "capfac_sanitycheck_", run_number, ".xls"), sep = ";", row.names = F)

