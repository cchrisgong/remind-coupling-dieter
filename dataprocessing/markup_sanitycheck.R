#for shared variable such as peak demand (one iteration series)

mypath = "~/remind-coupling-dieter/dataprocessing/"
run_number = "mrkup46"
mydatapath = paste0("~/remind-coupling-dieter/output/", run_number,"/")
# mydatapath2 = "~/remind-coupling-dieter/output/capfac32_valid3/"

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

iter_start = 1
sorted_files <- sorted_files0[2:34]
filenames <- filenames0[2:34]

# files_full <- list.files(mydatapath, pattern="fulldata_[0-9]+\\.gdx")
# sorted_files_full <- paste0(mydatapath, "fulldata_", 1:length(files_full), ".gdx")
# files_nonopt <- list.files(mydatapath, pattern="non_optimal_[0-9]+\\.gdx")
# sorted_files_nonopt <- paste0(mydatapath, "non_optimal_", (length(files_full)+1):(length(files_full)+length(files_nonopt)), ".gdx")
# sorted_files = c(sorted_files_full, sorted_files_nonopt)
# files = c(files_full, files_nonopt)

year_toplot = 2030

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

mycolors <- c("combined cycle gas" = "#999959", "lignite" = "#0c0c0c", "coal" = "#0c0c0c", "solar" = "#ffcc00", "wind" = "#337fff", "biomass" = "#005900", "open cycle gas turbine" = "#e51900", "hydro" =  "#191999", "nuclear" =  "#ff33ff", "hard coal" = "#808080", "coupled run seel price" = "#ff0000")

readMARG <- function(gdx, key){
  # gdx = sorted_files[[5]]
  vrdata <- read.gdx(gdx, key,field="m") %>% 
    filter(ttot == year_toplot) %>%
    filter(all_regi == REGIkey1)
  return(vrdata)
}

readPAR1 <- function(gdx, key){
  # gdx = sorted_files[[5]]
  vrdata <- read.gdx(gdx, key) %>% 
    filter(ttot == year_toplot) %>%
    filter(all_regi == REGIkey1)
  return(vrdata)
}

readPAR2 <- function(gdx, key){
  # gdx = sorted_files[[5]]
  vrdata <- read.gdx(gdx, key) %>% 
    filter(ttot == year_toplot) %>% 
    filter(all_te %in% FLEX_tech) 
  return(vrdata)
}

readPAR3 <- function(gdx, key){
  # gdx = sorted_files[[5]]
  vrdata <- read.gdx(gdx, key) %>% 
    filter(tall == year_toplot) %>%
    filter(all_regi == REGIkey1)
  return(vrdata)
}

readVAR1 <- function(gdx, key){
  # gdx = sorted_files[[5]]
  vrdata <- read.gdx(gdx, key,field="l") %>% 
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

budget0 <- lapply(sorted_files, readMARG, key = BUDGETkey1)
seelprice_i0 <- lapply(sorted_files, readMARG, key = SEELp_iter)

seelprice_ib40 <- lapply(sorted_files, readPAR1, key = SEELp_iterb4)
flex_tax_ib40 <- lapply(sorted_files, readPAR1, key = FLEX_TAX_iterb4)

pm_cf0 <- lapply(sorted_files, readPAR3, key = PM_CF)

# mult_markup0 <- lapply(sorted_files, readPAR2, key = mult_MARKUP)

markup0 <- lapply(sorted_files, readVAR1, key = MARKUP)
seeldem0 <- lapply(sorted_files, readPAR1, key = SEELdem)
prodse0 <- lapply(sorted_files, readVAR3, key = PRODSE)
flextax_c0 <- lapply(sorted_files, readVAR4, key = FLEX_TAX_conv)
tot_tax0 <- lapply(sorted_files, readVAR4, key = Total_tax)
vm_capfac0 <- lapply(sorted_files, readVAR2, key = CAPFAC)
vm_cap0 <- lapply(sorted_files, readVAR3, key = CAP)


for(fname in filenames){
  idx <- as.numeric(str_extract(fname, "[0-9]+"))
  budget0[[idx- iter_start]]$iter <- idx
  seelprice_i0[[idx- iter_start]]$iter <- idx
  seelprice_ib40[[idx- iter_start]]$iter <- idx
  flex_tax_ib40[[idx- iter_start]]$iter <- idx
  markup0[[idx- iter_start]]$iter <- idx
  seeldem0[[idx- iter_start]]$iter <- idx
  prodse0[[idx- iter_start]]$iter <- idx
  flextax_c0[[idx- iter_start]]$iter <- idx
  tot_tax0[[idx- iter_start]]$iter <- idx
  # mult_markup0[[idx- iter_start]]$iter <- idx
  vm_cap0[[idx- iter_start]]$iter <- idx
  vm_capfac0[[idx- iter_start]]$iter <- idx
  pm_cf0[[idx- iter_start]]$iter <- idx
}

budget1 <- rbindlist(budget0)
seelprice_i1 <- rbindlist(seelprice_i0)

budget <-  budget1 %>% 
  dplyr::rename(budget = m)

seelprice_i = list(seelprice_i1, budget) %>%
  reduce(left_join) %>%
  mutate(m = m / budget) 

seelprice_ib4 <- rbindlist(seelprice_ib40)

seeldem <- rbindlist(seeldem0)

flex_tax_ib4 <- rbindlist(flex_tax_ib40)

markup1 <- rbindlist(markup0)
markup <-  markup1 %>% 
  dplyr::rename(markup = value)

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
  select(iter,tall, all_te, vm_capfac, capfac, prodse, cap) %>% 
  filter(!all_te %in% c("spv","wind","hydro"))

capfac2 <- list(capfac,pm_cf) %>% 
  reduce(full_join) %>% 
  dplyr::rename(pm_cf = value)

flextax_i = list(markup, prodse) %>%
  reduce(left_join) %>%
  mutate(value = - prodse * markup) %>% 
  select(-all_regi)

flex_tax_ib4 <- rbindlist(flex_tax_ib40) #this is the same as flextax_i because it is from postsolve
flextax_c <- rbindlist(flextax_c0)

# mult_markup <- rbindlist(mult_markup0)




