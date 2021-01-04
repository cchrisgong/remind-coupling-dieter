#for shared variable such as peak demand (one iteration series)

mypath = "~/remind-coupling-dieter/dataprocessing/"
# run_number = "mrkup1_revise_4iter"
# run_number = "mrkup2_iter"
run_number = "mrkup9"
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

sorted_files <- sorted_files0[1:5]
filenames <- filenames0[1:5]

# files_full <- list.files(mydatapath, pattern="fulldata_[0-9]+\\.gdx")
# sorted_files_full <- paste0(mydatapath, "fulldata_", 1:length(files_full), ".gdx")
# files_nonopt <- list.files(mydatapath, pattern="non_optimal_[0-9]+\\.gdx")
# sorted_files_nonopt <- paste0(mydatapath, "non_optimal_", (length(files_full)+1):(length(files_full)+length(files_nonopt)), ".gdx")
# sorted_files = c(sorted_files_full, sorted_files_nonopt)
# files = c(files_full, files_nonopt)

year_toplot = 2055

BUDGETkey1 = "qm_budget"
SEELp_iter = "q32_balSe"

SEELp_iterb4 = "pm_priceseel"

MARKUP = "vm_flexAdj"
CAP = "vm_cap"
SEELdem = "v32_seelDem"
PRODSE = "vm_prodSe"
FLEX_TAX_conv = "v21_taxrevFlex"

FLEX_TAX_iterb4 = "p21_taxrevFlex0"
Total_tax = "vm_taxrev"

mult_MARKUP = "p32_DIETERmkup"

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
    filter(all_regi == REGIkey1)
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

budget0 <- lapply(sorted_files, readMARG, key = BUDGETkey1)
seelprice_i0 <- lapply(sorted_files, readMARG, key = SEELp_iter)

seelprice_ib40 <- lapply(sorted_files, readPAR1, key = SEELp_iterb4)
flex_tax_ib40 <- lapply(sorted_files, readPAR1, key = FLEX_TAX_iterb4)

mult_markup0 <- lapply(sorted_files, readPAR2, key = mult_MARKUP)

markup0 <- lapply(sorted_files, readVAR1, key = MARKUP)
seeldem0 <- lapply(sorted_files, readVAR2, key = SEELdem)
prodse0 <- lapply(sorted_files, readVAR3, key = PRODSE)
flextax_c0 <- lapply(sorted_files, readVAR2, key = FLEX_TAX_conv)
tot_tax0 <- lapply(sorted_files, readVAR2, key = Total_tax)
vm_cap0 <- lapply(sorted_files, readVAR3, key = CAP)

for(fname in filenames){
  idx <- as.numeric(str_extract(fname, "[0-9]+"))
  budget0[[idx]]$iter <- idx
  seelprice_i0[[idx]]$iter <- idx
  seelprice_ib40[[idx]]$iter <- idx
  flex_tax_ib40[[idx]]$iter <- idx
  markup0[[idx]]$iter <- idx
  seeldem0[[idx]]$iter <- idx
  prodse0[[idx]]$iter <- idx
  flextax_c0[[idx]]$iter <- idx
  tot_tax0[[idx]]$iter <- idx
  mult_markup0[[idx]]$iter <- idx
  vm_cap0[[idx]]$iter <- idx
}

budget1 <- rbindlist(budget0)
seelprice_i1 <- rbindlist(seelprice_i0)

budget <-  budget1 %>% 
  dplyr::rename(budget = m)

seelprice_i = list(seelprice_i1, budget) %>%
  reduce(left_join) %>%
  mutate(m = m/ budget) 

seelprice_ib4 <- rbindlist(seelprice_ib40)

seeldem <- rbindlist(seeldem0)

flex_tax_ib4 <- rbindlist(flex_tax_ib40)

markup1 <- rbindlist(markup0)
markup <-  markup1 %>% 
  dplyr::rename(markup = value)

prodse1 <- rbindlist(prodse0)
prodse <-  prodse1 %>% 
  dplyr::rename(prodse = value)

cap <- rbindlist(vm_cap0)

flextax_i = list(markup, prodse) %>%
  reduce(left_join) %>%
  mutate(value = - prodse * markup)

flex_tax_ib4 <- rbindlist(flex_tax_ib40) #this is the same as flextax_i because it is from postsolve
flextax_c <- rbindlist(flextax_c0)


mult_markup <- rbindlist(mult_markup0)






