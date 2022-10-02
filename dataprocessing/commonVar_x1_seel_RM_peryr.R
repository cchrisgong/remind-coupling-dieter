#for shared variable such as peak demand (one iteration series)

mypath = "~/remind-coupling-dieter/dataprocessing/"
run_number = "mrkup21"
# run_number = "mrkup14_uncoul"
mydatapath = paste0("~/remind-coupling-dieter/output/", run_number,"/")
# mydatapath2 = "~/remind-coupling-dieter/output/capfac32_valid3/"

# import library
source(paste0(mypath, "library_import.R"))
library(readr)
require(ggplot2)
require(lusweave)
require(rmndt)
library(ggallin)
igdx("/opt/gams/gams30.2_linux_x64_64_sfx")

miniter = 1

#remind output iteration gdx files
files <- list.files(mydatapath, pattern="fulldata_[0-9]+\\.gdx")
sorted_files0 <- paste0(mydatapath, "fulldata_", 1:length(files), ".gdx")
filenames0 <- paste0("fulldata_", 1:length(files), ".gdx")

maxiter = length(files)

sorted_files <- sorted_files0[1:maxiter]
filenames <- filenames0[1:maxiter]

# files_full <- list.files(mydatapath, pattern="fulldata_[0-9]+\\.gdx")
# sorted_files_full <- paste0(mydatapath, "fulldata_", 1:length(files_full), ".gdx")
# files_nonopt <- list.files(mydatapath, pattern="non_optimal_[0-9]+\\.gdx")
# sorted_files_nonopt <- paste0(mydatapath, "non_optimal_", (length(files_full)+1):(length(files_full)+length(files_nonopt)), ".gdx")
# sorted_files = c(sorted_files_full, sorted_files_nonopt)
# files = c(files_full, files_nonopt)

# files2 <- list.files(mydatapath2, pattern="fulldata_[0-9]+\\.gdx")
# sorted_files2 <- paste0(mydatapath2, "fulldata_", 1:length(files2), ".gdx")

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

remind.tech.mapping <- c(remind.nonvre.mapping, remind.vre.mapping)


BUDGETkey1 = "qm_budget"
# VARkey2 = "vm_Mrkup"
VARkey2 = "vm_flexAdj"
VARkey3 = "v21_taxrevMrkup"

# PARkey0 = "p32_priceSeel"
PARkey0 = "pm_priceSeel"
PARkey1 = "v32_shSeEl"
PARkey2 = "p21_taxrevMrkup0" # reference tax markup of the last iteration
PARkey3 = "p32_marketValue_spv"
PARkey4 = "pm_adjCostInv"
# PARkey5 = "p32_DIETER_VF"
PARkey5 = "p32_DIETERmkup"

REGIkey1 = "DEU"
sm_TWa_2_MWh = 8760000000

VARkey5 = "q_balPe"

VARkey6 = "vm_demPe"

CFkey1 = "vm_capFac"
CFkey2 = "pm_dataren"
CFkey3 = "vm_capDistr"

VARsubkey2_RM = "p32_seelDem" 

TECHkeylst_peakGas = c("ngt")
TECHkeylst_nonPeakGas = c("ngccc","ngcc",  "gaschp") 
TECHkeylst_coal = c("coalchp", "igccc", "igcc", "pcc", "pco","pc")
TECHkeylst_solar = c("spv")
TECHkeylst_wind = c("wind")
TECHkeylst_hydro = c("hydro")
TECHkeylst_nuclear = c("tnrs")
TECHkeylst_biomass = c("biochp", "bioigccc", "bioigcc")

# FLEX_tech = c(TECHkeylst_peakGas, TECHkeylst_solar, TECHkeylst_nonPeakGas,TECHkeylst_coal,TECHkeylst_wind,TECHkeylst_hydro,TECHkeylst_nuclear,TECHkeylst_biomass)

FLEX_tech = c(TECHkeylst_solar)

mycolors <- c("CCGT" = "#999959", "lignite" = "#0c0c0c", "Coal (Lig + HC)" = "#0c0c0c", "Solar" = "#ffcc00", "Wind" = "#337fff", "Biomass" = "#005900", "OCGT" = "#e51900", "Hydro" =  "#191999", "Nuclear" =  "#ff33ff", "hard coal" = "#808080", "seel price" = "#ff0000")

TECHkeylst <- c(TECHkeylst_peakGas, TECHkeylst_nonPeakGas, TECHkeylst_coal, TECHkeylst_solar, TECHkeylst_wind, TECHkeylst_hydro, TECHkeylst_biomass, TECHkeylst_nuclear)

TECHVREkeylst <- c(TECHkeylst_solar, TECHkeylst_wind,TECHkeylst_hydro)
TECH_NONVRE_keylst <- c(TECHkeylst_peakGas, TECHkeylst_nonPeakGas, TECHkeylst_coal, TECHkeylst_biomass, TECHkeylst_nuclear)

iter_toplot = 1:length(sorted_files)
# iter_toplot2 = 1:length(sorted_files2)
# 
CapConstraintKey = "q32_peakDemand_DT"

get_CAPCONvariable <- function(gdx){
  # gdx = sorted_files[[5]]
  budgetdata <- read.gdx(gdx, BUDGETkey1, field="m", squeeze = FALSE) %>%
    # filter(ttot == year_toplot) %>%
    filter(all_regi == REGIkey1) %>%
    mutate(m = -m) %>%
    dplyr::rename(budget = m)  %>%
    select(ttot, budget)

  capcondata <- read.gdx(gdx, CapConstraintKey, field="m") %>%
    # filter(ttot == year_toplot) %>%
    mutate(m = -m) %>%
    dplyr::rename(capcon = m)

  # transform from tr$2005/TW to $2015/kW
  vrdata = list(capcondata, budgetdata) %>%
    reduce(full_join) %>%
    select(ttot, capcon, budget) %>%
    replace(is.na(.), 0) %>%
    mutate(capcon= capcon/ budget * 1e12 / 1e9 * 1.2)

  return(vrdata)
}

vr1_capcon <- lapply(sorted_files, get_CAPCONvariable)
for(fname in filenames){
  idx <- as.numeric(str_extract(fname, "[0-9]+"))
  vr1_capcon[[idx]]$iter <- idx
  vr1_capcon[[idx]]$model <- "cap.constraint marginal"
}
vr1_capcon <- rbindlist(vr1_capcon)


get_MARKUPvariable <- function(gdx){
  # gdx = sorted_files[[5]]
  budgetdata <- read.gdx(gdx, BUDGETkey1, field="m", squeeze = FALSE) %>% 
    # filter(ttot == year_toplot) %>%
    filter(all_regi == REGIkey1) %>% 
    mutate(m = -m) %>% 
    dplyr::rename(budget = m)
  
  vrdata0 <- read.gdx(gdx, VARkey2, field="l", squeeze = FALSE)  %>% 
    filter(all_regi == REGIkey1) %>% 
    filter(all_te %in% FLEX_tech) %>% 
    dplyr::rename(ttot = tall)
  
  vrdata = list(vrdata0, budgetdata) %>%
    reduce(left_join) %>%
    mutate(value = value * 1e12 / sm_TWa_2_MWh * 1.2) %>%  # vm_flexAdj is proportional to pm_seeprice, which is already divided by budget
    filter(all_te %in% names(remind.tech.mapping)) %>% 
    revalue.levels(all_te = remind.tech.mapping) %>%
    select(ttot,all_te,value) %>% 
    dplyr::group_by(ttot,all_te) %>%
    dplyr::summarise( value = mean(value), .groups = "keep" ) %>% 
    dplyr::ungroup(ttot,all_te) %>% 
    select(ttot,value)
    
  return(vrdata)
}

# get_MRKT_VALUE <- function(gdx){
#   # gdx = sorted_files[[5]]
# 
#   vrdata <- read.gdx(gdx, PARkey3) %>% 
#     mutate(marketvalue = value) %>%
#     mutate(marketvalue = marketvalue * 1e12 / sm_TWa_2_MWh * 1.2) %>% 
#     filter(ttot >2005) %>% 
#     filter(ttot <2160) 
#   
#   return(vrdata)
# }

get_ADJ_COST <- function(gdx){
  # gdx = sorted_files[[5]]
  
  budgetdata <- read.gdx(gdx, BUDGETkey1, field="m", squeeze = FALSE) %>%
    # filter(ttot == year_toplot) %>%
    filter(all_regi == REGIkey1) %>%
    mutate(m = -m) %>%
    dplyr::rename(budget = m)  %>%
    select(ttot, budget)
  
  adjcost <- read.gdx(gdx, PARkey4) %>% 
    filter(all_te %in% names(remind.tech.mapping)) %>% 
    filter(all_regi == REGIkey1) %>%
    revalue.levels(all_te = remind.tech.mapping) %>%
    mutate(adjCost = value) %>%
    select(ttot, all_regi, all_te, adjCost) %>% 
    dplyr::group_by(ttot, all_regi, all_te) %>%
    dplyr::summarise( adjCost = mean(adjCost), .groups = "keep" ) %>% 
    dplyr::ungroup(ttot, all_regi, all_te)
  
  vrdata = list(adjcost, budgetdata) %>%
    reduce(full_join) %>%
    select(ttot, all_te, adjCost, budget) %>%
    mutate(adjCost = adjCost / budget * 1e12 / sm_TWa_2_MWh * 1.2) %>% 
    filter(ttot > 2005) %>% 
    select(ttot, all_te, adjCost) 
  
  return(vrdata)
}

get_GEN_SHARE <- function(gdx){
  # gdx = sorted_filses[[5]]
  
  vrdata <- read.gdx(gdx, PARkey1)  %>% 
    filter(all_regi == REGIkey1) %>% 
    filter(all_te %in% FLEX_tech) %>% 
    mutate(genshare = value)
  
  return(vrdata)
}

get_PRICEvariable <- function(gdx){
  # gdx = sorted_files[[5]]
  vrdata <- read.gdx(gdx, PARkey0, squeeze = FALSE) %>% 
    # filter(ttot == year_toplot) %>% 
    filter(all_regi == REGIkey1) %>% 
     mutate(value = value * 1e12 / sm_TWa_2_MWh * 1.2) 

  return(vrdata)
}

get_PEPRICEvariable <- function(gdx){
  # gdx = sorted_files[[5]]

    vrdata_price <- read.gdx(gdx, VARkey5, field="m", squeeze = FALSE) %>% 
    filter(all_regi == REGIkey1) %>% 
      filter(all_enty %in% c("pecoal", "pegas")) 
    
    vrdata_dem <- read.gdx(gdx, VARkey6, field="l", squeeze = FALSE)  %>% 
      filter(all_regi == REGIkey1) %>% 
      filter(all_enty %in% c("pecoal", "pegas")) 

    vrdata_prod <- read.gdx(gdx, "vm_prodPe", field="l", squeeze = FALSE)  %>% 
      filter(all_regi == REGIkey1) %>% 
      filter(all_enty %in% c("pecoal", "pegas")) 
    
    return(vrdata0)
}

get_BUDGET <- function(gdx){
  # gdx = sorted_files[[10]]
  budgetdata <- read.gdx(gdx, BUDGETkey1, field = "m", squeeze = FALSE) %>% 
    # filter(ttot == year_toplot) %>%
    filter(all_regi == REGIkey1) %>% 
    mutate(m = -m) %>% 
    dplyr::rename(budget = m)
  
  return(budgetdata)
}

readVAR1 <- function(gdx, key){
  # gdx = sorted_files[[5]]
  # key = VARkey3
  budgetdata <- read.gdx(gdx, BUDGETkey1,field="m",squeeze = F) %>% 
    filter(all_regi == REGIkey1) %>% 
    mutate(m = -m) %>% 
    dplyr::rename(budget = m) %>% 
    filter(ttot > 2005)
  
  vrdata0 <- read.gdx(gdx, key) %>% 
    filter(all_regi == REGIkey1) %>% 
    filter(ttot > 2005)
  
  vrdata = list(vrdata0, budgetdata) %>%
    reduce(full_join) %>%
    mutate(value = value / budget * 1e12 / sm_TWa_2_MWh * 1.2) 
  
  return(vrdata)
}

readPAR_DTVF <- function(gdx, key){
  # gdx = sorted_files[[22]]
  # key = PARkey5
  
  vrdata <- read.gdx(gdx, key, squeeze = F) %>% 
    filter(all_te %in% FLEX_tech) %>% 
    revalue.levels(all_te = remind.tech.mapping) %>%
    dplyr::group_by(ttot, all_te) %>%
    dplyr::summarise( value = mean(value), .groups = "keep" ) %>% 
    dplyr::ungroup(ttot, all_te)
    
  return(vrdata)
}

readPAR1 <- function(gdx, key){
  # gdx = sorted_files[[22]]
  # key = "p21_taxrevMrkup0"

  budgetdata <- read.gdx(gdx, BUDGETkey1, field = "m", squeeze = F) %>% 
    filter(all_regi == REGIkey1) %>% 
    mutate(m = -m) %>% 
    dplyr::rename(budget = m)
  
  vrdata0 <- read.gdx(gdx, key, squeeze = F) %>% 
    filter(all_regi == REGIkey1) 
  
  vrdata = list(vrdata0, budgetdata) %>%
    reduce(full_join) %>%
    replace(is.na(.), 0) %>%
    mutate(value = value / budget * 1e12 / sm_TWa_2_MWh * 1.2) 
  
  return(vrdata)
}

vr1_pr <- lapply(sorted_files, get_PRICEvariable)

vr1_bg <- lapply(sorted_files, get_BUDGET)

vr1_mk <- lapply(sorted_files, get_MARKUPvariable)

# vr1_mv <- lapply(sorted_files, get_MRKT_VALUE)

vr1_genSh <- lapply(sorted_files, get_GEN_SHARE)

vr1_taxrev <- lapply(sorted_files, readVAR1, key = VARkey3)

vr1_reference_mrkup_lastiter <- lapply(sorted_files, readPAR1, key = PARkey2)

vr1_DTVF <- lapply(sorted_files, readPAR_DTVF, key = PARkey5) 


for(fname in filenames){
  idx <- as.numeric(str_extract(fname, "[0-9]+"))
  vr1_pr[[idx]]$iter <- idx
  vr1_pr[[idx]]$model <- "seel price"
  vr1_mk[[idx]]$iter <- idx
  vr1_mk[[idx]]$model <- "markup"
  # vr1_mv[[idx]]$iter <- idx
  # vr1_mv[[idx]]$model <- "market value"
  # vr1_mv[[idx]]$all_te <- "solar"
  vr1_bg[[idx]]$iter <- idx
  vr1_bg[[idx]]$model <- "budget"
  
  vr1_genSh[[idx]]$iter <- idx
  vr1_genSh[[idx]]$model <- "solar share"
  vr1_taxrev[[idx]]$iter <- idx
  vr1_taxrev[[idx]]$model <- "total markup tax"
  vr1_reference_mrkup_lastiter[[idx]]$iter <- idx
  vr1_reference_mrkup_lastiter[[idx]]$model <- "reference markup from last iteration"
  
  vr1_DTVF[[idx]]$iter <- idx 
  vr1_DTVF[[idx]]$model <- "DIETER value factor"
  
}

# for(fname in files2){
#   idx <- as.numeric(str_extract(fname, "[0-9]+"))
#   vr1_2[[idx]]$iter <- idx
#   vr1_2[[idx]]$model <- "uncoupled"
# }

vr1_pr0 <- rbindlist(vr1_pr)
# vr1_2 <- rbindlist(vr1_2)
vr1_mk <- rbindlist(vr1_mk)
vr1_DTVF <- rbindlist(vr1_DTVF)
# vr1_mv0 <- rbindlist(vr1_mv)

vr1_pr <-  vr1_pr0 %>% 
  filter(iter >1) %>% 
  filter(ttot >2005) %>% 
  select(ttot,iter,value,model)

vr1_mk3 <- vr1_mk %>% 
  dplyr::rename(markup = value) %>% 
  select(ttot, iter,markup)

vr1_MarketValue <- list(vr1_pr, vr1_mk3) %>% 
  reduce(full_join) %>% 
  mutate(value = value + markup) %>% 
  select(ttot, iter, marketvalue = value)

vr1_mk3$model <- "markup"
vr1_MarketValue$model <- "market value"

# vr1_mv <- vr1_mv0 %>% 
#   filter(iter >1) %>% 
#   select(ttot,iter,all_te,marketvalue,model)

vr1_taxrev <- rbindlist(vr1_taxrev)
vr1_reference_mrkup_lastiter<- rbindlist(vr1_reference_mrkup_lastiter)

vr1_genSh <- rbindlist(vr1_genSh)

vr1_bg <- rbindlist(vr1_bg)

get_CAPFAC_variable <- function(iteration){
  # iteration = 3
  
  # do not comment:
  gdx = sorted_files[[iteration]]
  
  # first the dispatchable
  vrdata <- read.gdx(gdx, CFkey1, field="l", squeeze = FALSE) %>% 
    filter(all_te %in% names(remind.tech.mapping)) %>% 
    filter(all_regi == REGIkey1) %>%
    revalue.levels(all_te = remind.tech.mapping) %>%
    filter(!(all_te %in% remind.vre.mapping)) %>% 
    select(ttot, all_te, value) %>% 
    dplyr::group_by(ttot, all_te) %>%
    dplyr::summarise(value = mean(value), .groups = "keep") %>% 
    dplyr::ungroup(ttot, all_te) %>% 
    dplyr::rename(tall = ttot) 
  
  # second the VRE
  # pm_dataren("nur") = capacity factor at different grade levels (quality of wind resources), pm_dataren is used to represent different cost levels necessary to create the same amount of energy if you have to build your wind farm in places with lower wind. The grades are ordered from 1 to 10. 1 is closer to the wind always blowing (highest nur capacity factor), 10 is the worst place to install wind.
  dataren <- read.gdx(gdx, CFkey2) %>% 
    filter(char == "nur") %>%
    filter(all_regi == REGIkey1) %>%
    filter(all_te %in% TECHVREkeylst) %>% 
    revalue.levels(all_te = remind.vre.mapping) %>%
    dplyr::rename(ren_nur = value) %>% 
    select(all_te, rlf, ren_nur)
  
  #vm_capDistr is a "helper" variable that stores the amount of capacity that REMIND assign to each grade. Therefore it can have values from any grade. vm_cap is the total capacity (sum of vm_capDistr). It does not have grade infor anymore, everything is assigned to "1". vm_capDistr will retain the information of the optimal grades used in the REMIND decision
  
  percentage_cap_distr <- read.gdx(gdx, CFkey3) %>% 
    # filter(tall %in% year_toplot) %>%
    filter(all_regi == REGIkey1) %>% 
    filter(all_te %in% TECHVREkeylst) %>% 
    revalue.levels(all_te = remind.vre.mapping) %>% 
    dplyr::rename(cap_distr = value)  %>% 
    select(tall, all_te, rlf, cap_distr) %>% 
    dplyr::group_by(tall, all_te) %>%
    transmute(rlf, percentage_cap_distr = cap_distr/sum(cap_distr))
  
  vrdata2_0 = list(dataren, percentage_cap_distr) %>%
    reduce(right_join)
  
  vrdata2 <- vrdata2_0 %>% 
    select(tall, all_te, ren_nur, percentage_cap_distr) %>% 
    replace(is.na(.), 0) %>%
    dplyr::group_by(tall, all_te) %>%
    dplyr::summarise(value = sum(ren_nur * percentage_cap_distr), .groups = "keep" ) %>% 
    dplyr::ungroup(tall, all_te)
  
  vrdata_tot <- list(vrdata, vrdata2) %>% 
    reduce(full_join) %>% 
    dplyr::rename(ttot = tall) %>% 
    dplyr::rename(model = all_te)
  
  vrdata_tot$iter <- iteration
  
  return(vrdata_tot)
}

vr1_capfac_RM <- lapply(iter_toplot, get_CAPFAC_variable)
vr1_capfac_RM <- rbindlist(vr1_capfac_RM)

get_DEMvariable_RM <- function(gdx){
  vrdata <- read.gdx(gdx, VARsubkey2_RM, factor = FALSE)  %>% 
    filter(all_regi == REGIkey1) %>%
    filter(all_enty == "seel") %>%
    select(ttot,value) %>% 
    mutate(value = value * 1e3)
  
  return(vrdata)
}

vr1_DEM <- lapply(sorted_files, get_DEMvariable_RM)

for(fname in filenames){
  idx <- as.numeric(str_extract(fname, "[0-9]+"))
  vr1_DEM[[idx]]$iter <- idx
  vr1_DEM[[idx]]$legend <- "total demand"
}

vr1_DEM <- rbindlist(vr1_DEM)

#############################################
year_toplot = 2030

secAxisScale = 1/8.76

p1<-ggplot() +
  geom_line(data = vr1_pr %>% filter(ttot == year_toplot), aes(x = iter, y = value, color = model), size = 1.2, alpha = 0.5) +
  # geom_line(data = vr1_capcon, aes(x = iter, y = capcon*secAxisScale, color = model), size = 1.2, alpha = 0.5) +
  # geom_line(data = vr1_2, aes(x = iter, y = value, color = model), size = 1.2, alpha = 0.5) +
  scale_y_continuous(sec.axis = sec_axis(~./secAxisScale, name = paste0("capacity constraint (USD/kW)")))+
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10,face="bold"),legend.title =  element_blank()) +
  xlab("iteration") + ylab(paste0(PARkey0, "(USD/MWh)"))  +
  coord_cartesian(ylim = c(-20,200))

ggsave(filename = paste0(mypath, run_number, "_iter_seelprice_RM_", year_toplot,".png"),  p1, width = 8, height =5, units = "in", dpi = 120)

p2<-ggplot() +
  geom_line(data = vr1_capcon%>% filter(ttot == year_toplot), aes(x = iter, y = capcon, color = model), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10,face="bold"),legend.title =  element_blank()) +
  xlab("iteration") + scale_y_continuous(name = paste0(CapConstraintKey, "(USD/kW)"))+
  # xlab("iteration") + scale_y_continuous(trans='log10', name = paste0(CapConstraintKey, "(USD/kW)"))+
  coord_cartesian(ylim = c(0.001,200))

ggsave(filename = paste0(mypath, run_number, "_iter_capconShadow_RM_", year_toplot,".png"), p2, width = 8, height =5, units = "in", dpi = 120)

secAxisScale = 2

p3<-ggplot() +
  geom_line(data = vr1_pr%>% filter(ttot == year_toplot), aes(x = iter, y = value, color = model), size = 1.2, alpha = 0.5) +
  geom_line(data = vr1_capfac_RM%>% filter(ttot == year_toplot), aes(x = iter, y = value*100*secAxisScale, color = model), size = 1.2, alpha = 0.5) +
  scale_y_continuous(sec.axis = sec_axis(~./secAxisScale, name = paste0("CF", "(%)")))+
  scale_color_manual(name = "model", values = mycolors)+
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10,face="bold"),legend.title =  element_blank()) +
  xlab("iteration") + ylab(paste0(PARkey0, "(USD/MWh)"))  +
  coord_cartesian(ylim = c(-20,200))

ggsave(filename = paste0(mypath, run_number, "_iter_seelprice_wcapfac_RM_", year_toplot,".png"), p3, width = 8, height = 5, units = "in", dpi = 120)


p4<-ggplot() +
  geom_line(data = vr1_mk3%>% filter(ttot == year_toplot), aes(x = iter, y = markup), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10,face="bold"),legend.title =  element_blank()) +
  xlab("iteration") + ylab(paste0("absolute markup",  "(USD/MWh)"))  +
  scale_color_manual(name = "tech", values = mycolors)+
  coord_cartesian(ylim = c(-100,20))

ggsave(filename = paste0(mypath, run_number, "_iter_markup_RM_", year_toplot,".png"), p4, width = 8, height =5, units = "in", dpi = 120)

p5<-ggplot() +
  geom_line(data = vr1_pr%>% filter(ttot == year_toplot), aes(x = iter, y = value, color = model), size = 1.2, alpha = 0.5) +
  geom_line(data = vr1_DEM%>% filter(ttot == year_toplot), aes(x = iter, y = value, color =legend), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10,face="bold"),legend.title =  element_blank()) +
  scale_y_continuous(sec.axis = sec_axis(~./secAxisScale, name = paste0("total demand", "(TWh)")))+
  xlab("iteration") + ylab(paste0("wholesale electricity price",  "(USD/MWh)"))  +
  coord_cartesian(ylim = c(-20,500))

ggsave(filename = paste0(mypath, run_number, "_iter_price_dem_RM_", year_toplot,".png"), p5, width = 8, height =5, units = "in", dpi = 120)

# p7<-ggplot() +
#   geom_line(data = vr1_mv%>% filter(ttot == year_toplot), aes(x = iter, y = marketvalue, color = all_te), size = 1.2, alpha = 0.5) +
#   theme(axis.text=element_text(size=10), axis.title=element_text(size=10,face="bold"),legend.title =  element_blank()) +
#   xlab("iteration") + ylab(paste0("Market.Value(REMIND)"))  +
#   coord_cartesian(ylim = c(-40,350))
# 
# ggsave(filename = paste0(mypath, run_number, "_iter_market_value_RM_", year_toplot,".png"), p7, width = 8, height =5, units = "in", dpi = 120)

p8<-ggplot() +
  geom_line(data = vr1_taxrev%>% filter(ttot == year_toplot), aes(x = iter, y = value, color = model), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10,face="bold"),legend.title =  element_blank()) +
  xlab("iteration") + ylab(paste0("Total tax markup (REMIND) = markup - reference markup from last iteration"))  +
  coord_cartesian(ylim = c(-50,50))+
  scale_y_continuous(trans=ssqrt_trans)

ggsave(filename = paste0(mypath, run_number, "_iter_total_taxrev_markup_RM_", year_toplot,".png"), p8, width = 8, height =5, units = "in", dpi = 120)

p9<-ggplot() +
  geom_line(data = vr1_reference_mrkup_lastiter%>% filter(ttot == year_toplot), aes(x = iter, y = value, color = model), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10,face="bold"),legend.title =  element_blank()) +
  xlab("iteration") + ylab(paste0("reference markup from last iteration (REMIND)"))  +
  coord_cartesian(ylim = c(-50,200))

ggsave(filename = paste0(mypath, run_number, "_iter_reference_markup_lastiter_RM_", year_toplot,".png"), p9, width = 8, height =5, units = "in", dpi = 120)

p10<-ggplot() +
  geom_line(data = vr1_genSh%>% filter(ttot == year_toplot), aes(x = iter, y = value), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10,face="bold"),legend.title =  element_blank()) +
  xlab("iteration") + ylab(paste0("solar share (%)"))  +
  coord_cartesian(ylim = c(0,80))

ggsave(filename = paste0(mypath, run_number, "_iter_solarshare_RM_", year_toplot,".png"), p10, width = 8, height =5, units = "in", dpi = 120)

secAxisScale = .5

p12<-ggplot() +
  geom_line(data = vr1_mk3%>% filter(ttot == year_toplot), aes(x = iter, y = markup, color = model), size = 1.2, alpha = 0.5) +
  geom_line(data = vr1_MarketValue%>% filter(ttot == year_toplot), aes(x = iter, y = marketvalue, color = model), size = 1.2, alpha = 0.5) +
  geom_line(data = vr1_pr%>% filter(ttot == year_toplot), aes(x = iter, y = value, color = model), size = 1.2, alpha = 0.5) +
  geom_line(data = vr1_DTVF%>% filter(ttot == year_toplot), aes(x = iter, y = value*secAxisScale*100, color = model), size = 1.2, alpha = 0.5) +
  scale_y_continuous(sec.axis = sec_axis(~./secAxisScale, name = paste0("Value Factor (%)")))+
  theme(axis.text = element_text(size=10), axis.title = element_text(size=10, face = "bold"),legend.title =  element_blank()) + 
  xlab("iteration") + ylab(paste0("USD/MWh")) +
  coord_cartesian(ylim = c(-100,200)) 

ggsave(filename = paste0(mypath, run_number, "_iter_MV_VF_Price_RM_", year_toplot,".png"), p12, width = 8, height =5, units = "in", dpi = 120)

