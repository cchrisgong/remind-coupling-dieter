mypath = "~/remind-coupling-dieter/dataprocessing/"
myDIETERPLOT_path = "~/remind-coupling-dieter/dataprocessing/DIETER_plots/"
run_number = "hydro651"
mydatapath = paste0("~/remind-coupling-dieter/output/", run_number,"/")

mifpath = paste0(mydatapath,"REMIND_generic_xx_ref_FEmed.mif")

# import library
source(paste0(mypath, "library_import.R"))
source(paste0(myDIETERPLOT_path, "GDXtoQuitte.R"))
library(readr)
require(ggplot2)
require(lusweave)
require(rmndt)
library(ggplot2)
# igdx("/opt/gams/gams31.2_linux_x64_64_sfx")

miniter = 1

files_DT_rep <- list.files(mydatapath, pattern="report_DIETER_i[0-9]+\\.gdx") 
# for(fname in files_DT_rep){
#   gdxToQuitte_annual(mydatapath, fname, run_number)
# }

#remind output iteration gdx files
files <- list.files(mydatapath, pattern="fulldata_[0-9]+\\.gdx")
sorted_files1 <- paste0(mydatapath, "fulldata_", 0:(length(files)-1), ".gdx")
sorted_files <- paste0(mydatapath, "fulldata_", 1:(length(files)-1), ".gdx")
filenames1 <- paste0("fulldata_", 0:(length(files)-1), ".gdx")
filenames <- paste0("fulldata_", 1:(length(files)-1), ".gdx")
# sorted_files <- paste0(mydatapath, "fulldata_", 1:length(files), ".gdx")
# filenames <- paste0("fulldata_", 1:length(files), ".gdx")
maxiter = length(files)

iter_toplot = 1:length(sorted_files)
iter_toplot1 = 0:length(sorted_files)

#load H2 switch
h2switch0 <- read.gdx(sorted_files[[1]], "s32_H2switch", factors = FALSE, squeeze = FALSE)
h2switch = as.numeric(h2switch0)

# iter_toplot = 1:3
# iter_toplot1 = 0:3
# sorted_files <- sorted_files0[1:maxiter]
# filenames <- filenames0[1:maxiter]

# year_toplot = 2050
remind.nonvre.mapping <- c(
                           # coalchp = "Coal",
                           igcc = "Coal",
                           igccc = "Coal",
                           pcc = "Coal",
                           pco = "Coal",
                           pc = "Coal",
                           tnrs = "Nuclear",
                           ngt = "OCGT",
                           ngcc = "CCGT",
                           ngccc = "CCGT",
                           # gaschp = "CCGT",
                           # biochp = "Biomass",
                           bioigcc = "Biomass",
                           bioigccc = "Biomass",
                           NULL)

remind.sector.coupling.mapping <- c()

if (h2switch == 1){
  remind.sector.coupling.mapping <- c(elh2 = "Electrolyzers",
                                      tdels = "Electricity (stationary)",
                                      NULL)
}  

remind.vre.mapping <- c(hydro = "Hydro",
                        wind = "Wind",
                        spv = "Solar",
                        NULL)

dieter.tech.exclude <- c("OCGT_ineff", "Wind_off")

dieter.supply.tech.mapping <- c(hc = "Hard coal",
                                lig = "Lignite",
                                coal = "Coal",
                                nuc = "Nuclear",
                                OCGT_eff = "OCGT",
                                CCGT = "CCGT",
                                bio = "Biomass",
                                ror = "Hydro",
                                Wind_on = "Wind",
                                Solar = "Solar",
                                NULL)

dieter.supply.fuel.mapping <- c("Lignite" = "pecoal",
                                "OCGT" = "pegas",
                                "Biomass" = "pebiolc",
                                NULL)


dieter.demand.tech.mapping <- c()

if (h2switch == 1){
  dieter.demand.tech.mapping <- c(el = "Electricity (stationary)",
                                        elh2 = "Electrolyzers",
                                        NULL)
}  


 

dieter.tech.mapping <- c(dieter.supply.tech.mapping, dieter.demand.tech.mapping)

remind.tech.mapping <- c(remind.nonvre.mapping, remind.vre.mapping, remind.sector.coupling.mapping)

BUDGETkey1 = "qm_budget"
VARkey2 = "vm_Mrkup"
VARkey3 = "v21_taxrevMrkup"
VARkey4 = "vm_flexAdj"

exportVAR = "vm_Xport"
importVAR = "vm_Mport"
importCOST = "pm_costsPEtradeMp"
extrVAR = "vm_fuExtr"

CapConstraintKey = "q32_peakDemandDT"
# CapConstraintKey = "vm_priceCap"

MARGkey0 = "q32_balSe"
MARGkey1 = "q_balPe"

PARkey0 = "pm_SEPrice"
ProdKEY = "seel"
# PARkey1 = "v32_shSeEl" # post curtailment generation share of the total production in REMIND
PARkey1 = "v32_shSeElDisp" # post curtailment generation share of the DIETER-dispatched production in REMIND
PARkey2 = "p21_taxrevMrkup0" # reference tax markup of the last iteration
PARkey3 = "p32_marketValue"

PARkey4 = "pm_adjCostInv"
PARkey5 = "p32_DIETER_VF"
PARkey6 = "p32_DIETER_MV"
PARkey7 = "p32_DIETER_elecprice"
# PARkey7 = "p32_DIETER_elecprice_orig"

PARkey8 = "p32_marketPrice"

PARkey9 = "p32_fuelprice_avgiter"

REGIkey1 = "DEU"
# REGIkey1 = "USA"
sm_TWa_2_MWh = 8760000000

VARkey5 = "pm_PEPrice"

VARkey6 = "vm_demPe"

CFkey1 = "vm_capFac"
CFkey2 = "pm_dataren"
CFkey3 = "vm_capDistr"

# VARsubkey2_RM = "p32_seelUsableDem" 
VARsubkey2_RM = "p32_seelTotDem" 

TECHkeylst_peakGas = c("ngt")
TECHkeylst_nonPeakGas = c("ngccc","ngcc",  "gaschp") 
TECHkeylst_coal = c("coalchp", "igccc", "igcc", "pcc", "pco","pc")
TECHkeylst_solar = c("spv")
TECHkeylst_wind = c("wind")
TECHkeylst_hydro = c("hydro")
TECHkeylst_nuclear = c("tnrs")
TECHkeylst_biomass = c("biochp", "bioigccc", "bioigcc")
TECHkeylst_sectorCoup = c("elh2","tdels")

tech_RM = c(TECHkeylst_solar, TECHkeylst_nonPeakGas,TECHkeylst_peakGas,TECHkeylst_coal,TECHkeylst_wind,TECHkeylst_hydro,TECHkeylst_nuclear,TECHkeylst_biomass)
tech_RM2 = c(TECHkeylst_sectorCoup)

tech_RM_names = c("Coal",
                    "Nuclear",
                    "OCGT",
                    "CCGT",
                    "Biomass",
                    "Hydro",
                    "Wind",
                    "Solar",
                    "Electrolyzers",
                    "Electricity (stationary)"
                    )

mycolors <- c("CCGT" = "#999959", "lignite" = "#0c0c0c", "Coal" = "#0c0c0c", "Solar" = "#ffcc00", "Wind" = "#337fff", "Biomass" = "#005900", "OCGT" = "#e51900", "Hydro" =  "#191999", "Nuclear" =  "#ff33ff", "hard coal" = "#808080", "REMIND seel price ($/MWh)" = "#ff0000", "Electrolyzers" = "#48D1CC", "Electricity (stationary)" = "#7F7FFF")

mycolors2 <- c("CCGT" = "#999959", "Coal" = "#0c0c0c", "Solar" = "#ffcc00", "Wind" = "#337fff", "Biomass" = "#005900", "OCGT" = "#e51900", "Hydro" =  "#191999", "Nuclear" =  "#ff33ff", "Electrolyzers" = "#48D1CC", "Electricity (stationary)" = "#7F7FFF")

mycolors_nodem <- c("CCGT" = "#999959", "Coal" = "#0c0c0c", "Solar" = "#ffcc00", "Wind" = "#337fff", "Biomass" = "#005900", "OCGT" = "#e51900", "Hydro" =  "#191999", "Nuclear" =  "#ff33ff")

if (h2switch == 0){
  mycolors2 <- mycolors_nodem
}

color.mapping.var <- c("REMIND markup ($/MWh)" = "#0000cc", "DIETER Market value w/ scarcity price shaved ($/MWh)" = "#007f00", "REMIND market value ($/MWh)" = "#7F7FFF")
color.mapping.var2 <- c("DIETER secondary electricity price ($/MWh)" = "#007f00", "REMIND secondary electricity price ($/MWh)" = "#7F7FFF")

linetypemap <- c('DIETER' = 'dotted', 'REMIND' = 'solid')

TECHkeylst <- c(TECHkeylst_peakGas, TECHkeylst_nonPeakGas, TECHkeylst_coal, TECHkeylst_solar, TECHkeylst_wind, TECHkeylst_hydro, TECHkeylst_biomass, TECHkeylst_nuclear, TECHkeylst_sectorCoup)

TECHVREkeylst <- c(TECHkeylst_solar, TECHkeylst_wind,TECHkeylst_hydro)
TECH_NONVRE_keylst <- c(TECHkeylst_peakGas, TECHkeylst_nonPeakGas, TECHkeylst_coal, TECHkeylst_biomass, TECHkeylst_nuclear, TECHkeylst_sectorCoup)



VAR_report_key_DT = c(
                      # "DIETER Market value w/ scarcity price shaved ($/MWh)",
                      "DIETER Market value ($/MWh)",
                      "genshares (%)", 
                      # "price w/ scarcity price shaved ($/MWh)",
                      "load-weighted price for fixed demand ($/MWh)",
                      "primary energy price ($/MWh)")

get_PRICEvariable <- function(gdx){
  # gdx = sorted_files[[5]]
  
  vrdata <- read.gdx(gdx, PARkey0, squeeze = FALSE) %>% 
    filter(all_enty == ProdKEY) %>%
    # filter(ttot == year_toplot) %>% 
    filter(all_regi == REGIkey1) %>% 
    mutate(value = value * 1e12 / sm_TWa_2_MWh * 1.2) 
  
  return(vrdata)
}

pr <- lapply(sorted_files1, get_PRICEvariable)

for(fname in filenames1){
  idx <- as.numeric(str_extract(fname, "[0-9]+")) +1
  pr[[idx]]$iter <- idx
  pr[[idx]]$model <- "REMIND secondary electricity price ($/MWh)"
}
pr0 <- rbindlist(pr)

pr <-  pr0 %>% 
  select(period=ttot,iter,value,model)


get_DEMvariable_RM <- function(gdx){
  vrdata <- read.gdx(gdx, VARsubkey2_RM, factor = FALSE)  %>% 
    filter(all_regi == REGIkey1) %>%
    filter(all_enty == "seel") %>%
    select(period=ttot,value) %>% 
    mutate(value = value *sm_TWa_2_MWh/1e6)
  return(vrdata)
}

DEM <- lapply(sorted_files, get_DEMvariable_RM)

for(fname in filenames){
  idx <- as.numeric(str_extract(fname, "[0-9]+"))
  DEM[[idx]]$iter <- idx
  DEM[[idx]]$legend <- "total demand"
}

DEM <- rbindlist(DEM)

p1<-ggplot() +
  geom_line(data = pr, aes(x = iter, y = value, color = model), size = 1.2, alpha = 0.5) +
  scale_color_manual(name = "model", values = mycolors)+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold")) +
  xlab("iteration") + ylab(paste0("REMIND secondary electricity price ($/MWh)"))  +
  coord_cartesian(ylim = c(-20,200))+
  facet_wrap(~period, nrow = 3)

ggsave(filename = paste0(mypath, run_number, "_iter_seelprice_RM.png"), p1, width = 20, height = 12, units = "in", dpi = 120)

secAxisScale = 0.4

p5<-ggplot() +
  geom_line(data = pr, aes(x = iter, y = value, color = model), size = 1.2, alpha = 0.5) +
  geom_line(data = DEM, aes(x = iter, y = value * secAxisScale, color =legend), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold")) +
  scale_y_continuous(sec.axis = sec_axis(~./secAxisScale, name = paste0("total demand", "(TWh)")))+
  xlab("iteration") + ylab(paste0("REMIND secondary electricity price ($/MWh)"))  +
  coord_cartesian(ylim = c(-20,500))+
  facet_wrap(~period, nrow = 3)

ggsave(filename = paste0(mypath, run_number, "_iter_price_dem_RM.png"), p5, width = 20, height =12, units = "in", dpi = 120)


get_CAPFAC_variable <- function(iteration){
  # iteration = 3
  
  # do not comment:
  gdx = sorted_files1[[iteration]]
  
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
    dplyr::rename(period = tall) %>% 
    dplyr::rename(tech = all_te)
  
  vrdata_tot$iter <- iteration -1 
  
  return(vrdata_tot)
}

capfac_RM <- lapply(iter_toplot, get_CAPFAC_variable)
capfac_RM <- rbindlist(capfac_RM)


secAxisScale = 2

p2<-ggplot() +
  geom_line(data = pr, aes(x = iter, y = value, color = model), size = 1.2, alpha = 0.5) +
  geom_line(data = capfac_RM, aes(x = iter, y = value*100*secAxisScale, color = tech), size = 1.2, alpha = 0.5) +
  scale_y_continuous(sec.axis = sec_axis(~./secAxisScale, name = paste0("CF", "(%)")))+
  scale_color_manual(name = "tech", values = mycolors)+
  # scale_color_manual(name = "model", values = mycolors)+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold")) +
  xlab("iteration") + ylab(paste0(PARkey0, "(USD/MWh)"))  +
  coord_cartesian(ylim = c(-20,200))+
  facet_wrap(~period, nrow = 3)

ggsave(filename = paste0(mypath, run_number, "_iter_seelprice_wcapfac_RM.png"), p2, width = 20, height = 12, units = "in", dpi = 120)

# stop()


# dieter output iteration gdx files
files_DT <- list.files(mydatapath, pattern="results_DIETER_i[0-9]+\\.gdx")
id <- NULL
for(fname in files_DT){
  idx <- as.numeric(str_extract(fname, "[0-9]+"))
  id = c(id, idx)
}

id = sort(id)


if (length(files_DT) != 0) {
  sorted_files_DT <- paste0("results_DIETER_i", sort(id), ".gdx")
  sorted_paths_DT <- paste0(mydatapath, "results_DIETER_i", sort(id), ".gdx")
}

if (length(files_DT_rep) != 0) {
  sorted_annual_report_DT <- paste0(myDIETERPLOT_path, run_number, "_i", sort(id), "_annualreport.csv")
}

iter_toplot_DT = 1:(length(sorted_annual_report_DT))

get_DT_variable <- function(iteration){
  # iteration= 1
  cvs = sorted_annual_report_DT[[iteration]] # since DIETER files start from 2
  
  annual_reportCSV = read.csv(cvs, sep = ";", header = T, stringsAsFactors = F)
  annual_reportQUITT <- as.quitte(annual_reportCSV) 
  
  dieter.data1 <- annual_reportQUITT %>% 
    filter(period >2015) %>% 
    filter(variable %in% VAR_report_key_DT) %>% 
    filter(model == "DIETER") %>% 
    select(period, tech, variable, value) %>% 
    filter(!tech %in% dieter.tech.exclude) %>% 
    revalue.levels(tech = dieter.tech.mapping) %>% 
    replace(is.na(.), 0) 
  
  dieter.data1$iter = id[[iteration]]
  
  return(dieter.data1)
}

DT_list <- lapply(iter_toplot_DT, get_DT_variable)
DT <- rbindlist(DT_list)

MV_DT <- DT %>% 
  filter(variable == "DIETER Market value ($/MWh)") %>% 
  select(iter,period,tech,value,model=variable) 

seelprice_DT <- DT %>% 
  filter(variable == "load-weighted price for fixed demand ($/MWh)") %>% 
  mutate(variable = "DIETER seel price ($/MWh)") %>% 
  select(iter,period,value,model=variable) 

genshare_DT <- DT %>% 
  filter(variable == "genshares (%)") %>% 
  select(iter,period,tech,value) %>% 
  mutate(model = "DIETER")

peprice_DT <- DT %>% 
  filter(variable == "primary energy price ($/MWh)") %>%
  revalue.levels(tech = dieter.supply.fuel.mapping) %>% 
  filter(tech %in% dieter.supply.fuel.mapping) %>% 
  select(iter,period,variable,fuel=tech,value) %>% 
  mutate(variable = "DIETER PE price ($/MWh)")



# get marginal of PE balance equations
get_PEMarginals <- function(iteration){
  # iteration= 12
  # gdx = sorted_files[[12]]
  gdx = sorted_files[[iteration]]
  
  budgetdata <- read.gdx(gdx, BUDGETkey1, field="m", squeeze = FALSE) %>%
    filter(all_regi == REGIkey1) %>%
    mutate(m = -m) %>%
    dplyr::rename(budget = m)  %>%
    select(period= ttot, budget)
  
  PEprice <- read.gdx(gdx, MARGkey1, field="m", squeeze = FALSE) %>%
    filter(all_regi == REGIkey1) %>% 
    filter(all_enty %in% c("pegas","pecoal","pebiolc")) %>% 
    mutate(m = -m) %>% 
    dplyr::rename(value = m) %>% 
    select(period = ttot,fuel = all_enty, value) %>% 
    left_join(budgetdata) %>% 
    mutate(value = value/budget * 1e12 / sm_TWa_2_MWh * 1.2) %>% 
    replace(is.na(.), 0) %>%
    select(period,fuel,value) %>% 
    mutate(variable = "marginal")
  
  PEprice$iter <- iteration
  
  return(PEprice)
}

get_PEPRICEvariable <- function(iteration){
  # gdx = sorted_files[[5]]
  
  gdx = sorted_files[[iteration]]
  
  budgetdata <- read.gdx(gdx, BUDGETkey1, field="m", squeeze = FALSE) %>%
    filter(all_regi == REGIkey1) %>%
    mutate(m = -m) %>%
    dplyr::rename(budget = m)  %>%
    select(period= ttot, budget)
  
  vrdata_price <- read.gdx(gdx, VARkey5, squeeze = FALSE) %>% 
    filter(all_regi == REGIkey1) %>% 
    filter(all_enty %in% c("pecoal", "pegas","pebiolc")) %>% 
    mutate(value = value * 1e12 / sm_TWa_2_MWh * 1.2) %>% 
    select(period = ttot,fuel = all_enty, value)%>% 
    mutate( variable = "price")
  
  vrdata_prod <- read.gdx(gdx, "vm_prodPe", field="l", squeeze = FALSE)  %>% 
    filter(all_regi == REGIkey1) %>% 
    filter(all_enty %in% c("pecoal", "pegas","pebiolc"))  %>% 
    mutate(value = value *sm_TWa_2_MWh/1e6) %>% 
    select(period = ttot,fuel = all_enty, value) %>% 
    mutate(variable = "production")
  
  vrdata = list(vrdata_price, vrdata_prod) %>%
    reduce(full_join)
  vrdata$iter <- iteration
  
  return(vrdata)
}

pe <- lapply(iter_toplot, get_PEPRICEvariable)
pe <- rbindlist(pe)

pem <- lapply(iter_toplot, get_PEMarginals)
pem <- rbindlist(pem)

pe_price <- pe %>% 
  filter(variable == "price")

pe_prod <- pe %>% 
  filter(variable == "production")

#get import variable 
getTRADEvariable <- function(gdx){
  # gdx = sorted_files[[5]]
  
  vrdata_xport <- read.gdx(gdx, exportVAR, field="l", squeeze = FALSE) %>% 
    filter(all_regi == REGIkey1) %>% 
    filter(all_enty %in% c("pecoal", "pegas","pebiolc")) %>% 
    mutate(value = value * sm_TWa_2_MWh/1e6) %>% 
    select(period = tall,fuel = all_enty, export = value)
  
  vrdata_mport0 <- read.gdx(gdx, importVAR, field="l", squeeze = FALSE)  %>% 
    filter(all_regi == REGIkey1) %>% 
    filter(all_enty %in% c("pecoal", "pegas","pebiolc"))  %>% 
    mutate(value = value * sm_TWa_2_MWh/1e6) %>% 
    select(period = tall,fuel = all_enty, value)
  
  vrdata_mportcost <- read.gdx(gdx, importCOST, squeeze = FALSE)  %>% 
    filter(all_regi == REGIkey1) %>% 
    filter(all_enty %in% c("pecoal", "pegas","pebiolc")) %>% 
    select(fuel = all_enty, mportcost = value)
  
  vrdata_mport = list(vrdata_mport0, vrdata_mportcost) %>% 
    reduce(left_join) %>%
    replace(is.na(.), 0) %>%
    mutate(value = (1-mportcost) * value) %>% 
    select(period,fuel, import = value)
  
  vrdata_prod <- read.gdx(gdx, extrVAR, field="l", squeeze = FALSE)  %>% 
    filter(all_regi == REGIkey1) %>% 
    filter(all_enty %in% c("pecoal", "pegas","pebiolc"))  %>% 
    filter(rlf == "1") %>% 
    mutate(value = value * sm_TWa_2_MWh/1e6) %>% 
    select(period = ttot,fuel = all_enty, prod = value)
  
  vrdata = list(vrdata_xport, vrdata_mport,vrdata_prod) %>%
    reduce(full_join)
  
  return(vrdata)
}


trade <- lapply(sorted_files, getTRADEvariable)

for(fname in filenames){
  idx <- as.numeric(str_extract(fname, "[0-9]+"))
  trade[[idx]]$iter <- idx
}
trade <- rbindlist(trade)

trade <-trade%>% 
  gather(key = "variable", value = "value", -period,-fuel,-iter)

secAxisScale= 1/20
p1a<-ggplot() +
  geom_line(data = pem, aes(x = iter, y = value, color = fuel, linetype=variable), size = 1.2, alpha = 0.5) +
  geom_line(data = pe_price, aes(x = iter, y = value, color = fuel, linetype=variable), size = 1.2, alpha = 0.5) +
  geom_line(data = pe_prod, aes(x = iter, y = value * secAxisScale, color =fuel,linetype=variable), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold")) +
  scale_y_continuous(sec.axis = sec_axis(~./secAxisScale, name = paste0("PEprod", "(TWh)")))+
  xlab("iteration") + ylab(paste0("PE price",  "(USD/MWh)"))  +
  coord_cartesian(ylim = c(-0.5,80))+
  facet_wrap(~period, nrow = 3)

ggsave(filename = paste0(mypath, run_number, "_iter_PEprice_prod_RM.png"), p1a, width = 28, height =15, units = "in", dpi = 120)

p1b<-ggplot() +
  geom_line(data = peprice_DT, aes(x = iter, y = value, color = fuel, linetype=variable), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold")) +
  scale_y_continuous(sec.axis = sec_axis(~./secAxisScale, name = paste0("PEprod", "(TWh)")))+
  xlab("iteration") + ylab(paste0("PE price",  "(USD/MWh)"))  +
  coord_cartesian(ylim = c(-0.5,80))+
  facet_wrap(~period, nrow = 3)

ggsave(filename = paste0(mypath, run_number, "_iter_PEprice_prod_DT.png"), p1b, width = 28, height =15, units = "in", dpi = 120)


secAxisScale= 1/20
p1c<-ggplot() +
  geom_line(data = pe_price%>% filter(fuel == "pecoal") , aes(x = iter, y = value, linetype=variable), size = 1.2, alpha = 0.5) +
  geom_area(data = trade%>% filter(fuel == "pecoal"), aes(x = iter, y = value * secAxisScale, fill = variable), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold")) +
  scale_y_continuous(sec.axis = sec_axis(~./secAxisScale, name = paste0("PE breakdown", "(TWh)")))+
  xlab("iteration") + ylab(paste0("Coal price",  "(USD/MWh)"))  +
  coord_cartesian(ylim = c(-0.5,80))+
  facet_wrap(~period, nrow = 3)

ggsave(filename = paste0(mypath, run_number, "_iter_COAL_price_prodbreakdown_RM.png"), p1c, width = 28, height =15, units = "in", dpi = 120)

p1d<-ggplot() +
  geom_line(data = pe_price%>% filter(fuel == "pegas") , aes(x = iter, y = value, linetype=variable), size = 1.2, alpha = 0.5) +
  geom_area(data = trade%>% filter(fuel == "pegas"), aes(x = iter, y = value * secAxisScale, fill = variable), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold")) +
  scale_y_continuous(sec.axis = sec_axis(~./secAxisScale, name = paste0("PE breakdown", "(TWh)")))+
  xlab("iteration") + ylab(paste0("Gas price",  "(USD/MWh)"))  +
  coord_cartesian(ylim = c(-0.5,80))+
  facet_wrap(~period, nrow = 3)

ggsave(filename = paste0(mypath, run_number, "_iter_GAS_price_prodbreakdown_RM.png"), p1d, width = 28, height =15, units = "in", dpi = 120)

secAxisScale= 1/5
p1e<-ggplot() +
  geom_line(data = pe_price%>% filter(fuel == "pebiolc") , aes(x = iter, y = value, linetype=variable), size = 1.2, alpha = 0.5) +
  geom_area(data = trade%>% filter(fuel == "pebiolc"), aes(x = iter, y = value * secAxisScale, fill = variable), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold")) +
  scale_y_continuous(sec.axis = sec_axis(~./secAxisScale, name = paste0("PE breakdown", "(TWh)")))+
  xlab("iteration") + ylab(paste0("Biomass price",  "(USD/MWh)"))  +
  coord_cartesian(ylim = c(-0.5,70))+
  facet_wrap(~period, nrow = 3)

ggsave(filename = paste0(mypath, run_number, "_iter_BIO_price_prodbreakdown_RM.png"), p1e, width = 28, height =15, units = "in", dpi = 120)


secAxisScale = 80

# p1f<-ggplot() +
#   geom_line(data = pe_price%>% filter(fuel == "pebiolc"), aes(x = iter, y = value, linetype=variable), size = 1.2, alpha = 0.5, color = "blue") +
#   geom_line(data = capfac_RM%>% filter(tech == "Biomass"), aes(x = iter, y = value * secAxisScale), size = 1.2, alpha = 0.5, color = "red") +
#   geom_line(data = DT_CF%>% filter(tech == "Biomass"), aes(x = iter, y = value/100 * secAxisScale), size = 1.2, alpha = 0.5, color = "orange") +
#   theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold")) +
#   scale_y_continuous(sec.axis = sec_axis(~./secAxisScale, name = paste0("CF %")))+
#   xlab("iteration") + ylab(paste0("Biomass price",  "(USD/MWh)"))  +
#   coord_cartesian(ylim = c(-0.5,70), xlim = c(0,10))+
#   facet_wrap(~period, nrow = 3)
# 
# ggsave(filename = paste0(mypath, run_number, "_iter_BIO_price_capfac_RM.png"), p1f, width = 28, height =15, units = "in", dpi = 120)

# p1g<-ggplot() +
#   geom_line(data = pe_price%>% filter(fuel == "pecoal"), aes(x = iter, y = value, linetype=variable), size = 1.2, alpha = 0.5, color = "blue") +
#   geom_line(data = capfac_RM%>% filter(tech == "Coal"), aes(x = iter, y = value * secAxisScale), size = 1.2, alpha = 0.5, color = "red") +
#   geom_line(data = DT_CF%>% filter(tech == "Lignite"), aes(x = iter, y = value/100 * secAxisScale), size = 1.2, alpha = 0.5, color = "orange") +
#   theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold")) +
#   scale_y_continuous(sec.axis = sec_axis(~./secAxisScale, name = paste0("CF %")))+
#   xlab("iteration") + ylab(paste0("Coal price",  "(USD/MWh)"))  +
#   coord_cartesian(ylim = c(-0.5,70), xlim = c(0,10))+
#   facet_wrap(~period, nrow = 3)
# 
# ggsave(filename = paste0(mypath, run_number, "_iter_COAL_price_capfac_RM.png"), p1g, width = 28, height =15, units = "in", dpi = 120)

# stop()

if (length(files_DT) != 0) {
get_CAPCONvariable <- function(gdx){
  # gdx = sorted_files[[5]]

  budgetdata <- read.gdx(gdx, BUDGETkey1, field="m") %>%
    dplyr::rename(period = ttot)  %>%
    filter(all_regi == REGIkey1) %>%
    mutate(m = -m) %>%
    dplyr::rename(budget = m) %>%
    select(period, budget)
  
    # capcondata1 <- read.gdx(gdx, CapConstraintKey, field="l") %>%
    # mutate(value = -value) %>%
    # select(capcon = value, period = tall)
    # 
    capcondata1 <- read.gdx(gdx, CapConstraintKey, field="m") %>%
    mutate(m = m) %>%
    select(capcon = m, period = ttot)

    # transform from tr$2005/TW to $2015/kW
    capcondata = list(capcondata1, budgetdata) %>%
      reduce(full_join) %>%
      select(period, capcon, budget) %>%
      replace(is.na(.), 0) %>%
      mutate(capcon = capcon/ budget * 1e12 / 1e9 * 1.2) %>%
      select(period, capcon)
    
  return(capcondata)
}

capcon <- lapply(sorted_files, get_CAPCONvariable)

for(fname in filenames){
  idx <- as.numeric(str_extract(fname, "[0-9]+"))
  capcon[[idx]]$iter <- idx
  capcon[[idx]]$model <- "capacity price"
}
capcon <- rbindlist(capcon)
}

get_MARKUPvariable <- function(gdx){
  # gdx = sorted_files[[3]]
  budgetdata <- read.gdx(gdx, BUDGETkey1, field="m", squeeze = FALSE) %>% 
    # filter(ttot == year_toplot) %>%
    filter(all_regi == REGIkey1) %>% 
    mutate(m = -m) %>% 
    dplyr::rename(budget = m)
  
  vrdata0 <- read.gdx(gdx, VARkey2, field="l", squeeze = FALSE)  %>% 
    filter(all_regi == REGIkey1) %>% 
    filter(all_te %in% tech_RM) %>% 
    dplyr::rename(ttot = tall)
  
  vrdata1 <- read.gdx(gdx, VARkey4, field="l", squeeze = FALSE)  %>% 
    filter(all_regi == REGIkey1) %>% 
    filter(all_te %in% tech_RM2) %>% 
    dplyr::rename(ttot = tall) %>% 
    mutate(value = value)
  
  vrdata = list(vrdata0, vrdata1) %>%
    reduce(full_join) %>% 
    left_join(budgetdata) %>% 
    mutate(value = value * 1e12 / sm_TWa_2_MWh * 1.2) %>%  # markup is proportional to pm_seeprice, which is already divided by budget
    filter(all_te %in% names(remind.tech.mapping)) %>% 
    revalue.levels(all_te = remind.tech.mapping) %>% 
    select(ttot,all_te,value) %>% 
    dplyr::group_by(ttot,all_te) %>%
    dplyr::summarise( value = mean(value), .groups = "keep" ) %>% 
    dplyr::ungroup(ttot,all_te) %>% 
    select(tech=all_te,ttot,value)
 
  return(vrdata)
}

get_MRKT_VALUE <- function(gdx){
  # gdx = sorted_files[[5]]

  vrdata1 <- read.gdx(gdx, PARkey3) %>%
    mutate(marketvalue = value) %>%
    filter(all_te %in% tech_RM) %>%
    revalue.levels(all_te = remind.tech.mapping) %>%
    mutate(marketvalue = marketvalue * 1e12 / sm_TWa_2_MWh * 1.2) %>%
    dplyr::group_by(ttot,all_te) %>%
    dplyr::summarise( marketvalue = mean(marketvalue), .groups = "keep" ) %>%
    dplyr::ungroup(ttot,all_te) %>%
    select(period= ttot, tech=all_te, marketvalue)

  vrdata2 <- read.gdx(gdx, PARkey8) %>%
    mutate(marketvalue = value) %>%
    filter(all_te %in% tech_RM2) %>%
    revalue.levels(all_te = remind.tech.mapping) %>%
    mutate(marketvalue = marketvalue * 1e12 / sm_TWa_2_MWh * 1.2) %>%
    select(period= ttot, tech=all_te, marketvalue)

  vrdata = list(vrdata1, vrdata2) %>%
    reduce(full_join)

  return(vrdata)
}
# 
# get_ADJ_COST <- function(gdx){
#   # gdx = sorted_files[[5]]
#   
#   budgetdata <- read.gdx(gdx, BUDGETkey1, field="m", squeeze = FALSE) %>%
#     # filter(ttot == year_toplot) %>%
#     filter(all_regi == REGIkey1) %>%
#     mutate(m = -m) %>%
#     dplyr::rename(budget = m)  %>%
#     select(ttot, budget)
#   
#   adjcost <- read.gdx(gdx, PARkey4) %>% 
#     filter(all_te %in% names(remind.tech.mapping)) %>% 
#     filter(all_regi == REGIkey1) %>%
#     revalue.levels(all_te = remind.tech.mapping) %>%
#     mutate(adjCost = value) %>%
#     select(ttot, all_regi, all_te, adjCost) %>% 
#     dplyr::group_by(ttot, all_regi, all_te) %>%
#     dplyr::summarise( adjCost = mean(adjCost), .groups = "keep" ) %>% 
#     dplyr::ungroup(ttot, all_regi, all_te)
#   
#   vrdata = list(adjcost, budgetdata) %>%
#     reduce(full_join) %>%
#     select(ttot, all_te, adjCost, budget) %>%
#     mutate(adjCost = adjCost / budget * 1e12 / sm_TWa_2_MWh * 1.2) %>% 
#     filter(ttot > 2005) %>% 
#     select(ttot, all_te, adjCost) 
#   
#   return(vrdata)
# }

get_GEN_SHARE <- function(gdx){
  # gdx = sorted_files[[5]]
  
  vrdata <- read.gdx(gdx, PARkey1)  %>% 
    filter(all_regi == REGIkey1) %>% 
    filter(all_te %in% tech_RM) %>% 
    mutate(genshare = value) %>% 
    revalue.levels(all_te = remind.tech.mapping) %>%
    dplyr::group_by(ttot, all_te) %>%
    dplyr::summarise( genshare = sum(genshare), .groups = "keep" ) %>% 
    dplyr::ungroup(ttot, all_te) %>% 
    select(period=ttot, tech=all_te, genshare) 
  
  return(vrdata)
}

get_SEMarginals <- function(gdx){
  # gdx = sorted_files[[5]]
  
  budgetdata <- read.gdx(gdx, BUDGETkey1, field="m", squeeze = FALSE) %>%
    filter(all_regi == REGIkey1) %>%
    mutate(m = -m) %>%
    dplyr::rename(budget = m)  %>%
    select(period= ttot, budget)

  SEprice <- read.gdx(gdx, MARGkey0, field="m") %>%
    filter(all_regi == REGIkey1) %>% 
    mutate(m = -m) %>% 
    dplyr::rename(SEprice = m) %>%
    select(period= ttot,SEprice) %>% 
    left_join(budgetdata) %>% 
    mutate(SEprice = SEprice/budget * 1e12 / sm_TWa_2_MWh * 1.2) %>% 
    select(period,SEprice)
  
  return(SEprice)
}


# stop()
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

readPAR_DT_VF_MV <- function(gdx, key){
  # gdx = sorted_files[[22]]
  # key = PARkey5
  
  vrdata <- read.gdx(gdx, key, squeeze = F) %>% 
    filter(all_te %in% tech_RM) %>% 
    revalue.levels(all_te = remind.tech.mapping) %>%
    dplyr::group_by(ttot, all_te) %>%
    dplyr::summarise( value = mean(value), .groups = "keep" ) %>% 
    dplyr::ungroup(ttot, all_te) %>% 
    select(period=ttot, tech=all_te, value)
    
  return(vrdata)
}

readPAR_DT_Price<- function(gdx, key){
  # gdx = sorted_files[[22]]
  # key = PARkey5
  
  vrdata <- read.gdx(gdx, key, squeeze = F) %>% 
    select(period=ttot,DT_price=value)
  
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

bg <- lapply(sorted_files, get_BUDGET)

mk <- lapply(sorted_files, get_MARKUPvariable)

mv <- lapply(sorted_files, get_MRKT_VALUE)

genSh <- lapply(sorted_files, get_GEN_SHARE)

taxrev <- lapply(sorted_files, readVAR1, key = VARkey3)

reference_mrkup_lastiter <- lapply(sorted_files, readPAR1, key = PARkey2)

DTVF <- lapply(sorted_files, readPAR_DT_VF_MV, key = PARkey5) 

DTMV <- lapply(sorted_files, readPAR_DT_VF_MV, key = PARkey6) 

DT_Price <- lapply(sorted_files, readPAR_DT_Price, key = PARkey7) 

for(fname in filenames){
  idx <- as.numeric(str_extract(fname, "[0-9]+"))
  mk[[idx]]$iter <- idx
  mk[[idx]]$model <- "REMIND markup ($/MWh)"
  mv[[idx]]$iter <- idx
  mv[[idx]]$model <- "REMIND market value ($/MWh)"
  bg[[idx]]$iter <- idx
  bg[[idx]]$model <- "budget"
  
  genSh[[idx]]$iter <- idx
  genSh[[idx]]$model <- "REMIND"
  taxrev[[idx]]$iter <- idx
  taxrev[[idx]]$model <- "total markup tax (REMIND)"
  reference_mrkup_lastiter[[idx]]$iter <- idx
  reference_mrkup_lastiter[[idx]]$model <- "reference markup from last iteration (REMIND)"
  
  DTVF[[idx]]$iter <- idx 
  DTVF[[idx]]$model <- "DIETER value factor"
 
  DTMV[[idx]]$iter <- idx
  
  DT_Price[[idx]]$iter <- idx
}

mk <- rbindlist(mk)
DTVF <- rbindlist(DTVF)
DTMV <- rbindlist(DTMV)
DT_Price <- rbindlist(DT_Price)


mv0 <- rbindlist(mv)


taxrev <- rbindlist(taxrev)
reference_mrkup_lastiter<- rbindlist(reference_mrkup_lastiter)

genSh <- rbindlist(genSh)

bg <- rbindlist(bg)

genSh0 <- genSh %>% 
  select(!model)

mk2 <- mk %>% 
  dplyr::rename(markup = value) %>% 
  left_join(genSh0) %>% 
  filter(!genshare ==0) %>% 
  select(period=ttot, iter,tech,markup,model) %>% 
  # filter(markup >1e-4) %>% 
  filter((period >2015) & (period < 2110))

mv <- mv0 %>% 
  left_join(genSh0) %>% 
  select(period,iter,tech,marketvalue,model)

DT_mrkup = list(DTMV, DT_Price) %>%
  reduce(full_join) %>% 
  mutate(dieter_mrkup = value - DT_price) %>% 
  mutate(model = "DIETER markup ($/MWh)")
  

secAxisScale = 1/8.76

p2<-ggplot() +
  geom_line(data = capcon, aes(x = iter, y = -capcon, color = model), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold")) +
  xlab("iteration") + scale_y_continuous(name = paste0(CapConstraintKey, "(USD/kW)"))+
  # xlab("iteration") + scale_y_continuous(trans='log10', name = paste0(CapConstraintKey, "(USD/kW)"))+
  coord_cartesian(ylim = c(-200,230))+
  facet_wrap(~period, nrow = 3)

ggsave(filename = paste0(mypath, run_number, "_iter_capconShadow_RM.png"), p2, width = 20, height =10, units = "in", dpi = 120)

p4<-ggplot() +
  geom_line(data = mk2, aes(x = iter, y = markup, color=tech), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold")) +
  xlab("iteration") + ylab(paste0("absolute markup",  "(USD/MWh)"))  +
  scale_color_manual(name = "tech", values = mycolors)+
  coord_cartesian(ylim = c(-70,150))+
  facet_wrap(~period, nrow = 3)

ggsave(filename = paste0(mypath, run_number, "_iter_markup_RM.png"), p4, width = 28, height =15, units = "in", dpi = 120)


p4b<-ggplot() +
  geom_line(data = mk2 %>% filter(iter %in% c(10,20,maxiter-1)), aes(x = period, y = markup, color=tech), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"), strip.text = element_text(size = 20)) +
  xlab("year") + ylab(paste0("absolute markup",  "(USD/MWh)"))  +
  scale_color_manual(name = "tech", values = mycolors2)+
  # ggtitle(paste0("REMIND (100$/CO2)"))+
  # theme(plot.title = element_text(size = 19, face = "bold"))+
  coord_cartesian(ylim = c(-70,150))+
  facet_wrap(~iter, nrow = 3)
# print(p4b)

ggsave(filename = paste0(mypath, run_number, "_iter_markup_timeseries_RM_10,20,",maxiter,".png"), p4b, width = 8, height =8, units = "in", dpi = 120)

# p4c<-ggplot() +
#   geom_line(data = mk %>% filter(iter %in% c(maxiter-1)), aes(x = ttot, y = value, color=tech), size = 1.2, alpha = 0.5) +
#   theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"), strip.text = element_text(size = 20)) +
#   xlab("year") + ylab(paste0("absolute markup",  "(USD/MWh)"))  +
#   scale_color_manual(name = "tech", values = mycolors2)+
#   # ggtitle(paste0("REMIND (100$/tCO2)"))+
#   # theme(plot.title = element_text(size = 19, face = "bold"))+
#   coord_cartesian(ylim = c(-70,150))
# # +
#   # facet_wrap(~iter, nrow = 3)
# # print(p4b)
# 
# ggsave(filename = paste0(mypath, run_number, "_iter_markup_timeseries_RM_",maxiter,".png"), p4c, width = 8, height =5, units = "in", dpi = 120)

# stop()

p7<-ggplot() +
  geom_line(data = mv, aes(x = iter, y = marketvalue, color = tech), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20, face="bold")) +
  xlab("iteration") + ylab(paste0("Market Value (REMIND)", "(USD/MWh)"))  +
  scale_color_manual(name = "tech", values = mycolors2)+
  coord_cartesian(ylim = c(-40,250))+
  facet_wrap(~period, nrow = 3)

ggsave(filename = paste0(mypath, run_number, "_iter_market_value_RM.png"), p7, width = 28, height =15, units = "in", dpi = 120)

p8<-ggplot() +
  geom_line(data = taxrev, aes(x = iter, y = value, color = model), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size= 20,face="bold")) +
  xlab("iteration") + ylab(paste0("Total tax markup (REMIND) = markup - reference markup from last iteration"))  +
  coord_cartesian(ylim = c(-50,50))+
  # scale_y_continuous(trans=ssqrt_trans)+
  facet_wrap(~ttot, nrow = 3)

ggsave(filename = paste0(mypath, run_number, "_iter_total_taxrev_markup", "_RM.png"), p8, width = 28, height =15, units = "in", dpi = 120)

p9<-ggplot() +
  geom_line(data = reference_mrkup_lastiter, aes(x = iter, y = value, color = model), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size= 20,face="bold")) +
  xlab("iteration") + ylab(paste0("reference markup from last iteration (REMIND)"))  +
  coord_cartesian(ylim = c(-5,5))+
  # scale_y_continuous(trans=ssqrt_trans)+
  facet_wrap(~ttot, nrow = 3)

ggsave(filename = paste0(mypath, run_number, "_iter_reference_markup_lastiter_RM.png"), p9, width = 28, height =15, units = "in", dpi = 120)



p10<-ggplot() +
  geom_line(data = genSh, aes(x = iter, y = genshare, color = tech,linetype=model), size = 1.2, alpha = 0.5) +
  geom_line(data = genshare_DT, aes(x = iter, y = value, color = tech,linetype=model), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size= 20,face="bold")) +
  xlab("iteration") + ylab(paste0("generation share (%)"))  +
  scale_color_manual(name = "tech", values = mycolors)+
  scale_linetype_manual(name = "model", values = linetypemap)+
  theme(legend.title = element_text(size=25),legend.text = element_text(size=25))+
  coord_cartesian(ylim = c(0,60))+
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  facet_wrap(~period, nrow = 3)

ggsave(filename = paste0(mypath, run_number, "_iter_genshare_RMDT.png"), p10, width = 24, height =12, units = "in", dpi = 120)

genshareRM <- genSh %>% 
  select(period, tech, genshareRM = genshare, iter)

genshare_delta <- genshare_DT %>% 
  select(period, tech, genshareDT = value, iter) %>% 
  left_join(genshareRM) %>% 
  mutate(delta_genshare = genshareRM - genshareDT)

p10b<-ggplot() +
  geom_line(data = genshare_delta, aes(x = iter, y = delta_genshare, color = tech), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size= 20,face="bold")) +
  xlab("iteration") + ylab(paste0("difference of generation share (REMIND-DIETER) (%)"))  +
  scale_color_manual(name = "tech", values = mycolors)+
  # scale_linetype_manual(name = "model", values = linetypemap)+
  theme(legend.title = element_text(size=25),legend.text = element_text(size=25))+
  coord_cartesian(ylim = c(-10,10))+
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  facet_wrap(~period, nrow = 3)

ggsave(filename = paste0(mypath, run_number, "_iter_genshare_delta_RMDT.png"), p10b, width = 24, height =12, units = "in", dpi = 120)


p11<-ggplot() +
  geom_line(data = bg, aes(x = ttot, y = budget), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold")) +
  xlab("iteration") + ylab(paste0("budget", "(USD/MWh)"))  +
  coord_cartesian(ylim = c(0,.26))+
  facet_wrap(~iter, nrow = 3)

ggsave(filename = paste0(mypath, run_number, "_iter_budget_timeseries_RM.png"), p11, width = 28, height =15, units = "in", dpi = 120)

secAxisScale = .5

for(te in tech_RM_names){
  # te="Electrolyzers"
p12<-ggplot() +
  geom_line(data = mk2%>% filter(tech == te), aes(x = iter, y = markup, color = model), size = 1.2, alpha = 0.5) +
  geom_line(data = mv%>% filter(tech == te), aes(x = iter, y = marketvalue, color = model), size = 1.2, alpha = 0.5) +
  geom_line(data = pr, aes(x = iter, y = value, color = model), size = 1.2, alpha = 0.5) +
  # geom_line(data = DTVF%>% filter(tech == te), aes(x = iter, y = value*secAxisScale*100, color = model), size = 1.2, alpha = 0.5) +
  # geom_line(data = DT_mrkup%>% filter(tech == te), aes(x = iter, y = dieter_mrkup, color = model), size = 1.2, alpha = 0.5) +
  geom_line(data = MV_DT%>% filter(tech == te), aes(x = iter, y = value, color = model), size = 1.2, alpha = 0.5) +
  geom_line(data = seelprice_DT, aes(x = iter, y = value, color = model), size = 1.2, alpha = 0.5) +
  # scale_y_continuous(sec.axis = sec_axis(~./secAxisScale, name = paste0("Value Factor (%)")))+
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20, face = "bold")) + 
  theme(legend.text = element_text(size=20))+
  theme(legend.title = element_blank()) +
  xlab("iteration") + ylab(paste0("USD/MWh")) +
  ggtitle(paste0("TECH: ", te))+
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  theme(plot.title = element_text(size = 26, face = "bold"))+
  coord_cartesian(ylim = c(-100,400)) +
  facet_wrap(~period, nrow = 3)

ggsave(filename = paste0(mypath, run_number, "_iter_MV_VF_Price_", te,"_RM.png"), p12, width = 24, height =12, units = "in", dpi = 120)
}

for (iterp in c(14,19,24,maxiter-1)){
  # iterp = 28
RM_GEN_wCurt <- function(gdx){
  # gdx = sorted_files[[30]]
  vrdata<- read.gdx(gdx, "vm_prodSe", factors = FALSE, squeeze = FALSE) %>% 
    filter(all_regi == REGIkey1) %>%
    filter(all_te %in% names(remind.tech.mapping)) %>% 
    filter(all_enty.1 == "seel")  %>% 
    select(period = tall, value,tech=all_te) %>% 
    revalue.levels(tech = remind.tech.mapping) %>%
    dplyr::group_by(period, tech) %>%
    dplyr::summarise( value = sum(value) , .groups = 'keep' ) %>% 
    dplyr::ungroup(period, tech) %>% 
    mutate(value = value * sm_TWa_2_MWh/1e6) %>% 
    mutate(tech = factor(tech, levels=rev(unique(remind.tech.mapping)))) 
  
  return(vrdata)
}

GEN_wCurt <- lapply(sorted_files, RM_GEN_wCurt)

for(fname in filenames){
  idx <- as.numeric(str_extract(fname, "[0-9]+"))
  GEN_wCurt[[idx]]$iter <- idx
}

GEN_wCurt<- rbindlist(GEN_wCurt) 

mv_onlygen0 <- list(GEN_wCurt,mv) %>%
  reduce(full_join)%>% filter(iter == iterp) %>% 
  filter(value >1e-4) %>% 
  filter((period >2015) & (period < 2110))

demandTech_mv <- mv %>% 
  filter(tech %in% c("Electricity (stationary)",
                     "Electrolyzers"))%>% filter(iter == iterp) %>% 
  filter((period >2015) & (period < 2110))

mv_onlygen<- list(mv_onlygen0,demandTech_mv) %>%
  reduce(full_join)

p14<-ggplot() +
  geom_line(data = mv_onlygen, aes(x = period, y = marketvalue, color = tech), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20, face="bold")) +
  xlab("year") + ylab(paste0("Market Value (REMIND)", "(USD/MWh)"))  +
  scale_color_manual(name = "tech", values = mycolors2)+
  coord_cartesian(ylim = c(-50,450))

ggsave(filename = paste0(mypath, run_number, "_market_value_RM_lastiter.png"), p14, width = 8, height =5, units = "in", dpi = 120)


mk_onlygen0 <- list(GEN_wCurt, mk2) %>%
  reduce(full_join)%>% filter(iter == iterp) %>% 
  filter(value >1e-4) %>% 
  filter((period >2015) & (period < 2110))

demandTech_mk <- mk2 %>% 
  filter(tech %in% c("Electricity (stationary)",
                     "Electrolyzers")) %>% filter(iter == iterp) %>% 
  filter((period >2015) & (period < 2110))

mk_onlygen<- list(mk_onlygen0, demandTech_mk) %>%
  reduce(full_join)

p15<-ggplot() +
  geom_line(data = mk_onlygen, aes(x = period, y = markup, color = tech), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20, face="bold")) +
  xlab("year") + ylab(paste0("Markup (REMIND)", "(USD/MWh)"))  +
  scale_color_manual(name = "tech", values = mycolors2)+
  coord_cartesian(ylim = c(-50,120))

ggsave(filename = paste0(mypath, run_number, "_markup_RM_iter=", iterp,".png"), p15, width = 8, height =5, units = "in", dpi = 120)


p13<-ggplot() +
  geom_line(data = mk_onlygen, aes(x = period, y = markup, color = model), size = 1.2, alpha = 0.5) +
  geom_line(data = mv_onlygen, aes(x = period, y = marketvalue, color = model), size = 1.2, alpha = 0.5) +
  geom_line(data = MV_DT%>% filter(iter == iterp) %>% filter((period >2015) & (period < 2110))%>% filter(!tech %in% c("Hard coal", "Lignite")), aes(x = period, y = value, color = model), size = 1.2, alpha = 0.5) +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20, face = "bold")) + 
  theme(legend.text = element_text(size=20), strip.text = element_text(size = 20))+
  theme(legend.title = element_blank()) +
  theme(panel.spacing = unit(1, "lines")) +
  xlab("year") + ylab(paste0("USD/MWh")) +
  ggtitle(paste0("market value and markup time series"))+
  scale_color_manual(name = "model", values = color.mapping.var)+
  theme(aspect.ratio = .6) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  theme(plot.title = element_text(size = 26, face = "bold"))+
  coord_cartesian(xlim = c(2020,2100), ylim = c(-30,400)) +
  facet_wrap(~tech, nrow = 3)

ggsave(filename = paste0(mypath, run_number, "_iter_MV_VF_Price_RM_timeseries_iter=", iterp,".png"), p13, width = 20, height =12, units = "in", dpi = 120)
}

iter_toplot =  c(5,10,15,20,25,maxiter-1)

DT_Price$model <- "DIETER secondary electricity price ($/MWh)"

p14<-ggplot() +
  geom_line(data = pr %>% filter(iter %in% iter_toplot), aes(x = period, y = value, color = model), size = 1.2, alpha = 0.5) +
  geom_line(data = DT_Price %>% filter(iter %in% iter_toplot), aes(x = period, y = DT_price, color = model), size = 1.2, alpha = 0.5) +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20, face = "bold")) + 
  theme(legend.text = element_text(size=20), strip.text = element_text(size = 20))+
  theme(legend.title = element_blank()) +
  theme(panel.spacing = unit(1, "lines")) +
  xlab("year") + ylab(paste0("USD/MWh")) +
  ggtitle(paste0("Electricity price"))+
  scale_color_manual(name = "model", values = color.mapping.var2)+
  theme(aspect.ratio = .6) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  theme(plot.title = element_text(size = 26, face = "bold"))+
  coord_cartesian(xlim = c(2020,2150), ylim = c(-30,400)) +
  facet_wrap(~iter, nrow = 3)

ggsave(filename = paste0(mypath, run_number, "_ElecPrice_timeseries.png"), p14, width = 20, height =12, units = "in", dpi = 120)
