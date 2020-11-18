#for shared variable such as peak demand (one iteration series)

mypath = "~/remind-coupling-dieter/dataprocessing/"
# run_number = "mrkup5_iter"
run_number = "mrkup7"
mydatapath = paste0("~/remind-coupling-dieter/output/", run_number,"/")
# mydatapath2 = "~/remind-coupling-dieter/output/capfac32_valid3/"

# import library
source(paste0(mypath, "library_import.R"))
library(readr)
require(ggplot2)
require(lusweave)
require(rmndt)
igdx("/opt/gams/gams30.2_linux_x64_64_sfx")

miniter = 10
maxiter = 40
#remind output iteration gdx files
files <- list.files(mydatapath, pattern="fulldata_[0-9]+\\.gdx")
sorted_files0 <- paste0(mydatapath, "fulldata_", 1:length(files), ".gdx")
filenames0 <- paste0("fulldata_", 1:length(files), ".gdx")

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

# year_toplot = 2050


BUDGETkey1 = "qm_budget"
VARkey1 = "q32_balSe"
VARkey2 = "vm_flexAdj"
VARkey3 = "v21_taxrevMrkup"
PARkey1 = "p32_DIETERmkup"
# VARkey1 = "v32_seelDem"
REGIkey1 = "DEU"
sm_TWa_2_MWh = 8760000000

CFkey1 = "vm_capFac"
CFkey2 = "pm_dataren"
CFkey3 = "vm_capDistr"

VARsubkey2_RM = "v32_seelDem"

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

plot_RMte_names = c("combined cycle gas", "coal", "solar", "wind", "biomass", "open cycle gas turbine", "hydro", "nuclear")
plot_RMLCOEte_names = c("combined cycle gas", "lignite", "solar", "wind", "biomass", "open cycle gas turbine", "hydro", "nuclear")

TECHkeylst <- c(TECHkeylst_peakGas, TECHkeylst_nonPeakGas, TECHkeylst_coal, TECHkeylst_solar, TECHkeylst_wind, TECHkeylst_hydro, TECHkeylst_biomass, TECHkeylst_nuclear)
TECHVREkeylst <- c(TECHkeylst_solar, TECHkeylst_wind,TECHkeylst_hydro)
TECH_NONVRE_keylst <- c(TECHkeylst_peakGas, TECHkeylst_nonPeakGas, TECHkeylst_coal, TECHkeylst_biomass, TECHkeylst_nuclear)

iter_toplot = 1:length(sorted_files)
# iter_toplot2 = 1:length(sorted_files2)

CapConstraintKey = "q32_peakDemand_DT"
  
get_CAPCONvariable <- function(gdx){
  # gdx = sorted_files[[5]]
  budgetdata <- read.gdx(gdx, BUDGETkey1,field="m") %>% 
    # filter(ttot == year_toplot) %>%
    filter(all_regi == REGIkey1) %>% 
    mutate(m = -m) %>% 
    dplyr::rename(budget = m)  %>% 
    select(ttot, budget)
  
  capcondata <- read.gdx(gdx, CapConstraintKey,field="m") %>% 
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

get_PRICEvariable <- function(gdx){
  # gdx = sorted_files[[5]]
  budgetdata <- read.gdx(gdx, BUDGETkey1,field="m") %>% 
    # filter(ttot == year_toplot) %>%
    filter(all_regi == REGIkey1) %>% 
    mutate(m = -m) %>% 
    dplyr::rename(budget = m)
  
  vrdata0 <- read.gdx(gdx, VARkey1, field="m") %>% 
    # filter(ttot == year_toplot) %>%
    filter(all_regi == REGIkey1) 
  
  vrdata = list(vrdata0, budgetdata) %>%
    reduce(full_join) %>%
    mutate(m = -m/ budget * 1e12 / sm_TWa_2_MWh * 1.2) %>% 
    dplyr::rename(value = m)
    
  return(vrdata)
}

get_MARKUPvariable <- function(gdx){
  # gdx = sorted_files[[5]]
  budgetdata <- read.gdx(gdx, BUDGETkey1,field="m") %>% 
    # filter(ttot == year_toplot) %>%
    filter(all_regi == REGIkey1) %>% 
    mutate(m = -m) %>% 
    dplyr::rename(budget = m)
  
  vrdata0 <- read.gdx(gdx, VARkey2,field="l")  %>% 
    filter(all_regi == REGIkey1) %>% 
    filter(all_te %in% FLEX_tech) %>% 
    dplyr::rename(ttot = tall)
  
  vrdata = list(vrdata0, budgetdata) %>%
    reduce(left_join) %>%
    mutate(value = - value * 1e12 / sm_TWa_2_MWh * 1.2) %>%  # vm_flexAdj is proportional to pm_seeprice, which is already divided by budget
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
    mutate(all_te = str_replace(all_te, TECHkeylst_solar[[1]], plot_RMte_names[[3]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_wind[[1]], plot_RMte_names[[4]])) %>%
    mutate(all_te = str_replace(all_te, TECHkeylst_hydro[[1]], plot_RMte_names[[7]])) %>% 
    select(ttot,all_te,value) %>% 
    dplyr::group_by(ttot,all_te) %>%
    dplyr::summarise( value = mean(value), .groups = "keep" ) %>% 
    dplyr::ungroup(ttot,all_te)
    
  return(vrdata)
}

get_MRKUP_FACTOR <- function(gdx){
  # gdx = sorted_files[[5]]
  vrdata <- read.gdx(gdx, PARkey1)  %>% 
    filter(all_te %in% FLEX_tech) %>% 
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
    mutate(all_te = str_replace(all_te, TECHkeylst_solar[[1]], plot_RMte_names[[3]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_wind[[1]], plot_RMte_names[[4]])) %>%
    mutate(all_te = str_replace(all_te, TECHkeylst_hydro[[1]], plot_RMte_names[[7]])) %>% 
    dplyr::group_by(ttot, all_te) %>%
    dplyr::summarise( value = mean(value), .groups = "keep" ) %>% 
    dplyr::ungroup(ttot, all_te)
  
  return(vrdata)
}

vr1_pr <- lapply(sorted_files, get_PRICEvariable)

# vr1_2 <- lapply(sorted_files2, get_PRICEvariable)

vr1_mk <- lapply(sorted_files, get_MARKUPvariable)

vr1_mkft <- lapply(sorted_files, get_MRKUP_FACTOR) 
# print(vr1[[1]])

for(fname in filenames){
  idx <- as.numeric(str_extract(fname, "[0-9]+"))
  vr1_pr[[idx]]$iter <- idx
  vr1_pr[[idx]]$model <- "coupled run seel price"
  vr1_mk[[idx]]$iter <- idx
  vr1_mk[[idx]]$model <- "coupled run markup"
  vr1_mkft[[idx]]$iter <- idx
  vr1_mkft[[idx]]$model <- "coupled run markup factor"
}

# for(fname in files2){
#   idx <- as.numeric(str_extract(fname, "[0-9]+"))
#   vr1_2[[idx]]$iter <- idx
#   vr1_2[[idx]]$model <- "uncoupled"
# }

vr1_pr <- rbindlist(vr1_pr)
# vr1_2 <- rbindlist(vr1_2)
vr1_mk <- rbindlist(vr1_mk)
vr1_mkft <- rbindlist(vr1_mkft)

vr1_pr2 <-  vr1_pr %>% 
  filter(iter >1) %>% 
  filter(ttot >2005) %>% 
  filter(ttot <2110) %>% 
  select(ttot,iter,value)

vr1_mkft2 <-  vr1_mkft %>% 
  dplyr::rename(mkft = value) %>% 
  select(ttot,all_te,iter,mkft)
  
vr1_MV_RM <- list(vr1_pr2, vr1_mkft2) %>%
  reduce(full_join) %>% 
  mutate(mv_remind = mkft*value)

get_CAPFAC_variable <- function(iteration){
  # iteration = 15
  gdx = sorted_files[[iteration]]
  
  #first the dispatchable
  vrdata <- read.gdx(gdx, CFkey1,field="l", squeeze = FALSE) %>% 
    filter(all_regi == REGIkey1) %>%
    filter(all_te %in% TECHkeylst) %>% 
    filter(!(all_te %in% TECHVREkeylst)) %>% 
    select(ttot, all_te, value) %>% 
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
  
  #second the VRE
  # pm_dataren("nur") = capacity factor at different grade levels (quality of wind resources), pm_dataren is used to represent different cost levels necessary to create the same amount of energy if you have to build your wind farm in places with lower wind. The grades are ordered from 1 to 10. 1 is closer to the wind always blowing (highest nur capacity factor), 10 is the worst place to install wind.
  dataren <- read.gdx(gdx, CFkey2) %>% 
    filter(char == "nur") %>%
    filter(all_regi == REGIkey1) %>%
    filter(all_te %in% TECHVREkeylst) %>% 
    dplyr::rename(ren_nur = value) %>% 
    select(all_te, rlf, ren_nur)
  
  #vm_capDistr is a "helper" variable that stores the amount of capacity that REMIND assign to each grade. Therefore it can have values from any grade. vm_cap is the total capacity (sum of vm_capDistr). It does not have grade infor anymore, everything is assigned to "1". vm_capDistr will retain the information of the optimal grades used in the REMIND decision
  percentage_cap_distr <- read.gdx(gdx, CFkey3) %>% 
    # filter(tall %in% year_toplot) %>%
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
    dplyr::rename(ttot = tall) %>% 
    dplyr::rename(model = all_te)
  
  vrdata_tot$iter <- iteration
  
  return(vrdata_tot)
}

vr1_capfac_RM <- lapply(iter_toplot, get_CAPFAC_variable)
vr1_capfac_RM <- rbindlist(vr1_capfac_RM)

get_DEMvariable_RM <- function(gdx){
  vrdata <- read.gdx(gdx, VARsubkey2_RM, factor = FALSE)  %>% 
    # filter(ttot == year_toplot) %>% 
    filter(all_regi == REGIkey1) %>%
    mutate(value = value* 1e3) %>% 
    select(ttot,value)
  
  return(vrdata)
}

vr1_DEM <- lapply(sorted_files, get_DEMvariable_RM)

for(fname in filenames){
  idx <- as.numeric(str_extract(fname, "[0-9]+"))
  vr1_DEM[[idx]]$iter <- idx
  vr1_DEM[[idx]]$legend <- "total demand"
}

vr1_DEM <- rbindlist(vr1_DEM)

secAxisScale = 1/8.76

p1<-ggplot() +
  geom_line(data = vr1_pr, aes(x = iter, y = value, color = model), size = 1.2, alpha = 0.5) +
  geom_line(data = vr1_capcon, aes(x = iter, y = capcon*secAxisScale, color = model), size = 1.2, alpha = 0.5) +
  # geom_line(data = vr1_2, aes(x = iter, y = value, color = model), size = 1.2, alpha = 0.5) +
  scale_y_continuous(sec.axis = sec_axis(~./secAxisScale, name = paste0(CapConstraintKey, "(USD/kW)")))+
  theme(axis.text=element_text(size=10), axis.title=element_text(size= 10,face="bold")) +
  xlab("iteration") + ylab(paste0(VARkey1, "(USD/MWh)"))  +
  coord_cartesian(ylim = c(-20,200))+
  facet_wrap(~ttot, nrow = 3)
  
ggsave(filename = paste0(mypath, "iter_seelprice_", run_number, "_RM.png"),  p1, width = 28, height =15, units = "in", dpi = 120)

p2<-ggplot() +
  geom_line(data = vr1_capcon, aes(x = iter, y = capcon, color = model), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size= 10,face="bold")) +
  xlab("iteration") + scale_y_continuous(name = paste0(CapConstraintKey, "(USD/kW)"))+
  # xlab("iteration") + scale_y_continuous(trans='log10', name = paste0(CapConstraintKey, "(USD/kW)"))+
  coord_cartesian(ylim = c(0.001,200))+
  facet_wrap(~ttot, nrow = 3)

ggsave(filename = paste0(mypath, "iter_capconShadow_", run_number,"_RM.png"), p2, width = 28, height =15, units = "in", dpi = 120)


secAxisScale = 2

p3<-ggplot() +
  geom_line(data = vr1_pr, aes(x = iter, y = value, color = model), size = 1.2, alpha = 0.5) +
  geom_line(data = vr1_capfac_RM, aes(x = iter, y = value*100*secAxisScale, color = model), size = 1.2, alpha = 0.5) +
  scale_y_continuous(sec.axis = sec_axis(~./secAxisScale, name = paste0("CF", "(%)")))+
  scale_color_manual(name = "model", values = mycolors)+
  theme(axis.text=element_text(size=10), axis.title=element_text(size= 10,face="bold")) +
  xlab("iteration") + ylab(paste0(VARkey1, "(USD/MWh)"))  +
  coord_cartesian(ylim = c(-20,200))+
  facet_wrap(~ttot, nrow = 3)

ggsave(filename = paste0(mypath, "iter_seelprice_wcapfac_", run_number, "_RM.png"), p3, width = 28, height =15, units = "in", dpi = 120)


p4<-ggplot() +
  geom_line(data = vr1_mk, aes(x = iter, y = value, color = all_te), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size= 10,face="bold")) +
  xlab("iteration") + ylab(paste0("absolute markup",  "(USD/MWh)"))  +
  scale_color_manual(name = "tech", values = mycolors)+
  coord_cartesian(ylim = c(-200,200))+
  facet_wrap(~ttot, nrow = 3)

ggsave(filename = paste0(mypath, "iter_markup_", run_number, "_RM.png"), p4, width = 28, height =15, units = "in", dpi = 120)


# secAxisScale = 1/10

p5<-ggplot() +
  geom_line(data = vr1_pr, aes(x = iter, y = value, color = model), size = 1.2, alpha = 0.5) +
  geom_line(data = vr1_DEM, aes(x = iter, y = value, color =legend), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size= 10,face="bold")) +
  scale_y_continuous(sec.axis = sec_axis(~./secAxisScale, name = paste0("total demand", "(TWh)")))+
  xlab("iteration") + ylab(paste0("wholesale electricity price",  "(USD/MWh)"))  +
  coord_cartesian(ylim = c(-20,500))+
  facet_wrap(~ttot, nrow = 3)

ggsave(filename = paste0(mypath, "iter_price_dem_", run_number, "_RM.png"), p5, width = 28, height =15, units = "in", dpi = 120)

p6<-ggplot() +
  geom_line(data = vr1_mkft, aes(x = iter, y = value, color = all_te), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size= 10,face="bold")) +
  xlab("iteration") + ylab(paste0("markup factor = MarketValue(DIETER)/Elec.Price(DIETER)"))  +
  coord_cartesian(ylim = c(0,5))+
  facet_wrap(~ttot, nrow = 3)

ggsave(filename = paste0(mypath, "iter_value_factor_", run_number, "_DT.png"), p6, width = 28, height =15, units = "in", dpi = 120)

p7<-ggplot() +
  geom_line(data = vr1_MV_RM, aes(x = iter, y = mv_remind, color = all_te), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size= 10,face="bold")) +
  xlab("iteration") + ylab(paste0("Market.Value(REMIND)"))  +
  coord_cartesian(ylim = c(-40,350))+
  facet_wrap(~ttot, nrow = 3)

ggsave(filename = paste0(mypath, "iter_market_value_", run_number, "_RM.png"), p7, width = 28, height =15, units = "in", dpi = 120)
