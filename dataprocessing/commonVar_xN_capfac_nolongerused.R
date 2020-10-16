#for shared variable such as peak demand (one iteration series)

mypath = "~/remind/dataprocessing/"
run_number = "32_valid1"
mydatapath =  paste0("~/remind/output/capfac", run_number, "/")
mydatapath2 =  paste0("~/remind/output/capfac", 21, "_uncoupl/")

# import library
source(paste0(mypath, "library_import.R"))
library(readr)
require(ggplot2)
require(lusweave)
require(rmndt)
igdx("/opt/gams/gams30.2_linux_x64_64_sfx")

#dieter output iteration gdx files
files_DT <- list.files(mydatapath, pattern="results_DIETER_i[0-9]+\\.gdx")
sorted_files_DT <- paste0(mydatapath, "results_DIETER_i", seq(from = 5, to = length(files_DT)*5, by = 5), ".gdx")

files_DT2 <- list.files(mydatapath2, pattern="results_DIETER_i[0-9]+\\.gdx")
sorted_files_DT2 <- paste0(mydatapath2, "results_DIETER_i", seq(from = 5, to = length(files_DT2)*5, by = 5), ".gdx")

year_toplot_list <- c(2015, 2020, 2025, 2030, 2035, 2050) 
for(year_toplot in year_toplot_list){
  
# year_toplot = 2050
maxiter = 100
print(paste0("Year: ", year_toplot))


VARkey1_DT = "report4RM"
VARsubkey1_DT = "capfac"
TECHkeylst_DT = c("CCGT", "lig", "Solar", "Wind_on", "bio", "OCGT_eff", "ror", "nuc", "hc")
plot_DTte_names = c("combined cycle gas", "lignite", "solar", "wind", "biomass", "open cycle gas turbine", "hydro", "nuclear", "hard coal")
plot_RMte_names = c("combined cycle gas", "lignite", "solar", "wind", "biomass", "open cycle gas turbine", "hydro", "nuclear", "hard coal")

VARkey1 = "vm_capFac"
VARkey1_b = "pm_dataren"
VARkey1_c = "vm_capDistr"

TECHkeylst_peakGas = c("ngt")
TECHkeylst_nonPeakGas = c("ngccc","ngcc",  "gaschp") 
TECHkeylst_coal = c("coalchp", "igccc", "igcc", "pcc", "pco","pc")
TECHkeylst_solar = c("spv")
TECHkeylst_wind = c("wind")
TECHkeylst_hydro = c("hydro")
TECHkeylst_nuclear = c("tnrs")
TECHkeylst_placeholder = c("fnrs")
TECHkeylst_biomass = c("biochp", "bioigccc", "bioigcc")

TECHkeylst <- c(TECHkeylst_peakGas, TECHkeylst_nonPeakGas, TECHkeylst_coal, TECHkeylst_solar, TECHkeylst_wind, TECHkeylst_hydro, TECHkeylst_biomass, TECHkeylst_nuclear,TECHkeylst_placeholder)
TECHVREkeylst <- c(TECHkeylst_solar, TECHkeylst_wind,TECHkeylst_hydro)

get_CAPFAC_variable <- function(gdx){
  gdx = sorted_files[[1]]
  
  vrdata <- read.gdx(gdx, VARkey1,field="l") %>% 
    filter(ttot == year_toplot) %>%
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
    mutate(all_te = str_replace(all_te, TECHkeylst_placeholder[[1]], plot_RMte_names[[9]])) %>% 
    dplyr::group_by(all_te) %>%
    dplyr::summarise( value = mean(value), .groups = 'keep' ) %>% 
    dplyr::ungroup(all_te)
  
  vrdata$value[vrdata$all_te == "hard coal"] <- vrdata$value[vrdata$all_te == "lignite"]
                                                                  
  # pm_dataren("nur") = capacity factor at different grade levels (quality of wind resources), pm_dataren is used to represent different cost levels necessary to create the same amount of energy if you have to build your wind farm in places with lower wind. The grades are ordered from 1 to 10. 1 is closer to the wind always blowing (highest nur capacity factor), 10 is the worst place to install wind.
  dataren <- read.gdx(gdx, VARkey1_b) %>% 
    filter(char == "nur") %>%
    filter(all_regi == REGIkey1) %>%
    filter(all_te %in% TECHVREkeylst) %>% 
    dplyr::rename(ren_nur = value) %>% 
    select(all_te, rlf, ren_nur)

  #vm_capDistr is a "helper" variable that stores the amount of capacity that REMIND assign to each grade. Therefore it can have values from any grade. vm_cap is the total capacity (sum of vm_capDistr). It does not have grade infor anymore, everything is assigned to "1". vm_capDistr will retain the information of the optimal grades used in the REMINd decision
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

  vrdata$all_te <- factor(vrdata$all_te, levels= c("combined cycle gas", "biomass", "open cycle gas turbine", "nuclear", "lignite", "hard coal","solar", "wind", "hydro"))
  
  return(vrdata)
}

vr1 <- lapply(sorted_files, get_CAPFAC_variable)

for(fname in files){
  idx <- as.numeric(str_extract(fname, "[0-9]+"))
  vr1[[idx]]$iter <- idx
  vr1[[idx]]$model <- "REMIND default (uncoupled)"
}
vr1 <- rbindlist(vr1)

get_variable_DT <- function(gdx){
  # gdx = sorted_files_DT[[1]]
  vrdata <- read.gdx(gdx, VARkey1_DT) %>% 
    filter(X..1 == year_toplot) %>%
    dplyr::rename(all_te = X..3) %>% 
    filter(all_te %in% TECHkeylst_DT) %>% 
    filter(X..4 == VARsubkey1_DT) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_DT[[1]], plot_DTte_names[[1]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_DT[[2]], plot_DTte_names[[2]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_DT[[3]], plot_DTte_names[[3]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_DT[[4]], plot_DTte_names[[4]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_DT[[5]], plot_DTte_names[[5]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_DT[[6]], plot_DTte_names[[6]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_DT[[7]], plot_DTte_names[[7]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_DT[[8]], plot_DTte_names[[8]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_DT[[9]], plot_DTte_names[[9]])) 
  
  vrdata$all_te <- factor(vrdata$all_te, levels= c("combined cycle gas", "biomass", "open cycle gas turbine", "nuclear", "lignite", "hard coal","solar", "wind", "hydro"))
  
  return(vrdata)
}

vr1_DT <- lapply(sorted_files_DT, get_variable_DT)
vr1_DT2 <- lapply(sorted_files_DT2, get_variable_DT)

idx_DT <- 1:length(files_DT)
idx_DT2 <- 1:length(files_DT2)

for(id in idx_DT){
  vr1_DT[[id]]$iter <- id * 5
  vr1_DT[[id]]$model <- "coupled"
}

for(id in idx_DT2){
  vr1_DT2[[id]]$iter <- id * 5
  vr1_DT2[[id]]$model <- "DIETER (uncoupled)"
}

vr1_DT <- rbindlist(vr1_DT)
vr1_DT2 <- rbindlist(vr1_DT2)

p1<-ggplot() +
  geom_line(data = vr1_DT, aes(x = iter, y = value, color = model), size = 1.2, alpha = 0.5) +
  geom_line(data = vr1_DT2, aes(x = iter, y = value, color = model), size = 1.2, alpha = 0.5) +
  geom_line(data = vr1, aes(x = iter, y = value, color = model), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size= 10,face="bold")) +
  ggtitle(year_toplot)+
  xlab("iteration") + ylab(paste0(VARsubkey1_DT)) +
  facet_wrap(~all_te, nrow = 3)
# +
  # coord_cartesian(ylim = c(0,80)) 

ggsave(filename = paste0(mypath, "iter_capfac_capfac", run_number, "_", year_toplot, ".png"),  width = 8, height =6, units = "in", dpi = 120)


}

