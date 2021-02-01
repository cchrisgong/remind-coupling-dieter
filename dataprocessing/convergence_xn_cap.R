mypath = "~/remind-coupling-dieter/dataprocessing/"
run_number = "mrkup30"
# run_number = "mrkup14_uncoupl"
mydatapath = paste0("~/remind-coupling-dieter/output/", run_number, "/")

# import library
source(paste0(mypath, "library_import.R"))
library(readr)
require(rmndt)

igdx("/opt/gams/gams30.2_linux_x64_64_sfx")

maxiter = 35

# remind output iteration gdx files
filenames0 <- list.files(mydatapath, pattern="fulldata_[0-9]+\\.gdx")
sorted_files0 <- paste0(mydatapath, "fulldata_", 1:length(filenames0), ".gdx")
files0 <- paste0("fulldata_", 1:length(filenames0), ".gdx")

sorted_files <- sorted_files0[1:maxiter]
files <- files0[1:maxiter]

# files_full <- list.files(mydatapath, pattern="fulldata_[0-9]+\\.gdx")
# sorted_files_full <- paste0(mydatapath, "fulldata_", 1:length(files_full), ".gdx")
# files_nonopt <- list.files(mydatapath, pattern="non_optimal_[0-9]+\\.gdx")
# sorted_files_nonopt <- paste0(mydatapath, "non_optimal_", (length(files_full)+1):(length(files_full)+length(files_nonopt)), ".gdx")
# sorted_files = c(sorted_files_full, sorted_files_nonopt)
# files = c(files_full, files_nonopt)

# dieter output iteration gdx files
files_DT0 <- list.files(mydatapath, pattern="results_DIETER_i[0-9]+\\.gdx")
# sorted_files_DT <- paste0(mydatapath, "results_DIETER_i", seq(from = 4, to = length(files_DT)*4, by = 4), ".gdx")
# sorted_files_DT <- paste0(mydatapath, "results_DIETER_i", seq(from = 5, to = length(files_DT)*5, by = 5), ".gdx")
sorted_files_DT0 <- paste0(mydatapath, "results_DIETER_i", seq(from = 2, to = length(files_DT0)+1, by = 1), ".gdx")

files_DT <- files_DT0[1:maxiter]
sorted_files_DT <- sorted_files_DT0[1:maxiter]

# year_toplot_list <- c(2035,2045)
year_toplot_list <- c(2020,2030,2040,2050,2060,2070,2080)

iter_toplot = 1:length(sorted_files)

for(year_toplot in year_toplot_list){
# year_toplot = 2050

VARkey1 = "vm_cap"
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
TECHVREkeylst <- c(TECHkeylst_solar, TECHkeylst_wind,TECHkeylst_hydro)

VARkey1_DT = "report4RM"
VARsubkey1_DT = "capacity"
TECHkeylst_DT = c("CCGT", "lig", "Solar", "Wind_on", "bio", "OCGT_eff", "ror", "nuc", "hc")
plot_DTte_names = c("combined cycle gas", "lignite", "solar", "wind", "biomass", "open cycle gas turbine", "hydro", "nuclear", "hard coal")
plot_RMte_names = c("combined cycle gas", "coal", "solar", "wind", "biomass", "open cycle gas turbine", "hydro", "nuclear")
mycolors <- c("combined cycle gas" = "#999959", "lignite" = "#0c0c0c", "coal" = "#0c0c0c", "solar" = "#ffcc00", "wind" = "#337fff", "biomass" = "#005900", "open cycle gas turbine" = "#e51900", "hydro" =  "#191999", "nuclear" =  "#ff33ff", "hard coal" = "#808080")

VARsubkey1_RM = "p32_peakDemand_relFac"
VARsubkey2_RM = "p32_seelDem"

get_DEMvariable_RM <- function(gdx){
  # gdx = sorted_files[[2]]
  # vrdata1 <- read.gdx(gdx, VARsubkey1_RM, factor = FALSE)  %>% 
  #   filter(tall == year_toplot) %>% 
  #   mutate(relFac = value)
  
  vrdata2 <- read.gdx(gdx, VARsubkey2_RM, factor = FALSE) %>% 
    filter(ttot == year_toplot) %>% 
    filter(all_regi == REGIkey1) %>%
    filter(all_enty == "seel") %>%
    mutate(totDem = value) %>% 
    select(ttot,totDem) %>% 
    dplyr::rename(tall = ttot)
 
  # vrdata0 = list(vrdata1, vrdata2) %>%
  #   reduce(full_join) 
    
  vrdata <- vrdata2 %>% 
    mutate(value = totDem * 0.000155891 * 8760 * 1e3) 
  
  return(vrdata)
}

vr1_DEM <- lapply(sorted_files, get_DEMvariable_RM)

for(fname in files){
  idx <- as.numeric(str_extract(fname, "[0-9]+"))
  vr1_DEM[[idx]]$iter <- idx
  vr1_DEM[[idx]]$model <- "REMIND"
}

vr1_DEM <- rbindlist(vr1_DEM)

get_variable <- function(gdx){
  # gdx = sorted_files[[5]]
  vrdata <- read.gdx(gdx, VARkey1, factors = FALSE, squeeze = FALSE) %>% 
    filter(tall == year_toplot) %>%
    filter(all_regi == REGIkey1) %>%
    filter(all_te %in% TECHkeylst) %>%
    filter(rlf == grade) %>% 
    mutate(value = value*1e3) %>% 
    select(all_te, rlf, value) %>% 
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
    mutate(all_te = str_replace(all_te, TECHkeylst_wind[[1]], plot_RMte_names[[4]])) %>%
    mutate(all_te = str_replace(all_te, TECHkeylst_nuclear[[1]], plot_RMte_names[[8]])) %>%
    mutate(all_te = str_replace(all_te, TECHkeylst_hydro[[1]], plot_RMte_names[[7]])) %>%
    mutate(all_te = str_replace(all_te, TECHkeylst_peakGas[[1]], plot_RMte_names[[6]])) %>% 
    dplyr::group_by(all_te, rlf) %>%
    dplyr::summarise( value = sum(value) , .groups = 'keep' ) %>% 
    dplyr::ungroup(all_te, rlf)

    vrdata$all_te <- factor(vrdata$all_te, levels= c("solar", "wind", "combined cycle gas", "biomass", "open cycle gas turbine", "hydro", "nuclear", "coal"))
    return(vrdata)
}

get_variable_DT <- function(gdx){
  # gdx = sorted_files_DT[[2]]
  vrdata <- read.gdx(gdx, VARkey1_DT, factor = FALSE, squeeze = FALSE) %>%
    filter(X..1 == year_toplot) %>%
    dplyr::rename(all_te = X..3) %>%
    filter(all_te %in% TECHkeylst_DT) %>%
    filter(X..4 == VARsubkey1_DT) %>%
    mutate(value = value/1e3) %>%
    mutate(all_te = str_replace(all_te, TECHkeylst_DT[[1]], plot_DTte_names[[1]])) %>%
    mutate(all_te = str_replace(all_te, TECHkeylst_DT[[2]], plot_DTte_names[[2]])) %>%
    mutate(all_te = str_replace(all_te, TECHkeylst_DT[[3]], plot_DTte_names[[3]])) %>%
    mutate(all_te = str_replace(all_te, TECHkeylst_DT[[4]], plot_DTte_names[[4]])) %>%
    mutate(all_te = str_replace(all_te, TECHkeylst_DT[[5]], plot_DTte_names[[5]])) %>%
    mutate(all_te = str_replace(all_te, TECHkeylst_DT[[6]], plot_DTte_names[[6]])) %>%
    mutate(all_te = str_replace(all_te, TECHkeylst_DT[[7]], plot_DTte_names[[7]])) %>%
    mutate(all_te = str_replace(all_te, TECHkeylst_DT[[8]], plot_DTte_names[[8]])) %>%
    mutate(all_te = str_replace(all_te, TECHkeylst_DT[[9]], plot_DTte_names[[9]]))

  vrdata$all_te <- factor(vrdata$all_te, levels= c("solar", "wind", "combined cycle gas", "biomass", "open cycle gas turbine", "hydro", "nuclear", "lignite", "hard coal"))

    return(vrdata)
}

vrN <- lapply(sorted_files, get_variable)
# print(vrN[[1]])

vrN_DT <- lapply(sorted_files_DT, get_variable_DT)

for(fname in files){
  idx <- as.numeric(str_extract(fname, "[0-9]+"))
  vrN[[idx]]$iter <- idx
  vrN[[idx]]$model <- "REMIND"
}

idx_DT <- 1:maxiter
for(id in idx_DT){
  # vrN_DT[[id]]$iter <- id * 5
  # vrN_DT[[id]]$iter <- id * 4
  vrN_DT[[id]]$iter <- id + 1
  vrN_DT[[id]]$model <- "DIETER"
}

vrN <- rbindlist(vrN)
vrN_DT <- rbindlist(vrN_DT)

# vrN <- vrN %>%
#   filter(!(all_te %in% c("solar", "wind", "open cycle gas turbine", "biomass")))

CFkey1 = "vm_capFac"
CFkey2 = "pm_dataren"
CFkey3 = "vm_capDistr"

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
    dplyr::rename(model = all_te) %>% 
    filter(ttot == year_toplot)
  
  vrdata_tot$iter <- iteration-1
  
  return(vrdata_tot)
}

vr1_capfac_RM <- lapply(iter_toplot, get_CAPFAC_variable)
vr1_capfac_RM <- rbindlist(vr1_capfac_RM)

secAxisScale = 2

p2<-ggplot() +
  geom_area(data = vrN_DT, aes(x = iter, y = value, fill = all_te), size = 1.2, alpha = 0.5) +
  geom_line(data = vr1_DEM, aes(x = iter+1, y = value), size = 1.2, alpha = 1,linetype="dotted") +
  geom_line(data = vr1_capfac_RM, aes(x = iter, y = value*100*secAxisScale, color = model), size = 1.2, alpha = 0.5) +
  scale_y_continuous(sec.axis = sec_axis(~./secAxisScale, name = paste0("CF", "(%)")))+
  scale_fill_manual(name = "Technology", values = mycolors)+
  # scale_color_manual(values=c("black"))+
  scale_color_manual(name = "model", values = mycolors)+
  theme(axis.text=element_text(size=14), axis.title=element_text(size= 14,face="bold")) +
  xlab("iteration") + ylab(paste0(VARkey1, "(GW)")) +
  coord_cartesian(xlim = c(0, max(vrN_DT$iter)))+
  ggtitle(paste0("DIETER ", year_toplot))+
  theme(plot.title = element_text(size = 16, face = "bold"))+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text=element_text(size=14)) +
  theme(aspect.ratio = .5)

p1<-ggplot() +
  geom_area(data = vrN, aes(x = iter, y = value, fill = all_te), size = 1.2, alpha = 0.5) +
  geom_line(data = vr1_DEM, aes(x = iter, y = value), size = 1.2, alpha = 0.5,linetype="dotted") +
  geom_line(data = vr1_capfac_RM, aes(x = iter+1, y = value*100*secAxisScale, color = model), size = 1.2, alpha = 0.5) +
  scale_y_continuous(sec.axis = sec_axis(~./secAxisScale, name = paste0("CF", "(%)")))+
  scale_fill_manual(name = "Technology", values = mycolors) +
  # scale_color_manual(values=c("black"))+
  scale_color_manual(name = "model", values = mycolors)+
  theme(axis.text=element_text(size=14), axis.title=element_text(size= 14,face="bold")) + 
  xlab("iteration") + ylab(paste0(VARkey1, "(GW)")) +
  ggtitle(paste0("REMIND ", year_toplot))+
  coord_cartesian(xlim = c(0, max(vrN$iter))) +
  theme(plot.title = element_text(size = 16, face = "bold")) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text=element_text(size=14)) +
  theme(aspect.ratio = .5)

                    
library(grid)
grid.newpage()
p <- arrangeGrob(rbind(ggplotGrob(p1), ggplotGrob(p2)))
grid.draw(p)

ggsave(filename = paste0(mypath, "CAP_", run_number, "_", year_toplot, "wcapfac.png"),  p,  width = 12, height =16, units = "in", dpi = 120)
# ggsave(filename = paste0(mypath, "CAP_", run_number, "_uncoupl", year_toplot, ".png"),  p,  width = 12, height =16, units = "in", dpi = 120)
}

