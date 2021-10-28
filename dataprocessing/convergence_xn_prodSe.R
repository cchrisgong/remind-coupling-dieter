mypath = "~/remind-coupling-dieter/dataprocessing/"
run_number = "hydro365"
mydatapath = paste0("~/remind-coupling-dieter/output/", run_number, "/")
# import library
source(paste0(mypath, "library_import.R"))
library(readr)
require(rmndt)
library(forcats)

igdx("/opt/gams/gams30.2_linux_x64_64_sfx")


# remind output iteration gdx files
filenames <- list.files(mydatapath, pattern="fulldata_[0-9]+\\.gdx")
sorted_paths <- paste0(mydatapath, "fulldata_", 1:length(filenames), ".gdx")
sorted_files <- paste0("fulldata_", 1:length(filenames), ".gdx")
maxiter = length(filenames)
  
# dieter output iteration gdx files
files_DT <- list.files(mydatapath, pattern="results_DIETER_i[0-9]+\\.gdx")
id <- NULL
for(fname in files_DT){
  idx <- as.numeric(str_extract(fname, "[0-9]+"))
  id = c(id, idx)
}

if (length(files_DT) != 0) {
sorted_files_DT <- paste0("results_DIETER_i", sort(id), ".gdx")
sorted_paths_DT <- paste0(mydatapath, "results_DIETER_i", sort(id), ".gdx")
}

year_toplot_list <- c(2020,2030,2040,2050,2060,2070,2080)
year_toplot_list <- c(2025,2030,2035,2040,2045,2050,2055,2060,2070,2080,2090,2100,2110,2130,2150)
# year_toplot_list <- c(2025,2035)
  
# year_toplot = 2035
# iter_toplot = 30
sm_TWa_2_MWh = 8760000000

print(paste0("Year: ", year_toplot_list))
# print(paste0("Iteration: ", iter_toplot))

VARkey1 = "vm_prodSe"
VARkey2 = "vm_usableSeTe"

VARkey4 = "p32_seelUsableDem" # normal electricity demand
VARkey5 = "vm_demSe" 


REGIkey1 = "DEU"
# REGIkey1 = "USA"
ProdKEY1 = "seel"
ProdKEY2 = "seh2"
TECHkeylst_peakGas = c("ngt")
TECHkeylst_nonPeakGas = c("ngccc","ngcc",  "gaschp") 
TECHkeylst_coal = c("coalchp", "igccc", "igcc", "pcc", "pco","pc")
TECHkeylst_solar = c("spv")
TECHkeylst_wind = c("wind")
TECHkeylst_hydro = c("hydro")
TECHkeylst_nuclear = c("tnrs")
TECHkeylst_biomass = c("biochp", "bioigccc", "bioigcc")

TECHkeylst <- c(TECHkeylst_peakGas, TECHkeylst_nonPeakGas, TECHkeylst_coal, TECHkeylst_solar, TECHkeylst_wind, TECHkeylst_hydro, TECHkeylst_biomass, TECHkeylst_nuclear)

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

# shifting hydro to dispatchable because in REMIND usable energy is only defined for spv, wind, csp
remind.nonvre.mapping2 <- c(remind.nonvre.mapping, hydro = "Hydro")

remind.vre.mapping <- c(wind = "Wind",
                        spv = "Solar",
                        NULL)

table_ordered_name = c("Solar", "Wind", "Biomass", "Hydro", "Nuclear","CCGT", "OCGT", "Coal (Lig + HC)", "Lignite", "Hard coal")
table_ordered_name_dem = c("Electricity used for Electrolysis","Electricity")

remind.tech.mapping <- c(remind.nonvre.mapping2, remind.vre.mapping)

dieter.supply.tech.mapping <- c(hc = "Hard coal",
                         lig = "Lignite",
                         coal = "Coal (Lig + HC)",
                         nuc = "Nuclear",
                         OCGT_eff = "OCGT",
                         CCGT = "CCGT",
                         bio = "Biomass",
                         ror = "Hydro",
                         Wind_on = "Wind",
                         Solar = "Solar",
                         NULL)

dieter.demand.tech.mapping <- c(seel = "Electricity",
                                elh2 = "Electricity used for Electrolysis",
                                NULL)

dieter.tech.mapping <- c(dieter.supply.tech.mapping, dieter.demand.tech.mapping)

VARkey1_DT = "p32_report4RM"
VARsubkey1_DT = c("total generation", "usable generation", "total consumption")
# VARsubkey1_DT = c("total generation", "usable generation")
TECHkeylst_DT = c("CCGT", "lig", "Solar", "Wind_on", "bio", "OCGT_eff", "ror", "nuc", "hc","elh2","seel")

color.mapping1 <- c("CCGT" = "#999959", "Coal (Lig + HC)" = "#0c0c0c",
                   "Solar" = "#ffcc00", "Wind" = "#337fff", "Biomass" = "#005900",
                   "OCGT" = "#e51900", "Hydro" = "#191999", "Nuclear" = "#ff33ff", "Electricity used for Electrolysis" = "#48D1CC", "Electricity" = "#6495ED")

color.mapping2 <- c("CCGT" = "#999959", "Lignite" = "#0c0c0c",
                   "Solar" = "#ffcc00", "Wind" = "#337fff", "Biomass" = "#005900",
                   "OCGT" = "#e51900", "Hydro" = "#191999", "Nuclear" = "#ff33ff",
                   "Hard coal" = "#808080", "Electricity used for Electrolysis" = "#48D1CC", "Electricity" = "#6495ED")
# Coal        Oil        Gas    Biomass    Nuclear      Hydro       Wind      Solar Geothermal 
# "#0c0c0c"  "#cc7500"  "#999959"  "#005900"  "#ff33ff"  "#191999"  "#337fff"  "#ffcc00"  "#e51900"

get_variable <- function(gdx){
  # gdx = sorted_files[[9]]
  vrdata_disp<- read.gdx(gdx, VARkey1, factors = FALSE, squeeze = FALSE) %>% 
    filter(tall %in% year_toplot_list) %>%
    filter(all_regi == REGIkey1) %>%
    filter(all_te %in% names(remind.nonvre.mapping2)) %>% 
    filter(all_enty.1 == ProdKEY1) %>% 
    select(period = tall, all_te, value) %>% 
    revalue.levels(all_te = remind.nonvre.mapping2) %>%
    mutate(value = value*sm_TWa_2_MWh/1e6) %>% 
    dplyr::group_by(period, all_te) %>%
    dplyr::summarise( value = sum(value) , .groups = 'keep' ) %>% 
    dplyr::ungroup(period, all_te) 
  
  vrdata_vre<- read.gdx(gdx, VARkey2, factors = FALSE, squeeze = FALSE) %>% 
    filter(ttot %in% year_toplot_list) %>%
    filter(all_regi == REGIkey1) %>%
    filter(all_te %in% names(remind.vre.mapping)) %>% 
    filter(entySe == ProdKEY1)  %>% 
    select(period = ttot, all_te, value) %>% 
    revalue.levels(all_te = remind.vre.mapping) %>% 
    mutate(value = value*sm_TWa_2_MWh/1e6) %>% 
    dplyr::group_by(period, all_te) %>%
    dplyr::summarise( value = sum(value) , .groups = 'keep' ) %>% 
    dplyr::ungroup(period, all_te)
  
  vrdata <- list(vrdata_disp, vrdata_vre) %>% 
    reduce(full_join) %>% 
    mutate(all_te = factor(all_te, levels=rev(unique(remind.tech.mapping)))) 
    
    return(vrdata)
}

RM_GEN_wCurt <- function(gdx){
  # gdx = sorted_files[[30]]
  vrdata<- read.gdx(gdx, VARkey1, factors = FALSE, squeeze = FALSE) %>% 
    filter(tall %in% year_toplot_list) %>%
    filter(all_regi == REGIkey1) %>%
    filter(all_te %in% names(remind.tech.mapping)) %>% 
    filter(all_enty.1 == ProdKEY1)  %>% 
    select(period = tall, value,all_te) %>% 
    revalue.levels(all_te = remind.tech.mapping) %>%
    dplyr::group_by(period, all_te) %>%
    dplyr::summarise( value = sum(value) , .groups = 'keep' ) %>% 
    dplyr::ungroup(period, all_te) %>% 
    mutate(value = value * sm_TWa_2_MWh/1e6) %>% 
    mutate(all_te = factor(all_te, levels=rev(unique(remind.tech.mapping)))) 
  
  return(vrdata)
}

RM_CONSM <- function(gdx){
  
  # gdx = sorted_files[[7]]
  h2consum <- read.gdx(gdx, VARkey5, factors = FALSE, field = "l", squeeze = FALSE) %>% 
    filter(ttot %in% year_toplot_list) %>%
    filter(all_regi == REGIkey1) %>%
    filter(all_te == "elh2") %>% 
    mutate(value = value * sm_TWa_2_MWh / 1e6) %>% 
    select(period = ttot,value) 
  
  h2consum2 <- h2consum %>% 
    mutate(all_te = "elh2")
  
  totelconsum <- read.gdx(gdx, VARkey4, factors = FALSE, squeeze = FALSE) %>% 
    filter(ttot %in% year_toplot_list) %>%
    filter(all_regi == REGIkey1) %>%
    mutate(value = value * sm_TWa_2_MWh / 1e6) %>% 
    select(period = ttot,totelcon=value)
  
  vrdata = list(h2consum, totelconsum) %>%
    reduce(full_join) %>% 
    mutate(value = totelcon - value)  %>% 
    mutate(all_te = "seel") %>% 
    select(period,all_te,value) %>% 
    full_join(h2consum2) %>% 
    revalue.levels(all_te = dieter.demand.tech.mapping) 
    
  return(vrdata)
}

get_variable_DT <- function(gdx){
  # gdx = sorted_files_DT[[3]]
  
  vrdata <- read.gdx(gdx, VARkey1_DT, factors = FALSE, squeeze = FALSE) %>% 
    select(period = X..1, regi=X..2, all_te=X..3, variable = X..4, value) %>% 
    # filter(period %in% year_toplot_list) %>%
    filter(all_te %in% TECHkeylst_DT) %>% 
    filter(variable %in% VARsubkey1_DT) %>% 
    mutate(value = value/1e6) %>% 
    filter(all_te %in% names(dieter.tech.mapping)) %>% 
    revalue.levels(all_te = dieter.tech.mapping) %>%
    mutate(all_te = factor(all_te, levels=rev(unique(dieter.tech.mapping)))) 
  
    return(vrdata)
}

vrN <- lapply(sorted_paths, get_variable)
vr1_GEN_wCurt <- lapply(sorted_paths, RM_GEN_wCurt)

vrN_COSUM <- lapply(sorted_paths, RM_CONSM)

for(fname in sorted_files){
  # fname = sorted_files[[29]]
  idx <- as.numeric(str_extract(fname, "[0-9]+"))
  # test <- vrN[[1]]
  vrN[[idx]]$iter <- idx
  vr1_GEN_wCurt[[idx]]$iter <- idx
  vrN_COSUM[[idx]]$iter <- idx
}

vrN <- rbindlist(vrN)
vr1_GEN_wCurt<- rbindlist(vr1_GEN_wCurt)
vrN_COSUM<- rbindlist(vrN_COSUM)

vr1_GEN_wCurt$tech <- "total generation w/ curtailment"

vrN_COSUM <- vrN_COSUM %>%
mutate(all_te = fct_relevel(all_te,table_ordered_name_dem))

# vrN <- vrN%>% 
#   filter(all_te == "Hydro") 
 
# vr1_GEN_wCurt <- vr1_GEN_wCurt%>% 
#   filter(all_te == "Hydro") 


if (length(files_DT) != 0) {
    
  vrN_DT <- lapply(sorted_paths_DT, get_variable_DT)
  
  idx_DT <- 1:length(sorted_files_DT)
  for(id in idx_DT){
    idx <- as.numeric(str_extract(sorted_files_DT[[id]], "[0-9]+"))
    vrN_DT[[id]]$iter <- idx
  }
  vrN_DT <- rbindlist(vrN_DT)
  
  vr1_DT_GEN_wCurt <- vrN_DT %>%
    filter(variable == "total generation") %>%
    select(period,iter,all_te,value) 
  
  vr1_DT_CONSUMP <- vrN_DT %>%
    filter(variable == "total consumption") %>%
    select(period,iter,all_te,value) 
  # %>% 
    # filter(iter == 3)
  
  vr1_DT_GEN_wCurt_tot <- vrN_DT %>%
    filter(variable == "total generation") %>%
    select(period, iter, all_te,value) %>%
    dplyr::group_by(period, iter) %>%
    dplyr::summarise( value = sum(value) , .groups = 'keep' ) %>%
    dplyr::ungroup(period, iter)
  
  vr1_DT_GEN_wCurt$tech <- "total generation w/ curtailment"
  
}


for(year_toplot in year_toplot_list){
  
  vr1_GEN_wCurt_yr = vr1_GEN_wCurt %>% filter(period == year_toplot)
  ymax = max(vrN_COSUM$value)*2
  ymin = -ymax
  
  if (length(files_DT) != 0) {
  vr1_DT_GEN_wCurt_yr = vr1_DT_GEN_wCurt %>% filter(period == year_toplot)
  vr1_DT_GEN_wCurt_tot_yr = vr1_DT_GEN_wCurt_tot%>% filter(period == year_toplot) 
  vr1_DT_CONSUMP_yr = vr1_DT_CONSUMP%>% filter(period == year_toplot) 
  
  ymax = max(vr1_DT_GEN_wCurt_tot_yr$value)*1.3
  
  
  if (max(vr1_DT_CONSUMP$value) == 0) {
    ymin = 0
  } else {ymin = -max(vr1_DT_CONSUMP_yr$value)*1.7}
  }
  
  p1<-ggplot() +
  geom_area(data = vrN  %>% filter(period == year_toplot), aes(x = iter, y = value, fill = all_te), size = 1.2, alpha = 0.5, stat = "identity") +
  geom_area(data = vr1_GEN_wCurt_yr, aes(x = iter, y = value, color = all_te), size = 1.2, alpha = 0,linetype="dotted") +
  geom_area(data = vrN_COSUM %>% filter(period == year_toplot), aes(x = iter, y = -value, fill = all_te), size = 1.2, alpha = 0.5, stat = "identity") +
  scale_fill_manual(name = "Technology", values = color.mapping1)+
  scale_color_manual(name = "Technology", values = color.mapping1)+
  theme(axis.text=element_text(size=10), axis.title=element_text(size= 10,face="bold")) +
  xlab("iteration") + ylab(paste0("Usable generation (TWh)")) +
  ggtitle(paste0("REMIND", year_toplot))+
  coord_cartesian(ylim = c(ymin,ymax), xlim = c(0, max(vrN$iter)))+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank()) +
  theme(aspect.ratio = .5)

  if (length(files_DT) != 0) {
p2<-ggplot() +
  geom_area(data = vrN_DT%>% filter(period == year_toplot, variable == "usable generation"), aes(x = iter, y = value, fill = all_te), size = 1.2, alpha = 0.5) +
  geom_area(data = vr1_DT_GEN_wCurt_yr, aes(x = iter, y = value, color = all_te), size = 1.2, alpha = 0,linetype="dotted") +
  geom_area(data = vr1_DT_CONSUMP %>% filter(period == year_toplot), aes(x = iter, y = -value, fill = all_te), size = 1.2, alpha = 0.5, stat = "identity") +
  scale_fill_manual(name = "Technology", values = color.mapping2)+
  scale_color_manual(name = "Technology", values = color.mapping2)+
  theme(axis.text=element_text(size=10), axis.title=element_text(size= 10,face="bold")) +
  xlab("iteration") + ylab(paste0(VARsubkey1_DT, "(TWh)")) +
  coord_cartesian(ylim = c(ymin,ymax), xlim = c(0, max(vrN$iter)))+
  ggtitle(paste0("DIETER", year_toplot))+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank()) +
  theme(aspect.ratio = .5)
  }
  
library(grid)
grid.newpage()
if (length(files_DT) != 0) {
p <- arrangeGrob(rbind(ggplotGrob(p1), ggplotGrob(p2)))
} else {p <- p1}

grid.draw(p)

ggsave(filename = paste0(mypath, run_number, "_GEN_", year_toplot, ".png"),  p,  width = 12, height =16, units = "in", dpi = 120)

}

