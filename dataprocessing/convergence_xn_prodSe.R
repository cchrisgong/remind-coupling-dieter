mypath = "~/remind-coupling-dieter/dataprocessing/"
run_number = "hydro798"
mydatapath = paste0("~/remind-coupling-dieter/output/", run_number, "/")
# import library
source(paste0(mypath, "library_import.R"))
library(readr)
require(rmndt)
library(forcats)

CHP_coup = FALSE

# remind output iteration gdx files
filenames <- list.files(mydatapath, pattern="fulldata_[0-9]+\\.gdx")
sorted_paths <- paste0(mydatapath, "fulldata_", 0:(length(filenames)-1), ".gdx")
sorted_paths1 <- paste0(mydatapath, "fulldata_", 1:(length(filenames)-1), ".gdx")
sorted_files <- paste0("fulldata_", 0:(length(filenames)-1), ".gdx")
sorted_files1 <- paste0("fulldata_", 1:(length(filenames)-1), ".gdx")
maxiter = length(filenames)-1
# maxiter = 3

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

# year_toplot_list <- c(2020,2030,2040,2050,2060,2070,2080)
year_toplot_list <- c(2020,2025,2030,2035,2040,2045,2050,2055,2060,2070,2080,2090,2100,2110,2130,2150)
# year_toplot_list <- c(2025,2035)
  
# year_toplot = 2035
# iter_toplot = 30
sm_TWa_2_MWh = 8760000000

print(paste0("Year: ", year_toplot_list))
# print(paste0("Iteration: ", iter_toplot))

VARkey1 = "vm_prodSe"
VARkey2 = "vm_usableSeTe"

VARkey4 = "p32_usableSeDisp" # normal electricity demand

VARkey5 = "vm_demSe"


REGIkey1 = "DEU"
# REGIkey1 = "USA"
ProdKEY1 = "seel"
ProdKEY2 = "seh2"

remind.nonvre.mapping <- c(
                           igcc = "Coal",
                           igccc = "Coal",
                           pcc = "Coal",
                           pco = "Coal",
                           pc = "Coal",
                           tnrs = "Nuclear",
                           ngt = "OCGT",
                           ngcc = "CCGT",
                           ngccc = "CCGT",
                           bioigcc = "Biomass",
                           bioigccc = "Biomass",
                           NULL)
if (CHP_coup == TRUE){
  remind.nonvre.mapping = c(remind.nonvre.mapping, c(
  gaschp = "CCGT",
  biochp = "Biomass",
  coalchp = "Coal")
  )
}
# shifting hydro to dispatchable because in REMIND usable energy is only defined for spv, wind, csp
remind.nonvre.mapping2 <- c(remind.nonvre.mapping, hydro = "Hydro")

remind.vre.mapping <- c(wind = "Wind Onshore",
                        windoff = "Wind Offshore",
                        spv = "Solar",
                        NULL)

table_ordered_name = c("Solar", "Wind Onshore", "Wind Offshore", "Biomass", "Hydro", "Nuclear","CCGT", "OCGT", "Coal")
table_ordered_name_dem = c("Electricity used for Electrolysis","Electricity")

remind.tech.mapping <- c(remind.nonvre.mapping2, remind.vre.mapping)

dieter.tech.mapping <- c(lig = "Coal",
                               nuc = "Nuclear",
                               OCGT_eff = "OCGT",
                               CCGT = "CCGT",
                               bio = "Biomass",
                               ror = "Hydro",
                               Wind_on = "Wind Onshore",
                               Wind_off = "Wind Offshore",
                               Solar = "Solar",
                               el = "Electricity",
                               seel = "Electricity",
                               NULL)

dieter.demand.tech.mapping <- c(
                                elh2 = "Electricity used for Electrolysis",
                                NULL)

## load H2 switch
h2switch0 <- read.gdx(sorted_paths[[2]], "s32_H2switch", factors = FALSE, squeeze = FALSE)
h2switch = as.numeric(h2switch0)
# h2switch = 1

if (h2switch == 1){
  dieter.tech.mapping <- c(dieter.tech.mapping, dieter.demand.tech.mapping)
}


VARkey1_DT = "p32_report4RM"
VARsubkey1_DT = c("total_generation", "usable_generation", "total_consumption")
TECHkeylst_DT = c("CCGT", "lig", "Solar", "Wind_on","Wind_off", "bio", "OCGT_eff", "ror", "nuc","elh2","el")

color.mapping_vre <- c("Solar" = "#ffcc00", "Wind Onshore" = "#337fff", "Wind Offshore" = "#334cff")

color.mapping <- c("CCGT" = "#999959", "Coal" = "#0c0c0c",
                   "Solar" = "#ffcc00", "Wind Onshore" = "#337fff", "Wind Offshore" = "#334cff", "Biomass" = "#005900",
                   "OCGT" = "#e51900", "Hydro" = "#191999", "Nuclear" = "#ff33ff", "Electricity" = "#6495ED")

if (h2switch == 1){
color.mapping <- c(color.mapping, c("Electricity used for Electrolysis" = "#48D1CC"))
}


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
    mutate(all_te = "Electricity") %>% 
    select(period,all_te,value) %>% 
    full_join(h2consum2) %>% 
    revalue.levels(all_te = dieter.demand.tech.mapping) 
    
  return(vrdata)
}

get_variable_DT <- function(gdx){
  # gdx = sorted_paths_DT[[3]]
  
  vrdata <- read.gdx(gdx, VARkey1_DT, factors = FALSE, squeeze = FALSE) %>% 
    select(period = X..1, regi=X..2, all_te=X..3, variable = X..4, value) %>% 
    # filter(period %in% year_toplot_list) %>%
    filter(all_te %in% TECHkeylst_DT) %>% 
    filter(variable %in% VARsubkey1_DT) %>% 
    mutate(value = value / 1e6) %>% 
    filter(all_te %in% names(dieter.tech.mapping)) %>% 
    revalue.levels(all_te = dieter.tech.mapping) %>%
    mutate(all_te = factor(all_te, levels=rev(unique(dieter.tech.mapping)))) 
  
    return(vrdata)
}

GEN <- lapply(sorted_paths, get_variable)
GEN_wCurt <- lapply(sorted_paths, RM_GEN_wCurt)

COSUM <- lapply(sorted_paths1, RM_CONSM)

for(fname in sorted_files){
  # fname = sorted_files[[1]]
  idx <- as.numeric(str_extract(fname, "[0-9]+"))+1
  GEN[[idx]]$iter <- idx -1
  GEN_wCurt[[idx]]$iter <- idx -1
}

for(fname in sorted_files1){
  # fname = sorted_files[[1]]
  idx <- as.numeric(str_extract(fname, "[0-9]+"))
  COSUM[[idx]]$iter <- idx
}

GEN <- rbindlist(GEN)
GEN_wCurt<- rbindlist(GEN_wCurt)

COSUM<- rbindlist(COSUM)

GEN_wCurt_disp <- GEN_wCurt %>% 
  filter(!all_te %in% c("Solar", "Wind Onshore", "Wind Offshore")) %>% 
  dplyr::group_by(period, iter) %>%
  dplyr::summarise( disp = sum(value) , .groups = 'keep' ) %>%
  dplyr::ungroup(period, iter)

GEN_wCurt_dispwWind <- GEN_wCurt %>% 
  filter(!all_te %in% c("Solar", "Wind Offshore")) %>% 
  dplyr::group_by(period, iter) %>%
  dplyr::summarise( disp = sum(value) , .groups = 'keep' ) %>%
  dplyr::ungroup(period, iter)

GEN_wCurt_wind <- GEN_wCurt %>% 
  filter(all_te %in% c("Wind Onshore")) %>% 
  left_join(GEN_wCurt_disp) %>% 
  mutate(value = value +disp) %>% 
  select(iter,period,all_te,value)

GEN_wCurt_vre <- GEN_wCurt %>% 
  filter(all_te %in% c("Wind Offshore", "Solar")) %>% 
  select(iter,period,all_te,value)%>% 
  full_join(GEN_wCurt_wind) 

GEN_wCurt_vre$tech <- "total generation w/ curtailment"   

if (h2switch == 1){
COSUM <- COSUM %>%
mutate(all_te = fct_relevel(all_te,table_ordered_name_dem))
}
# vrN <- vrN%>% 
#   filter(all_te == "Hydro") 
 
# GEN_wCurt <- GEN_wCurt%>% 
#   filter(all_te == "Hydro")

if (length(files_DT) != 0) {
  GEN_DT <- lapply(sorted_paths_DT, get_variable_DT)
  
  idx_DT <- 1:length(sorted_files_DT)
  for(id in idx_DT){
    idx <- as.numeric(str_extract(sorted_files_DT[[id]], "[0-9]+"))
    GEN_DT[[id]]$iter <- idx
  }
  GEN_DT <- rbindlist(GEN_DT)
  
  DT_GEN_wCurt <- GEN_DT %>%
    filter(variable == "total_generation") %>%
    select(period,iter,all_te,value) 
  
  DT_CONSUMP <- GEN_DT %>%
    filter(variable == "total_consumption") %>%
    select(period,iter,all_te,value) 
  # %>% 
    # filter(iter == 3)
  
  DT_GEN_wCurt_tot <- GEN_DT %>%
    filter(variable == "total_generation") %>%
    select(period, iter, all_te,value) %>%
    dplyr::group_by(period, iter) %>%
    dplyr::summarise( value = sum(value) , .groups = 'keep' ) %>%
    dplyr::ungroup(period, iter)
  
  DT_GEN_wCurt_disp <- DT_GEN_wCurt %>% 
    filter(!all_te %in% c("Solar", "Wind Onshore", "Wind Offshore")) %>% 
    dplyr::group_by(period, iter) %>%
    dplyr::summarise( disp = sum(value) , .groups = 'keep' ) %>%
    dplyr::ungroup(period, iter)

  DT_GEN_wCurt_dispwWind <- DT_GEN_wCurt %>% 
    filter(!all_te %in% c("Wind Onshore")) %>% 
    dplyr::group_by(period, iter) %>%
    dplyr::summarise( disp = sum(value) , .groups = 'keep' ) %>%
    dplyr::ungroup(period, iter)
  
  DT_GEN_wCurt_wind <- DT_GEN_wCurt %>% 
    filter(all_te %in% c("Wind Onshore")) %>% 
    left_join(DT_GEN_wCurt_disp) %>% 
    mutate(value = value +disp) %>% 
    select(iter,period,all_te,value)
  
  DT_GEN_wCurt_vre <- DT_GEN_wCurt %>% 
    filter(all_te %in% c("Wind Offshore","Solar")) %>% 
    select(iter,period,all_te,value)%>% 
    full_join(DT_GEN_wCurt_wind) 

  DT_GEN_wCurt_vre$tech <- "total generation w/ curtailment"    
}


for(year_toplot in year_toplot_list){
  # year_toplot = 2040
  GEN_wCurt_yr = GEN_wCurt_vre %>% filter(period == year_toplot)
  ymax = max(COSUM$value)*1.5
  ymin = -ymax
  
  if (length(files_DT) != 0) {
  DT_GEN_wCurt_yr <- DT_GEN_wCurt_vre %>% filter(period == year_toplot)
  DT_CONSUMP_yr <- DT_CONSUMP%>% filter(period == year_toplot) 
  }

  p1<-ggplot() +
  geom_area(data = GEN %>% filter(period == year_toplot), aes(x = iter, y = value, fill = all_te), size = 1.2, alpha = 0.5, stat = "identity") +
  geom_area(data = GEN_wCurt_yr, aes(x = iter, y = value, color = all_te), size = 0.8, alpha = 0,linetype="dotted") +
  geom_area(data = COSUM %>% filter(period == year_toplot), aes(x = iter, y = -value, fill = all_te), size = 1.2, alpha = 0.5, stat = "identity") +
  scale_fill_manual(name = "Technology", values = color.mapping)+
  scale_color_manual(name = "Technology", values = color.mapping_vre)+
  theme(axis.text=element_text(size=10), axis.title=element_text(size= 10,face="bold")) +
  xlab("iteration") + ylab(paste0("Usable generation (TWh)")) +
  ggtitle(paste0("REMIND: Germany ", year_toplot))+
  # coord_cartesian(ylim = c(ymin,ymax), xlim = c(0, 20))+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank()) +
  theme(aspect.ratio = .5)

  if (length(files_DT) != 0) {
p2<-ggplot() +
  geom_area(data = GEN_DT%>% filter(period == year_toplot, variable == "usable_generation"), aes(x = iter, y = value, fill = all_te), size = 1.2, alpha = 0.5) +
  geom_area(data = DT_GEN_wCurt_yr, aes(x = iter, y = value, color = all_te), size = 0.8, alpha = 0,linetype="dotted") +
  geom_area(data = DT_CONSUMP %>% filter(period == year_toplot), aes(x = iter, y = -value, fill = all_te), size = 1.2, alpha = 0.5, stat = "identity") +
  scale_fill_manual(name = "Technology", values = color.mapping)+
  scale_color_manual(name = "Technology", values = color.mapping_vre)+
  theme(axis.text=element_text(size=10), axis.title=element_text(size= 10,face="bold")) +
  xlab("iteration") + ylab(paste0("Usable generation (TWh)")) +
  # coord_cartesian(ylim = c(ymin,ymax), xlim = c(0, 20))+
  ggtitle(paste0("DIETER: Germany ", year_toplot))+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank()) +
  theme(aspect.ratio = .5)
  }
  
library(grid)
grid.newpage()
if (length(files_DT) != 0) {
p <- arrangeGrob(rbind(ggplotGrob(p1), ggplotGrob(p2)))
} else {p <- p1}

grid.draw(p)

ggsave(filename = paste0(mypath, run_number, "_GEN_", year_toplot, ".png"),  p,  width = 8, height =10, units = "in", dpi = 120)

}

# timeseries plot
for (i in c(maxiter)){  
   GEN_i = GEN %>% 
    filter(iter == i) %>% 
    mutate(model = "REMIND ") %>% 
    mutate(period = as.numeric(period))
  
  if (length(files_DT) != 0) {
    DT_GEN_i <- GEN_DT %>% 
      filter(iter == i) %>% 
      filter(variable == "usable_generation") %>% 
      mutate(model = "DIETER") %>% 
      mutate(period = as.numeric(period))
    
    GEN_i <- list(GEN_i, DT_GEN_i) %>% 
      reduce(full_join)
  }
   GEN_i <- GEN_i %>% 
      mutate(model = fct_relevel(model,c("REMIND","DIETER")))
  
  p3<-ggplot() +
    geom_area(data = GEN_i %>% filter(period <2110) , aes(x = period, y = value, fill = all_te),  alpha = 0.5, stat = "identity") +
    scale_fill_manual(name = "Technology", values = color.mapping)+
    theme(axis.text=element_text(size=10), axis.title=element_text(size= 10,face="bold")) +
    xlab("period") + ylab(paste0("Usable generation (TWh)")) +
    ggtitle(paste0("Generation Mix"))+
    theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank()) +
    theme(aspect.ratio = .5) +
    facet_wrap(~model, nrow = 2)
  
  ggsave(filename = paste0(mypath, run_number, "_GEN_timeseries_i", i, ".png"),  p3,  width = 6, height =8, units = "in", dpi = 120)
  
}


for (i in c(5,10,20,maxiter)){
  # i = 5

  GEN_i <- GEN %>%
    filter(iter == i) %>%
    mutate(period = as.numeric(period)) %>% 
    dplyr::rename(remind_gen = value)


  if (length(files_DT) != 0) {
    DT_GEN_i <- GEN_DT %>%
      filter(iter == i) %>%
      filter(variable == "usable_generation") %>%
      mutate(period = as.numeric(period)) %>%
      dplyr::rename(dieter_gen = value)

    GEN_i <- list(GEN_i, DT_GEN_i) %>%
      reduce(full_join) %>%
      mutate(delta_gen = remind_gen - dieter_gen)

  }

  p4 <-ggplot() +
    geom_bar(data = GEN_i %>% filter(period <2110) , aes(x = period, y = delta_gen, fill = all_te, label = delta_gen),  alpha = 0.5, stat = "identity") +
    geom_label(size = 3, position = position_stack(vjust = 0.5)) +
    scale_fill_manual(name = "Technology", values = color.mapping)+
    theme(axis.text=element_text(size=10), axis.title=element_text(size= 10,face="bold")) +
    xlab("period") + ylab(paste0("Usable generation (TWh)")) +
    ggtitle(paste0("Generation difference REMIND - DIETER"))+
    theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank()) +
    theme(aspect.ratio = .5) 
  
  ggsave(filename = paste0(mypath, run_number, "_delGEN_timeseries_i", i, ".png"),  p4,  width = 6, height =8, units = "in", dpi = 120)
  

}
