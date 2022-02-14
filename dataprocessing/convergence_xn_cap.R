mypath = "~/remind-coupling-dieter/dataprocessing/"
run_number = "hydro798"
mydatapath = paste0("~/remind-coupling-dieter/output/", run_number, "/")
# import library

source(paste0(mypath, "library_import.R"))
library(readr)
require(rmndt)

igdx("/opt/gams/gams30.2_linux_x64_64_sfx")

# remind output iteration gdx files
filenames <- list.files(mydatapath, pattern="fulldata_[0-9]+\\.gdx")
sorted_files <- paste0(mydatapath, "fulldata_", 0:(length(filenames)-1), ".gdx")
files <- paste0("fulldata_", 0:(length(filenames)-1), ".gdx")

maxiter = length(filenames)

# dieter output iteration gdx files
files_DT <- list.files(mydatapath, pattern="results_DIETER_i[0-9]+\\.gdx")

id <- NULL
for(fname in files_DT){
  idx <- as.numeric(str_extract(fname, "[0-9]+"))
  id = c(id, idx)
}
id = sort(id)

if (length(files_DT) != 0) {
  sorted_paths_DT <- paste0(mydatapath, "results_DIETER_i", id, ".gdx")
  sorted_files_DT <- paste0("results_DIETER_i", id, ".gdx")
}

# year_toplot_list <- c(2025,2035,2045,2055)
year_toplot_list <- c(2020,2030,2040,2050,2060,2070,2080)
year_toplot_list <- c(2020,2025,2030,2035,2040,2045,2050,2055,2060,2070,2080,2090,2100,2110,2130)
# year_toplot_list <- c(2050)

iter_toplot_RM0 = 1:(length(filenames)-1)
iter_toplot_RM = 0:(length(filenames)-1)
iter_toplot_DT = 1:length(sorted_files_DT)

remind.nonvre.mapping <- c(
                           # coalchp = "Coal (Lig + HC)",
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

remind.sector.coupling.mapping <- c(elh2 = "Electrolyzers",
                                    NULL)
                                      
remind.vre.mapping <- c(hydro = "Hydro",
                        wind = "Wind Onshore",
                        windoff = "Wind Offshore",
                        spv = "Solar",
                        NULL)

table_ordered_name = c("Coal", "CCGT", "Solar", "Wind Onshore", "Wind Offshore", "Biomass", "OCGT", "Hydro", "Nuclear","Electrolyzers")

remind.tech.mapping <- c(remind.nonvre.mapping, remind.vre.mapping)

if (h2switch == 1){
  remind.tech.mapping <- c(remind.tech.mapping, remind.sector.coupling.mapping ,NULL)
}

dieter.tech.exclude <- c("OCGT_ineff")

dieter.tech.mapping <- c(lig = "Coal",
                         nuc = "Nuclear",
                         OCGT_eff = "OCGT",
                         CCGT = "CCGT",
                         bio = "Biomass",
                         ror = "Hydro",
                         Wind_on = "Wind Onshore",
                         Wind_off = "Wind Offshore",
                         Solar = "Solar",
                         NULL)

if (h2switch == 1){
  dieter.tech.mapping <- c(dieter.tech.mapping, elh2 = "Electrolyzers",NULL)
}

# year_toplot = 2050

VARkey1 = "vm_cap"
REGIkey1 = "DEU"
# REGIkey1 = "USA"
grade = "1"

VARkey1_DT = "p32_report4RM"
VARsubkey1_DT = "capacity"
VARsubkey2_DT = "capfac"

VARsubkey1_RM = "p32_peakDemand_relFac"
VARsubkey2_RM = "v32_usableSeDisp"
VARsubkey3_RM = "p32_seh2elh2Dem"

color.mapping <- c("CCGT" = "#999959", "Coal" = "#0c0c0c",
                   "Solar" = "#ffcc00", "Wind Onshore" = "#337fff", "Wind Offshore" = "#334cff", "Biomass" = "#005900",
                   "OCGT" = "#e51900", "Hydro" = "#191999", "Nuclear" = "#ff33ff", NULL)

color.mapping2 <- c(color.mapping,
                   "peak hourly residual demand" = "#0c0c0c", NULL)


if (h2switch == 1){
  color.mapping <- c(color.mapping,c("Electrolyzers" = "#48D1CC"))
}


########################################################
########################################################
if (length(files_DT) != 0) {
get_DEMvariable_RM <- function(iteration){
  gdx = sorted_files[[iteration+1]]
  
  resfrac <- read.gdx(gdx, VARsubkey1_RM, factor = FALSE) %>% 
    filter(ttot %in% year_toplot_list) %>% 
    filter(all_regi == REGIkey1) %>%
    select(period=ttot,resfrac = value) 
  
  h2dem <- read.gdx(gdx, VARsubkey3_RM, factor = FALSE) %>% 
    filter(ttot %in% year_toplot_list) %>% 
    filter(all_regi == REGIkey1) %>%
    select(period=ttot,h2dem = value) 
  
  vrdata <- read.gdx(gdx, VARsubkey2_RM, field="l", factor = FALSE) %>% 
    filter(ttot %in% year_toplot_list) %>% 
    filter(all_regi == REGIkey1) %>%
    filter(entySe == "seel") %>%
    select(period=ttot,value) %>% 
    right_join(resfrac) %>% 
    right_join(h2dem) %>% 
    mutate(value = (value - h2dem) * resfrac * 8760 * 1e3) 
  
  vrdata$iter <- iteration
  vrdata$model <- "REMIND"
  return(vrdata)
}

vr1_DEM <- lapply(iter_toplot_RM0, get_DEMvariable_RM)
vr1_DEM <- rbindlist(vr1_DEM)
}
get_CAPvariable <- function(iteration){
  gdx = sorted_files[[iteration+1]]
  vrdata <- read.gdx(gdx, VARkey1, factors = FALSE, squeeze = FALSE) %>% 
    filter(tall %in% year_toplot_list) %>%
    filter(all_regi == REGIkey1) %>%
    filter(all_te %in% names(remind.tech.mapping)) %>%
    filter(rlf == grade) %>% 
    mutate(value = value * 1e3) %>% #TW->GW
    select(period = tall, all_te, rlf, value) %>% 
    revalue.levels(all_te = remind.tech.mapping) %>%
    dplyr::group_by(period, all_te, rlf) %>%
    dplyr::summarise( value = sum(value) , .groups = 'keep' ) %>% 
    dplyr::ungroup(period, all_te, rlf) %>% 
    mutate(all_te = factor(all_te, levels=rev(unique(remind.tech.mapping))))

    vrdata$iter <- iteration
    vrdata$model <- "REMIND"
  
    return(vrdata)
}

vrN_RM_CAP <- lapply(iter_toplot_RM, get_CAPvariable)
vrN_RM_CAP <- rbindlist(vrN_RM_CAP)

if (length(files_DT) != 0) {
  get_variable_DT <- function(iteration){
  # iteration = 3
  gdx = sorted_paths_DT[[iteration]]
  vrdata <- read.gdx(gdx, VARkey1_DT, factor = FALSE, squeeze = FALSE) %>%
    select(period = X..1, all_te = X..3,variable=X..4,value)  %>%
    filter(period %in% year_toplot_list) %>%
    filter(all_te %in% names(dieter.tech.mapping)) %>%
    filter(variable %in% c(VARsubkey1_DT,VARsubkey2_DT)) %>%
    revalue.levels(all_te = dieter.tech.mapping) %>%
    mutate(all_te = factor(all_te, levels=rev(unique(dieter.tech.mapping))))

    vrdata$iter <- id[[iteration]]
    vrdata$model <- "DIETER"
    return(vrdata)
}

vrN_DT <- lapply(iter_toplot_DT, get_variable_DT)
vrN_DT <- rbindlist(vrN_DT)

vrN_DT_CF <- vrN_DT %>%
  filter(variable == "capfac") %>%
  mutate(value = value * 100) %>%
  select(period, tech=all_te, value, iter)

vrN_DT_CAP<- vrN_DT %>%
  filter(variable == "capacity") %>%
  mutate(value = value/1e3) %>% #MW->GW
  select(period, all_te, value, iter)
}
CFkey1 = "vm_capFac"
CFkey2 = "pm_dataren"
CFkey3 = "vm_capDistr"

get_CAPFAC_variable <- function(iteration){
  # iteration = 3
  
  # do not comment:
  gdx = sorted_files[[iteration+1]]
  
  # first the dispatchable
  vrdata <- read.gdx(gdx, CFkey1, field="l", squeeze = FALSE) %>% 
    filter(all_te %in% names(remind.tech.mapping)) %>% 
    filter(all_regi == REGIkey1) %>%
    revalue.levels(all_te = remind.tech.mapping) %>%
    filter(!(all_te %in% remind.vre.mapping)) %>% 
    select(period = ttot, all_te, value) %>% 
    dplyr::group_by(period, all_te) %>%
    dplyr::summarise(value = mean(value), .groups = "keep") %>% 
    dplyr::ungroup(period, all_te)
  
  # second the VRE
  # pm_dataren("nur") = capacity factor at different grade levels (quality of wind resources), pm_dataren is used to represent different cost levels necessary to create the same amount of energy if you have to build your wind farm in places with lower wind. The grades are ordered from 1 to 10. 1 is closer to the wind always blowing (highest nur capacity factor), 10 is the worst place to install wind.
  dataren <- read.gdx(gdx, CFkey2) %>% 
    filter(char == "nur") %>%
    filter(all_regi == REGIkey1) %>%
    filter(all_te %in% names(remind.vre.mapping)) %>% 
    revalue.levels(all_te = remind.vre.mapping) %>%
    select(all_te, rlf, ren_nur= value)
  
  #vm_capDistr is a "helper" variable that stores the amount of capacity that REMIND assign to each grade. Therefore it can have values from any grade. vm_cap is the total capacity (sum of vm_capDistr). It does not have grade infor anymore, everything is assigned to "1". vm_capDistr will retain the information of the optimal grades used in the REMIND decision
  
  percentage_cap_distr <- read.gdx(gdx, CFkey3) %>% 
    filter(tall %in% year_toplot_list) %>%
    filter(all_regi == REGIkey1) %>% 
    filter(all_te %in% names(remind.vre.mapping)) %>% 
    revalue.levels(all_te = remind.vre.mapping) %>% 
    select(period=tall, all_te, rlf, cap_distr = value) %>% 
    dplyr::group_by(period, all_te) %>% 
    transmute(rlf, percentage_cap_distr = cap_distr/sum(cap_distr))
  
  vrdata2_0 = list(dataren, percentage_cap_distr) %>%
    reduce(right_join)
  
  vrdata2 <- vrdata2_0 %>% 
    select(period, all_te, ren_nur, percentage_cap_distr) %>% 
    replace(is.na(.), 0) %>%
    dplyr::group_by(period, all_te) %>%
    dplyr::summarise(value = sum(ren_nur * percentage_cap_distr), .groups = "keep" ) %>% 
    dplyr::ungroup(period, all_te)
  
  vrdata_tot <- list(vrdata, vrdata2) %>% 
    reduce(full_join) %>% 
    dplyr::rename(tech = all_te) %>% 
    mutate(value = value * 100)
  
  vrdata_tot$iter <- iteration
  
  return(vrdata_tot)
}

vrN_RM_CF <- lapply(iter_toplot_RM, get_CAPFAC_variable)
vrN_RM_CF <- rbindlist(vrN_RM_CF)

for(year_toplot in year_toplot_list){
  vrN_RM_CAP_plot <- vrN_RM_CAP %>% 
    filter(period == year_toplot)
  
  if (length(files_DT) != 0) {
  vr1_DEM_plot <- vr1_DEM %>% 
    filter(period == year_toplot)
  vr1_DEM_plot$tech <- "peak hourly residual demand"
  }
  
  vrN_RM_CF_plot <- vrN_RM_CF %>% 
    filter(period == year_toplot)
  
  secAxisScale1 = max(vrN_RM_CAP_plot$value) / 60

  vrN_RM_CAP_plot2<- vrN_RM_CAP_plot %>% 
    dplyr::group_by(period, rlf, iter, model) %>%
    dplyr::summarise( value = sum(value) , .groups = 'keep' ) %>% 
    dplyr::ungroup(period, rlf, iter, model) 
  
  ymax = max(vrN_RM_CAP_plot2$value) * 1.1
  
p0<-ggplot() +
  geom_area(data = vrN_RM_CAP_plot, aes(x = iter, y = value, fill = all_te), size = 1.2, alpha = 0.5) +
  geom_line(data = vrN_RM_CF_plot, aes(x = iter, y = value*secAxisScale1, color = tech), size = 1.2, alpha = 0.5) + 
  scale_y_continuous(sec.axis = sec_axis(~./secAxisScale1, name = paste0("capacity factor (%)")))+
  scale_fill_manual(name = "Technology", values = color.mapping) +
  scale_color_manual(name = "tech", values = color.mapping2) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size = 20,face="bold")) + 
  xlab("iteration") + ylab(paste0("capacity (GW)")) +
  # ggtitle(paste0("REMIND ", year_toplot, " (100$/tCO2)"))+
  ggtitle(paste0("REMIND: Germany ", year_toplot))+
  coord_cartesian(xlim = c(0, max(vrN_RM_CAP_plot$iter)+1),ylim = c(0, ymax)) +
  theme(plot.title = element_text(size = 16, face = "bold")) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text=element_text(size=14)) +
  theme(aspect.ratio = .5)

if (length(files_DT) != 0) {
  p1 <- p0 + 
    geom_line(data = vr1_DEM_plot, aes(x = iter, y = value, color = tech), size = 1.2, alpha = 0.5,linetype="dotted")
}

vrN_DT_CAP_plot <- vrN_DT_CAP %>%
  filter(period == year_toplot)

if (length(files_DT) != 0) {
vrN_DT_CF_plot <- vrN_DT_CF %>%
  filter(period == year_toplot)
}
secAxisScale2 = max(vrN_RM_CAP_plot$value) / 60

if (length(files_DT) != 0) {
p2<-ggplot() +
  geom_area(data = vrN_DT_CAP_plot, aes(x = iter, y = value, fill = all_te), size = 1.2, alpha = 0.5) +
  geom_line(data = vr1_DEM_plot, aes(x = iter, y = value, color = tech), size = 1.2, alpha = 1,linetype="dotted") +
  geom_line(data = vrN_DT_CF_plot, aes(x = iter, y = value*secAxisScale2, color = tech), size = 1.2, alpha = 0.5) +
  scale_y_continuous(sec.axis = sec_axis(~./secAxisScale2, name = paste0("capacity factor (%)")))+
  scale_fill_manual(name = "Technology", values = color.mapping) +
  scale_color_manual(name = "tech", values = color.mapping2)+
  theme(axis.text=element_text(size=14), axis.title=element_text(size= 20,face="bold")) +
  xlab("iteration") + ylab(paste0("capacity (GW)")) +
  coord_cartesian(xlim = c(0, max(vrN_DT_CAP_plot$iter)),ylim = c(0, ymax))+
  # ggtitle(paste0("DIETER ", year_toplot, " (100$/tCO2)"))+
  ggtitle(paste0("DIETER: Germany ", year_toplot))+
  theme(plot.title = element_text(size = 16, face = "bold"))+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text=element_text(size=14)) +
  theme(aspect.ratio = .5)
}

library(grid)
grid.newpage()
if (length(files_DT) != 0) {
p <- arrangeGrob(rbind(ggplotGrob(p1), ggplotGrob(p2)))
} else { p<-p1 }

grid.draw(p)

ggsave(filename = paste0(mypath, run_number, "_CAP_", year_toplot, "wCF.png"),  p,  width = 12, height =15, units = "in", dpi = 120)
}

