# comparison of cap between coupled and uncoupled runs
mypath = "~/remind-coupling-dieter/dataprocessing/"
run_number_uncpl = "hydroU15"
run_number = "hydro257"
mydatapath = paste0("~/remind-coupling-dieter/output/", run_number, "/")
mydatapath_uncpl = paste0("~/remind-coupling-dieter/output/", run_number_uncpl, "/")
# import library

source(paste0(mypath, "library_import.R"))
library(readr)
require(rmndt)

igdx("/opt/gams/gams30.2_linux_x64_64_sfx")

# remind output iteration gdx files
filenames <- list.files(mydatapath, pattern="fulldata_[0-9]+\\.gdx")
sorted_files <- paste0(mydatapath, "fulldata_", 1:length(filenames), ".gdx")
files <- paste0("fulldata_", 1:length(filenames), ".gdx")
# dieter output iteration gdx files
files_DT <- list.files(mydatapath, pattern="results_DIETER_i[0-9]+\\.gdx")
sorted_files_DT <- paste0(mydatapath, "results_DIETER_i", seq(from = 1, to = length(files_DT), by = 1), ".gdx")

# remind output iteration gdx files
filenames2 <- list.files(mydatapath_uncpl, pattern="fulldata_[0-9]+\\.gdx")
sorted_files2 <- paste0(mydatapath_uncpl, "fulldata_", 1:length(filenames2), ".gdx")
files2 <- paste0("fulldata_", 1:length(filenames2), ".gdx")

# year_toplot_list <- c(2025,2030,2035,2040,2045,2050,2055,2060,2070,2080,2090,2100,2110,2130)
year_toplot_list <- c(2030,2050,2070,2080,2100)

iter_toplot = length(sorted_files_DT)

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

remind.sector.coupling.mapping <- c(elh2 = "Electrolyzers",
                                    NULL)

remind.vre.mapping <- c(hydro = "Hydro",
                        wind = "Wind",
                        spv = "Solar",
                        NULL)

vre.names <- c("Hydro","Wind","Solar")
nonvre.names <- c("Lignite", "Hard coal","Coal (Lig + HC)", "Nuclear","OCGT","CCGT","Biomass","Electrolyzers")
table_ordered_name = c("Coal (Lig + HC)", "Lignite", "Hard coal","CCGT", "Solar", "Wind", "Biomass", "OCGT", "Hydro", "Nuclear","Electrolyzers")

remind.tech.mapping <- c(remind.nonvre.mapping, remind.vre.mapping, remind.sector.coupling.mapping)

dieter.tech.exclude <- c("OCGT_ineff", "Wind_off")

dieter.tech.mapping <- c(hc = "Hard coal",
                         lig = "Lignite",
                         coal = "Coal (Lig + HC)",
                         nuc = "Nuclear",
                         OCGT_eff = "OCGT",
                         CCGT = "CCGT",
                         bio = "Biomass",
                         ror = "Hydro",
                         Wind_on = "Wind",
                         Solar = "Solar",
                         elh2 = "Electrolyzers",
                         NULL)

# year_toplot = 2050

VARkey1 = "vm_cap"
REGIkey1 = "DEU"
# REGIkey1 = "USA"
grade = "1"

VARkey1_DT = "p32_report4RM"
VARsubkey1_DT = "capacity"
VARsubkey2_DT = "capfac"

VARsubkey1_RM = "p32_peakDemand_relFac"
VARsubkey2_RM = "p32_seelUsableDem"

color.mapping <- c("CCGT" = "#999959", "Lignite" = "#0c0c0c", "Coal (Lig + HC)" = "#0c0c0c",
                   "Solar" = "#ffcc00", "Wind" = "#337fff", "Biomass" = "#005900",
                   "OCGT" = "#e51900", "Hydro" = "#191999", "Nuclear" = "#ff33ff",
                   "Hard coal" = "#808080", "Electrolyzers" = "#48D1CC")

color.mapping.model <- c("coupled REMIND" = "#0000cc", "coupled DIETER" = "#99cc99", "uncoupled REMIND" = "#7F7FFF")

########################################################
########################################################
get_variable <- function(gdxfiles, iteration){
  gdx = gdxfiles[[iteration]]
  vrdata <- read.gdx(gdx, VARkey1, factors = FALSE, squeeze = FALSE) %>% 
    filter(tall %in% year_toplot_list) %>%
    filter(all_regi == REGIkey1) %>%
    filter(all_te %in% names(remind.tech.mapping)) %>%
    filter(rlf == grade) %>% 
    mutate(value = value*1e3) %>% #TW->GW
    select(period = tall, all_te, rlf, value) %>% 
    revalue.levels(all_te = remind.tech.mapping) %>%
    dplyr::group_by(period, all_te, rlf) %>%
    dplyr::summarise( value = sum(value) , .groups = 'keep' ) %>% 
    dplyr::ungroup(period, all_te, rlf) %>% 
    mutate(all_te = factor(all_te, levels=rev(unique(remind.tech.mapping)))) %>% 
    select(period, tech= all_te,value) %>% 
    mutate(period = as.numeric(period)) %>% 
    mutate(value = as.numeric(value))
    
  vrdata$iter <- iteration
  
  return(vrdata)
}

vrN_RM_CAP <- lapply(iter_toplot, get_variable, gdxfiles=sorted_files)
vrN_RM_CAP <- rbindlist(vrN_RM_CAP)

vrN_RM_CAP2 <- lapply(iter_toplot, get_variable, gdxfiles=sorted_files2)
vrN_RM_CAP2 <- rbindlist(vrN_RM_CAP2)

vrN_RM_CAP$run <- "coupled REMIND"
vrN_RM_CAP2$run <- "uncoupled REMIND"
  
get_variable_DT <- function(iteration){
  # iteration = 3
  gdx = sorted_files_DT[[iteration]]
  vrdata <- read.gdx(gdx, VARkey1_DT, factor = FALSE, squeeze = FALSE) %>%
    select(period = X..1, tech = X..3,variable=X..4,value)  %>%
    filter(period %in% year_toplot_list) %>%
    filter(tech %in% names(dieter.tech.mapping)) %>%
    filter(variable %in% c(VARsubkey1_DT,VARsubkey2_DT)) %>%
    revalue.levels(tech = dieter.tech.mapping) %>%
    mutate(tech = factor(tech, levels=rev(unique(dieter.tech.mapping)))) %>% 
    mutate(period = as.numeric(period)) %>% 
    mutate(value = as.numeric(value))
  
  vrdata$iter <- iteration
  return(vrdata)
}
  
  vrN_DT <- lapply(iter_toplot, get_variable_DT)
  vrN_DT <- rbindlist(vrN_DT)
  
  vrN_DT_CAP <- vrN_DT %>%
    filter(variable == "capacity") %>%
    mutate(value = value/1e3) %>% #MW->GW
    mutate(value = ifelse(tech=="Electrolyzers", value * 0.75, value)) %>% #GW_el to GW_H2, 0.75 is the REMIND default efficiency of elh2 (pm_eta_conv) 
    select(period, tech, value, iter)%>% 
    mutate(value = as.numeric(value))

  vrN_DT_CAP_coal <- vrN_DT_CAP %>% 
    filter(tech == c("Hard coal","Lignite")) %>%
    dplyr::group_by(period, iter) %>%
    dplyr::summarise(value = sum(value), .groups = "keep") %>% 
    dplyr::ungroup(period,iter) %>% 
    mutate(tech="Coal (Lig + HC)")
    
  vrN_DT_CAP <- list(vrN_DT_CAP,vrN_DT_CAP_coal) %>% 
    reduce(full_join)
  # mutate_rows(vrN_DT_CAP,tech=="Electrolyzers",value=value *0.75)
  
  vrN_DT_CAP$run <- "coupled DIETER"
  
  vr_Plot <- list(vrN_RM_CAP, vrN_RM_CAP2, vrN_DT_CAP) %>% 
    reduce(full_join)
  
  secAxisScale1 = max(vr_Plot$value) / 100
  
  for(te in unique(remind.tech.mapping)){
    # te="Electrolyzers"
  p1<-ggplot() +
    geom_col(data = vr_Plot%>% filter(tech == te), aes(x = run, y = value,fill=run), size = 1.2, alpha = 0.9) +
    scale_fill_manual(name = "run/model", values = color.mapping.model) +
    # scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
    theme(axis.text=element_text(size=14), axis.title=element_text(size = 14,face="bold")) + 
    xlab("") + ylab(paste0(VARkey1, "(GW)")) +
    ggtitle(paste0("Capacity ", te, " (iter = 30) comparison: uncoupled vs. coupled runs"))+
    theme(plot.title = element_text(size = 16, face = "bold")) +
    # theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text=element_text(size=14)) +
    theme(aspect.ratio = 3,axis.text.x=element_blank())+
    facet_wrap(~period, nrow = 1) 
  
  ggsave(filename = paste0(mypath, run_number, "_CAP_coup_VS_uncoup_", te,".png"),  p1,  width = 12, height =5, units = "in", dpi = 120)
}
