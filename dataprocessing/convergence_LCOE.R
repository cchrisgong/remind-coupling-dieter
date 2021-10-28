mypath = "~/remind-coupling-dieter/dataprocessing/"
run_number = "hydro257"
mydatapath = paste0("~/remind-coupling-dieter/output/", run_number, "/")
# import library
source(paste0(mypath, "library_import.R"))
library(readr)
require(rmndt)

myDIETERPLOT_path = "~/remind-coupling-dieter/dataprocessing/DIETER_plots/"
source(paste0(myDIETERPLOT_path, "GDXtoQuitte.R"))
library(readr)
require(ggplot2)
require(lusweave)

#remind output iteration gdx files
files <- list.files(mydatapath, pattern="fulldata_[0-9]+\\.gdx")
sorted_files0 <- paste0(mydatapath, "fulldata_", 1:length(files), ".gdx")
filenames0 <- paste0("fulldata_", 1:length(files), ".gdx")

maxiter = length(files)

iter_toplot= 28

sorted_files <- sorted_files0[1:maxiter]

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


color.mapping <- c("CCGT" = "#999959", "Lignite" = "#0c0c0c", "Coal (Lig + HC)" = "#0c0c0c",
                   "Solar" = "#ffcc00", "Wind" = "#337fff", "Biomass" = "#005900",
                   "OCGT" = "#e51900", "Hydro" = "#191999", "Nuclear" = "#ff33ff",
                   "Hard coal" = "#808080", "Electrolyzers" = "#48D1CC")


files_DT_rep <- list.files(mydatapath, pattern = "report_DIETER_i[0-9]+\\.gdx")
sorted_files_DT <- paste0(mydatapath, "report_DIETER_i", seq(from = 1, to = length(files_DT_rep), by = 1), ".gdx")

maxiter = length(files_DT_rep)

REGIkey = "DEU"
ProdKEY = "seh2"
PARkey3 = "p32_marketValue"
PARkey8 = "p32_marketPrice"
TECHkeylst_peakGas = c("ngt")
TECHkeylst_nonPeakGas = c("ngccc","ngcc",  "gaschp") 
TECHkeylst_coal = c("coalchp", "igccc", "igcc", "pcc", "pco","pc")
TECHkeylst_solar = c("spv")
TECHkeylst_wind = c("wind")
TECHkeylst_hydro = c("hydro")
TECHkeylst_nuclear = c("tnrs")
TECHkeylst_biomass = c("biochp", "bioigccc", "bioigcc")
TECHkeylst_sectorCoup = c("elh2","tdels")

FLEX_tech = c(TECHkeylst_solar, TECHkeylst_nonPeakGas,TECHkeylst_peakGas,TECHkeylst_coal,TECHkeylst_wind,TECHkeylst_hydro,TECHkeylst_nuclear,TECHkeylst_biomass)
FLEX_tech2 = c(TECHkeylst_sectorCoup)
# for(fname in files_DT_rep){
# gdxToQuitte_annual(mydatapath, fname, run_number)
# }

sorted_annual_report_DT <- paste0(myDIETERPLOT_path, run_number, "_i", seq(from =1, to = length(files_DT_rep), by = 1), "_annualreport.csv")

VAR_report_key_DT =  c("annualized investment cost ($/MWh)", "O&M cost ($/MWh)", "fuel cost - divided by eta ($/MWh)","CO2 cost ($/MWh)")
# VAR_report_key_DT =  c("REMIND LCOE ($/MWh)")
VAR_report_key2_DT = c("DIETER Market value w/ scarcity price shaved ($/MWh)")

SEELPRICEkey = "pm_SEPrice"

# year_toplot <- c(2020,2025,2030,2035,2040,2045,2050,2055,2060,2070,2080,2090,2100) 

  get_report_variable_DT <- function(iteration, varlist){
    # iteration = 20
    # varlist =VAR_report_key4_DT
    
    cvs = sorted_annual_report_DT[[iteration]]
    annual_reportCSV = read.csv(cvs, sep = ";", header = T, stringsAsFactors = F)
    annual_reportQUITT <- as.quitte(annual_reportCSV) 
    
    vrdata <- annual_reportQUITT %>% 
      filter(period >2015) %>% 
      filter(tech %in% names(dieter.tech.mapping)) %>% 
      filter(model == "REMIND") %>% 
      revalue.levels(tech = dieter.tech.mapping) %>%
      mutate(tech = factor(tech, levels=rev(unique(dieter.tech.mapping)))) %>% 
      filter(variable %in% varlist) %>% 
      # mutate(value = value*36.1111/1000/.75/.75) %>% 
      select(period, tech, variable, value, unit)
    
    vrdata$iter = iteration
    
    return(vrdata)  
  }
  
vrN_cost_bkdw  <- lapply(iter_toplot, get_report_variable_DT, varlist=VAR_report_key_DT)

  gdx = sorted_files[[iter_toplot]]
  
  vrN_mv <- read.gdx(gdx, PARkey3) %>%
    mutate(marketvalue = value) %>%
    filter(all_te %in% FLEX_tech) %>%
    revalue.levels(all_te = remind.tech.mapping) %>%
    mutate(marketvalue = marketvalue * 1e12 / sm_TWa_2_MWh * 1.2) %>%
    dplyr::group_by(ttot,all_te) %>%
    dplyr::summarise( marketvalue = mean(marketvalue), .groups = "keep" ) %>%
    dplyr::ungroup(ttot,all_te) %>%
    select(period= ttot, tech=all_te, marketvalue) %>% 
    filter(period >2015)
  
  vrN_mv$variable <- "REMIND market value /market price ($/MWh)"
  
vrN_cost <- rbindlist(vrN_cost_bkdw) %>% 
  filter(iter %in% iter_toplot)

p <- ggplot(data = vrN_cost) +
  geom_line( aes(x = period, y = value, color = tech), size = 1.2, alpha = 0.5) +
  scale_color_manual(name = "tech", values = color.mapping)+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20, face="bold"), strip.text = element_text(size = 20)) +
  xlab("year") +
  coord_cartesian(ylim = c(0,700))+
  facet_wrap(~variable, nrow = 3, scales = "free")

ggsave(filename = paste0(mypath, run_number, "_iter", iter_toplot, "_LCOE_timeseries.png"), width = 16, height =10, units = "in", dpi = 120)

p <- ggplot() +
  geom_bar(data = vrN_cost, aes(x = period, y = value,  fill = variable), stat='identity', size = 1.2, alpha = 0.5)+
  geom_line(data = vrN_mv , aes(x = period, y = marketvalue, linetype = variable), size = 1.2, alpha = 1, color= 'black')+
  labs(color = "LCOE") +
  labs(linetype = "") +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20, face="bold"), strip.text = element_text(size = 20)) +
  xlab("year") + ylab(paste0("LCOH ($/tH2)"))  +
  coord_cartesian(ylim = c(0,200))+
  facet_wrap(~tech, nrow = 3, scales = "free")

ggsave(filename = paste0(mypath, run_number, "_iter", iter_toplot, "_LCOE_bar.png"), width = 20, height =10, units = "in", dpi = 120)


vrN_cost_alliter <- rbindlist(vrN_cost_bkdw)
VAR_names = c("IC", "OM", "FC", "CO2")

for (i in seq(1:length(VAR_report_key_DT))){
p<-ggplot() +
  geom_line(data = vrN_cost_alliter %>% filter(variable == VAR_report_key_DT[[i]]), aes(x = iter, y = value, color = tech), size = 1.2, alpha = 0.5) +
  scale_color_manual(name = "tech", values = color.mapping)+
  theme(axis.text=element_text(size=20), axis.title=element_text(size = 20,face="bold")) +
  xlab("iteration") + ylab(VAR_report_key_DT[[i]])  +
  facet_wrap(~period, nrow = 3, scales = "free")

ggsave(filename = paste0(mypath, run_number, "_",VAR_names[[i]],".png"), width = 16, height =10, units = "in", dpi = 120)
}

#================================================================================================
# LCOH

get_PRICEvariable <- function(iteration){
  # iteration =29
  gdx = sorted_files[[iteration]]
  
  vrdata <- read.gdx(gdx, SEELPRICEkey, squeeze = FALSE) %>%  
    filter(all_enty == ProdKEY) %>%
    filter(ttot %in% year_tovalid) %>%
    filter(all_regi == REGIkey1) %>% 
    mutate(value = value * 1e12 / sm_TWa_2_MWh * 1.2) %>% 
    select(period = ttot, seh2price = value) 
  
  vrdata$iter <- iteration
  return(vrdata)
}

vrN_PRICE0 <- lapply(iter_toplot, get_PRICEvariable)
vrN_PRICE<- rbindlist(vrN_PRICE0) 

vrN_PRICE$variable <- "seh2 price"

p <- ggplot() +
  geom_bar(data = vrN_cost %>% filter (tech == "Electrolyzers"), aes(x = period, y = value,  fill = variable), stat='identity', size = 1.2, alpha = 0.5)+
  geom_line(data = vrN_PRICE , aes(x = period, y = seh2price, linetype = variable), size = 1.2, alpha = 1, color= 'black')+
  labs(color = "LCOE") +
  labs(linetype = "") +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20, face="bold"), strip.text = element_text(size = 20)) +
  xlab("year") + ylab(paste0("LCOH ($/tH2)"))  +
  coord_cartesian(ylim = c(0,200))+
  facet_wrap(~tech, nrow = 3, scales = "free")

ggsave(filename = paste0(mypath, run_number, "_iter", iter_toplot, "_LCOH_bar.png"), width = 20, height =10, units = "in", dpi = 120)

