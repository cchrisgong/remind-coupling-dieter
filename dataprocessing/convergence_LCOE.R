mypath = "~/remind-coupling-dieter/dataprocessing/"
run_number = "mrkup19"
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

maxiter = 37

mycolors <- c("combined cycle gas" = "#999959", "lignite" = "#0c0c0c", "coal" = "#0c0c0c", "solar" = "#ffcc00", "wind" = "#337fff", "biomass" = "#005900", "open cycle gas turbine" = "#e51900", "hydro" =  "#191999", "nuclear" =  "#ff33ff", "hard coal" = "#808080", "coupled run seel price" = "#ff0000","uncoupled run seel price" = "#ff0000")
TECH_report_keylst_DT = c("coal", "CCGT", "lig", "Solar", "Wind_on", "bio", "OCGT_eff", "ror", "nuc", "hc")
plot_DTte_names = c("coal","combined cycle gas", "lignite", "solar", "wind", "biomass", "open cycle gas turbine", "hydro", "nuclear", "hard coal")

files_DT_rep <- list.files(mydatapath, pattern="report_DIETER_i[0-9]+\\.gdx")
# sorted_files_DT_rep <- paste0(mydatapath, "report_DIETER_i", seq(from = 5, to = length(files_DT_rep)*5, by = 5), ".gdx")
sorted_files_DT <- paste0(mydatapath, "report_DIETER_i", seq(from = 2, to = length(files_DT_rep), by = 1), ".gdx")

# for(fname in files_DT_rep){
# gdxToQuitte_annual(mydatapath, fname, run_number)
# }

# sorted_annual_report_DT <- paste0(myDIETERPLOT_path, run_number, "_i", seq(from = 5, to = length(files_DT_rep)*5, by = 5), "_annualreport.csv")
sorted_annual_report_DT <- paste0(myDIETERPLOT_path, run_number, "_i", seq(from = 2, to = length(files_DT_rep), by = 1), "_annualreport.csv")

# VAR_report_key4_DT = c("annualized investment cost","O&M cost","fuel cost (divided by eta)","CO2 cost")
VAR_report_key4_DT = c("fuel cost (divided by eta)")
VAR_report_key1_DT = c("DIETER Market value ($/MWh)")

iter_toplot = 2:length(sorted_files_DT)+1
TECH_report_keylst_DT = c("coal", "CCGT", "lig", "Solar", "Wind_on", "bio", "OCGT_eff", "ror", "nuc", "hc")
TECH_report_toplot = c("solar", "wind")

# year_toplot_list <- c(2050,2070) 
year_toplot <- c(2010,2015,2020,2025,2030,2035,2040,2045,2050,2055,2060,2070,2080,2090,2100) 

# for(year_toplot in year_toplot_list){
  # year_toplot = 2030
  
  get_report_variable_DT <- function(iteration, varlist){
    # iteration = 15
    # varlist =VAR_report_key4_DT
    
    cvs = sorted_annual_report_DT[[iteration-1]]
    annual_reportCSV = read.csv(cvs, sep = ";", header = T, stringsAsFactors = F)
    annual_reportQUITT <- as.quitte(annual_reportCSV) 
    
    vrdata <- annual_reportQUITT %>% 
      filter(period %in% year_toplot) %>% 
      filter(tech %in% TECH_report_keylst_DT) %>% 
      filter(variable %in% varlist) %>% 
      select(period, tech, variable, value, unit) %>% 
      mutate(tech = str_replace(tech, TECH_report_keylst_DT[[1]], plot_DTte_names[[1]])) %>% 
      mutate(tech = str_replace(tech, TECH_report_keylst_DT[[2]], plot_DTte_names[[2]])) %>% 
      mutate(tech = str_replace(tech, TECH_report_keylst_DT[[3]], plot_DTte_names[[3]])) %>% 
      mutate(tech = str_replace(tech, TECH_report_keylst_DT[[4]], plot_DTte_names[[4]])) %>% 
      mutate(tech = str_replace(tech, TECH_report_keylst_DT[[5]], plot_DTte_names[[5]])) %>% 
      mutate(tech = str_replace(tech, TECH_report_keylst_DT[[6]], plot_DTte_names[[6]])) %>% 
      mutate(tech = str_replace(tech, TECH_report_keylst_DT[[7]], plot_DTte_names[[7]])) %>%  
      mutate(tech = str_replace(tech, TECH_report_keylst_DT[[8]], plot_DTte_names[[8]])) %>% 
      mutate(tech = str_replace(tech, TECH_report_keylst_DT[[9]], plot_DTte_names[[9]])) %>% 
      mutate(tech = str_replace(tech, TECH_report_keylst_DT[[10]], plot_DTte_names[[10]])) 
    # %>% 
      # filter(tech %in% TECH_report_toplot)
    
    vrdata$iter = iteration
    
    return(vrdata)  
  }
  
vrN_cost_bkdw  <- lapply(iter_toplot, get_report_variable_DT, varlist=VAR_report_key4_DT)
  
vrN_cost <- rbindlist(vrN_cost_bkdw) %>% 
  filter(iter %in% c(10,20,30,36))

p<-ggplot() +
  geom_line(data = vrN_cost, aes(x = period, y = value, color = tech), size = 1.2, alpha = 0.5) +
  scale_color_manual(name = "tech", values = mycolors)+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold")) +
  xlab("year") + ylab(paste0("fuel cost(USD/MWh)"))  +
  # coord_cartesian(ylim = c(24,27))+
  facet_wrap(~iter, nrow = 3, scales = "free")

ggsave(filename = paste0(mypath, "iter_xN_fuelcost_", run_number, ".png"), width = 16, height =10, units = "in", dpi = 120)
# ggsave(filename = paste0(mypath, "iter_IC_", run_number, "_yr", year_toplot, ".png"), width = 16, height =10, units = "in", dpi = 120)
print(paste0("figures saved to", mypath))

# }

