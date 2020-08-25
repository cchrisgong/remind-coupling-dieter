# import library
mypath = "~/remind/dataprocessing/"
run_number = 18
myDIETERPLOT_path = "~/remind/dataprocessing/DIETER_plots/"

source(paste0(mypath, "library_import.R"))
source(paste0(myDIETERPLOT_path, "GDXtoQuitte.R"))
library(readr)
require(rmndt)

mydatapath = paste0("~/remind/output/capfac", run_number, "/")

#remind output iteration gdx files
files <- list.files(mydatapath, pattern="fulldata_[0-9]+\\.gdx")
sorted_files <- paste0(mydatapath, "fulldata_", 1:length(files), ".gdx")
#dieter output iteration gdx files
files_DT_rep <- list.files(mydatapath, pattern="report_DIETER_i[0-9]+\\.gdx")
sorted_files_DT_rep <- paste0(mydatapath, "report_DIETER_i", seq(from = 5, to = length(files_DT_rep)*5, by = 5), ".gdx")

for(fname in files_DT_rep){
  gdxToQuitte_annual(mydatapath, fname)
  }
#dieter output iteration gdx files
sorted_annual_report_DT <- paste0(myDIETERPLOT_path, "capfac", run_number, "_i", seq(from = 5, to = length(files_DT_rep)*5, by = 5), "_annualreport.csv")

iteration_toplot_list <- c(5,10,20,30)
for(iteration in iteration_toplot_list){

# iteration = 5

VARkey1 = "vm_cap"
REGIkey1 = "DEU"
TECHkeylst_peakGas = c("ngt")
TECHkeylst_nonPeakGas = c("ngccc","ngcc",  "gaschp") 
TECHkeylst_coal = c("coalchp", "igccc", "igcc", "pcc", "pco","pc")
TECHkeylst_solar = c("spv")
TECHkeylst_wind = c("wind")
TECHkeylst_hydro = c("hydro")
TECHkeylst_nuclear = c("tnrs")
TECHkeylst_placeholder = c("fnrs") #for splitting REMIND coal into lignite and hard coal
TECHkeylst_biomass = c("biochp", "bioigccc", "bioigcc")
grade = "1"

TECHkeylst <- c(TECHkeylst_peakGas, TECHkeylst_nonPeakGas, TECHkeylst_coal, TECHkeylst_solar, TECHkeylst_wind, TECHkeylst_hydro,TECHkeylst_biomass, TECHkeylst_nuclear, TECHkeylst_placeholder)

# VAR_report_key1_DT = c("Pre-investment capacities", "REMIND added capacities", "DIETER added capacities", "REMIND divestment")
VAR_report_key1_DT = c("REMIND pre-investment capacities", "DIETER pre-investment capacities","REMIND added capacities from REMIND pre-investment", "DIETER added capacities", "REMIND divestment")
TECH_report_keylst_DT = c("CCGT", "Lignite", "Solar PV", "Wind_on", "Biomass", "OCGT_eff", "Run-of-River", "Nuclear", "Hard coal")
plot_RMte_names = c("combined cycle gas", "coal", "solar", "wind", "biomass", "open cycle gas turbine", "hydro", "nuclear")
plot_DTte_names = c("combined cycle gas", "lignite", "solar", "wind", "biomass", "open cycle gas turbine", "hydro", "nuclear", "hard coal")
mycolors <- c("combined cycle gas" = "#999959", "lignite" = "#0c0c0c", "coal" = "#0c0c0c", "solar" = "#ffcc00", "wind" = "#337fff", "biomass" = "#005900", "open cycle gas turbine" = "#e51900", "hydro" =  "#191999", "nuclear" =  "#ff33ff", "hard coal" = "#808080")

get_report_variable_DT <- function(cvs){
  # cvs = sorted_annual_report_DT[[1]]
  annual_reportCSV = read.csv(cvs, sep = ';', header = T, stringsAsFactors = F)
  annual_reportQUITT <- as.quitte(annual_reportCSV) 
  
  vrdata <- annual_reportQUITT %>% 
    # filter(period == "2055") %>%
    filter(tech %in% TECH_report_keylst_DT)%>% 
    filter(variable %in% VAR_report_key1_DT) %>% 
    select(period, tech, variable, value) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_DT[[1]], plot_DTte_names[[1]])) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_DT[[2]], plot_DTte_names[[2]])) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_DT[[3]], plot_DTte_names[[3]])) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_DT[[4]], plot_DTte_names[[4]])) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_DT[[5]], plot_DTte_names[[5]])) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_DT[[6]], plot_DTte_names[[6]])) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_DT[[7]], plot_DTte_names[[7]])) %>%  
    mutate(tech = str_replace(tech, TECH_report_keylst_DT[[8]], plot_DTte_names[[8]])) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_DT[[9]], plot_DTte_names[[9]])) 
  
  return(vrdata)
}

vrN_rep_DTlist  <- lapply(sorted_annual_report_DT, get_report_variable_DT)

#===================================================
idx_DT <- 1:length(files_DT_rep)
for(id in idx_DT){
  vrN_rep_DTlist[[id]]$iter <- id*5
}

vrN_rep_DT0 <- rbindlist(vrN_rep_DTlist)

RM_preInvest <- vrN_rep_DT0 %>% 
  filter(variable == "REMIND pre-investment capacities") %>% 
  filter(iter == iteration) %>% 
  mutate(model = "REMIND") %>% 
  mutate(variable = "Pre-investment capacities") %>% 
  select(period, tech, variable, value, iter, model) 

DT_preInvest <- vrN_rep_DT0 %>% 
  filter(variable == "DIETER pre-investment capacities") %>% 
  filter(iter == iteration) %>% 
  mutate(model = "DIETER") %>% 
  mutate(variable = "Pre-investment capacities") %>% 
  select(period, tech, variable, value, iter, model) 

#REMIND divestment read from DIETER reporting
RMdivest <- vrN_rep_DT0 %>% 
  filter(variable == "REMIND divestment") %>% 
  filter(iter == iteration) %>%
  mutate(model = "REMIND") %>% 
  mutate(value = -value) %>% 
  select(period, tech, variable, value, iter, model) 

RM_AddedCap <- vrN_rep_DT0 %>% 
  filter(variable == "REMIND added capacities from REMIND pre-investment") %>% 
  filter(iter == iteration) %>%
  mutate(model = "REMIND") %>% 
  mutate(variable = "Added capacities") %>% 
  select(period, tech, variable, value, iter, model) 

#REMIND addedCAP read from DIETER reporting
DT_AddedCap <- vrN_rep_DT0 %>% 
  filter(variable == "DIETER added capacities") %>% 
  filter(iter == iteration) %>%
  mutate(model = "DIETER") %>% 
  mutate(variable = "Added capacities") %>% 
  select(period, tech, variable, value, iter, model) 

#===================================================

vrN_plot = list(RM_AddedCap, RMdivest, RM_preInvest) %>% 
  reduce(full_join)

vrN_plot$variable <- factor(vrN_plot$variable, levels= unique(vrN_plot$variable), ordered=TRUE)


vrN_DT_plot = list(DT_AddedCap, DT_preInvest) %>% 
  reduce(full_join)

vrN_DT_plot$variable <- factor(vrN_DT_plot$variable, levels= unique(vrN_DT_plot$variable), ordered=TRUE)

#===================================================

barwidth = 1.5
mycolors2 = c("Added capacities" = "#0c0c0c", "Pre-investment capacities" = "#337fff", "REMIND divestment" = "#339900")

p<-ggplot() +
  geom_col(data = vrN_plot, aes(x = period-barwidth/2-0.1, y = value, alpha = model, color = variable, fill = tech), position='stack', size = 1, width = barwidth) +
  geom_col(data = vrN_DT_plot, aes(x = period+barwidth/2+0.1, y = value, alpha = model, color = variable, fill = tech), position='stack', size = 1,
  width = barwidth) +
  scale_color_manual(name = "variable", values = mycolors2)+
  scale_alpha_discrete(range = c(0.4,1)) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size= 10,face="bold")) +
  xlab("year") + ylab(paste0(VARkey1, "(GW)")) +
  ggtitle(paste0("Added capacity (iter = ", iteration, ")"))+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank()) +
  theme(aspect.ratio = .5) +
  scale_fill_manual(name = "all_te", values = mycolors)+
  facet_wrap(~tech, nrow = 3, scales = "free") 

ggsave(filename = paste0(mypath, "iter_xN_addedCAP_capFac", run_number, "_iter", iteration, ".png"), width = 16, height =10, units = "in", dpi = 220)

}

