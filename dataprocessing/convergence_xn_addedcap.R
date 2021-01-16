mypath = "~/remind-coupling-dieter/dataprocessing/"
run_number = "mrkup19"
# run_number = "mrkup5_iter"
mydatapath = paste0("~/remind-coupling-dieter/output/", run_number, "/")

myDIETERPLOT_path = "~/remind-coupling-dieter/dataprocessing/DIETER_plots/"

source(paste0(mypath, "library_import.R"))
source(paste0(myDIETERPLOT_path, "GDXtoQuitte.R"))
library(readr)
require(rmndt)

# iteration_toplot_list <- c(30,50,80,90,95,100)
iteration_toplot_list <- c(10,25,37)


#remind output iteration gdx files
files <- list.files(mydatapath, pattern="fulldata_[0-9]+\\.gdx")
sorted_files <- paste0(mydatapath, "fulldata_", 1:length(files), ".gdx")
#dieter output iteration gdx files
files_DT_rep <- list.files(mydatapath, pattern="report_DIETER_i[0-9]+\\.gdx")

# for(fname in files_DT_rep){
#   gdxToQuitte_annual(mydatapath, fname,run_number)
# }

#dieter output iteration gdx files
# sorted_annual_report_DT <- paste0(myDIETERPLOT_path, run_number, "_i", seq(from = 5, to = length(files_DT_rep)*5, by = 5), "_annualreport.csv")
sorted_annual_report_DT <- paste0(myDIETERPLOT_path, run_number, "_i", seq(from = 2, to = length(files_DT_rep), by = 1), "_annualreport.csv")

VARkey1 = "capacity"
# VAR_report_key1_DT = c("Pre-investment capacities", "REMIND added capacities", "DIETER added capacities", "REMIND divestment")
VAR_report_key1_DT = c("REMIND pre-investment capacities", "DIETER pre-investment capacities","REMIND added capacities (GW)", "DIETER added capacities (GW)", "REMIND divestment (GW)")
TECH_report_keylst_DT = c("CCGT", "lig", "Solar", "Wind_on", "bio", "OCGT_eff", "ror", "nuc", "hc")
plot_DTte_names = c("combined cycle gas", "lignite", "solar", "wind", "biomass", "open cycle gas turbine", "hydro", "nuclear", "hard coal")
mycolors <- c("combined cycle gas" = "#999959", "lignite" = "#0c0c0c", "coal" = "#0c0c0c", "solar" = "#ffcc00", "wind" = "#337fff", "biomass" = "#005900", "open cycle gas turbine" = "#e51900", "hydro" =  "#191999", "nuclear" =  "#ff33ff", "hard coal" = "#808080")

get_report_variable_DT <- function(iteration){
  # iteration = 5
  # cvs = sorted_annual_report_DT[[iteration/5]]
  cvs = sorted_annual_report_DT[[iteration-1]]
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
  
  vrdata$iter <- iteration
  
  return(vrdata)
}

vrN_rep_DTlist  <- lapply(iteration_toplot_list, get_report_variable_DT)
#===================================================
vrN_rep_DT0 <- rbindlist(vrN_rep_DTlist)

for(iteration in iteration_toplot_list){

  # iteration = 10
RM_preInvest <- vrN_rep_DT0 %>% 
  filter(variable == "REMIND pre-investment capacities") %>% 
  filter(iter == iteration) %>% 
  mutate(model = "REMIND") %>% 
  mutate(variable = "Pre-investment capacities") %>% 
  select(period, tech, variable, value, iter, model) 

DT_preInvest <- vrN_rep_DT0 %>% 
  filter(variable == "REMIND pre-investment capacities") %>% 
  filter(iter == iteration) %>% 
  mutate(model = "DIETER") %>% 
  mutate(variable = "Pre-investment capacities") %>% 
  select(period, tech, variable, value, iter, model) 

#REMIND divestment read from DIETER reporting
RMdivest <- vrN_rep_DT0 %>% 
  filter(variable == "REMIND divestment (GW)") %>% 
  filter(iter == iteration) %>%
  mutate(model = "REMIND") %>% 
  mutate(value = -value) %>% 
  select(period, tech, variable, value, iter, model) 

RM_AddedCap <- vrN_rep_DT0 %>% 
  filter(variable == "REMIND added capacities (GW)") %>% 
  filter(iter == iteration) %>%
  mutate(model = "REMIND") %>% 
  mutate(variable = "Added capacities") %>% 
  select(period, tech, variable, value, iter, model) 

#REMIND addedCAP read from DIETER reporting
DT_AddedCap <- vrN_rep_DT0 %>% 
  filter(variable == "DIETER added capacities (GW)") %>% 
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
  theme(axis.text=element_text(size=10), axis.title=element_text(size= 10, face="bold")) +
  xlab("year") + ylab(paste0(VARkey1, "(GW)")) +
  ggtitle(paste0("Added capacity (iter = ", iteration, ")"))+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank()) +
  theme(aspect.ratio = .5) +
  scale_fill_manual(name = "all_te", values = mycolors)+
  facet_wrap(~tech, nrow = 3, scales = "free") 

ggsave(filename = paste0(mypath, "iter_xN_addedCAP_", run_number, "_iter", iteration, ".png"), width = 16, height =10, units = "in", dpi = 220)
}

