mypath = "~/remind-coupling-dieter/dataprocessing/"
run_number = "hydro654"
mydatapath = paste0("~/remind-coupling-dieter/output/", run_number, "/")

myDIETERPLOT_path = "~/remind-coupling-dieter/dataprocessing/DIETER_plots/"

source(paste0(mypath, "library_import.R"))
source(paste0(myDIETERPLOT_path, "GDXtoQuitte.R"))
library(readr)
require(rmndt)

#remind output iteration gdx files
files <- list.files(mydatapath, pattern="fulldata_[0-9]+\\.gdx")
sorted_files <- paste0(mydatapath, "fulldata_", 1:length(files), ".gdx")
#dieter output iteration gdx files
files_DT_rep <- list.files(mydatapath, pattern="report_DIETER_i[0-9]+\\.gdx")

# for(fname in files_DT_rep){
# gdxToQuitte_annual(mydatapath, fname,run_number)
# }

#dieter output iteration gdx files
id <- NULL
for(fname in files_DT_rep){
  idx <- as.numeric(str_extract(fname, "[0-9]+"))
  id = c(id, idx)
}

if (length(files_DT_rep) != 0) {
  sorted_annual_report_DT <- paste0(myDIETERPLOT_path, run_number, "_i", sort(id), "_annualreport.csv")
}

# iteration_toplot_list <-c(1,2,3,4,5)
iteration_toplot_list <-c(5,10,20,max(id)+1)

dieter.report.cap <- c("DIETER pre-investment capacities (GW)","REMIND pre-investment capacities (GW)")
dieter.report.addcap <- c("DIETER added capacities (GW)", "REMIND added capacities (GW)")
dieter.report.divest <- c("REMIND divestment (GW)")

dieter.report.vars <- c(dieter.report.cap, dieter.report.addcap, dieter.report.divest)

# plot_DTte_names = c("combined cycle gas", "lignite", "solar", "wind", "biomass", "open cycle gas turbine", "hydro", "nuclear", "hard coal")
plot_DTte_names = c("combined cycle gas", "coal", "solar", "wind", "biomass", "open cycle gas turbine", "hydro", "nuclear")
mycolors_model = c("REMIND" = "#FF0000FF", "DIETER" = "#00FF66FF")

get_report_variable_DT <- function(iteration){
  # iteration = 5
  cvs = sorted_annual_report_DT[[iteration]]
  annual_reportCSV = read.csv(cvs, sep = ';', header = T, stringsAsFactors = F)
  annual_reportQUITT <- as.quitte(annual_reportCSV) 
  
  vrdata <- annual_reportQUITT %>% 
    filter(variable %in% dieter.report.vars) %>% 
    filter(!tech %in% c("lig","hc")) %>%
    revalue.levels(tech = dieter.tech.mapping) %>%
    select(period, tech, variable, value,model)
  
  vrdata$iter <- iteration
  
  return(vrdata)
}

vrN_rep_DTlist  <- lapply(iteration_toplot_list, get_report_variable_DT)
#===================================================
vrN_rep_DT0 <- rbindlist(vrN_rep_DTlist)

for(iteration in iteration_toplot_list){

  # iteration = 10
df.RM <- vrN_rep_DT0 %>% 
  filter(iter == iteration) %>% 
  filter(model == "REMIND") %>% 
  mutate(value = ifelse(variable == "REMIND divestment (GW)", -value, value)) %>% 
  mutate(variable = stringr::str_replace(variable, "REMIND", "")) %>% 
  mutate(variable = stringr::str_replace(variable, "\\(GW\\)", ""))

df.DT <- vrN_rep_DT0 %>% 
  filter(iter == iteration) %>% 
  filter(model == "DIETER") %>% 
  mutate(variable = stringr::str_replace(variable, "DIETER", "")) %>% 
  mutate(variable = stringr::str_replace(variable, "\\(GW\\)", ""))

#===================================================

barwidth = 1.5
# mycolors2 = c("Added capacities" = "#0c0c0c", "Pre-investment capacities" = "#337fff", "REMIND divestment" = "#339900")

p<-ggplot() +
  geom_col(data = df.RM %>% filter(period > 2015 & period <2110), aes(x = period-barwidth/2-0.1, y = value, fill = model, alpha = variable), colour="black", position='stack', size = 1, width = barwidth) +
  geom_col(data = df.DT %>% filter(period > 2015 & period <2110), aes(x = period+barwidth/2+0.1, y = value, fill = model, alpha = variable), colour="black", position='stack', size = 1,
  width = barwidth) +
  scale_alpha_manual(values=c("Pre-investment capacities"= 1, "Added capacities"=0.5, "divestment"=0.2), limits=c("Pre-inv. cap.", "Added cap.", "Divestment")) +
  scale_alpha_discrete(range = c(0.4,1)) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size= 10, face="bold")) +
  xlab("year") + ylab(paste0("Capacity (GW)")) +
  ggtitle(paste0("Added capacity (iter = ", iteration, ")"))+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank()) +
  theme(aspect.ratio = .5) +
  scale_fill_manual(name = "model", values =mycolors_model)+
  facet_wrap(~tech, nrow = 3, scales = "free") 

ggsave(filename = paste0(mypath, run_number, "_addedCAP_iter=", iteration, ".png"),p, width = 19, height =10, units = "in", dpi = 220)
}

