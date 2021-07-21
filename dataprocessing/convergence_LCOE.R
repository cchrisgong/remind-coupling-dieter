mypath = "~/remind-coupling-dieter/dataprocessing/"
run_number = "mrkup106"
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
                         NULL)


color.mapping <- c("CCGT" = "#999959", "Lignite" = "#0c0c0c", "Coal (Lig + HC)" = "#0c0c0c",
                   "Solar" = "#ffcc00", "Wind" = "#337fff", "Biomass" = "#005900",
                   "OCGT" = "#e51900", "Hydro" = "#191999", "Nuclear" = "#ff33ff",
                   "Hard coal" = "#808080", "peak demand" = "#0c0c0c")


files_DT_rep <- list.files(mydatapath, pattern = "report_DIETER_i[0-9]+\\.gdx")
sorted_files_DT <- paste0(mydatapath, "report_DIETER_i", seq(from = 2, to = length(files_DT_rep), by = 1), ".gdx")

maxiter = length(files_DT_rep)

# for(fname in files_DT_rep){
# gdxToQuitte_annual(mydatapath, fname, run_number)
# }

sorted_annual_report_DT <- paste0(myDIETERPLOT_path, run_number, "_i", seq(from = 2, to = length(files_DT_rep), by = 1), "_annualreport.csv")

VAR_report_key_DT =  c("annualized investment cost ($/kW)", "O&M cost ($/kW)", "fuel cost - divided by eta ($/MWh)","CO2 cost ($/MWh)")
VAR_report_key2_DT = c("DIETER Market value ($/MWh)")

iter_toplot = 2:length(sorted_files_DT) + 1

year_toplot <- c(2020,2025,2030,2035,2040,2045,2050,2055,2060,2070,2080,2090,2100) 

  get_report_variable_DT <- function(iteration, varlist){
    # iteration = 15
    # varlist =VAR_report_key4_DT
    
    cvs = sorted_annual_report_DT[[iteration - 1]]
    annual_reportCSV = read.csv(cvs, sep = ";", header = T, stringsAsFactors = F)
    annual_reportQUITT <- as.quitte(annual_reportCSV) 
    
    vrdata <- annual_reportQUITT %>% 
      filter(period %in% year_toplot) %>% 
      filter(tech %in% names(dieter.tech.mapping)) %>% 
      revalue.levels(tech = dieter.tech.mapping) %>%
      mutate(tech = factor(tech, levels=rev(unique(dieter.tech.mapping)))) %>% 
      filter(variable %in% varlist) %>% 
      select(period, tech, variable, value, unit)
    
    vrdata$iter = iteration
    
    return(vrdata)  
  }
  
vrN_cost_bkdw  <- lapply(iter_toplot, get_report_variable_DT, varlist=VAR_report_key_DT)

iter_toplot2= c(10,20,30,35)
vrN_cost <- rbindlist(vrN_cost_bkdw) %>% 
  filter(iter %in% iter_toplot2)

for (i in iter_toplot2){
  print(i)

p <- ggplot(data = vrN_cost%>% filter(iter == i)) +
  geom_line( aes(x = period, y = value, color = tech), size = 1.2, alpha = 0.5) +
  scale_color_manual(name = "tech", values = color.mapping)+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20, face="bold"), strip.text = element_text(size = 20)) +
  xlab("year") +
  # coord_cartesian(ylim = c(24,27))+
  facet_wrap(~variable, nrow = 3, scales = "free")

ggsave(filename = paste0(mypath, run_number, "_iter", i, "_LCOE_timeseries.png"), width = 16, height =10, units = "in", dpi = 120)
}

vrN_cost_alliter <- rbindlist(vrN_cost_bkdw)
VAR_names = c("IC", "OM", "FC", "CO2")

for (i in seq(1:length(VAR_report_key_DT))){
p<-ggplot() +
  geom_line(data = vrN_cost_alliter%>% filter(variable == VAR_report_key_DT[[i]]), aes(x = iter, y = value, color = tech), size = 1.2, alpha = 0.5) +
  scale_color_manual(name = "tech", values = color.mapping)+
  theme(axis.text=element_text(size=20), axis.title=element_text(size = 20,face="bold")) +
  xlab("year") + ylab(paste0("LCOE cost component"))  +
  facet_wrap(~period, nrow = 3, scales = "free")

ggsave(filename = paste0(mypath, run_number, "_",VAR_names[[i]],".png"), width = 16, height =10, units = "in", dpi = 120)
print(paste0("figures saved to", mypath))
}
