mypath = "~/remind/dataprocessing/"
run_number = 29
mydatapath = paste0("~/remind/output/capfac", run_number, "/")
myDIETERPLOT_path = "~/remind/dataprocessing/DIETER_plots/"
# import library
source(paste0(mypath, "library_import.R"))
source(paste0(myDIETERPLOT_path, "GDXtoQuitte.R"))
library(readr)
require(rmndt)

igdx("/opt/gams/gams30.2_linux_x64_64_sfx")

#dieter output iteration gdx files
files_DT_rep <- list.files(mydatapath, pattern="report_DIETER_i[0-9]+\\.gdx")
sorted_files_DT_rep <- paste0(mydatapath, "report_DIETER_i", seq(from = 5, to = length(files_DT_rep)*5, by = 5), ".gdx")

for(fname in files_DT_rep){
  gdxToQuitte_annual(mydatapath, fname)
}

sorted_annual_report_DT <- paste0(myDIETERPLOT_path, "capfac", run_number, "_i", seq(from = 5, to = length(files_DT_rep)*5, by = 5), "_annualreport.csv")

iteration_toplot_list <- c(5, 10, 20, 30)

# iteration = 30

year_toplot_list <- c(2010,2015,2020,2025,2030,2035,2040,2045,2050,2055,2060,2070,2080,2090,2100)
for(year_toplot in year_toplot_list){

# year_toplot = 2025

TECHkeylst_DT = c("CCGT", "Lignite", "Solar PV", "Wind_on", "Biomass", "OCGT_eff", "Run-of-River", "Nuclear", "Hard coal")
TECHFCkeylst_DT = c("CCGT", "Lignite",  "Biomass", "OCGT_eff", "Nuclear", "Hard coal")
plot_te_names = c("combined cycle gas", "lignite", "solar", "wind", "biomass", "open cycle gas turbine", "hydro", "nuclear", "hard coal")
mycolors <- c("combined cycle gas" = "#999959", "lignite" = "#0c0c0c", "solar" = "#ffcc00", "wind" = "#337fff", "biomass" = "#005900", "open cycle gas turbine" = "#e51900", "hydro" =  "#191999", "nuclear" =  "#ff33ff", "hard coal" = "#808080")

te_toplot = TECHkeylst_DT
cost_toplot = c("fuel cost (divided by eta)", "O&M cost", "Annualized investment cost", "CO2 cost")

# VAR_report_key1_DT = c("marginal market value", "marginal capacity factor","Marginal Revenue","Revenue marginal plant per MW")
VAR_report_key1_DT = "marginal market value"
VAR_report_key2_DT = "marginal capacity factor"

VAR_report_keylst_DT = c(VAR_report_key1_DT,VAR_report_key2_DT)

#LCOE_marg = FC[$/kWh] + CO2 [$/kWh] + (IC [$/kW] + O&M [$/kW])/FLh_marg

#load FLH_marg

##########################################
##########market value ##################

get_report_variable_DT <- function(iteration){
  
  annual_reportCSV = read.csv(paste0(myDIETERPLOT_path, "capfac", run_number, "_i", iteration, "_annualreport.csv"), sep = ';', header = T, stringsAsFactors = F)
  annual_reportQUITT <- as.quitte(annual_reportCSV) 
  
  vrdata <- annual_reportQUITT %>% 
    filter(period == year_toplot) %>%
    filter(tech %in% TECHkeylst_DT)%>% 
    filter(variable%in%VAR_report_keylst_DT) %>% 
    select(period, tech, variable, value, unit) 
  
  vrdata$iter <- iteration
  
  return(vrdata)
}

marg_DT <- lapply(iteration_toplot_list, get_report_variable_DT)

#===================================================

marg_DT <- rbindlist(marg_DT)


MV_marg_DT <- marg_DT %>% 
  filter(variable == VAR_report_key1_DT) 

CF_marg_DT <- marg_DT %>% 
  filter(variable == VAR_report_key2_DT) %>% 
  dplyr::rename(capfac = value) %>% 
  mutate(value = as.numeric(150))

##########################################
########## cost ##################
get_cost <- function(iteration){
  
  run_number <- as.numeric(str_extract(mydatapath, "[0-9]+"))
  
  annual_reportCSV = read.csv(paste0(myDIETERPLOT_path, "capfac", run_number, "_i", iteration, "_annualreport.csv"), sep = ';', header = T, stringsAsFactors = F)
  annual_reportQUITT <- as.quitte(annual_reportCSV) 
  
FC <- annual_reportQUITT %>% 
  filter(period == year_toplot) %>% 
  filter(variable == cost_toplot[[1]]) %>% 
  filter(tech %in% TECHFCkeylst_DT) %>% 
  select(period, variable, tech, value) %>% 
  mutate(variable = str_replace(variable, " \\(divided by eta\\)", ""))

CO2 <- annual_reportQUITT %>% 
  filter(period == year_toplot) %>% 
  filter(variable == cost_toplot[[4]]) %>% 
  filter(tech %in% TECHFCkeylst_DT) %>% 
  select(period, variable, tech, value)

#=====================================================
EDem <- annual_reportQUITT %>%
  filter(period == year_toplot) %>%
  filter(variable == "Gross electricity demand") %>%
  select(period, value) %>%
  dplyr::rename(Edem = value)

Eshare <- annual_reportQUITT %>%
  filter(period == year_toplot) %>%
  filter(variable %in% c("Conventional generation share", "Renewable generation share")) %>%
  select(period, value, tech)%>%
  mutate(value = value/1e2)

#TWh to MWh
GEN <- Eshare %>%
  left_join(EDem) %>%
  mutate(value = Edem * value * 1e6) %>%
  select(period, value, tech)%>% 
  dplyr::rename(gen = value)

CAP <- annual_reportQUITT %>% 
  filter(period == year_toplot) %>% 
  filter(variable == "Installed capacity") %>% 
  select(period, value, tech)%>% 
  mutate(value = value * 1e6)%>% 
  dplyr::rename(cap = value) 

FLH_marg <- annual_reportQUITT %>% 
  filter(period == year_toplot) %>%
  filter(variable == "marginal capacity factor")%>% 
  mutate(value = as.integer(value / 1e2 * 8760))%>% 
  select(period, tech, value)%>% 
  dplyr::rename(flh_marg = value) 
  
OM0 <- annual_reportQUITT %>% 
  filter(period == year_toplot) %>% 
  filter(variable == cost_toplot[[2]]) %>% 
  filter(tech %in% te_toplot) %>% 
  select(period, variable, tech, value)

OM <- OM0 %>% 
  left_join(GEN) %>% 
  left_join(CAP) %>% 
  left_join(FLH_marg) %>% 
  mutate(value = value * cap /flh_marg) %>% 
  select(period, variable, tech, value)

IC0 <- annual_reportQUITT %>% 
  filter(period == year_toplot) %>% 
  filter(variable == cost_toplot[[3]]) %>% 
  filter(tech %in% te_toplot) %>% 
  select(period, variable, tech, value)

IC <- IC0 %>% 
  left_join(GEN) %>% 
  left_join(CAP) %>% 
  left_join(FLH_marg) %>% 
  mutate(value = value * cap / flh_marg) %>% 
  select(period, variable, tech, value)

COST = list(FC,CO2, OM,IC) %>% 
  reduce(full_join)

COST$variable <- factor( COST$variable, levels = c("Annualized investment cost","O&M cost","fuel cost","CO2 cost") )

COST$iter <- iteration

return(COST)

}



COST_list <- lapply(iteration_toplot_list, get_cost)


COST <- rbindlist(COST_list)

         
barwidth = .3

p<-ggplot()+
  geom_col(data = COST, aes(x = factor(iter), y = value, fill = variable), alpha = 0.7, position='stack', size = 1, width = barwidth) +
  geom_point(data = MV_marg_DT, aes(x = factor(iter), y = value, fill = variable), size = 3) +
  ggrepel::geom_text_repel(data = CF_marg_DT,
                            mapping = aes(x = factor(iter), y = value, label = paste0(capfac, "%")),
                            nudge_y = 0.5, point.padding = 3, segment.size = 0)+
  theme(axis.text=element_text(size = 10), axis.title = element_text(size= 10, face = "bold")) +
  xlab("iter") + ylab("$/MWh") +
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank()) +
  theme(aspect.ratio = .5) +
  # coord_cartesian(ylim = c(0,400))+
  ggtitle(paste0("Year = ", year_toplot)) +
  # scale_fill_manual(values = mycolors, name = "variable") +
  facet_wrap(~tech, nrow = 3, scales = "free")

ggsave(filename = paste0(mypath, "iter_xN_LCOE_capFac", run_number, "_yr", year_toplot, ".png"), width = 16, height =10, units = "in", dpi = 120)
print(paste0("figures saved to", mypath))

}

