# import library
library(stringr)
library(purrr)
library(reshape2)
library(quitte)
library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)
library(readxl)
library(ggthemes)
library(gridExtra)
library(cowplot)
library(lubridate)
library(RColorBrewer)
library(zoo)

library(tibble)
library(dygraphs)
library(grid)
library(plyr)
library(gridExtra)
library(cowplot)
library(lubridate)
library(RColorBrewer)
library(openxlsx)

library(gdxrrw)

  # ~/remind/dataprocessing/debug/anastasis_vm_cap_inconsistency/
gdx = paste0(getwd(),"/report_test.gdx")
  
VAR_report_key1_DT = c("REMIND pre-investment capacities", "REMIND added capacities (GW)", "REMIND divestment (GW)")
TECH_report_keylst_RM = c("CCGT", "coal", "Solar", "Wind_on", "bio", "OCGT_eff", "ror", "nuc")
plot_DTte_names = c("combined cycle gas", "coal", "solar", "wind", "biomass", "open cycle gas turbine", "hydro", "nuclear")
mycolors <- c("combined cycle gas" = "#999959", "lignite" = "#0c0c0c", "coal" = "#0c0c0c", "solar" = "#ffcc00", "wind" = "#337fff", "biomass" = "#005900", "open cycle gas turbine" = "#e51900", "hydro" =  "#191999", "nuclear" =  "#ff33ff", "hard coal" = "#808080")

vrN_rep <-read.gdx(gdxName = gdx, requestList = 'report_tech', factors = FALSE) %>% 
    dplyr::rename(tech = X..5)  %>% 
    dplyr::rename(variable = X..4)  %>% 
    dplyr::rename(reg = X..3)  %>% 
    dplyr::rename(period = X..2)  %>% 
    dplyr::rename(model = X..1)  %>% 
    dplyr::rename(iter = X.) %>%
    mutate(iter = as.numeric(str_extract(iter, "[0-9]+"))) %>% 
  group_by(iter, model, reg, tech, variable) %>%
  mutate(period = as.numeric(period)) %>% 
  complete(period = c(2010,2015,2020,2025,2030,2035,2040,2045,2050,2055,2060,2070,2080,2090,2100)) %>%
  replace(is.na(.), 0) %>%
  ungroup(iter, model, reg, tech, variable) %>%

    filter(tech %in% TECH_report_keylst_RM)%>%
    select(iter,period, tech, variable, value) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_RM[[1]], plot_DTte_names[[1]])) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_RM[[2]], plot_DTte_names[[2]])) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_RM[[3]], plot_DTte_names[[3]])) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_RM[[4]], plot_DTte_names[[4]])) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_RM[[5]], plot_DTte_names[[5]])) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_RM[[6]], plot_DTte_names[[6]])) %>% 
    mutate(tech = str_replace(tech, TECH_report_keylst_RM[[7]], plot_DTte_names[[7]])) %>%  
    mutate(tech = str_replace(tech, TECH_report_keylst_RM[[8]], plot_DTte_names[[8]])) 

iteration_toplot_list <- c(1:length(files))
    
for(iteration in iteration_toplot_list){
      # iteration = 5
RM_preInvest <- vrN_rep %>% 
  filter(variable == "REMIND pre-investment capacities") %>% 
  filter(iter == iteration) %>%
  mutate(variable = "Pre-investment capacities") %>% 
  select(period, tech, variable, value, iter) 

#REMIND divestment read from DIETER reporting
RMdivest <- vrN_rep %>% 
  filter(variable == "REMIND divestment (GW)") %>% 
  filter(iter == iteration) %>%
  mutate(value = -value) %>% 
  select(period, tech, variable, value, iter) 

RM_AddedCap <- vrN_rep %>% 
  filter(variable == "REMIND added capacities (GW)") %>% 
  filter(iter == iteration) %>%
  mutate(variable = "Added capacities") %>% 
  select(period, tech, variable, value, iter) 

#===================================================

vrN_plot = list(RM_AddedCap, RMdivest, RM_preInvest) %>% 
  reduce(full_join)

vrN_plot$variable <- factor(vrN_plot$variable, levels= unique(vrN_plot$variable), ordered=TRUE)

#===================================================

barwidth = 1.5
mycolors2 = c("Added capacities" = "#0c0c0c", "Pre-investment capacities" = "#337fff", "REMIND divestment" = "#339900")

p<-ggplot() +
  geom_col(data = vrN_plot, aes(x = period-barwidth/2, y = value, color = variable, fill = tech), position='stack', size = 1, width = barwidth) +
  scale_color_manual(name = "variable", values = mycolors2)+
  theme(axis.text=element_text(size=10), axis.title=element_text(size= 10, face="bold")) +
  xlab("year") + ylab(paste0(VARkey1, "(GW)")) +
  ggtitle(paste0("Added capacity (iter = ", iteration, ")"))+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank()) +
  theme(aspect.ratio = .5) +
  scale_fill_manual(name = "all_te", values = mycolors)+
  facet_wrap(~tech, nrow = 3, scales = "free") 

ggsave(filename = paste0(getwd(),"/REMIND_debug_cap_iter", iteration, ".png"), width = 16, height =10, units = "in", dpi = 220)
}

