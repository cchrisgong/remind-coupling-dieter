mypath = "~/remind-coupling-dieter/dataprocessing/"
run_number = "mrkup111"
mydatapath = paste0("~/remind-coupling-dieter/output/", run_number, "/")
# import library
source(paste0(mypath, "library_import.R"))
library(readr)
require(rmndt)

igdx("/opt/gams/gams30.2_linux_x64_64_sfx")


# remind output iteration gdx files
filenames0 <- list.files(mydatapath, pattern="fulldata_[0-9]+\\.gdx")
sorted_files0 <- paste0(mydatapath, "fulldata_", 1:length(filenames0), ".gdx")
files0 <- paste0("fulldata_", 1:length(filenames0), ".gdx")
maxiter = length(filenames0)

sorted_files <- sorted_files0[1:maxiter]
files <- files0[1:maxiter]


#dieter output iteration gdx files
files_DT <- list.files(mydatapath, pattern="results_DIETER_i[0-9]+\\.gdx")
# sorted_files_DT <- paste0(mydatapath, "results_DIETER_i", seq(from = 5, to = length(files_DT)*5, by = 5), ".gdx")
sorted_files_DT <- paste0(mydatapath, "results_DIETER_i", seq(from = 2, to = length(files_DT)+1, by = 1), ".gdx")

year_toplot_list <- c(2020,2030,2040,2050,2060,2070,2080)
# year_toplot_list <- c(2025,2035)
  
# year_toplot = 2035
iter_toplot = 30
sm_TWa_2_MWh = 8760000000

print(paste0("Year: ", year_toplot_list))
print(paste0("Iteration: ", iter_toplot))

VARkey1 = "vm_prodSe"
VARkey2 = "vm_usableSeTe"
REGIkey1 = "DEU"
ProdKEY = "seel"
TECHkeylst_peakGas = c("ngt")
TECHkeylst_nonPeakGas = c("ngccc","ngcc",  "gaschp") 
TECHkeylst_coal = c("coalchp", "igccc", "igcc", "pcc", "pco","pc")
TECHkeylst_solar = c("spv")
TECHkeylst_wind = c("wind")
TECHkeylst_hydro = c("hydro")
TECHkeylst_nuclear = c("tnrs")
TECHkeylst_biomass = c("biochp", "bioigccc", "bioigcc")

TECHkeylst <- c(TECHkeylst_peakGas, TECHkeylst_nonPeakGas, TECHkeylst_coal, TECHkeylst_solar, TECHkeylst_wind, TECHkeylst_hydro, TECHkeylst_biomass, TECHkeylst_nuclear)


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


remind.vre.mapping <- c(hydro = "Hydro",
                        wind = "Wind",
                        spv = "Solar",
                        NULL)

table_ordered_name = c("Coal (Lig + HC)", "Lignite", "Hard coal","CCGT", "Solar", "Wind", "Biomass", "OCGT", "Hydro", "Nuclear")

remind.tech.mapping <- c(remind.nonvre.mapping, remind.vre.mapping)

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


VARkey1_DT = "p32_report4RM"
VARsubkey1_DT = c("total generation", "usable generation")
TECHkeylst_DT = c("CCGT", "lig", "Solar", "Wind_on", "bio", "OCGT_eff", "ror", "nuc", "hc")
plot_DTte_names = c("combined cycle gas", "lignite", "solar", "wind", "biomass", "open cycle gas turbine", "hydro", "nuclear", "hard coal")

color.mapping <- c("CCGT" = "#999959", "Lignite" = "#0c0c0c", "Coal (Lig + HC)" = "#0c0c0c",
                   "Solar" = "#ffcc00", "Wind" = "#337fff", "Biomass" = "#005900",
                   "OCGT" = "#e51900", "Hydro" = "#191999", "Nuclear" = "#ff33ff",
                   "Hard coal" = "#808080",  "total generation w/ curtailment" = "#0c0c0c")

# Coal        Oil        Gas    Biomass    Nuclear      Hydro       Wind      Solar Geothermal 
# "#0c0c0c"  "#cc7500"  "#999959"  "#005900"  "#ff33ff"  "#191999"  "#337fff"  "#ffcc00"  "#e51900"

get_variable <- function(gdx){
  # gdx = sorted_files[[30]]
  vrdata_disp<- read.gdx(gdx, VARkey1, factors = FALSE, squeeze = FALSE) %>% 
    filter(tall %in% year_toplot_list) %>%
    filter(all_regi == REGIkey1) %>%
    filter(all_te %in% names(remind.nonvre.mapping)) %>% 
    filter(all_enty.1 == ProdKEY)  %>% 
    select(period = tall, all_te, value) %>% 
    revalue.levels(all_te = remind.nonvre.mapping) %>%
    mutate(value = value*sm_TWa_2_MWh/1e6) %>% 
    dplyr::group_by(period, all_te) %>%
    dplyr::summarise( value = sum(value) , .groups = 'keep' ) %>% 
    dplyr::ungroup(period, all_te) 
  
  vrdata_vre<- read.gdx(gdx, VARkey2, factors = FALSE, squeeze = FALSE) %>% 
    filter(ttot %in% year_toplot_list) %>%
    filter(all_regi == REGIkey1) %>%
    filter(all_te %in% names(remind.vre.mapping)) %>% 
    filter(entySe == ProdKEY)  %>% 
    select(period = ttot, all_te, value) %>% 
    revalue.levels(all_te = remind.vre.mapping) %>%
    mutate(value = value*sm_TWa_2_MWh/1e6) %>% 
    dplyr::group_by(period, all_te) %>%
    dplyr::summarise( value = sum(value) , .groups = 'keep' ) %>% 
    dplyr::ungroup(period, all_te)
  
  vrdata <- list(vrdata_disp, vrdata_vre) %>% 
    reduce(full_join) %>% 
    mutate(all_te = factor(all_te, levels=rev(unique(remind.tech.mapping)))) 
    
    return(vrdata)
}

RM_GEN_wCurt <- function(gdx){
  # gdx = sorted_files[[30]]
  vrdata<- read.gdx(gdx, VARkey1, factors = FALSE, squeeze = FALSE) %>% 
    filter(tall %in% year_toplot_list) %>%
    filter(all_regi == REGIkey1) %>%
    filter(all_te %in% names(remind.tech.mapping)) %>% 
    filter(all_enty.1 == ProdKEY)  %>% 
    select(period = tall, value) %>% 
    dplyr::group_by(period) %>%
    dplyr::summarise( value = sum(value) , .groups = 'keep' ) %>% 
    dplyr::ungroup(period) %>% 
    mutate(value = value * sm_TWa_2_MWh/1e6) 
  
  return(vrdata)
}

get_variable_DT <- function(gdx){
  # gdx = sorted_files_DT[[6]]
  
  vrdata <- read.gdx(gdx, VARkey1_DT, factors = FALSE, squeeze = FALSE) %>% 
  select(period = X..1, regi=X..2, all_te=X..3, variable = X..4, value) %>% 
    filter(period %in% year_toplot_list) %>%
    filter(all_te %in% TECHkeylst_DT) %>% 
    filter(variable %in% VARsubkey1_DT) %>% 
    mutate(value = value/1e6) %>% 
    filter(all_te %in% names(dieter.tech.mapping)) %>% 
    revalue.levels(all_te = dieter.tech.mapping) %>%
    mutate(all_te = factor(all_te, levels=rev(unique(dieter.tech.mapping)))) 
  
    return(vrdata)
}

vrN <- lapply(sorted_files, get_variable)
vr1_GEN_wCurt <- lapply(sorted_files, RM_GEN_wCurt)
vrN_DT <- lapply(sorted_files_DT, get_variable_DT)

for(fname in files){
  idx <- as.numeric(str_extract(fname, "[0-9]+"))
  vrN[[idx]]$iter <- idx
  vr1_GEN_wCurt[[idx]]$iter <- idx
}

idx_DT <- 1:length(files_DT)
for(id in idx_DT){
  # vrN_DT[[id]]$iter <- id*5
  vrN_DT[[id]]$iter <- id + 1
}

vrN <- rbindlist(vrN)
vrN_DT <- rbindlist(vrN_DT)
vr1_GEN_wCurt<- rbindlist(vr1_GEN_wCurt)

vr1_GEN_wCurt$tech <- "total generation w/ curtailment"

vr1_DT_GEN_wCurt <- vrN_DT %>% 
  filter(variable == "total generation") %>% 
  select(period, iter, value) %>% 
  dplyr::group_by(period, iter) %>%
  dplyr::summarise( value = sum(value) , .groups = 'keep' ) %>% 
  dplyr::ungroup(period, iter)

vr1_DT_GEN_wCurt$tech <- "total generation w/ curtailment"

for(year_toplot in year_toplot_list){
  
p1<-ggplot() +
  geom_area(data = vrN %>% filter(period == year_toplot), aes(x = iter, y = value, fill = all_te), size = 1.2, alpha = 0.5, stat = "identity") +
  geom_line(data = vr1_GEN_wCurt%>% filter(period == year_toplot), aes(x = iter, y = value, color = tech), size = 1.2, alpha = 1,linetype="dotted") +
  scale_fill_manual(name = "Technology", values = color.mapping)+
  theme(axis.text=element_text(size=10), axis.title=element_text(size= 10,face="bold")) +
  xlab("iteration") + ylab(paste0("Usable generation (TWh)")) +
  ggtitle(paste0("REMIND", year_toplot))+
  coord_cartesian(ylim = c(0,1.8e3), xlim = c(0, max(vrN$iter)))+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank()) +
  theme(aspect.ratio = .5)

p2<-ggplot() +
  geom_area(data = vrN_DT%>% filter(period == year_toplot, variable == "usable generation"), aes(x = iter, y = value, fill = all_te), size = 1.2, alpha = 0.5) +
  geom_line(data = vr1_DT_GEN_wCurt%>% filter(period == year_toplot), aes(x = iter, y = value, color = tech), size = 1.2, alpha = 1,linetype="dotted") +
  scale_fill_manual(name = "Technology", values = color.mapping)+
  theme(axis.text=element_text(size=10), axis.title=element_text(size= 10,face="bold")) +
  xlab("iteration") + ylab(paste0(VARsubkey1_DT, "(TWh)")) +
  coord_cartesian(ylim = c(0,1.8e3), xlim = c(0, max(vrN$iter)))+
  ggtitle(paste0("DIETER", year_toplot))+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank()) +
  theme(aspect.ratio = .5)

library(grid)
grid.newpage()
p <- arrangeGrob(rbind(ggplotGrob(p1), ggplotGrob(p2)))
grid.draw(p)

ggsave(filename = paste0(mypath, run_number, "_GEN_", year_toplot, ".png"),  p,  width = 12, height =16, units = "in", dpi = 120)

}

