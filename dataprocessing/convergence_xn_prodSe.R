mypath = "~/remind-coupling-dieter/dataprocessing/"
run_number = "capfac32_valid1"
mydatapath = paste0("~/remind-coupling-dieter/output/", run_number, "/")
# import library
source(paste0(mypath, "library_import.R"))
library(readr)
require(rmndt)

igdx("/opt/gams/gams30.2_linux_x64_64_sfx")

#remind output iteration gdx files
files <- list.files(mydatapath, pattern="fulldata_[0-9]+\\.gdx")
sorted_files <- paste0(mydatapath, "fulldata_", 1:length(files), ".gdx")

# files_full <- list.files(mydatapath, pattern="fulldata_[0-9]+\\.gdx")
# sorted_files_full <- paste0(mydatapath, "fulldata_", 1:length(files_full), ".gdx")
# files_nonopt <- list.files(mydatapath, pattern="non_optimal_[0-9]+\\.gdx")
# sorted_files_nonopt <- paste0(mydatapath, "non_optimal_", (length(files_full)+1):(length(files_full)+length(files_nonopt)), ".gdx")
# sorted_files = c(sorted_files_full, sorted_files_nonopt)
# files = c(files_full, files_nonopt)

#dieter output iteration gdx files
files_DT <- list.files(mydatapath, pattern="results_DIETER_i[0-9]+\\.gdx")
sorted_files_DT <- paste0(mydatapath, "results_DIETER_i", seq(from = 5, to = length(files_DT)*5, by = 5), ".gdx")

year_toplot_list <- c(2015, 2025, 2035, 2050) 
for(year_toplot in year_toplot_list){
  
# year_toplot = 2035
iter_toplot = 30
maxiter = 100
sm_TWa_2_MWh = 8760000000

print(paste0("Year: ", year_toplot))
print(paste0("Iteration: ", iter_toplot))

VARkey1 = "vm_prodSe"
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

VARkey1_DT = "report4RM"
VARsubkey1_DT = "generation"
TECHkeylst_DT = c("CCGT", "lig", "Solar", "Wind_on", "bio", "OCGT_eff", "ror", "nuc", "hc")
plot_DTte_names = c("combined cycle gas", "lignite", "solar", "wind", "biomass", "open cycle gas turbine", "hydro", "nuclear", "hard coal")
plot_RMte_names = c("combined cycle gas", "coal", "solar", "wind", "biomass", "open cycle gas turbine", "hydro", "nuclear")
mycolors <- c("combined cycle gas" = "#999959", "lignite" = "#0c0c0c", "coal" = "#0c0c0c", "solar" = "#ffcc00", "wind" = "#337fff", "biomass" = "#005900", "open cycle gas turbine" = "#e51900", "hydro" =  "#191999", "nuclear" =  "#ff33ff", "hard coal" = "#808080")
# Coal        Oil        Gas    Biomass    Nuclear      Hydro       Wind      Solar Geothermal 
# "#0c0c0c"  "#cc7500"  "#999959"  "#005900"  "#ff33ff"  "#191999"  "#337fff"  "#ffcc00"  "#e51900"

get_variable <- function(gdx){
  # gdx = sorted_files[[30]]
  vrdata <- read.gdx(gdx, VARkey1, factors = FALSE) %>% 
    filter(tall == year_toplot) %>%
    filter(all_regi == REGIkey1) %>%
    filter(all_te %in% TECHkeylst) %>% 
    filter(all_enty.1 == ProdKEY)  %>% 
    mutate(value = value*sm_TWa_2_MWh/1e6) %>% 
    select(all_te, value) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_biomass[[1]], plot_RMte_names[[5]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_biomass[[2]], plot_RMte_names[[5]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_biomass[[3]], plot_RMte_names[[5]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_nonPeakGas[[1]], plot_RMte_names[[1]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_nonPeakGas[[2]], plot_RMte_names[[1]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_nonPeakGas[[3]], plot_RMte_names[[1]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_coal[[1]], plot_RMte_names[[2]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_coal[[2]], plot_RMte_names[[2]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_coal[[3]], plot_RMte_names[[2]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_coal[[4]], plot_RMte_names[[2]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_coal[[5]], plot_RMte_names[[2]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_coal[[6]], plot_RMte_names[[2]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_solar[[1]], plot_RMte_names[[3]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_nuclear[[1]], plot_RMte_names[[8]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_wind[[1]], plot_RMte_names[[4]])) %>%
    mutate(all_te = str_replace(all_te, TECHkeylst_hydro[[1]], plot_RMte_names[[7]])) %>%
    mutate(all_te = str_replace(all_te, TECHkeylst_peakGas[[1]], plot_RMte_names[[6]])) %>% 
    dplyr::group_by(all_te) %>%
    dplyr::summarise( value = sum(value) , .groups = 'keep' ) %>% 
    dplyr::ungroup(all_te)

  vrdata$all_te <- factor(vrdata$all_te, levels= c("solar", "wind", "hydro", "combined cycle gas", "biomass", "open cycle gas turbine",  "nuclear", "coal"))
  
    return(vrdata)
}

get_variable_DT <- function(gdx){
  # gdx = sorted_files_DT[[6]]
  vrdata <- gdx::readGDX(gdx, VARkey1_DT, squeeze = FALSE)
  df.vrdata0 <- as.quitte(vrdata)
  vrdata <- df.vrdata0 %>% 
    filter(period == year_toplot) %>% 
    dplyr::rename(all_te = X.3) %>% 
    filter(all_te %in% TECHkeylst_DT) %>% 
    dplyr::rename(varname = X.4) %>% 
    filter(varname == VARsubkey1_DT) %>% 
    mutate(value = value/1e6) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_DT[[1]], plot_DTte_names[[1]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_DT[[2]], plot_DTte_names[[2]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_DT[[3]], plot_DTte_names[[3]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_DT[[4]], plot_DTte_names[[4]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_DT[[5]], plot_DTte_names[[5]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_DT[[6]], plot_DTte_names[[6]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_DT[[7]], plot_DTte_names[[7]])) %>%
    mutate(all_te = str_replace(all_te, TECHkeylst_DT[[8]], plot_DTte_names[[8]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_DT[[9]], plot_DTte_names[[9]])) %>% 
    dplyr::group_by(all_te) %>%
    dplyr::summarise( value = sum(value) , .groups = 'keep' ) %>% 
    dplyr::ungroup(all_te)
  
  vrdata$all_te <- factor(vrdata$all_te, levels= c("solar", "wind", "hydro", "combined cycle gas", "biomass", "open cycle gas turbine", "nuclear","lignite",  "hard coal"))
  
    return(vrdata)
}

vrN <- lapply(sorted_files, get_variable)
# print(vrN[[1]])

vrN_DT <- lapply(sorted_files_DT, get_variable_DT)

for(fname in files){
  idx <- as.numeric(str_extract(fname, "[0-9]+"))
  vrN[[idx]]$iter <- idx
  vrN[[idx]]$model <- "REMIND"
}

idx_DT <- 1:length(files_DT)
for(id in idx_DT){
  vrN_DT[[id]]$iter <- id*5
  vrN_DT[[id]]$model <- "DIETER"
}

vrN <- rbindlist(vrN)
vrN_DT <- rbindlist(vrN_DT)
  
p1<-ggplot() +
  geom_area(data = vrN, aes(x = iter, y = value, fill = all_te), size = 1.2, alpha = 0.5, stat = "identity") +
  scale_fill_manual(name = "Technology", values = mycolors)+
  theme(axis.text=element_text(size=10), axis.title=element_text(size= 10,face="bold")) +
  xlab("iteration") + ylab(paste0(VARkey1, "(TWh)")) +
  ggtitle(paste0("REMIND", year_toplot))+
  coord_cartesian(ylim = c(0,1.5e3), xlim = c(0, max(vrN$iter)))+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank()) +
  theme(aspect.ratio = .5)


p2<-ggplot() +
  geom_area(data = vrN_DT, aes(x = iter, y = value, fill = all_te), size = 1.2, alpha = 0.5) +
  scale_fill_manual(name = "Technology", values = mycolors)+
  theme(axis.text=element_text(size=10), axis.title=element_text(size= 10,face="bold")) +
  xlab("iteration") + ylab(paste0(VARsubkey1_DT, "(TWh)")) +
  coord_cartesian(ylim = c(0,1.5e3), xlim = c(0, max(vrN$iter)))+
  ggtitle(paste0("DIETER", year_toplot))+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank()) +
  theme(aspect.ratio = .5)

library(grid)
grid.newpage()
p <- arrangeGrob(rbind(ggplotGrob(p1), ggplotGrob(p2)))
grid.draw(p)

ggsave(filename = paste0(mypath, "iter_xN_GEN_capFac", run_number, "_", year_toplot, ".png"),  p,  width = 12, height =16, units = "in", dpi = 120)


}

