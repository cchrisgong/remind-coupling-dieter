mypath = "~/remind-coupling-dieter/dataprocessing/"
run_number = "capfac32_valid1"
mydatapath = paste0("~/remind-coupling-dieter/output/", run_number, "/")

# import library
source(paste0(mypath, "library_import.R"))
library(readr)
require(rmndt)

igdx("/opt/gams/gams30.2_linux_x64_64_sfx")

# remind output iteration gdx files
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
# sorted_files_DT <- paste0(mydatapath, "results_DIETER_i", seq(from = 2, to = length(files_DT), by = 1), ".gdx")

year_toplot_list <- c(2030,2050,2070) 
for(year_toplot in year_toplot_list){
# year_toplot = 2035
maxiter = 100

VARkey1 = "vm_cap"
REGIkey1 = "DEU"
TECHkeylst_peakGas = c("ngt")
TECHkeylst_nonPeakGas = c("ngccc","ngcc",  "gaschp") 
TECHkeylst_coal = c("coalchp", "igccc", "igcc", "pcc", "pco","pc")
TECHkeylst_solar = c("spv")
TECHkeylst_wind = c("wind")
TECHkeylst_hydro = c("hydro")
TECHkeylst_nuclear = c("tnrs")
TECHkeylst_biomass = c("biochp", "bioigccc", "bioigcc")
grade = "1"

TECHkeylst <- c(TECHkeylst_peakGas, TECHkeylst_nonPeakGas, TECHkeylst_coal, TECHkeylst_solar, TECHkeylst_wind, TECHkeylst_hydro,TECHkeylst_biomass, TECHkeylst_nuclear)

VARkey1_DT = "report4RM"
VARsubkey1_DT = "capacity"
TECHkeylst_DT = c("CCGT", "lig", "Solar", "Wind_on", "bio", "OCGT_eff", "ror", "nuc", "hc")
plot_DTte_names = c("combined cycle gas", "lignite", "solar", "wind", "biomass", "open cycle gas turbine", "hydro", "nuclear", "hard coal")
plot_RMte_names = c("combined cycle gas", "coal", "solar", "wind", "biomass", "open cycle gas turbine", "hydro", "nuclear")
mycolors <- c("combined cycle gas" = "#999959", "lignite" = "#0c0c0c", "coal" = "#0c0c0c", "solar" = "#ffcc00", "wind" = "#337fff", "biomass" = "#005900", "open cycle gas turbine" = "#e51900", "hydro" =  "#191999", "nuclear" =  "#ff33ff", "hard coal" = "#808080")

VARsubkey1_RM = "p32_peakDemand_relFac"
VARsubkey2_RM = "v32_seelDem"

get_DEMvariable_RM <- function(gdx){
  # gdx = sorted_files[[5]]
  # vrdata1 <- read.gdx(gdx, VARsubkey1_RM, factor = FALSE)  %>% 
  #   filter(tall == year_toplot) %>% 
  #   mutate(relFac = value)
  
  vrdata2 <- read.gdx(gdx, VARsubkey2_RM, factor = FALSE)  %>% 
    filter(tall == year_toplot) %>% 
    filter(all_regi == REGIkey1) %>%
    mutate(totDem = value) %>% 
    select(tall,totDem)
 
  # vrdata0 = list(vrdata1, vrdata2) %>%
  #   reduce(full_join) 
    
  vrdata <- vrdata2 %>% 
    mutate(value = totDem * 0.000155891 * 8760 * 1e3) 
  
  return(vrdata)
}

vr1_DEM <- lapply(sorted_files, get_DEMvariable_RM)

for(fname in files){
  idx <- as.numeric(str_extract(fname, "[0-9]+"))
  vr1_DEM[[idx]]$iter <- idx
  vr1_DEM[[idx]]$model <- "REMIND"
}

vr1_DEM <- rbindlist(vr1_DEM)

get_variable <- function(gdx){
  # gdx = sorted_files[[5]]
  vrdata <- read.gdx(gdx, VARkey1, factors = FALSE) %>% 
    filter(tall == year_toplot) %>%
    filter(all_regi == REGIkey1) %>%
    filter(all_te %in% TECHkeylst) %>%
    filter(rlf == grade) %>% 
    mutate(value = value*1e3) %>% 
    select(all_te, rlf, value) %>% 
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
    mutate(all_te = str_replace(all_te, TECHkeylst_wind[[1]], plot_RMte_names[[4]])) %>%
    mutate(all_te = str_replace(all_te, TECHkeylst_nuclear[[1]], plot_RMte_names[[8]])) %>%
    mutate(all_te = str_replace(all_te, TECHkeylst_hydro[[1]], plot_RMte_names[[7]])) %>%
    mutate(all_te = str_replace(all_te, TECHkeylst_peakGas[[1]], plot_RMte_names[[6]])) %>% 
    dplyr::group_by(all_te, rlf) %>%
    dplyr::summarise( value = sum(value) , .groups = 'keep' ) %>% 
    dplyr::ungroup(all_te, rlf)

    vrdata$all_te <- factor(vrdata$all_te, levels= c("solar", "wind", "combined cycle gas", "biomass", "open cycle gas turbine", "hydro", "nuclear", "coal"))
    return(vrdata)
}

get_variable_DT <- function(gdx){
  # gdx = sorted_files_DT[[1]]
  vrdata <- read.gdx(gdx, VARkey1_DT, factor = FALSE) %>% 
    filter(X..1 == year_toplot) %>% 
    dplyr::rename(all_te = X..3) %>% 
    filter(all_te %in% TECHkeylst_DT) %>%
    filter(X..4 == VARsubkey1_DT) %>%
    mutate(value = value/1e3) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_DT[[1]], plot_DTte_names[[1]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_DT[[2]], plot_DTte_names[[2]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_DT[[3]], plot_DTte_names[[3]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_DT[[4]], plot_DTte_names[[4]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_DT[[5]], plot_DTte_names[[5]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_DT[[6]], plot_DTte_names[[6]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_DT[[7]], plot_DTte_names[[7]])) %>%  
    mutate(all_te = str_replace(all_te, TECHkeylst_DT[[8]], plot_DTte_names[[8]])) %>% 
    mutate(all_te = str_replace(all_te, TECHkeylst_DT[[9]], plot_DTte_names[[9]])) 
  
  vrdata$all_te <- factor(vrdata$all_te, levels= c("solar", "wind", "combined cycle gas", "biomass", "open cycle gas turbine", "hydro", "nuclear", "lignite", "hard coal"))
  
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
# idx_DT <- 1:(length(files_DT)-1)
for(id in idx_DT){
  vrN_DT[[id]]$iter <- id*5
  # vrN_DT[[id]]$iter <- id+1
  vrN_DT[[id]]$model <- "DIETER"
}

vrN <- rbindlist(vrN)
vrN_DT <- rbindlist(vrN_DT)

# vrN <- vrN %>% 
#   filter(!(all_te %in% c("solar", "wind", "open cycle gas turbine", "biomass")))

p2<-ggplot() +
  geom_area(data = vrN_DT, aes(x = iter, y = value, fill = all_te), size = 1.2, alpha = 0.5) +
  geom_line(data = vr1_DEM, aes(x = iter, y = value), size = 1.2, alpha = 1,linetype="dotted") +
  scale_fill_manual(name = "Technology", values = mycolors)+
  scale_color_manual(values=c("black"))+
  theme(axis.text=element_text(size=14), axis.title=element_text(size= 14,face="bold")) +
  xlab("iteration") + ylab(paste0(VARkey1, "(GW)")) +
  coord_cartesian(xlim = c(0, max(vr1_DEM$iter)))+
  ggtitle(paste0("DIETER ", year_toplot))+
  theme(plot.title = element_text(size = 16, face = "bold"))+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text=element_text(size=14)) +
  theme(aspect.ratio = .5)


p1<-ggplot() +
  geom_area(data = vrN, aes(x = iter, y = value, fill = all_te), size = 1.2, alpha = 0.5) +
  geom_line(data = vr1_DEM, aes(x = iter, y = value), size = 1.2, alpha = 0.5,linetype="dotted") +
  scale_fill_manual(name = "Technology", values = mycolors)+
  scale_color_manual(values=c("black"))+
  theme(axis.text=element_text(size=14), axis.title=element_text(size= 14,face="bold")) + 
  xlab("iteration") + ylab(paste0(VARkey1, "(GW)")) +
  ggtitle(paste0("REMIND ", year_toplot))+
  coord_cartesian(xlim = c(0, max(vr1_DEM$iter)))+
  theme(plot.title = element_text(size = 16, face = "bold"))+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text=element_text(size=14)) +
  theme(aspect.ratio = .5)

                    
library(grid)
grid.newpage()
p <- arrangeGrob(rbind(ggplotGrob(p1), ggplotGrob(p2)))
grid.draw(p)

ggsave(filename = paste0(mypath, "iter_xN_CAP_capfac", run_number, "_", year_toplot, ".png"),  p,  width = 12, height =16, units = "in", dpi = 120)
# ggsave(filename = paste0(mypath, "iter_xN_CAP_capfac", run_number, "_uncoupl", year_toplot, ".png"),  p,  width = 12, height =16, units = "in", dpi = 120)
}
