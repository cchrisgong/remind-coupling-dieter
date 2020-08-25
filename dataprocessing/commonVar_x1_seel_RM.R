#for shared variable such as peak demand (one iteration series)

mypath = "~/remind/dataprocessing/"
mydatapath = "~/remind/output/capfac18/"
mydatapath2 = "~/remind/output/capfac14_nocoupl/"

# import library
source(paste0(mypath, "library_import.R"))
library(readr)
require(ggplot2)
require(lusweave)
require(rmndt)
igdx("/opt/gams/gams30.2_linux_x64_64_sfx")


#dieter output iteration gdx files
#remind output iteration gdx files
files <- list.files(mydatapath, pattern="fulldata_[0-9]+\\.gdx")
sorted_files <- paste0(mydatapath, "fulldata_", 1:length(files), ".gdx")

# files_full <- list.files(mydatapath, pattern="fulldata_[0-9]+\\.gdx")
# sorted_files_full <- paste0(mydatapath, "fulldata_", 1:length(files_full), ".gdx")
# files_nonopt <- list.files(mydatapath, pattern="non_optimal_[0-9]+\\.gdx")
# sorted_files_nonopt <- paste0(mydatapath, "non_optimal_", (length(files_full)+1):(length(files_full)+length(files_nonopt)), ".gdx")
# sorted_files = c(sorted_files_full, sorted_files_nonopt)
# files = c(files_full, files_nonopt)

files2 <- list.files(mydatapath2, pattern="fulldata_[0-9]+\\.gdx")
sorted_files2 <- paste0(mydatapath2, "fulldata_", 1:length(files2), ".gdx")

# year_toplot = 2050
maxiter = 100

BUDGETkey1 = "qm_budget"
VARkey1 = "q32_balSe"
# VARkey1 = "v32_seelDem"
REGIkey1 = "DEU"
sm_TWa_2_MWh = 8760000000

CapConstraintKey = "q32_peakDemand_DT"
  
get_CAPCONvariable <- function(gdx){
  # gdx = sorted_files[[15]]
  budgetdata <- read.gdx(gdx, BUDGETkey1,field="m") %>% 
    # filter(ttot == year_toplot) %>%
    filter(all_regi == REGIkey1) %>% 
    mutate(m = -m) %>% 
    dplyr::rename(budget = m)  %>% 
    select(ttot, budget)
  
  capcondata <- read.gdx(gdx, CapConstraintKey,field="m") %>% 
    # filter(tall == year_toplot) %>%
    mutate(m = -m) %>% 
    dplyr::rename(ttot = tall) %>% 
    dplyr::rename(capcon = m)
  
  # transform from tr$2005/TW to $2015/kW
  vrdata = list(capcondata, budgetdata) %>%
    reduce(full_join) %>%
    mutate(capcon= capcon/ budget * 1e12 / 1e9 * 1.2) 
    
  return(vrdata)
}

vr1_capcon <- lapply(sorted_files, get_CAPCONvariable)
for(fname in files){
  idx <- as.numeric(str_extract(fname, "[0-9]+"))
  vr1_capcon[[idx]]$iter <- idx
  vr1_capcon[[idx]]$model <- "cap.constraint marginal"
}
vr1_capcon <- rbindlist(vr1_capcon)

get_PRICEvariable <- function(gdx){
  # gdx = sorted_files[[1]]
  budgetdata <- read.gdx(gdx, BUDGETkey1,field="m") %>% 
    # filter(ttot == year_toplot) %>%
    filter(all_regi == REGIkey1) %>% 
    mutate(m = -m) %>% 
    dplyr::rename(budget = m)
  
  vrdata0 <- read.gdx(gdx, VARkey1,field="m") %>% 
    # filter(ttot == year_toplot) %>%
    filter(all_regi == REGIkey1) 
  
  vrdata = list(vrdata0, budgetdata) %>%
    reduce(full_join) %>%
    mutate(m = -m/ budget * 1e12 / sm_TWa_2_MWh * 1.2) %>% 
    dplyr::rename(value = m)
    
  return(vrdata)
}

vr1 <- lapply(sorted_files, get_PRICEvariable)
vr1_2 <- lapply(sorted_files2, get_PRICEvariable)

# print(vr1[[1]])

for(fname in files){
  idx <- as.numeric(str_extract(fname, "[0-9]+"))
  vr1[[idx]]$iter <- idx
  vr1[[idx]]$model <- "coupled"
}

for(fname in files2){
  idx <- as.numeric(str_extract(fname, "[0-9]+"))
  vr1_2[[idx]]$iter <- idx
  vr1_2[[idx]]$model <- "uncoupled"
}

vr1 <- rbindlist(vr1)
vr1_2 <- rbindlist(vr1_2)

secAxisScale = 1/8.76

p1<-ggplot() +
  geom_line(data = vr1, aes(x = iter, y = value, color = model), size = 1.2, alpha = 0.5) +
  geom_line(data = vr1_capcon, aes(x = iter, y = capcon*secAxisScale, color = model), size = 1.2, alpha = 0.5) +
  # geom_line(data = vr1_2, aes(x = iter, y = value, color = model), size = 1.2, alpha = 0.5) +
  scale_y_continuous(sec.axis = sec_axis(~./secAxisScale, name = paste0(CapConstraintKey, "(USD/kW)")))+
  theme(axis.text=element_text(size=10), axis.title=element_text(size= 10,face="bold")) +
  xlab("iteration") + ylab(paste0(VARkey1, "(USD/MWh)"))  +
  coord_cartesian(ylim = c(-300,300))+
  facet_wrap(~ttot, nrow = 3)
  
ggsave(filename = paste0(mypath, "iter_seelprice_capfac", str_sub(mydatapath,-3,-2), "_RM.png"),  width = 28, height =15, units = "in", dpi = 120)

