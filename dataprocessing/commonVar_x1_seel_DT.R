#for shared variable such as peak demand (one iteration series)

mypath = "~/remind/dataprocessing/"
mydatapath = "~/remind/output/capfac18/"

# import library
source(paste0(mypath, "library_import.R"))
library(readr)
require(ggplot2)
require(lusweave)
require(rmndt)
igdx("/opt/gams/gams30.2_linux_x64_64_sfx")

#dieter output iteration gdx files
files_DT <- list.files(mydatapath, pattern="report_DIETER_i[0-9]+\\.gdx")
sorted_files_DT <- paste0(mydatapath, "report_DIETER_i", seq(from = 5, to = length(files_DT)*5, by = 5), ".gdx")

# year_toplot_list <- c(2025, 2030, 2040, 2050, 2070) 
# for(year_toplot in year_toplot_list){

# year_toplot = 2030
print(paste0("Year: ", year_toplot))

VARkey1_DT = "report"
VARsubkey1_DT = "load-weighted price for fixed demand"

get_variable_DT <- function(gdx){
  # gdx = sorted_files_DT[[1]]
  vrdata <- read.gdx(gdx, VARkey1_DT) %>% 
    # filter(X..2 == year_toplot) %>% 
    filter(X..4 == VARsubkey1_DT) %>% 
    mutate(value = value) 
  return(vrdata)
}

vr1_DT <- lapply(sorted_files_DT, get_variable_DT)
# print(vr1[[1]])

idx_DT <- 1:length(files_DT)

for(id in idx_DT){
  vr1_DT[[id]]$iter <- id * 5
  vr1_DT[[id]]$model <- "coupled"
}


vr1_DT <- rbindlist(vr1_DT)

p1<-ggplot() +
  geom_line(data = vr1_DT, aes(x = iter, y = value, color = model), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size= 10,face="bold")) +
  xlab("iteration") + ylab(paste0(VARsubkey1_DT, "(USD/MWh)")) +
  facet_wrap(~X..2, nrow = 3)
# +
  # coord_cartesian(ylim = c(0,80)) 

ggsave(filename = paste0(mypath, "iter_seelprice_capfac", str_sub(mydatapath,-3,-2), "_DT.png"),  width = 8, height =6, units = "in", dpi = 120)

# }