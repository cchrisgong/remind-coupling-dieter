mypath = "~/remind-coupling-dieter/dataprocessing/"
myDIETERPLOT_path = "~/remind-coupling-dieter/dataprocessing/DIETER_plots/"
run_number = "hydro744"
mydatapath = paste0("~/remind-coupling-dieter/output/", run_number,"/")

mifpath = paste0(mydatapath,"REMIND_generic_xx_ref_FEmed.mif")

# import library
source(paste0(mypath, "library_import.R"))
source(paste0(myDIETERPLOT_path, "GDXtoQuitte.R"))
library(readr)
require(ggplot2)
require(lusweave)
require(rmndt)
igdx("/opt/gams/gams30.2_linux_x64_64_sfx")

miniter = 1

files_DT_rep <- list.files(mydatapath, pattern="report_DIETER_i[0-9]+\\.gdx") 
# for(fname in files_DT_rep){
#   gdxToQuitte_annual(mydatapath, fname, run_number)
# }

#remind output iteration gdx files
filenames <- list.files(mydatapath, pattern="fulldata_[0-9]+\\.gdx")
sorted_files <- paste0(mydatapath, "fulldata_", 1:(length(filenames)-1), ".gdx")
files <- paste0("fulldata_", 1:(length(filenames)-1), ".gdx")

id <- NULL
for(fname in files_DT_rep){
  idx <- as.numeric(str_extract(fname, "[0-9]+"))
  id = c(id, idx)
}

id = sort(id)

if (length(files_DT_rep) != 0) {
  sorted_annual_report_DT <- paste0(myDIETERPLOT_path, run_number, "_i", id, "_annualreport.csv")
}

iter_toplot = 1:length(sorted_files)
iter_toplot_DT = 1:(length(sorted_annual_report_DT))

readDampen <- function(gdx, key){
  # gdx = sorted_files[[22]]
    vrdata <- read.gdx(gdx, key, squeeze = F) %>%
    filter(all_regi == REGIkey1)

  return(vrdata)
}

vr1_dampen_coalgas <- lapply(sorted_files, readDampen, key = "v21_prodse_dampen")
vr1_dampen_h2 <- lapply(sorted_files, readDampen, key = "v21_greenh2dem_dampen")

for(fname in files){
  idx <- as.numeric(str_extract(fname, "[0-9]+")) 
  vr1_dampen_coalgas[[idx]]$iter <- idx
  vr1_dampen_coalgas[[idx]]$label <- "coal and gas prodse_dampen"
  vr1_dampen_h2[[idx]]$iter <- idx 
  vr1_dampen_h2[[idx]]$label <- "green H2 dampen"
}
vr1_dampen_coalgas <- rbindlist(vr1_dampen_coalgas)
vr1_dampen_h2 <- rbindlist(vr1_dampen_h2)

p<-ggplot() +
  geom_line(data = vr1_dampen_coalgas, aes(x = iter, y = value, color = label), size = 1.2, alpha = 0.5) +
  geom_line(data = vr1_dampen_h2, aes(x = iter, y = value, color = label), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold")) +
  xlab("iteration") + ylab(paste0("dampening value")) +
  # scale_color_manual(name = "tech", values = mycolors)+
  # coord_cartesian(ylim = c(-70,150))+
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
  facet_wrap(~ttot, nrow = 3, scales = "free_y")


ggsave(filename = paste0(mypath, run_number, "_iter_dampen_terms.png"), p, width = 28, height =15, units = "in", dpi = 120)


#biomass dampen
readBioDampen <- function(gdx, key){
  # gdx = sorted_files[[22]]
  vrdata <- read.gdx(gdx, key, field = "l",squeeze = F) %>%
    filter(all_regi == REGIkey1) %>% 
    filter(all_enty == "pebiolc") %>% 
    filter(rlf == "1") %>% 
    filter(ttot <2080) %>% 
    filter(ttot > 2030)
  
  return(vrdata)
}

#biomass dampen
readBioDampen2 <- function(gdx, key){
  # gdx = sorted_files[[22]]
  vrdata <- read.gdx(gdx, key, squeeze = F) %>%
    filter(all_regi == REGIkey1) 
  
  return(vrdata)
}

vr1_dampen_biomass <- lapply(sorted_files, readBioDampen, key = "vm_fuExtr")
vr1_dampen_biomrkup <- lapply(sorted_files, readBioDampen2, key = "v30_multcost")
vr1_dampen_biomrkupDT <- lapply(sorted_files, readBioDampen2, key = "v30_multcostDIETER")
vr1_dampen_coalgasmrkupDT <- lapply(sorted_files, readBioDampen2, key = "v_costFuExdampen")


for(fname in files){
  idx <- as.numeric(str_extract(fname, "[0-9]+")) 
  vr1_dampen_biomass[[idx]]$iter <- idx
  vr1_dampen_biomass[[idx]]$label <- "vm_fuExtr"
  vr1_dampen_biomrkup[[idx]]$iter <- idx
  vr1_dampen_biomrkup[[idx]]$label <- "v30_multcost"
  vr1_dampen_biomrkupDT[[idx]]$iter <- idx
  vr1_dampen_biomrkupDT[[idx]]$label <- "v30_multcostDIETER"
  vr1_dampen_coalgasmrkupDT[[idx]]$iter <- idx
  vr1_dampen_coalgasmrkupDT[[idx]]$label <- "v_costFuExdampen"
}
vr1_dampen_biomass <- rbindlist(vr1_dampen_biomass)
vr1_dampen_biomrkup <- rbindlist(vr1_dampen_biomrkup)
vr1_dampen_biomrkupDT <- rbindlist(vr1_dampen_biomrkupDT)
vr1_dampen_coalgasmrkupDT <- rbindlist(vr1_dampen_coalgasmrkupDT)
#
vr1_dampen_coalgasmrkupDT <- vr1_dampen_coalgasmrkupDT%>% 
 filter(all_enty %in% c("pegas","pecoal"))

p<-ggplot() +
  geom_line(data = vr1_dampen_biomass, aes(x = iter, y = value, color = label), size = 1.2, alpha = 0.5) + 
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold")) + 
  xlab("iteration") + ylab(paste0("dampening value")) + 
  # scale_color_manual(name = "tech", values = mycolors) + 
  # coord_cartesian(ylim = c(-70,150)) + 
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) + 
  facet_wrap(~ttot, nrow = 3, scales = "free_y")


ggsave(filename = paste0(mypath, run_number, "_dampen_bio.png"), p, width = 28, height =15, units = "in", dpi = 120)

p1<-ggplot() +
  geom_line(data = vr1_dampen_biomrkup, aes(x = iter, y = value, color = label), size = 1.2, alpha = 0.5) +
  geom_line(data = vr1_dampen_biomrkupDT, aes(x = iter, y = value, color = label), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold")) + 
  xlab("iteration") + ylab(paste0("dampening value")) + 
  # scale_color_manual(name = "tech", values = mycolors) + 
  # coord_cartesian(ylim = c(-70,150)) + 
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) + 
  facet_wrap(~ttot, nrow = 3, scales = "free_y")

ggsave(filename = paste0(mypath, run_number, "_dampen_bio.png"), p1, width = 28, height =15, units = "in", dpi = 120)

p2<-ggplot() +
  geom_line(data = vr1_dampen_coalgasmrkupDT, aes(x = iter, y = value, color = all_enty), size = 1.2, alpha = 0.5) +
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold")) + 
  xlab("iteration") + ylab(paste0("v_costFuExdampen")) + 
  # scale_color_manual(name = "tech", values = mycolors) + 
  # coord_cartesian(ylim = c(-70,150)) + 
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) + 
  facet_wrap(~ttot, nrow = 3, scales = "free_y")

ggsave(filename = paste0(mypath, run_number, "_dampen_coalgas.png"), p2, width = 28, height =15, units = "in", dpi = 120)
