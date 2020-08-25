# import library
source("library_import.R")
source("GDXtoQuitte.R")
library(readr)
# specify output file
testfile = "~/DIETER/myFirstDIETER/DIETER/results_mSS_exoWS_PtG-ONLY.gdx"
gdxToQuitte(testfile)
# annual_reportCSV = read.csv("/home/chengong/DIETER/myFirstDIETER/dataprocessing/annual_report.csv", sep = ',', header = T, stringsAsFactors = F)
annual_reportCSV = read.csv("annual_report.csv", sep = ',', header = T, stringsAsFactors = F)
annual_reportQUITT <- as.quitte(annual_reportCSV) 

QUITTobj = annual_reportQUITT

#####################################################
#plot market value of technology "TECHkey" in quitte object "QUITTobj"
#this 
market_value_lineplot_Wind_Solar_Share <- function(QUITTobj, TECHkey){
  
  marketvalues <- QUITTobj %>% 
    filter(variable == "Market value") %>% 
    filter(tech == TECHkey) %>%  select("variable", "scenario", "unit", "tech", "value") %>%
    separate(scenario, c("scenario", "PVshare", "WindOnshare", "WindOffshare", "P2Gshare"), "_") %>% 
    mutate(PVshare = parse_number(PVshare)) %>% mutate(WindOnshare = parse_number(WindOnshare)) %>% mutate(WindOffshare = parse_number(WindOffshare)) %>% 
    mutate(P2Gshare = parse_number(P2Gshare)) %>% 
    mutate(PVshare = as.numeric(PVshare)) %>%
    mutate(WindOnshare = as.numeric(WindOnshare))
  
    if (TECHkey == "Solar PV") {xname <-"PVshare"; techname <- "Solar PV"}
  
    if (TECHkey == "Wind On") {xname <- "WindOnshare"; techname <- "Onshore Wind"}
  
    if (TECHkey == "WindO ff") {xname <- "WindOffshare"; techname <- "Offshore Wind"}
  
    if (TECHkey == "P2G") {xname <- "P2Gshare"; techname <- "Power to Gas"}
    
    xvar <- as.name(xname)
    MV = marketvalues[[xvar]]
    
    p <- ggplot() + geom_line(data = marketvalues, mapping = aes_string(x = xname, y = "value"), group = 1) +
    xlim(0, max(MV)) + ylim(0, 100) + xlab(paste0(techname, " share")) + ylab("$/MWh") + 
    ggtitle(paste0("Market Value for varying exogenous ", techname, " Share"))
  
  plotfilename = paste0("Marketvalue_", TECHkey, ".png")
  ggsave(plotfilename, width = 7, height = 5)
  
}



# EEtype_total = c("exo", "end") # endogenous or exogenous
# VREtype_total = c("SS", "WS")
# VREtype_NAME_total = c("Solar", "Wind")
# EEtype_NAME_total = c("Exogenous", "Endogenous")
# xname = c("pv_share", "wind_on_share")
# 
# for (i in c(0,1)){
#   print(i+1)
#   print((i+1)%%2+1)
#   print(xname[i+1])
#   print(VREtype_total[i+1])
#   for (j in c(0,1)){
#         #plot for one varying wind or solar share
#         techfile = paste0("~/DIETER/myFirstDIETER/DIETER/results_m", VREtype_total[i+1], "_", EEtype_total[j+1], VREtype_total[(i+1)%%2+1], ".gdx")
#         print(techfile)
#         market_value_lineplot_Wind_Solar_Share(techfile, i, j)
#     }
# }


market_value_lineplot_Wind_Solar_Share(QUITTobj, "Solar PV")
  