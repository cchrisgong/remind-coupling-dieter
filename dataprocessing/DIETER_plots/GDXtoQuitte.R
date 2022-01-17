
# get data from config.R file
# file: config.R
require(quitte)
require(gdxrrw)
require(tidyverse)
options(scipen = 999) #disable scientific notation

# IGNORE THE WARNING WHICH APPEARS AFTER EXECUTION OF THE FILE.. 

myDIETERPLOT_path = "~/remind-coupling-dieter/dataprocessing/DIETER_plots/"

source(paste0(myDIETERPLOT_path, "library_import.R"))
igdx("/opt/gams/gams30.2_linux_x64_64_sfx")

# variable_units <- read.csv(paste0(myDIETERPLOT_path, "variable_dict.csv"), sep = ',', header = T, stringsAsFactors = F)
# variables <- unique(variable_units$dieter_name)
# variable_dict <- setNames(as.list(variable_units$report_name), variable_units$dieter_name)
# unit_dict <- setNames(as.list(variable_units$Units), variable_units$dieter_name)

# CHECK IF THE SEPERATOR IS CORRECT!!!!!!!!!! sometimes it is changed from ; to , in Linux and windows
# tech_dictionary <- read.csv(paste0(myDIETERPLOT_path, "tech_dict.csv"), sep = ",", header = T, stringsAsFactors = F)
# tech_dict <- setNames(as.list(tech_dictionary$report_name), tech_dictionary$dieter_name)
# scenario_desc <- read.table("~/DIETER/myFirstParallelDIETER/dataprocessing/scenario_desc.csv", sep = ";", head = T, stringsAsFactors = F)

gdxToQuitte_hourly <- function(mydatapath, gdxfile, run_number){
  file = paste0(mydatapath, gdxfile)
  # file = paste0(mydatapath, "report_DIETER_i30.gdx")
  out_hourly <- NULL
  
  ######################################################################################################################## 
  rep_hrs = read.gdx(gdxName = file, requestList = 'report_hours', factors = FALSE)
  names(rep_hrs) <- c("gdxfile", "model", "year", "country", "variable", "hour", "value")
  
  out_h <- rep_hrs %>% 
    select(model, year, variable, country, hour,value) %>%
    mutate(hour = as.numeric(str_extract(hour, "[0-9]+"))) %>% 
    dplyr::group_by(model, variable, year,  country) %>%
    # filter(year == "2010") %>% 
    complete(hour = (1:8760)) %>%
    replace(is.na(.), 0) %>%
    dplyr::ungroup(model, variable, year, country) %>%
    mutate(MODEL = model, SCENARIO = paste0("baseline"), REGION = country,
           HOUR = hour, TECH = "all Tech") %>% 
    mutate(VARIABLE = variable, PERIOD = year,
           # UNIT = as.vector(unlist(unit_dict[variable])), 
           VALUE = round(value, digits = 4)) %>%
    arrange(PERIOD) %>% 
    select(MODEL, SCENARIO, PERIOD, HOUR, REGION, VARIABLE, TECH, VALUE)
  
  ###################################################################
  rep_techHrs = read.gdx(gdxName = file, requestList = 'report_tech_hours', factors = FALSE)
  
  names(rep_techHrs) <- c("gdxfile", "model","year","country","variable", "tech", "hour", "value")
  
  out_th <- rep_techHrs %>% 
  select(model, year, tech,variable, country, hour,value) %>%
  mutate(hour = as.numeric(str_extract(hour, "[0-9]+"))) %>% 
  mutate(tech = as.character(tech)) %>%
  group_by(model, tech, hour, variable, country) %>%
  mutate(year = as.numeric(year)) %>% 
  complete(year = c(2010,2015,2020,2025,2030,2035,2040,2045,2050,2055,2060,2070,2080,2090,2100)) %>%
  # filter(year == "2010") %>% 
  # filter(tech == "ror") %>%
  ungroup(model, tech, hour, variable, country) %>% 
  group_by(model, year, variable, country,tech) %>%
  complete(hour = (1:8760)) %>%
  replace(is.na(.), 0) %>%
  ungroup(model, year, variable, country,tech) %>% 
  mutate(MODEL = model, SCENARIO = paste0("baseline"),  REGION = country,
         HOUR = hour, YEAR = year, TECH = tech) %>%
  mutate(VARIABLE = variable, PERIOD = year,
         # UNIT = as.vector(unlist(unit_dict[variable])), 
         VALUE = round(value, digits = 4)) %>%
  arrange(YEAR) %>% 
  select(MODEL, SCENARIO, PERIOD, HOUR, REGION, VARIABLE, TECH, VALUE)
  
  #################################################################
  
  out_hourly <- rbind(out_hourly, out_h)
  out_hourly <- rbind(out_hourly, out_th)
  
  idx <- as.numeric(str_extract(gdxfile, "[0-9]+"))
  
  write.table(out_hourly, paste0(myDIETERPLOT_path, run_number, "_i", idx, "_hourlyreport.csv"), sep = ";", row.names = F)
  print(paste0("csv table saved to", myDIETERPLOT_path))
}

gdxToQuitte_annual <- function(mydatapath, gdxfile, run_number){
  file = paste0(mydatapath, gdxfile)
  # file = paste0(mydatapath, "report_DIETER_i5.gdx")
  out_annual <- NULL
  ###########################################################################################################################
  rep = read.gdx(gdxName = file, requestList = 'report', factors = FALSE)
  
  names(rep) <- c("gdxfile", "model","year", "country","variable", "value")
  out <- rep %>% 
    mutate(MODEL = model, SCENARIO = paste0("baseline"), 
           REGION = country, YEAR = year, VALUE = round(value, digits = 4), 
           TECH = "all Tech",
           VARIABLE = variable,
           # UNIT = as.vector(unlist(unit_dict[variable])), 
           PERIOD = "annual") %>%
    arrange(YEAR) %>% 
    select(MODEL, SCENARIO, YEAR, REGION, PERIOD, VARIABLE, TECH, VALUE)
  
  #################################################################
  rep_Tech = read.gdx(gdxName = file, requestList = 'report_tech', factors = FALSE)
  
  names(rep_Tech) <- c("gdxfile", "model","year", "country","variable", "tech", "value")

  # unique(rep_Tech$variable)
  # unique(variable_dict[rep_Tech$variable])
  # length(rep_Tech$variable)
  # length(variable_dict[rep_Tech$variable])
  # unique(rep_Tech$tech)
  # unique(tech_dict[rep_Tech$tech])
  
  out_t <- rep_Tech %>% 
    select(model, year, tech,variable, country,value) %>%
    group_by(model, tech, variable, country) %>%
    mutate(year = as.numeric(year)) %>%
    complete(year = c(2010,2015,2020,2025,2030,2035,2040,2045,2050,2055,2060,2070,2080,2090,2100)) %>%
    replace(is.na(.), 0) %>%
    ungroup(model, tech, variable, country) %>% 
    # group_by(model, tech, variable, country) %>% 
    # complete(year, nesting(variable, tech), fill = list(value = 0)) %>% 
    # ungroup(model, tech, variable, country) %>% 
    # mutate(tech = as.character(tech)) %>%
    # mutate(year = as.factor(year)) %>%
    # filter(year == 2025) %>% 
    # filter(tech == "Solar") %>% 
    mutate(MODEL = model, SCENARIO = paste0("baseline"), 
           REGION = country, YEAR = year, VALUE = round(value, digits = 4), 
           # TECH = as.vector(unlist(tech_dict[tech])),
           TECH = tech,
           # VARIABLE = as.vector(unlist(variable_dict[variable])),
           VARIABLE = variable,
           # UNIT = as.vector(unlist(unit_dict[variable])), 
           PERIOD = "annual"
           ) %>%
    arrange(YEAR) %>%
    select(MODEL, SCENARIO, YEAR, REGION, PERIOD, VARIABLE, TECH, VALUE)
  
  #################################################################
  out_annual <- rbind(out_annual, out)
  out_annual <- rbind(out_annual, out_t)

  idx <- as.numeric(str_extract(gdxfile, "[0-9]+"))  
  
  write.table(out_annual, paste0(myDIETERPLOT_path, run_number, "_i", idx, "_annualreport.csv"), sep = ";", row.names = F)
  
  print(paste0("csv table saved to", myDIETERPLOT_path))
  
}


# file = "~/DIETER/myFirstDIETER/DIETER/results_mSS_exoWS_PtG-ONLY.gdx"
# gdxToQuitte(file)

