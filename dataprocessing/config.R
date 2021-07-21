
#set GAMS location
#
# igdx("C:/GAMS/win64/25.1/")

#specify file location where the results.gdx is
#
file_loc <- "/home/chengong/DIETER/myFirstParallelDIETER/DIETER"

# report type
# based on the tag in the GDX input files
# possible tags  : "report_hours", "report_tech_hours", "report_tech", "report"
report_type <- "report_hours"

# CHECK IF THE SEPERATOR IS CORRECT!!!!!!!!!! sometimes it is changed from ; to ,
variable_units <- read.csv("/home/chengong/DIETER/myFirstParallelDIETER/dataprocessing/variable_dict.csv", sep = ',', header = T, stringsAsFactors = F)
# variable_units$dieter_name
# variable_units$report_name
variables <- unique(variable_units$dieter_name)
variable_dict <- setNames(as.list(variable_units$report_name), variable_units$dieter_name)
unit_dict <- setNames(as.list(variable_units$Units), variable_units$dieter_name)

#get tech dict
#
# CHECK IF THE SEPERATOR IS CORRECT!!!!!!!!!! sometimes it is changed from ; to , in Linux and windows
tech_dictionary <- read.csv("/home/chengong/DIETER/myFirstParallelDIETER/dataprocessing/tech_dict.csv", sep = ",", header = T, stringsAsFactors = F)
tech_dict <- setNames(as.list(tech_dictionary$report_name), tech_dictionary$dieter_name)

#model
model <- c("DIETER V1.0.2")

state <- "N/A"
# for hourly reports, make the "hourly" variable true
# for tech reports, make the "tech" variable true
# hourly <- FALSE
hourly_flag <- T
tech_flag <- F
# tech <- TRUE

#set year
year <- "2030"

scenario_desc <- read.table("~/DIETER/myFirstParallelDIETER/dataprocessing/scenario_desc.csv", sep = ";", head = T, stringsAsFactors = F)

