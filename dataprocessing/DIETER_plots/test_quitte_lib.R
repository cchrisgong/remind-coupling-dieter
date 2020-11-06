
require(quitte)
require(gdxrrw)
igdx("/opt/gams/gams30.2_linux_x64_64_sfx")

mydatapath = "~/DIETER/myFirstParallelDIETER/DIETER/"
file = "results_DIETER_1.gdx"

setwd(mydatapath)
getwd()
######################################################################################################################## 
rep_hrs = quitte::read.gdx(file, requestList = 'report_hours', factors = FALSE)

print(rep_hrs)
