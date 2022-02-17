# |  (C) 2006-2019 Potsdam Institute for Climate Impact Research (PIK)
# |  authors, and contributors see CITATION.cff file. This file is part
# |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
# |  AGPL-3.0, you are granted additional permissions described in the
# |  REMIND License Exception, version 1.0 (see LICENSE file).
# |  Contact: remind@pik-potsdam.de

require(lucode2)

# outputdir = "./output/DIETERcoup_base_25_2022-02-15_10.54.13"
# setwd("/home/chengong/remind-coupling-dieter/")
options(warn=-1)

cat("DIETERvalidation.R called \n")
if(!exists("source_include")) {
  readArgs("outputdir")
}
cat("ReadArgs processed \n")

load(file.path(outputdir, "config.Rdata"))
cat("cfg loaded \n")

dieter.scripts.folder <- "./scripts/output/single/DIETER"

## Run DIETER output
if(cfg$gms$power == "DTcoup"){
  # Source function
  dieterenv <- new.env()
  #sys.source(file.path(dieter.scripts.folder, "DIETERValidationPlots.R"), envir = dieterenv)
  source(file.path(dieter.scripts.folder, "DIETERValidationPlots.R"))
  # Call function
  DIETERValidationPlots(outputdir = outputdir, dieter.scripts.folder = dieter.scripts.folder, cfg = cfg)
}
