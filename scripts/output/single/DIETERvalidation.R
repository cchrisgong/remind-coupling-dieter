# |  (C) 2006-2019 Potsdam Institute for Climate Impact Research (PIK)
# |  authors, and contributors see CITATION.cff file. This file is part
# |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
# |  AGPL-3.0, you are granted additional permissions described in the
# |  REMIND License Exception, version 1.0 (see LICENSE file).
# |  Contact: remind@pik-potsdam.de

#if(!exists("source_include")) {
  ## Define arguments that can be read from command line
#  readArgs("outputdir")
#}

dieter.scripts.folder <- "./scripts/output/single/DIETER"

## Run DIETER output
if(cfg$gms$power == "DTcoup"){
  cat("Starting DIETER output")
  cat(getwd())
  source(file.path(dieter.scripts.folder, "main.R"))
}
