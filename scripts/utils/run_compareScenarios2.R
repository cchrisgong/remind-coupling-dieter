# |  (C) 2006-2020 Potsdam Institute for Climate Impact Research (PIK)
# |  authors, and contributors see CITATION.cff file. This file is part
# |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
# |  AGPL-3.0, you are granted additional permissions described in the
# |  REMIND License Exception, version 1.0 (see LICENSE file).
# |  Contact: remind@pik-potsdam.de
library(lucode2) # getScenNames
library(remind2)

if (!exists("source_include")) {
  readArgs("outputdirs")
  readArgs("shortTerm")
  readArgs("outfilename")
  readArgs("regionList")
  readArgs("mainRegName")
}

run_compareScenarios2 <- function(outputdirs, shortTerm, outfilename, regionList, mainRegName) {

  scenNames <- getScenNames(outputdirs)
  # Add '../' in front of the paths as compareScenarios2() will be run in individual temporary subfolders (see below).
  mif_path  <- file.path("..", outputdirs, paste("REMIND_generic_", scenNames, ".mif", sep = ""))
  hist_path <- file.path("..", outputdirs[1], "historical.mif")

  # Create temporary folder. This is necessary because each compareScenarios2 creates a folder names 'figure'.
  # If multiple compareScenarios2 run in parallel they would interfere with the others' figure folder.
  # So we create a temporary subfolder in which each compareScenarios2 creates its own figure folder.
  system(paste0("mkdir ", outfilename))
  outfilename <- normalizePath(outfilename) # Make path absolute.
  wd <- getwd()

  setwd(outfilename)
  # remove temporary folder
  on.exit(system(paste0("mv ", outfilename, ".pdf ..")))
  on.exit(setwd(wd), add = TRUE)
  on.exit(system(paste0("rm -rf ", outfilename)), add = TRUE)

  # Make paths absolute.
  mif_path <- normalizePath(mif_path)
  hist_path <- normalizePath(hist_path)

  splt <- strsplit(outfilename, "/", fixed=TRUE)[[1]]
  outputFile <- splt[length(splt)]
  outputDir <- paste(splt[-length(splt)], collapse="/")

  if (!shortTerm) {
    try(compareScenarios2(
      mifScen = mif_path,
      mifHist = hist_path,
      outputDir = outputDir,
      outputFile = outputFile,
      outputFormat = "PDF",
      reg = regionList,
      mainReg = mainRegName))
  } else {
    try(compareScenarios2(
      mifScen = mif_path,
      mifHist = hist_path,
      outputDir = outputDir,
      outputFile = outputFile,
      outputFormat = "PDF",
      reg = regionList,
      mainReg = mainRegName,
      yearsScen = seq(2005, 2050, 5),
      yearsHist = c(seq(1990, 2020, 1), seq(2025, 2050, 5)),
      yearsBarPlot = c(2010, 2030, 2050)))
  }
}

run_compareScenarios2(outputdirs, shortTerm, outfilename, regionList, mainRegName)