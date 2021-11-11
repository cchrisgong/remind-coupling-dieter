# Main function for REMIND-DIETER plots

DIETERValidationPlots <- function(outputdir, dieter.scripts.folder) {
  
  # Load libraries ----------------------------------------------------------

  library(tidyverse)
  library(quitte)
  library(lusweave)
  library(magclass)
  library(mip)
  library(grid)
  library(gridExtra)
  
  # Configurations ----------------------------------------------------------

  report.periods <- c(seq(2005, 2060, 5), seq(2070, 2100, 10), seq(2110, 2150, 20))

  remind.nonvre.mapping <- c(coalchp = "Coal (Lig + HC)",
                             igcc = "Coal (Lig + HC)",
                             igccc = "Coal (Lig + HC)",
                             pcc = "Coal (Lig + HC)",
                             pco = "Coal (Lig + HC)",
                             pc = "Coal (Lig + HC)",
                             tnrs = "Nuclear",
                             ngt = "OCGT",
                             ngcc = "CCGT",
                             ngccc = "CCGT",
                             gaschp = "CCGT",
                             biochp = "Biomass",
                             bioigcc = "Biomass",
                             bioigccc = "Biomass")
  
  # shifting hydro to dispatchable because in REMIND usable energy is only defined for spv, wind, csp
  remind.nonvre.mapping2 <- c(remind.nonvre.mapping, hydro = "Hydro")
  
  remind.vre.mapping <- c(wind = "Wind",
                          spv = "Solar")
  remind.sector.coupling.mapping <- c(elh2 = "Electrolyzers")
  remind.tech.mapping <- c(remind.nonvre.mapping2, remind.vre.mapping,remind.sector.coupling.mapping)
  table_ordered_name = c("Coal (Lig + HC)", "Lignite", "Hard coal","CCGT", "Solar", "Wind", "Biomass", "OCGT", "Hydro", "Nuclear","Electrolyzers")
  #table_ordered_name = c("Solar", "Wind", "Biomass", "Hydro", "Nuclear","CCGT", "OCGT", "Coal (Lig + HC)", "Lignite", "Hard coal")
  table_ordered_name_dem = c("Electricity used for Electrolysis","Electricity")
  
 
  
  dieter.tech.exclude <- c("OCGT_ineff", "Wind_off")
  
  dieter.supply.tech.mapping <- c(hc = "Hard coal",
                                  lig = "Lignite",
                                  coal = "Coal (Lig + HC)",
                                  nuc = "Nuclear",
                                  OCGT_eff = "OCGT",
                                  CCGT = "CCGT",
                                  bio = "Biomass",
                                  ror = "Hydro",
                                  Wind_on = "Wind",
                                  Solar = "Solar")
  
  dieter.demand.tech.mapping <- c(seel = "Electricity",
                                  elh2 = "Electrolyzers")
  
  dieter.tech.mapping <- c(dieter.supply.tech.mapping, dieter.demand.tech.mapping)
  
  color.mapping1 <- c("CCGT" = "#999959", "Coal (Lig + HC)" = "#0c0c0c",
                      "Solar" = "#ffcc00", "Wind" = "#337fff", "Biomass" = "#005900",
                      "OCGT" = "#e51900", "Hydro" = "#191999", "Nuclear" = "#ff33ff", "Electrolyzers" = "#48D1CC", "Electricity" = "#6495ED")
  
  color.mapping2 <- c("CCGT" = "#999959", "Lignite" = "#0c0c0c",
                      "Solar" = "#ffcc00", "Wind" = "#337fff", "Biomass" = "#005900",
                      "OCGT" = "#e51900", "Hydro" = "#191999", "Nuclear" = "#ff33ff",
                      "Hard coal" = "#808080", "Electrolyzers" = "#48D1CC", "Electricity" = "#6495ED", "peak demand" = "#0c0c0c")
  
  vre.names <- c("Hydro","Wind","Solar")
  nonvre.names <- c("Lignite", "Hard coal","Coal (Lig + HC)", "Nuclear","OCGT","CCGT","Biomass","Electrolyzers")
  
  TECHkeylst_DT = c("CCGT",
                    "lig",
                    "Solar",
                    "Wind_on",
                    "bio",
                    "OCGT_eff",
                    "ror",
                    "nuc",
                    "hc",
                    "elh2",
                    "seel")
  sm_TWa_2_MWh <- 8.76E9

  # Directories -------------------------------------------------------------

  report.output.file <- file.path(outputdir, paste0("REMIND-DIETER_validation_", str_sub(outputdir, start=8), ".pdf"))
  #report.output.file <- file.path(outputdir, paste0("REMIND-DIETER_validation_",".pdf"))
  
  remind.files <- list.files(outputdir, pattern = "fulldata_[0-9]+\\.gdx") %>%
  str_sort(numeric = TRUE)
  cat(paste0("REMIND files: ", length(remind.files), "\n"))
  maxiter = length(remind.files)
  
  dieter.files <- list.files(outputdir, pattern = "results_DIETER_i[0-9]+\\.gdx") %>%
  str_sort(numeric = TRUE)
  cat(paste0("DIETER files: ", length(dieter.files), "\n"))

  dieter.files.report <- list.files(outputdir, pattern = "report_DIETER_i[0-9]+\\.gdx") %>%
  str_sort(numeric = TRUE)
  cat(paste0("DIETER report files: ", length(dieter.files.report), "\n"))

  
 
  id <- NULL
  for(fname in dieter.files){
    idx <- as.numeric(str_extract(fname, "[0-9]+"))
    id = c(id, idx)
  }
  id = sort(id)
  
  if (length(dieter.files) != 0) {
    sorted_paths_DT <- paste0(outputdir, "results_DIETER_i", id, ".gdx")
    sorted_files_DT <- paste0("results_DIETER_i", id, ".gdx")
  }
  

  # LaTeX configurations ----------------------------------------------------

  template <- c("\\documentclass[a4paper,landscape,twocolumn]{article}",
               "\\setlength{\\oddsidemargin}{-0.8in}",
               "\\setlength{\\evensidemargin}{-0.5in}",
               "\\setlength{\\topmargin}{-0.8in}",
               "\\setlength{\\parindent}{0in}",
               "\\setlength{\\headheight}{0in}",
               "\\setlength{\\topskip}{0in}",
               "\\setlength{\\headsep}{0in}",
               "\\setlength{\\footskip}{0.2in}",
               "\\setlength\\textheight{0.95\\paperheight}",
               "\\setlength\\textwidth{0.95\\paperwidth}",
               "\\setlength{\\parindent}{0in}",
               "\\usepackage{float}",
               "\\usepackage[bookmarksopenlevel=section,colorlinks=true,linkbordercolor={0.9882353 0.8352941 0.7098039}]{hyperref}",
               "\\hypersetup{bookmarks=true,pdfauthor={PIK}}",
               "\\usepackage{graphicx}",
               "\\usepackage[strings]{underscore}",
               "\\usepackage{Sweave}",
               "\\begin{document}",
               "<<echo=false>>=",
               "options(width=110)",
               "@")

  # Open LaTeX PDF ----------------------------------------------------------

  sw <- swopen(report.output.file, template = template)

  swlatex(sw, "\\tableofcontents\\newpage")
  
  # Capacity factors --------------------------------------------------------

  # Capacities --------------------------------------------------------------

  source(file.path(dieter.scripts.folder, "plotCapAndCF.R"), local=TRUE)

  # Generation --------------------------------------------------------------

  source(file.path(dieter.scripts.folder, "plotGeneration.R"), local=TRUE)

  # Added capacities --------------------------------------------------------

  #source(file.path(dieter.scripts.folder, "plotAddedCapacities.R"), local=TRUE)

  # LCOEs -------------------------------------------------------------------

  #source(file.path(dieter.scripts.folder, "plotLCOEs.R"), local=TRUE)

  # Price: Secondary electricity --------------------------------------------

  source(file.path(dieter.scripts.folder, "plotSeelPrice.R"), local=TRUE)

  # Price: Peak demand ------------------------------------------------------

  #source(file.path(dieter.scripts.folder, "plotPeakDemandPrice.R"), local=TRUE)

  # (Residual) load duration curves -----------------------------------------

  #source(file.path(dieter.scripts.folder, "plotRLDCs.R"), local=TRUE) # Attention: computationally heavy on standard PC

  # Price duration curves ---------------------------------------------------

  #source(file.path(dieter.scripts.folder, "plotPriceDurationCurve.R"), local=TRUE)

  # (Inverse) screening curves ----------------------------------------------

  #source(file.path(dieter.scripts.folder, "plotInverseScreeningCurve.R"), local=TRUE)

  # Markups -----------------------------------------------------------------


  # Close LaTeX PDF ---------------------------------------------------------

  swclose(sw)

}
