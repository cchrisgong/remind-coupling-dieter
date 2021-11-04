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

  report.periods <- c(seq(2020, 2060, 5), seq(2070, 2100, 10))

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

  
  sm_TWa_2_MWh <- 8.76E9

  # Directories -------------------------------------------------------------

  report.output.file <- file.path(outputdir, paste0("REMIND-DIETER_validation_", str_sub(outputdir, start=8), ".pdf"))
  
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

  source(file.path(dieter.scripts.folder, "plotCapacityFactors.R"), local=TRUE)

  # Capacities --------------------------------------------------------------

  source(file.path(dieter.scripts.folder, "plotCapacities.R"), local=TRUE)

  # Generation --------------------------------------------------------------

  source(file.path(dieter.scripts.folder, "plotGeneration.R"), local=TRUE)

  # Added capacities --------------------------------------------------------

  source(file.path(dieter.scripts.folder, "plotAddedCapacities.R"), local=TRUE)

  # LCOEs -------------------------------------------------------------------

  source(file.path(dieter.scripts.folder, "plotLCOEs.R"), local=TRUE)

  # Price: Secondary electricity --------------------------------------------

  source(file.path(dieter.scripts.folder, "plotSeelPrice.R"), local=TRUE)

  # Price: Peak demand ------------------------------------------------------

  source(file.path(dieter.scripts.folder, "plotPeakDemandPrice.R"), local=TRUE)

  # (Residual) load duration curves -----------------------------------------

  source(file.path(dieter.scripts.folder, "plotRLDCs.R"), local=TRUE) # Attention: computationally heavy on standard PC

  # Price duration curves ---------------------------------------------------

  source(file.path(dieter.scripts.folder, "plotPriceDurationCurve.R"), local=TRUE)

  # (Inverse) screening curves ----------------------------------------------

  source(file.path(dieter.scripts.folder, "plotInverseScreeningCurve.R"), local=TRUE)

  # Markups -----------------------------------------------------------------


  # Close LaTeX PDF ---------------------------------------------------------

  swclose(sw)

}
