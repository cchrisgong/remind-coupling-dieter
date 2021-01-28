# Main function for REMIND-DIETER plots

DIETERValidationPlots <- function(outputdir, title, dieter.scripts.folder) {

  # Load libraries ----------------------------------------------------------

  library(tidyverse)
  library(quitte)
  library(lusweave)
  library(magclass)
  library(mip)
  library(grid)
  library(gridExtra)

  # Configurations ----------------------------------------------------------

  report.periods <- seq(2015, 2050, 5)
  report.periods.long <- c(seq(2015, 2060, 5), seq(2070, 2100, 10))

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
                           bioigccc = "Biomass",
                           NULL)

  remind.vre.mapping <- c(hydro = "Hydro",
                        wind = "Wind",
                        spv = "Solar",
                        NULL)

  remind.tech.mapping <- c(remind.nonvre.mapping, remind.vre.mapping)

  dieter.tech.exclude <- c("OCGT_ineff", "Wind_off")

  dieter.tech.mapping <- c(hc = "Hard coal",
                         lig = "Lignite",
                         coal = "Coal (Lig + HC)",
                         nuc = "Nuclear",
                         OCGT_eff = "OCGT",
                         CCGT = "CCGT",
                         bio = "Biomass",
                         ror = "Hydro",
                         Wind_on = "Wind",
                         Solar = "Solar",
                         NULL)

  color.mapping <- c("CCGT" = "#999959", "Lignite" = "#0c0c0c", "Coal (Lig + HC)" = "#0c0c0c",
                   "Solar" = "#ffcc00", "Wind" = "#337fff", "Biomass" = "#005900",
                   "OCGT" = "#e51900", "Hydro" = "#191999", "Nuclear" = "#ff33ff",
                   "Hard coal" = "#808080")

  sm_TWa_2_MWh <- 8.76E9

  # Directories -------------------------------------------------------------

  report.output.file <- file.path(outputdir, paste0("REMIND-DIETER_validation_", title, ".pdf"))

  remind.files <- list.files(outputdir, pattern = "fulldata_[0-9]+\\.gdx") %>%
  str_sort(numeric = TRUE)

  dieter.files <- list.files(outputdir, pattern = "results_DIETER_i[0-9]+\\.gdx") %>%
  str_sort(numeric = TRUE)

  dieter.files.report <- list.files(outputdir, pattern = "report_DIETER_i[0-9]+\\.gdx") %>%
  str_sort(numeric = TRUE)

  # Determine iteration step of DIETER
  dieter.iter.step <- floor(length(remind.files) / length(dieter.files))

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
