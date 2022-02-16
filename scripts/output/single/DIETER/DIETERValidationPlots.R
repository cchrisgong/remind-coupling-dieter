# Main function for REMIND-DIETER plots

# DIETERValidationPlots <- function(outputdir, dieter.scripts.folder) {
  
  # Load libraries ----------------------------------------------------------

  library(tidyverse)
  library(quitte)
  library(lusweave)
  library(magclass)
  library(mip)
  library(grid)
  library(gridExtra)
  
  outputdir = "./output/DIETERcoup_base_25_2022-02-15_20.40.46"
  setwd("~/remind-coupling-dieter/")
  # Configurations ----------------------------------------------------------
  ##
  # whether to save png
  save_png = 1
  
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
  
  dieter.mif.annual.report <- paste0(outputdir, "/DIETER/Dieter_Annual.mif")
  
  cat(paste0("DIETER mif report files: ", dieter.mif.annual.report, "\n"))
  
  ## for plots every few steps
  dieter.iter.step = 5
  
  ## load cm_startyear
  startyear <- cfg$gms$cm_startyear
  model.startyear = max(2020,startyear)
  model.periods <- c(seq(model.startyear, 2060, 5), seq(2070, 2100, 10), seq(2110, 2150, 20))
  model.periods.till2100 <- c(seq(model.startyear, 2060, 5), seq(2070, 2100, 10))
  
  report.periods <- c(seq(2005, 2060, 5), seq(2070, 2100, 10), seq(2110, 2150, 20))

  # load coupled region
  DTcoupreg <- file.path(outputdir, remind.files[2]) %>%  
    read.gdx("regDTcoup", factor = FALSE) 
  reg <- as.character(DTcoupreg)
  
  ## define technologies
  ############### REMIND #########################
  remind.nonvre.mapping <- c(igcc = "Coal",
                             igccc = "Coal",
                             pcc = "Coal",
                             pco = "Coal",
                             pc = "Coal",
                             tnrs = "Nuclear",
                             fnrs = "Nuclear",
                             ngt = "OCGT",
                             ngcc = "CCGT",
                             ngccc = "CCGT",
                             bioigcc = "Biomass",
                             bioigccc = "Biomass",
                             NULL)
  
  if(cfg$gms$cm_DT_CHP_coup == "on"){
    remind.nonvreCHP.mapping <- c(coalchp = "Coal",
                                  gaschp = "CCGT",
                                  biochp = "Biomass",
                                  NULL)
    remind.nonvre.mapping <- c(remind.nonvre.mapping, remind.nonvreCHP.mapping)
  }
  
  # shifting hydro to dispatchable because in REMIND usable energy is only defined for spv, wind, csp
  remind.nonvre.mapping.whyd <- c(remind.nonvre.mapping, hydro = "Hydro")
  
  remind.vre.mapping <- c(wind = "Wind Onshore",
                          windoff = "Wind Offshore",
                          spv = "Solar")
  
  remind.sector.coupling.mapping <- c(seel = "Electricity",
                                      elh2 = "Electrolyzers")
  
  remind.tech.mapping <- c(remind.nonvre.mapping.whyd, remind.vre.mapping, remind.sector.coupling.mapping)
  
  ############### DIETER #########################
  table_ordered_name = c("Coal", "CCGT", "Solar", "Wind Onshore", "Wind Offshore", "Biomass", "OCGT", "Hydro", "Nuclear","Electrolyzers")
  
  table_ordered_name_dem = c("Electrolyzers","Electricity")
  
  dieter.tech.exclude <- c("OCGT_ineff")
  
  dieter.supply.tech.mapping <- c(coal = "Coal",
                                  nuc = "Nuclear",
                                  OCGT_eff = "OCGT",
                                  CCGT = "CCGT",
                                  bio = "Biomass",
                                  ror = "Hydro",
                                  Wind_on = "Wind Onshore",
                                  Wind_off = "Wind Offshore",
                                  Solar = "Solar",
                                  NULL)
  
  dieter.demand.tech.mapping <- c(el = "Electricity",
                                  elh2 = "Electrolyzers")
  
  dieter.nonvre.mapping<- c(coal = "Coal",
                            nuc = "Nuclear",
                            OCGT_eff = "OCGT",
                            CCGT = "CCGT",
                            bio = "Biomass",
                            NULL)
  
  dieter.tech.mapping <- c(dieter.supply.tech.mapping, dieter.demand.tech.mapping)
  
  dieter.supply.fuel.mapping <- c("Coal" = "pecoal",
                                  "OCGT" = "pegas",
                                  "Biomass" = "pebiolc",
                                  NULL)
  
  vre.names <- c("Hydro","Wind Onshore","Wind Offshore", "Solar")
  nonvre.names <- c("Coal", "Nuclear","OCGT","CCGT","Biomass","Electrolyzers")
  
  dieter.tech = c("CCGT",
                    "Solar",
                    "Wind_on",
                    "Wind_off",
                    "bio",
                    "OCGT_eff",
                    "ror",
                    "nuc",
                    "elh2",
                    "el")

  ## define color mapping
  color.mapping <- c("CCGT" = "#999959", "Coal" = "#0c0c0c",
                      "Solar" = "#ffcc00", "Wind Onshore" = "#337fff", 
                      "Wind Offshore" = "#334cff", "Biomass" = "#005900",
                      "OCGT" = "#e51900", "Hydro" = "#191999", "Nuclear" = "#ff33ff",
                      "Electrolyzers" = "#48D1CC", "Electricity" = "#6495ED",
                      NULL)
  
  color.mapping_vre <- c("Solar" = "#ffcc00", "Wind Onshore" = "#337fff", "Wind Offshore" = "#334cff")
  
  color.mapping.capfac.line <- c(color.mapping,
                      "peak hourly residual demand" = "#0c0c0c", NULL)
  
  color.mapping.seel.line <- c(color.mapping,
                               "REMIND secondary electricity price ($/MWh) " = "#FFA500", 
                               NULL)
  
  color.mapping.cf <-c("REMIND real CapFac (%)" =  "#191999",
                       "DIETER real avg CapFac (%)" = "#999959")
  
  color.mapping.cf.detail <- c(color.mapping.cf, 
                               "DIETER avg CapFac (%)" = '#005900', 
                               "REMIND CapFac (%)" = "#0c0c0c",
                               'DIETER marg CapFac (%)' = '#7F7FFF',
                               "DIETER real marg CapFac (%)" = "#ff33ff")
  ##### variable names
  
  capfac.detail.report.dieter = c( "DIETER avg CapFac (%)",
                                   "DIETER real avg CapFac (%)",
                                   "DIETER marg CapFac (%)",
                                   "DIETER real marg CapFac (%)",
                                   "REMIND CapFac (%)",
                                   "REMIND real CapFac (%)")
  
  
  sm_TWa_2_MWh <- 8.76E9

  

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

  # source(file.path(dieter.scripts.folder, "plotCapAndCF.R"), local=TRUE)

  # Generation --------------------------------------------------------------

  # source(file.path(dieter.scripts.folder, "plotGeneration.R"), local=TRUE)

  # Added capacities --------------------------------------------------------

  # source(file.path(dieter.scripts.folder, "plotAddedCapacities.R"), local=TRUE)

  # Price: Secondary electricity --------------------------------------------
  
  # source(file.path(dieter.scripts.folder, "plotPrice.R"), local=TRUE)

  # Price: primary energy and fuel --------------------------------------------
  
  source(file.path(dieter.scripts.folder, "plotFuelPriceAndTrade.R"), local=TRUE)
  
  # LCOEs -------------------------------------------------------------------

  #source(file.path(dieter.scripts.folder, "plotLCOEs.R"), local=TRUE)

  # Price: Peak demand ------------------------------------------------------

  # source(file.path(dieter.scripts.folder, "plotPeakDemandPrice.R"), local=TRUE)

  # (Residual) load duration curves -----------------------------------------

  #source(file.path(dieter.scripts.folder, "plotRLDCs.R"), local=TRUE) # Attention: computationally heavy on standard PC

  # Price duration curves ---------------------------------------------------

  #source(file.path(dieter.scripts.folder, "plotPriceDurationCurve.R"), local=TRUE)

  # (Inverse) screening curves ----------------------------------------------

  #source(file.path(dieter.scripts.folder, "plotInverseScreeningCurve.R"), local=TRUE)

  # Markups -----------------------------------------------------------------


  # Close LaTeX PDF ---------------------------------------------------------

  # swclose(sw)

# }
