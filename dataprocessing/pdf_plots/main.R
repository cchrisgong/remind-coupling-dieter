# Main script for REMIND-DIETER plots

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(quitte)
library(lusweave)
library(magclass)
library(mip)
library(grid)
library(gridExtra)

# Configurations ----------------------------------------------------------

# Make sure paths end on /
remind.dieter.path <- "C:/Users/adrianod/Documents/PhD/Modelling/remind-dieter/output/"
scenario.name <- "REMIND-EU_xx_ref_FEmed_2020-11-16_18.14.29/"
report.periods <- seq(2015,2050,5)
#report.path <- "C:/Users/adrianod/Documents/PhD/Modelling/remind-dieter/report/"

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
                   "OCGT" = "#e51900", "Hydro" =  "#191999", "Nuclear" =  "#ff33ff",
                   "Hard coal" = "#808080")


# Directories -------------------------------------------------------------

report.output.file <- paste0(remind.dieter.path, scenario.name, "REMIND-DIETER_report.pdf")

dieter.files <- paste0(remind.dieter.path, scenario.name) %>% 
  list.files(pattern="results_DIETER_i[0-9]+\\.gdx") %>% 
  str_sort(numeric=TRUE)

remind.files <- paste0(remind.dieter.path, scenario.name) %>% 
  list.files(pattern="fulldata_[0-9]+\\.gdx") %>% 
  str_sort(numeric=TRUE)

# Determine iteration step of DIETER
dieter.iter.step <- floor(length(remind.files)/length(dieter.files))


# LaTeX configurations ----------------------------------------------------

template <-  c("\\documentclass[a4paper,landscape,twocolumn]{article}",
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

swlatex(sw,"\\tableofcontents\\newpage")

# Capacity factors --------------------------------------------------------

source("./plotCapacityFactors.R")

# Capacities --------------------------------------------------------------

source("./plotCapacities.R")

# Generation --------------------------------------------------------------

source("./plotGeneration.R")

# Added capacities --------------------------------------------------------

source("./plotAddedCapacities.R")

# LCOEs -------------------------------------------------------------------



# Price: Secondary electricity --------------------------------------------


# Price: Peak demand ------------------------------------------------------


# Price: Demand -----------------------------------------------------------


# RLDCs -------------------------------------------------------------------


# Price duration curves ---------------------------------------------------





# Close LaTeX PDF ---------------------------------------------------------

swclose(sw)
