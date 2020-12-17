# Main script for REMIND-DIETER plots

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(quitte)
library(lusweave)
library(magclass)
library(mip)

# Configurations ----------------------------------------------------------

# Make sure paths end on /
remind.dieter.path <- "C:/Users/adrianod/Documents/PhD/Modelling/remind-dieter/output/"
scenario.name <- "REMIND-EU_xx_ref_FEmed_2020-11-16_18.14.29/"
report.periods <- seq(2015,2050,5)
#report.path <- "C:/Users/adrianod/Documents/PhD/Modelling/remind-dieter/report/"

remind.nonvre.mapping <- c(ngcc = "CCGT",
                         ngccc = "CCGT",
                         gaschp = "CCGT",
                         ngt = "OCGT",
                         coalchp = "Coal (Lig + HC)",
                         igcc = "Coal (Lig + HC)",
                         igccc = "Coal (Lig + HC)",
                         pcc = "Coal (Lig + HC)",
                         pco = "Coal (Lig + HC)",
                         pc = "Coal (Lig + HC)",
                         tnrs = "Nuclear",
                         biochp = "Biomass",
                         bioigcc = "Biomass",
                         bioigccc = "Biomass",
                         NULL)

remind.vre.mapping <- c(spv = "Solar",
                         wind = "Wind",
                         hydro = "Hydro",
                         NULL)

dieter.tech.exclude <- c("OCGT_ineff", "Wind_off", "lig", "hc")

dieter.tech.mapping <- c(CCGT = "CCGT",
                         #lig = "Lignite",
                         Solar = "Solar",
                         Wind_on = "Wind",
                         bio = "Biomass",
                         OCGT_eff = "OCGT",
                         ror = "Hydro",
                         nuc = "Nuclear",
                         #hc = "Hard coal",
                         coal = "Coal (Lig + HC)",
                         NULL)

# Directories -------------------------------------------------------------

report.output.file <- paste0(remind.dieter.path, scenario.name, "REMIND-DIETER_report.pdf")

dieter.files <- paste0(remind.dieter.path, scenario.name) %>% 
  list.files(pattern="results_DIETER_i[0-9]+\\.gdx") %>% 
  str_sort(numeric=TRUE)

remind.files <- paste0(remind.dieter.path, scenario.name) %>% 
  list.files(pattern="fulldata_[0-9]+\\.gdx") %>% 
  str_sort(numeric=TRUE)


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



# Generation --------------------------------------------------------------



# LCOEs -------------------------------------------------------------------




# Close LaTeX PDF ---------------------------------------------------------

swclose(sw)
