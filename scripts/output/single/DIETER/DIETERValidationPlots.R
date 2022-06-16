# Main function for REMIND-DIETER plots

DIETERValidationPlots <- function(outputdir, dieter.scripts.folder, cfg) {
  
  # Load libraries ----------------------------------------------------------

  library(tidyverse)
  library(quitte)
  library(lusweave)
  library(magclass)
  library(grid)
  library(gridExtra)
  library(data.table)
  
  # Configurations ----------------------------------------------------------
  ##
  # whether to save png
  save_png = 1
  CAPwith_CF = 0 # whether to plot capacities with CF
  options(warn=-1)
  
  # Directories -------------------------------------------------------------
  
  report.output.file <- file.path(outputdir, paste0("REMIND-DIETER_validation_", str_sub(outputdir, start=8), ".pdf"))
  
  remind.files <- list.files(outputdir, pattern = "fulldata_[0-9]+\\.gdx") %>%
    str_sort(numeric = TRUE)
  cat(paste0("REMIND files: ", length(remind.files), "\n"))
  maxiter =  as.numeric(str_extract(remind.files[length(remind.files)], "[0-9]+"))
  
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
  model.periods.till2070 <- c(seq(model.startyear, 2060, 5), 2070)
  model.periods.RLDC <- c(seq(model.startyear, 2045, 5))
  
  report.periods <- c(seq(2005, 2060, 5), seq(2070, 2100, 10), seq(2110, 2150, 20))

  ## load the number of iteration when coupling starts
  start_i = as.numeric(str_extract(dieter.files[2], "[0-9]+"))
  iteration.list = seq(start_i, str_extract(dieter.files[length(dieter.files)], "[0-9]+"),1)
  
  # load coupled region
  DTcoupreg <- file.path(outputdir, remind.files[2]) %>%  
    read.gdx("regDTcoup", factor = FALSE) 
  reg <- as.character(DTcoupreg)
  
  # load switches
  h2switch <- cfg$gms$cm_DT_elh2_coup
  storswitch <- cfg$gms$cm_DTstor
  
  
  ## define technologies
  ############### REMIND #########################
  remind.nonvre.mapping <- c(igcc = "Coal",
                             igccc = "Coal",
                             pcc = "Coal",
                             pco = "Coal",
                             pc = "Coal",
                             tnrs = "Nuclear",
                             # fnrs = "Nuclear",
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
                                      elh2 = "Flexible electrolyzers (PtG)",
                                      tdels = "Stationary Electricity",
                                      tdelt = "Transport Electricity")
 
  remind.grid.mapping <- c(gridwindoff = "VRE Grid")
  
  remind.sector.coupling.mapping.narrow <- c(elh2 = "Flexible electrolyzers (PtG)")
  
  remind.sector.coupling.mapping.exclude <- c(tdels = "Stationary Electricity",
                                              tdelt = "Transport Electricity")

  remind.storage.mapping.narrow <- c(storspv = "Lithium-ion Battery",
                                     storcsp = "H2 storage")
  
  remind.tech.mapping <- c(remind.nonvre.mapping.whyd, remind.vre.mapping)
  remind.tech.mapping.narrow <- remind.tech.mapping
  
  ## only report electrolyers in demand side tech
  if (h2switch == "on"){
    remind.tech.mapping.narrow <- c(remind.tech.mapping.narrow,
                                    remind.sector.coupling.mapping.narrow, 
                                    NULL)
    
    remind.tech.mapping <- c(remind.tech.mapping,
                             remind.sector.coupling.mapping, 
                             NULL)
  }  
  
  
  if (storswitch == "on"){
    remind.tech.mapping.narrow <- c(remind.tech.mapping.narrow,
                                    remind.storage.mapping.narrow,
                                    NULL)
    
    remind.tech.mapping <- c(remind.tech.mapping, 
                             remind.storage.mapping.narrow, 
                             NULL)
  }
  
  remind.tech.storloss.mapping <-  c(wind = "Wind Onshore Curtailment",
                                     windoff = "Wind Offshore Curtailment",
                                     spv = "Solar Curtailment")
  
  ############### DIETER #########################
  table_ordered_name = c("Coal", "CCGT", "Solar", "Wind Onshore", "Wind Offshore", "Biomass", "OCGT", "Hydro", "Nuclear","Flexible electrolyzers (PtG)")
  
  table_ordered_name_dem = c("Flexible electrolyzers (PtG)","Electricity")
  
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
                                  elh2 = "Flexible electrolyzers (PtG)")
  
  dieter.nonvre.mapping<- c(coal = "Coal",
                            nuc = "Nuclear",
                            OCGT_eff = "OCGT",
                            CCGT = "CCGT",
                            bio = "Biomass",
                            NULL)
  
  dieter.supply.fuel.mapping <- c("Coal" = "pecoal",
                                  "OCGT" = "pegas",
                                  "Biomass" = "pebiolc",
                                  NULL)
  
  vre.names <- c("Hydro","Wind Onshore","Wind Offshore", "Solar")
  nonvre.names <- c("Coal", "Nuclear","OCGT","CCGT","Biomass","Flexible electrolyzers (PtG)")
  
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

  ##### plotLCOEs config
  
  
  ### config
  plot.tech <- c("elh2","system")
  plot.sector <- c("supply-side")
  plot.name <- "SE"
  
  variable = c(       "Price|Secondary Energy|Hydrogen", 
                      #Coal
                      "Price|Secondary Energy|Electricity", 
                      "Price|Secondary Energy|Electricity",
                      "Price|Secondary Energy|Electricity", 
                      "Price|Secondary Energy|Electricity",
                      "Price|Secondary Energy|Electricity", 
                      # "Price|Secondary Energy|Electricity",
                      #Gas
                      "Price|Secondary Energy|Electricity",
                      "Price|Secondary Energy|Electricity", 
                      "Price|Secondary Energy|Electricity",
                      # "Price|Secondary Energy|Electricity",
                      #Biomass
                      "Price|Secondary Energy|Electricity",
                      "Price|Secondary Energy|Electricity", 
                      # "Price|Secondary Energy|Electricity",
                      
                      "Price|Secondary Energy|Electricity", 
                      "Price|Secondary Energy|Electricity",
                      "Price|Secondary Energy|Electricity",
                      "Price|Secondary Energy|Electricity",
                      "Price|Secondary Energy|Electricity", 
                      "Price|Secondary Energy|Electricity"                    
  )
  tech = c(
    "elh2",
    # Coal
    "igcc","igccc","pc","pcc","pco",
    # "coalchp", # disable CHP
    # Gas
    "ngcc","ngccc",
    # "gaschp",  # disable CHP
    "ngt",
    # Biomass
    # "biochp",  # disable CHP
    "bioigcc","bioigccc",
    "wind",
    "windoff",
    "spv",
    "tnrs",
    "hydro",
    "system")
  
  map.price.tech <- data.frame(
    variable,
    tech ,
    sector = rep("supply-side",length(tech)) 
  )
  
  tech.label <- c("spv" ="Solar PV", "wind" = "Wind On","wind" = "Wind Off", "csp" = "Solar CSP", "ngt" = "OCGT",
                  "pcc" = "Coal w/ CCS", "ngccc" = "Gas CC w/ CCS", "tnrs" = "Nuclear","bioigcc" = "Biomass","bioigcc" = "Biomass w/ CCS",
                  "igcc" = "Coal", 
                  "hydro" = "Hydro", "ngcc" = "Gas","pc" = "Coal", "system" = "System", "elh2" = "Flexible electrolyzers (PtG)")
  
## define color mapping 
  

    color.mapping <- c("CCGT" = "#999959", "Coal" = "#0c0c0c",
                       "Solar" = "#ffcc00", "Wind Onshore" = "#337fff", 
                       "Wind Offshore" = "#334cff", "Biomass" = "#005900",
                       "OCGT" = "#e51900", "Hydro" = "#191999", "Nuclear" = "#ff33ff",
                       NULL)
  
  if (h2switch == "on"){
    color.mapping <- c(color.mapping,
                       "Flexible electrolyzers (PtG)" = "#48D1CC", "Electricity" = "#6495ED",
                       NULL)
  }
  
  color.mapping.cap <- c("CCGT" = "#999959", "Coal" = "#0c0c0c",
                     "Solar" = "#ffcc00", "Wind Onshore" = "#337fff", 
                     "Wind Offshore" = "#334cff", "Biomass" = "#005900",
                     "OCGT" = "#e51900", "Hydro" = "#191999", "Nuclear" = "#ff33ff",
                     NULL)
  
  if (h2switch == "on"){
    color.mapping.cap <- c(color.mapping.cap,
                       "Flexible electrolyzers (PtG)" = "#48D1CC", 
                       NULL)
  }
  
  if (storswitch == "on"){
    color.mapping.cap <- c(color.mapping.cap,
                           "Lithium-ion Battery" = "cyan", 
                           "H2 storage" = "#56B4E9",
                           NULL)
  }
  
  color.mapping_vre <- c("Solar" = "#ffcc00", "Wind Onshore" = "#337fff", "Wind Offshore" = "#334cff")
  
  color.mapping.cap.line <- c("peak hourly residual demand" = "#0c0c0c")
  
  color.mapping.capfac.line <- c(color.mapping,color.mapping.cap.line,
                                 NULL)
  
  color.mapping.seel.line <- c(color.mapping,
                               "REMIND secondary electricity price ($/MWh)" = "#FFA500", 
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
  
  color.mapping.mv.var <- c(
                         "DIETER Market value ($/MWh)" = "#005900",
                         "DIETER Market value with scarcity price ($/MWh)" = "#007f00",
                         "REMIND market value ($/MWh)" = "#7F7FFF")
  
  color.mapping.wloss <- c(color.mapping, 
                           "Wind Onshore Curtailment" = "#a8c9ff",
                           "Solar Curtailment" = "#f7e7a6",
                           "Wind Offshore Curtailment" = "#919efa",
                           NULL)
  
  linetype.map <- c('DIETER' = 'dotted', 'REMIND' = 'solid')
  
  
  # cost components
  cost.variables <- c(
    "Curtailment Cost" = "Curtailment Cost",
    "Storage Cost" = "Storage Cost",
    "Grid Cost" = "Grid Cost",
    "CCS Cost" = "CCS Cost",
    "CO2 Cost" = "CO2 Tax Cost",
    "CO2 Tax Cost" = "CO2 Tax Cost",
    "OMV Cost" = "OMV Cost",
    "OMF Cost" = "OMF Cost",
    "Investment Cost" = "Investment Cost" ,
    "Fuel Cost" = "Fuel Cost",
    # "Adjustment Cost" = "Adjustment Cost",
    # "Markup" = "Markup",
    NULL)
  
  cost.colors <- c(
                      "Curtailment Cost" = "darkblue",
                      "Storage Cost" = "darkorchid",
                      "Grid Cost" = "darkolivegreen3",
                      "CO2 Tax Cost" = "indianred",
                      "OMV Cost" = "cyan",
                      "OMF Cost" = "darkcyan",
                      "Investment Cost" = "deepskyblue2",
                      "Fuel Cost" = "orange3",
                      "Adjustment Cost" = "darkgoldenrod1",
                      NULL)
  
  cost.colors.policy <- c(
    cost.colors,
    "CCS Cost" = "violet",
    NULL)
  
  cost.colors.wmarkup <- c(
    cost.colors,
    "Markup" = "lightblue",
    NULL)
  
  cost.colors.te <- c(cost.colors,
                      "Flexibility subsidy" = "lightblue")
  
  if (h2switch == "on"){
    cost.colors <- c(cost.colors, 
                      "Flexibility subsidy" = "mediumpurple3")
  }
  
  cost.colors.dieter<- c(
                    "Curtailment Cost" = "darkblue",
                    "Storage Cost" = "darkorchid",
                    "Grid Cost" = "darkolivegreen3",
                    # "CCS Cost" = "darkgoldenrod2",
                    "CO2 Tax Cost" = "indianred",
                    "OMV Cost" = "cyan",
                    "OMF Cost" = "darkcyan",
                    "Investment Cost" = "deepskyblue2",
                    "Fuel Cost" = "orange3",
                    "Adjustment Cost" = "darkgoldenrod1",
                    'DIETER annual average electricity price with scarcity price' = "indianred3",
                    'DIETER shadow price due to capacity constraint from REMIND' = "mediumpurple3",
                    # 'DIETER shadow price due to capacity constraint from REMIND (with grid)' = "mediumpurple2",
                     NULL)
  
  
  # DIETER system and technology (marginal LCOE)
  reportLCOE_DT = c("DIETER LCOE_avg ($/MWh)","DIETER LCOE_marg ($/MWh)")
  
  reportLCOEcomponents_DT_marg = c(
    # LCOE component
    "fuel cost - divided by eta ($/MWh)", 
    "CO2 cost ($/MWh)", 
    "annualized investment cost - marg ($/MWh)", 
    "annualized adjustment cost - marg ($/MWh)", 
    "O&M fixed cost - marg ($/MWh)", 
    "O&M var cost ($/MWh)", 
    # grid cost for VRE
    "grid cost ($/MWh)", 
    # shadow price
    "shadow price of capacity bound from REMIND - marg ($/MWh)", 
    # market value
    "DIETER Market value ($/MWh)",
    "DIETER Market value with scarcity price ($/MWh)",
    NULL
  )
  
  reportLCOEcomponents_DT_avg = c(
    "fuel cost - divided by eta ($/MWh)", 
    "CO2 cost ($/MWh)", 
    "annualized investment cost - avg ($/MWh)", 
    "annualized adjustment cost - avg ($/MWh)", 
    "O&M fixed cost - avg ($/MWh)", 
    "O&M var cost ($/MWh)", 
    "grid cost ($/MWh)", 
    "shadow price of capacity bound from REMIND - avg ($/MWh)", 
    "DIETER Market value ($/MWh)",
    "DIETER Market value with scarcity price ($/MWh)",
    NULL
  )
  
  reportLCOEcomponents_REMIND_avg = c(
    "annualized adjustment cost - avg ($/MWh)", 
    NULL
  )
  
  reportLCOEcomponents_REMIND_marg = c(
    "annualized adjustment cost - marg ($/MWh)", 
    NULL
  )
  
  report_DT_genshare = c("genshares (%)")
  
  report_DT_prices = c(
    'price for total demand - with scarcity price ($/MWh)',
    'price for total demand ($/MWh)',
    'total system shadow price of cap bound w/ grid - avg ($/MWh)',
    'total system shadow price of cap bound - avg ($/MWh)',
    # 'price for fixed demand ($/MWh)'
    NULL
  )
  
  dieter.tech.mapping <- c(coal = "Coal",
                           nuc = "Nuclear",
                           OCGT_eff = "OCGT",
                           CCGT = "CCGT",
                           bio = "Biomass",
                           ror = "Hydro",
                           Wind_on = "Wind Onshore",
                           Wind_off = "Wind Offshore",
                           Solar = "Solar",
                           elh2 = "Flexible electrolyzers (PtG)",
                           el = "Electricity",
                           `all Tech` = "All Tech",
                           vregrid = "VRE grid",
                           lith = "Lithium-ion Battery",
                           hydrogen = "H2 storage",
                           NULL)
  
  dieter.storage.mapping <- c( lith = "Lithium-ion Battery",
                               hydrogen = "H2 storage",
                               NULL)
  
  dieter.tech.mapping.cost.order <- c(coal = "Coal",
                           OCGT_eff = "OCGT",
                           CCGT = "CCGT",
                           bio = "Biomass",
                           ror = "Hydro",
                           Wind_on = "Wind Onshore",
                           Wind_off = "Wind Offshore",
                           Solar = "Solar",
                           nuc = "Nuclear",
                           NULL)
  
  dieter.tech.mapping.hrly <- c(coal = "Coal",
                                nuc = "Nuclear",
                                OCGT_eff = "OCGT",
                                CCGT = "CCGT",
                                bio = "Biomass",
                                ror = "Hydro",
                                Wind_on = "Wind_Onshore",
                                Wind_off = "Wind_Offshore",
                                Solar = "Solar",
                                elh2 = "Flexible electrolyzers (PtG)",
                                el = "Electricity",
                               lith = "Lithium-ion Battery",
                               PSH = "Pumped_Storage_Hydro",
                               hydrogen = "Hydrogen Storage",
                               caes = "Compressed_Air_Energy_Storage",
                               NULL)
  
  
  running_lcoe_components <- c(
    "Grid Cost",
    "Adjustment Cost",
    "CO2 Tax Cost",
    "OMV Cost",
    "Fuel Cost")
  
  conventionals <- c("Coal", "OCGT", "CCGT", "Biomass")
  renewables <- c("Solar", "Wind Onshore", "Wind Offshore")
  
  
  # label mapping for plots
  dieter.variable.mapping <- c( 
    `DIETER Market value ($/MWh)` = 'Market Value',
    `DIETER Market value with scarcity price ($/MWh)` = 'Market value with scarcity price', 
    `O&M var cost ($/MWh)` = "OMV Cost",
    `O&M fixed cost - avg ($/MWh)` = "OMF Cost",
    `annualized investment cost - avg ($/MWh)` = "Investment Cost",
    `annualized adjustment cost - avg ($/MWh)` = "Adjustment Cost",
    `O&M fixed cost - marg ($/MWh)` = "OMF Cost",
    `annualized investment cost - marg ($/MWh)` = "Investment Cost",              
    `annualized adjustment cost - marg ($/MWh)` = "Adjustment Cost",
    `fuel cost - divided by eta ($/MWh)` = "Fuel Cost",
    `CO2 cost ($/MWh)` = "CO2 Tax Cost",
    `grid cost ($/MWh)` = "Grid Cost",
    `price for total demand ($/MWh)` = 'DIETER annual average electricity price',
    `price for total demand - with scarcity price ($/MWh)` = 'DIETER annual average electricity price with scarcity price',
    # `total system shadow price of cap bound w/ grid - avg ($/MWh)` = 'DIETER shadow price due to capacity constraint from REMIND (with grid)',
    `total system shadow price of cap bound - avg ($/MWh)` = 'DIETER shadow price due to capacity constraint from REMIND',
    `total system shadow price of cap bound - marg ($/MWh)` = 'DIETER shadow price due to capacity constraint from REMIND',
    `shadow price of capacity bound from REMIND - marg ($/MWh)` = 'Shadow Price',
    `shadow price of capacity bound from REMIND - avg ($/MWh)` = 'Shadow Price',
    `Total Markup` = 'Total Markup',
    NULL)
  
  dieter.hourly.variables <- c( 
    "generation (GWh)",
    "curtailment renewable (GWh)",
    "storage generation (GWh)",
    "storage loading (GWh)",
    "consumption (GWh)",
    NULL)
  
  
  # color mapping
  cost.colors_DT.bar <- c(
    # "Curtailment Cost" = "darkblue",
    "Storage Cost" = "darkorchid",
    "Grid Cost" = "darkolivegreen3",
    # "CO2 Provision Cost" = "red",
    "CO2 Tax Cost" = "indianred",
    "OMV Cost" = "cyan",
    "OMF Cost" = "darkcyan",
    "Investment Cost" = "deepskyblue2",
    "Fuel Cost" = "orange3",
    # "Market Value" = "violet",
    "Market value with scarcity price" = "violet",
    "Adjustment Cost" = "orange2",
    "Shadow Price" = "lightblue")
  
  cost.colors_DT.line <- c(
    # "Curtailment Cost" = "darkblue",
    "Storage Cost" = "darkorchid",
    "Grid Cost" = "darkolivegreen3",
    # "CO2 Provision Cost" = "red",
    "CO2 Tax Cost" = "indianred",
    "OMV Cost" = "cyan",
    "OMF Cost" = "darkcyan",
    "Investment Cost" = "deepskyblue2",
    "Fuel Cost" = "orange3",
    # "Market Value" = "violet",
    # "Market value with scarcity price" = "violet",
    "Adjustment Cost" = "orange2",
    # "Shadow Price" = "lightblue",
    NULL
  )
  
  cost.colors_DT_running <- c(
    "Grid Cost" = "darkolivegreen3",
    "CO2 Tax Cost" = "indianred",
    "OMV Cost" = "cyan",
    "OMF Cost" = "darkcyan",
    "Investment Cost" = "deepskyblue2",
    "Fuel Cost" = "orange3",
    "Adjustment Cost" = "darkorchid"
    # ,
  )
  
  label.price <- c("Market Value", 
                   "Shadow Price", 
                   "Market value with scarcity price",
                   NULL)
  
  price.colors <- c(
    # "REMIND Price" = "darkblue",
    "REMIND price moving average" = "darkorchid",
    "REMIND price + shadow price (historical and peak load bound on cap.)" = "#ff0090",
    # "Total (marginal) LCOE + Markup" = "darkorchid",
    # "DIETER annual average electricity price" = "darkcyan",
    "DIETER annual average electricity price with scarcity price" = "#8DB600",
    "DIETER annual average electricity price with scarcity price + shadow price" = "violet",
    # 'DIETER shadow price due to capacity constraint from REMIND' = "DodgerBlue4",
    NULL
  )
  
  dieter.report.mv <- c("DIETER Market value with scarcity price ($/MWh)", 
                        "DIETER Market value ($/MWh)",
                        NULL)
  
  color.mapping.RLDC.basic <- c("CCGT" = "#999959", "Coal" = "#0c0c0c", 
                                "Solar" = "#ffcc00", "Wind" = "#337fff", "Biomass" = "#005900",
                                "OCGT" = "#e51900", "Nuclear" = "#ff33ff","Hydro" = "#191999", 
                                NULL)
  
  color.mapping.RLDC.fancy <- c("CCGT" = "#999959", "Coal" = "#0c0c0c", 
                                "Solar" = "#ffcc00", "Wind" = "#337fff", "Biomass" = "#005900",
                                "OCGT" = "#e51900", "Nuclear" = "#ff33ff","Hydro" = "#191999", 
                                # "Wind Onshore" = "#337fff", 
                                # "Wind Offshore" = "#334cff",
                                "Flexible electrolyzers (PtG)" = "#66cccc", "Electricity" = "red", 
                                "Lithium-ion Battery" ="cyan",
                                "Pumped Storage Hydro" ="#D55E00",
                                "Hydrogen Storage" = "#56B4E9",
                                "Compressed Air Energy Storage" =  "#CC79A7",
                                NULL)
  
  dieter.RLDC.variables <- c( 
    "generation (GWh)",
    "curtailment renewable (GWh)",
    "storage generation (GWh)",
    "storage loading (GWh)",
    "consumption (GWh)",
    NULL)
  
  dieter.runningcost.variables = c("fuel cost - divided by eta ($/MWh)","CO2 cost ($/MWh)","O&M var cost ($/MWh)")
  
  dieter.dispatch.tech = c("CCGT", "coal","bio", "OCGT_eff", "nuc")
  dieter.dispatch.tech.whyd = c("CCGT", "coal","bio", "OCGT_eff", "nuc","ror")
  
  
  #price duration curve color
  color.mapping.PDC <- c("CCGT" = "#999959", 
                     "Coal" = "#0c0c0c",
                     "Biomass" = "#005900",
                     "OCGT" = "#e51900",
                     "Nuclear" = "#ff33ff",
                     "Flexible electrolyzers (PtG)" = "#48D1CC")
  
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
  
###  Coupled model plots (annual resolution)
  
  # Capacity factors --------------------------------------------------------

  # Capacities --------------------------------------------------------------

  source(file.path(dieter.scripts.folder, "plotCapAndCF.R"), local=TRUE)
#
#   # Generation --------------------------------------------------------------
#
  source(file.path(dieter.scripts.folder, "plotGeneration.R"), local=TRUE)
#
#   # Added capacities --------------------------------------------------------
#
  source(file.path(dieter.scripts.folder, "plotAddedCapacities.R"), local=TRUE)
#
#   # Price: Secondary electricity --------------------------------------------
#
  # this needs plotCapAndCF.R
  source(file.path(dieter.scripts.folder, "plotPrice.R"), local=TRUE)
#
#   # Price: primary energy and fuel --------------------------------------------
#
  source(file.path(dieter.scripts.folder, "plotFuelPriceAndTrade.R"), local=TRUE)
#
#   # Market value and shadow price ------------------------------------------------------
#
  # this needs plotPrice.R
  source(file.path(dieter.scripts.folder, "plotMarketValuePrice.R"), local=TRUE)
#
#   # Market value and shadow price ------------------------------------------------------
#
  # this needs plotPrice.R & plotMarketValuePrice.R
  source(file.path(dieter.scripts.folder, "plotConvergence.R"), local=TRUE)
#
#   # LCOEs -------------------------------------------------------------------
#
  # this needs plotMarketValuePrice.R, plotCapAndCF.R
  source(file.path(dieter.scripts.folder, "plotLCOEs.R"), local=TRUE)

  # (Residual) load duration curves -----------------------------------------
  
  source(file.path(dieter.scripts.folder, "plotRLDC_REMIND.R"), local=TRUE) 
  
###  DIETER plots (hourly resolution)
  
  # (Residual) load duration curves -----------------------------------------

  source(file.path(dieter.scripts.folder, "plotRLDC_DIETER.R"), local=TRUE) 

  # Price duration curves ---------------------------------------------------

  source(file.path(dieter.scripts.folder, "plotPriceDurationCurve.R"), local=TRUE)

  # (Inverse) screening curves ----------------------------------------------

  # source(file.path(dieter.scripts.folder, "plotInverseScreeningCurve.R"), local=TRUE)
  
# Figures for GMD paper ---------------------------------------------------
  
  # source(file.path(dieter.scripts.folder, "plotPaperFigures.R"), local = TRUE)
  
  # Close LaTeX PDF ---------------------------------------------------------

  swclose(sw)
}

  