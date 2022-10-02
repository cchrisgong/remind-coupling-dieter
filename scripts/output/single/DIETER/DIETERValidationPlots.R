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
  model.startyear = max(2020, startyear)
  model.periods <- c(seq(model.startyear, 2060, 5), seq(2070, 2100, 10), seq(2110, 2150, 20))
  model.periods.till2100 <- c(seq(model.startyear, 2060, 5), seq(2070, 2100, 10))
  model.periods.till2070 <- c(seq(model.startyear, 2060, 5), 2070)
  model.periods.till2045 <- c(seq(model.startyear, 2045, 5))
  model.periods.RLDC <- c(seq(model.startyear, 2045, 5))
  model.periods.from2020 <- c(seq(2020, 2060, 5), seq(2070, 2100, 10), seq(2110, 2150, 20))
  model.periods.from2020.till2100 <- c(seq(2020, 2060, 5), seq(2070, 2100, 10))
  
  report.periods <- c(seq(2005, 2060, 5), seq(2070, 2100, 10), seq(2110, 2150, 20))

  ## load the number of iteration when coupling starts
  if (length(dieter.files) != 0) {
  start_i = as.numeric(str_extract(dieter.files[2], "[0-9]+"))
  iteration.list = seq(start_i, str_extract(dieter.files[length(dieter.files)], "[0-9]+"),1)
  }
  # load coupled region
  DTcoupreg <- file.path(outputdir, remind.files[2]) %>%  
    read.gdx("regDTcoup", factor = FALSE) 
  
  reg <- as.character(DTcoupreg)
  
  # load switches
  h2switch <- cfg$gms$cm_DT_elh2_coup
  storswitch <- cfg$gms$cm_DTstor
  coupMode <- cfg$gms$cm_DTmode
  policyMode <- cfg$gms$cm_emiscen  # 1= baseline, 9= policy (others see default.cfg in REMIND)

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
  
  remind.vre.mapping <- c(wind = "Wind onshore",
                          windoff = "Wind offshore",
                          spv = "Solar")
  
  remind.sector.coupling.mapping <- c(seel = "Electricity demand",
                                      elh2 = "Electrolyzers for PtG",
                                      tdels = "Stationary Electricity",
                                      tdelt = "Transport Electricity")
 
  remind.grid.mapping <- c(gridwindoff = "VRE Grid")
  
  remind.sector.coupling.mapping.narrow <- c(elh2 = "Electrolyzers for PtG")
  
  remind.sector.coupling.mapping.exclude <- c(tdels = "Stationary Electricity",
                                              tdelt = "Transport Electricity")

  remind.storage.mapping.narrow <- c(storspv = "Lithium-ion battery",
                                     storcsp = "Electrolyzers for long-term storage")
  
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
  
  remind.tech.mapping.narrow.wsto.welh2 <- c(remind.tech.mapping.narrow,
                                            remind.storage.mapping.narrow,
                                            remind.sector.coupling.mapping.narrow,
                                            NULL)
  
  remind.tech.storloss.mapping <-  c(wind = "Wind onshore curtailment",
                                     windoff = "Wind offshore curtailment",
                                     spv = "Solar curtailment")
  
  remind.tech.mapping.narrow.wh2turb <- c(remind.nonvre.mapping, 
                                          h2turbVRE ="Hydrogen turbine",
                                          hydro = "Hydro",
                                          remind.vre.mapping,
                                          NULL)             
  
 dieter.tech.curt.mapping <-  c("Wind onshore" = "Wind onshore curtailment",
                                "Wind offshore" = "Wind offshore curtailment",
                                "Solar" = "Solar curtailment")
  
  ############### DIETER #########################
  table_ordered_name = c("Coal", "CCGT", "Solar", "Wind onshore", "Wind offshore", "Biomass", "OCGT", "Hydro", "Nuclear","Electrolyzers for PtG")
  
  table_ordered_name_dem = c("Electrolyzers for PtG","Electricity demand")
  
  dieter.tech.exclude <- c("OCGT_ineff")
  
  dieter.supply.tech.mapping <- c(coal = "Coal",
                                  nuc = "Nuclear",
                                  OCGT_eff = "OCGT",
                                  CCGT = "CCGT",
                                  bio = "Biomass",
                                  ror = "Hydro",
                                  Wind_on = "Wind onshore",
                                  Wind_off = "Wind offshore",
                                  Solar = "Solar",
                                  NULL)
  
  dieter.demand.tech.mapping <- c(el = "Electricity demand",
                                  elh2 = "Electrolyzers for PtG")
  
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
  
  vre.names <- c("Hydro","Wind onshore","Wind offshore", "Solar")
  nonvre.names <- c("Coal", "Nuclear","OCGT","CCGT","Biomass","Electrolyzers for PtG")
  
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
                  "hydro" = "Hydro", "ngcc" = "Gas","pc" = "Coal", "system" = "System", "elh2" = "Electrolyzers for PtG")
  
## define color mapping 
  

    color.mapping <- c("CCGT" = "#999959", "Coal" = "#0c0c0c",
                       "Solar" = "#ffcc00", "Wind onshore" = "#337fff", 
                       "Wind offshore" = "#334cff", "Biomass" = "#005900",
                       "OCGT" = "#e51900", "Hydro" = "#191999", "Nuclear" = "#ff33ff",
                       NULL)
  
  if (h2switch == "on"){
    color.mapping <- c(color.mapping,
                       "Electrolyzers for PtG" = "darksalmon", "Electricity demand" = "#6495ED",
                       NULL)
  }
  
  color.mapping.cap <- c("CCGT" = "#999959", "Coal" = "#0c0c0c",
                     "Solar" = "#ffcc00", "Wind onshore" = "#337fff", 
                     "Wind offshore" = "#334cff", "Biomass" = "#005900",
                     "OCGT" = "#e51900", "Hydro" = "#191999", "Nuclear" = "#ff33ff",
                     NULL)
  
  if (h2switch == "on"){
    color.mapping.cap <- c(color.mapping.cap,
                       "Electrolyzers for PtG" = "darksalmon", 
                       NULL)
  }
  
  if (storswitch == "on"){
    color.mapping.cap <- c(color.mapping.cap,
                           "Lithium-ion battery" = "seagreen3", 
                           "Electrolyzers for long-term storage" = "paleturquoise1",
                           "Hydrogen turbine" = "#e3c8fa",
                           NULL)
  }
  
  color.mapping.cap.paper <- c("Lithium-ion battery" = "seagreen3", 
                               "Electrolyzers for long-term storage" = "paleturquoise1",
                               "Electrolyzers for PtG" = "darksalmon", 
                               "Solar" = "#ffcc00", 
                               "Wind offshore" = "#334cff",
                               "Wind onshore" = "#337fff", 
                               "Nuclear" = "#ff33ff",
                               "Biomass" = "#005900",
                               "Coal" = "#0c0c0c",
                               "Hydro" = "#191999",
                               "CCGT" = "#999959",
                               "OCGT" = "#e51900",
                               "Hydrogen turbine" = "#e3c8fa",
                               NULL)
                               
  color.mapping.paper <- c("Lithium-ion battery" = "seagreen3", 
                           "Solar" = "#ffcc00", 
                           "Wind offshore" = "#334cff",
                           "Wind onshore" = "#337fff", 
                           "Nuclear" = "#ff33ff",
                           "Biomass" = "#005900",
                           "Coal" = "#0c0c0c",
                           "Hydro" = "#191999",
                           "CCGT" = "#999959",
                           "OCGT" = "#e51900",
                           "Hydrogen turbine" = "#e3c8fa",
                           NULL)
  
  color.mapping_vre <- c("Solar" = "#ffcc00", "Wind onshore" = "#337fff", "Wind offshore" = "#334cff")
  
  color.mapping.cap.line <- c("peak hourly residual demand" = "#0c0c0c")
  
  color.mapping.cap.wh2turb <- c(color.mapping.cap,
                                 "Hydrogen turbine" = "#e3c8fa")
    
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
  
  color.mapping.wloss <- c("CCGT" = "#999959", "Coal" = "#0c0c0c",
                           "Solar" = "#ffcc00", "Wind onshore" = "#337fff", 
                           "Wind offshore" = "#334cff", "Biomass" = "#005900",
                           "OCGT" = "#e51900", "Hydro" = "#191999", "Nuclear" = "#ff33ff",
                           "Wind onshore curtailment" = "#a8c9ff",
                           "Solar curtailment" = "#f7e7a6",
                           "Wind offshore curtailment" = "#919efa",
                           NULL)
  
  color.mapping.cap.wsto.welh2 <- c(color.mapping.cap, 
                                    "Electrolyzers for PtG" = "darksalmon", 
                                    "Lithium-ion battery" = "seagreen3", 
                                    "Electrolyzers for long-term storage" = "paleturquoise1",
                                    "Hydrogen turbine" = "#e3c8fa",
                                    NULL)
  
  linetype.map <- c('DIETER' = 'dotted', 'REMIND' = 'solid')
  
  linetype.map.techZPR.RM <- c('Market value' = 'solid', "Market value + peak demand capacity shadow price (&other)" = 'twodash', "REMIND electricity price" = 'solid')

  linetype.map.techZPR.DT <- c('Market value' = 'solid', "Market value + standing capacity shadow price (&other)" = 'dashed', "DIETER annual avg. electricity price" = 'solid')
  
  color.map.techZPR.RM <- c('Market value' = 'gray24', "Market value + peak demand capacity shadow price (&other)" = 'royalblue2', "REMIND electricity price" = 'darkorchid1')
  
  color.map.techZPR.DT <- c('Market value' = 'gray24', "Market value + standing capacity shadow price (&other)" = 'royalblue2', "DIETER annual avg. electricity price" = 'darkorchid1')
  
  # cost components
  cost.variables <- c(
    "Curtailment Cost" = "Curtailment Cost",
    "Storage Cost" = "Storage Cost",
    "Grid Cost" = "Grid Cost",
    "CCS Cost" = "CCS Cost",
    "CO2 Cost" = "CO2 Price",
    "CO2 Tax Cost" = "CO2 Price",
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
                      "CO2 Price" = "indianred",
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
                    "CO2 Price" = "indianred",
                    "OMV Cost" = "cyan",
                    "OMF Cost" = "darkcyan",
                    "Investment Cost" = "deepskyblue2",
                    "Fuel Cost" = "orange3",
                    "Adjustment Cost" = "darkgoldenrod1",
                    'DIETER annual average electricity price with scarcity price' = "indianred3",
                    'DIETER shadow price due to capacity constraint from REMIND' = "mediumpurple3",
                    'DIETER shadow price due to capacity constraint from REMIND (with grid)' = "mediumpurple2",
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
    # 'total system shadow price of cap bound w/ grid - avg ($/MWh)',
    'total system shadow price of capacity bound - avg ($/MWh)',
    # 'price for fixed demand ($/MWh)'
    NULL
  )
  
  dieter.tech.mapping <- c(coal = "Coal",
                           nuc = "Nuclear",
                           OCGT_eff = "OCGT",
                           CCGT = "CCGT",
                           bio = "Biomass",
                           ror = "Hydro",
                           Wind_on = "Wind onshore",
                           Wind_off = "Wind offshore",
                           Solar = "Solar",
                           elh2 = "Electrolyzers for PtG",
                           el = "Electricity demand",
                           `all Tech` = "All Tech",
                           vregrid = "VRE grid",
                           hydrogen = "Electrolyzers for long-term storage",
                           lith = "Lithium-ion battery",
                           NULL)
  
  dieter.storage.mapping <- c( lith = "Lithium-ion battery",
                               hydrogen = "Electrolyzers for long-term storage",
                               NULL)
  
  dieter.tech.mapping.cost.order <- c(coal = "Coal",
                           OCGT_eff = "OCGT",
                           CCGT = "CCGT",
                           bio = "Biomass",
                           ror = "Hydro",
                           Wind_on = "Wind onshore",
                           Wind_off = "Wind offshore",
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
                                elh2 = "Electrolyzers for PtG",
                                el = "Electricity demand",
                               lith = "Lithium-ion battery",
                               # PSH = "Pumped_Storage_Hydro",
                               hydrogen = "Electrolyzers for long-term storage",
                               # caes = "Compressed_Air_Energy_Storage",
                               NULL)
  
  
  running_lcoe_components <- c(
    "Grid Cost",
    "Adjustment Cost",
    "CO2 Price",
    "OMV Cost",
    "Fuel Cost")
  
  conventionals <- c("Coal", "OCGT", "CCGT", "Biomass")
  renewables <- c("Solar", "Wind onshore", "Wind offshore")
  
  
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
    `CO2 cost ($/MWh)` = "CO2 Price",
    `grid cost ($/MWh)` = "Grid Cost",
    `price for total demand ($/MWh)` = 'DIETER annual average electricity price',
    `price for total demand - with scarcity price ($/MWh)` = 'DIETER annual average electricity price with scarcity price',
    `total system shadow price of cap bound w/ grid - avg ($/MWh)` = 'DIETER shadow price due to capacity constraint from REMIND (with grid)',
    `total system shadow price of capacity bound - avg ($/MWh)` = 'DIETER shadow price due to capacity constraint from REMIND',
    `total system shadow price of capacity bound - marg ($/MWh)` = 'DIETER shadow price due to capacity constraint from REMIND',
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
    "CO2 Price" = "indianred",
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
    "CO2 Price" = "indianred",
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
    "CO2 Price" = "indianred",
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
    # "REMIND electricity Price" = "darkblue",
    "REMIND electricity price" = "darkorchid",
    # "Total (marginal) LCOE + Markup" = "darkorchid",
    "DIETER annual average electricity price" = "darkcyan",
    "DIETER annual average electricity price with scarcity price" = "#8DB600",
    "DIETER annual average electricity price with scarcity price + capacity shadow price" = "violet",
    # 'DIETER shadow price due to capacity constraint from REMIND' = "DodgerBlue4",
    NULL
  )
  
  price.colors.RM.fancy <- c("REMIND electricity price" = "gray24",
                            "REMIND electricity price + capacity shadow price" = "royalblue2",
                          NULL)
  
  price.colors.DT.fancy <- c(
                          "DIETER annual average electricity price with scarcity price" = "gray24",
                          "DIETER annual average electricity price with scarcity price + capacity shadow price" = "royalblue2",                          NULL)
  
  price.colors.fancy <- c(price.colors.RM.fancy,price.colors.DT.fancy)
  
  
  price.colors.DT.fancy.forPaper <- c(
    "DIETER annual avg. electricity price" = "gray24",
    "DIETER annual avg. electricity price + capacity shadow price" = "royalblue2",
    NULL)
  
  mv.colors.DT.fancy.forPaper <- c(
    "DIETER annual avg. electricity price" = "gray24",
    "DIETER annual avg. electricity price + capacity shadow price" = "royalblue2",
    NULL)
  
  
  dieter.report.mv <- c("DIETER Market value with scarcity price ($/MWh)", 
                        "DIETER Market value ($/MWh)",
                        NULL)
  
  color.mapping.RLDC.basic <- c("Wind" = "#337fff", "Solar" = "#ffcc00",
                                "OCGT" = "#e51900", 
                                "CCGT" = "#999959", 
                                "Biomass" = "#005900",
                                "Hydro" = "#191999", 
                                "Nuclear" = "#ff33ff",
                                "Coal" = "#0c0c0c", 
                                NULL)
  
  color.mapping.RLDC.fancy <- c("Wind" = "#337fff", "Solar" = "#ffcc00", 
                                "Lithium-ion battery" ="seagreen3",
                                "Hydrogen turbine" = "#e3c8fa",
                                "OCGT" = "#e51900", 
                                "CCGT" = "#999959",
                                "Biomass" = "#005900",
                                "Hydro" = "#191999", 
                                "Nuclear" = "#ff33ff",
                                "Coal" = "#0c0c0c", 
                                "Electrolyzers for PtG" = "darksalmon", 
                                "Electrolyzers for long-term storage" = "paleturquoise1",
                                # "Pumped Storage Hydro" ="#D55E00",
                                # "Compressed Air Energy Storage" =  "#CC79A7",
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
                     NULL)
  
  if (h2switch == "on"){
    color.mapping.PDC <- c(color.mapping.PDC,
                           "Electrolyzers for PtG" = "darksalmon")
  }
  
  # label mapping for plots
  dieter.variables.hrly.mix <- c( 
    "generation (GWh)",
    "curtailment renewable (GWh)",
    "storage generation (GWh)",
    "storage loading (GWh)",
    "consumption (GWh)",
    NULL)
  
  supply.color.mapping.hrly.mix <- c(
                                     "Wind onshore curtailment" = "#a8c9ff",
                                     "Solar curtailment" = "#f7e7a6",
                                     "Wind offshore curtailment" = "#919efa",
                                     "Solar" = "#ffcc00", 
                                     "Wind onshore" = "#337fff", 
                                     "Wind offshore" = "#334cff",
                                     "Lithium-ion battery" ="seagreen3",
                                     # "Pumped Storage Hydro" ="#D55E00",
                                     "Electrolyzers for long-term storage" = "paleturquoise1",
                                     # "Compressed Air Energy Storage" =  "#CC79A7",
                                     "Wind" = "#337fff", "OCGT" = "#e51900", 
                                     "Biomass" = "#005900",
                                     "CCGT" = "#999959", "Coal" = "#0c0c0c", 
                                     "Hydro" = "#191999", "Nuclear" = "#ff33ff",
                                     "Hydrogen turbine" = "#e3c8fa",
                                     NULL)
  
  demand.color.mapping.hrly.mix <- c("Electrolyzers for PtG" = "#c0660c", "Electricity demand" = "orchid3", 
                                     NULL)
  
  color.mapping.hrly.mix <- c(supply.color.mapping.hrly.mix, demand.color.mapping.hrly.mix)
  
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
  
  source(file.path(dieter.scripts.folder, "plotPaperFigures.R"), local = TRUE)
  
  # Close LaTeX PDF ---------------------------------------------------------

  swclose(sw)
}

  