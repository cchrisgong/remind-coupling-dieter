# Data preparation --------------------------------------------------------

cat("Plot RLDCs \n")

# Order of technologies in RLDC plot
rldc.order <-
  c("Nuclear",
    "Hydro",
    "Coal",
    "Biomass",
    "CCGT",
    "OCGT")
vre.order <- c("Wind_Onshore", "Wind_Offshore","Solar")
battery.order <- c("Lithium_ion_Battery")
order <- c(rldc.order, vre.order)

# Read in hourly data
hr_data <- file.path(outputdir, dieter.files.report[maxiter]) %>%
  read.gdx("report_tech_hours", factors = FALSE, squeeze = FALSE) %>% 
  select(period = X..2, variable = X..4, tech = X..5, hour = X..6, value) %>% 
  revalue.levels(tech = dieter.tech.mapping.hrly) %>% 
  mutate(hour = as.numeric(str_extract(hour, "[0-9]+"))) %>% 
  mutate(tech = factor(tech, levels = rev(unique(dieter.tech.mapping.hrly))))  %>% 
  # filter(tech == "Biomass") %>%
  filter(variable%in% dieter.hourly.variables) %>% 
  mutate(period = as.numeric(period)) %>% 
  dplyr::group_by(tech,variable) %>%
  complete(period = model.periods, hour = 1:8760, fill = list(value = 0)) %>% 
  dplyr::ungroup(tech,variable) %>% 
  # filter(period == year_toplot) %>% 
  select(period, tech, variable, value,hour) %>% 
  mutate(period = as.numeric(period))

hr_load <- hr_data %>% 
  filter(variable == "consumption (GWh)") %>% 
  filter(tech == "Electricity") %>% 
  select(period,hour, value) %>% 
  dplyr::rename(fixedload.RLDC = value)%>% 
  complete(hour = 1:8760, fill = list(value = 0)) %>% 
  dplyr::group_by(period) %>%
  arrange(desc(fixedload.RLDC)) %>% 
  mutate(fixedload.hour.sorted = seq(1, 8760)) %>% # Descending hour (sorted)
  dplyr::ungroup(period) 

# Read in generation(hour,tech)
hr_generation <-hr_data %>% 
  filter(!variable == "consumption (GWh)") %>% 
  mutate(tech = as.character(tech)) %>% 
  mutate(tech = case_when( # Make curtailment a separate "tech"
    variable == "curtailment renewable (GWh)" ~ paste0(tech, "_curtailed"),
    TRUE ~ tech
  )) %>%
  mutate(tech = case_when( # storage in
    variable == "storage loading (GWh)" ~ paste0(tech, "_in"),
    TRUE ~ tech
  )) %>%
  mutate(tech = case_when( # storage out
    variable == "storage generation (GWh)" ~ paste0(tech, "_out"),
    TRUE ~ tech
  )) %>%
  select(!variable) %>%
  complete(tech, period, hour = 1:8760, fill = list(value = 0))  # Fill up missing hours with 0

# Join both datasets for RLDC calculation
dieter.data <-
  inner_join(hr_load, hr_generation) %>%
  spread(tech, value)

# Calculate RLDCs for solar and wind (with curtailment) and battery
dieter.data <- dieter.data %>%
  mutate(Solar.RLDC = fixedload.RLDC - Solar - Solar_curtailed) %>%
  group_by(period) %>%
  arrange(desc(Solar.RLDC), group_by = T) %>%
  mutate(Solar.hour.sorted = seq(1, 8760)) %>%
  mutate(Wind_Onshore.RLDC = Solar.RLDC - Wind_Onshore - Wind_Onshore_curtailed ) %>%
  arrange(desc(Wind_Onshore.RLDC), group_by = T) %>%
  mutate(Wind_Onshore.hour.sorted = seq(1, 8760)) %>% 
  mutate(Wind_Offshore.RLDC = Wind_Onshore.RLDC - Wind_Offshore - Wind_Offshore_curtailed ) %>%
  arrange(desc(Wind_Offshore.RLDC), group_by = T) %>%
  mutate(Wind_Offshore.hour.sorted = seq(1, 8760)) %>% 
  mutate(Battery.RLDC = Wind_Offshore.RLDC + Lithium_ion_Battery_in - Lithium_ion_Battery_out ) %>%
  arrange(desc(Battery.RLDC), group_by = T) %>%
  mutate(Battery.hour.sorted = seq(1, 8760))

# Calculate RLDCs for dispatchable technologies (without curtailment)
# This loop calculates the RLDC lines, with different x-axes for each technology
vars <- c("Battery", rev(rldc.order))
for (t in 1:(length(vars) - 1)) {
  var1 <- vars[t]
  var2 <- vars[t + 1]
  dieter.data <- dieter.data %>%
    mutate(!!paste0(var2, ".RLDC") := !!sym(paste0(var1, ".RLDC")) - !!sym(var2)) %>%
    arrange(desc(!!sym(paste0(var2, ".RLDC")))) %>%
    mutate(!!paste0(var2, ".hour.sorted") := seq(1, 8760))
}


# Calculate differences between RLDC lines for area plot
# This loop calculates the height of each stacked sorted technology for the same x-axis
vars <- c("demand", rev(order))
for (v in 1:(length(vars) - 1)) {
  var1 <- vars[v]
  var2 <- vars[v + 1]
  for (t in sort(unique(dieter.data$tall))) {
    dieter.temp <- dieter.data %>%
      select(
        tall,
        paste0(var1, ".RLDC"),
        paste0(var1, ".hour.sorted"),
        paste0(var2, ".RLDC"),
        paste0(var2, ".hour.sorted")
      ) %>%
      filter(tall == t)
    
    # Sort both RLDCs from large to small for subsequent subtraction
    m1 <- match(1:8760, dieter.temp[paste0(var1, ".hour.sorted")][[1]])
    m2 <- match(1:8760, dieter.temp[paste0(var2, ".hour.sorted")][[1]])
    rldc1 <- dieter.temp[paste0(var1, ".RLDC")][[1]][m1]
    rldc2 <- dieter.temp[paste0(var2, ".RLDC")][[1]][m2]
    
    # Without curtailment
    rldc.temp <- pmax(rldc1, 0) - pmax(rldc2, 0)
    
    dieter.temp <- dieter.temp %>%
      mutate(value = rldc.temp) %>%
      mutate(hour.sorted = 1:8760) %>%
      mutate(technology = var2) %>%
      select(tall, hour.sorted, technology, value) %>% 
      mutate(iteration = dieter.iter.step*i)
    
    # Calculate curtailment for solar and wind
    if (v %in% c(1, 2)) {
      rldc.temp.curt <- pmin(rldc1, 0) - pmin(rldc2, 0)
      
      dieter.temp.curt <- dieter.temp %>%
        mutate(value = -rldc.temp.curt) %>%
        mutate(technology = paste0(var2, "_curt"))
      
      dieter.temp <- rbind(dieter.temp, dieter.temp.curt)
    }
    
    # Append output
    out.dieter.rldc <- rbind(out.dieter.rldc, dieter.temp)
  }
}

# Plotting ----------------------------------------------------------------

color.mapping.rldc <- c(color.mapping, "Wind_Onshore_curt" = "#66b3ff", "Wind_Offshore_curt" = "#6685ff","Solar_curt" = "#ffd940")

swlatex(sw,"\\onecolumn")
swlatex(sw, paste0("\\section{Residual load duration curves (RLDCs)}"))

  plot.dieter.rldc <- dieter.data %>% 
    mutate(tech = factor(tech, levels=c(rev(order), "Wind_Onshore_curtailed", "WindOffshore_curtailed", "Solar_curt"))) %>%  # Sort technologies for plotting
    filter(hour.sorted %in% c(seq(1,8760,20),8760)) %>%  # Only plot every 20-th hour to decrease file size 
    mutate(value = value/1e3)  # MW -> GW
  
  plot.dieter.demand <- hr_load %>% 
    filter(fixedload.hour.sorted %in% c(seq(1,8760,20),8760)) %>%   # Only plot every 20-th hour to decrease file size
    mutate(fixedload.RLDC = fixedload.RLDC/1e3)  # MW -> GW
  
  swlatex(sw, paste0("\\subsection{RLDCs in last iteration}"))
  
  p <- ggplot() +
    geom_area(data = plot.dieter.rldc, aes(x = hour.sorted, y = value, fill = tech), position = "stack", alpha = 0.8) +
    scale_fill_manual(name = "Technology", values = color.mapping.rldc) +
    geom_line(data = plot.dieter.demand, aes(x = fixedload.hour.sorted, y = fixedload.RLDC, colour = "Demand"), size = 1) +
    scale_colour_manual(name = NULL, values = c("Demand" = "black")) +
    guides(colour = guide_legend(order = 1), fill = guide_legend(order = 2)) +
    xlab("Hours (sorted)") + 
    ylab("Generation [GW]") +
    facet_wrap(~period,ncol=4, scale="free")
  
  swfigure(sw,print,p,sw_option="width=20, height=12")
  
  if (save_png == 1){
    ggsave(filename = paste0(outputdir, "/DIETER/RLDC_lastIter.png"),  p,  width = 30, height =18, units = "in", dpi = 120)
  }
  
