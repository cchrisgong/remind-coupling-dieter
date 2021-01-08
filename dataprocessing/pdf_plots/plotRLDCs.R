# Data preparation --------------------------------------------------------

# Order of technologies in RLDC plot
rldc.order <-
  c("Nuclear",
    "Hydro",
    "Lignite",
    "Hard coal",
    "Biomass",
    "CCGT",
    "OCGT")
vre.order <- c("Wind", "Solar")
order <- c(rldc.order, vre.order)

# Initialise output files
out.dieter.demand <- NULL
out.dieter.rldc <- NULL
# Loop over DIETER iterations
for (i in 1:length(dieter.files.report)){
  # Read in demand(hour)
  dieter.report_hours <-
    paste0(remind.dieter.path, scenario.name, dieter.files.report[i]) %>%
    read.gdx("report_hours", squeeze = F) %>%
    rename(tall = X..2, var = X..4, hour = X..5) %>%
    select(!c(X., X..1, X..3)) %>%
    filter(var == "fixed demand") %>%
    select(!var) %>%
    rename(demand.RLDC = value) %>%  # Rename demand for RLDC calculation later
    mutate(hour = as.numeric(substring(hour, 2))) %>%
    group_by(tall) %>%
    arrange(desc(demand.RLDC)) %>%  # Descending order
    mutate(demand.hour.sorted = seq(1, 8760)) %>%  # Descending hour (sorted)
    mutate(iteration = dieter.iter.step*i) %>%
    ungroup()
  
  out.dieter.demand <- rbind(out.dieter.demand, dieter.report_hours)
  
  # Read in generation(hour,tech)
  dieter.report_tech_hours <-
    paste0(remind.dieter.path, scenario.name, dieter.files.report[i]) %>%
    read.gdx("report_tech_hours", squeeze = F) %>%
    rename(tall = X..2, var = X..4, technology = X..5, hour = X..6) %>%
    select(!c(X., X..1, X..3)) %>%
    revalue.levels(technology = dieter.tech.mapping) %>%
    mutate(hour = as.numeric(substring(hour, 2))) %>%
    mutate(technology = case_when(  # Make curtailment a separate "technology"
      var == "curtailment of fluct res" ~ paste0(technology, "_curtailed"),
      TRUE ~ technology
    )) %>%
    select(!var) %>%
    complete(technology, tall, hour = 1:8760, fill = list(value = 0))  # Fill up missing hours with 0
  
  # Join both datasets for RLDC calculation
  dieter.data <-
    inner_join(dieter.report_hours, dieter.report_tech_hours) %>%
    spread(technology, value)
  
  # Calculate RLDCs for solar and wind (with curtailment)
  dieter.data <- dieter.data %>%
    mutate(Solar.RLDC = demand.RLDC - Solar - Solar_curtailed) %>%
    group_by(tall) %>%
    arrange(desc(Solar.RLDC), group_by = T) %>%
    mutate(Solar.hour.sorted = seq(1, 8760)) %>%
    mutate(Wind.RLDC = Solar.RLDC - Wind - Wind_curtailed) %>%
    arrange(desc(Wind.RLDC), group_by = T) %>%
    mutate(Wind.hour.sorted = seq(1, 8760))
  
  # Calculate RLDCs for dispatchable technologies (without curtailment)
  # This loop calculates the RLDC lines, with different x-axes for each technology
  vars <- c("Wind", rev(rldc.order))
  for (t in 1:(length(vars) - 1)) {
    var1 <- vars[t]
    var2 <- vars[t+1]
    dieter.data <- dieter.data %>%
      mutate(!!paste0(var2, ".RLDC") := !!sym(paste0(var1, ".RLDC")) -!!sym(var2)) %>%
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
}

# Plotting ----------------------------------------------------------------

color.mapping.rldc <- c(color.mapping, "Wind_curt" = "#66b3ff", "Solar_curt" = "#ffd940")

swlatex(sw,"\\onecolumn")
swlatex(sw, paste0("\\section{Residual load duration curves (RLDCs)}"))

for(i in unique(out.dieter.rldc$iteration)){
  plot.dieter.rldc <- out.dieter.rldc %>% 
    filter(iteration==i) %>% 
    mutate(technology = factor(technology, levels=c(rev(order), "Wind_curt", "Solar_curt"))) %>%  # Sort technologies for plotting
    filter(hour.sorted %in% c(seq(1,8760,20),8760)) %>%  # Only plot every 20-th hour to decrease file size 
    mutate(value = value/1e3)  # MW -> GW
  
  plot.dieter.demand <- out.dieter.demand %>% 
    filter(iteration==i) %>% 
    filter(demand.hour.sorted %in% c(seq(1,8760,20),8760)) %>%   # Only plot every 20-th hour to decrease file size
    mutate(demand.RLDC = demand.RLDC/1e3)  # MW -> GW
  
  swlatex(sw, paste0("\\subsection{RLDCs in iteration ", i, "}"))
  
  p <- ggplot() +
    geom_area(data = plot.dieter.rldc, aes(x = hour.sorted, y = value, fill = technology), position = "stack", alpha = 0.8) +
    scale_fill_manual(name = "Technology", values = color.mapping.rldc) +
    geom_line(data = plot.dieter.demand, aes(x = demand.hour.sorted, y = demand.RLDC, colour = "Demand"), size = 1) +
    scale_colour_manual(name = NULL, values = c("Demand" = "black")) +
    guides(colour = guide_legend(order = 1), fill = guide_legend(order = 2)) +
    xlab("Hours (sorted)") + 
    ylab("Generation [GW]") +
    facet_wrap(~tall,ncol=4, scale="free")
  
  swfigure(sw,print,p,sw_option="width=20, height=12")
}
