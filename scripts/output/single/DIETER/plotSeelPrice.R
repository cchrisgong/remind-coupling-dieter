# Data preparation --------------------------------------------------------

cat("Plot Seel price \n")

out.remind.seel <- NULL
out.pm.SEPrice <- NULL
out.p32.seelUsableDem <- NULL

for (i in 1:length(remind.files)){
  remind.q32_balSe <- file.path(outputdir, remind.files[i]) %>% 
    read.gdx("q32_balSe", field="m", squeeze=F) %>% 
    filter(all_regi == "DEU") %>%
    select(!all_regi) %>% 
    filter(ttot %in% report.periods) %>% 
    select(!all_enty) %>%
    rename(tall = ttot) %>% 
    rename(q32_balSe.m = m)
  
  remind.qm_budget <- file.path(outputdir, remind.files[i]) %>% 
    read.gdx("qm_budget", field="m", squeeze=F) %>% 
    filter(all_regi == "DEU") %>%
    select(!all_regi) %>%  
    filter(ttot %in% report.periods) %>% 
    rename(tall = ttot) %>% 
    rename(qm_budget.m = m)
  
  remind.seel <- left_join(remind.q32_balSe, remind.qm_budget) %>% 
    mutate(seel.price = 1.2 * 1e12 / sm_TWa_2_MWh * q32_balSe.m / qm_budget.m) %>%  # (10^12 2005$)/TWa -> 2015$/MWh
    mutate(iteration = i)
  
  out.remind.seel <- rbind(out.remind.seel, remind.seel)
  
  
  pm.SEPrice <- file.path(outputdir, remind.files[i]) %>% 
    read.gdx( "pm_SEPrice", squeeze = FALSE) %>% 
    filter(all_enty == "seel") %>%
    filter(all_regi == "DEU") %>% 
    mutate(value = value * 1e12 / sm_TWa_2_MWh * 1.2)  %>%  # (10^12 2005$)/TWa -> 2015$/MWh
    mutate(iteration = i) %>%
    mutate(period=ttot) %>%
    mutate(model = "REMIND secondary electricity price ($/MWh)")
  
  p32.seelUsableDem <- file.path(outputdir, remind.files[i]) %>% 
    read.gdx( "p32_seelUsableDem", factor = FALSE)  %>% 
    filter(all_regi == "DEU") %>%
    filter(all_enty == "seel") %>%
    select(period=ttot,value) %>% 
    mutate(value = value *sm_TWa_2_MWh/1e6)  %>%
    mutate(iteration = i)  %>%
    mutate(legend = "total demand")
  
  out.pm.SEPrice <- rbind(out.pm.SEPrice,pm.SEPrice)
  out.p32.seelUsableDem <- rbind(out.p32.seelUsableDem,p32.seelUsableDem)
}
custColors <- c("CCGT" = "#999959", "lignite" = "#0c0c0c", "Coal" = "#0c0c0c", "Solar" = "#ffcc00", "Wind" = "#337fff", "Biomass" = "#005900", "OCGT" = "#e51900", "Hydro" =  "#191999", "Nuclear" =  "#ff33ff", "hard coal" = "#808080", "REMIND secondary electricity price ($/MWh)" = "#FFA500", "Electrolyzers" = "#48D1CC", "Electricity (stationary)" = "#7F7FFF")


# Plotting ----------------------------------------------------------------

swlatex(sw,"\\onecolumn")
swlatex(sw, paste0("\\section{REMIND secondary electricity price}"))

swlatex(sw, paste0("\\subsection{Secondary electricity (seel) price }"))

p1<-ggplot() +
  geom_line(data = out.pm.SEPrice, aes(x = iteration, y = value, color = "red"), show.legend = FALSE,size = 1.2, alpha = 0.5) +
 # theme(axis.text=element_text(), axis.title=element_text()) +
  xlab("iteration") + ylab(paste0("REMIND secondary electricity price ($/MWh)"))  +
  coord_cartesian(ylim = c(-20,200))+
  facet_wrap(~period, nrow = 3)

swfigure(sw,print,p1,sw_option="width=20, height=12")

swlatex(sw,"\\onecolumn")
swlatex(sw, paste0("\\subsection{Secondary electricity (seel) price + total demand }"))

secAxisScale1 = 0.4

p5<-ggplot() +
  geom_line(data = out.pm.SEPrice, aes(x = iteration, y = value, color = model), size = 1.2, alpha = 0.5) +
  geom_line(data = out.p32.seelUsableDem, aes(x = iteration, y = as.integer(value) * secAxisScale1 , color =legend), size = 1.2, alpha = 0.5) +
 # theme(axis.text=element_text(), axis.title=element_text()) +
  scale_y_continuous(sec.axis = sec_axis(~./secAxisScale1, name = paste0("total demand ", "(TWh)")))+
  xlab("iteration") + ylab(paste0("REMIND secondary electricity price ($/MWh)"))  +
  coord_cartesian(ylim = c(-20,500))+
  facet_wrap(~period, nrow = 3)

swfigure(sw,print,p5,sw_option="width=20, height=12")

swlatex(sw,"\\onecolumn")
swlatex(sw, paste0("\\subsection{Secondary electricity (seel) price + capacity factors}"))


secAxisScale = 1
out.pm.SEPrice <- out.pm.SEPrice %>% mutate(tech=model)
p2<-ggplot() +
  geom_line(data = out.pm.SEPrice, aes(x = iteration, y = value, color = tech), size = 2, alpha = 0.5) +
  geom_line(data = out.remind.capfac, aes(x = iter, y = value, color = tech), size = 1, alpha = 0.5) +
  scale_y_continuous(sec.axis = sec_axis(~./secAxisScale, name = paste0("CF", "(%)")))+
 scale_color_manual(name = "tech", values = custColors) +
  theme(axis.text=element_text(), axis.title=element_text()) +
  xlab("iteration") + ylab(paste0("REMIND secondary electricity price ", "[$/MWh]"))  +
  coord_cartesian(ylim = c(-20,200))+
  facet_wrap(~period, nrow = 3)

swfigure(sw,print,p2,sw_option="width=20, height=12")


swlatex(sw, paste0("\\subsection{Seel price and capacity factor over time (last iteration)}"))

plot.remind.seel <- out.pm.SEPrice %>% 
  filter(iteration == max(iteration))

plot.remind.capfac <- out.remind.capfac %>% 
  filter(iter == max(iter))

p <- ggplot() + 
  geom_line(data=plot.remind.seel, aes(x=period, y=value, color = tech), size = 2, alpha = 0.5) +
 scale_size_manual(name="Shadow price", values=1) + 
  geom_line(data=plot.remind.capfac, aes(x=period, y=value, colour=tech)) +
  scale_colour_manual(name = "tech", values = custColors) +
  scale_y_continuous(name="REMIND secondary electricity price [$/MWh]", limits=c(-120,250), sec.axis = sec_axis(~./1, name = paste0("CF", "(%)"))) +
    xlab("Time")

swfigure(sw,print,p,sw_option="width=20, height=12")