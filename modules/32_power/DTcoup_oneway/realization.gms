*** |  (C) 2006-2019 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de
*** SOF ./modules/32_power/DTcoup/realization.gms

*' @description  
*'
*'The `DTcoup` realization (Dieter Coupling) 
*'
*' @authors Robert Pietzcker, Falko Ueckerdt, Renato Rodrigues, Chris Gong

*####################### R SECTION START (PHASES) ##############################
$Ifi "%phase%" == "declarations" $include "./modules/32_power/DTcoup/declarations.gms"
$Ifi "%phase%" == "datainput" $include "./modules/32_power/DTcoup/datainput.gms"
$Ifi "%phase%" == "equations" $include "./modules/32_power/DTcoup/equations.gms"
$Ifi "%phase%" == "bounds" $include "./modules/32_power/DTcoup/bounds.gms"
*######################## R SECTION END (PHASES) ###############################

*** EOF ./modules/32_power/DTcoup/realization.gms