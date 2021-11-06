*** |  (C) 2006-2020 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de
*------------------------------------------------------------------------------------
***                        DTcoup specific data input
*------------------------------------------------------------------------------------

***parameter p32_shCHP(all_regi,char) - upper boundary of chp electricity generation
parameter f32_shCHP(all_regi,char)  "upper boundary of chp electricity generation"
/
$ondelim
$include "./modules/32_power/DTcoup/input/f32_shCHP.cs4r"
$offdelim
/
;
p32_shCHP(all_regi,char) = f32_shCHP(all_regi,char);


***parameter p32_grid_factor(all_regi) - multiplicative factor that scales total grid requirements down in comparatively small or homogeneous regions like Japan, Europe or India
parameter p32_grid_factor(all_regi)                "multiplicative factor that scales total grid requirements down in comparatively small or homogeneous regions like Japan, Europe or India"
/
$ondelim
$include "./modules/32_power/DTcoup/input/p32_grid_factor.cs4r"
$offdelim
/
;

***parameter p32_factorStorage(all_regi,all_te) - multiplicative factor that scales total curtailment and storage requirements up or down in different regions for different technologies (e.g. down for PV in regions where high solar radiation coincides with high electricity demand)
parameter f32_factorStorage(all_regi,all_te)                  "multiplicative factor that scales total curtailment and storage requirements up or down in different regions for different technologies (e.g. down for PV in regions where high solar radiation coincides with high electricity demand)"
/
$ondelim
$include "./modules/32_power/DTcoup/input/f32_factorStorage.cs4r"
$offdelim
/
;
$IFTHEN.WindOff %cm_wind_offshore% == "1"
f32_factorStorage(all_regi,"windoff") = f32_factorStorage(all_regi,"wind");
f32_factorStorage(all_regi,"wind")      = 1.35 * f32_factorStorage(all_regi,"wind");
$ENDIF.WindOff
p32_factorStorage(all_regi,all_te) = f32_factorStorage(all_regi,all_te);

***INNOPATHS
$if not "%cm_INNOPATHS_storageFactor%" == "off" p32_factorStorage(all_regi,all_te)=%cm_INNOPATHS_storageFactor%*p32_factorStorage(all_regi,all_te);

***parameter p32_storexp(all_regi,all_te) - exponent that determines how curtailment and storage requirements per kW increase with market share of wind and solar. 1 means specific marginal costs increase linearly
p32_storexp(regi,"spv")     = 1;
p32_storexp(regi,"csp")     = 1;
p32_storexp(regi,"wind")    = 1;
$IFTHEN.WindOff %cm_wind_offshore% == "1"
p32_storexp(regi,"windoff")    = 1;
$ENDIF.WindOff


***parameter p32_gridexp(all_regi,all_te) - exponent that determines how grid requirement per kW increases with market share of wind and solar. 1 means specific marginal costs increase linearly
p32_gridexp(regi,"spv")     = 1;
p32_gridexp(regi,"csp")     = 1;
p32_gridexp(regi,"wind")    = 1;


table f32_storageCap(char, all_te)  "multiplicative factor between dummy seel<-->h2 technologies and storXXX technologies"
$include "./modules/32_power/DTcoup/input/f32_storageCap.prn"
;

$IFTHEN.WindOff %cm_wind_offshore% == "1"
f32_storageCap(char,"windoff") = f32_storageCap(char,"wind");
$ENDIF.WindOff

p32_storageCap(te,char) = f32_storageCap(char,te);
display p32_storageCap;

$ontext
parameter p32_flex_maxdiscount(all_regi,all_te) "maximum electricity price discount for flexible technologies reached at high VRE shares"
/
$ondelim
$include "./modules/32_power/DTcoup/input/p32_flex_maxdiscount.cs4r"
$offdelim
/
;
*** convert from USD2015/MWh to trUSD2005/TWa
p32_flex_maxdiscount(regi,te) = p32_flex_maxdiscount(regi,te) * sm_TWa_2_MWh * sm_D2015_2_D2005 * 1e-12;
display p32_flex_maxdiscount;
$offtext

*** initialize p32_PriceDurSlope parameter
p32_PriceDurSlope(regi,"elh2") = cm_PriceDurSlope_elh2;

*Display "end of 32/datainput, pm_cf", pm_cf;

$IFTHEN.DTcoup %cm_DTcoup% == "on"
p32_minVF_spv = 0.1;
** loading variable without .l is ok

Execute_Loadpoint 'input' p32_fuelprice_curriter = q_balPe.m;
Execute_Loadpoint 'input' p32_fuelprice_lastiter = q_balPe.m;
Execute_Loadpoint 'input' p32_fuelprice_avgiter = q_balPe.m;
Execute_Loadpoint 'input' v32_storloss = v32_storloss;
Execute_Loadpoint 'input' vm_prodSe = vm_prodSe;
Execute_Loadpoint 'input' vm_cap = vm_cap;
Execute_Loadpoint 'input' vm_usableSeTe = vm_usableSeTe;
Execute_Loadpoint 'input' f21_taxCO2eqHist = f21_taxCO2eqHist;
Execute_Loadpoint 'input' pm_data = pm_data;
Execute_Loadpoint 'input' vm_costTeCapital = vm_costTeCapital;
Execute_Loadpoint 'input' fm_dataglob = fm_dataglob;
Execute_Loadpoint 'input' pm_dataeta = pm_dataeta;
Execute_Loadpoint 'input' pm_eta_conv = pm_eta_conv;
Execute_Loadpoint 'input' p32_grid_factor = p32_grid_factor;
Execute_Loadpoint 'input' pm_ts = pm_ts;
Execute_Loadpoint 'input' vm_deltaCap = vm_deltaCap;
Execute_Loadpoint 'input' vm_capEarlyReti = vm_capEarlyReti;
Execute_Loadpoint 'input' fm_dataemiglob = fm_dataemiglob;
Execute_Loadpoint 'input' pm_cf = pm_cf;
Execute_Loadpoint 'input' vm_capFac = vm_capFac;
Execute_Loadpoint 'input' pm_dataren = pm_dataren;
Execute_Loadpoint 'input' vm_capDistr = vm_capDistr;
Execute_Loadpoint 'input' p32_shSeEl = v32_shSeEl.l;
Execute_Loadpoint 'input' vm_demSe = vm_demSe;

Display "vm_cap for DIETER datainput", vm_cap.l;

p32_seelUsableProd(t,regi,enty2)$(sameas(enty2,"seel")) = sum( pe2se(enty,enty2,te), vm_prodSe.l(t,regi,enty,enty2,te) )
                                                        + sum(se2se(enty,enty2,te), vm_prodSe.l(t,regi,enty,enty2,te) )
                                                        - sum(teVRE, v32_storloss.l(t,regi,teVRE) )
;

p32_seelUsableDem(t,regi,enty2)$(sameas(enty2,"seel")) = p32_seelUsableProd(t,regi,enty2);
p32_seh2elh2Dem(t,regi,enty)$(regDTCoup(regi) AND sameas(enty,"seh2")) = vm_demSe.l(t,regi,"seel","seh2","elh2");
p32_DIETERCurtRatioLaIter(t,regi,"spv") = v32_storloss.l(t,regi,"spv")/(vm_usableSeTe.l(t,regi,"seel","spv")+sm_eps);
p32_DIETERCurtRatioLaIter(t,regi,"wind") = v32_storloss.l(t,regi,"wind")/(vm_usableSeTe.l(t,regi,"seel","wind")+sm_eps);

execute_unload "RMdata_4DT_input.gdx", vm_cap, p32_seelUsableProd, p32_seh2elh2Dem, p32_fuelprice_curriter,
f21_taxCO2eqHist, pm_data, vm_costTeCapital, vm_prodSe, vm_usableSeTe, fm_dataglob, pm_dataeta, pm_eta_conv, p32_grid_factor,
pm_ts, vm_deltaCap, vm_capEarlyReti, fm_dataemiglob, vm_capFac, pm_dataren, vm_capDistr;
$ENDIF.DTcoup
