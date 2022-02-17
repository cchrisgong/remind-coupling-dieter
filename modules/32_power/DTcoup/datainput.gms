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

************************************************************************************************
**************************************** DIETER coupling ***************************************
************************************************************************************************

$IFTHEN.DTcoup %cm_DTcoup% == "on"
p32_minVF_spv = 0.1;
sm32_iter = 0;  !!initialize REMIND iteration scalar
sm32_DTiter = 1; !!the iteration of REMIND when DIETER is first coupled

*** initiating parameters for first iteration of DIETER based on input.gdx
** loading variable directly without .l

Execute_Loadpoint 'input' q_balPe.m = q_balPe.m;
Execute_Loadpoint 'input' qm_budget.m = qm_budget.m;
Execute_Loadpoint 'input' v32_storloss = v32_storloss;
Execute_Loadpoint 'input' vm_prodSe = vm_prodSe;
Execute_Loadpoint 'input' vm_cap = vm_cap;
Execute_Loadpoint 'input' vm_usableSe = vm_usableSe;
Execute_Loadpoint 'input' vm_usableSeTe = vm_usableSeTe;
**CG: can load a flat "historical CO2 tax" here, however, once loaded this parameter won't be updated by REMIND
***(because it will overwrite f21_taxCO2eqHist in 21/datainput); in the future,
***if we do not iterate DIETER from input.gdx, we can simply read in pm_taxCO2eq
*Execute_Loadpoint 'input' f21_taxCO2eqHist = f21_taxCO2eqHist;
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
Execute_Loadpoint 'input' vm_cons = vm_cons;
Execute_Loadpoint 'input' pm_pop = pm_pop;
Execute_Loadpoint 'input' pm_ttot_val = pm_ttot_val;
Execute_Loadpoint 'input' pm_prtp = pm_prtp;
Execute_Loadpoint 'input' o_margAdjCostInv = o_margAdjCostInv;
Execute_Loadpoint 'input' pm_priceCO2 = pm_priceCO2;
*Execute_Loadpoint 'input' p_teAnnuity = p_teAnnuity;

p32_realCapfacVRE(t,regi,teVRE)$(vm_cap.l(t,regi,teVRE,"1"))
    = ( sum(pe2se(enty,"seel",teVRE), vm_prodSe.l(t,regi,enty,"seel",teVRE)) - v32_storloss.l(t,regi,teVRE) )
    / vm_cap.l(t,regi,teVRE,"1") *100;

*** CG: calculate share of dispatched generations for postsolve
p32_usableSeDisp(t,regi,entySe)$(regDTCoup(regi) AND sameas(entySe,"seel")) =
  sum(pe2se(enty,entySe,te), vm_prodSe.l(t,regi,enty,entySe,te)$(teDTCoupSupp(te)) )
	+ sum(se2se(enty,entySe,te), vm_prodSe.l(t,regi,enty,entySe,te)$(teDTCoupSupp(te)) )
	- sum(te, v32_storloss.l(t,regi,te)$(teDTCoupSupp(te) AND teVRE(te)) )
;
p32_usableSeTeDisp(t,regi,entySe,te)$(regDTCoup(regi) AND sameas(entySe,"seel")) =
 	sum(pe2se(enty,entySe,te), vm_prodSe.l(t,regi,enty,entySe,te)$teDTCoupSupp(te) )
	+ sum(se2se(enty,entySe,te), vm_prodSe.l(t,regi,enty,entySe,te)$teDTCoupSupp(te) )
 	- v32_storloss.l(t,regi,te)$(teDTCoupSupp(te) AND teVRE(te))
;
***p32_shSeElDisp is needed for downscaling generation shares in presolve.gms
p32_shSeElDisp(t,regi,te)$(tDT32(t) AND regDTCoup(regi) AND teDTCoupSupp(te)) =
  p32_usableSeTeDisp(t,regi,"seel",te) / p32_usableSeDisp(t,regi,"seel") *100
;


***CG: time-dependent interest rate for DIETER (Marian's formula in core/postsolve)
p32_r4DT(ttot,regi)$(tDT32s2(ttot) AND regDTCoup(regi))
    = (( (vm_cons.l(ttot+1,regi)/pm_pop(ttot+1,regi)) /
      (vm_cons.l(ttot-1,regi)/pm_pop(ttot-1,regi)) )
      ** (1 / ( pm_ttot_val(ttot+1)- pm_ttot_val(ttot-1))) - 1) + pm_prtp(regi);

*** CG: since we would like to couple all years to limit distortions, but growth rate after 2100 is weird (2130 has negative growth rate) due to various artefact, we simply set interest rates
*** after 2100 to 5%, this only sets 2110, 2130, 2150 three years
p32_r4DT(ttot,regi)$((ttot.val gt 2100) AND regDTCoup(regi)) = 0.05;

***CG: passing the CO2 price to DIETER
p32_CO2price4DT(t,regi)$((t.val le 2020) AND regDTCoup(regi)) = f21_taxCO2eqHist(t,regi);

$IFTHEN.Base_Cprice %carbonprice% == "none"
*** CG: updating CO2 price from REMIND to DIETER
p32_CO2price4DT(t,regi)$((t.val gt 2020) AND regDTCoup(regi)) = cm_DTcoup_flatco2;
$ENDIF.Base_Cprice

$IFTHEN.Base_Cprice not %carbonprice% == "none"
*** CG: updating CO2 price from REMIND to DIETER
p32_CO2price4DT(t,regi)$((t.val gt 2020) AND regDTCoup(regi)) = pm_priceCO2(t,regi)/sm_C_2_CO2;
$ENDIF.Base_Cprice


* calculate fuel prices (only prices in REMIND in the form of marginals need to be divided by qm_budget.m)
p32_fuelprice_curriter(t,regi,entyPe)$(regDTCoup(regi) AND (abs(q_balPe.m(t,regi,entyPe)) gt sm_eps) AND (abs(qm_budget.m(t,regi)) gt sm_eps)) =
            q_balPe.m(t,regi,entyPe) / qm_budget.m(t,regi);
p32_fuelprice_avgiter(t,regi,entyPe) = p32_fuelprice_curriter(t,regi,entyPe);

*** CG: only for reporting
p32_seelTotDem(t,regi,entySE)$(sameas(entySE,"seel")) = sum(en2en(entySE,enty2,te),vm_demSe.l(t,regi,entySE,enty2,te)$(sameas(entySE,"seel")));
p32_shSeElDem(t,regi,te)$(regDTCoup(regi)) = sum(en2en(enty,enty2,te),vm_demSe.l(t,regi,enty,enty2,te)$(sameas(enty,"seel")))/p32_seelTotDem(t,regi,"seel") *100;

p32_seh2elh2Dem(t,regi,entySE)$(tDT32(t) AND regDTCoup(regi) AND sameas(entySE,"seh2")) = vm_demSe.l(t,regi,"seel","seh2","elh2");

$IFTHEN.hardcap not %cm_DTcapcon% == "hard"
s32_hardcap = 0;
$ENDIF.hardcap

$IFTHEN.hardcap %cm_DTcapcon% == "hard"
s32_hardcap = 1;
$ENDIF.hardcap

s32_mrkupCoup = 0;
$IFTHEN.hardcap %cm_DTmrkup% == "on"
s32_mrkupCoup = 1;
$ENDIF.hardcap

*** dumping REMIND input for DIETER iteration
*** CG: export H2 switch
$IFTHEN.elh2_coup %cm_DT_elh2_coup% == "on"
s32_H2switch = 1;
$ENDIF.elh2_coup
$IFTHEN.elh2_coup_off %cm_DT_elh2_coup% == "off"
s32_H2switch = 0;
$ENDIF.elh2_coup_off

*** CG: export validation switch
$IFTHEN.valid %cm_DTmode% == "validation"
s32_DTcoupModeswitch = 0;
$ENDIF.valid
$IFTHEN.dispatch %cm_DTmode% == "dispatch"
s32_DTcoupModeswitch = 1;
$ENDIF.dispatch
$IFTHEN.nobound %cm_DTmode% == "none"
s32_DTcoupModeswitch = 2;
$ENDIF.nobound

*** CG: export wind offshore switch
$IFTHEN.noWindOff %cm_wind_offshore% == "0"
s32_windoff = 0;
$ENDIF.noWindOff
$IFTHEN.windOff %cm_wind_offshore% == "1"
s32_windoff = 1;
$ENDIF.windOff

*** CG: export REMIND capacity constraint bound to DIETER, if REMIND has no such constraint on capacity due to residual
** hourly demands, DIETER needs to export to REMIND a market value that includes the scarcity price
$IFTHEN.nocapcon %cm_DTcapcon% == "none"
s32_scarPrice = 0;  !!do not shave off scarcity price
$ENDIF.nocapcon
$IFTHEN.capcon not %cm_DTcapcon% == "none"
s32_scarPrice = 1;  !! shave off scarcity price
$ENDIF.capcon
*** initiating other parameters for averaging in loop

p32_fuelprice_lastiter(t,regi,entyPe)$(regDTCoup(regi)) = p32_fuelprice_curriter(t,regi,entyPe);

p32_cf_last_iter(t,regi,te)$(tDT32(t) AND regDTCoup(regi) AND teDTCoupSupp(te)) = pm_cf(t,regi,te);

$IFTHEN.curt_avg %cm_DTcurt_avg% == "on"
p32_DIETERCurtRatioLaIter(t,regi,"spv")$(tDT32(t) AND regDTCoup(regi)) = v32_storloss.l(t,regi,"spv")/(vm_usableSeTe.l(t,regi,"seel","spv")+sm_eps);
p32_DIETERCurtRatioLaIter(t,regi,"wind")$(tDT32(t) AND regDTCoup(regi)) = v32_storloss.l(t,regi,"wind")/(vm_usableSeTe.l(t,regi,"seel","wind")+sm_eps);
$IFTHEN.WindOff %cm_wind_offshore% == "1"
p32_DIETERCurtRatioLaIter(t,regi,"windoff")$(tDT32(t) AND regDTCoup(regi)) = v32_storloss.l(t,regi,"windoff")/(vm_usableSeTe.l(t,regi,"seel","windoff")+sm_eps);
$ENDIF.WindOff
$ENDIF.curt_avg

* REMIND data for DIETER
execute_unload "RMdata_4DT.gdx",tDT32,regDTCoup,sm32_iter, !! basic info: coupled time and regions, iteration number,
    s32_H2switch,s32_DTcoupModeswitch,cm_DT_dispatch_i1,cm_DT_dispatch_i2,s32_windoff,s32_scarPrice, !! switches: H2 switch, mode switch, dispatch iterational switches, offshore switch
    COALte32,NonPeakGASte32,BIOte32,NUCte32,REMINDte4DT32,      !! tech sets: REMIND technology definition
    vm_cap, vm_deltaCap, vm_capDistr, v32_storloss,vm_capEarlyReti,vm_prodSe,vm_usableSeTe, !! quantities: capacity, generation, curtailment,
    p32_realCapfacVRE,vm_capFac,pm_cf,pm_dataren, !! CF
    p32_usableSeDisp,p32_seh2elh2Dem, !! total demand
    vm_costTeCapital, o_margAdjCostInv, pm_data,fm_dataglob,p32_r4DT, !! capex related tech parameters, interest rate
    pm_dataeta, pm_eta_conv, p32_fuelprice_avgiter, p32_CO2price4DT, fm_dataemiglob, !! running cost related tech parameters
    p32_grid_factor,pm_dt, !! misc
    p32_shSeElDem, p32_shSeElDisp, p32_DIETERCurtRatioLaIter;  !! for reporting first iteration DIETER from input.gdx

put_utility "shell" /
  "cp RMdata_4DT.gdx RMdata_4DT_i0.gdx";

$ENDIF.DTcoup
************************************************************************************************
********************************** END of DIETER coupling **************************************
************************************************************************************************
