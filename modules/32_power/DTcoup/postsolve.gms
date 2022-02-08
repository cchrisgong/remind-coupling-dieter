*** |  (C) 2006-2020 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de
*** SOF ./modules/32_power/DTcoup/postsolve.gms

* calculate post curtailment "real" VRE capfac
p32_realCapfacVRE(t,regi,teVRE)$(vm_cap.l(t,regi,teVRE,"1"))
    = ( sum(pe2se(enty,"seel",teVRE), vm_prodSe.l(t,regi,enty,"seel",teVRE)) - v32_storloss.l(t,regi,teVRE) )
    / vm_cap.l(t,regi,teVRE,"1") *100;
* calculate pre curtailment "theoretical" VRE capfac
p32_theoCapfacVRE(t,regi,teVRE)$(vm_cap.l(t,regi,teVRE,"1"))
    = sum(pe2se(enty,"seel",teVRE), vm_prodSe.l(t,regi,enty,"seel",teVRE)) / vm_cap.l(t,regi,teVRE,"1") *100;

*** calculation of SE electricity price (for internal use and reporting purposes), excluding 0 cases
pm_SEPrice(t,regi,entySE)$(abs(qm_budget.m(t,regi)) gt sm_eps AND sameas(entySE,"seel")) =
       q32_balSe.m(t,regi,entySE) / qm_budget.m(t,regi);

*** DIETER coupling currently not used in calibration run
$ifthen.calibrate %CES_parameters% == "load"
***************************************************************
*** CG: parameters needed for diagnosis and reporting
***************************************************************

*** CG: market value as seen by REMIND
p32_marketValue(t,regi,te)$(tDT32(t) AND regDTCoup(regi) AND teDTCoupSupp(te))
      = pm_SEPrice(t,regi,"seel")$regDTCoup(regi) + vm_Mrkup.l(t,regi,te)$regDTCoup(regi);
*** CG: market price seen by sector coupling usage of power (e.g. green H2)

p32_marketPrice(t,regi,te)$(tDT32(t) AND regDTCoup(regi) AND teDTCoupSupp(te))
      = pm_SEPrice(t,regi,"seel")$regDTCoup(regi) - vm_flexAdj.l(t,regi,te)$regDTCoup(regi);

*** CG: value factor in REMIND
p32_valueFactor(t,regi,te)$(tDT32(t) AND regDTCoup(regi) AND teDTCoupSupp(te))
      = p32_marketValue(t,regi,te)$regDTCoup(regi)/(pm_SEPrice(t,regi,"seel")$regDTCoup(regi) + sm_eps);

p32_shSeElDisp(t,regi,te)$(tDT32(t) AND regDTCoup(regi) AND teDTCoupSupp(te)) = v32_shSeElDisp.l(t,regi,te);
p32_shSeEl(t,regi,te)$(tDT32(t) AND regDTCoup(regi) AND teDTCoupSupp(te)) = v32_shSeEl.l(t,regi,te);

*** CG: calculate budget from last iteration
p32_budget(t,regi)$(tDT32(t) AND regDTCoup(regi)) = qm_budget.m(t,regi);

*** CG: Total power produced (including curtailment, co-production, own consumption)
p32_totProd(t,regi,enty2)$(sameas(enty2,"seel")) =
* PE to SE transformation
    sum(pe2se(enty,enty2,te), vm_prodSe.l(t,regi,enty,enty2,te) )
* SE to SE transformation
	+ sum(se2se(enty,enty2,te), vm_prodSe.l(t,regi,enty,enty2,te) )
* Coupled production of two types of SE: such as sehe + seel, or seh2 + seel: p32_coupledProd
	+ sum(pc2te(enty,entySE(enty3),te,enty2),
		pm_prodCouple(regi,enty,enty3,te,enty2) * vm_prodSe.l(t,regi,enty,enty3,te) )
* Power used in transporting and distributing FE: p32_prod4dtFE
	+ sum(pc2te(enty4,entyFE(enty5),te,enty2),
		pm_prodCouple(regi,enty4,enty5,te,enty2) * vm_prodFe.l(t,regi,enty4,enty5,te) )
* Power used in CCS: p32_prod4CCS
	+ sum(pc2te(enty,enty3,te,enty2),
		sum(teCCS2rlf(te,rlf), pm_prodCouple(regi,enty,enty3,te,enty2) * vm_co2CCS.l(t,regi,enty,enty3,te,rlf) ) )
;

*** coupled production
p32_coupledProd(t,regi,enty2)$(sameas(enty2,"seel")) = sum(pc2te(entyPe,entySe(enty3),te,enty2),
		pm_prodCouple(regi,entyPe,enty3,te,enty2) * vm_prodSe.l(t,regi,entyPe,enty3,te) );

*** power for d&t of FE
p32_prod4dtFE(t,regi,enty2)$(sameas(enty2,"seel")) = sum(pc2te(enty4,entyFE(enty5),te,enty2),
		pm_prodCouple(regi,enty4,enty5,te,enty2) * vm_prodFe.l(t,regi,enty4,enty5,te) );
*** power for CCS
p32_prod4CCS(t,regi,enty2)$(sameas(enty2,"seel")) = sum(pc2te(enty,enty3,te,enty2),
		sum(teCCS2rlf(te,rlf), pm_prodCouple(regi,enty,enty3,te,enty2) * vm_co2CCS.l(t,regi,enty,enty3,te,rlf) ) );

*** all non SE2SE or PE2SE production terms
p32_nonSEPE2SE(t,regi,enty2)$(sameas(enty2,"seel")) = p32_coupledProd(t,regi,enty2)
    + p32_prod4dtFE(t,regi,enty2) + p32_prod4CCS(t,regi,enty2);

p32_extrEnergyUsage(t,regi,enty2)$(sameas(enty2,"seel")) =
 sum(pe2rlf(enty3,rlf2), (pm_fuExtrOwnCons(regi, enty2, enty3) * vm_fuExtr.l(t,regi,enty3,rlf2))$(pm_fuExtrOwnCons(regi, enty2, enty3) gt 0))$(t.val > 2005) !! do not use in 2005 because this demand is not contained in 05_initialCap
;
*** CG: total curtailment
p32_seelCurt(t,regi) = sum(teVRE, v32_storloss.l(t,regi,teVRE) );

*** total demand: excluding fuel extraction power usage for simplicity (and excluding curtailment)
p32_seelTotDem(t,regi,enty2)$(sameas(enty2,"seel")) =
  sum(se2fe(enty2,enty3,te), vm_demSe.l(t,regi,enty2,enty3,te) )
+ sum(se2se(enty2,enty3,te), vm_demSe.l(t,regi,enty2,enty3,te) )
* own consumption: electricity used for extracting fossil fuel, ususally negative
+ sum(pe2rlf(enty3,rlf2), (pm_fuExtrOwnCons(regi, enty2, enty3) * vm_fuExtr.l(t,regi,enty3,rlf2))$(pm_fuExtrOwnCons(regi, enty2, enty3) gt 0))$(t.val > 2005) !! do not use in 2005 because this demand is not contained in 05_initialCap
;

*** CG: total usable demand to pass on to DIETER: this has to
*** include the electricity consumed for extracting fuels (included in p32_seelTotDem),
*** as well as distributing and transporting them (p32_prod4dtFE) and doing CCS (p32_prod4CCS)
*** and exclude curtailment
*** (note: p32_prod4dtFE and p32_prod4CCS are negative)
*** (we do not consider power trading for now, since vm_Xport and vm_Mport are 0)
p32_seelUsableDem(t,regi,entySE)$(sameas(entySE,"seel")) = p32_seelTotDem(t,regi,entySE)
- p32_prod4dtFE(t,regi,entySE) - p32_prod4CCS(t,regi,entySE) - p32_coupledProd(t,regi,entySE)
;

** total production except co-production term (they might create distortion), subtracted by curtailment from VRE
** sanity check: p32_seelUsableDem should match p32_seelUsableProd according to q32_balSe
p32_seelUsableProd(t,regi,entySE)$(sameas(entySE,"seel")) = sum( pe2se(enty,entySE,te), vm_prodSe.l(t,regi,enty,entySE,te) )
                                                        + sum(se2se(enty,entySE,te), vm_prodSe.l(t,regi,enty,entySE,te) )
                                                      	- sum(teVRE, v32_storloss.l(t,regi,teVRE))
;
*** CG: dispatched part of the demand to be passed on to DIETER
p32_usableSeDisp(t,regi,entySE)$(tDT32(t) AND regDTCoup(regi) AND sameas(entySE,"seel"))
                              = v32_usableSeDisp.l(t,regi,entySE)
;


$IFTHEN.DTcoup %cm_DTcoup% == "on"

*** CG: flexible H2 demand to be passed on to DIETER
** vm_demSe.l(t,regi,"seel","seh2","elh2") is how much electricity is needed to produse seh2 (green h2)
** p32_seh2elh2Dem < p32_seelUsableDem (p32_seh2elh2Dem is part of the total usable demand p32_seelUsableDem)
p32_seh2elh2Dem(t,regi,entySE)$(tDT32(t) AND regDTCoup(regi) AND sameas(entySE,"seh2")) = vm_demSe.l(t,regi,"seel","seh2","elh2");

$IFTHEN.elh2_coup %cm_DT_elh2_coup% == "on"
p32_shSeElDem(t,regi,te)$(regDTCoup(regi)) = v32_shSeElDem.l(t,regi,te);
$ENDIF.elh2_coup

**** CG: DIETER coupling
*###################################################################
*********** INTERFACE FOR READING IN DIETER RESULTS ****************
*###################################################################
*note: !!!! if the coupling is turned on after some iterations of uncoupled REMIND run,
*           then don't use a last fulldata.gdx from a previous run as input
*           (this will messed up the variables and equations that are turning on later)

*** CG: start of DIETER coupling
*sm32_DTiter = 15
*if( ((ord(iteration) ge sm32_DTiter) and ( mod(ord(iteration), 3) eq 0)),

 if( ((ord(iteration) ge sm32_DTiter) and ( mod(ord(iteration), 1) eq 0)),

sm32_iter = iteration.val;
display "DIETER iteration", sm32_iter;

*** CG: fuel cost to be passed on to DIETER
*** sometimes for some reason the marginals of the PE equation is 0
** if condition not satisfied, last iteration values of p32_fuelprice_curriter will be automatically taken
p32_fuelprice_curriter(t,regi,entyPe)$(regDTCoup(regi) AND (abs(q_balPe.m(t,regi,entyPe)) gt sm_eps) AND (abs(qm_budget.m(t,regi)) gt sm_eps)) = q_balPe.m(t,regi,entyPe)/ (qm_budget.m(t,regi));

$IFTHEN.fc_avg %cm_DTfc_avg% == "on"
p32_fuelprice_avgiter(t,regi,entyPe)$(regDTCoup(regi) AND (abs(q_balPe.m(t,regi,entyPe)) gt sm_eps))
          = (p32_fuelprice_curriter(t,regi,entyPe)
    		   + 2 * p32_fuelprice_lastiter(t,regi,entyPe)
    			 + p32_fuelprice_lastx2iter(t,regi,entyPe))
    			 / 4 ;
$ENDIF.fc_avg

$IFTHEN.fc_avg %cm_DTfc_avg% == "off"
p32_fuelprice_avgiter(t,regi,entyPe)$(regDTCoup(regi) AND (abs(q_balPe.m(t,regi,entyPe)) gt sm_eps))
          = p32_fuelprice_curriter(t,regi,entyPe);
$ENDIF.fc_avg

*** CG: demand averaging to be passed on to DIETER
$IFTHEN.dem_avg %cm_DTdem_avg% == "on"
p32_usableSeDispCurrIter(t,regi,entySE)$(tDT32(t) AND regDTCoup(regi) AND sameas(entySE,"seel")) = p32_usableSeDisp(t,regi,entySE);
p32_usableSeDisp(t,regi,entySE)$(tDT32(t) AND regDTCoup(regi) AND sameas(entySE,"seel")) =
  0.5 * (p32_usableSeDispCurrIter(t,regi,entySE) + p32_usableSeDispLaIter(t,regi,entySE));

p32_seh2elh2DemCurrIter(t,regi,entySE)$(tDT32(t) AND regDTCoup(regi) AND sameas(entySE,"seh2")) = p32_seh2elh2Dem(t,regi,"seh2");
p32_seh2elh2Dem(t,regi,entySE)$(tDT32(t) AND regDTCoup(regi) AND sameas(entySE,"seh2")) =
  0.5 * (p32_seh2elh2DemCurrIter(t,regi,entySE) + p32_seh2elh2DemLaIter(t,regi,entySE));
$ENDIF.dem_avg


***CG:interest rate (Marian's formula) (should move this to core/postsolve at some point)
p32_r4DT(ttot,regi)$(tDT32s2(ttot))
= (( (vm_cons.l(ttot+1,regi)/pm_pop(ttot+1,regi)) /
  (vm_cons.l(ttot-1,regi)/pm_pop(ttot-1,regi)) )
  ** (1 / ( pm_ttot_val(ttot+1)- pm_ttot_val(ttot-1))) - 1) + pm_prtp(regi);

p32_test1(t,regi,te) =  sum(en2en(enty,enty2,te),
			vm_demSe.l(t,regi,enty,enty2,te)$(sameas(enty,"seel")))
;

p32_test2(t,regi) =  sum(en2en(enty,enty2,te),
			vm_demSe.l(t,regi,enty,enty2,te)$(sameas(enty,"seel")))
;

***CG:
* since we would like to couple all years to limit distortions, but growth rate after 2100 is weird (2130 has negative growth rate) due to various artefact, we simply set interest rates
* after 2100 to 5%, this only sets 2110, 2130, 2150 three years
p32_r4DT(ttot,regi)$(ttot.val gt 2100) = 0.05;

$IFTHEN.policy_Cprice not %carbonprice% == "none"
*** CG: updating CO2 price from REMIND to DIETER
p32_CO2price4DT(t,regi)$(tDT32(t) AND regDTCoup(regi)) = pm_taxCO2eq(t,regi)/sm_DptCO2_2_TDpGtC;
!!p32_CO2price4DT(t,regi)$(cm_startyear AND regDTCoup(regi)) = pm_taxCO2eq(t,regi)/sm_DptCO2_2_TDpGtC;
$ENDIF.policy_Cprice


* REMIND data for DIETER
    execute_unload "RMdata_4DT.gdx", tDT32,regDTCoup,sm32_iter, !! basic info: coupled time and regions, iteration number,
    s32_H2switch,s32_DTcoupModeswitch,cm_DT_dispatch_i1,cm_DT_dispatch_i2,s32_windoff, s32_scarPrice, !! switches: H2 switch, mode switch, dispatch iterational switches, offshore switch
    COALte32,NonPeakGASte32,BIOte32,NUCte32,REMINDte4DT32,      !! tech sets: REMIND technology definition
    vm_cap, vm_deltaCap, vm_capDistr, v32_storloss,vm_capEarlyReti,vm_prodSe,vm_usableSeTe, !! quantities: capacity, generation, curtailment,
    p32_realCapfacVRE,vm_capFac,pm_cf, pm_dataren, !! CF
    p32_usableSeDisp,p32_seh2elh2Dem, !! total demand
    vm_costTeCapital, o_margAdjCostInv, pm_data,fm_dataglob,p32_r4DT, !! capex related tech parameters, interest rate
    pm_dataeta, pm_eta_conv, p32_fuelprice_avgiter, p32_CO2price4DT, fm_dataemiglob, !! running cost related tech parameters
    p32_grid_factor,pm_dt, !! misc
    p32_shSeElDisp; !! just for comparison

logfile.nr = 1;
if ( (c_keep_iteration_gdxes eq 1) ,
    put_utility "shell" /
      "cp RMdata_4DT.gdx RMdata_4DT_i" sm32_iter:0:0 ".gdx";
);
logfile.nr = 2;

*** CG: fit a polynom through oscillating fuel price data for biomass, coal and gas
    execute "Rscript fuelPriceCubRegr.R";

    execute "./DIETER_parallel.sh";
		put "running DIETER iteration", sm32_iter:0:0;

    execute './mergegdx.sh';

*   .nr = 2 formats numbers in scientific notation (what we usually want for
*   debugging, because small numbers get rounded to zero otherwise even though
*   they are significant, e.g. for the calibration).  So there's a .nr = 1 up
*   front to ensure "normal" numbers.

logfile.nr = 1;

if ( (c_keep_DTiteration_gdxes eq 1) ,

***CG: make iteration copies of DIETER results
    put_utility "shell" /
      "cp results_DIETER.gdx results_DIETER_i" sm32_iter:0:0 ".gdx";

    put_utility "shell" /
      "cp report_DIETER.gdx report_DIETER_i" sm32_iter:0:0 ".gdx";

    put_utility "shell" /
      "cp full_DIETER.gdx full_DIETER_i" sm32_iter:0:0 ".gdx";

***CG: saves some lst files for diagnosis
    put_utility "shell" /
      "cp DIETER_v1.0.2_1.lst DIETER_v1.0.2_1_i" sm32_iter:0:0 ".lst";
    put_utility "shell" /
      "cp DIETER_v1.0.2_5.lst DIETER_v1.0.2_5_i" sm32_iter:0:0 ".lst";
    put_utility "shell" /
      "cp DIETER_v1.0.2_10.lst DIETER_v1.0.2_10_i" sm32_iter:0:0 ".lst";

);
logfile.nr = 2;

*** CG: after DIETER is executed, check if they all have optimal status
$IFTHEN.DTcoup %cm_DTcoup% == "on"
    Execute_Loadpoint 'results_DIETER' p32_report4RM;
    Execute_Loadpoint 'results_DIETER' p32_reportmk_4RM;
*** check DIETER solver status
p32_DTstatus(t,regi)$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"el","model_status"));
display p32_DTstatus;
loop (t$tDT32(t),
  loop (regi$regDTCoup(regi),
    if (p32_DTstatus(t,regi) ne 1,
        abort "one or more DIETER LP have non optimal solver status";
        );
  );
);
$ENDIF.DTcoup


$IFTHEN.curt_avg %cm_DTcurt_avg% == "on"
p32_DIETERCurtRatioLaIter(t,regi,"spv")$(tDT32(t) AND regDTCoup(regi)) = p32_DIETERCurtRatio(t,regi,"spv");
p32_DIETERCurtRatioLaIter(t,regi,"wind")$(tDT32(t) AND regDTCoup(regi)) = p32_DIETERCurtRatio(t,regi,"wind");
$IFTHEN.WindOff %cm_wind_offshore% == "1"
p32_DIETERCurtRatioLaIter(t,regi,"windoff")$(tDT32(t) AND regDTCoup(regi)) = p32_DIETERCurtRatio(t,regi,"windoff");
$ENDIF.WindOff
$ENDIF.curt_avg

* coupled demand side or supply side technologies:
p32_cf_last_iter(t,regi,te)$(tDT32(t) AND regDTCoup(regi) AND (teDTCoupSupp(te) OR CFcoupDemte32(te))) = vm_capFac.l(t,regi,te);

);


$ENDIF.DTcoup

$endif.calibrate
*** EOF ./modules/32_power/DTcoup/postsolve.gms
