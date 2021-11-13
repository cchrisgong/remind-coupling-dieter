*** |  (C) 2006-2020 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de
***---------------------------------------------------------------------------
*** Balance equation for electricity secondary energy type:
***---------------------------------------------------------------------------
q32_balSe(t,regi,enty2)$(sameas(enty2,"seel"))..
	sum(pe2se(enty,enty2,te), vm_prodSe(t,regi,enty,enty2,te) )
	+ sum(se2se(enty,enty2,te), vm_prodSe(t,regi,enty,enty2,te) )
	+ sum(pc2te(enty,entySE(enty3),te,enty2),
		pm_prodCouple(regi,enty,enty3,te,enty2) * vm_prodSe(t,regi,enty,enty3,te) )
	+ sum(pc2te(enty4,entyFE(enty5),te,enty2),
		pm_prodCouple(regi,enty4,enty5,te,enty2) * vm_prodFe(t,regi,enty4,enty5,te) )
	+ sum(pc2te(enty,enty3,te,enty2),
		sum(teCCS2rlf(te,rlf),
			pm_prodCouple(regi,enty,enty3,te,enty2) * vm_co2CCS(t,regi,enty,enty3,te,rlf) ) )
	+ vm_Mport(t,regi,enty2)
  =e=
    sum(se2fe(enty2,enty3,te), vm_demSe(t,regi,enty2,enty3,te) )
	+ sum(se2se(enty2,enty3,te), vm_demSe(t,regi,enty2,enty3,te) )
	+ sum(teVRE, v32_storloss(t,regi,teVRE) )
	+ sum(pe2rlf(enty3,rlf2), (pm_fuExtrOwnCons(regi, enty2, enty3) * vm_fuExtr(t,regi,enty3,rlf2))$(pm_fuExtrOwnCons(regi, enty2, enty3) gt 0))$(t.val > 2005) !! do not use in 2005 because this demand is not contained in 05_initialCap
	+ vm_Xport(t,regi,enty2)
;

q32_usableSe(t,regi,entySe)$(sameas(entySe,"seel"))..
	vm_usableSe(t,regi,entySe)
	=e=
	sum(pe2se(enty,entySe,te), vm_prodSe(t,regi,enty,entySe,te) )
	+ sum(se2se(enty,entySe,te), vm_prodSe(t,regi,enty,entySe,te) )
	+ sum(pc2te(entyPe,entySe(enty3),te,entySe)$(pm_prodCouple(regi,entyPe,enty3,te,entySe) gt 0),
		pm_prodCouple(regi,entyPe,enty3,te,entySe)*vm_prodSe(t,regi,entyPe,enty3,te) )
	- sum(teVRE, v32_storloss(t,regi,teVRE) )
;

q32_usableSeTe(t,regi,entySe,te)$(sameas(entySe,"seel") AND teDTCoupSupp(te))..
 	vm_usableSeTe(t,regi,entySe,te)
 	=e=
 	sum(pe2se(enty,entySe,te), vm_prodSe(t,regi,enty,entySe,te) )
	+ sum(se2se(enty,entySe,te), vm_prodSe(t,regi,enty,entySe,te) )
 	+ sum(pc2te(enty,entySe(enty3),te,entySe)$(pm_prodCouple(regi,enty,enty3,te,entySe) gt 0),
		pm_prodCouple(regi,enty,enty3,te,entySe) * vm_prodSe(t,regi,enty,enty3,te) )
 	- sum(teVRE$sameas(te,teVRE), v32_storloss(t,regi,teVRE) )
;

q32_usableSeDisp(t,regi,entySe)$(tDT32(t) AND regDTCoup(regi) AND sameas(entySe,"seel"))..
	v32_usableSeDisp(t,regi,entySe)
	=e=
	sum(pe2se(enty,entySe,te), vm_prodSe(t,regi,enty,entySe,te) )
	+ sum(se2se(enty,entySe,te), vm_prodSe(t,regi,enty,entySe,te) )
	- sum(teVRE, v32_storloss(t,regi,teVRE) )
;

q32_usableSeTeDisp(t,regi,entySe,te)$(tDT32(t) AND regDTCoup(regi) AND sameas(entySe,"seel") AND teDTCoupSupp(te))..
 	v32_usableSeTeDisp(t,regi,entySe,te)
 	=e=
 	sum(pe2se(enty,entySe,te), vm_prodSe(t,regi,enty,entySe,te) )
	+ sum(se2se(enty,entySe,te), vm_prodSe(t,regi,enty,entySe,te) )
 	- sum(teVRE$sameas(te,teVRE), v32_storloss(t,regi,teVRE) )
;

***---------------------------------------------------------------------------
*** Definition of capacity constraints for storage:
***---------------------------------------------------------------------------
q32_limitCapTeStor(t,regi,teStor)$( t.val ge 2015 AND ((regDTCoup(regi) AND (cm_DTcoup_eq eq 0)) OR regNoDTCoup(regi))) ..
    ( 0.5$( cm_VRE_supply_assumptions eq 1 )
    + 1$( cm_VRE_supply_assumptions ne 1 )
    )
  * sum(VRE2teStor(teVRE,teStor), v32_storloss(t,regi,teVRE))
  * pm_eta_conv(t,regi,teStor)
  / (1 - pm_eta_conv(t,regi,teStor))
  =l=
  sum(te2rlf(teStor,rlf),
    vm_capFac(t,regi,teStor)
  * pm_dataren(regi,"nur",rlf,teStor)
  * vm_cap(t,regi,teStor,rlf)
  )
;


*** H2 storage implementation: Storage technologies (storspv, storwind etc.) also
*** represent H2 storage. This is implemented by automatically scaling up capacities of
*** elh2VRE (electrolysis from VRE, seel -> seh2) and H2 turbines (h2turbVRE, seh2 -> seel)
*** with VRE capacities which require storage (according to q32_limitCapTeStor):


*** build additional electrolysis capacities with stored VRE electricity
q32_elh2VREcapfromTestor(t,regi)..
  vm_cap(t,regi,"elh2","1")
  =g=
  sum(te$testor(te), p32_storageCap(te,"elh2VREcapratio") * vm_cap(t,regi,te,"1") )
;

*** build additional h2 to seel capacities to use stored hydrogen
q32_h2turbVREcapfromTestor(t,regi)..
  vm_cap(t,regi,"h2turbVRE","1")
  =e=
  sum(te$testor(te), p32_storageCap(te,"h2turbVREcapratio") * vm_cap(t,regi,te,"1") )
;


***---------------------------------------------------------------------------
*** Definition of capacity constraints for CHP technologies:
***---------------------------------------------------------------------------
q32_limitCapTeChp(t,regi)..
    sum(pe2se(enty,"seel",teChp(te)), vm_prodSe(t,regi,enty,"seel",te) )
    =l=
    p32_shCHP(regi,"bscu")
    * sum(pe2se(enty,"seel",te), vm_prodSe(t,regi,enty,"seel",te) )
;

***---------------------------------------------------------------------------
*** Calculation of necessary grid installations for centralized renewables:
***---------------------------------------------------------------------------
q32_limitCapTeGrid(t,regi)$( t.val ge 2015 ) ..
    vm_cap(t,regi,"gridwind",'1')      !! Technology is now parameterized to yield marginal costs of ~3.5$/MWh VRE electricity
    / p32_grid_factor(regi)        		!! It is assumed that large regions require higher grid investment
    =g=
    vm_prodSe(t,regi,"pesol","seel","spv")
    + vm_prodSe(t,regi,"pesol","seel","csp")
    + 1.5 * vm_prodSe(t,regi,"pewin","seel","wind")                 !! wind has larger variations accross space, so adding grid is more important for wind (result of REMIX runs for ADVANCE project)
$IFTHEN.WindOff %cm_wind_offshore% == "1"
    + 3 * vm_prodSe(t,regi,"pewin","seel","windoff")
$ENDIF.WindOff
;

***---------------------------------------------------------------------------
*** Calculation of share of electricity production of a technology:
***---------------------------------------------------------------------------
q32_shSeEl(t,regi,te)$(teDTCoupSupp(te))..
    v32_shSeEl(t,regi,te) / 100 * vm_usableSe(t,regi,"seel")
    =e=
    vm_usableSeTe(t,regi,"seel",te)
;

q32_shSeElDisp(t,regi,te)$(tDT32(t) AND regDTCoup(regi) AND teDTCoupSupp(te))..
    v32_shSeElDisp(t,regi,te) / 100 * v32_usableSeDisp(t,regi,"seel")
    =e=
    v32_usableSeTeDisp(t,regi,"seel",te)
;


$IFTHEN.DTcoup %cm_DTcoup% == "on"
*** CG: Calculation of share of electricity demand, e.g. of green h2 using elh2 (note: RHS of demSe contains also deamand met by co-production, which
*** is not coupled to DIETER, ways to improve? v32_shSeElDem probably add up to more than 100%)
$IFTHEN.elh2_coup %cm_elh2_coup% == "on"
***---------------------------------------------------------------------------
q32_shSeElDem(t,regi,te)$(teFlexTax(te) AND regDTCoup(regi))..
    v32_shSeElDem(t,regi,te) / 100 * vm_usableSeDisp(t,regi,"seel")
    =e=
    sum(en2en(enty,enty2,te),
			vm_demSe(t,regi,enty,enty2,te)$(sameas(enty, "seel")))
;
$ENDIF.elh2_coup
$ENDIF.DTcoup

***---------------------------------------------------------------------------
*** Calculation of necessary storage electricity production:
***---------------------------------------------------------------------------
q32_shStor(t,regi,teVRE)$(t.val ge 2015)..
	v32_shStor(t,regi,teVRE)
	=g=
	(p32_factorStorage(regi,teVRE) * 100
	* (
		(1.e-10 + (v32_shSeEl(t,regi,teVRE) + sum(VRE2teVRElinked(teVRE,teVRE2), v32_shSeEl(t,regi,teVRE2))/s32_storlink)/100 ) ** p32_storexp(regi,teVRE)    !! offset of 1.e-10 for numerical reasons: gams doesn't like 0 if the exponent is not integer
		- (1.e-10 ** p32_storexp(regi,teVRE) )       !! offset correction
		- 0.07                                      !! first 7% of VRE share bring no negative effects
	) )
$IFTHEN.DTcoup %cm_DTcoup% == "on"
	* 1$((regDTCoup(regi) AND (cm_DTcoup_eq eq 0)) OR regNoDTCoup(regi))
$ENDIF.DTcoup
;

q32_storloss(t,regi,teVRE)$(t.val ge 2015)..
	v32_storloss(t,regi,teVRE)
	=e=
	( v32_shStor(t,regi,teVRE) / 93    !! corrects for the 7%-shift in v32_shStor: at 100% the value is correct again
	* sum(VRE2teStor(teVRE,teStor), (1 - pm_eta_conv(t,regi,teStor) ) /  pm_eta_conv(t,regi,teStor) )
	* vm_usableSeTe(t,regi,"seel",teVRE) )
$IFTHEN.DTcoup %cm_DTcoup% == "on"
	* 1$((regDTCoup(regi) AND (cm_DTcoup_eq eq 0)) OR regNoDTCoup(regi))
	+ (p32_DIETERCurtRatio(t,regi,teVRE) * vm_usableSeTe(t,regi,"seel",teVRE) )
      * ( 1 - (p32_DIETER_shSeEl(t,regi,teVRE) / 100 - v32_shSeEl(t,regi,teVRE) / 100) )   !!! this is important to keep for stability
	* 1$(regDTCoup(regi) AND (cm_DTcoup_eq eq 1))
*	+ 0 * 1$(regDTCoup(regi)) !! turn off curtailment for coupled region
$ENDIF.DTcoup
;

$IFTHEN.DTcoup_off %cm_DTcoup% == "off"
**** without DIETER coupling

***---------------------------------------------------------------------------
*** Operating reserve constraint
***---------------------------------------------------------------------------
q32_operatingReserve(t,regi)$(t.val ge 2010)..
***1 is the chosen load coefficient
	vm_usableSe(t,regi,"seel")
	=l=
***Variable renewable coefficients could be expected to be negative because they are variable.
***However they are modeled positive because storage conditions make variable renewables controllable.
	sum(pe2se(enty,"seel",te)$(NOT teVRE(te)),
		pm_data(regi,"flexibility",te) * vm_prodSe(t,regi,enty,"seel",te) )
	+ sum(se2se(enty,"seel",te)$(NOT teVRE(te)),
		pm_data(regi,"flexibility",te) * vm_prodSe(t,regi,enty,"seel",te) )
	+ sum(pe2se(enty,"seel",teVRE),
		pm_data(regi,"flexibility",teVRE) * (vm_prodSe(t,regi,enty,"seel",teVRE)-v32_storloss(t,regi,teVRE)) )
	+
	sum(pe2se(enty,"seel",teVRE),
		sum(VRE2teStor(teVRE,teStor),
			pm_data(regi,"flexibility",teStor) * (vm_prodSe(t,regi,enty,"seel",teVRE)-v32_storloss(t,regi,teVRE)) ) )
;
$ENDIF.DTcoup_off

***---------------------------------------------------------------------------
*** EMF27 limits on fluctuating renewables, only turned on for special EMF27 and AWP 2 scenarios, not for SSP
***---------------------------------------------------------------------------
q32_limitSolarWind(t,regi)$( (cm_solwindenergyscen = 2) OR (cm_solwindenergyscen = 3) )..
	vm_usableSeTe(t,regi,"seel","spv") + vm_usableSeTe(t,regi,"seel","wind") + vm_usableSeTe(t,regi,"seel","csp")
	=l=
	0.2 * vm_usableSe(t,regi,"seel")
;

$IFTHEN.DTcoup %cm_DTcoup% == "on"
**** with DIETER coupling
***---------------------------------------------------------------------------
*** DIETER coupling equations
***---------------------------------------------------------------------------
$IFTHEN.hardcap %cm_softcap% == "off"
*** hard capacity constraint to peak residual load demand

q32_peakDemandDT(t,regi,"seel")$(tDT32(t) AND regDTCoup(regi) AND (cm_DTcoup_eq ne 0) ) ..
	sum(te$(DISPATCHte32(te)), sum(rlf, vm_cap(t,regi,te,rlf)))
	=e=
* use in-iteration variable
*	p32_peakDemand_relFac(t,regi) * (p32_seelUsableDem(t,regi,"seel") - p32_seh2elh2Dem(t,regi,"seh2")) * 8760
* p32_peakDemand_relFac(t,regi) * (v32_seelUsableDem(t,regi,"seel") - vm_demSe(t,regi,"seel","seh2","elh2")) * 8760
  p32_peakDemand_relFac(t,regi) * (vm_demSe(t,regi,"seel","feels","tdels") + vm_demSe(t,regi,"seel","feelt","tdelt")) * 8760
	;

$ENDIF.hardcap

$IFTHEN.softcap %cm_softcap% == "on"
** CG: implementing a softer capacity bound, with a flat capacity subsidy, once the sum of dispatchable capacity exceeds
** the bound which is the peak demand from last iteration, the subsidy rapidly drops according to logistic function
** Logistic function exponent for additional dispatchable capacity, LHS bound up to 20 to reduce computational intensity
*  because the exponent is capped by 20, we introduce
*  v32_expSlack, to save the remaining values if the exponent value on equation q32_auxPriceCap is bigger due to the
*  "x" value (vm_reqCap). At the same time we want this slack variable to be used only if necessary, so we add a penalization term
*  in the main equation q32_priceCap (v32_expSlack(t,regi)*1e-8), such that the result variable is a
*  investment cost variable and the model will try to keep its value at the minimal possible.

q32_reqCap(t,regi,enty2)$(tDT32(t) AND sameas(enty2,"seel") AND regDTCoup(regi) AND (cm_DTcoup_eq ne 0) ) ..
	vm_reqCap(t,regi)
 	=e=
 	sum(te$(DISPATCHte32(te)), sum(rlf, vm_cap(t,regi,te,rlf)))
 	;

q32_capEndStart(t,regi)$(tDT32(t) AND regDTCoup(regi) AND (cm_DTcoup_eq ne 0) )..
	v32_capDecayEnd(t,regi)
	=e=
  p32_peakDemand_relFac(t,regi) * (vm_demSe(t,regi,"seel","feels","tdels") + vm_demSe(t,regi,"seel","feelt","tdelt")) * 8760 *1.1
;


q32_priceCap(t,regi)$(tDT32(t) AND regDTCoup(regi) AND (cm_DTcoup_eq ne 0) )..
  vm_priceCap(t,regi)
  =e=
	1 / 1.2 * ( -p32_budget(t,regi)) !! 0.1 = 100$/kW * 1e9 / 1e12, this is the capacity subsidy per kW of dispatchable, kW -> TW, USD -> trUSD
  * ( 1 / ( 1 + ( 3 ** ( (10 / ( v32_capDecayEnd(t,regi) - v32_capDecayEnd(t,regi)/1.1 ))
		* ( vm_reqCap(t,regi) + 1e-11 - ( v32_capDecayEnd(t,regi) + v32_capDecayEnd(t,regi)/1.1 ) / 2	) ) ) )	)
		 + ( v32_expSlack(t,regi) * 1e-8 )
;

$ENDIF.softcap
***----------------------------------------------------------------------------
*** CG: calculate markup adjustment used in flexibility tax for supply-side technologies:
*** supply-side technology (absolute) markup p32_DIETER_MV from DIETER
*** multiplied with a prefactor that depends on generation share of current REMIND iteration
*** Note: price conversions between REMIND and DIETER:
*** multiply by budget from DIETER to REMIND, but divide here again by budget
*** price_DIETER = price_REMIND/budget_REMIND * 1e12 / sm_TWa_2_MWh * 1.2
*** price_REMIND = price_DIETER * budget_REMIND/1e12 * sm_TWa_2_MWh/1.2
***----------------------------------------------------------------------------
*q32_mkup(t,regi,te)$(tDT32(t) AND regDTCoup(regi) AND teDTCoupSupp(te) AND (cm_DTcoup_eq eq 3))..
q32_mkup(t,regi,te)$(tDT32(t) AND regDTCoup(regi) AND teDTCoupSupp(te) AND (cm_DTcoup_eq ne 0))..
	vm_Mrkup(t,regi,te)$( regDTCoup(regi) )
	=e=
* with prefactor
 (p32_DIETER_MV(t,regi,te)$( regDTCoup(regi) )
  * (1 - (v32_shSeEl(t,regi,te)$( regDTCoup(regi) ) / 100 - p32_DIETER_shSeEl(t,regi,te)$( regDTCoup(regi) ) / 100 )  ) - p32_DIETER_elecprice(t,regi)$( regDTCoup(regi) ) )
	 / 1e12 * sm_TWa_2_MWh / 1.2
* no prefactor
* ( (p32_DIETER_MV(t,regi,te)  - p32_DIETER_elecprice(t,regi) ) / 1e12 * sm_TWa_2_MWh / 1.2 ) * 1$( regDTCoup(regi) )
;

$IFTHEN.elh2_coup %cm_elh2_coup% == "on"
*** CG: giving flexible demand side technology, e.g. electrolyzer a subsidy, non DIETER coupled version is q32_flexAdj below
*** on prefactor: beacuse if v32_shSeElDem is low for elh2 compared to last iter p32_shSeElDem, to balance out oscillation for elh2
*** we want flexadj to be high, so f has to be <1. if v32_shSeElDem - p32_shSeElDem is negative, then f is larger
*** than one. so the sign has to be changed

*q32_flexAdj(t,regi,te)$(tDT32(t) AND regDTCoup(regi) AND teFlexTax(te) AND (cm_DTcoup_eq eq 3))..
q32_flexAdj(t,regi,te)$(tDT32(t) AND regDTCoup(regi) AND teFlexTax(te) AND (cm_DTcoup_eq ne 0))..
	vm_flexAdj(t,regi,te)$( regDTCoup(regi) )
	=e=
* no prefactor: this seems the more reasonable option: since more h2 (flexible) demand allow more VRE, which lower overall seel price,
* so it is not like VRE with decreasing market value with increasing share
* (p32_DIETER_elecprice(t,regi)$( regDTCoup(regi) ) - p32_DIETER_MP(t,regi,te)$( regDTCoup(regi) ))	/ 1e12 * sm_TWa_2_MWh / 1.2
* with prefactor (absolute markup)
( p32_DIETER_elecprice(t,regi)$( regDTCoup(regi) ) - p32_DIETER_MP(t,regi,te)$( regDTCoup(regi) )
 * ( 1 + ( v32_shSeElDem(t,regi,te)$( regDTCoup(regi) ) / 100 - p32_shSeElDem(t,regi,te)$( regDTCoup(regi) ) / 100 ) )
)
/ 1e12 * sm_TWa_2_MWh / 1.2
;
$ENDIF.elh2_coup
***---------------------------------------------------------------------------
*** Capacity factor for dispatchable power plants
***---------------------------------------------------------------------------
** CG: prefactor for dispatchable capfac necessary, since if generation share in current REMIND iteration is higher than last iteration
** then capfac should be lower (since VRE share is high, depressing utilization rate of dispatchable plants)
q32_capFac(t,regi,te)$( tDT32(t) AND regDTCoup(regi) AND CFcoupSuppte32(te) AND (cm_DTcoup_eq ne 0))..
*q32_capFac(t,regi,te)$( tDT32(t) AND regDTCoup(regi) AND CFcoupSuppte32(te) AND (cm_DTcoup_eq eq 3))..
    vm_capFac(t,regi,te) * 1$(tDT32(t) AND regDTCoup(regi) AND CFcoupSuppte32(te))
    =e=
	  pm_cf(t,regi,te)
 * ( 1 + 0.5 * (v32_shSeEl(t,regi,te) / 100 - p32_DIETER_shSeEl(t,regi,te) / 100 ) )
	  * 1$(tDT32(t) AND regDTCoup(regi) AND CFcoupSuppte32(te))
;

$IFTHEN.elh2_coup %cm_elh2_coup% == "on"
** CG: if elh2 demand share is high, then capfac should be increased..
q32_capFac_dem(t,regi,te)$( tDT32(t) AND regDTCoup(regi) AND CFcoupDemte32(te) AND (cm_DTcoup_eq ne 0))..
*q32_capFac_dem(t,regi,te)$( tDT32(t) AND regDTCoup(regi) AND CFcoupDemte32(te) AND (cm_DTcoup_eq eq 3))..
    vm_capFac(t,regi,te) * 1$(tDT32(t) AND regDTCoup(regi) AND CFcoupDemte32(te))
    =e=
	  pm_cf(t,regi,te)
 * ( 1 - 0.7 * (v32_shSeElDem(t,regi,te) / 100 - p32_shSeElDem(t,regi,te) / 100 ) )
	  * 1$(tDT32(t) AND regDTCoup(regi) AND CFcoupDemte32(te))
;

$ENDIF.elh2_coup

$ENDIF.DTcoup


$IFTHEN.DTcoup_off %cm_DTcoup% == "off"
***----------------------------------------------------------------------------
*** FS: calculate flexibility adjustment used in flexibility tax for technologies with electricity input
***----------------------------------------------------------------------------

*** This equation calculates the minimal flexible electricity price that flexible technologies (like elh2) can see. It is reached when the VRE share is 100%.
*** It depends on the capacity factor with a hyperbolic function. The equation ensures that by decreasing
*** capacity factor of flexible technologies (teFlex) these technologies see lower electricity prices given that there is a high VRE share in the power system.

*** On the derivation of the equation:
*** The formulation assumes a cubic price duration curve. That is, the effective electricity price the flexible technologies sees
*** depends on the capacity factor (CF) with a cubic function centered at (0.5,1):
*** p32_PriceDurSlope * (CF-0.5)^3 + 1,
*** Hence, at CF = 0.5, the REMIND average price pm_SEPrice(t,regi,"seel") is paid.
*** To get the average electricity price that a flexible technology sees at a certain CF,
*** we need to integrate this function with respect to CF and divide by CF. This gives the formulation below:
*** v32_flexPriceShareMin = p32_PriceDurSlope * ((CF-0.5)^4-0.5^4) / (4*CF) + 1.
*** This is the new average electricity price a technology sees if it runs on (a possibly lower than one) capacity factor CF
*** and deliberately uses hours of low-cost electricity.
 q32_flexPriceShareMin(t,regi,te)$(teFlex(te))..
  v32_flexPriceShareMin(t,regi,te) * 4 * vm_capFac(t,regi,te)
  =e=
  p32_PriceDurSlope(regi,te) * (power(vm_capFac(t,regi,te) - 0.5,4) - 0.5**4) +
  4 * vm_capFac(t,regi,te)
;

*** Calculates the electricity price of flexible technologies:
*** The effective flexible price linearly decreases with VRE share
*** from 1 (at 0% VRE share) to v32_flexPriceShareMin (at 100% VRE).
q32_flexPriceShare(t,regi,te)$(teFlex(te))..
  v32_flexPriceShare(t,regi,te)
  =e=
  1 - (1-v32_flexPriceShareMin(t,regi,te)) * sum(teVRE, v32_shSeEl(t,regi,teVRE))/100
;

*** This balance ensures that the lower electricity prices of flexible technologies are compensated
*** by higher electricity prices of inflexible technologies. Inflexible technologies are all technologies
*** which are part of teFlexTax but not of teFlex. The weighted sum of
*** flexible/inflexible electricity prices (v32_flexPriceShare) and electricity demand must be one.
*** Note: this is only on if cm_FlexTaxFeedback = 1. Otherwise, there is no change in electricity prices for inflexible technologies.
q32_flexPriceBalance(t,regi)$(cm_FlexTaxFeedback eq 1)..
  sum(en2en(enty,enty2,te)$(teFlexTax(te)),
  	vm_demSe(t,regi,enty,enty2,te))
  =e=
  sum(en2en(enty,enty2,te)$(teFlexTax(te)),
  	vm_demSe(t,regi,enty,enty2,te) * v32_flexPriceShare(t,regi,te))
;


*** This calculates the flexibility benefit or cost per unit electricity input
*** of flexibile or inflexibly technology.
*** In the tax module, vm_flexAdj is then deduced from the electricity price via the flexibility tax formulation.
*** Below, pm_SEPrice(t,regi,"seel") is the (average) electricity price from the last iteration.
*** Flexible technologies benefit (v32_flexPriceShare < 1),
*** while inflexible technologies are penalized (v32_flexPriceShare > 1).
*** Flexibility tax is switched only if cm_flex_tax = 1 and is active from 2025 onwards.
q32_flexAdj(t,regi,te)$(teFlexTax(te))..
	vm_flexAdj(t,regi,te)
	=e=
	(1-v32_flexPriceShare(t,regi,te)) * pm_SEPrice(t,regi,"seel")$(cm_flex_tax eq 1 AND t.val ge 2025)
;

$ENDIF.DTcoup_off
*** EOF ./modules/32_power/DTcoup2/equations.gms
