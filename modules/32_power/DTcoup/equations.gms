*** |  (C) 2006-2019 Potsdam Institute for Climate Impact Research (PIK)
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
		sum(teCCS2rlf(te,rlf), pm_prodCouple(regi,enty,enty3,te,enty2) * vm_co2CCS(t,regi,enty,enty3,te,rlf) ) )
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

q32_usableSeTe(t,regi,entySe,te)$(sameas(entySe,"seel") AND teVRE(te))..
 	vm_usableSeTe(t,regi,entySe,te)
 	=e=
 	sum(pe2se(enty,entySe,te), vm_prodSe(t,regi,enty,entySe,te) )
	+ sum(se2se(enty,entySe,te), vm_prodSe(t,regi,enty,entySe,te) )
 	- sum(teVRE$sameas(te,teVRE), v32_storloss(t,regi,teVRE) )
;

**---------------------------------------------------------------------------
** Definition of capacity constraints for storage:
**---------------------------------------------------------------------------
* q32_limitCapTeStor(t,regi,teStor)$(t.val ge 2015)..
* 	sum(VRE2teStor(teVRE,teStor), v32_storloss(t,regi,teVRE) )
* 	* pm_eta_conv(t,regi,teStor) / ( 1 - pm_eta_conv(t,regi,teStor))
* 	=l=
* 	sum(te2rlf(teStor,rlf),
* 		vm_capFac(t,regi,teStor) * pm_dataren(regi,"nur",rlf,teStor) * vm_cap(t,regi,teStor,rlf) )
* ;
*
* q32_h2turbVREcapfromTestor(t,regi)..
*  vm_cap(t,regi,"h2turbVRE","1")
*  =e=
*  sum(te$testor(te), p32_storageCap(te,"h2turbVREcapratio") * vm_cap(t,regi,te,"1") )
* ;
*
* q32_elh2VREcapfromTestor(t,regi)..
*  vm_cap(t,regi,"elh2VRE","1")
*  =e=
*  sum(te$testor(te), p32_storageCap(te,"elh2VREcapratio") * vm_cap(t,regi,te,"1") )
* ;

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
*** Calculation of share of electricity production
***---------------------------------------------------------------------------
q32_shSeEl(t,regi,te)..
    v32_shSeEl(t,regi,te) / 100 * vm_usableSe(t,regi,"seel")
    =e=
    vm_usableSeTe(t,regi,"seel",te)
;
* ***---------------------------------------------------------------------------
* *** Calculation of share of electricity production of nonVRE
* ***---------------------------------------------------------------------------
* q32_shSeEl_disp(t,regi,teNoRe)..
*     v32_shSeEl(t,regi,teNoRe) / 100 * vm_usableSe(t,regi,"seel")
*     =e=
*     vm_usableSeTe(t,regi,"seel",teVRE)
* ;
***---------------------------------------------------------------------------
*** Calculation of necessary storage electricity production:
***---------------------------------------------------------------------------
q32_shStor(t,regi,teVRE)$(t.val ge 2015 AND regNoDTCoup(regi))..
	v32_shStor(t,regi,teVRE)  * 1$(regNoDTCoup(regi))
	=g=
	( p32_factorStorage(regi,teVRE) * 100
	* (
		(1.e-10 + (v32_shSeEl(t,regi,teVRE) + sum(VRE2teVRElinked(teVRE,teVRE2), v32_shSeEl(t,regi,teVRE2)) /s32_storlink)/100 ) ** p32_storexp(regi,teVRE)    !! offset of 1.e-10 for numerical reasons: gams doesn't like 0 if the exponent is not integer
		- (1.e-10 ** p32_storexp(regi,teVRE) )       !! offset correction
		- 0.07                                      !! first 7% of VRE share bring no negative effects
	)  )* 1$(regNoDTCoup(regi))
;

q32_storloss(t,regi,teVRE)$(t.val ge 2015 )..
	v32_storloss(t,regi,teVRE)
	=e=
	(v32_shStor(t,regi,teVRE) / 93    !! corrects for the 7%-shift in v32_shStor: at 100% the value is correct again
	* sum(VRE2teStor(teVRE,teStor), (1 - pm_eta_conv(t,regi,teStor) ) /  pm_eta_conv(t,regi,teStor) )
	* vm_usableSeTe(t,regi,"seel",teVRE) ) * 1$(regNoDTCoup(regi))
	+ (p32_DIETER_curtailmentratio(t,regi,teVRE) * vm_usableSeTe(t,regi,"seel",teVRE) ) * 1$(regDTCoup(regi))
;

***---------------------------------------------------------------------------
*** EMF27 limits on fluctuating renewables, only turned on for special EMF27 and AWP 2 scenarios, not for SSP
***---------------------------------------------------------------------------
q32_limitSolarWind(t,regi)$( (cm_solwindenergyscen = 2) OR (cm_solwindenergyscen = 3) )..
	vm_usableSeTe(t,regi,"seel","spv") + vm_usableSeTe(t,regi,"seel","wind") + vm_usableSeTe(t,regi,"seel","csp")
	=l=
	0.2 * vm_usableSe(t,regi,"seel")
;

$IFTHEN.DTcoup %cm_DTcoup% == "on"
***---------------------------------------------------------------------------
*** DIETER coupling equations
***---------------------------------------------------------------------------

q32_peakDemand_DT(t,regi,enty2)$(tDT32(t) AND sameas(enty2,"seel") AND regDTCoup(regi) AND (cm_DTcoup_capcon = 1) ) ..
	sum(te$(DISPATCHte32(te)), sum(rlf, vm_cap(t,regi,te,rlf)$( regDTCoup(regi) )))
	=g=
	p32_peakDemand_relFac(t,regi)$( regDTCoup(regi) ) * p32_seelDem(t,regi,enty2)$( regDTCoup(regi) ) * 8760
	;

***----------------------------------------------------------------------------
*** CG: calculate markup adjustment used in flexibility tax for supply-side technologies
***----------------------------------------------------------------------------
q32_mkup(t,regi,te)$(tDT32(t) AND teDTCoupSupp(te) AND (cm_DTcoup_capcon = 1) AND regDTCoup(regi))..
	vm_Mrkup(t,regi,te)$( regDTCoup(regi) )
	=e=
*** supply-side technology markup v32_DIETER_VF as a multiplicative factor
*** of the wholesale electricity price of the last iteration
*** v32_DIETER_VF < 1 -> market value lower than average electricity price (usually fluctuating VRE generation),
*** v32_DIETER_VF > 1 -> market value higher than average electricity price (usually firm generation technologies)
*** prefactor depends on difference between gen share of DIETER and current REMIND iter
* (v32_DIETER_VF(t,regi,te) * ( 1 - (v32_shSeEl(t,regi,te) / 100 - p32_DIETER_shSeEl(t,regi,te) / 100) ) - 1 ) * pm_SEPrice(t,regi,"seel")
*** prefactor depends on difference between gen share of last and current REMIND iter
* (v32_DIETER_VF(t,regi,te) * ( 1 - (v32_shSeEl(t,regi,te) / 100 - p32_shSeEl(t,regi,te) / 100 ) ) - 1 ) * pm_SEPrice(t,regi,"seel")
*** NO prefactor
* (v32_DIETER_VF(t,regi,te) - 1 ) * pm_SEPrice(t,regi,"seel") * 1$( regDTCoup(regi) )
*** nonlinear relation
* ((v32_DIETER_VF(t,regi,te) * p32_shSeEl(t,regi,te)) / (v32_shSeEl(t,regi,te) + sm_eps) * pm_SEPrice(t,regi,"seel") - pm_SEPrice(t,regi,"seel")) * 1$( regDTCoup(regi) )

*** absolute markup, multiply by budget from DIETER to REMIND, but divide here again by budget
*** price_new = price/budget * 1e12 / sm_TWa_2_MWh * 1.2
*** price = price_new * budget/1e12 * sm_TWa_2_MWh/1.2
( (p32_DIETER_MV(t,regi,te)$( regDTCoup(regi) ) *
   (1 - (v32_shSeEl(t,regi,te)$( regDTCoup(regi) ) / 100 - p32_DIETER_shSeEl(t,regi,te)$( regDTCoup(regi) ) / 100 )  ) - p32_DIETER_elecprice(t,regi)$( regDTCoup(regi) ) )
	 / 1e12 * sm_TWa_2_MWh / 1.2 )
* ( (p32_DIETER_MV(t,regi,te)  - p32_DIETER_elecprice(t,regi) ) / 1e12 * sm_TWa_2_MWh / 1.2 ) * 1$( regDTCoup(regi) )
* (p32_DIETER_MV(t,regi,te) - p32_DIETER_elecprice(t,regi) ) / 1e12 * sm_TWa_2_MWh / 1.2
;
$ENDIF.DTcoup
