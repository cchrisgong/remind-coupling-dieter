*** |  (C) 2006-2020 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de
***-----------------------------------------------------------
***                  module specific bounds
***------------------------------------------------------------

*** =====================================
*** IntC bounds: important - do NOT change the orders of the lines!!
*** =====================================

*** Fix capacity factors to the standard value from data
vm_capFac.fx(t,regi,te) = pm_cf(t,regi,te);

*** FS: for historically limited biomass production scenario (cm_bioprod_histlim >= 0)
*** to avoid infeasibilities with vintage biomass capacities
*** allow bio techs to reduce capacity factor
if ( cm_bioprod_histlim ge 0,
        vm_capFac.lo(t,regi_sensscen,teBioPebiolc)$(t.val ge 2030) = 0;
);


$IFTHEN.DTcoup %cm_DTcoup% == "on"
*** CG: when coupling is on relax constraint for coupled region, coupled years and coupled tech
*** such that it can be adjusted depending on share dependent prefactor
if ((cm_DTcoup_eq eq 1),
		loop(regi$(regDTCoup(regi)),
			loop(t$(tDT32(t)),
				loop(te$(CFcoupSuppte32(te)),
				vm_capFac.lo(t,regi,te)=0;
				vm_capFac.up(t,regi,te)=INF;  !! must not be capped by one, as some vm_capFac are larger than 1 due to scaling
				);
			);
		);
);

$IFTHEN.elh2_coup %cm_elh2_coup% == "on"
if ((cm_DTcoup_eq eq 1),
		loop(regi$(regDTCoup(regi)),
			loop(t$(tDT32(t)),
				loop(te$(CFcoupDemte32(te)),
				vm_capFac.lo(t,regi,te)=0;
				vm_capFac.up(t,regi,te)=1;   !!capped by 1
				);
			);
		);
);
$ENDIF.elh2_coup

$ENDIF.DTcoup

*$IFTHEN.DTcoup %cm_DTcoup% == "on"
*v32_capPriceExponent.up(t,regi)$(regDTCoup(regi)) = 20;
* vm_capFac.fx(t,regi,te) = pm_cf_linear(t,regi,te);
*$ENDIF.DTcoup


$IFTHEN.DTcoup %cm_DTcoup% == "on"
*** FS: if flexibility tax on, let capacity factor be endogenuously determined between 0.1 and 1
*** for technologies that get flexibility tax/subsity (teFlexTax)
*** in case cm_FlexTaxFeedback ne 1, but cm_flex_tax is on and DIETER coupling is on ,then set the second two
*** statements only for non-coupled regions
*** if flexibility tax feedback is on, let model choose capacity factor of flexible technologies freely
$IFTHEN.elh2_coup_off %cm_elh2_coup% == "off"
    vm_capFac.fx(t,regi,teFlex)$((t.val ge 2010) AND regDTCoup(regi)) = 0.5;
*** electricity price of inflexible technologies the same w/o feedback
    v32_flexPriceShare.fx(t,regi,te)$(regDTCoup(regi) AND (teFlexTax(te) AND NOT(teFlex(te)))) = 1;
$ENDIF.elh2_coup_off

$IFTHEN.elh2_coup %cm_elh2_coup% == "on"
    if (cm_DTcoup_eq eq 0,
       vm_capFac.fx(t,regi,teFlex)$((t.val ge 2010) AND regDTCoup(regi)) = 0.5;
       v32_flexPriceShare.fx(t,regi,te)$(regDTCoup(regi) AND teFlexTax(te) AND NOT(teFlex(te))) = 1;
    );
    if (cm_DTcoup_eq ne 0,
       vm_capFac.fx(t,regi,teFlex)$(((t.val ge 2010 ) AND NOT tDT32(t)) AND regDTCoup(regi)) = 0.5;
       v32_flexPriceShare.fx(t,regi,te)$(regDTCoup(regi) AND teFlexTax(te) AND NOT(teFlex(te))) = 1;
    );
$ENDIF.elh2_coup
$ENDIF.DTcoup


*** Lower bounds on VRE use (more than 0.01% of electricity demand) after 2015 to prevent the model from overlooking spv and wind and csp
loop(regi,
  loop(te$(teVRE(te)),
    if ( (sum(rlf, pm_dataren(regi,"maxprod",rlf,te)) > 0.01 * pm_IO_input(regi,"seel","feels","tdels")) ,
         v32_shSeEl.lo(t,regi,te)$(t.val>2020) = 0.01;
    );
  );
);

*RP* upper bound of 90% on share of electricity produced by a single VRE technology, and lower bound on usablese to prevent the solver from dividing by 0
v32_shSeEl.up(t,regi,teVRE) = 90;

vm_usableSe.lo(t,regi,"seel")  = 1e-6;

*** Fix capacity for h2curt technology (modeled only in RLDC)
vm_cap.fx(t,regi,"h2curt",rlf) = 0;

*RP To ensure that the REMIND model doesn't overlook CSP due to gdx effects, ensure some minimum use in regions with good solar insolation, here proxied from the csp storage factor:
loop(regi$(p32_factorStorage(regi,"csp") < 1),
  v32_shSeEl.lo(t,regi,"csp")$(t.val > 2025) = 0.5;
  v32_shSeEl.lo(t,regi,"csp")$(t.val > 2050) = 1;
  v32_shSeEl.lo(t,regi,"csp")$(t.val > 2100) = 2;
);

*** Fix capacity to 0 for elh2VRE now that the equation q32_elh2VREcapfromTestor pushes elh2, not anymore elh2VRE, and capital costs are 1
vm_cap.fx(t,regi,"elh2VRE",rlf) = 0;

*** =====================================
*** END OF IntC bounds
*** =====================================

$ifthen.chpoff %cm_CHP_coup% == "off"
vm_capFac.fx(t,regi,"biochp")$(tDT32(t) AND (cm_DTcoup_eq eq 1) AND regDTCoup(regi)) = 0;
vm_capFac.fx(t,regi,"gaschp")$(tDT32(t) AND (cm_DTcoup_eq eq 1) AND regDTCoup(regi)) = 0;
vm_capFac.fx(t,regi,"coalchp")$(tDT32(t) AND (cm_DTcoup_eq eq 1) AND regDTCoup(regi)) = 0;
$endif.chpoff
vm_capFac.fx(t,regi,"csp")$(tDT32(t) AND (cm_DTcoup_eq eq 1) AND regDTCoup(regi))  = 0;
vm_capFac.fx(t,regi,"dot")$(tDT32(t) AND (cm_DTcoup_eq eq 1) AND regDTCoup(regi))  = 0;
vm_capFac.fx(t,regi,"geohdr")$(tDT32(t) AND (cm_DTcoup_eq eq 1) AND regDTCoup(regi))  = 0;

*** all flexible subsidies are set to 0 for non-coupled regions in DTcoup realization (regardless of whether cm_DTcoup is on, or elh2_coup is on)
*** This is because in calibration cm_flex_tax is turned off, and only in policy runs they are turned on
*** so one can turn off electrolysers subsidies for non coupled regions, without distortions
vm_flexAdj.fx(t,regi,te)$(teFlexTax(te) AND regNoDTCoup(regi)) = 0;
vm_flexAdj.fx(t,regi,te)$(teFlexTax(te) AND regDTCoup(regi) AND not tDT32(t)) = 0;

***CG: bound shares between 0 and 100
* v32_shStor.up(t,regi,teVRE) = 100;
* v32_shStor.lo(t,regi,teVRE) = 0;

$IFTHEN.DTcoup %cm_DTcoup% == "on"

$IFTHEN.elh2_coup %cm_elh2_coup% == "on"
v32_shSeElDem.up(t,regi,teFlexTax)$(regDTCoup(regi)) = 100;
v32_shSeElDem.lo(t,regi,teFlexTax)$(regDTCoup(regi)) = 0;
$ENDIF.elh2_coup

v32_shSeEl.up(t,regi,teDTCoupSupp)$(tDT32(t) AND regDTCoup(regi)) = 100;
v32_shSeEl.lo(t,regi,teDTCoupSupp)$(tDT32(t) AND regDTCoup(regi)) = 0;

*this turns off storage for coupled region, no need to put any additional switches on the storage equations
v32_shStor.fx(t,regi,teVRE)$(tDT32(t) AND regDTCoup(regi) AND (cm_DTcoup_eq ne 0)) = 0;

*** Fix capacity for seh2 -> seel for coupled region for now (no H2 as grid storage)
vm_cap.fx(t,regi,"h2turbVRE","1")$(tDT32(t) AND regDTCoup(regi) AND (cm_DTcoup_eq ne 0)) = 0;

*** turn off H2 turbine
vm_cap.fx(t,regi,"h2turb","1")$(tDT32(t) AND regDTCoup(regi) AND (cm_DTcoup_eq ne 0)) = 0;
vm_capFac.fx(t,regi,"h2turb")$(tDT32(t) AND regDTCoup(regi) AND (cm_DTcoup_eq ne 0)) = 0;

*fixing some less used technologies (at least for Germany) to 0 to avoid distortions
* vm_cap.fx(t,regi,"csp",rlf)$(tDT32(t) AND (cm_DTcoup_eq eq 1) AND regDTCoup(regi))  = 0;
* vm_cap.fx(t,regi,"dot",rlf)$(tDT32(t) AND (cm_DTcoup_eq eq 1) AND regDTCoup(regi))  = 0;
* vm_cap.fx(t,regi,"geohdr",rlf)$(tDT32(t) AND (cm_DTcoup_eq eq 1) AND regDTCoup(regi))  = 0;

$ENDIF.DTcoup
