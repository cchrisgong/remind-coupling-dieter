*** |  (C) 2006-2020 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de
***-----------------------------------------------------------
***                  module specific bounds
***------------------------------------------------------------

vm_capFac.fx(t,regi,te) = pm_cf(t,regi,te);

$IFTHEN.DTcoup %cm_DTcoup% == "on"
*** CG: when coupling is on relax constraint for coupled region, coupled years and coupled tech
*** such that it can be adjusted depending on share dependent prefactor
if ((cm_DTcoup_eq eq 1),
		loop(regi$(regDTCoup(regi)),
			loop(t$(tDT32(t)),
				loop(te$(teDTCoupSupp(te)),
				vm_capFac.lo(t,regi,te)=0;
				vm_capFac.up(t,regi,te)=INF;
				);
			);
		);
);
$ENDIF.DTcoup


$IFTHEN.DTcoup %cm_DTcoup% == "on"

*v32_capPriceExponent.up(t,regi)$(regDTCoup(regi)) = 20;

* vm_capFac.fx(t,regi,te) = pm_cf_linear(t,regi,te);
$ENDIF.DTcoup

*** FS: for historically limited biomass production scenario (cm_bioprod_histlim >= 0)
*** to avoid infeasibilities with vintage biomass capacities
*** allow bio techs to reduce capacity factor
if ( cm_bioprod_histlim ge 0,
	vm_capFac.lo(t,regi_sensscen,teBioPebiolc)$(t.val ge 2030) = 0;
);

*** FS: if flexibility tax on, let capacity factor be endogenuously determined between 0.1 and 1
*** for technologies that get flexibility tax/subsity (teFlexTax)
if ( cm_flex_tax eq 1,
  if ( cm_FlexTaxFeedback eq 1,
*** if flexibility tax feedback is on, let model choose capacity factor of flexible technologies freely
	  vm_capFac.lo(t,regi,teFlexTax)$(t.val ge 2010) = 0.1;
    vm_capFac.up(t,regi,teFlexTax)$(t.val ge 2010) = pm_cf(t,regi,teFlexTax);
  else
*** if flexibility tax feedback is off, only flexibliity tax benefit for flexible technologies and 0.5 capacity factor
    vm_capFac.fx(t,regi,teFlex)$(t.val ge 2010) = 0.5;
*** electricity price of inflexible technologies the same w/o feedback
    v32_flexPriceShare.fx(t,regi,te)$(teFlexTax(te) AND NOT(teFlex(te))) = 1;
  );
);

*RP* upper bound of 90% on share of electricity produced by a single VRE technology, and lower bound on usablese to prevent the solver from dividing by 0
v32_shSeEl.up(t,regi,teVRE) = 90;

vm_usableSe.lo(t,regi,"seel")= 1e-6;
*RP To ensure that the REMIND model doesn't overlook CSP due to gdx effects, ensure some minimum use in regions with good solar insolation, here proxied from the csp storage factor:
loop(regi$(p32_factorStorage(regi,"csp") < 1),
  v32_shSeEl.lo(t,regi,"csp")$(t.val > 2025) = 0.5;
  v32_shSeEl.lo(t,regi,"csp")$(t.val > 2050) = 1;
  v32_shSeEl.lo(t,regi,"csp")$(t.val > 2100) = 2;
);

*** Lower bounds on VRE use (more than 0.01% of electricity demand) after 2015 to prevent the model from overlooking spv and wind and csp
loop(regi$(regNoDTCoup(regi)),
  loop(te$(teVRE(te)),
    if ( (sum(rlf, pm_dataren(regi,"maxprod",rlf,te)) > 0.01 * pm_IO_input(regi,"seel","feels","tdels")) ,
         v32_shSeEl.lo(t,regi,te)$(t.val>2020) = 0.01;
    );
  );
);

*** Fix capacity for h2curt technology (modeled only in RLDC)
vm_cap.fx(t,regi,"h2curt",rlf) = 0;


*** Fix capacity to 0 for elh2VRE now that the equation q32_elh2VREcapfromTestor pushes elh2, not anymore elh2VRE, and capital costs are 1
vm_cap.fx(t,regi,"elh2VRE",rlf) = 0;

$IFTHEN.DTcoup %cm_DTcoup% == "on"

***CG: bound shares between 0 and 100
v32_shStor.up(t,regi,te) = 100;
v32_shStor.lo(t,regi,te) = 0;

*this turns off storage for coupled region, no need to put any additional switches on the storage equations
v32_shStor.fx(t,regi,te)$(regDTCoup(regi) AND cm_DTcoup_eq eq 1) = 0;

v32_shSeElDem.up(t,regi,teFlexTax) = 100;
v32_shSeElDem.lo(t,regi,teFlexTax) = 0;

*** Fix capacity for seh2 -> seel for DEU for now (no H2 as grid storage)
vm_cap.fx(t,regi,"h2turbVRE","1")$(regDTCoup(regi) AND cm_DTcoup_eq eq 1) = 0;

*fixing some less used technologies (at least for Germany) to 0 to avoid distortions
* vm_cap.fx(t,regi,"csp",rlf)$(regDTCoup(regi) AND cm_DTcoup_eq eq 1 AND (t.val > 2020))  = 0;
* vm_cap.fx(t,regi,"dot",rlf)$(regDTCoup(regi) AND cm_DTcoup_eq eq 1 AND (t.val > 2020))  = 0;
* vm_cap.fx(t,regi,"geohdr",rlf)$(regDTCoup(regi) AND cm_DTcoup_eq eq 1 AND (t.val > 2020))  = 0;

$ENDIF.DTcoup
