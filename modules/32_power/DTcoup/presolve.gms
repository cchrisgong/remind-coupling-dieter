*** |  (C) 2006-2020 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de
*** SOF ./modules/32_power/DTcoup/presolve.gms

* *** calculate CF for dispatchable from solar pv share
* pm_cf_linear(tDT32,regi,DISPATCHte32_2)$regDTCoup(regi) = pm_cf(tDT32,regi,DISPATCHte32_2)$regDTCoup(regi) * ( 1 - 0.5 * v32_shSeEl.l(tDT32,regi,"spv")$regDTCoup(regi) / 100);


*** calculation of SE electricity price (useful for internal use and reporting purposes)
pm_SEPrice(t,regi,entySE)$(abs (qm_budget.m(t,regi)) gt sm_eps AND sameas(entySE,"seel")) =
       q32_balSe.m(t,regi,entySE) / qm_budget.m(t,regi);
*Display "electricity price", pm_SEPrice(t,"DEU","seel");


p32_budget(t,regi) = qm_budget.m(t,regi);
pm_prodSe(t,regi,enty,enty2,te) = vm_prodSe.l(t,regi,enty,enty2,te);
pm_demSe(t,regi,enty,enty2,te) = vm_demSe.l(t,regi,enty,enty2,te);

*** CG:load fuel prices from two previous iterations, avoid using marginals in case they are 0
p32_fuelprice_lastx2iter(t,regi,entyPe)$(regDTCoup(regi)) = p32_fuelprice_lastiter(t,regi,entyPe);
p32_fuelprice_lastiter(t,regi,entyPe)$(regDTCoup(regi)) = p32_fuelprice_curriter(t,regi,entyPe);

p32_seelUsableDem_last_iter(t,regi,enty)$(sameas(enty,"seel")) = p32_seelUsableDem(t,regi,enty);

$IFTHEN.DTcoup %cm_DTcoup% == "on"
$IFTHEN.elh2_coup %cm_elh2_coup% == "on"
p32_shSeElDem(t,regi,te)$regDTCoup(regi) = v32_shSeElDem.l(t,regi,te)$regDTCoup(regi);
p32_seh2elh2Dem_last_iter(t,regi,enty)$(sameas(enty,"seh2")) = vm_demSe.l(t,regi,"seel","seh2","elh2");
$ENDIF.elh2_coup
$ENDIF.DTcoup

*** EOF ./modules/32_power/DTcoup/presolve.gms
