*** |  (C) 2006-2019 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de
*** SOF ./modules/32_power/DTcoup/presolve.gms


* *** calculate CF for dispatchable from solar pv share
* pm_cf_linear(tDT32,regi,DISPATCHte32_2)$regDTCoup(regi) = pm_cf(tDT32,regi,DISPATCHte32_2)$regDTCoup(regi) * ( 1 - 0.5 * v32_shSeEl.l(tDT32,regi,"spv")$regDTCoup(regi) / 100);

*** FS: calculate electricity price of last iteration in trUSD2005/TWa
p32_budget(t,regi) = qm_budget.m(t,regi);
pm_SEPrice(t,regi,"seel") = q32_balSe.m(t,regi,"seel")/(qm_budget.m(t,regi) + sm_eps);
*Display "electricity price", pm_SEPrice(t,"DEU","seel");

$IFTHEN.DTcoup %cm_DTcoup% == "on"

p32_seh2elh2Dem_last_iter(t,regi,enty)$(sameas(enty,"seh2")) = vm_demSe.l(t,regi,"seel","seh2","elh2");
p32_seelUsableDem_last_iter(t,regi,enty)$(sameas(enty,"seel")) = p32_seelUsableDem(t,regi,enty);

*** CG:load fuel prices from two previous iterations
p32_fuelprice_lastx2iter(t,regi,entyPe)$(regDTCoup(regi)) = p32_fuelprice_lastiter(t,regi,entyPe);
p32_fuelprice_lastiter(t,regi,entyPe)$(regDTCoup(regi)) = q_balPe.m(t,regi,entyPe);

p32_reqCap(t,regi)$(regDTCoup(regi)) = p32_peakDemand_relFac(t,regi) * p32_seelUsableDem(t,regi,"seel") * 8760;
p32_capDecayStart(t,regi)$(regDTCoup(regi)) = p32_reqCap(t,regi) * 1.0;
p32_capDecayEnd(t,regi)$(regDTCoup(regi)) = p32_reqCap(t,regi) * 1.1;
Display p32_capDecayStart;
Display p32_capDecayEnd;
$ENDIF.DTcoup
*** EOF ./modules/32_power/DTcoup/presolve.gms
