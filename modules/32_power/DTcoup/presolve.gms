*** |  (C) 2006-2019 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de
*** SOF ./modules/32_power/DTcoup/presolve.gms


* *** calculate CF for dispatchable from solar pv share
* pm_cf_linear(tDT32,"DEU",DISPATCHte32_2) = pm_cf(tDT32,"DEU",DISPATCHte32_2) * ( 1 - 0.5 * v32_shSeEl.l(tDT32,"DEU","spv") / 100);

*** FS: calculate electricity price of last iteration in trUSD2005/TWa
p32_budget(t,regi) = qm_budget.m(t,regi);
pm_SEPrice(t,regi,"seel") = q32_balSe.m(t,regi,"seel")/(qm_budget.m(t,regi) + sm_eps);
*Display "electricity price", pm_SEPrice(t,"DEU","seel");

*** CG:load fuel prices from two previous iterations
p32_fuelprice_lastx2iter(t,"DEU",entyPe) = p32_fuelprice_lastiter(t,"DEU",entyPe);
p32_fuelprice_lastiter(t,"DEU",entyPe) = q_balPe.m(t,"DEU",entyPe);

*** EOF ./modules/32_power/DTcoup/presolve.gms
