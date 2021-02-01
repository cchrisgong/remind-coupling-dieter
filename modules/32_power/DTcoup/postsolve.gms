*** |  (C) 2006-2019 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de
*** SOF ./modules/32_power/DTcoup/postsolve.gms
	p32_seelDem(t,regi,enty2)$(sameas(enty2,"seel")) = sum(se2fe(enty2,enty3,te), vm_demSe.l(t,regi,enty2,enty3,te) )
									+ sum(se2se(enty2,enty3,te), vm_demSe.l(t,regi,enty2,enty3,te) )
									+ sum(pe2rlf(enty3,rlf2), (pm_fuExtrOwnCons(regi, enty2, enty3) * vm_fuExtr.l(t,regi,enty3,rlf2))$(pm_fuExtrOwnCons(regi, enty2, enty3) gt 0))$(t.val > 2005) !! don't use in 2005 because this demand is not contained in 05_initialCap
	;

  p32_marketValue_spv(t) = p32_DIETER_VF(t,"spv") * ( 1 - (v32_shSeEl.l(t,"DEU","spv")/ 100  - p32_shSeEl(t,"DEU","spv")/ 100 )  )
											* pm_priceSeel(t,"DEU")
	;

*** EOF ./modules/32_power/DTcoup/postsolve.gms
