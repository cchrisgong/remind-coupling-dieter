*** |  (C) 2006-2019 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de
*** SOF ./modules/21_tax/on/bounds.gms
*cb no taxes in 2005, fix budget equation term to zero
vm_taxrev.fx("2005",regi) = 0;

*** CG: set non-DEU spv markups to 0
vm_Mrkup.fx(t,all_regi,all_te)$(teDTCoupSupp(all_te) AND not SameAs(all_regi,"DEU")) = 0;
*** CG: set elh2 markups to 0 (Felix's input is read in in p80 from input.gdx), later this line can be removed
vm_Mrkup.fx(t,all_regi,all_te)$(teFlex(all_te)) = 0;

*** CG dislay:
sm21_tmp = iteration.val;
display "vm_Mrkup", vm_Mrkup.l;
display "iteration", sm21_tmp;
Display "end of bound tax chris";


*** EOF ./modules/21_tax/on/bounds.gms
