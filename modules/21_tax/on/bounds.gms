*** |  (C) 2006-2019 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de
*** SOF ./modules/21_tax/on/bounds.gms
*cb no taxes in 2005, fix budget equation term to zero
vm_taxrev.fx("2005",regi) = 0;

*** set non-DEU spv markups to 0
vm_flexAdj.fx(t,all_regi,all_te)$(COUPte(all_te) AND not SameAs(all_regi,"DEU")) = 0;
*** set elh2 markups to 0 (Felix's input is read in in p80 from input.gdx)
vm_flexAdj.fx(t,all_regi,all_te)$(teFlex(all_te)) = 0;
*** set later years' markups to 2100's value
vm_flexAdj.fx(t,all_regi,all_te)$(t.val ge 2110) = vm_flexAdj("2100",all_regi,all_te);

sm21_tmp = iteration.val;
display "vm_flexAdj", vm_flexAdj.l;
display "iteration", sm21_tmp;
Display "end of bound tax chris";
* Display iteration.val;
*** EOF ./modules/21_tax/on/bounds.gms
