*** |  (C) 2006-2020 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de
*** SOF ./modules/21_tax/on/bounds.gms
*cb no taxes in 2005, fix budget equation term to zero
vm_taxrev.fx("2005",regi) = 0;

$IFTHEN.DTcoup %cm_DTcoup% == "on"
*** CG: set spv markups to 0 for non coupled regions
vm_Mrkup.fx(t,regi,te)$(teDTCoupSupp(te) AND regNoDTCoup(regi)) = 0;
*** CG: set spv markups to 0 for non-coupled years for coupled regions
vm_Mrkup.fx(t,regi,te)$(teDTCoupSupp(te) AND regDTCoup(regi) AND not tDT32(t)) = 0;
*v21_taxrevMrkup.fx(t,all_regi)$(regDTCoup(regi) AND not tDT32(t)) = 0;


* vm_flexAdj.fx(t,regi,te)$(teFlexTax(te) AND regNoDTCoup(regi)) = 0;
* vm_flexAdj.fx(t,regi,te)$(teFlexTax(te) AND regDTCoup(regi) AND not tDT32(t)) = 0;

*** CG dislay:
sm21_tmp = iteration.val;
display "vm_Mrkup", vm_Mrkup.l;
display "vm_flexAdj", vm_flexAdj.l;
display "iteration", sm21_tmp;
Display "end of bound tax chris";
$ENDIF.DTcoup

*** EOF ./modules/21_tax/on/bounds.gms
