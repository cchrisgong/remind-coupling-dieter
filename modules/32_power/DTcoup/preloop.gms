*** |  (C) 2006-2020 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de

*** SOF ./modules/32_power/DTcoup/preloop.gms

*** read marginal of seel balance equation
Execute_Loadpoint 'input' q32_balSe.m = q32_balSe.m;

Execute_Loadpoint 'input' vm_demSe.l = vm_demSe.l;

Execute_Loadpoint 'input' q_balPe.m = q_balPe.m;

Execute_Loadpoint "RMdata_4RM" p21_taxrevMrkup0 = p21_taxrevMrkup0;

Execute_Loadpoint "RMdata_4RM" p21_taxrevFlex0 = p21_taxrevFlex0;

Execute_Loadpoint "RMdata_4RM" v21_taxrevFlex.l = v21_taxrevFlex.l;

Execute_Loadpoint "RMdata_4RM" v21_taxrevMrkup.l = v21_taxrevMrkup.l;

*** EOF ./modules/32_power/DTcoup/preloop.gms
