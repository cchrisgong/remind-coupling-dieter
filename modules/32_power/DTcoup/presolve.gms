*** |  (C) 2006-2019 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de
*** SOF ./modules/35_transport/edge_esm/presolve.gms
$ifthen.calibrate %CES_parameters% == "load"
*iteration happens at N=5, 10, 15, 20, ...; if (ord(iteration) ge 5) N starts at N= 4
if( ((ord(iteration) ge 5) and ( mod(ord(iteration), 5) eq 0)),
    Execute "gams DIETER_v1.0.2.gms";

*need to figure out this part more carefully
    iternum = ord(iteration);

    put_utility "shell" / "printf '%03i\n'" iteration.val:0:0
                              "| sed 's/\(.*\)/results_DIETER.gdx results_DIETER_\1.gdx/'"
                              "| xargs -n 2 cp"

    Execute_Loadpoint 'results_DIETER' report;

*need to figure out this part more carefully´

    p32_capStor_DIET(tall,all_regi)$(tall.val > 2010 AND sameas(all_regi, "DEU")) =
    sum(DIETER_te_stor, report(tall, "Capacity", DIETER_te_stor)$(tall.val > 2010));

**  Execute_Loadpoint 'p35_esCapCost' p35_esCapCost;
**  pm_esCapCost(t,regi,teEs_dyn35)$(t.val > 2010 AND sameas(regi,"DEU")) = p32_capStor(t,"IND","%cm_GDPscen%","%cm_EDGEtr_scen%",teEs_dyn35);

**  Execute_Loadpoint 'p35_fe2es' p35_fe2es;
**  pm_fe2es(t,regi,teEs_dyn35)$(t.val > 2010) = p35_fe2es(t,regi,"%cm_GDPscen%","%cm_EDGEtr_scen%",teEs_dyn35);

**  Execute_Loadpoint 'p35_shFeCes' p35_shFeCes;
**  pm_shFeCes(t,regi,entyFe,ppfen_dyn35,teEs_dyn35)$(p35_shFeCes(t,regi,"%cm_GDPscen%","%cm_EDGEtr_scen%",entyFe,ppfen_dyn35,teEs_dyn35) AND t.val > 2010) = p35_shFeCes(t,regi,"%cm_GDPscen%","%cm_EDGEtr_scen%",entyFe,ppfen_dyn35,teEs_dyn35);

);
$endif.calibrate

*** EOF ./modules/32_power/DTcoup/presolve.gms
