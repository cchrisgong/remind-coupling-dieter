*** |  (C) 2006-2020 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de

*** SOF ./modules/32_power/DTcoup/preloop.gms

*** read marginal of seel balance equation
Execute_Loadpoint 'input' q32_balSe.m = q32_balSe.m;

*** CG: begin DIETER coupling before REMIND starts, based on input.gdx (this might
*** be revised later if coupling starts in the middle of the REMIND nash run)
$IFTHEN.DTcoup %cm_DTcoup% == "on"
***  switch on second coupling switch when coupling actually begins

*** CG: fit a polynom through oscillating fuel price data for biomass, coal and gas
execute "Rscript fuelPriceCubRegr.R";
*** make sure all DIETER problems look like the first year file
execute "python DIETER_rewrite.py";

*** CG: start a first iteration DIETER
execute "./DIETER_parallel.sh";
put "running DIETER iteration", sm32_iter:0:0;
execute './mergegdx.sh';

logfile.nr = 1;

if ( (c_keep_iteration_gdxes eq 1) ,

***CG: make iteration copies of DIETER results
    put_utility "shell" /
      "cp results_DIETER.gdx results_DIETER_i" sm32_iter:0:0 ".gdx";

    put_utility "shell" /
      "cp report_DIETER.gdx report_DIETER_i" sm32_iter:0:0 ".gdx";

    put_utility "shell" /
      "cp full_DIETER.gdx full_DIETER_i" sm32_iter:0:0 ".gdx";

***CG: saves some lst files for diagnosis
    put_utility "shell" /
      "cp DIETER_v1.0.2_1.lst DIETER_v1.0.2_1_i" sm32_iter:0:0 ".lst";
    put_utility "shell" /
      "cp DIETER_v1.0.2_10.lst DIETER_v1.0.2_10_i" sm32_iter:0:0 ".lst";
    put_utility "shell" /
      "cp DIETER_v1.0.2_15.lst DIETER_v1.0.2_15_i" sm32_iter:0:0 ".lst";

);
logfile.nr = 2;

v32_shStor.l(t,regi,te) = 0;

$ENDIF.DTcoup
*** EOF ./modules/32_power/DTcoup/preloop.gms
