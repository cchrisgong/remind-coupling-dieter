*** |  (C) 2006-2019 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de
*** SOF ./modules/32_power/DTcoup/preloop.gms
$ifthen.calibrate %CES_parameters% == "load"
*
*iteration happens at N=5, 10, 15, 20, ...; if (ord(iteration) ge 5) N starts at N= 4

if( ((ord(iteration) ge 5) and ( mod(ord(iteration), 5) eq 0)),
* if( ((ord(iteration) ge 2) and ( mod(ord(iteration), 1) eq 0)),
    execute "./DIETER_parallel.sh";

    display$sleep(150) 'wait 150 seconds till DIETER is finished';

    execute './mergegdx.sh';

* .nr = 2 formats numbers in scientific notation (what we usually want for
*   debugging, because small numbers get rounded to zero otherwise, although
*   they are significant, e.g. for the calibration).  So there's a .nr = 1 up
*   front to ensure "normal" numbers.

logfile.nr = 1;

if ( (c_keep_iteration_gdxes eq 1) ,

    put_utility "shell" /
      "cp results_DIETER.gdx results_DIETER_i" iteration.val:0:0 ".gdx";

    put_utility "shell" /
      "cp report_DIETER.gdx report_DIETER_i" iteration.val:0:0 ".gdx";

    put_utility "shell" /
      "cp full_DIETER.gdx full_DIETER_i" iteration.val:0:0 ".gdx";

);
logfile.nr = 2;

$IFTHEN.DTcoup %cm_DTcoup% == "on"
    Execute_Loadpoint 'results_DIETER' report4RM;
*   ONLY pass on the disptachable capacity factors, since the VRE's capfac are treated differently in REMIND
*   sum over gdxfile set removes this extra index that comes from gdxmerge algorithm
    pm_cf(tall,"DEU",all_te)$(t_DT_32(tall) AND COALte_32(all_te)) = sum(gdxfile_32, report4RM(gdxfile_32, tall, "DEU", "coal", "capfac")$(t_DT_32(tall)));
    pm_cf(tall,"DEU",all_te)$(t_DT_32(tall) AND NonPeakGASte_32(all_te)) = sum(gdxfile_32, report4RM(gdxfile_32, tall, "DEU", "CCGT", "capfac")$(t_DT_32(tall)));
    pm_cf(tall,"DEU",all_te)$(t_DT_32(tall) AND BIOte_32(all_te)) = sum(gdxfile_32, report4RM(gdxfile_32, tall, "DEU", "bio", "capfac")$(t_DT_32(tall)));
    pm_cf(tall,"DEU","ngt")$(t_DT_32(tall)) = sum(gdxfile_32, report4RM(gdxfile_32, tall, "DEU", "OCGT_eff", "capfac")$(t_DT_32(tall)));
    pm_cf(tall,"DEU",all_te)$(t_DT_32(tall) AND NUCte_32(all_te)) = sum(gdxfile_32, report4RM(gdxfile_32, tall, "DEU", "nuc", "capfac")$(t_DT_32(tall)));

p32_peakDemand_relFac(tall,"DEU")$(t_DT_32(tall)) = sum(gdxfile_32, report4RM(gdxfile_32, tall, "DEU", "all_te", "peakDem_relFac")$(t_DT_32(tall)));

* switch on the capacity constraint
cm_DTcoup_capcon = 1;

* s32_iteration_ge_5 = 1;

*p32_flex_multmk(tall,"elh2")$(t_DT_32(tall)) = sum(gdxfile_32, report4RM(gdxfile_32, tall, "DEU", "elh2", "mult_markup")$(t_DT_32(tall)));

$ENDIF.DTcoup

);
$endif.calibrate

*** EOF ./modules/32_power/DTcoup/presolve.gms
