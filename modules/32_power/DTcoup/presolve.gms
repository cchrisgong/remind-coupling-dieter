*** |  (C) 2006-2019 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de
*** SOF ./modules/32_power/DTcoup/presolve.gms
$ifthen.calibrate %CES_parameters% == "load"
*
*iteration happens at N=5, 10, 15, 20, ...; if (ord(iteration) ge 5) N starts at N= 4

* if( ((ord(iteration) ge 1) and ( mod(ord(iteration), 5) eq 0)),
if( ((ord(iteration) ge 2) and ( mod(ord(iteration), 1) eq 0)),

sm32_tmp = iteration.val;
display "iteration", sm32_tmp;

*   switch on second coupling switch when coupling actually begins
    cm_DTcoup_capcon = 1;

*   alternative scalar switch
*   s32_iteration_ge_5 = 1;

    execute "./DIETER_parallel.sh";
    display$sleep(60) 'wait 100 seconds till DIETER is finished';
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

    put_utility "shell" /
      "cp DIETER_v1.0.2_10.lst DIETER_v1.0.2_10_i" iteration.val:0:0 ".lst";
    put_utility "shell" /
      "cp DIETER_v1.0.2_11.lst DIETER_v1.0.2_11_i" iteration.val:0:0 ".lst";
    put_utility "shell" /
      "cp DIETER_v1.0.2_12.lst DIETER_v1.0.2_12_i" iteration.val:0:0 ".lst";
);
logfile.nr = 2;

$IFTHEN.DTcoup %cm_DTcoup% == "on"

    Execute_Loadpoint 'results_DIETER' report4RM;
*   ONLY pass on the disptachable capacity factors, since the VRE's capfac are treated differently in REMIND
*   sum over gdxfile set removes this extra index that comes from gdxmerge algorithm
    pm_cf(t,"DEU",te)$(tDT32(t) AND COALte32(te)) = sum(gdxfile32,report4RM(gdxfile32,t,"DEU","coal","capfac")$(tDT32(t)));
    pm_cf(t,"DEU",te)$(tDT32(t) AND NonPeakGASte32(te)) = sum(gdxfile32,report4RM(gdxfile32,t,"DEU","CCGT","capfac")$(tDT32(t)));
    pm_cf(t,"DEU",te)$(tDT32(t) AND BIOte32(te)) = sum(gdxfile32,report4RM(gdxfile32,t,"DEU","bio","capfac")$(tDT32(t)));
    pm_cf(t,"DEU","ngt")$(tDT32(t)) = sum(gdxfile32, report4RM(gdxfile32,t,"DEU","OCGT_eff","capfac")$(tDT32(t)));
    pm_cf(t,"DEU",te)$(tDT32(t) AND NUCte32(te)) = sum(gdxfile32,report4RM(gdxfile32,t,"DEU","nuc","capfac")$(tDT32(t)));

*   pass peak demand from DIETER to REMIND as a fraction of the total demand
    p32_peakDemand_relFac(t,"DEU")$(tDT32(t)) = sum(gdxfile32, report4RM(gdxfile32,t,"DEU","all_te","peakDem_relFac")$(tDT32(t)));

*   flexible demand side tech (might be able to be replaced by the mkup implementation)
*   p32_flex_multmk(t,"elh2")$(tDT32(t)) = sum(gdxfile32, report4RM(gdxfile32, t, "DEU", "elh2", "valuefactor")$(tDT32(t)));

    p32_DIETER_shSeEl(t,"DEU","spv")$(tDT32(t)) = sum(gdxfile32,report4RM(gdxfile32,t,"DEU","Solar","gen_share")$(tDT32(t)));

    p32_shSeEl(t,"DEU",te) = v32_shSeEl.l(t,"DEU",te);
    p32_deltaCap(t,"DEU",te,rlf) = vm_deltaCap.l(t,"DEU",te,rlf);

    Execute_Loadpoint 'results_DIETER' reportmk_4RM;

    p32_DIETER_VF(t,te)$(tDT32(t) AND BIOte32(te)) = sum(gdxfile32,reportmk_4RM(gdxfile32,t,"DEU","bio","valuefactor")$(tDT32(t)));
    p32_DIETER_VF(t,te)$(tDT32(t) AND NonPeakGASte32(te)) = sum(gdxfile32,reportmk_4RM(gdxfile32,t,"DEU","CCGT","valuefactor")$(tDT32(t)));
    p32_DIETER_VF(t,"ngt")$(tDT32(t)) = sum(gdxfile32,reportmk_4RM(gdxfile32,t,"DEU","OCGT_eff","valuefactor")$(tDT32(t)));
    p32_DIETER_VF(t,te)$(tDT32(t) AND NUCte32(te)) = sum(gdxfile32,reportmk_4RM(gdxfile32,t,"DEU","nuc","valuefactor")$(tDT32(t)));
    p32_DIETER_VF(t,te)$(tDT32(t) AND COALte32(te)) = sum(gdxfile32,reportmk_4RM(gdxfile32,t,"DEU","coal","valuefactor")$(tDT32(t)));
    p32_DIETER_VF(t,"spv")$(tDT32(t)) = sum(gdxfile32,reportmk_4RM(gdxfile32,t,"DEU","Solar","valuefactor")$(tDT32(t)));
    p32_DIETER_VF(t,"hydro")$(tDT32(t)) = sum(gdxfile32,reportmk_4RM(gdxfile32,t,"DEU","ror","valuefactor")$(tDT32(t)));
    p32_DIETER_VF(t,"wind")$(tDT32(t)) = sum(gdxfile32,reportmk_4RM(gdxfile32,t,"DEU","Wind_on","valuefactor")$(tDT32(t)));

$ENDIF.DTcoup

);

* p32_peakDemand_relFac(t,"DEU")$(tDT32(t)) = 0.000155891;
*
* *** calculate CF for dispatchable from solar pv share
* pm_cf_linear(tDT32,"DEU",DISPATCHte32_2) = pm_cf(tDT32,"DEU",DISPATCHte32_2) * ( 1 - 0.5 * v32_shSeEl.l(tDT32,"DEU","spv") / 100);


$endif.calibrate

*** EOF ./modules/32_power/DTcoup/presolve.gms
