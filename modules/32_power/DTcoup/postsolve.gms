*** |  (C) 2006-2019 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de
*** SOF ./modules/32_power/DTcoup/postsolve.gms

p32_shSeEl(t,"DEU",te) = v32_shSeEl.l(t,"DEU",te);
* p32_deltaCap(t,"DEU",te,rlf) = vm_deltaCap.l(t,"DEU",te,rlf);

*** CG: total electricity demand to be passed on to DIETER
p32_seelDem(t,regi,enty2) = sum(se2fe(enty2,enty3,te), vm_demSe.l(t,regi,enty2,enty3,te) )
									+ sum(se2se(enty2,enty3,te), vm_demSe.l(t,regi,enty2,enty3,te) )
* + sum(teVRE, v32_storloss.l(t,regi,teVRE) )
									+ sum(pe2rlf(enty3,rlf2), (pm_fuExtrOwnCons(regi, enty2, enty3) * vm_fuExtr.l(t,regi,enty3,rlf2))$(pm_fuExtrOwnCons(regi, enty2, enty3) gt 0))$(t.val > 2005) !! don't use in 2005 because this demand is not contained in 05_initialCap;
;

*** CG: smoothing fuel cost over iterations
p32_fuelprice_avgiter(t,"DEU",entyPe) = (q_balPe.m(t,"DEU",entyPe) + 2 * p32_fuelprice_lastiter(t,"DEU",entyPe) + p32_fuelprice_lastx2iter(t,"DEU",entyPe)) / 4;

*** CG: market value as seen by REMIND
p32_marketValue(t,te) = pm_SEPrice(t,"DEU","seel") + vm_Mrkup.l(t,"DEU",te);

*** CG: value factor in REMIND
p32_valueFactor(t,te) = p32_marketValue(t,te)/(pm_SEPrice(t,"DEU","seel") + sm_eps);



$ifthen.calibrate %CES_parameters% == "load"
*** CG: DIETER coupling
$IFTHEN.DTcoup %cm_DTcoup% == "on"
*iteration happens at N=5, 10, 15, 20, ...; if (ord(iteration) ge 5) N starts at N= 4

* if( ((ord(iteration) ge 1) and ( mod(ord(iteration), 5) eq 0)),
if( ((ord(iteration) ge 2) and ( mod(ord(iteration), 1) eq 0)),

sm32_tmp = iteration.val;
display "iteration", sm32_tmp;

*   switch on second coupling switch when coupling actually begins
    if( (ord(iteration) eq 2) ,
        cm_DTcoup_capcon = 1;
    );

    execute "./DIETER_parallel.sh";
		put "running DIETER iteration", sm32_tmp:0:0;

    execute './mergegdx.sh';

* .nr = 2 formats numbers in scientific notation (what we usually want for
*   debugging, because small numbers get rounded to zero otherwise, although
*   they are significant, e.g. for the calibration).  So there's a .nr = 1 up
*   front to ensure "normal" numbers.

logfile.nr = 1;

if ( (c_keep_iteration_gdxes eq 1) ,

***CG: make iteration copies of DIETER results
    put_utility "shell" /
      "cp results_DIETER.gdx results_DIETER_i" sm32_tmp:0:0 ".gdx";

    put_utility "shell" /
      "cp report_DIETER.gdx report_DIETER_i" sm32_tmp:0:0 ".gdx";

    put_utility "shell" /
      "cp full_DIETER.gdx full_DIETER_i" sm32_tmp:0:0 ".gdx";

***CG: saves some lst files for diagnosis
    put_utility "shell" /
      "cp DIETER_v1.0.2_10.lst DIETER_v1.0.2_10_i" sm32_tmp:0:0 ".lst";
    put_utility "shell" /
      "cp DIETER_v1.0.2_11.lst DIETER_v1.0.2_11_i" sm32_tmp:0:0 ".lst";
    put_utility "shell" /
      "cp DIETER_v1.0.2_12.lst DIETER_v1.0.2_12_i" sm32_tmp:0:0 ".lst";


);
logfile.nr = 2;

    Execute_Loadpoint 'results_DIETER' p32_report4RM;
    Execute_Loadpoint 'results_DIETER' p32_reportmk_4RM;
*   ONLY pass on the disptachable capacity factors, since the VRE's capfac are treated differently in REMIND
*   sum over gdxfile set removes this extra index that comes from gdxmerge algorithm
    pm_cf(t,"DEU",te)$(tDT32(t) AND COALte32(te)) = sum(gdxfile32,p32_report4RM(gdxfile32,t,"DEU","coal","capfac")$(tDT32(t)));
    pm_cf(t,"DEU",te)$(tDT32(t) AND NonPeakGASte32(te)) = sum(gdxfile32,p32_report4RM(gdxfile32,t,"DEU","CCGT","capfac")$(tDT32(t)));
    pm_cf(t,"DEU",te)$(tDT32(t) AND BIOte32(te)) = sum(gdxfile32,p32_report4RM(gdxfile32,t,"DEU","bio","capfac")$(tDT32(t)));
    pm_cf(t,"DEU","ngt")$(tDT32(t)) = sum(gdxfile32, p32_report4RM(gdxfile32,t,"DEU","OCGT_eff","capfac")$(tDT32(t)));
    pm_cf(t,"DEU",te)$(tDT32(t) AND NUCte32(te)) = sum(gdxfile32,p32_report4RM(gdxfile32,t,"DEU","nuc","capfac")$(tDT32(t)));

*   pass peak demand from DIETER to REMIND as a fraction of the total demand
    p32_peakDemand_relFac(t,"DEU")$(tDT32(t)) = sum(gdxfile32, p32_report4RM(gdxfile32,t,"DEU","all_te","ResPeakDem_relFac")$(tDT32(t)));

*   flexible demand side tech (might be able to be replaced by the mkup implementation)
*   p32_flex_multmk(t,"elh2")$(tDT32(t)) = sum(gdxfile32, p32_report4RM(gdxfile32, t, "DEU", "elh2", "valuefactor")$(tDT32(t)));

*** dividing each DIETER tech into REMIND tech, using the last iteration REMIND share within DIETER tech category to scale down the generation share
    p32_tech_category_genshare(t,"DEU",te)$(BIOte32(te)) = p32_shSeEl(t,"DEU",te)$(BIOte32(te))/sum(te2$(BIOte32(te2)),p32_shSeEl(t,"DEU",te2)+sm_eps);
		p32_tech_category_genshare(t,"DEU",te)$(NonPeakGASte32(te)) = p32_shSeEl(t,"DEU",te)$(NonPeakGASte32(te))/sum(te2$(NonPeakGASte32(te2)),p32_shSeEl(t,"DEU",te2)+sm_eps);
		p32_tech_category_genshare(t,"DEU",te)$(NUCte32(te)) = p32_shSeEl(t,"DEU",te)$(NUCte32(te))/sum(te2$(NUCte32(te2)),p32_shSeEl(t,"DEU",te2)+sm_eps);
		p32_tech_category_genshare(t,"DEU",te)$(COALte32(te)) = p32_shSeEl(t,"DEU",te)$(COALte32(te))/sum(te2$(COALte32(te2)),p32_shSeEl(t,"DEU",te2) +sm_eps);

    p32_DIETER_shSeEl(t,"DEU","spv")$(tDT32(t)) = sum(gdxfile32,p32_report4RM(gdxfile32,t,"DEU","Solar","gen_share")$(tDT32(t)));
    p32_DIETER_shSeEl(t,"DEU","wind")$(tDT32(t)) = sum(gdxfile32,p32_report4RM(gdxfile32,t,"DEU","Wind_on","gen_share")$(tDT32(t)));
    p32_DIETER_shSeEl(t,"DEU","ngt")$(tDT32(t)) = sum(gdxfile32,p32_report4RM(gdxfile32,t,"DEU","OCGT_eff","gen_share")$(tDT32(t)));
    p32_DIETER_shSeEl(t,"DEU","hydro")$(tDT32(t)) = sum(gdxfile32,p32_report4RM(gdxfile32,t,"DEU","ror","gen_share")$(tDT32(t)));
    p32_DIETER_shSeEl(t,"DEU",te)$(tDT32(t) AND BIOte32(te)) = sum(gdxfile32,p32_report4RM(gdxfile32,t,"DEU","bio","gen_share")$(tDT32(t)))
																																*	p32_tech_category_genshare(t,"DEU",te)$(BIOte32(te)) ;
    p32_DIETER_shSeEl(t,"DEU",te)$(tDT32(t) AND NonPeakGASte32(te))= sum(gdxfile32,p32_report4RM(gdxfile32,t,"DEU","CCGT","gen_share")$(tDT32(t)))
																											            	* p32_tech_category_genshare(t,"DEU",te)$(NonPeakGASte32(te)) ;
    p32_DIETER_shSeEl(t,"DEU",te)$(tDT32(t) AND NUCte32(te)) = sum(gdxfile32,p32_report4RM(gdxfile32,t,"DEU","nuc","gen_share")$(tDT32(t)))
																													   	* p32_tech_category_genshare(t,"DEU",te)$(NUCte32(te)) ;
    p32_DIETER_shSeEl(t,"DEU",te)$(tDT32(t) AND COALte32(te)) = sum(gdxfile32,p32_report4RM(gdxfile32,t,"DEU","coal","gen_share")$(tDT32(t)))
																																			* p32_tech_category_genshare(t,"DEU",te)$(COALte32(te)) ;

    p32_DIETER_VF(t,te)$(tDT32(t) AND BIOte32(te)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,"DEU","bio","valuefactor")$(tDT32(t)));
    p32_DIETER_VF(t,te)$(tDT32(t) AND NonPeakGASte32(te)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,"DEU","CCGT","valuefactor")$(tDT32(t)));
    p32_DIETER_VF(t,"ngt")$(tDT32(t)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,"DEU","OCGT_eff","valuefactor")$(tDT32(t)));
    p32_DIETER_VF(t,te)$(tDT32(t) AND NUCte32(te)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,"DEU","nuc","valuefactor")$(tDT32(t)));
    p32_DIETER_VF(t,te)$(tDT32(t) AND COALte32(te)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,"DEU","coal","valuefactor")$(tDT32(t)));
    p32_DIETER_VF(t,"spv")$(tDT32(t)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,"DEU","Solar","valuefactor")$(tDT32(t)));
    p32_DIETER_VF(t,"hydro")$(tDT32(t)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,"DEU","ror","valuefactor")$(tDT32(t)));
    p32_DIETER_VF(t,"wind")$(tDT32(t)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,"DEU","Wind_on","valuefactor")$(tDT32(t)));

    p32_DIETER_MV(t,te)$(tDT32(t) AND BIOte32(te)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,"DEU","bio","market_value")$(tDT32(t)));
    p32_DIETER_MV(t,te)$(tDT32(t) AND NonPeakGASte32(te)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,"DEU","CCGT","market_value")$(tDT32(t)));
    p32_DIETER_MV(t,"ngt")$(tDT32(t)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,"DEU","OCGT_eff","market_value")$(tDT32(t)));
    p32_DIETER_MV(t,te)$(tDT32(t) AND NUCte32(te)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,"DEU","nuc","market_value")$(tDT32(t)));
    p32_DIETER_MV(t,te)$(tDT32(t) AND COALte32(te)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,"DEU","coal","market_value")$(tDT32(t)));
    p32_DIETER_MV(t,"spv")$(tDT32(t)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,"DEU","Solar","market_value")$(tDT32(t)));
    p32_DIETER_MV(t,"hydro")$(tDT32(t)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,"DEU","ror","market_value")$(tDT32(t)));
    p32_DIETER_MV(t,"wind")$(tDT32(t)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,"DEU","Wind_on","market_value")$(tDT32(t)));

    p32_DIETER_elecprice(t)$(tDT32(t)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,"DEU","all_te","elec_price")$(tDT32(t)));


);

$ENDIF.DTcoup

$endif.calibrate


*** EOF ./modules/32_power/DTcoup/postsolve.gms
