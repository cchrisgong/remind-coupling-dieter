*** |  (C) 2006-2020 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de
*** SOF ./modules/32_power/DTcoup/postsolve.gms


***CG: fuel price to pass to DIETER (3 iter after coupling starts, can smooth over 3 iterations and overwrite this)
* p32_fuelprice_avgiter(t,regi,entyPe)$regDTCoup(regi) = q_balPe.m(t,regi,entyPe)$regDTCoup(regi);
** CG: smoothing fuel cost over iterations to pass to DIETER
p32_fuelprice_avgiter(t,regi,entyPe)$regDTCoup(regi)
          = (q_balPe.m(t,regi,entyPe)$regDTCoup(regi)
    		   + 2 * p32_fuelprice_lastiter(t,regi,entyPe)$regDTCoup(regi)
    			 + p32_fuelprice_lastx2iter(t,regi,entyPe)$regDTCoup(regi))
    			 / 4;

*** calculation of SE electricity price (useful for internal use and reporting purposes)
pm_SEPrice(t,regi,entySE)$(abs (qm_budget.m(t,regi)) gt sm_eps AND sameas(entySE,"seel")) =
       q32_balSe.m(t,regi,entySE) / qm_budget.m(t,regi);


*** DIETER coupling currently not used in calibration run
$ifthen.calibrate %CES_parameters% == "load"


***************************************************************
*** CG: parameters needed for diagnosis and reporting
***************************************************************

*** CG: market value as seen by REMIND
p32_marketValue(t,regi,te)$regDTCoup(regi)
      = pm_SEPrice(t,regi,"seel")$regDTCoup(regi) + vm_Mrkup.l(t,regi,te)$regDTCoup(regi);
*** CG: market price seen by sector coupling usage of power (e.g. green H2)

p32_marketPrice(t,regi,te)$regDTCoup(regi)
      = pm_SEPrice(t,regi,"seel")$regDTCoup(regi) - vm_flexAdj.l(t,regi,te)$regDTCoup(regi);

*** CG: value factor in REMIND
p32_valueFactor(t,regi,te)$regDTCoup(regi)
      = p32_marketValue(t,regi,te)$regDTCoup(regi)/(pm_SEPrice(t,regi,"seel")$regDTCoup(regi) + sm_eps);


p32_shSeEl(t,regi,te)$regDTCoup(regi) = v32_shSeEl.l(t,regi,te)$regDTCoup(regi);

*** CG: calculate budget from last iteration
p32_budget(t,regi) = qm_budget.m(t,regi);


*** CG: Total power produced (including curtailment, co-production, own consumption)
p32_totProd(t,regi,enty2)$(sameas(enty2,"seel")) =
* PE to SE transformation
    sum(pe2se(enty,enty2,te), vm_prodSe.l(t,regi,enty,enty2,te) )
* SE to SE transformation
	+ sum(se2se(enty,enty2,te), vm_prodSe.l(t,regi,enty,enty2,te) )
* Coupled production of two types of SE: such as sehe + seel, or seh2 + seel
	+ sum(pc2te(enty,entySE(enty3),te,enty2),
		pm_prodCouple(regi,enty,enty3,te,enty2) * vm_prodSe.l(t,regi,enty,enty3,te) )
* Power used in transporting and distributing FE
	+ sum(pc2te(enty4,entyFE(enty5),te,enty2),
		pm_prodCouple(regi,enty4,enty5,te,enty2) * vm_prodFe.l(t,regi,enty4,enty5,te) )
* Power used in CCS
	+ sum(pc2te(enty,enty3,te,enty2),
		sum(teCCS2rlf(te,rlf), pm_prodCouple(regi,enty,enty3,te,enty2) * vm_co2CCS.l(t,regi,enty,enty3,te,rlf) ) )
;


*** coupled production
p32_coupledProd(t,regi,enty2)$(sameas(enty2,"seel")) = sum(pc2te(enty,entySE(enty3),te,enty2),
		pm_prodCouple(regi,enty,enty3,te,enty2) * vm_prodSe.l(t,regi,enty,enty3,te) );
*** power for d&t of FE
p32_prod4dtFE(t,regi,enty2)$(sameas(enty2,"seel")) = sum(pc2te(enty4,entyFE(enty5),te,enty2),
		pm_prodCouple(regi,enty4,enty5,te,enty2) * vm_prodFe.l(t,regi,enty4,enty5,te) );
*** power for CCS
p32_prod4CCS(t,regi,enty2)$(sameas(enty2,"seel")) = sum(pc2te(enty,enty3,te,enty2),
		sum(teCCS2rlf(te,rlf), pm_prodCouple(regi,enty,enty3,te,enty2) * vm_co2CCS.l(t,regi,enty,enty3,te,rlf) ) );

*** all non SE2SE or PE2SE production terms
p32_nonSEPE2SE(t,regi,enty2)$(sameas(enty2,"seel")) = p32_coupledProd(t,regi,enty2)
    + p32_prod4dtFE(t,regi,enty2) + p32_prod4CCS(t,regi,enty2);


*** CG: total curtailment
p32_seelCurt(t,regi) = sum(teVRE, v32_storloss.l(t,regi,teVRE) );


*** total demand: including curtailment and fuel extraction power usage
p32_seelTotDem(t,regi,enty2)$(sameas(enty2,"seel")) =
  sum(se2fe(enty2,enty3,te), vm_demSe.l(t,regi,enty2,enty3,te) )
+ sum(se2se(enty2,enty3,te), vm_demSe.l(t,regi,enty2,enty3,te) )
* VRE curtailment
+ sum(teVRE, v32_storloss.l(t,regi,teVRE) )
* own consumption: electricity used for extracting fossil fuel, ususally negative
+ sum(pe2rlf(enty3,rlf2), (pm_fuExtrOwnCons(regi, enty2, enty3) * vm_fuExtr.l(t,regi,enty3,rlf2))$(pm_fuExtrOwnCons(regi, enty2, enty3) gt 0))$(t.val > 2005) !! do not use in 2005 because this demand is not contained in 05_initialCap
;

*** CG: total usable demand to pass on to DIETER: this has to
*** include the electricity consumed for extracting fuels (included in p32_seelTotDem),
*** as well as distributing and transporting them (p32_prod4dtFE) and doing CCS (p32_prod4CCS)
*** and exclude curtailment
*** (note: p32_prod4dtFE and p32_prod4CCS are negative)
p32_seelUsableDem(t,regi,enty)$(sameas(enty,"seel")) = p32_seelTotDem(t,regi,enty)
- sum(teVRE, v32_storloss.l(t,regi,teVRE) )
- p32_prod4dtFE(t,regi,enty) - p32_prod4CCS(t,regi,enty)
;

** CG: vm_demSe.l(t,regi,"seel","seh2","elh2") is how much electricity is needed to produse seh2 (green h2)
** p32_seh2elh2Dem < p32_seelUsableDem (p32_seh2elh2Dem is part of the total usable demand p32_seelUsableDem)
*** CG: without averaging
p32_seh2elh2Dem(t,regi,enty)$(sameas(enty,"seh2")) = vm_demSe.l(t,regi,"seel","seh2","elh2");

**** CG: DIETER coupling
*###################################################################
*********** INTERFACE FOR READING IN DIETER RESULTS ****************
*###################################################################
*iteration happens at N=5, 10, 15, 20, ...; if (ord(iteration) ge 5) N starts at N= 4

*** start of DIETER coupling
sm32_DTiter = 2

* if( ((ord(iteration) ge 1) and ( mod(ord(iteration), 5) eq 0)),
if( ((ord(iteration) ge sm32_DTiter) and ( mod(ord(iteration), 1) eq 0)),

sm32_iter = iteration.val - 1;
display "DIETER iteration", sm32_iter;

*   switch on second coupling switch when coupling actually begins
    if( (ord(iteration) eq sm32_DTiter) ,
        cm_DTcoup_eq = 1;
    );

*** with demand averaging
*** CG: smoothing fuel cost over iterations to pass to DIETER
* if( (ord(iteration) gt sm32_DTiter) ,
p32_seh2elh2Dem_avg(t,regi,enty)$(sameas(enty,"seh2")) =
  0.5 * (p32_seh2elh2Dem(t,regi,enty) + p32_seh2elh2Dem_last_iter(t,regi,enty));

p32_seelUsableDem_avg(t,regi,enty)$(sameas(enty,"seel")) = 0.5 * ( p32_seelUsableDem_last_iter(t,regi,enty) +
p32_seelUsableDem(t,regi,enty));
* );

* p32_seh2elh2Dem_avg(t,regi,enty)$(sameas(enty,"seh2")) = p32_seh2elh2Dem(t,regi,enty);
*
* p32_seelUsableDem_avg(t,regi,enty)$(sameas(enty,"seel")) = p32_seelUsableDem_last_iter(t,regi,enty);


* REMIND data for DIETER
    execute_unload "RMdata_4DT.gdx", vm_cap, sm32_iter, qm_budget, p32_budget, p32_seelUsableDem_avg, p32_seh2elh2Dem_avg,p32_fuelprice_avgiter,
    f21_taxCO2eqHist, pm_data, vm_costTeCapital, vm_prodSe, vm_usableSeTe, fm_dataglob, pm_dataeta, pm_eta_conv, p32_grid_factor,
    pm_ts, vm_deltaCap, vm_capEarlyReti, fm_dataemiglob, pm_cf, pm_dataren, vm_capDistr;

    execute "./DIETER_parallel.sh";
		put "running DIETER iteration", sm32_iter:0:0;

    execute './mergegdx.sh';

* .nr = 2 formats numbers in scientific notation (what we usually want for
*   debugging, because small numbers get rounded to zero otherwise, although
*   they are significant, e.g. for the calibration).  So there's a .nr = 1 up
*   front to ensure "normal" numbers.

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
      "cp DIETER_v1.0.2_10.lst DIETER_v1.0.2_10_i" sm32_iter:0:0 ".lst";
    put_utility "shell" /
      "cp DIETER_v1.0.2_11.lst DIETER_v1.0.2_11_i" sm32_iter:0:0 ".lst";
    put_utility "shell" /
      "cp DIETER_v1.0.2_12.lst DIETER_v1.0.2_12_i" sm32_iter:0:0 ".lst";

);
logfile.nr = 2;

$IFTHEN.DTcoup %cm_DTcoup% == "on"
    Execute_Loadpoint 'results_DIETER' p32_report4RM;
    Execute_Loadpoint 'results_DIETER' p32_reportmk_4RM;
*   ONLY pass on the disptachable capacity factors, since the VRE's capfac are treated differently in REMIND
*   sum over gdxfile set removes this extra index that comes from gdxmerge algorithm
*   optional: averaging capfac over 2 iterations

    p32_DIETER_curtailmentratio_last_iter(t,regi,te)$(tDT32(t) AND regDTCoup(regi)) = p32_DIETER_curtailmentratio(t,regi,te);

***CG:noCF averaging
if( (ord(iteration) eq sm32_DTiter) ,
    pm_cf(t,regi,te)$(tDT32(t) AND COALte32(te) AND regDTCoup(regi))
    			= sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"coal","capfac")$(tDT32(t) AND regDTCoup(regi)));
    pm_cf(t,regi,te)$(tDT32(t) AND NonPeakGASte32(te) AND regDTCoup(regi))
    			= sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"CCGT","capfac")$(tDT32(t) AND regDTCoup(regi)));
    pm_cf(t,regi,te)$(tDT32(t) AND BIOte32(te) AND regDTCoup(regi))
    			= sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"bio","capfac")$(tDT32(t) AND regDTCoup(regi)));
    pm_cf(t,regi,"ngt")$(tDT32(t) AND regDTCoup(regi))
    			= sum(gdxfile32, p32_report4RM(gdxfile32,t,regi,"OCGT_eff","capfac")$(tDT32(t) AND regDTCoup(regi)));
    pm_cf(t,regi,te)$(tDT32(t) AND NUCte32(te) AND regDTCoup(regi))
    			= sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"nuc","capfac")$(tDT32(t) AND regDTCoup(regi)));
);


***CG:CF averaging, only after DT is coupled for one iteration (to avoid pm_cf being distorted by default high values)
if( (ord(iteration) gt sm32_DTiter),
    p32_cf_curr_iter(t,regi,te)$(tDT32(t) AND regDTCoup(regi)) = pm_cf(t,regi,te);
    p32_cf_next_iter(t,regi,te)$(COALte32(te)) = sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"coal","capfac")$(tDT32(t) AND regDTCoup(regi)));
    p32_cf_next_iter(t,regi,te)$(NonPeakGASte32(te)) = sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"CCGT","capfac")$(tDT32(t) AND regDTCoup(regi)));

    pm_cf(t,regi,te)$(tDT32(t) AND COALte32(te) AND regDTCoup(regi))
    			= 0.5 * ( p32_cf_curr_iter(t,regi,te)$(COALte32(te)) + p32_cf_next_iter(t,regi,te)$(COALte32(te))  );
    pm_cf(t,regi,te)$(tDT32(t) AND NonPeakGASte32(te) AND regDTCoup(regi))
    			= 0.5 * ( p32_cf_curr_iter(t,regi,te)$(NonPeakGASte32(te)) + p32_cf_next_iter(t,regi,te)$(NonPeakGASte32(te)) );
    pm_cf(t,regi,te)$(tDT32(t) AND BIOte32(te) AND regDTCoup(regi))
    			= 0.5 * ( p32_cf_curr_iter(t,regi,te)$(BIOte32(te))
    			+ sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"bio","capfac")$(tDT32(t) AND regDTCoup(regi))) );
    pm_cf(t,regi,"ngt")$(tDT32(t) AND regDTCoup(regi))
    			= 0.5 * ( p32_cf_curr_iter(t,regi,"ngt")
    			+ sum(gdxfile32, p32_report4RM(gdxfile32,t,regi,"OCGT_eff","capfac")$(tDT32(t) AND regDTCoup(regi))) );
    pm_cf(t,regi,te)$(tDT32(t) AND NUCte32(te) AND regDTCoup(regi))
    			= 0.5 * ( p32_cf_curr_iter(t,regi,te)$(NUCte32(te))
    			+ sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"nuc","capfac")$(tDT32(t) AND regDTCoup(regi))) );
    pm_cf(t,regi,"elh2")$(tDT32(t) AND regDTCoup(regi)) = 0.5 * ( p32_cf_curr_iter(t,regi,"elh2")
    			+ sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"elh2","capfac")$(tDT32(t) AND regDTCoup(regi))) );
);


*   pass peak demand from DIETER to REMIND as a fraction of the total demand
    p32_peakDemand_relFac(t,regi)$(tDT32(t) AND regDTCoup(regi))
		      = sum(gdxfile32, p32_report4RM(gdxfile32,t,regi,"all_te","ResPeakDem_relFac")$(tDT32(t) AND regDTCoup(regi)));

*** dividing each DIETER tech into REMIND tech, using the last iteration REMIND share within DIETER tech category to scale down the generation share
    p32_tech_category_genshare(t,regi,te)$(BIOte32(te) AND regDTCoup(regi))
		      = p32_shSeEl(t,regi,te)$(BIOte32(te) AND regDTCoup(regi))/sum(te2$(BIOte32(te2)),p32_shSeEl(t,regi,te2)$regDTCoup(regi) + sm_eps);
		p32_tech_category_genshare(t,regi,te)$(NonPeakGASte32(te) AND regDTCoup(regi))
		      = p32_shSeEl(t,regi,te)$(NonPeakGASte32(te) AND regDTCoup(regi))/sum(te2$(NonPeakGASte32(te2)),p32_shSeEl(t,regi,te2)$regDTCoup(regi) + sm_eps);
		p32_tech_category_genshare(t,regi,te)$(NUCte32(te) AND regDTCoup(regi))
		      = p32_shSeEl(t,regi,te)$(NUCte32(te) AND regDTCoup(regi))/sum(te2$(NUCte32(te2)),p32_shSeEl(t,regi,te2)$regDTCoup(regi) + sm_eps);
		p32_tech_category_genshare(t,regi,te)$(COALte32(te) AND regDTCoup(regi))
		      = p32_shSeEl(t,regi,te)$(COALte32(te) AND regDTCoup(regi))/sum(te2$(COALte32(te2)),p32_shSeEl(t,regi,te2)$regDTCoup(regi) + sm_eps);

    p32_DIETER_shSeEl(t,regi,"spv")$(tDT32(t) AND regDTCoup(regi))
					= sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"Solar","gen_share")$(tDT32(t) AND regDTCoup(regi)));
    p32_DIETER_shSeEl(t,regi,"wind")$(tDT32(t) AND regDTCoup(regi))
					= sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"Wind_on","gen_share")$(tDT32(t) AND regDTCoup(regi)));
    p32_DIETER_shSeEl(t,regi,"ngt")$(tDT32(t) AND regDTCoup(regi))
					= sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"OCGT_eff","gen_share")$(tDT32(t) AND regDTCoup(regi)));
    p32_DIETER_shSeEl(t,regi,"hydro")$(tDT32(t) AND regDTCoup(regi))
					= sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"ror","gen_share")$(tDT32(t) AND regDTCoup(regi)));
*CG* downscaling technology shares in REMIND
    p32_DIETER_shSeEl(t,regi,te)$(tDT32(t) AND regDTCoup(regi)AND BIOte32(te) )
					= sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"bio","gen_share")$(tDT32(t) AND regDTCoup(regi)))
							*	p32_tech_category_genshare(t,regi,te)$(BIOte32(te) AND regDTCoup(regi)) ;
    p32_DIETER_shSeEl(t,regi,te)$(tDT32(t) AND regDTCoup(regi) AND NonPeakGASte32(te))
					= sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"CCGT","gen_share")$(tDT32(t) AND regDTCoup(regi)))
						 	* p32_tech_category_genshare(t,regi,te)$(NonPeakGASte32(te) AND regDTCoup(regi)) ;
    p32_DIETER_shSeEl(t,regi,te)$(tDT32(t) AND regDTCoup(regi) AND NUCte32(te))
					= sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"nuc","gen_share")$(tDT32(t) AND regDTCoup(regi)))
					  	* p32_tech_category_genshare(t,regi,te)$(NUCte32(te) AND regDTCoup(regi)) ;
    p32_DIETER_shSeEl(t,regi,te)$(tDT32(t) AND regDTCoup(regi) AND COALte32(te))
					= sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"coal","gen_share")$(tDT32(t) AND regDTCoup(regi)))
				    	* p32_tech_category_genshare(t,regi,te)$(COALte32(te) AND regDTCoup(regi)) ;

*    p32_DIETER_shSeElDem(t,regi,"elh2")$(tDT32(t) AND regDTCoup(regi))
*					= sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"elh2","dem_share")) ;

*   supply side tech market value
    p32_DIETER_MV(t,regi,te)$(tDT32(t) AND BIOte32(te) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"bio","market_value")$(tDT32(t) AND regDTCoup(regi)));
    p32_DIETER_MV(t,regi,te)$(tDT32(t) AND NonPeakGASte32(te) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"CCGT","market_value")$(tDT32(t) AND regDTCoup(regi)));
    p32_DIETER_MV(t,regi,"ngt")$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"OCGT_eff","market_value")$(tDT32(t) AND regDTCoup(regi)));
    p32_DIETER_MV(t,regi,te)$(tDT32(t) AND NUCte32(te) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"nuc","market_value")$(tDT32(t) AND regDTCoup(regi)));
    p32_DIETER_MV(t,regi,te)$(tDT32(t) AND COALte32(te) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"coal","market_value")$(tDT32(t) AND regDTCoup(regi)));
    p32_DIETER_MV(t,regi,"spv")$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"Solar","market_value")$(tDT32(t) AND regDTCoup(regi)));
    p32_DIETER_MV(t,regi,"hydro")$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"ror","market_value")$(tDT32(t) AND regDTCoup(regi)));
    p32_DIETER_MV(t,regi,"wind")$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"Wind_on","market_value")$(tDT32(t) AND regDTCoup(regi)));
*   flexible demand side tech market value (electricity price that the flex tech "sees")
    p32_DIETER_MP(t,regi,"elh2")$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"elh2","market_price")$(tDT32(t) AND regDTCoup(regi)));
    p32_DIETER_MP(t,regi,"tdels")$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"el","market_price")$(tDT32(t) AND regDTCoup(regi)));
    p32_DIETER_MP(t,regi,"tdelt")$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"el","market_price")$(tDT32(t) AND regDTCoup(regi)));
    p32_DIETER_VF(t,regi,"elh2")$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"elh2","value_factor")$(tDT32(t) AND regDTCoup(regi)));
    p32_DIETER_VF(t,regi,"tdels")$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"el","value_factor")$(tDT32(t) AND regDTCoup(regi)));
    p32_DIETER_VF(t,regi,"tdelt")$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"el","value_factor")$(tDT32(t) AND regDTCoup(regi)));

*** DIETER electricity price
    p32_DIETER_elecprice(t,regi)$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"all_te","elec_price")$(tDT32(t) AND regDTCoup(regi)));

*** CG: storage related coupling parameters
** no curt_ratio averaging
p32_DIETER_curtailmentratio(t,regi,"spv")$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"Solar","curt_ratio")$(tDT32(t) AND regDTCoup(regi)));
p32_DIETER_curtailmentratio(t,regi,"wind")$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"Wind_on","curt_ratio")$(tDT32(t) AND regDTCoup(regi)));

* with curt_ratio averaging
* p32_DIETER_curtailmentratio(t,regi,"spv")$(tDT32(t) AND regDTCoup(regi)) =
*       0.5 * (p32_DIETER_curtailmentratio_last_iter(t,regi,"spv")
*       + sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"Solar","curt_ratio")$(tDT32(t) AND regDTCoup(regi))));
*
* p32_DIETER_curtailmentratio(t,regi,"wind")$(tDT32(t) AND regDTCoup(regi)) =
*       0.5* (p32_DIETER_curtailmentratio_last_iter(t,regi,"wind")
*       + sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"Wind_on","curt_ratio")$(tDT32(t) AND regDTCoup(regi))));

* commented out no curtailment for ror right now from DIETER
*p32_DIETER_curtailmentratio(t,regi,"hydro")$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"ror","curt_ratio")$(tDT32(t) AND regDTCoup(regi)));

$ENDIF.DTcoup
);


$endif.calibrate
*** EOF ./modules/32_power/DTcoup/postsolve.gms