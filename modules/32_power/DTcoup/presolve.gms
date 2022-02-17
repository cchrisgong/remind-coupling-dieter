*** |  (C) 2006-2020 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de
*** SOF ./modules/32_power/DTcoup/presolve.gms

* *** calculate CF for dispatchable from solar pv share
* pm_cf_linear(tDT32,regi,DISPATCHte32_2)$regDTCoup(regi) = pm_cf(tDT32,regi,DISPATCHte32_2)$regDTCoup(regi) * ( 1 - 0.5 * v32_shSeEl.l(tDT32,regi,"spv")$regDTCoup(regi) / 100);

*** calculation of SE electricity price (useful for internal use and reporting purposes)
pm_SEPrice(t,regi,entySE)$(abs (qm_budget.m(t,regi)) gt sm_eps AND sameas(entySE,"seel")) =
       q32_balSe.m(t,regi,entySE) / qm_budget.m(t,regi);

$IFTHEN.DTcoup %cm_DTcoup% == "on"
p32_budget(t,regi)$(tDT32(t) AND regDTCoup(regi)) = qm_budget.m(t,regi);

*** CG: load fuel prices from two previous iterations, avoid using marginals in case they might be zero
p32_fuelprice_lastx2iter(t,regi,entyPe)$(regDTCoup(regi)) = p32_fuelprice_lastiter(t,regi,entyPe);
p32_fuelprice_lastiter(t,regi,entyPe)$(regDTCoup(regi)) = p32_fuelprice_curriter(t,regi,entyPe);

$IFTHEN.dem_avg %cm_DTdem_avg% == "on"
*** CG: last iteration total and flexible seel demand/production to be passed to dieter
p32_usableSeDispLaIter(t,regi,entySE)$(tDT32(t) AND regDTCoup(regi) AND sameas(entySE,"seel")) = p32_usableSeDisp(t,regi,entySE);
p32_seh2elh2DemLaIter(t,regi,entySE)$(tDT32(t) AND regDTCoup(regi) AND sameas(entySE,"seh2")) = p32_seh2elh2Dem(t,regi,entySE);
$ENDIF.dem_avg
$ENDIF.DTcoup

$IFTHEN.DTcoup %cm_DTcoup% == "on"
    Execute_Loadpoint 'results_DIETER' p32_report4RM;
    Execute_Loadpoint 'results_DIETER' p32_reportmk_4RM;
*   Couple capacity factor from DIETER to REMIND
*   sum over gdxfile set removes this extra index that comes from gdxmerge algorithm
*   optional: averaging capfac over 2 iterations

***CG:noCF averaging
*if( (ord(iteration) le sm32_DTiter) ,
    pm_cf(t,regi,te)$(tDT32(t) AND COALte32(te) AND regDTCoup(regi))
    			= sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"coal","capfac"));
    pm_cf(t,regi,te)$(tDT32(t) AND NonPeakGASte32(te) AND regDTCoup(regi))
    			= sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"CCGT","capfac"));
    pm_cf(t,regi,te)$(tDT32(t) AND BIOte32(te) AND regDTCoup(regi))
    			= sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"bio","capfac"));
    pm_cf(t,regi,"ngt")$(tDT32(t) AND regDTCoup(regi))
    			= sum(gdxfile32, p32_report4RM(gdxfile32,t,regi,"OCGT_eff","capfac"));
    pm_cf(t,regi,te)$(tDT32(t) AND NUCte32(te) AND regDTCoup(regi))
    			= sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"nuc","capfac"));
$IFTHEN.elh2_coup %cm_DT_elh2_coup% == "on"
    pm_cf(t,regi,"elh2")$(tDT32(t) AND regDTCoup(regi))
    			= sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"elh2","capfac"));
$ENDIF.elh2_coup
*);

$IFTHEN.cf_avg %cm_DTcf_avg% == "on"
*** CG: CF averaging, only after DT is coupled for one iteration (to avoid pm_cf being distorted by default high values)
if( (ord(iteration) gt sm32_DTiter),
pm_cf(t,regi,te)$(tDT32(t) AND regDTCoup(regi) AND COALte32(te) )
			= 0.5 * ( p32_cf_last_iter(t,regi,te)$(COALte32(te))
      + sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"coal","capfac")) );
pm_cf(t,regi,te)$(tDT32(t) AND regDTCoup(regi) AND NonPeakGASte32(te) )
			= 0.5 * ( p32_cf_last_iter(t,regi,te)$(NonPeakGASte32(te))
      + sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"CCGT","capfac")) );
pm_cf(t,regi,te)$(tDT32(t) AND regDTCoup(regi) AND BIOte32(te) )
			= 0.5 * ( p32_cf_last_iter(t,regi,te)$(BIOte32(te))
			+ sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"bio","capfac")) );
pm_cf(t,regi,"ngt")$(tDT32(t) AND regDTCoup(regi))
			= 0.5 * ( p32_cf_last_iter(t,regi,"ngt")
			+ sum(gdxfile32, p32_report4RM(gdxfile32,t,regi,"OCGT_eff","capfac")) );
pm_cf(t,regi,te)$(tDT32(t) AND regDTCoup(regi) AND NUCte32(te) )
			= 0.5 * ( p32_cf_last_iter(t,regi,te)$(NUCte32(te))
			+ sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"nuc","capfac")) );
$IFTHEN.elh2_coup %cm_DT_elh2_coup% == "on"
pm_cf(t,regi,"elh2")$(tDT32(t) AND regDTCoup(regi))
      = 0.5 * ( p32_cf_last_iter(t,regi,"elh2")
			+ sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"elh2","capfac")) );
$ENDIF.elh2_coup
);
$ENDIF.cf_avg

*** pass peak demand from DIETER to REMIND as a relative fraction of the total demand
    p32_peakDemand_relFac(t,regi)$(tDT32(t) AND regDTCoup(regi))
		      = sum(gdxfile32, p32_report4RM(gdxfile32,t,regi,"all_te","ResPeakDem_relFac"));

*** dividing each DIETER tech into REMIND tech, using the last iteration REMIND share within DIETER tech category to scale down the generation share
    p32_tech_category_genshare(t,regi,te)$(tDT32(t) AND regDTCoup(regi) AND BIOte32(te) )
		      = p32_shSeElDisp(t,regi,te)$(BIOte32(te))/(sum(te2$(BIOte32(te2)),p32_shSeElDisp(t,regi,te2)) + sm_eps);
		p32_tech_category_genshare(t,regi,te)$(tDT32(t) AND regDTCoup(regi) AND NonPeakGASte32(te) )
		      = p32_shSeElDisp(t,regi,te)$(NonPeakGASte32(te))/(sum(te2$(NonPeakGASte32(te2)),p32_shSeElDisp(t,regi,te2)) + sm_eps);
		p32_tech_category_genshare(t,regi,te)$(tDT32(t) AND NUCte32(te) AND regDTCoup(regi))
		      = p32_shSeElDisp(t,regi,te)$(NUCte32(te))/(sum(te2$(NUCte32(te2)),p32_shSeElDisp(t,regi,te2)) + sm_eps);
		p32_tech_category_genshare(t,regi,te)$(tDT32(t) AND COALte32(te) AND regDTCoup(regi))
		      = p32_shSeElDisp(t,regi,te)$(COALte32(te))/(sum(te2$(COALte32(te2)),p32_shSeElDisp(t,regi,te2)) + sm_eps);

*** CG: writing DIETER generation shares to parameters
    p32_DIETER_shSeEl(t,regi,"spv")$(tDT32(t) AND regDTCoup(regi))
					= sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"Solar","gen_share"));
    p32_DIETER_shSeEl(t,regi,"wind")$(tDT32(t) AND regDTCoup(regi))
					= sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"Wind_on","gen_share"));
$IFTHEN.WindOff %cm_wind_offshore% == "1"
      p32_DIETER_shSeEl(t,regi,"windoff")$(tDT32(t) AND regDTCoup(regi))
  					= sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"Wind_off","gen_share"));
$ENDIF.WindOff

    p32_DIETER_shSeEl(t,regi,"ngt")$(tDT32(t) AND regDTCoup(regi))
					= sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"OCGT_eff","gen_share"));
    p32_DIETER_shSeEl(t,regi,"hydro")$(tDT32(t) AND regDTCoup(regi))
					= sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"ror","gen_share"));

*** CG: for those DIETER technologies which maps to several REMIND technologies: downscaling generation shares in REMIND
    p32_DIETER_shSeEl(t,regi,te)$(tDT32(t) AND regDTCoup(regi)AND BIOte32(te) )
					= sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"bio","gen_share"))
							*	p32_tech_category_genshare(t,regi,te) ;
    p32_DIETER_shSeEl(t,regi,te)$(tDT32(t) AND regDTCoup(regi) AND NonPeakGASte32(te))
					= sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"CCGT","gen_share"))
						 	* p32_tech_category_genshare(t,regi,te) ;
    p32_DIETER_shSeEl(t,regi,te)$(tDT32(t) AND regDTCoup(regi) AND NUCte32(te))
					= sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"nuc","gen_share"))
					  	* p32_tech_category_genshare(t,regi,te) ;
    p32_DIETER_shSeEl(t,regi,te)$(tDT32(t) AND regDTCoup(regi) AND COALte32(te))
					= sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"coal","gen_share"))
				    	* p32_tech_category_genshare(t,regi,te) ;

*** supply side tech market value
    p32_DIETER_MV(t,regi,te)$(tDT32(t) AND BIOte32(te) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"bio","market_value"));
    p32_DIETER_MV(t,regi,te)$(tDT32(t) AND NonPeakGASte32(te) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"CCGT","market_value"));
    p32_DIETER_MV(t,regi,"ngt")$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"OCGT_eff","market_value"));
    p32_DIETER_MV(t,regi,te)$(tDT32(t) AND NUCte32(te) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"nuc","market_value"));
    p32_DIETER_MV(t,regi,te)$(tDT32(t) AND COALte32(te) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"coal","market_value"));
    p32_DIETER_MV(t,regi,"spv")$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"Solar","market_value"));
    p32_DIETER_MV(t,regi,"hydro")$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"ror","market_value"));
    p32_DIETER_MV(t,regi,"wind")$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"Wind_on","market_value"));
$IFTHEN.WindOff %cm_wind_offshore% == "1"
    p32_DIETER_MV(t,regi,"windoff")$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"Wind_off","market_value"));
$ENDIF.WindOff

*** supply side tech value factor
    p32_DIETER_VF(t,regi,te)$(tDT32(t) AND BIOte32(te) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"bio","value_factor"));
    p32_DIETER_VF(t,regi,te)$(tDT32(t) AND NonPeakGASte32(te) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"CCGT","value_factor"));
    p32_DIETER_VF(t,regi,"ngt")$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"OCGT_eff","value_factor"));
    p32_DIETER_VF(t,regi,te)$(tDT32(t) AND NUCte32(te) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"nuc","value_factor"));
    p32_DIETER_VF(t,regi,te)$(tDT32(t) AND COALte32(te) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"coal","value_factor"));
    p32_DIETER_VF(t,regi,"spv")$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"Solar","value_factor"));
    p32_DIETER_VF(t,regi,"hydro")$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"ror","value_factor"));
    p32_DIETER_VF(t,regi,"wind")$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"Wind_on","value_factor"));

$IFTHEN.WindOff %cm_wind_offshore% == "1"
    p32_DIETER_VF(t,regi,"windoff")$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"Wind_off","value_factor"));
$ENDIF.WindOff

$IFTHEN.elh2_coup %cm_DT_elh2_coup% == "on"
*   flexible demand side tech market value (electricity price that the flex tech "sees")
    p32_DIETER_MP(t,regi,"elh2")$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"elh2","market_price"));
    p32_DIETER_MP(t,regi,"tdels")$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"el","market_price"));
    p32_DIETER_MP(t,regi,"tdelt")$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"el","market_price"));

    p32_DIETER_VF(t,regi,"elh2")$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"elh2","value_factor"));
    p32_DIETER_VF(t,regi,"tdels")$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"el","value_factor"));
    p32_DIETER_VF(t,regi,"tdelt")$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"el","value_factor"));

    p32_shSeElDemDIETER(t,regi,"elh2")$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"elh2","dem_share"));
*    downscale demand share from one type ("electricity") in DIETER to two types ("stationary electricity" and "transport electricity") in REMIND
    p32_shSeElDemDIETER(t,regi,"tdels")$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"el","dem_share")) *
    vm_demSe.l(t,regi,"seel","feels","tdels")/(vm_demSe.l(t,regi,"seel","feels","tdels") + vm_demSe.l(t,regi,"seel","feelt","tdelt"));
    p32_shSeElDemDIETER(t,regi,"tdelt")$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"el","dem_share")) *
    vm_demSe.l(t,regi,"seel","feelt","tdelt")/(vm_demSe.l(t,regi,"seel","feels","tdels") + vm_demSe.l(t,regi,"seel","feelt","tdelt"));

$ENDIF.elh2_coup

*** set prefactor to value factor
    p32_prefac(t,regi,te) = p32_DIETER_VF(t,regi,te);
*** CG: if value factor if lower than 1, say for solar, take the inverse, since we feed VF into prefactor of markup, and solar being a lot lower than elec price
**  it means markup need to be adjusted more aggressively (like OCGT)
    p32_prefac(t,regi,te)$((p32_DIETER_VF(t,regi,te) lt 1) AND (p32_DIETER_VF(t,regi,te) ne 0)) = 1 / p32_DIETER_VF(t,regi,te);

*** DIETER electricity price (when DIETER price_shave is off, then elec_price = elec_price_wscar)
    p32_DIETER_elecprice(t,regi)$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"all_te","elec_price"));
*** only for reporting
    p32_DIETER_elecprice_wscar(t,regi)$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_reportmk_4RM(gdxfile32,t,regi,"all_te","elec_price_wscar"));

*** CG: storage related coupling parameters
* ** no curt_ratio averaging
p32_DIETERCurtRatio(t,regi,"spv")$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"Solar","curt_ratio"));
p32_DIETERCurtRatio(t,regi,"wind")$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"Wind_on","curt_ratio"));
$IFTHEN.WindOff %cm_wind_offshore% == "1"
p32_DIETERCurtRatio(t,regi,"windoff")$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"Wind_off","curt_ratio"));
$ENDIF.WindOff

$IFTHEN.curt_avg %cm_DTcurt_avg% == "on"
* with curt_ratio averaging
p32_DIETERCurtRatioCurrIter(t,regi,"spv")$(tDT32(t) AND regDTCoup(regi)) = p32_DIETERCurtRatio(t,regi,"spv");
p32_DIETERCurtRatioCurrIter(t,regi,"wind")$(tDT32(t) AND regDTCoup(regi)) = p32_DIETERCurtRatio(t,regi,"wind");
$IFTHEN.WindOff %cm_wind_offshore% == "1"
p32_DIETERCurtRatioCurrIter(t,regi,"windoff")$(tDT32(t) AND regDTCoup(regi)) = p32_DIETERCurtRatio(t,regi,"windoff");
$ENDIF.WindOff

p32_DIETERCurtRatio(t,regi,"spv")$(tDT32(t) AND regDTCoup(regi)) =
      0.5 * (p32_DIETERCurtRatioLaIter(t,regi,"spv") + p32_DIETERCurtRatio(t,regi,"spv"));
p32_DIETERCurtRatio(t,regi,"wind")$(tDT32(t) AND regDTCoup(regi)) =
      0.5 * (p32_DIETERCurtRatioLaIter(t,regi,"wind") + p32_DIETERCurtRatio(t,regi,"wind"));
$IFTHEN.WindOff %cm_wind_offshore% == "1"
p32_DIETERCurtRatio(t,regi,"windoff")$(tDT32(t) AND regDTCoup(regi)) =
      0.5 * (p32_DIETERCurtRatioLaIter(t,regi,"windoff") + p32_DIETERCurtRatio(t,regi,"windoff"));
$ENDIF.WindOff
* ror capfac is harmonized by setting capfac in DIETER to be the same as that in REMIND
$ENDIF.curt_avg


$ENDIF.DTcoup

*** EOF ./modules/32_power/DTcoup/presolve.gms
