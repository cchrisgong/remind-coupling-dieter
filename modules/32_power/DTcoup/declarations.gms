*** |  (C) 2006-2019 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de

parameters
    p32_grid_factor(all_regi)						  "multiplicative factor that scales total grid requirements down in comparatively small or homogeneous regions like Japan, Europe or India"
    p32_gridexp(all_regi,all_te)					"exponent that determines how grid requirement per kW increases with market share of wind and solar. 1 means specific marginal costs increase linearly"
    p32_storexp(all_regi,all_te)					"exponent that determines how curtailment and storage requirements per kW increase with market share of wind and solar. 1 means specific marginal costs increase linearly"
    p32_shCHP(all_regi,char)            	"upper boundary of chp electricity generation"
    p32_factorStorage(all_regi,all_te)    "multiplicative factor that scales total curtailment and storage requirements up or down in different regions for different technologies (e.g. down for PV in regions where high solar radiation coincides with high electricity demand)"
    f32_storageCap(char, all_te)          "multiplicative factor between dummy seel<-->h2 technologies and storXXX technologies"
    p32_storageCap(all_te,char)           "multiplicative factor between dummy seel<-->h2 technologies and storXXX technologies"
*   p32_capStor_DIET(tall,all_regi)       "storage cap from DIETER"
    p32_minVF_spv                         "value factor of solar at 100% VRE shares"
    p32_seelTotDem(ttot,all_regi,all_enty)    "total secondary electricity demand (including curtailment)"
    p32_seelUsableDem(ttot,all_regi,all_enty) "total usable secondary electricity demand"
    p32_shSeEl(ttot,all_regi,all_te)      "generation share of the last iteration"
    p32_deltaCap(ttot,all_regi,all_te,rlf)"capacity of the last iteration"
    p32_marketValue(ttot,all_regi,all_te) "market value seen by REMIND"
    p32_valueFactor(ttot,all_regi,all_te) "value factor seen by REMIND"
    p32_peakDemand_relFac(ttot,all_regi)  "annual peak demand as a relative ratio of total annual power demand from DIETER"
    p32_budget(ttot,all_regi)             "budget from last iteration"

*   for diagnostics
    p32_nonSEPE2SE(ttot,all_regi,all_enty)  "all non SE2SE PE2SE terms"
    p32_coupledProd(ttot,all_regi,all_enty) "coupled production"
    p32_prod4dtFE(ttot,all_regi,all_enty)   "power used in transporting and distributing FE"
    p32_prod4CCS(ttot,all_regi,all_enty)    "power consumption for CCS"
    p32_totProd(ttot,all_regi,all_enty)     "total seel production (both coupled and uncoupled production)"
    p32_seelCurt(ttot,all_regi)             "total curtailment"

$IFTHEN.DTcoup %cm_DTcoup% == "on"
    p32_report4RM(gdxfile32,ttot,all_regi,DIETERte32,DIETERvarname32)    "load report from DIETER"
    p32_reportmk_4RM(gdxfile32,ttot,all_regi,DIETERte32,DIETERvarname32) "load markup report from DIETER"

    p32_peakDemand_relFac(ttot,all_regi)         "annual peak demand as a relative ratio of total annual power demand from DIETER"
    p32_DIETER_VF(ttot,all_regi,all_te)                   "multiplicative value factor from DIETER that indicates the multiplicative price mark up of technology"
    p32_DIETER_MV(ttot,all_regi,all_te)                   "absolute markup from DIETER"
    p32_DIETER_elecprice(ttot,all_regi)                   "elec price in DIETER"
    p32_DIETER_shSeEl(ttot,all_regi,all_te)      "generation share of technology from DIETER"
    p32_reqCap(ttot,all_regi)                    "required total dispatchable capacity to meet peak demand (from last iteration)"
    p32_capDecayStart(ttot,all_regi)             "where capacity subsidy cost function starts to decay, it is a portion of the total required dispatchable capacities"
    p32_capDecayEnd(ttot,all_regi)               "where capacity subsidy cost function decaying ends, it is a portion of the total required dispatchable capacities"
    p32_tech_category_genshare(ttot,all_regi,all_te)                 "generation share of sub-technology within a DIETER tech category from last REMIND iteration"

*** CG: storage related parameters
    p32_DIETER_curtailmentratio(ttot,all_regi,all_te)               "ratio of curtailed energy to usable energy for VRE from DIETER"
    p32_cf_last_iter(ttot,all_regi,all_te)                          "pm_cf of last iteration, used for averaging"

$ENDIF.DTcoup

p32_fuelprice_lastiter(ttot,all_regi,all_enty)                      "fuel cost of the last iteration"
p32_fuelprice_lastx2iter(ttot,all_regi,all_enty)                    "fuel cost of the second to last iteration"
p32_fuelprice_curriter(ttot,all_regi,all_enty)                      "fuel cost of the current iteration"
p32_fuelprice_avgiter(ttot,all_regi,all_enty)                       "fuel cost over the three iterations averaged through a low pass filter"
;


scalars
s32_storlink                              "how strong is the influence of two similar renewable energies on each other's storage requirements (1= complete, 4= rather small)" /3/
sm32_tmp                                  "iteration.val"
sm32_discount_factor                      "discount factor for investment"

positive variables
    v32_shStor(ttot,all_regi,all_te)      "share of seel production from renewables that needs to be stored, range 0..1 [0,1]"
    v32_storloss(ttot,all_regi,all_te)    "total energy loss from storage for a given technology [TWa]"
*   v32_storloss_DT(ttot,all_regi,all_te)    "curtailment given by DIETER [TWa]"
    v32_shSeEl(ttot,all_regi,all_te)			"new share of electricity production in % [%]"
*   v32_seelDem(ttot,all_regi,all_enty)   "total secondary electricity demand (excluding curtailment)"
    v32_capPriceExponent(ttot,all_regi)   "exponent for the cost of additional dispatchable capacity"
    v32_expSlack(ttot,all_regi)           "slack variable to make sure the exponent is bounded"
;

equations
    q32_balSe(ttot,all_regi,all_enty)			        "balance equation for electricity secondary energy for non-DIETER coupled regions"
    q32_usableSe(ttot,all_regi,all_enty)			    "calculate usable se before se2se and MP/XP (without storage) for non-DIETER coupled regions"
    q32_usableSeTe(ttot,all_regi,entySe,all_te)   "calculate usable se produced by one technology (vm_usableSeTe) for non-DIETER coupled regions"
    q32_limitCapTeStor(ttot,all_regi,teStor)		  "calculate the storage capacity required by vm_storloss"
	  q32_h2turbVREcapfromTestor(tall,all_regi)     "calculate capacities of dummy seel<--h2 technology from storXXX technologies"
    q32_elh2VREcapfromTestor(tall,all_regi)       "calculate capacities of dummy seel-->h2 technology from storXXX technologies"
    q32_limitCapTeChp(ttot,all_regi)              "capacitiy constraint for chp electricity generation"
    q32_limitCapTeGrid(ttot,all_regi)          		"calculate the additional grid capacity required by VRE"
    q32_shSeEl(ttot,all_regi,all_te)         		  "calculate share of electricity production of a technology (v32_shSeEl)"
    q32_shStor(ttot,all_regi,all_te)              "equation to calculate v32_shStor for non-DIETER coupled regions"
    q32_storloss(ttot,all_regi,all_te)            "equation to calculate vm_storloss for non-DIETER coupled regions"
*** disabling flexibility constraint q32_operatingReserve in coupled mode
*   q32_operatingReserve(ttot,all_regi)  			    "operating reserve for necessary flexibility"

    q32_limitSolarWind(ttot,all_regi)           	"limits on fluctuating renewables, only turned on for special EMF27 scenarios"
$IFTHEN.DTcoup %cm_DTcoup% == "on"
$IFTHEN.hardcap %cm_softcap% == "off"
    q32_peakDemand_DT(ttot,all_regi,all_enty)     "limit yearly sum of dispatchable capacities by the peak demand given by DIETER"
$ENDIF.hardcap
    q32_mkup(ttot,all_regi,all_te)                "calculate markup or markdown of generation technology value"

$IFTHEN.softcap %cm_softcap% == "on"
    q32_reqCap(ttot,all_regi,all_enty)            "required total dispatchable capacities"
    q32_priceCap(ttot,all_regi)                   "calculates subsidy for disptachable capacity / capacity shadow price"

$ENDIF.softcap

$ENDIF.DTcoup

*   q32_flexAdj(tall,all_regi,all_te)             "calculate flexibility used in flexibility tax for technologies with electricity input"
;
