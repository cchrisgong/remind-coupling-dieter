*** |  (C) 2006-2020 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de

parameters
    p32_grid_factor(all_regi)                           "multiplicative factor that scales total grid requirements down in comparatively small or homogeneous regions like Japan, Europe or India"
    p32_gridexp(all_regi,all_te)                        "exponent that determines how grid requirement per kW increases with market share of wind and solar. 1 means specific marginal costs increase linearly"
    p32_storexp(all_regi,all_te)                        "exponent that determines how curtailment and storage requirements per kW increase with market share of wind and solar. 1 means specific marginal costs increase linearly"
    p32_shCHP(all_regi,char)                            "upper boundary of chp electricity generation"
    p32_factorStorage(all_regi,all_te)                  "multiplicative factor that scales total curtailment and storage requirements up or down in different regions for different technologies (e.g. down for PV in regions where high solar radiation coincides with high electricity demand)"
    f32_storageCap(char, all_te)                        "multiplicative factor between dummy seel<-->h2 technologies and storXXX technologies"
    p32_storageCap(all_te,char)                         "multiplicative factor between dummy seel<-->h2 technologies and storXXX technologies"
    p32_PriceDurSlope(all_regi,all_te)                  "slope of price duration curve used for calculation of electricity price for flexible technologies, determines how fast electricity price declines at lower capacity factors"

*** for diagnostics and reporting (calculation of p32_seelUsableDem)
    p32_seelTotDem(ttot,all_regi,all_enty)              "total secondary electricity demand (including curtailment)"
    p32_seelUsableDem(ttot,all_regi,all_enty)           "total usable secondary electricity demand"
    p32_seelUsableProd(ttot,all_regi,all_enty)          "total usable secondary electricity production (no co-production included, but including not dispatched tech)"
    p32_seh2elh2Dem(ttot,all_regi,all_enty)             "total green H2 demand"
*    p32_seh2elh2DemAvg(ttot,all_regi,all_enty)          "total green H2 demand averaged over 2 iterations"
    p32_seh2elh2DemLaIter(ttot,all_regi,all_enty)       "total green H2 demand from last iteration"
    p32_extrEnergyUsage(ttot,all_regi,all_enty)         "Energy used in extraction"

    p32_shSeEl(ttot,all_regi,all_te)                    "generation share of the last iteration"
    p32_shSeElDisp(ttot,all_regi,all_te)                "generation share (dispatched, no co-production) of the last iteration"
    p32_usableSeDisp(ttot,all_regi,entySe)              "solved value for usable se that are dispatched by DIETER, i.e. excluding co-production"
*    p32_usableSeDispAvg(ttot,all_regi,entySe)       "usable se that are dispatched by DIETER, i.e. excluding co-production averaged over 2 iterations"
    p32_usableSeDispLaIter(ttot,all_regi,entySe)        "last iteration value for usable se that are dispatched by DIETER, i.e. excluding co-production"
    p32_usableSeDispCurrIter(ttot,all_regi,entySe)      "last iteration value for usable se that are dispatched by DIETER, i.e. excluding co-production, current iteration"
    p32_usableSeTeDisp(ttot,all_regi,entySe,all_te)     "last iteration value for calculate usable se produced by one technology that are dispatched by DIETER, i.e. excluding co-production"
    p32_budget(ttot,all_regi)                           "budget from last iteration"
    p32_nonSEPE2SE(ttot,all_regi,all_enty)              "all non SE2SE PE2SE terms"
    p32_coupledProd(ttot,all_regi,all_enty)             "coupled production"
    p32_prod4dtFE(ttot,all_regi,all_enty)               "power used in transporting and distributing FE"
    p32_prod4CCS(ttot,all_regi,all_enty)                "power consumption for CCS"
    p32_totProd(ttot,all_regi,all_enty)                 "total seel production (both coupled and uncoupled production)"
    p32_seelCurt(ttot,all_regi)                         "total curtailment"
    p32_shSeElDem(ttot,all_regi,all_te)	                "share of electricity demand in % [%] in last iter REMIND"
    p32_shSeElDemDIETER(ttot,all_regi,all_te)	          "share of electricity demand in % [%] in last iter DIETER - only as a share of dispatched tech generation"
    p32_realCapfacVRE(ttot,all_regi,all_te)             "post curtailment - real VRE capfac"
    p32_theoCapfacVRE(ttot,all_regi,all_te)             "pre curtailment - theoretical VRE capfac"

$IFTHEN.DTcoup %cm_DTcoup% == "on"
    p32_marketValue(ttot,all_regi,all_te)               "market value seen by REMIND"
    p32_marketPrice(ttot,all_regi,all_te)               "market price seen by REMIND"
    p32_valueFactor(ttot,all_regi,all_te)               "value factor seen by REMIND"

*   p32_capStor_DIET(tall,all_regi)       "storage cap from DIETER"
    p32_minVF_spv                         "value factor of solar at 100% VRE shares"

    p32_report4RM(gdxfile32,ttot,all_regi,DIETERte32,DIETERvarname32)    "load report from DIETER"
    p32_reportmk_4RM(gdxfile32,ttot,all_regi,DIETERte32,DIETERvarname32) "load markup report from DIETER"

    p32_peakDemand_RelFac(ttot,all_regi)         "annual peak demand as a relative ratio of total annual power demand from DIETER"
    p32_DIETER_VF(ttot,all_regi,all_te)          "multiplicative value factor from DIETER that indicates the multiplicative price mark up of technology"
    p32_DIETER_MV(ttot,all_regi,all_te)          "market value for power generating technology rom DIETER"
    p32_DIETER_MP(ttot,all_regi,all_te)          "market price for power consumption technology DIETER"
    p32_DIETER_elecprice(ttot,all_regi)          "elec price in DIETER"
    p32_DIETER_shSeEl(ttot,all_regi,all_te)      "electricity generation share of technology from DIETER"

    p32_reqCap(ttot,all_regi)                              "required total dispatchable capacity to meet peak demand (from last iteration)"
    p32_capDecayStart(ttot,all_regi)                       "where capacity subsidy cost function starts to decay, it is a portion of the total required dispatchable capacities"
    p32_capDecayEnd(ttot,all_regi)                         "where capacity subsidy cost function decaying ends, it is a portion of the total required dispatchable capacities"
    p32_tech_category_genshare(ttot,all_regi,all_te)       "generation share of sub-technology within a DIETER tech category from last REMIND iteration"

    p32_cf_last_iter(ttot,all_regi,all_te)                 "pm_cf of current iteration"
$ENDIF.DTcoup

    p32_DIETERCurtRatio(ttot,all_regi,all_te)              "ratio of curtailed energy to usable energy for VRE from DIETER"
    p32_DIETERCurtRatioLaIter(ttot,all_regi,all_te)        "ratio of curtailed energy to usable energy for VRE from DIETER from last iteration DIETER"
    p32_DIETERCurtRatioCurrIter(ttot,all_regi,all_te)        "ratio of curtailed energy to usable energy for VRE from DIETER from current iteration DIETER"
    p32_r4DT(ttot,all_regi)                                            "interest rate to be passed on to DIETER"
    p32_fuelprice_lastiter(ttot,all_regi,all_enty)                      "fuel cost of the last iteration"
    p32_fuelprice_lastx2iter(ttot,all_regi,all_enty)                    "fuel cost of the second to last iteration"
    p32_fuelprice_curriter(ttot,all_regi,all_enty)                      "fuel cost of the current iteration"
    p32_fuelprice_avgiter(ttot,all_regi,all_enty)                       "fuel cost over the three iterations averaged through a low pass filter"

    p32_test1(ttot,all_regi,all_te)
    p32_test2(ttot,all_regi)
;

scalars
    s32_storlink                              "how strong is the influence of two similar renewable energies on each other's storage requirements (1= complete, 4= rather small)" /3/
    sm32_iter                                 "iteration.val"
    sm32_DTiter                               "iteration when DIETER starts to be coupled"
    s32_H2switch                              "a binary switch for flexible H2 coupling, to be exported to DIETER"
;

positive variables
    v32_shStor(ttot,all_regi,all_te)         	   "share of seel production from renewables that needs to be stored, range 0..1 [0,1]"
    v32_storloss(ttot,all_regi,all_te)         	   "total energy loss from storage for a given technology [TWa]"
    v32_shSeEl(ttot,all_regi,all_te)		   "new share of electricity production in % [%]"
    v32_shSeElDisp(ttot,all_regi,all_te)		   "new share of electricity production in % [%] (exluding co-production)"
    v32_testdemSeShare(ttot,all_regi,all_te)       "test variable for tech share of SE electricity demand"
    v32_shSeElDem(ttot,all_regi,all_te)		   "new share of electricity demand in % [%]"
    v32_seelUsableDem(ttot,all_regi,all_enty)      "demand of usable seel in current iteration"
    v32_expSlack(ttot,all_regi)                    "slack variable to make sure the exponent is bounded"
    v32_capDecayEnd(ttot,all_regi)                 "the end decay point of the logit function for required dispatchable capacity as a function of electricity demand"
    v32_flexPriceShare(tall,all_regi,all_te)       "share of average electricity price that flexible technologies see [share: 0...1]"
    v32_flexPriceShareMin(tall,all_regi,all_te)    "possible minimum of share of average electricity price that flexible technologies see [share: 0...1]"
;

equations
    q32_balSe(ttot,all_regi,all_enty)		   "balance equation for electricity secondary energy"
    q32_usableSe(ttot,all_regi,all_enty)	   "calculate usable se before se2se and MP/XP (without storage)"
    q32_usableSeTe(ttot,all_regi,entySe,all_te)    "calculate usable se produced by one technology (vm_usableSeTe)"
    q32_usableSeDisp(ttot,all_regi,all_enty)  "calculate usable se before se2se and MP/XP (without storage) that are dispatched by DIETER, i.e. excluding co-production"
    q32_usableSeTeDisp(ttot,all_regi,entySe,all_te)    "calculate usable se produced by one technology (v32_usableSeTeDisp) that are dispatched by DIETER, i.e. excluding co-production"

    q32_seelUsableDem(ttot,all_regi,all_enty)      "calculate total usable seel for demand"
    q32_limitCapTeStor(ttot,all_regi,teStor)	   "calculate the storage capacity required by vm_storloss"
    q32_limitCapTeChp(ttot,all_regi)               "capacitiy constraint for chp electricity generation"
    q32_limitCapTeGrid(ttot,all_regi)          	   "calculate the additional grid capacity required by VRE"
    q32_shSeEl(ttot,all_regi,all_te)         	   "calculate share of electricity production of a technology (v32_shSeEl)"
    q32_shSeElDisp(ttot,all_regi,all_te)         	 "calculate share of electricity production of a technology (v32_shSeElDisp)"
    q32_shStor(ttot,all_regi,all_te)               "equation to calculate v32_shStor"
    q32_storloss(ttot,all_regi,all_te)             "equation to calculate vm_storloss"
    q32_h2turbVREcapfromTestor(tall,all_regi)      "calculate capacities of dummy seel<--h2 technology from storXXX technologies"
    q32_elh2VREcapfromTestor(tall,all_regi)        "calculate capacities of dummy seel-->h2 technology from storXXX technologies"

***CG: temporarily disabling operating reserves to eliminate distortion between DIETER and REMIND
$IFTHEN.DTcoup_off %cm_DTcoup% == "off"
*** only enable flexibility constraint q32_operatingReserve in uncoupled mode
    q32_operatingReserve(ttot,all_regi)  	   "operating reserve for necessary flexibility"
$ENDIF.DTcoup_off

    q32_limitSolarWind(tall,all_regi)              "limits on fluctuating renewables, only turned on for special EMF27 scenarios"

    q32_flexAdj(tall,all_regi,all_te)              "calculate flexibility used in flexibility tax for technologies with electricity input"
    q32_flexPriceShareMin(tall,all_regi,all_te)    "calculatae miniumum share of average electricity that flexible technologies can see"
    q32_flexPriceShare(tall,all_regi,all_te)       "calculate share of average electricity price that flexible technologies see"

$IFTHEN.DTcoup %cm_DTcoup% == "on"
$IFTHEN.elh2_coup %cm_elh2_coup% == "on"
    q32_shSeElDem(ttot,all_regi,all_te)            "calculate share of electricity demand of a technology (v32_shSeElDem)"
$ENDIF.elh2_coup
    q32_mkup(ttot,all_regi,all_te)                 "calculate markup or markdown of generation technology value"
    q32_capFac(ttot,all_regi,all_te)        	   "Calculate resulting capacity factor for supply-side technologies"

$IFTHEN.elh2_coup %cm_elh2_coup% == "on"
    q32_capFac_dem(ttot,all_regi,all_te)           "Calculate resulting capacity factor for demand-side technologies"
    q32_flexAdj(tall,all_regi,all_te)              "from DIETER coupling: calculate flexibility used in flexibility tax for technologies with electricity input"
$ENDIF.elh2_coup

$IFTHEN.hardcap %cm_softcap% == "off"
    q32_peakDemandDT(ttot,all_regi,all_enty)      "limit yearly sum of dispatchable capacities by the peak demand given by DIETER"
$ENDIF.hardcap

$IFTHEN.softcap %cm_softcap% == "on"
    q32_reqCap(ttot,all_regi,all_enty)             "sum of total dispatchable capacities"
    q32_capEndStart(ttot,all_regi)                 "required dispatchable capacities"
    q32_priceCap(ttot,all_regi)                    "calculates subsidy for disptachable capacity / capacity shadow price"
$ENDIF.softcap

$ENDIF.DTcoup
;

variables
v32_usableSeDisp(ttot,all_regi,entySe)                    "usable se that are dispatched by DIETER, i.e. excluding co-production"
v32_usableSeTeDisp(ttot,all_regi,entySe,all_te)           "calculate usable se produced by one technology that are dispatched by DIETER, i.e. excluding co-production"
v32_flexPriceShare(tall,all_regi,all_te)           "share of average electricity price that flexible technologies see [share: 0...1]"
v32_flexPriceShareMin(tall,all_regi,all_te)        "possible minimum of share of average electricity price that flexible technologies see [share: 0...1]"
;
