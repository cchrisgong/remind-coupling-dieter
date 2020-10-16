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
$IFTHEN.DTcoup %cm_DTcoup% == "on"
    report4RM(gdxfile_32,tall,all_regi,DIETER_te_32,DIETERvarname_32) "load report from DIETER"
    p32_peakDemand_relFac(tall)           "peak demand from DIETER of a year"
    p32_peakDemand(tall)                  "peak demand from DIETER of a year"
*    p32_flex_multmk(tall,all_te) "fractional parameter from DIETER that indicates the multiplicative price mark up of a flexible demand side technology"
$ENDIF.DTcoup
;

scalars
s32_storlink                              "how strong is the influence of two similar renewable energies on each other's storage requirements (1= complete, 4= rather small)" /3/
s32_iteration_ge_5                        "to control that only after iter 5 is capacity constraint from DIETER implemented"
;

positive variables
    v32_shStor(ttot,all_regi,all_te)      "share of seel production from renewables that needs to be stored, range 0..1 [0,1]"
    v32_storloss(ttot,all_regi,all_te)    "total energy loss from storage for a given technology [TWa]"
    v32_shSeEl(ttot,all_regi,all_te)			"new share of electricity production in % [%]"
    v32_seelDem(tall,all_regi,all_enty)   "total secondary electricity demand"
;

equations
    q32_balSe(ttot,all_regi,all_enty)			"balance equation for electricity secondary energy"
    q32_seelDem(ttot, all_regi, all_enty)         "total secondary electricity demand"
    q32_usableSe(ttot,all_regi,all_enty)			    "calculate usable se before se2se and MP/XP (without storage)"
    q32_usableSeTe(ttot,all_regi,entySe,all_te)   "calculate usable se produced by one technology (vm_usableSeTe)"
*   q32_limitCapTeStor(ttot,all_regi,teStor)		  "calculate the storage capacity required by vm_storloss"
    q32_limitCapTeChp(ttot,all_regi)              "capacitiy constraint for chp electricity generation"
    q32_limitCapTeGrid(ttot,all_regi)          		"calculate the additional grid capacity required by VRE"
    q32_shSeEl(ttot,all_regi,all_te)         		  "calculate share of electricity production of a technology (v32_shSeEl)"
    q32_shStor(ttot,all_regi,all_te)              "equation to calculate v32_shStor"
    q32_storloss(ttot,all_regi,all_te)            "equation to calculate vm_storloss"
    q32_operatingReserve(ttot,all_regi)  			    "operating reserve for necessary flexibility"
    q32_limitSolarWind(tall,all_regi)           	"limits on fluctuating renewables, only turned on for special EMF27 scenarios"
$IFTHEN.DTcoup %cm_DTcoup% == "on"
q32_peakDemand_DT(tall, all_enty)              "limit yearly sum of dispatchable capacities by the peak demand given by DIETER"
*q32_flexAdj(tall,all_regi,all_te)               "calculate flexibility used in flexibility tax for technologies with electricity input"
$ENDIF.DTcoup
*	  q32_h2turbVREcapfromTestor(tall,all_regi)     "calculate capacities of dummy seel<--h2 technology from storXXX technologies"
*   q32_elh2VREcapfromTestor(tall,all_regi)       "calculate capacities of dummy seel-->h2 technology from storXXX technologies"
;
