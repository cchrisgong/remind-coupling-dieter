*** |  (C) 2006-2019 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de
*------------------------------------------------------------------------------------
***                        IntC specific data input
*------------------------------------------------------------------------------------

***parameter p32_shCHP(all_regi,char) - upper boundary of chp electricity generation
parameter f32_shCHP(all_regi,char)  "upper boundary of chp electricity generation"
/
$ondelim
$include "./modules/32_power/IntC/input/f32_shCHP.cs4r"
$offdelim
/
;
p32_shCHP(all_regi,char) = f32_shCHP(all_regi,char);


***parameter p32_grid_factor(all_regi) - multiplicative factor that scales total grid requirements down in comparatively small or homogeneous regions like Japan, Europe or India
parameter p32_grid_factor(all_regi)  "multiplicative factor that scales total grid requirements down in comparatively small or homogeneous regions like Japan, Europe or India"
/
$ondelim
$include "./modules/32_power/IntC/input/p32_grid_factor.cs4r"
$offdelim
/
;

***parameter p32_factorStorage(all_regi,all_te) - multiplicative factor that scales total curtailment and storage requirements up or down in different regions for different technologies (e.g. down for PV in regions where high solar radiation coincides with high electricity demand)
parameter f32_factorStorage(all_regi,all_te)                  "multiplicative factor that scales total curtailment and storage requirements up or down in different regions for different technologies (e.g. down for PV in regions where high solar radiation coincides with high electricity demand)"
/
$ondelim
$include "./modules/32_power/IntC/input/f32_factorStorage.cs4r"
$offdelim
/
;
p32_factorStorage(all_regi,all_te) = f32_factorStorage(all_regi,all_te);

***INNOPATHS
$if not "%cm_INNOPATHS_storageFactor%" == "off" p32_factorStorage(all_regi,all_te)=%cm_INNOPATHS_storageFactor%*p32_factorStorage(all_regi,all_te);

***parameter p32_storexp(all_regi,all_te) - exponent that determines how curtailment and storage requirements per kW increase with market share of wind and solar. 1 means specific marginal costs increase linearly
p32_storexp(regi,"spv")     = 1;
p32_storexp(regi,"csp")     = 1;
p32_storexp(regi,"wind")    = 1;

***parameter p32_gridexp(all_regi,all_te) - exponent that determines how grid requirement per kW increases with market share of wind and solar. 1 means specific marginal costs increase linearly
p32_gridexp(regi,"spv")     = 1;
p32_gridexp(regi,"csp")     = 1;
p32_gridexp(regi,"wind")    = 1;


table f32_storageCap(char, all_te)  "multiplicative factor between dummy seel<-->h2 technologies and storXXX technologies"
$include "./modules/32_power/IntC/input/f32_storageCap.prn"
;

p32_storageCap(te,char) = f32_storageCap(char,te);
display p32_storageCap;


$IFTHEN.DTcoup %cm_DTcoup% == "on"

p32_minVF_spv = 0.1;

p32_peakDemand_relFac(t,regi) = 0;
p32_DIETER_VF(t,regi,te) = 0;
p32_DIETER_MV(t,regi,te) = 0;
p32_DIETER_elecprice(t,regi) = 0;
p32_seelUsableDem(t,regi,enty) = 0;
p32_shSeEl(t,regi,te) = 0;
p32_DIETER_shSeEl(t,regi,te) = 0;
p32_deltaCap(t,regi,te,rlf) = 0;
p32_marketValue(t,regi,te) = 0;
p32_valueFactor(t,regi,te) = 1;
p32_budget(t,regi) = 0;
p32_DIETER_curtailmentratio(t,regi,te) = 0;

Execute_Loadpoint 'input_DIETER' p32_report4RM;
p32_DIETER_curtailmentratio(t,regi,"spv")$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"Solar","curt_share")$(tDT32(t) AND regDTCoup(regi)));
p32_DIETER_curtailmentratio(t,regi,"wind")$(tDT32(t) AND regDTCoup(regi)) = sum(gdxfile32,p32_report4RM(gdxfile32,t,regi,"Wind_on","curt_share")$(tDT32(t) AND regDTCoup(regi)));

Execute_Loadpoint 'input' q_balPe.m = q_balPe.m;
p32_fuelprice_lastiter(t,regi,entyPe) = q_balPe.m(t,regi,entyPe);
p32_fuelprice_lastx2iter(t,regi,entyPe) = q_balPe.m(t,regi,entyPe);
$ENDIF.DTcoup

*** initialize p32_PriceDurSlope parameter
p32_PriceDurSlope(regi,"elh2") = cm_PriceDurSlope_elh2;
