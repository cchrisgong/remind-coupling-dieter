*==========
$ontext
The Dispatch and Investment Evaluation Tool with Endogenous Renewables (DIETER).
Version 1.0.2, January 2016.
Written by Alexander Zerrahn and Wolf-Peter Schill.
This work is licensed under the MIT License (MIT).
For more information on this license, visit http://opensource.org/licenses/mit-license.php.
Whenever you use this code, please refer to http://www.diw.de/DIETER.
This version constitutes a minor revision of the model documented in Zerrahn, A., Schill, W.-P. (2055): A greenfield model to evaluate long-run power storage requirements for high shares of renewables. DIW Discussion Paper 1457. http://www.diw.de/documents/publikationen/73/diw_01.c.498475.de/dp1457.pdf
We are happy to receive feedback under azerrahn@diw.de and wschill@diw.de.
$offtext
*==========


*Fluctuating renewable Scenarios within each loop
*$include Scenario.gms

*For endogenous VRE shares
*parameter spv_share, wind_share_on, wind_share_off;



* Define net energy demand


*==========
*==========           GLOBAL OPTIONS *==========
*==========

* Set star to skip Excel upload and load data from gdx
$setglobal skip_Excel ""

* Set star to write Excel output file
$setglobal write_to_excel ""

* Set star to activate options
$setglobal DSM ""
$setglobal reserves ""

* Set star to select run-of-river options either as a simple exogenous parameter or as an endogenous variable including reserve provision:
* if nothing is selected, ROR capacity will be set to zero
* if parameter option is selected, set appropriate values in fix.gmx
* if variable option is selected, set appropriate bound in data_input excel
$setglobal ror_parameter ""
$setglobal ror_variable ""


* Set star to run test variant with each second hour
$setglobal second_hour ""

*==========

* Definition of strings for report parameters and sanity checks
* (Do not change settings below)
$setglobal em_share ""
$setglobal sec_hour "1"

%second_hour%$ontext
$setglobal sec_hour "8760/2208"
$ontext
$offtext

$if "%ror_parameter%" == "*" $if "%ror_variable%" == "*" $abort Choose appropriate ROR option! ;

*==========
*==========

Sets
*============== remind sets ==================
yr          year for remind power sector             /2055/
yr_before   previous year from remind                /2050/
*t           year from remind to be loaded                
te_remind   remind technonlogy					    /spv, wind, hydro, elh2, ngcc, ngccc, gaschp, ngt, biochp, bioigcc, bioigccc, igcc, igccc, pc, pcc, pco, coalchp, storspv, storwind, tnrs, fnrs/
gas_remind  remind emission gases                    /co2/
COALte(te_remind) "coal to seel tech in REMIND"      /igcc, igccc, pc, pcc, pco, coalchp/
NonPeakGASte(te_remind) "gas to seel tech in REMIND" /ngcc, ngccc, gaschp/
BIOte(te_remind) "biomass to seel tech in REMIND"    /biochp, bioigcc, bioigccc/
NUCte(te_remind) "nuclear to seel tech in REMIND"    /tnrs, fnrs/

pe_remind   remind primary energy                    /pegas, pecoal,pewin,pesol,pebiolc,peur,pehyd/
se_remind   remind secondary energy                  /seel,seh2/
*omf is for fixed O&M cost
char_remind remind character                         /omf, lifetime/
grade 	    remind grade level for technology	    /1*12/
reg         region set                               /DEU/

*============== DIETER sets ==================
year      yearly time data                       /2011, 2012, 2013, 2013_windonsmooth,2018/
all_cdata Data for Conventional Technologies     /eta_con,carbon_content,c_up,c_do,c_fix_con,c_var_con,c_inv_overnight_con,inv_lifetime_con,inv_recovery_con,inv_interest_con,m_con,m_con_e,grad_per_min/
all_rdata Data for Renewable Technologies        /c_cu,c_fix_res,phi_min_res,c_inv_overnight_res,inv_lifetime_res,inv_recovery_res,inv_interest_res,m_res,m_res_e/
ct        Conventional Technologies              /ror, nuc, lig, hc, CCGT, OCGT_eff, OCGT_ineff, bio/
ct_remind Conventional Technologies mapped from REMIND /ror, nuc, coal, CCGT, OCGT_eff, OCGT_ineff, bio/
non_nuc_ct(ct) Conv. Technologies except nuclear /ror, lig, hc, CCGT, OCGT_eff, OCGT_ineff, bio/
res       Renewable technologies                 /Wind_on, Wind_off, Solar/
flexTe    flexible sector coupling techno        /Electrolysis/
sto       Storage technolgies                    /Sto1*Sto7/
all_dsm_cu Data for DSM curt                     /c_m_dsm_cu,c_fix_dsm_cu,c_inv_overnight_dsm_cu,inv_recovery_dsm_cu,inv_interest_dsm_cu,m_dsm_cu,t_dur_dsm_cu,t_off_dsm_cu/
all_dsm_shift Data for DSM shift                 /c_m_dsm_shift,eta_dsm_shift,c_fix_dsm_shift,c_inv_overnight_dsm_shift,inv_recovery_dsm_shift,inv_interest_dsm_shift,m_dsm_shift,t_dur_dsm_shift,t_off_dsm_shift/
all_storage Data for Storagge                    /c_m_sto,eta_sto,c_fix_sto,c_inv_overnight_sto_e,c_inv_overnight_sto_p,inv_lifetime_sto,inv_interest_sto,m_sto_e,m_sto_p,phi_sto_ini,etop_max/
all_reserve Data for Reserves                    /reserves_intercept,phi_reserves_share/
dsm_shift DSM shifting technologies              /DSM_shift1*DSM_shift5/
dsm_curt  Set of load curtailment technologies   /DSM_curt1*DSM_curt3/
reserves  Set of reserve qualities               /PR_up, PR_do, SR_up, SR_do, MR_up, MR_do/
h         hour                                   /h1*h8760/

Alias (h,hh) ;
alias (res,resres) ;
alias(se_remind,se_remind2);
alias (reserves,reservesreserves) ;

*==========

$include dataload.gms
*$stop

Parameter P ratio of fixed power load and flexible power load /1/ ;
Parameter totFixedLoad total fixed load;
Parameter totFlexLoad total flexible load;
Parameter capfac_const(res) constant capacity factor as average of hourly RES potential;
Parameter capfac_ror constant cap. factor of hydro power /0.35/;
Parameter r Investment interest rate /0.06/;
Parameter disc_fac_con(ct) Discount factor for overnight investment;
Parameter disc_fac_res(res) Discount factor for overnight investment;
Parameter preInv_remind_cap(yr, reg, te_remind, grade) Pre investment remind cap for dispatchable te transfer;
Parameter added_remind_cap(yr, reg, te_remind, grade) added cap in REMIND for reporting;
Parameter preInv_remind_prodSe(yr, reg, pe_remind, se_remind, te_remind) Pre investment remind prodSe for VRE gen share transfer;
Parameter RM_postInv_cap_con(yr,reg,ct_remind) Post-investment REMIND capacity for conventional
Parameter RM_postInv_cap_res(yr,reg,res) Post-investment REMIND capacity for renewable
Parameter RM_postInv_prodSe_con(yr,reg,ct_remind) Post-investment REMIND generation for conventional
Parameter RM_postInv_prodSe_res(yr,reg,res) Post-investment REMIND generation for conventional

*==========

Variables
Z                Value objective function
;

Positive Variables
G_L(ct,h)        Generation level in hour h in MWh
G_UP(ct,h)       Generation upshift in hour h in MWh
G_DO(ct,h)       Generation downshift in hour h in MWh

G_RES(res,h)     Generation renewables type res in hour h in MWh
CU(res,h)        Renewables curtailment technology res in hour h in MWh

STO_IN(sto,h)    Storage inflow technology sto hour h in MWh
STO_OUT(sto,h)   Storage outflow technology sto hour h in MWh
STO_L(sto,h)     Storage level technology sto hour h in MWh

P_RES(res)     Renewable technology built in MW
N_CON(ct)        Conventional technology ct built in MW

N_STO_E(sto)     Storage technology built - Energy in MWh
N_STO_P(sto)     Storage loading and discharging capacity built - Capacity in MW

DSM_CU(dsm_curt,h)       DSM: Load curtailment hour h in MWh
DSM_UP(dsm_shift,h)      DSM: Load shifting up hour h technology dsm in MWh
DSM_DO(dsm_shift,h,hh)   DSM: Load shifting down in hour hh to account for upshifts in hour h technology dsm in MWh

DSM_UP_DEMAND(dsm_shift,h)   DSM: Load shifting up active for wholesale demand in hour h of technology dsm in MWh
DSM_DO_DEMAND(dsm_shift,h)   DSM: Load shifting down active for wholesale demand in hour h of technology dsm in MWh

N_DSM_CU(dsm_curt)        DSM: Load curtailment capacity in MW
N_DSM_SHIFT(dsm_shift)    DSM: Load shifting capacity in MWh

RP_CON(reserves,ct,h)                     Reserve provision by conventionals in hour h in MW
RP_RES(reserves,res,h)                    Reserve provision by renewables in hour h in MW
RP_STO_IN(reserves,sto,h)                 Reserve provision by storage in in hour h in MW
RP_STO_OUT(reserves,sto,h)                Reserve provision by storage out in hour h in MW
RP_DSM_CU(reserves,dsm_curt,h)            Reserve provision by DSM load curtailment in hour h in MW
RP_DSM_SHIFT(reserves,dsm_shift,h)        Reserve provision by DSM load shifting in hour h in MW

demand
;

****************
*REMIND disinvestments cap: disinvestments = (early_reti(t) - early_reti(t-1) ) * cap(t) / (1 - early_reti(t)), it doesn't go into DIETER, I am only using DIETER for reporting
earlyRetiCap_reporting("2055", reg, te_remind) = (remind_capEarlyReti("2055", reg, te_remind) - remind_capEarlyReti2("2050", reg, te_remind) ) * remind_cap("2055", reg, te_remind, "1")
                                                                            / (1 - remind_capEarlyReti("2055", reg, te_remind)) ;

****************
*pass on VRE gen share from RM to DT instead of capacities, using the following transformation
*(total generation X gen.share) / (cap.fac. X 8760) = capacity, where (total generation X gen.share) = generation
*capacity = VRE_seProd / sum(h, cap.fac.(h))

capfac_const(res) = sum(h, phi_res_y_reg("2018", "DEU", h, res));

* the prodSe that pre-investment REMIND sees in time step t: prodSe(t) -  pm_ts(t)/2 * prodSE(t) *(vm_deltacap(t)/vm_cap(t))
preInv_remind_prodSe("2055", "DEU", pe_remind, se_remind, te_remind)$(remind_cap("2055", "DEU", te_remind, "1") ne 0 ) = remind_prodSe("2055", "DEU", pe_remind, se_remind, te_remind)
                                                                       - remind_pm_ts("2055") /2 * remind_prodSe("2055", "DEU", pe_remind, se_remind, te_remind)
                                                                       * remind_deltaCap("2055", "DEU", te_remind, "1")
                                                                       /remind_cap("2055", "DEU", te_remind, "1");

**********************************************************************
*Remind post-investment cap ( TW-> MW )
RM_postInv_cap_con(yr,reg,"coal") = sum(te_remind, sum( grade, remind_cap(yr, reg, te_remind, grade)$(COALte(te_remind)) )) * 1e6;
RM_postInv_cap_con(yr,reg,"CCGT") =  sum(te_remind,sum( grade, remind_cap(yr, reg, te_remind, grade)$(NonPeakGASte(te_remind)) )) * 1e6;
RM_postInv_cap_con(yr,reg,"OCGT_eff") = sum(   grade, remind_cap(yr, reg, "ngt", grade) )* 1e6;
RM_postInv_cap_con(yr,reg,"bio") = sum(te_remind,sum( grade, remind_cap(yr, reg, te_remind, grade)$(BIOte(te_remind)) )) * 1e6;
RM_postInv_cap_con(yr,reg,"nuc") = sum(te_remind,sum( grade, remind_cap(yr, reg, te_remind, grade)$(NUCte(te_remind)) )) * 1e6;
RM_postInv_cap_res(yr,reg,"Solar") = sum( grade, remind_cap(yr, reg, "spv", grade)) * 1e6;
RM_postInv_cap_res(yr,reg,"Wind_on") = sum( grade, remind_cap(yr, reg, "wind", grade))* 1e6;
RM_postInv_cap_con(yr,reg,"ror") = sum( grade, remind_cap(yr, reg, "hydro", grade)) * 1e6;

* Remind post-investment gen for reporting ( TWa-> MWh )
RM_postInv_prodSe_con(yr,reg,"coal") = sum(te_remind, remind_prodSe(yr,reg, "pecoal", "seel", te_remind)$(COALte(te_remind)) )* sm_TWa_2_MWh;
RM_postInv_prodSe_con(yr,reg,"CCGT") = sum(te_remind, remind_prodSe(yr,reg, "pegas", "seel", te_remind)$(NonPeakGASte(te_remind)) )* sm_TWa_2_MWh;
RM_postInv_prodSe_con(yr,reg,"OCGT_eff") = remind_prodSe(yr,reg, "pegas", "seel", "ngt") * sm_TWa_2_MWh;
RM_postInv_prodSe_con(yr,reg,"bio") = sum(te_remind, remind_prodSe(yr,reg, "pebiolc", "seel", te_remind)$(BIOte(te_remind)) )* sm_TWa_2_MWh;
RM_postInv_prodSe_con(yr,reg,"nuc") = sum(te_remind, remind_prodSe(yr,reg, "peur", "seel", te_remind)$(NUCte(te_remind)) )* sm_TWa_2_MWh;
RM_postInv_prodSe_res(yr,reg,"Solar") = remind_prodSe(yr, reg, "pesol", "seel", "spv")* sm_TWa_2_MWh;
RM_postInv_prodSe_res(yr,reg,"Wind_on") = remind_prodSe(yr, reg, "pewin", "seel", "wind")* sm_TWa_2_MWh ;
RM_postInv_prodSe_con(yr,reg,"ror") = remind_prodSe(yr, reg, "pehyd", "seel", "hydro")* sm_TWa_2_MWh;
**********************************************************************

P_RES.fx("Wind_off") = 0; 
N_CON.fx("OCGT_ineff") = 0;

**********************************************************************
*********************** VALIDATION MODE ******************************
***   THIS MEANS CAP FROM REMIND IS PASSED AS LOWER BOUNDS ***********
**********************************************************************

P_RES.lo("Solar") = preInv_remind_prodSe("2055", "DEU", "pesol", "seel", "spv") * sm_TWa_2_MWh / capfac_const("Solar") ;
P_RES.lo("Wind_on") = preInv_remind_prodSe("2055", "DEU", "pewin", "seel", "wind") * sm_TWa_2_MWh / capfac_const("Wind_on") ;
N_CON.lo("ror") = preInv_remind_prodSe("2055", "DEU", "pehyd", "seel", "hydro") * sm_TWa_2_MWh / (capfac_ror * 8760) ;

*****************
* the cap that pre-investment REMIND sees in time step t: vm_cap(t) - pm_ts(t)/2 * vm_deltaCap(t) * (1-vm_earlyRetire) 
preInv_remind_cap("2055", "DEU", te_remind, grade) = remind_cap("2055", "DEU", te_remind, grade) - remind_pm_ts("2055") / 2 * remind_deltaCap("2055", "DEU", te_remind, grade) * (1 - remind_capEarlyReti("2055", "DEU", te_remind));
added_remind_cap("2055", "DEU", te_remind, grade) = remind_pm_ts("2055") / 2 * remind_deltaCap("2055", "DEU", te_remind, grade);

* half-half split between lig and hc for DEU
* TW-> MW
N_CON.lo("lig") = sum(te_remind,
                    sum(   grade, preInv_remind_cap("2055", "DEU", te_remind, grade)$(COALte(te_remind))   )
                    ) * 1e6 /2;

N_CON.lo("hc") = sum(te_remind,
                    sum(   grade, preInv_remind_cap("2055", "DEU", te_remind, grade)$(COALte(te_remind))   )
                    ) * 1e6 /2;
                    
N_CON.lo("nuc") = sum(te_remind,
                   sum(   grade, preInv_remind_cap("2055", "DEU", te_remind, grade)$(NUCte(te_remind))   )
                  ) * 1e6;

N_CON.lo("CCGT") = sum(te_remind,
                    sum(   grade, preInv_remind_cap("2055", "DEU", te_remind, grade)$(NonPeakGASte(te_remind))   )
                    ) * 1e6;

N_CON.lo("OCGT_eff") = sum(grade, preInv_remind_cap("2055", "DEU", "ngt", grade)) * 1e6;

      
N_CON.lo("bio") =  sum(te_remind,
                    sum(   grade, preInv_remind_cap("2055", "DEU", te_remind, grade)$(BIOte(te_remind))   )
                    ) * 1e6;
              
**********************************************************************
*********************** END OF VALIDATION MODE ***********************
**********************************************************************

**********************************************************************
*********************** COUPLED MODE ******************************
***   THIS MEANS CAP FROM REMIND IS PASSED AS FIXED BOUNDS ********
**********************************************************************

*P_RES.fx("Solar") = remind_prodSe("2055", "DEU", "pesol", "seel", "spv") * sm_TWa_2_MWh / capfac_const("Solar") ;
*P_RES.fx("Wind_on") = remind_prodSe("2055", "DEU", "pewin", "seel", "wind") * sm_TWa_2_MWh / capfac_const("Wind_on") ;
*N_CON.fx("ror") = remind_prodSe("2055", "DEU", "pehyd", "seel", "hydro") * sm_TWa_2_MWh / (capfac_ror * 8760) ;
*N_CON.fx("CCGT")= RM_postInv_cap_con("2055", "DEU", "CCGT") ;
*N_CON.fx("OCGT_eff")= RM_postInv_cap_con("2055", "DEU", "OCGT_eff") ;
*N_CON.fx("bio")= RM_postInv_cap_con("2055", "DEU", "bio") ;
*N_CON.fx("nuc")= RM_postInv_cap_con("2055", "DEU", "nuc") ;
*N_CON.fx("lig") = RM_postInv_cap_con("2055", "DEU", "coal")/2 ;
*N_CON.fx("hc") = RM_postInv_cap_con("2055", "DEU", "coal")/2 ;

**********************************************************************
*********************** END OF COUPLED MODE ***********************
**********************************************************************


  
*N_STO_P.fx('Sto1') = remind_cap("2055", "DEU", "storspv", "1") * 3 * 1e6+ remind_cap("2055", "DEU", "storwind", "1") * 0.3* 1e6;

N_STO_P.fx(sto) = 0 ;
N_STO_P.fx(sto) = 0 ;
N_STO_E.fx(sto) = 0 ;
STO_IN.fx(sto,h) = 0 ;
STO_OUT.fx(sto,h) = 0 ;
STO_L.fx(sto,h) = 0 ;
RP_STO_IN.fx(reserves,sto,h) = 0 ;
RP_STO_OUT.fx(reserves,sto,h) = 0 ;

*================================================================
*================ read in fuel price from remind ================
*1.2 is the conversion btw 2050$ and 2015$
*1e12 is the conversion btw Trillion$ to $
*remind_budget is kind of like inflation rate
** split fuel cost of pecoal into lignite and hc for rough comparison (not finalized)
con_fuelprice_reg("lig",reg) = -remind_fuelcost("2055",reg,"pecoal") / (-remind_budget("2055",reg)) * 1e12 / sm_TWa_2_MWh * 1.2 - 3.6;
con_fuelprice_reg("hc",reg) = -remind_fuelcost("2055",reg,"pecoal") / (-remind_budget("2055",reg)) * 1e12 / sm_TWa_2_MWh * 1.2 + 1.8;
con_fuelprice_reg("CCGT",reg) = -remind_fuelcost("2055",reg,"pegas") / (-remind_budget("2055",reg)) * 1e12 / sm_TWa_2_MWh * 1.2;
con_fuelprice_reg("OCGT_eff",reg) = con_fuelprice_reg("CCGT",reg);
con_fuelprice_reg("bio",reg) = -remind_fuelcost("2055",reg,"pebiolc") / (-remind_budget("2055",reg)) * 1e12 / sm_TWa_2_MWh * 1.2;
con_fuelprice_reg("nuc",reg) = -remind_fuelcost("2055",reg,"peur") / (-remind_budget("2055",reg)) * 1e12 / sm_TWa_2_MWh * 1.2;
con_fuelprice_reg("ror",reg) = 0;

*eta from remind
** split pecoal into lignite and hc for rough comparison (not finalized)
cdata("eta_con","lig") = remind_eta1("2055","DEU","pc");
cdata("eta_con","hc") = remind_eta1("2055","DEU","pc") * 0.92;
cdata("eta_con","CCGT") = remind_eta1("2055","DEU","ngcc");
cdata("eta_con","OCGT_eff") = remind_eta1("2055","DEU","ngt");
cdata("eta_con","bio") = remind_eta1("2055","DEU","bioigcc");
cdata("eta_con","ror") = remind_eta2("2055","DEU","hydro");
cdata("eta_con","nuc") = remind_eta2("2055","DEU","tnrs");

*carbon content from remind
*dieter value (tCO2/MWh) = remind value (GtC/TWa) * (sm_c_2_co2 * sm_Gt_2_t) / sm_TWa_2_MWh) = remind value * (44/12 * 1e9) / (8760000000) 
cdata("carbon_content","lig") = remind_carboncontent("pecoal","seel","pc","co2") * sm_c_2_co2 * sm_Gt_2_t / sm_TWa_2_MWh;
cdata("carbon_content","hc") = remind_carboncontent("pecoal","seel","pc","co2") * sm_c_2_co2 * sm_Gt_2_t / sm_TWa_2_MWh;
cdata("carbon_content","CCGT") = remind_carboncontent("pegas","seel","ngcc","co2") * sm_c_2_co2 * sm_Gt_2_t / sm_TWa_2_MWh;
cdata("carbon_content","OCGT_eff") = remind_carboncontent("pegas","seel","ngt","co2") * sm_c_2_co2 * sm_Gt_2_t / sm_TWa_2_MWh;

c_m_reg(ct,reg) = con_fuelprice_reg(ct,reg)/cdata("eta_con",ct) + cdata("carbon_content",ct)/cdata("eta_con",ct) * con_CO2price + cdata("c_var_con",ct)   ;
c_m(ct) = c_m_reg(ct,"DEU");

*================================================================
*=======annualize investment cost ==================
*disc.fac = r * (1+r)^lifetime/(-1+(1+r)^lifetime)
disc_fac_con("lig") = r * (1+r) ** remind_lifetime("lifetime", "pc") / (-1+(1+r) ** remind_lifetime("lifetime", "pc")) ;
disc_fac_con("hc") = disc_fac_con("lig");
disc_fac_con("CCGT") = r * (1+r) ** remind_lifetime("lifetime", "ngcc") / (-1+(1+r) ** remind_lifetime("lifetime", "ngcc")) ;
disc_fac_con("OCGT_eff") = r * (1+r) ** remind_lifetime("lifetime", "ngt") / (-1+(1+r) ** remind_lifetime("lifetime", "ngt")) ;
disc_fac_con("bio") = r * (1+r) ** remind_lifetime("lifetime", "biochp") / (-1+(1+r) ** remind_lifetime("lifetime", "biochp")) ;
disc_fac_con("ror") = r * (1+r) ** remind_lifetime("lifetime", "hydro") / (-1+(1+r) ** remind_lifetime("lifetime", "hydro")) ;
disc_fac_con("nuc") = r * (1+r) ** remind_lifetime("lifetime", "tnrs") / (-1+(1+r) ** remind_lifetime("lifetime", "tnrs")) ;

disc_fac_res("Solar") = r * (1+r) ** remind_lifetime("lifetime", "spv") / (-1+(1+r) ** remind_lifetime("lifetime", "spv")) ;
disc_fac_res("Wind_on") = r * (1+r) ** remind_lifetime("lifetime", "wind") / (-1+(1+r) ** remind_lifetime("lifetime", "wind")) ;

*=======read in investment cost from remind ========
*overnight investment cost
*# conversion from tr USD 2050/TW to USD2015/MW
** split pecoal into lignite and hc for rough comparison (not finalized):  set lignite as REMIND-pc cost + 300€/kW
c_i_ovnt("lig") = remind_CapCost("2055", "DEU", "pc") * 1e6 * 1.2 ;
c_i_ovnt("hc") = c_i_ovnt("lig") + 300000 ;
c_i_ovnt("CCGT") = remind_CapCost("2055", "DEU", "ngcc")  * 1e6 * 1.2;
c_i_ovnt("OCGT_eff") = remind_CapCost("2055", "DEU", "ngt") * 1e6 * 1.2;
c_i_ovnt("bio") = remind_CapCost("2055", "DEU", "biochp") * 1e6 * 1.2;
c_i_ovnt("ror") = remind_CapCost("2055", "DEU", "hydro") * 1e6 * 1.2;
c_i_ovnt("nuc") = remind_CapCost("2055", "DEU", "tnrs") * 1e6 * 1.2;
 
c_i_ovnt_res("Solar") = remind_CapCost("2055", "DEU", "spv") * 1e6 * 1.2 ;
c_i_ovnt_res("Wind_on") = remind_CapCost("2055", "DEU", "wind") * 1e6 * 1.2;

*annualized investment cost
c_i(ct) = c_i_ovnt(ct) * disc_fac_con(ct);
c_i_res(res) = c_i_ovnt_res(res) * disc_fac_res(res);

*================================================================
*=======read in fixed OM cost from remind ========
*note that omf is the proportion from overnight investment cost, not annualized
** split pecoal into lignite and hc for rough comparison (not finalized)

cdata("c_fix_con","lig") = remind_OMcost("DEU","omf","pc") * c_i_ovnt("lig");
cdata("c_fix_con","hc") = remind_OMcost("DEU","omf","pc") * c_i_ovnt("hc") ;
cdata("c_fix_con","CCGT") = remind_OMcost("DEU","omf","ngcc") * c_i_ovnt("CCGT");
cdata("c_fix_con","OCGT_eff") = remind_OMcost("DEU","omf","ngt") * c_i_ovnt("OCGT_eff");
cdata("c_fix_con","bio") = remind_OMcost("DEU","omf","bioigcc") * c_i_ovnt("bio");
cdata("c_fix_con","ror") = remind_OMcost("DEU","omf","hydro") * c_i_ovnt("ror");
cdata("c_fix_con","nuc") = remind_OMcost("DEU","omf","tnrs") * c_i_ovnt("nuc");

rdata("c_fix_res","Solar") = remind_OMcost("DEU","omf","spv") * c_i_ovnt_res("Solar");
rdata("c_fix_res","Wind_on") = remind_OMcost("DEU","omf","wind") * c_i_ovnt_res("Wind_on");

*================================================================
*================ scale up demand ===============================
DIETER_OLDtotdem = sum( h , d_y_reg('2018',"DEU",h));
totFixedLoad = remind_totdemand("2055", "DEU", "seel") * sm_TWa_2_MWh;
*totFlexLoad = remind_totdemand("2055", "DEU", "seel") * sm_TWa_2_MWh * (1 - P);
d(h) = d_y_reg('2018',"DEU",h) * totFixedLoad / DIETER_OLDtotdem;

*==========
*scale up wind theoretical capfac to be closer to current generation of wind turbine, 0.32
DIETER_OLDWindOnCapfac = sum(h, phi_res_y_reg('2018',"DEU",h,"Wind_on"))/8760;
phi_res_y_reg('2018',"DEU",h,"Wind_on") = phi_res_y_reg('2018',"DEU",h,"Wind_on") * 0.32 / DIETER_OLDWindOnCapfac;
*phi_res_y_reg('2018',"DEU",h,"Wind_on") = phi_res_y_reg('2018',"DEU",h,"Wind_on");

phi_res(res,h) = phi_res_y_reg('2018',"DEU",h,res) ;


Equations
* Objective
obj                      Objective cost minimization

* Energy balance
con1a_bal                Supply Demand Balance in case of cost minimization

* Flex load total
*eq1_flexload

* Load change costs
con2a_loadlevel          Load change costs: Level
con2b_loadlevelstart     Load change costs: Level for first period

*full-load hours
con2c_maxprodannual_conv Full load hour constraint (for non-nuclear conventional)
con2c_maxprodannual_conv_nuc Full load hour constraint (for coal)
* Capacity contraints and flexibility constraints
con3a_maxprod_conv       Capacity Constraint conventionals
con3b_minprod_conv       Minimum production conventionals if reserves contracted

con3c_flex_PR_up        Flexibility of conventionals - provision PR up
con3d_flex_PR_do        Flexibility of conventionals - provision PR do
con3e_flex_SR_up        Flexibility of conventionals - provision SR up
con3f_flex_SR_do        Flexibility of conventionals - provision SR do
con3g_flex_MR_up        Flexibility of conventionals - provision MR up
con3h_flex_MR_do        Flexibility of conventionals - provision MR do

con3i_maxprod_ror        Capacity constraint Run-of-river
con3j_minprod_ror        Minimum production RoR if reserves contracted

con3k_maxprod_res        Capacity constraints renewables
con3l_minprod_res        Minimum production RES if reserves contracted

* Storage constraints
con4a_stolev_start        Storage Level Dynamics Initial Condition
con4b_stolev              Storage Level Dynamics

con4c_stolev_max          Storage Power Capacity
con4d_maxin_sto           Storage maximum inflow
con4e_maxout_sto          Storage maximum outflow
con4f_resrv_sto           Constraint on reserves (up)
con4g_resrv_sto           Constraint on reserves (down)

con4h_maxout_lev          Maximum storage outflow - no more than level of last period
con4i_maxin_lev           Maximum storage inflow - no more than ebergy capacity minus level of last period
con4j_ending              End level equal to initial level
con4k_PHS_EtoP            Maximum E to P ratio for PHS

* Minimum restrictions for renewables and biomass
con5a_spv_share             Gross solar PV share
con5b_wind_on_share         Gross wind onshore share
con5c_wind_off_share        Gross wind offshore share
con5d_maxBIO             Maximum yearly biomass energy
con5_demand
con5e_P2Gshare              Gross power to gas share

* DSM conditions: Load curtailment
con6a_DSMcurt_duration_max       Maximum curtailment energy budget per time
con6b_DSMcurt_max                Maximum curtailment per period

* DSM conditions: Load shifting
con7a_DSMshift_upanddown         Equalization of upshifts and downshifts in due time
con7b_DSMshift_granular_max      Maximum shifting in either direction per period
con7c_DSM_distrib_up             Distribution of upshifts between wholesale and reserves
con7d_DSM_distrib_do             Distribution of downshifts between wholesale and reserves
con7e_DSMshift_recovery          Recovery times
*con7f_DSMshift_profile           AC profile to give DSM a time-dependant nature
*con7g_DSMshift_profile_maxACpower      Maximum AC power limit
* Maximum installation conditions
con8a_max_I_con                 Maximum installable capacity: Conventionals
con8b_max_I_res                 Maximum installable capacity: Renewables
con8c_max_I_sto_e               Maximum installable energy: Storage in MWh
con8d_max_I_sto_p               Maximum installable capacity: Storage inflow-outflow in MW
con8e_max_I_dsm_cu              Maximum installable capacity: DSM load curtailment
con8f_max_I_dsm_shift_pos       Maximum installable capacity: DSM load shifting

* Reserves
con10a_reserve_prov             Reserve provision SR and MR
con10b_reserve_prov_PR          Reserve provision PR
;


*==========

* ---------------------------------------------------------------------------- *
*==========           Objective function *==========
* ---------------------------------------------------------------------------- *

obj..
         Z =E=
                 sum( (ct,h) , c_m(ct)*G_L(ct,h) )
*  turning off ramping cost for the coupled version for consistency reasons
*                 + sum( (ct,h)$(ord(h)>1) , cdata("c_up",ct)*G_UP(ct,h) )
*                 + sum( (ct,h) , cdata("c_do",ct)*G_DO(ct,h) )
*                 + sum( (res,h) , rdata("c_cu",res)*CU(res,h) )
                 + sum( (sto,h) , stodata("c_m_sto",sto) * ( STO_OUT(sto,h) + STO_IN(sto,h) ) )
%DSM%$ontext
                 + sum( (dsm_curt,h) , dsmdata_cu("c_m_dsm_cu",dsm_curt)*DSM_CU(dsm_curt,h) )
                 + sum( (dsm_shift,h) , dsmdata_shift("c_m_dsm_shift",dsm_shift) * DSM_UP_DEMAND(dsm_shift,h) )
                 + sum( (dsm_shift,h) , dsmdata_shift("c_m_dsm_shift",dsm_shift) * DSM_DO_DEMAND(dsm_shift,h) )
$ontext
$offtext
                 + sum( ct , c_i(ct)*N_CON(ct) )
                 + sum( ct , cdata("c_fix_con",ct)*N_CON(ct) )

                 + sum( res , c_i_res(res)*P_RES(res) )
                 + sum( res , rdata("c_fix_res",res)*P_RES(res) )

                 + sum( sto , c_i_sto_e(sto)*N_STO_E(sto) )
                 + sum( sto , stodata("c_fix_sto",sto)/2*(N_STO_P(sto)+N_STO_E(sto)) )
                 + sum( sto , c_i_sto_p(sto)*N_STO_P(sto) )
%DSM%$ontext
                 + sum( dsm_curt , c_i_dsm_cu(dsm_curt)*N_DSM_CU(dsm_curt) )
                 + sum( dsm_curt , dsmdata_cu("c_fix_dsm_cu",dsm_curt)*N_DSM_CU(dsm_curt) )
                 + sum( dsm_shift , c_i_dsm_shift(dsm_shift)*N_DSM_SHIFT(dsm_shift) )
                 + sum( dsm_shift , dsmdata_shift("c_fix_dsm_shift",dsm_shift)*N_DSM_SHIFT(dsm_shift) )
$ontext
$offtext
%reserves%$ontext
                 - sum( (reserves,sto,h)$(ord(reserves) = 1 or ord(reserves) = 3 or ord(reserves) = 5) , RP_STO_IN(reserves,sto,h)* phi_reserves_call(reserves,h) * c_m_sto(sto) )
                 + sum( (reserves,sto,h)$(ord(reserves) = 2 or ord(reserves) = 4 or ord(reserves) = 6) , RP_STO_IN(reserves,sto,h)* phi_reserves_call(reserves,h) * c_m_sto(sto) )
                 + sum( (reserves,sto,h)$(ord(reserves) = 1 or ord(reserves) = 3 or ord(reserves) = 5) , RP_STO_OUT(reserves,sto,h)* phi_reserves_call(reserves,h) * c_m_sto(sto) )
                 - sum( (reserves,sto,h)$(ord(reserves) = 2 or ord(reserves) = 4 or ord(reserves) = 6) , RP_STO_OUT(reserves,sto,h)* phi_reserves_call(reserves,h) * c_m_sto(sto) )
$ontext
$offtext
%DSM%$ontext
%reserves%$ontext
                 + sum( (reserves,dsm_curt,h)$(ord(reserves) = 3 or ord(reserves) = 5) , RP_DSM_CU(reserves,dsm_curt,h) * phi_reserves_call(reserves,h) * dsmdata_cu("c_m_dsm_cu",dsm_curt) )
                 + sum( (reserves,dsm_shift,h)$(ord(reserves) > 2) , RP_DSM_SHIFT(reserves,dsm_shift,h) * phi_reserves_call(reserves,h) * dsmdata_shift("c_m_dsm_shift",dsm_shift) )
$ontext
$offtext
;


* ---------------------------------------------------------------------------- *
*==========           Energy balance and load levels *==========
* ---------------------------------------------------------------------------- *

* Energy balance
con1a_bal(hh)..
         d(hh) + sum( sto , STO_IN(sto,hh) )
%DSM%$ontext
         + sum( dsm_shift , DSM_UP_DEMAND(dsm_shift,hh) )

$ontext
$offtext
         =E=
         sum( ct , G_L(ct,hh)) + sum( res , G_RES(res,hh)) + sum( sto , STO_OUT(sto,hh) )
%reserves%$ontext
* Balancing Correction Factor
        + sum( ct ,
        - RP_CON('PR_up',ct,hh) * phi_reserves_call('PR_up',hh)
        - RP_CON('SR_up',ct,hh) * phi_reserves_call('SR_up',hh)
        - RP_CON('MR_up',ct,hh) * phi_reserves_call('MR_up',hh)
        + RP_CON('PR_do',ct,hh) * phi_reserves_call('PR_do',hh)
        + RP_CON('SR_do',ct,hh) * phi_reserves_call('SR_do',hh)
        + RP_CON('MR_do',ct,hh) * phi_reserves_call('MR_do',hh) )
$ontext
$offtext
%DSM%$ontext

         + sum(dsm_shift, DSM_DO_DEMAND(dsm_shift,hh))
                    + sum(dsm_curt, DSM_CU(dsm_curt,hh))
$ontext
$offtext
;

*eq1_flexload..
*         sum( h , d2(h))  =E=  totFlexLoad
*;
*
con2a_loadlevel(ct,h)$(ord(h) > 1)..
        G_L(ct,h) =E= G_L(ct,h-1) + G_UP(ct,h) - G_DO(ct,h)
;

con2b_loadlevelstart(ct,'h1')..
         G_L(ct,'h1') =E= G_UP(ct,'h1')
;

* ---------------------------------------------------------------------------- *
*==========           CONSTRAINING ANNUAL FULL/LOAD HOURS FOR CONVENTIONAL TECHNOLOGIES   *==========
* ---------------------------------------------------------------------------- *
con2c_maxprodannual_conv(ct)$(non_nuc_ct(ct))..
       sum(h, G_L(ct,h) ) =L= 0.8*8760*N_CON(ct)
;

con2c_maxprodannual_conv_nuc("nuc")..
       sum(h, G_L("nuc",h) ) =L= 0.85*8760*N_CON("nuc")
;

* ---------------------------------------------------------------------------- *
*==========           Hourly maximum generation caps and constraints related to reserves   *==========
* ---------------------------------------------------------------------------- *

con3a_maxprod_conv(ct,h)$(ord(ct)>1 )..
        G_L(ct,h)
%reserves%$ontext
        + RP_CON('PR_up',ct,h)
        + RP_CON('SR_up',ct,h)
        + RP_CON('MR_up',ct,h)
* Balancing Correction Factor
        - RP_CON('PR_up',ct,h) * phi_reserves_call('PR_up',h)
        - RP_CON('SR_up',ct,h) * phi_reserves_call('SR_up',h)
        - RP_CON('MR_up',ct,h) * phi_reserves_call('MR_up',h)
        + RP_CON('PR_do',ct,h) * phi_reserves_call('PR_do',h)
        + RP_CON('SR_do',ct,h) * phi_reserves_call('SR_do',h)
        + RP_CON('MR_do',ct,h) * phi_reserves_call('MR_do',h)
$ontext
$offtext
        =L= N_CON(ct)
;

con3b_minprod_conv(ct,h)..
        RP_CON('PR_do',ct,h)
        + RP_CON('SR_do',ct,h)
        + RP_CON('MR_do',ct,h)
        =L= G_L(ct,h)
* Balancing Correction Factor
        - RP_CON('PR_up',ct,h) * phi_reserves_call('PR_up',h)
        - RP_CON('SR_up',ct,h) * phi_reserves_call('SR_up',h)
        - RP_CON('MR_up',ct,h) * phi_reserves_call('MR_up',h)
        + RP_CON('PR_do',ct,h) * phi_reserves_call('PR_do',h)
        + RP_CON('SR_do',ct,h) * phi_reserves_call('SR_do',h)
        + RP_CON('MR_do',ct,h) * phi_reserves_call('MR_do',h)
;

con3c_flex_PR_up(ct,h)$(ord(ct)>1 )..
        RP_CON('PR_up',ct,h)
        =L= cdata("grad_per_min",ct) * 0.5 * ( G_L(ct,h)
* Balancing Correction Factor
        - RP_CON('PR_up',ct,h) * phi_reserves_call('PR_up',h)
        - RP_CON('SR_up',ct,h) * phi_reserves_call('SR_up',h)
        - RP_CON('MR_up',ct,h) * phi_reserves_call('MR_up',h)
        + RP_CON('PR_do',ct,h) * phi_reserves_call('PR_do',h)
        + RP_CON('SR_do',ct,h) * phi_reserves_call('SR_do',h)
        + RP_CON('MR_do',ct,h) * phi_reserves_call('MR_do',h) )
;

con3d_flex_PR_do(ct,h)$(ord(ct)>1 )..
        RP_CON('PR_do',ct,h)
        =L= cdata("grad_per_min",ct) * 0.5 * ( G_L(ct,h)
* Balancing Correction Factor
        - RP_CON('PR_up',ct,h) * phi_reserves_call('PR_up',h)
        - RP_CON('SR_up',ct,h) * phi_reserves_call('SR_up',h)
        - RP_CON('MR_up',ct,h) * phi_reserves_call('MR_up',h)
        + RP_CON('PR_do',ct,h) * phi_reserves_call('PR_do',h)
        + RP_CON('SR_do',ct,h) * phi_reserves_call('SR_do',h)
        + RP_CON('MR_do',ct,h) * phi_reserves_call('MR_do',h) )
;

con3e_flex_SR_up(ct,h)$(ord(ct)>1 )..
        RP_CON('SR_up',ct,h)
        =L= cdata("grad_per_min",ct) * 5 * ( G_L(ct,h)
* Balancing Correction Factor
        - RP_CON('PR_up',ct,h) * phi_reserves_call('PR_up',h)
        - RP_CON('SR_up',ct,h) * phi_reserves_call('SR_up',h)
        - RP_CON('MR_up',ct,h) * phi_reserves_call('MR_up',h)
        + RP_CON('PR_do',ct,h) * phi_reserves_call('PR_do',h)
        + RP_CON('SR_do',ct,h) * phi_reserves_call('SR_do',h)
        + RP_CON('MR_do',ct,h) * phi_reserves_call('MR_do',h) )
;

con3f_flex_SR_do(ct,h)$(ord(ct)>1 )..
        RP_CON('SR_do',ct,h)
        =L= cdata("grad_per_min",ct) * 5 * ( G_L(ct,h)
* Balancing Correction Factor
        - RP_CON('PR_up',ct,h) * phi_reserves_call('PR_up',h)
        - RP_CON('SR_up',ct,h) * phi_reserves_call('SR_up',h)
        - RP_CON('MR_up',ct,h) * phi_reserves_call('MR_up',h)
        + RP_CON('PR_do',ct,h) * phi_reserves_call('PR_do',h)
        + RP_CON('SR_do',ct,h) * phi_reserves_call('SR_do',h)
        + RP_CON('MR_do',ct,h) * phi_reserves_call('MR_do',h) )
;

con3g_flex_MR_up(ct,h)$(ord(ct)>1 )..
        RP_CON('MR_up',ct,h)
        =L= cdata("grad_per_min",ct) * 15 * ( G_L(ct,h)
* Balancing Correction Factor
        - RP_CON('PR_up',ct,h) * phi_reserves_call('PR_up',h)
        - RP_CON('SR_up',ct,h) * phi_reserves_call('SR_up',h)
        - RP_CON('MR_up',ct,h) * phi_reserves_call('MR_up',h)
        + RP_CON('PR_do',ct,h) * phi_reserves_call('PR_do',h)
        + RP_CON('SR_do',ct,h) * phi_reserves_call('SR_do',h)
        + RP_CON('MR_do',ct,h) * phi_reserves_call('MR_do',h) )
;

con3h_flex_MR_do(ct,h)$(ord(ct)>1 )..
        RP_CON('MR_do',ct,h)
        =L= cdata("grad_per_min",ct) * 15 * ( G_L(ct,h)
* Balancing Correction Factor
        - RP_CON('PR_up',ct,h) * phi_reserves_call('PR_up',h)
        - RP_CON('SR_up',ct,h) * phi_reserves_call('SR_up',h)
        - RP_CON('MR_up',ct,h) * phi_reserves_call('MR_up',h)
        + RP_CON('PR_do',ct,h) * phi_reserves_call('PR_do',h)
        + RP_CON('SR_do',ct,h) * phi_reserves_call('SR_do',h)
        + RP_CON('MR_do',ct,h) * phi_reserves_call('MR_do',h) )
;

* Constraints on run of river
con3i_maxprod_ror(h)..
        G_L('ror',h)
%reserves%$ontext
        + RP_CON('PR_up','ror',h)
        + RP_CON('SR_up','ror',h)
        + RP_CON('MR_up','ror',h)
* Balancing Correction Factor
        - RP_CON('PR_up','ror',h) * phi_reserves_call('PR_up',h)
        - RP_CON('SR_up','ror',h) * phi_reserves_call('SR_up',h)
        - RP_CON('MR_up','ror',h) * phi_reserves_call('MR_up',h)
        + RP_CON('PR_do','ror',h) * phi_reserves_call('PR_do',h)
        + RP_CON('SR_do','ror',h) * phi_reserves_call('SR_do',h)
        + RP_CON('MR_do','ror',h) * phi_reserves_call('MR_do',h)
$ontext
$offtext
        =L= capfac_ror *N_CON('ror')
;

con3j_minprod_ror(h)..
        RP_CON('PR_do','ror',h)
        + RP_CON('SR_do','ror',h)
        + RP_CON('MR_do','ror',h)
        =L= G_L('ror',h)
* Balancing Correction Factor
        - RP_CON('PR_up','ror',h) * phi_reserves_call('PR_up',h)
        - RP_CON('SR_up','ror',h) * phi_reserves_call('SR_up',h)
        - RP_CON('MR_up','ror',h) * phi_reserves_call('MR_up',h)
        + RP_CON('PR_do','ror',h) * phi_reserves_call('PR_do',h)
        + RP_CON('SR_do','ror',h) * phi_reserves_call('SR_do',h)
        + RP_CON('MR_do','ror',h) * phi_reserves_call('MR_do',h)
;


* Constraints on renewables
con3k_maxprod_res(res,h)..
        G_RES(res,h) + CU(res,h)
%reserves%$ontext
        + RP_RES('PR_up',res,h)
        + RP_RES('SR_up',res,h)
        + RP_RES('MR_up',res,h)
$ontext
$offtext
        =E= phi_res(res,h)*P_RES(res)
;

con3l_minprod_res(res,h)..
        RP_RES('PR_do',res,h)
        + RP_RES('SR_do',res,h)
        + RP_RES('MR_do',res,h)
        =L= G_RES(res,h)
;


* ---------------------------------------------------------------------------- *
*==========           Storage constraints *==========
* ---------------------------------------------------------------------------- *

con4a_stolev_start(sto,'h1')..
        STO_L(sto,'h1') =E= stodata("phi_sto_ini",sto) * N_STO_E(sto) + STO_IN(sto,'h1')*(1+stodata("eta_sto",sto))/2 - STO_OUT(sto,'h1')/(1+stodata("eta_sto",sto))*2
;

con4b_stolev(sto,h)$( (ord(h)>1) )..
         STO_L(sto,h) =E= STO_L(sto,h-1) + STO_IN(sto,h)*(1+stodata("eta_sto",sto))/2 - STO_OUT(sto,h)/(1+stodata("eta_sto",sto))*2
%reserves%$ontext
        + sum( (reserves)$(ord(reserves) = 2 or ord(reserves) = 4 or ord(reserves) = 6) ,(RP_STO_IN(reserves,sto,h) * phi_reserves_call(reserves,h) )*(1+stodata("eta_sto",sto))/2 )
        - sum( (reserves)$(ord(reserves) = 1 or ord(reserves) = 3 or ord(reserves) = 5) ,(RP_STO_IN(reserves,sto,h) * phi_reserves_call(reserves,h) )*(1+stodata("eta_sto",sto))/2 )
        - sum( (reserves)$(ord(reserves) = 1 or ord(reserves) = 3 or ord(reserves) = 5) ,(RP_STO_OUT(reserves,sto,h) * phi_reserves_call(reserves,h) )/(1+stodata("eta_sto",sto))*2 )
        + sum( (reserves)$(ord(reserves) = 2 or ord(reserves) = 4 or ord(reserves) = 6) ,(RP_STO_OUT(reserves,sto,h) * phi_reserves_call(reserves,h) )/(1+stodata("eta_sto",sto))*2 )
$ontext
$offtext
;

con4c_stolev_max(sto,h)..
        STO_L(sto,h) =L= N_STO_E(sto)
;

con4d_maxin_sto(sto,h)..
        STO_IN(sto,h)
%reserves%$ontext
        + RP_STO_IN('PR_do',sto,h) + RP_STO_IN('SR_do',sto,h) + RP_STO_IN('MR_do',sto,h)
$ontext
$offtext
        =L= N_STO_P(sto)
;

con4e_maxout_sto(sto,h)..
        STO_OUT(sto,h)
%reserves%$ontext
        + RP_STO_OUT('PR_up',sto,h) + RP_STO_OUT('SR_up',sto,h) + RP_STO_OUT('MR_up',sto,h)
$ontext
$offtext
        =L= N_STO_P(sto)
;

con4f_resrv_sto(sto,h)..
        RP_STO_IN('PR_up',sto,h) + RP_STO_IN('SR_up',sto,h) + RP_STO_IN('MR_up',sto,h)
        =L= STO_IN(sto,h)
;

con4g_resrv_sto(sto,h)..
        RP_STO_OUT('PR_do',sto,h) + RP_STO_OUT('SR_do',sto,h) + RP_STO_OUT('MR_do',sto,h)
        =L= STO_OUT(sto,h)
;

con4h_maxout_lev(sto,h)..
        ( STO_OUT(sto,h)
%reserves%$ontext
        + RP_STO_OUT('PR_up',sto,h) + RP_STO_OUT('SR_up',sto,h) + RP_STO_OUT('MR_up',sto,h)
$ontext
$offtext
        ) /(1+stodata("eta_sto",sto))*2
        =L= STO_L(sto,h-1)
;

con4i_maxin_lev(sto,h)..
        ( STO_IN(sto,h)
%reserves%$ontext
        + RP_STO_IN('PR_do',sto,h) + RP_STO_IN('SR_do',sto,h) + RP_STO_IN('MR_do',sto,h)
$ontext
$offtext
        ) * (1+stodata("eta_sto",sto))/2
        =L= N_STO_E(sto) - STO_L(sto,h-1)
;

con4j_ending(sto,h)$( ord(h) = card(h) )..
         STO_L(sto,h) =E= stodata("phi_sto_ini",sto) * N_STO_E(sto)
;

con4k_PHS_EtoP('Sto5')..
        N_STO_E('Sto5') =L= stodata("etop_max",'Sto5') * N_STO_P('Sto5')
;


* ---------------------------------------------------------------------------- *
*==========           Quotas for renewables and biomass *==========
* ---------------------------------------------------------------------------- *

*sum( h , P_RES("Solar") * phi_res("Solar",h ) )
*       =E= spv_share * net_energy_demand ;
*sum( h , P_RES("Wind_on") * phi_res("Wind_on",h )+P_RES("Wind_off") * phi_res("Wind_off",h ) )
*      =E= wind_share_on * net_energy_demand;



*
*con5a_spv_share..
*sum( h , G_RES("Solar",h))
*        =E= spv_share * sum(h,d(h)) ;
*
*con5b_wind_on_share..
*sum( h , (G_RES("Wind_on",h) ))
*        =E= wind_share_on * sum(h,d(h));
*
*
*con5c_wind_off_share..
*        sum( h , (G_RES("Wind_off",h)+CU("Wind_off",h) ))
*        =E= wind_share_off * sum(h,d(h));
*
*
*con5d_maxBIO..
*         sum( h , G_L('bio',h) ) =L= cdata("m_con_e",'bio')
*;
*
*
**power to gas share
*con5e_P2Gshare..
*sum( h , STO_OUT("sto7",h) )
*        =E= p2g_share * sum(h,d(h)) ;
*


* ---------------------------------------------------------------------------- *
*==========           DSM constraints - curtailment *==========
* ---------------------------------------------------------------------------- *

con6a_DSMcurt_duration_max(dsm_curt,h)..
         sum( hh$( ord(hh) >= ord(h) AND ord(hh) < ord(h) + dsmdata_cu("t_off_dsm_cu",dsm_curt) ) , DSM_CU(dsm_curt,hh)
%reserves%$ontext
         + RP_DSM_CU('SR_up',dsm_curt,hh) * phi_reserves_call('SR_up',hh)
         + RP_DSM_CU('MR_up',dsm_curt,hh) * phi_reserves_call('MR_up',hh)
$ontext
$offtext
         )
         =L= N_DSM_CU(dsm_curt) * dsmdata_cu("t_dur_dsm_cu",dsm_curt)
;

con6b_DSMcurt_max(dsm_curt,h)..
        DSM_CU(dsm_curt,h)
%reserves%$ontext
        + RP_DSM_CU('SR_up',dsm_curt,h)
        + RP_DSM_CU('MR_up',dsm_curt,h)
$ontext
$offtext
          =L= N_DSM_CU(dsm_curt)
;


* ---------------------------------------------------------------------------- *
*==========           DSM constraints - shifting *==========
* ---------------------------------------------------------------------------- *

con7a_DSMshift_upanddown(dsm_shift,h)..
    DSM_UP(dsm_shift,h) * dsmdata_shift("eta_dsm_shift",dsm_shift)
    =E=
   sum( hh$( ( ord(hh) >= ord(h) ) AND ( ord(hh) <= ( ord(h) + dsmdata_shift("t_dur_dsm_shift",dsm_shift) ) ) ) ,
    DSM_DO(dsm_shift,h,hh)
  )
;

con7b_DSMshift_granular_max(dsm_shift,h)..
         DSM_UP_DEMAND(dsm_shift,h) + DSM_DO_DEMAND(dsm_shift,h)
%reserves%$ontext
         + sum( reserves$(ord(reserves) > 2) , RP_DSM_SHIFT(reserves,dsm_shift,h) )
$ontext
$offtext
         =L= N_DSM_SHIFT(dsm_shift)
;

con7c_DSM_distrib_up(dsm_shift,h)..
         DSM_UP(dsm_shift,h) =E= DSM_UP_DEMAND(dsm_shift,h)
%reserves%$ontext
         + RP_DSM_SHIFT('SR_do',dsm_shift,h) * phi_reserves_call('SR_do',h)
         + RP_DSM_SHIFT('MR_do',dsm_shift,h) * phi_reserves_call('MR_do',h)
$ontext
$offtext
;

con7d_DSM_distrib_do(dsm_shift,h)..
         sum( hh$( ord(hh) >= ord(h) - dsmdata_shift("t_dur_dsm_shift",dsm_shift) AND ord(hh) <= ord(h) + dsmdata_shift("t_dur_dsm_shift",dsm_shift) ) , DSM_DO(dsm_shift,hh,h) )
                 =E=
         DSM_DO_DEMAND(dsm_shift,h)
%reserves%$ontext
         + RP_DSM_SHIFT('SR_up',dsm_shift,h) * phi_reserves_call('SR_up',h)
         + RP_DSM_SHIFT('MR_up',dsm_shift,h) * phi_reserves_call('MR_up',h)
$ontext
$offtext
;

con7e_DSMshift_recovery(dsm_shift,h)..
         sum( hh$( ord(hh) >= ord(h) AND ord(hh) < ord(h) + dsmdata_shift("t_off_dsm_shift",dsm_shift) ) , DSM_UP(dsm_shift,hh))
         =L= N_DSM_SHIFT(dsm_shift) * dsmdata_shift("t_dur_dsm_shift",dsm_shift)
;
*con7f_DSMshift_profile(dsm_shift,h)..
*
*        DSM_DO_DEMAND(dsm_shift,h)
*
*           =L=
*
*phi_AC('2013','IND',h)* N_DSM_SHIFT(dsm_shift)
*
*;
*
*
*con7g_DSMshift_profile_maxACpower(dsm_shift,h)..
*
*        DSM_UP_DEMAND(dsm_shift,h)
*
*           =L=
*
*        1.2 * N_DSM_SHIFT(dsm_shift) -  N_DSM_SHIFT(dsm_shift)* phi_AC('2013','IND',h)
*;
*
* ---------------------------------------------------------------------------- *
*==========           Maximum installation constraints *==========
* ---------------------------------------------------------------------------- *

con8a_max_I_con(ct)..
         N_CON(ct) =L= cdata("m_con",ct)
;

con8b_max_I_res(res)..
         P_RES(res) =L= rdata("m_res",res)
;

con8c_max_I_sto_e(sto)..
         N_STO_E(sto) =L= stodata("m_sto_e",sto)
;

con8d_max_I_sto_p(sto)..
         N_STO_P(sto) =L= stodata("m_sto_p",sto)
;

con8e_max_I_dsm_cu(dsm_curt)..
         N_DSM_CU(dsm_curt) =L= dsmdata_cu("m_dsm_cu",dsm_curt)
;

con8f_max_I_dsm_shift_pos(dsm_shift)..
         N_DSM_SHIFT(dsm_shift) =L= dsmdata_shift("m_dsm_shift",dsm_shift)
;

* ---------------------------------------------------------------------------- *
*==========          Reserve constraints *==========
* ---------------------------------------------------------------------------- *

con10a_reserve_prov(reserves,h)$( ord(reserves) > 2)..
        sum(ct, RP_CON(reserves,ct,h))
        + sum(res, RP_RES(reserves,res,h))
        + sum(sto, RP_STO_IN(reserves,sto,h) + RP_STO_OUT(reserves,sto,h))
%DSM%$ontext
        + sum(dsm_curt, RP_DSM_CU(reserves,dsm_curt,h))$( ord(reserves) = 3 OR ord(reserves) = 5 )
        + sum(dsm_shift , RP_DSM_SHIFT(reserves,dsm_shift,h) )
$ontext
$offtext
        =E= (
            1000 * phi_reserves_share(reserves) * (
            reservedata("reserves_intercept",reserves) + sum( res , reserves_slope(reserves,res) * P_RES(res)/1000 ) ) )$(ord(h) > 1)
;

con10b_reserve_prov_PR(reserves,h)$( ord(reserves) < 3)..
        sum(ct, RP_CON(reserves,ct,h))
        + sum(res, RP_RES(reserves,res,h))
        + sum(sto, RP_STO_IN(reserves,sto,h) + RP_STO_OUT(reserves,sto,h) )
         =E= phi_reserves_pr* sum( reservesreserves$( ord(reservesreserves) > 2), 1000 * phi_reserves_share(reservesreserves) * (
            reservedata("reserves_intercept",reservesreserves) + sum( res , reserves_slope(reservesreserves,res) * P_RES(res)/1000 ) ) )$(ord(h) > 1)
;


********************************************************************************
*==========           MODEL *==========
********************************************************************************

model DIETER /
obj

con1a_bal
*eq1_flexload
con2a_loadlevel
con2b_loadlevelstart

con2c_maxprodannual_conv
con2c_maxprodannual_conv_nuc

con3a_maxprod_conv
con3i_maxprod_ror
con3k_maxprod_res

con4a_stolev_start
con4b_stolev
con4c_stolev_max
con4d_maxin_sto
con4e_maxout_sto
con4h_maxout_lev
con4i_maxin_lev
con4j_ending
con4k_PHS_EtoP

*con5d_maxBIO

*con8a_max_I_con
*con8b_max_I_res
*con8c_max_I_sto_e
*con8d_max_I_sto_p

%DSM%$ontext
con6a_DSMcurt_duration_max
con6b_DSMcurt_max

con7a_DSMshift_upanddown
con7b_DSMshift_granular_max
con7c_DSM_distrib_up
con7d_DSM_distrib_do
*con_7e_DSMshift_recovery
*con7f_DSMshift_profile
*con7g_DSMshift_profile_maxACpower

con8e_max_I_dsm_cu
con8f_max_I_dsm_shift_pos
$ontext
$offtext

%reserves%$ontext
con3b_minprod_conv
con3c_flex_PR_up
con3d_flex_PR_do
con3e_flex_SR_up
con3f_flex_SR_do
con3g_flex_MR_up
con3h_flex_MR_do
con3j_minprod_ror
con3l_minprod_res

con4f_resrv_sto
con4g_resrv_sto

con10a_reserve_prov
con10b_reserve_prov_PR
$ontext
$offtext
/;

*==========
*==========           Options, fixings, report preparation *==========
*==========

*==========
*==========           SOLVER OPTIONS *==========
*==========

option lp = cplex;
option threads = 6;

parameter
corr_fac_con
corr_fac_res
corr_fac_sto
corr_fac_dsm_cu
corr_fac_dsm_shift
gross_energy_demand
calc_maxprice
calc_minprice
report4RM
report
report_tech
report_tech_hours
report_hours
report_reserves
report_reserves_hours
report_reserves_tech
report_reserves_tech_hours
res_min
;

* Min and max for prices
calc_maxprice = 0 ;
calc_minprice = 1000 ;
res_min       = 0;

* Assign default correction factors
corr_fac_con(ct,h) = 0 ;
corr_fac_res(res,h) = 0 ;
corr_fac_sto(sto,h) = 0 ;
corr_fac_dsm_cu(dsm_curt,h) = 0 ;
corr_fac_dsm_shift(dsm_shift,h) = 0 ;

solve DIETER using lp minimizing Z ;
$include reporting.gms

report4RM(yr,reg,ct,'capacity') = N_CON.l(ct);
report4RM(yr,reg,res,'capacity') = P_RES.l(res);

report4RM(yr,reg,"all_te",'peakDem_relFac') = SMax(h, d(h))/sum(h,d(h));

report4RM(yr,reg,"all_te",'peakDem') = SMax(h, d(h));

report4RM(yr,reg,ct,'capfac')$( N_CON.l(ct) ne 0 ) = sum( h , G_L.l(ct,h)) / (N_CON.l(ct) * card(h));
*combine hc and lig together into one CF to pass onto REMIND
report4RM(yr,reg,'coal','capfac')$( N_CON.l("hc") + N_CON.l("lig") ne 0 ) = sum( h , (G_L.l("hc",h) + G_L.l("lig",h))) / ((N_CON.l("hc") + N_CON.l("lig")) * card(h));
report4RM(yr,reg,res,'capfac')$(P_RES.l(res) ne 0 ) = sum( h , G_RES.l(res,h)) / (P_RES.l(res) * card(h));

report4RM(yr,reg,res,'generation') = sum( h , G_RES.l(res,h) );
report4RM(yr,reg,ct,'generation') = sum( h , G_L.l(ct,h) );

*also export zero values (prevent compression)
report4RM(yr,reg,ct,'capacity')$(not report4RM(yr,reg,ct,'capacity')) = eps;
report4RM(yr,reg,res,'capacity')$(not report4RM(yr,reg,res,'capacity')) = eps;
report4RM(yr,reg,ct,'capfac')$(not report4RM(yr,reg,ct,'capfac')) = eps;
report4RM(yr,reg,'coal','capfac')$(not report4RM(yr,reg,'coal','capfac')) = eps;
report4RM(yr,reg,res,'capfac')$(not report4RM(yr,reg,res,'capfac')) = eps;
report4RM(yr,reg,ct,'generation')$(not report4RM(yr,reg,ct,'generation')) = eps;
report4RM(yr,reg,res,'generation')$(not report4RM(yr,reg,res,'generation')) = eps;

execute_unload "results_DIETER_y10", report4RM;

execute_unload "full_DIETER_y10";
execute_unload "report_DIETER_y10", report, report_tech, report_hours, report_tech_hours;

%write_to_excel%$ontext
$include report_to_excel
$ontext
$offtext


* ---------------------------------------------------------------------------- *
* ---------------------------------------------------------------------------- *
* ---------------------------------------------------------------------------- *

