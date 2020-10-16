*==========
$ontext
The Dispatch and Investment Evaluation Tool with Endogenous Renewables (DIETER).
Version 1.0.2, January 2016.
Written by Alexander Zerrahn and Wolf-Peter Schill.
This work is licensed under the MIT License (MIT).
For more information on this license, visit http://opensource.org/licenses/mit-license.php.
Whenever you use this code, please refer to http://www.diw.de/DIETER.
This version constitutes a minor revision of the model documented in Zerrahn, A., Schill, W.-P. (2015): A greenfield model to evaluate long-run power storage requirements for high shares of renewables. DIW Discussion Paper 1457. http://www.diw.de/documents/publikationen/73/diw_01.c.498475.de/dp1457.pdf
We are happy to receive feedback under azerrahn@diw.de and wschill@diw.de.
$offtext
*==========


*Fluctuating renewable Scenarios within each loop
$include Scenario.gms

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
se_remind   secondary energy of remind             /seel/
*yr          year for remind                       /2000,2010, 2015,2020,2025/
yr          year for remind power sector

t           year from remind to be loaded                
te_remind   remind technonlogy					  /spv, wind/
grade 	    remind grade level for technology	  /1*12/
reg        region set                              /IND,DEU/
r(reg)     one region set                          /IND/

*============== DIETER sets ==================
year      yearly time data                       /2011, 2012, 2013, 2013_windonsmooth/
all_cdata Data for Conventional Technologies     /eta_con,carbon_content,c_up,c_do,c_fix_con,c_var_con,c_inv_overnight_con,inv_lifetime_con,inv_recovery_con,inv_interest_con,m_con,m_con_e,grad_per_min/
all_rdata Data for Renewable Technologies        /c_cu,c_fix_res,phi_min_res,c_inv_overnight_res,inv_lifetime_res,inv_recovery_res,inv_interest_res,m_res,m_res_e/
ct        Conventional Technologies              /ror, nuc, lig, hc, CCGT, OCGT_eff, OCGT_ineff, bio/
res       Renewable technologies                 /Wind_on, Wind_off, Solar/
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
alias (reserves,reservesreserves) ;

*==========

$include dataload.gms
*$stop

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

*==========


Equations
* Objective
obj                      Objective cost minimization

* Energy balance
con1a_bal                Supply Demand Balance in case of cost minimization

* Load change costs
con2a_loadlevel          Load change costs: Level
con2b_loadlevelstart     Load change costs: Level for first period

*full-load hours
con2c_maxprodannual_conv Full load hour constraint (for coal)

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
con7f_DSMshift_profile           AC profile to give DSM a time-dependant nature
con7g_DSMshift_profile_maxACpower      Maximum AC power limit
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
                 + sum( (ct,h)$(ord(h)>1) , cdata("c_up",ct)*G_UP(ct,h) )
                 + sum( (ct,h) , cdata("c_do",ct)*G_DO(ct,h) )
                 + sum( (res,h) , rdata("c_cu",res)*CU(res,h) )
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

con2a_loadlevel(ct,h)$(ord(h) > 1)..
        G_L(ct,h) =E= G_L(ct,h-1) + G_UP(ct,h) - G_DO(ct,h)
;

con2b_loadlevelstart(ct,'h1')..
         G_L(ct,'h1') =E= G_UP(ct,'h1')
;

* ---------------------------------------------------------------------------- *
*==========           cONSTRAINING ANNUAL FULL/LOAD HOURS FOR CONVENTIONAL TECHNOLOGIES   *==========
* ---------------------------------------------------------------------------- *
con2c_maxprodannual_conv(ct)$(ord(ct)>1 )..
       sum(h, G_L(ct,h) ) =L=  0.85*8760*N_CON(ct)
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
        =L= phi_ror("2013","IND",h)*N_CON('ror')
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




con5a_spv_share..
sum( h , G_RES("Solar",h))
        =E= spv_share * sum(h,d(h)) ;

con5b_wind_on_share..
sum( h , (G_RES("Wind_on",h) ))
        =E= wind_share_on * sum(h,d(h));


con5c_wind_off_share..
        sum( h , (G_RES("Wind_off",h)+CU("Wind_off",h) ))
        =E= wind_share_off * sum(h,d(h));


con5d_maxBIO..
         sum( h , G_L('bio',h) ) =L= cdata("m_con_e",'bio')
;


*power to gas share
con5e_P2Gshare..
sum( h , STO_OUT("sto7",h) )
        =E= p2g_share * sum(h,d(h)) ;



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
con7f_DSMshift_profile(dsm_shift,h)..

        DSM_DO_DEMAND(dsm_shift,h)

           =L=

phi_AC('2013','IND',h)* N_DSM_SHIFT(dsm_shift)

;


con7g_DSMshift_profile_maxACpower(dsm_shift,h)..

        DSM_UP_DEMAND(dsm_shift,h)

           =L=

        1.2 * N_DSM_SHIFT(dsm_shift) -  N_DSM_SHIFT(dsm_shift)* phi_AC('2013','IND',h)
;

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
con2a_loadlevel
con2b_loadlevelstart

con2c_maxprodannual_conv

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

con5d_maxBIO

con8a_max_I_con
con8b_max_I_res
con8c_max_I_sto_e
con8d_max_I_sto_p

%DSM%$ontext
con6a_DSMcurt_duration_max
con6b_DSMcurt_max

con7a_DSMshift_upanddown
con7b_DSMshift_granular_max
con7c_DSM_distrib_up
con7d_DSM_distrib_do
* con_7e_DSMshift_recovery
con7f_DSMshift_profile
con7g_DSMshift_profile_maxACpower

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

* Parameters for the report file
parameter
corr_fac_con
corr_fac_res
corr_fac_sto
corr_fac_dsm_cu
corr_fac_dsm_shift
gross_energy_demand

;


* Parameters for default base year
*d(h) = d_y('2013',h) ;
*phi_res(res,h) = phi_res_y('2013',res,h) ;
phi_reserves_call(reserves,h) = phi_reserves_call_y('2013',h,reserves) ;
phi_mean_reserves_call(reserves) = phi_mean_reserves_call_y('2013',reserves) ;



*==========
*==========           Solve *==========
*==========


phi_res(res,h) = phi_res_y_reg('2013',"IND",h,res) ;
c_m(ct) = c_m_reg(ct,"IND");


* Assign default correction factors
corr_fac_con(ct,h) = 0 ;
corr_fac_res(res,h) = 0 ;
corr_fac_sto(sto,h) = 0 ;
corr_fac_dsm_cu(dsm_curt,h) = 0 ;
corr_fac_dsm_shift(dsm_shift,h) = 0 ;


*==========
*==========           SOLVER OPTIONS *==========
*==========

*options
*optcr = 0.00
*reslim = 10000000
*lp = cplex
*;
*
*option
*dispwidth = 15,
*limrow = 0,
*limcol = 0,
**solprint = off,res_share_0
*sysout = off ,
*threads = 10 ;
**
*$onecho > cplex.opt
*lpmethod 6
*threads = 6
*$offecho
*DIETER.optfile=1;
*option lp=cplexd;



$onecho > cplex.opt
threads=6
parallelmode -1
$offecho
DIETER.optfile=1;

***      -------------------------------------------------------------------
***                     open year loop
***      -------------------------------------------------------------------
set varname /Capacity/;
*set model   /DIETER/;

Parameter handle(yr)      'model handles';
Parameter report(yr, varname, sto) 'report';

DIETER.solveLink = 3;

loop(yr$(yr.val > 2010),

***      -------------------------------------------------------------------
***                     SOLVE statement
***      -------------------------------------------------------------------

    
P_RES("Solar") = sum(grade, remind_cap(yr, "DEU", "spv", grade)) * 1e6;
P_RES("Wind_on") = sum(grade, remind_cap(yr, "DEU", "wind", grade)) * 1e6;
P_RES("Wind_off") = 0;

*converting TWa to MWh
remind_totdemand_inMWh = remind_totdemand(yr, "DEU", "seel") * 365 * 24 * 1e6;

DIETER_OLDtotdem = sum( h , d_y_reg('2013',"IND",h)); ;
demConvR = remind_totdemand_inMWh / DIETER_OLDtotdem;
d(h) = d_y_reg('2013',"IND",h) * demConvR ;

solve DIETER using lp minimizing Z ;

handle(yr) = DIETER.handle;
);

repeat
    loop(yr$handleCollect(handle(yr)),
       report(yr, varname, sto) = N_STO_P.l(sto);
       display$handleDelete(handle(yr)) 'trouble deleting handles' ;
       handle(yr) = 0; 
    );

display$readyCollect(handle, 100) 'Problem waiting for next instance to complete';
until card(handle) = 0 or timeelapsed > 1;
*until card(handle) = 0;

* Read out solutions
execute_unload "results_DIETER", report ;

%write_to_excel%$ontext
$include report_to_excel
$ontext
$offtext


* ---------------------------------------------------------------------------- *
* ---------------------------------------------------------------------------- *
* ---------------------------------------------------------------------------- *
