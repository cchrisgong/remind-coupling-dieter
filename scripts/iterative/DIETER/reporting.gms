        report_tech_hours('DIETER',yr,reg,'generation',ct,h) =  G_L.l(ct,h) + corr_fac_con(ct,h) ;
        report_tech_hours('DIETER',yr,reg,'generation',res,h) = G_RES.l(res,h) ;
        report_tech_hours('DIETER',yr,reg,'curtailment of fluct res',res,h) =  CU.l(res,h) ;
        report_tech_hours('DIETER',yr,reg,'generation storage',sto,h) =  STO_OUT.l(sto,h) ;
        report_tech_hours('DIETER',yr,reg,'storage loading',sto,h) =  STO_IN.l(sto,h) ;
        report_tech_hours('DIETER',yr,reg,'storage level',sto,h) =  STO_L.l(sto,h) ;
        report_hours('DIETER',yr,reg,'fixed demand',h) = d(h) ;
*       report_hours('DIETER',yr,reg,'flexible demand',h) = d2.l(h) ;

        report_hours('DIETER',yr,reg,'price',h) = -con1a_bal.m(h);

* Report files: cap. transformed into GW (divide by 1e3), generation transformed into TWh (divide by 1e6), costs to $/kW, share to *100%
   
        report('DIETER',yr,reg,'net_energy_demand') = sum( h , d(h)) /1e6;
        report('DIETER',yr,reg,'model status') = DIETER.modelstat ;
        report('DIETER',yr,reg,'solve time') = DIETER.resUsd ;
*       transform into BillionUSD
        report('DIETER',yr,reg,'obj value') = Z.l /1e9;
        report('DIETER',yr,reg,'CO2 price') = con_CO2price; 
        report('DIETER',yr,reg,'load-weighted price for fixed demand') = -sum(h,con1a_bal.m(h)*d(h))/sum(h,d(h)) ;
        
* Define gross energy demand for reporting, egual to equation 5a
        gross_energy_demand = sum( h , d(h) + sum( sto , STO_IN.l(sto,h) - STO_OUT.l(sto,h) ));        
        report('DIETER',yr,reg,'gross_energy_demand') = gross_energy_demand /1e6;

*       report('DIETER',yr,reg,'curtailment of fluct res absolute') = sum((res,h),CU.l(res,h))  /1e6 ;
*       report('DIETER',yr,reg,'curtailment of fluct res relative')$report('DIETER',yr,reg,'curtailment of fluct res absolute') = sum((res,h),CU.l(res,h))/( sum((res,h),G_RES.l(res,h) - corr_fac_res(res,h) ) + sum((res,h),CU.l(res,h)) ) ;
*       report('DIETER',yr,reg,'bio not utilized absolute')$(cdata("m_con_e",'bio')) = (cdata("m_con_e",'bio') - sum(h,G_L.l('bio',h)))  ;
*       report('DIETER',yr,reg,'bio not utilized relative')$(cdata("m_con_e",'bio')) = (cdata("m_con_e",'bio') - sum(h,G_L.l('bio',h)))/cdata("m_con_e",'bio') ;

        report_tech('DIETER',yr,reg,'net shares',res) = sum( h , G_RES.l(res,h))/sum(h,d(h)) * 1e2;


***     capacity MW -> GW

        report_tech('DIETER',yr,reg,'DIETER pre-investment capacities',ct) = N_CON.lo(ct) / 1e3 ;
        report_tech('DIETER',yr,reg,'DIETER pre-investment capacities',res) = P_RES.lo(res) / 1e3 ;
        
        report_tech('DIETER',yr,reg,'DIETER post-investment capacities',ct) =  N_CON.l(ct) / 1e3 ;
        report_tech('DIETER',yr,reg,'DIETER post-investment capacities',res) =  P_RES.l(res) / 1e3 ;

***     reporting on remind run but only every 5 iterations (can be used as a check to reporting in R or as validation)
        
        report_tech('REMIND',yr,reg,'REMIND pre-investment capacities',ct) = N_CON.lo(ct) / 1e3 ;
        report_tech('REMIND',yr,reg,'REMIND pre-investment capacities',"ror") = preInv_remind_cap(yr, reg, "hydro", "1") * 1e3;
        report_tech('REMIND',yr,reg,'REMIND pre-investment capacities',"Solar") = preInv_remind_cap(yr, reg, "spv", "1") * 1e3;
        report_tech('REMIND',yr,reg,'REMIND pre-investment capacities',"Wind_on") = preInv_remind_cap(yr, reg, "wind", "1") * 1e3;
   
*       MW -GW
        report_tech('REMIND',yr,reg,'REMIND post-investment capacities',ct_remind) = RM_postInv_cap_con(yr,reg,ct_remind)/ 1e3;
        report_tech('REMIND',yr,reg,'REMIND post-investment capacities',res) = RM_postInv_cap_res(yr,reg,res)/ 1e3;

***     generation MWh -> TWh
        report_tech('REMIND',yr,reg,'REMIND post-investment generation',ct_remind) = RM_postInv_prodSe_con(yr,reg,ct_remind) /1e6;
        report_tech('REMIND',yr,reg,'REMIND post-investment generation',res) = RM_postInv_prodSe_res(yr,reg,res) /1e6;

*       ========== report cost ============
*       efficiency -> %
        report_tech('DIETER',yr,reg,'fuel efficiency', ct) = cdata("eta_con",ct) * 1e2;

*       investment cost ($/MW -> $/kW)
        
        report_tech('DIETER',yr,reg,'annualized investment cost',ct) = c_i(ct) /1e3;
        report_tech('DIETER',yr,reg,'annualized investment cost',res) = c_i_res(res) / 1e3;
*       OM cost
        report_tech('DIETER',yr,reg,'O&M cost',ct) = cdata("c_fix_con",ct) / 1e3;
        report_tech('DIETER',yr,reg,'O&M cost',res) = rdata("c_fix_res",res) / 1e3;
*       fuel cost
        report_tech('DIETER',yr,reg,'primary energy price',ct) = con_fuelprice_reg(ct,reg);
        report_tech('DIETER',yr,reg,'fuel cost (divided by eta)',ct) = con_fuelprice_reg(ct,reg)/cdata("eta_con",ct) ;
*       CO2 cost
        report_tech('DIETER',yr,reg,'CO2 cost',ct) = cdata("carbon_content",ct)/cdata("eta_con",ct) * con_CO2price ;
        

***     LCOE (for investment c_i "lig" is the same as "coal" ) LCOE = (IC+OM) * cap /gen + CO2 + FC
*       IC cost: $/MW, CAP: MW, PRODSE: MWh
        report_tech('REMIND',yr,reg,'REMIND LCOE ($/MWh)','coal') = ( c_i("lig") + cdata("c_fix_con","lig") ) *  RM_postInv_cap_con(yr,reg,"coal") / RM_postInv_prodSe_con(yr,reg,"coal")
                                                                                + con_fuelprice_reg("lig",reg)/cdata("eta_con","lig") + cdata("carbon_content","lig")/cdata("eta_con","lig") * con_CO2price ;
        report_tech('REMIND',yr,reg,'REMIND LCOE ($/MWh)','CCGT') = ( c_i("CCGT") + cdata("c_fix_con","CCGT")) *  RM_postInv_cap_con(yr,reg,"CCGT") / RM_postInv_prodSe_con(yr,reg,"CCGT")
                                                                                + con_fuelprice_reg("CCGT",reg)/cdata("eta_con","CCGT") + cdata("carbon_content","CCGT")/cdata("eta_con","CCGT") * con_CO2price ;
        report_tech('REMIND',yr,reg,'REMIND LCOE ($/MWh)','OCGT_eff') = ( c_i("OCGT_eff") + cdata("c_fix_con","OCGT_eff") ) * RM_postInv_cap_con(yr,reg,"OCGT_eff") / RM_postInv_prodSe_con(yr,reg,"OCGT_eff")
                                                                                + con_fuelprice_reg("OCGT_eff",reg)/cdata("eta_con","OCGT_eff") + cdata("carbon_content","OCGT_eff")/cdata("eta_con","OCGT_eff") * con_CO2price ;
        report_tech('REMIND',yr,reg,'REMIND LCOE ($/MWh)','bio') = ( c_i("bio") + cdata("c_fix_con","bio") )  *  RM_postInv_cap_con(yr,reg,"bio") / RM_postInv_prodSe_con(yr,reg,"bio")
                                                                                + con_fuelprice_reg("bio",reg) /cdata("eta_con","bio");
        report_tech('REMIND',yr,reg,'REMIND LCOE ($/MWh)','nuc') = ( c_i("nuc") + cdata("c_fix_con","nuc") )  *  RM_postInv_cap_con(yr,reg,"nuc") / RM_postInv_prodSe_con(yr,reg,"nuc")
                                                                                + con_fuelprice_reg("nuc",reg) /cdata("eta_con","nuc");
        report_tech('REMIND',yr,reg,'REMIND LCOE ($/MWh)','ror') = ( c_i("ror") + cdata("c_fix_con","ror") )  *  RM_postInv_cap_con(yr,reg,"ror") / RM_postInv_prodSe_con(yr,reg,"ror") ;
        report_tech('REMIND',yr,reg,'REMIND LCOE ($/MWh)',res) = ( c_i_res(res) + rdata("c_fix_res",res) ) *  RM_postInv_cap_res(yr,reg,res) / RM_postInv_prodSe_res(yr,reg,res) ;

***     TW -> GW
        report_tech('REMIND',yr,reg,'REMIND divestment (GW)',"lig") = sum(te_remind, earlyRetiCap_reporting(yr, reg, te_remind)$(COALte(te_remind)) ) * 1e3 /2 ;
        report_tech('REMIND',yr,reg,'REMIND divestment (GW)',"hc") = sum(te_remind, earlyRetiCap_reporting(yr, reg, te_remind)$(COALte(te_remind)) ) * 1e3 /2 ;
        report_tech('REMIND',yr,reg,'REMIND divestment (GW)',"CCGT") = sum(te_remind, earlyRetiCap_reporting(yr, reg, te_remind)$(NonPeakGASte(te_remind)) ) * 1e3;
        report_tech('REMIND',yr,reg,'REMIND divestment (GW)',"OCGT_eff") = earlyRetiCap_reporting(yr, reg, "ngt") * 1e3;
        report_tech('REMIND',yr,reg,'REMIND divestment (GW)',"bio") = sum(te_remind, earlyRetiCap_reporting(yr, reg, te_remind)$(BIOte(te_remind)) ) * 1e3;
        report_tech('REMIND',yr,reg,'REMIND divestment (GW)',"nuc") = sum(te_remind, earlyRetiCap_reporting(yr, reg, te_remind)$(NUCte(te_remind)) ) * 1e3;
        report_tech('REMIND',yr,reg,'REMIND divestment (GW)',"ror") = earlyRetiCap_reporting(yr, reg, "hydro") * 1e3;
        report_tech('REMIND',yr,reg,'REMIND divestment (GW)',"Solar") = earlyRetiCap_reporting(yr, reg, "spv") * 1e3;
        report_tech('REMIND',yr,reg,'REMIND divestment (GW)',"Wind_on") = earlyRetiCap_reporting(yr, reg, "wind") * 1e3;
        
***     
        report_tech('REMIND',yr,reg,'REMIND added capacities (GW)',"coal") = sum(te_remind, added_remind_cap(yr, reg, te_remind, "1")$(COALte(te_remind))) * 1e3;
        report_tech('REMIND',yr,reg,'REMIND added capacities (GW)',"lig") = sum(te_remind, added_remind_cap(yr, reg, te_remind, "1")$(COALte(te_remind))) * 1e3 /2;
        report_tech('REMIND',yr,reg,'REMIND added capacities (GW)',"hc") = sum(te_remind, added_remind_cap(yr, reg, te_remind, "1")$(COALte(te_remind))) * 1e3 /2;
        report_tech('REMIND',yr,reg,'REMIND added capacities (GW)',"CCGT") = sum(te_remind, added_remind_cap(yr, reg, te_remind, "1")$(NonPeakGASte(te_remind))) * 1e3;
        report_tech('REMIND',yr,reg,'REMIND added capacities (GW)',"OCGT_eff") = added_remind_cap(yr, reg, "ngt", "1") * 1e3;
        report_tech('REMIND',yr,reg,'REMIND added capacities (GW)',"bio") = sum(te_remind, added_remind_cap(yr, reg, te_remind, "1")$(BIOte(te_remind))) * 1e3;
        report_tech('REMIND',yr,reg,'REMIND added capacities (GW)',"ror") = added_remind_cap(yr, reg, "hydro", "1") * 1e3;
        report_tech('REMIND',yr,reg,'REMIND added capacities (GW)',"nuc") = sum(te_remind, added_remind_cap(yr, reg, te_remind, "1")$(NUCte(te_remind))) * 1e3;
        report_tech('REMIND',yr,reg,'REMIND added capacities (GW)',"Solar") = added_remind_cap(yr, reg, "spv", "1") * 1e3;
        report_tech('REMIND',yr,reg,'REMIND added capacities (GW)',"Wind_on") = added_remind_cap(yr, reg, "wind", "1") * 1e3;
        

        report_tech('REMIND',yr,reg,'REMIND CapFac (%)',"coal") = remind_CF(yr,reg,"pc")*1e2;
        report_tech('REMIND',yr,reg,'REMIND CapFac (%)',"CCGT") = remind_CF(yr,reg,"ngcc")*1e2;
        report_tech('REMIND',yr,reg,'REMIND CapFac (%)',"OCGT_eff") = remind_CF(yr,reg,"ngt")*1e2;
        report_tech('REMIND',yr,reg,'REMIND CapFac (%)',"bio") = remind_CF(yr,reg,"biochp")*1e2;
        report_tech('REMIND',yr,reg,'REMIND CapFac (%)',"nuc") = remind_CF(yr,reg,"tnrs")*1e2;
        report_tech('REMIND',yr,reg,'REMIND CapFac (%)',"ror") = remind_CF(yr,reg,"hydro")*1e2;
        report_tech('REMIND',yr,reg,'REMIND CapFac (%)',"Solar") = remind_CF(yr,reg,"spv")*1e2;
        report_tech('REMIND',yr,reg,'REMIND CapFac (%)',"Wind_on") = remind_CF(yr,reg,"wind")*1e2;
        
***     ^^^ reporting on remind stuff
 
        report_tech('DIETER',yr,reg,'DIETER added capacities (GW)',ct) =  (N_CON.l(ct) - N_CON.lo(ct)) / 1e3 ;
        report_tech('DIETER',yr,reg,'DIETER added capacities (GW)',res) =  (P_RES.l(res) - P_RES.lo(res)) / 1e3 ;
        
        report_tech('DIETER',yr,reg,'capacities storage GW',sto) =  N_STO_P.l(sto) / 1e3 ;
        report_tech('DIETER',yr,reg,'capacities storage TWh',sto) =  N_STO_E.l(sto) /1e6;
        report_tech('DIETER',yr,reg,'conshares',ct) = sum( h, G_L.l(ct,h) ) / gross_energy_demand  * 1e2;
        report_tech('DIETER',yr,reg,'renshares',res) = sum( h, G_RES.l(res,h) - corr_fac_res(res,h))/ gross_energy_demand  * 1e2;
        report_tech('DIETER',yr,reg,'curtailment of fluct res relative',res) =  sum(h,CU.l(res,h))/ (sum(h,G_RES.l(res,h) - corr_fac_res(res,h) ) + sum(h,CU.l(res,h)) )  * 1e2;
        
*       report_tech('DIETER',yr,reg,'load-weighted price for flex demand', flexTe) = -sum(h,con1a_bal.m(h)*d2.l(h))/sum(h,d2.l(h)) ;
        
*       ===================================

        report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',ct)$(N_CON.l(ct) ne 0 ) = sum( h , G_L.l(ct,h)) / (N_CON.l(ct) * card(h)) * 1e2;
        report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',res)$(P_RES.l(res) ne 0 ) = sum( h , G_RES.l(res,h)) / (P_RES.l(res) * card(h)) * 1e2;

        report_tech('DIETER',yr,reg,'DIETER marg CapFac (%)',ct)$(N_CON.l(ct) ne 0)
          = sum( h$(G_L.l(ct,h) = N_CON.l(ct)), G_L.l(ct,h)) / (N_CON.l(ct) * card(h)) * 1e2;
        report_tech('DIETER',yr,reg,'DIETER marg CapFac (%)',res)$(P_RES.l(res) ne 0 )
          = sum( h$(G_RES.l(res,h) = P_RES.l(res)) , G_RES.l(res,h)) / (P_RES.l(res) * card(h)) * 1e2;

        report_tech('DIETER',yr,reg,'DIETER Revenue (billionUSD)',ct) = sum( h, G_L.l(ct,h)*(-con1a_bal.m(h)))/1e9;
        report_tech('DIETER',yr,reg,'DIETER Revenue (billionUSD)',res) = sum( h, G_RES.l(res,h)*(-con1a_bal.m(h)))/1e9;
        
        report_tech('DIETER',yr,reg,'DIETER Marginal Revenue (billionUSD)',ct) = sum( h$(G_L.l(ct,h) = N_CON.l(ct)), G_L.l(ct,h)*(-con1a_bal.m(h)) )/1e9;
        report_tech('DIETER',yr,reg,'DIETER Marginal Revenue (billionUSD)',res) = sum( h$(G_RES.l(res,h) = P_RES.l(res) ), G_RES.l(res,h)*(-con1a_bal.m(h)) )/1e9;
          
        report_tech('DIETER',yr,reg,'DIETER Revenue marginal plant ($/MW)',ct) = sum( h$(G_L.l(ct,h) = N_CON.l(ct) ),(-con1a_bal.m(h)) );
        report_tech('DIETER',yr,reg,'DIETER Revenue marginal plant ($/MW)',res) = sum( h$(G_RES.l(res,h) = P_RES.l(res) ), (-con1a_bal.m(h)) );
        
        report_tech('DIETER',yr,reg,'DIETER Market value ($/MWh)',ct)$(sum(h, G_L.l(ct,h) ne 0 )) = sum( h , G_L.l(ct,h)*(-con1a_bal.m(h)))/sum( h , G_L.l(ct,h));
        report_tech('DIETER',yr,reg,'DIETER Market value ($/MWh)',res)$(sum(h, G_RES.l(res,h) ne 0 )) = sum( h , G_RES.l(res,h)*(-con1a_bal.m(h)))/sum( h , G_RES.l(res,h) );

        report_tech('DIETER',yr,reg,'DIETER LCOE_avg ($/MWh)',ct) = ( c_i(ct) + cdata("c_fix_con",ct) ) *  N_CON.l(ct) / sum( h , G_L.l(ct,h))
                                                                                + con_fuelprice_reg(ct,reg) + cdata("carbon_content",ct)/cdata("eta_con",ct) * con_CO2price ;

        report_tech('DIETER',yr,reg,'DIETER LCOE_avg ($/MWh)',res) = ( c_i_res(res) + rdata("c_fix_res",res) ) * P_RES.l(res) / sum( h , G_RES.l(res,h)) ;

*       LCOE_marg $/MWh -> $/kWh
        report_tech('DIETER',yr,reg,'DIETER LCOE_marg ($/kWh)',ct) = ( ( c_i(ct) + cdata("c_fix_con",ct) ) *  N_CON.l(ct) / sum( h$(G_L.l(ct,h) = N_CON.l(ct) ) , G_L.l(ct,h))
                                                                                + con_fuelprice_reg(ct,reg) + cdata("carbon_content",ct)/cdata("eta_con",ct) * con_CO2price ) / 1e3;

        report_tech('DIETER',yr,reg,'DIETER LCOE_marg ($/kWh)',res) = ( ( c_i_res(res) + rdata("c_fix_res",res) ) * P_RES.l(res) / sum( h$(G_RES.l(res,h) = P_RES.l(res) ) , G_RES.l(res,h)) ) / 1e3;


*        report_tech('DIETER',yr,reg,'marginal generation',ct)$(sum(h, G_L.l(ct,h) ne 0 )) = sum( h$(G_L.l(ct,h) = N_CON.l(ct)) , G_L.l(ct,h)) /1e6 ;
*        report_tech('DIETER',yr,reg,'marginal generation',res)$(sum(h, G_RES.l(res,h) ne 0 )) = sum( h$(G_RES.l(res,h) = P_RES.l(res) ), G_RES.l(res,h)) /1e6 ;
*        
        report_tech('DIETER',yr,reg,'Marginal market value ($/MWh)',ct)$(sum(h, G_L.l(ct,h) ne 0 ))
            = sum( h$(G_L.l(ct,h) = N_CON.l(ct)), G_L.l(ct,h)*(-con1a_bal.m(h))) / sum( h$(G_L.l(ct,h) = N_CON.l(ct)), G_L.l(ct,h));
        report_tech('DIETER',yr,reg,'Marginal market value ($/MWh)',res)$(sum(h, G_RES.l(res,h) ne 0 ))
            = sum( h$(G_RES.l(res,h) = P_RES.l(res) ), G_RES.l(res,h)*(-con1a_bal.m(h)) )/ sum( h$(G_RES.l(res,h) = P_RES.l(res) ), G_RES.l(res,h));
        
        report_tech('DIETER',yr,reg,'Total Generation',ct) = sum( h , G_L.l(ct,h)) /1e6 ;
        report_tech('DIETER',yr,reg,'Total Renewable Generation',res) = sum( h , G_RES.l(res,h)) /1e6 ;
        report_tech('DIETER',yr,reg,'Total Renewable Curtailment',res) = sum( h , CU.l(res,h)) /1e6 ;
        report_tech('DIETER',yr,reg,'Storage out total wholesale',sto) = sum(h, report_tech_hours('DIETER',yr,reg,'generation storage',sto,h) )   /1e6 ;
        report_tech('DIETER',yr,reg,'Storage in total wholesale',sto) = sum(h, report_tech_hours('DIETER',yr,reg,'storage loading',sto,h) )   /1e6;
