*** |  (C) 2006-2020 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de
*** SOF ./modules/24_trade/se_trade/datainput.gms


pm_Xport0("2005",regi,peFos) = 0;

*ML* Reintroduction of trade cost for composite good (based on export/import value difference for non-energy goods in GTAP6)
pm_tradecostgood(regi)        = 0.03;

*** load data on transportation costs of imports
parameter pm_costsPEtradeMp(all_regi,all_enty)                   "PE tradecosts (energy losses on import)"
/
$ondelim
$include "./modules/24_trade/se_trade/input/pm_costsPEtradeMp.cs4r"
$offdelim
/
;


table pm_costsTradePeFinancial(all_regi,char,all_enty)          "PE tradecosts (financial costs on import, export and use)"
$ondelim
$include "./modules/24_trade/se_trade/input/pm_costsTradePeFinancial.cs3r"
$offdelim
;
pm_costsTradePeFinancial(regi,"XportElasticity", tradePe(enty)) = 100;
pm_costsTradePeFinancial(regi, "tradeFloor", tradePe(enty))     = 0.0125;

*DK* Only for SSP cases other than SSP2: use default trade costs
if(cm_tradecost_bio = 1,
pm_costsTradePeFinancial(regi,"Xport", "pebiolc") = pm_costsTradePeFinancial(regi,"Xport", "pebiolc")/2;
);

pm_costsTradePeFinancial(regi,"Xport", "pegas") = cm_trdcst * pm_costsTradePeFinancial(regi,"Xport", "pegas") ;
pm_costsTradePeFinancial(regi,"XportElasticity","pegas") = cm_trdadj *pm_costsTradePeFinancial(regi,"XportElasticity","pegas");


*** Secondary Energy exogenously defined trade scenarios
$ifthen.seTradeScenario "%cm_seTradeScenario%" == "DEU_Low_H2"
*Low Hydrogen trade in Germany only (all imports from MEA)
  pm_seTradeCapacity("2040","MEA","DEU","seh2") = 10/8760; !! TWh to TWa
  pm_seTradeCapacity("2045","MEA","DEU","seh2") = 30/8760;
  pm_seTradeCapacity("2050","MEA","DEU","seh2") = 100/8760;
  pm_seTradeCapacity(t,"MEA","DEU","seh2")$(t.val ge 2055) = 150/8760;
$elseif.seTradeScenario "%cm_seTradeScenario%" == "DEU_High_H2"
  pm_seTradeCapacity("2030","MEA","DEU","seh2") = 30/8760;
  pm_seTradeCapacity("2035","MEA","DEU","seh2") = 100/8760;
  pm_seTradeCapacity("2040","MEA","DEU","seh2") = 200/8760;
  pm_seTradeCapacity("2045","MEA","DEU","seh2") = 400/8760;
  pm_seTradeCapacity("2050","MEA","DEU","seh2") = 500/8760;
  pm_seTradeCapacity(t,"MEA","DEU","seh2")$(t.val ge 2055) = 600/8760;
$endif.seTradeScenario


*** EOF ./modules/24_trade/se_trade/datainput.gms
