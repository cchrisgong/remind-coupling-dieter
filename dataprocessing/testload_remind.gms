Set
*gdxfile /results_DIETER_y1, results_DIETER_y2, results_DIETER_y3, results_DIETER_y4, results_DIETER_y5,results_DIETER_y6, results_DIETER_y7,
*results_DIETER_y8, results_DIETER_y9, results_DIETER_y10, results_DIETER_y11, results_DIETER_y12/
gdxfile /results_DIETER_y1*results_DIETER_y12/

yr /2025, 2030, 2035, 2040, 2045/								                      
reg 	 /DEU/
te 	/Solar, Wind_on, Wind_off, all_te, ror, nuc, lig, hc, CCGT, OCGT_eff, OCGT_ineff, bio/
char      /capacity, capfac, generation, peakDem/
;

Parameter report4RM(gdxfile, yr, reg, te, char);
Parameter dummy;

Execute_Loadpoint 'results_DIETER' report4RM;

display report4RM;

dummy = report4RM("results_DIETER_y1", "2025","DEU","all_te","peakDem");

display dummy;