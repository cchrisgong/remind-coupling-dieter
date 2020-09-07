Sets
gdxfile_32 "gdx file names that were merged" /results_DIETER_y1*results_DIETER_y15/

***DO NOT COUPLE 2005, otherwise will result in non-optimal - because REMIND has quite stringent constraint in 2005
t_DT_32(tall) "time points" /2010,2015,2020,2025,2030,2035,2040,2045,2050,2055,2060,2070,2080,2090,2100/

* DIETER_te_stor "technologies for storage in DIETER" /Sto1*Sto7/

DIETER_te_32 "technologies in DIETER" /coal,hc,lig,nuc,CCGT,OCGT_eff,bio,Wind_on,Solar,ror,all_te,elh2/

DIETERvarname_32 "DIETER name" /capfac,peakDem_relFac,peakDem,mult_markup/

COALte_32(all_te) "coal to seel tech in REMIND" /igcc,igccc,pc,pcc,pco,coalchp/

NonPeakGASte_32(all_te) "gas to seel tech in REMIND" /ngcc,ngccc,gaschp/

BIOte_32(all_te) "biomass to seel tech in REMIND" /biochp,bioigcc,bioigccc/

NUCte_32(all_te) "nuclear to seel tech in REMIND" /tnrs,fnrs/

DISPATCHte_32(all_te) "dispatchable technologies" /hydro,igcc,igccc,pc,pcc,pco,coalchp,
                                  ngcc,ngccc,gaschp,biochp,bioigcc,bioigccc,ngt,tnrs,fnrs/
