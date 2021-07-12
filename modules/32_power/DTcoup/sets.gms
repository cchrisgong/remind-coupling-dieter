Sets
gdxfile32 "gdx file names merged by mergegdx.sh" /results_DIETER_y1*results_DIETER_y16/

***DO NOT COUPLE 2005, otherwise will result in non-optimal - because REMIND has quite stringent constraint in 2005
* tDT32(ttot) "time points" /2010,2015,2020,2025,2030,2035,2040,2045,2050,2055,2060,2070,2080,2090,2100,2110,2130,2150/
tDT32(ttot) "time points that are coupled to DIETER" /2020,2025,2030,2035,2040,2045,2050,2055,2060,2070,2080,2090,2100,2110,2130,2150/

* DIETER_te_stor "technologies for storage in DIETER" /Sto1*Sto7/

DIETERte32 "technologies in DIETER" /coal,hc,lig,nuc,CCGT,OCGT_eff,bio,Wind_on,Solar,ror,all_te,elh2/

DIETERvarname32 "DIETER name" /capfac,ResPeakDem_relFac,peakDem,valuefactor,gen_share,market_value,elec_price,curt_share/

COALte32(all_te) "coal to seel tech in REMIND" /igcc,igccc,pc,pcc,pco,coalchp/

NonPeakGASte32(all_te) "gas to seel tech in REMIND" /ngcc,ngccc,gaschp/

BIOte32(all_te) "biomass to seel tech in REMIND" /biochp,bioigcc,bioigccc/

NUCte32(all_te) "nuclear to seel tech in REMIND" /tnrs,fnrs/

* DISPATCHte32_2(all_te) "dispatchable technologies" /coalchp,ngcc,ngt/

DISPATCHte32(all_te) "dispatchable technologies in REMIND" /igcc,igccc,pc,pcc,pco,coalchp,
ngcc,ngccc,gaschp,biochp,bioigcc,bioigccc,ngt,tnrs,fnrs/
