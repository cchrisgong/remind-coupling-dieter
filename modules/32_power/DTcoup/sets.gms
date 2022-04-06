Sets
gdxfile32 "gdx file names merged by mergegdx.sh" /results_DIETER_y1*results_DIETER_y16/

***DO NOT COUPLE 2005, otherwise will result in non-optimal - because REMIND has quite stringent constraint in 2005
* tDT32(ttot) "time points" /2010,2015,2020,2025,2030,2035,2040,2045,2050,2055,2060,2070,2080,2090,2100,2110,2130,2150/
tDT32(ttot) "time points that are coupled to DIETER" /2020,2025,2030,2035,2040,2045,2050,2055,2060,2070,2080,2090,2100,2110,2130,2150/
tDT32s(ttot) "shorter list of time points that are coupled to DIETER" /2025,2030,2035,2040,2045,2050,2055,2060,2070,2080,2090,2100,2110,2130,2150/
tDT32s2(ttot) "second shorter list of time points that are coupled to DIETER" /2020, 2025,2030,2035,2040,2045,2050,2055,2060,2070,2080,2090,2100,2110,2130/

* DIETER_te_stor "technologies for storage in DIETER" /Sto1*Sto7/

DIETERte32 "technologies in DIETER"
/coal
nuc
CCGT
OCGT_eff
bio
Wind_on
$IFTHEN.WindOff %cm_wind_offshore% == "1"
Wind_off
$ENDIF.WindOff
Solar
ror
all_te
elh2
el
vregrid
/

techUpscaledNames32 "Upscaled names for checking convergence"
/coal
nuclear
ccgt
ocgt
biomass
windon
$IFTHEN.WindOff %cm_wind_offshore% == "1"
windoff
$ENDIF.WindOff
solar
hydro/

techUpscaledConv32 "tech for which inter-iteration convergence is checked"
/coal
ccgt
ocgt
windon
$IFTHEN.WindOff %cm_wind_offshore% == "1"
windoff
$ENDIF.WindOff
solar/

DIETERvarname32 "DIETER name" /capfac,ResPeakDem_relFac,value_factor,gen_share,
market_value,market_price,elec_price, elec_price_wscar,curt_share,curt_ratio,
dem_share,usable_generation,model_status,peak_gen_bin/

$ifthen.chp %cm_DT_CHP_coup% == "on"
COALte32(all_te) "coal to seel tech in REMIND" /igcc,igccc,pc,pcc,pco,coalchp/
NonPeakGASte32(all_te) "gas to seel tech in REMIND" /ngcc,ngccc,gaschp/
BIOte32(all_te) "biomass to seel tech in REMIND" /biochp,bioigcc,bioigccc/
DISPATCHte32(all_te) "dispatchable technologies in REMIND" /igcc,igccc,pc,pcc,pco,coalchp,ngcc,ngccc,gaschp,biochp,bioigcc,bioigccc,ngt,tnrs,fnrs/
REMINDte4DT32(all_te) "REMIND tech for DIETER"
/
spv
wind
*$IFTHEN.WindOff %cm_wind_offshore% == "1"
windoff
*$ENDIF.WindOff
hydro
elh2
coalchp
gaschp
biochp
ngcc
ngccc
ngt
bioigcc
bioigccc
igcc
igccc
pc
pcc
pco
storspv
storwind
$IFTHEN.WindOff %cm_wind_offshore% == "1"
storwindoff
$ENDIF.WindOff
storcsp
tnrs
fnrs
gridwind
/
CFcoupSuppte32(all_te) "technologies in REMIND which have prefactors to capacity factor" /igcc,igccc,pc,pcc,pco,coalchp,
ngcc,ngccc,gaschp,biochp,bioigcc,bioigccc,ngt,tnrs,fnrs/
$endif.chp

$ifthen.chpoff %cm_DT_CHP_coup% == "off"
COALte32(all_te) "coal to seel tech in REMIND" /igcc,igccc,pc,pcc,pco/
NonPeakGASte32(all_te) "gas to seel tech in REMIND" /ngcc,ngccc/
BIOte32(all_te) "biomass to seel tech in REMIND" /bioigcc,bioigccc/
DISPATCHte32(all_te) "dispatchable technologies in REMIND" /igcc,igccc,pc,pcc,pco,ngcc,ngccc,bioigcc,bioigccc,ngt,tnrs,fnrs/

STOte32(all_te) "storage technologies in REMIND"
/storspv
storwind
$IFTHEN.WindOff %cm_wind_offshore% == "1"
storwindoff
$ENDIF.WindOff
storcsp
/

REMINDte4DT32(all_te) "REMIND tech for DIETER"
/
spv
wind
*$IFTHEN.WindOff %cm_wind_offshore% == "1"
windoff
*$ENDIF.WindOff
hydro
elh2
ngcc
ngccc
ngt
bioigcc
bioigccc
igcc
igccc
pc
pcc
pco
storspv
storwind
$IFTHEN.WindOff %cm_wind_offshore% == "1"
storwindoff
$ENDIF.WindOff
storcsp
tnrs
fnrs
gridwind
/
CFcoupSuppte32(all_te) "technologies in REMIND which have prefactors to capacity factor" /igcc,igccc,pc,pcc,pco,ngcc,ngccc,bioigcc,bioigccc,ngt,tnrs,fnrs/
$endif.chpoff

NUCte32(all_te) "nuclear to seel tech in REMIND" /tnrs,fnrs/
CFcoupDemte32(all_te) "demand-side technologies in REMIND which have prefactors to capacity factor" /elh2/
Aggte32 "aggregated technologies" /Bio,coal,CCGT,nuc/
