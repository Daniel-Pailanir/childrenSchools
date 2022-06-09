/* SchoolClosureGraphs.do          DanielPailanir          yyyy-mm-dd:2022-06-06
*----|----1----|----2----|----3----|----4----|----5----|----6----|----7----|----8
	This code is to replicate the Supplementary Information graphs:
	"Schools as a Safety-net: The Impact of School Closures and Reopenings on 
	Rates of Reporting of Violence Against Children"
	
	To store the results it is necessary to define a ROOT path and have the 
	following folder scheme:
	
	- MAIN FOLDER
		- DATA: containg the XXXX.dta
		- GRA: for save the figures
		- OUT: for save .tex files
		- source: folder that contains this .do file
		
	requirements: we use the plotplainblind scheme (ssc install plotplainblind), 
	so if you want to change this to another scheme, just change line 30.
*/
clear all
set more off

*-------------------------------------------------------------------------------
*Global and some details
*-------------------------------------------------------------------------------
global ROOT "/SchoolClosureViolence/replication/"

global DAT "$ROOT/data"
global GRA "$ROOT/results/graphs"

set scheme plotplainblind
graph set window fontface "Times New Roman"

*-------------------------------------------------------------------------------
*Figure S3: Extended Trends
*-------------------------------------------------------------------------------
use "$DAT/SchoolClosure_Final", clear
collapse (sum) caso* VIF* (mean) monday, by(week)

*DV, Sexual Abuse and Rape
#delimit ;
twoway line caso monday  if week<157
    || line VIF_1 monday if week<157
    || line VIF_2 monday if week<157
    || line VIF_3 monday if week<157, 
xtitle("") xlabel(#25, angle(45)) xline(21989 22144, lc(red))
ytitle("Formal reporting violence") ylabel(0(50)300)
legend(order(1 "Total VIF" 2 "Psychological" 3 "Minor injuries" 
             4 "Serious injuries") pos(12) col(4));
graph export "$GRA/VIFreportAll.eps", replace;

twoway line casoSA monday if week<154&week>-467, xtitle("") xline(21989 22144, lc(red))
xlabel(#25, angle(45)) ylabel(0(25)150) ytitle("Formal reporting sexual abuse");
graph export "$GRA/VIFreportAll_SA.eps", replace;

twoway line casoV monday if week<154&week>-467, xtitle("") xlabel(#25, angle(45))
ylabel(0(5)35) xline(21989 22144, lc(red)) ytitle("Formal reporting rape");
graph export "$GRA/VIFreportAll_R.eps", replace;
#delimit cr

*-------------------------------------------------------------------------------
*Figure S4: Temporal Trends
*-------------------------------------------------------------------------------
#delimit ;
twoway line casoSA_old monday if monday>21556 & week<154, lc(gs10) lp(dash) ||
       line casoSA monday if monday>21556 & week<154, lc(black) lp(solid) 
xtitle("") xline(21989 22144, lc(red)) ylabel(0(25)225)
ytitle("Formal reporting of Sexual Abuse") xlabel(#13, angle(45))
legend(order(1 "Observed values" 2 "Adjusted values") pos(6) col(2));
graph export "$GRA/SAbusereport_2lines.eps", replace;

twoway line casoV_old monday if monday>21556 & week<154, lc(gs10) lp(dash) ||
       line casoV monday if monday>21556 & week<154, lc(black) lp(solid) 
xtitle("") xline(21989 22144, lc(red)) ylabel(0(5)55)
ytitle("Formal reporting of Rape") xlabel(#13, angle(45))
legend(order(1 "Observed values" 2 "Adjusted values") pos(6) col(2));
graph export "$GRA/Rapereport_2lines.eps", replace;
#delimit cr

*-------------------------------------------------------------------------------
*Figure S5: Temporal Trends
*-------------------------------------------------------------------------------
use "$DAT/SchoolClosurePartes.dta", clear
collapse (sum) partes* (mean) monday, by(week)
tsset week

foreach n in V SA R {
    #delimit ;
    gen partes`n'0_n = (F3.partes`n'0+F2.partes`n'0+F1.partes`n'0+partes`n'0+
                        L1.partes`n'0+L2.partes`n'0+L3.partes`n'0)/7;
    gen partes`n'1_n = (F3.partes`n'1+F2.partes`n'1+F1.partes`n'1+partes`n'1+
                        L1.partes`n'1+L2.partes`n'1+L3.partes`n'1)/7;
    #delimit cr
}
format monday %d

#delimit ;
twoway line partesV1_n monday if monday>21556 & week<154 || 
       line partesV0_n monday if monday>21556 & week<154, 
xtitle("") xlabel(#13, angle(45)) ylabel(0(25)150)
ytitle("Formal reporting VIF") xline(21989 22144, lc(red))
legend(order(1 "Domestic" 2 "Other places") pos(12) col(2));
graph export "$GRA/partesVif_byplace.eps", replace;

twoway line partesSA1_n monday if monday>21556 & week<154 || 
       line partesSA0_n monday if monday>21556 & week<154, 
xtitle("") xlabel(#13, angle(45)) ylabel(0(25)150)
legend(order(1 "Domestic" 2 "Other places") pos(12) col(2))
ytitle("Formal reporting Sexual Abuse") xline(21989 22144, lc(red));
graph export "$GRA/partesSA_byplace.eps", replace;

twoway line partesR1_n monday if monday>21556 & week<154 || 
       line partesR0_n monday if monday>21556 & week<154, 
xtitle("") xlabel(#13, angle(45)) ylabel(0(5)40)
ytitle("Formal reporting Rape") xline(21989 22144, lc(red))
legend(order(1 "Domestic" 2 "Other places") pos(12) col(2));
graph export "$GRA/partesR_byplace.eps", replace;
#delimit cr

gen partesV=partesV0_n+partesV1_n //total partes
gen p_partesV0=partesV0_n/partesV //proportion 'Other'
gen p_partesV1=partesV1_n/partesV //proportion 'Domestic'
gen g1=p_partesV1+p_partesV0      //for rarea plot

#delimit ;
tw area p_partesV1 monday if monday>21556 & week<154, color(%50) || 
   rarea p_partesV1 g1 monday if monday>21556 & week<154, color(%50)
ylabel(0(0.1)1, format(%5.1f)) xlabel(#13, angle(45)) xtitle("")
ytitle("Proportion") xline(21989 22144, lc(red) lp(solid))
legend(order(1 "Domestic" 2 "Other places") pos(12) col(2));
graph export "$GRA/prop_partesV.pdf", replace;
#delimit cr

gen partesSA=partesSA0_n+partesSA1_n
gen p_partesSA0=partesSA0_n/partesSA
gen p_partesSA1=partesSA1_n/partesSA
gen g2=p_partesSA1+p_partesSA0

#delimit ;
tw area p_partesSA1 monday if monday>21556 & week<154, color(%50) || 
   rarea p_partesSA1 g2 monday if monday>21556 & week<154, color(%50)
ylabel(0(0.1)1, format(%5.1f)) xlabel(#13, angle(45)) xtitle("")
ytitle("Proportion") xline(21989 22144, lc(red) lp(solid))
legend(order(1 "Domestic" 2 "Other places") pos(12) col(2));
graph export "$GRA/prop_partesSA.pdf", replace;
#delimit cr

gen partesR=partesR0_n+partesR1_n
gen p_partesR0=partesR0_n/partesR
gen p_partesR1=partesR1_n/partesR
gen g3=p_partesR1+p_partesR0

#delimit ;
tw area p_partesR1 monday if monday>21556 & week<154, color(%50) || 
   rarea p_partesR1 g3 monday if monday>21556 & week<154, color(%50)
ylabel(0(0.1)1, format(%5.1f)) xlabel(#13, angle(45)) xtitle("")
ytitle("Proportion") xline(21989 22144, lc(red) lp(solid))
legend(order(1 "Domestic" 2 "Other places") pos(12) col(2));
graph export "$GRA/prop_partesR.pdf", replace;
#delimit cr

*-------------------------------------------------------------------------------
*Figure S13
*-------------------------------------------------------------------------------
use "$DAT/SchoolClosure_OPD.dta", clear
gen reporting=Courts+HealthCenters+Schools+Other
gen sum1=Schools+HealthCenters
gen sum2=Schools+HealthCenters+Courts
gen sum3=Schools+HealthCenters+Courts+Other

*percentage of total reporting
local vari Courts HealthCenters Schools Other
foreach v of local vari {
    gen `v'_p=`v'/reporting*100
}
keep if Date<=743 //keep until december 2021
sort Date

#delimit ;
twoway area Schools Date, color(%50)
       || rarea Schools sum1 Date, color(%40) 
	   || rarea sum1 sum2 Date, color(%60)
	   || rarea sum2 sum3 Date, color(%70)
	   || line reporting Date, lc(black) lp(solid)
ytitle("Formal reporting violence") xtitle("")
xlabel(708(2)743, angle(45)) xline(722 727, lc(red)) 
legend(order(4 "Others" 3 "Courts" 2 "Health Centers" 1 "Schools") pos(12) col(4));
graph export "$GRA/OPD1.pdf", replace;
#delimit cr

gen p_sum1=Schools_p+HealthCenters_p
gen p_sum2=Schools_p+HealthCenters_p+Courts_p
gen p_sum3=Schools_p+HealthCenters_p+Courts_p+Other_p

#delimit ;
twoway area Schools_p Date, color(%50)
       || rarea Schools_p p_sum1 Date, color(%40) 
	   || rarea p_sum1 p_sum2 Date, color(%60)
	   || rarea p_sum2 p_sum3 Date, color(%70)
ytitle("Percentage of formal reporting violence")
xtitle("") xlabel(708(2)743, angle(45)) xline(722 727, lc(red)) 
legend(order(4 "Others" 3 "Courts" 2 "Health Centers" 1 "Schools") pos(12) col(4));
graph export "$GRA/OPD2.pdf", replace;
#delimit cr

*-------------------------------------------------------------------------------
*Figure S14-S15
*-------------------------------------------------------------------------------
use $DAT/SchoolClosure_Final.dta, clear
local cond1 "if year>=2019 [aw=populationyoung]"
local cond "if year>=2019"
local opt2 "cluster(comuna) abs(comuna)"
local indvar1 "SchoolClose2 SchoolOpen_i"

foreach en in V SA R {
    matrix `en'1=J(36,5,.) //no control
    matrix `en'2=J(36,5,.) //week/comuna fe
}

local varr rate rateSA rateV
foreach v of local varr {
    if "`v'"=="rate" local en V
    if "`v'"=="rateSA" local en SA
    if "`v'"=="rateV" local en R
    *for outcome mean
    qui sum `v' if week<=61 & year>=2019 [aw=populationyoung]
    local `en'_mean=`r(mean)'
    matrix `en'1[1,5] = `r(mean)'
    matrix `en'2[1,5] = `r(mean)'

    *(1) no controls
    qui reg `v' `indvar1' `cond1', cluster(comuna)
    matrix `en'1[1,1] = _b[SchoolClose2]
    matrix `en'1[2,1] = _b[SchoolOpen_i]
    matrix `en'1[1,2] = _b[SchoolClose2] - invttail(e(df_r),0.025)*_se[SchoolClose]
    matrix `en'1[2,2] = _b[SchoolOpen_i] - invttail(e(df_r),0.025)*_se[SchoolOpen_i]
    matrix `en'1[1,3] = _b[SchoolClose2] + invttail(e(df_r),0.025)*_se[SchoolClose]
    matrix `en'1[2,3] = _b[SchoolOpen_i] + invttail(e(df_r),0.025)*_se[SchoolOpen_i]
    matrix `en'1[1,4] = e(N)

    *(2) week and comuna fe
    qui areg `v' `indvar1' i.w `cond1', `opt2'
    matrix `en'2[1,1] = _b[SchoolClose2]
    matrix `en'2[2,1] = _b[SchoolOpen_i]
    matrix `en'2[1,2] = _b[SchoolClose2] - invttail(e(df_r),0.025)*_se[SchoolClose]
    matrix `en'2[2,2] = _b[SchoolOpen_i] - invttail(e(df_r),0.025)*_se[SchoolOpen_i]
    matrix `en'2[1,3] = _b[SchoolClose2] + invttail(e(df_r),0.025)*_se[SchoolClose]
    matrix `en'2[2,3] = _b[SchoolOpen_i] + invttail(e(df_r),0.025)*_se[SchoolOpen_i]
    matrix `en'2[1,4] = e(N)
}

*-------*
*By Age
*-------*
local varr rate rateSA rateV
foreach v of local varr {
    if "`v'"=="rate" local en V
    if "`v'"=="rateSA" local en SA
    if "`v'"=="rateV" local en R
    local j=4
    local k=5
    forvalues i=1/5 {
        *for outcome mean
        qui sum `v'`i' if week<=61 & year>=2019 [aw=population`i']
        matrix `en'1[`j',5] = `r(mean)'
        matrix `en'2[`j',5] = `r(mean)'

        *(1) no controls
        qui reg `v'`i' `indvar1' `cond' [aw=population`i'], cluster(comuna)
        matrix `en'1[`j',1] = _b[SchoolClose2]
        matrix `en'1[`k',1] = _b[SchoolOpen_i]
        matrix `en'1[`j',2] = _b[SchoolClose2] - invttail(e(df_r),0.025)*_se[SchoolClose]
        matrix `en'1[`k',2] = _b[SchoolOpen_i] - invttail(e(df_r),0.025)*_se[SchoolOpen_i]
        matrix `en'1[`j',3] = _b[SchoolClose2] + invttail(e(df_r),0.025)*_se[SchoolClose]
        matrix `en'1[`k',3] = _b[SchoolOpen_i] + invttail(e(df_r),0.025)*_se[SchoolOpen_i]
        matrix `en'1[`j',4] = e(N)

        *(2) week and comuna fe
        qui areg `v'`i' `indvar1' i.w `cond' [aw=population`i'], `opt2'
        matrix `en'2[`j',1] = _b[SchoolClose2]
        matrix `en'2[`k',1] = _b[SchoolOpen_i]
        matrix `en'2[`j',2] = _b[SchoolClose2] - invttail(e(df_r),0.025)*_se[SchoolClose]
        matrix `en'2[`k',2] = _b[SchoolOpen_i] - invttail(e(df_r),0.025)*_se[SchoolOpen_i]
        matrix `en'2[`j',3] = _b[SchoolClose2] + invttail(e(df_r),0.025)*_se[SchoolClose]
        matrix `en'2[`k',3] = _b[SchoolOpen_i] + invttail(e(df_r),0.025)*_se[SchoolOpen_i]
        matrix `en'2[`j',4] = e(N)
		
        local j=`j'+2
        local k=`k'+2
    }
}

*-------*
*By Sex
*-------*
foreach en in V SA R {
    if "`en'"=="V"  local depvar rate
    if "`en'"=="SA" local depvar rateSA
    if "`en'"=="R"  local depvar rateV
    local j=15
    local k=16
    foreach i in Girls Boys {
        *for outcome mean
        qui sum `depvar'`i' if week<=61 & year>=2019 [aw=population`i']
        matrix `en'1[`j',5] = `r(mean)'
        matrix `en'2[`j',5] = `r(mean)'

        *(1) no controls
        qui reg `depvar'`i' `indvar1' `cond' [aw=population`i'], cluster(comuna)
        matrix `en'1[`j',1] = _b[SchoolClose2]
        matrix `en'1[`k',1] = _b[SchoolOpen_i]
        matrix `en'1[`j',2] = _b[SchoolClose2] - invttail(e(df_r),0.025)*_se[SchoolClose]
        matrix `en'1[`k',2] = _b[SchoolOpen_i] - invttail(e(df_r),0.025)*_se[SchoolOpen_i]
        matrix `en'1[`j',3] = _b[SchoolClose2] + invttail(e(df_r),0.025)*_se[SchoolClose]
        matrix `en'1[`k',3] = _b[SchoolOpen_i] + invttail(e(df_r),0.025)*_se[SchoolOpen_i]
        matrix `en'1[`j',4] = e(N)

        *(2) week and comuna fe
        qui areg `depvar'`i' `indvar1' i.w `cond' [aw=population`i'], `opt2'
        matrix `en'2[`j',1] = _b[SchoolClose2]
        matrix `en'2[`k',1] = _b[SchoolOpen_i]
        matrix `en'2[`j',2] = _b[SchoolClose2] - invttail(e(df_r),0.025)*_se[SchoolClose]
        matrix `en'2[`k',2] = _b[SchoolOpen_i] - invttail(e(df_r),0.025)*_se[SchoolOpen_i]
        matrix `en'2[`j',3] = _b[SchoolClose2] + invttail(e(df_r),0.025)*_se[SchoolClose]
        matrix `en'2[`k',3] = _b[SchoolOpen_i] + invttail(e(df_r),0.025)*_se[SchoolOpen_i]
        matrix `en'2[`j',4] = e(N)
		
        local j=`j'+2
        local k=`k'+2
    }
}

*-----------*
*Development
*-----------*
foreach en in V SA R {
    if "`en'"=="V"  local depvar rate
	if "`en'"=="SA" local depvar rateSA
    if "`en'"=="R"  local depvar rateV
    local j=20
    local k=21
    local rnk A MA M MB B
    foreach i of local rnk {
        *local options
        local cond "if year>=2019 & r_IDC=="`i'" [aw=populationyoung]"
        *for outcome mean
        qui sum `depvar' if week<=61 & r_IDC=="`i'" & year>=2019 [aw=populationyoung]
        matrix `en'1[`j',5] = `r(mean)'
        matrix `en'2[`j',5] = `r(mean)'

        *(1) no controls
        qui reg `depvar' `indvar1' `cond', cluster(comuna)
        matrix `en'1[`j',1] = _b[SchoolClose2]
        matrix `en'1[`k',1] = _b[SchoolOpen_i]
        matrix `en'1[`j',2] = _b[SchoolClose2] - invttail(e(df_r),0.025)*_se[SchoolClose]
        matrix `en'1[`k',2] = _b[SchoolOpen_i] - invttail(e(df_r),0.025)*_se[SchoolOpen_i]
        matrix `en'1[`j',3] = _b[SchoolClose2] + invttail(e(df_r),0.025)*_se[SchoolClose]
        matrix `en'1[`k',3] = _b[SchoolOpen_i] + invttail(e(df_r),0.025)*_se[SchoolOpen_i]
        matrix `en'1[`j',4] = e(N)
		
        *(2) week and comuna fe
        qui areg `depvar' `indvar1' i.w `cond', `opt2'
        matrix `en'2[`j',1] = _b[SchoolClose2]
        matrix `en'2[`k',1] = _b[SchoolOpen_i]
        matrix `en'2[`j',2] = _b[SchoolClose2] - invttail(e(df_r),0.025)*_se[SchoolClose]
        matrix `en'2[`k',2] = _b[SchoolOpen_i] - invttail(e(df_r),0.025)*_se[SchoolOpen_i]
        matrix `en'2[`j',3] = _b[SchoolClose2] + invttail(e(df_r),0.025)*_se[SchoolClose]
        matrix `en'2[`k',3] = _b[SchoolOpen_i] + invttail(e(df_r),0.025)*_se[SchoolOpen_i]
        matrix `en'2[`j',4] = e(N)
		
        local j=`j'+2
        local k=`k'+2
    }
}

*----------*
*Quarantine
*----------*
gen q=0
replace q=1 if week>=65 & week<=87
replace q=2 if week>87
egen ind=mean(quarantine) if q==0, by(comuna) 
replace ind=0 if ind==.
egen ind2=mean(quarantine) if q==1, by(comuna)
replace ind2=0 if ind2==.
egen ind3=mean(quarantine) if q==2, by(comuna) 
replace ind3=0 if ind3==.
egen c2=mean(ind2), by(comuna) 
egen c3=mean(ind3), by(comuna) 
replace c2=1 if c2!=0
replace c3=2 if c3!=0 & c2!=1
replace c3=0 if c3!=2
gen d=c2+c3
drop q ind*
la def d 0 "Never" 1 "Early" 2 "Later"
la val d d
xtset comuna week

foreach en in V SA R {
    if "`en'"=="V"  local depvar rate
	if "`en'"=="SA" local depvar rateSA
    if "`en'"=="R"  local depvar rateV
    local j=31
    local k=32
    forvalues i=0(1)2 {
        if "`i'"=="0" local m ind0
        if "`i'"=="1" local m ind1
        if "`i'"=="2" local m ind2	
        *local options
        local cond "if year>=2019 & d==`i' [aw=populationyoung]"
        *for outcome mean
        qui sum `depvar' if week<=61 & d==`i' & year>=2019 [aw=populationyoung]
        matrix `en'1[`j',5] = `r(mean)'
        matrix `en'2[`j',5] = `r(mean)'
		
        *(1) no controls
        qui reg `depvar' `indvar1' `cond', cluster(comuna)
        matrix `en'1[`j',1] = _b[SchoolClose2]
        matrix `en'1[`k',1] = _b[SchoolOpen_i]
        matrix `en'1[`j',2] = _b[SchoolClose2] - invttail(e(df_r),0.025)*_se[SchoolClose]
        matrix `en'1[`k',2] = _b[SchoolOpen_i] - invttail(e(df_r),0.025)*_se[SchoolOpen_i]
        matrix `en'1[`j',3] = _b[SchoolClose2] + invttail(e(df_r),0.025)*_se[SchoolClose]
        matrix `en'1[`k',3] = _b[SchoolOpen_i] + invttail(e(df_r),0.025)*_se[SchoolOpen_i]
        matrix `en'1[`j',4] = e(N)

        *(2) week and comuna fe
        qui areg `depvar' `indvar1' i.w `cond', `opt2'
        matrix `en'2[`j',1] = _b[SchoolClose2]
        matrix `en'2[`k',1] = _b[SchoolOpen_i]
        matrix `en'2[`j',2] = _b[SchoolClose2] - invttail(e(df_r),0.025)*_se[SchoolClose]
        matrix `en'2[`k',2] = _b[SchoolOpen_i] - invttail(e(df_r),0.025)*_se[SchoolOpen_i]
        matrix `en'2[`j',3] = _b[SchoolClose2] + invttail(e(df_r),0.025)*_se[SchoolClose]
        matrix `en'2[`k',3] = _b[SchoolOpen_i] + invttail(e(df_r),0.025)*_se[SchoolOpen_i]
        matrix `en'2[`j',4] = e(N)
		
        local j=`j'+2
        local k=`k'+2
    }
}

*-----------------------------
*SCHOOL CLOSURE AND REOPENING
*-----------------------------
local j=1
forvalues i=1/2 {
    clear
    svmat V`i'
    gen gr=1 if V`i'4!=.
    replace gr=2 if V`i'4==. & V`i'1!=.
    gen orden=-_n	
    gen l=-6.2
    gen u=1
    gen x1=-6.2
    gen x2=-5
    *color in age range
    gen orden1=-3
    replace orden1=-14 if orden==-36
    *color in development range
    gen orden2=-19
    replace orden2=-30 if orden==-36
    format V`i'5 %9.3f
    gen aux=V`i'4/54250*100  
    format aux %9.0f
    gen aux3=" ("
    gen aux4=")"
    egen lab=concat(V`i'4 aux3 aux aux4), format(%9.0fc)
    set obs 37
    replace orden1=0 in 37

    #delimit ;
    twoway  rarea l u orden1, hor color(gs14) fcol(gs14) fi(gs14) 
            || rarea l u orden2, hor color(gs14) fcol(gs14) fi(gs14)
            || pci 0 0 -36 0, lp(dash) lc(red)
            || rcap V`i'2 V`i'3 orden if gr==1, hor lc(black)
            || rcap V`i'2 V`i'3 orden if gr==2, hor lc(blue)
            || scatter orden V`i'1 if gr==1, mc(black) msym(Dh)
            || scatter orden V`i'1 if gr==2, mc(blue) msym(O)
            || scatter orden x1 if V`i'4!=., mlabel(lab) ms(none)
            || scatter orden x2 if V`i'4!=., mlabel(V`i'5) ms(none)
    text(0.7   -5.8 "{bf:Observations (%)}", size(.25cm))
    text(0.7   -4.6 "{bf:Baseline rate}", size(.25cm))
    text(-2.9  -6.7 "{bf:Age Group}", size(.22cm))
    text(-13.9  -6.6 "{bf:Sex}", size(.22cm))
    text(-18.9 -6.8 "{bf:Development}", size(.22cm))
    text(-29.9 -6.7 "{bf:Quarantine}", size(.22cm))	
    text(-41   -1.5 "Change in reporting per 100,000 children", size(.36cm))	
    ylab(-1  "{bf:Overall}"     -4  "   [1-6]" 
         -6  "   [7-10]"        -8  "   [11-13]" 
         -10  "   [14-15]"      -12  "   [16-17]"
         -15  "Female"          -17 "Male"
         -20 "High"             -22 "Medium-High"
         -24 "Medium"           -26 "Medium-Low"
         -28 "Low"              -31 "Never"
         -33 "Early Quarantine" -35 "Later Quarantine", labsize(vsmall) nogrid) 
    xlabel(-4(1)1, nogrid format(%9.1f)) yticks() yline(0, ext lp(solid) lc(gs10))
    xtitle("") ytitle("") legend(order(6 "School Closure" 7 "School Reopening") pos(1) col(2)); 
    graph export "$GRA/SchoolsClose_`j'_both.eps", replace;
    #delimit cr
    local ++j
}

*SEXUAL ABUSE
local j=1
forvalues i=1/2 {
    clear
    svmat SA`i'
    gen gr=1 if SA`i'4!=.
    replace gr=2 if SA`i'4==. & SA`i'1!=.
    gen orden=-_n	
    gen l=-5.2
    gen u=2
    gen x1=-5.2
    gen x2=-3.8
    *color in age range
    gen orden1=-3
    replace orden1=-14 if orden==-36
    *color in development range
    gen orden2=-19
    replace orden2=-30 if orden==-36
    format SA`i'5 %9.3f
    gen aux=SA`i'4/53215*100  
    format aux %9.0f
    gen aux3=" ("
    gen aux4=")"
    egen lab=concat(SA`i'4 aux3 aux aux4), format(%9.0fc)
    set obs 37
    replace orden1=0 in 37
	
    #delimit ;
    twoway  rarea l u orden1, hor color(gs14) fcol(gs14) fi(gs14) 
            || rarea l u orden2, hor color(gs14) fcol(gs14) fi(gs14)
            || pci 0 0 -36 0, lp(dash) lc(red)
            || rcap SA`i'2 SA`i'3 orden if gr==1, hor lc(black)
            || rcap SA`i'2 SA`i'3 orden if gr==2, hor lc(blue)
            || scatter orden SA`i'1 if gr==1, mc(black) msym(Dh)
            || scatter orden SA`i'1 if gr==2, mc(blue) msym(O)
            || scatter orden x1 if SA`i'4!=., mlabel(lab) ms(none)
            || scatter orden x2 if SA`i'4!=., mlabel(SA`i'5) ms(none)
    text(0.7   -4.8 "{bf:Observations (%)}", size(.25cm))
    text(0.7   -3.5 "{bf:Baseline rate}", size(.25cm))
    text(-2.9  -5.7 "{bf:Age Group}", size(.22cm))
    text(-13.9  -5.6 "{bf:Sex}", size(.22cm))
    text(-18.9 -5.8 "{bf:Development}", size(.22cm))
    text(-29.9 -5.7 "{bf:Quarantine}", size(.22cm))	
    text(-41   -0.5 "Change in reporting per 100,000 children", size(.36cm))	
    ylab(-1  "{bf:Overall}"     -4  "   [1-6]" 
         -6  "   [7-10]"        -8  "   [11-13]" 
         -10  "   [14-15]"      -12  "   [16-17]"
         -15  "Female"          -17 "Male"
         -20 "High"             -22 "Medium-High"
         -24 "Medium"           -26 "Medium-Low"
         -28 "Low"              -31 "Never"
         -33 "Early Quarantine" -35 "Later Quarantine", labsize(vsmall) nogrid) 
    xlabel(-3(1)2, nogrid format(%9.1f)) yticks() yline(0, ext lp(solid) lc(gs10))
    xtitle("") ytitle("") legend(order(6 "School Closure" 7 "School Reopening") pos(1) col(2)); 
    graph export "$GRA/SchoolsClose_`j'_SA_both.eps", replace;
    #delimit cr
    local ++j
}

*RAPE
local j=1
forvalues i=1/2 {
    clear
    svmat R`i'
    gen gr=1 if R`i'4!=.
    replace gr=2 if R`i'4==. & R`i'1!=.
    gen orden=-_n	
    gen l=-1.7
    gen u=0.5
    gen x1=-1.7
    gen x2=-1.3
    *color in age range
    gen orden1=-3
    replace orden1=-14 if orden==-36
    *color in development range
    gen orden2=-19
    replace orden2=-30 if orden==-36
    format R`i'5 %9.3f
    gen aux=R`i'4/53215*100  
    format aux %9.0f
    gen aux3=" ("
    gen aux4=")"
    egen lab=concat(R`i'4 aux3 aux aux4), format(%9.0fc)
    set obs 37
    replace orden1=0 in 37
	
    #delimit ;
    twoway  rarea l u orden1, hor color(gs14) fcol(gs14) fi(gs14) 
            || rarea l u orden2, hor color(gs14) fcol(gs14) fi(gs14)
            || pci 0 0 -36 0, lp(dash) lc(red)
            || rcap R`i'2 R`i'3  orden if gr==1, hor lc(black)
            || rcap R`i'2 R`i'3 orden if gr==2, hor lc(blue)
            || scatter orden R`i'1 if gr==1, mc(black) msym(Dh)
            || scatter orden R`i'1 if gr==2, mc(blue) msym(O)
            || scatter orden x1 if R`i'4!=., mlabel(lab) ms(none)
            || scatter orden x2 if R`i'4!=., mlabel(R`i'5) ms(none)
    text(0.7   -1.6 "{bf:Observations (%)}", size(.25cm))
    text(0.7   -1.2 "{bf:Baseline rate}", size(.25cm))
    text(-2.9  -1.86 "{bf:Age Group}", size(.22cm))
    text(-13.9  -1.85 "{bf:Sex}", size(.22cm))
    text(-18.9 -1.89 "{bf:Development}", size(.22cm))
    text(-29.9 -1.86 "{bf:Quarantine}", size(.22cm))	
    text(-41   -0.25 "Change in reporting per 100,000 children", size(.36cm))	
    ylab(-1  "{bf:Overall}"     -4  "   [1-6]" 
         -6  "   [7-10]"        -8  "   [11-13]" 
         -10  "   [14-15]"      -12  "   [16-17]"
         -15  "Female"          -17 "Male"
         -20 "High"             -22 "Medium-High"
         -24 "Medium"           -26 "Medium-Low"
         -28 "Low"              -31 "Never"
         -33 "Early Quarantine" -35 "Later Quarantine", labsize(vsmall) nogrid)  
    xlabel(-1(0.5)0.5, nogrid format(%9.1f)) yticks() yline(0, ext lp(solid) lc(gs10))
    xtitle("") ytitle("") legend(order(6 "School Closure" 7 "School Reopening") pos(1) col(2)); 
    graph export "$GRA/SchoolsClose_`j'_R_both.eps", replace;
    #delimit cr
    local ++j
}

*-------------------------------------------------------------------------------
*Figure S16-S17: Attendance
*-------------------------------------------------------------------------------
use "$DAT/SchoolClosureAttendance.dta", clear
local cond1 "& cod==2"
#delimit ;
kdensity Att1 if Att1>0 `cond1', addplot(kdensity Att2 if Att2>0 `cond1', lw(0.5)
                                 || kdensity Att3 if Att3>0 `cond1', lw(0.5)
                                 || kdensity Att4 if Att4>0 `cond1', lw(0.5) 
								 || kdensity Att5 if Att5>0 `cond1', lw(0.5))
xtitle("Attendance") ylabel(,format(%9.1f)) xlabel(,format(%9.1f)) title("")
legend(order(1 "July" 2 "August" 3 "September" 4 "October" 5 "November") pos(12) col(5));
graph export "$GRA/kden_primary.eps", replace;

local cond1 "& cod==3";
kdensity Att1 if Att1>0 `cond1', addplot(kdensity Att2 if Att2>0 `cond1', lw(0.5) 
                                 || kdensity Att3 if Att3>0 `cond1', lw(0.5)
                                 || kdensity Att4 if Att4>0 `cond1', lw(0.5) 
								 || kdensity Att5 if Att5>0 `cond1', lw(0.5))
xtitle("Attendance") ylabel(,format(%9.1f)) xlabel(,format(%9.1f)) title("")
legend(order(1 "July" 2 "August" 3 "September" 4 "October" 5 "November") pos(12) col(5));
graph export "$GRA/kden_secundary.eps", replace;

local cond1 "& cod==4";
kdensity Att1 if Att1>0 `cond1', addplot(kdensity Att2 if Att2>0 `cond1', lw(0.5) 
                                 || kdensity Att3 if Att3>0 `cond1', lw(0.5)
                                 || kdensity Att4 if Att4>0 `cond1', lw(0.5) 
								 || kdensity Att5 if Att5>0 `cond1', lw(0.5))
xtitle("Attendance") ylabel(,format(%9.1f)) xlabel(,format(%9.1f)) title("")
legend(order(1 "July" 2 "August" 3 "September" 4 "October" 5 "November") pos(12) col(5));
graph export "$GRA/kden_preschool_primary.eps", replace;

local cond1 "& cod==5";
kdensity Att1 if Att1>0 `cond1', addplot(kdensity Att2 if Att2>0 `cond1', lw(0.5) 
                                 || kdensity Att3 if Att3>0 `cond1', lw(0.5)
                                 || kdensity Att4 if Att4>0 `cond1', lw(0.5) 
								 || kdensity Att5 if Att5>0 `cond1', lw(0.5))
xtitle("Attendance") ylabel(,format(%9.1f)) xlabel(,format(%9.1f)) title("")
legend(order(1 "July" 2 "August" 3 "September" 4 "October" 5 "November") pos(12) col(5));
graph export "$GRA/kden_secundary2.eps", replace;

keep rbd Att* cod;
reshape long Att@, i(rbd cod) j(mes);

kdensity Att if Att>0 & cod==2, addplot(kdensity Att if Att>0 & cod==3, lw(0.5)
                                 || kdensity Att if Att>0 & cod==4, lw(0.5)
                                 || kdensity Att if Att>0 & cod==5, lw(0.5))
xtitle("Attendance") ylabel(,format(%9.1f)) xlabel(,format(%9.1f)) title("") note("")
legend(order(1 "Primary" "(2,068)" 2 "Secondary" "    (538)" 
             3 "Preschool-Primary" "       (3,423)" 
			 4 "{&le}Secondary" "     (2,028)") pos(12) col(4));
graph export "$GRA/kden_bylvl.eps", replace;
#delimit cr


*-------------------------------------------------------------------------------
*Figure S18
*-------------------------------------------------------------------------------




*-------------------------------------------------------------------------------
*Figure S24-S25
*-------------------------------------------------------------------------------
*DV: S24
import excel "$DAT/SchoolClosureCounterfactual", clear firstrow sheet(VIF_1)
gen orden1=-_n
gen x1=-500 //axis for rmse
gen l=-500
gen u=2600
gen orden2=-9.5 //indicate minimun rmse
replace orden2=-10.5 if orden1==-15

#delimit ;
tw rarea l u orden2, hor color(gs14) fcol(gs14) fi(gs14)
    || rcap lower1 upper1 orden1 if min==0, hor 
    || rcap lower1 upper1 orden1 if min==1, hor lc(black)
    || pci -1 0 -15.5 0, lp(dash) lc(red)
    || scatter orden1 beta1, ms(C)     
    || scatter orden1 x1 if beta1!=. , 
mlabel(RMSE) ms(none) legend(off) ytitle("") xlabel(-200(200)2600, nogrid) 
yticks() yline(-1 -6  -11, ext lp(solid) lc(gs10)) text(-0.5 -310 "RMSPE")
ylab(-1 "{bf:Quadratic}" -2 "2015" -3 "2016" -4 "2017" -5 "2018" -6 "{bf:Linear}"
     -7 "2015" -8 "2016" -9 "2017" -10 "2018" -11 "{bf:No Trend}" -12 "2015"
     -13 "2016" -14 "2017" -15 "2018", nogrid);
graph export "$GRA/VIF_1_before.eps", replace;
#delimit cr

replace x1=-2500
replace l=-2500
replace u=6500

#delimit ;
tw rarea l u orden2, hor color(gs14) fcol(gs14) fi(gs14)
    || rcap lower2 upper2 orden1 if min==0, hor 
    || rcap lower2 upper2 orden1 if min==1, hor lc(black)
    || pci -1 0 -15.5 0, lp(dash) lc(red)
    || scatter orden1 beta2, ms(C)     
    || scatter orden1 x1 if beta1!=. , 
mlabel(RMSE) ms(none) legend(off) ytitle("") xlabel(-1000(1000)6500, nogrid) 
yticks() yline(-1 -6 -11, ext lp(solid) lc(gs10)) text(-0.5 -2000 "RMSPE")
ylab(-1 "{bf:Quadratic}" -2 "2015" -3 "2016" -4 "2017" -5 "2018" -6 "{bf:Linear}"
     -7 "2015" -8 "2016" -9 "2017" -10 "2018" -11 "{bf:No Trend}" -12 "2015"
     -13 "2016" -14 "2017" -15 "2018", nogrid);
graph export "$GRA/VIF_1_after.eps", replace;
#delimit cr


*VIF: S25
import excel "$DAT/SchoolClosureCounterfactual", clear firstrow sheet(VIF_3)
gen orden1=-_n
gen x1=-500 //axis for rmse
gen l=-500 
gen u=2600
gen orden2=-14.5 //indicate minimun rmse
replace orden2=-15.5 if orden1==-15

#delimit ;
tw rarea l u orden2, hor color(gs14) fcol(gs14) fi(gs14)
    || rcap lower1 upper1 orden1 if min==0, hor 
    || rcap lower1 upper1 orden1 if min==1, hor lc(black)
    || pci -1 0 -15.5 0, lp(dash) lc(red)
    || scatter orden1 beta1, ms(C)     
    || scatter orden1 x1 if beta1!=. , 
mlabel(RMSE) ms(none) legend(off) ytitle("") xlabel(-200(200)2600, nogrid) 
yticks() yline(-1 -6 -11, ext lp(solid) lc(gs10)) text(-0.5 -310 "RMSPE")
ylab(-1 "{bf:Quadratic}" -2 "2015" -3 "2016" -4 "2017" -5 "2018" -6 "{bf:Linear}"
     -7 "2015" -8 "2016" -9 "2017" -10 "2018" -11 "{bf:No Trend}" -12 "2015"
     -13 "2016" -14 "2017" -15 "2018", nogrid );
graph export "$GRA/VIF_3_before.eps", replace;
#delimit cr

replace x1=-2500
replace l=-2500
replace u=6500

#delimit ;
tw rarea l u orden2, hor color(gs14) fcol(gs14) fi(gs14)
    || rcap lower2 upper2 orden1 if min==0, hor 
    || rcap lower2 upper2 orden1 if min==1, hor lc(black)
    || pci -1 0 -15.5 0, lp(dash) lc(red)
    || scatter orden1 beta2, ms(C)     
    || scatter orden1 x1 if beta1!=. , 
mlabel(RMSE) ms(none) legend(off) ytitle("") xlabel(-1000(1000)6500, nogrid) 
yticks() yline(-1 -6 -11, ext lp(solid) lc(gs10)) text(-0.5 -2000 "RMSPE")
ylab(-1 "{bf:Quadratic}" -2 "2015" -3 "2016" -4 "2017" -5 "2018" -6 "{bf:Linear}"
     -7 "2015" -8 "2016" -9 "2017" -10 "2018" -11 "{bf:No Trend}" -12 "2015"
     -13 "2016" -14 "2017" -15 "2018", nogrid );
graph export "$GRA/VIF_3_after.eps", replace;
#delimit cr

*SA: S24
import excel "$DAT/SchoolClosureCounterfactual", clear firstrow sheet(SA_1)
gen orden1=-_n
gen x1=-500 //axis for rmse
gen l=-500
gen u=1600
gen orden2=-4.5 //indicate minimun rmse
replace orden2=-5.5 if orden1==-15

#delimit ;
tw rarea l u orden2, hor color(gs14) fcol(gs14) fi(gs14)
|| rcap lower1 upper1 orden1 if min==0, hor 
|| rcap lower1 upper1 orden1 if min==1, hor lc(black)
|| pci -1 0 -15.5 0, lp(dash) lc(red)
|| scatter orden1 beta1, ms(C)     
|| scatter orden1 x1 if beta1!=. , 
mlabel(RMSE) ms(none) legend(off) ytitle("") xlabel(-200(200)1600, nogrid) 
yticks() yline(-1 -6 -11, ext lp(solid) lc(gs10)) text(-0.5 -380 "RMSPE")
ylab(-1 "{bf:Quadratic}" -2 "2015" -3 "2016" -4 "2017" -5 "2018" -6 "{bf:Linear}"
     -7 "2015" -8 "2016" -9 "2017" -10 "2018" -11 "{bf:No Trend}" -12 "2015"
     -13 "2016" -14 "2017" -15 "2018", nogrid );
graph export "$GRA/SA_1_before.eps", replace;
#delimit cr

replace x1=-2500
replace l=-2500
replace u=4000

#delimit ;
tw rarea l u orden2, hor color(gs14) fcol(gs14) fi(gs14)
    || rcap lower2 upper2 orden1 if min==0, hor 
    || rcap lower2 upper2 orden1 if min==1, hor lc(black)
    || pci -1 0 -15.5 0, lp(dash) lc(red)
    || scatter orden1 beta2, ms(C)     
    || scatter orden1 x1 if beta1!=. , 
mlabel(RMSE) ms(none) legend(off) ytitle("") xlabel(-1500(500)4000, nogrid) 
yticks() yline(-1 -6 -11, ext lp(solid) lc(gs10)) text(-0.5 -2200 "RMSPE")
ylab(-1 "{bf:Quadratic}" -2 "2015" -3 "2016" -4 "2017" -5 "2018" -6 "{bf:Linear}"
     -7 "2015" -8 "2016" -9 "2017" -10 "2018" -11 "{bf:No Trend}" -12 "2015"
     -13 "2016" -14 "2017" -15 "2018", nogrid );
graph export "$GRA/SA_1_after.eps", replace;
#delimit cr

*SA: S25
import excel "$DAT/SchoolClosureCounterfactual", clear firstrow sheet(SA_3)
gen orden1=-_n
gen x1=-500 //axis for rmse
gen l=-500 
gen u=1600
gen orden2=-4.5 //indicate minimun rmse
replace orden2=-5.5 if orden1==-15

#delimit ;
tw rarea l u orden2, hor color(gs14) fcol(gs14) fi(gs14)
    || rcap lower1 upper1 orden1 if min==0, hor 
    || rcap lower1 upper1 orden1 if min==1, hor lc(black)
    || pci -1 0 -15.5 0, lp(dash) lc(red)
    || scatter orden1 beta1, ms(C)     
    || scatter orden1 x1 if beta1!=. , 
mlabel(RMSE) ms(none) legend(off) ytitle("") xlabel(-200(200)1600, nogrid) 
yticks() yline(-1 -6 -11, ext lp(solid) lc(gs10)) text(-0.5 -380 "RMSPE")
ylab(-1 "{bf:Quadratic}" -2 "2015" -3 "2016" -4 "2017" -5 "2018" -6 "{bf:Linear}"
     -7 "2015" -8 "2016" -9 "2017" -10 "2018" -11 "{bf:No Trend}" -12 "2015"
     -13 "2016" -14 "2017" -15 "2018", nogrid );
graph export "$GRA/SA_3_before.eps", replace;
#delimit cr

replace x1=-2500
replace l=-2500
replace u=4000

#delimit ;
tw rarea l u orden2, hor color(gs14) fcol(gs14) fi(gs14)
    || rcap lower2 upper2 orden1 if min==0, hor 
    || rcap lower2 upper2 orden1 if min==1, hor lc(black)
    || pci -1 0 -15.5 0, lp(dash) lc(red)
    || scatter orden1 beta2, ms(C)     
    || scatter orden1 x1 if beta1!=. , 
mlabel(RMSE) ms(none) legend(off) ytitle("") xlabel(-1500(500)4000, nogrid) 
yticks() yline(-1 -6 -11, ext lp(solid) lc(gs10)) text(-0.5 -2000 "RMSPE")
ylab(-1 "{bf:Quadratic}" -2 "2015" -3 "2016" -4 "2017" -5 "2018" -6 "{bf:Linear}"
     -7 "2015" -8 "2016" -9 "2017" -10 "2018" -11 "{bf:No Trend}" -12 "2015"
     -13 "2016" -14 "2017" -15 "2018", nogrid );
graph export "$GRA/SA_3_after.eps", replace;
#delimit cr

*R: S24
import excel "$DAT/SchoolClosureCounterfactual", clear firstrow sheet(R_1)
gen orden1=-_n
gen x1=-200 //axis for rmse
gen l=-200
gen u=250
gen orden2=-4.5 //indicate minimun rmse
replace orden2=-5.5 if orden1==-15

#delimit ;
tw rarea l u orden2, hor color(gs14) fcol(gs14) fi(gs14)
    || rcap lower1 upper1 orden1 if min==0, hor 
    || rcap lower1 upper1 orden1 if min==1, hor lc(black)
    || pci -1 0 -15.5 0, lp(dash) lc(red)
    || scatter orden1 beta1, ms(C)     
    || scatter orden1 x1 if beta1!=. , 
mlabel(RMSE) ms(none) legend(off) ytitle("") xlabel(-150(50)250, nogrid) 
yticks() yline(-1 -6 -11, ext lp(solid) lc(gs10)) text(-0.5 -180 "RMSPE")
ylab(-1 "{bf:Quadratic}" -2 "2015" -3 "2016" -4 "2017" -5 "2018" -6 "{bf:Linear}"
     -7 "2015" -8 "2016" -9 "2017" -10 "2018" -11 "{bf:No Trend}" -12 "2015"
     -13 "2016" -14 "2017" -15 "2018", nogrid );
graph export "$GRA/R_1_before.eps", replace;
#delimit cr

replace x1=-550
replace l=-550
replace u=800

#delimit ;
tw rarea l u orden2, hor color(gs14) fcol(gs14) fi(gs14)
    || rcap lower2 upper2 orden1 if min==0, hor 
    || rcap lower2 upper2 orden1 if min==1, hor lc(black)
    || pci -1 0 -15.5 0, lp(dash) lc(red)
    || scatter orden1 beta2, ms(C)     
    || scatter orden1 x1 if beta1!=. , 
mlabel(RMSE) ms(none) legend(off) ytitle("") xlabel(-400(100)800, nogrid) 
yticks() yline(-1 -6 -11, ext lp(solid) lc(gs10)) text(-0.5 -480 "RMSPE")
ylab(-1 "{bf:Quadratic}" -2 "2015" -3 "2016" -4 "2017" -5 "2018" -6 "{bf:Linear}"
     -7 "2015" -8 "2016" -9 "2017" -10 "2018" -11 "{bf:No Trend}" -12 "2015"
     -13 "2016" -14 "2017" -15 "2018", nogrid );
graph export "$GRA/R_1_after.eps", replace;
#delimit cr

*R: S25
import excel "$DAT/SchoolClosureCounterfactual", clear firstrow sheet(R_3)
gen orden1=-_n
gen x1=-200 //axis for rmse
gen l=-200
gen u=250
gen orden2=-4.5 //indicate minimun rmse
replace orden2=-5.5 if orden1==-15

#delimit ;
tw rarea l u orden2, hor color(gs14) fcol(gs14) fi(gs14)
    || rcap lower1 upper1 orden1 if min==0, hor 
    || rcap lower1 upper1 orden1 if min==1, hor lc(black)
    || pci -1 0 -15.5 0, lp(dash) lc(red)
    || scatter orden1 beta1, ms(C)     
    || scatter orden1 x1 if beta1!=. , 
mlabel(RMSE) ms(none) legend(off) ytitle("") xlabel(-150(50)250, nogrid) 
yticks() yline(-1 -6 -11, ext lp(solid) lc(gs10)) text(-0.5 -180 "RMSPE")
ylab(-1 "{bf:Quadratic}" -2 "2015" -3 "2016" -4 "2017" -5 "2018" -6 "{bf:Linear}"
     -7 "2015" -8 "2016" -9 "2017" -10 "2018" -11 "{bf:No Trend}" -12 "2015"
     -13 "2016" -14 "2017" -15 "2018", nogrid );
graph export "$GRA/R_3_before.eps", replace;
#delimit cr

replace x1=-550
replace l=-550
replace u=800

#delimit ;
tw rarea l u orden2, hor color(gs14) fcol(gs14) fi(gs14)
    || rcap lower2 upper2 orden1 if min==0, hor 
    || rcap lower2 upper2 orden1 if min==1, hor lc(black)
    || pci -1 0 -15.5 0, lp(dash) lc(red)
    || scatter orden1 beta2, ms(C)     
    || scatter orden1 x1 if beta1!=. , 
mlabel(RMSE) ms(none) legend(off) ytitle("") xlabel(-400(100)800, nogrid) 
yticks() yline(-1 -6 -11, ext lp(solid) lc(gs10)) text(-0.5 -480 "RMSPE")
ylab(-1 "{bf:Quadratic}" -2 "2015" -3 "2016" -4 "2017" -5 "2018" -6 "{bf:Linear}"
     -7 "2015" -8 "2016" -9 "2017" -10 "2018" -11 "{bf:No Trend}" -12 "2015"
     -13 "2016" -14 "2017" -15 "2018", nogrid );
graph export "$GRA/R_3_after.eps", replace;
#delimit cr


