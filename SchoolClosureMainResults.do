/* SchoolClosureMainResults.do       DanielPailanir        yyyy-mm-dd:2022-06-06
*----|----1----|----2----|----3----|----4----|----5----|----6----|----7----|----8
	This code is for replicate the main results of the paper:
	"Schools as a Safety-net: The Impact of School Closures and Reopenings on 
	Rates of Reporting of Violence Against Children"
	
	To store the results it is necessary to define a ROOT path and have the 
	following folder scheme:
	
	- MAIN FOLDER
		- DATA: containg the XXXX.dta
		- GRA: for save the figures
		- OUT: for save .tex files
		- source: folder that contains this .do file
		
	requirements: we use the plotplainblind scheme (ssc instal plotplainblind), 
	so if you want to change this to another scheme, just change line 30.
*/
clear all
set more off

*-------------------------------------------------------------------------------
*Global and some details
*-------------------------------------------------------------------------------
global ROOT ""

global DAT "$ROOT/data"
global GRA "$ROOT/results/graphs"
global OUT "$ROOT/results/reg"

set scheme plotplainblind
graph set window fontface "Times New Roman"

*-------------------------------------------------------------------------------
*Figure 1: Temporal Trends
*-------------------------------------------------------------------------------
use $DAT/SchoolClosure_Final , clear
gen pop_underQ=population //create population under quarantine
replace pop_underQ=0 if quarantine==0
local svar caso* VIF* quarantine pop_underQ population
collapse (sum) `svar' (mean) monday prop_schools_i, by(week)
gen prop_pop=pop_underQ/population //create proportion of population under Q
replace prop_schools_i=1 if week<=63
format prop_pop %9.1f
gen g2=VIF_2+VIF_3
gen g3=g2+VIF_1

#delimit ;
local c "if monday>21556&week<157";

twoway area VIF_3 monday `c', color(%50) 
       || rarea VIF_3 g2 monday `c', color(%50) 
       || rarea g2 g3 monday `c', color(%60)
       || line caso monday `c', lc(black) lp(solid)
ylabel(0(50)250) xlabel(#13, angle(45)) xline(21989 22144, lc(red))
ytitle("Formal reporting violence") xtitle("")
legend(order(3 "Psychological" 2 "Minor injuries" 1 "Serious injuries") 
       pos(12) col(4));
graph export "$GRA/VIFreport_byClass.pdf", replace;

twoway line casoSA monday if monday>21556 & week<154, xlabel(#13, angle(45)) 
								   xtitle("") xline(21989, lc(red)) 
								   xline(22144, lc(red)) ylabel(0(25)150)
                                   ytitle("Formal reporting Sexual Abuse");
graph export "$GRA/SAbusereport.eps", replace;

twoway line casoV monday if monday>21556 & week<154, xlabel(#13, angle(45)) 
								   xtitle("") xline(21989, lc(red)) 
								   xline(22144, lc(red)) ylabel(0(5)30)
                                   ytitle("Formal reporting Rape");
graph export "$GRA/Rapereport.eps", replace;

twoway (line quarantine monday if monday>21556, yaxis(1) 
              ytitle("Municipalities under quarantine", axis(1) size(medsmall)))
	   (line prop_pop monday if monday>21556, yaxis(2) lc(blue)
              ytitle("Proportion under quarantine", axis(2) size(medsmall) 
				                                       orientation(rvertical))),
       xlabel(#13, angle(45)) xline(21989 22144, lc(red))
	   legend(order(1 "Municipalities" 2 "Population") col(2)
	   pos(11) ring(0) colg(1pt) bm(zero) keyg(.8pt) size(small)
	   region(lcolor(gs8))) xtitle("");
graph export "$GRA/quarantine.eps", replace;

twoway (line prop_schools_i monday if monday<21990&week>0)
       (line prop_schools_i monday if monday>=21990&monday<22265&week>0, 
	                                                        lp(solid) lc(black))
	   (line prop_schools_i monday if monday>=22265&monday<=22343&week>0, 
	                                                    lp(shortdash) lc(black))
       (line prop_schools_i monday if monday>=22344&week>0, lp(solid) lc(black)), 
       ylabel(, format(%9.1f)) xlabel(#13, angle(45) format(%td)) 
       xline(21989, lc(red)) xline(22144, lc(red)) xtitle("")
       ytitle("Proportion of schools") legend(off) name(g4, replace);
graph export "$GRA/prop_school.eps", replace;
#delimit cr

*COVID cases graph
local GIT "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/"
import delimited "`GIT'/producto5/TotalesNacionales.csv", clear
keep fecha-v660

forvalues i=2/660 {
	local varlabel : var label v`i'
	scalar def f`i'="`varlabel'"
}

keep if fecha=="Casos nuevos totales" | fecha=="Fallecidos"
reshape long v@, i(fecha) j(date)
encode fecha, gen(tipo)
drop fecha
reshape wide v, i(date) j(tipo)
ren v1 NewTotalCases
ren v2 TotalDeaths

gen time=f2 if date==2
forvalues i=3/660 {
	replace time=f`i' if date==`i'
}

gen year=substr(t, 1, 4)
gen month=substr(t, 6, 2)
gen day=substr(t, 9, 2)

destring year, replace
destring month, replace
destring day, replace
gen t=mdy(month, day, year)
drop time month day year
format t %d
tsset t
gen Deaths=TotalDeaths-L.TotalDeaths

set obs 660
replace t=21550 in 660
replace NewTotalCases=0 in 660
replace Deaths=0 in 660
sort t

#delimit ;
twoway (line NewTotalCases t, yaxis(1) ytitle("Total Cases", 
                                                        axis(1) size(medsmall)))
       (line Deaths t, yaxis(2) lc(blue) ytitle("Total Deaths", 
	                            axis(2) size(medsmall) orientation(rvertical))),
       xlabel(#13, angle(45)) xline(21989, lc(red)) xline(22144, lc(red))
	   legend(order(1 "Total Cases" 2 "Total Deaths") pos(1) ring(0) col(2) 
       colg(1pt) bm(zero) keyg(.8pt) size(small) region(lcolor(gs8)))
	   xtitle("");
graph export "$GRA/COVID.eps", replace;
#delimit cr

*-------------------------------------------------------------------------------
*Table 1: Estimation
*-------------------------------------------------------------------------------
use $DAT/SchoolClosure_Final.dta, clear
xtset comuna week
local cond "if year>=2019 [aw=populationyoung]"
local opt2 "cluster(comuna) abs(comuna)"

local varr rate rateSA rateV
foreach v of local varr {
    if "`v'"=="rate" local en V
    if "`v'"=="rateSA" local en SA
    if "`v'"=="rateV" local en R

    *for outcome mean
    qui sum `v' if week<=61 & year>=2019 [aw=populationyoung]
    local `en'_mean=`r(mean)'
	
    *(1) no controls
    eststo `en'_1_d: reg `v' SchoolClose2 SchoolOpen_i `cond', cluster(comuna)
    local `en'_1_d_N=e(N)
    test _b[SchoolClose2]=_b[SchoolOpen_i]
    local `en'_1_d_p=r(p)

    eststo `en'_1_c: reg `v' SchoolClose2 prop_schools_i `cond', cluster(comuna)
    local `en'_1_c_N=e(N)
    test _b[SchoolClose2]=_b[prop_schools_i]
    local `en'_1_c_p=r(p)

    *(2) week and comuna fe
    eststo `en'_2_d: areg `v' SchoolClose2 SchoolOpen_i i.w `cond', `opt2'
    local `en'_2_d_N=e(N)
    test _b[SchoolClose2]=_b[SchoolOpen_i]
    local `en'_2_d_p=r(p)

    eststo `en'_2_c: areg `v' SchoolClose2 prop_schools_i i.w `cond', `opt2'
    local `en'_2_c_N=e(N)
    test _b[SchoolClose2]=_b[prop_schools_i]
    local `en'_2_c_p=r(p)

    *(3) week and comuna fe, quarantine control and COVID controls
    local controls quarantine caseRate pcr positivity
    eststo `en'_3_d: areg `v' SchoolClose2 SchoolOpen_i `controls' i.w `cond', `opt2'
    local `en'_3_d_N=e(N)
    test _b[SchoolClose2]=_b[SchoolOpen_i]
    local `en'_3_d_p=r(p)

    eststo `en'_3_c: areg `v' SchoolClose2 prop_schools_i `controls' i.w `cond', `opt2'
    local `en'_3_c_N=e(N)
    test _b[SchoolClose2]=_b[prop_schools_i]
    local `en'_3_c_p=r(p)
}

foreach n in V SA R {
    forval i=1/3 {
        local `n'_`i'_d_p : display %9.3f ``n'_`i'_d_p'
        local `n'_`i'_c_p : display %9.3f ``n'_`i'_c_p'
        local `n'_`i'_d_N : display %9.0fc ``n'_`i'_d_N'
        local `n'_`i'_d_N : display %9.0fc ``n'_`i'_c_N'
    }
}

local V_mean  : display %9.3f `V_mean'
local SA_mean : display %9.3f `SA_mean'
local R_mean  : display %9.3f `R_mean'

*Standardized Estimate
#delimit ;
local pa "Test of $\beta=\gamma$ (p-value) & `V_1_d_p' & `V_2_d_p' & `V_3_d_p' 
& `SA_1_d_p' & `SA_2_d_p' & `SA_3_d_p'
& `R_1_d_p'  & `R_2_d_p'  & `R_3_d_p' ";
local pb "Test of $\beta=\gamma$ (p-value) & `V_1_c_p' & `V_2_c_p' & `V_3_c_p' 
& `SA_1_c_p' & `SA_2_c_p' & `SA_3_c_p'
& `R_1_c_p'  & `R_2_c_p'  & `R_3_c_p' ";
local obs "Observations   & `V_1_d_N' & `V_2_d_N' & `V_3_d_N' 
& `SA_1_d_N' & `SA_2_d_N' & `SA_3_d_N'
& `R_1_d_N'  & `R_2_d_N'  & `R_3_d_N'";
local mn "Baseline Mean   & `V_mean' & `V_mean' & `V_mean' 
& `SA_mean' & `SA_mean' & `SA_mean' 
& `R_mean'  & `R_mean' & `R_mean'";
#delimit cr

*export tex file
file open ff  using "$OUT/panelA_extra.tex", write replace
file write ff "`pa'    \\" _n
file write ff "`obs'  \\" _n
file write ff "`mn' \\" _n
file close ff

*export tex file
file open ff  using "$OUT/panelB_extra.tex", write replace
file write ff "`pb'   \\" _n
file write ff "`obs'  \\" _n
file write ff "`mn' \\" _n
file close ff

*PANEL A
#delimit ;
esttab V_1_d V_2_d V_3_d SA_1_d SA_2_d SA_3_d R_1_d R_2_d R_3_d, 
       keep(SchoolClose2 SchoolOpen_i) b(%-9.4f) se(%-9.4f) noobs;
esttab V_1_d V_2_d V_3_d SA_1_d SA_2_d SA_3_d R_1_d R_2_d R_3_d 
       using "$OUT/panelA.tex", b(%-9.3f) se(%-9.3f) noobs
       keep(SchoolClose2 SchoolOpen_i) nonotes nogaps mlabels(, none) 
       nonumbers style(tex) starlevel ("*" 0.10 "**" 0.05 "***" 0.01) 
       fragment replace noline label;
#delimit cr

*PANEL B
#delimit ;
esttab V_1_c V_2_c V_3_c SA_1_c SA_2_c SA_3_c R_1_c R_2_c R_3_c, 
       keep(SchoolClose2 prop_schools_i) b(%-9.4f) se(%-9.4f) noobs;
esttab V_1_c V_2_c V_3_c SA_1_c SA_2_c SA_3_c R_1_c R_2_c R_3_c 
       using "$OUT/panelB.tex", b(%-9.3f) se(%-9.3f) noobs
       keep(SchoolClose2 prop_schools_i) nonotes nogaps mlabels(, none) 
       nonumbers style(tex) starlevel ("*" 0.10 "**" 0.05 "***" 0.01) 
       fragment replace noline label;
#delimit cr

*-------------------------------------------------------------------------------
* Figure 2: Heterogeneity
*-------------------------------------------------------------------------------
use $DAT/SchoolClosure_Final.dta, clear

*local options
local cond1 "if year>=2019 [aw=populationyoung]"
local cond "if year>=2019"
local opt2 "cluster(comuna) abs(comuna)"
local indvar1 "SchoolClose2 SchoolOpen_i"

*matrix for store results
foreach en in V SA R {
    matrix `en'3=J(36,5,.) //week comuna fe and COVID controls
}

local varr rate rateSA rateV
foreach v of local varr {
    if "`v'"=="rate" local en V
    if "`v'"=="rateSA" local en SA
    if "`v'"=="rateV" local en R

    *for outcome mean
    qui sum `v' if week<=61 & year>=2019 [aw=populationyoung]
    local `en'_mean=`r(mean)'
    matrix `en'3[1,5] = `r(mean)'

    *(3) week comuna fe and COVID controls
    local controls quarantine caseRate pcr positivity
    qui areg `v' `indvar1' `controls' i.w `cond1', `opt2'
	
    matrix `en'3[1,1] = _b[SchoolClose2]
    matrix `en'3[2,1] = _b[SchoolOpen_i]
    matrix `en'3[1,2] = _b[SchoolClose2]  - invttail(e(df_r),0.025)*_se[SchoolClose]
    matrix `en'3[2,2] = _b[SchoolOpen_i] - invttail(e(df_r),0.025)*_se[SchoolOpen_i]
    matrix `en'3[1,3] = _b[SchoolClose2]  + invttail(e(df_r),0.025)*_se[SchoolClose]
    matrix `en'3[2,3] = _b[SchoolOpen_i] + invttail(e(df_r),0.025)*_se[SchoolOpen_i]
    matrix `en'3[1,4] = e(N)
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
		qui sum `v'`i' if week<=61 & year>=2019 [aw=population`i']
		matrix `en'3[`j',5] = `r(mean)'

		*(3)week comuna fe and COVID controls
		local controls quarantine caseRate pcr positivity
		qui areg `v'`i' `indvar1' `controls'  i.w `cond' [aw=population`i'], `opt2'
		matrix `en'3[`j',1] = _b[SchoolClose2]
		matrix `en'3[`k',1] = _b[SchoolOpen_i]
		matrix `en'3[`j',2] = _b[SchoolClose2] - invttail(e(df_r),0.025)*_se[SchoolClose]
		matrix `en'3[`k',2] = _b[SchoolOpen_i] - invttail(e(df_r),0.025)*_se[SchoolOpen_i]
		matrix `en'3[`j',3] = _b[SchoolClose2] + invttail(e(df_r),0.025)*_se[SchoolClose]
		matrix `en'3[`k',3] = _b[SchoolOpen_i] + invttail(e(df_r),0.025)*_se[SchoolOpen_i]
		matrix `en'3[`j',4] = e(N)
		
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
		matrix `en'3[`j',5] = `r(mean)'
		
		*(3) week comuna fe and COVID controls
		local controls quarantine caseRate pcr positivity
		qui areg `depvar'`i' `indvar1' `controls' i.w `cond' [aw=population`i'], `opt2'
		matrix `en'3[`j',1] = _b[SchoolClose2]
		matrix `en'3[`k',1] = _b[SchoolOpen_i]
		matrix `en'3[`j',2] = _b[SchoolClose2]  - invttail(e(df_r),0.025)*_se[SchoolClose]
		matrix `en'3[`k',2] = _b[SchoolOpen_i] - invttail(e(df_r),0.025)*_se[SchoolOpen_i]
		matrix `en'3[`j',3] = _b[SchoolClose2]  + invttail(e(df_r),0.025)*_se[SchoolClose]
		matrix `en'3[`k',3] = _b[SchoolOpen_i] + invttail(e(df_r),0.025)*_se[SchoolOpen_i]
		matrix `en'3[`j',4] = e(N)
		
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
		matrix `en'3[`j',5] = `r(mean)'

		*(3) week comuna fe and COVID controls
		local controls quarantine caseRate pcr positivity
		qui areg `depvar' `indvar1' `controls' i.w `cond', `opt2'
		matrix `en'3[`j',1] = _b[SchoolClose2]
		matrix `en'3[`k',1] = _b[SchoolOpen_i]
		matrix `en'3[`j',2] = _b[SchoolClose2]  - invttail(e(df_r),0.025)*_se[SchoolClose]
		matrix `en'3[`k',2] = _b[SchoolOpen_i] - invttail(e(df_r),0.025)*_se[SchoolOpen_i]
		matrix `en'3[`j',3] = _b[SchoolClose2]  + invttail(e(df_r),0.025)*_se[SchoolClose]
		matrix `en'3[`k',3] = _b[SchoolOpen_i] + invttail(e(df_r),0.025)*_se[SchoolOpen_i]
		matrix `en'3[`j',4] = e(N)
		
		local j=`j'+2
		local k=`k'+2
	}
}

*----------*
*Quarantine
*----------*
*indicator for municipality under quarantine early of later
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
		matrix `en'3[`j',5] = `r(mean)'
		
		*(3) week comuna fe and COVID controls
		local controls quarantine caseRate pcr positivity
		qui areg `depvar' `indvar1' `controls' i.w `cond', `opt2'
		matrix `en'3[`j',1] = _b[SchoolClose2]
		matrix `en'3[`k',1] = _b[SchoolOpen_i]
		matrix `en'3[`j',2] = _b[SchoolClose2]  - invttail(e(df_r),0.025)*_se[SchoolClose]
		matrix `en'3[`k',2] = _b[SchoolOpen_i] - invttail(e(df_r),0.025)*_se[SchoolOpen_i]
		matrix `en'3[`j',3] = _b[SchoolClose2]  + invttail(e(df_r),0.025)*_se[SchoolClose]
		matrix `en'3[`k',3] = _b[SchoolOpen_i] + invttail(e(df_r),0.025)*_se[SchoolOpen_i]
		matrix `en'3[`j',4] = e(N)
		
		local j=`j'+2
		local k=`k'+2
	}
}

*-----------------------------
*SCHOOL CLOSURE AND REOPENING
*-----------------------------
*VIF
clear
svmat V3
gen gr=1 if V34!=.
replace gr=2 if V34==. & V31!=.
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

format V35 %9.3f
gen aux=V34/54250*100  
format aux %9.0f
gen aux3=" ("
gen aux4=")"
egen lab=concat(V34 aux3 aux aux4), format(%9.0fc)
set obs 37
replace orden1=0 in 37

#delimit ;
twoway  rarea l u orden1, hor color(gs14) fcol(gs14) fi(gs14) 
		|| rarea l u orden2, hor color(gs14) fcol(gs14) fi(gs14)
		|| pci 0 0 -36 0, lp(dash) lc(red)
		|| rcap V32 V33 orden if gr==1, hor lc(black)
		|| rcap V32 V33 orden if gr==2, hor lc(blue)
		|| scatter orden V31 if gr==1, mc(black) msym(Dh)
		|| scatter orden V31 if gr==2, mc(blue) msym(O)
		|| scatter orden x1 if V34!=., mlabel(lab) ms(none)
		|| scatter orden x2 if V34!=., mlabel(V35) ms(none)
		text(0.7   -5.8 "{bf:Observations (%)}", size(.25cm))
		text(0.7   -4.6 "{bf:Baseline rate}", size(.25cm))
		text(-2.9  -6.7 "{bf:Age Group}", size(.22cm))
		text(-13.9  -6.6 "{bf:Sex}", size(.22cm))
		text(-18.9 -6.8 "{bf:Development}", size(.22cm))
		text(-29.9 -6.7 "{bf:Quarantine}", size(.22cm))	
		text(-41   -1.5 "Change in reporting per 100,000 children", size(.36cm))	
		ylab(-1  "{bf:Overall}" -4  "   [1-6]"    -6  "   [7-10]"
			 -8  "   [11-13]"   -10  "   [14-15]" -12  "   [16-17]"
			 -15  "Female"      -17 "Male"        -20 "High"
			 -22 "Medium-High"  -24 "Medium"      -26 "Medium-Low"
			 -28 "Low"          -31 "Never"       -33 "Early Quarantine"
			 -35 "Later Quarantine", labsize(vsmall) nogrid) 
		xlabel(-4(1)1, nogrid format(%9.1f)) xtitle("  ") ytitle("")
		yticks() yline(0, ext lp(solid) lc(gs10))
		legend(order(6 "School Closure" 7 "School Reopening") pos(1) col(2)); 
graph export "$GRA/SchoolsClose_3_both.eps", replace;
#delimit cr


*SEXUAL ABUSE
clear
svmat SA3
gen gr=1 if SA34!=.
replace gr=2 if SA34==. & SA31!=.
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
	
format SA35 %9.3f
gen aux=SA34/53215*100  
format aux %9.0f
gen aux3=" ("
gen aux4=")"
egen lab=concat(SA34 aux3 aux aux4), format(%9.0fc)
set obs 37
replace orden1=0 in 37
	
#delimit ;
twoway  rarea l u orden1, hor color(gs14) fcol(gs14) fi(gs14) 
		|| rarea l u orden2, hor color(gs14) fcol(gs14) fi(gs14)
		|| pci 0 0 -36 0, lp(dash) lc(red)
		|| rcap SA32 SA33 orden if gr==1, hor lc(black)
		|| rcap SA32 SA33 orden if gr==2, hor lc(blue)
		|| scatter orden SA31 if gr==1, mc(black) msym(Dh)
		|| scatter orden SA31 if gr==2, mc(blue) msym(O)
		|| scatter orden x1 if SA34!=., mlabel(lab) ms(none)
		|| scatter orden x2 if SA34!=., mlabel(SA35) ms(none)
		text(0.7   -4.8 "{bf:Observations (%)}", size(.25cm))
		text(0.7   -3.5 "{bf:Baseline rate}", size(.25cm))
		text(-2.9  -5.7 "{bf:Age Group}", size(.22cm))
		text(-13.9  -5.6 "{bf:Sex}", size(.22cm))
		text(-18.9 -5.8 "{bf:Development}", size(.22cm))
		text(-29.9 -5.7 "{bf:Quarantine}", size(.22cm))	
		text(-41   -0.5 "Change in reporting per 100,000 children", size(.36cm))	
		ylab(-1  "{bf:Overall}" -4  "   [1-6]"    -6  "   [7-10]"
			 -8  "   [11-13]"   -10  "   [14-15]" -12  "   [16-17]"
			 -15  "Female"      -17 "Male"        -20 "High"
			 -22 "Medium-High"  -24 "Medium"      -26 "Medium-Low"
			 -28 "Low"          -31 "Never"       -33 "Early Quarantine"
			 -35 "Later Quarantine", labsize(vsmall) nogrid) 
		xlabel(-3(1)2, nogrid format(%9.1f))
		yticks() yline(0, ext lp(solid) lc(gs10))
		xtitle("  ") ytitle("") legend(order(6 "School Closure" 7 "School Reopening") pos(1) col(2)); 
graph export "$GRA/SchoolsClose_3_SA_both.eps", replace;
#delimit cr


*RAPE
clear
svmat R3
gen gr=1 if R34!=.
replace gr=2 if R34==. & R31!=.
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

format R35 %9.3f
gen aux=R34/53215*100  
format aux %9.0f
gen aux3=" ("
gen aux4=")"
egen lab=concat(R34 aux3 aux aux4), format(%9.0fc)
set obs 37
replace orden1=0 in 37
	
#delimit ;
twoway  rarea l u orden1, hor color(gs14) fcol(gs14) fi(gs14) 
		|| rarea l u orden2, hor color(gs14) fcol(gs14) fi(gs14)
		|| pci 0 0 -36 0, lp(dash) lc(red)
		|| rcap R32 R33  orden if gr==1, hor lc(black)
		|| rcap R32 R33 orden if gr==2, hor lc(blue)
		|| scatter orden R31 if gr==1, mc(black) msym(Dh)
		|| scatter orden R31 if gr==2, mc(blue) msym(O)
		|| scatter orden x1 if R34!=., mlabel(lab) ms(none)
		|| scatter orden x2 if R34!=., mlabel(R35) ms(none)
		text(0.7   -1.6 "{bf:Observations (%)}", size(.25cm))
		text(0.7   -1.2 "{bf:Baseline rate}", size(.25cm))
		text(-2.9  -1.86 "{bf:Age Group}", size(.22cm))
		text(-13.9  -1.85 "{bf:Sex}", size(.22cm))
		text(-18.9 -1.89 "{bf:Development}", size(.22cm))
		text(-29.9 -1.86 "{bf:Quarantine}", size(.22cm))	
		text(-41   -0.25 "Change in reporting per 100,000 children", size(.36cm))	
		ylab(-1  "{bf:Overall}" -4  "   [1-6]"    -6  "   [7-10]"
			 -8  "   [11-13]"   -10  "   [14-15]" -12  "   [16-17]"
			 -15  "Female"      -17 "Male"        -20 "High"
			 -22 "Medium-High"  -24 "Medium"      -26 "Medium-Low"
			 -28 "Low"          -31 "Never"       -33 "Early Quarantine"
			 -35 "Later Quarantine", labsize(vsmall) nogrid)  
		xlabel(-1(0.5)0.5, nogrid format(%9.1f))
		yticks() yline(0, ext lp(solid) lc(gs10))
		xtitle("  ") ytitle("") legend(order(6 "School Closure" 7 "School Reopening") pos(1) col(2)); 
graph export "$GRA/SchoolsClose_3_R_both.eps", replace;
#delimit cr

*-------------------------------------------------------------------------------
* Figure 3: Counterfactual
*-------------------------------------------------------------------------------
local variables rate rateSA rateV
local trends notrend lineal cuadratic
local boot=25

foreach v of local variables {
	use $DAT/SchoolClosure_Final.dta, clear
	bys comuna: gen t=_n
	gen t2=t^2
    gen pre = week<63
    gen pre_t = pre*t
    gen pre_t2 = pre*t2
	format %tdDD/NN/CCYY monday
	local boot=25
	xtset comuna week
	local opt2 "cluster(comuna) abs(comuna)"
        
	gen schoolVar1 = 1 if SchoolClose==0
	qui replace schoolVar1 = 0 if SchoolClose==1
	qui replace schoolVar1 = prop_schools_i if prop_schools_i !=0 & prop_schools_i!=.

	local c1
	local c2 schoolVar1
	keep if week<=156
 
	*keep dates for bounds
	preserve
	keep if week>0 & week<157
	keep monday
	duplicates drop
	gen dates=monday
	qui levelsof dates, local(dates)
	restore

    if "`v'"=="rate" {
	    local en V
		local td pre_t
	}
    if "`v'"=="rateSA" {
		local en SA
		local td pre_t pre_t2
	}
    if "`v'"=="rateV" {
		local en R
		local td pre_t pre_t2
	}
	
	qui keep if year>=2018
	local cond2 "[aw=populationyoung]"
			
	*Baseline
	cap drop weekpre ratehat*
	gen weekpre = w
	replace weekpre = 54 if year>=2020
	qui tab weekpre, gen(_week)
	drop _week54

	*(1) only time
	areg `v' _week* `c1' `td' `cond2', `opt2'

	drop _week*
	qui tab w, gen(_week)
		rename pre_t Xpre_t
		rename pre_t2 Xpre_t2
		rename t pre_t 
		rename t2 pre_t2
	predict ratehat1
	
	*(2) School Open
	cap drop _week*
	qui tab weekpre, gen(_week)
	drop _week54
	rename pre_t t
	rename pre_t2 t2
	rename Xpre_t pre_t
	rename Xpre_t2 pre_t2
	
	areg `v' _week* `c2' `td' `cond2', `opt2'
	drop _week*
	qui tab w, gen(_week)
	rename pre_t Xpre_t
	rename pre_t2 Xpre_t2
	rename t pre_t 
	rename t2 pre_t2
	predict ratehat2
	drop _week*
	
	preserve
	collapse (sum) `v' ratehat* (mean) monday w  [aw=populationyoung], by(week)
	mkmat monday, matrix(R0)
	mkmat `v', matrix(R1)
	mkmat ratehat1, matrix(R2)
	mkmat ratehat2, matrix(T2)
			
    qui gen EC1=(ratehat1-`v')^2 if week>=1 & week<63
    qui gen EC3=(ratehat2-`v')^2 if week>=1 & week<63
			
	forval i=1(2)3 {
		qui sum EC`i' if EC`i'!=.
		local rmse`i'=sqrt(r(mean))
		local rmse`i' : display %9.4f `rmse`i''
	}
	restore
	
	*bootstrap baseline prediction by regression
	foreach j of num  1(1)2 {
		if "`j'"=="1" local name R
		if "`j'"=="2" local name T
				
		set seed 2525
		local B = `boot'
		local i = 3
		foreach b of num 1(1)`B' {
			preserve
			di "Counterfactual `j' and boot `b'"
			bsample , cluster(comuna) idcluster(com2)
			qui tab weekpre, gen(_week)
			drop _week54
			rename pre_t t
			rename pre_t2 t2
			rename Xpre_t pre_t
			rename Xpre_t2 pre_t2
					
			qui areg `v' _week* `c`j'' `td' `cond2', cluster(com2) abs(com2)
			drop _week*
			qui tab w, gen(_week)				
			rename pre_t Xpre_t
			rename pre_t2 Xpre_t2
			rename t pre_t 
			rename t2 pre_t2
					
			cap drop ratehat`j'
			predict ratehat`j'
			collapse (sum) ratehat`j' [aw=populationyoung], by(week)
			mkmat ratehat`j', matrix(`name'`i')
			restore
			local ++i
		}
	}
	
	clear
	svmat R0
	format %d R01
	svmat R1

	local B=`boot'
	local up=`B'+2
	local names R T
	foreach n of local names {
		forvalues i=2/`up' {
			svmat `n'`i'
		}
	
		*gen lower and upper bound
		preserve
		keep if R01>=21550
		cap drop `n'11
		keep R01 `n'*
		reshape long `n', i(R01) j(a)
		drop if a==1
		reshape wide `n', i(a) j(R01)
		local i=1
		foreach vr of local dates {
			_pctile `n'`vr', p(2.5)
			scalar `n'lb`i' = r(r1)
			_pctile `n'`vr', p(97.5)
			scalar `n'ub`i'  = r(r1)
			local ++i
		}
		restore
					
		gen `n'lb = .
		gen `n'ub = .
		local j=1
			
		sort R01
		tempvar ttime
		gen `ttime'=_n
		sum `ttime' if R01>=21550
		local lmin=r(min)
		local lmax=r(max)
				
		forvalues i=`lmin'/`lmax' {
			qui replace `n'lb = `n'lb`j' in `i'
			qui replace `n'ub = `n'ub`j' in `i'
			local ++j
		}
	}
			
	*keep data
	if "`v'"=="rateSA" keep if R01<=22614
	if "`v'"=="rateV" keep if R01<=22614	
	
	gen year = year(R01)
	local i=1
	foreach n in R T {
		gen dff`i'    = (`n'21 - R11)*10240/100000 
		gen dff`i'_lb = (`n'lb - R11)*10240/100000 
		gen dff`i'_ub = (`n'ub - R11)*10240/100000 
		local ++i
	}
	
	gen     g = 1 if R01>=21984 & R01<=22145
	replace g = 2 if R01>22145 & R01<22642 

	forvalues i=1/2 {
		bys g: egen sumdff`i' = total(dff`i')
		bys g: egen sumdff`i'_lb = total(dff`i'_lb)
		bys g: egen sumdff`i'_ub = total(dff`i'_ub)
	}

	forvalues j=1/2 {
		forvalues i=1/2 {
			qui {
			sum sumdff`i' if g==`j'
			local tot`i'_`j'=r(mean)
			sum sumdff`i'_lb if g==`j'
			local tot`i'_`j'_lb=r(mean)
			sum sumdff`i'_ub if g==`j'
			local tot`i'_`j'_ub=r(mean)
			}
		}
	}

	forvalues j=1/2 {
		forvalues i=1/2 {
			local tot`i'_`j': display %5.0fc `tot`i'_`j''
			local tot`i'_`j'_lb: display %5.0fc `tot`i'_`j'_lb'
			local tot`i'_`j'_ub: display %5.0fc `tot`i'_`j'_ub'
		}
	}


	tsset R01

	local serie R
	local n=1
	foreach s of local serie {
		cap drop diffvar* area* aux* sumd* ax*
		gen diffvar1    = `s'21-R11   
		gen diffvar1_10 = 1.10*`s'21-R11
		gen diffvar1_20 = 1.20*`s'21-R11
		gen diffvar1_30 = 1.30*`s'21-R11
		gen diffvar1_40 = 1.40*`s'21-R11
				
		*series for rarea
		gen aux1=diffvar1_10-diffvar1
		gen area2=diffvar1+aux1
		gen aux2=diffvar1_20-area2
		gen area3=area2+aux2
		gen aux3=diffvar1_30-area3
		gen area4=area3+aux3
		gen aux4=diffvar1_40-area4
		gen area5=area4+aux4
				
		gen ax1 = (`s'21-R11)*10240/100000  
		gen ax2 = (1.10*`s'21-R11)*10240/100000
		gen ax3 = (1.20*`s'21-R11)*10240/100000
		gen ax4 = (1.30*`s'21-R11)*10240/100000
		gen ax5 = (1.40*`s'21-R11)*10240/100000

		egen sumd1 = total(ax1) if g!=.
		egen sumd2 = total(ax2) if g!=.
		egen sumd3 = total(ax3) if g!=.
		egen sumd4 = total(ax4) if g!=.
		egen sumd5 = total(ax5) if g!=.
	
		format %6.0fc sumd1 sumd2 sumd3 sumd4 sumd5
		forvalues i=1/5 {
			qui sum sumd`i'
			local sumd`i'=r(mean)
			local sumd`i': display %6.0fc `sumd`i''
		}
				
		if "`v'"=="rate" {
			sum diffvar1 if R01==22635
			local yyc1=r(mean)
			sum area2 if R01==22635
			local yyc2=r(mean)
			sum area3 if R01==22635
			local yyc3=r(mean)
			sum area4 if R01==22635
			local yyc4=r(mean)
			sum area5 if R01==22635
			local yyc5=r(mean)
					
			local ylabels -350(250)1550
			local cord1 1650 22018
			local cord2 1650 22196
			local ycord1 `yyc1' 22700
			local ycord2 `yyc2' 22700
			local ycord3 `yyc3' 22700
			local ycord4 `yyc4' 22700
			local ycord5 `yyc5' 22700
			
			local tr lineal
		}
		if "`v'"=="rateSA" {
			sum diffvar1 if R01==22614
			local yyc1=r(mean)
			sum area2 if R01==22614
			local yyc2=r(mean)
			sum area3 if R01==22614
			local yyc3=r(mean)
			sum area4 if R01==22614
			local yyc4=r(mean)
			sum area5 if R01==22614
			local yyc5=r(mean)
				
			local ylabels -150(200)1300
			local cord1 1200 22018
			local cord2 1200 22196
			local ycord1 `yyc1' 22670
			local ycord2 `yyc2' 22670
			local ycord3 `yyc3' 22670
			local ycord4 `yyc4' 22670
			local ycord5 `yyc5' 22670
			
			local tr cuadratic
		}
		if "`v'"=="rateV" {
			sum diffvar1 if R01==22614
			local yyc1=r(mean)
			sum area2 if R01==22614
			local yyc2=r(mean)
			sum area3 if R01==22614
			local yyc3=r(mean)
			sum area4 if R01==22614
			local yyc4=r(mean)
			sum area5 if R01==22614
			local yyc5=r(mean)
					
			local ylabels -50(25)225
			local cord1 210 22018
			local cord2 210 22196
			local ycord1 `yyc1' 22670
			local ycord2 `yyc2' 22670
			local ycord3 `yyc3' 22670
			local ycord4 `yyc4' 22670
			local ycord5 `yyc5' 22670
			
			local tr cuadratic
		}
								
		#delimit ;
		tw area diffvar1 R01, lc(black) color("253 231 37%50") || 
		   rarea diffvar1 area2 R01,  color("93 201 99%50") || 
		   rarea area2 diffvar1_20 R01, color("33 144 140%50") || 
		   rarea area3 diffvar1_30 R01, color("59 82 139%50") || 
		   rarea area4 diffvar1_40 R01, color("68 1 84%50") || 
			   if R01>=21550 & R01<22642 & R01>21984, 
			   yline(0, lc(red) lp(dash)) xline(21984 22145, lc(red) lp(solid) lw(0.4))
			   ttitle("") tlabel(#14, angle(45)) ylabel(`ylabels', angle(0)) ytitle("Difference") 
			   legend(order(1 "Observed" 2 "{&Delta}10%" 3 "{&Delta}20%"
							4 "{&Delta}30%" 5 "{&Delta}40%") pos(6) col(5))
			   text(`cord1' "School" "Close")
			   text(`cord2' "School" "Reopening") 
			   text(`ycord1' "Diff = `sumd1'") 
			   text(`ycord2' "Diff = `sumd2'") 
			   text(`ycord3' "Diff = `sumd3'") 
			   text(`ycord4' "Diff = `sumd4'") 
			   text(`ycord5' "Diff = `sumd5'") 
			   graphregion(margin(r=18 t=8));
		graph export "$GRA/diff_Count_`n'_`en'_2018_`tr'.pdf", replace;
        graph save "$GRA/diff_Count_`n'_`en'_2018_`tr'.gph", replace;
		#delimit cr
		local ++n				
	}
				
	sort R01
	*Baseline graph
	local i1 "if R01>=21550 & R01<22642"
	local i2 "if R01>=21984 & R01<22642"
				
	if "`v'"=="rate" {
		local cord1 2100 22065
		local cord2 2100 22340
		local ylabels 600(200)2200
		local tr lineal
	}
	if "`v'"=="rateSA" {
		local cord1 1500 22065
		local cord2 1500 22340
		local ylabels 400(200)1600
		local tr cuadratic
	}
	if "`v'"=="rateV" {
		local cord1 325 22065
		local cord2 325 22340
		local ylabels 100(50)350
		local tr cuadratic
	}
					
	#delimit ;
	twoway line R11 R01 `i1', lc(gs6) lp(solid)
	|| rarea Rlb Rub R01 `i2', color(gs8%40) fcol(gs8%40) fi(gs8%40)
	|| line R21 R01 `i1', lc(blue) lp(solid)
		   xline(21984 22145, lc(red) lp(solid))
		   xtitle("")
		   ylabel(`ylabels') xlabel(#13, angle(45))
		   legend(order(1 "Actual Values"
						3 "Counterfactual (time only)") pos(6) col(3))
		   text(`cord1' "Under-reporting" "(closure)" "`tot1_1'" "[`tot1_1_lb'-`tot1_1_ub']",
				size(2.5) box bc(dkgreen%30) fc(dkgreen%30) lc(dkgreen%30) bexpand bmargin(b+2))
		   text(`cord2' "Under-reporting" "(re-opening)" "`tot1_2'" "[`tot1_2_lb'-`tot1_2_ub']",
				size(2.5) box bc(dkgreen%30) fc(dkgreen%30) lc(dkgreen%30) bexpand bmargin(b+2))
		   note("RMSPE = `rmse1'");
	graph export "$GRA/C1_`en'_2018_`tr'.pdf", replace;
	graph save   "$GRA/C1_`en'_2018_`tr'.gph", replace;
	#delimit cr
				
	#delimit ;
	twoway line R11 R01 `i1', lc(gs6) lp(solid) 
	|| rarea Tlb Tub R01 `i2', color(gs8%40) fcol(gs8%40) fi(gs8%40)
	|| line T21 R01 `i1', lc(blue) lp(solid) 
		   xline(21984 22145, lc(red) lp(solid)) 
		   xtitle("") 
		   ylabel(`ylabels') xlabel(#13, angle(45))
		   legend(order(1 "Actual Values" 
						3 "Counterfactual (school controls)") pos(6) col(3))
		   text(`cord1' "Under-reporting" "(closure)" "`tot2_1'" "[`tot2_1_lb'-`tot2_1_ub']",
				size(2.5) box bc(dkgreen%30) fc(dkgreen%30) lc(dkgreen%30) bexpand bmargin(b+2))
		   text(`cord2' "Under-reporting" "(re-opening)" "`tot2_2'" "[`tot2_2_lb'-`tot2_2_ub']",
				size(2.5) box bc(dkgreen%30) fc(dkgreen%30) lc(dkgreen%30) bexpand bmargin(b+2))
		   note("RMSPE = `rmse3'");
	graph export "$GRA/C3_`en'_2018_`tr'.pdf", replace;
	graph save   "$GRA/C3_`en'_2018_`tr'.gph", replace;
	#delimit cr				
}	

