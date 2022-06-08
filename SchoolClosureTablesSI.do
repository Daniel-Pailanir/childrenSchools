/* SchoolClosureMainResults.do       DanielPailanir        yyyy-mm-dd:2022-06-06
*----|----1----|----2----|----3----|----4----|----5----|----6----|----7----|----8
	This code is for replicate the tables for Supplementary Information:
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
global ROOT "C:/Users/danie/OneDrive/Escritorio/Research/SchoolClosureViolence/replication/"

global DAT "$ROOT/data"
global OUT "$ROOT/results/reg"

*-------------------------------------------------------------------------------
*Table S1: Summary Statistics
*-------------------------------------------------------------------------------
use $DAT/SchoolClosure_Final.dta, clear
la var rate     "Intra-family Violence"
la var rateVIF3 " \ \ Physical Violence (serious)"
la var rateVIF2 " \ \ Physical Violence (moderate)"
la var rateVIF1 " \ \ Psychological Violence"
la var rateSA   "Sexual Abuse"
la var rateV    "Rape"

local PA rate rateVIF3 rateVIF2 rateVIF1 rateSA rateV
estpost sum `PA' if year>=2019
#delimit ;
estout using "$OUT/summaryPA.tex", replace label  mlabels(,none) collabels(,none)
cells("count() mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2))") style(tex);
#delimit cr

la var SchoolClose2   "School Closure "
la var SchoolOpen_i   "School Reopening (Binary)"
la var prop_schools_i "School Reopening (Continuous)"
la var Attendance1    "Attendance (1 day)"
la var Attendance2    "Attendance (1-5 days)"
la var Attendance3    "Attendance (6-10 days)"
la var Attendance4    "Attendance (10+ days)"

local PB SchoolClose2 SchoolOpen_i prop_schools_i Attendance1 Attendance2 Attendance3 Attendance4
estpost sum `PB' if year>=2019
#delimit ;
estout using "$OUT/summaryPB.tex", replace label mlabels(,none) collabels(,none)
cells("count() mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2))") style(tex);
#delimit cr

la var quarantine "Quarantine"
la var caseRate   "COVID-19 Cases per 1,000"
la var pcr        "PCR Testing per 1,000"
la var positivity "PCR Test Positivity"
la var populationyoung "Popultaion between 5-18 years"

local PC quarantine caseRate pcr positivity populationyoung
estpost sum `PC' if year>=2019
#delimit ;
estout using "$OUT/summaryPC.tex", replace label mlabels(,none) collabels(,none)
cells("count() mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2))") style(tex);
#delimit cr

*-------------------------------------------------------------------------------
*Table S2-S4: Attendance
*-------------------------------------------------------------------------------
xtset comuna week
local cond "if year>=2019 [aw=populationyoung]"
local opt2 "cluster(comuna) abs(comuna)"
local indvar1 SchoolClose2 SchoolOpen_i
local indvar2 SchoolClose2 prop_schools_i

local varr rate rateSA rateV
foreach v of local varr {
	if "`v'"=="rate" local en V
	if "`v'"=="rateSA" local en SA
	if "`v'"=="rateV" local en R 
    *for outcome mean
    qui sum `v' if week<=61 & year>=2019 [aw=populationyoung]
    local `en'_mean=`r(mean)'
    local `en'_mean : display %9.3f ``en'_mean'

		qui sum Attendance1 if SchoolOpen_i>0, d
	    local p25=r(p25)
		local p50=r(p50)
		local p75=r(p75)
		local p90=r(p90)

		xtset comuna week
		local cond2 "if year>=2019 & Attendance1!=. [aw=populationyoung]"
		local opt2 "cluster(comuna) abs(comuna)"
			
		*(1) no controls
		eststo `en'_1_att1_d: reg `v' `indvar1' `cond2', cluster(comuna)
		local `en'_1_att1_d_N=e(N)
		eststo `en'_1_att1_c: reg `v' `indvar2' `cond2', cluster(comuna)
		local `en'_1_att1_c_N=e(N)
		*add attendance interaction
		eststo `en'_1_att2_d: reg `v' `indvar1' OpenAttendance1 `cond', cluster(comuna)
		local Attp25_1d = _b[SchoolOpen_i]+_b[OpenAttendance1]*`p25'
		local Attp50_1d = _b[SchoolOpen_i]+_b[OpenAttendance1]*`p50'
		local Attp75_1d = _b[SchoolOpen_i]+_b[OpenAttendance1]*`p75'
		local Attp90_1d = _b[SchoolOpen_i]+_b[OpenAttendance1]*`p90'
		local `en'_1_att2_d_N=e(N)

		eststo `en'_1_att2_c: reg `v' `indvar2' propOpenAttendance1 `cond', cluster(comuna)
		local Attp25_1c = _b[prop_schools_i]+_b[propOpenAttendance1]*`p25'
		local Attp50_1c = _b[prop_schools_i]+_b[propOpenAttendance1]*`p50'
		local Attp75_1c = _b[prop_schools_i]+_b[propOpenAttendance1]*`p75'
		local Attp90_1c = _b[prop_schools_i]+_b[propOpenAttendance1]*`p90'
		local `en'_1_att2_c_N=e(N)

		*(2) week and comuna fe
		eststo `en'_2_att1_d: areg `v' `indvar1' i.w `cond2', `opt2'
		local `en'_2_att1_d_N=e(N)
		eststo `en'_2_att1_c: areg `v' `indvar2' i.w `cond2', `opt2'
		local `en'_2_att1_c_N=e(N)
		*add attendance interaction
		eststo `en'_2_att2_d: reg `v' `indvar1' OpenAttendance1 i.w  `cond', `opt2'
		local Attp25_2d = _b[SchoolOpen_i]+_b[OpenAttendance1]*`p25'
		local Attp50_2d = _b[SchoolOpen_i]+_b[OpenAttendance1]*`p50'
		local Attp75_2d = _b[SchoolOpen_i]+_b[OpenAttendance1]*`p75'
		local Attp90_2d = _b[SchoolOpen_i]+_b[OpenAttendance1]*`p90'
		local `en'_2_att2_d_N=e(N)

		eststo `en'_2_att2_c: reg `v' `indvar2' propOpenAttendance1 i.w  `cond', `opt2'
		local Attp25_2c = _b[prop_schools_i]+_b[propOpenAttendance1]*`p25'
		local Attp50_2c = _b[prop_schools_i]+_b[propOpenAttendance1]*`p50'
		local Attp75_2c = _b[prop_schools_i]+_b[propOpenAttendance1]*`p75'
		local Attp90_2c = _b[prop_schools_i]+_b[propOpenAttendance1]*`p90'
		local `en'_2_att2_c_N=e(N)

		*(3) week and comuna fe, quarantine control and COVID controls
		local controls quarantine caseRate pcr positivity
		eststo `en'_3_att1_d: areg `v' `indvar1' `controls' i.w `cond2', `opt2'
		local `en'_3_att1_d_N=e(N)
		eststo `en'_3_att1_c: areg `v' `indvar2' `controls' i.w `cond2', `opt2'
		local `en'_3_att1_c_N=e(N)
		*add attendance interaction
		eststo `en'_3_att2_d: reg `v' `indvar1' OpenAttendance1 `controls' i.w `cond', `opt2'
		local Attp25_3d = _b[SchoolOpen_i]+_b[OpenAttendance1]*`p25'
		local Attp50_3d = _b[SchoolOpen_i]+_b[OpenAttendance1]*`p50'
		local Attp75_3d = _b[SchoolOpen_i]+_b[OpenAttendance1]*`p75'
		local Attp90_3d = _b[SchoolOpen_i]+_b[OpenAttendance1]*`p90'
		local `en'_3_att2_d_N=e(N)

		eststo `en'_3_att2_c: reg `v' `indvar2' propOpenAttendance1 `controls' i.w `cond', `opt2'
		local Attp25_3c = _b[prop_schools_i]+_b[propOpenAttendance1]*`p25'
		local Attp50_3c = _b[prop_schools_i]+_b[propOpenAttendance1]*`p50'
		local Attp75_3c = _b[prop_schools_i]+_b[propOpenAttendance1]*`p75'
		local Attp90_3c = _b[prop_schools_i]+_b[propOpenAttendance1]*`p90'
		local `en'_3_att2_c_N=e(N)

		foreach t in d c {
			forval j=1/3 {
				local Attp25_`j'`t' : display %9.3f `Attp25_`j'`t''
				local Attp50_`j'`t' : display %9.3f `Attp50_`j'`t''
				local Attp75_`j'`t' : display %9.3f `Attp75_`j'`t''
				local Attp90_`j'`t' : display %9.3f `Attp90_`j'`t''
				local `en'_`j'_att1_`t'_N: display %9.0fc ``en'_`j'_att1_`t'_N'
				local `en'_`j'_att2_`t'_N: display %9.0fc ``en'_`j'_att2_`t'_N'

			}
		}

		foreach t in d c {
			#delimit ;
			local R25Att`t' "Reopening Effect at Percentile 25 of Attendance && `Attp25_1`t''
			&& `Attp25_2`t'' && `Attp25_3`t''";
			local R50Att`t' "Reopening Effect at Percentile 50 of Attendance && `Attp50_1`t''
			&& `Attp50_2`t'' && `Attp50_3`t''";
			local R75Att`t' "Reopening Effect at Percentile 75 of Attendance && `Attp75_1`t''
			&& `Attp75_2`t'' && `Attp75_3`t''";
			local R90Att`t' "Reopening Effect at Percentile 90 of Attendance && `Attp90_1`t''
			&& `Attp90_2`t'' && `Attp90_3`t''";
			local mn "Baseline Mean    & ``en'_mean' & ``en'_mean' & ``en'_mean' 
			& ``en'_mean' & ``en'_mean' & ``en'_mean'";
			local obs`t' "Observations & ``en'_1_att1_`t'_N' & ``en'_1_att2_`t'_N' 
			& ``en'_2_att1_`t'_N' & ``en'_2_att2_`t'_N' & ``en'_3_att1_`t'_N' & ``en'_3_att2_`t'_N'"; 
			#delimit cr
		}

		*export tex file: dummy
		file open ff  using "$OUT/panelA_attendance1_extra_`en'.tex", write replace
		file write ff "`R25Attd'   \\" _n
		file write ff "`R50Attd'   \\" _n
		file write ff "`R75Attd'   \\" _n
		file write ff "`R90Attd'   \\" _n
		file write ff "`mn'        \\" _n
		file write ff "`obsd'      \\" _n
		file close ff

		*export tex file: continuous
		file open ff  using "$OUT/panelB_attendance1_extra_`en'.tex", write replace
		file write ff "`R25Attc'   \\" _n
		file write ff "`R50Attc'   \\" _n
		file write ff "`R75Attc'   \\" _n
		file write ff "`R90Attc'   \\" _n
		file write ff "`mn'        \\" _n
		file write ff "`obsc'      \\" _n
		file close ff

		*PANEL A
		local rg1 `en'_1_att1_d `en'_1_att2_d `en'_2_att1_d `en'_2_att2_d `en'_3_att1_d `en'_3_att2_d
		#delimit ;
		esttab `rg1', 
			   keep(SchoolClose2 SchoolOpen_i OpenAttendance1) b(%-9.4f) se(%-9.4f) noobs;
		esttab `rg1'
			   using "$OUT/panelA_attendance1_`en'.tex", b(%-9.3f) se(%-9.3f) noobs
			   keep(SchoolClose2 SchoolOpen_i OpenAttendance1) nonotes nogaps mlabels(, none) 
			   nonumbers style(tex) starlevel ("*" 0.10 "**" 0.05 "***" 0.01) 
			   fragment replace noline label;
		#delimit cr

		*PANEL B
		local rg2 `en'_1_att1_c `en'_1_att2_c `en'_2_att1_c `en'_2_att2_c `en'_3_att1_c `en'_3_att2_c
		#delimit ;
		esttab `rg2', 
			   keep(SchoolClose2 prop_schools_i propOpenAttendance1) b(%-9.4f) se(%-9.4f) noobs;
		esttab `rg2'
			   using "$OUT/panelB_attendance1_`en'.tex", b(%-9.3f) se(%-9.3f) noobs
			   keep(SchoolClose2 prop_schools_i propOpenAttendance1) nonotes nogaps mlabels(, none) 
			   nonumbers style(tex) starlevel ("*" 0.10 "**" 0.05 "***" 0.01) 
			   fragment replace noline label;
		#delimit cr
}

*-------------------------------------------------------------------------------
*Table S5: No January and February
*-------------------------------------------------------------------------------
preserve
gen m=month(monday)
drop if m==1 | m==2

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
file open ff  using "$OUT/panelA_extra_noFM.tex", write replace
file write ff "`pa'    \\" _n
file write ff "`obs'  \\" _n
file write ff "`mn' \\" _n
file close ff

*export tex file
file open ff  using "$OUT/panelB_extra_noFM.tex", write replace
file write ff "`pb'   \\" _n
file write ff "`obs'  \\" _n
file write ff "`mn' \\" _n
file close ff

*PANEL A
#delimit ;
esttab V_1_d V_2_d V_3_d SA_1_d SA_2_d SA_3_d R_1_d R_2_d R_3_d, 
       keep(SchoolClose2 SchoolOpen_i) b(%-9.4f) se(%-9.4f) noobs;
esttab V_1_d V_2_d V_3_d SA_1_d SA_2_d SA_3_d R_1_d R_2_d R_3_d 
       using "$OUT/panelA_noFM.tex", b(%-9.3f) se(%-9.3f) noobs
       keep(SchoolClose2 SchoolOpen_i) nonotes nogaps mlabels(, none) 
       nonumbers style(tex) starlevel ("*" 0.10 "**" 0.05 "***" 0.01) 
       fragment replace noline label;
#delimit cr

*PANEL B
#delimit ;
esttab V_1_c V_2_c V_3_c SA_1_c SA_2_c SA_3_c R_1_c R_2_c R_3_c, 
       keep(SchoolClose2 prop_schools_i) b(%-9.4f) se(%-9.4f) noobs;
esttab V_1_c V_2_c V_3_c SA_1_c SA_2_c SA_3_c R_1_c R_2_c R_3_c 
       using "$OUT/panelB_noFM.tex", b(%-9.3f) se(%-9.3f) noobs
       keep(SchoolClose2 prop_schools_i) nonotes nogaps mlabels(, none) 
       nonumbers style(tex) starlevel ("*" 0.10 "**" 0.05 "***" 0.01) 
       fragment replace noline label;
#delimit cr
restore

*-------------------------------------------------------------------------------
*Table S6: Unadjusted Variables
*-------------------------------------------------------------------------------
local varr rateSA_old rateV_old
foreach v of local varr {
    if "`v'"=="rateSA_old" local en SA
    if "`v'"=="rateV_old" local en R

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

foreach n in SA R {
    forval i=1/3 {
        local `n'_`i'_d_p : display %9.3f ``n'_`i'_d_p'
        local `n'_`i'_c_p : display %9.3f ``n'_`i'_c_p'
        local `n'_`i'_d_N : display %9.0fc ``n'_`i'_d_N'
        local `n'_`i'_d_N : display %9.0fc ``n'_`i'_c_N'
    }
}

local SA_mean : display %9.3f `SA_mean'
local R_mean  : display %9.3f `R_mean'

*Standardized Estimate
#delimit ;
local pa "Test of $\beta=\gamma$ (p-value) & `SA_1_d_p' & `SA_2_d_p' & `SA_3_d_p'
& `R_1_d_p'  & `R_2_d_p'  & `R_3_d_p' ";
local pb "Test of $\beta=\gamma$ (p-value) & `SA_1_c_p' & `SA_2_c_p' & `SA_3_c_p'
& `R_1_c_p'  & `R_2_c_p'  & `R_3_c_p' ";
local obs "Observations   & `SA_1_d_N' & `SA_2_d_N' & `SA_3_d_N'
& `R_1_d_N'  & `R_2_d_N'  & `R_3_d_N'";
local mn "Baseline Mean   & `SA_mean' & `SA_mean' & `SA_mean' 
& `R_mean'  & `R_mean' & `R_mean'";
#delimit cr

*export tex file
file open ff  using "$OUT/panelA_extra_unadj.tex", write replace
file write ff "`pa'    \\" _n
file write ff "`obs'  \\" _n
file write ff "`mn' \\" _n
file close ff

*export tex file
file open ff  using "$OUT/panelB_extra_unadj.tex", write replace
file write ff "`pb'   \\" _n
file write ff "`obs'  \\" _n
file write ff "`mn' \\" _n
file close ff

*PANEL A
#delimit ;
esttab SA_1_d SA_2_d SA_3_d R_1_d R_2_d R_3_d, 
       keep(SchoolClose2 SchoolOpen_i) b(%-9.4f) se(%-9.4f) noobs;
esttab SA_1_d SA_2_d SA_3_d R_1_d R_2_d R_3_d 
       using "$OUT/panelA_unadj.tex", b(%-9.3f) se(%-9.3f) noobs
       keep(SchoolClose2 SchoolOpen_i) nonotes nogaps mlabels(, none) 
       nonumbers style(tex) starlevel ("*" 0.10 "**" 0.05 "***" 0.01) 
       fragment replace noline label;
#delimit cr

*PANEL B
#delimit ;
esttab SA_1_c SA_2_c SA_3_c R_1_c R_2_c R_3_c, 
       keep(SchoolClose2 prop_schools_i) b(%-9.4f) se(%-9.4f) noobs;
esttab SA_1_c SA_2_c SA_3_c R_1_c R_2_c R_3_c 
       using "$OUT/panelB_unadj.tex", b(%-9.3f) se(%-9.3f) noobs
       keep(SchoolClose2 prop_schools_i) nonotes nogaps mlabels(, none) 
       nonumbers style(tex) starlevel ("*" 0.10 "**" 0.05 "***" 0.01) 
       fragment replace noline label;
#delimit cr

*-------------------------------------------------------------------------------
*Table S7: By type of DV
*-------------------------------------------------------------------------------
local varr rateVIF3 rateVIF2 rateVIF1
foreach v of local varr {
    if "`v'"=="rateVIF1" local en VIF1
    if "`v'"=="rateVIF2" local en VIF2
    if "`v'"=="rateVIF3" local en VIF3

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

foreach n in VIF1 VIF2 VIF3 {
    forval i=1/3 {
        local `n'_`i'_d_p : display %9.3f ``n'_`i'_d_p'
        local `n'_`i'_c_p : display %9.3f ``n'_`i'_c_p'
        local `n'_`i'_d_N : display %9.0fc ``n'_`i'_d_N'
        local `n'_`i'_d_N : display %9.0fc ``n'_`i'_c_N'
    }
}

local VIF1_mean : display %9.3f `VIF1_mean'
local VIF2_mean : display %9.3f `VIF2_mean'
local VIF3_mean : display %9.3f `VIF3_mean'

*Standardized Estimate
#delimit ;
local pa "Test of $\beta=\gamma$ (p-value) & `VIF3_1_d_p' & `VIF3_2_d_p' & `VIF3_3_d_p'
& `VIF2_1_d_p'  & `VIF2_2_d_p'  & `VIF2_3_d_p' 
& `VIF1_1_d_p'  & `VIF1_2_d_p'  & `VIF1_3_d_p'";
local pb "Test of $\beta=\gamma$ (p-value) & `VIF3_1_c_p' & `VIF3_2_c_p' & `VIF3_3_c_p'
& `VIF2_1_c_p'  & `VIF2_2_c_p'  & `VIF2_3_c_p' 
& `VIF1_1_c_p'  & `VIF1_2_c_p'  & `VIF1_3_c_p'";
local obs "Observations   & `VIF3_1_d_N' & `VIF3_2_d_N' & `VIF3_3_d_N'
& `VIF2_1_d_N'  & `VIF2_2_d_N'  & `VIF2_3_d_N'
& `VIF1_1_d_N'  & `VIF1_2_d_N'  & `VIF1_3_d_N'";
local mn "Baseline Mean   & `VIF3_mean' & `VIF3_mean' & `VIF3_mean' 
& `VIF2_mean'  & `VIF2_mean' & `VIF2_mean'
& `VIF1_mean'  & `VIF1_mean' & `VIF1_mean'";
#delimit cr

*export tex file
file open ff  using "$OUT/panelA_extra_VIFT.tex", write replace
file write ff "`pa'    \\" _n
file write ff "`obs'  \\" _n
file write ff "`mn' \\" _n
file close ff

*export tex file
file open ff  using "$OUT/panelB_extra_VIFT.tex", write replace
file write ff "`pb'   \\" _n
file write ff "`obs'  \\" _n
file write ff "`mn' \\" _n
file close ff

*PANEL A
#delimit ;
esttab VIF3_1_d VIF3_2_d VIF3_3_d VIF2_1_d VIF2_2_d VIF2_3_d VIF1_1_d VIF1_2_d VIF1_3_d, 
       keep(SchoolClose2 SchoolOpen_i) b(%-9.4f) se(%-9.4f) noobs;
esttab VIF3_1_d VIF3_2_d VIF3_3_d VIF2_1_d VIF2_2_d VIF2_3_d VIF1_1_d VIF1_2_d VIF1_3_d 
       using "$OUT/panelA_VIFT.tex", b(%-9.3f) se(%-9.3f) noobs
       keep(SchoolClose2 SchoolOpen_i) nonotes nogaps mlabels(, none) 
       nonumbers style(tex) starlevel ("*" 0.10 "**" 0.05 "***" 0.01) 
       fragment replace noline label;
#delimit cr

*PANEL B
#delimit ;
esttab VIF3_1_c VIF3_2_c VIF3_3_c VIF2_1_c VIF2_2_c VIF2_3_c VIF1_1_c VIF1_2_c VIF1_3_c, 
       keep(SchoolClose2 prop_schools_i) b(%-9.4f) se(%-9.4f) noobs;
esttab VIF3_1_c VIF3_2_c VIF3_3_c VIF2_1_c VIF2_2_c VIF2_3_c VIF1_1_c VIF1_2_c VIF1_3_c 
       using "$OUT/panelB_VIFT.tex", b(%-9.3f) se(%-9.3f) noobs
       keep(SchoolClose2 prop_schools_i) nonotes nogaps mlabels(, none) 
       nonumbers style(tex) starlevel ("*" 0.10 "**" 0.05 "***" 0.01) 
       fragment replace noline label;
#delimit cr





