/* SchoolClosureCounterfactual.do      DanielPailanir      yyyy-mm-dd:2022-06-11
*----|----1----|----2----|----3----|----4----|----5----|----6----|----7----|----8
	This code is for replicate the counterfactual results:
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
	
	Recomendation: This code takes about 18 hours of work (i7 11gen) because 
	we iterate 250 times on each estimate (144). So be careful and maybe run a 
	subset or reduce local boot.
*/
clear all
set more off

*-------------------------------------------------------------------------------
*Global and some details
*-------------------------------------------------------------------------------
global ROOT "/SchoolClosureViolence/"

global DAT "$ROOT/data"
global GRA "$ROOT/results/graphs"

set scheme plotplainblind, permanently
graph set window fontface "Times New Roman"

*-------------------------------------------------------------------------------
*Prediction
*-------------------------------------------------------------------------------
local variables rate rateSA rateV
local trends notrend lineal cuadratic
local boot=250

foreach v of local variables {
    use $DAT/SchoolClosure_Final.dta, clear
    bys comuna: gen t=_n
    gen t2=t^2
    gen pre = week<63
    gen pre_t = pre*t
    gen pre_t2 = pre*t2
    format %tdDD/NN/CCYY monday
    xtset comuna week
    local opt2 "cluster(comuna) abs(comuna)"
    gen schoolVar1 = 1 if SchoolClose==0
    qui replace schoolVar1 = 0 if SchoolClose==1
    qui replace schoolVar1 = prop_schools_i if prop_schools_i !=0 & prop_schools_i!=.
    local c1
    local c2 caseRate pcr positivity quarantine
    local c3 schoolVar1
    local c4 schoolVar1 caseRate pcr positivity quarantine	
    keep if week<=156
    tempfile CounterfactualData
    save `CounterfactualData'
		
    *keep dates for bounds
    preserve
    keep if week>0 & week<157
    keep monday
    duplicates drop
    gen dates=monday
    qui levelsof dates, local(dates)
    restore
		
    if "`v'"=="rate"   local en V
    if "`v'"=="rateSA" local en SA
    if "`v'"=="rateV"  local en R
		
    foreach tr of local trends {
        if "`tr'"=="notrend"   local td 
        if "`tr'"=="lineal"    local td pre_t
        if "`tr'"=="cuadratic" local td pre_t pre_t2
		
        forvalues t=2015/2018 {
            use `CounterfactualData' , clear
            qui keep if year>=`t'
            local cond2 "[aw=populationyoung]"
			
            *Baseline
            cap drop weekpre ratehat*
            gen weekpre = w
            replace weekpre = 54 if year>=2020
            qui tab weekpre, gen(_week)
            drop _week54

            *(1) only time
            qui areg `v' _week* `c1' `td' `cond2', `opt2'
            drop _week*
            qui tab w, gen(_week)
            rename pre_t Xpre_t
            rename pre_t2 Xpre_t2
            rename t pre_t 
            rename t2 pre_t2
            predict ratehat1
			
			*(2) Covid controls and quarantine
            cap drop _week*
            qui tab weekpre, gen(_week)
            drop _week54
            rename pre_t t
            rename pre_t2 t2
            rename Xpre_t pre_t
            rename Xpre_t2 pre_t2
			
            qui areg `v' _week* `c2' `td' `cond2', `opt2'
            drop _week*
            qui tab w, gen(_week)
            rename pre_t Xpre_t
            rename pre_t2 Xpre_t2
            rename t pre_t 
            rename t2 pre_t2
            predict ratehat2
			
            *(3) School Open
            cap drop _week*
            qui tab weekpre, gen(_week)
            drop _week54
            rename pre_t t
            rename pre_t2 t2
            rename Xpre_t pre_t
            rename Xpre_t2 pre_t2

            qui areg `v' _week* `c3' `td' `cond2', `opt2'
            drop _week*
            qui tab w, gen(_week)
            rename pre_t Xpre_t
            rename pre_t2 Xpre_t2
            rename t pre_t 
            rename t2 pre_t2
            predict ratehat3
			
            *(4) School Open + Covid controls and quarantine
            cap drop _week*
            qui tab weekpre, gen(_week)
            drop _week54
            rename pre_t t
            rename pre_t2 t2
            rename Xpre_t pre_t
            rename Xpre_t2 pre_t2

            qui areg `v' _week* `c4' `td' `cond2', `opt2'
            drop _week*
            qui tab w, gen(_week)
            rename pre_t Xpre_t
            rename pre_t2 Xpre_t2
            rename t pre_t 
            rename t2 pre_t2
            predict ratehat4
            drop _week*
			
            preserve
            collapse (sum) `v' ratehat* (mean) monday w  [aw=populationyoung], by(week)
            mkmat monday, matrix(R0)
            mkmat `v', matrix(R1)
            mkmat ratehat1, matrix(R2)
            mkmat ratehat2, matrix(S2)
            mkmat ratehat3, matrix(T2)
            mkmat ratehat4, matrix(U2)
            qui gen EC1=(ratehat1-`v')^2 if week>=1 & week<63
            qui gen EC2=(ratehat2-`v')^2 if week>=1 & week<63
            qui gen EC3=(ratehat3-`v')^2 if week>=1 & week<63
            qui gen EC4=(ratehat4-`v')^2 if week>=1 & week<63
			
            forval i=1/4 {
                qui sum EC`i' if EC`i'!=.
                local rmse`i'=sqrt(r(mean))
                local rmse`i' : display %9.4f `rmse`i''
            }
            restore
	
            *bootstrap baseline prediction by regression
            foreach j of num  1(1)4 {
                if "`j'"=="1" local name R
                if "`j'"=="2" local name S
                if "`j'"=="3" local name T
                if "`j'"=="4" local name U
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
            local names R S T U
            foreach n of local names {
                forvalues i=2/`up' {
                    svmat `n'`i'
                }
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
                qui sum `ttime' if R01>=21550
                local lmin=r(min)
                local lmax=r(max)
				
                forvalues i=`lmin'/`lmax' {
                    qui replace `n'lb = `n'lb`j' in `i'
                    qui replace `n'ub = `n'ub`j' in `i'
                    local ++j
                }
            }
			
            if "`v'"=="rateSA" keep if R01<=22614
            if "`v'"=="rateV"  keep if R01<=22614
            gen year = year(R01)
            local i=1
            foreach n in R S T U {
                gen dff`i'    = (`n'21 - R11)*10240/100000 
                gen dff`i'_lb = (`n'lb - R11)*10240/100000 
                gen dff`i'_ub = (`n'ub - R11)*10240/100000 
                local ++i
            }

            gen g = 1 if R01>=21984 & R01<=22145
            replace g = 2 if R01>22145 & R01<22642 

            forvalues i=1/4 {
                bys g: egen sumdff`i' = total(dff`i')
                bys g: egen sumdff`i'_lb = total(dff`i'_lb)
                bys g: egen sumdff`i'_ub = total(dff`i'_ub)
            }

            forvalues j=1/2 {
                forvalues i=1/4 {
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
                forvalues i=1/4 {
                    local tot`i'_`j': display %5.0fc `tot`i'_`j''
                    local tot`i'_`j'_lb: display %5.0fc `tot`i'_`j'_lb'
                    local tot`i'_`j'_ub: display %5.0fc `tot`i'_`j'_ub'
                }
            }

            tsset R01
            local serie R S T U
            local n=1
            foreach s of local serie {
                cap drop diffvar* area* aux* sumd* ax*
                gen diffvar1    = `s'21-R11   
                gen diffvar1_10 = 1.10*`s'21-R11
                gen diffvar1_20 = 1.20*`s'21-R11
                gen diffvar1_30 = 1.30*`s'21-R11
                gen diffvar1_40 = 1.40*`s'21-R11
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
                    qui sum diffvar1 if R01==22635
                    local yyc1=r(mean)
                    qui sum area2 if R01==22635
                    local yyc2=r(mean)
                    qui sum area3 if R01==22635
                    local yyc3=r(mean)
                    qui sum area4 if R01==22635
                    local yyc4=r(mean)
                    qui sum area5 if R01==22635
                    local yyc5=r(mean)
                    local ylabels -350(250)1550
                    local cord1 1650 22018
                    local cord2 1650 22196
                    local ycord1 `yyc1' 22700
                    local ycord2 `yyc2' 22700
                    local ycord3 `yyc3' 22700
                    local ycord4 `yyc4' 22700
                    local ycord5 `yyc5' 22700
				}
				if "`v'"=="rateSA" {
                    qui sum diffvar1 if R01==22614
                    local yyc1=r(mean)
                    qui sum area2 if R01==22614
                    local yyc2=r(mean)
                    qui sum area3 if R01==22614
                    local yyc3=r(mean)
                    qui sum area4 if R01==22614
                    local yyc4=r(mean)
                    qui sum area5 if R01==22614
                    local yyc5=r(mean)
                    local ylabels -350(200)1100
                    local cord1 1000 22018
                    local cord2 1000 22196
                    local ycord1 `yyc1' 22670
                    local ycord2 `yyc2' 22670
                    local ycord3 `yyc3' 22670
                    local ycord4 `yyc4' 22670
                    local ycord5 `yyc5' 22670
				}
				if "`v'"=="rateV" {
                    qui sum diffvar1 if R01==22614
                    local yyc1=r(mean)
                    qui sum area2 if R01==22614
                    local yyc2=r(mean)
                    qui sum area3 if R01==22614
                    local yyc3=r(mean)
                    qui sum area4 if R01==22614
                    local yyc4=r(mean)
                    qui sum area5 if R01==22614
                    local yyc5=r(mean)
                    local ylabels -100(25)175
                    local cord1 155 22018
                    local cord2 155 22196
                    local ycord1 `yyc1' 22670
                    local ycord2 `yyc2' 22670
                    local ycord3 `yyc3' 22670
                    local ycord4 `yyc4' 22670
                    local ycord5 `yyc5' 22670
                }
				
                #delimit ;
                tw area diffvar1 R01, lc(black) color("253 231 37%50") || 
                   rarea diffvar1 area2 R01,  color("93 201 99%50")    || 
                   rarea area2 diffvar1_20 R01, color("33 144 140%50") || 
                   rarea area3 diffvar1_30 R01, color("59 82 139%50")  || 
                   rarea area4 diffvar1_40 R01, color("68 1 84%50")    || 
                if R01>=21550 & R01<22642 & R01>21984, ytitle("Difference") 
                yline(0, lc(red) lp(dash)) xline(21984 22145, lc(red) lp(solid) lw(0.4))
                ttitle("") tlabel(#14, angle(45)) ylabel(`ylabels', angle(0)) 
                legend(order(1 "Observed" 2 "{&Delta}10%" 3 "{&Delta}20%"
                             4 "{&Delta}30%" 5 "{&Delta}40%") pos(6) col(5))
                text(`cord1' "School" "Close") text(`cord2' "School" "Reopening") 
                text(`ycord1' "Diff = `sumd1'") text(`ycord2' "Diff = `sumd2'") 
                text(`ycord3' "Diff = `sumd3'") text(`ycord4' "Diff = `sumd4'") 
                text(`ycord5' "Diff = `sumd5'") graphregion(margin(r=18 t=8));
				graph export "$GRA/counterfactual/diff_Count_`n'_`en'_`t'_`tr'.pdf", replace;
                graph save "$GRA/counterfactual/diff_Count_`n'_`en'_`t'_`tr'.gph", replace;
				#delimit cr
				local ++n
			}

            sort R01
            local i1 "if R01>=21550 & R01<22642" //22404
            local i2 "if R01>=21984 & R01<22642" //22404
			
            #delimit ;
            twoway line R11 R01 `i1', lc(gs6) lp(solid) || 
                rarea Rlb Rub R01 `i2', color(gs8%40) fcol(gs8%40) fi(gs8%40) || 
                line R21 R01 `i1', lc(blue) lp(solid)
            xline(21984 22145, lc(red) lp(solid)) xtitle("")
            ylabel(`ylabels') xlabel(#13, angle(45))
            legend(order(1 "Actual Values"
                         3 "Counterfactual (time only)") pos(6) col(3))
            text(`cord1' "Under-reporting" "(closure)" "`tot1_1'" "[`tot1_1_lb'-`tot1_1_ub']",
            size(2.5) box bc(dkgreen%30) fc(dkgreen%30) lc(dkgreen%30) bexpand bmargin(b+2))
            text(`cord2' "Under-reporting" "(re-opening)" "`tot1_2'" "[`tot1_2_lb'-`tot1_2_ub']",
            size(2.5) box bc(dkgreen%30) fc(dkgreen%30) lc(dkgreen%30) bexpand bmargin(b+2))
            note("RMSPE = `rmse1'");
            graph export "$GRA/counterfactual/C1_`en'_`t'_`tr'.pdf", replace;
            graph save   "$GRA/counterfactual/C1_`en'_`t'_`tr'.gph", replace;
            #delimit cr

            #delimit ;
            twoway line R11 R01 `i1', lc(gs6) lp(solid) || 
                rarea Slb Sub R01 `i2', color(gs8%40) fcol(gs8%40) fi(gs8%40) || 
                line S21 R01 `i1', lc(blue) lp(solid) 
            xline(21984 22145, lc(red) lp(solid)) xtitle("") 
            ylabel(`ylabels') xlabel(#13, angle(45))
            legend(order(1 "Actual Values" 
                   3 "Counterfactual (epidemiological controls)") pos(6) col(3))
            text(`cord1' "Under-reporting" "(closure)" "`tot2_1'" "[`tot2_1_lb'-`tot2_1_ub']",
            size(2.5) box bc(dkgreen%30) fc(dkgreen%30) lc(dkgreen%30) bexpand bmargin(b+2))
            text(`cord2' "Under-reporting" "(re-opening)" "`tot2_2'" "[`tot2_2_lb'-`tot2_2_ub']",
            size(2.5) box bc(dkgreen%30) fc(dkgreen%30) lc(dkgreen%30) bexpand bmargin(b+2))
            note("RMSPE = `rmse2'");
            graph export "$GRA/counterfactual/C2_`en'_`t'_`tr'.pdf", replace;
            graph save   "$GRA/counterfactual/C2_`en'_`t'_`tr'.gph", replace;
            #delimit cr

            #delimit ;
            twoway line R11 R01 `i1', lc(gs6) lp(solid) || 
                rarea Tlb Tub R01 `i2', color(gs8%40) fcol(gs8%40) fi(gs8%40) || 
                line T21 R01 `i1', lc(blue) lp(solid) 
            xline(21984 22145, lc(red) lp(solid)) xtitle("") 
            ylabel(`ylabels') xlabel(#13, angle(45))
            legend(order(1 "Actual Values" 
                         3 "Counterfactual (school controls)") pos(6) col(3))
            text(`cord1' "Under-reporting" "(closure)" "`tot3_1'" "[`tot3_1_lb'-`tot3_1_ub']",
            size(2.5) box bc(dkgreen%30) fc(dkgreen%30) lc(dkgreen%30) bexpand bmargin(b+2))
            text(`cord2' "Under-reporting" "(re-opening)" "`tot3_2'" "[`tot3_2_lb'-`tot3_2_ub']",
            size(2.5) box bc(dkgreen%30) fc(dkgreen%30) lc(dkgreen%30) bexpand bmargin(b+2))
            note("RMSPE = `rmse3'");
            graph export "$GRA/counterfactual/C3_`en'_`t'_`tr'.pdf", replace;
            graph save   "$GRA/counterfactual/C3_`en'_`t'_`tr'.gph", replace;
            #delimit cr

            #delimit ;
            twoway line R11 R01 `i1', lc(gs6) lp(solid) || 
                rarea Ulb Uub R01 `i2', color(gs8%40) fcol(gs8%40) fi(gs8%40) || 
                line U21 R01 `i1', lc(blue) lp(solid) 
            xline(21984 22145, lc(red) lp(solid)) xtitle("") 
            ylabel(`ylabels') xlabel(#13, angle(45))
            legend(order(1 "Actual Values" 
                         3 "Counterfactual (school + epi)") pos(6) col(3))
            text(`cord1' "Under-reporting" "(closure)" "`tot4_1'" "[`tot4_1_lb'-`tot4_1_ub']",
            size(2.5) box bc(dkgreen%30) fc(dkgreen%30) lc(dkgreen%30) bexpand bmargin(b+2))
            text(`cord2' "Under-reporting" "(re-opening)" "`tot4_2'" "[`tot4_2_lb'-`tot4_2_ub']",
            size(2.5) box bc(dkgreen%30) fc(dkgreen%30) lc(dkgreen%30) bexpand bmargin(b+2))
            note("RMSPE = `rmse4'");
            graph export "$GRA/counterfactual/C4_`en'_`t'_`tr'.pdf", replace;
            graph save   "$GRA/counterfactual/C4_`en'_`t'_`tr'.gph", replace;
            #delimit cr
        }
    }
}


