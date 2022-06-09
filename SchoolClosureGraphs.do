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
*Figure S14-S15
*-------------------------------------------------------------------------------


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






