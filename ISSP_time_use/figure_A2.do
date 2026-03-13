/******************************************************************************
 * Figure 9: Disagreement over fertility and men’s share in caring for children, measured with time use data
 ******************************************************************************/
 

clear all
set maxvar 30000
set more off

* set colors
global lcolor = "gs6"
global scolor1 = "midblue"
global scolor2 = "orange_red"
global symb2 = "t" 
 
/*
 * create dataset
 */
 
 
use ZA5900_v4-0-0.dta

* Keep countries we need
local countries `" "AT" "BE" "BG" "CZ" "FR" "DE" "LT" "NO" "PL" "RU" "'
gen keep = .
foreach c of local countries{
	replace keep = 1 if C_ALPHAN == "`c'"
}
keep if keep == 1
drop keep

local vars V38 V40 V3 V4 C_ALPHAN SEX BIRTH AGE PARTLIV HHCHILDR HHTODD WEIGHT 
keep `vars'

rename `vars', lower

* fix missing values
replace sex =. if sex==9
replace v38 = . if v38==98 | v38==99
replace v40 = . if inlist(v40, 0, 98, 99)
replace v40 = 0 if v40 == 96
lab def v40s  0 "0 hours"
lab val v40 v40s
replace birth = . if inlist(birth, 9999, 9998)
replace age = . if inlist(age, 999, 998)
replace partliv = . if inlist(partliv, 0, 7, 9)
replace hhchildr = . if inlist(hhchildr, 96, 97, 99)
replace hhtodd = . if inlist(hhtodd, 96, 97, 99)



/*
 * Construct average hours of child care datasets
 */
 
 
drop if sex == . 
rename v4 country

* keep only those living with a partner in the same household
keep if partliv == 1

* create indicators for having at least one toddler
gen toddler = 1 if hhtodd >=1
replace toddler = 0 if hhtodd == 0 
replace toddler = . if hhtodd == .
lab var toddler "Has 1+ below school age child"

* create a fraction of the self-reported total in childcare 
gen fraccare = v38/(v38 + v40)
gen fraccare_sp = 1-fraccare

* collapse accordingly and relabel
collapse (mean) v38 v40 fraccare fraccare_sp (count) obs = v38 [aw=weight] if toddler == 1, by(country c_alphan sex)
lab var v38 "Hours spent on family members per week"
lab var fraccare "Reported fraction of total hours spent on family members in the couple"
lab var v40 "Hours spent on family members per week (spouse)"
lab var fraccare_sp "Reported fraction of total hours spent on family members in the couple (spouse)"


/*
 * Merge in disagreement data
 */
 
rename country country_issp
gen     country = 21 if c_alphan == "AT"
replace country = 23 if c_alphan == "BE"
replace country = 11 if c_alphan == "BG"
replace country = 28 if c_alphan == "CZ"
replace country = 14 if c_alphan == "DE"
replace country = 15 if c_alphan == "FR"
replace country = 25 if c_alphan == "LT"
replace country = 20 if c_alphan == "NO"
replace country = 26 if c_alphan == "PL"
replace country = 12 if c_alphan == "RU"

* create a fraction of the mean hours
bys country: egen tothrs = total(v38)
lab var tothrs "Total mean hours across men and women"
gen frac_hrs = v38/tothrs

* keep only male shares
keep if sex==1

* merge with disagreement dataset
merge 1:m country using fmdiff.dta, update

* only missing Romania (not in ISSP)
drop _merge

* check countries were merged correctly
tab country country_issp

* Replicate graphs
lab var frac_hrs "Male share of total mean hours caring for family members"
lab var fmdiff "Disagree Female - Disagree Male"
 
* extract coefficient
corr fmdiff frac_hrs
local coeff: di %5.3f `r(rho)'
regress fmdiff frac_hrs, robust

* create respective graph
tw (function y=_b[_cons]+_b[frac_hrs]*x, range(0.2 0.45) lcolor($lcolor)) /// 
  (scatter fmdiff frac_hrs if(fertrate < 1.7) , mlabel(ccf) mcolor($scolor1) mlabcolor($scolor1)) ///  
  (scatter fmdiff frac_hrs if(fertrate >= 1.7) , mlabel(ccf) mcolor($scolor2) mlabcolor($scolor2) msymbol($symb2)) /// 
  , graphregion(color(white)) ylabel(-0.1[0.1]0.3) ///
	legend(pos(1) ring(0) col(1) order(4) lab(4 "Correlation = `coeff'") size(medlarge)) ///
	xtitle("Male share of total mean hours caring for family members", size(medlarge)) xlabel(,labsize(medlarge)) ///
	ytitle("Disagree Female - Disagree Male", size(medlarge)) ylabel(,labsize(medlarge)) ///
	title("", color(black) size(large))  ///
	ylabel(, grid glcolor(white)) name(saver_all, replace)
graph export "figure_A2.pdf", replace
