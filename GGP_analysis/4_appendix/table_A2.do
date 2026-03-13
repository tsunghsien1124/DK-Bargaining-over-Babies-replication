/******************************************************************************
 * Table A.2: Additional descriptive statistics of the sample (Wave 1)
 ******************************************************************************/

* use wave 1 data
use "$repo/wave1.dta", clear
drop if missing(fintent) | missing(mintent)
drop if missing(aweight)

* is currently employed
generate empl_f = .
replace empl_f = 0 if !missing(a801) & asex == 2
replace empl_f = 0 if !missing(a901) & asex == 1
replace empl_f = 1 if (a801 == 1 | a801 == 2) & asex == 2
replace empl_f = 1 if (a901 == 1 | a901 == 2) & asex == 1
label variable empl_f "Working woman"

* partner is currently employed
generate empl_m = .
replace empl_m = 0 if !missing(a801) & asex == 1
replace empl_m = 0 if !missing(a901) & asex == 2
replace empl_m = 1 if (a801 == 1 | a801 == 2) & asex == 1
replace empl_m = 1 if (a901 == 1 | a901 == 2) & asex == 2

* child care arrangement (institutional)
generate child_ins = .
replace child_ins = 1 if a203a == 1
replace child_ins = 0 if a203a == 2
replace child_ins = 0 if numkids == 0

* child care arrangement (family)
generate child_fam = .
replace child_fam = 1 if a204a == 1
replace child_fam = 0 if a204a == 2
replace child_fam = 0 if numkids == 0

* tabulate a couple of explaining variables
summarize edu_f edu_m empl_f empl_m [aweight = aweight2]
summarize child_ins child_fam if(numkids > 0) [aweight = aweight2]
