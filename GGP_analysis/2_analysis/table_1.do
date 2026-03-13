/******************************************************************************
 * Table 1: Summary statistics of the Wave 1 sample
 ******************************************************************************/

use "$repo/wave1.dta", clear

drop if missing(fintent) | missing(mintent)

* partner status
generate part_pres = .
replace part_pres = 1 if aparstat == 1
replace part_pres = 0 if aparstat == 2
label variable part_pres "Cohabiting" 

* martial status
generate married = .
replace married = 1 if a302a == 1
replace married = 0 if a302a == 2 | part_pres == 0
label variable married "Married" 

* get dummy for low and high fertility
generate highfert = 0
replace highfert = 1 if fertrat > 1.6

* is respondent female
generate female = .
replace female = 1 if asex == 2
replace female = 0 if asex == 1
label variable female "Respondent female"

generate potentials = 0
replace potentials = 1 if fintent == 1
replace potentials = 1 if mintent == 1

* summary statistics
summarize age_f age_m female married part_pres ankids1 fintent mintent potentials fmintent [aweight = aweight2]
