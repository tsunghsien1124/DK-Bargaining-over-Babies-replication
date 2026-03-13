/******************************************************************************
 * Table 4: Distribution of fertility intentions in GGP data and model
 ******************************************************************************/

* use wave 1 data 
use "$repo/wave1.dta", clear

drop if fertrat >= 1.7
drop if missing(edu_f)
drop if missing(fintent) | missing(mintent)

by edu_f numkids, sort: tab fintent mintent [aweight=aweight2], cell

* generate the respective matrix entries
generate bothno = 0
replace bothno = 100 if fintent == 0 & mintent == 0

generate sheno = 0
replace sheno = 100 if fintent == 0 & mintent == 1

generate heno = 0
replace heno = 100 if fintent == 1 & mintent == 0

generate agree = 0
replace agree = 100 if fintent == 1 & mintent == 1

* collapse
collapse bothno sheno heno agree [aweight=aweight2], by(edu_f numkids)

local i = 1

forval e = 0/1 {

	forval n = 0/2 {
	
		quietly {
			local cell00 = bothno[`i']
			local cell01 = sheno[`i']
			local cell10 = heno[`i']
			local cell11 = agree[`i']
			
			if(`e' == 0) {		
				local edu = "hs"
			}
			else {
				local edu = "co"
			}
		}
		
		disp
	    disp "fert_int_targ(0, :, `n', `edu')  =  (/ `cell00'd0, `cell01'd0/)"
        disp "fert_int_targ(1, :, `n', `edu')  =  (/ `cell10'd0, `cell11'd0/)"
		
		local i = `i' + 1
	}
}
