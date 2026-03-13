/******************************************************************************
 * Table 3: Fertility rates in GGP data by fertility intention
 ******************************************************************************/

 * use complete dataset
use "$repo/complete.dta", clear
drop if missing(fintent) | missing(mintent)

* generate different groups
generate heno = fintent - fmintent
generate sheno = mintent - fmintent
generate bothno = 1 - fintent - mintent + fmintent
generate agree = fmintent

forval e = 0/1 {

	forval n = 0/2 {
	
		disp
		disp
		disp "education = `e' and numkids = `n'"
		disp
		
		quietly {
			regress haskids heno sheno agree if(numkids == `n' & edu_f == `e') [aweight = aweight2], robust
			
			local cell00 = _b[_cons]
			local cell10 = _b[_cons]
			local cell01 = _b[_cons]
			local cell11 = _b[_cons]
			
			if(ttail(e(df_r), abs(_b[ heno]/_se[ heno]))*2 < 0.05) {
				local cell10 = `cell10' + _b[ heno]
			}
			
			if(ttail(e(df_r), abs(_b[sheno]/_se[sheno]))*2 < 0.05) {
				local cell01 = `cell01' + _b[sheno]
			}
			
			if(ttail(e(df_r), abs(_b[agree]/_se[agree]))*2 < 0.05) {
				local cell11 = `cell11' + _b[agree]
			}
			
			if(`e' == 0) {		
				local edu = "hs"
			}
			else {
				local edu = "co"
			}
		}
		
		disp
		disp
		disp "nu(`n', 0, :, `edu') = (/ 0`cell00'd0, 0`cell01'd0/)"
		disp "nu(`n', 1, :, `edu') = (/ 0`cell10'd0, 0`cell11'd0/)"
	}
	
}
