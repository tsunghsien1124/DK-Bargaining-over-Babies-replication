/******************************************************************************
 * Figure 5: Fertility intentions across countries, GGP data and model
 ******************************************************************************/

/*
 * share part
 */
 
* use the wave 1 dataset
use "$repo/wave1.dta", clear
drop if missing(alfp_f)

* calculate FLFP for couples with children under age of 3 and all others, then take difference
collapse alfp_f [aweight = aweight2], by(country cc ccf fertrate numsmall)
replace alfp_f = -alfp_f if numsmall == 0
collapse (sum) alfp_f, by(country cc ccf fertrate)
rename alfp_f LFP_with

* merge in wave1 data again and select sample
merge 1:m country using "$repo/wave1.dta", nogenerate
drop if missing(fintent) | missing(mintent)
drop if missing(alfp_f) | alfp_f == 0
drop if ankids1 == 0
keep if ageyoungest <= 14
drop if fertrat >= 1.7
drop if missing(as_aver)

* collapse
collapse as_aver LFP_with [aweight = aweight2], by(country cc ccf fertrate)

* get minimum and maximum shares
sum as_aver
local chi_min = `r(min)'
local chi_max = `r(max)'

* regress shares on LFP
reg LFP_with as_aver

local LFP_min = _b[_cons] + _b[as_aver]*`chi_min'
local LFP_max = _b[_cons] + _b[as_aver]*`chi_max'

* create scatter plot
disp
disp
disp "Least Favorable Country for Women"
disp "chi_m = `chi_min'   and   LFP_gap = `LFP_min'"
disp
disp "Most Favorable Country for Women"
disp "chi_m = `chi_max'   and   LFP_gap = `LFP_max'"




/*
 * disagreement part
 */

* use wave 1 data 
use "$repo/wave1.dta", clear
drop if missing(fintent) | missing(mintent)

drop if fertrat >= 1.7

* collapse by intentions and calculate blocking data
collapse fintent mintent fmintent [aweight = aweight3], by(country cc ccf fertrate numkids)
generate mblock = fintent-fmintent
generate fblock = mintent-fmintent
generate fb_share = fblock/(fblock+mblock+fmintent)*100
generate mb_share = mblock/(fblock+mblock+fmintent)*100


forvalues n = 1/2 {
	disp
	disp
	disp "numkids = `n'"
	disp

	quietly: sum fb_share if numkids == `n'
	local fb_min = `r(min)'
	local fb_max = `r(max)'

	regress mb_share fb_share if numkids == `n', robust
	local mb_min = _b[_cons] + _b[fb_share]*`fb_min'
	local mb_max = _b[_cons] + _b[fb_share]*`fb_max'

	disp
	disp "fblock and mblock combinations"
	disp "[`fb_min', `mb_min'] and [`fb_max', `mb_max']"
	disp
	
	if `n' == 1 {
	    local title = "Couples With One Child"
		local rmax = 40
		local graph = "figure_5a"
		
		local fm_min = 18.76659
		local fm_max = 25.79178
	    local mm_min = 10.97772
		local mm_max = 7.121110
	} 
	if `n' == 2 {
	    local title = "Couples With Two or More Children"
		local rmax = 60
		local graph = "figure_5b"
		
		local fm_min = 33.92787
		local fm_max = 53.99685
	    local mm_min = 29.41618
		local mm_max = 17.91606
	}

	tw (function y=x, range(0 `rmax') lcolor($lcolor)) /// 
	  (scatter mb_share fb_share if(fertrate < 1.7 & numkids == `n') , mlabel(ccf) mcolor($scolor1) mlabcolor($scolor1)) ///  
	  (function y=_b[_cons] + _b[fb_share]*x, range(`fb_min' `fb_max') lcolor(orange) lwidth(thick) lpattern(dash)) /// 
	  (function y= `mm_min' + (`mm_max'-`mm_min')/(`fm_max'-`fm_min')*(x-`fm_min'), range(`fm_min' `fm_max') lcolor(orange_red) lwidth(thick)) /// 
	  , graphregion(color(white)) ///
		xtitle("Disagree Female", size(medlarge)) xlabel(,labsize(medlarge)) ///
		ytitle("Disagree Male", size(medlarge)) ylabel(,labsize(medlarge)) ///
		title("`title'", color(black) size(large)) ///
		legend(pos(10) ring(0) col(1) order(3 4) lab(3 "Data") lab(4 "Model") size(medlarge)) ///
		ylabel(, grid glcolor(white)) name(`graph', replace)
	graph export "$graphs/`graph'.pdf", replace
}
