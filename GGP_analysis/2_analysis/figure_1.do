/******************************************************************************
 * Figure 1: Disagreement over having a baby across countries
 ******************************************************************************/

 
/*
 * Figure 1a: for total sample
 */
 
* use wave 1 data 
use "$repo/wave1.dta", clear
drop if missing(fintent) | missing(mintent)

* collapse by intentions and calculate blocking data
collapse fintent mintent fmintent [aweight = aweight2], by(country cc ccf fertrate)
generate mblock = fintent-fmintent
generate fblock = mintent-fmintent
generate fb_share = fblock/(fblock+mblock+fmintent)
generate mb_share = mblock/(fblock+mblock+fmintent)

* create respective graph
tw (function y=x, range(0 0.6) lcolor($lcolor)) /// 
  (scatter mb_share fb_share if(fertrate < 1.7) , mlabel(ccf) mcolor($scolor1) mlabcolor($scolor1)) ///  
  (scatter mb_share fb_share if(fertrate >= 1.7) , mlabel(ccf) mcolor($scolor2) mlabcolor($scolor2) msymbol($symb2)) /// 
  , legend(off) graphregion(color(white)) ///
    xtitle("Disagree Female", size(medlarge)) xlabel(,labsize(medlarge)) ///
	ytitle("Disagree Male", size(medlarge)) ylabel(,labsize(medlarge)) ///
	title("All couples", color(black) size(large)) ///
	ylabel(, grid glcolor(white)) name(figure_1a, replace)
graph export "$graphs/figure_1a.pdf", replace



/*
 * Figure 1b-d: by number of existing children
 */

* use wave 1 data 
use "$repo/wave1.dta", clear
drop if missing(fintent) | missing(mintent)

* collapse by intentions and calculate blocking data
collapse fintent mintent fmintent [aweight = aweight2], by(country cc ccf fertrate numkids)
generate mblock = fintent-fmintent
generate fblock = mintent-fmintent
generate fb_share = fblock/(fblock+mblock+fmintent)
generate mb_share = mblock/(fblock+mblock+fmintent)


* create respective graph by number of children
forvalues n = 0/2 {

	* define respective headline
    if `n' == 0 {
	    local title = "Couples Without Children"
		local graph = "figure_1b"
	}
	if `n' == 1 {
	    local title = "Couples With One Child"
		local graph = "figure_1c"
	} 
	if `n' == 2 {
	    local title = "Couples With Two or More Children"
		local graph = "figure_1d"
	}

	tw (function y=x, range(0 0.6) lcolor($lcolor)) /// 
	  (scatter mb_share fb_share if(fertrate < 1.7 & numkids == `n') , mlabel(ccf) mcolor($scolor1) mlabcolor($scolor1)) ///  
	  (scatter mb_share fb_share if(fertrate >= 1.7 & numkids == `n') , mlabel(ccf) mcolor($scolor2) mlabcolor($scolor2) msymbol($symb2)) /// 
	  , legend(off) graphregion(color(white)) ///
		xtitle("Disagree Female", size(medlarge)) xlabel(,labsize(medlarge)) ///
		ytitle("Disagree Male", size(medlarge)) ylabel(,labsize(medlarge)) ///
		title("`title'", color(black) size(large)) ///
		ylabel(, grid glcolor(white)) name(`graph', replace)		
	graph export "$graphs/`graph'.pdf", replace
}
