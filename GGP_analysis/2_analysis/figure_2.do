/******************************************************************************
 * Figure 2: Disagreement over fertility and men’s share in caring for children
 ******************************************************************************/

* use the childcost dataset
use "$repo/wave1.dta", clear

* drop if data on men's share in child rearing is missing
drop if missing(as_aver)

* drop those without children
drop if ankids1 == 0

* only those with children under the age of 14
keep if ageyoungest <= 14

* collapse
collapse as_aver [aweight = aweight2], by(country cc ccf fertrate)

* merge in regular data and collapse intentions
merge 1:m country using "$repo/wave1.dta", nogenerate
drop if missing(fintent) | missing(mintent)
drop if ankids1 == 0
keep if ageyoungest <= 14

collapse fintent mintent fmintent as_aver [aweight = aweight2], by(country cc ccf fertrate)

* generate blocking shares in potentials
generate mblock = fintent-fmintent
generate fblock = mintent-fmintent
generate fb_share = fblock/(fblock+mblock+fmintent)
generate mb_share = mblock/(fblock+mblock+fmintent)
generate fmdiff = fb_share-mb_share

* extract coefficient
corr fmdiff as_aver
local coeff: di %5.3f `r(rho)'
regress fmdiff as_aver, robust

* create respective graph
tw (function y=_b[_cons]+_b[as_aver]*x, range(0.2 0.45) lcolor($lcolor)) /// 
  (scatter fmdiff as_aver if(fertrate < 1.7) , mlabel(ccf) mcolor($scolor1) mlabcolor($scolor1)) ///  
  (scatter fmdiff as_aver if(fertrate >= 1.7) , mlabel(ccf) mcolor($scolor2) mlabcolor($scolor2) msymbol($symb2)) /// 
  , graphregion(color(white)) ylabel(-0.1[0.1]0.3) ///
    legend(pos(1) ring(0) col(1) order(4) lab(4 "Correlation = `coeff'") size(medlarge)) ///
    xtitle("Share of men caring for childen", size(medlarge)) xlabel(,labsize(medlarge)) ///
	ytitle("Disagree Female - Disagree Male", size(medlarge)) ylabel(,labsize(medlarge)) ///
	title("", color(black) size(large))  ///
	ylabel(, grid glcolor(white)) name(figure_2, replace)
graph export "$graphs/figure_2.pdf", replace
