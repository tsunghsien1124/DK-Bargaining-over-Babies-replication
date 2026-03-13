/******************************************************************************
 * Figure 3: Disagreement over fertility and mother’s labor market behavior
 ******************************************************************************/

 
/*
 * Figure 3a: labor force participation of mothers
 */
 
* use the wave1 dataset
use "$repo/wave1.dta", clear
drop if missing(alfp_f)

* calculate FLFP for couples with children under age of 3 and all others, then take difference
collapse alfp_f [aweight = aweight2], by(country cc ccf fertrate numsmall)
replace alfp_f = -alfp_f if numsmall == 0
collapse (sum) alfp_f, by(country cc ccf fertrate)

* merge in regular data again and collapse intentions
merge 1:m country using "$repo/wave1.dta", nogenerate
drop if missing(fintent) | missing(mintent)
drop if ankids1 == 0
keep if ageyoungest <= 14

collapse fintent mintent fmintent alfp_f [aweight = aweight2], by(country cc ccf fertrate)

* generate blocking shares in potentials
generate mblock = fintent-fmintent
generate fblock = mintent-fmintent
generate fb_share = fblock/(fblock+mblock+fmintent)
generate mb_share = mblock/(fblock+mblock+fmintent)
generate fmdiff = fb_share-mb_share

* extract coefficient
corr fmdiff alfp_f
local coeff: di %5.3f `r(rho)'
reg fmdiff alfp_f, robust

* create respective graph
tw (function y=_b[_cons]+_b[alfp_f]*x, range(-0.75 0.05) lcolor($lcolor)) /// 
  (scatter fmdiff alfp_f if(fertrate < 1.7) , mlabel(ccf) mcolor($scolor1) mlabcolor($scolor1)) ///  
  (scatter fmdiff alfp_f if(fertrate >= 1.7) , mlabel(ccf) mcolor($scolor2) mlabcolor($scolor2) msymbol($symb2)) /// 
  , graphregion(color(white)) ylabel(-0.15[0.15]0.3)  xlabel(-0.8[0.2]0) xscale(range(-0.8 0.1)) ///
    legend(pos(1) ring(0) col(1) order(4) lab(4 "Correlation = `coeff'") size(medlarge)) ///
    xtitle("Gap in LFP Women with Children Below Age 3", size(medlarge)) xlabel(,labsize(medlarge)) ///
	ytitle("Disagree Female - Disagree Male", size(medlarge)) ylabel(,labsize(medlarge)) ///
	title("", color(black) size(large))  ///
	ylabel(, grid glcolor(white)) name(figure_3a, replace)
graph export "$graphs/figure_3a.pdf", replace



/*
 * Figure 3b: labor hours of mothers
 */
 
* use the wave1 dataset
use "$repo/wave1.dta", clear
drop if missing(ahours_f)

* calculate labor hours for couples with children under age of 3 and all others, then take difference
collapse ahours_f [aweight = aweight2], by(country cc ccf fertrate numsmall)
replace ahours_f = -ahours_f if numsmall == 0
collapse (sum) ahours_f, by(country cc ccf fertrate)

* merge in regular data and collapse intentions
merge 1:m country using "$repo/wave1.dta", nogenerate
drop if missing(fintent) | missing(mintent)
drop if ankids1 == 0
keep if ageyoungest <= 14

collapse fintent mintent fmintent ahours_f [aweight = aweight2], by(country cc ccf fertrate)

* generate blocking shares in potentials
generate mblock = fintent-fmintent
generate fblock = mintent-fmintent
generate fb_share = fblock/(fblock+mblock+fmintent)
generate mb_share = mblock/(fblock+mblock+fmintent)
generate fmdiff = fb_share-mb_share

* extract coefficient
corr fmdiff ahours_f
local coeff: di %5.3f `r(rho)'
reg fmdiff ahours_f, robust

* create respective graph
tw (function y=_b[_cons]+_b[ahours_f]*x, range(-33 2) lcolor($lcolor)) /// 
  (scatter fmdiff ahours_f if(fertrate < 1.7) , mlabel(ccf) mcolor($scolor1) mlabcolor($scolor1)) ///  
  (scatter fmdiff ahours_f if(fertrate >= 1.7) , mlabel(ccf) mcolor($scolor2) mlabcolor($scolor2) msymbol($symb2)) /// 
  , graphregion(color(white)) ylabel(-0.15[0.15]0.3) xlabel(-35[5]5) xscale(range(-35 5)) ///
    legend(pos(1) ring(0) col(1) order(4) lab(4 "Correlation = `coeff'") size(medlarge)) ///
    xtitle("Gap in Hours Women with Children Below Age 3", size(medlarge)) xlabel(,labsize(medlarge)) ///
	ytitle("Disagree Female - Disagree Male", size(medlarge)) ylabel(,labsize(medlarge)) ///
	title("", color(black) size(large))   ///
	ylabel(, grid glcolor(white)) name(figure_3b, replace)
graph export "$graphs/figure_3b.pdf", replace
