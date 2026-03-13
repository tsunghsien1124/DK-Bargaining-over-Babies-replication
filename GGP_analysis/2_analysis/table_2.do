/******************************************************************************
 * Table 2: Impact of fertility intentions on probability of birth
 ******************************************************************************/

 * use complete dataset
use "$repo/complete.dta", clear

* generate different groups
generate heno = fintent - fmintent
generate sheno = mintent - fmintent
generate bothno = 1 - fintent - mintent + fmintent
generate agree = fmintent

* run regressions (weigh every sample country equally)
quietly: regress haskids heno sheno agree [aweight = aweight2], robust
eststo C1
quietly: regress haskids heno sheno agree [aweight = aweight2] if(numkids == 0), robust
eststo C2
quietly: regress haskids heno sheno agree [aweight = aweight2] if(numkids == 1), robust
eststo C3
quietly: regress haskids heno sheno agree [aweight = aweight2] if(numkids == 2), robust
eststo C4

* export results
esttab C1 C2 C3 C4 using "$texres/table_2.tex", replace ///
    se(%7.3f) b(%7.3f) r2 booktabs compress label nonotes nonumbers nomtitles extracols(2) varwidth(20) ///
	nostar ///
	mgroups("Whole Sample" "By Number of Children", pattern(1 1 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
	mlabels("" "$ n = 0$" "$ n = 1$" "$ n \geq 2$", prefix("{") suffix("}")) ///
	coeflabels(_cons "Constant" heno "SHE YES/HE NO" sheno "SHE NO/HE YES" agree "AGREE") ///
	stats(N r2, layout({@} {@}) fmt(a3 %5.3f) labels("Number of Cases" "R-Square")) ///
	addnotes("Standard errors reported in parentheses.")
