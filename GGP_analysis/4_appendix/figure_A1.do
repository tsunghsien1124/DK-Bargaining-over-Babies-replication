/******************************************************************************
 * Figure A.1: Life cycle profiles of fertility intentions and agreement
 ******************************************************************************/

 * use complete data
use "$repo/wave1.dta", clear
drop if missing(fintent) | missing(mintent)
drop if missing(aweight)

* generate agreement status stuff
generate heno = fintent - fmintent
generate sheno = mintent - fmintent
generate bothno = 1 - fintent - mintent + fmintent
generate agree = fmintent

* age of woman
label variable age_f "Age woman" 

generate age_f2 = age_f*age_f/100
label variable age_f2 "Age squared/100"

* age of man
generate age_diff = age_m - age_f
label variable age_diff "Age difference"

label variable edu_f "Educ. woman" 
label variable edu_m "Educ. man" 

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
label variable empl_m "Working man"

* gender of the first child
generate firstmale = .
replace firstmale = 1 if a252_1 == 1
replace firstmale = 0 if a252_1 == 2
replace firstmale = 0 if numkids == 0
label variable firstmale "First kid male"

* gender of the second child
generate secondmale = .
replace secondmale = 1 if a252_2 == 1
replace secondmale = 0 if a252_2 == 2
replace secondmale = 0 if numkids <= 1
label variable secondmale "Second kid male"

* child care arrangement (buying)
generate child_ins = .
replace child_ins = 1 if a203a == 1
replace child_ins = 0 if a203a == 2
replace child_ins = 0 if numkids == 0
label variable child_ins "Inst. child care"

* child care arrangement (family)
generate child_fam = .
replace child_fam = 1 if a204a == 1
replace child_fam = 0 if a204a == 2
replace child_fam = 0 if numkids == 0
label variable child_fam "Family child care"

* is respondent female
generate female = .
replace female = 1 if asex == 2
replace female = 0 if asex == 1
label variable female "Respondent female"

* order variables
order age_f age_f2 age_diff edu_f edu_m empl_f empl_m female child_ins child_fam firstmale secondmale, last

* generate country fixed effects
tabulate country, gen(countryFE)
	
* generate age profiles for couples without children and plot
quietly: regress fintent   married   age_f-female countryFE* [aweight=aweight2] if(numkids == 0), noconstant robust

preserve
adjust married age_diff-female countryFE*, by(age_f) replace
rename xb f_pred
keep age_f f_pred
save "$repo/f_pred.dta",replace
restore

quietly: regress mintent   married   age_f-female countryFE* if(numkids == 0) [aweight = aweight2], noconstant robust
adjust married age_diff-female countryFE*, by(age_f)

preserve
adjust married age_diff-female countryFE*, by(age_f) replace
rename xb m_pred
keep age_f m_pred
merge 1:1 age_f using "$repo/f_pred.dta"

replace f_pred = f_pred*100
replace m_pred = m_pred*100

tw (line f_pred age_f, lcolor($scolor1)  lwidth(thick) lcolor($scolor2)) ///  
  (line m_pred age_f, lcolor($scolor1)  lwidth(thick) lcolor($scolor1) lpattern(dash)) ///  
  , graphregion(color(white)) ylabel(0[20]100) ///
    legend(pos(10) ring(0) col(1) order(1 2) lab(1 "Intentation woman") lab(2 "Intentation man") size(medlarge)) ///
    xtitle("Age of female partner", size(medlarge)) xlabel(,labsize(medlarge)) ///
	ytitle("Percentage", size(medlarge)) ylabel(,labsize(medlarge)) ///
	title("Age profiles without children", color(black) size(large))  ///
	ylabel(, grid glcolor(white)) name(figure_A1a, replace)
graph export "$graphs/figure_A1a.pdf", replace

restore

	
* generate age profiles for couples with one child and plot
quietly: regress fintent   married   age_f-female countryFE* [aweight=aweight2] if(numkids == 1), noconstant robust

preserve
adjust married age_diff-female countryFE*, by(age_f) replace
rename xb f_pred
keep age_f f_pred
save "$repo/f_pred.dta",replace
restore

quietly: regress mintent   married   age_f-female countryFE* if(numkids == 1) [aweight = aweight2], noconstant robust
adjust married age_diff-female countryFE*, by(age_f)

preserve
adjust married age_diff-female countryFE*, by(age_f) replace
rename xb m_pred
keep age_f m_pred
merge 1:1 age_f using "$repo/f_pred.dta"

replace f_pred = f_pred*100
replace m_pred = m_pred*100

tw (line f_pred age_f, lcolor($scolor1)  lwidth(thick) lcolor($scolor2)) ///  
  (line m_pred age_f, lcolor($scolor1)  lwidth(thick) lcolor($scolor1) lpattern(dash)) ///  
  , graphregion(color(white)) ylabel(0[20]100) ///
    legend(pos(10) ring(0) col(1) order(1 2) lab(1 "Intention woman") lab(2 "Intention man") size(medlarge)) ///
    xtitle("Age of female partner", size(medlarge)) xlabel(,labsize(medlarge)) ///
	ytitle("Percentage", size(medlarge)) ylabel(,labsize(medlarge)) ///
	title("Age profiles one child", color(black) size(large))  ///
	ylabel(, grid glcolor(white)) name(figure_A1b, replace)
graph export "$graphs/figure_A1b.pdf", replace

restore


