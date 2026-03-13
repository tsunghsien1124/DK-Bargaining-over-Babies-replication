/******************************************************************************
 * Table A.6: What covaries with agreement on wanting a baby?
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
	
* regress agreement in sample with at least 1 intender on different explanatory factors
display "AGREE WITHOUT KIDS"
quietly: regress agree   married   age_f-female countryFE* if(numkids == 0 & bothno == 0) [aweight = aweight2], noconstant robust
eststo C1

display "AGREE WITHOUT KIDS"
quietly: regress agree   part_pres age_f-female countryFE* if(numkids == 0 & bothno == 0) [aweight = aweight2], noconstant robust 
eststo C2

display "AGREE WITH KIDS"
quietly: regress agree   married   age_f-firstmale countryFE* if(numkids == 1 & bothno == 0) [aweight = aweight2], noconstant robust
eststo C3

display "AGREE WITH KIDS"
quietly: regress agree   part_pres age_f-firstmale countryFE* if(numkids == 1 & bothno == 0) [aweight = aweight2], noconstant robust 
eststo C4

display "AGREE WITH KIDS"
quietly: regress agree   married   age_f-firstmale countryFE* if(numkids > 1 & bothno == 0) [aweight = aweight2], noconstant robust
eststo C5

display "AGREE WITH KIDS"
quietly: regress agree   part_pres age_f-firstmale countryFE* if(numkids >  1 & bothno == 0) [aweight = aweight2], noconstant robust 
eststo C6

esttab C1 C2 C3 C4 C5 C6 using "$texres/table_A6.tex", replace ///
    se(%7.4f) b(%7.4f) r2 booktabs compress label nonotes nonumbers nomtitles extracols(3 5) varwidth(20) ///
    order(age_f age_f2 age_diff married part_pres edu_f edu_m empl_f empl_m child_ins child_fam firstmale female) ///
	nostar ///
	mgroups("without children" "with 1 child" "with 2+ children", pattern(1 0 1 0 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
	mlabels("(1)" "(2)" "(1)" "(2)" "(1)" "(2)", prefix("{") suffix("}")) ///
	stats(N r2, layout({@} {@}) fmt(a3 %5.3f) labels("Number of Cases" "R-Square")) ///
	addnotes("Standard errors reported in parentheses. ") ///
	drop(countryFE*)
