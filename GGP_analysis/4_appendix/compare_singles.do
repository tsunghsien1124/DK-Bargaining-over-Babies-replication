/******************************************************************************
 * CHECK HOW MANY SINGLE WOMEN GIVE BIRTH TO CHILDREN
 *     APPLY SAME CRITERIA AS WITH THE COUPLES DATASET
 ******************************************************************************/

/*
 * CONSTRUCT A NEW SINGLE WOMEN DATASET
 */
 
quietly {

	use "$raw/GGS_Wave1_Austria_V.4.3.dta", clear
	foreach country in "Belgium" "Bulgaria" "CzechRepublic" "France" "Germany" "Lithuania" "Norway" "Poland" "Romania" "Russia" {
		quietly: append using "$raw/GGS_Wave1_`country'_V.4.3.dta"
	}
	rename arid rid
	rename acountry country

	* select only women who report not to have a partner
	keep if asex == 2
	keep if aparstat == 3


	/*
	 * GENERATE THE INTENTION VARIABLES
	 */

	replace a611 = 1 if missing(a611) & (a622 == 3 | a622 == 4 | a622 == 1602) & (a602 == 2 | a602 == 3)
	replace a611 = 2 if missing(a611) & (a622 == 1 | a622 == 2 | a622 == 1601) & (a602 == 2 | a602 == 3)

	replace a611 = . if a611 == 3 | a611 == 1601 | a611 == .a

	generate fintent = a611


	/*
	 * DEMOGRAPHIC DATA OF THE HOUSEHOLD
	 */

	* age of the female partner
	gen age_f = aage

	* regenerate number of children
	foreach var of varlist ahg3_* {
	  generate amyis`var' = 0
	  replace amyis`var' = 1 if `var' == 2
	  replace amyis`var' = 1 if `var' == 3
	}
	foreach var of varlist a213_* {
	  generate amyis`var' = 0
	  replace amyis`var' = 1 if `var' == 1
	}
	egen ankids1 = rowtotal(amyis*)

	* numkids as no kid, one kid or two and more
	generate numkids = 0
	replace numkids = 1 if ankids1 == 1
	replace numkids = 2 if ankids1 > 1

	* generate number of children between 0 and 3
	forval num = 1/17 {
	  generate akids`num' = 0
	  replace akids`num' = 1 if ahg3_`num' == 2 & ahg5_`num' <= 3
	  replace akids`num' = 1 if ahg3_`num' == 3 & ahg5_`num' <= 3
	}
	forval num = 1/12 {
	  generate akidso`num' = 0
	  replace akidso`num' = 1 if a213_`num' == 1 &  a216a_`num' <= 3
	}
	egen ankids2 = rowtotal(akids*)

	* number of small kids (0 = no or 1 = yes)
	generate numsmall = 0
	replace numsmall = 1 if ankids2 >= 1


	/*
	 * SAMPLE SELECTTION PROCESS
	 */

	* drop if partners can't have any children anymore
	drop if a612 == 1

	* drop if female is above age 45 or below age 20
	drop if age_f > 45 | age_f < 20

	tab country

	/*
	 * COUNTRY SPECIFIC DATA
	 */

	* country codes
	generate cc = ""
	replace cc = "BUL" if country == 11
	replace cc = "RUS" if country == 12
	replace cc = "GEO" if country == 13
	replace cc = "GER" if country == 14
	replace cc = "FRA" if country == 15
	replace cc = "HUN" if country == 16
	replace cc = "ROU" if country == 19
	replace cc = "NOR" if country == 20
	replace cc = "AUT" if country == 21
	replace cc = "BEL" if country == 23
	replace cc = "LTU" if country == 25
	replace cc = "POL" if country == 26
	replace cc = "CZE" if country == 28

	* country codes including fertility rate
	generate ccf = ""
	replace ccf = "BUL (1.38)" if country == 11
	replace ccf = "RUS (1.36)" if country == 12
	replace ccf = "GEO (1.69)" if country == 13
	replace ccf = "GER (1.36)" if country == 14
	replace ccf = "FRA (1.95)" if country == 15
	replace ccf = "HUN (1.31)" if country == 16
	replace ccf = "ROU (1.40)" if country == 19
	replace ccf = "NOR (1.87)" if country == 20
	replace ccf = "AUT (1.39)" if country == 21
	replace ccf = "BEL (1.76)" if country == 23
	replace ccf = "LTU (1.35)" if country == 25
	replace ccf = "POL (1.31)" if country == 26
	replace ccf = "CZE (1.32)" if country == 28

	* fertility rate
	generate fertrate = .
	replace fertrate = 1.38 if country == 11
	replace fertrate = 1.36 if country == 12
	replace fertrate = 1.69 if country == 13
	replace fertrate = 1.36 if country == 14
	replace fertrate = 1.95 if country == 15
	replace fertrate = 1.31 if country == 16
	replace fertrate = 1.40 if country == 19
	replace fertrate = 1.87 if country == 20
	replace fertrate = 1.39 if country == 21
	replace fertrate = 1.76 if country == 23
	replace fertrate = 1.35 if country == 25
	replace fertrate = 1.31 if country == 26
	replace fertrate = 1.32 if country == 28

	* population size (in 100000)
	generate popsize = .
	replace popsize = 77.25500091 if country == 11
	replace popsize = 1440.312979 if country == 12
	replace popsize = 41.86336364 if country == 13
	replace popsize = 822.7290391 if country == 14
	replace popsize = 630.8649855 if country == 15
	replace popsize = 100.9717536 if country == 16
	replace popsize = 212.6183373 if country == 19
	replace popsize = 46.52632091 if country == 20
	replace popsize = 82.04489545 if country == 21
	replace popsize = 105.2020018 if country == 23
	replace popsize = 33.17079000 if country == 25
	replace popsize = 381.7011718 if country == 26
	replace popsize = 102.8287100 if country == 28


	/*
	 * CREATE WEIGHT ADJUSTMENTS
	 */

	levelsof country, local(countries)

	* Romania has all zero weights
	replace aweight = 1 if country == 19

	* aweight2 is the relative weight for each household (summing up to 1000 in each country)
	gen aweight2 = .
	levelsof country, local(countries)
	foreach c of local countries {
	   sum aweight if country == `c'
	   local div = `r(sum)'/1000
	   display "`div'"
	   replace aweight2 = aweight/`div' if country == `c'
	}

	* aweight3 is normalized to the respective population size of the country (2000-2010 average)
	gen aweight3 = aweight2/1000*popsize

	* merge in wave 2 data
	merge 1:1 country rid using "$repo/wave2.dta"
	keep if _merge == 3

	* generate variables using kids
	generate addkids = (bnkids1-ankids1)
	drop if addkids < 0 | addkids > 2
	generate haskids = 0
	replace haskids = 1 if addkids > 0

	* additional sample selection
	drop if missing(fintent)
	drop if missing(aweight)

}


/*
 * DATA ANALYSIS
 */

* check whether dataset has comparable age structure
summarize age_f [aweight = aweight2]

* tabulate fraction of women who have a kid in between waves
tabulate haskids [aweight=aweight2]

* tabulate fraction of women having a kid depending on partner status in wave 2
tabulate bparstat haskids [aweight=aweight2], row
