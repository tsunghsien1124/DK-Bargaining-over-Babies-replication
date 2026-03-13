/******************************************************************************
 * WAVE 1 DATA
 ******************************************************************************/

quietly {
 
	/*
	 * MERGE
	 */
	use "$raw/GGS_Wave1_Austria_V.4.3.dta", clear
	foreach country in "Belgium" "Bulgaria" "CzechRepublic" "France" "Germany" "Lithuania" "Norway" "Poland" "Romania" "Russia" {
		quietly: append using "$raw/GGS_Wave1_`country'_V.4.3.dta"
	}
	rename arid rid
	rename acountry country


	/*
	 * GENERATE THE INTENTION VARIABLES
	 */

	* there is a new variable a611 for France, but this looks weird, hence use old 
	* methodology to recode a611 from a622
	replace a611 = . if country == 15

	* recoding from a622 for missing data in a611
	replace a611 = 1 if (missing(a611)) & (a622 == 3 | a622 == 4 | a622 == 1602) & (a602 == 2 | a602 == 3)
	replace a611 = 2 if (missing(a611)) & (a622 == 1 | a622 == 2 | a622 == 1601) & (a602 == 2 | a602 == 3)

	* recode missings on the intention variables
	replace a611 = . if a611 == 3 | a611 == 1601 | a611 == .a
	replace a615 = . if a615 == 3 | a615 == .a

	* get intention variables by gender
	generate mintent = .
	replace  mintent = 0 if !missing(a611) & !missing(a615)
	replace  mintent = 1 if( (asex == 1 & a611 == 1) | (asex == 2 & a615 == 1))

	generate fintent = .
	replace  fintent = 0 if !missing(a611) & !missing(a615)
	replace  fintent = 1 if( (asex == 2 & a611 == 1) | (asex == 1 & a615 == 1))

	generate fmintent = fintent*mintent


	/*
	 * DEMOGRAPHIC DATA OF THE HOUSEHOLD
	 */

	 * age of the male partner
	gen age_m = .
	replace age_m = aage if asex == 1
	replace age_m = a383 if asex == 2

	* age of the female partner
	gen age_f = .
	replace age_f = aage if asex == 2
	replace age_f = a383 if asex == 1

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

	* drop if female is above age 55 or below age 20
	drop if age_m < 20 | age_m > 55

	tab country


	/*
	 * WHO IS CARING FOR WHAT DURING NON-MARKET TIME
	 */
	 
	* dressing the child
	generate s_dress = .
	replace s_dress = 0.00 if (asex == 2 & a201a_a == 1)
	replace s_dress = 0.15 if (asex == 2 & a201a_a == 2)
	replace s_dress = 0.50 if (asex == 2 & a201a_a == 3)
	replace s_dress = 0.85 if (asex == 2 & a201a_a == 4)
	replace s_dress = 1.00 if (asex == 2 & a201a_a == 5)
	replace s_dress = 1.00 if (asex == 1 & a201a_a == 1)
	replace s_dress = 0.85 if (asex == 1 & a201a_a == 2)
	replace s_dress = 0.50 if (asex == 1 & a201a_a == 3)
	replace s_dress = 0.15 if (asex == 1 & a201a_a == 4)
	replace s_dress = 0.00 if (asex == 1 & a201a_a == 5)

	* bringing the child to bed
	generate s_bed = .
	replace s_bed = 0.00 if (asex == 2 & a201a_b == 1)
	replace s_bed = 0.15 if (asex == 2 & a201a_b == 2)
	replace s_bed = 0.50 if (asex == 2 & a201a_b == 3)
	replace s_bed = 0.85 if (asex == 2 & a201a_b == 4)
	replace s_bed = 1.00 if (asex == 2 & a201a_b == 5)
	replace s_bed = 1.00 if (asex == 1 & a201a_b == 1)
	replace s_bed = 0.85 if (asex == 1 & a201a_b == 2)
	replace s_bed = 0.50 if (asex == 1 & a201a_b == 3)
	replace s_bed = 0.15 if (asex == 1 & a201a_b == 4)
	replace s_bed = 0.00 if (asex == 1 & a201a_b == 5)

	* staying home with the child when it is ill
	generate s_ill = .
	replace s_ill = 0.00 if (asex == 2 & a201a_c == 1)
	replace s_ill = 0.15 if (asex == 2 & a201a_c == 2)
	replace s_ill = 0.50 if (asex == 2 & a201a_c == 3)
	replace s_ill = 0.85 if (asex == 2 & a201a_c == 4)
	replace s_ill = 1.00 if (asex == 2 & a201a_c == 5)
	replace s_ill = 1.00 if (asex == 1 & a201a_c == 1)
	replace s_ill = 0.85 if (asex == 1 & a201a_c == 2)
	replace s_ill = 0.50 if (asex == 1 & a201a_c == 3)
	replace s_ill = 0.15 if (asex == 1 & a201a_c == 4)
	replace s_ill = 0.00 if (asex == 1 & a201a_c == 5) 

	* playing with the child
	generate s_leis = .
	replace s_leis = 0.00 if (asex == 2 & a201a_d == 1)
	replace s_leis = 0.15 if (asex == 2 & a201a_d == 2)
	replace s_leis = 0.50 if (asex == 2 & a201a_d == 3)
	replace s_leis = 0.85 if (asex == 2 & a201a_d == 4)
	replace s_leis = 1.00 if (asex == 2 & a201a_d == 5)
	replace s_leis = 1.00 if (asex == 1 & a201a_d == 1)
	replace s_leis = 0.85 if (asex == 1 & a201a_d == 2)
	replace s_leis = 0.50 if (asex == 1 & a201a_d == 3)
	replace s_leis = 0.15 if (asex == 1 & a201a_d == 4)
	replace s_leis = 0.00 if (asex == 1 & a201a_d == 5)

	* doing the homework with the child
	generate s_home = .
	replace s_home = 0.00 if (asex == 2 & a201a_e == 1)
	replace s_home = 0.15 if (asex == 2 & a201a_e == 2)
	replace s_home = 0.50 if (asex == 2 & a201a_e == 3)
	replace s_home = 0.85 if (asex == 2 & a201a_e == 4)
	replace s_home = 1.00 if (asex == 2 & a201a_e == 5)
	replace s_home = 1.00 if (asex == 1 & a201a_e == 1)
	replace s_home = 0.85 if (asex == 1 & a201a_e == 2)
	replace s_home = 0.50 if (asex == 1 & a201a_e == 3)
	replace s_home = 0.15 if (asex == 1 & a201a_e == 4)
	replace s_home = 0.00 if (asex == 1 & a201a_e == 5) 

	* drive kid to school/leisure activities, ect.
	generate s_trans = .
	replace s_trans = 0.00 if (asex == 2 & a201a_f == 1)
	replace s_trans = 0.15 if (asex == 2 & a201a_f == 2)
	replace s_trans = 0.50 if (asex == 2 & a201a_f == 3)
	replace s_trans = 0.85 if (asex == 2 & a201a_f == 4)
	replace s_trans = 1.00 if (asex == 2 & a201a_f == 5)
	replace s_trans = 1.00 if (asex == 1 & a201a_f == 1)
	replace s_trans = 0.85 if (asex == 1 & a201a_f == 2)
	replace s_trans = 0.50 if (asex == 1 & a201a_f == 3)
	replace s_trans = 0.15 if (asex == 1 & a201a_f == 4)
	replace s_trans = 0.00 if (asex == 1 & a201a_f == 5)

	egen as_aver = rmean(s_dress s_bed s_ill s_leis s_home s_trans)

	drop s_dress s_bed s_ill s_leis s_home s_trans


	/*
	 * LABOR FORCE ACTIVITY
	 */

	* labor force activity status of respondent (not working = 0, part time = 0.5, working = 1)
	gen act_r = .
	replace act_r = 0 if a801 == 3 | a801 == 4 | a801 == 5 | a801 == 6 | a801 == 7 | a801 == 8 | a801 == 9 | a801 == 10 | a801 == 1201 | a801 == 1501
	replace act_r = 0.5 if (a801 == 1 | a801 == 2) & (a834 == 2 | (missing(a834) & a835 < 30))
	replace act_r = 1 if (a801 == 1 | a801 == 2) & (a834 == 1 | (missing(a834) & a835 >= 30))

	* labor force activity status of partner (not working = 0, part time = 0.5, working = 1)
	gen act_p = .
	replace act_p = 0 if a901 == 3 | a901 == 4 | a901 == 5 | a901 == 6 | a901 == 7 | a901 == 8 | a901 == 9 | a901 == 10 | a901 == 1201 | a901 == 1501
	replace act_p = 0.5 if (a901 == 1 | a901 == 2) & (a922 == 2 | (missing(a922) & a923 < 30))
	replace act_p = 1 if (a901 == 1 | a901 == 2) & (a922 == 1 | (missing(a922) & a923 >= 30))

	* recode to male and female variables
	gen alfp_m = .
	replace alfp_m = act_r if asex == 1
	replace alfp_m = act_p if asex == 2

	gen alfp_f = .
	replace alfp_f = act_r if asex == 2
	replace alfp_f = act_p if asex == 1

	drop act_r act_p

	gen alfp_f_2 = .
	replace alfp_f_2 = 0 if alfp_f == 0
	replace alfp_f_2 = 1 if alfp_f > 0 & !missing(alfp_f)

	gen alfp_m_2 = .
	replace alfp_m_2 = 0 if alfp_m == 0
	replace alfp_m_2 = 1 if alfp_m > 0 & !missing(alfp_m)


	/*
	 * HOURS WORKED
	 */

	* hours worked of the respondent
	gen h_r = .
	replace h_r = 0 if a801 == 3 | a801 == 4 | a801 == 5 | a801 == 7 | a801 == 8 | a801 == 9 | a801 == 10 | a801 == 1201 | a801 == 1501
	replace h_r = 0 if a801 == 6
	replace h_r = a835 if a801 == 1 | a801 == 2

	* hours worked of the partner
	gen h_p = .
	replace h_p = 0 if a901 == 3 | a901 == 4 | a901 == 5 | a901 == 7 | a901 == 8 | a901 == 9 | a901 == 10 | a901 == 1201 | a901 == 1501
	replace h_p = 0 if a901 == 6
	replace h_p = a923 if a901 == 1 | a901 == 2

	* recode to male and female variables
	gen ahours_m = .
	replace ahours_m = h_r if asex == 1
	replace ahours_m = h_p if asex == 2

	gen ahours_f = .
	replace ahours_f = h_r if asex == 2
	replace ahours_f = h_p if asex == 1

	drop h_r h_p


	/*
	 * EDUCATION
	 */

	* higher education of respondent
	generate edu_r = .
	replace edu_r = 0 if !missing(a148)
	replace edu_r = 1 if (a148 == 5 | a148 == 6 | a148 == 1506 | a148 == 1507 | a148 == 1701 | a148 == 1702 | a148 == 1703)

	generate edu2_r = edu_r
	replace edu2_r = 1 if a148 == 4

	* higher education of the partner
	generate edu_p = .
	replace edu_p = 0 if (!missing(a308) | !missing(a321))
	replace edu_p = 1 if a308 == 5 | a308 == 6 | a308 == 1506 | a308 == 1507 | a308 == 1701 | a308 == 1702 | a308 == 1703
	replace edu_p = 1 if a321 == 5 | a321 == 6 | a321 == 1506 | a321 == 1507 | a321 == 1701 | a321 == 1702 | a321 == 1703

	* ütgher classification of isced 4 category
	generate edu2_p = edu_p
	replace edu2_p = 1 if a308 == 4
	replace edu2_p = 1 if a321 == 4

	* recode to male and female variables
	gen edu_m = .
	replace edu_m = edu_r if asex == 1
	replace edu_m = edu_p if asex == 2

	gen edu2_m = .
	replace edu2_m = edu2_r if asex == 1
	replace edu2_m = edu2_p if asex == 2

	gen edu_f = .
	replace edu_f = edu_r if asex == 2
	replace edu_f = edu_p if asex == 1

	gen edu2_f = .
	replace edu2_f = edu2_r if asex == 2
	replace edu2_f = edu2_p if asex == 1

	drop edu_r edu_p edu2_r edu2_p


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

}

/*
 * SORT AND SAVE WAVE 1 DATASET
 */

sort country rid

save "$repo/wave1.dta", replace
