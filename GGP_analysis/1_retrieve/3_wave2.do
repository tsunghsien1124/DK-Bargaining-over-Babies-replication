/******************************************************************************
 * WAVE 2 DATA
 ******************************************************************************/
 
quietly {
 
	/*
	 * MERGE
	 */
	use "$raw/GGS_Wave2_Austria_V.1.3.dta", clear
	foreach country in "Bulgaria" "CzechRepublic" "France" "Germany" "Lithuania" "Russia" {
		quietly: append using "$raw/GGS_Wave2_`country'_V.1.3.dta"
	}
	rename brid rid
	rename bcountry country
	sort country rid


	/*
	 * GENERATE THE INTENTION VARIABLES
	 */

	* there is a new variable b611 for France, but this looks weird, hence use old 
	* methodology to recode b611 from b622
	replace b611 = . if country == 15

	* recoding from a622 for missing data in a611
	replace b611 = 1 if missing(b611) & (b622 == 3 | b622 == 4 | b622 == 1602) & (b602 == 2 | b602 == 3)
	replace b611 = 2 if missing(b611) & (b622 == 1 | b622 == 2 | b622 == 1601) & (b602 == 2 | b602 == 3)

	* recode missings on the intention variables
	replace b611 = . if b611 == 3 | b611 == 1601 | b611 == .a
	replace b615 = . if b615 == 3 | b615 == .a

	* get intention variables by gender
	generate mintent2 = .
	replace  mintent2 = 0 if !missing(b611) & !missing(b615)
	replace  mintent2 = 1 if( (bsex == 1 & b611 == 1) | (bsex == 2 & b615 == 1))

	generate fintent2 = .
	replace  fintent2 = 0 if !missing(b611) & !missing(b615)
	replace  fintent2 = 1 if( (bsex == 2 & b611 == 1) | (bsex == 1 & b615 == 1))

	generate fmintent2 = fintent2*mintent2


	/*
	 * DEMOGRAPHIC DATA OF THE HOUSEHOLD
	 */
	 
	 * age of the male partner
	gen bage_m = .
	replace bage_m = bage if bsex == 1
	replace bage_m = b383 if bsex == 2

	* age of the female partner
	gen bage_f = .
	replace bage_f = bage if bsex == 2
	replace bage_f = b383 if bsex == 1
	 
	* generate number of children
	foreach var of varlist bhg3_* {
	  generate bmyis`var' = 0
	  replace bmyis`var' = 1 if `var' == 2
	  replace bmyis`var' = 1 if `var' == 3
	}
	foreach var of varlist b213_* {
	  generate bmyis`var' = 0
	  replace bmyis`var' = 1 if `var' == 1
	}
	egen bnkids1 = rowtotal(bmyis*)


	/*
	 * WHO IS CARING FOR WHAT DURING NON-MARKET TIME
	 */
	 
	* dressing the child
	generate s_dress = .
	replace s_dress = 0.00 if (bsex == 2 & b201a_a == 1)
	replace s_dress = 0.15 if (bsex == 2 & b201a_a == 2)
	replace s_dress = 0.50 if (bsex == 2 & b201a_a == 3)
	replace s_dress = 0.85 if (bsex == 2 & b201a_a == 4)
	replace s_dress = 1.00 if (bsex == 2 & b201a_a == 5)
	replace s_dress = 1.00 if (bsex == 1 & b201a_a == 1)
	replace s_dress = 0.85 if (bsex == 1 & b201a_a == 2)
	replace s_dress = 0.50 if (bsex == 1 & b201a_a == 3)
	replace s_dress = 0.15 if (bsex == 1 & b201a_a == 4)
	replace s_dress = 0.00 if (bsex == 1 & b201a_a == 5)

	* bringing the child to bed
	generate s_bed = .
	replace s_bed = 0.00 if (bsex == 2 & b201a_b == 1)
	replace s_bed = 0.15 if (bsex == 2 & b201a_b == 2)
	replace s_bed = 0.50 if (bsex == 2 & b201a_b == 3)
	replace s_bed = 0.85 if (bsex == 2 & b201a_b == 4)
	replace s_bed = 1.00 if (bsex == 2 & b201a_b == 5)
	replace s_bed = 1.00 if (bsex == 1 & b201a_b == 1)
	replace s_bed = 0.85 if (bsex == 1 & b201a_b == 2)
	replace s_bed = 0.50 if (bsex == 1 & b201a_b == 3)
	replace s_bed = 0.15 if (bsex == 1 & b201a_b == 4)
	replace s_bed = 0.00 if (bsex == 1 & b201a_b == 5)

	* staying home with the child when it is ill
	generate s_ill = .
	replace s_ill = 0.00 if (bsex == 2 & b201a_c == 1)
	replace s_ill = 0.15 if (bsex == 2 & b201a_c == 2)
	replace s_ill = 0.50 if (bsex == 2 & b201a_c == 3)
	replace s_ill = 0.85 if (bsex == 2 & b201a_c == 4)
	replace s_ill = 1.00 if (bsex == 2 & b201a_c == 5)
	replace s_ill = 1.00 if (bsex == 1 & b201a_c == 1)
	replace s_ill = 0.85 if (bsex == 1 & b201a_c == 2)
	replace s_ill = 0.50 if (bsex == 1 & b201a_c == 3)
	replace s_ill = 0.15 if (bsex == 1 & b201a_c == 4)
	replace s_ill = 0.00 if (bsex == 1 & b201a_c == 5) 

	* playing with the child
	generate s_leis = .
	replace s_leis = 0.00 if (bsex == 2 & b201a_d == 1)
	replace s_leis = 0.15 if (bsex == 2 & b201a_d == 2)
	replace s_leis = 0.50 if (bsex == 2 & b201a_d == 3)
	replace s_leis = 0.85 if (bsex == 2 & b201a_d == 4)
	replace s_leis = 1.00 if (bsex == 2 & b201a_d == 5)
	replace s_leis = 1.00 if (bsex == 1 & b201a_d == 1)
	replace s_leis = 0.85 if (bsex == 1 & b201a_d == 2)
	replace s_leis = 0.50 if (bsex == 1 & b201a_d == 3)
	replace s_leis = 0.15 if (bsex == 1 & b201a_d == 4)
	replace s_leis = 0.00 if (bsex == 1 & b201a_d == 5)

	* doing the homework with the child
	generate s_home = .
	replace s_home = 0.00 if (bsex == 2 & b201a_e == 1)
	replace s_home = 0.15 if (bsex == 2 & b201a_e == 2)
	replace s_home = 0.50 if (bsex == 2 & b201a_e == 3)
	replace s_home = 0.85 if (bsex == 2 & b201a_e == 4)
	replace s_home = 1.00 if (bsex == 2 & b201a_e == 5)
	replace s_home = 1.00 if (bsex == 1 & b201a_e == 1)
	replace s_home = 0.85 if (bsex == 1 & b201a_e == 2)
	replace s_home = 0.50 if (bsex == 1 & b201a_e == 3)
	replace s_home = 0.15 if (bsex == 1 & b201a_e == 4)
	replace s_home = 0.00 if (bsex == 1 & b201a_e == 5) 

	* drive kid to school/leisure activities, ect.
	generate s_trans = .
	replace s_trans = 0.00 if (bsex == 2 & b201a_f == 1)
	replace s_trans = 0.15 if (bsex == 2 & b201a_f == 2)
	replace s_trans = 0.50 if (bsex == 2 & b201a_f == 3)
	replace s_trans = 0.85 if (bsex == 2 & b201a_f == 4)
	replace s_trans = 1.00 if (bsex == 2 & b201a_f == 5)
	replace s_trans = 1.00 if (bsex == 1 & b201a_f == 1)
	replace s_trans = 0.85 if (bsex == 1 & b201a_f == 2)
	replace s_trans = 0.50 if (bsex == 1 & b201a_f == 3)
	replace s_trans = 0.15 if (bsex == 1 & b201a_f == 4)
	replace s_trans = 0.00 if (bsex == 1 & b201a_f == 5)

	egen s_aver = rmean(s_dress s_bed s_ill s_leis s_home s_trans)
	drop s_dress s_bed s_ill s_leis s_home s_trans


	/*
	 * LABOR FORCE ACTIVITY
	 */

	* labor force activity status of respondent (not working = 0, part time = 0.5, working = 1)
	gen act_r = .
	replace act_r = 0 if b801 == 1 | b801 == 5 | b801 == 6 | b801 == 7 | b801 == 8 | b801 == 9 | b801 == 10 | b801 == 11 | b801 == 12 | b801 == 1301 | b801 == 1401 | b801 == 1501
	replace act_r = 0.5 if (b801 == 2 | b801 == 3 | b801 == 4 | b801 == 1402 | b801 == 1502)  & b835 < 30
	replace act_r = 1 if (b801 == 2 | b801 == 3 | b801 == 4 | b801 == 1402 | b801 == 1502) & b835 >= 30

	* labor force activity status of partner (not working = 0, part time = 0.5, working = 1)
	gen act_p = .
	replace act_p = 0 if b901 == 1 | b901 == 5 | b901 == 6 | b901 == 7 | b901 == 8 | b901 == 9 | b901 == 10 | b901 == 11 | b901 == 12 | b901 == 1301 | b901 == 1401 | b901 == 1501
	replace act_p = 0.5 if (b901 == 2 | b901 == 3 | b901 == 4 | b901 == 1402 | b901 == 1502) & (b922 == 2 | (missing(b922) & b923 < 30))
	replace act_p = 1 if (b901 == 2 | b901 == 3 | b901 == 4 | b901 == 1402 | b901 == 1502) & (b922 == 1 | (missing(b922) & b923 >= 30))

	* recode to male and female variables
	gen blfp_m = .
	replace blfp_m = act_r if bsex == 1
	replace blfp_m = act_p if bsex == 2

	gen blfp_f = .
	replace blfp_f = act_r if bsex == 2
	replace blfp_f = act_p if bsex == 1

	drop act_r act_p


	/*
	 * HOURS WORKED
	 */

	* hours worked of the respondent
	gen h_r = .
	replace h_r = 0 if b801 == 3 | b801 == 4 | b801 == 5 | b801 == 7 | b801 == 8 | b801 == 9 | b801 == 10 | b801 == 1201 | b801 == 1501
	replace h_r = 0 if b801 == 6
	replace h_r = b835 if b801 == 1 | b801 == 2

	* hours worked of the partner
	gen h_p = .
	replace h_p = 0 if b901 == 3 | b901 == 4 | b901 == 5 | b901 == 7 | b901 == 8 | b901 == 9 | b901 == 10 | b901 == 1201 | b901 == 1501
	replace h_p = 0 if b901 == 6
	replace h_p = b923 if b901 == 1 | b901 == 2

	* recode to male and female variables
	gen bhours_m = .
	replace bhours_m = h_r if bsex == 1
	replace bhours_m = h_p if bsex == 2

	gen bhours_f = .
	replace bhours_f = h_r if bsex == 2
	replace bhours_f = h_p if bsex == 1

	drop h_r h_p
	
}

/*
 * SORT AND SAVE WAVE 2 DATASET
 */

sort country rid

save "$repo/wave2.dta", replace




/******************************************************************************
 * MERGING OF WAVE 1 AND WAVE 2 DATA
 ******************************************************************************/

quietly {

	* merge and keep sucessful data
	use "$repo/wave1.dta", clear
	merge 1:1 country rid using "$repo/wave2.dta"
	keep if _merge == 3
	drop _merge

	* generate variables using kids
	generate addkids = (bnkids1-ankids1)
	drop if addkids < 0 | addkids > 2
	generate haskids = 0
	replace haskids = 1 if addkids > 0
}

/*
 * SORT AND SAVE COMPLETE DATASET
 */

sort country rid

save "$repo/complete.dta", replace
