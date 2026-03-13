/******************************************************************************
 * Table A.1: Wave 1 sample with questions about fertility preferences
 ******************************************************************************/

 * use wave 1 data
use "$repo/wave1.dta", clear
drop if missing(fintent) | missing(mintent)
drop if missing(aweight)

* generate decoded variables and tab
decode country, generate(country2) 
decode asex, generate(sex)

tab country2 sex
