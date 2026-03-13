/******************************************************************************
 * Table A.3: Wave 2 sample with questions about fertility preferences and observed fertility
 ******************************************************************************/

 * use complete data
use "$repo/complete.dta", clear
drop if missing(fintent) | missing(mintent)
drop if missing(aweight)

* generate decoded variables and tab
decode country, generate(country2) 
decode asex, generate(sex)

tab country2 sex
