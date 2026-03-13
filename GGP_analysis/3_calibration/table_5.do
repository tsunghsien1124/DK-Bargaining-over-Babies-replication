/******************************************************************************
 * Table 5: Share of couples with same fertility intentions in both waves in GGP data
 ******************************************************************************/

* use complete dataset
use "$repo/complete.dta", clear
drop if missing(fintent) | missing(mintent)

* take out high fertility countries
drop if fertrat >= 1.7

* take out women above age 35
keep if age_f <= 35

* only use those couples who had no kids
keep if haskids == 0

* drop missing second period observations
drop if missing(b611) | b611 == 3


* generate table
quietly: tabulate fintent2 mintent2 [aweight = aweight2] if(fintent == 0 & mintent == 0), cell matcell(x)
local cell11 = x[1,1]/(x[1,1]+x[1,2]+x[2,1]+x[2,2])*100

quietly: tabulate fintent2 mintent2 [aweight = aweight2] if(fintent == 1 & mintent == 0), cell matcell(x)
local cell21 = x[2,1]/(x[1,1]+x[1,2]+x[2,1]+x[2,2])*100

quietly: tabulate fintent2 mintent2 [aweight = aweight2] if(fintent == 0 & mintent == 1), cell matcell(x)
local cell12 = x[1,2]/(x[1,1]+x[1,2]+x[2,1]+x[2,2])*100

quietly: tabulate fintent2 mintent2 [aweight = aweight2] if(fintent == 1 & mintent == 1), cell matcell(x)
local cell22 = x[2,2]/(x[1,1]+x[1,2]+x[2,1]+x[2,2])*100


disp "He no  / She no : `cell11'd0"
disp "He yes / She no : `cell12'd0"
disp "He no  / She yes: `cell21'd0"
disp "He yes / She yes: `cell22'd0<"
