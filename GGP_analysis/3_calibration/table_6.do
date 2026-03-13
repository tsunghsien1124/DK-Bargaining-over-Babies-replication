/******************************************************************************
 * Table 6: Women’s labor force participation in GGP data and model
 ******************************************************************************/

use "$repo/wave1.dta", clear
drop if missing(alfp_f)
drop if fertrat >= 1.7
drop if missing(edu_f)

tab edu_f [aweight=aweight2]

table edu_f numsmall [aweight=aweight2], contents(mean alfp_f)
