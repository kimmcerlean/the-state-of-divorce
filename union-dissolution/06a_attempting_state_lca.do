/* Resources
** https://www.stata.com/features/overview/latent-class-analysis/
** https://www.stata.com/features/latent-class-analysis/
** https://www.stata.com/meeting/uk18/slides/uk18_MacDonald.pdf
Convergence problems
https://www.statalist.org/forums/forum/general-stata-discussion/general/1629031-not-achieving-convergence-in-latent-class-analysis
*/

********************************************************************************
* Get data and create new variables
********************************************************************************

**Import state data:
use "T:\Research Projects\State data\data_keep\final_measures.dta", clear

keep if year >=1997
drop if state_fips==11

/*
attitudes: disapproval
market: min_above_fed (logit) unemployment_comp cc_pct_income
policy: senate_dems paid_leave (logit) right2work (logit) educ_spend
*/

gen egal_attitudes = 0
replace egal_attitudes=1 if disapproval < 2.21

sum unemployment_comp
gen good_unemployment=0
replace good_unemployment = 1 if unemployment_comp > (`r(mean)' + `r(sd)') 
replace good_unemployment=. if unemployment_comp==.

gen cc_afford=0
replace cc_afford=1 if cc_pct_income <=0.100000

gen liberal=0
replace liberal=1 if senate_dems > 0.50

sum educ_spend
gen high_educ_spend=0
replace high_educ_spend = 1 if educ_spend > (`r(mean)' + `r(sd)') 
replace high_educ_spend=. if educ_spend==.

********************************************************************************
* Attempt analysis
********************************************************************************

/* Failures */
gsem (egal_attitudes min_above_fed good_unemployment cc_afford liberal paid_leave right2work high_educ_spend <-), logit lclass(C 3) // STILL not estimating - does this mean not enough variation?

gsem (egal_attitudes min_above_fed good_unemployment cc_afford liberal paid_leave right2work high_educ_spend <-), logit lclass(C 3) startvalues(randompr, draws(20) seed(15)) // not helping - I don't know enough but I am not sure that it's the starting values?

gsem (egal_attitudes min_above_fed good_unemployment cc_afford liberal paid_leave right2work high_educ_spend <-), logit lclass(C 3) nonrtolerance // with DC removed - right2work estimates, but cc_afford still doesn't I'm so confused
gsem (egal_attitudes min_above_fed good_unemployment cc_afford liberal paid_leave high_educ_spend <-), logit lclass(C 3) // took out right2work based on above, but still don't think helping

/* Progress */
// worked sorta??
gsem (egal_attitudes min_above_fed liberal paid_leave right2work <-  , logit) ///
(unemployment_comp cc_pct_income educ_spend <-  , regress) ///
, lclass(C 3) nonrtolerance // this maybe worked?

estat lcprob
estat lcmean
estat lcgof // goodness of fit

// https://www.stata.com/manuals/semexample52g.pdf: how to test # of classes?
gsem (egal_attitudes min_above_fed liberal paid_leave  <-  , logit) ///
(unemployment_comp cc_pct_income educ_spend <-  , regress) ///
, lclass(C 3) nonrtolerance // this maybe worked?
estimates store class3

gsem (egal_attitudes min_above_fed liberal paid_leave  <-  , logit) ///
(unemployment_comp cc_pct_income educ_spend <-  , regress) ///
, lclass(C 4) nonrtolerance
estimates store class4

gsem (egal_attitudes min_above_fed liberal paid_leave <-  , logit) ///
(unemployment_comp cc_pct_income educ_spend <-  , regress) ///
, lclass(C 5) nonrtolerance // feel like this didn't estimate, not adding a lot of incremental value either
estimates store class5

gsem (egal_attitudes min_above_fed liberal paid_leave <-  , logit) ///
(unemployment_comp cc_pct_income educ_spend <-  , regress) ///
, lclass(C 6) nonrtolerance // removing DC maybe removed variation bc such an outlier? this didn't achieve convergence
estimates store class6

estimates stats class3 class4 class5 class6 // want low AIC /BIC

estimates restore class3
estat lcprob  // prob of being in each group
estat lcmean // values by group

estimates restore class4 // okay so this not really working either
estat lcprob  // prob of being in each group
estat lcmean // values by group

/* Trying to remove CC */
gsem (egal_attitudes min_above_fed liberal paid_leave  <-  , logit) ///
(unemployment_comp educ_spend <-  , regress) ///
, lclass(C 3) nonrtolerance // this maybe worked?
estimates store class3_alt

gsem (egal_attitudes min_above_fed liberal paid_leave  <-  , logit) ///
(unemployment_comp educ_spend <-  , regress) ///
, lclass(C 4) nonrtolerance
estimates store class4_alt

gsem (egal_attitudes min_above_fed liberal paid_leave <-  , logit) ///
(unemployment_comp educ_spend <-  , regress) ///
, lclass(C 5) nonrtolerance
estimates store class5_alt

estimates stats class3_alt class4_alt class5_alt // want low AIC /BIC

estimates restore class3_alt
estat lcprob  // prob of being in each group
estat lcmean // values by group

estimates restore class4_alt
estat lcprob  // prob of being in each group
estat lcmean // values by group

estimates restore class5_alt
estat lcprob  // prob of being in each group
estat lcmean // values by group

/* Kim test groups*/
egen policy_support = rowtotal(min_above_fed liberal paid_leave high_educ_spend good_unemployment)

gen policy_group = . 
replace policy_group = 1 if egal_attitudes==1 & policy_support >=2
replace policy_group = 2 if egal_attitudes==1 & policy_support < 2
replace policy_group = 3 if egal_attitudes==0 & policy_support >=2
replace policy_group = 4 if egal_attitudes==0 & policy_support < 2

label define policy_group 1 "Egal, support" 2 "Egal, no" 3 "Traditional, support" 4 "Traditional, no"
label values policy_group policy_group

********************************************************************************
* Make var to use?!
********************************************************************************

// predict which class a state is in to create a variable
estimates restore class4_alt

predict cpost*, classposteriorpr
egen max = rowmax(cpost*)
generate predclass = 1 if cpost1==max
replace predclass = 2 if cpost2==max
replace predclass = 3 if cpost3==max
replace predclass = 4 if cpost4==max

tabulate predclass

tab predclass policy_group, row

browse state_name year predclass policy_group egal_attitudes min_above_fed liberal paid_leave high_educ_spend good_unemployment

save "T:\Research Projects\State data\data_keep\state_lca.dta", replace

********************************************************************************
* Quick models - eventually merge with other file and make sure accurage but this is back of the envelope
********************************************************************************

use "$data_keep\PSID_marriage_recoded_sample.dta", clear

gen cohort=.
replace cohort=1 if inrange(rel_start_all,1969,1979)
replace cohort=2 if inrange(rel_start_all,1980,1989)
replace cohort=3 if inrange(rel_start_all,1990,2010)
replace cohort=4 if inrange(rel_start_all,2011,2019)

keep if cohort==3
keep if inlist(IN_UNIT,1,2)
keep if marriage_order_real==1
keep if (AGE_REF_>=18 & AGE_REF_<=55) &  (AGE_SPOUSE_>=18 & AGE_SPOUSE_<=55)

// alternate earnings measures
*Convert to 1000s
gen earnings_1000s = couple_earnings / 1000

*Spline
mkspline knot1 0 knot2 20 knot3 = earnings_1000s

rename STATE_ state_fips
rename survey_yr year

merge m:1 state_fips year using "T:\Research Projects\State data\data_keep\state_lca.dta"
drop if _merge==2
drop _merge

label define pred 1 "Trad No" 2 "Egal No" 3 "Trad Yes" 4 "Egal Yes"
label values predclass pred

/* Paid */
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth knot1 knot2 knot3"
melogit dissolve_lag i.dur i.predclass i.hh_earn_type i.predclass#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(predclass=(1(1)4))

melogit dissolve_lag i.dur i.predclass i.hh_earn_type i.predclass#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(predclass=(1(1)4))

/* Unpaid*/
melogit dissolve_lag i.dur i.predclass i.housework_bkt i.predclass#i.housework_bkt `controls' if couple_educ_gp==0 & housework_bkt < 4 || state_fips:, or
margins, dydx(housework_bkt) at(predclass=(1(1)4))

melogit dissolve_lag i.dur i.predclass i.housework_bkt i.predclass#i.housework_bkt `controls' if couple_educ_gp==1 & housework_bkt < 4 || state_fips:, or
margins, dydx(housework_bkt) at(predclass=(1(1)4))