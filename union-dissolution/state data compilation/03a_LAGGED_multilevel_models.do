********************************************************************************
* Aggregating state + individual files + analysis
* lagged_multilevel_models.do
* Kim McErlean
********************************************************************************
* This is a replica of step 3, but instead of using IVs in the SAME year as DV
* I am using IV two years before DV, so time to react to information

// first compile data
use "$temp/2010_2018_compiled_dataset_factor.dta", clear // created in step 2, saving new copy to use as lagged
drop if year > 2016
recode year (2010=2012) (2011=2013) (2012=2014) (2013=2015) (2014=2016) (2015=2017) (2016=2018)
save "$temp/2010_2018_compiled_dataset_factor_LAG.dta", replace

use "$created_data\acs_married_last_yr.dta", clear // created in "state_hh_type_married_last_yr"
keep if year>=2012 & year <=2018 // so just doing 2012-2018, using indicators from 2010-2016
//need to make a COUPLE LEVEL file, right now individual, so there might be duplicates?
keep if person==1 // so only keep FIRST record from household

merge m:1 year statefip using "$temp/2010_2018_compiled_dataset_factor_LAG.dta" // created above
drop _merge

save "$created_data/2010_2018_individ_analysis_LAG.dta", replace

gen abortion_rec=abortion+3 // had negative values, but if I want to use as factor variable, can't, so this sets min to 0 (was previously -3)

sum f_leave_policy
gen sc_f_leave_policy = (f_leave_policy-`r(min)') / (`r(max)'-`r(min)')

sum f_tanf
gen sc_f_tanf = (f_tanf-`r(min)') / (`r(max)'-`r(min)')

sum f_childcare
gen sc_f_childcare = (f_childcare-`r(min)') / (`r(max)'-`r(min)')

// basic regression
logit dual_earn i.college, or
logit male_bw i.college, or

logit dual_earn i.statefip, nocons or

********************************************************************************
* Multi-level models
********************************************************************************
/* variables
cultural support for motherhood: fepresch
childcare costs: infant_care_pct
childcare subsidies: cc_percent_served
family policy: leave_policy_score
percent access to paid leave: pct_workers_paid_leave
eitc: eitc_credit
tanf: tanf_rate tanf_basic tanf_cc tanf_max tanf_max_cost
women leg: women_legis
dems leg: dems_legis
abortion: abortion
poverty rate: x_pov_rate

factors:
f_tanf
f_childcare
f_implicit
f_culture
f_family_policy
  
browse fepresch infant_care_pct cc_percent_served leave_policy_score pct_workers_paid_leave eitc_credit tanf_rate women_legis dems_legis abortion x_pov_rate
// use SC versions of all but leave_policy_score and abortion
sc_fepresch sc_infant_care_pct sc_cc_percent_served sc_pct_workers_paid_leave sc_eitc_credit sc_tanf_rate sc_women_legis sc_dems_legis sc_x_pov_rate
// okay actually use mean centered version
center_fepresch center_infant_care_pct center_cc_percent_served center_pct_workers_paid_leave center_eitc_credit center_tanf_rate center_tanf_basic center_tanf_cc center_tanf_max center_tanf_max_cost center_women_legis center_dems_legis center_x_pov_rate
*/

// what TANF to use?
melogit male_bw year if college==0 || statefip:, or
estimates store x1
melogit male_bw year center_tanf_rate if college==0 || statefip:, or
estimates store x2
melogit male_bw year center_tanf_basic if college==0 || statefip:, or
estimates store x3
melogit male_bw year center_tanf_cc if college==0 || statefip:, or
estimates store x4
melogit male_bw year center_tanf_max if college==0 || statefip:, or
estimates store x5
melogit male_bw year center_tanf_max_cost if college==0 || statefip:, or
estimates store x6
melogit male_bw year f_tanf if college==0 || statefip:, or
estimates store x7

estimates table x1 x2 x3 x4 x5 x6 x7, star b(%9.3f)
esttab x1 x2 x3 x4 x5 x6 x7 using "$results/multi-level_non_tanf.csv", star b(%9.5f) replace

// Just less-educated - male BW
melogit male_bw year if college==0 || statefip:, or
estimates store b1
melogit male_bw year center_fepresch if college==0 || statefip:, or
estimates store b2
melogit male_bw year center_infant_care_pct if college==0 || statefip:, or
estimates store b3
melogit male_bw year center_cc_percent_served if college==0 || statefip:, or
estimates store b4
melogit male_bw year center_pct_workers_paid_leave if college==0 || statefip:, or
estimates store b5
melogit male_bw year center_eitc_credit if college==0 || statefip:, or
estimates store b6
melogit male_bw year sc_f_tanf if college==0 || statefip:, or
estimates store b7
melogit male_bw year center_women_legis if college==0 || statefip:, or
estimates store b8
melogit male_bw year center_dems_legis if college==0 || statefip:, or
estimates store b9
melogit male_bw year center_x_pov_rate if college==0 || statefip:, or
estimates store b10
melogit male_bw year center_leave_policy_score if college==0 || statefip:, or
estimates store b11
melogit male_bw year sc_f_leave_policy if college==0 || statefip:, or
estimates store b12
melogit male_bw year abortion_rec if college==0 || statefip:, or
estimates store b13
melogit male_bw year center_fepresch center_infant_care_pct center_cc_percent_served center_pct_workers_paid_leave center_eitc_credit sc_f_tanf center_women_legis center_dems_legis center_x_pov_rate center_leave_policy_score sc_f_leave_policy abortion_rec if college==0 || statefip:, or
estimates store b14
melogit male_bw center_fepresch center_infant_care_pct center_cc_percent_served center_pct_workers_paid_leave center_eitc_credit sc_f_tanf center_women_legis center_dems_legis center_x_pov_rate center_leave_policy_score sc_f_leave_policy abortion_rec if college==0 || statefip:, or
estimates store b15 // no year

melogit male_bw year if college==0 || statefip: center_fepresch center_infant_care_pct center_cc_percent_served center_pct_workers_paid_leave center_eitc_credit sc_f_tanf center_women_legis center_dems_legis center_x_pov_rate center_leave_policy_score sc_f_leave_policy abortion_rec, or

// doesn't work: melogit male_bw if college==0 || statefip: center_fepresch center_dems_legis center_leave_policy_score, or
// doesn't work: melogit male_bw year if college==0 || statefip: center_fepresch center_dems_legis center_leave_policy_score, or
melogit male_bw year center_fepresch center_dems_legis center_leave_policy_score if college==0 || statefip: center_fepresch center_dems_legis center_leave_policy_score, or

/*
melogit male_bw year f_childcare if college==0 || statefip:, or
estimates store f1
melogit male_bw year f_implicit if college==0 || statefip:, or
estimates store f2
// melogit male_bw year f_culture if college==0 || statefip:, or
// estimates store f3
melogit male_bw year f_family_policy if college==0 || statefip:, or
estimates store f4

melogit male_bw year center_fepresch f_childcare f_implicit f_family_policy if college==0 || statefip:, or
estimates store f10
*/

// estimates table b9 b10 b11 b12 b13 b14, star b(%9.3f)
esttab b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 using "$results/lagged_multi-level_non.csv", star b(%9.5f) replace

// Just college-educated - male BW
melogit male_bw year if college==1 || statefip:, or
estimates store c1
melogit male_bw year center_fepresch if college==1 || statefip:, or
estimates store c2
melogit male_bw year center_infant_care_pct if college==1 || statefip:, or
estimates store c3
melogit male_bw year center_cc_percent_served if college==1 || statefip:, or
estimates store c4
melogit male_bw year center_pct_workers_paid_leave if college==1 || statefip:, or
estimates store c5
melogit male_bw year center_eitc_credit if college==1 || statefip:, or
estimates store c6
melogit male_bw year sc_f_tanf if college==1 || statefip:, or
estimates store c7
melogit male_bw year center_women_legis if college==1 || statefip:, or
estimates store c8
melogit male_bw year center_dems_legis if college==1 || statefip:, or
estimates store c9
melogit male_bw year center_x_pov_rate if college==1 || statefip:, or
estimates store c10
melogit male_bw year center_leave_policy_score if college==1 || statefip:, or
estimates store c11
melogit male_bw year sc_f_leave_policy if college==1 || statefip:, or
estimates store c12
melogit male_bw year abortion_rec if college==1 || statefip:, or
estimates store c13
melogit male_bw year center_fepresch center_infant_care_pct center_cc_percent_served center_pct_workers_paid_leave center_eitc_credit sc_f_tanf center_women_legis center_dems_legis center_x_pov_rate center_leave_policy_score sc_f_leave_policy abortion_rec if college==1 || statefip:, or
estimates store c14
melogit male_bw center_fepresch center_infant_care_pct center_cc_percent_served center_pct_workers_paid_leave center_eitc_credit sc_f_tanf center_women_legis center_dems_legis center_x_pov_rate center_leave_policy_score sc_f_leave_policy abortion_rec if college==1 || statefip:, or
estimates store c15 // no year 

/*
melogit male_bw year f_childcare if college==1 || statefip:, or
estimates store f5
melogit male_bw year f_implicit if college==1 || statefip:, or
estimates store f6
// melogit male_bw year f_culture if college==1 || statefip:, or
// estimates store f7
melogit male_bw year f_family_policy if college==1 || statefip:, or
estimates store f8

melogit male_bw year center_fepresch f_childcare f_implicit f_family_policy if college==1 || statefip:, or
estimates store f11
*/

// estimates table c9 c10 c11 c12 c13 c14, star b(%9.3f)
esttab c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 using "$results/lagged_multi-level_coll.csv", star b(%9.5f) replace

// add one at a time and see what contributes the MOST to variance? like why is it so high for college, what variables are doing that?
melogit male_bw year  if college==1 || statefip:, or
estimates store d2

melogit male_bw year center_fepresch  if college==1 || statefip:, or
estimates store d3

melogit male_bw year center_fepresch center_infant_care_pct  if college==1 || statefip:, or
estimates store d4

melogit male_bw year center_fepresch center_infant_care_pct center_cc_percent_served if college==1 || statefip:, or
estimates store d5

melogit male_bw year center_fepresch center_infant_care_pct center_cc_percent_served center_pct_workers_paid_leave if college==1 || statefip:, or
estimates store d6

melogit male_bw year center_fepresch center_infant_care_pct center_cc_percent_served center_pct_workers_paid_leave center_eitc_credit if college==1 || statefip:, or
estimates store d7

melogit male_bw year center_fepresch center_infant_care_pct center_cc_percent_served center_pct_workers_paid_leave center_eitc_credit f_tanf if college==1 || statefip:, or
estimates store d8

melogit male_bw year center_fepresch center_infant_care_pct center_cc_percent_served center_pct_workers_paid_leave center_eitc_credit f_tanf center_women_legis  if college==1 || statefip:, or
estimates store d9

melogit male_bw year center_fepresch center_infant_care_pct center_cc_percent_served center_pct_workers_paid_leave center_eitc_credit f_tanf center_women_legis center_dems_legis  if college==1 || statefip:, or
estimates store d10

melogit male_bw year center_fepresch center_infant_care_pct center_cc_percent_served center_pct_workers_paid_leave center_eitc_credit f_tanf center_women_legis center_dems_legis center_x_pov_rate if college==1 || statefip:, or
estimates store d11

melogit male_bw year center_fepresch center_infant_care_pct center_cc_percent_served center_pct_workers_paid_leave center_eitc_credit f_tanf center_women_legis center_dems_legis center_x_pov_rate leave_policy_score if college==1 || statefip:, or
estimates store d12

melogit male_bw year center_fepresch center_infant_care_pct center_cc_percent_served center_pct_workers_paid_leave center_eitc_credit f_tanf center_women_legis center_dems_legis center_x_pov_rate leave_policy_score abortion_rec if college==1 || statefip:, or
estimates store d13

melogit male_bw year center_fepresch center_infant_care_pct center_cc_percent_served center_pct_workers_paid_leave center_eitc_credit f_tanf center_women_legis center_dems_legis center_x_pov_rate abortion_rec if college==1 || statefip:, or // take out leave policy score
estimates store d14

melogit male_bw year center_fepresch center_infant_care_pct center_cc_percent_served center_pct_workers_paid_leave center_eitc_credit f_tanf center_women_legis center_dems_legis center_x_pov_rate abortion_rec i.leave_policy_score if college==1 || statefip:, or // leave policy as factor
estimates store d15

esttab d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 using "$results/lagged_multi-level_coll_VARIANCE.csv", star b(%9.5f) replace

********************************************************************************
* Think I really want to explain the gap/ but does the gap actually vary across states?
********************************************************************************
preserve
collapse (sum) male_bw dual_earn hh_count, by(statefip college)
restore
/////

log using "$logdir/multilevel_interactions.log", replace

melogit male_bw year i.college || statefip:, or
margins college
estimates store m1

melogit male_bw year c.center_fepresch##i.college || statefip:, or // 
margins college, at(center_fepresch=(-.25 -.10 0 .10 .25)) // deviation from mean - higher = more egal? lower = more traditional? (Confirm I reverse coded - was that higher = more traditional)
marginsplot
estimates store m2

melogit male_bw year c.center_infant_care_pct##i.college || statefip:, or
margins college, at(center_infant_care_pct=(-.10 -.05 0 .05 .10 .15)) // higher = more expensive? right that is how you interpret mean centered?
estimates store m3

melogit male_bw year c.center_cc_percent_served##i.college || statefip:, or
margins college, at(center_cc_percent_served=(-.15 -.08 0 .08 .15)) //
marginsplot
estimates store m4

melogit male_bw year c.center_pct_workers_paid_leave##i.college || statefip:, or
margins college, at(center_pct_workers_paid_leave=(-.05 0 .05)) //
marginsplot
estimates store m5

melogit male_bw year c.center_leave_policy_score##i.college || statefip:, or
margins college, at(center_leave_policy_score=(-25 -10 0 10 25 50 100)) // 
marginsplot
estimates store m6
// margins, dydx(college) at(center_leave_policy_score=(-25 -10 0 10 25 50 100))
// marginsplot

melogit male_bw year c.center_eitc_credit##i.college || statefip:, or
margins college, at(center_eitc_credit=(-.10 0 .10 .25 .50 .75)) 
estimates store m7
/*
melogit male_bw year c.center_eitc_credit i.college || statefip:, or // wanted to see what happened without interaction
margins college, at(center_eitc_credit=(-.10 0 .10 .25 .50 .75)) 
marginsplot
*/

melogit male_bw year c.center_dems_legis##i.college || statefip:, or
margins college, at(center_dems_legis=(-.30 -.15 0 .15 .30 .45)) 
marginsplot
estimates store m8

melogit male_bw year c.sc_f_tanf##i.college || statefip:, or
margins college, at(sc_f_tanf=(0 (0.2) 1)) 
marginsplot
estimates store m9

melogit male_bw year c.abortion##i.college || statefip:, or
margins college, at(abortion=(-3 (1) 2))
marginsplot
estimates store m10

melogit male_bw year c.center_childcare_gap##i.college || statefip:, or
margins college, at(center_childcare_gap=(-.70 -.50 -.30 0 .30 .50 .70 .90))
marginsplot
estimates store m11

melogit male_bw year c.center_x_all_pt##i.college || statefip:, or
margins college, at(center_x_all_pt=(-.06 -.03 0 .03 .06))
marginsplot
estimates store m12

melogit male_bw year c.center_x_all_prob##i.college || statefip:, or
margins college, at(center_x_all_prob=(-.01 0 .01))
marginsplot
estimates store m13

esttab m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 using "$results/lagged_multi-level_interactions.csv", star b(%9.5f) replace

log close