********************************************************************************
* Project: Work-family policy and divorce
* Merge state-level data and conduct analyses
* state-level-analysis-main-results.do
* Code owner: Kimberly McErlean
********************************************************************************

********************************************************************************
* First just get data and do final sample restrictions
********************************************************************************
use "$created_data/PSID_union_sample_dedup.dta", clear // created in step 5 in main folder

// Final sample restrictions
tab matrix_marr_num, m
tab matrix_rel_num, m
tab current_rel_number current_marr_number, m // this includes historical relationships

browse unique_id survey_yr partner_unique_id in_marital_history current_rel_type current_rel_number current_marr_number rel_start_yr_couple rel_end_yr_couple

// keep if matrix_marr_num==1
keep if current_rel_number==1 | current_marr_number==1
keep if (AGE_HEAD_>=18 & AGE_HEAD_<=55) &  (AGE_WIFE_>=18 & AGE_WIFE_<=55)

gen age_flag_2554=0 // robustness check for reviewer
replace age_flag_2554=1 if (AGE_HEAD_>=25 & AGE_HEAD_<=54) &  (AGE_WIFE_>=25 & AGE_WIFE_<=54)

gen age_flag_2055=0 // robustness check for reviewer
replace age_flag_2055=1 if (AGE_HEAD_>=20 & AGE_HEAD_<=55) &  (AGE_WIFE_>=20 & AGE_WIFE_<=55)

keep if inrange(rel_start_yr_couple,1995,2014)
	tab transition_year, m
keep if inlist(IN_UNIT,0,1,2)
drop if survey_yr==2021 // I don't know about covid and so since this is like PEAK COVID, not using (also 2021 wasn't available when I started this analysis LOL)
drop if STATE_==11 // DC is missing a lot of state variables, so need to remove.
drop if STATE_==0
drop if STATE_==99

// re: age
tab educ_type age_flag_2055, row // oh this is very interesting
tab current_rel_type age_flag_2055, row // this also. I mean, it's still small, but there is a demographic / relationship type selection into young parenthood for sure

// okay, let's create marriage specific start year and duration for now
gen marriage_start_yr = rel_start_yr_couple
replace marriage_start_yr = transition_year if ever_transition==1

gen marr_dur = survey_yr - marriage_start_yr // duh these are negative for the cohab years
tab marr_dur, m
tab marr_dur if current_rel_type==20, m

	// need to group long durations because of collinearity as well. Even worse of a problem for restricting to parents of young children, so needs to be closer to 15+
	gen marr_dur_raw = marr_dur
	replace marr_dur = 16 if marr_dur>=16 & marr_dur<1000

browse unique_id survey_yr partner_unique_id ever_transition current_rel_type current_rel_number current_marr_number rel_start_yr_couple rel_end_yr_couple dur marr_dur marriage_start_yr transition_year

// drop those with no earnings or housework hours the whole time
bysort unique_id: egen min_type = min(hh_earn_type_t1) // since no earners is 4, if the minimum is 4, means that was it the whole time
label values min_type hh_earn_type
sort unique_id survey_yr
browse unique_id survey_yr min_type hh_earn_type_t1

tab min_type // okay very few people had no earnings whole time
drop if min_type ==4

bysort id: egen min_hw_type = min(housework_bkt_t) // since no earners is 4, if the minimum is 4, means that was it the whole time
label values min_hw_type housework_bkt
sort unique_id survey_yr
browse unique_id survey_yr min_hw_type housework_bkt_t

tab min_hw_type // same here
drop if min_hw_type ==4

********************************************************************************
* Last variable creation / checks and data cleaning / set-up
********************************************************************************

// Migration status - wanted to wait until had final sample IDK why
label values STATE_ MOVED_ .
quietly unique STATE_ if STATE_!=., by(unique_id) gen(state_change)
bysort unique_id (state_change): replace state_change=state_change[1]
tab state_change, m

sort unique_id survey_yr 
browse unique_id partner_unique_id survey_yr STATE_ state_change MOVED_ MOVED_YEAR_

gen moved_states = .
replace moved_states = 0 if STATE_==STATE_[_n-1] & unique_id==unique_id[_n-1] & wave==wave[_n-1]+1
replace moved_states = 0 if state_change==1
replace moved_states = 1 if STATE_!=STATE_[_n-1] & unique_id==unique_id[_n-1] & wave==wave[_n-1]+1
replace moved_states = 0 if moved_states==. & state_change!=0 // remaining are first observations
tab moved_states, m

browse unique_id partner_unique_id survey_yr STATE_ state_change moved_states rel_start_all MOVED_ MOVED_YEAR_
tab state_change moved_states, m

gen moved_states_lag = .
replace moved_states_lag = 0 if STATE_==STATE_[_n+1] & unique_id==unique_id[_n+1] & wave==wave[_n+1]-1
replace moved_states_lag = 0 if state_change==1
replace moved_states_lag = 1 if STATE_!=STATE_[_n+1] & unique_id==unique_id[_n+1] & wave==wave[_n+1]-1
replace moved_states_lag = 0 if moved_states_lag==. & STATE_==STATE_[_n-1] & unique_id==unique_id[_n-1] & wave==wave[_n-1]+1 // last survey waves
replace moved_states_lag = 0 if moved_states_lag==. & state_change!=0 // remaining are last observations

tab moved_states_lag, m
tab moved_states moved_states_lag, m
tab state_change moved_states_lag, m

browse unique_id partner_unique_id survey_yr STATE_ state_change moved_states moved_states_lag rel_start_all dissolve MOVED_ MOVED_YEAR_

gen moved_last2=.
replace moved_last2 = 0 if moved_states_lag==0 & moved_states==0
replace moved_last2 = 1 if moved_states_lag==1 | moved_states==1

// create child indicator for under 6
gen children_under6=0
replace children_under6=1 if children==1 & AGE_YOUNG_CHILD_ < 6

* Final sample restrictions based on missing values and division of labor to divide
// missing value inspect
inspect age_mar_wife // 18
inspect age_mar_head // 3
// inspect raceth_head
inspect raceth_head_fixed // 39
inspect same_race // 0
inspect either_enrolled // 0
inspect region // 0 
inspect STATE_ // 0
inspect cohab_with_partner // 0
inspect cohab_with_other // 0 
inspect pre_marital_birth // 0
inspect home_owner // 0
inspect couple_joint_religion // this has the most missing (502 - about 3.5%)
inspect earnings_bucket_t1 // 0
inspect couple_earnings_t1 // 0 (underlying variable)
inspect couple_educ_gp // 360 (2.5%)
inspect educ_type // 424
inspect moved_last2 // 0
inspect num_children // 0
inspect hh_hours_type_t1 // 0
inspect housework_bkt_t // 196
inspect division_bucket_hrs_t1 // 196

// create flag
gen any_missing = 0
replace any_missing = 1 if age_mar_wife==. | age_mar_head==. | raceth_head_fixed==. | couple_joint_religion ==. | educ_type==. | housework_bkt_t==. | division_bucket_hrs_t1==.
tab any_missing , m // this is closer to 7%

// flag for no paid or unpaid labor to observe
gen no_labor = 0 
replace no_labor = 1 if hh_hours_type_t1==4 | housework_bkt_t ==4
tab no_labor, m

tab any_missing no_labor, m

// Time does not work as discrete for fixed effects (early and late years are being removed), so need to bucket
gen year_gp=. 
replace year_gp=1 if survey_yr<2000
replace year_gp=2 if survey_yr >=2000 & survey_yr < 2005
replace year_gp=3 if survey_yr >=2005 & survey_yr < 2010
replace year_gp=4 if survey_yr >=2010 & survey_yr < 2015
replace year_gp=5 if survey_yr >=2015 & survey_yr < 2020

label define year_gp 1 "Pre-2000s" 2 "2000-2005" 3 "2005-2010" 4 "2010-2015" 5 "2010-2020"
label values year_gp year_gp
tab year_gp

* Small things needed for analysis (run through this so you have controls and for figures)
set scheme cleanplots

global controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.educ_type i.moved_last2 i.couple_joint_religion i.num_children i.year_gp"

global macro_controls "women_college_rate_wt_t married_women_emp_rate_wt_t avg_egal_reg_t married_pure_male_bw_rate_t evang_rate_t"

gen in_analytical_sample = 0
replace in_analytical_sample = 1 if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0

/* Controls from R1
global controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.educ_type i.moved_last2 i.couple_joint_religion i.num_children"  // i.region knot1 knot2 knot3 

global macro_controls "women_college_rate_wt_t married_women_emp_rate_wt_t avg_egal_reg_t married_pure_male_bw_rate_t"
*/

// browse unique_id partner_unique_id year dissolve children_under6 current_rel_type marr_dur any_missing no_labor housework_bkt_t hh_hours_type_t1 hh_earn_type_t1 female_hours_pct_t1 weekly_hrs_t1_wife weekly_hrs_t1_head

********************************************************************************
**# Merge onto policy data
********************************************************************************
// note, did robustness checks with different time lags per reviewer comments

local scale_vars "structural_familism structural_factor cc_cost_orig cc_pct_income_orig cc_pct_inc_neg prek_enrolled_public cc_pct_served policy_lib_all policy_lib_econ policy_lib_soc gender_factor_reg fepresch_reg fechld_reg fefam_reg preschool_egal_reg working_mom_egal_reg genderroles_egal_reg avg_egal_reg fepresch_state fechld_state fefam_state gender_factor_state preschool_egal_state working_mom_egal_state genderroles_egal_state avg_egal_state evang_rate evang_lds_rate relig_rate married_dual_earn_rate married_pure_male_bw_rate women_emp_rate_wt married_women_emp_rate_wt maternal_u5_employment_wt min_amt_above_fed unemployment_percap wba_max high_inc_prem_pct low_inc_prem_pct earn_ratio married_earn_ratio welfare_all paid_leave abortion_protected educ_spend_percap headstart_pct headstart_pct_totalpop earlyhs_pct earlyhs_pct_totalpop total_headstart_pct total_headstart_pct_totalpop diffusion policy_group_v1 policy_group_v2 policy_group_v3 women_college_rate_wt married_women_college_rt_wt college_ratio_wt married_college_ratio_wt sf_cc_income sf_ccdf_served sf_head_start sf_early_hs sf_total_hs sf_educ_spend broad_policy family_investment sf_childcare sf_policy sf_childcare_wt sex_ratio_marriage_wt men_unemp_rate_wt" 

rename STATE_ state_fips
rename survey_yr year

// merge m:1 state_fips year using "$raw_state_data/structural_familism_june25_int.dta" // old file
merge m:1 state_fips year using "$created_data/scale_refresh.dta", keepusing(`scale_vars')
drop if _merge==2
drop _merge

foreach var in `scale_vars'{
	rename `var' `var'_t
}

gen year_t1 = year - 1
merge m:1 year_t1 state_fips using "$created_data/scale_refresh.dta", keepusing(`scale_vars')
drop if _merge==2
drop _merge

foreach var in `scale_vars'{
	rename `var' `var'_t1
}

gen year_t2 = year - 2
merge m:1 year_t2 state_fips using "$created_data/scale_refresh.dta", keepusing(`scale_vars')
drop if _merge==2
drop _merge

foreach var in `scale_vars'{
	rename `var' `var'_t2
}

// forward lags
gen year_tf2 = year + 2
gen year_tf4 = year + 4 // some of these won't have matches because I only have data through 2021, but 2019 + 4 = 2023

merge m:1 year_tf2 state_fips using "$created_data/scale_refresh.dta", keepusing(`scale_vars')
drop if _merge==2
drop _merge

foreach var in `scale_vars'{
	rename `var' `var'_tf2
}

merge m:1 year_tf4 state_fips using "$created_data/scale_refresh.dta", keepusing(`scale_vars')
drop if _merge==2
tab year _merge // should be 2019 - yes okay
drop _merge

foreach var in `scale_vars'{
	rename `var' `var'_tf4
}

pwcorr structural_familism_t structural_familism_t1 structural_familism_t2 structural_familism_tf2 structural_familism_tf4

**# Save for use in other data files - this way I don't need to ensure if I change something here, I change everywhere. THIS BECOMES THE MAIN FILE
save "$created_data/PSID_union_sample_with_policy.dta", replace 


////////////////////////////////////////////////////////////////////////////////
********************************************************************************
********************************************************************************
********************************************************************************
**# Analysis starts
********************************************************************************
********************************************************************************
********************************************************************************
////////////////////////////////////////////////////////////////////////////////

use "$created_data/PSID_union_sample_with_policy.dta", clear

global controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.educ_type i.moved_last2 i.couple_joint_religion i.num_children i.year_gp"

global macro_controls "women_college_rate_wt_t married_women_emp_rate_wt_t avg_egal_reg_t married_pure_male_bw_rate_t evang_rate_t"


********************************************************************************
********************************************************************************
********************************************************************************
**# Main Effects (Married Couples Only)
********************************************************************************
********************************************************************************
********************************************************************************

********************************************************************************
* Total Sample
********************************************************************************
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins hh_hours_type_t1
margins, dydx(hh_hours_type_t1) level(95) post
estimates store est1a

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins division_bucket_hrs_t1
margins, dydx(division_bucket_hrs_t1) level(95) post
estimates store est2a

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(structural_familism_t) post
estimates store est3a

 // just hours and maybe add structural support
coefplot (est1a, offset(.20) nokey) (est2a, offset(-.20) nokey) (est3a, nokey lcolor("black") mcolor("black") ciopts(color("black"))) , drop(_cons) xline(0) levels(95) base xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners"  1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" structural_familism_t = "Structural Support Scale") ///
 headings(1.hh_hours_type_t1= `""{bf:Division of Work Hours}" "{it:(Model 1)}""'  1.division_bucket_hrs_t1 = `""{bf:Combined Paid and Unpaid Labor}" "{it:(Model 2)}""' structural_familism_t1 = `""{bf:Structural Support for Working Families}" "{it:(Model 2)}"')
 

********************************************************************************
* All parents
********************************************************************************
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 $controls $macro_controls if children==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins hh_hours_type_t1
margins, dydx(hh_hours_type_t1) level(95) post
estimates store est1b

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls $macro_controls if children==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins division_bucket_hrs_t1
margins, dydx(division_bucket_hrs_t1) level(95) post
estimates store est2b

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls $macro_controls if children==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(structural_familism_t) post
estimates store est3b
 
 // just hours and maybe add structural support
coefplot (est1b, offset(.20) nokey) (est2b, offset(-.20) nokey) (est3b, nokey lcolor("black") mcolor("black") ciopts(color("black"))) , drop(_cons) xline(0) levels(95) base xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners"  1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" structural_familism_t = "Structural Support Scale") ///
 headings(1.hh_hours_type_t1= `""{bf:Division of Work Hours}" "{it:(Model 1)}""'  1.division_bucket_hrs_t1 = `""{bf:Combined Paid and Unpaid Labor}" "{it:(Model 2)}""' structural_familism_t1 = `""{bf:Structural Support for Working Families}" "{it:(Model 2)}"')
 
********************************************************************************
* All Parents of children under the age of 6
********************************************************************************
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(hh_hours_type_t1) level(95) post
estimates store est1c

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1) level(95) post
estimates store est2c

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(structural_familism_t) post
estimates store est3c

 // just hours and maybe add structural support
coefplot (est1c, offset(.20) nokey) (est2c, offset(-.20) nokey) (est3c, nokey lcolor("black") mcolor("black") ciopts(color("black"))) , drop(_cons) xline(0) levels(95) base xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners"  1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" structural_familism_t = "Structural Support Scale") ///
 headings(1.hh_hours_type_t1= `""{bf:Division of Work Hours}" "{it:(Model 1)}""'  1.division_bucket_hrs_t1 = `""{bf:Combined Paid and Unpaid Labor}" "{it:(Model 2)}""' structural_familism_t = `""{bf:Structural Support for Working Families}" "{it:(Model 2)}"')
 
********************************************************************************
* Create a figure across all three
********************************************************************************
// https://www.statalist.org/forums/forum/general-stata-discussion/general/1488181-title-size-in-coefplot-graphs

// no structural support
coefplot (est1a, nokey) (est2a, nokey), bylabel("Total Sample")  || ///
		est1b est2b, bylabel("All Parents")  || ///
		est1c est2c, bylabel("Parents of Young Children"),  ///
, byopts(rows(1) xrescale) drop(_cons 1.hh_hours_type_t1 1.division_bucket_hrs_t1) legend(position(bottom) rows(1)) ///
xline(0, lstyle(solid) lwidth(thin)) levels(90) base xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner"  1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others", labsize(small)) ///
 headings(2.hh_hours_type_t1= `""{bf:Division of Work Hours}" "{it:(Model 1)}""'  2.division_bucket_hrs_t1 = `""{bf:Combined Division of Labor}" "{it:(Model 2)}""', labsize(small)) ///
 subtitle(, size(small))
 
// with structural support
 coefplot (est1a, nokey) (est2a, nokey) (est3a, nokey), bylabel("Total Sample")  || ///
		est1b est2b est3b, bylabel("All Parents")  || ///
		est1c est2c est3c, bylabel("Parents of Young Children"),  ///
, byopts(rows(1) xrescale) drop(_cons 1.hh_hours_type_t1 1.division_bucket_hrs_t1) legend(position(bottom) rows(1)) ///
xline(0, lstyle(solid) lwidth(thin)) levels(90) base xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner"  1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" structural_familism_t = "Structural Support Scale", labsize(small)) ///
 headings(2.hh_hours_type_t1= `""{bf:Division of Work Hours}" "{it:(Model 1)}""'  2.division_bucket_hrs_t1 = `""{bf:Combined Division of Labor}" "{it:(Model 2)}""' structural_familism_t = `""{bf:Structural Support}" "{it:(Model 2)}"', labsize(small)) ///
 subtitle(, size(small))
 
********************************************************************************
* Export ORs for Table 3
********************************************************************************
label values marr_dur $macro_controls . 

// total sample
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
outreg2 using "$results/main results/dissolution_OR_main_effects.xls", sideway stats(coef se pval) ctitle(Total Sample 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace // label

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
outreg2 using "$results/main results/dissolution_OR_main_effects.xls", sideway stats(coef se pval) ctitle(Total Sample 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// all parents
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 $controls $macro_controls if children==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
outreg2 using "$results/main results/dissolution_OR_main_effects.xls", sideway stats(coef se pval) ctitle(Parents 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls $macro_controls if children==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
outreg2 using "$results/main results/dissolution_OR_main_effects.xls", sideway stats(coef se pval) ctitle(Parents 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// child under 6
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(hh_hours_type_t1)
outreg2 using "$results/main results/dissolution_OR_main_effects.xls", sideway stats(coef se pval) ctitle(Under6 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
outreg2 using "$results/main results/dissolution_OR_main_effects.xls", sideway stats(coef se pval) ctitle(Under6 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// should I include interactions actually?
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
outreg2 using "$results/main results/dissolution_OR_main_effects.xls", sideway stats(coef se pval) ctitle(Under6 Int1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
outreg2 using "$results/main results/dissolution_OR_main_effects.xls", sideway stats(coef se pval) ctitle(Under6 Int2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

********************************************************************************
********************************************************************************
********************************************************************************
**# Interactions (Parents of Children under 6)
********************************************************************************
********************************************************************************
********************************************************************************

/// Correlation matrix of the macro-level factors. wait do I want these here or at state level? I think I actually want at state-level (so file A)
pwcorr structural_familism_t women_college_rate_wt_t married_women_emp_rate_wt_t married_pure_male_bw_rate_t avg_egal_reg_t

// updates for R2: add religion as other macro-indicator
// add two mediation tests: women's earning, total couple work burden

********************************************************************************
* Let's do Male BW first
********************************************************************************

******************
** Interactions
******************

// Main model - just control for macro factors, but not yet interacted
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-paid.xls", ctitle(Main Model) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

// Interact women's degree attainment
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.women_college_rate_wt_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-paid.xls", ctitle(college: familism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.women_college_rate_wt_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum women_college_rate_wt_t, detail
margins, dydx(hh_hours_type_t1) at(women_college_rate_wt_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-paid.xls", ctitle(college: college) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// women's employment
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.married_women_emp_rate_wt_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-paid.xls", ctitle(emp: familism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.married_women_emp_rate_wt_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum married_women_emp_rate_wt_t, detail
margins, dydx(hh_hours_type_t1) at(married_women_emp_rate_wt_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-paid.xls", ctitle(emp: emp) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// male BW rate
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.married_pure_male_bw_rate_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-paid.xls", ctitle(male bw: familism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.married_pure_male_bw_rate_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum married_pure_male_bw_rate_t, detail
margins, dydx(hh_hours_type_t1) at(married_pure_male_bw_rate_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-paid.xls", ctitle(male bw: male bw) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// regional gender norms
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.avg_egal_reg_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-paid.xls", ctitle(norms: familism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.avg_egal_reg_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum avg_egal_reg_t, detail
margins, dydx(hh_hours_type_t1) at(avg_egal_reg_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-paid.xls", ctitle(norms: norms) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// evangelicalism
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.evang_rate_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-paid.xls", ctitle(relig: familism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.evang_rate_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum evang_rate_t, detail
margins, dydx(hh_hours_type_t1) at(evang_rate_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-paid.xls", ctitle(relig: relig) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// Mediation: women's earnings
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-paid.xls", ctitle(mediate: dependence) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// Mediation: total work burden
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 total_work_couple $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-paid.xls", ctitle(mediate: time) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

******************
** Main Effects
******************
// main
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(hh_hours_type_t1) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-paid.xls", ctitle(main effects: main) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// women's degree attainment
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.women_college_rate_wt_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(hh_hours_type_t1) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-paid.xls", ctitle(main effects: college) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// women's employment
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.married_women_emp_rate_wt_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(hh_hours_type_t1)  post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-paid.xls", ctitle(main effects: emp) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// male BW rate
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.married_pure_male_bw_rate_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id) 
margins, dydx(hh_hours_type_t1)  post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-paid.xls", ctitle(main effects: male BW) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// regional gender norms
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.avg_egal_reg_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(hh_hours_type_t1)  post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-paid.xls", ctitle(main effects: norms) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// evangelicalism
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.evang_rate_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(hh_hours_type_t1)  post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-paid.xls", ctitle(main effects: relig) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// Mediation: women's earnings
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(hh_hours_type_t1)  post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-paid.xls", ctitle(main effects: dependence) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(earnings_wife_1000s)  post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-paid.xls", ctitle(dependence: earnings main) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// Mediation: total work burden
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 total_work_couple $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(hh_hours_type_t1)  post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-paid.xls", ctitle(main effects: time) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 total_work_couple $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(total_work_couple)  post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-paid.xls", ctitle(time: time main) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

********************************************************************************
* Then combined DoL
********************************************************************************

******************
** Interactions
******************

// Main model - just control for macro factors, but not yet interacted
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-unpaid.xls", ctitle(Main Model) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

// Interact women's degree attainment
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.women_college_rate_wt_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-unpaid.xls", ctitle(college: familism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.women_college_rate_wt_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum women_college_rate_wt_t, detail
margins, dydx(division_bucket_hrs_t1) at(women_college_rate_wt_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-unpaid.xls", ctitle(college: college) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// women's employment
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.married_women_emp_rate_wt_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-unpaid.xls", ctitle(emp: familism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.married_women_emp_rate_wt_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum married_women_emp_rate_wt_t, detail
margins, dydx(division_bucket_hrs_t1) at(married_women_emp_rate_wt_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-unpaid.xls", ctitle(emp: emp) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// male BW rate
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.married_pure_male_bw_rate_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-unpaid.xls", ctitle(male bw: familism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.married_pure_male_bw_rate_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum married_pure_male_bw_rate_t, detail
margins, dydx(division_bucket_hrs_t1) at(married_pure_male_bw_rate_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-unpaid.xls", ctitle(male bw: male bw) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// regional gender norms
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.avg_egal_reg_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-unpaid.xls", ctitle(norms: familism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.avg_egal_reg_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum avg_egal_reg_t, detail
margins, dydx(division_bucket_hrs_t1) at(avg_egal_reg_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-unpaid.xls", ctitle(norms: norms) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// evangelicalism
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.evang_rate_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-unpaid.xls", ctitle(relig: familism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.evang_rate_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum evang_rate_t, detail
margins, dydx(division_bucket_hrs_t1) at(evang_rate_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-unpaid.xls", ctitle(relig: relig) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// Mediation: women's earnings
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-unpaid.xls", ctitle(mediate: dependence) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// Mediation: total work burden
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 total_work_couple $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-unpaid.xls", ctitle(mediate: time) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

******************
** Main Effects
******************
// main
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-unpaid.xls", ctitle(main effects: main) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// women's degree attainment
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.women_college_rate_wt_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-unpaid.xls", ctitle(main effects: college) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// women's employment
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.married_women_emp_rate_wt_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-unpaid.xls", ctitle(main effects: emp) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// male BW rate
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.married_pure_male_bw_rate_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-unpaid.xls", ctitle(main effects: male bw) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// regional gender norms
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.avg_egal_reg_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-unpaid.xls", ctitle(main effects: norms) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// evangelicalism
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.evang_rate_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-unpaid.xls", ctitle(main effects: relig) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// Mediation: women's earnings
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1)  post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-unpaid.xls", ctitle(main effects: dependence) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(earnings_wife_1000s)  post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-unpaid.xls", ctitle(dependence: earnings main) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// Mediation: total work burden
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 total_work_couple $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1)  post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-unpaid.xls", ctitle(main effects: time) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 total_work_couple $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(total_work_couple)  post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6-unpaid.xls", ctitle(time: time main) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

********************************************************************************
* Get  predicted probabilities to discuss in text
********************************************************************************
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(hh_hours_type_t1)
margins hh_hours_type_t1

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1)
margins division_bucket_hrs_t1

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins hh_hours_type_t1, at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins division_bucket_hrs_t1, at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

********************************************************************************
* Then I want the two normal figures (interaction - each type of labor)
********************************************************************************
* Paid Labor: Predicted Probability
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins hh_hours_type_t1, at(structural_familism_t=(`r(min)'(1)`r(max)'))
marginsplot, xtitle("Structural Support for Working Families") ytitle("Predicted Probability of Marital Dissolution") title("") legend(position(6) ring(3) rows(1)) noci recast(line) xlabel(#10) plot2opts(lcolor("navy") mcolor("navy")) plot3opts(lcolor("ltblue") mcolor("ltblue"))

* Paid Labor: AMEs
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

estimates store paid_m

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(3.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

estimates store paid_f

coefplot (paid_m, mcolor(navy) ciopts(color(navy)) label("Male BW")) (paid_f, label("Female BW")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical))

* Combined: Predicted Probability
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins division_bucket_hrs_t1, at(structural_familism_t=(`r(min)'(1)`r(max)'))
marginsplot, xtitle("Structural Support for Working Families") ytitle("Predicted Probability of Marital Dissolution") title("") legend(position(6) ring(3) rows(1)) noci recast(line) xlabel(#10) plot2opts(lcolor("blue") mcolor("blue")) plot3opts(lcolor("ltblue") mcolor("ltblue")) plot4opts(lcolor("gs8") mcolor("gs8")) plot5opts(lcolor("black") mcolor("black"))  

* Combined: AMEs
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est10

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(3.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est11

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(4.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est12

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(5.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est13

coefplot (est10, mcolor(blue) ciopts(color(blue)) label("Traditional")) (est11, label("Counter-Traditional")) (est12, label("Second Shift"))  (est13, label("All Others")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical)) xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) legend(position(bottom) rows(1)) xscale(range(-.2 .2)) xlabel(-.2(.1).2)


////////////////////////////////////////////////////////////////////////////////
// Appendix but discussed in main text
////////////////////////////////////////////////////////////////////////////////

********************************************************************************
********************************************************************************
********************************************************************************
**# Figures for Alternative Samples
* (these technically appendix but considering main results since I mention)
********************************************************************************
********************************************************************************
********************************************************************************

*******************************
* All Parents
*******************************
** Main effects (did above, but need to see how I will display, so let's just do again)
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 $controls $macro_controls if children==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(hh_hours_type_t1) level(95) post
estimates store p_esta

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls $macro_controls if children==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1) level(95) post
estimates store p_estb

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls $macro_controls if children==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(structural_familism_t) post
estimates store p_estc

* Interaction with Paid Labor
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store p_est1

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(3.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store p_est2

** Combined DoL (Hours)
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store p_est3

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(3.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store p_est4

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(4.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store p_est5

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(5.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store p_est6

*******************************
* Total Sample
*******************************
** Main effects
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(hh_hours_type_t1) level(95) post
estimates store t_esta

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1) level(95) post
estimates store t_estb

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(structural_familism_t) post
estimates store t_estc

* Interaction with Paid Labor
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store t_est1

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(3.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store t_est2

** Combined DoL (Hours)
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store t_est3

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(3.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store t_est4

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(4.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store t_est5

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(5.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store t_est6

*******************************
* Combined Figures
*******************************
* Main effects
/// coefplot (est2, offset(.20) nokey) (est4, offset(-.20) nokey) (esta, nokey lcolor("black") mcolor("black") ciopts(color("black"))), bylabel("Parents of Young Children")

coefplot (p_esta, offset(.20) nokey) (p_estb, offset(-.20) nokey) (p_estc, nokey lcolor("black") mcolor("black") ciopts(color("black"))), bylabel("All Parents")  || ///
(t_esta, offset(.20) nokey) (t_estb, offset(-.20) nokey) (t_estc, nokey lcolor("black") mcolor("black") ciopts(color("black"))), bylabel("Total Sample") || ///
, drop(_cons) xline(0) levels(95) base xtitle(Average Marginal Effects, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners"  1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" structural_familism_t = "Structural Support Scale") xsize(8) ///
 headings(1.hh_hours_type_t1= "{bf:Division of Work Hours}" 1.division_bucket_hrs_t1 = "{bf:Combined DoL}" structural_familism_t = "{bf:Structural Support}")
 
* Paid Labor
coefplot (p_est1, mcolor(navy) ciopts(color(navy)) label("Male BW")) (p_est2, label("Female BW")), bylabel("All Parents") || ///
		(t_est1, mcolor(navy) ciopts(color(navy)) label("Male BW")) (t_est2, label("Female BW")), bylabel("Total Sample") || ///
,  drop(_cons) nolabel xline(0, lcolor("red")) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical)) xsize(8) xscale(range(-.1 .1)) xlabel(-.1(.05).1)

* Combined DoL
coefplot (p_est3, mcolor(blue) ciopts(color(blue)) label("Traditional")) (p_est4, label("Counter-Traditional")) (p_est5, label("Second Shift"))  (p_est6, label("All Others")), bylabel("All Parents") || ///
(t_est3, mcolor(blue) ciopts(color(blue)) label("Traditional")) (t_est4, label("Counter-Traditional")) (t_est5, label("Second Shift") mcolor(gs8) ciopts(color(gs8)))  (t_est6, label("All Others") mcolor(black) ciopts(color(black))), bylabel("Total Sample")  || ///
,  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical)) xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) legend(position(bottom) rows(1)) xsize(8) xscale(range(-.2 .1)) xlabel(-.2(.1).1)


********************************************************************************
********************************************************************************
********************************************************************************
**# Individual indicators of scale
********************************************************************************
********************************************************************************
********************************************************************************

// scale, for reference: paid_leave_st prek_enrolled_public_st min_amt_above_fed_st earn_ratio_st unemployment_percap_st abortion_protected_st welfare_all_st


********************************************************************************
*Paid Work Hours
********************************************************************************
* Main model
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details.xls", ctitle(main) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

* Paid Leave
logit dissolve i.marr_dur i.paid_leave_t i.hh_hours_type_t1 i.paid_leave_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(2.hh_hours_type_t1) at(paid_leave_t=(0 1)) post // had to update bc #3 is collinnear
outreg2 using "$results/main results/dissolution_AMES_scale_details.xls", ctitle(paidleave) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* PreK Enrollment
logit dissolve i.marr_dur c.prek_enrolled_public_t i.hh_hours_type_t1 c.prek_enrolled_public_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum prek_enrolled_public_t, detail
margins, dydx(hh_hours_type_t1) at(prek_enrolled_public_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details.xls", ctitle(prek) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Min Wage
logit dissolve i.marr_dur c.min_amt_above_fed_t i.hh_hours_type_t1 c.min_amt_above_fed_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum min_amt_above_fed_t, detail
margins, dydx(hh_hours_type_t1) at(min_amt_above_fed_t=(`r(p5)' `r(p50)' `r(p75)' `r(p95)' `r(p99)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details.xls", ctitle(minwage) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Earnings Ratio
logit dissolve i.marr_dur c.earn_ratio_t i.hh_hours_type_t1 c.earn_ratio_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum earn_ratio_t, detail
margins, dydx(hh_hours_type_t1) at(earn_ratio_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details.xls", ctitle(earnings) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Unemployment Compensation
logit dissolve i.marr_dur c.unemployment_percap_t i.hh_hours_type_t1 c.unemployment_percap_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum unemployment_percap_t, detail
margins, dydx(hh_hours_type_t1) at(unemployment_percap_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details.xls", ctitle(unemployment) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Abortion protected
logit dissolve i.marr_dur i.abortion_protected_t i.hh_hours_type_t1 i.abortion_protected_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(hh_hours_type_t1) at(abortion_protected_t=(0 1)) post
outreg2 using "$results/main results/dissolution_AMES_scale_details.xls", ctitle(abortion) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Welfare Expenditures
logit dissolve i.marr_dur c.welfare_all_t i.hh_hours_type_t1 c.welfare_all_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum welfare_all_t, detail
margins, dydx(hh_hours_type_t1) at(welfare_all_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details.xls", ctitle(welfare) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Childcare latent indicator
logit dissolve i.marr_dur c.family_investment_t i.hh_hours_type_t1 c.family_investment_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum family_investment_t, detail
margins, dydx(hh_hours_type_t1) at(family_investment_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details.xls", ctitle(childcare latent) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* WFP latent indicator
logit dissolve i.marr_dur c.broad_policy_t i.hh_hours_type_t1 c.broad_policy_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum broad_policy_t, detail
margins, dydx(hh_hours_type_t1) at(broad_policy_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details.xls", ctitle(policy latent) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Structural scale using Factor Analysis
logit dissolve i.marr_dur c.structural_factor_t i.hh_hours_type_t1 c.structural_factor_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_factor_t, detail
margins, dydx(hh_hours_type_t1) at(structural_factor_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details.xls", ctitle(sf latent) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Additive Scale, but with more childcare measures (so essentially does over-emphasis childcare)
logit dissolve i.marr_dur c.sf_childcare_wt_t i.hh_hours_type_t1 c.sf_childcare_wt_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum sf_childcare_wt_t, detail
margins, dydx(hh_hours_type_t1) at(sf_childcare_wt_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details.xls", ctitle(scale + cc) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Policy liberalism - do I need *this* to support the broad policy environment argument? because - someone else made this, not me
logit dissolve i.marr_dur c.policy_lib_all_t i.hh_hours_type_t1 c.policy_lib_all_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum policy_lib_all_t, detail
margins, dydx(hh_hours_type_t1) at(policy_lib_all_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details.xls", ctitle(policy lib) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

********************************************************************************
*Combined DoL
********************************************************************************
* Main model
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details.xls", ctitle(main) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Paid Leave
logit dissolve i.marr_dur i.paid_leave_t i.division_bucket_hrs_t1 i.paid_leave_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(2.division_bucket_hrs_t1 4.division_bucket_hrs_t1) at(paid_leave_t=(0 1)) post
outreg2 using "$results/main results/dissolution_AMES_scale_details.xls", ctitle(paidleave) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* PreK Enrollment
logit dissolve i.marr_dur c.prek_enrolled_public_t i.division_bucket_hrs_t1 c.prek_enrolled_public_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum prek_enrolled_public_t, detail
margins, dydx(division_bucket_hrs_t1) at(prek_enrolled_public_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details.xls", ctitle(prek) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Min Wage
logit dissolve i.marr_dur c.min_amt_above_fed_t i.division_bucket_hrs_t1 c.min_amt_above_fed_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum min_amt_above_fed_t, detail
margins, dydx(division_bucket_hrs_t1) at(min_amt_above_fed_t=(`r(p5)' `r(p50)' `r(p75)' `r(p95)' `r(p99)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details.xls", ctitle(minwage) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Earnings Ratio
logit dissolve i.marr_dur c.earn_ratio_t i.division_bucket_hrs_t1 c.earn_ratio_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum earn_ratio_t, detail
margins, dydx(division_bucket_hrs_t1) at(earn_ratio_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details.xls", ctitle(earnings) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Unemployment Compensation
logit dissolve i.marr_dur c.unemployment_percap_t i.division_bucket_hrs_t1 c.unemployment_percap_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum unemployment_percap_t, detail
margins, dydx(division_bucket_hrs_t1) at(unemployment_percap_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details.xls", ctitle(unemployment) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Abortion protected
logit dissolve i.marr_dur i.abortion_protected_t i.division_bucket_hrs_t1 i.abortion_protected_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(2.division_bucket_hrs_t1 4.division_bucket_hrs_t1 5.division_bucket_hrs_t1) at(abortion_protected_t=(0 1)) post
outreg2 using "$results/main results/dissolution_AMES_scale_details.xls", ctitle(abortion) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Welfare Expenditures
logit dissolve i.marr_dur c.welfare_all_t i.division_bucket_hrs_t1 c.welfare_all_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum welfare_all_t, detail
margins, dydx(division_bucket_hrs_t1) at(welfare_all_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details.xls", ctitle(welfare) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Childcare latent indicator
logit dissolve i.marr_dur c.family_investment_t i.division_bucket_hrs_t1 c.family_investment_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum family_investment_t, detail
margins, dydx(division_bucket_hrs_t1) at(family_investment_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details.xls", ctitle(childcare latent) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* WFP latent indicator
logit dissolve i.marr_dur c.broad_policy_t i.division_bucket_hrs_t1 c.broad_policy_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum broad_policy_t, detail
margins, dydx(division_bucket_hrs_t1) at(broad_policy_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details.xls", ctitle(policy latent) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Structural scale using Factor Analysis
logit dissolve i.marr_dur c.structural_factor_t i.division_bucket_hrs_t1 c.structural_factor_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_factor_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_factor_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details.xls", ctitle(sf latent) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Additive Scale, but with more childcare measures (so essentially does over-emphasis childcare)
logit dissolve i.marr_dur c.sf_childcare_wt_t i.division_bucket_hrs_t1 c.sf_childcare_wt_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum sf_childcare_wt_t, detail
margins, dydx(division_bucket_hrs_t1) at(sf_childcare_wt_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details.xls", ctitle(scale + cc) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Policy liberalism
logit dissolve i.marr_dur c.policy_lib_all_t i.division_bucket_hrs_t1 c.policy_lib_all_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum policy_lib_all_t, detail
margins, dydx(division_bucket_hrs_t1) at(policy_lib_all_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details.xls", ctitle(policy lib) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

********************************************************************************
********************************************************************************
********************************************************************************
**# Results by level of education (Figures)
********************************************************************************
********************************************************************************
********************************************************************************

********************************************************************************
* College (At least one)
* Okay, attempted to add both but sample is far too small (bc too much becomes collinear or perfectly predictive)
********************************************************************************
** Main effects
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0  & couple_educ_gp==1, or cluster(couple_id)
margins, dydx(hh_hours_type_t1) level(95) post
estimates store c_esta

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & couple_educ_gp==1, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1) level(95) post
estimates store c_estb

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & couple_educ_gp==1, or cluster(couple_id)
margins, dydx(structural_familism_t) post
estimates store c_estc

** Paid Labor interaction
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & couple_educ_gp==1, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est5c

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & couple_educ_gp==1, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(3.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est6c

coefplot (est5c, mcolor(navy) ciopts(color(navy)) label("Male BW")) (est6c, label("Female BW")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical))

** Combined DoL (Hours) Interaction
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & couple_educ_gp==1, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est10c

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & couple_educ_gp==1, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(3.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est11c

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & couple_educ_gp==1, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(4.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est12c

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & couple_educ_gp==1, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(5.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est13c

coefplot (est10c, mcolor(blue) ciopts(color(blue)) label("Traditional")) (est12c, label("Second Shift") mcolor(gs8) ciopts(color(gs8)))  (est13c, label("All Others") mcolor(black) ciopts(color(black))),  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical)) xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) legend(position(bottom) rows(1)) // (est11c, label("Counter-Traditional"))

********************************************************************************
* No College
********************************************************************************
** Main effects
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0  & couple_educ_gp==0, or cluster(couple_id)
margins, dydx(hh_hours_type_t1) level(95) post
estimates store n_esta

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & couple_educ_gp==0, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1) level(95) post
estimates store n_estb

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & couple_educ_gp==0, or cluster(couple_id)
margins, dydx(structural_familism_t) post
estimates store n_estc

** Paid Labor interaction
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & couple_educ_gp==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est5n

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & couple_educ_gp==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(3.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est6n

coefplot (est5n, mcolor(navy) ciopts(color(navy)) label("Male BW")) (est6n, label("Female BW")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical))

** Combined DoL (Hours) Interaction
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & couple_educ_gp==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est10n

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & couple_educ_gp==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(3.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est11n

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & couple_educ_gp==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(4.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est12n

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & couple_educ_gp==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(5.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est13n

coefplot (est10n, mcolor(blue) ciopts(color(blue)) label("Traditional")) (est11n, label("Counter-Traditional")) (est12n, label("Second Shift"))  (est13n, label("All Others")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical)) xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) legend(position(bottom) rows(1))

********************************************************************************
* Combined Figures
********************************************************************************
* Main effects
// without scale
coefplot (n_esta, offset(.20) nokey) (n_estb, offset(-.20) nokey), bylabel("No College Degree")  || ///
(c_esta, offset(.20) nokey) (c_estb, offset(-.20) nokey), bylabel("One Partner Has College Degree") || ///
, drop(_cons) xline(0) levels(95) base xtitle(Average Marginal Effects, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners"  1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others") xsize(8) ///
 headings(1.hh_hours_type_t1= "{bf:Division of Work Hours}" 1.division_bucket_hrs_t1 = "{bf:Combined DoL}")

// with scale
 coefplot (n_esta, offset(.20) nokey) (n_estb, offset(-.20) nokey) (n_estc, nokey lcolor("black") mcolor("black") ciopts(color("black"))), bylabel("No College Degree")  || ///
(c_esta, offset(.20) nokey) (c_estb, offset(-.20) nokey) (c_estc, nokey lcolor("black") mcolor("black") ciopts(color("black"))), bylabel("One Partner Has College Degree") || ///
, drop(_cons) xline(0) levels(95) base xtitle(Average Marginal Effects, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners"  1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" structural_familism_t = "Structural Support Scale") xsize(8) ///
 headings(1.hh_hours_type_t1= "{bf:Division of Work Hours}" 1.division_bucket_hrs_t1 = "{bf:Combined DoL}" structural_familism_t = "{bf:Structural Support}")
 
* Paid Labor
coefplot (est5n, mcolor(navy) ciopts(color(navy)) label("Male BW")) (est6n, label("Female BW")), bylabel("No College Degree") || ///
		(est5c, mcolor(navy) ciopts(color(navy)) label("Male BW")) (est6c, label("Female BW")), bylabel("One Partner Has College  Degree") || ///
,  drop(_cons) nolabel xline(0, lcolor("red")) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical)) xsize(8)

/*
Alt for comparison
	coefplot (est5n, mcolor(navy) ciopts(color(navy) lpattern("---")) label("No College: Male BW")) (est5c, mcolor(navy) ciopts(color(navy)) label("College: Male BW")) ///
	(est6n, mcolor(ltblue) ciopts(color(ltblue) lpattern("---")) label("No College: Female BW")) (est6c, mcolor(ltblue) ciopts(color(ltblue)) label("College: Female BW")) ///
	,  drop(_cons) nolabel xline(0, lcolor("red")) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical)) xsize(7)
*/

* Combined DoL
coefplot (est10n, mcolor(blue) ciopts(color(blue)) label("Traditional")) (est11n, label("Counter-Traditional")) (est12n, label("Second Shift"))  (est13n, label("All Others")), bylabel("No College Degree") || ///
(est10c, mcolor(blue) ciopts(color(blue)) label("Traditional")) (est11c, label("Counter-Traditional")) (est12c, label("Second Shift") mcolor(gs8) ciopts(color(gs8)))  (est13c, label("All Others") mcolor(black) ciopts(color(black))), bylabel("One Partner Has College  Degree")  || ///
,  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical)) xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) legend(position(bottom) rows(1)) xsize(8) // byopts(xrescale)

/*
Alt for comparison
	coefplot (est10n, mcolor(blue) ciopts(color(blue) lpattern("---")) label("No College: Trad")) (est10c, mcolor(blue) ciopts(color(blue)) label("College: Trad")) ///
	(est12n, mcolor(black) ciopts(color(black) lpattern("---")) label("No College: Second Shift")) (est12c, mcolor(black) ciopts(color(black)) label("College: Second Shift")) ///
	,  drop(_cons) nolabel xline(0, lcolor("red")) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical))
*/

********************************************************************************
********************************************************************************
********************************************************************************
**# Descriptive statistics
********************************************************************************
********************************************************************************
********************************************************************************

/* for ref:
global controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.educ_type i.moved_last2 i.couple_joint_religion i.num_children i.year_gp"  // add year here.

global macro_controls "women_college_rate_wt_t married_women_emp_rate_wt_t avg_egal_reg_t married_pure_male_bw_rate_t evang_rate_t" // add religion here 

logit dissolve i.marr_dur if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)

*/

tab hh_hours_type_t1, gen(hours_type)
tab division_bucket_hrs_t1, gen(combined_hours)
tab educ_type, gen(educ_type)
tab raceth_head_fixed, gen(race_head)
tab raceth_wife_fixed, gen(race_wife)
tab couple_joint_religion, gen(religion)
tab year_gp, gen(year_gp)

********************************************************************************
// Weighted
********************************************************************************
svyset [pweight=weight]

putexcel set "$results/main results/Table1_Descriptives_weighted", replace
putexcel B1:C1 = "Parents of children under 6", merge border(bottom)
putexcel D1:E1 = "Total Sample", merge border(bottom)
putexcel B2 = ("All") C2 = ("Dissolved") D2 = ("All") E2 = ("Dissolved")
putexcel A3 = "Unique Couples"

putexcel A4 = "Dual Earning HH (Hours)"
putexcel A5 = "Male Breadwinner (Hours)"
putexcel A6 = "Female Breadwinner (Hours)"
putexcel A7 = "Egalitarian"
putexcel A8 = "Traditional"
putexcel A9 = "Counter-Traditional"
putexcel A10 = "Second Shift"
putexcel A11 = "All Other"
putexcel A12 = "Structural Support for Working Families"
putexcel A13 = "% Women with College Degrees"
putexcel A14 = "Married Women's Employment Rate"
putexcel A15 = "% Male-Breadwinning Married Couples"
putexcel A16 = "% Egalitarian Norms (Regional)"
putexcel A17 = "% Religious Conservatives"
putexcel A18 = "Relationship Duration"
putexcel A19 = "Husband's age at marriage"
putexcel A20 = "Wife's age at marriage"
putexcel A21 = "Total Couple Earnings"
putexcel A22 = "Neither partner has college degree"
putexcel A23 = "He has college degree"
putexcel A24 = "She has college degree"
putexcel A25 = "Both have college degree"
putexcel A26 = "Couple owns home"
putexcel A27 = "Husband's Race: NH White"
putexcel A28 = "Husband's Race: Black"
putexcel A29 = "Husband's Race: Hispanic"
putexcel A30 = "Husband's Race: NH Asian"
putexcel A31 = "Husband's Race: NH Other"
putexcel A32 = "Husband and wife same race"
putexcel A33 = "Either partner enrolled in school"
putexcel A34 = "Husband Wife Cohabit"
putexcel A35 = "Other Premarital Cohabit"
putexcel A36 = "First Birth Premarital"
putexcel A37 = "Religion: Both No Religion"
putexcel A38 = "Religion: Both Catholic"
putexcel A39 = "Religion: Both Protestant"
putexcel A40 = "Religion: One Catholic"
putexcel A41 = "Religion: One No Religion"
putexcel A42 = "Religion: Other"
putexcel A43 = "Moved Within 2 Survey Waves"
putexcel A44 = "Number of Children"
putexcel A45 = "Survey Year: Pre-2000s"
putexcel A46 = "Survey Year: 2000-2004"
putexcel A47 = "Survey Year: 2005-2009"
putexcel A48 = "Survey Year: 2010-2014"
putexcel A49 = "Survey Year: 2015-2019"

// think use this bc I use this for stratification
putexcel A50 = "Wife's Race: NH White"
putexcel A51 = "Wife's Race: Black"
putexcel A52 = "Wife's Race: Hispanic"
putexcel A53 = "Wife's Race: NH Asian"
putexcel A54 = "Wife's Race: NH Other"


local meanvars "hours_type1 hours_type2 hours_type3 combined_hours1 combined_hours2 combined_hours3 combined_hours4 combined_hours5 structural_familism_t women_college_rate_wt_t married_women_emp_rate_wt_t married_pure_male_bw_rate_t avg_egal_reg_t evang_rate_t marr_dur age_mar_head age_mar_wife couple_earnings_t1 educ_type1 educ_type2 educ_type3 educ_type4 home_owner race_head1 race_head2 race_head3 race_head4 race_head5 same_race either_enrolled cohab_with_partner cohab_with_other pre_marital_birth religion1 religion2 religion3 religion4 religion5 religion6 moved_last2 NUM_CHILDREN_ year_gp1 year_gp2 year_gp3 year_gp4 year_gp5 race_wife1 race_wife2 race_wife3 race_wife4 race_wife5"

// Parents
forvalues w=1/51{
	local row=`w'+3
	local var: word `w' of `meanvars'
	svy: mean `var' if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0
	matrix t`var'= e(b)
	putexcel B`row' = matrix(t`var'), nformat(#.#%)
}

// those who dissolved; value when dissolve==1
forvalues w=1/51{
	local row=`w'+3
	local var: word `w' of `meanvars' 
	svy: mean `var' if dissolve==1 & children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0
	matrix t`var'= e(b)
	putexcel C`row' = matrix(t`var'), nformat(#.#%)
}


// All couples
forvalues w=1/51{
	local row=`w'+3
	local var: word `w' of `meanvars'
	svy: mean `var' if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0
	matrix t`var'= e(b)
	putexcel D`row' = matrix(t`var'), nformat(#.#%)
}

// those who dissolved; value when dissolve==1
forvalues w=1/51{
	local row=`w'+3
	local var: word `w' of `meanvars'
	svy: mean `var' if dissolve==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0
	matrix t`var'= e(b)
	putexcel E`row' = matrix(t`var'), nformat(#.#%)
}

unique unique_id if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0
unique unique_id if children_under6==1 & dissolve==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0
unique unique_id if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0
unique unique_id if dissolve==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0
unique unique_id if children==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0


********************************************************************************
// Unweighted (for comparison to previous)
********************************************************************************

putexcel set "$results/main results/Table1_Descriptives_unweighted", replace
putexcel B1:C1 = "Parents of children under 6", merge border(bottom)
putexcel D1:E1 = "Total Sample", merge border(bottom)
putexcel B2 = ("All") C2 = ("Dissolved") D2 = ("All") E2 = ("Dissolved")
putexcel A3 = "Unique Couples"

putexcel A4 = "Dual Earning HH (Hours)"
putexcel A5 = "Male Breadwinner (Hours)"
putexcel A6 = "Female Breadwinner (Hours)"
putexcel A7 = "Egalitarian"
putexcel A8 = "Traditional"
putexcel A9 = "Counter-Traditional"
putexcel A10 = "Second Shift"
putexcel A11 = "All Other"
putexcel A12 = "Structural Support for Working Families"
putexcel A13 = "% Women with College Degrees"
putexcel A14 = "Married Women's Employment Rate"
putexcel A15 = "% Male-Breadwinning Married Couples"
putexcel A16 = "% Egalitarian Norms (Regional)"
putexcel A17 = "% Religious Conservatives"
putexcel A18 = "Relationship Duration"
putexcel A19 = "Husband's age at marriage"
putexcel A20 = "Wife's age at marriage"
putexcel A21 = "Total Couple Earnings"
putexcel A22 = "Neither partner has college degree"
putexcel A23 = "He has college degree"
putexcel A24 = "She has college degree"
putexcel A25 = "Both have college degree"
putexcel A26 = "Couple owns home"
putexcel A27 = "Husband's Race: NH White"
putexcel A28 = "Husband's Race: Black"
putexcel A29 = "Husband's Race: Hispanic"
putexcel A30 = "Husband's Race: NH Asian"
putexcel A31 = "Husband's Race: NH Other"
putexcel A32 = "Husband and wife same race"
putexcel A33 = "Either partner enrolled in school"
putexcel A34 = "Husband Wife Cohabit"
putexcel A35 = "Other Premarital Cohabit"
putexcel A36 = "First Birth Premarital"
putexcel A37 = "Religion: Both No Religion"
putexcel A38 = "Religion: Both Catholic"
putexcel A39 = "Religion: Both Protestant"
putexcel A40 = "Religion: One Catholic"
putexcel A41 = "Religion: One No Religion"
putexcel A42 = "Religion: Other"
putexcel A43 = "Moved Within 2 Survey Waves"
putexcel A44 = "Number of Children"
putexcel A45 = "Survey Year: Pre-2000s"
putexcel A46 = "Survey Year: 2000-2004"
putexcel A47 = "Survey Year: 2005-2009"
putexcel A48 = "Survey Year: 2010-2014"
putexcel A49 = "Survey Year: 2015-2019"

local meanvars "hours_type1 hours_type2 hours_type3 combined_hours1 combined_hours2 combined_hours3 combined_hours4 combined_hours5 structural_familism_t women_college_rate_wt_t married_women_emp_rate_wt_t married_pure_male_bw_rate_t avg_egal_reg_t evang_rate_t marr_dur age_mar_head age_mar_wife couple_earnings_t1 educ_type1 educ_type2 educ_type3 educ_type4 home_owner race_head1 race_head2 race_head3 race_head4 race_head5 same_race either_enrolled cohab_with_partner cohab_with_other pre_marital_birth religion1 religion2 religion3 religion4 religion5 religion6 moved_last2 NUM_CHILDREN_ year_gp1 year_gp2 year_gp3 year_gp4 year_gp5"

// Parents
forvalues w=1/46{
	local row=`w'+3
	local var: word `w' of `meanvars'
	mean `var' if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0
	matrix t`var'= e(b)
	putexcel B`row' = matrix(t`var'), nformat(#.#%)
}

// those who dissolved; value when dissolve==1
forvalues w=1/46{
	local row=`w'+3
	local var: word `w' of `meanvars' 
	mean `var' if dissolve==1 & children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0
	matrix t`var'= e(b)
	putexcel C`row' = matrix(t`var'), nformat(#.#%)
}


// All couples
forvalues w=1/46{
	local row=`w'+3
	local var: word `w' of `meanvars'
	mean `var' if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0
	matrix t`var'= e(b)
	putexcel D`row' = matrix(t`var'), nformat(#.#%)
}

// those who dissolved; value when dissolve==1
forvalues w=1/46{
	local row=`w'+3
	local var: word `w' of `meanvars'
	mean `var' if dissolve==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0
	matrix t`var'= e(b)
	putexcel E`row' = matrix(t`var'), nformat(#.#%)
}
