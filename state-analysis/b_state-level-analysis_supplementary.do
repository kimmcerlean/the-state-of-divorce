* need to also REALLY clean up this and other file - there are so many extraneous variables and analyses; I need to get rid of the ones not in the paper to avoid confusion
* Like this this becomes SUPPLEMENTARY in paper. A second file is EXPLORATORY not paper? (bc some of this even ends up in main file)

********************************************************************************
* Adding in state-level data
* state-level-analysis.do
* Kim McErlean
********************************************************************************

********************************************************************************
* Import data
********************************************************************************
use "$created_data/PSID_union_sample_with_policy.dta", clear // deciding I will use the file created in MAIN RESULTS so I don't need to run through all of this again - so i know the files are consistent

********************************************************************************
* Last checks and data cleaning / set-up
********************************************************************************
* Small things needed for analysis (run through this so you have controls and for figures - do here also in case don't run together)
set scheme cleanplots

global controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.educ_type i.moved_last2 i.couple_joint_religion i.num_children i.year_gp"  // add year here.

global macro_controls "women_college_rate_wt_t married_women_emp_rate_wt_t avg_egal_reg_t married_pure_male_bw_rate_t evang_rate_t" // add religion here 

/* Controls from R1
global controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.educ_type i.moved_last2 i.couple_joint_religion i.num_children"  // i.region knot1 knot2 knot3 

global macro_controls "women_college_rate_wt_t married_women_emp_rate_wt_t avg_egal_reg_t married_pure_male_bw_rate_t"
*/

* Alt controls for some exploratory things
global controls_nofe "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.educ_type i.moved_last2 i.couple_joint_religion i.num_children i.year_gp" 

global combo_controls "i.current_rel_type age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.educ_type i.moved_last2 i.couple_joint_religion i.num_children i.year_gp" // cohab_with_partner cohab_with_other // these causing problems right now when combined


********************************************************************************
**# Merge onto policy data
********************************************************************************
// I moved most of this to the other file. HOWEVER, in this file, I do a robustness check with conditions at time of marriage, but i don't want to distract in main file - so just do here (beacuse also, it's different variables than in main file)

// now want to merge on conditions at TIME OF MARRIAGE (just select variables - I guess for funsies I can also merge on structural variable just to have, and all macro?
local marr_vars "structural_familism women_college_rate_wt married_women_emp_rate_wt avg_egal_reg married_pure_male_bw_rate sex_ratio_marriage_wt men_unemp_rate_wt college_ratio_wt"

merge m:1 rel_start_yr_couple state_fips using "$created_data/scale_refresh.dta", keepusing(`marr_vars')
drop if _merge==2
drop _merge

foreach var in `marr_vars'{
	rename `var' `var'_tmar
}

// browse id state_fips year rel_start_yr_couple structural_familism_t structural_familism_tmar men_unemp_rate_wt_t men_unemp_rate_wt_tmar // right the tmar ones are CONSTANT but only within couples. i CAN still control for these, though, because people get married at different years within a state so there is still variation. but...this might still cause problems let's see
tabstat men_unemp_rate_wt_tmar college_ratio_wt_tmar sex_ratio_marriage_wt_tmar, by(year)

////////////////////////////////////////////////////////////////////////////////
********************************************************************************
********************************************************************************
********************************************************************************
**# Analysis starts
********************************************************************************
********************************************************************************
********************************************************************************
////////////////////////////////////////////////////////////////////////////////

********************************************************************************
********************************************************************************
********************************************************************************
**# Macro-level interactions: t-2 (R2, Response 1)
********************************************************************************
********************************************************************************
********************************************************************************

// Notes for round 2: I ended up NOT including these in Supplementary Materials bc results so similar. I actually don't even reference in the text, I just included in letter. So, I think we can move this?

global macro_controls_t2 "women_college_rate_wt_t2 married_women_emp_rate_wt_t2 avg_egal_reg_t2 married_pure_male_bw_rate_t2 evang_rate_t2"

// tabstat structural_familism_t women_college_rate_wt_t2 married_women_emp_rate_wt_t2 avg_egal_reg_t2 married_pure_male_bw_rate_t2 evang_rate_t2, by(state_fips)
// tabstat structural_familism_t women_college_rate_wt_t women_college_rate_wt_t2 married_women_emp_rate_wt_t2 avg_egal_reg_t2 married_pure_male_bw_rate_t2 evang_rate_t2, by(year)

********************************************************************************
* Let's do Male BW first
********************************************************************************

// Main model - just control for macro factors, but not yet interacted
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls_t2 if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6_robust.xls", ctitle(Main Model) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

// Interact women's degree attainment
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.women_college_rate_wt_t2#i.hh_hours_type_t1 $controls $macro_controls_t2 if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6_robust.xls", ctitle(college: familism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.women_college_rate_wt_t2#i.hh_hours_type_t1 $controls $macro_controls_t2 if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum women_college_rate_wt_t2, detail
margins, dydx(hh_hours_type_t1) at(women_college_rate_wt_t2=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6_robust.xls", ctitle(college: college) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// women's employment
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.married_women_emp_rate_wt_t2#i.hh_hours_type_t1 $controls $macro_controls_t2 if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6_robust.xls", ctitle(emp: familism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.married_women_emp_rate_wt_t2#i.hh_hours_type_t1 $controls $macro_controls_t2 if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum married_women_emp_rate_wt_t2, detail
margins, dydx(hh_hours_type_t1) at(married_women_emp_rate_wt_t2=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6_robust.xls", ctitle(emp: emp) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// male BW rate
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.married_pure_male_bw_rate_t2#i.hh_hours_type_t1 $controls $macro_controls_t2 if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6_robust.xls", ctitle(male bw: familism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.married_pure_male_bw_rate_t2#i.hh_hours_type_t1 $controls $macro_controls_t2 if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum married_pure_male_bw_rate_t2, detail
margins, dydx(hh_hours_type_t1) at(married_pure_male_bw_rate_t2=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6_robust.xls", ctitle(male bw: male bw) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// regional gender norms
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.avg_egal_reg_t2#i.hh_hours_type_t1 $controls $macro_controls_t2 if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6_robust.xls", ctitle(norms: familism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.avg_egal_reg_t2#i.hh_hours_type_t1 $controls $macro_controls_t2 if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum avg_egal_reg_t2, detail
margins, dydx(hh_hours_type_t1) at(avg_egal_reg_t2=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6_robust.xls", ctitle(norms: norms) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

** OH should I add main effects GAH
// main
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls_t2 if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(hh_hours_type_t1) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6_robust.xls", ctitle(main effects: main) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// women's degree attainment
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.women_college_rate_wt_t2#i.hh_hours_type_t1 $controls $macro_controls_t2 if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(hh_hours_type_t1) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6_robust.xls", ctitle(main effects: college) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// women's employment
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.married_women_emp_rate_wt_t2#i.hh_hours_type_t1 $controls $macro_controls_t2 if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(hh_hours_type_t1) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6_robust.xls", ctitle(main effects: emp) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// male BW rate
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.married_pure_male_bw_rate_t2#i.hh_hours_type_t1 $controls $macro_controls_t2 if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(hh_hours_type_t1) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6_robust.xls", ctitle(main effects: male BW) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// regional gender norms
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.avg_egal_reg_t2#i.hh_hours_type_t1 $controls $macro_controls_t2 if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(hh_hours_type_t1) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6_robust.xls", ctitle(main effects: norms) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

********************************************************************************
* Then combined DoL
********************************************************************************
// Main model - just control for macro factors, but not yet interacted
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls_t2 if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6_robust.xls", ctitle(Main Model) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// Interact women's degree attainment
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.women_college_rate_wt_t2#i.division_bucket_hrs_t1 $controls $macro_controls_t2 if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6_robust.xls", ctitle(college: familism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.women_college_rate_wt_t2#i.division_bucket_hrs_t1 $controls $macro_controls_t2 if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum women_college_rate_wt_t2, detail
margins, dydx(division_bucket_hrs_t1) at(women_college_rate_wt_t2=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6_robust.xls", ctitle(college: college) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// women's employment
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.married_women_emp_rate_wt_t2#i.division_bucket_hrs_t1 $controls $macro_controls_t2 if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6_robust.xls", ctitle(emp: familism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.married_women_emp_rate_wt_t2#i.division_bucket_hrs_t1 $controls $macro_controls_t2 if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum married_women_emp_rate_wt_t2, detail
margins, dydx(division_bucket_hrs_t1) at(married_women_emp_rate_wt_t2=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6_robust.xls", ctitle(emp: emp) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// male BW rate
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.married_pure_male_bw_rate_t2#i.division_bucket_hrs_t1 $controls $macro_controls_t2 if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6_robust.xls", ctitle(male bw: familism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.married_pure_male_bw_rate_t2#i.division_bucket_hrs_t1 $controls $macro_controls_t2 if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum married_pure_male_bw_rate_t2, detail
margins, dydx(division_bucket_hrs_t1) at(married_pure_male_bw_rate_t2=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6_robust.xls", ctitle(male bw: male bw) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// regional gender norms
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.avg_egal_reg_t2#i.division_bucket_hrs_t1 $controls $macro_controls_t2 if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6_robust.xls", ctitle(norms: familism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.avg_egal_reg_t2#i.division_bucket_hrs_t1 $controls $macro_controls_t2 if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum avg_egal_reg_t2, detail
margins, dydx(division_bucket_hrs_t1) at(avg_egal_reg_t2=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6_robust.xls", ctitle(norms: norms) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

** OH should I add main effects GAH
// main
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls_t2 if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1)  post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6_robust.xls", ctitle(main effects: main) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// women's degree attainment
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.women_college_rate_wt_t2#i.division_bucket_hrs_t1 $controls $macro_controls_t2 if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1)  post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6_robust.xls", ctitle(main effects: college) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// women's employment
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.married_women_emp_rate_wt_t2#i.division_bucket_hrs_t1 $controls $macro_controls_t2 if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1)  post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6_robust.xls", ctitle(main effects: emp) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// male BW rate
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.married_pure_male_bw_rate_t2#i.division_bucket_hrs_t1 $controls $macro_controls_t2 if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1)  post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6_robust.xls", ctitle(main effects: male BW) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// regional gender norms
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.avg_egal_reg_t2#i.division_bucket_hrs_t1 $controls $macro_controls_t2 if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1)  post
outreg2 using "$results/main results/dissolution_AMEs_parentsu6_robust.xls", ctitle(main effects: norms) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

********************************************************************************
********************************************************************************
********************************************************************************
**# Relationship Type (R2, Response 2)
********************************************************************************
********************************************************************************
********************************************************************************

// this is all exploratory. Not including cohabitation for theoretical and methodological reasons - so eventually can move to "exploratory" file.

/// Full Sample
* Individual models
// Marriage
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(hh_hours_type_t1) 

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1) 

// Cohab
logit dissolve i.coh_dur c.structural_familism_t i.hh_hours_type_t1 $controls $macro_controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or cluster(couple_id)
margins, dydx(hh_hours_type_t1) 

logit dissolve i.coh_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls $macro_controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1) // division_bucket_hrs_gp_t1

// Combined
logit dissolve i.combined_dur c.structural_familism_t i.hh_hours_type_t1 $combo_controls  $macro_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or cluster(couple_id)
margins, dydx(hh_hours_type_t1) 

logit dissolve i.combined_dur c.structural_familism_t i.division_bucket_hrs_t1 $combo_controls  $macro_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1) 

* Now - interact instead of separate?
logit dissolve i.combined_dur c.structural_familism_t i.current_rel_type##i.hh_hours_type_t1 $combo_controls  $macro_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or cluster(couple_id)
estimates store rel_type
margins current_rel_type, dydx(hh_hours_type_t1) 

estimates restore rel_type
margins, dydx(hh_hours_type_t1) at(current_rel_type=(20 22)) post
mlincom 1-2, detail // marriage - cohab, male BW // okay this is sig

estimates restore rel_type
margins, dydx(2.hh_hours_type_t1) at(current_rel_type==20) post
estimates store marr_male

estimates restore rel_type
margins, dydx(3.hh_hours_type_t1) at(current_rel_type==20) post
estimates store marr_fem

estimates restore rel_type
margins, dydx(2.hh_hours_type_t1) at(current_rel_type==22) post
estimates store coh_male

estimates restore rel_type
margins, dydx(3.hh_hours_type_t1) at(current_rel_type==22) post
estimates store coh_fem

coefplot (marr_male, mcolor(navy) ciopts(color(navy)) label("────── Marriage")) ///
		(coh_male, mcolor(navy) ciopts(color(navy) lpattern(shortdash)) label("- - - - - Cohabitation")) ///
		(marr_fem, mcolor(eltblue) ciopts(color(eltblue)) nokey) ///
		(coh_fem, mcolor(eltblue) ciopts(color(eltblue) lpattern(shortdash)) nokey) ///
		,  drop(_cons) xline(0, lcolor("red") lpattern(solid)) levels(95) legend(position(bottom) rows(1)) ///
		xtitle(Average Marginal Effect Relative to Dual-Earning, size(small))
		
logit dissolve i.combined_dur c.structural_familism_t i.current_rel_type##i.division_bucket_hrs_t1 $combo_controls  $macro_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1) at(current_rel_type=(20 22)) post
mlincom 1-2, detail // marriage - cohab, trad // this is NOT sig

/// Parents of Young Children. Not using. This is not the point here.
* Individual models
// Marriage
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(hh_hours_type_t1) 

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1) 

// Cohab
logit dissolve i.coh_dur c.structural_familism_t i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or cluster(couple_id)
margins, dydx(hh_hours_type_t1) 

logit dissolve i.coh_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1) // division_bucket_hrs_gp_t1

// Combined
logit dissolve i.combined_dur c.structural_familism_t i.hh_hours_type_t1 $combo_controls  $macro_controls if children_under6==1 & combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or cluster(couple_id)
margins, dydx(hh_hours_type_t1) 

logit dissolve i.combined_dur c.structural_familism_t i.division_bucket_hrs_t1 $combo_controls  $macro_controls if children_under6==1 & combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1) 

* Now - interact instead of separate?
logit dissolve i.combined_dur c.structural_familism_t i.current_rel_type##i.hh_hours_type_t1 $combo_controls  $macro_controls if children_under6==1 & combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or cluster(couple_id)
margins current_rel_type, dydx(hh_hours_type_t1)
marginsplot , recast(bar)


********************************************************************************
********************************************************************************
********************************************************************************
**# Child Age Robustness (R2, Response 3)
********************************************************************************
********************************************************************************
********************************************************************************

// this IS in paper.

********************************************************************************
* Male BW
********************************************************************************

// main effects
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(hh_hours_type_t1)

// I can't remember - does male BW lower divorce already for all parents or just those under 6? Thinking of using this as setup segue frm total sample to parents (re: research on institutions and maternal employment specifically). okay yeah so even no main effects here.
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 $controls $macro_controls if children==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(hh_hours_type_t1)

forvalues a=2/18{
	logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 $controls $macro_controls if children==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & AGE_YOUNG_CHILD_<`a', or cluster(couple_id)
	margins, dydx(2.hh_hours_type_t1 3.hh_hours_type_t1) post
	estimates store main_a`a'
}

// total sample:
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(2.hh_hours_type_t1 3.hh_hours_type_t1) post
estimates store main_all

coefplot (main_a2, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("2")) (main_a3, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("3")) ///
(main_a4, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("4")) (main_a5, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("5")) ///
(main_a6, lcolor("red") mcolor("red") ciopts(color("red")) label("6")) ///
(main_a7, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("7")) (main_a8, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("8")) ///
(main_a9, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("9"))  (main_a10, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("10")) ///
(main_a11, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("11"))  (main_a12, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("12")) ///
(main_a13, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("13"))  (main_a14, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("14")) ///
(main_a15, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("15"))  (main_a16, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("16")) ///
(main_a17, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("17"))  (main_a18, lcolor("eltblue") mcolor("eltblue") ciopts(color("eltblue")) label("18 (all parents)")) ///
(main_all, lcolor("navy") mcolor("navy") ciopts(color("navy")) label("full sample")) ///
, horizontal drop(_cons 1.hh_hours_type_t1) xline(0, lcolor(black) lstyle(solid)) levels(90) base xtitle(Average Marginal Effects Relative to Dual Earning, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinning" 3.hh_hours_type_t1 = "Female Breadwinning", labsize(small)) ///
legend(order(- "youngest child <" 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36))

// interaction
forvalues a=2/18{
	logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & children==1 & AGE_YOUNG_CHILD_<`a', or cluster(couple_id)
	sum structural_familism_t, detail
	margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
	estimates store mbw_a`a'
}

// total sample:
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store mbw_all

coefplot (mbw_a2, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("2")) (mbw_a3, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("3")) ///
(mbw_a4, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("4")) (mbw_a5, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("5")) ///
(mbw_a6, lcolor("red") mcolor("red") ciopts(color("red")) label("6")) ///
(mbw_a7, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("7")) (mbw_a8, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("8")) ///
(mbw_a9, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("9"))  (mbw_a10, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("10")) ///
(mbw_a11, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("11"))  (mbw_a12, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("12")) ///
(mbw_a13, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("13"))  (mbw_a14, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("14")) ///
(mbw_a15, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("15"))  (mbw_a16, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("16")) ///
(mbw_a17, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("17"))  (mbw_a18, lcolor("eltblue") mcolor("eltblue") ciopts(color("eltblue")) label("18")) ///
(mbw_all, lcolor("navy") mcolor("navy") ciopts(color("navy")) label("full sample")) ///
, horizontal drop(_cons) xline(0, lcolor(black) lstyle(solid)) levels(90) base xtitle(Average Marginal Effects Relative to Dual Earning, size(small)) ///
coeflabels(1._at = "5th Pct" 2._at = "25th Pct" 3._at = "50th Pct" 4._at = "75th Pct" 5._at = "95th Pct", labsize(small)) ///
legend(position(bottom) rows(2) order(- "youngest child <" 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36)) groups(?._at = "{bf:Structural Support}", angle(vertical)) //  aspect(0.8)

// just do every 2 years 6-18 for ease of showing?
coefplot (mbw_a2, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("2")) (mbw_a3, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("3")) ///
(mbw_a4, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("4")) (mbw_a5, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("5")) ///
(mbw_a6, lcolor("red") mcolor("red") ciopts(color("red")) label("6")) ///
(mbw_a7, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("7")) (mbw_a8, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("8")) ///
 (mbw_a10, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("10")) ///
 (mbw_a12, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("12")) ///
(mbw_a14, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("14")) ///
 (mbw_a16, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("16")) ///
 (mbw_a18, lcolor("eltblue") mcolor("eltblue") ciopts(color("eltblue")) label("18 (all parents)")) ///
(mbw_all, lcolor("navy") mcolor("navy") ciopts(color("navy")) label("full sample")) ///
, horizontal drop(_cons) xline(0, lcolor(black) lstyle(solid)) levels(90) base xtitle(Average Marginal Effects: Male BW Relative to Dual Earning, size(small)) ///
coeflabels(1._at = "5th Pct" 2._at = "25th Pct" 3._at = "50th Pct" 4._at = "75th Pct" 5._at = "95th Pct", labsize(small)) ///
legend(order(- "youngest child <" 2 4 6 8 10 12 14 16 18 20 22 24 26)) ///
 groups(?._at = "{bf:Structural Support}", angle(vertical)) //  aspect(0.8) legend(position(bottom) rows(2))
 
********************************************************************************
* Combined - Do i need this?
********************************************************************************
// individ
forvalues a=2/18{
	logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls $macro_controls if children==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & AGE_YOUNG_CHILD_<`a', or cluster(couple_id)
	margins, dydx(2.division_bucket_hrs_t1 3.division_bucket_hrs_t1) post
	estimates store main_t`a'
}

// total sample:
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(2.division_bucket_hrs_t1 3.division_bucket_hrs_t1) post
estimates store main_tll

coefplot (main_t2, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("2")) (main_t3, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("3")) ///
(main_t4, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("4")) (main_t5, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("5")) ///
(main_t6, lcolor("red") mcolor("red") ciopts(color("red")) label("6")) ///
(main_t7, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("7")) (main_t8, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("8")) ///
(main_t9, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("9"))  (main_t10, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("10")) ///
(main_t11, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("11"))  (main_t12, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("12")) ///
(main_t13, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("13"))  (main_t14, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("14")) ///
(main_t15, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("15"))  (main_t16, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("16")) ///
(main_t17, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("17"))  (main_t18, lcolor("eltblue") mcolor("eltblue") ciopts(color("eltblue")) label("18 (all parents)")) ///
(main_tll, lcolor("navy") mcolor("navy") ciopts(color("navy")) label("full sample")) ///
, horizontal drop(_cons 1.division_bucket_hrs_t1) xline(0, lcolor(black) lstyle(solid)) levels(90) base xtitle(Average Marginal Effects Relative to Egalitarian, size(small)) ///
coeflabels(2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter-Trad", labsize(small)) ///
legend(order(- "youngest child <" 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36))

// interaction
forvalues a=2/18{
	logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & children==1 & AGE_YOUNG_CHILD_<`a', or cluster(couple_id)
	sum structural_familism_t, detail
	margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
	estimates store trad_a`a'
}

// total sample:
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store trad_all

// just do every 2 years 6-18 for ease of showing?
coefplot (trad_a2, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("2")) (trad_a3, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("3")) ///
(trad_a4, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("4")) (trad_a5, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("5")) ///
(trad_a6, lcolor("red") mcolor("red") ciopts(color("red")) label("6")) ///
(trad_a7, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("7")) (trad_a8, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("8")) ///
 (trad_a10, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("10")) ///
 (trad_a12, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("12")) ///
(trad_a14, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("14")) ///
 (trad_a16, lcolor("gs10") mcolor("gs10") ciopts(color("gs10")) label("16")) ///
 (trad_a18, lcolor("eltblue") mcolor("eltblue") ciopts(color("eltblue")) label("18 (all parents)")) ///
(trad_all, lcolor("navy") mcolor("navy") ciopts(color("navy")) label("full sample")) ///
, horizontal drop(_cons) xline(0, lcolor(black) lstyle(solid)) levels(90) base xtitle(Average Marginal Effects: Traditional Relative to Egalitarian, size(small)) ///
coeflabels(1._at = "5th Pct" 2._at = "25th Pct" 3._at = "50th Pct" 4._at = "75th Pct" 5._at = "95th Pct", labsize(small)) ///
legend(order(- "youngest child <" 2 4 6 8 10 12 14 16 18 20 22 24 26)) ///
 groups(?._at = "{bf:Structural Support}", angle(vertical)) //  aspect(0.8) legend(position(bottom) rows(2))
 
********************************************************************************
********************************************************************************
********************************************************************************
**# Childcare Indicator Robustness (R2, Response 4)
********************************************************************************
********************************************************************************
********************************************************************************

// currently in supplementary materials

tabstat prek_enrolled_public_t cc_pct_income_orig_t cc_pct_served_t headstart_pct_t earlyhs_pct_t total_headstart_pct_t educ_spend_percap_t, by(year)
// individ correlations:
pwcorr prek_enrolled_public_t cc_pct_income_orig_t cc_pct_served_t headstart_pct_t earlyhs_pct_t total_headstart_pct_t educ_spend_percap_t
// scale correlations:
pwcorr sf_cc_income_t sf_ccdf_served_t sf_head_start_t sf_early_hs_t sf_total_hs_t sf_educ_spend_t // not surprising, but these are all highly correlated...
// sf_cc_income_t sf_ccdf_served_t sf_head_start_t sf_early_hs_t sf_total_hs_t sf_educ_spend_t
// OH - update these from step A instead (so at state-level not skewed by population) - duh it's already there

********************************************************************************
* Singular Indicators (Male BW)
********************************************************************************

*1. Original measures to orient myself
// the full scale
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/childcare_robustness_marriage.xls", ctitle(main model) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

// just Pre-K enrollment
logit dissolve i.marr_dur c.prek_enrolled_public_t i.hh_hours_type_t1 c.prek_enrolled_public_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum prek_enrolled_public_t, detail
margins, dydx(hh_hours_type_t1) at(prek_enrolled_public_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/childcare_robustness_marriage.xls", ctitle(main: PreK) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

*2. Original measures with alt time frames (To align with those two variables not measured always)
* A. 2009+ (for CC pct income)
// the full scale
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & year>=2009, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/childcare_robustness_marriage.xls", ctitle(sf: 2009) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// just Pre-K enrollment
logit dissolve i.marr_dur c.prek_enrolled_public_t i.hh_hours_type_t1 c.prek_enrolled_public_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & year>=2009, or cluster(couple_id)
sum prek_enrolled_public_t, detail
margins, dydx(hh_hours_type_t1) at(prek_enrolled_public_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/childcare_robustness_marriage.xls", ctitle(PreK: 2009) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* B. 1999+ (for CC pct income)
// the full scale
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & year>=1999, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/childcare_robustness_marriage.xls", ctitle(sf: 1999) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// just Pre-K enrollment
logit dissolve i.marr_dur c.prek_enrolled_public_t i.hh_hours_type_t1 c.prek_enrolled_public_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & year>=1999, or cluster(couple_id)
sum prek_enrolled_public_t, detail
margins, dydx(hh_hours_type_t1) at(prek_enrolled_public_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/childcare_robustness_marriage.xls", ctitle(PreK: 1999) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

*3. Individual alt measures
*A. covered in full time frame:  headstart_pct_t earlyhs_pct_t total_headstart_pct_t educ_spend_percap_t
// Head Start
logit dissolve i.marr_dur c.headstart_pct_t i.hh_hours_type_t1 c.headstart_pct_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum headstart_pct_t, detail
margins, dydx(hh_hours_type_t1) at(headstart_pct_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/childcare_robustness_marriage.xls", ctitle(Head Start) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// Early Head Start
logit dissolve i.marr_dur c.earlyhs_pct_t i.hh_hours_type_t1 c.earlyhs_pct_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum earlyhs_pct_t, detail
margins, dydx(hh_hours_type_t1) at(earlyhs_pct_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/childcare_robustness_marriage.xls", ctitle(Early HS) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// Total Head Start
logit dissolve i.marr_dur c.total_headstart_pct_t i.hh_hours_type_t1 c.total_headstart_pct_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum total_headstart_pct_t, detail
margins, dydx(hh_hours_type_t1) at(total_headstart_pct_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/childcare_robustness_marriage.xls", ctitle(Total HS) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// Educ Spend per Capita
logit dissolve i.marr_dur c.educ_spend_percap_t i.hh_hours_type_t1 c.educ_spend_percap_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum educ_spend_percap_t, detail
margins, dydx(hh_hours_type_t1) at(educ_spend_percap_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/childcare_robustness_marriage.xls", ctitle(Educ Spend) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

*B. 2009+: cc_pct_income_orig_t 
logit dissolve i.marr_dur c.cc_pct_income_orig_t i.hh_hours_type_t1 c.cc_pct_income_orig_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & year>=2009, or cluster(couple_id)
sum cc_pct_income_orig_t, detail
margins, dydx(hh_hours_type_t1) at(cc_pct_income_orig_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/childcare_robustness_marriage.xls", ctitle(CC Costs) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

*C. 1999+: cc_pct_served_t
logit dissolve i.marr_dur c.cc_pct_served_t i.hh_hours_type_t1 c.cc_pct_served_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & year>=1999, or cluster(couple_id)
sum cc_pct_served_t, detail
margins, dydx(hh_hours_type_t1) at(cc_pct_served_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/childcare_robustness_marriage.xls", ctitle(CCDF Served) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append


********************************************************************************
* Updated in scale (Replace Pre-K) (Male BW)
********************************************************************************
*4. Now, scale updated to replace
*A. covered in full time frame:  sf_head_start_t sf_early_hs_t sf_total_hs_t sf_educ_spend_t
// Head Start
logit dissolve i.marr_dur c.sf_head_start_t i.hh_hours_type_t1 c.sf_head_start_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum sf_head_start_t, detail
margins, dydx(hh_hours_type_t1) at(sf_head_start_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/childcare_robustness_marriage.xls", ctitle(SF: Head Start) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// Early Head Start
logit dissolve i.marr_dur c.sf_early_hs_t i.hh_hours_type_t1 c.sf_early_hs_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum sf_early_hs_t, detail
margins, dydx(hh_hours_type_t1) at(sf_early_hs_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/childcare_robustness_marriage.xls", ctitle(SF: Early HS) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// Total Head Start
logit dissolve i.marr_dur c.sf_total_hs_t i.hh_hours_type_t1 c.sf_total_hs_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum sf_total_hs_t, detail
margins, dydx(hh_hours_type_t1) at(sf_total_hs_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/childcare_robustness_marriage.xls", ctitle(SF: Total HS) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// Educ Spend per Capita
logit dissolve i.marr_dur c.sf_educ_spend_t i.hh_hours_type_t1 c.sf_educ_spend_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum sf_educ_spend_t, detail
margins, dydx(hh_hours_type_t1) at(sf_educ_spend_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/childcare_robustness_marriage.xls", ctitle(SF: Educ Spend) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

*B. 2009+: sf_cc_income_t 
logit dissolve i.marr_dur c.sf_cc_income_t i.hh_hours_type_t1 c.sf_cc_income_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & year>=2009, or cluster(couple_id)
sum sf_cc_income_t, detail
margins, dydx(hh_hours_type_t1) at(sf_cc_income_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/childcare_robustness_marriage.xls", ctitle(SF: CC Costs) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

*C. 1999+: sf_ccdf_served_t
logit dissolve i.marr_dur c.sf_ccdf_served_t i.hh_hours_type_t1 c.sf_ccdf_served_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & year>=1999, or cluster(couple_id)
sum sf_ccdf_served_t, detail
margins, dydx(hh_hours_type_t1) at(sf_ccdf_served_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/childcare_robustness_marriage.xls", ctitle(SF: CCDF Served) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

********************************************************************************
* Singular Indicators (Combined DoL)
* Not sure I need all of this to make my point, so will pull and decide later
********************************************************************************

*1. Original measures to orient myself
// the full scale
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/childcare_robustness_marriage.xls", ctitle(main model) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// just Pre-K enrollment
logit dissolve i.marr_dur c.prek_enrolled_public_t i.division_bucket_hrs_t1 c.prek_enrolled_public_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum prek_enrolled_public_t, detail
margins, dydx(division_bucket_hrs_t1) at(prek_enrolled_public_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/childcare_robustness_marriage.xls", ctitle(main: PreK) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

*2. Original measures with alt time frames (To align with those two variables not measured always)
* A. 2009+ (for CC pct income)
// the full scale
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & year>=2009, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/childcare_robustness_marriage.xls", ctitle(sf: 2009) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// just Pre-K enrollment
logit dissolve i.marr_dur c.prek_enrolled_public_t i.division_bucket_hrs_t1 c.prek_enrolled_public_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & year>=2009, or cluster(couple_id)
sum prek_enrolled_public_t, detail
margins, dydx(division_bucket_hrs_t1) at(prek_enrolled_public_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/childcare_robustness_marriage.xls", ctitle(PreK: 2009) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* B. 1999+ (for CC pct income)
// the full scale
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & year>=1999, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/childcare_robustness_marriage.xls", ctitle(sf: 1999) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// just Pre-K enrollment
logit dissolve i.marr_dur c.prek_enrolled_public_t i.division_bucket_hrs_t1 c.prek_enrolled_public_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & year>=1999, or cluster(couple_id)
sum prek_enrolled_public_t, detail
margins, dydx(division_bucket_hrs_t1) at(prek_enrolled_public_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/childcare_robustness_marriage.xls", ctitle(PreK: 1999) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

*3. Individual alt measures
*A. covered in full time frame:  headstart_pct_t earlyhs_pct_t total_headstart_pct_t educ_spend_percap_t
// Head Start
logit dissolve i.marr_dur c.headstart_pct_t i.division_bucket_hrs_t1 c.headstart_pct_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum headstart_pct_t, detail
margins, dydx(division_bucket_hrs_t1) at(headstart_pct_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/childcare_robustness_marriage.xls", ctitle(Head Start) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// Early Head Start
logit dissolve i.marr_dur c.earlyhs_pct_t i.division_bucket_hrs_t1 c.earlyhs_pct_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum earlyhs_pct_t, detail
margins, dydx(division_bucket_hrs_t1) at(earlyhs_pct_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/childcare_robustness_marriage.xls", ctitle(Early HS) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// Total Head Start
logit dissolve i.marr_dur c.total_headstart_pct_t i.division_bucket_hrs_t1 c.total_headstart_pct_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum total_headstart_pct_t, detail
margins, dydx(division_bucket_hrs_t1) at(total_headstart_pct_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/childcare_robustness_marriage.xls", ctitle(Total HS) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// Educ Spend per Capita
logit dissolve i.marr_dur c.educ_spend_percap_t i.division_bucket_hrs_t1 c.educ_spend_percap_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum educ_spend_percap_t, detail
margins, dydx(division_bucket_hrs_t1) at(educ_spend_percap_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/childcare_robustness_marriage.xls", ctitle(Educ Spend) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

*B. 2009+: cc_pct_income_orig_t 
logit dissolve i.marr_dur c.cc_pct_income_orig_t i.division_bucket_hrs_t1 c.cc_pct_income_orig_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & year>=2009, or cluster(couple_id)
sum cc_pct_income_orig_t, detail
margins, dydx(division_bucket_hrs_t1) at(cc_pct_income_orig_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/childcare_robustness_marriage.xls", ctitle(CC Costs) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

*C. 1999+: cc_pct_served_t
logit dissolve i.marr_dur c.cc_pct_served_t i.division_bucket_hrs_t1 c.cc_pct_served_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & year>=1999, or cluster(couple_id)
sum cc_pct_served_t, detail
margins, dydx(division_bucket_hrs_t1) at(cc_pct_served_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/childcare_robustness_marriage.xls", ctitle(CCDF Served) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append


********************************************************************************
* Updated in scale (Replace Pre-K) (Combined DoL)
* Not sure I need all of this to make my point, so will pull and decide later
********************************************************************************
*4. Now, scale updated to replace
*A. covered in full time frame:  sf_head_start_t sf_early_hs_t sf_total_hs_t sf_educ_spend_t
// Head Start
logit dissolve i.marr_dur c.sf_head_start_t i.division_bucket_hrs_t1 c.sf_head_start_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum sf_head_start_t, detail
margins, dydx(division_bucket_hrs_t1) at(sf_head_start_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/childcare_robustness_marriage.xls", ctitle(SF: Head Start) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// Early Head Start
logit dissolve i.marr_dur c.sf_early_hs_t i.division_bucket_hrs_t1 c.sf_early_hs_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum sf_early_hs_t, detail
margins, dydx(division_bucket_hrs_t1) at(sf_early_hs_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/childcare_robustness_marriage.xls", ctitle(SF: Early HS) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// Total Head Start
logit dissolve i.marr_dur c.sf_total_hs_t i.division_bucket_hrs_t1 c.sf_total_hs_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum sf_total_hs_t, detail
margins, dydx(division_bucket_hrs_t1) at(sf_total_hs_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/childcare_robustness_marriage.xls", ctitle(SF: Total HS) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// Educ Spend per Capita
logit dissolve i.marr_dur c.sf_educ_spend_t i.division_bucket_hrs_t1 c.sf_educ_spend_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum sf_educ_spend_t, detail
margins, dydx(division_bucket_hrs_t1) at(sf_educ_spend_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/childcare_robustness_marriage.xls", ctitle(SF: Educ Spend) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

*B. 2009+: sf_cc_income_t 
logit dissolve i.marr_dur c.sf_cc_income_t i.division_bucket_hrs_t1 c.sf_cc_income_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & year>=2009, or cluster(couple_id)
sum sf_cc_income_t, detail
margins, dydx(division_bucket_hrs_t1) at(sf_cc_income_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/childcare_robustness_marriage.xls", ctitle(SF: CC Costs) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

*C. 1999+: sf_ccdf_served_t
logit dissolve i.marr_dur c.sf_ccdf_served_t i.division_bucket_hrs_t1 c.sf_ccdf_served_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & year>=1999, or cluster(couple_id)
sum sf_ccdf_served_t, detail
margins, dydx(division_bucket_hrs_t1) at(sf_ccdf_served_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/childcare_robustness_marriage.xls", ctitle(SF: CCDF Served) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append


********************************************************************************
********************************************************************************
********************************************************************************
**# Individual indicators of scale: Total Sample
* (not sure if I will use this, but let's pull jic)
* Relates to R2, Response 5) - much of this is in main results file,
* Because i do talk more about this in the manuscript
********************************************************************************
********************************************************************************
********************************************************************************

// scale, for reference: paid_leave_st prek_enrolled_public_st min_amt_above_fed_st earn_ratio_st unemployment_percap_st abortion_protected_st welfare_all_st
/* I am not even going to run this

********************************************************************************
*Paid Work Hours
********************************************************************************
* Main model
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details_full.xls", ctitle(main) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

* Paid Leave
logit dissolve i.marr_dur i.paid_leave_t i.hh_hours_type_t1 i.paid_leave_t#i.hh_hours_type_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(2.hh_hours_type_t1) at(paid_leave_t=(0 1)) post // had to update bc #3 is collinnear
outreg2 using "$results/main results/dissolution_AMES_scale_details_full.xls", ctitle(paidleave) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* PreK Enrollment
logit dissolve i.marr_dur c.prek_enrolled_public_t i.hh_hours_type_t1 c.prek_enrolled_public_t#i.hh_hours_type_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum prek_enrolled_public_t, detail
margins, dydx(hh_hours_type_t1) at(prek_enrolled_public_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details_full.xls", ctitle(prek) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Min Wage
logit dissolve i.marr_dur c.min_amt_above_fed_t i.hh_hours_type_t1 c.min_amt_above_fed_t#i.hh_hours_type_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum min_amt_above_fed_t, detail
margins, dydx(hh_hours_type_t1) at(min_amt_above_fed_t=(`r(p5)' `r(p50)' `r(p75)' `r(p95)' `r(p99)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details_full.xls", ctitle(minwage) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Earnings Ratio
logit dissolve i.marr_dur c.earn_ratio_t i.hh_hours_type_t1 c.earn_ratio_t#i.hh_hours_type_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum earn_ratio_t, detail
margins, dydx(hh_hours_type_t1) at(earn_ratio_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details_full.xls", ctitle(earnings) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Unemployment Compensation
logit dissolve i.marr_dur c.unemployment_percap_t i.hh_hours_type_t1 c.unemployment_percap_t#i.hh_hours_type_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum unemployment_percap_t, detail
margins, dydx(hh_hours_type_t1) at(unemployment_percap_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details_full.xls", ctitle(unemployment) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Abortion protected
logit dissolve i.marr_dur i.abortion_protected_t i.hh_hours_type_t1 i.abortion_protected_t#i.hh_hours_type_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(hh_hours_type_t1) at(abortion_protected_t=(0 1)) post
outreg2 using "$results/main results/dissolution_AMES_scale_details_full.xls", ctitle(abortion) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Welfare Expenditures
logit dissolve i.marr_dur c.welfare_all_t i.hh_hours_type_t1 c.welfare_all_t#i.hh_hours_type_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum welfare_all_t, detail
margins, dydx(hh_hours_type_t1) at(welfare_all_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details_full.xls", ctitle(welfare) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Childcare latent indicator
logit dissolve i.marr_dur c.family_investment_t i.hh_hours_type_t1 c.family_investment_t#i.hh_hours_type_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum family_investment_t, detail
margins, dydx(hh_hours_type_t1) at(family_investment_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details_full.xls", ctitle(childcare latent) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* WFP latent indicator
logit dissolve i.marr_dur c.broad_policy_t i.hh_hours_type_t1 c.broad_policy_t#i.hh_hours_type_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum broad_policy_t, detail
margins, dydx(hh_hours_type_t1) at(broad_policy_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details_full.xls", ctitle(policy latent) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Structural scale using Factor Analysis
logit dissolve i.marr_dur c.structural_factor_t i.hh_hours_type_t1 c.structural_factor_t#i.hh_hours_type_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_factor_t, detail
margins, dydx(hh_hours_type_t1) at(structural_factor_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details_full.xls", ctitle(sf latent) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

********************************************************************************
*Combined DoL
********************************************************************************
* Main model
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details_full.xls", ctitle(main) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Paid Leave
logit dissolve i.marr_dur i.paid_leave_t i.division_bucket_hrs_t1 i.paid_leave_t#i.division_bucket_hrs_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(2.division_bucket_hrs_t1 4.division_bucket_hrs_t1) at(paid_leave_t=(0 1)) post
outreg2 using "$results/main results/dissolution_AMES_scale_details_full.xls", ctitle(paidleave) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* PreK Enrollment
logit dissolve i.marr_dur c.prek_enrolled_public_t i.division_bucket_hrs_t1 c.prek_enrolled_public_t#i.division_bucket_hrs_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum prek_enrolled_public_t, detail
margins, dydx(division_bucket_hrs_t1) at(prek_enrolled_public_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details_full.xls", ctitle(prek) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Min Wage
logit dissolve i.marr_dur c.min_amt_above_fed_t i.division_bucket_hrs_t1 c.min_amt_above_fed_t#i.division_bucket_hrs_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum min_amt_above_fed_t, detail
margins, dydx(division_bucket_hrs_t1) at(min_amt_above_fed_t=(`r(p5)' `r(p50)' `r(p75)' `r(p95)' `r(p99)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details_full.xls", ctitle(minwage) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Earnings Ratio
logit dissolve i.marr_dur c.earn_ratio_t i.division_bucket_hrs_t1 c.earn_ratio_t#i.division_bucket_hrs_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum earn_ratio_t, detail
margins, dydx(division_bucket_hrs_t1) at(earn_ratio_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details_full.xls", ctitle(earnings) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Unemployment Compensation
logit dissolve i.marr_dur c.unemployment_percap_t i.division_bucket_hrs_t1 c.unemployment_percap_t#i.division_bucket_hrs_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum unemployment_percap_t, detail
margins, dydx(division_bucket_hrs_t1) at(unemployment_percap_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details_full.xls", ctitle(unemployment) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Abortion protected
logit dissolve i.marr_dur i.abortion_protected_t i.division_bucket_hrs_t1 i.abortion_protected_t#i.division_bucket_hrs_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(2.division_bucket_hrs_t1 4.division_bucket_hrs_t1 5.division_bucket_hrs_t1) at(abortion_protected_t=(0 1)) post
outreg2 using "$results/main results/dissolution_AMES_scale_details_full.xls", ctitle(abortion) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Welfare Expenditures
logit dissolve i.marr_dur c.welfare_all_t i.division_bucket_hrs_t1 c.welfare_all_t#i.division_bucket_hrs_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum welfare_all_t, detail
margins, dydx(division_bucket_hrs_t1) at(welfare_all_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details_full.xls", ctitle(welfare) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Childcare latent indicator
logit dissolve i.marr_dur c.family_investment_t i.division_bucket_hrs_t1 c.family_investment_t#i.division_bucket_hrs_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum family_investment_t, detail
margins, dydx(division_bucket_hrs_t1) at(family_investment_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details_full.xls", ctitle(childcare latent) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* WFP latent indicator
logit dissolve i.marr_dur c.broad_policy_t i.division_bucket_hrs_t1 c.broad_policy_t#i.division_bucket_hrs_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum broad_policy_t, detail
margins, dydx(division_bucket_hrs_t1) at(broad_policy_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details_full.xls", ctitle(policy latent) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Structural scale using Factor Analysis
logit dissolve i.marr_dur c.structural_factor_t i.division_bucket_hrs_t1 c.structural_factor_t#i.division_bucket_hrs_t1 $controls $macro_controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_factor_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_factor_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_scale_details_full.xls", ctitle(sf latent) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
*/

********************************************************************************
********************************************************************************
********************************************************************************
**# Results by education - moreso for selection (R2, Response 6)
* Some of this in main file, but have deeper dive here esp. re: relationship type
********************************************************************************
********************************************************************************
********************************************************************************
// not in paper

// First, need to show the supporting evidence about divorce rates (since I can't split specifically by couple type. Let's see if I can make same argument for division of labor?) Okay I need to think about this. Have I tried neither, one, both?. YES KIM START DOCUMENTING THIS - there are just truly not enough divorces (especially considering I am using an interaction which is just creating many cells)
tab educ_type dissolve if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, row
tab educ_type dissolve if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & children_under6==1, row

tab educ_type hh_hours_type_t1 if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, row
tab educ_type hh_hours_type_t1 if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & children_under6==1, row

********************************************************************************
* Cohab main effects
********************************************************************************
// have to remove educ type from controls

local cont "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.moved_last2 i.couple_joint_religion i.num_children" // i.educ_type

logit dissolve i.coh_dur i.hh_hours_type_t1##i.couple_educ_gp `cont' $macro_controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or cluster(couple_id)
margins 1.couple_educ_gp, dydx(2.hh_hours_type_t1) level(95) post
est store col_male

logit dissolve i.coh_dur i.hh_hours_type_t1##i.couple_educ_gp `cont' $macro_controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or cluster(couple_id)
margins 1.couple_educ_gp, dydx(3.hh_hours_type_t1) level(95) post
est store col_fem

logit dissolve i.coh_dur i.hh_hours_type_t1##i.couple_educ_gp `cont' $macro_controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or cluster(couple_id)
margins 0.couple_educ_gp, dydx(2.hh_hours_type_t1) level(95) post
est store no_male

logit dissolve i.coh_dur i.hh_hours_type_t1##i.couple_educ_gp `cont' $macro_controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or cluster(couple_id)
margins 0.couple_educ_gp, dydx(3.hh_hours_type_t1) level(95) post
est store no_fem

coefplot col_male col_fem no_male no_fem

coefplot (col_male, mcolor(navy) ciopts(color(navy)) label("Male BW")) (col_fem, mcolor(eltblue) ciopts(color(eltblue)) label("Female BW")) ///
		(no_male, mcolor(navy) ciopts(color(navy)) nokey) (no_fem, mcolor(eltblue) ciopts(color(eltblue)) nokey) ///
		,  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) ///
		coeflabels(1.couple_educ_gp = "At Least One Has College Degree" 0.couple_educ_gp = "Neither Has College Degree") legend(position(bottom) rows(1))

********************************************************************************
* Marriage main effects
********************************************************************************
// make above cohab plot but for marriage as well
local cont "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.moved_last2 i.couple_joint_religion i.num_children" // i.educ_type

logit dissolve i.marr_dur i.hh_hours_type_t1##i.couple_educ_gp `cont' $macro_controls if current_rel_type==20 & any_missing==0 & no_labor==0 & marr_dur>=0, or cluster(couple_id)
margins 1.couple_educ_gp, dydx(2.hh_hours_type_t1) level(95) post
est store col_male1

logit dissolve i.marr_dur i.hh_hours_type_t1##i.couple_educ_gp `cont' $macro_controls if current_rel_type==20 & any_missing==0 & no_labor==0 & marr_dur>=0, or cluster(couple_id)
margins 1.couple_educ_gp, dydx(3.hh_hours_type_t1) level(95) post
est store col_fem1

logit dissolve i.marr_dur i.hh_hours_type_t1##i.couple_educ_gp `cont' $macro_controls if current_rel_type==20 & any_missing==0 & no_labor==0 & marr_dur>=0, or cluster(couple_id)
margins 0.couple_educ_gp, dydx(2.hh_hours_type_t1) level(95) post
est store no_male1

logit dissolve i.marr_dur i.hh_hours_type_t1##i.couple_educ_gp `cont' $macro_controls if current_rel_type==20 & any_missing==0 & no_labor==0 & marr_dur>=0, or cluster(couple_id)
margins 0.couple_educ_gp, dydx(3.hh_hours_type_t1) level(95) post
est store no_fem1

coefplot (col_male1, mcolor(navy) ciopts(color(navy)) label("Male BW")) (col_fem1, mcolor(eltblue) ciopts(color(eltblue)) label("Female BW")) ///
		(no_male1, mcolor(navy) ciopts(color(navy)) nokey) (no_fem1, mcolor(eltblue) ciopts(color(eltblue)) nokey) ///
		,  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) ///
		coeflabels(1.couple_educ_gp = "At Least One Has College Degree" 0.couple_educ_gp = "Neither Has College Degree") legend(position(bottom) rows(1))

// Can I combine?
coefplot (col_male, mcolor(navy) ciopts(color(navy)) label("Male BW")) (col_fem, mcolor(eltblue) ciopts(color(eltblue)) label("Female BW")) ///
		(no_male, mcolor(navy) ciopts(color(navy)) nokey) (no_fem, mcolor(eltblue) ciopts(color(eltblue)) nokey), bylabel("A. Cohabitation") || ///
		(col_male1, mcolor(navy) ciopts(color(navy)) label("Male BW")) (col_fem1, mcolor(eltblue) ciopts(color(eltblue)) label("Female BW")) ///
		(no_male1, mcolor(navy) ciopts(color(navy)) nokey) (no_fem1, mcolor(eltblue) ciopts(color(eltblue)) nokey), bylabel("B. Marriage"), ///
		,  xline(0, lcolor("red")) levels(90) xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) byopts(xrescale) ///
		coeflabels(1.couple_educ_gp = "At Least One Has College Degree" 0.couple_educ_gp = "Neither Has College Degree") legend(position(bottom) rows(1))
		// groups(1.couple_educ_gp = "At Least One Has College Degree" 0.couple_educ_gp = "Neither Has College Degree") nolabel
		
********************************************************************************
* Moderation (this is what I think might be in main results at the moment)
********************************************************************************
// So, the No College, 1 College is in main. I want to try *one* more time the one v. both
// this is the problem I am remembering - there are in theory 1500 couples with both college, but because of perfect prediction / collinearity, in my normal models, the same is only ~600
tab educ_type if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & children_under6==1

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & educ_type==4, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) 

// it gets closer if I remove state fixed effects and marital duration as discrete (1200) - the results are the same as above, just the above feel so problematic bc it's like 1/3 of the sample...
// if I also don't make earnings discrete, I get more. again, the results are all the same direction (and get smaller effects here than above, which feels better actually)
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner c.earnings_1000s i.educ_type i.moved_last2 i.couple_joint_religion i.num_children" // i.state_fips 

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 `controls' $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & educ_type==4, or cluster(couple_id)
margins, dydx(hh_hours_type_t1)

logit dissolve marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 `controls' $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & educ_type==4, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) 


********************************************************************************
********************************************************************************
********************************************************************************
**# Results by race (Figures) - bc could also be select on race?? (R2, Response 6)
* Think I will not include bc they mention education and I already can't do a
* lot on selection, so don't want to add additional layers to confuse the key
* points I am attempting to make...
* Think I WILL include actually as I assess for R&R take 2. leaving here for now, 
* Might move to main file
********************************************************************************
********************************************************************************
********************************************************************************

********************************************************************************
* White (wife's race)
********************************************************************************
** Main effects
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0  & raceth_wife==1, or cluster(couple_id)
margins, dydx(hh_hours_type_t1) level(95) post
estimates store w_esta

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & raceth_wife==1, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1) level(95) post
estimates store w_estb

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & raceth_wife==1, or cluster(couple_id)
margins, dydx(structural_familism_t) post
estimates store w_estc

** Paid Labor interaction
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & raceth_wife==1, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est5w

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & raceth_wife==1, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(3.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est6w

coefplot (est5w, mcolor(navy) ciopts(color(navy)) label("Male BW")) (est6w, label("Female BW")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical))

** Combined DoL (Hours) Interaction
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & raceth_wife==1, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est10w

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & raceth_wife==1, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(3.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est11w

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & raceth_wife==1, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(4.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est12w

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & raceth_wife==1, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(5.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est13w

coefplot (est10w, mcolor(blue) ciopts(color(blue)) label("Traditional")) (est12w, label("Second Shift") mcolor(gs8) ciopts(color(gs8)))  (est13w, label("All Others") mcolor(black) ciopts(color(black))),  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical)) xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) legend(position(bottom) rows(1)) // (est11c, label("Counter-Traditional"))

********************************************************************************
* Black (wife's race)
********************************************************************************
** Main effects
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0  & raceth_wife==2, or cluster(couple_id)
margins, dydx(hh_hours_type_t1) level(95) post
estimates store b_esta

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & raceth_wife==2, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1) level(95) post
estimates store b_estb

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & raceth_wife==2, or cluster(couple_id)
margins, dydx(structural_familism_t) post
estimates store b_estc

** Paid Labor interaction
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & raceth_wife==2, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est5b

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & raceth_wife==2, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(3.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est6b

coefplot (est5b, mcolor(navy) ciopts(color(navy)) label("Male BW")) (est6b, label("Female BW")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical))

** Combined DoL (Hours) Interaction
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & raceth_wife==2, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est10b

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & raceth_wife==2, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(3.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est11b

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & raceth_wife==2, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(4.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est12b

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & raceth_wife==2, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(5.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est13b

coefplot (est10b, mcolor(blue) ciopts(color(blue)) label("Traditional")) (est11b, label("Counter-Traditional")) (est12b, label("Second Shift"))  (est13b, label("All Others")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical)) xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) legend(position(bottom) rows(1))

********************************************************************************
* Combined Figures
********************************************************************************
* Main effects
coefplot (b_esta, offset(.20) nokey) (b_estb, offset(-.20) nokey) , bylabel("Black Women")  || ///
(w_esta, offset(.20) nokey) (w_estb, offset(-.20) nokey), bylabel("White Women") || ///
, drop(_cons) xline(0) levels(95) base xtitle(Average Marginal Effects, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners"  1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" structural_familism_t = "Structural Support Scale") xsize(8) ///
 headings(1.hh_hours_type_t1= "{bf:Division of Work Hours}" 1.division_bucket_hrs_t1 = "{bf:Combined DoL}" structural_familism_t = "{bf:Structural Support}")
 
 // (w_estc, nokey lcolor("black") mcolor("black") ciopts(color("black")))
 // (b_estc, nokey lcolor("black") mcolor("black") ciopts(color("black")))
 
* Paid Labor
coefplot (est5b, mcolor(navy) ciopts(color(navy)) label("Male BW")) (est6b, label("Female BW")), bylabel("Black Women") || ///
		(est5w, mcolor(navy) ciopts(color(navy)) label("Male BW")) (est6w, label("Female BW")), bylabel("White Women") || ///
,  drop(_cons) nolabel xline(0, lcolor("red")) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical)) xsize(8) byopts(xrescale)

/*
Alt for comparison
	coefplot (est5b, mcolor(navy) ciopts(color(navy) lpattern("---")) label("Black Women: Male BW")) (est5w, mcolor(navy) ciopts(color(navy)) label("White Women: Male BW")) ///
	(est6b, mcolor(ltblue) ciopts(color(ltblue) lpattern("---")) label("Black Women: Female BW")) (est6w, mcolor(ltblue) ciopts(color(ltblue)) label("White Women: Female BW")) ///
	,  drop(_cons) nolabel xline(0, lcolor("red")) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical)) xsize(7)
	
		coefplot (est5b, mcolor(navy) ciopts(color(navy) lpattern("---")) label("Black Women: Male BW")) (est5w, mcolor(navy) ciopts(color(navy)) label("White Women: Male BW")) ///
	,  drop(_cons) nolabel xline(0, lcolor("red")) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical)) xsize(7)
*/

* Combined DoL
coefplot (est10b, mcolor(blue) ciopts(color(blue)) label("Traditional")) (est11b, label("Counter-Traditional")) (est12b, label("Second Shift"))  (est13b, label("All Others")), bylabel("Black Women") || ///
(est10w, mcolor(blue) ciopts(color(blue)) label("Traditional")) (est11w, label("Counter-Traditional")) (est12w, label("Second Shift") mcolor(gs8) ciopts(color(gs8)))  (est13w, label("All Others") mcolor(black) ciopts(color(black))), bylabel("White Women")  || ///
,  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical)) xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) legend(position(bottom) rows(1)) xsize(8) byopts(xrescale)

/*
Alt for comparison
	coefplot (est10b, mcolor(blue) ciopts(color(blue) lpattern("---")) label("Black Women: Trad")) (est10w, mcolor(blue) ciopts(color(blue)) label("White Women: Trad")) ///
	(est12b, mcolor(black) ciopts(color(black) lpattern("---")) label("Black Women: Second Shift")) (est12w, mcolor(black) ciopts(color(black)) label("White Women: Second Shift")) ///
	,  drop(_cons) nolabel xline(0, lcolor("red")) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Egalitarian, size(small)) legend(position(bottom) rows(1)) groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical))
	
	coefplot (est10b, mcolor(blue) ciopts(color(blue) lpattern("---")) label("Black Women: Trad")) (est10w, mcolor(blue) ciopts(color(blue)) label("White Women: Trad")) ///
	,  drop(_cons) nolabel xline(0, lcolor("red")) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Egalitarian, size(small)) legend(position(bottom) rows(1)) groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical))
*/

********************************************************************************
********************************************************************************
********************************************************************************
**# Sample age robustsness (R2, Response 7)
********************************************************************************
********************************************************************************
********************************************************************************

// This was just in R1 Letter, does not remain in paper, but probably should remain in code.

tab age_flag_2055 if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & children_under6==1
unique unique_id partner_unique_id if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & children_under6==1 // 1828
unique unique_id partner_unique_id if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & children_under6==1 & age_flag_2055==1 // 1813

tab age_flag_2055 educ_type if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & children_under6==1

// is it even worth showing if I literally only remove 12 couples?
// main effects - compare to total sample for ref
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(hh_hours_type_t1) post
outreg2 using "$results/main results/dissolution_AMES_age.xls", ctitle(18+) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & age_flag_2055==1, or cluster(couple_id)
margins, dydx(hh_hours_type_t1) post
outreg2 using "$results/main results/dissolution_AMES_age.xls", ctitle(20+) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1) post
outreg2 using "$results/main results/dissolution_AMES_age.xls", ctitle(18+) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & age_flag_2055==1, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1) post
outreg2 using "$results/main results/dissolution_AMES_age.xls", ctitle(20+) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// interaction
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))  post
outreg2 using "$results/main results/dissolution_AMES_age.xls", ctitle(18+) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0  & age_flag_2055==1, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))  post
outreg2 using "$results/main results/dissolution_AMES_age.xls", ctitle(20+) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))  post
outreg2 using "$results/main results/dissolution_AMES_age.xls", ctitle(18+) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0  & age_flag_2055==1, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))  post
outreg2 using "$results/main results/dissolution_AMES_age.xls", ctitle(20+) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

********************************************************************************
********************************************************************************
********************************************************************************
**# Relationship start robustness (R1, Response 4)
********************************************************************************
********************************************************************************
********************************************************************************

**Main effects
// Current (1995)
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(hh_hours_type_t1) post
outreg2 using "$results/main results/dissolution_AMES_start.xls", ctitle(Main: 1995+) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1) post
outreg2 using "$results/main results/dissolution_AMES_start.xls", ctitle(Main: 1995+) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// 2000 
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & marriage_start_yr>=2000, or cluster(couple_id)
margins, dydx(hh_hours_type_t1) post
outreg2 using "$results/main results/dissolution_AMES_start.xls", ctitle(Main: 2000+) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & marriage_start_yr>=2000, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1) post
outreg2 using "$results/main results/dissolution_AMES_start.xls", ctitle(Main: 2000+) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// 2005
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & marriage_start_yr>=2005, or cluster(couple_id)
margins, dydx(hh_hours_type_t1) post
outreg2 using "$results/main results/dissolution_AMES_start.xls", ctitle(Main: 2005+) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & marriage_start_yr>=2005, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1) post
outreg2 using "$results/main results/dissolution_AMES_start.xls", ctitle(Main: 2005+) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

**Interaction
// Current (1995) - 5,610 observations used
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_start.xls", ctitle(SF: 1995+) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_start.xls", ctitle(SF: 1995+) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// 2000 - 4271 observations - same, standard errors get larger, but still a lot sig
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & marriage_start_yr>=2000, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_start.xls", ctitle(SF: 2000+) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & marriage_start_yr>=2000, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_start.xls", ctitle(SF: 2000+) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// 2005 - 2730 observations. directly all same - but here, no sig. because sample is like halved (even less) - also, we're now putting upper limit on length of relationships, so these are shorter relationships than above
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & marriage_start_yr>=2005, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_start.xls", ctitle(SF: 2005+) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & marriage_start_yr>=2005, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_start.xls", ctitle(SF: 2005+) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

********************************************************************************
*Is there CHANGE over time? (R3, Round 2)
********************************************************************************
// Wait I wanted to see what happened if I went back to 1990. BUT i can't do that here because I do the restrictions earlier in file. so need to put this in NEW FILE
// see: tmp_change-over-time.do

********************************************************************************
********************************************************************************
********************************************************************************
**# Alt operationalization of Combined DoL (R1, Response 6)
********************************************************************************
********************************************************************************
********************************************************************************
/*
// In looking at R1, I am wondering, to make the point about WFC, do I need to specifically compare male-BW HHs to male-BW dual-HW HHs, and possibly her second-shift HHs?
// male-BW dual-HW HHs are v. small. But if it's about reducing conflict - it really needs to be specialized on both? But also - if it was about NORMS - that is why her second shift provides this evidence
// let's come back to this...okay I was thinking about this all wrong - it's like true egal usually not possible - her second shift. instead of that- is male BW better.
// so do I need to compare trad to her second shifT? let's see what happens if I make trad the ref group not egal

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1)

logit dissolve i.marr_dur c.structural_familism_t ib2.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1) // so it is lower but not sig.

logit dissolve i.marr_dur c.structural_familism_t ib2.division_bucket_hrs_t1 c.structural_familism_t#ib2.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) // so there is a similar moderation effect relative to her second shift - but not sig. so it is interesting - like why not egal? I think *this* is where the labor market part comes in - like her employment is possibly not worth it when there is not supportive work-family policy. like women cannot stay attached to the labor market because it's too difficult (like Damaske book argument, it's just too hard)

// is this useful to show at all? or not concrete enough?
* Combined: Predicted Probability
logit dissolve i.marr_dur c.structural_familism_t ib2.division_bucket_hrs_t1 c.structural_familism_t#ib2.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins division_bucket_hrs_t1, at(structural_familism_t=(`r(min)'(1)`r(max)'))
marginsplot, xtitle("Structural Support for Dual-Earning") ytitle("Predicted Probability of Marital Dissolution") title("") legend(position(6) ring(3) rows(1)) noci recast(line) xlabel(#10) plot2opts(lcolor("blue") mcolor("blue")) plot3opts(lcolor("ltblue") mcolor("ltblue")) plot4opts(lcolor("gs8") mcolor("gs8")) plot5opts(lcolor("black") mcolor("black"))  

* Combined: AMEs
logit dissolve i.marr_dur c.structural_familism_t ib2.division_bucket_hrs_t1 c.structural_familism_t#ib2.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(1.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store m_1

logit dissolve i.marr_dur c.structural_familism_t ib2.division_bucket_hrs_t1 c.structural_familism_t#ib2.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(3.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store m_3

logit dissolve i.marr_dur c.structural_familism_t ib2.division_bucket_hrs_t1 c.structural_familism_t#ib2.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(4.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store m_4

logit dissolve i.marr_dur c.structural_familism_t ib2.division_bucket_hrs_t1 c.structural_familism_t#ib2.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(5.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store m_5

coefplot (m_1, mcolor(red) ciopts(color(red)) label("Egalitarian")) (m_3, label("Counter-Traditional")) (m_4, label("Second Shift"))  (m_5, label("All Others")),  drop(_cons) nolabel xline(0, lcolor("blue")) levels(90) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical)) xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) legend(position(bottom) rows(1))
*/

********************************************************************************
********************************************************************************
********************************************************************************
**# Attempting to deal with selection into the DoL itself (R&R Take 2)
********************************************************************************
********************************************************************************
********************************************************************************

// if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0

********************************************************************************
* Distribution of DoL across states / at different levels of state policy indicator
********************************************************************************
// let's get state distro: a. on average and b. over time so can maybe go with state policy over time chart
gen egal = .
replace egal = 0 if inlist(division_bucket_hrs_t1,2,3,4,5)
replace egal = 1 if division_bucket_hrs_t1==1

gen trad = .
replace trad = 0 if inlist(division_bucket_hrs_t1,1,3,4,5)
replace trad = 1 if division_bucket_hrs_t1==2

tab division_bucket_hrs_t1, gen(div_labor)
tab hh_hours_type_t1, gen(paid_labor)

tabstat div_labor2 if in_analytical_sample, by(state_fips)
tabstat div_labor2 if in_analytical_sample & year==1995, by(state_fips)
tabstat div_labor2 if in_analytical_sample & year==2005, by(state_fips) // I actually don't think this is going to work bc sample size so small.
tabstat div_labor2 if in_analytical_sample & year==2015, by(state_fips)

tabstat div_labor2 if in_analytical_sample & year>1995 & year<2005, by(state_fips)
tabstat div_labor2 if in_analytical_sample & year>=2005 & year<2015, by(state_fips)
tabstat div_labor2 if in_analytical_sample & year>=2015 & year<2020, by(state_fips)

tabstat div_labor2 if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & year>1995 & year<2005, by(state_fips)
tabstat div_labor2 if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & year>=2005 & year<2015, by(state_fips)
tabstat div_labor2 if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & year>=2015 & year<2020, by(state_fips)

// create policy variables to differentiate
gen high_policy_support=.
sum structural_familism_t, detail
replace high_policy_support=0 if structural_familism_t < = `r(p50)'
replace high_policy_support=1 if structural_familism_t > `r(p50)' & structural_familism_t!=.

xtile policy_gp = structural_familism_t, nq(4)

// descriptive
tabstat structural_familism_t, by(high_policy_support) stats(min max mean)
tabstat structural_familism_t, by(policy_gp) stats(min max mean)

tab hh_hours_type_t1 high_policy_support if in_analytical_sample==1, col
tab division_bucket_hrs_t1 high_policy_support if in_analytical_sample==1, col
tab ft_t1_wife high_policy_support if in_analytical_sample==1, col
tab ft_t1_head high_policy_support if in_analytical_sample==1, col

tab hh_hours_type_t1 policy_gp if in_analytical_sample==1, col
tab division_bucket_hrs_t1 policy_gp if in_analytical_sample==1, col
tab ft_t1_wife policy_gp if in_analytical_sample==1, col
tab ft_t1_head policy_gp if in_analytical_sample==1, col

tabstat female_hours_pct_t1 wife_housework_pct_t if in_analytical_sample==1, by(high_policy_support) // wait does THIS illustrate the paradox? (from Ruppanner)
tabstat female_hours_pct_t1 wife_housework_pct_t if in_analytical_sample==1, by(high_policy_support) stats(p50)
tabstat female_hours_pct_t1 wife_housework_pct_t if in_analytical_sample==1, by(policy_gp) stats(mean p25 p50 p75)
tabstat female_hours_pct_t1 wife_housework_pct_t if in_analytical_sample==1, by(policy_gp) stats(mean)
tabstat female_hours_pct_t1 wife_housework_pct_t if in_analytical_sample==1, by(policy_gp) stats(p50)

tabstat structural_familism_t, by(state_fips) stats(mean N)
tab state_fips division_bucket_hrs_t1 if in_analytical_sample==1, row nofreq
tab state_fips division_bucket_hrs_t1 if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, row nofreq // all married but not just parents

twoway (histogram female_hours_pct_t1 if in_analytical_sample==1 & high_policy_support==0, width(.025) color(teal%30)) (histogram female_hours_pct_t1 if in_analytical_sample==1 & high_policy_support==1, width(.025) color(pink%30)), legend(order(1 "Low Support" 2 "High Support") rows(1) position(6)) xtitle("Women's % Paid Work Hours") // width(.05) 

twoway (histogram wife_housework_pct_t if in_analytical_sample==1 & high_policy_support==0, width(.025) color(teal%30)) (histogram wife_housework_pct_t if in_analytical_sample==1 & high_policy_support==1, width(.025) color(pink%30)), legend(order(1 "Low Support" 2 "High Support") rows(1) position(6)) xtitle("Women's % Housework Hours") // width(.05) 

	// oh wait this was not yet restricted to married couples OR parents DUH KIM
	tab hh_hours_type_t1 high_policy_support, col
	tab division_bucket_hrs_t1 high_policy_support, col

	tab hh_hours_type_t1 policy_gp, col
	tab division_bucket_hrs_t1 policy_gp, col

	tabstat female_hours_pct_t1 wife_housework_pct_t, by(high_policy_support) // wait does THIS illustrate the paradox? (from Ruppanner)
	tabstat female_hours_pct_t1 wife_housework_pct_t, by(policy_gp)
	
// I guess I could also see if my state policy indicator predicts either women's share of earnings OR DoL. DOL probably harder because 5 categories so let's do women's share of earnings. could even use a chi-squared?
tab hh_hours_type_t1 policy_gp if in_analytical_sample==1, col chi
tab division_bucket_hrs_t1 policy_gp if in_analytical_sample==1, col chi

tab division_bucket_hrs_t1 egal
tab division_bucket_hrs_t1 trad

ttest trad if in_analytical_sample==1, by(high_policy_support)
ttest egal if in_analytical_sample==1, by(high_policy_support)

pwcorr structural_familism_t trad egal // there is no correlation here either. I could produce 3 side-by-side maps? if that helps...(but really want to show the distribution by group of policies I think)

// formally t-test all
ttest div_labor1 if in_analytical_sample==1, by(high_policy_support) // Egal
ttest div_labor2 if in_analytical_sample==1, by(high_policy_support) // Trad 
ttest div_labor3 if in_analytical_sample==1, by(high_policy_support) // Counter
ttest div_labor4 if in_analytical_sample==1, by(high_policy_support) // Second Shift 
ttest div_labor5 if in_analytical_sample==1, by(high_policy_support) // Other

ttest paid_labor1 if in_analytical_sample==1, by(high_policy_support) // Dual
ttest paid_labor2 if in_analytical_sample==1, by(high_policy_support) // Male
ttest paid_labor3 if in_analytical_sample==1, by(high_policy_support) // Female

ttest ft_t1_wife if in_analytical_sample==1, by(high_policy_support)
ttest ft_t1_head if in_analytical_sample==1, by(high_policy_support)
ttest female_hours_pct_t1 if in_analytical_sample==1, by(high_policy_support)
ttest wife_housework_pct_t if in_analytical_sample==1, by(high_policy_support)

regress female_hours_pct_t1 i.policy_gp if in_analytical_sample==1 // this is just mean differences though. Could adjust for controls?
regress female_hours_pct_t1 i.policy_gp $controls $macro_controls if in_analytical_sample==1

regress wife_housework_pct_t i.policy_gp if in_analytical_sample==1 // this is just mean differences though. Could adjust for controls?
regress wife_housework_pct_t i.policy_gp $controls $macro_controls if in_analytical_sample==1

// just two groups (feels easier to compare)
regress female_hours_pct_t1 i.high_policy_support if in_analytical_sample==1 // this is just mean differences though. Could adjust for controls?
margins, at(high_policy_support=(0 1))
regress female_hours_pct_t1 i.high_policy_support $controls $macro_controls if in_analytical_sample==1
margins, at(high_policy_support=(0 1))

regress wife_housework_pct_t i.high_policy_support if in_analytical_sample==1 // this is just mean differences though. Could adjust for controls?
regress wife_housework_pct_t i.high_policy_support $controls $macro_controls if in_analytical_sample==1
margins, at(high_policy_support=(0 1))

// okay wait this actually works
mlogit division_bucket_hrs_t1 structural_familism_t if in_analytical_sample==1  // $controls $macro_controls 
sum structural_familism_t, det
margins, at(structural_familism_t=(`r(min)' (1) `r(max)'))
marginsplot, recast(line) ciopts(recast(rarea)) xlabel(#10) plot2opts(lcolor("blue") mcolor("blue")) plot3opts(lcolor("ltblue") mcolor("ltblue")) plot4opts(lcolor("gs8") mcolor("gs8")) plot5opts(lcolor("black") mcolor("black")) ci1opts(color(red%70)) ci2opts(color(blue%70))  ci3opts(color(ltblue%70))  ci4opts(color(gs8%70))  ci5opts(color(black%70)) xtitle("Structural Support for Working Families") ytitle("Predicted Probability of Given Arrangement")  title("") legend(order(1 "Egalitarian" 2 "Traditional" 3 "Counter-Traditional" 4 "Second Shift" 5 "All Others") position(6) ring(3) rows(1))

mlogit division_bucket_hrs_t1 structural_familism_t $controls_nofe $macro_controls if in_analytical_sample==1 // not sure if it's because I remove time frm models, but the state FE don't work to estimate (i think probably because this is now CROSS-SECTIONAL and i don't want between states). okay but then I can't get SE / CIs and I think it's because of counter-traditional. will see if removing macro controls help but I don't think so. so either display without or consolidate? I just think those are intresting. and I am struggling with how to get SEs for some but not all (there is a statalist thread and the answer is that you can't)

// this runs - no state fe, no year fe, earnings as continuous (bucketed didn't work)
mlogit division_bucket_hrs_t1 structural_familism_t age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled cohab_with_partner cohab_with_other pre_marital_birth i.home_owner i.educ_type i.moved_last2 i.couple_joint_religion i.num_children i.interval c.earnings_1000s if in_analytical_sample==1 // i.year_gp 

***********************************
* this is what I am using
***********************************

// oh wait I CAN include other macro - should I do that??
mlogit division_bucket_hrs_t1 structural_familism_t age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled cohab_with_partner cohab_with_other pre_marital_birth i.home_owner i.educ_type i.moved_last2 i.couple_joint_religion i.num_children i.interval c.earnings_1000s $macro_controls if in_analytical_sample==1, rrr // i.year_gp 
margins, dydx(structural_familism_t) // is this how to get effects for all? I think so? For each outcome?
sum structural_familism_t, det
margins, at(structural_familism_t=(`r(min)' (1) `r(max)'))

marginsplot, recast(line) ciopts(recast(rarea)) xlabel(#10) plot2opts(lcolor("blue") mcolor("blue")) plot3opts(lcolor("ltblue") mcolor("ltblue")) plot4opts(lcolor("gs8") mcolor("gs8")) plot5opts(lcolor("black") mcolor("black")) ci1opts(color(red%30)) ci2opts(color(blue%30))  ci3opts(color(ltblue%30))  ci4opts(color(gs8%30))  ci5opts(color(black%30)) xtitle("Structural Support for Working Families") ytitle("Predicted Probability of Given Arrangement")  title("") legend(order(1 "Egalitarian" 2 "Traditional" 3 "Counter-Traditional" 4 "Second Shift" 5 "All Others") position(6) ring(3) rows(1))

sum structural_familism_t, det
margins, at(structural_familism_t=(`r(p25)' `r(p75)'))

/*
// could try logit with JUST egal / trad. I just worry the categorical is too much to attempt to do all at once?
logit trad i.high_policy_support if in_analytical_sample==1 
logit trad i.high_policy_support $controls $macro_controls if in_analytical_sample==1
margins, at(high_policy_support=(0 1))

logit egal i.high_policy_support $controls $macro_controls if in_analytical_sample==1
margins, at(high_policy_support=(0 1))

logit div_labor2 i.high_policy_support $controls $macro_controls if in_analytical_sample==1
margins, at(high_policy_support=(0 1))

logit div_labor3 i.high_policy_support $controls $macro_controls if in_analytical_sample==1 // ugh just do all
margins, at(high_policy_support=(0 1))

logit div_labor4 i.high_policy_support $controls $macro_controls if in_analytical_sample==1 // let's do her second shift also
margins, at(high_policy_support=(0 1))

logit div_labor5 i.high_policy_support $controls $macro_controls if in_analytical_sample==1
margins, at(high_policy_support=(0 1))

logit paid_labor1 i.high_policy_support $controls $macro_controls if in_analytical_sample==1
margins, at(high_policy_support=(0 1))

logit paid_labor2 i.high_policy_support $controls $macro_controls if in_analytical_sample==1
margins, at(high_policy_support=(0 1))

logit paid_labor3 i.high_policy_support $controls $macro_controls if in_analytical_sample==1
margins, at(high_policy_support=(0 1))

logit ft_t1_wife i.high_policy_support $controls $macro_controls if in_analytical_sample==1
margins, at(high_policy_support=(0 1))

logit ft_t1_wife structural_familism_t if in_analytical_sample==1 // kim duh you could put this in model continuous I AM DUMB
sum structural_familism_t, detail
margins, at(structural_familism_t=(`r(p25)' `r(p50)' `r(p75)'))

logit ft_t1_wife structural_familism_t $controls $macro_controls  if in_analytical_sample==1 // kim duh you could put this in model continuous I AM DUMB
sum structural_familism_t, detail
margins, at(structural_familism_t=(`r(p25)' `r(p50)' `r(p75)'))

	// differences negligible
	// mlogit division_bucket_hrs_t1 i.high_policy_support $controls $macro_controls if in_analytical_sample==1 // won't estimate - prob bc of counter-trad
	// margins, at(high_policy_support=(0 1))

	mlogit hh_hours_type_t1 i.high_policy_support $controls $macro_controls if in_analytical_sample==1 // validate negligible compared to logit each individual - it is
	margins, at(high_policy_support=(0 1))
	
// alt option - use continuous and show in figure? A map can work but a. I can't do over time and b. than I have to pick ONE % to show.
// this doesn't work because NOT COMPETING In this framework so it's confusing. I figured out the models I am just dumb
// https://repec.sowi.unibe.ch/stata/coefplot/getting-started.html#h-8

logit div_labor1 structural_familism_t $controls $macro_controls if in_analytical_sample==1
sum structural_familism_t, det
margins, at(structural_familism_t=(`r(min)' (1) `r(max)')) post
estimates store egal

logit div_labor2 structural_familism_t $controls $macro_controls if in_analytical_sample==1
sum structural_familism_t, det
margins, at(structural_familism_t=(`r(min)' (1) `r(max)')) post
estimates store trad
// marginsplot, xlabel(#10) noci recast(line) yscale(range(.2 .6)) ylabel(.2(.1).6)

logit div_labor3 structural_familism_t $controls $macro_controls if in_analytical_sample==1
sum structural_familism_t, det
margins, at(structural_familism_t=(`r(min)' (1) `r(max)')) post
estimates store counter

logit div_labor4 structural_familism_t $controls $macro_controls if in_analytical_sample==1
sum structural_familism_t, det
margins, at(structural_familism_t=(`r(min)' (1) `r(max)')) post
estimates store second

coefplot (egal, label("Egalitarian")) ///
		(trad, mcolor(blue) ciopts(color(blue)) label("Traditional")) ///
		(counter, mcolor(ltblue) ciopts(color(ltblue)) label("Counter-Traditional")) ///
		(second, mcolor(gs8) ciopts(color(gs8)) label("Second Shift")) ///
		, at recast(line) noci
*/

********************************************************************************
* then should I see if the egalitarian and traditional samples within different
* policy groups are different? (aka selection in that way?)
********************************************************************************
/*
// tab educ_type high_policy_support, col

putexcel set "$results/DoL_sample_selection_check", replace
putexcel B1:D1 = "Low Policy Support", merge border(bottom)
putexcel E1:G1 = "High Policy Support", merge border(bottom)
putexcel B2 = ("Total") C2 = ("Trad") D2 = ("Egal")
putexcel E2 = ("Total") F2 = ("Trad") G2 = ("Egal")
putexcel A3 = "Unique Couples"

putexcel A4 = "Relationship Duration"
putexcel A5 = "Husband's age at marriage"
putexcel A6 = "Wife's age at marriage"
putexcel A7 = "Total Couple Earnings"
putexcel A8 = "Neither partner has college degree"
putexcel A9 = "He has college degree"
putexcel A10 = "She has college degree"
putexcel A11 = "Both have college degree"
putexcel A12 = "Couple owns home"
putexcel A13 = "Husband's Race: NH White"
putexcel A14 = "Husband's Race: Black"
putexcel A15 = "Husband's Race: Hispanic"
putexcel A16 = "Husband's Race: NH Asian"
putexcel A17 = "Husband's Race: NH Other"
putexcel A18 = "Husband and wife same race"
putexcel A19 = "Either partner enrolled in school"
putexcel A20 = "Husband Wife Cohabit"
putexcel A21 = "Other Premarital Cohabit"
putexcel A22 = "First Birth Premarital"
putexcel A23 = "Religion: Both No Religion"
putexcel A24 = "Religion: Both Catholic"
putexcel A25 = "Religion: Both Protestant"
putexcel A26 = "Religion: One Catholic"
putexcel A27 = "Religion: One No Religion"
putexcel A28 = "Religion: Other"
putexcel A29 = "Moved Within 2 Survey Waves"
putexcel A30 = "Number of Children"

unique unique_id if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & high_policy_support==0 & trad==1
unique unique_id if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & high_policy_support==0 & egal==1
unique unique_id if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & high_policy_support==1 & trad==1
unique unique_id if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & high_policy_support==1 & egal==1

// 27
local meanvars "marr_dur age_mar_head age_mar_wife couple_earnings_t1 educ_type1 educ_type2 educ_type3 educ_type4 home_owner race_head1 race_head2 race_head3 race_head4 race_head5 same_race either_enrolled cohab_with_partner cohab_with_other pre_marital_birth religion1 religion2 religion3 religion4 religion5 religion6 moved_last2 NUM_CHILDREN_"

// Low Support: Total
forvalues w=1/27{
	local row=`w'+3
	local var: word `w' of `meanvars'
	mean `var' if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & high_policy_support==0
	matrix t`var'= e(b)
	putexcel B`row' = matrix(t`var'), nformat(#.#%)
}

// Low Support: Trad
forvalues w=1/27{
	local row=`w'+3
	local var: word `w' of `meanvars'
	mean `var' if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & high_policy_support==0 & trad==1
	matrix t`var'= e(b)
	putexcel C`row' = matrix(t`var'), nformat(#.#%)
}


// Low Support: Egal
forvalues w=1/27{
	local row=`w'+3
	local var: word `w' of `meanvars'
	mean `var' if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & high_policy_support==0 & egal==1
	matrix t`var'= e(b)
	putexcel D`row' = matrix(t`var'), nformat(#.#%)
}

// High Support: Total
forvalues w=1/27{
	local row=`w'+3
	local var: word `w' of `meanvars'
	mean `var' if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & high_policy_support==1
	matrix t`var'= e(b)
	putexcel E`row' = matrix(t`var'), nformat(#.#%)
}

// High Support: Trad
forvalues w=1/27{
	local row=`w'+3
	local var: word `w' of `meanvars'
	mean `var' if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & high_policy_support==1 & trad==1
	matrix t`var'= e(b)
	putexcel F`row' = matrix(t`var'), nformat(#.#%)
}


// High Support: Egal
forvalues w=1/27{
	local row=`w'+3
	local var: word `w' of `meanvars'
	mean `var' if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & high_policy_support==1 & egal==1
	matrix t`var'= e(b)
	putexcel G`row' = matrix(t`var'), nformat(#.#%)
}

*/

********************************************************************************
* To check against Ruppanner:
* Do childcare costs specifically predict employment??
********************************************************************************
// I only have this since 2008
// tabstat cc_pct_income_orig_t, by(year)

gen high_cc_costs=.
sum cc_pct_income_orig_t, detail
replace high_cc_costs=0 if cc_pct_income_orig_t < = `r(p50)'
replace high_cc_costs=1 if cc_pct_income_orig_t > `r(p50)' & cc_pct_income_orig_t!=.

tabstat cc_pct_income_orig_t, by(high_cc_costs)

tab hh_hours_type_t1 high_cc_costs, col
tab division_bucket_hrs_t1 high_cc_costs, col
tab high_cc_costs ft_t1_wife, row // so it does reduce women's employment...
tab high_policy_support ft_t1_wife, row // compare to here. okay these aren't actually that different...

tabstat female_hours_pct_t1 wife_housework_pct_t, by(high_cc_costs) stats(mean p50)

logit ft_t1_wife i.high_policy_support $controls $macro_controls if in_analytical_sample==1
margins, at(high_policy_support=(0 1))

logit ft_t1_wife i.high_cc_costs $controls $macro_controls if in_analytical_sample==1
margins, at(high_cc_costs=(0 1))

pwcorr cc_pct_income_orig_t maternal_u5_employment_wt_t structural_familism_t // but the high-level correlation IS negative

logit ft_t1_wife cc_pct_income_orig_t  $controls $macro_controls if in_analytical_sample==1 // so here, the effects ARE much larger than for SF, just much less sample
sum cc_pct_income_orig_t, detail
margins, at(cc_pct_income_orig_t=(`r(p25)' `r(p50)' `r(p75)'))

logit ft_t1_wife structural_familism_t  $controls $macro_controls if in_analytical_sample==1 // compare to here
sum structural_familism_t, detail
margins, at(structural_familism_t=(`r(p25)' `r(p50)' `r(p75)'))

gen cc_cost_log = ln(cc_cost_orig_t)
logit ft_t1_wife cc_cost_log if in_analytical_sample==1

gen cc_cost_1000s = cc_cost_orig_t / 1000

logit ft_t1_wife cc_cost_orig_t  $controls $macro_controls if in_analytical_sample==1 // okay coefficient very small because the scale is different, but actually large effects
sum cc_cost_orig_t, detail
margins, at(cc_cost_orig_t=(`r(p25)' `r(p50)' `r(p75)'))

logit ft_t1_wife cc_cost_1000s  $controls $macro_controls if in_analytical_sample==1 // okay I will say that I think CC costs DO negatively predict her employment. and moreso than scale? is takeaway when i use the CONTINUOUS variables

// Try this figures (exploratory) for CC costs also
mlogit division_bucket_hrs_t1 cc_pct_income_orig_t age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled cohab_with_partner cohab_with_other pre_marital_birth i.home_owner i.educ_type i.moved_last2 i.couple_joint_religion i.num_children i.interval c.earnings_1000s $macro_controls if in_analytical_sample==1, rrr // i.year_gp 

sum cc_pct_income_orig_t, det
margins, at(cc_pct_income_orig_t=(`r(min)' (.01) `r(max)'))

marginsplot, recast(line) ciopts(recast(rarea)) plot2opts(lcolor("blue") mcolor("blue")) plot3opts(lcolor("ltblue") mcolor("ltblue")) plot4opts(lcolor("gs8") mcolor("gs8")) plot5opts(lcolor("black") mcolor("black")) ci1opts(color(red%30)) ci2opts(color(blue%30))  ci3opts(color(ltblue%30))  ci4opts(color(gs8%30))  ci5opts(color(black%30)) xtitle("Childcare Costs as % of Income") ytitle("Predicted Probability of Given Arrangement")  title("") legend(order(1 "Egalitarian" 2 "Traditional" 3 "Counter-Traditional" 4 "Second Shift" 5 "All Others") position(6) ring(3) rows(1)) // xlabel(#10)

margins, dydx(cc_pct_income_orig_t) // is this how to get effects for all? I think so? For each outcome?

// And for raw costs (in $1000s)
mlogit division_bucket_hrs_t1 cc_cost_1000s age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled cohab_with_partner cohab_with_other pre_marital_birth i.home_owner i.educ_type i.moved_last2 i.couple_joint_religion i.num_children i.interval c.earnings_1000s $macro_controls if in_analytical_sample==1, rrr // i.year_gp 

sum cc_cost_1000s, det
margins, at(cc_cost_1000s=(`r(min)' (1) `r(max)'))

marginsplot, recast(line) ciopts(recast(rarea)) plot2opts(lcolor("blue") mcolor("blue")) plot3opts(lcolor("ltblue") mcolor("ltblue")) plot4opts(lcolor("gs8") mcolor("gs8")) plot5opts(lcolor("black") mcolor("black")) ci1opts(color(red%30)) ci2opts(color(blue%30))  ci3opts(color(ltblue%30))  ci4opts(color(gs8%30))  ci5opts(color(black%30)) xtitle("Childcare Costs ($1000s)") ytitle("Predicted Probability of Given Arrangement")  title("") legend(order(1 "Egalitarian" 2 "Traditional" 3 "Counter-Traditional" 4 "Second Shift" 5 "All Others") position(6) ring(3) rows(1)) // xlabel(#10)

margins, dydx(cc_cost_1000s) // is this how to get effects for all? I think so? For each outcome?

************************
* Did I ever put CC costs and SF in same model?
************************
pwcorr structural_familism_t cc_pct_income_orig_t cc_cost_orig_t earn_ratio_t married_earn_ratio_t

// I actually think these results are OPPOSITE to how I have been thinking - so in places where CC costs are higher (which are also more gender empowered - see correlation) - the association is actually opposite to what i'd expect? I'd expect male-BW to be more beneficial when costs HIGH if she needs to leave labor market. BUT it migth be these are capturing something different which is actually what these results are saying? (if higher costs associated with more policy support and smaller gender wage gap - which is what these are saying...). I think I have to revisit if I wrote this up right in paper but I think this makes the case as to why selection into employment doesn't mean SATISFACTION
logit dissolve i.marr_dur c.cc_pct_income_orig_t c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1  c.cc_pct_income_orig_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & year>=2009, or cluster(couple_id)
sum cc_pct_income_orig_t, detail
margins, dydx(division_bucket_hrs_t1) at(cc_pct_income_orig_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

// let's check if holds for wife's FT employment (aka the Ruppanner outcome) - so yeah it does. so this is exactly why it's a puzzle. CC costs REDUCES her employemnt - but her employment is WORSE for divorce when state context is poor AND childcare costs are low (because NOT SUPPORTED IN OTHER WAYS). so this directly speaks to how my results speak to ruppanner, okay this is extremely interesting.
logit dissolve i.marr_dur c.cc_pct_income_orig_t c.structural_familism_t i.ft_t1_wife c.structural_familism_t#i.ft_t1_wife  c.cc_pct_income_orig_t#i.ft_t1_wife $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & year>=2009, or cluster(couple_id)
sum cc_pct_income_orig_t, detail
margins, dydx(ft_t1_wife) at(cc_pct_income_orig_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

sum structural_familism_t, detail
margins, dydx(ft_t1_wife) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

********************************************************************************
********************************************************************************
********************************************************************************
**# Do I want to control for any conditions at time of marriage?? (R&R Take 2)
********************************************************************************
********************************************************************************
********************************************************************************

// This would be to possibly account for selection. I still think lack of selection into DoL is sufficient, but maybe you can argue these associations reflect selection into marriage based on conditions then? (that feeels like a stretch) but this is what would be accounted for. but I still don't have the CF of who didn't marry then [that is sort of the Schneider argument].

global marr_controls "men_unemp_rate_wt_tmar college_ratio_wt_tmar sex_ratio_marriage_wt_tmar"

// pwcorr structural_familism_t men_unemp_rate_wt_tmar college_ratio_wt_tmar sex_ratio_marriage_wt_tmar
// pwcorr structural_familism_tmar men_unemp_rate_wt_tmar college_ratio_wt_tmar sex_ratio_marriage_wt_tmar

/*Reminder of original models:
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) 

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) 
*/

// Okay, results are virtually identical with ALL as controls
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls $marr_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) 
margins, dydx(hh_hours_type_t1)

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls $marr_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) 
margins, dydx(division_bucket_hrs_t1)

// do I also want to interact these? That feels CRAZY. Okay did one example and the results still remain virtually unchanged (men's unemployment at time of marriage)
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.men_unemp_rate_wt_tmar#i.division_bucket_hrs_t1 $controls $macro_controls $marr_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.men_unemp_rate_wt_tmar#i.division_bucket_hrs_t1 $controls $macro_controls $marr_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum men_unemp_rate_wt_tmar, detail
margins, dydx(division_bucket_hrs_t1) at(men_unemp_rate_wt_tmar=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

// what stata list says is I can actually just use regress and then do vif because it's really about right hand side-by-side

/*
https://stats.oarc.ucla.edu/stata/webbooks/reg/chapter2/stata-webbooksregressionwith-statachapter-2-regression-diagnostics/
As a rule of thumb, a variable whose VIF values are greater than 10 may merit further investigation. Tolerance, defined as 1/VIF, is used by many researchers to check on the degree of collinearity. A tolerance value lower than 0.1 is comparable to a VIF of 10. It means that the variable could be considered as a linear combination of other independent variables. 
But see also (re: interactions): https://www.statalist.org/forums/forum/general-stata-discussion/general/1404320-how-to-interpret-different-vif-values-and-coefficients-before-after-introducing-interaction-terms
*/

regress dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls $marr_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0
estat vif
// variables with high VIF include women's college rates and married women's employment rates (this is probably not surprising)
// also age at marriage variables which are also not surprising
// okay - but now that i have added EVANGELICAL RATE - it actually has a VERY HIGH VIF. is THIS a justification to remove?? omg i am soooo dumb what do I do
// but okay, not like a ton of variables with high VIFs all around (though all of the macro variables at time t are somewhat elevated (6-10))

********************************************************************************
********************************************************************************
********************************************************************************
**# Regional Specific Estimates (R&R Take 2)
********************************************************************************
********************************************************************************
********************************************************************************
// thinking this addresses both R2 and R3 point re: selection or if certain states / regions driving results [if variation within similar regions, like the South, exists, suggest robustness - a la Glass and Levchak robustness]. Think need to remove state fixed effects here as well
tabstat structural_familism_t, by(region) stats(mean p50 min max)

* Add main models to outreg
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_R&R2.xls", ctitle(Paid: Main) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_R&R2.xls", ctitle(Combined: Main) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Paid labor only
local cont "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.educ_type i.moved_last2 i.couple_joint_religion i.num_children" 

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 `cont' $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & region==3, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_R&R2.xls", ctitle(Paid: South) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Combined
local cont "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.educ_type i.moved_last2 i.couple_joint_religion i.num_children" 

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 `cont' $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & region==3, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_R&R2.xls", ctitle(Combined: South) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

*** Do I have to show other regions? G&L just do south. one challenge is that sample is really restricted in some regions. In line with Ruppanner's book, could group NE / West (though really about coastal west I think so we're starting to get less restrictive with just "west"). I guess I have states - even though PSID uses big region, I could use state to get division instead that is more detailed? But again, now I'm just creating arbitrary groups of states. I think the South makes sense because justified by a. G&L 2014 and b. it's the bible belt R3 mentioned. The rest will become a bit arbitrary (but also - even this analysis here supports main findings)

// also if the question, like R3 is about South specifically, not sure this adds value
tab region if in_analytical_sample==1

// NE and West
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls_nofe $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & inlist(region,1,4), or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_R&R2.xls", ctitle(Paid: NE + West) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls_nofe $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & inlist(region,1,4), or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_R&R2.xls", ctitle(Combined: NE + West) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// but then do I need to do the North Central?
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls_nofe $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & region==2, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_R&R2.xls", ctitle(Paid: Central) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls_nofe $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & region==2, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_R&R2.xls", ctitle(Combined: Central) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append


********************************************************************************
* I wonder if I do at quintiles of norms - this is also somewhat regional because I have regional norms?
* (again, inspired by G&L 2014)
********************************************************************************
// I think I am struggling - in some ways, how is this different to regional? we want to see if variation holding something else constant
// but now we're conditioning on norms. Is this another way of testing the independent effect of structural support v. norms? 
// aka can good policy overcome unsupportive norms? BUT - is there REALLY GOOD policy when norms are low (this is why I am concnered this is now telling me something different) - because like yes it's holding norms constant but if we're restricting policy this doesn't necessarily tell us if policy can overcome if not enough additional policy variation (but again - isn't this also true in regional analysis?)

// I think I also have some concerns that low norms are also just HISTORICAL time? 
tabstat avg_egal_reg_t structural_familism_t, by(year)
tab year norms_gp, row // okay yeah - this is now about historical time NOT just norms - so this is a problem. dO i do the quantiles WITHIn years? then i really like ... no longer no what I am doing...
// can I use by or group?

xtile norms_gp = avg_egal_reg_t, nq(4)
tabstat avg_egal_reg_t, by(norms_gp) stats(min max mean)
tabstat structural_familism_t, by(norms_gp) stats(mean p50 min max)
pwcorr avg_egal_reg_t structural_familism_t

gen norms_gp_year=.

forvalues y=1995/1996{
	capture drop xq
	xtile xq=avg_egal_reg_t if year==`y', nq(4)
	replace norms_gp_year=xq if year==`y'
}
forvalues y=1997(2)2019{
	capture drop xq
	xtile xq=avg_egal_reg_t if year==`y', nq(4)
	replace norms_gp_year=xq if year==`y'
}

browse year avg_egal_reg_t norms_gp_year norms_gp
tab year norms_gp_year, row
tabstat avg_egal_reg_t, by(norms_gp_year) stats(min max mean)

// basically - the moderation effect exists at all but lowest norms - which makes sense? but still not convinced this is useful given many other confounders
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls_nofe if in_analytical_sample==1 & norms_gp==1, or cluster(couple_id) // should I remove macro-controls AND state-fe? is this essentially an interaction?
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls_nofe if in_analytical_sample==1 & norms_gp==2, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls_nofe if in_analytical_sample==1 & norms_gp==3, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls_nofe if in_analytical_sample==1 & norms_gp==4, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

// adjusted for YEAR. the problem is this like...still isn't telling me that much? because might be high in a given year but low relatively...
// honestly, the results are similar. effects same except for in norms group 1
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls_nofe if in_analytical_sample==1 & norms_gp_year==1, or cluster(couple_id) // should I remove macro-controls? is this essentially an interaction?
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls_nofe if in_analytical_sample==1 & norms_gp_year==2, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls_nofe if in_analytical_sample==1 & norms_gp_year==3, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls_nofe if in_analytical_sample==1 & norms_gp_year==4, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

********************************************************************************
* Could also try by women's employment rates? (even further selection argument?) - but is this becoming too endogenous at this point?
* UNemployment rates might be better, but still need to add those on. let's just see what this does for RN
********************************************************************************
tabstat married_women_emp_rate_wt_t structural_familism_t, by(year) // this one is better because not changing as much over time

xtile emp_gp = married_women_emp_rate_wt_t, nq(4)
tabstat married_women_emp_rate_wt_t, by(emp_gp) stats(min max mean)
tabstat structural_familism_t, by(emp_gp) stats(mean p50 min max)
pwcorr married_women_emp_rate_wt_t structural_familism_t
tab emp_gp division_bucket_hrs_gp_t1, row

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $controls_nofe if in_analytical_sample==1 & emp_gp==1, or cluster(couple_id) // I think I need to take out state fixed effects now in case states don't change as much over time with this measure? (I feel like there is less variation. some models weren't estimating and I think maybe that is why
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $controls_nofe if in_analytical_sample==1 & emp_gp==2, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $controls_nofe if in_analytical_sample==1 & emp_gp==3, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $controls_nofe if in_analytical_sample==1 & emp_gp==4, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))


********************************************************************************
********************************************************************************
********************************************************************************
**# State fixed effects robustness (R&R Take 2)
********************************************************************************
********************************************************************************
********************************************************************************
// state fixed effects should be taking care of what they think is happening. BUT want to show that that also doesn't matter (might need to consider showing that with and without macro-controls as well because those probably help with state fixed effects)

* Paid labor only
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

// no fixed effects
local cont "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.educ_type i.moved_last2 i.couple_joint_religion i.num_children" 

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 `cont' $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_R&R2.xls", ctitle(Paid: No FE) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// no fixed effects AND no macro?! think don't need these bc have established I need macro controls
local cont "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.educ_type i.moved_last2 i.couple_joint_religion i.num_children" 

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 `cont' if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

* Combined
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

// no fixed effects
local cont "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.educ_type i.moved_last2 i.couple_joint_religion i.num_children" 

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 `cont' $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_R&R2.xls", ctitle(Combined: No FE) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// no fixed effects AND no macro?! think don't need these bc have established I need macro controls
local cont "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.educ_type i.moved_last2 i.couple_joint_religion i.num_children" 

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 `cont' if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

********************************************************************************
* What if I also add time fixed effects
* Okay none of these results change
********************************************************************************
// I need to bin years for this to work because some are getting omitted
gen year_gp=. 
replace year_gp=1 if year<2000
replace year_gp=2 if year >=2000 & year < 2005
replace year_gp=3 if year >=2005 & year < 2010
replace year_gp=4 if year >=2010 & year < 2015
replace year_gp=5 if year >=2015 & year < 2020

label define year_gp 1 "Pre-2000s" 2 "2000-2005" 3 "2005-2010" 4 "2010-2015" 5 "2010-2020"
label values year_gp year_gp
tab year_gp

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls i.year_gp if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls i.year_gp if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))


********************************************************************************
* OR birth OR marital cohort fixed effects
* Results still the same
********************************************************************************
tab rel_start_yr_couple

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls i.rel_start_yr_couple if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls i.rel_start_yr_couple if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

gen bcohort_head=.
replace bcohort_head=1 if yr_born_head <1970
replace bcohort_head=2 if yr_born_head >=1970 & yr_born_head < 1975
replace bcohort_head=3 if yr_born_head >=1975 & yr_born_head < 1980
replace bcohort_head=4 if yr_born_head >=1980 & yr_born_head < 1985
replace bcohort_head=5 if yr_born_head >=1985 & yr_born_head < 1990
replace bcohort_head=6 if yr_born_head >=1990 & yr_born_head < 2000

label define bcohort 1 "Pre-1970s" 2 "1970-75" 3 "1975-80" 4 "1980-85" 5 "1985-90" 6 "1990s+"
label values bcohort_head bcohort

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls i.bcohort_head if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls i.bcohort_head if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

********************************************************************************
********************************************************************************
********************************************************************************
**# Should I ALSO explore between-within?! (R&R Take 2)
* Inspired by reading of the Landivar /  Ruppanner articles
********************************************************************************
********************************************************************************
********************************************************************************
// a small worry here is that these models are extremely complicated already. is this adding too much additional complication?
// I got sidetracked. NO RANDOM EFFECTS KIM CAN STILL DO BETWEEN WITHIN MODELS

// this is causing problems so let's just temp remove non-analytical sample
preserve
keep if in_analytical_sample==1

// xtset state_fips year
sort couple_id marr_dur
browse couple_id unique_id partner_unique_id marr_dur
// xtset couple_id marr_dur // think marriage transition causing problems. should I just remove if not in analytical sample?

bysort state_fips: egen structural_familism_mean = mean(structural_familism_t)
gen structural_familism_within = structural_familism_t - structural_familism_mean
sum structural_familism_mean

browse state_fips year structural_familism_t structural_familism_mean structural_familism_within

gen pure_male_bw_rate_t = married_pure_male_bw_rate_t // shorten name but don't want to rename because I don't want to mess up models above
gen married_women_emp_rate_t = married_women_emp_rate_wt_t

foreach var of varlist women_college_rate_wt_t married_women_emp_rate_t avg_egal_reg_t pure_male_bw_rate_t{
    bysort state_fips: egen `var'_mean = mean(`var')
    gen `var'_within = `var' - `var'_mean
}

// Want to eventually try to standardize effects - use 1 SD
// sum structural_familism_within structural_familism_mean structural_familism_t
sum structural_familism_within
global sd_within = `r(sd)'
global sd2_within = `r(sd)' * 2
global mean_within = `r(mean)'

sum structural_familism_mean  
global sd_between = `r(sd)'
global sd2_between = `r(sd)' * 2
global mean_between = `r(mean)'

// do I want to create same comparison for the other models?
sum structural_familism_t
global sd_main = `r(sd)'
global sd2_main = `r(sd)' * 2
global mean_main = `r(mean)'

****************
** Paid DoL
****************
gen hh_hours_binary=0 if hh_hours_type_t1==1
replace hh_hours_binary = 1 if inlist(hh_hours_type_t1,2,3) // this was just problem with RE, i don't need this now.

/*
// should I first compare this model without between / within? I think I tried this originally (yes, see the validation.do file - has all of those models, though I did just use state NOT couple level to estimate). I think this is too much for stata to handle though...
melogit dissolve i.marr_dur i.hh_hours_type_t1##c.structural_familism_t ///
$controls_nofe $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, ///
|| state_fips: , vce(cluster state_fips) or // || couple_id: couple_id: vce(cluster couple_id)

/*
so the coefficients are all literally the same. what is different are the standard errors and therefore p-values. I actually think they are more significant with multilevel models? (perhaps because better accounts for clustering?)
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
*/

margins, dydx(hh_hours_type_t1) // right this takes awhile, I forget.
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

// let's start with JUST partitioning main indicator not yet other macro to see what happens
melogit dissolve i.marr_dur i.hh_hours_type_t1##c.structural_familism_within i.hh_hours_type_t1##c.structural_familism_mean ///
$controls_nofe $macro_controls ///
if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, ///
|| state_fips: , vce(cluster state_fips) or // || couple_id: vce(cluster couple_id)

estat ic
estat sd

margins, dydx(hh_hours_type_t1)

sum structural_familism_within, detail
// sum structural_familism_mean, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_within=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)') structural_familism_mean=(1))

sum structural_familism_mean, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_mean=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)') structural_familism_within=(0))
margins, dydx(hh_hours_type_t1) at(structural_familism_mean=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) asbalanced emptycells(reweight) 
	
	
// then this also partitions those other indicators.
melogit dissolve i.marr_dur i.hh_hours_type_t1##c.structural_familism_within i.hh_hours_type_t1##c.structural_familism_mean ///
$controls_nofe women_college_rate_wt_t_within women_college_rate_wt_t_mean married_women_emp_rate_t_within married_women_emp_rate_t_mean ///
avg_egal_reg_t_within avg_egal_reg_t_mean pure_male_bw_rate_t_within pure_male_bw_rate_t_mean ///
if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, ///
|| state_fips: || couple_id:, vce(cluster state_fips) or
*/

// okay actually, i don't need to use melogit at all i am dumb
logit dissolve i.marr_dur i.hh_hours_type_t1##c.structural_familism_within i.hh_hours_type_t1##c.structural_familism_mean ///
$controls_nofe $macro_controls ///
if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, ///
vce(cluster state_fips) or 

sum structural_familism_within, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_within=(`r(p1)' `r(p25)' `r(p50)' `r(p75)' `r(p99)') structural_familism_mean=($mean_between)) // do I have to extend scale because different now (not actual but deviation frm mean)?

sum structural_familism_mean, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_mean=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)') structural_familism_within=($mean_within))

	* Within effect for 1 SD change - BUT the problem is this is on log odds scale not predicted prob
	display "Within effect (1 SD): " _b[2.hh_hours_type_t1#c.structural_familism_within] * $sd_within

	* Between effect for 1 SD difference
	display "Between effect (1 SD): " _b[2.hh_hours_type_t1#c.structural_familism_mean] * $sd_between
	
	* See if effects the same or not
	test (2.hh_hours_type_t1#c.structural_familism_within = 2.hh_hours_type_t1#c.structural_familism_mean) ///
     (3.hh_hours_type_t1#c.structural_familism_within = 3.hh_hours_type_t1#c.structural_familism_mean)

	test (2.hh_hours_type_t1#c.structural_familism_within = 2.hh_hours_type_t1#c.structural_familism_mean) // , coef
	test (3.hh_hours_type_t1#c.structural_familism_within = 3.hh_hours_type_t1#c.structural_familism_mean) // , coef
	lincom (2.hh_hours_type_t1#c.structural_familism_within - 2.hh_hours_type_t1#c.structural_familism_mean)
	
	* Substantive interpretation - but on predicted probability scale
	
	margins, dydx(hh_hours_type_t1) at(structural_familism_within=(`=$mean_within-$sd2_within' `=$mean_within-$sd_within' $mean_within `=$mean_within+$sd_within' `=$mean_within+$sd2_within') structural_familism_mean=($mean_between))

	matrix w = r(table)
	scalar w_low = w[1,6] * 100
	scalar w_high = w[1,10] * 100
	scalar within_mod = w_low - w_high
	display within_mod
	
	margins, dydx(hh_hours_type_t1) at(structural_familism_mean=(`=$mean_between-$sd2_between' `=$mean_between-$sd_between' $mean_between `=$mean_between+$sd_between' `=$mean_between+$sd2_between') structural_familism_within=($mean_within)) 
	matrix b = r(table)
	scalar b_low = b[1,6] * 100
	scalar b_high = b[1,10] * 100
	scalar between_mod = b_low - b_high
	display between_mod

	display within_mod w_low w_high between_mod b_low b_high

	
// okay this doesn't really matter [if i also split the other macro variables]
logit dissolve i.marr_dur i.hh_hours_type_t1##c.structural_familism_within i.hh_hours_type_t1##c.structural_familism_mean ///
$controls_nofe women_college_rate_wt_t_within women_college_rate_wt_t_mean married_women_emp_rate_t_within married_women_emp_rate_t_mean ///
avg_egal_reg_t_within avg_egal_reg_t_mean pure_male_bw_rate_t_within pure_male_bw_rate_t_mean ///
if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, ///
vce(cluster state_fips) or

margins, dydx(hh_hours_type_t1) at(structural_familism_within=(`=$mean_within-$sd_within' $mean_within `=$mean_within+$sd_within') structural_familism_mean=($mean_between))
margins, dydx(hh_hours_type_t1) at(structural_familism_mean=(`=$mean_between-$sd_between' $mean_between `=$mean_between+$sd_between') structural_familism_within=($mean_within)) 
	
sum structural_familism_t, det
sum structural_familism_within, detail
sum structural_familism_mean, detail

// can I use THIS to also quantify between / within variation. following: https://www.reddit.com/r/stata/comments/1eqaq7d/testing_variance_withinbetween/
xtset state_fips // (can't use time with state, though, because I have individual-level data - is this problem?)
xtsum structural_familism_t

// I think this is actually just above
sum structural_familism_t structural_familism_mean structural_familism_within

****************
**Combined DoL
****************
/*
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)

melogit dissolve i.marr_dur i.division_bucket_hrs_t1##c.structural_familism_t ///
$controls_nofe $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, ///
|| state_fips: , vce(cluster state_fips) or 
margins, dydx(division_bucket_hrs_t1)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
*/

logit dissolve i.marr_dur i.division_bucket_hrs_t1##c.structural_familism_within i.division_bucket_hrs_t1##c.structural_familism_mean ///
$controls_nofe $macro_controls ///
if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, ///
vce(cluster state_fips) or 

sum structural_familism_within, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_within=(`r(p1)' `r(p25)' `r(p50)' `r(p75)' `r(p99)') structural_familism_mean=(1)) // do I have to extend scale because different now (not actual but deviation frm mean)?

sum structural_familism_mean, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_mean=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)') structural_familism_within=(0))

	* Within effect for 1 SD change - BUT the problem is this is on log odds scale not predicted prob
	display "Within effect (1 SD): " _b[2.division_bucket_hrs_t1#c.structural_familism_within] * $sd_within
	margins, dydx(division_bucket_hrs_t1) at(structural_familism_within=(`=$mean_within-$sd_within' $mean_within `=$mean_within+$sd_within') structural_familism_mean=($mean_between)) 

	* Between effect for 1 SD difference
	display "Between effect (1 SD): " _b[2.division_bucket_hrs_t1#c.structural_familism_mean] * $sd_between
	margins, dydx(division_bucket_hrs_t1) at(structural_familism_mean=(`=$mean_between-$sd_between' $mean_between `=$mean_between+$sd_between') structural_familism_within=($mean_within)) 
	
	*Test
	lincom (2.division_bucket_hrs_t1#c.structural_familism_within - 2.division_bucket_hrs_t1#c.structural_familism_mean)
	
	*Substantive interpretation of test
	logit dissolve i.marr_dur i.division_bucket_hrs_t1##c.structural_familism_within i.division_bucket_hrs_t1##c.structural_familism_mean ///
	$controls_nofe $macro_controls ///
	if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, ///
	vce(cluster state_fips) or 

	margins, dydx(division_bucket_hrs_t1) at(structural_familism_within=(`=$mean_within-$sd2_within' `=$mean_within-$sd_within' $mean_within `=$mean_within+$sd_within' `=$mean_within+$sd2_within') structural_familism_mean=($mean_between))

	matrix w = r(table)
	scalar w_low = w[1,6] * 100
	scalar w_high = w[1,10] * 100
	scalar within_mod = w_low - w_high
	display within_mod
	
	margins, dydx(division_bucket_hrs_t1) at(structural_familism_mean=(`=$mean_between-$sd2_between' `=$mean_between-$sd_between' $mean_between `=$mean_between+$sd_between' `=$mean_between+$sd2_between') structural_familism_within=($mean_within)) 
	matrix b = r(table)
	scalar b_low = b[1,6] * 100
	scalar b_high = b[1,10] * 100
	scalar between_mod = b_low - b_high
	display between_mod

	display within_mod w_low w_high between_mod b_low b_high
	
****************	
** Can i get similar output for other comparisons?
****************

// State FE (main)
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`=$mean_main-$sd2_main' `=$mean_main-$sd_main' $mean_main `=$mean_main+$sd_main' `=$mean_main+$sd2_main'))

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`=$mean_main-$sd2_main' `=$mean_main-$sd_main' $mean_main `=$mean_main+$sd_main' `=$mean_main+$sd2_main'))

// No FE
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls_nofe $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`=$mean_main-$sd2_main' `=$mean_main-$sd_main' $mean_main `=$mean_main+$sd_main' `=$mean_main+$sd2_main'))

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls_nofe $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`=$mean_main-$sd2_main' `=$mean_main-$sd_main' $mean_main `=$mean_main+$sd_main' `=$mean_main+$sd2_main'))
	
****************
* Attempting to get FE and Within on same scale
****************
logit dissolve i.marr_dur i.hh_hours_type_t1##c.structural_familism_within i.hh_hours_type_t1##c.structural_familism_mean ///
$controls_nofe $macro_controls ///
if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, ///
vce(cluster state_fips) or 

// First, get main percentiles
sum structural_familism_t, detail
local p5_total = `r(p5)'
local p25_total = `r(p25)'
local p50_total = `r(p50)'
local p75_total = `r(p75)'
local p95_total = `r(p95)'

// Get on within scale
local w_at_p5 = `p5_total' - $mean_between
local w_at_p25 = `p25_total' - $mean_between
local w_at_p50 = `p50_total' - $mean_between
local w_at_p75 = `p75_total' - $mean_between
local w_at_p95 = `p95_total' - $mean_between

// Validate
display `p5_total' " " `p25_total' " " `p50_total' " " `p75_total' " " `p95_total'
display `w_at_p5' " " `w_at_p25' " " `w_at_p50' " " `w_at_p75' " " `w_at_p95' " " $mean_between

// Margins
margins, dydx(hh_hours_type_t1) at(structural_familism_within=(`w_at_p5' `w_at_p25' `w_at_p50' `w_at_p75' `w_at_p95') structural_familism_mean=($mean_between))

logit dissolve i.marr_dur i.division_bucket_hrs_t1##c.structural_familism_within i.division_bucket_hrs_t1##c.structural_familism_mean ///
$controls_nofe $macro_controls ///
if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, ///
vce(cluster state_fips) or 

// First, get main percentiles
sum structural_familism_t, detail
local p5_total = `r(p5)'
local p25_total = `r(p25)'
local p50_total = `r(p50)'
local p75_total = `r(p75)'
local p95_total = `r(p95)'

// Get on within scale
local w_at_p5 = `p5_total' - $mean_between
local w_at_p25 = `p25_total' - $mean_between
local w_at_p50 = `p50_total' - $mean_between
local w_at_p75 = `p75_total' - $mean_between
local w_at_p95 = `p95_total' - $mean_between

// Validate
display `p5_total' " " `p25_total' " " `p50_total' " " `p75_total' " " `p95_total'
display `w_at_p5' " " `w_at_p25' " " `w_at_p50' " " `w_at_p75' " " `w_at_p95' " " $mean_between

// Margins
margins, dydx(division_bucket_hrs_t1) at(structural_familism_within=(`w_at_p5' `w_at_p25' `w_at_p50' `w_at_p75' `w_at_p95') structural_familism_mean=($mean_between))
margins, dydx(division_bucket_hrs_t1) at(structural_familism_within=(`w_at_p5' `w_at_p25' `w_at_p50' `w_at_p75' `w_at_p95') structural_familism_mean=(0))	

restore

********************************************************************************
********************************************************************************
********************************************************************************
**# Moderation of Religion (R&R Take 2)
********************************************************************************
********************************************************************************
********************************************************************************

// Okay, I ended up adding these to main models, so this is actually not that helpful here because evangelical is now a control - so these models won't export properly

// I pulled this in - have many religion indicators (need to revisit my notes but I def based them off of prior research). Let's do prob most expansive?
// also - is it sufficient to show this as moderation or do I need to add a control? This is where correlation will help; I believe correlated with norms but let's check
/*
evang_rate_t // just evang
evang_lds_rate_t // just evang and lds - this is only one pulled in originally, should I pull all three in? let's see how correlated. once catholic added, not actually correlated. but when they are talking about the BIBLE BELT. i don't think they are talking about catholicism. that's evangelical protestantism...so actually, should I use that? The problem with Catholicism is it is prob also in NE. I guess I could do all three? or at least evang plus LDS because I think evang is concern (and relates to G&L)
relig_rate_t // also includes catholic

pwcorr evang_rate_t evang_lds_rate_t relig_rate_t
pwcorr evang_rate_t avg_egal_reg_t // this is actually not as correlated as I thought?
pwcorr evang_rate_t avg_egal_reg_t women_college_rate_wt_t married_women_emp_rate_wt_t married_pure_male_bw_rate_t // they are all negative, though
*/

// correlations
pwcorr evang_rate_t evang_lds_rate_t relig_rate_t
pwcorr evang_rate_t structural_familism_t avg_egal_reg_t women_college_rate_wt_t married_women_emp_rate_wt_t married_pure_male_bw_rate_t
pwcorr evang_lds_rate_t structural_familism_t avg_egal_reg_t women_college_rate_wt_t married_women_emp_rate_wt_t married_pure_male_bw_rate_t
pwcorr relig_rate_t structural_familism_t avg_egal_reg_t women_college_rate_wt_t married_women_emp_rate_wt_t married_pure_male_bw_rate_t

*******************
* JUST Evangelical
*******************
// I think there are two questions. are they robust to religion? yes. 
// is religion driving? I do worry there are like 1000 macro level controls + state fixed effects that I am not sure what the effect of religion is and how to capture. I think this is why I like the idea of trying the within-region robustness. I mean I guess this is true of all macro-level variables. but i am not really trying to quantify those.

* Let's remind ourselves of main models
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

* Paid work only
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.evang_rate_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_R&R2.xls", ctitle(Paid: Evang) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.evang_rate_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum evang_rate_t, detail
margins, dydx(hh_hours_type_t1) at(evang_rate_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_R&R2.xls", ctitle(Paid: Evang MOD) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Combined DoL
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.evang_rate_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_R&R2.xls", ctitle(Combined: Evang) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.evang_rate_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum evang_rate_t, detail
margins, dydx(division_bucket_hrs_t1) at(evang_rate_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_R&R2.xls", ctitle(Combined: Evang MOD) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// what if we just add religion as control with other macro level variables?
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls evang_rate_t if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_R&R2.xls", ctitle(Paid: Evang just Control) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls evang_rate_t if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_R&R2.xls", ctitle(Combined: Evang just Control) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append


*******************
* Now evang PLUS LDS (but this is different to the bible belt really)
*******************
* Paid work only
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.evang_lds_rate_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_R&R2.xls", ctitle(Paid: EvangLDS) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.evang_lds_rate_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum evang_lds_rate_t, detail
margins, dydx(hh_hours_type_t1) at(evang_lds_rate_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_R&R2.xls", ctitle(Paid: EvangLDS MOD) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Combined DoL
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.evang_lds_rate_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_R&R2.xls", ctitle(Combined: EvangLDS) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.evang_lds_rate_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum evang_lds_rate_t, detail
margins, dydx(division_bucket_hrs_t1) at(evang_lds_rate_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_R&R2.xls", ctitle(Combined: EvangLDS MOD) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// what if we just add religion as control with other macro level variables?
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls evang_lds_rate_t if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_R&R2.xls", ctitle(Paid: EvangLDS just Control) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls evang_lds_rate_t if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_R&R2.xls", ctitle(Combined: EvangLDS just Control) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

********************************************************************************
********************************************************************************
********************************************************************************
**# Earnings or Employment to get at DEPENDENCE question?? (R&R Take 2)
********************************************************************************
********************************************************************************
********************************************************************************

// I already exported some of these but think I might make this a whole new export just for this to better organize these thoughts and figure out what / how to present
// I know I am now throwing a lot at a wall and these indicators can all mean so many things and hard to directly compare most.
// I also moved some of these to the wald file to make it easier to compare between measures

tab hh_earn_type_t1 if in_analytical_sample==1
tab hh_hours_type_t1 if in_analytical_sample==1

tabstat female_earn_pct_t1 female_hours_pct_t1 if in_analytical_sample==1, stats(mean p50)

** Here is the earnings version of hours measure (I think I actually used this originally and moved away to emphasize conflict argument with parenthood)
// have to use the non-bucketed earnings (the continuous version) to get estimates
local cont "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner c.earnings_1000s i.educ_type i.moved_last2 i.couple_joint_religion i.num_children"  // i.region knot1 knot2 knot3 

// so direction is same but not sig. is this enough to say it's not dependence? prob not given the other two indicators below.
logit dissolve i.marr_dur c.structural_familism_t i.hh_earn_type_t1 `cont' $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(hh_earn_type_t1)

logit dissolve i.marr_dur c.structural_familism_t i.hh_earn_type_t1 c.structural_familism_t#i.hh_earn_type_t1 `cont' $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_earn_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_R&R2.xls", ctitle(earnings bucket) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

** Here is her share of earnings - so no main effect the way there is with others BUT same moderation (her share = bad when support is low. wait is this norms? no this can still be economic independence - because she can only leave if she has earnings. In my head i was like would it be negative but no - her LOW SHARE reduces divorce, so we don't know if again dependence or norms violation)
logit dissolve i.marr_dur c.structural_familism_t c.female_earn_pct_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(female_earn_pct_t1)

logit dissolve i.marr_dur c.structural_familism_t c.female_earn_pct_t1 c.structural_familism_t#c.female_earn_pct_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(female_earn_pct_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))  post
outreg2 using "$results/main results/dissolution_AMES_R&R2.xls", ctitle(earnings share) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

** Do I need to complement this with her HOURS share??
logit dissolve i.marr_dur c.structural_familism_t c.female_hours_pct_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(female_hours_pct_t1)

logit dissolve i.marr_dur c.structural_familism_t c.female_hours_pct_t1 c.structural_familism_t#c.female_hours_pct_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(female_hours_pct_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))  post
outreg2 using "$results/main results/dissolution_AMES_R&R2.xls", ctitle(hours share) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// test diffs between her earnings and hours share [See WALD document for now; trying to not make this so long and figure this out elsewhere, need to better organize]

*******************************
* Employment status
*******************************

** Here is her FT employment status - okay yeah and works the exact same way. her FT employment increases risk of divorce when support is low - no effect when high. Is this interesting (and more intuitive?!)
tab ft_pt_t1_wife
tab ft_t1_wife if in_analytical_sample==1
tab ft_t1_head if in_analytical_sample==1

logit dissolve i.marr_dur c.structural_familism_t i.ft_t1_wife $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(ft_t1_wife)

logit dissolve i.marr_dur c.structural_familism_t i.ft_pt_t1_wife $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(ft_pt_t1_wife)

logit dissolve i.marr_dur c.structural_familism_t i.ft_t1_wife c.structural_familism_t#i.ft_t1_wife $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(ft_t1_wife) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))  post
outreg2 using "$results/main results/dissolution_AMES_R&R2.xls", ctitle(wife ft) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// ONLY true for FT employment, not PT - so again, this makes it hard to disentangle conflict from earnings dependence, tbh. i think if PT had effect, it could be dependence?
logit dissolve i.marr_dur c.structural_familism_t i.ft_pt_t1_wife c.structural_familism_t#i.ft_pt_t1_wife $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(ft_pt_t1_wife) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

// I guess doing husband is a good antidote - is it NORMS or conflict also [relates to counter-trad / trad also]. effect not SIG but same for husbands. this feels important (just husbands WAY more likely to be working FT hence why non-sig)
// because if norms, I'd expect his UNemployment to have an effect. I could compare these effect sizes to women's FT employment at least
tab ft_pt_t1_head if in_analytical_sample==1 // problem is: NO MEN not working. and just over 5% not FT, so I think it's just not enough variation?

logit dissolve i.marr_dur c.structural_familism_t i.ft_t1_head c.structural_familism_t#i.ft_t1_head $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(ft_t1_head) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_R&R2.xls", ctitle(husband ft) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// okay there isn't enough non-employment among husbands here
logit dissolve i.marr_dur c.structural_familism_t i.ft_pt_t1_head c.structural_familism_t#i.ft_pt_t1_head $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(ft_pt_t1_head) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

*******************************
* Absolute levels
*******************************

** What about her RAW earnings?
// should probably log?
gen earnings_wife_ln = ln(earnings_t1_wife+1)

logit dissolve i.marr_dur c.structural_familism_t c.earnings_wife_ln c.structural_familism_t#c.earnings_wife_ln $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(earnings_wife_ln) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

** I guess then raw work hours (paid AND housework) - if it's a conflict argument, do higher hours in general lead to divorce?? [I might need to interact HW hours with employment, though?] THis is where this gets tricky...Also, the effects are the same for like all of these operationalizations. It is also hard to compare effect sizes because weekly hours is SO DIFFERENT to annual earnings. I mean on one hand, I think this means my results are robust. On the other, not 100% sure how to interpret
logit dissolve i.marr_dur c.structural_familism_t c.weekly_hrs_t1_wife c.structural_familism_t#c.weekly_hrs_t1_wife $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(weekly_hrs_t1_wife) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
sum structural_familism_t, detail
margins, at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)') weekly_hrs_t1_wife=(0 20 40 60) )
marginsplot

// okay so housework probably has the SMALLEST moderation effect, but still supports this...really her housework just lowers divorce risk always.
logit dissolve i.marr_dur c.structural_familism_t c.housework_wife c.structural_familism_t#c.housework_wife $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(housework_wife) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
sum structural_familism_t, detail
margins, at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)') housework_wife=(0 5 10 20) )
marginsplot

// I don't want toooo many interaction, so what about just among FT employed? (in lieu of an interaction - just do separate)
tab ft_t1_wife if in_analytical_sample==1 // it is 50/50 so could so like this
tabstat housework_wife if in_analytical_sample==1, by(ft_t1_wife) stats(p25 p50 p75)

	// really her housework does not have an effect among the FT employed - and not really moderated. so does this suggest NOT about conflict?? (i think this is harder because I don't have childcare. I would feel more comfortable making this argument if I had childcare)
	logit dissolve i.marr_dur c.structural_familism_t c.housework_wife c.structural_familism_t#c.housework_wife $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & ft_t1_wife==1, or cluster(couple_id)
	sum structural_familism_t, detail
	margins, dydx(housework_wife) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

	// but it DOES have a negative effect among those not employed - also not moderated
	logit dissolve i.marr_dur c.structural_familism_t c.housework_wife c.structural_familism_t#c.housework_wife $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & ft_t1_wife==0, or cluster(couple_id)
	sum structural_familism_t, detail
	margins, dydx(housework_wife) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

// this is where I'm like - the second shift as a category makes this interesting. That doesn't have an effect when egal is reference, but I think it DOES when male BW is...I mean - the moderator effect is similar - it lowers risk of divorce when support is low (gender deviance neutralization?) but I guess I'd expect it to RAISE risk of divorce when support is low - because most conflict. so this is interesting like...is this evidence of NORMS v. anything else?? OR - is it really just about women's employment but then no because second shift and egal would be more similar. I mean  I guess it's not statistical significant...
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

// so okay not sig but is ABOVE male-BW. but not at same level as egal. is it because of gender deviance neutralization? or is it just that HW doesn't matter as much as her employment? (but again hard to say if norms or economic dependence - we're still in the first half of the gender revolution)
logit dissolve i.marr_dur c.structural_familism_t ib2.division_bucket_hrs_t1 c.structural_familism_t#ib2.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

*******************************
* Alt: SPLIT male-BW into yes / no her earnings
*******************************
gen wife_no_earnings = . 
replace wife_no_earnings = 0 if earnings_t1_wife > 0 & earnings_t1_wife!=.
replace wife_no_earnings = 1 if earnings_t1_wife == 0

tab hh_hours_type_t1 wife_no_earnings if in_analytical_sample, row

gen hh_hours_type_t1_alt = .
replace hh_hours_type_t1_alt = 1 if hh_hours_type_t1==1 // dual
replace hh_hours_type_t1_alt = 2 if hh_hours_type_t1==2 & wife_no_earnings == 1 // pure male BW
replace hh_hours_type_t1_alt = 3 if hh_hours_type_t1==2 & wife_no_earnings == 0 // neo trad
replace hh_hours_type_t1_alt = 4 if hh_hours_type_t1==3 // female BW 

label define hh_hours_type_t1_alt 1 "Dual-Earner" 2 "Pure Male BW" 3 "Neo-Trad" 4 "Female BW"
label values hh_hours_type_t1_alt hh_hours_type_t1_alt

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
margins, dydx(hh_hours_type_t1)  

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1_alt c.structural_familism_t#i.hh_hours_type_t1_alt $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1_alt) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) 
margins, dydx(hh_hours_type_t1)
// what's SUPER interesting here is that the MAIN EFFECT is ONLY for neo-trad BUT the moderation effect is nearly identical. need ot think about what this means but this feels helpful.

********************************************************************************
* Then need to do some mediation
********************************************************************************
** Main models include total couple earnings. What if HER EARNINGS SPECIFICALLY moderate the effects of male-BW
// okay this is actually very interesting. I think what is cool - but need to quantify better than as is here: is the main effects attenuate but I think the moderation effect actually doesn't as much (though some) - so I think it is some economic dependence for sure - and maybe the MODERATION could help. but not totally?

// how to formally test? I think Wald doesn't work when IVs differ??? do I have to use suest? i go through this EVERY TIME i do things liek this, and then I end up doing by hand LOL (can I revisit Socius paper for how to do by hand? I have an excel, just need to figure out the inputs but might just come from margins).


*******************************
* Just Paid Labor
*******************************
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
matrix m1 = r(table)

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls earnings_t1_wife if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
matrix m2 = r(table)

matrix list m1
matrix list m2

forvalues i = 6/10 { // have to start at 6 because 1-5 are the ref category
	
    // NO earnings
	local ame1 = m1[1, `i'] // ame is in row 1, then each percentile is a column
    local se1 = m1[2, `i'] // se is in row 2
    
	// WITH earnings
	local ame2 = m2[1, `i']
    local se2 = m2[2, `i']
    
    local diff = (`ame2' - `ame1')
	local wald = (`ame1' - `ame2')^2 / (`se1'^2 + `se2'^2)
    local pval = chi2tail(1, `wald')
    
    di "Level `i': Diff = " %6.3f `diff' " Wald = " %6.3f `wald' ", p = " %5.3f `pval'
}

// main effect
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(hh_hours_type_t1) post

matrix m3 = r(table)

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls earnings_t1_wife if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(hh_hours_type_t1) post

matrix m4 = r(table)

local ame3 = m3[1, 2]
local se3 = m3[2, 2]
local ame4 = m4[1, 2]
local se4 = m4[2, 2]

local diff = (`ame4' - `ame3')
local wald = (`ame3' - `ame4')^2 / (`se3'^2 + `se4'^2)
local pval = chi2tail(1, `wald')

di "Main effect comparison: Diff = " %6.3f `diff' " Wald = " %6.3f `wald' ", p = " %5.3f `pval'

///

*******************************
*Combined DoL
*******************************
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
matrix m1 = r(table)

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls earnings_t1_wife if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
matrix m2 = r(table)

matrix list m1
matrix list m2

forvalues i = 6/10 { // have to start at 6 because 1-5 are the ref category
	
    // NO earnings
	local ame1 = m1[1, `i'] // ame is in row 1, then each percentile is a column
    local se1 = m1[2, `i'] // se is in row 2
    
	// WITH earnings
	local ame2 = m2[1, `i']
    local se2 = m2[2, `i']
    
    local diff = (`ame2' - `ame1')
	local wald = (`ame1' - `ame2')^2 / (`se1'^2 + `se2'^2)
    local pval = chi2tail(1, `wald')
    
    di "Level `i': Diff = " %6.3f `diff' " Wald = " %6.3f `wald' ", p = " %5.3f `pval'
}

// main effect // could it change the MAIN effect? so...it actually DOES reduce the main effect to non-sig. whether or not that is a SIG change remains to be seen. so would i need to test the main AND moderator effects? because it could change main effect but not moderation?
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1) post

matrix m3 = r(table)

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls earnings_t1_wife if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1) post

matrix m4 = r(table)

local ame3 = m3[1, 2]
local se3 = m3[2, 2]
local ame4 = m4[1, 2]
local se4 = m4[2, 2]

local diff = (`ame4' - `ame3')
local wald = (`ame3' - `ame4')^2 / (`se3'^2 + `se4'^2)
local pval = chi2tail(1, `wald')

di "Main effect comparison: Diff = " %6.3f `diff' " Wald = " %6.3f `wald' ", p = " %5.3f `pval'

///

*******************************
* Her Earnings Share (maybe also interesting a la K&S)
*******************************

logit dissolve i.marr_dur c.structural_familism_t c.female_earn_pct_t1 c.structural_familism_t#c.female_earn_pct_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(female_earn_pct_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
matrix m1 = r(table)

logit dissolve i.marr_dur c.structural_familism_t c.female_earn_pct_t1 c.structural_familism_t#c.female_earn_pct_t1 $controls $macro_controls earnings_t1_wife if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(female_earn_pct_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
matrix m2 = r(table)

matrix list m1
matrix list m2

forvalues i = 1/5 {
	
    // NO earnings
	local ame1 = m1[1, `i'] // ame is in row 1, then each percentile is a column
    local se1 = m1[2, `i'] // se is in row 2
    
	// WITH earnings
	local ame2 = m2[1, `i']
    local se2 = m2[2, `i']
    
    local diff = (`ame2' - `ame1')
	local wald = (`ame1' - `ame2')^2 / (`se1'^2 + `se2'^2)
    local pval = chi2tail(1, `wald')
    
    di "Level `i': Diff = " %6.3f `diff' " Wald = " %6.3f `wald' ", p = " %5.3f `pval'
}

// main effect
logit dissolve i.marr_dur c.structural_familism_t c.female_earn_pct_t1 c.structural_familism_t#c.female_earn_pct_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(female_earn_pct_t1) post

matrix m3 = r(table)

logit dissolve i.marr_dur c.structural_familism_t c.female_earn_pct_t1 c.structural_familism_t#c.female_earn_pct_t1 $controls $macro_controls earnings_t1_wife if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(female_earn_pct_t1) post

matrix m4 = r(table)

local ame3 = m3[1, 1]
local se3 = m3[2, 1]
local ame4 = m4[1, 1]
local se4 = m4[2, 1]

local diff = (`ame4' - `ame3')
local wald = (`ame3' - `ame4')^2 / (`se3'^2 + `se4'^2)
local pval = chi2tail(1, `wald')

di "Main effect comparison: Diff = " %6.3f `diff' " Wald = " %6.3f `wald' ", p = " %5.3f `pval'

********************************************************************************
* Okay do I actually need to INTERACT her earnings with policy AND the DoL??
* Because I show her earnings on their own matter - which alone could be 
* economic dependence. SO then do I need to show that it's not JUST about her earnings
* BUT also the DoL once earnings accounted for?
* I am losing the plot. I really want to attempt to follow GP & Gangl and they
* JUST include as mediator 1x (not interacted, just as is)
********************************************************************************
/* Let's show a progression
1. DoL Main effect (policy interaction)
1a. Earnings main effect (policy interaction)
2. DoL with earnings as control - not let earnings differ across policy
3. DoL with earnings x policy as control - let earnings differ across policy
*/

gen earnings_wife_1000s = earnings_t1_wife / 1000 // easier to interpret coefficients

log using "$logdir\earnings_mediation.log", replace

*******************************
* Just Paid Labor
*******************************
// M1

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix dol1 = r(table)

// M1 A
logit dissolve i.marr_dur c.structural_familism_t c.earnings_wife_1000s c.structural_familism_t#c.earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(earnings_wife_1000s) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix earn1 = r(table)

// M2
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix dol2 = r(table)

// M3
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.earnings_wife_1000s c.structural_familism_t#c.earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix dol3 = r(table)

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.earnings_wife_1000s c.structural_familism_t#c.earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(earnings_wife_1000s) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix earn3 = r(table)

matrix list dol1
matrix list dol2
matrix list dol3

matrix list earn1
matrix list earn3

// Compare M1 to M2
forvalues i = 6/10 {
	
    // NO earnings
	local ame1 = dol1[1, `i'] // ame is in row 1, then each percentile is a column
    local se1 = dol1[2, `i'] // se is in row 2
    local p1 = dol1[4, `i'] // p is in row 4
    
	// WITH earnings
	local ame2 = dol2[1, `i']
    local se2 = dol2[2, `i']
    local p2 = dol2[4, `i'] // p is in row 2
	
    local diff = (`ame2' - `ame1')
	local wald = (`ame1' - `ame2')^2 / (`se1'^2 + `se2'^2)
    local pval = chi2tail(1, `wald')
    
    di "Level `i': M1 = " %6.3f `ame1' " (" %6.3f `p1' ") M2 = " %6.3f `ame2' " (" %6.3f `p2' ") Diff = " %6.3f `diff' " Wald = " %6.3f `wald' ", p = " %5.3f `pval'
}


// Compare M1 to M3
forvalues i = 6/10 {
	
    // NO earnings
	local ame1 = dol1[1, `i'] // ame is in row 1, then each percentile is a column
    local se1 = dol1[2, `i'] // se is in row 2
    local p1 = dol1[4, `i'] // p is in row 4
    
	// WITH earnings
	local ame3 = dol3[1, `i']
    local se3 = dol3[2, `i']
    local p3 = dol3[4, `i']
	
    local diff = (`ame3' - `ame1')
	local wald = (`ame1' - `ame3')^2 / (`se1'^2 + `se3'^2)
    local pval = chi2tail(1, `wald')
    
    di "Level `i': M1 = " %6.3f `ame1' " (" %6.3f `p1' ") M2 = " %6.3f `ame3' " (" %6.3f `p3' ") Diff = " %6.3f `diff' " Wald = " %6.3f `wald' ", p = " %5.3f `pval'
}


// Earnings: Compare M1 to M3
forvalues i = 1/5 {
	
    // NO earnings
	local ame1 = earn1[1, `i'] // ame is in row 1, then each percentile is a column
    local se1 = earn1[2, `i'] // se is in row 2
    local p1 = earn1[4, `i'] // p is in row 4
    
	// WITH earnings
	local ame3 = earn3[1, `i']
    local se3 = earn3[2, `i']
    local p3 = earn3[4, `i']
	
    local diff = (`ame3' - `ame1')
	local wald = (`ame1' - `ame3')^2 / (`se1'^2 + `se3'^2)
    local pval = chi2tail(1, `wald')
    
    di "Level `i': M1 = " %6.4f `ame1' " (" %6.3f `p1' ") M2 = " %6.4f `ame3' " (" %6.3f `p3' ") Diff = " %6.3f `diff' " Wald = " %6.3f `wald' ", p = " %5.3f `pval'
}

*******************************
*Combined DoL
*******************************

// M1

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix dol1 = r(table)

// M1 A
logit dissolve i.marr_dur c.structural_familism_t c.earnings_wife_1000s c.structural_familism_t#c.earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(earnings_wife_1000s) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix earn1 = r(table)

// M2
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix dol2 = r(table)

// M3
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.earnings_wife_1000s c.structural_familism_t#c.earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix dol3 = r(table)

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.earnings_wife_1000s c.structural_familism_t#c.earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(earnings_wife_1000s) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix earn3 = r(table)

matrix list dol1
matrix list dol2
matrix list dol3

matrix list earn1
matrix list earn3

// Compare M1 to M2
forvalues i = 6/10 {
	
    // NO earnings
	local ame1 = dol1[1, `i'] // ame is in row 1, then each percentile is a column
    local se1 = dol1[2, `i'] // se is in row 2
    local p1 = dol1[4, `i'] // p is in row 4
    
	// WITH earnings
	local ame2 = dol2[1, `i']
    local se2 = dol2[2, `i']
    local p2 = dol2[4, `i'] // p is in row 2
	
    local diff = (`ame2' - `ame1')
	local wald = (`ame1' - `ame2')^2 / (`se1'^2 + `se2'^2)
    local pval = chi2tail(1, `wald')
    
    di "Level `i': M1 = " %6.3f `ame1' " (" %6.3f `p1' ") M2 = " %6.3f `ame2' " (" %6.3f `p2' ") Diff = " %6.3f `diff' " Wald = " %6.3f `wald' ", p = " %5.3f `pval'
}


// Compare M1 to M3
forvalues i = 6/10 {
	
    // NO earnings
	local ame1 = dol1[1, `i'] // ame is in row 1, then each percentile is a column
    local se1 = dol1[2, `i'] // se is in row 2
    local p1 = dol1[4, `i'] // p is in row 4
    
	// WITH earnings
	local ame3 = dol3[1, `i']
    local se3 = dol3[2, `i']
    local p3 = dol3[4, `i']
	
    local diff = (`ame3' - `ame1')
	local wald = (`ame1' - `ame3')^2 / (`se1'^2 + `se3'^2)
    local pval = chi2tail(1, `wald')
    
    di "Level `i': M1 = " %6.3f `ame1' " (" %6.3f `p1' ") M2 = " %6.3f `ame3' " (" %6.3f `p3' ") Diff = " %6.3f `diff' " Wald = " %6.3f `wald' ", p = " %5.3f `pval'
}


// Earnings: Compare M1 to M3
forvalues i = 1/5 {
	
    // NO earnings
	local ame1 = earn1[1, `i'] // ame is in row 1, then each percentile is a column
    local se1 = earn1[2, `i'] // se is in row 2
    local p1 = earn1[4, `i'] // p is in row 4
    
	// WITH earnings
	local ame3 = earn3[1, `i']
    local se3 = earn3[2, `i']
    local p3 = earn3[4, `i']
	
    local diff = (`ame3' - `ame1')
	local wald = (`ame1' - `ame3')^2 / (`se1'^2 + `se3'^2)
    local pval = chi2tail(1, `wald')
    
    di "Level `i': M1 = " %6.4f `ame1' " (" %6.3f `p1' ") M2 = " %6.4f `ame3' " (" %6.3f `p3' ") Diff = " %6.3f `diff' " Wald = " %6.3f `wald' ", p = " %5.3f `pval'
}

log close

********************************************************************************
** Then moderator: are the effects of male-BW conditional on her total earnings??
********************************************************************************
// going to try to use tertiles or something to avoid interacting
xtile earnings_tertile = earnings_t1_wife, nq(3)
tab earnings_tertile
tabstat(earnings_t1_wife), by(earnings_tertile) stats(min max mean p50)

tab hh_hours_type_t1 earnings_tertile, col
tab division_bucket_hrs_t1 earnings_tertile, col

** Paid
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(hh_hours_type_t1)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) 

// low
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & earnings_tertile==1, or cluster(couple_id)
margins, dydx(hh_hours_type_t1)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) 

// med
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & earnings_tertile==2, or cluster(couple_id)
margins, dydx(hh_hours_type_t1)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) 

// high
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & earnings_tertile==3, or cluster(couple_id)
margins, dydx(hh_hours_type_t1)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) 

** Now i am confusing myself. Do I actually restrict to MALE BW and see if her earnings matter differently?? I am losing the plot. I think I am struggling because restricting to high earnings means more women are sorting into dual-earning. but they can afford to leave any arrnagement but if I think male-BW is problematic they can afford to leave male BW MORE when have high earnings than low - so perhaps reversal only comes among high earning women?


********************************************************************************
********************************************************************************
********************************************************************************
**# What about work-family conflict?
********************************************************************************
********************************************************************************
********************************************************************************
// one mechanism is that dual-earning creates more conflict because more total work hours. Does THIS mediate?
tabstat total_work_wife total_work_head total_work_couple, by(hh_hours_type_t1) // validate more total hours 
tabstat total_work_wife total_work_head total_work_couple, by(division_bucket_hrs_t1)

**********************
* Paid
**********************
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

// add total work couple
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls c.total_work_couple if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(total_work_couple) // no main effect
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) // results get STRONGER once I add

// interact total work couple
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls c.total_work_couple c.total_work_couple#c.structural_familism_t if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) // but interacting total work doens't change anything further

sum structural_familism_t, detail
margins, dydx(total_work_couple) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) // nor is there a moderation effect here

// add total work wife - because my mechanism is really SHE drops out of the labor market - so is it about HER total burden?
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls c.total_work_wife if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(total_work_wife) // interestingly - main effect is negative. what's interesting is that the effect of her PAID WORK hours is positive but the effect of her housework is negative (from the other models). is this just capturing the HW effects?
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) // results get EVEN STRONGER when I add total work of wife?

/*
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls c.weekly_hrs_t1_wife c.housework_wife if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(weekly_hrs_t1_wife) // though NOT when in same model
margins, dydx(housework_wife)
*/

// interact total work wife
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls c.total_work_wife c.total_work_wife#c.structural_familism_t if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) // but no real further change from interaction (bc no interaction effect)

sum structural_familism_t, detail
margins, dydx(total_work_wife) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) // the effect of her total hours is not MODERATED by the variable, though (I'd expect her total hours to be more problematic at lower levels of the scale? so positive at lower levels - but it is not)

// is it JUST about her work hours (again if the mechanism is her LEAVING the labor market?)
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls c.weekly_hrs_t1_wife if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(weekly_hrs_t1_wife)  // so positive effect
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) // okay INTERESTINGLY the moderation effect is still there but now in an opposite direction - once accounted for, male BW barely has effect at low levels but then becomes MORE POSITIVE at high levels?
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)') weekly_hrs_t1_wife=(0)) // sort of true at all values of her hours?
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)') weekly_hrs_t1_wife=(40))

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls c.weekly_hrs_t1_wife c.weekly_hrs_t1_wife#c.structural_familism_t if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(weekly_hrs_t1_wife) 
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

sum structural_familism_t, detail
margins, dydx(weekly_hrs_t1_wife) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) //but the moderation effect is in opposite way I'd expect - her hours get MORE positive for divorce when support is high. which should be the opposite?

// should I add separately since the direction of the effects is opposite? See my thoughts on this below. This is really losing the plot on interpretation.
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls c.weekly_hrs_t1_wife c.housework_wife if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(housework_wife) // negative
margins, dydx(weekly_hrs_t1_wife) // positive but not sig
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) // so yeah results become very odd - same moderation but moves the effect...

**********************
* Combined
**********************
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

// add total work couple
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls c.total_work_couple if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(total_work_couple) // no main effect
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) // also get stronger here

// interact total work couple
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls c.total_work_couple c.total_work_couple#c.structural_familism_t if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

sum structural_familism_t, detail
margins, dydx(total_work_couple) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

// add total work wife (is it HER burden specifically)
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls c.total_work_wife if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(total_work_wife) // no main effect here
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) // also get stronger here

// interact total work wife
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls c.total_work_wife c.total_work_wife#c.structural_familism_t if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

sum structural_familism_t, detail
margins, dydx(total_work_wife) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) // gets MORE negative as support increases (i guess I'd expect it to get less positive so this is, in some ways, that...)

// add paid work hours wife instead (is it HER EMPLOYMENT burden specifically)
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls c.weekly_hrs_t1_wife if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(weekly_hrs_t1_wife) // no main effect but positive
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) // results attenuate, honestly probably more than earnings

// should I add separately since the direction of the effects is opposite? okay but the problem is - these variables are inputs into my DoL measure. so what is left when I control for these. I think I am now not capturing what i think I am capturing with DoL and that is the problem and why te results change so much. it is like I am now controlling the causal pathway (not that I am trying to identify hte causal pathway BUT) if these are the component variables and I control for them - what is left? and what does that tell me? I guess if anything, this is quantifying what part of the DoL operates via her time and what operates via something else? I guess taht "something else" is interesting (I mean some of it is men's time but his time generally doesn't have an effect) but then maybe it's about the non-time part?? but then that is speculation. and this is becoming detached from WFC. So, i think this is useful to look at, but not related to my goal, so let's use her total work hours as FOIL to total couple annd then move on and say it's at least not about the total time burden...
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls c.weekly_hrs_t1_wife c.housework_wife if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(housework_wife) // negative
margins, dydx(weekly_hrs_t1_wife) // positive but not sig
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) // results really attenuate HERE

********************************************************************************
********************************************************************************
********************************************************************************
**# Opportunity costs (attempt to rule out other mechanisms)
********************************************************************************
********************************************************************************
********************************************************************************

// okay these results are confusing and vary DRAMATICALLY based on measure. I think this is getting into like complicated territory because this captures so many thinks (ESPECIALLY because I am restricting to parents) - the effect of education may well differ among parents in light of concerted cultivation and intensive parenting norms. that have nothing to do with economic status or independence. because the effect of MALE BW is the same across levels but once I turnt o gender specialization, it's not, and I am just wondering if this is now capturing something else. I think this is getting into like -- I am trying to test too much territory beyond the goals of my paper (which I already sort of knew LOL)

// Use just HER education (not couple-level because that's different question)
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & college_wife==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est1n

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & college_wife==1, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est1c

/// 

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & college_wife==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est2n

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & college_wife==1, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est2c

/// 

logit dissolve i.marr_dur c.structural_familism_t i.ft_t1_wife c.structural_familism_t#i.ft_t1_wife $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & college_wife==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(ft_t1_wife) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est3n

logit dissolve i.marr_dur c.structural_familism_t i.ft_t1_wife c.structural_familism_t#i.ft_t1_wife $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & college_wife==1, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(ft_t1_wife) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est3c

coefplot est1n est1c
coefplot est2n est2c
coefplot est3n est3c

// revisit gender wage gap individual results (and maybe minimum wage?) I guess all three LABOR market ones? but i think gender wage gap more support as concept from prior research (see GP and Gangl, Ruppanner)
**********
* Paid
**********
* Earnings Ratio
* I am getting confused - so male BW would INCREASE divorce MORE when wage gap is higher because she SHOULD be working and is NOT (I mean, this is generally the argument at higher levels of the scale) but it's actually the opposite? here the moderation effect is in the other direction? male BW lowers divorce when gender wage gap is HIGHER this doesn't make any sense...it's the ONE variable in the scale that works in opposite direction
pwcorr earn_ratio_t structural_familism_t  married_women_emp_rate_wt_t avg_egal_reg_t // so inversely correlated with employment which makes sense...

logit dissolve i.marr_dur c.earn_ratio_t i.hh_hours_type_t1 c.earn_ratio_t#i.hh_hours_type_t1 $controls $macro_controls structural_familism_t if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum earn_ratio_t, detail
margins, dydx(hh_hours_type_t1) at(earn_ratio_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

	// even weirder: her FT employment becomes MORE positively associated with divorce when gender wage gap higher. is THIS actually ECONOMIC INDEPENDENCE. is THIS the mechanism?
	logit dissolve i.marr_dur c.earn_ratio_t i.ft_t1_wife c.earn_ratio_t#i.ft_t1_wife $controls $macro_controls structural_familism_t if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
	sum earn_ratio_t, detail
	margins, dydx(ft_t1_wife) at(earn_ratio_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
	
	// does education matter HERE?!
	logit dissolve i.marr_dur c.earn_ratio_t i.hh_hours_type_t1 c.earn_ratio_t#i.hh_hours_type_t1 $controls $macro_controls structural_familism_t if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & college_wife==0, or cluster(couple_id)
	sum earn_ratio_t, detail
	margins, dydx(hh_hours_type_t1) at(earn_ratio_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

	logit dissolve i.marr_dur c.earn_ratio_t i.hh_hours_type_t1 c.earn_ratio_t#i.hh_hours_type_t1 $controls $macro_controls structural_familism_t if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & college_wife==1, or cluster(couple_id)
	sum earn_ratio_t, detail
	margins, dydx(hh_hours_type_t1) at(earn_ratio_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

* Min Wage
logit dissolve i.marr_dur c.min_amt_above_fed_t i.hh_hours_type_t1 c.min_amt_above_fed_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum min_amt_above_fed_t, detail
margins, dydx(hh_hours_type_t1) at(min_amt_above_fed_t=(`r(p5)' `r(p50)' `r(p75)' `r(p95)' `r(p99)'))

	logit dissolve i.marr_dur c.min_amt_above_fed_t i.ft_t1_wife c.min_amt_above_fed_t#i.ft_t1_wife $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
	sum min_amt_above_fed_t, detail
	margins, dydx(ft_t1_wife) at(min_amt_above_fed_t=(`r(p5)' `r(p50)' `r(p75)' `r(p95)' `r(p99)'))

* Unemployment Compensation
logit dissolve i.marr_dur c.unemployment_percap_t i.hh_hours_type_t1 c.unemployment_percap_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum unemployment_percap_t, detail
margins, dydx(hh_hours_type_t1) at(unemployment_percap_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

**********
* Combined
**********
* Earnings Ratio-  what's weird is that it ALSO reverses direction of counter-traditional...so both specialized are better for marriage when gender wage gap is smaller. this doesn't make sense...
logit dissolve i.marr_dur c.earn_ratio_t i.division_bucket_hrs_t1 c.earn_ratio_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum earn_ratio_t, detail
margins, dydx(division_bucket_hrs_t1) at(earn_ratio_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

* Min Wage - wait this is also opposite. BUT the male-BW version AND her employment ARE in expected direction, so this one is even HARDER to interpret
logit dissolve i.marr_dur c.min_amt_above_fed_t i.division_bucket_hrs_t1 c.min_amt_above_fed_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum min_amt_above_fed_t, detail
margins, dydx(division_bucket_hrs_t1) at(min_amt_above_fed_t=(`r(p5)' `r(p50)' `r(p75)' `r(p95)' `r(p99)'))

* Unemployment Compensation
logit dissolve i.marr_dur c.unemployment_percap_t i.division_bucket_hrs_t1 c.unemployment_percap_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum unemployment_percap_t, detail
margins, dydx(division_bucket_hrs_t1) at(unemployment_percap_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))