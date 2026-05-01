********************************************************************************
* Project: Work-family policy and divorce
* Analyses for supplementary materials
* state-level-analysis-supplementary.do
* Code owner: Kimberly McErlean
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

// merge onto census division lookup - doing this because I am trying to estimate other regions but estimates aren't stable. I think I could argue to combine COASTAL states (See Ruppaner, but also motivated by scores - see below) but not the entire West and NE. So want to instead do South, Coastal, Mid-country. I think *this* works.
merge m:1 state_fips using "$state_data/division_lookup.dta"
drop if _merge==2 // DC

tab region region_check

label define division 1	"New England" 2	"Middle Atlantic" 3	"East North Central" 4 "West North Central" ///
5	"South Atlantic" 6	"East South Central" 7	"West South Central" 8	"Mountain" 9	"Pacific"
label values census_division_code division

tab census_division_code region

gen region_alt = .
replace region_alt = 1 if region==3 // south
replace region_alt = 2 if inlist(census_division_code, 1,2,9) // coastal
replace region_alt = 3 if inlist(census_division_code, 8,3,4)

label define region_alt 1 "South" 2 "Coastal" 3 "Midwest / West"
label values region_alt region_alt

tabstat structural_familism_t, by(region) stats(min max mean sd)
tabstat structural_familism_t, by(census_division_code) stats(min max mean sd)

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

* B. 1999+ (for CCDF)
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
**# Results by race (Figures) - bc could also be select on race?? (R2, Response 6)
* Leaving here for now, might move to main file
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
**# Let's export results for education / race to have for Supplemental Materials
* Might use this instead of figures (DO use this instead of figures)
********************************************************************************
********************************************************************************
********************************************************************************

********************************************************************************
* Education
********************************************************************************

**********************
*College
**********************

** Main effects
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0  & couple_educ_gp==1, or cluster(couple_id)
margins, dydx(hh_hours_type_t1) post
outreg2 using "$results/main results/dissolution_AMEs_demographics.xls", ctitle(College Paid) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & couple_educ_gp==1, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1) post
outreg2 using "$results/main results/dissolution_AMEs_demographics.xls", ctitle(College Combined) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

** Interactions
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & couple_educ_gp==1, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_demographics.xls", ctitle(College Paid Int) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & couple_educ_gp==1, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_demographics.xls", ctitle(College Combined Int) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

**********************
* No College
**********************
** Main effects
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0  & couple_educ_gp==0, or cluster(couple_id)
margins, dydx(hh_hours_type_t1) post
outreg2 using "$results/main results/dissolution_AMEs_demographics.xls", ctitle(No College Paid) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & couple_educ_gp==0, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1) post
outreg2 using "$results/main results/dissolution_AMEs_demographics.xls", ctitle(No College Combined) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

** Interactions
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & couple_educ_gp==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_demographics.xls", ctitle(No College Paid Int) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & couple_educ_gp==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_demographics.xls", ctitle(No College Combined Int) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append


********************************************************************************
* Race 
********************************************************************************

**********************
* White (wife's race)
**********************
** Main effects
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0  & raceth_wife==1, or cluster(couple_id)
margins, dydx(hh_hours_type_t1) post
outreg2 using "$results/main results/dissolution_AMEs_demographics.xls", ctitle(White Paid) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & raceth_wife==1, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1) post
outreg2 using "$results/main results/dissolution_AMEs_demographics.xls", ctitle(White Combined) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

** Interactions
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & raceth_wife==1, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_demographics.xls", ctitle(White Paid Int) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & raceth_wife==1, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_demographics.xls", ctitle(White Combined Int) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

**********************
* Black (wife's race)
**********************
** Main effects
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0  & raceth_wife==2, or cluster(couple_id)
margins, dydx(hh_hours_type_t1) post
outreg2 using "$results/main results/dissolution_AMEs_demographics.xls", ctitle(Black Paid) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & raceth_wife==2, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1) post
outreg2 using "$results/main results/dissolution_AMEs_demographics.xls", ctitle(Black Combined) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

**interactions
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & raceth_wife==2, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_demographics.xls", ctitle(Black Paid Int) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & raceth_wife==2, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMEs_demographics.xls", ctitle(Black Combined Int) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

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
**# Attempting to deal with selection into the DoL itself (R&R Take 2)
********************************************************************************
********************************************************************************
********************************************************************************

// if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0

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

// Actual figure being used
mlogit division_bucket_hrs_t1 structural_familism_t age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled cohab_with_partner cohab_with_other pre_marital_birth i.home_owner i.educ_type i.moved_last2 i.couple_joint_religion i.num_children i.interval c.earnings_1000s $macro_controls if in_analytical_sample==1, rrr // i.year_gp 
margins, dydx(structural_familism_t) // is this how to get effects for all? I think so? For each outcome?
sum structural_familism_t, det
margins, at(structural_familism_t=(`r(min)' (1) `r(max)'))

marginsplot, recast(line) ciopts(recast(rarea)) xlabel(#10) plot2opts(lcolor("blue") mcolor("blue")) plot3opts(lcolor("ltblue") mcolor("ltblue")) plot4opts(lcolor("gs8") mcolor("gs8")) plot5opts(lcolor("black") mcolor("black")) ci1opts(color(red%30)) ci2opts(color(blue%30))  ci3opts(color(ltblue%30))  ci4opts(color(gs8%30))  ci5opts(color(black%30)) xtitle("Structural Support for Working Families") ytitle("Predicted Probability of Given Arrangement")  title("") legend(order(1 "Egalitarian" 2 "Traditional" 3 "Counter-Traditional" 4 "Second Shift" 5 "All Others") position(6) ring(3) rows(1))

sum structural_familism_t, det
margins, at(structural_familism_t=(`r(p25)' `r(p75)'))


// another thing I wanted to look at is if policy changes the education gradient of employment. wait it does this is SO interesting and actually further shows it's not really at odds - bc it's not the most economically independent women selecting out.
tab hh_hours_type_t1 high_policy_support if in_analytical_sample==1, col
tab division_bucket_hrs_t1 high_policy_support if in_analytical_sample==1, col
tab ft_t1_wife high_policy_support if in_analytical_sample==1, col
tab ft_t1_head high_policy_support if in_analytical_sample==1, col

tab hh_hours_type_t1 college_wife if in_analytical_sample==1 & high_policy_support ==0, col
tab hh_hours_type_t1 college_wife if in_analytical_sample==1 & high_policy_support ==1, col

tab hh_hours_type_t1 high_policy_support if in_analytical_sample==1 & college_wife ==0, col
tab hh_hours_type_t1 high_policy_support if in_analytical_sample==1 & college_wife ==1, col

tab division_bucket_hrs_t1 high_policy_support if in_analytical_sample==1 & college_wife ==0, col
tab division_bucket_hrs_t1 high_policy_support if in_analytical_sample==1 & college_wife ==1, col

tab ft_t1_wife high_policy_support if in_analytical_sample==1 & college_wife ==0, col
tab ft_t1_wife high_policy_support if in_analytical_sample==1 & college_wife ==1, col

tab ft_t1_head high_policy_support if in_analytical_sample==1 & college_wife ==0, col
tab ft_t1_head high_policy_support if in_analytical_sample==1 & college_wife ==1, col

tabstat female_hours_pct_t1 wife_housework_pct_t if in_analytical_sample==1 & college_wife==0, by(high_policy_support)
tabstat female_hours_pct_t1 wife_housework_pct_t if in_analytical_sample==1 & college_wife==0, by(high_policy_support) stats(p50)
tabstat female_hours_pct_t1 wife_housework_pct_t if in_analytical_sample==1 & college_wife==1, by(high_policy_support) stats(mean)
tabstat female_hours_pct_t1 wife_housework_pct_t if in_analytical_sample==1 & college_wife==1, by(high_policy_support) stats(p50)


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

// NE and Coastal
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls_nofe $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & region_alt==2, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_R&R2.xls", ctitle(Paid: Coastal) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls_nofe $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & region_alt==2, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_R&R2.xls", ctitle(Combined: Coastal) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// Midwest / West (non-CoastaL)
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls_nofe $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & region_alt==3, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_R&R2.xls", ctitle(Paid: Midwest / Mountain) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls_nofe $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & region_alt==3, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_R&R2.xls", ctitle(Combined: Midwest / Mountain) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

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
// okay actually, i don't need to use melogit at all
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
xtset state_fips // (can't use time with state, though, because I have individual-level data)
xtsum structural_familism_t

// I think this is actually just above
sum structural_familism_t structural_familism_mean structural_familism_within

****************
**Combined DoL
****************
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
**# Backup tables for earnings mediation (R&R Take 2)
********************************************************************************
********************************************************************************
********************************************************************************

*******************************
*Paid
*******************************
// M1
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_earnings_mediation.xls", ctitle(Paid Main M1) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

// M1 A
logit dissolve i.marr_dur c.structural_familism_t c.earnings_wife_1000s c.structural_familism_t#c.earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(earnings_wife_1000s) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_earnings_mediation.xls", ctitle(Paid Earnings M1a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// M2
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_earnings_mediation.xls", ctitle(Paid Mediate M2) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// M3
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.earnings_wife_1000s c.structural_familism_t#c.earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_earnings_mediation.xls", ctitle(Paid Mod Med M3) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.earnings_wife_1000s c.structural_familism_t#c.earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(earnings_wife_1000s) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_earnings_mediation.xls", ctitle(Paid Earnings Mod M3a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

** main effects
// m1
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(hh_hours_type_t1) post
outreg2 using "$results/main results/dissolution_AMES_earnings_mediation.xls", ctitle(Paid Main M1) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// m1a
logit dissolve i.marr_dur c.structural_familism_t c.earnings_wife_1000s c.structural_familism_t#c.earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(earnings_wife_1000s) post
outreg2 using "$results/main results/dissolution_AMES_earnings_mediation.xls", ctitle(Paid Earnings M1a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// m2
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(hh_hours_type_t1) post
outreg2 using "$results/main results/dissolution_AMES_earnings_mediation.xls", ctitle(Paid Mediate M2) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

	logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
	margins, dydx(earnings_wife_1000s)

// M3
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.earnings_wife_1000s c.structural_familism_t#c.earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) post
outreg2 using "$results/main results/dissolution_AMES_earnings_mediation.xls", ctitle(Paid Mod Med M3) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.earnings_wife_1000s c.structural_familism_t#c.earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(earnings_wife_1000s) post
outreg2 using "$results/main results/dissolution_AMES_earnings_mediation.xls", ctitle(Paid Earnings Mod M3a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

*******************************
*Combined DoL
*******************************
// M1
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_earnings_mediation.xls", ctitle(Combined Main M1) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// M1 A
logit dissolve i.marr_dur c.structural_familism_t c.earnings_wife_1000s c.structural_familism_t#c.earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(earnings_wife_1000s) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_earnings_mediation.xls", ctitle(Combined Earnings M1a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// M2
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_earnings_mediation.xls", ctitle(Combined Mediate M2) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// M3
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.earnings_wife_1000s c.structural_familism_t#c.earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_earnings_mediation.xls", ctitle(Combined Mod Med M3) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.earnings_wife_1000s c.structural_familism_t#c.earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(earnings_wife_1000s) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/main results/dissolution_AMES_earnings_mediation.xls", ctitle(Combined Earnings Mod M3a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

** main effects
// m1
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1) post
outreg2 using "$results/main results/dissolution_AMES_earnings_mediation.xls", ctitle(Combined Main M1) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// m1a
logit dissolve i.marr_dur c.structural_familism_t c.earnings_wife_1000s c.structural_familism_t#c.earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(earnings_wife_1000s) post
outreg2 using "$results/main results/dissolution_AMES_earnings_mediation.xls", ctitle(Combined Earnings M1a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// m2
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1) post
outreg2 using "$results/main results/dissolution_AMES_earnings_mediation.xls", ctitle(Combined Mediate M2) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append


	logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
	margins, dydx(earnings_wife_1000s)

// M3
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.earnings_wife_1000s c.structural_familism_t#c.earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) post
outreg2 using "$results/main results/dissolution_AMES_earnings_mediation.xls", ctitle(Combined Mod Med M3) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.earnings_wife_1000s c.structural_familism_t#c.earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(earnings_wife_1000s) post
outreg2 using "$results/main results/dissolution_AMES_earnings_mediation.xls", ctitle(Combined Earnings Mod M3a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append


********************************************************************************
********************************************************************************
********************************************************************************
**# Time adjustment for paid work hours
********************************************************************************
********************************************************************************
********************************************************************************
// I am using last measured paid work and housework prior to divorce. To adjust paid work in biennial years, I need to use measures recorded in years *after* divorce (since I measure divorce between survey waves). Some people disappear in this time frame and I have concerns on measurement error given this reporting time frame. This section confirms robustness to using these measures instead.

use "$temp/PSID_full_long.dta", clear // created in step 1
egen wave = group(survey_yr) // this will make years consecutive, easier for later

label define sample 0 "not sample" 1 "original sample" 2 "born-in" 3 "moved in" 4 "joint inclusion" 5 "followable nonsample parent" 6 "nonsample elderly"
label values SAMPLE sample
gen has_psid_gene=0
replace has_psid_gene = 1 if inlist(SAMPLE,1,2)

replace SEQ_NUMBER_= 0 if survey_yr==1968 & RELATION_==0 // no seq number in 1968, make it 0 if not in sample based on response to RELATION
bysort unique_id (SEQ_NUMBER_): egen in_sample=max(SEQ_NUMBER_)

drop if in_sample==0 // people with NO DATA in any year
drop if SEQ_NUMBER_==0 // won't have data because not in that year -- like SIPP, how do I know if last year is because divorced or last year in sample? right now individual level file, so fine - this is JUST last year in sample at the moment

gen relation = .
replace relation = 1 if inlist(RELATION_,1,10)
replace relation = 2 if inlist(RELATION_,2,20,22)
replace relation = 3 if !inlist(RELATION_,1,2,10,20,22)
label define relation 1 "Head" 2 "Partner" 3 "Other"
label values relation relation

tab RELATION_ relation, m

bysort unique_id: egen first_survey_yr = min(survey_yr)
bysort unique_id: egen last_survey_yr = max(survey_yr)

merge 1:1 unique_id survey_yr using "$temp/PSID_relationship_list_tomatch.dta", keepusing(MX8 partner_unique_id rel_num marr_num)
drop if _merge==2
tab relation _merge, m
drop _merge

// hours instead of earnings	
browse unique_id survey_yr WEEKLY_HRS1_T1_WIFE_ WEEKLY_HRS_T1_WIFE_ WEEKLY_HRS1_T1_HEAD_ WEEKLY_HRS_T1_HEAD_

gen weekly_hrs_t1_wife = .
replace weekly_hrs_t1_wife = WEEKLY_HRS1_T1_WIFE_ if survey_yr > 1969 & survey_yr <1994
replace weekly_hrs_t1_wife = WEEKLY_HRS_T1_WIFE_ if survey_yr >=1994
replace weekly_hrs_t1_wife = 0 if inrange(survey_yr,1968,1969) & inlist(WEEKLY_HRS1_T1_WIFE_,9,0)
replace weekly_hrs_t1_wife = 10 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_WIFE_ ==1
replace weekly_hrs_t1_wife = 27 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_WIFE_ ==2
replace weekly_hrs_t1_wife = 35 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_WIFE_ ==3
replace weekly_hrs_t1_wife = 40 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_WIFE_ ==4
replace weekly_hrs_t1_wife = 45 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_WIFE_ ==5
replace weekly_hrs_t1_wife = 48 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_WIFE_ ==6
replace weekly_hrs_t1_wife = 55 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_WIFE_ ==7
replace weekly_hrs_t1_wife = 60 if inrange(survey_yr,1968,1969)  & WEEKLY_HRS1_T1_WIFE_ ==8
replace weekly_hrs_t1_wife=. if weekly_hrs_t1_wife==999

gen weekly_hrs_t1_head = .
replace weekly_hrs_t1_head = WEEKLY_HRS1_T1_HEAD_ if survey_yr > 1969 & survey_yr <1994
replace weekly_hrs_t1_head = WEEKLY_HRS_T1_HEAD_ if survey_yr >=1994
replace weekly_hrs_t1_head = 0 if inrange(survey_yr,1968,1969) & inlist(WEEKLY_HRS1_T1_HEAD_,9,0)
replace weekly_hrs_t1_head = 10 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_HEAD_ ==1
replace weekly_hrs_t1_head = 27 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_HEAD_ ==2
replace weekly_hrs_t1_head = 35 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_HEAD_ ==3
replace weekly_hrs_t1_head = 40 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_HEAD_ ==4
replace weekly_hrs_t1_head = 45 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_HEAD_ ==5
replace weekly_hrs_t1_head = 48 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_HEAD_ ==6
replace weekly_hrs_t1_head = 55 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_HEAD_ ==7
replace weekly_hrs_t1_head = 60 if inrange(survey_yr,1968,1969)  & WEEKLY_HRS1_T1_HEAD_ ==8
replace weekly_hrs_t1_head=. if weekly_hrs_t1_head==999

sort unique_id survey_yr
browse unique_id partner_unique_id survey_yr relation weekly_hrs_t1_wife WEEKLY_HRS_T2_WIFE_ weekly_hrs_t1_head WEEKLY_HRS_T2_HEAD_   WEEKLY_HRS_T2_INDV_ if survey_yr>1995

tabstat(WEEKLY_HRS_T2_INDV_ WEEKLY_HRS_T2_HEAD_ WEEKLY_HRS_T2_WIFE_), by(survey_yr)

gen weekly_hrs_t2_focal=.
replace weekly_hrs_t2_focal = WEEKLY_HRS_T2_INDV_ if inlist(survey_yr,1999,2001)
replace weekly_hrs_t2_focal = WEEKLY_HRS_T2_HEAD_ if relation==1 & inrange(survey_yr,2003,2023)
replace weekly_hrs_t2_focal = WEEKLY_HRS_T2_WIFE_ if relation==2 & inrange(survey_yr,2003,2023)

gen weekly_hrs_t1_focal=.
replace weekly_hrs_t1_focal = weekly_hrs_t1_head if relation==1
replace weekly_hrs_t1_focal = weekly_hrs_t1_wife if relation==2

inspect weekly_hrs_t1_focal weekly_hrs_t2_focal if inlist(relation,1,2)

browse unique_id partner_unique_id survey_yr relation weekly_hrs_t1_focal weekly_hrs_t2_focal weekly_hrs_t1_wife WEEKLY_HRS_T2_WIFE_ weekly_hrs_t1_head WEEKLY_HRS_T2_HEAD_   WEEKLY_HRS_T2_INDV_ if survey_yr>1995
 
pwcorr weekly_hrs_t1_focal weekly_hrs_t2_focal if inlist(relation,1,2)

********************************************************************************
* Here is where adjustment happens
********************************************************************************
sort unique_id survey_yr
gen weekly_hrs_t_focal = .
replace weekly_hrs_t_focal = weekly_hrs_t1_focal[_n+1] if inrange(survey_yr,1968,1996) & wave==wave[_n+1]-1 & unique_id==unique_id[_n+1]
replace weekly_hrs_t_focal = weekly_hrs_t2_focal[_n+1] if inrange(survey_yr,1997,2019) & wave==wave[_n+1]-1 & unique_id==unique_id[_n+1]

browse unique_id partner_unique_id FAMILY_INTERVIEW_NUM_ survey_yr relation weekly_hrs_t1_focal weekly_hrs_t2_focal

gen weekly_hrs_t_head = weekly_hrs_t_focal if relation==1
bysort FAMILY_INTERVIEW_NUM_ survey_yr (weekly_hrs_t_head): replace weekly_hrs_t_head = weekly_hrs_t_head[1] 
gen weekly_hrs_t_wife = weekly_hrs_t_focal if relation==2
bysort FAMILY_INTERVIEW_NUM_ survey_yr (weekly_hrs_t_wife): replace weekly_hrs_t_wife = weekly_hrs_t_wife[1] 


sort FAMILY_INTERVIEW_NUM_ survey_yr

browse unique_id partner_unique_id FAMILY_INTERVIEW_NUM_ survey_yr relation weekly_hrs_t_focal weekly_hrs_t_head weekly_hrs_t_wife

keep unique_id partner_unique_id FAMILY_INTERVIEW_NUM_ survey_yr relation weekly_hrs_t_focal weekly_hrs_t_head weekly_hrs_t_wife weekly_hrs_t1_focal weekly_hrs_t2_focal WEEKLY_HRS_T2_INDV_ WEEKLY_HRS_T2_HEAD_ WEEKLY_HRS_T2_WIFE_

foreach var in weekly_hrs_t_focal weekly_hrs_t_head weekly_hrs_t_wife weekly_hrs_t1_focal weekly_hrs_t2_focal WEEKLY_HRS_T2_INDV_ WEEKLY_HRS_T2_HEAD_ WEEKLY_HRS_T2_WIFE_{
	rename `var' chk_`var'
}

save "$temp/PSID_hours_t_lookup.dta", replace

use "$created_data/PSID_union_sample_with_policy.dta", clear
rename year survey_yr

merge 1:1 unique_id partner_unique_id survey_yr using "$temp/PSID_hours_t_lookup.dta", keepusing(chk*)
drop if _merge==2
tab _merge
drop _merge

browse unique_id partner_unique_id survey_yr weekly_hrs_t1_wife weekly_hrs_t1_head chk_weekly_hrs_t_wife chk_weekly_hrs_t_head // WEEKLY_HRS_T2_HEAD_ chk_WEEKLY_HRS_T2_HEAD_ WEEKLY_HRS_T2_WIFE_ chk_WEEKLY_HRS_T2_WIFE_

replace chk_weekly_hrs_t_wife = . if chk_weekly_hrs_t_wife==999 | chk_weekly_hrs_t_wife==998 
replace chk_weekly_hrs_t_head = . if chk_weekly_hrs_t_head==999 | chk_weekly_hrs_t_head==998

inspect weekly_hrs_t1_wife weekly_hrs_t1_head chk_weekly_hrs_t_wife chk_weekly_hrs_t_head

egen chk_couple_hours_t = rowtotal(chk_weekly_hrs_t_wife chk_weekly_hrs_t_head )
gen chk_female_hours_pct_t = chk_weekly_hrs_t_wife/chk_couple_hours_t

gen chk_hh_hours_type_t=.
replace chk_hh_hours_type_t=1 if chk_female_hours_pct_t >=.4000 & chk_female_hours_pct_t <=.6000
replace chk_hh_hours_type_t=2 if chk_female_hours_pct_t <.4000
replace chk_hh_hours_type_t=3 if chk_female_hours_pct_t >.6000 & chk_female_hours_pct_t!=.
replace chk_hh_hours_type_t=4 if chk_weekly_hrs_t_wife==0 & chk_weekly_hrs_t_head==0

label define hh_hours_type 1 "Dual Earner" 2 "Male BW" 3 "Female BW" 4 "No Earners"
label values chk_hh_hours_type_t hh_hours_type

tab hh_hours_type_t1 chk_hh_hours_type_t, m

gen chk_division_bucket_hrs_t=5
replace chk_division_bucket_hrs_t = 1 if chk_hh_hours_type_t== 1 & housework_bkt_t== 1 // dual, dual
replace chk_division_bucket_hrs_t = 2 if chk_hh_hours_type_t== 2 & housework_bkt_t== 2 // male bw, female hw
replace chk_division_bucket_hrs_t = 3 if chk_hh_hours_type_t== 3 & housework_bkt_t== 3 // female bw, male hw
replace chk_division_bucket_hrs_t = 4 if chk_hh_hours_type_t== 1 & housework_bkt_t== 2 // dual, female hw
replace chk_division_bucket_hrs_t = . if chk_hh_hours_type_t== . | housework_bkt_t== .

tab division_bucket_hrs_t1 chk_division_bucket_hrs_t, m

capture label define division_bucket 1 "Egalitarian" 2 "Traditional" 3 "Counter-traditional" 4 "Second shift" 5 "All Other"
label values chk_division_bucket_hrs_t division_bucket

********************************************************************************
* Analysis
********************************************************************************

global controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.educ_type i.moved_last2 i.couple_joint_religion i.num_children i.year_gp"  // add year here.

global macro_controls "women_college_rate_wt_t married_women_emp_rate_wt_t avg_egal_reg_t married_pure_male_bw_rate_t evang_rate_t" // add religion here 


**********************
* Main effects
**********************
// see my concern is I think for PAID results to be comparable to others, Ido NEED t-1. and the fact that THESE results get stronger actually support the anticipation- if women who are thinking about divorce increase their earnings in ancitipation - more earning s- mroe dual-earning, dual earning MORE LIKELY to divoce relative to male-BW. so the stronger results actually suggest this anticipation COULD be happening.

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(hh_hours_type_t1) level(95) post
estimates store est1c

count if e(sample) // 5,610
tab dissolve if e(sample) // 353

logit dissolve i.marr_dur c.structural_familism_t i.chk_hh_hours_type_t $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(chk_hh_hours_type_t) level(95) post
estimates store est1c0

count if e(sample) // 5,392
tab dissolve if e(sample) // 344 // so it's just 9 dissolutions okay

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(division_bucket_hrs_t1) level(95) post
estimates store est2c

logit dissolve i.marr_dur c.structural_familism_t i.chk_division_bucket_hrs_t $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
margins, dydx(chk_division_bucket_hrs_t) level(95) post
estimates store est2c0

**********************
* Interaction effects 
**********************
// Main model - just control for macro factors, but not yet interacted
logit dissolve i.marr_dur c.structural_familism_t i.chk_hh_hours_type_t c.structural_familism_t#i.chk_hh_hours_type_t $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(chk_hh_hours_type_t) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) 

// Main model - just control for macro factors, but not yet interacted
logit dissolve i.marr_dur c.structural_familism_t i.chk_division_bucket_hrs_t c.structural_familism_t#i.chk_division_bucket_hrs_t $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or cluster(couple_id)
sum structural_familism_t, detail
margins, dydx(chk_division_bucket_hrs_t) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) 


**********************
* Selection
**********************
mlogit chk_division_bucket_hrs_t structural_familism_t if in_analytical_sample==1  // $controls $macro_controls 
sum structural_familism_t, det
margins, at(structural_familism_t=(`r(min)' (1) `r(max)'))
marginsplot, recast(line) ciopts(recast(rarea)) xlabel(#10) plot2opts(lcolor("blue") mcolor("blue")) plot3opts(lcolor("ltblue") mcolor("ltblue")) plot4opts(lcolor("gs8") mcolor("gs8")) plot5opts(lcolor("black") mcolor("black")) ci1opts(color(red%70)) ci2opts(color(blue%70))  ci3opts(color(ltblue%70))  ci4opts(color(gs8%70))  ci5opts(color(black%70)) xtitle("Structural Support for Working Families") ytitle("Predicted Probability of Given Arrangement")  title("") legend(order(1 "Egalitarian" 2 "Traditional" 3 "Counter-Traditional" 4 "Second Shift" 5 "All Others") position(6) ring(3) rows(1))

save  "$created_data/PSID_union_sample_with_policy_adj.dta", replace

/*
Exploratory for reviews but not used:

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
**# What about work-family conflict?
* (Not in paper)
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

*/
