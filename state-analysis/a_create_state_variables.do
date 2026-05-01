********************************************************************************
* Project: Work-family policy and divorce
* Create structural support variable
* create-state-variables.do
* Code owner: Kimberly McErlean
********************************************************************************

// Note: if following replication code, you can download the output file of this step directly from github (scale_refresh.dta). Many of my state policy variables were created outside of Stata environment

********************************************************************************
********************************************************************************
********************************************************************************
**# MAIN SCALE (This is for R1 and R2)
********************************************************************************
********************************************************************************
********************************************************************************

// use "$structural/structural_familism_june25_int.dta", clear
use "$created_data/structural_familism_june25_int.dta", clear

// In first pass, I downloaded from the CPS output to Excel and compiled in Excel with other policy variables not from CPS, but as I add new variables, it's easier to just add them on here because then I don't need to reinterpolate and mess up other things I cleaned
merge 1:1 year state_fips using "$created_data/sexism_measures_1988_2023.dta", keepusing(women_college_rate_wt married_women_college_rate_wt college_ratio_wt married_college_ratio_wt sex_ratio_marriage_wt men_unemp_rate_wt)
drop if _merge==2
drop _merge

rename married_women_college_rate_wt married_women_college_rt_wt // too long

/*
Work-family policy: explicit
- paid_leave
- cc_pct_income
- prek_enrolled_public
- cc_pct_served
- headstart_pct_totalpop
- earlyhs_pct_totalpop

Work-family policy: implicit
- policy_lib_all
- abortion_protected
- married_women_pt_rate_wt
- maternal_u5_pt_rate

Normative diffusion of gender revolution
- gender_factor_reg
- avg_egal_reg
- gender_factor_state
- avg_egal_state
- evang_lds_rate

Behavioral diffusion of gender revolution
- married_dual_earn_rate
- married_pure_male_bw_rate
- married_women_emp_rate_wt
- maternal_u5_employment_wt

Women's economic dependence on marriage (some of these could arguably be explicit work-family policy)
- min_amt_above_fed
- unemployment_percap
- wba_max
- high_inc_prem_pct
- low_inc_prem_pct
- married_earn_ratio
- welfare_all
*/

// any variables still need to be recoded so higher = more support for gender egalitarianism? (I tried to update most of this in underlying data).
gen cc_pct_inc_neg = 0 - cc_pct_income_orig // higher = bad
gen married_wom_pt_rate_neg = 0 - married_women_pt_rate_wt // is higher pt good or bad? (not included anyway)
gen mom_u5_pt_rate_neg = 0 - maternal_u5_pt_rate // is higher pt good or bad? (not included anyway)
gen non_evang_lds_rate = 1 - evang_lds_rate
gen non_pure_male_bw_rate = 1 - married_pure_male_bw_rate // not *quite* a direct foil to dual-earn percent, so should recode
// high_inc_prem_pct // negative = more of a premium, so less negative = I guess better, so is this actually fine?
// low_inc_prem_pct

// then standardize variables
foreach var in paid_leave paid_leave_length cc_pct_income_orig prek_enrolled_public cc_pct_served headstart_pct earlyhs_pct total_headstart_pct headstart_pct_totalpop earlyhs_pct_totalpop educ_spend_percap policy_lib_all abortion_protected married_women_pt_rate_wt maternal_u5_pt_rate gender_factor_reg avg_egal_reg gender_factor_state avg_egal_state evang_rate evang_lds_rate married_dual_earn_rate married_pure_male_bw_rate married_women_emp_rate_wt maternal_u5_employment_wt min_amt_above_fed unemployment_percap wba_max high_inc_prem_pct low_inc_prem_pct earn_ratio married_earn_ratio welfare_all cc_pct_inc_neg married_wom_pt_rate_neg mom_u5_pt_rate_neg non_evang_lds_rate non_pure_male_bw_rate women_college_rate_wt married_women_college_rt_wt college_ratio married_college_ratio{
	sum `var'
	gen `var'_st = (`var'- `r(mean)') / `r(sd)'
}

// MAIN SCALE
alpha paid_leave_st prek_enrolled_public_st min_amt_above_fed_st earn_ratio_st unemployment_percap_st abortion_protected_st welfare_all_st

egen structural_familism = rowtotal(paid_leave_st prek_enrolled_public_st min_amt_above_fed_st earn_ratio_st unemployment_percap_st abortion_protected_st welfare_all_st)
pwcorr structural_familism policy_lib_all policy_lib_econ policy_lib_soc

// try factor based on current also
factor paid_leave_st prek_enrolled_public_st min_amt_above_fed_st earn_ratio_st unemployment_percap_st abortion_protected_st welfare_all_st, ipf
predict f1
rename f1 structural_factor
pwcorr structural_factor structural_familism // so if I make a factor variable, also VERY correlated (~0.95)

// okay, want to explore several ways of doing robustness checks on childcare measure. one way is I want to update the scale to replace (for now; later - possibly add) prek with another indicator. I then need to be careful about timing
// will create the variable as is here? but I will just not include those years when I use
tabstat prek_enrolled_public cc_pct_income_orig cc_pct_served headstart_pct earlyhs_pct total_headstart_pct educ_spend_percap, by(year)
pwcorr prek_enrolled_public cc_pct_income_orig cc_pct_served headstart_pct earlyhs_pct total_headstart_pct educ_spend_percap
pwcorr  headstart_pct earlyhs_pct total_headstart_pct 

* current
alpha paid_leave_st prek_enrolled_public_st min_amt_above_fed_st earn_ratio_st unemployment_percap_st abortion_protected_st welfare_all_st // 0.71

* cc % income
alpha paid_leave_st min_amt_above_fed_st earn_ratio_st unemployment_percap_st abortion_protected_st welfare_all_st cc_pct_income_orig_st // 0.76
alpha paid_leave_st min_amt_above_fed_st earn_ratio_st unemployment_percap_st abortion_protected_st welfare_all_st cc_pct_inc_neg_st // 0.76
alpha paid_leave_st min_amt_above_fed_st earn_ratio_st unemployment_percap_st abortion_protected_st welfare_all_st cc_pct_inc_neg_st if year >=2009 // 0.69

egen sf_cc_income = rowtotal(paid_leave_st min_amt_above_fed_st earn_ratio_st unemployment_percap_st abortion_protected_st welfare_all_st cc_pct_inc_neg_st) // so in the scale, I do use the REVERSE coded one. When I do the individual, I use AS IS

* ccdf % served
alpha paid_leave_st min_amt_above_fed_st earn_ratio_st unemployment_percap_st abortion_protected_st welfare_all_st cc_pct_served_st // 0.71

egen sf_ccdf_served = rowtotal(paid_leave_st min_amt_above_fed_st earn_ratio_st unemployment_percap_st abortion_protected_st welfare_all_st cc_pct_served_st)

* head start %
alpha paid_leave_st min_amt_above_fed_st earn_ratio_st unemployment_percap_st abortion_protected_st welfare_all_st headstart_pct_st // 0.68

egen sf_head_start = rowtotal(paid_leave_st min_amt_above_fed_st earn_ratio_st unemployment_percap_st abortion_protected_st welfare_all_st headstart_pct_st)

* early head start %
alpha paid_leave_st min_amt_above_fed_st earn_ratio_st unemployment_percap_st abortion_protected_st welfare_all_st earlyhs_pct_st // 0.76

egen sf_early_hs = rowtotal(paid_leave_st min_amt_above_fed_st earn_ratio_st unemployment_percap_st abortion_protected_st welfare_all_st earlyhs_pct_st)

* head start combined %
alpha paid_leave_st min_amt_above_fed_st earn_ratio_st unemployment_percap_st abortion_protected_st welfare_all_st total_headstart_pct_st // 0.71

egen sf_total_hs = rowtotal(paid_leave_st min_amt_above_fed_st earn_ratio_st unemployment_percap_st abortion_protected_st welfare_all_st total_headstart_pct_st)

* prek-12 educ spending
alpha paid_leave_st min_amt_above_fed_st earn_ratio_st unemployment_percap_st abortion_protected_st welfare_all_st educ_spend_percap_st // 0.77

egen sf_educ_spend = rowtotal(paid_leave_st min_amt_above_fed_st earn_ratio_st unemployment_percap_st abortion_protected_st welfare_all_st educ_spend_percap_st)

* what if we added both head starts
alpha paid_leave_st prek_enrolled_public_st min_amt_above_fed_st earn_ratio_st unemployment_percap_st abortion_protected_st welfare_all_st headstart_pct_st earlyhs_pct_st // 0.73

* based on results, could add total HS and spending (but then this becoming HIGHLY childcare oriented) - moving some of this further exploration below with factor analysis
alpha paid_leave_st prek_enrolled_public_st min_amt_above_fed_st earn_ratio_st unemployment_percap_st abortion_protected_st welfare_all_st total_headstart_pct_st educ_spend_percap_st // 0.76

	// childcare only
	alpha prek_enrolled_public_st total_headstart_pct_st educ_spend_percap_st // 0.65
	alpha prek_enrolled_public_st total_headstart_pct_st educ_spend_percap_st paid_leave_st // 0.56 // broadening it actually ruins alpha
	
	egen sf_childcare = rowtotal(prek_enrolled_public_st total_headstart_pct_st educ_spend_percap_st)
	
	// non-childcare (following Ruppanner argument)
	alpha paid_leave_st min_amt_above_fed_st earn_ratio_st unemployment_percap_st abortion_protected_st welfare_all_st // 0.74. okay this is quite interesting...
	alpha min_amt_above_fed_st earn_ratio_st unemployment_percap_st abortion_protected_st welfare_all_st // 0.73

	egen sf_policy = rowtotal(paid_leave_st min_amt_above_fed_st earn_ratio_st unemployment_percap_st abortion_protected_st welfare_all_st)
	
* Going to create an indicator that just combines all three: PreK, Head Start, Spending. This essentially creates higher value weighting because now three childcare variables
// reminder of current: alpha paid_leave_st prek_enrolled_public_st min_amt_above_fed_st earn_ratio_st unemployment_percap_st abortion_protected_st welfare_all_st // 0.71
alpha paid_leave_st prek_enrolled_public_st min_amt_above_fed_st earn_ratio_st unemployment_percap_st abortion_protected_st welfare_all_st total_headstart_pct_st educ_spend_percap_st // 0.76 (oh duh this is above)

egen sf_childcare_wt = rowtotal(paid_leave_st prek_enrolled_public_st min_amt_above_fed_st earn_ratio_st unemployment_percap_st abortion_protected_st welfare_all_st total_headstart_pct_st educ_spend_percap_st)
pwcorr sf_childcare_wt structural_familism

// for forwards and lags
gen year_t1 = year // to get t-1 measures
gen year_t2 = year // to get t-2 measures (for robustness)

// also going to try a forward lag for policy robustness - so do t+2 and t+4. Here, I just duplicate the variable, but it's in the other file I actually create the +2 / +4 version to merge this on (so, if the divorce is 2010, I want policy info from 2012 to show the causal direction, but I will create that variable next to 2010 - and then just merge on the 2012 info from here as is)
gen year_tf2 = year
gen year_tf4 = year 

// for year of marriage control, need that variable - this file has real years, I need this to match on rel start year so I don't change it here, it's still year.
gen rel_start_yr_couple = year

// for acs analysis, need state_t1
gen state_t1 = state_fips

save "$temp/data_for_scale.dta", replace

********************************************************************************
********************************************************************************
********************************************************************************
**# Exploring factor analysis (robustness)
********************************************************************************
********************************************************************************
********************************************************************************

use "$temp/data_for_scale.dta", clear

**********************
* Following RUPPANNER
**********************
net from http://www.radyakin.org/stata/labloadingplot/
label variable paid_leave_st "Paid Leave Y/N"
label variable prek_enrolled_public_st "Pre-K Enrollment"
label variable min_amt_above_fed_st "Relative Min. Wage"
label variable earn_ratio_st "Gender Earnings Ratio"
label variable unemployment_percap_st "Unemployment Compensation"
label variable abortion_protected_st "Abortion Protected"
label variable welfare_all_st "Welfare Expenditures"
label variable total_headstart_pct_st "Total HS % Enrolled"
label variable educ_spend_percap_st "PreK-12 Spending"

// to use
factor paid_leave_st prek_enrolled_public_st min_amt_above_fed_st earn_ratio_st unemployment_percap_st abortion_protected_st welfare_all_st total_headstart_pct_st educ_spend_percap_st, ipf factors(2) // blanks(0.3)
estat kmo // 0.76. SO this one is actually better than below (was toying with no including educ spend bc v correlated with welfare spend)
rotate, varimax horst blanks(.3) // from bgsu / ucla 
// rotate, promax horst blanks(.3) // okay, this one makes the spend variables a bit more unique to one factor (i don't hate the idea that they span both though...)

labloadingplot, xlab(0(.2)1) ylab(0(.2)1) aspect(1) mlabsize(small) /// yline(0) xline(0)
xtitle(Investments in Children and Families, size(small)) ///
ytitle(Broad Policy, size(small)) mlabposition(3) 

matrix list e(r_L) // here is where I would need to add the position. how do I do this?! https://www.statalist.org/forums/forum/general-stata-discussion/general/1522966-editing-e-results-and-storing-results

predict f1 f2
rename f1 family_investment
rename f2 broad_policy

pwcorr structural_familism family_investment broad_policy sf_childcare_wt

// right so this is where UCLA recos doing polychoric (if you have binary + continuous)
polychoric paid_leave prek_enrolled_public_st min_amt_above_fed_st earn_ratio_st unemployment_percap_st abortion_protected welfare_all_st total_headstart_pct_st educ_spend_percap_st
display r(sum_w)
global N = r(sum_w)

matrix r = r(R)
factormat r, n($N) factors(2) blanks(0.3)
rotate, varimax horst blanks(.3) // okay, once I do this, these are actually really similar to the above. so that makes me feel okay

**********************
* Exploring DIFFUSION
**********************
// None of this in paper but leaving because it is the file...

// okay, I am doing all of this. Is there a world where I use my ORIGINAL scale (maybe with small variable and / or construction tweaks) and I just control for other possible factors - including attitudes, dual earning rate, religiosity, women's employment rate, etc? [This is what I end up doing]
// MAYBE make a factor for like "Diffusion" of gender rev, but that's it?
alpha avg_egal_reg_st non_evang_lds_rate_st married_dual_earn_rate_st married_women_emp_rate_wt_st // 0.6116
alpha avg_egal_reg_st non_evang_lds_rate_st non_pure_male_bw_rate_st married_women_emp_rate_wt_st //  0.6616
alpha gender_factor_reg_st non_evang_lds_rate_st non_pure_male_bw_rate_st married_women_emp_rate_wt_st //   0.6531
pwcorr avg_egal_reg_st gender_factor_reg_st fepresch_reg fechld_reg fefam_reg preschool_egal_reg working_mom_egal_reg genderroles_egal_reg non_evang_lds_rate_st non_pure_male_bw_rate_st married_dual_earn_rate_st married_women_emp_rate_wt_st // so all positively correlated - but maybe not as much as I thought? Like attitudes and behavior (male BW) not highly correlated

factor avg_egal_reg_st non_evang_lds_rate_st non_pure_male_bw_rate_st married_women_emp_rate_wt_st , ipf // okay but attitudes and religiosity quite unique. so attitudinal diffusion + behavioral diffusion might not be the same?
predict f1
rename f1 diffusion

pwcorr diffusion avg_egal_reg_st non_evang_lds_rate_st non_pure_male_bw_rate_st married_women_emp_rate_wt_st
pwcorr diffusion avg_egal_reg policy_lib_all policy_lib_soc // like - they are not really correlated at all....I mean, they are positive, but it's clear they are not at all the same...

// should I try this 2x2 indicator again??
sum avg_egal_reg, det
gen egal_attitudes = 0 if avg_egal_reg < `r(p50)'
replace egal_attitudes = 1 if avg_egal_reg >= `r(p50)'

sum diffusion, det
gen diffusion_yn = 0 if diffusion < `r(p50)'
replace diffusion_yn = 1 if diffusion >= `r(p50)'

tab egal_attitudes diffusion_yn, m row // actually less congruent than policy

sum policy_lib_all, det
gen policy_support = 0 if policy_lib_all < `r(p50)'
replace policy_support = 1 if policy_lib_all >= `r(p50)'

sum structural_familism, det
gen family_support = 0 if structural_familism < `r(p50)'
replace family_support = 1 if structural_familism >= `r(p50)'

tab policy_support family_support, m row // def not super congruent either (but my scale is much more limited than theirs)

gen policy_group_v1 = . 
replace policy_group_v1 = 1 if egal_attitudes==1 & policy_support==1
replace policy_group_v1 = 2 if egal_attitudes==1 & policy_support==0
replace policy_group_v1 = 3 if egal_attitudes==0 & policy_support==1
replace policy_group_v1 = 4 if egal_attitudes==0 & policy_support==0

gen policy_group_v2 = . 
replace policy_group_v2 = 1 if egal_attitudes==1 & family_support==1
replace policy_group_v2 = 2 if egal_attitudes==1 & family_support==0
replace policy_group_v2 = 3 if egal_attitudes==0 & family_support==1
replace policy_group_v2 = 4 if egal_attitudes==0 & family_support==0

gen policy_group_v3 = . 
replace policy_group_v3 = 1 if diffusion_yn==1 & family_support==1
replace policy_group_v3 = 2 if diffusion_yn==1 & family_support==0
replace policy_group_v3 = 3 if diffusion_yn==0 & family_support==1
replace policy_group_v3 = 4 if diffusion_yn==0 & family_support==0

label define policy_group 1 "Egal, support" 2 "Egal, no" 3 "Traditional, support" 4 "Traditional, no"
label values policy_group_v1 policy_group_v2 policy_group_v3 policy_group

tab policy_group_v1, m
tab policy_group_v2, m
tab policy_group_v3, m

**********************
* Save for use
**********************

save "$created_data/scale_refresh.dta", replace

********************************************************************************
* Small descriptives for various parts of paper
********************************************************************************
// use  "$created_data/scale_refresh.dta", clear

**********************
* For Figure 2
**********************

tabstat structural_familism, by(state_fips)
tabstat structural_familism if year==1995, by(state_fips)
tabstat structural_familism if year==2005, by(state_fips)
tabstat structural_familism if year==2015, by(state_fips)
tabstat structural_familism, by(year)

**********************
* Supplementary Materials
**********************
/// Correlation matrix of the macro-level factors. wait do I want these here or at state level? I think I actually want at state-level (so file A aka here)
pwcorr structural_familism women_college_rate_wt married_women_emp_rate_wt married_pure_male_bw_rate avg_egal_reg evang_rate
pwcorr structural_familism women_college_rate_wt married_women_emp_rate_wt married_pure_male_bw_rate avg_egal_reg evang_rate min_amt_above_fed unemployment_percap earn_ratio abortion_protected welfare_all paid_leave prek_enrolled_public
pwcorr structural_familism women_college_rate_wt married_women_emp_rate_wt married_pure_male_bw_rate avg_egal_reg evang_rate cc_pct_income_orig cc_pct_served headstart_pct earlyhs_pct total_headstart_pct educ_spend_percap broad_policy family_investment structural_factor

// childcare costs specifically
pwcorr maternal_u5_employment_wt maternal_employment_wt cc_cost_orig cc_pct_income_orig structural_familism 

// correlations with religion
pwcorr evang_rate evang_lds_rate relig_rate
pwcorr evang_rate structural_familism avg_egal_reg women_college_rate_wt married_women_emp_rate_wt married_pure_male_bw_rate
pwcorr evang_lds_rate structural_familism avg_egal_reg women_college_rate_wt married_women_emp_rate_wt married_pure_male_bw_rate

// paid leave correlation with broad policy environment
bysort state_fips: egen paid_leave_state = max(paid_leave)
tabstat structural_familism, by(paid_leave_state)

**********************
* Supplementary Materials
**********************
 // summarize inputs: base variable
tabstat min_amt_above_fed unemployment_percap earn_ratio abortion_protected welfare_all paid_leave prek_enrolled_public structural_familism women_college_rate_wt married_women_emp_rate_wt married_pure_male_bw_rate avg_egal_reg evang_rate, stats(min max mean sd)

// summarize inputs: standardized
tabstat  min_amt_above_fed_st unemployment_percap_st earn_ratio_st abortion_protected_st welfare_all_st paid_leave_st prek_enrolled_public_st women_college_rate_wt_st married_women_emp_rate_wt_st married_pure_male_bw_rate_st avg_egal_reg_st evang_rate_st, stats(min max mean sd)

// to pick example states
tabstat structural_familism paid_leave_length prek_enrolled_public min_amt_above_fed earn_ratio unemployment_percap abortion_protected welfare_all if year<=2019, by(state_name)
sum structural_familism, detail // 25th =  -2.780044 ; 50th = -.7112048 ; 75th = 1.749628

/*
********************************************************************************
**# Scale for first Social Forces submission
********************************************************************************
use "$structural/structural_support_2021.dta", clear

drop if state_fips==11 // DC doesn't have a lot of these variables

// first recode so all in the same direction - want higher to be MORE support
gen earn_ratio_neg = 0-earn_ratio // bc was calculated as men's wages over women's, so currently, above 1 = men higher, want opposite
gen parent_earn_ratio_neg = 0-parent_earn_ratio
// gen welfare_neg = 0-welfare_all
// gen welfare_cash_neg = 0 -welfare_cash_asst

// standardize
foreach var in min_wage min_above_fed min_amt_above_fed paid_leave paid_leave_length earn_ratio earn_ratio_neg parent_earn_ratio parent_earn_ratio_neg welfare_all welfare_cash_asst abortion_protected unemployment_percap prek_enrolled prek_enrolled_public{
	sum `var'
	gen `var'_st = (`var'- `r(mean)') / `r(sd)'
}

** Current
alpha paid_leave_st prek_enrolled_public_st min_above_fed_st earn_ratio_neg_st unemployment_percap_st abortion_protected_st welfare_all_st
egen structural_familism_v0 = rowtotal(paid_leave_st prek_enrolled_public_st min_above_fed_st earn_ratio_neg_st unemployment_percap_st abortion_protected_st welfare_all_st)

** Explore new measure based on reviewers
alpha paid_leave_st prek_enrolled_public_st min_amt_above_fed_st earn_ratio_neg_st unemployment_percap_st abortion_protected_st welfare_all_st // replace min wage binary with indicator of amount above
alpha paid_leave_length_st prek_enrolled_public_st min_amt_above_fed_st earn_ratio_neg_st unemployment_percap_st abortion_protected_st welfare_all_st // replace paid leave binary with length
alpha paid_leave_length_st prek_enrolled_public_st min_amt_above_fed_st parent_earn_ratio_neg_st unemployment_percap_st abortion_protected_st welfare_all_st

egen structural_familism = rowtotal(paid_leave_st prek_enrolled_public_st min_amt_above_fed_st earn_ratio_neg_st unemployment_percap_st abortion_protected_st welfare_all_st) // for not including 2019/2021 - going to just use paid leave bc length really doesn't vary until after 2018 and I don't want to overexaggerate differences

// summarize inputs: base variable
tabstat paid_leave prek_enrolled_public min_amt_above_fed earn_ratio_neg unemployment_percap abortion_protected welfare_all structural_familism, stats(min max mean sd)

// summarize inputs: standardized
tabstat paid_leave_st prek_enrolled_public_st min_amt_above_fed_st earn_ratio_neg_st unemployment_percap_st abortion_protected_st welfare_all_st, stats(min max mean sd)

// egen structural_familism = rowtotal(paid_leave_length_st prek_enrolled_public_st min_amt_above_fed_st earn_ratio_neg_st unemployment_percap_st abortion_protected_st welfare_all_st)

pwcorr structural_familism_v0 structural_familism // 0.9821 correlation
browse state_fips year structural_familism structural_familism_v0 paid_leave_length prek_enrolled_public min_amt_above_fed earn_ratio_neg unemployment_percap abortion_protected welfare_all

sum structural_familism_v0, detail
sum structural_familism, detail

** test centering the variable as well
gen sf_centered=.
sum structural_familism, detail
// replace sf_centered = structural_familism - `r(p50)'
replace sf_centered = structural_familism - `r(mean)'

sum structural_familism sf_centered, detail // okay it is basically already mean centered, so it is fine?
browse structural_familism sf_centered

tabstat structural_familism paid_leave_length prek_enrolled_public min_amt_above_fed earn_ratio_neg unemployment_percap abortion_protected welfare_all, by(state_name)
tabstat structural_familism paid_leave_length prek_enrolled_public min_amt_above_fed earn_ratio_neg unemployment_percap abortion_protected welfare_all if year<=2019, by(state_name)

**Second approach: Factor-based
factor paid_leave_length_st prek_enrolled_public_st min_amt_above_fed_st earn_ratio_neg_st unemployment_percap_st abortion_protected_st welfare_all_st, ipf
predict f1
pwcorr f1 structural_familism // so if I make a factor variable, also VERY correlated (~0.95)
// browse structural_familism f1
rename f1 structural_factor

save "$state_data/structural_familism.dta", replace
*/