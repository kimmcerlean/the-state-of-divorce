********************************************************************************
********************************************************************************
********************************************************************************
**# Quantify effect of adding Other Macro Variables
********************************************************************************
********************************************************************************
********************************************************************************
log using "$logdir\macro_walds.log", replace

********************************************************************************
* Let's do Male BW first
********************************************************************************
// Main model - just control for macro factors, but not yet interacted
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix paid1 = r(table)

// Interact women's degree attainment
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.women_college_rate_wt_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix paid2 = r(table)

// women's employment
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.married_women_emp_rate_wt_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix paid3 = r(table)

// male BW rate
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.married_pure_male_bw_rate_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix paid4 = r(table)

// regional gender norms
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.avg_egal_reg_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix paid5 = r(table)

matrix list paid1
matrix list paid5


// Compare M1 to M2 (Degree Attainment)
forvalues i = 6/10 {
	
    // Main Model
	local ame1 = paid1[1, `i'] // ame is in row 1, then each percentile is a column
    local se1 = paid1[2, `i'] // se is in row 2
    local p1 = paid1[4, `i'] // p is in row 4
    
	// Degree attainment interacted
	local ame2 = paid2[1, `i']
    local se2 = paid2[2, `i']
    local p2 = paid2[4, `i'] // p is in row 2
	
    local diff = (`ame2' - `ame1')
	local wald = (`ame1' - `ame2')^2 / (`se1'^2 + `se2'^2)
    local pval = chi2tail(1, `wald')
    
    di "Level `i': M1 = " %6.3f `ame1' " (" %6.3f `p1' ") M2 = " %6.3f `ame2' " (" %6.3f `p2' ") Diff = " %6.3f `diff' " Wald = " %6.3f `wald' ", p = " %5.3f `pval'
}


// Compare M1 to M3 (Women's Employment)
forvalues i = 6/10 {
	
    // Main model
	local ame1 = paid1[1, `i'] // ame is in row 1, then each percentile is a column
    local se1 = paid1[2, `i'] // se is in row 2
    local p1 = paid1[4, `i'] // p is in row 4
    
	// Women's employment interacted
	local ame3 = paid3[1, `i']
    local se3 = paid3[2, `i']
    local p3 = paid3[4, `i']
	
    local diff = (`ame3' - `ame1')
	local wald = (`ame1' - `ame3')^2 / (`se1'^2 + `se3'^2)
    local pval = chi2tail(1, `wald')
    
    di "Level `i': M1 = " %6.3f `ame1' " (" %6.3f `p1' ") M2 = " %6.3f `ame3' " (" %6.3f `p3' ") Diff = " %6.3f `diff' " Wald = " %6.3f `wald' ", p = " %5.3f `pval'
}

// Compare M1 to M4 (Male BW Rate)
forvalues i = 6/10 {
	
    // Main model
	local ame1 = paid1[1, `i'] // ame is in row 1, then each percentile is a column
    local se1 = paid1[2, `i'] // se is in row 2
    local p1 = paid1[4, `i'] // p is in row 4
    
	// Male BW rate interacted
	local ame4 = paid4[1, `i']
    local se4 = paid4[2, `i']
    local p4 = paid4[4, `i']
	
    local diff = (`ame4' - `ame1')
	local wald = (`ame1' - `ame4')^2 / (`se1'^2 + `se4'^2)
    local pval = chi2tail(1, `wald')
    
    di "Level `i': M1 = " %6.3f `ame1' " (" %6.3f `p1' ") M2 = " %6.3f `ame4' " (" %6.3f `p4' ") Diff = " %6.3f `diff' " Wald = " %6.3f `wald' ", p = " %5.3f `pval'
}

// Compare M1 to M5 (Gender Norms)
forvalues i = 6/10 {
	
    // Main model
	local ame1 = paid1[1, `i'] // ame is in row 1, then each percentile is a column
    local se1 = paid1[2, `i'] // se is in row 2
    local p1 = paid1[4, `i'] // p is in row 4
    
	// Gender norms interacted
	local ame5 = paid5[1, `i']
    local se5 = paid5[2, `i']
    local p5 = paid5[4, `i']
	
    local diff = (`ame5' - `ame1')
	local wald = (`ame1' - `ame5')^2 / (`se1'^2 + `se5'^2)
    local pval = chi2tail(1, `wald')
    
    di "Level `i': M1 = " %6.3f `ame1' " (" %6.3f `p1' ") M2 = " %6.3f `ame5' " (" %6.3f `p5' ") Diff = " %6.3f `diff' " Wald = " %6.3f `wald' ", p = " %5.3f `pval'
}


********************************************************************************
* Combined DoL
********************************************************************************
// Main model - just control for macro factors, but not yet interacted
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix dol1 = r(table)

// Interact women's degree attainment
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.women_college_rate_wt_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix dol2 = r(table)

// women's employment
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.married_women_emp_rate_wt_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix dol3 = r(table)

// male BW rate
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.married_pure_male_bw_rate_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix dol4 = r(table)

// regional gender norms
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.avg_egal_reg_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix dol5 = r(table)

matrix list dol1
matrix list dol5


// Compare M1 to M2 (Degree Attainment)
forvalues i = 6/10 {
	
    // Main Model
	local ame1 = dol1[1, `i'] // ame is in row 1, then each percentile is a column
    local se1 = dol1[2, `i'] // se is in row 2
    local p1 = dol1[4, `i'] // p is in row 4
    
	// Degree attainment interacted
	local ame2 = dol2[1, `i']
    local se2 = dol2[2, `i']
    local p2 = dol2[4, `i'] // p is in row 2
	
    local diff = (`ame2' - `ame1')
	local wald = (`ame1' - `ame2')^2 / (`se1'^2 + `se2'^2)
    local pval = chi2tail(1, `wald')
    
    di "Level `i': M1 = " %6.3f `ame1' " (" %6.3f `p1' ") M2 = " %6.3f `ame2' " (" %6.3f `p2' ") Diff = " %6.3f `diff' " Wald = " %6.3f `wald' ", p = " %5.3f `pval'
}


// Compare M1 to M3 (Women's Employment)
forvalues i = 6/10 {
	
    // Main model
	local ame1 = dol1[1, `i'] // ame is in row 1, then each percentile is a column
    local se1 = dol1[2, `i'] // se is in row 2
    local p1 = dol1[4, `i'] // p is in row 4
    
	// Women's employment interacted
	local ame3 = dol3[1, `i']
    local se3 = dol3[2, `i']
    local p3 = dol3[4, `i']
	
    local diff = (`ame3' - `ame1')
	local wald = (`ame1' - `ame3')^2 / (`se1'^2 + `se3'^2)
    local pval = chi2tail(1, `wald')
    
    di "Level `i': M1 = " %6.3f `ame1' " (" %6.3f `p1' ") M2 = " %6.3f `ame3' " (" %6.3f `p3' ") Diff = " %6.3f `diff' " Wald = " %6.3f `wald' ", p = " %5.3f `pval'
}

// Compare M1 to M4 (Male BW Rate)
forvalues i = 6/10 {
	
    // Main model
	local ame1 = dol1[1, `i'] // ame is in row 1, then each percentile is a column
    local se1 = dol1[2, `i'] // se is in row 2
    local p1 = dol1[4, `i'] // p is in row 4
    
	// Male BW rate interacted
	local ame4 = dol4[1, `i']
    local se4 = dol4[2, `i']
    local p4 = dol4[4, `i']
	
    local diff = (`ame4' - `ame1')
	local wald = (`ame1' - `ame4')^2 / (`se1'^2 + `se4'^2)
    local pval = chi2tail(1, `wald')
    
    di "Level `i': M1 = " %6.3f `ame1' " (" %6.3f `p1' ") M2 = " %6.3f `ame4' " (" %6.3f `p4' ") Diff = " %6.3f `diff' " Wald = " %6.3f `wald' ", p = " %5.3f `pval'
}

// Compare M1 to M5 (Gender Norms)
forvalues i = 6/10 {
	
    // Main model
	local ame1 = dol1[1, `i'] // ame is in row 1, then each percentile is a column
    local se1 = dol1[2, `i'] // se is in row 2
    local p1 = dol1[4, `i'] // p is in row 4
    
	// Gender norms interacted
	local ame5 = dol5[1, `i']
    local se5 = dol5[2, `i']
    local p5 = dol5[4, `i']
	
    local diff = (`ame5' - `ame1')
	local wald = (`ame1' - `ame5')^2 / (`se1'^2 + `se5'^2)
    local pval = chi2tail(1, `wald')
    
    di "Level `i': M1 = " %6.3f `ame1' " (" %6.3f `p1' ") M2 = " %6.3f `ame5' " (" %6.3f `p5' ") Diff = " %6.3f `diff' " Wald = " %6.3f `wald' ", p = " %5.3f `pval'
}

log close

********************************************************************************
********************************************************************************
********************************************************************************
**# Hours v. Earnings
********************************************************************************
********************************************************************************
********************************************************************************
log using "$logdir\hours_earnings_wald.log", replace

*******************
* Categorical
*******************
** Hours (MAIN)
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(hh_hours_type_t1) post

matrix hours1 = r(table)

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix hours2 = r(table)

** Earnings
// have to use the non-bucketed earnings (the continuous version) to get estimates
local cont "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner c.earnings_1000s i.educ_type i.moved_last2 i.couple_joint_religion i.num_children"  // i.region knot1 knot2 knot3 

// so direction is same but not sig. (NOR substantively large) is this enough to say it's not dependence? 
logit dissolve i.marr_dur c.structural_familism_t i.hh_earn_type_t1 `cont' $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(hh_earn_type_t1) post

matrix earn1 = r(table)

logit dissolve i.marr_dur c.structural_familism_t i.hh_earn_type_t1 c.structural_familism_t#i.hh_earn_type_t1 `cont' $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(hh_earn_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix earn2 = r(table)

** Test Main Effects
local ame3 = hours1[1, 2]
local se3 = hours1[2, 2]
local ame4 = earn1[1, 2]
local se4 = earn1[2, 2]

local diff = (`ame4' - `ame3')
local wald = (`ame3' - `ame4')^2 / (`se3'^2 + `se4'^2)
local pval = chi2tail(1, `wald')

di "Main effect comparison: M1 = " %6.3f `ame3' " (" %6.3f `p3' ") M2 = " %6.3f `ame4' " (" %6.3f `p4' ")  Diff = " %6.3f `diff' " Wald = " %6.3f `wald' ", p = " %5.3f `pval'

local ame3 = hours1[1, 3]
local se3 = hours1[2, 3]
local ame4 = earn1[1, 3]
local se4 = earn1[2, 3]

local diff = (`ame4' - `ame3')
local wald = (`ame3' - `ame4')^2 / (`se3'^2 + `se4'^2)
local pval = chi2tail(1, `wald')

di "Main effect comparison: M1 = " %6.3f `ame3' " (" %6.3f `p3' ") M2 = " %6.3f `ame4' " (" %6.3f `p4' ")  Diff = " %6.3f `diff' " Wald = " %6.3f `wald' ", p = " %5.3f `pval'

** Test Interactions
// Compare M1 to M2
forvalues i = 6/10 {
	
    // Hours
	local ame1 = hours2[1, `i'] // ame is in row 1, then each percentile is a column
    local se1 = hours2[2, `i'] // se is in row 2
    local p1 = hours2[4, `i'] // p is in row 4
    
	// Earnings
	local ame2 = earn2[1, `i']
    local se2 = earn2[2, `i']
    local p2 = earn2[4, `i'] // p is in row 2
	
    local diff = (`ame2' - `ame1')
	local wald = (`ame1' - `ame2')^2 / (`se1'^2 + `se2'^2)
    local pval = chi2tail(1, `wald')
    
    di "Level `i': M1 = " %6.3f `ame1' " (" %6.3f `p1' ") M2 = " %6.3f `ame2' " (" %6.3f `p2' ") Diff = " %6.3f `diff' " Wald = " %6.3f `wald' ", p = " %5.3f `pval'
}


*******************
* Continuous Share
*******************

** Here is her share of earnings - so no main effect the way there is with others BUT same moderation (her share = bad when support is low. wait is this norms? no this can still be economic independence - because she can only leave if she has earnings. In my head i was like would it be negative but no - her LOW SHARE reduces divorce, so we don't know if again dependence or norms violation)
logit dissolve i.marr_dur c.structural_familism_t c.female_earn_pct_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(female_earn_pct_t1) post

matrix earn3 = r(table)

logit dissolve i.marr_dur c.structural_familism_t c.female_earn_pct_t1 c.structural_familism_t#c.female_earn_pct_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(female_earn_pct_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))  post

matrix earn4 = r(table)

** Do I need to complement this with her HOURS share??
logit dissolve i.marr_dur c.structural_familism_t c.female_hours_pct_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(female_hours_pct_t1) post

matrix hours3 = r(table)

logit dissolve i.marr_dur c.structural_familism_t c.female_hours_pct_t1 c.structural_familism_t#c.female_hours_pct_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(female_hours_pct_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))  post

matrix hours4 = r(table)

** Test Main Effects
local ame3 = hours3[1, 1]
local se3 = hours3[2, 1]
local ame4 = earn3[1, 1]
local se4 = earn3[2, 1]

local diff = (`ame4' - `ame3')
local wald = (`ame3' - `ame4')^2 / (`se3'^2 + `se4'^2)
local pval = chi2tail(1, `wald')

di "Main effect comparison: M1 = " %6.3f `ame3' " (" %6.3f `p3' ") M2 = " %6.3f `ame4' " (" %6.3f `p4' ")  Diff = " %6.3f `diff' " Wald = " %6.3f `wald' ", p = " %5.3f `pval'

** Test Interactions
forvalues i = 1/5 {
	
    // Hours
	local ame1 = hours4[1, `i'] // ame is in row 1, then each percentile is a column
    local se1 = hours4[2, `i'] // se is in row 2
    local p1 = hours4[4, `i'] // p is in row 4
    
	// Earnings
	local ame2 = earn4[1, `i']
    local se2 = earn4[2, `i']
    local p2 = earn4[4, `i'] // p is in row 2
	
    local diff = (`ame2' - `ame1')
	local wald = (`ame1' - `ame2')^2 / (`se1'^2 + `se2'^2)
    local pval = chi2tail(1, `wald')
    
    di "Level `i': M1 = " %6.3f `ame1' " (" %6.3f `p1' ") M2 = " %6.3f `ame2' " (" %6.3f `p2' ") Diff = " %6.3f `diff' " Wald = " %6.3f `wald' ", p = " %5.3f `pval'
}

log close

********************************************************************************
********************************************************************************
********************************************************************************
**# Earnings Mediation
********************************************************************************
********************************************************************************
********************************************************************************

// gen earnings_wife_1000s = earnings_t1_wife / 1000
histogram earnings_wife_1000s if in_analytical_sample & earnings_wife_1000s < 200

log using "$logdir\earnings_mediation.log", replace

********************************************************************************
********************************************************************************
**# Absolute Earnings ($1000s of dollars)
********************************************************************************
********************************************************************************

*******************************
*Paid
*******************************
// M1

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix dol1 = r(table)

// M1 A
logit dissolve i.marr_dur c.structural_familism_t c.earnings_wife_1000s c.structural_familism_t#c.earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(earnings_wife_1000s) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix earn1 = r(table)

// M2
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix dol2 = r(table)

// M3
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.earnings_wife_1000s c.structural_familism_t#c.earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix dol3 = r(table)

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.earnings_wife_1000s c.structural_familism_t#c.earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
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

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix dol1 = r(table)

// M1 A
logit dissolve i.marr_dur c.structural_familism_t c.earnings_wife_1000s c.structural_familism_t#c.earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(earnings_wife_1000s) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix earn1 = r(table)

// M2
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix dol2 = r(table)

// M3
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.earnings_wife_1000s c.structural_familism_t#c.earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix dol3 = r(table)

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.earnings_wife_1000s c.structural_familism_t#c.earnings_wife_1000s $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
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



********************************************************************************
********************************************************************************
**# Logged Earnings
********************************************************************************
********************************************************************************

*******************************
*Paid
*******************************
// M1

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix dol1 = r(table)

// M1 A
logit dissolve i.marr_dur c.structural_familism_t c.earnings_wife_ln c.structural_familism_t#c.earnings_wife_ln $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(earnings_wife_ln) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix earn1 = r(table)

// M2
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 earnings_wife_ln $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix dol2 = r(table)

// M3
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.earnings_wife_ln c.structural_familism_t#c.earnings_wife_ln $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix dol3 = r(table)

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 c.earnings_wife_ln c.structural_familism_t#c.earnings_wife_ln $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(earnings_wife_ln) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

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

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix dol1 = r(table)

// M1 A
logit dissolve i.marr_dur c.structural_familism_t c.earnings_wife_ln c.structural_familism_t#c.earnings_wife_ln $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(earnings_wife_ln) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix earn1 = r(table)

// M2
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 earnings_wife_ln $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix dol2 = r(table)

// M3
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.earnings_wife_ln c.structural_familism_t#c.earnings_wife_ln $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix dol3 = r(table)

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 c.earnings_wife_ln c.structural_familism_t#c.earnings_wife_ln $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(earnings_wife_ln) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

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
********************************************************************************
********************************************************************************
**# Work Hours Mediation (test of WFC)
********************************************************************************
********************************************************************************
********************************************************************************

log using "$logdir\hours_mediation.log", replace

// Will use total couple hours and then her work / housework hours separately

*******************************
*Paid
*******************************
// M1: Main

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix dol1 = r(table)

// M2: Total Work Hours
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 total_work_couple $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix dol2 = r(table)

// M3: Wife's Hours
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 total_work_wife $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix dol3 = r(table)

matrix list dol1
matrix list dol2
matrix list dol3

// Compare M1 to M2
forvalues i = 6/10 {
	
    // Main
	local ame1 = dol1[1, `i'] // ame is in row 1, then each percentile is a column
    local se1 = dol1[2, `i'] // se is in row 2
    local p1 = dol1[4, `i'] // p is in row 4
    
	// Total couple burden
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
	
    // Main
	local ame1 = dol1[1, `i'] // ame is in row 1, then each percentile is a column
    local se1 = dol1[2, `i'] // se is in row 2
    local p1 = dol1[4, `i'] // p is in row 4
    
	// Wife's hours
	local ame3 = dol3[1, `i']
    local se3 = dol3[2, `i']
    local p3 = dol3[4, `i']
	
    local diff = (`ame3' - `ame1')
	local wald = (`ame1' - `ame3')^2 / (`se1'^2 + `se3'^2)
    local pval = chi2tail(1, `wald')
    
    di "Level `i': M1 = " %6.3f `ame1' " (" %6.3f `p1' ") M2 = " %6.3f `ame3' " (" %6.3f `p3' ") Diff = " %6.3f `diff' " Wald = " %6.3f `wald' ", p = " %5.3f `pval'
}

*******************************
*Combined DoL
*******************************

// M1

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix dol1 = r(table)

// M2
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 total_work_couple $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix dol2 = r(table)

// M3
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 total_work_wife $controls $macro_controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post

matrix dol3 = r(table)

matrix list dol1
matrix list dol2
matrix list dol3

// Compare M1 to M2
forvalues i = 6/10 {
	
    // Main
	local ame1 = dol1[1, `i'] // ame is in row 1, then each percentile is a column
    local se1 = dol1[2, `i'] // se is in row 2
    local p1 = dol1[4, `i'] // p is in row 4
    
	// Total couple hours
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
	
    // Main
	local ame1 = dol1[1, `i'] // ame is in row 1, then each percentile is a column
    local se1 = dol1[2, `i'] // se is in row 2
    local p1 = dol1[4, `i'] // p is in row 4
    
	// Her hours
	local ame3 = dol3[1, `i']
    local se3 = dol3[2, `i']
    local p3 = dol3[4, `i']
	
    local diff = (`ame3' - `ame1')
	local wald = (`ame1' - `ame3')^2 / (`se1'^2 + `se3'^2)
    local pval = chi2tail(1, `wald')
    
    di "Level `i': M1 = " %6.3f `ame1' " (" %6.3f `p1' ") M2 = " %6.3f `ame3' " (" %6.3f `p3' ") Diff = " %6.3f `diff' " Wald = " %6.3f `wald' ", p = " %5.3f `pval'
}


log close