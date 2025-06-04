********************************************************************************
* Pull together all data sources
* compiled data.do
* Kim McErlean
********************************************************************************

********************************************************************************
* Merges
********************************************************************************
* Load ACS file first
use "$created_data\2010_2018_state_data_main.dta", clear

keep year statefip region x_* men_mean_hours women_mean_hours hhincome_children

merge 1:1 year statefip using "$created_data/atus_state_lookup.dta"

drop _merge
merge 1:1 year statefip using "$created_data/2010_2018_childcare_costs.dta"

gen infant_care_pct=infant_care_cost/hhincome_children

drop _merge
merge m:1 year region using "$created_data/GSS_region.dta", keepusing(famsuffr twoincs1 kidsuffr mawrkwrm paidlvdv hubbywk_all fepresch)
// disagree==1; agree==5

drop _merge
merge m:1 year region using "$created_data/2010_2018_region_policies.dta", keepusing(pct_workers_paid_leave)

drop _merge
merge 1:1 year statefip using "$created_data/2010_2018_state_policies.dta", keepusing(cc_percent_served leave_policy leave_policy_score eitc_credit tanf_rate tanf_basic tanf_cc tanf_max cost_of_living20 tanf_max_cost abortion dems_legis women_legis)

drop _merge
merge 1:1 year statefip using "$created_data/2010_2019_state_leave_data.dta", keepusing(x_*)
drop if _merge==2 // 2019 data

drop _merge
merge 1:1 year statefip using "$created_data/state_licensing.dta", keepusing(total_centers total_capacity employed_all childcare_gap)
drop if _merge==2 // territories

drop _merge

save "$created_data/2010_2018_compiled_dataset.dta", replace


********************************************************************************
* Analysis
********************************************************************************

// now thinking - should outcome be division of HH labor for JUST those who married in last year!??! I think it should be...

local ivs "men_mean_hours women_mean_hours x_lfp_women x_employed_women x_employed_women_lf x_women_wage_ratio x_women_ceo x_women_manage x_college_women x_sex_ratio x_educ_work_ratio x_90_10_inequality x_pov_rate x_fem_pov_rate household_hrs_male_valid childcare_hrs_male_valid infant_care_pct fepresch hubbywk_all pct_workers_paid_leave cc_percent_served leave_policy_score eitc_credit tanf_rate x_women_leave_par_paid"

foreach var in `ivs'{
	regress x_married_dual_non `var'
}

regress x_married_dual_non x_lfp_women x_women_wage_ratio x_women_manage x_sex_ratio x_educ_work_ratio x_90_10_inequality x_fem_pov_rate household_hrs_male_valid childcare_hrs_male_valid infant_care_pct pct_workers_paid_leave cc_percent_served leave_policy_score eitc_credit tanf_rate x_women_leave_par_paid
predict all

* work equality
regress x_married_dual_non x_lfp_women x_women_wage_ratio x_women_manage 
predict work_equal

* home equality
regress x_married_dual_non household_hrs_male_valid childcare_hrs_male_valid 
predict home_equal

* attitudes
regress x_married_dual_non x_sex_ratio fepresch hubbywk_all
predict attitudes

//PRESCHOOL KIDS SUFFER IF MOTHER WORKS - so more agreement = less egal
// HUSB SHLD WORK WIFE SHLD LOOK AFTER HOME = agreement here = also less egal

* constraints
regress x_married_dual_non infant_care_pct pct_workers_paid_leave cc_percent_served leave_policy_score eitc_credit tanf_rate
predict constraints

* inequality
regress x_married_dual_non x_educ_work_ratio x_90_10_inequality x_fem_pov_rate 
predict inequality

twoway (line x_married_dual_non year) (line all year) (line work_equal year) (line home_equal year) (line attitudes year) (line constraints year) (line inequality year) if inlist(statefip,6,12,36,48,30), by(statefip) legend(order(1 "Actual" 2 "All" 3 "Work Equal" 4 "Home Equal" 5 "Attitudes" 6 "Constraints" 7 "Inequality") size(tiny) rowgap(*.25))


********************************************************************************
* Factor analysis: see Stats semester 2, assignment 3
********************************************************************************
// first, standardize variables? Did this for stats, do I have to?

/* REU practice
tab fepresch
sum fepresch
egen fepresch_std = std(fepresch)
sum fepresch
gen fepresch_sc = (fepresch-`r(min)') / (`r(max)'-`r(min)')

sum fepresch_std // mean 0 sd 1
/* Because responses for
some questions ranged from 0 to 3 and others from 0 to 4, all items were
standardized with a mean of 0 and a standard deviation of 1
*/
sum fepresch_sc // between zero and 1

browse fepresch*
*/

foreach x of varlist fepresch infant_care_pct cc_percent_served pct_workers_paid_leave x_women_leave_par x_women_leave_par_paid leave_policy_score eitc_credit tanf_rate tanf_basic tanf_cc tanf_max tanf_max_cost women_legis dems_legis x_pov_rate x_all_pt x_women_pt x_all_prob childcare_gap{
	// egen z_`x'=std(`x')
	sum `x'
	gen sc_`x' = (`x'-`r(min)') / (`r(max)'-`r(min)')
	summarize `x', meanonly
	gen center_`x' = `x' - `r(mean)'
}

// Factor-analysis
* tanf
factor sc_tanf_rate sc_tanf_basic sc_tanf_max, pf
alpha sc_tanf_rate sc_tanf_basic sc_tanf_max // .72
predict f1
rename f1 f_tanf

*childcare
factor sc_cc_percent_served sc_infant_care_pct sc_childcare_gap, pf
alpha sc_cc_percent_served sc_infant_care_pct sc_childcare_gap // poor loading
rotate, varimax
predict f1
rename f1 f_childcare

/* childcare - old
factor sc_cc_percent_served sc_infant_care_pct sc_tanf_cc, pf
rotate, varimax
alpha sc_cc_percent_served sc_infant_care_pct sc_tanf_cc // okay no combination of this loads well
predict f1
rename f1 f_childcare
*/

* leave policies
alpha x_women_leave_par x_women_leave_par_paid x_women_ratio_paid pct_workers_paid_leave
alpha x_women_leave_par_paid pct_workers_paid_leave // these don't really cluster together
correlate x_women_leave_par x_women_leave_par_paid x_women_ratio_paid pct_workers_paid_leave

alpha x_women_leave_par x_women_leave_par_paid pct_workers_paid_leave // 0.36
alpha sc_x_women_leave_par sc_x_women_leave_par_paid sc_pct_workers_paid_leave // 0.55
factor sc_x_women_leave_par sc_x_women_leave_par_paid sc_pct_workers_paid_leave
predict f1
rename f1 f_leave_policy

* implicit policies
factor sc_dems_legis sc_women_legis, pf
alpha sc_dems_legis sc_women_legis // 0.63
predict f1
rename f1 f_implicit

* all "cultural support"
factor sc_fepresch sc_cc_percent_served, pf
alpha sc_fepresch sc_cc_percent_served // 0.26
predict f1
rename f1 f_culture

* all family policy
factor sc_cc_percent_served sc_tanf_cc sc_pct_workers_paid_leave leave_policy_score sc_eitc_credit sc_tanf_rate abortion, pf
alpha sc_cc_percent_served sc_tanf_cc sc_pct_workers_paid_leave leave_policy_score sc_eitc_credit sc_tanf_rate abortion // 0.49
predict f1
rename f1 f_family_policy

/*old
local factor_vars "x_lfp_women x_women_wage_ratio x_women_manage sc_household_hrs_male_valid sc_childcare_hrs_male_valid sc_x_sex_ratio sc_fepresch sc_hubbywk_all sc_women_mean_hours infant_care_pct x_educ_work_ratio sc_x_90_10_inequality x_fem_pov_rate "

factor `factor_vars', pf // Factors 1 and 2 have eigenvalues greater than 1 stats used ipf, pf is the default.
predict f1 f2
gen f1_orig=f1
gen f2_orig=f2
drop f1 f2
rotate, varimax // now f1-f3 higher than 1
predict f1 f2 f3
drop f1 f2 f3

* work equality
factor x_lfp_women x_women_wage_ratio x_women_manage, pf // think you want eigenvalue to be greater than 1? oh wait do I put ALLL in factor and see what cluster?
alpha x_lfp_women x_women_wage_ratio x_women_manage // 0.58
predict f1
rename f1 f_we

* home equality -- okay these don't really cluster together
factor household_hrs_male_valid childcare_hrs_male_valid, pf
rotate, varimax
alpha household_hrs_male_valid childcare_hrs_male_valid

* attitudes
factor z_x_sex_ratio z_fepresch z_hubbywk_all, pf
alpha z_x_sex_ratio z_fepresch z_hubbywk_all // 0.50 - perhaps remove sex_ratio, then alpha is 0.83

factor z_fepresch z_hubbywk_all, pf
predict f1
rename f1 f_att

* constraints -- also don't seem to cluster
factor sc_women_mean_hours infant_care_pct, pf
rotate, varimax

factor infant_care_pct pct_workers_paid_leave cc_percent_served sc_leave_policy_score eitc_credit tanf_rate, pf
alpha infant_care_pct pct_workers_paid_leave cc_percent_served sc_leave_policy_score eitc_credit tanf_rate // 0.54
alpha infant_care_pct pct_workers_paid_leave sc_leave_policy_score eitc_credit tanf_rate // 0.58
alpha infant_care_pct cc_percent_served // lol .16
alpha pct_workers_paid_leave sc_leave_policy_score
alpha eitc_credit tanf_rate // 0.58
predict f1
rename f1 f_const

* inequality
factor x_educ_work_ratio sc_x_90_10_inequality x_fem_pov_rate, pf
alpha x_educ_work_ratio sc_x_90_10_inequality x_fem_pov_rate // 0.54
alpha x_educ_work_ratio x_fem_pov_rate // 0.74
factor x_educ_work_ratio x_fem_pov_rate, pf
predict f1
rename f1 f_ineq

regress x_married_dual_non f_we // greater = more dual-earnings
regress x_married_dual_non f_att // higher = less egal, so makes sense negative relationship?
regress x_married_dual_non f_const // sig. positive, but this is like when policy  better, so makes sense
regress x_married_dual_non f_ineq // negative associated, makes sense
regress x_married_dual_non f_we f_att f_ineq f_const
regress x_married_dual_non f_we f_att f_ineq household_hrs_male_valid infant_care_pct // okay this actually starts to make sense? do this same thing, but specifically use an outcome of who married in last year?!

regress x_married_dual_non_ly f_we // okay NOT significant.
regress x_married_dual_non_ly f_att // still negative and sig.
regress x_married_dual_non_ly f_ineq // neg and sig.
regress x_married_dual_non_ly f_const // not sig.
regress x_married_dual_non_ly f_we f_att f_ineq // interesting f_we becomes sig neg here. is it because these are then FEMALE BW households?!
regress x_married_dual_non_ly f_we f_att f_ineq household_hrs_male_valid infant_care_pct
*/

save "$temp/2010_2018_compiled_dataset_factor.dta", replace

/*
------------------------------------------------------------------------------------------
   x_married_dual_non_ly | Coefficient  Std. err.      t    P>|t|     [95% conf. interval]
-------------------------+----------------------------------------------------------------
                    f_we |  -.0110473   .0044199    -2.50   0.013    -.0197333   -.0023612
                   f_att |  -.0074709   .0033072    -2.26   0.024    -.0139702   -.0009716
                  f_ineq |  -.0276108   .0039077    -7.07   0.000    -.0352902   -.0199313
household_hrs_male_valid |   .0035375   .0048517     0.73   0.466    -.0059972    .0130722
         infant_care_pct |  -.2574292    .099123    -2.60   0.010    -.4522271   -.0626313
*/

regress x_married_malebw_non_ly f_we // sig and negative, makes sense for male BW
regress x_married_malebw_non_ly f_att // not sig.
regress x_married_malebw_non_ly f_ineq // pos, but not sig
regress x_married_malebw_non_ly f_const // yes - better policy = less male BW
regress x_married_malebw_non_ly f_we f_att f_ineq // inequality becomes sig and negative - also just marry less.
regress x_married_malebw_non_ly f_we f_att f_ineq household_hrs_male_valid infant_care_pct // this also feels the same as above.  - like infant care sig and neg. like I probably want the predictors to be opposite?! is this a flaw? try male sole earner? am I losing the plot a bit?

********************************************************************************
* Analysis
********************************************************************************
log using "$logdir/state-level-acs-analysis.log", replace

regress x_married_malebw_non_ly year fepresch
regress x_married_malebw_non_ly year infant_care_pct
regress x_married_malebw_non_ly year cc_percent_served
regress x_married_malebw_non_ly year pct_workers_paid_leave
regress x_married_malebw_non_ly year eitc_credit
regress x_married_malebw_non_ly year f_tanf
regress x_married_malebw_non_ly year women_legis
regress x_married_malebw_non_ly year dems_legis
regress x_married_malebw_non_ly year x_pov_rate
regress x_married_malebw_non_ly year leave_policy_score
regress x_married_malebw_non_ly year abortion

regress x_married_malebw_non_ly year f_family_policy
regress x_married_malebw_non_ly year f_implicit
regress x_married_malebw_non_ly year f_family_policy

regress x_married_malebw_non_ly fepresch infant_care_pct pct_workers_paid_leave eitc_credit f_tanf women_legis dems_legis x_pov_rate leave_policy_score abortion
outreg2 using "$results/state_level_malebw.xls", sideway stats(coef pval) label ctitle(No College - LY) dec(5) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

regress x_married_malebw_non fepresch infant_care_pct pct_workers_paid_leave eitc_credit f_tanf women_legis dems_legis x_pov_rate leave_policy_score abortion
outreg2 using "$results/state_level_malebw.xls", sideway stats(coef pval) label ctitle(No College - All) dec(5) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append


regress x_married_malebw_coll_ly year fepresch
regress x_married_malebw_coll_ly year infant_care_pct
regress x_married_malebw_coll_ly year cc_percent_served
regress x_married_malebw_coll_ly year pct_workers_paid_leave
regress x_married_malebw_coll_ly year eitc_credit
regress x_married_malebw_coll_ly year f_tanf
regress x_married_malebw_coll_ly year women_legis
regress x_married_malebw_coll_ly year dems_legis
regress x_married_malebw_coll_ly year x_pov_rate
regress x_married_malebw_coll_ly year leave_policy_score
regress x_married_malebw_coll_ly year abortion

regress x_married_malebw_coll_ly year f_family_policy
regress x_married_malebw_coll_ly year f_implicit
regress x_married_malebw_coll_ly year f_family_policy

regress x_married_malebw_coll_ly fepresch infant_care_pct pct_workers_paid_leave eitc_credit f_tanf women_legis dems_legis x_pov_rate leave_policy_score abortion
outreg2 using "$results/state_level_malebw.xls", sideway stats(coef pval) label ctitle(College - LY) dec(5) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

regress x_married_malebw_coll fepresch infant_care_pct pct_workers_paid_leave eitc_credit f_tanf women_legis dems_legis x_pov_rate leave_policy_score abortion
outreg2 using "$results/state_level_malebw.xls", sideway stats(coef pval) label ctitle(College - All) dec(5) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append


log close

********************************************************************************
* Correlations
********************************************************************************
corr x_married_malebw_non_ly fepresch infant_care_pct pct_workers_paid_leave eitc_credit f_tanf women_legis dems_legis x_pov_rate leave_policy_score abortion
corr x_married_malebw_coll_ly fepresch infant_care_pct pct_workers_paid_leave eitc_credit f_tanf women_legis dems_legis x_pov_rate leave_policy_score abortion

corr x_married_malebw_non fepresch infant_care_pct pct_workers_paid_leave eitc_credit f_tanf women_legis dems_legis x_pov_rate leave_policy_score abortion
corr x_married_malebw_coll fepresch infant_care_pct pct_workers_paid_leave eitc_credit f_tanf women_legis dems_legis x_pov_rate leave_policy_score abortion

********************************************************************************
* Fixed effects?
********************************************************************************
/*
Fixed v. random
Use fixed-effects (FE) whenever you are only interested in analyzing the impact of
variables that vary over time.  FE remove the effect of those time-invariant characteristics so we can assess the net effect of
the predictors on the outcome variable. Substantively, fixed-effects models are designed to study the causes of changes within a person [or
entity].

In RE: Interpretation of the coefficients is tricky since they include both the within-entity and between-entity effects.
In the case of TSCS data represents the average effect of X over Y when X changes across time and
between countries by one unit.
xtreg y x1, fe
estimates store fixed
xtreg y x1, re
estimates store random
hausman fixed random
if p<0.05 = use FIXED

xtreg x_married_dual_non_ly f_we if year >=2008, fe
estimates store fixed
xtreg x_married_dual_non_ly f_we if year >=2008, re
estimates store random
hausman fixed random
// p=0.1313. so use random?!


xtreg x_married_dual_non_ly f_we if year >=2008, fe
regress x_married_dual_non_ly f_we i.statefip if year >=2008 // this literally matches above
/*By adding the dummy for each country we are estimating the pure effect of x1 (by
controlling for the unobserved heterogeneity).
*/
xtreg x_married_dual_non_ly f_we if year >=2008, re // have no clue what this is doing, but v different

xtreg x_married_dual_non_ly f_we i.year if year >=2008, fe
regress x_married_dual_non_ly f_we i.statefip i.year if year >=2008 // this also matches above - I feel like I *don't* want year fixed effects?
*/

xtset statefip year // unclear if I put both state and year in there... and then do I put year in model as well?
xtreg x_married_dual_non_ly if year >=2008, fe

xtreg x_married_dual_non_ly f_we if year >=2008, fe // marginally sig - as work equality goes up, so does dual-earner % - is that how I interpret?
xtreg x_married_dual_non_ly f_att if year >=2008, fe // not sig, but the R square explains much more BETWEEN STATE
xtreg x_married_dual_non_ly f_ineq if year >=2008, fe // also not sig, but explains BETWEEN STATE
xtreg x_married_dual_non_ly f_const if year >=2008, fe // not sig and doesn't explain a lot.

regress x_married_dual_non_ly f_we i.year if year >=2008 // not sig
regress x_married_dual_non_ly f_att i.year if year >=2008 // sig neg - so this aligns with above, like the r-squared explains between state; this explains between state?
regress x_married_dual_non_ly f_ineq i.year if year >=2008 // sig neg
regress x_married_dual_non_ly f_const i.year if year >=2008 // neg but not sig.

// this uses all available info?! okay similar to above. OKAY i think if I do this, fine, but what value am I adding?! like need to quantify these impacts - is that why the multi-level model is useful? OR see if multi-level model - is more explained for more educated v. less educated??
regress x_married_dual_non_ly f_we if year >=2008 // not sig - because this xplains diffs BETWEEN EDUCATION - but not specifically for less educated?! is that an insight?! do I actually want to explain DIFFERENCES between education groups??
regress x_married_dual_non_ly f_att if year >=2008 //  sig neg
regress x_married_dual_non_ly f_ineq if year >=2008 // sig neg
regress x_married_dual_non_ly f_const if year >=2008 // neg but not sig - okay seems similar to above
regress x_married_dual_non_ly infant_care_pct if year >=2008 // neg and sig.


// entity fixed effects: regress y x1 x2 x3 x4 x5 x6 x7 i.country - this controls for ENTITY and gives you over time. I want to control for time and GET ENTITY?! so these above?
// entity and time: regress y x1 x2 x3 x4 x5 x6 x7 i.country i.year


// I think I want to estimate between state not within state over time-  so is that just LITERALLY a simple regression?

/* For goodness of fit, the R2 between is directly relevant; our R2 is 0.4900. If, however, we use
these estimates to predict the within model, we have an R2 of 0.1591. If we use these estimates to
fit the overall data, our R2 is 0.3695. */


/* rescale to make more interpretable? stats example:
sum f1
gen gun_scale_factor=(f1 - r(min)) /  (r(max) - r(min))
sum gun_scale_factor
*/

/* import child care costs 
save "$created_data/2010_2018_childcare_costs.dta", replace

import policy data
// rename Year year
destring women_legis, replace
destring dems_legis, replace
save "$created_data/2010_2018_state_policies.dta", replace

import regional policy data
save "$created_data/2010_2018_region_policies.dta", replace
*/