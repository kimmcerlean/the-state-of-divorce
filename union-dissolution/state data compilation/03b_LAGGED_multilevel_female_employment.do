********************************************************************************
* Aggregating state + individual files + analysis
* lagged_multilevel_female_employment.do
* Kim McErlean
********************************************************************************
// first compile data
use "$temp/2010_2018_compiled_dataset_factor.dta", clear // created in step 2, saving new copy to use as lagged
drop if year > 2016
recode year (2010=2012) (2011=2013) (2012=2014) (2013=2015) (2014=2016) (2015=2017) (2016=2018)
save "$temp/2010_2018_compiled_dataset_factor_LAG.dta", replace

use "$created_data\acs_single_females.dta", clear // created in "state_single_female_employment"
keep if year>=2012 & year <=2018 // so just doing 2012-2018, using indicators from 2010-2016

merge m:1 year statefip using "$temp/2010_2018_compiled_dataset_factor_LAG.dta" // created above
drop _merge

save "$created_data/2010_2018_employment_analysis_LAG.dta", replace

gen abortion_rec=abortion+3 // had negative values, but if I want to use as factor variable, can't, so this sets min to 0 (was previously -3)

sum f_leave_policy
gen sc_f_leave_policy = (f_leave_policy-`r(min)') / (`r(max)'-`r(min)')

sum f_tanf
gen sc_f_tanf = (f_tanf-`r(min)') / (`r(max)'-`r(min)')

// basic regression
logit employed i.college, or
logit ft_employed i.college, or

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
 
// Just less-educated - employed FT
melogit ft_employed year if college==0 & year>=2015 || statefip:, or // had to add year because too many observations to estimate with full sample
estimates store b1
melogit ft_employed year center_fepresch if college==0 & year>=2015 || statefip:, or
estimates store b2
melogit ft_employed year center_infant_care_pct if college==0 & year>=2015 || statefip:, or
estimates store b3
melogit ft_employed year center_cc_percent_served if college==0 & year>=2015 || statefip:, or
estimates store b4
melogit ft_employed year center_pct_workers_paid_leave if college==0 & year>=2015 || statefip:, or
estimates store b5
melogit ft_employed year center_eitc_credit if college==0 & year>=2015 || statefip:, or
estimates store b6
melogit ft_employed year sc_f_tanf if college==0 & year>=2015 || statefip:, or
estimates store b7
melogit ft_employed year center_women_legis if college==0 & year>=2015 || statefip:, or
estimates store b8
melogit ft_employed year center_dems_legis if college==0 & year>=2015 || statefip:, or
estimates store b9
melogit ft_employed year center_x_pov_rate if college==0 & year>=2015 || statefip:, or
estimates store b10
melogit ft_employed year center_leave_policy_score if college==0 & year>=2015 || statefip:, or
estimates store b11
melogit ft_employed year sc_f_leave_policy if college==0 & year>=2015 || statefip:, or
estimates store b12
melogit ft_employed year abortion_rec if college==0 & year>=2015 || statefip:, or
estimates store b13
melogit ft_employed year center_fepresch center_infant_care_pct center_cc_percent_served center_pct_workers_paid_leave center_eitc_credit sc_f_tanf center_women_legis center_dems_legis center_x_pov_rate center_leave_policy_score sc_f_leave_policy abortion_rec if college==0 & year>=2015 || statefip:, or
estimates store b14
melogit ft_employed center_fepresch center_infant_care_pct center_cc_percent_served center_pct_workers_paid_leave center_eitc_credit sc_f_tanf center_women_legis center_dems_legis center_x_pov_rate center_leave_policy_score sc_f_leave_policy abortion_rec if college==0 & year>=2015 || statefip:, or
estimates store b15 // no year

/*
melogit ft_employed year f_childcare if college==0 & year>=2015 || statefip:, or
estimates store f1
melogit ft_employed year f_implicit if college==0 & year>=2015 || statefip:, or
estimates store f2
// melogit ft_employed year f_culture if college==0 & year>=2015 || statefip:, or
// estimates store f3
melogit ft_employed year f_family_policy if college==0 & year>=2015 || statefip:, or
estimates store f4

melogit ft_employed year center_fepresch f_childcare f_implicit f_family_policy if college==0 & year>=2015 || statefip:, or
estimates store f10
*/

// estimates table b9 b10 b11 b12 b13 b14, star b(%9.3f)
esttab b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 using "$results/lagged_multi-level_employment_non.csv", star b(%9.5f) replace

// Just college-educated - employed FT
melogit ft_employed year if college==1 & year>=2015 || statefip:, or
estimates store c1
melogit ft_employed year center_fepresch if college==1 & year>=2015 || statefip:, or
estimates store c2
melogit ft_employed year center_infant_care_pct if college==1 & year>=2015 || statefip:, or
estimates store c3
melogit ft_employed year center_cc_percent_served if college==1 & year>=2015 || statefip:, or
estimates store c4
melogit ft_employed year center_pct_workers_paid_leave if college==1 & year>=2015 || statefip:, or
estimates store c5
melogit ft_employed year center_eitc_credit if college==1 & year>=2015 || statefip:, or
estimates store c6
melogit ft_employed year sc_f_tanf if college==1 & year>=2015 || statefip:, or
estimates store c7
melogit ft_employed year center_women_legis if college==1 & year>=2015 || statefip:, or
estimates store c8
melogit ft_employed year center_dems_legis if college==1 & year>=2015 || statefip:, or
estimates store c9
melogit ft_employed year center_x_pov_rate if college==1 & year>=2015 || statefip:, or
estimates store c10
melogit ft_employed year center_leave_policy_score if college==1 & year>=2015 || statefip:, or
estimates store c11
melogit ft_employed year sc_f_leave_policy if college==1 & year>=2015 || statefip:, or
estimates store c12
melogit ft_employed year abortion_rec if college==1 & year>=2015 || statefip:, or
estimates store c13
melogit ft_employed year center_fepresch center_infant_care_pct center_cc_percent_served center_pct_workers_paid_leave center_eitc_credit sc_f_tanf center_women_legis center_dems_legis center_x_pov_rate center_leave_policy_score sc_f_leave_policy abortion_rec if college==1 & year>=2015 || statefip:, or
estimates store c14
melogit ft_employed center_fepresch center_infant_care_pct center_cc_percent_served center_pct_workers_paid_leave center_eitc_credit sc_f_tanf center_women_legis center_dems_legis center_x_pov_rate center_leave_policy_score sc_f_leave_policy abortion_rec if college==1 & year>=2015 || statefip:, or
estimates store c15 // no year 

/*
melogit ft_employed year f_childcare if college==1 & year>=2015 || statefip:, or
estimates store f5
melogit ft_employed year f_implicit if college==1 & year>=2015 || statefip:, or
estimates store f6
// melogit ft_employed year f_culture if college==1 & year>=2015 || statefip:, or
// estimates store f7
melogit ft_employed year f_family_policy if college==1 & year>=2015 || statefip:, or
estimates store f8

melogit ft_employed year center_fepresch f_childcare f_implicit f_family_policy if college==1 & year>=2015 || statefip:, or
estimates store f11
*/

// estimates table c9 c10 c11 c12 c13 c14, star b(%9.3f)
esttab c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 using "$results/lagged_multi-level_employment_coll.csv", star b(%9.5f) replace
