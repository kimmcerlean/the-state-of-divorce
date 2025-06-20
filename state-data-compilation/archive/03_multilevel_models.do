********************************************************************************
* Aggregating state + individual files + analysis
* multilevel_models.do
* Kim McErlean
********************************************************************************
// first compile data
use "$created_data\acs_married_last_yr.dta", clear // created in "state_hh_type_married_last_yr"
keep if year>=2010 & year <=2018
//need to make a COUPLE LEVEL file, right now individual, so there might be duplicates?
keep if person==1 // so only keep FIRST record from household

merge m:1 year statefip using "$temp/2010_2018_compiled_dataset_factor.dta" // created in step 2
drop _merge

save "$created_data/2010_2018_individ_analysis.dta", replace

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

gen abortion_rec=abortion+3 // had negative values, but if I want to use as factor variable, can't, so this sets min to 0 (was previously -3)

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
melogit male_bw year f_tanf if college==0 || statefip:, or
estimates store b7
melogit male_bw year center_women_legis if college==0 || statefip:, or
estimates store b8
melogit male_bw year center_dems_legis if college==0 || statefip:, or
estimates store b9
melogit male_bw year center_x_pov_rate if college==0 || statefip:, or
estimates store b10
melogit male_bw year leave_policy_score if college==0 || statefip:, or // use this as factor var or no?
estimates store b11
melogit male_bw year abortion_rec if college==0 || statefip:, or
estimates store b12
melogit male_bw year center_fepresch center_infant_care_pct center_cc_percent_served center_pct_workers_paid_leave center_eitc_credit f_tanf center_women_legis center_dems_legis center_x_pov_rate leave_policy_score abortion_rec if college==0 || statefip:, or
estimates store b13
melogit male_bw center_fepresch center_infant_care_pct center_cc_percent_served center_pct_workers_paid_leave center_eitc_credit f_tanf center_women_legis center_dems_legis center_x_pov_rate leave_policy_score abortion_rec if college==0 || statefip:, or
estimates store b14 // no year

melogit male_bw year f_childcare if college==0 || statefip:, or
estimates store f1
melogit male_bw year f_implicit if college==0 || statefip:, or
estimates store f2
// melogit male_bw year f_culture if college==0 || statefip:, or
// estimates store f3
melogit male_bw year f_family_policy if college==0 || statefip:, or
estimates store f4

// estimates table b9 b10 b11 b12 b13 b14, star b(%9.3f)
esttab b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 f1 f2 f4 using "$results/multi-level_non.csv", star b(%9.5f) replace

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
melogit male_bw year f_tanf if college==1 || statefip:, or
estimates store c7
melogit male_bw year center_women_legis if college==1 || statefip:, or
estimates store c8
melogit male_bw year center_dems_legis if college==1 || statefip:, or
estimates store c9
melogit male_bw year center_x_pov_rate if college==1 || statefip:, or
estimates store c10
melogit male_bw year leave_policy_score if college==1 || statefip:, or
estimates store c11
melogit male_bw year abortion_rec if college==1 || statefip:, or
estimates store c12
melogit male_bw year center_fepresch center_infant_care_pct center_cc_percent_served center_pct_workers_paid_leave center_eitc_credit f_tanf center_women_legis center_dems_legis center_x_pov_rate leave_policy_score abortion_rec if college==1 || statefip:, or
estimates store c13
melogit male_bw center_fepresch center_infant_care_pct center_cc_percent_served center_pct_workers_paid_leave center_eitc_credit f_tanf center_women_legis center_dems_legis center_x_pov_rate leave_policy_score abortion_rec if college==1 || statefip:, or
estimates store c14 // no year 

melogit male_bw year f_childcare if college==1 || statefip:, or
estimates store f5
melogit male_bw year f_implicit if college==1 || statefip:, or
estimates store f6
// melogit male_bw year f_culture if college==1 || statefip:, or
// estimates store f7
melogit male_bw year f_family_policy if college==1 || statefip:, or
estimates store f8

// estimates table c9 c10 c11 c12 c13 c14, star b(%9.3f)
esttab c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 f5 f6 f8 using "$results/multi-level_coll.csv", star b(%9.5f) replace

********************************************************************************
* try to explain the gap? is this possible, like will any state policy reduce the gap??
* do I need to test with individual things as well? like even income??
********************************************************************************
melogit male_bw year i.college || statefip:, or
melogit male_bw year i.college || statefip: i.college, or // feel like this isn't running
melogit male_bw year || statefip: i.college, or // also same here
melogit male_bw year center_fepresch i.college || statefip:, or // not sig and college still predictive
melogit male_bw year center_infant_care_pct i.college || statefip:, or // very sig negative, but college still predictive
melogit male_bw year center_cc_percent_served i.college || statefip:, or // not sig, college still is
melogit male_bw year center_pct_workers_paid_leave i.college || statefip:, or // very sig neg, but college still predictive
melogit male_bw year f_tanf i.college || statefip:, or // slightly sig negative but so is college
melogit male_bw year center_women_legis i.college || statefip:, or // very sig neg, college still is
melogit male_bw year center_dems_legis i.college || statefip:, or // sig neg, so is college
melogit male_bw year center_x_pov_rate i.college || statefip:, or // not sig
melogit male_bw year leave_policy_score i.college || statefip:, or // sig negative
melogit male_bw year i.abortion_rec i.college || statefip:, or // sig negative but college still is
melogit male_bw year abortion_rec i.college || statefip:, or // sig negative but college still is

melogit male_bw year f_childcare i.college || statefip:, or // not sig
melogit male_bw year f_implicit i.college || statefip:, or // sig negative but college still sig
melogit male_bw year f_family_policy i.college || statefip:, or // sig negative, college still sig

melogit male_bw year center_fepresch center_infant_care_pct center_cc_percent_served center_pct_workers_paid_leave center_eitc_credit f_tanf center_women_legis center_dems_legis center_x_pov_rate leave_policy_score abortion_rec i.college || statefip:, or // lol still significantly negatively - is this also like, whatever college means dictates this - not swayed by policy? and like education is just as strong??
// attitudes = sig pos; paid leave = sig neg; tanf = sig neg; dems leg = sig neg

//try predicting dual earning?!
// also INTERACT variables with college?!? do that here?! or in state-level, this is where I am confused...test the interesting ones? probably need to interact state with individual right, because at state-level it would have to be with percent educated, which is weird. so state is just useful for predicting and comparing strength by education?

melogit male_bw year i.college##c.center_infant_care_pct  || statefip:, or
melogit male_bw year i.college##c.f_implicit || statefip:, or //
melogit male_bw year i.college##c.f_family_policy || statefip:, or // 
//are these all going to be negative positive? I still am confused on how to interpret interactions - like it's positive for college and negative for not?

// I think what I want to close is REALLY the male bw / dual earner gap for less educated, right? in marriage but what is my outcome? married last year? reduce the constant to insignificant?

********************************************************************************
* Original exploratory
********************************************************************************
// do I put any of the state level variables after statefip: ?? need to learn more about this...

xtmelogit dual_earn || statefip:, or // this means state can't be a predictor though? hence why you need to explain variance across states, a la Montez .160831 
xtmelogit dual_earn i.college || statefip:, or // .1439917

melogit dual_earn || statefip:, or // .0258741 (.160831 - okay square root of above - that is what Montez uses - not the variance, but the SD)
melogit dual_earn i.college || statefip:, or // .0207452 (.1439917 - I think this is the SQUARE of what it was before, so, in this new one, I want to take SQUARE ROOT)

melogit dual_earn || statefip:, or // .160831
estimates store m1
/* matrix m1 = e(b)
local id_var_col = colnumb(m1, "lns1_1_1:_cons")
local id_variance_m1 = exp(m1[1, `id_var_col'])
display `id_variance_m1'
*/

melogit dual_earn i.college || statefip:, or
estimates store m2

melogit dual_earn i.college f_we || statefip:, or
estimates store m3

melogit dual_earn i.college f_att || statefip:, or
estimates store m4

melogit dual_earn i.college f_const || statefip:, or
estimates store m5

melogit dual_earn i.college f_ineq || statefip:, or
estimates store m6

melogit dual_earn i.college household_hrs_male_valid || statefip:, or
estimates store m7

melogit dual_earn i.college f_we f_att f_ineq f_const household_hrs_male_valid || statefip:, or
estimates store m8

melogit dual_earn f_we f_att f_ineq f_const household_hrs_male_valid || statefip:, or
estimates store m9

estimates table m1 m2 m3 m4 m5 m6 m7 m8 m9, star

// final_cons = e(x) gets you to SD

// no idea where to put variables tbh - https://www.stata.com/features/overview/linear-mixed-models/
melogit dual_earn || statefip:, or
estimates store x1

melogit dual_earn i.college || statefip:, or
estimates store x2

melogit dual_earn i.college f_we || statefip: f_we, or
estimates store x3

melogit dual_earn i.college f_att || statefip: f_att, or
estimates store x4

melogit dual_earn i.college f_const || statefip: f_const, or 
estimates store x5

melogit dual_earn i.college f_ineq || statefip: f_ineq , or
estimates store x6

melogit dual_earn i.college household_hrs_male_valid || statefip: household_hrs_male_valid, or
estimates store x7

melogit dual_earn i.college f_we f_att f_ineq f_const household_hrs_male_valid || statefip: f_we f_att f_ineq f_const household_hrs_male_valid, or
estimates store x8

melogit dual_earn f_we f_att f_ineq f_const household_hrs_male_valid || statefip: f_we f_att f_ineq f_const household_hrs_male_valid, or
estimates store x9

estimates table x1 x2 x3 x4 x5 x6 x7 x8 x9

// Just less-educated
melogit dual_earn if college==0 || statefip:, or
estimates store e1

melogit dual_earn f_we if college==0 || statefip:, or
estimates store e2

melogit dual_earn f_att if college==0 || statefip:, or
estimates store e3

melogit dual_earn f_const if college==0 || statefip:, or
estimates store e4

melogit dual_earn f_ineq if college==0 || statefip:, or
estimates store e5

melogit dual_earn household_hrs_male_valid if college==0 || statefip:, or
estimates store e6

melogit dual_earn f_we f_att f_ineq f_const household_hrs_male_valid if college==0 || statefip:, or
estimates store e7

estimates table e1 e2 e3 e4 e5 e6 e7


// Just less-educated - male BW
melogit male_bw year if college==0 || statefip:, or
estimates store b1

melogit male_bw year f_we if college==0 || statefip:, or
estimates store b2

melogit male_bw year f_att if college==0 || statefip:, or
estimates store b3

melogit male_bw year f_const if college==0 || statefip:, or
estimates store b4

melogit male_bw year f_ineq if college==0 || statefip:, or
estimates store b5

melogit male_bw year household_hrs_male_valid if college==0 || statefip:, or
estimates store b6

melogit male_bw year f_we f_att f_ineq f_const household_hrs_male_valid if college==0 || statefip:, or
estimates store b7

melogit male_bw f_we f_att f_ineq f_const household_hrs_male_valid if college==0 || statefip:, or
estimates store b8

estimates table b1 b2 b3 b4 b5 b6 b7 b8, star b(%9.3f)
// esttab b1 b2 b3 b4 b5 b6 b7 b8 using "$results/multi-level_non1.csv", star b(%9.3f)

melogit male_bw year infant_care_pct if college==0 || statefip:, or
estimates store b9
melogit male_bw year pct_workers_paid_leave if college==0 || statefip:, or
estimates store b10
melogit male_bw year sc_leave_policy_score if college==0 || statefip:, or
estimates store b11
melogit male_bw year eitc_credit if college==0 || statefip:, or
estimates store b12
melogit male_bw year tanf_rate if college==0 || statefip:, or
estimates store b13
melogit male_bw year infant_care_pct pct_workers_paid_leave sc_leave_policy_score eitc_credit tanf_rate if college==0 || statefip:, or
estimates store b14

estimates table b9 b10 b11 b12 b13 b14, star b(%9.3f)
esttab b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 using "$results/multi-level_non.csv", star b(%9.3f)

// Just college-educated - male BW
melogit male_bw year if college==1 || statefip:, or
estimates store c1

melogit male_bw year f_we if college==1 || statefip:, or
estimates store c2

melogit male_bw year f_att if college==1 || statefip:, or
estimates store c3

melogit male_bw year f_const if college==1 || statefip:, or
estimates store c4

melogit male_bw year f_ineq if college==1 || statefip:, or
estimates store c5

melogit male_bw year household_hrs_male_valid if college==1 || statefip:, or
estimates store c6

melogit male_bw year f_we f_att f_ineq f_const household_hrs_male_valid if college==1 || statefip:, or
estimates store c7

melogit male_bw f_we f_att f_ineq f_const household_hrs_male_valid if college==1 || statefip:, or
estimates store c8

estimates table c1 c2 c3 c4 c5 c6 c7 c8, star b(%9.3f)

melogit male_bw year infant_care_pct if college==1 || statefip:, or
estimates store c9
melogit male_bw year pct_workers_paid_leave if college==1 || statefip:, or
estimates store c10
melogit male_bw year sc_leave_policy_score if college==1 || statefip:, or
estimates store c11
melogit male_bw year eitc_credit if college==1 || statefip:, or
estimates store c12
melogit male_bw year tanf_rate if college==1 || statefip:, or
estimates store c13
melogit male_bw year infant_care_pct pct_workers_paid_leave sc_leave_policy_score eitc_credit tanf_rate if college==1 || statefip:, or
estimates store c14

estimates table c9 c10 c11 c12 c13 c14, star b(%9.3f)
esttab c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 using "$results/multi-level_coll.csv", star b(%9.3f)
// esttab c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 using "$results/x_multi-level_coll.csv", star b(%9.5f)

********************************************************************************
* Variance by State and Education
********************************************************************************
gen count=1

preserve

collapse (sum) count male_bw dual_earn, by(statefip college)

restore

********************************************************************************
* Alternative, can I attentuate the sig. education differences in likelihood
* of being in a male-bw relationship?
********************************************************************************

// basic regression
logit dual_earn i.college, or
logit male_bw i.college, or

logit male_bw f_we i.college, or // still sig
logit male_bw f_att i.college, or // stil sig
logit male_bw f_ineq i.college, or // still sig
logit male_bw f_const i.college, or // stil sig
logit male_bw household_hrs_male_valid i.college, or
logit male_bw f_we f_att f_ineq f_const household_hrs_male_valid i.college, or // lol still sig

// INTERACT with college?
logit male_bw c.f_we##i.college, or // int sig
logit male_bw c.f_att##i.college, or // int not sig
logit male_bw c.f_ineq##i.college, or // int sig
logit male_bw c.f_const##i.college, or // int sig
logit male_bw c.household_hrs_male_valid##i.college, or //int sig, but college not?