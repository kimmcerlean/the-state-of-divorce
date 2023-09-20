********************************************************************************
* Getting PSID sample for union dissolution
* create_cohab_sample.do
* Kim McErlean
********************************************************************************

use "$created_data\PSID_cohab_sample.dta", clear // created in step 1c

********************************************************************************
* More sample restrictions and variables
********************************************************************************
// first marriage only
browse unique_id survey_yr partner_unique_id rel_type rel_num marr_num
bysort unique_id partner_unique_id (marr_num): replace marr_num=marr_num[1]
sort unique_id partner_unique_id survey_yr 
browse unique_id partner_unique_id survey_yr rel_type rel_num marr_num

keep if marr_num==1 | marr_num==.

// time variables
recode start_this_rel (1970/1999=1)(2000/2019=2), gen(time)
replace time=. if start_this_rel<=1969

recode start_this_rel (1970/1979=1)(1980/1989=2)(1990/1999=3)(2000/2009=4)(2010/2019=5), gen(time_detail)
replace time_detail=. if start_this_rel<=1969

recode start_this_rel (1970/1989=1)(1990/1999=2)(2000/2019=3), gen(time_v3)
replace time_v3=. if  start_this_rel<=1969

recode survey_yr (1970/1989=1)(1990/1999=2)(2000/2019=3), gen(time_survey)
replace time_survey=. if inlist(survey_yr,1968,1969)

// log earnings // remember this isn't complete for 1st year cohabitors
gen earnings_ln = ln(couple_earnings+0.01)
gen earnings_1000s = couple_earnings / 1000

********************************************************************************
* Models
********************************************************************************
* Initial exploration, trends over time
tab couple_educ_gp marr_trans if rel_start_flag==0 & time==1, row
tab couple_educ_gp marr_trans if rel_start_flag==0 & time==2, row

tab couple_educ_gp marr_trans if rel_start_flag==0 & time==1 & dur<=5, row
tab couple_educ_gp marr_trans if rel_start_flag==0 & time==2 & dur<=5, row

logit marr_trans i.couple_educ_gp##i.time if rel_start_flag==0, or
margins time#couple_educ_gp // this could work
marginsplot

logit marr_trans i.couple_educ_gp##i.time if rel_start_flag==0 & dur<=5, or
margins time#couple_educ_gp // this could work
marginsplot

logit marr_trans i.educ##i.time if rel_start_flag==0 & dur<=5, or
margins time#educ // this could do it
marginsplot

logit marr_trans i.educ##i.time_detail if rel_start_flag==0, or // & MARITAL_PAIRS_==1, or
margins time_detail#educ
marginsplot

logit marr_trans i.couple_educ_gp##i.time_detail if rel_start_flag==0, or // & MARITAL_PAIRS_==1, or
margins time_detail#couple_educ_gp
marginsplot

logit marr_trans i.couple_educ_gp##i.time_v3 earnings_ln if rel_start_flag==0, or // & MARITAL_PAIRS_==1, or
margins time_v3#couple_educ_gp
marginsplot

logit marr_trans i.couple_educ_gp##i.time_v3 if rel_start_flag==0 & dur<=5 & inlist(time_v3,1,3), or
margins time_v3#couple_educ_gp
marginsplot

logit marr_trans i.couple_educ_gp##i.time_survey if rel_start_flag==0, or // & MARITAL_PAIRS_==1, or
margins time_survey#couple_educ_gp
marginsplot

* Have predictors changed?
// I think income is measured as last year, so already lagged by default? confirm this.. and remember i am missing a bunch of income for those in the middle because no labor market income recorded. need CNEF file. need to think about marriage bar because won't be able to get that index with non ref/head
// also treat the middle time (1990-2000) as transition year, so don't measure that? i think that is problem is like the time groups currently too large, need them to be wider and further apart because there was a time of change, then stagnation

logit marr_trans i.hh_earn_type dur earnings_ln if couple_educ_gp==0 & time_v3==1 & dur<=5, or
logit marr_trans i.hh_earn_type dur earnings_ln if couple_educ_gp==0 & time_v3==3 & dur<=5, or
logit marr_trans i.hh_earn_type##i.time_v3 dur earnings_ln if couple_educ_gp==0 & dur<=5, or
margins time_v3#hh_earn_type

logit marr_trans i.employed_man i.employed_woman dur earnings_ln if couple_educ_gp==0 & time_v3==1 & dur<=5, or
logit marr_trans i.employed_man i.employed_woman dur earnings_ln if couple_educ_gp==0 & time_v3==3 & dur<=5, or

logit marr_trans i.hh_earn_type dur earnings_ln if couple_educ_gp==1 & time_v3==1 & dur<=5, or
logit marr_trans i.hh_earn_type dur earnings_ln if couple_educ_gp==1 & time_v3==3 & dur<=5, or
logit marr_trans i.hh_earn_type##i.time_v3 dur if couple_educ_gp==1 & dur<=5, or
margins time_v3#hh_earn_type

logit marr_trans i.employed_man i.employed_woman dur earnings_ln if couple_educ_gp==1 & time_v3==1, or
logit marr_trans i.employed_man i.employed_woman dur earnings_ln if couple_educ_gp==1 & time_v3==3, or

// browse survey_yr unique_id partner_unique_id employed_head employed_wife employed_man employed_woman