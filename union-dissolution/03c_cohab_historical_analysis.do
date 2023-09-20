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

// time variables
recode start_this_rel (1970/1999=1)(2000/2019=2), gen(time)

recode start_this_rel (1970/1979=1)(1980/1989=2)(1990/1999=3)(2000/2009=4)(2010/2019=5), gen(time_detail)
replace time_detail=. if inlist(time_detail,1968,1969)

recode start_this_rel (1970/1989=1)(1990/1999=2)(2000/2019=3), gen(time_v3)
replace time_v3=. if inlist(time_v3,1968,1969)

********************************************************************************
* Models
********************************************************************************
* Initial exploration

tab couple_educ_gp marr_trans if MARITAL_PAIRS_==1, row
tab couple_educ_gp marr_trans if rel_start_flag==0 & MARITAL_PAIRS_==1, row

tab couple_educ_gp marr_trans if rel_start_flag==0 & time==1 & MARITAL_PAIRS_==1, row
tab couple_educ_gp marr_trans if rel_start_flag==0 & time==2 & MARITAL_PAIRS_==1, row

tab college marr_trans if rel_start_flag==0 & time==1 & MARITAL_PAIRS_==1, row
tab college marr_trans if rel_start_flag==0 & time==2 & MARITAL_PAIRS_==1, row

tab couple_educ_gp marr_trans if rel_start_flag==0 & time==1 & MARITAL_PAIRS_==1 & dur<=5, row // bc older relationships have longer duration (if didn't transition)
tab couple_educ_gp marr_trans if rel_start_flag==0 & time==2 & MARITAL_PAIRS_==1 & dur<=5, row

tab couple_educ_gp marr_trans if rel_start_flag==0 & time==1 & MARITAL_PAIRS_==1 & dur>0 & dur<=5, row
tab couple_educ_gp marr_trans if rel_start_flag==0 & time==2 & MARITAL_PAIRS_==1 & dur>0 & dur<=5, row

logit marr_trans i.couple_educ_gp if rel_start_flag==0, or
logit marr_trans i.couple_educ_gp##i.time if rel_start_flag==0 & inlist(time,1,2) & dur<=5, or
margins time#couple_educ_gp // this could work too.

logit marr_trans i.educ##i.time if rel_start_flag==0 & inlist(time,1,2) & dur<=5, or
margins time#educ // this could do it
marginsplot

logit marr_trans i.couple_educ_gp i.hh_earn_type_lag if rel_start_flag==0 & MARITAL_PAIRS_==1, or

logit marr_trans i.couple_educ_gp##i.time_detail if rel_start_flag==0 & MARITAL_PAIRS_==1, or
margins time_detail#couple_educ_gp

logit marr_trans i.educ##i.time_detail if rel_start_flag==0, or // & MARITAL_PAIRS_==1, or
margins time_detail#educ
marginsplot

logit marr_trans i.educ##i.time_v3 if rel_start_flag==0, or // & MARITAL_PAIRS_==1, or
margins time_v3#educ
marginsplot

logit marr_trans i.couple_educ_gp##i.time_v3 if rel_start_flag==0, or // & MARITAL_PAIRS_==1, or
margins time_v3#couple_educ_gp
marginsplot

// I think income is measured as last year, so already lagged by default? confirm this.. and remember i am missing a bunch of income for those in the middle because no labor market income recorded. need CNEF file. need to think about marriage bar because won't be able to get that index with non ref/head
logit marr_trans i.hh_earn_type dur if couple_educ_gp==0 & time==1 & dur<=5, or
logit marr_trans i.hh_earn_type dur if couple_educ_gp==0 & time==2 & dur<=5, or

logit marr_trans i.employed_man i.employed_woman dur if couple_educ_gp==0 & time==1 & dur<=5, or
logit marr_trans i.employed_man i.employed_woman dur if couple_educ_gp==0 & time==2 & dur<=5, or

logit marr_trans i.hh_earn_type dur if couple_educ_gp==1 & time==1 & dur<=5, or
logit marr_trans i.hh_earn_type dur if couple_educ_gp==1 & time==2 & dur<=5, or

logit marr_trans i.employed_man i.employed_woman dur if couple_educ_gp==1 & time==1 & dur<=5, or
logit marr_trans i.employed_man i.employed_woman dur if couple_educ_gp==1 & time==2 & dur<=5, or // okay THIS is interesting