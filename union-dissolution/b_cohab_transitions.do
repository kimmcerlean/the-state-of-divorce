********************************************************************************
* Modeling the transition from cohabitation to marriage
* cohab_transitions.do
* Kim McErlean
********************************************************************************

use "$data_keep\PSID_cohab_sample.dta", clear

tab college_wife_all college_complete_head

drop couple_educ_gp
gen couple_educ_gp=0
replace couple_educ_gp =1 if (college_wife_all==1 | college_complete_head==1)

drop if inlist(IN_UNIT,2,3) // immigrant samples, not consistently tracked


// also forgot I wanted to restrict to first marriage GAH, so num_marriages only gets me part way because I need to know FIRST - so use first_marriage yr? (FIRST_MARRIAGE_YR_START)
drop if FIRST_MARRIAGE_YR_START < start_rel // will this work? 


** Should I do competing risk or just logit? Also revisit SIPP and see if it matters?!
gen outcome=0
replace outcome = 1 if marr_trans==1
replace outcome = 2 if how_end_cohab == 2 & survey_yr == end_rel


********************************************************************************
* All
********************************************************************************
// have to restrict to full_data==1
logit marr_trans i.college_wife_all if full_data==1, or
logit marr_trans i.college_complete_head, or
logit marr_trans i.college_complete_head if full_data==1, or

logit marr_trans i.hh_earn_type if full_data==1, or
logit marr_trans i.hh_earn_type if full_data==1 & couple_educ_gp==0, or
logit marr_trans i.hh_earn_type if full_data==1 & couple_educ_gp==1, or

logit marr_trans i.hh_earn_type_bkd if full_data==1, or // no diff dual and male primary, but male sole = less
logit marr_trans i.hh_earn_type_bkd if full_data==1 & couple_educ_gp==0, or // same - okay so no diffs?
logit marr_trans i.hh_earn_type_bkd if full_data==1 & couple_educ_gp==1, or // same

// is it a diff of short v. LT cohab?

********************************************************************************
* Just long-term cohabitors
********************************************************************************
// can use longer-time frame, always_married technically tells me, wait though, GAH some switch from ST to LT, so need like whenever they are in the LT one?? and can't just use COUPLE_STATUS because some are from when they are married - get status immediately prior?
sort id survey_yr

gen couple_status_cohab=COUPLE_STATUS_REF_
replace couple_status_cohab = COUPLE_STATUS_REF_[_n-1] if COUPLE_STATUS_REF_==1 & id==id[_n-1]
label define couple_status 2 "LT" 4 "ST"
label values couple_status_cohab couple_status

browse id survey_yr couple_status_cohab COUPLE_STATUS_REF_

/* check
tab educ_wife educ_wife_all 
tab educ_wife educ_wife_all if couple_status_cohab==2
*/

logit marr_trans i.college_wife_all if couple_status_cohab==2, or
logit marr_trans i.college_complete_head if couple_status_cohab==2, or

logit marr_trans i.hh_earn_type if couple_status_cohab==2, or // male BW not sig here (well, is marginally - but this is a larger sample)
logit marr_trans i.hh_earn_type if couple_status_cohab==2 & couple_educ_gp==0, or // okay so here male BW not sig like very much so
logit marr_trans i.hh_earn_type if couple_status_cohab==2 & couple_educ_gp==1, or // okay nothing sig here...

logit marr_trans i.hh_earn_type_bkd if couple_status_cohab==2, or // no diff dual and male primary, but male sole = less
logit marr_trans i.hh_earn_type_bkd if couple_status_cohab==2 & couple_educ_gp==0, or // same
logit marr_trans i.hh_earn_type_bkd if couple_status_cohab==2 & couple_educ_gp==1, or // nothing sig here, but this might be sample?

browse id survey_yr female_earn_pct earnings_female earnings_male hh_earn_type
