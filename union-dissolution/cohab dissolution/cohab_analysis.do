********************************************************************************
* Cohabitation dissolution models
* cohab_analysis.do
* Kim McErlean
********************************************************************************

********************************************************************************
* Restrict to eligible cohabitors - cohab whole time and first year of marriage if applicable
********************************************************************************
use "$data_keep\PSID_union_validation_sample.dta", clear

// need to keep them in first year of marriage if they marry so cannot just restrict based on relationship type
label define RELATION_ 1 "Head" 2 "Wife or Cohab" 10 "Head" 20 "Wife" 22 "Cohab"
label values RELATION_ RELATION_ 

sort id survey_yr
// browse id unique_id family_intvw_num survey_yr relationship relationship_type relationship_yr marital_status rel_start_all rel_end_all status_all RELATION_ in_marital_history

by family_intvw_num id (survey_yr), sort: gen marr_trans = relationship_type==2 & relationship_type[_n-1]==1 & id==id[_n-1] & family_intvw_num==family_intvw_num[_n-1] & (survey_yr==survey_yr[_n-1]+1 | survey_yr==survey_yr[_n-1]+2)
browse id unique_id family_intvw_num FAMILY_INTERVIEW_NUM_ survey_yr relationship relationship_type marr_trans relationship_yr marital_status rel_start_all rel_end_all status_all RELATION_ in_marital_history

*remove those married whole time
drop if relationship_type==2 & marr_trans==0 // this is when married and NOT the month transitioned

*do I also need first marriage only? relationship_order marriage_order marriage_order_real - but there are a lot of missing, I think because the dates are weird for cohabiting relationships because they are not in the marital history (which is how I got some of the dates)
browse id survey_yr relationship_type relationship_order marrno num_marriages marriage_order marriage_order_real // i think marrno might only be attached to the first year, but also it is relno really not marriage number.

*Probably do need to drop people I only have one year of data on - is this problematic? (see end of marriage file for this)
*Handled the 1 per HH thing in step 1a

save "$data_keep\PSID_cohab_sample.dta", replace


********************************************************************************
* Specifically 2000s onward
********************************************************************************

use "$data_keep\PSID_relationships_post2000.dta", clear

keep if relationship_type==1 // only keep those cohabiting

// might eventually need to update to be mlogit....

local controls "i.race_head i.same_race i.children i.either_enrolled TAXABLE_HEAD_WIFE_ i.religion_head age_mar_head age_mar_wife"

// overall
logit dissolve dur i.hh_earn_type_lag, or
outreg2 using "$results/psid_cohab_dissolution.xls", sideway stats(coef pval) label ctitle(Overall 1) dec(2) eform 
lpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

logit dissolve dur i.hh_earn_type_lag i.couple_educ_gp, or
outreg2 using "$results/psid_cohab_dissolution.xls", sideway stats(coef pval) label ctitle(Overall 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve dur TAXABLE_HEAD_WIFE_ i.couple_educ_gp, or // total earnings
outreg2 using "$results/psid_cohab_dissolution.xls", sideway stats(coef pval) label ctitle(Overall - Earnings) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve dur i.ft_head i.ft_wife i.couple_educ_gp, or // emmployment
outreg2 using "$results/psid_cohab_dissolution.xls", sideway stats(coef pval) label ctitle(Overall - Employment) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve dur female_earn_pct i.couple_educ_gp, or
outreg2 using "$results/psid_cohab_dissolution.xls", sideway stats(coef pval) label ctitle(Overall - Female %) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve dur i.hh_earn_type_lag i.couple_educ_gp `controls', or
outreg2 using "$results/psid_cohab_dissolution.xls", sideway stats(coef pval) label ctitle(Overall 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// no college
logit dissolve dur i.hh_earn_type_lag if couple_educ_gp==0, or
logit dissolve dur i.hh_earn_type_lag if couple_educ_gp==0, or
outreg2 using "$results/psid_cohab_dissolution.xls", sideway stats(coef pval) label ctitle(No College 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve dur TAXABLE_HEAD_WIFE_ if couple_educ_gp==0, or // earnings
outreg2 using "$results/psid_cohab_dissolution.xls", sideway stats(coef pval) label ctitle(No College - Earn) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve dur i.ft_head i.ft_wife if couple_educ_gp==0, or 
outreg2 using "$results/psid_cohab_dissolution.xls", sideway stats(coef pval) label ctitle(No College - Employ) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve dur female_earn_pct if couple_educ_gp==0, or
outreg2 using "$results/psid_cohab_dissolution.xls", sideway stats(coef pval) label ctitle(No College - Female %) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve dur i.hh_earn_type_lag `controls' if couple_educ_gp==0, or
outreg2 using "$results/psid_cohab_dissolution.xls", sideway stats(coef pval) label ctitle(No College 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// college
logit dissolve dur i.hh_earn_type_lag if couple_educ_gp==1, or
logit dissolve dur i.hh_earn_type_lag if couple_educ_gp==1, or
outreg2 using "$results/psid_cohab_dissolution.xls", sideway stats(coef pval) label ctitle(College 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve dur TAXABLE_HEAD_WIFE_ if couple_educ_gp==1, or // earnings
outreg2 using "$results/psid_cohab_dissolution.xls", sideway stats(coef pval) label ctitle(College - Earn) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve dur i.ft_head i.ft_wife if couple_educ_gp==1, or 
outreg2 using "$results/psid_cohab_dissolution.xls", sideway stats(coef pval) label ctitle(College - Employ) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve dur female_earn_pct if couple_educ_gp==1, or
outreg2 using "$results/psid_cohab_dissolution.xls", sideway stats(coef pval) label ctitle(College - Female %) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve dur i.hh_earn_type_lag `controls' if couple_educ_gp==1, or
outreg2 using "$results/psid_cohab_dissolution.xls", sideway stats(coef pval) label ctitle(College 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append