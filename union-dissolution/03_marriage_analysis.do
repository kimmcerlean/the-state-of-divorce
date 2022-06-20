********************************************************************************
* Marriage dissolution models
* marriage_analysis.do
* Kim McErlean
********************************************************************************

use "$data_keep\PSID_relationships_post2000.dta", clear

keep if relationship_type==2 // only keep those married - will replicate for cohab in new step

local controls "i.race_head i.same_race i.children i.either_enrolled TAXABLE_HEAD_WIFE_ i.religion_head age_mar_head age_mar_wife"

// overall
logit dissolve dur i.hh_earn_type_lag, or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

logit dissolve dur i.hh_earn_type_lag i.couple_educ_gp, or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve dur TAXABLE_HEAD_WIFE_ i.couple_educ_gp, or // total earnings
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall - Earnings) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve dur i.ft_head i.ft_wife i.couple_educ_gp, or // emmployment
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall - Employment) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve dur female_earn_pct i.couple_educ_gp, or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall - Female %) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve dur i.hh_earn_type_lag i.couple_educ_gp `controls', or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// no college
logit dissolve dur i.hh_earn_type_lag if couple_educ_gp==0, or
logit dissolve dur i.hh_earn_type_lag if couple_educ_gp==0, or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve dur TAXABLE_HEAD_WIFE_ if couple_educ_gp==0, or // earnings
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College - Earn) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve dur i.ft_head i.ft_wife if couple_educ_gp==0, or 
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College - Employ) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve dur female_earn_pct if couple_educ_gp==0, or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College - Female %) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve dur i.hh_earn_type_lag `controls' if couple_educ_gp==0, or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// college
logit dissolve dur i.hh_earn_type_lag if couple_educ_gp==1, or
logit dissolve dur i.hh_earn_type_lag if couple_educ_gp==1, or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve dur TAXABLE_HEAD_WIFE_ if couple_educ_gp==1, or // earnings
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College - Earn) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve dur i.ft_head i.ft_wife if couple_educ_gp==1, or 
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College - Employ) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve dur female_earn_pct if couple_educ_gp==1, or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College - Female %) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve dur i.hh_earn_type_lag `controls' if couple_educ_gp==1, or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append