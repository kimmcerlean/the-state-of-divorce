********************************************************************************
* Marriage dissolution models
* marriage_analysis.do
* Kim McErlean
********************************************************************************

use "$data_keep\PSID_relationships_post2000.dta", clear

keep if relationship_type==2 // only keep those married - will replicate for cohab in new step

local controls "from_TAGE to_TAGE i.from_race i.same_race i.children i.either_enrolled i.metro couple_earnings i.religion_head age_mar_head age_mar_wife dur"

// overall
logit dissolved dur i.hh_earn_type_lag if cohort==1, or  // S&H did 2000-2004 as most recent cohort. Is that fine?
outreg2 using "$results/marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

logit dissolved dur i.hh_earn_type_lag i.couple_educ_gp if cohort==1, or
outreg2 using "$results/marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolved dur couple_earnings i.couple_educ_gp if cohort==1, or // total earnings
outreg2 using "$results/marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall - Earnings) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolved dur i.to_ft i.from_ft i.couple_educ_gp if cohort==1, or // emmployment
outreg2 using "$results/marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall - Employment) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolved dur female_earn_pct i.couple_educ_gp if cohort==1, or
outreg2 using "$results/marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall - Female %) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolved dur i.hh_earn_type_lag i.couple_educ_gp `controls' if cohort==1, or
outreg2 using "$results/marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// no college
logit dissolved dur i.hh_earn_type_lag if couple_educ_gp==0, or
logit dissolved dur i.hh_earn_type_lag if couple_educ_gp==0 & cohort==1, or
outreg2 using "$results/marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolved dur couple_earnings if couple_educ_gp==0 & cohort==1, or // earnings
outreg2 using "$results/marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College - Earn) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolved dur i.to_ft i.from_ft if couple_educ_gp==0 & cohort==1, or 
outreg2 using "$results/marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College - Employ) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolved dur female_earn_pct if couple_educ_gp==0 & cohort==1, or
outreg2 using "$results/marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College - Female %) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolved dur i.hh_earn_type_lag `controls' if couple_educ_gp==0 & cohort==1, or
outreg2 using "$results/marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// college
logit dissolved dur i.hh_earn_type_lag if couple_educ_gp==1, or
logit dissolved dur i.hh_earn_type_lag if couple_educ_gp==1 & cohort==1, or
outreg2 using "$results/marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolved dur couple_earnings if couple_educ_gp==1 & cohort==1, or // earnings
outreg2 using "$results/marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College - Earn) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolved dur i.to_ft i.from_ft if couple_educ_gp==1 & cohort==1, or 
outreg2 using "$results/marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College - Employ) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolved dur female_earn_pct if couple_educ_gp==1 & cohort==1, or
outreg2 using "$results/marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College - Female %) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolved dur i.hh_earn_type_lag `controls' if couple_educ_gp==1 & cohort==1, or
outreg2 using "$results/marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append