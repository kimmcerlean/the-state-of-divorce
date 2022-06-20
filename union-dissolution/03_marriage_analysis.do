********************************************************************************
* Marriage dissolution models
* marriage_analysis.do
* Kim McErlean
********************************************************************************

use "$data_keep\PSID_relationships_post2000.dta", clear

// first drop the other half so female is reference group ("from")
drop if from_ESEX==to_ESEX // need different sex couples
drop if from_ESEX==1

// adding controls (trying to use same as for cohab) - probably need to validate
gen either_enrolled=0
replace either_enrolled = 1 if from_RENROLL==1 | to_RENROLL==1

gen same_race=0
replace same_race=1 if from_race==to_race

gen children=. // from step 6 in cohab
replace children=0 if (from_num_births==0 & to_num_births==0)
replace children=1 if (inlist(from_num_births,1,2) | inlist(to_num_births,1,2))

gen metro=(from_TEHC_METRO==1)

// religion??

local controls "from_TAGE to_TAGE i.from_race i.same_race i.children i.either_enrolled i.metro couple_earnings"

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