********************************************************************************
* Marriage dissolution models
* marriage_analysis.do
* Kim McErlean
********************************************************************************

use "$data_keep\PSID_marriage_recoded_sample.dta", clear // created in 1a - no longer using my original order

gen cohort=.
replace cohort=1 if inrange(rel_start_all,1969,1989)
replace cohort=2 if inrange(rel_start_all,1990,2010)
replace cohort=3 if inrange(rel_start_all,2011,2019)

tab cohort dissolve, row

keep if cohort==2
// need to decide - ALL MARRIAGES or just first? - killewald restricts to just first, so does cooke. My validation is MUCH BETTER against those with first marraiges only...
keep if marriage_order_real==1
keep if (AGE_REF_>=18 & AGE_REF_<=55) &  (AGE_SPOUSE_>=18 & AGE_SPOUSE_<=55)

// need to make religion
// religion is new, but think I need to add given historical research. coding changes between 1984 and 1985, then again between 1994 and 1995. using past then, so this is fine. otherwise, need to recode in MAIN FILE before combining. okay still somewhat sketchy. coding like this for now, will update in real analysis

label define update_religion  ///
       1 "Catholic"  ///
       2 "Jewish"  ///
       8 "Protestant unspecified"  ///
      10 "Other non-Christian: Muslim, Rastafarian, etc."  ///
      13 "Greek/Russian/Eastern Orthodox"  ///
      97 "Other"  ///
      98 "DK"  ///
      99 "NA; refused"  ///
       0 "None"

recode RELIGION_HEAD_ (3/7=97)(9=97)(11/12=97)(14/31=97), gen(religion_head)
recode RELIGION_WIFE_ (3/7=97)(9=97)(11/12=97)(14/31=97), gen(religion_wife)
	   
label values religion_head religion_wife update_religion

// test spline at 0.5
mkspline ratio1 0.5 ratio2 = female_earn_pct
browse female_earn_pct ratio1 ratio2 

local controls "i.race_head i.same_race i.children i.either_enrolled TAXABLE_HEAD_WIFE_ i.religion_head age_mar_head age_mar_wife"

// overall
logit dissolve_lag dur i.hh_earn_type_bkd i.couple_educ_gp if inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall - 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

logit dissolve_lag dur i.hh_earn_type_bkd i.couple_educ_gp `controls' if inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.ft_head i.ft_wife i.couple_educ_gp if inlist(IN_UNIT,1,2), or // employment
logit dissolve_lag dur ib2.ft_pt_head i.ft_pt_wife i.couple_educ_gp if inlist(IN_UNIT,1,2), or // employment
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall - 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ib2.ft_pt_head i.ft_pt_wife i.couple_educ_gp `controls' if inlist(IN_UNIT,1,2), or // employment
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall - 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ratio1 ratio2 i.couple_educ_gp if inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall - 5) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ratio1 ratio2 i.couple_educ_gp `controls' if inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall - 6) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur TAXABLE_HEAD_WIFE_ i.couple_educ_gp if inlist(IN_UNIT,1,2), or // total earnings
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall - Earnings) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur female_earn_pct i.couple_educ_gp if inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall - Female %) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append


// no college
logit dissolve_lag dur i.hh_earn_type_bkd if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_earn_type_bkd `controls' if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.ft_head i.ft_wife if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or 
logit dissolve_lag dur ib2.ft_pt_head i.ft_pt_wife if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or 
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College - 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ib2.ft_pt_head i.ft_pt_wife `controls' if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or 
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College - 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ratio1 ratio2 if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or 
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College - 5) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ratio1 ratio2 `controls' if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or 
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College - 6) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur TAXABLE_HEAD_WIFE_ if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or // earnings
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College - Earn) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur female_earn_pct if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College - Female %) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append


// college
logit dissolve_lag dur i.hh_earn_type_bkd if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_earn_type_bkd `controls' if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.ft_head i.ft_wife if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or 
logit dissolve_lag dur ib2.ft_pt_head i.ft_pt_wife if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or 
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College - 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ib2.ft_pt_head i.ft_pt_wife `controls' if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or 
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College - 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ratio1 ratio2 if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or 
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College - 5) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ratio1 ratio2 `controls' if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or 
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College - 6) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur TAXABLE_HEAD_WIFE_ if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or // earnings
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College - Earn) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur female_earn_pct if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College - Female %) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

tab hh_earn_type_bkd, sum(TAXABLE_HEAD_WIFE_)

********************************************************************************
* Historical for reference
********************************************************************************

use "$data_keep\PSID_marriage_recoded_sample.dta", clear // created in 1a - no longer using my original order

gen cohort=.
replace cohort=1 if inrange(rel_start_all,1969,1989)
replace cohort=2 if inrange(rel_start_all,1990,2010)
replace cohort=3 if inrange(rel_start_all,2011,2019)

tab cohort dissolve, row

keep if cohort==1
// need to decide - ALL MARRIAGES or just first? - killewald restricts to just first, so does cooke. My validation is MUCH BETTER against those with first marraiges only...
keep if marriage_order_real==1
keep if (AGE_REF_>=18 & AGE_REF_<=55) &  (AGE_SPOUSE_>=18 & AGE_SPOUSE_<=55)

// need to make religion
// religion is new, but think I need to add given historical research. coding changes between 1984 and 1985, then again between 1994 and 1995. using past then, so this is fine. otherwise, need to recode in MAIN FILE before combining. okay still somewhat sketchy. coding like this for now, will update in real analysis

label define update_religion  ///
       1 "Catholic"  ///
       2 "Jewish"  ///
       8 "Protestant unspecified"  ///
      10 "Other non-Christian: Muslim, Rastafarian, etc."  ///
      13 "Greek/Russian/Eastern Orthodox"  ///
      97 "Other"  ///
      98 "DK"  ///
      99 "NA; refused"  ///
       0 "None"

recode RELIGION_HEAD_ (3/7=97)(9=97)(11/12=97)(14/31=97), gen(religion_head)
recode RELIGION_WIFE_ (3/7=97)(9=97)(11/12=97)(14/31=97), gen(religion_wife)
	   
label values religion_head religion_wife update_religion

// test spline at 0.5
mkspline ratio1 0.5 ratio2 = female_earn_pct
browse female_earn_pct ratio1 ratio2 

local controls "i.race_head i.same_race i.children i.either_enrolled TAXABLE_HEAD_WIFE_ i.religion_head age_mar_head age_mar_wife"

// overall
logit dissolve_lag dur i.hh_earn_type_bkd i.couple_educ_gp if inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(Overall - 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

logit dissolve_lag dur i.hh_earn_type_bkd i.couple_educ_gp `controls' if inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(Overall 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.ft_head i.ft_wife i.couple_educ_gp if inlist(IN_UNIT,1,2), or // employment
logit dissolve_lag dur ib2.ft_pt_head i.ft_pt_wife i.couple_educ_gp if inlist(IN_UNIT,1,2), or // employment
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(Overall - 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ib2.ft_pt_head i.ft_pt_wife i.couple_educ_gp `controls' if inlist(IN_UNIT,1,2), or // employment
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(Overall - 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ratio1 ratio2 i.couple_educ_gp if inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(Overall - 5) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ratio1 ratio2 i.couple_educ_gp `controls' if inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(Overall - 6) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur TAXABLE_HEAD_WIFE_ i.couple_educ_gp if inlist(IN_UNIT,1,2), or // total earnings
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(Overall - Earnings) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur female_earn_pct i.couple_educ_gp if inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(Overall - Female %) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append


// no college
logit dissolve_lag dur i.hh_earn_type_bkd if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(No College 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_earn_type_bkd `controls' if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(No College 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.ft_head i.ft_wife if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or 
logit dissolve_lag dur ib2.ft_pt_head i.ft_pt_wife if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or 
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(No College - 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ib2.ft_pt_head i.ft_pt_wife `controls' if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or 
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(No College - 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ratio1 ratio2 if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or 
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(No College - 5) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ratio1 ratio2 `controls' if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or 
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(No College - 6) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur TAXABLE_HEAD_WIFE_ if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or // earnings
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(No College - Earn) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur female_earn_pct if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(No College - Female %) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append


// college
logit dissolve_lag dur i.hh_earn_type_bkd if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(College 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_earn_type_bkd `controls' if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(College 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.ft_head i.ft_wife if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or 
logit dissolve_lag dur ib2.ft_pt_head i.ft_pt_wife if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or 
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(College - 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ib2.ft_pt_head i.ft_pt_wife `controls' if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or 
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(College - 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ratio1 ratio2 if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or 
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(College - 5) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ratio1 ratio2 `controls' if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or 
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(College - 6) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur TAXABLE_HEAD_WIFE_ if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or // earnings
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(College - Earn) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur female_earn_pct if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(College - Female %) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append