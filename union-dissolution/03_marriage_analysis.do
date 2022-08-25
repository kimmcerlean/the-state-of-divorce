********************************************************************************
* Marriage dissolution models
* marriage_analysis.do
* Kim McErlean
********************************************************************************

use "$data_keep\PSID_marriage_recoded_sample.dta", clear // created in 1a - no longer using my original order

gen cohort=.
replace cohort=1 if inrange(rel_start_all,1969,1979)
replace cohort=2 if inrange(rel_start_all,1980,1989)
replace cohort=3 if inrange(rel_start_all,1990,2010)
replace cohort=4 if inrange(rel_start_all,2011,2019)

tab cohort dissolve, row

keep if cohort==3
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

// want to create time-invariant indicator of hh type in first year of marriage (but need to make sure it's year both spouses in hh) - some started in of year gah. use DUR? or rank years and use first rank? (actually is that a better duration?)
browse id survey_yr rel_start_all dur hh_earn_type_bkd
bysort id (survey_yr): egen yr_rank=rank(survey_yr)
gen hh_earn_type_mar = hh_earn_type_bkd if yr_rank==1
bysort id (hh_earn_type_mar): replace hh_earn_type_mar=hh_earn_type_mar[1]
label values hh_earn_type_mar earn_type_bkd

browse id survey_yr rel_start_all yr_rank dur hh_earn_type_bkd hh_earn_type_mar

// okay rolling change in female earn pct - absolute or relative?! absolute for now...
sort id survey_yr
gen female_earn_pct_chg = (female_earn_pct-female_earn_pct[_n-1]) if id==id[_n-1]
browse id survey_yr rel_start_all female_earn_pct female_earn_pct_chg


********************************************************************************
* Models
********************************************************************************
* not adding controls to start, but will include education and total earnings
* Do these need to be COX??

// overall
logit dissolve_lag dur female_earn_pct i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2), or // continuous female earnings percentage
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall - 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

logit dissolve_lag dur i.hh_earnings_3070 i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2), or // earnings - bucketed
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall - 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur female_hours_pct i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2), or //  continuous paid hours
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall - 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_hours_3070 i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2), or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall - 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur wife_housework_pct i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2), or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall - 5) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.housework_bkt i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2), or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall - 6) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.division_labor i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2), or // combined division of labor
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall - 7) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur c.female_hours_pct##c.wife_housework_pct i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2), or // testing interactions - 1 (continuous)
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall - 8) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_hours_3070##i.housework_bkt i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2), or // testing interactions - 2 (bucketed)
margins hh_hours_3070#housework_bkt
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall - 9) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur female_hours_pct wife_housework_pct TAXABLE_HEAD_WIFE_ i.couple_educ_gp if inlist(IN_UNIT,1,2), or // continuous paid and unpaid hours


// no college
logit dissolve_lag dur female_earn_pct i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & couple_educ_gp==0, or // continuous female earnings percentage
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College - 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_earnings_3070 i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2)  & couple_educ_gp==0, or // earnings - bucketed
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College - 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur female_hours_pct i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2)  & couple_educ_gp==0, or //  continuous paid hours
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College - 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_hours_3070 i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2)  & couple_educ_gp==0, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College - 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur wife_housework_pct i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2)  & couple_educ_gp==0, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College - 5) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.housework_bkt i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2)  & couple_educ_gp==0, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College - 6) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.division_labor i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2)  & couple_educ_gp==0, or // combined division of labor
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College - 7) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur c.female_hours_pct##c.wife_housework_pct i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & couple_educ_gp==0, or // testing interactions - 1 (continuous)
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College - 8) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_hours_3070##i.housework_bkt i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & couple_educ_gp==0, or // testing interactions - 2 (bucketed)
margins hh_hours_3070#housework_bkt
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College - 9) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur female_hours_pct wife_housework_pct TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & couple_educ_gp==0, or // continuous paid and unpaid hours

// college
logit dissolve_lag dur female_earn_pct i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & couple_educ_gp==1, or // continuous female earnings percentage
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College - 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_earnings_3070 i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2)  & couple_educ_gp==1, or // earnings - bucketed
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College - 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur female_hours_pct i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2)  & couple_educ_gp==1, or //  continuous paid hours
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College - 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_hours_3070 i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2)  & couple_educ_gp==1, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College - 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur wife_housework_pct i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2)  & couple_educ_gp==1, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College - 5) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.housework_bkt i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2)  & couple_educ_gp==1, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College - 6) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.division_labor i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2)  & couple_educ_gp==1, or // combined division of labor
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College - 7) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur c.female_hours_pct##c.wife_housework_pct i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & couple_educ_gp==1, or // testing interactions - 1 (continuous)
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College - 8) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_hours_3070##i.housework_bkt i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & couple_educ_gp==1, or // testing interactions - 2 (bucketed)
margins hh_hours_3070#housework_bkt
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College - 9) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur female_hours_pct wife_housework_pct TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & couple_educ_gp==1, or // continuous paid and unpaid hours

/* for proposal
local controls "i.race_head i.same_race i.children i.either_enrolled TAXABLE_HEAD_WIFE_ i.religion_head age_mar_head age_mar_wife"

// overall
logit dissolve_lag dur i.hh_earn_type_bkd i.couple_educ_gp if inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall - 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

logit dissolve_lag dur i.hh_earn_type_bkd i.couple_educ_gp `controls' if inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_earn_type_mar i.couple_educ_gp if inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall - 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_earn_type_mar i.couple_educ_gp `controls' if inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ib2.ft_pt_head i.ft_pt_wife i.couple_educ_gp if inlist(IN_UNIT,1,2), or // employment
logit dissolve_lag dur i.ft_head i.ft_wife i.couple_educ_gp if inlist(IN_UNIT,1,2), or // employment
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall - 5) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ib2.ft_pt_head i.ft_pt_wife i.couple_educ_gp `controls' if inlist(IN_UNIT,1,2), or // employment
logit dissolve_lag dur i.ft_head i.ft_wife `controls' i.couple_educ_gp if inlist(IN_UNIT,1,2), or // employment
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall - 6) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ratio1 ratio2 i.couple_educ_gp if inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall - 7) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ratio1 ratio2 i.couple_educ_gp `controls' if inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall - 8) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur TAXABLE_HEAD_WIFE_ i.couple_educ_gp if inlist(IN_UNIT,1,2), or // total earnings
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall - Earnings) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur female_earn_pct i.couple_educ_gp if inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall - Female %) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur female_earn_pct_chg i.couple_educ_gp if inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall - Female % Chg) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur wife_housework_pct i.couple_educ_gp if inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall - HW %) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.housework_bkt_lag i.couple_educ_gp if inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Overall - HW Group) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append


// no college
logit dissolve_lag dur i.hh_earn_type_bkd if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_earn_type_bkd `controls' if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_earn_type_mar if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_earn_type_mar `controls' if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ib2.ft_pt_head i.ft_pt_wife if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or 
logit dissolve_lag dur i.ft_head i.ft_wife if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or 
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College - 5) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ib2.ft_pt_head i.ft_pt_wife `controls' if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or 
logit dissolve_lag dur i.ft_head i.ft_wife `controls' if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or 
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College - 6) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ratio1 ratio2 if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or 
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College - 7) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ratio1 ratio2 `controls' if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or 
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College - 8) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur TAXABLE_HEAD_WIFE_ if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or // earnings
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College - Earn) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur female_earn_pct if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College - Female %) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur female_earn_pct_chg if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College - Female % Chg) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur wife_housework_pct if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College - HW %) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.housework_bkt_lag if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(No College - HW Group) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// college
logit dissolve_lag dur i.hh_earn_type_bkd if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_earn_type_bkd `controls' if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_earn_type_mar if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_earn_type_mar `controls' if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ib2.ft_pt_head i.ft_pt_wife if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or 
logit dissolve_lag dur i.ft_head i.ft_wife if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or 
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College - 5) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ib2.ft_pt_head i.ft_pt_wife `controls' if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or 
logit dissolve_lag dur i.ft_head i.ft_wife `controls' if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or 
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College - 6) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ratio1 ratio2 if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or 
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College - 7) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ratio1 ratio2 `controls' if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or 
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College - 8) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur TAXABLE_HEAD_WIFE_ if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or // earnings
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College - Earn) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur female_earn_pct if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College - Female %) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur female_earn_pct_chg if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College - Female % Chg) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur wife_housework_pct if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College - HW %) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.housework_bkt_lag if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(College - HW Group) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
*/

tab hh_earn_type_bkd, sum(TAXABLE_HEAD_WIFE_)

// margins for figure
local controls "i.race_head i.same_race i.children i.either_enrolled TAXABLE_HEAD_WIFE_ i.religion_head age_mar_head age_mar_wife"
logit dissolve_lag dur i.hh_earn_type_bkd `controls' if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or
margins hh_earn_type_bkd

logit dissolve_lag dur i.hh_earn_type_bkd `controls' if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or
margins hh_earn_type_bkd

********************************************************************************
*Quick descriptives for proposal
********************************************************************************
// total
tab couple_educ_gp
unique id, by(couple_educ_gp)

tab couple_educ_gp hh_earn_type_bkd, row
tab couple_educ_gp hh_earn_type_mar, row
tabstat female_earn_pct, by(couple_educ_gp)
tab couple_educ_gp ft_head, row
tab couple_educ_gp ft_wife, row
tabstat wife_housework_pct, by(couple_educ_gp)
tabstat TAXABLE_HEAD_WIFE_, by(couple_educ_gp) stat(mean p50)
 
// dissolved
tab couple_educ_gp if dissolve_lag==1
unique id if dissolve_lag==1, by(couple_educ_gp)

tab couple_educ_gp hh_earn_type_bkd if dissolve_lag==1, row
tab couple_educ_gp hh_earn_type_mar if dissolve_lag==1, row
tabstat female_earn_pct if dissolve_lag==1, by(couple_educ_gp)
tab couple_educ_gp ft_head if dissolve_lag==1, row
tab couple_educ_gp ft_wife if dissolve_lag==1, row
tabstat wife_housework_pct if dissolve_lag==1, by(couple_educ_gp)
tabstat TAXABLE_HEAD_WIFE_ if dissolve_lag==1, by(couple_educ_gp) stat(mean p50)

// pie chart
tab couple_educ_gp hh_earn_type_bkd if dissolve_lag==1, row nofreq
tab couple_educ_gp hh_earn_type_bkd if dissolve_lag==0, row nofreq // intact for ref
 

********************************************************************************
* Historical for reference: 1970
********************************************************************************

use "$data_keep\PSID_marriage_recoded_sample.dta", clear // created in 1a - no longer using my original order

gen cohort=.
replace cohort=1 if inrange(rel_start_all,1969,1979)
replace cohort=2 if inrange(rel_start_all,1980,1989)
replace cohort=3 if inrange(rel_start_all,1990,2010)
replace cohort=4 if inrange(rel_start_all,2011,2019)

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


// want to create time-invariant indicator of hh type in first year of marriage (but need to make sure it's year both spouses in hh) - some started in of year gah. use DUR? or rank years and use first rank? (actually is that a better duration?)
browse id survey_yr rel_start_all dur hh_earn_type_bkd
bysort id (survey_yr): egen yr_rank=rank(survey_yr)
gen hh_earn_type_mar = hh_earn_type_bkd if yr_rank==1
bysort id (hh_earn_type_mar): replace hh_earn_type_mar=hh_earn_type_mar[1]
label values hh_earn_type_mar earn_type_bkd

browse id survey_yr rel_start_all yr_rank dur hh_earn_type_bkd hh_earn_type_mar

// okay rolling change in female earn pct - absolute or relative?! absolute for now...
sort id survey_yr
gen female_earn_pct_chg = (female_earn_pct-female_earn_pct[_n-1]) if id==id[_n-1]
browse id survey_yr rel_start_all female_earn_pct female_earn_pct_chg

// overall
logit dissolve_lag dur female_earn_pct i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2), or // continuous female earnings percentage
outreg2 using "$results/psid_marriage_dissolution_1970.xls", sideway stats(coef pval) label ctitle(Overall - 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

logit dissolve_lag dur i.hh_earnings_3070 i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2), or // earnings - bucketed
outreg2 using "$results/psid_marriage_dissolution_1970.xls", sideway stats(coef pval) label ctitle(Overall - 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur female_hours_pct i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2), or //  continuous paid hours
outreg2 using "$results/psid_marriage_dissolution_1970.xls", sideway stats(coef pval) label ctitle(Overall - 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_hours_3070 i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2), or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_1970.xls", sideway stats(coef pval) label ctitle(Overall - 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur wife_housework_pct i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2), or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_1970.xls", sideway stats(coef pval) label ctitle(Overall - 5) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.housework_bkt i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2), or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_1970.xls", sideway stats(coef pval) label ctitle(Overall - 6) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.division_labor i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2), or // combined division of labor
outreg2 using "$results/psid_marriage_dissolution_1970.xls", sideway stats(coef pval) label ctitle(Overall - 7) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur c.female_hours_pct##c.wife_housework_pct i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2), or // testing interactions - 1 (continuous)
outreg2 using "$results/psid_marriage_dissolution_1970.xls", sideway stats(coef pval) label ctitle(Overall - 8) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_hours_3070##i.housework_bkt i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2), or // testing interactions - 2 (bucketed)
margins hh_hours_3070#housework_bkt
outreg2 using "$results/psid_marriage_dissolution_1970.xls", sideway stats(coef pval) label ctitle(Overall - 9) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

*all in same model
logit dissolve_lag dur i.hh_earnings_3070 i.housework_bkt i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2), or // bucketed paid and unpaid hours

// no college
logit dissolve_lag dur female_earn_pct  TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & couple_educ_gp==0, or // continuous female earnings percentage
outreg2 using "$results/psid_marriage_dissolution_1970.xls", sideway stats(coef pval) label ctitle(No College - 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_earnings_3070  TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2)  & couple_educ_gp==0, or // earnings - bucketed
outreg2 using "$results/psid_marriage_dissolution_1970.xls", sideway stats(coef pval) label ctitle(No College - 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur female_hours_pct  TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2)  & couple_educ_gp==0, or //  continuous paid hours
outreg2 using "$results/psid_marriage_dissolution_1970.xls", sideway stats(coef pval) label ctitle(No College - 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_hours_3070  TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2)  & couple_educ_gp==0, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_1970.xls", sideway stats(coef pval) label ctitle(No College - 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur wife_housework_pct  TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2)  & couple_educ_gp==0, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_1970.xls", sideway stats(coef pval) label ctitle(No College - 5) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.housework_bkt  TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2)  & couple_educ_gp==0, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_1970.xls", sideway stats(coef pval) label ctitle(No College - 6) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.division_labor  TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2)  & couple_educ_gp==0, or // combined division of labor
outreg2 using "$results/psid_marriage_dissolution_1970.xls", sideway stats(coef pval) label ctitle(No College - 7) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur c.female_hours_pct##c.wife_housework_pct  TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & couple_educ_gp==0, or // testing interactions - 1 (continuous)
outreg2 using "$results/psid_marriage_dissolution_1970.xls", sideway stats(coef pval) label ctitle(No College - 8) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_hours_3070##i.housework_bkt  TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & couple_educ_gp==0, or // testing interactions - 2 (bucketed)
margins hh_hours_3070#housework_bkt
outreg2 using "$results/psid_marriage_dissolution_1970.xls", sideway stats(coef pval) label ctitle(No College - 9) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_earnings_3070 i.housework_bkt TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & couple_educ_gp==0, or // bucketed paid and unpaid hours
logit dissolve_lag dur female_hours_pct wife_housework_pct TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & couple_educ_gp==0, or // bucketed paid and unpaid hours

// college
logit dissolve_lag dur female_earn_pct  TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & couple_educ_gp==1, or // continuous female earnings percentage
outreg2 using "$results/psid_marriage_dissolution_1970.xls", sideway stats(coef pval) label ctitle(College - 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_earnings_3070  TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2)  & couple_educ_gp==1, or // earnings - bucketed
outreg2 using "$results/psid_marriage_dissolution_1970.xls", sideway stats(coef pval) label ctitle(College - 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur female_hours_pct  TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2)  & couple_educ_gp==1, or //  continuous paid hours
outreg2 using "$results/psid_marriage_dissolution_1970.xls", sideway stats(coef pval) label ctitle(College - 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_hours_3070  TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2)  & couple_educ_gp==1, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_1970.xls", sideway stats(coef pval) label ctitle(College - 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur wife_housework_pct  TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2)  & couple_educ_gp==1, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_1970.xls", sideway stats(coef pval) label ctitle(College - 5) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.housework_bkt  TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2)  & couple_educ_gp==1, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_1970.xls", sideway stats(coef pval) label ctitle(College - 6) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.division_labor  TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2)  & couple_educ_gp==1, or // combined division of labor
outreg2 using "$results/psid_marriage_dissolution_1970.xls", sideway stats(coef pval) label ctitle(College - 7) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur c.female_hours_pct##c.wife_housework_pct  TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & couple_educ_gp==1, or // testing interactions - 1 (continuous)
outreg2 using "$results/psid_marriage_dissolution_1970.xls", sideway stats(coef pval) label ctitle(College - 8) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_hours_3070##i.housework_bkt  TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & couple_educ_gp==1, or // testing interactions - 2 (bucketed)
margins hh_hours_3070#housework_bkt
outreg2 using "$results/psid_marriage_dissolution_1970.xls", sideway stats(coef pval) label ctitle(College - 9) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur female_hours_pct wife_housework_pct TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & couple_educ_gp==1, or // bucketed paid and unpaid hours

********************************************************************************
* Historical for reference: 1980
********************************************************************************

use "$data_keep\PSID_marriage_recoded_sample.dta", clear // created in 1a - no longer using my original order

gen cohort=.
replace cohort=1 if inrange(rel_start_all,1969,1979)
replace cohort=2 if inrange(rel_start_all,1980,1989)
replace cohort=3 if inrange(rel_start_all,1990,2010)
replace cohort=4 if inrange(rel_start_all,2011,2019)

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


// want to create time-invariant indicator of hh type in first year of marriage (but need to make sure it's year both spouses in hh) - some started in of year gah. use DUR? or rank years and use first rank? (actually is that a better duration?)
browse id survey_yr rel_start_all dur hh_earn_type_bkd
bysort id (survey_yr): egen yr_rank=rank(survey_yr)
gen hh_earn_type_mar = hh_earn_type_bkd if yr_rank==1
bysort id (hh_earn_type_mar): replace hh_earn_type_mar=hh_earn_type_mar[1]
label values hh_earn_type_mar earn_type_bkd

browse id survey_yr rel_start_all yr_rank dur hh_earn_type_bkd hh_earn_type_mar

// okay rolling change in female earn pct - absolute or relative?! absolute for now...
sort id survey_yr
gen female_earn_pct_chg = (female_earn_pct-female_earn_pct[_n-1]) if id==id[_n-1]
browse id survey_yr rel_start_all female_earn_pct female_earn_pct_chg

// overall
logit dissolve_lag dur female_earn_pct i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2), or // continuous female earnings percentage
outreg2 using "$results/psid_marriage_dissolution_1980.xls", sideway stats(coef pval) label ctitle(Overall - 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

logit dissolve_lag dur i.hh_earnings_3070 i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2), or // earnings - bucketed
outreg2 using "$results/psid_marriage_dissolution_1980.xls", sideway stats(coef pval) label ctitle(Overall - 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur female_hours_pct i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2), or //  continuous paid hours
outreg2 using "$results/psid_marriage_dissolution_1980.xls", sideway stats(coef pval) label ctitle(Overall - 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_hours_3070 i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2), or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_1980.xls", sideway stats(coef pval) label ctitle(Overall - 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur wife_housework_pct i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2), or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_1980.xls", sideway stats(coef pval) label ctitle(Overall - 5) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.housework_bkt i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2), or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_1980.xls", sideway stats(coef pval) label ctitle(Overall - 6) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.division_labor i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2), or // combined division of labor
outreg2 using "$results/psid_marriage_dissolution_1980.xls", sideway stats(coef pval) label ctitle(Overall - 7) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur c.female_hours_pct##c.wife_housework_pct i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2), or // testing interactions - 1 (continuous)
outreg2 using "$results/psid_marriage_dissolution_1980.xls", sideway stats(coef pval) label ctitle(Overall - 8) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_hours_3070##i.housework_bkt i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2), or // testing interactions - 2 (bucketed)
margins hh_hours_3070#housework_bkt
outreg2 using "$results/psid_marriage_dissolution_1980.xls", sideway stats(coef pval) label ctitle(Overall - 9) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur female_hours_pct wife_housework_pct TAXABLE_HEAD_WIFE_ i.couple_educ_gp if inlist(IN_UNIT,1,2), or // continuous paid and unpaid hours

// no college
logit dissolve_lag dur female_earn_pct  TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & couple_educ_gp==0, or // continuous female earnings percentage
outreg2 using "$results/psid_marriage_dissolution_1980.xls", sideway stats(coef pval) label ctitle(No College - 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_earnings_3070  TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2)  & couple_educ_gp==0, or // earnings - bucketed
outreg2 using "$results/psid_marriage_dissolution_1980.xls", sideway stats(coef pval) label ctitle(No College - 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur female_hours_pct  TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2)  & couple_educ_gp==0, or //  continuous paid hours
outreg2 using "$results/psid_marriage_dissolution_1980.xls", sideway stats(coef pval) label ctitle(No College - 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_hours_3070  TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2)  & couple_educ_gp==0, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_1980.xls", sideway stats(coef pval) label ctitle(No College - 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur wife_housework_pct  TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2)  & couple_educ_gp==0, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_1980.xls", sideway stats(coef pval) label ctitle(No College - 5) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.housework_bkt  TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2)  & couple_educ_gp==0, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_1980.xls", sideway stats(coef pval) label ctitle(No College - 6) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.division_labor  TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2)  & couple_educ_gp==0, or // combined division of labor
outreg2 using "$results/psid_marriage_dissolution_1980.xls", sideway stats(coef pval) label ctitle(No College - 7) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur c.female_hours_pct##c.wife_housework_pct  TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & couple_educ_gp==0, or // testing interactions - 1 (continuous)
outreg2 using "$results/psid_marriage_dissolution_1980.xls", sideway stats(coef pval) label ctitle(No College - 8) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_hours_3070##i.housework_bkt  TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & couple_educ_gp==0, or // testing interactions - 2 (bucketed)
margins hh_hours_3070#housework_bkt
outreg2 using "$results/psid_marriage_dissolution_1980.xls", sideway stats(coef pval) label ctitle(No College - 9) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur female_hours_pct wife_housework_pct TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & couple_educ_gp==0, or // continuous paid and unpaid hours


// college
logit dissolve_lag dur female_earn_pct TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & couple_educ_gp==1, or // continuous female earnings percentage
outreg2 using "$results/psid_marriage_dissolution_1980.xls", sideway stats(coef pval) label ctitle(College - 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_earnings_3070 TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2)  & couple_educ_gp==1, or // earnings - bucketed
outreg2 using "$results/psid_marriage_dissolution_1980.xls", sideway stats(coef pval) label ctitle(College - 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur female_hours_pct TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2)  & couple_educ_gp==1, or //  continuous paid hours
outreg2 using "$results/psid_marriage_dissolution_1980.xls", sideway stats(coef pval) label ctitle(College - 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_hours_3070 TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2)  & couple_educ_gp==1, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_1980.xls", sideway stats(coef pval) label ctitle(College - 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur wife_housework_pct TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2)  & couple_educ_gp==1, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_1980.xls", sideway stats(coef pval) label ctitle(College - 5) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.housework_bkt TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2)  & couple_educ_gp==1, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_1980.xls", sideway stats(coef pval) label ctitle(College - 6) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.division_labor TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2)  & couple_educ_gp==1, or // combined division of labor
outreg2 using "$results/psid_marriage_dissolution_1980.xls", sideway stats(coef pval) label ctitle(College - 7) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur c.female_hours_pct##c.wife_housework_pct TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & couple_educ_gp==1, or // testing interactions - 1 (continuous)
outreg2 using "$results/psid_marriage_dissolution_1980.xls", sideway stats(coef pval) label ctitle(College - 8) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_hours_3070##i.housework_bkt TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & couple_educ_gp==1, or // testing interactions - 2 (bucketed)
margins hh_hours_3070#housework_bkt
outreg2 using "$results/psid_marriage_dissolution_1980.xls", sideway stats(coef pval) label ctitle(College - 9) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur female_hours_pct wife_housework_pct TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & couple_educ_gp==1, or // continuous paid and unpaid hours

/* Proposal: 1970-1990

local controls "i.race_head i.same_race i.children i.either_enrolled TAXABLE_HEAD_WIFE_ i.religion_head age_mar_head age_mar_wife"

// overall
logit dissolve_lag dur i.hh_earn_type_bkd i.couple_educ_gp if inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(Overall - 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

logit dissolve_lag dur i.hh_earn_type_bkd i.couple_educ_gp `controls' if inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(Overall 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_earn_type_mar i.couple_educ_gp if inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(Overall - 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_earn_type_mar i.couple_educ_gp `controls' if inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(Overall 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ib2.ft_pt_head i.ft_pt_wife i.couple_educ_gp if inlist(IN_UNIT,1,2), or // employment
logit dissolve_lag dur i.ft_head i.ft_wife i.couple_educ_gp if inlist(IN_UNIT,1,2), or // employment
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(Overall - 5) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ib2.ft_pt_head i.ft_pt_wife i.couple_educ_gp `controls' if inlist(IN_UNIT,1,2), or // employment
logit dissolve_lag dur i.ft_head i.ft_wife `controls' i.couple_educ_gp if inlist(IN_UNIT,1,2), or // employment
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(Overall - 6) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ratio1 ratio2 i.couple_educ_gp if inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(Overall - 7) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ratio1 ratio2 i.couple_educ_gp `controls' if inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(Overall - 8) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur TAXABLE_HEAD_WIFE_ i.couple_educ_gp if inlist(IN_UNIT,1,2), or // total earnings
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(Overall - Earnings) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur female_earn_pct i.couple_educ_gp if inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(Overall - Female %) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur female_earn_pct_chg i.couple_educ_gp if inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(Overall - Female % Chg) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur wife_housework_pct i.couple_educ_gp if inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(Overall - HW %) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.housework_bkt_lag i.couple_educ_gp if inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(Overall - HW Group) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append


// no college
logit dissolve_lag dur i.hh_earn_type_bkd if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(No College 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_earn_type_bkd `controls' if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(No College 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_earn_type_mar if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(No College 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_earn_type_mar `controls' if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(No College 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ib2.ft_pt_head i.ft_pt_wife if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or 
logit dissolve_lag dur i.ft_head i.ft_wife if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or 
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(No College - 5) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ib2.ft_pt_head i.ft_pt_wife `controls' if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or 
logit dissolve_lag dur i.ft_head i.ft_wife `controls' if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or 
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(No College - 6) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ratio1 ratio2 if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or 
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(No College - 7) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ratio1 ratio2 `controls' if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or 
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(No College - 8) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur TAXABLE_HEAD_WIFE_ if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or // earnings
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(No College - Earn) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur female_earn_pct if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(No College - Female %) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur female_earn_pct_chg if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(No College - Female % Chg) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur wife_housework_pct if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(No College - HW %) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.housework_bkt_lag if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(No College - HW Group) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// college
logit dissolve_lag dur i.hh_earn_type_bkd if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(College 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_earn_type_bkd `controls' if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(College 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_earn_type_mar if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(College 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.hh_earn_type_mar `controls' if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(College 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ib2.ft_pt_head i.ft_pt_wife if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or 
logit dissolve_lag dur i.ft_head i.ft_wife if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or 
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(College - 5) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ib2.ft_pt_head i.ft_pt_wife `controls' if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or 
logit dissolve_lag dur i.ft_head i.ft_wife `controls' if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or 
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(College - 6) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ratio1 ratio2 if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or 
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(College - 7) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur ratio1 ratio2 `controls' if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or 
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(College - 8) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur TAXABLE_HEAD_WIFE_ if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or // earnings
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(College - Earn) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur female_earn_pct if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(College - Female %) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur female_earn_pct_chg if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(College - Female % Chg) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur wife_housework_pct if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(College - HW %) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag dur i.housework_bkt_lag if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or
outreg2 using "$results/psid_marriage_dissolution_pre.xls", sideway stats(coef pval) label ctitle(College - HW Group) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
*/

********************************************************************************
* Year interactions
********************************************************************************

use "$data_keep\PSID_marriage_recoded_sample.dta", clear // created in 1a - no longer using my original order

gen cohort=.
replace cohort=1 if inrange(rel_start_all,1969,1979)
replace cohort=2 if inrange(rel_start_all,1980,1989)
replace cohort=3 if inrange(rel_start_all,1990,2010)
replace cohort=4 if inrange(rel_start_all,2011,2019)

tab cohort dissolve, row

drop if cohort==4
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

// want to create time-invariant indicator of hh type in first year of marriage (but need to make sure it's year both spouses in hh) - some started in of year gah. use DUR? or rank years and use first rank? (actually is that a better duration?)
browse id survey_yr rel_start_all dur hh_earn_type_bkd
bysort id (survey_yr): egen yr_rank=rank(survey_yr)
gen hh_earn_type_mar = hh_earn_type_bkd if yr_rank==1
bysort id (hh_earn_type_mar): replace hh_earn_type_mar=hh_earn_type_mar[1]
label values hh_earn_type_mar earn_type_bkd

browse id survey_yr rel_start_all yr_rank dur hh_earn_type_bkd hh_earn_type_mar

drop if hours_housework==8 // no earners; skewing, especially for college

// validate
logit dissolve_lag dur i.hh_hours_3070##i.housework_bkt TAXABLE_HEAD_WIFE_ i.couple_educ_gp if inlist(IN_UNIT,1,2) & cohort==3, or
logit dissolve_lag dur i.hours_housework TAXABLE_HEAD_WIFE_ i.couple_educ_gp if inlist(IN_UNIT,1,2) & cohort==3, or
logit dissolve_lag dur ib4.hours_housework TAXABLE_HEAD_WIFE_ i.couple_educ_gp if inlist(IN_UNIT,1,2) & cohort==3, or

logit dissolve_lag dur i.cohort##ib4.hours_housework TAXABLE_HEAD_WIFE_ i.couple_educ_gp if inlist(IN_UNIT,1,2), or
margins cohort#hours_housework
marginsplot

logit dissolve_lag dur i.cohort##ib4.hours_housework TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & couple_educ_gp==0, or
margins cohort#hours_housework
marginsplot

logit dissolve_lag dur i.cohort##ib4.hours_housework TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & couple_educ_gp==1, or
margins cohort#hours_housework
marginsplot

// want coefficients for each year
logit dissolve_lag dur ib4.hours_housework TAXABLE_HEAD_WIFE_ i.couple_educ_gp if inlist(IN_UNIT,1,2) & cohort==1, or
logit dissolve_lag dur ib4.hours_housework TAXABLE_HEAD_WIFE_ i.couple_educ_gp if inlist(IN_UNIT,1,2) & cohort==2, or
logit dissolve_lag dur ib4.hours_housework TAXABLE_HEAD_WIFE_ i.couple_educ_gp if inlist(IN_UNIT,1,2) & cohort==3, or

logit dissolve_lag dur ib4.hours_housework TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==1 & couple_educ_gp==0, or
logit dissolve_lag dur ib4.hours_housework TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==2 & couple_educ_gp==0, or
logit dissolve_lag dur ib4.hours_housework TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or

logit dissolve_lag dur ib4.hours_housework TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==1 & couple_educ_gp==1, or
logit dissolve_lag dur ib4.hours_housework TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==2 & couple_educ_gp==1, or
logit dissolve_lag dur ib4.hours_housework TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or

recode hours_housework (1=3) (2=2) (3=5) (4=1) (5=5) (6=4) (7=5), gen(config)
label define config 1 "Conventional" 2 "Neotraditional" 3 "Egal" 4 "Doing gender" 5 "Gender-atypical"
label values config config

// want coefficients for each year
logit dissolve_lag dur i.config TAXABLE_HEAD_WIFE_ i.couple_educ_gp if inlist(IN_UNIT,1,2) & cohort==1, or
logit dissolve_lag dur i.config TAXABLE_HEAD_WIFE_ i.couple_educ_gp if inlist(IN_UNIT,1,2) & cohort==2, or
logit dissolve_lag dur i.config TAXABLE_HEAD_WIFE_ i.couple_educ_gp if inlist(IN_UNIT,1,2) & cohort==3, or

logit dissolve_lag dur i.config TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==1 & couple_educ_gp==0, or
logit dissolve_lag dur i.config TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==2 & couple_educ_gp==0, or
logit dissolve_lag dur i.config TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or

logit dissolve_lag dur i.config TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==1 & couple_educ_gp==1, or
logit dissolve_lag dur i.config TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==2 & couple_educ_gp==1, or
logit dissolve_lag dur i.config TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or

********************************************************************************
*Updated descriptives for proposal revision
********************************************************************************
 // 1970-1979
tab couple_educ_gp if cohort==1
unique id if cohort==1, by(couple_educ_gp) // unique couples
unique id if cohort==1 & dissolve_lag==1, by(couple_educ_gp) // dissolutions

tabstat female_hours_pct  if cohort==1, by(couple_educ_gp)
tab couple_educ_gp hh_hours_3070 if cohort==1, row
tabstat wife_housework_pct if cohort==1, by(couple_educ_gp)
tab couple_educ_gp housework_bkt if cohort==1, row
tabstat TAXABLE_HEAD_WIFE_ if cohort==1, by(couple_educ_gp) stat(mean p50)

 // 1980-1989
tab couple_educ_gp if cohort==2
unique id if cohort==2, by(couple_educ_gp) // unique couples
unique id if cohort==2 & dissolve_lag==1, by(couple_educ_gp) // dissolutions

tabstat female_hours_pct  if cohort==2, by(couple_educ_gp)
tab couple_educ_gp hh_hours_3070 if cohort==2, row
tabstat wife_housework_pct if cohort==2, by(couple_educ_gp)
tab couple_educ_gp housework_bkt if cohort==2, row
tabstat TAXABLE_HEAD_WIFE_ if cohort==2, by(couple_educ_gp) stat(mean p50)

 // 1990-2010
tab couple_educ_gp if cohort==3
unique id if cohort==3, by(couple_educ_gp) // unique couples
unique id if cohort==3 & dissolve_lag==1, by(couple_educ_gp) // dissolutions

tabstat female_hours_pct  if cohort==3, by(couple_educ_gp)
tab couple_educ_gp hh_hours_3070 if cohort==3, row
tabstat wife_housework_pct if cohort==3, by(couple_educ_gp)
tab couple_educ_gp housework_bkt if cohort==3, row
tabstat TAXABLE_HEAD_WIFE_ if cohort==3, by(couple_educ_gp) stat(mean p50)