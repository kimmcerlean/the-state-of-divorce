********************************************************************************
* Marriage dissolution models
* state-level-analysis.do
* Kim McErlean
********************************************************************************

use "$data_keep\PSID_marriage_recoded_sample.dta", clear // created in 1a - no longer using my original order

gen cohort=.
replace cohort=1 if inrange(rel_start_all,1969,1979)
replace cohort=2 if inrange(rel_start_all,1980,1989)
replace cohort=3 if inrange(rel_start_all,1990,2010)
replace cohort=4 if inrange(rel_start_all,2011,2019)

tab cohort dissolve, row

// keep if cohort==3, need to just use filters so I don't have to keep using and updating the data
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
* Merge state-level data
********************************************************************************
// save "$data_tmp\PSID_state_data.dta", replace - importing excel
keep if cohort==3
keep if inlist(IN_UNIT,1,2)

rename STATE_ statefip // okay wait a lot of missing - old survey_years
merge m:1 statefip using "$data_tmp\PSID_state_data.dta", keepusing(cost_living	living_wage	leave_policy_score)

drop _merge

// standardize variables
sum leave_policy_score
//gen std_leave_policy_score = (leave_policy_score -`r(min)') / (`r(max)'-`r(min)')
gen center_leave_policy_score = leave_policy_score - `r(mean)'

sum living_wage
gen center_living_wage = living_wage - `r(mean)'

sum cost_living
gen center_cost_living = cost_living - `r(mean)'

********************************************************************************
* Models
********************************************************************************
melogit dissolve_lag i.dur i.hh_hours_3070 || statefip:, or
melogit dissolve_lag i.dur i.hh_hours_3070 if couple_educ_gp==0 || statefip:, or
melogit dissolve_lag i.dur i.hh_hours_3070 if couple_educ_gp==1 || statefip:, or

melogit dissolve_lag i.dur i.ft_wife || statefip:, or
melogit dissolve_lag i.dur i.ft_wife if couple_educ_gp==0 || statefip:, or
melogit dissolve_lag i.dur i.ft_wife if couple_educ_gp==1 || statefip:, or

// no college
melogit dissolve_lag i.dur c.center_cost_living##i.hh_hours_3070 if couple_educ_gp==0 || statefip:, or
margins hh_hours_3070, at(center_cost_living=(-10000 -5000 0 5000 10000)) 
marginsplot

melogit dissolve_lag i.dur c.center_cost_living##i.ft_wife if couple_educ_gp==0 || statefip:, or
margins ft_wife, at(center_cost_living=(-10000 -5000 0 5000 10000)) 
marginsplot

melogit dissolve_lag i.dur c.center_leave_policy_score##i.hh_hours_3070 if couple_educ_gp==0 || statefip:, or
margins hh_hours_3070, at(center_leave_policy_score=(-20 -10 0 10 20 50)) 
marginsplot

melogit dissolve_lag i.dur c.center_leave_policy_score##i.ft_wife if couple_educ_gp==0 || statefip:, or
margins ft_wife, at(center_leave_policy_score=(-20 -10 0 10 20 50)) 
marginsplot

// college
melogit dissolve_lag i.dur c.center_cost_living##i.hh_hours_3070 if couple_educ_gp==1 || statefip:, or
margins hh_hours_3070, at(center_cost_living=(-10000 -5000 0 5000 10000)) 
marginsplot

melogit dissolve_lag i.dur c.center_cost_living##i.ft_wife if couple_educ_gp==1 || statefip:, or
margins ft_wife, at(center_cost_living=(-10000 -5000 0 5000 10000)) 
marginsplot

melogit dissolve_lag i.dur c.center_leave_policy_score##i.hh_hours_3070 if couple_educ_gp==1 & hh_hours_3070<4 || statefip:, or
margins hh_hours_3070, at(center_leave_policy_score=(-20 -10 0 10 20 50)) 
marginsplot

melogit dissolve_lag i.dur c.center_leave_policy_score##i.ft_wife if couple_educ_gp==1 || statefip:, or
margins ft_wife, at(center_leave_policy_score=(-20 -10 0 10 20 50)) 
marginsplot

// control variables: age of marriage (both), race (head + same race), religion (head), region? (head), cohab_with_wife, cohab_with_other, pre_marital_birth, post_marital_birth
// both pre and post marital birth should NOT be in model because they are essentially inverse. do I want to add if they have a child together as new flag?
// taking out religion for now because not asked in 1968 / 1968
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled REGION_ cohab_with_wife cohab_with_other pre_marital_birth"
// outreg2 using "$results/psid_marriage_dissolution_nocoll_1990.xls", sideway stats(coef pval) label ctitle(1 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace