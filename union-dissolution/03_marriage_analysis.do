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

gen cohort_alt=.
replace cohort_alt=1 if inrange(rel_start_all,1969,1979)
replace cohort_alt=2 if inrange(rel_start_all,1980,1989)
replace cohort_alt=3 if inrange(rel_start_all,1990,1999)
replace cohort_alt=4 if inrange(rel_start_all,2000,2014)

label define cohort 1 "pre-1980s" 2 "1980s" 3 "1990s" 4 "2000s"
label value cohort_alt cohort

tab cohort_alt dissolve, row

gen cohort_v2=.
replace cohort_v2=0 if inrange(rel_start_all,1969,1989)
replace cohort_v2=1 if inrange(rel_start_all,1990,2014)

// keep if cohort==3, need to just use filters so I don't have to keep using and updating the data
// need to decide - ALL MARRIAGES or just first? - killewald restricts to just first, so does cooke. My validation is MUCH BETTER against those with first marraiges only...
keep if marriage_order_real==1
keep if (AGE_REF_>=18 & AGE_REF_<=55) &  (AGE_SPOUSE_>=18 & AGE_SPOUSE_<=55)

// drop those with no earnings or housework hours the whole time
bysort id: egen min_type = min(hh_earn_type) // since no earners is 4, if the minimum is 4, means that was it the whole time
label values min_type hh_earn_type
sort id survey_yr
browse id survey_yr min_type hh_earn_type

tab min_type // okay very few people had no earnings whole time
drop if min_type ==4

bysort id: egen min_hw_type = min(housework_bkt) // since no earners is 4, if the minimum is 4, means that was it the whole time
label values min_hw_type housework_bkt
sort id survey_yr
browse id survey_yr min_hw_type housework_bkt

tab min_hw_type // same here
drop if min_hw_type ==4

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

// fix region
gen region = REGION_
replace region = . if inlist(REGION_,0,9)
label define region 1 "Northeast" 2 "North Central" 3 "South" 4 "West" 5 "Alaska,Hawaii" 6 "Foreign"
label values region region

// splitting the college group into who has a degree. also considering advanced degree as higher than college -- this currently only works for cohort 3. I think for college - the specific years matter to split advanced, but for no college - distinguishing between grades less relevant?
gen college_bkd=.
replace college_bkd=1 if (EDUC_WIFE_==16 & EDUC_HEAD_==16) | (EDUC_WIFE_==17 & EDUC_HEAD_==17)
replace college_bkd=2 if (EDUC_WIFE_==17 & EDUC_HEAD_ <= 16) | (EDUC_WIFE_==16 & EDUC_HEAD_ <= 15) 
replace college_bkd=3 if (EDUC_HEAD_==17 & EDUC_WIFE_ <= 16) | (EDUC_HEAD_==16 & EDUC_WIFE_ <= 15)
replace college_bkd=0 if couple_educ_gp==0

label define college_bkd 1 "Both" 2 "Wife" 3 "Husband"
label values college_bkd college_bkd

gen no_college_bkd=.
replace no_college_bkd=1 if couple_educ_gp==0 & educ_wife==educ_head
replace no_college_bkd=2 if couple_educ_gp==0 & educ_wife>educ_head & educ_wife!=.
replace no_college_bkd=3 if couple_educ_gp==0 & educ_wife<educ_head & educ_head!=.
replace no_college_bkd=0 if couple_educ_gp==1
label values no_college_bkd college_bkd

// more discrete measures of work contributions
input group
.10
.20
.30
.40
.50
.60
.70
.80
1
end

xtile female_hours_bucket = female_hours_pct, cut(group)
browse female_hours_bucket female_hours_pct weekly_hrs_wife ft_pt_wife weekly_hrs_head ft_pt_head

// something went wrong here
drop ft_pt_head
drop ft_pt_wife

gen ft_pt_head=0
replace ft_pt_head=1 if weekly_hrs_head>0 & weekly_hrs_head <=35
replace ft_pt_head=2 if weekly_hrs_head >35 & weekly_hrs_head<=200

gen ft_pt_wife=0
replace ft_pt_wife=1 if weekly_hrs_wife>0 & weekly_hrs_wife <=35
replace ft_pt_wife=2 if weekly_hrs_wife >35 & weekly_hrs_wife<=200

gen overwork_head = 0
replace overwork_head =1 if weekly_hrs_head >50 & weekly_hrs_head<=200 // used by Cha 2013

gen overwork_wife = 0 
replace overwork_wife = 1 if weekly_hrs_wife > 50 & weekly_hrs_wife<=200

gen bw_type=.
replace bw_type=1 if inlist(ft_pt_head,1,2) & ft_pt_wife==0
replace bw_type=2 if ft_pt_head==2 & ft_pt_wife==1
replace bw_type=3 if (ft_pt_head==2 & ft_pt_wife==2) | (ft_pt_wife==1 & ft_pt_head==1)
replace bw_type=4 if ft_pt_head==1 & ft_pt_wife==2
replace bw_type=5 if ft_pt_head==0 & inlist(ft_pt_wife,1,2)

label define bw_type 1 "Male BW" 2 "Male and a half" 3 "Dual" 4 "Female and a half" 5 "Female BW"
label values bw_type bw_type

gen bw_type_alt=.
replace bw_type_alt=1 if inlist(ft_pt_head,1,2) & ft_pt_wife==0
replace bw_type_alt=2 if ft_pt_head==2 & ft_pt_wife==1
replace bw_type_alt=3 if ft_pt_head==2 & ft_pt_wife==2
replace bw_type_alt=4 if ft_pt_wife==1 & ft_pt_head==1
replace bw_type_alt=5 if ft_pt_head==1 & ft_pt_wife==2
replace bw_type_alt=6 if ft_pt_head==0 & inlist(ft_pt_wife,1,2)

label define bw_type_alt 1 "Male BW" 2 "Male and a half" 3 "Dual FT" 4 "Dual PT" 5 "Female and a half" 6 "Female BW"
label values bw_type_alt bw_type_alt

gen hours_type_hw=.
replace hours_type_hw=1 if bw_type==3 & housework_bkt==1
replace hours_type_hw=2 if bw_type==3 & housework_bkt==2
replace hours_type_hw=3 if bw_type==3 & housework_bkt==3
replace hours_type_hw=4 if inlist(bw_type,1,2) & housework_bkt==1
replace hours_type_hw=5 if inlist(bw_type,1,2) & housework_bkt==2
replace hours_type_hw=6 if inlist(bw_type,1,2) & housework_bkt==3
replace hours_type_hw=7 if inlist(bw_type,4,5) & housework_bkt==1
replace hours_type_hw=8 if inlist(bw_type,4,5) & housework_bkt==2
replace hours_type_hw=9 if inlist(bw_type,4,5) & housework_bkt==3

label define hours_type_hw 1 "Dual: Equal" 2 "Dual: Woman" 3 "Dual: Man" 4 "Male BW: Equal" 5 "Male BW: Woman" 6 "Male BW: Man" 7 "Female BW: Equal" 8 "Female BW: Woman" 9 "Female BW: Man"
label values hours_type_hw hours_type_hw


gen earn_type_hw=.
replace earn_type_hw=1 if hh_earn_type==1 & housework_bkt==1
replace earn_type_hw=2 if hh_earn_type==1 & housework_bkt==2
replace earn_type_hw=3 if hh_earn_type==1 & housework_bkt==3
replace earn_type_hw=4 if hh_earn_type==2 & housework_bkt==1
replace earn_type_hw=5 if hh_earn_type==2 & housework_bkt==2
replace earn_type_hw=6 if hh_earn_type==2 & housework_bkt==3
replace earn_type_hw=7 if hh_earn_type==3 & housework_bkt==1
replace earn_type_hw=8 if hh_earn_type==3 & housework_bkt==2
replace earn_type_hw=9 if hh_earn_type==3 & housework_bkt==3

label define earn_type_hw 1 "Dual: Equal" 2 "Dual: Woman" 3 "Dual: Man" 4 "Male BW: Equal" 5 "Male BW: Woman" 6 "Male BW: Man" 7 "Female BW: Equal" 8 "Female BW: Woman" 9 "Female BW: Man"
label values earn_type_hw earn_type_hw

tab earn_type_hw couple_educ_gp if inlist(IN_UNIT,1,2) & cohort==3

gen division_bucket=5
replace division_bucket = 1 if hh_earn_type== 1 & housework_bkt== 1 // dual, dual
replace division_bucket = 2 if hh_earn_type== 2 & housework_bkt== 2 // male bw, female hw
replace division_bucket = 3 if hh_earn_type== 3 & housework_bkt== 3 // female bw, male hw
replace division_bucket = 4 if hh_earn_type== 1 & housework_bkt== 2 // dual, female hw

label define division_bucket 1 "Dual" 2 "Male BW" 3 "Female BW" 4 "Necessity" 5 "All Other"
label values division_bucket division_bucket

// this doesn't capture OVERWORK
sum weekly_hrs_head if ft_pt_head==2, detail
sum weekly_hrs_wife if ft_pt_wife==2, detail

// dissimilarity
gen hours_diff = weekly_hrs_head - weekly_hrs_wife
browse hours_diff weekly_hrs_head weekly_hrs_wife
gen hours_diff_bkt = .
replace hours_diff_bkt = 1 if hours_diff <=10 & hours_diff >=-10
replace hours_diff_bkt = 2 if hours_diff >10 & hours_diff <=150
replace hours_diff_bkt = 3 if hours_diff <-10 & hours_diff >=-150

label define hours_diff_bkt 1 "Similar" 2 "Skew Male" 3 "Skew Female"
label values hours_diff_bkt hours_diff_bkt 

browse hours_diff_bkt hours_diff


gen hw_diff = housework_wife - housework_head
browse hw_diff housework_wife housework_head
gen hw_diff_bkt = .
replace hw_diff_bkt = 1 if hw_diff <=10 & hw_diff >=-10
replace hw_diff_bkt = 2 if hw_diff >10 & hw_diff <=150
replace hw_diff_bkt = 3 if hw_diff <-10 & hw_diff >=-150

label define hw_diff_bkt 1 "Similar" 2 "Skew Female" 3 "Skew Male"
label values hw_diff_bkt hw_diff_bkt 

browse hw_diff_bkt hw_diff

// test spline at 0.5
mkspline earn_ratio1 0.5 earn_ratio2 = female_earn_pct
browse female_earn_pct earn_ratio1 earn_ratio2 

mkspline hrs_ratio1 0.5 hrs_ratio2 = female_hours_pct
browse female_hours_pct hrs_ratio1 hrs_ratio2

// alternate earnings measures
*Convert to 1000s
gen earnings_1000s = couple_earnings / 1000

*log
gen earnings_total = couple_earnings + 1 
gen earnings_ln = ln(earnings_total)
* browse TAXABLE_HEAD_WIFE_ earnings_total earnings_ln

// gen earnings_ln2 = ln(TAXABLE_HEAD_WIFE_)
// replace earnings_ln2 = 0 if TAXABLE_HEAD_WIFE_ <=0

*square
gen earnings_sq = TAXABLE_HEAD_WIFE_ * TAXABLE_HEAD_WIFE_

* groups
gen earnings_bucket=.
replace earnings_bucket = 0 if TAXABLE_HEAD_WIFE_ <=0
replace earnings_bucket = 1 if TAXABLE_HEAD_WIFE_ > 0 		& TAXABLE_HEAD_WIFE_ <=10000
replace earnings_bucket = 2 if TAXABLE_HEAD_WIFE_ > 10000 	& TAXABLE_HEAD_WIFE_ <=20000
replace earnings_bucket = 3 if TAXABLE_HEAD_WIFE_ > 20000 	& TAXABLE_HEAD_WIFE_ <=30000
replace earnings_bucket = 4 if TAXABLE_HEAD_WIFE_ > 30000 	& TAXABLE_HEAD_WIFE_ <=40000
replace earnings_bucket = 5 if TAXABLE_HEAD_WIFE_ > 40000 	& TAXABLE_HEAD_WIFE_ <=50000
replace earnings_bucket = 6 if TAXABLE_HEAD_WIFE_ > 50000 	& TAXABLE_HEAD_WIFE_ <=60000
replace earnings_bucket = 7 if TAXABLE_HEAD_WIFE_ > 60000 	& TAXABLE_HEAD_WIFE_ <=70000
replace earnings_bucket = 8 if TAXABLE_HEAD_WIFE_ > 70000 	& TAXABLE_HEAD_WIFE_ <=80000
replace earnings_bucket = 9 if TAXABLE_HEAD_WIFE_ > 80000 	& TAXABLE_HEAD_WIFE_ <=90000
replace earnings_bucket = 10 if TAXABLE_HEAD_WIFE_ > 90000 	& TAXABLE_HEAD_WIFE_ <=100000
replace earnings_bucket = 11 if TAXABLE_HEAD_WIFE_ > 100000 & TAXABLE_HEAD_WIFE_ <=150000
replace earnings_bucket = 12 if TAXABLE_HEAD_WIFE_ > 150000 & TAXABLE_HEAD_WIFE_ !=.

label define earnings_bucket 0 "0" 1 "0-10000" 2 "10000-20000" 3 "20000-30000" 4 "30000-40000" 5 "40000-50000" 6 "50000-60000" 7 "60000-70000" ///
8 "70000-80000" 9 "80000-90000" 10 "90000-100000" 11 "100000-150000" 12 "150000+"
label values earnings_bucket earnings_bucket

* Spline
mkspline earnx 4 = couple_earnings, displayknots pctile
mkspline earn = couple_earnings, cubic displayknots

browse couple_earnings earn1 earn2 earn3 earn4 earnx1 earnx2 earnx3 earnx4

mkspline knot1 0 knot2 20 knot3 = earnings_1000s

// i'm confused
mkspline knotx1 20 knotx2 = earnings_1000s

* Employment 
gen couple_work=.
replace couple_work=1 if ft_head==1 & ft_wife==1
replace couple_work=2 if ft_head==0 & ft_wife==0
replace couple_work=3 if ft_head==1 & ft_wife==0
replace couple_work=4 if ft_head==0 & ft_wife==1

label define couple_work 1 "Both FT" 2 "Neither FT" 3 "Him FT Her Not" 4 "Her FT Him Not"
label values couple_work couple_work

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

// alt cohab
gen ever_cohab=0
replace ever_cohab=1 if cohab_with_wife==1 | cohab_with_other==1

// categorical for number of children
recode NUM_CHILDREN_ (0=0)(1=1)(2=2)(3/13=3), gen(num_children)
label define num_children 0 "None" 1 "1 Child" 2 "2 Children" 3 "3+ Children"
label values num_children num_children

// square age of marriage
gen age_mar_head_sq = age_mar_head * age_mar_head
gen age_mar_wife_sq = age_mar_wife * age_mar_wife

// create dummy variable for interval length
gen interval=.
replace interval=1 if inrange(survey_yr,1968,1997)
replace interval=2 if inrange(survey_yr,1999,2019)

// need to combine weight variables
gen weight=.
replace weight=CORE_WEIGHT_ if inrange(survey_yr,1968,1992)
replace weight=COR_IMM_WT_ if inrange(survey_yr,1993,2019)

gen weight_rescale=.

forvalues y=1991/1997{
	summarize weight if survey_yr==`y'
	local rescalefactor `r(N)'/`r(sum)'
	display `rescalefactor'
	replace weight_rescale = weight*`rescalefactor' if survey_yr==`y'
	summarize weight_rescale if survey_yr==`y'
}

forvalues y=1999(2)2019{
	summarize weight if survey_yr==`y'
	local rescalefactor `r(N)'/`r(sum)'
	display `rescalefactor'
	replace weight_rescale = weight*`rescalefactor' if survey_yr==`y'
	summarize weight_rescale if survey_yr==`y'
}

tabstat weight, by(interval)
tabstat weight_rescale, by(interval)

// think need to update the cds eligiblity variable to not be missing
gen cds_sample=0
replace cds_sample=1 if CDS_ELIGIBLE_==1

// also add weight adjustment thing - "$temp\psid_weight_adjustment.dta"
merge m:1 AGE_REF_ survey_yr using "$temp\psid_weight_adjustment.dta"
drop if _merge==2
drop _merge

browse survey_yr children children_ever num_children AGE_YOUNG_CHILD_ FIRST_BIRTH_YR
tab AGE_YOUNG_CHILD_ num_children, m

gen weight_adjust=weight
replace weight_adjust = weight * adjust_child if race_head==2 & inrange(survey_yr,1997,2019) & num_children>=1 & AGE_YOUNG_CHILD <=13
replace weight_adjust = weight * adjust_no_child if race_head==2 & inrange(survey_yr,1997,2019) & (num_children==0 | (num_children>=1 & AGE_YOUNG_CHILD >13))

browse survey_yr race_head AGE_YOUNG_CHILD_ weight weight_adjust adjust_child adjust_no_child

// missing value inspect
inspect age_mar_wife // 0
inspect age_mar_head // 0
inspect race_head // 2
inspect same_race // 0
inspect either_enrolled // 0
inspect REGION_ // 0
inspect cohab_with_wife // 0
inspect cohab_with_other // 0 
inspect pre_marital_birth // 0

// indicators of paid leave
gen paid_leave_state=0
replace paid_leave_state=1 if inlist(STATE_,6,34,36,44)

gen time_leave=.
replace time_leave=0 if STATE_==6 & survey_yr < 2004
replace time_leave=0 if STATE_==34 & survey_yr < 2009
replace time_leave=0 if STATE_==36 & survey_yr < 2014
replace time_leave=0 if STATE_==44 & survey_yr < 2018
replace time_leave=1 if STATE_==6 & survey_yr >= 2004
replace time_leave=1 if STATE_==34 & survey_yr >= 2009
replace time_leave=1 if STATE_==36 & survey_yr >= 2014
replace time_leave=1 if STATE_==44 & survey_yr >= 2018

// minimum wage
gen min_wage=0
replace min_wage=1 if inlist(STATE_,2,4,5,6,8,9,10,11,12,15,17,23,24,25,26,27,29,30,31,34,35,36,39,41,44,46,50,53,54)

// control variables: age of marriage (both), race (head + same race), religion (head), region? (head), cohab_with_wife, cohab_with_other, pre_marital_birth, post_marital_birth
// both pre and post marital birth should NOT be in model because they are essentially inverse. do I want to add if they have a child together as new flag?
// taking out religion for now because not asked in 1968 / 1968

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"

**# Analysis starts

********************************************************************************
********************************************************************************
********************************************************************************
* For PAA Final Paper: main analysis
********************************************************************************
********************************************************************************
********************************************************************************

********************************************************************************
* Overall models
********************************************************************************
logit dissolve_lag i.dur i.couple_educ_gp if inlist(IN_UNIT,1,2) & cohort==3, or
logit dissolve_lag i.dur earnings_ln2 if inlist(IN_UNIT,1,2) & cohort==3, or
logit dissolve_lag i.dur earnings_ln2 i.couple_educ_gp if inlist(IN_UNIT,1,2) & cohort==3, or
logit dissolve_lag i.dur i.couple_educ_gp##c.earnings_ln2 if inlist(IN_UNIT,1,2) & cohort==3, or
margins couple_educ_gp, at(earnings_ln2=(0(1)10))

logit dissolve_lag i.dur i.couple_educ_gp##c.couple_earnings if inlist(IN_UNIT,1,2) & cohort==3, or
marginscontplot couple_earnings couple_educ_gp if couple_earnings<200000, ci var1(20) // diff plots - want them on same
marginscontplot couple_earnings couple_educ_gp if couple_earnings<200000, ci var1(20) at2(0 1) showmarginscmd // still not
marginscontplot couple_earnings couple_educ_gp if couple_earnings<200000, var1(20) var2(2) // here we go

logit dissolve_lag i.dur i.couple_educ_gp##c.earnings_ln if inlist(IN_UNIT,1,2) & cohort==3, or
marginscontplot earnings_ln couple_educ_gp, var1(20) var2(2) // here we go

summarize TAXABLE_HEAD_WIFE_
range earn 1 100000 20
gen loge = ln(earn)
marginscontplot TAXABLE_HEAD_WIFE_(earnings_ln), var1(earn(loge)) ci
marginscontplot TAXABLE_HEAD_WIFE_(earnings_ln) couple_educ_gp, var1(earn(loge)) var2(2) ci

logit dissolve_lag i.dur earnings_ln if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
estimates store e0
logit dissolve_lag i.dur earnings_ln if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
estimates store e1

suest e0 e1 // i have no idea what this is. Are these marginal effects isntead of or? or covariance?

logit dissolve_lag i.dur i.couple_educ_gp if inlist(IN_UNIT,1,2) & cohort==3, or
est store m1

logit dissolve_lag i.dur##i.couple_educ_gp if inlist(IN_UNIT,1,2) & cohort==3, or
est store m2

lrtest m1 m2

margins couple_educ_gp#dur
margins dur, dydx(couple_educ_gp)
margins couple_educ_gp, dydx(dur)

********************************************************************************
* Stratified
********************************************************************************

/* test for adding margins
**Paid work
logit dissolve_lag i.dur i.hh_earn_type if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
outreg2 using "$results/margins_test.xls", sideway stats(coef pval) label ctitle(Paid No) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace
margins, dydx(hh_earn_type) post
outreg2 using "$results/margins_test.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

logit dissolve_lag i.dur i.hh_earn_type if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
outreg2 using "$results/margins_test.xls", sideway stats(coef pval) label ctitle(Paid coll) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(hh_earn_type) post
outreg2 using "$results/margins_test.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

logit dissolve_lag i.dur earnings_ln if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
margins, dydx(earnings_ln)
*/

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"

////////// No College \\\\\\\\\\\/
** Total earnings
logit dissolve_lag i.dur TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
logit dissolve_lag i.dur couple_earnings if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
marginscontplot couple_earnings if couple_earnings<200000, ci var1(20)
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Earnings No) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

logit dissolve_lag i.dur TAXABLE_HEAD_WIFE_ `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Earnings No+) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

**Paid work
logit dissolve_lag i.dur i.hh_earn_type if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Paid No) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_earn_type TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Paid No+) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins hh_earn_type, at(dur=(1 6 12 18 24))

histogram TAXABLE_HEAD_WIFE_ if couple_educ_gp==0 & cohort==3 & inrange(TAXABLE_HEAD_WIFE_,-10000,100000)
margins hh_earn_type
margins, dydx(hh_earn_type)
margins, at(TAXABLE_HEAD_WIFE_ =(0(10000)100000))

logit dissolve_lag i.dur##i.hh_earn_type TAXABLE_HEAD_WIFE_ age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0 & hh_earn_type<4 & dur<=15, or
margins dur, dydx(hh_earn_type)

**Unpaid work
logit dissolve_lag i.dur i.housework_bkt if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Unpaid No) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur i.housework_bkt TAXABLE_HEAD_WIFE_ `controls' if inlist(IN_UNIT,1,2)  & cohort==3 & couple_educ_gp==0, or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Unpaid No+) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins housework_bkt
margins r.housework_bkt
margins, dydx(housework_bkt)

/* attempting to figure out which to use:
in model: p=.034
test 1.housework_bkt=3.housework_bkt // p =.0344 - but does this have anything to do with margins?
Using dydx, p=.051
margins r.housework_bkt
*/

logit dissolve_lag i.dur##i.housework_bkt TAXABLE_HEAD_WIFE_ age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0 & housework_bkt<4 & dur<=15, or
margins dur, dydx(housework_bkt)

////////// College \\\\\\\\\\\/
** Total earnings
logit dissolve_lag i.dur TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Earnings Coll) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur TAXABLE_HEAD_WIFE_ `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Earnings Coll+) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

**Paid work
logit dissolve_lag i.dur i.hh_earn_type if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Paid Coll) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_earn_type TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Paid Coll+) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins hh_earn_type
margins, at(TAXABLE_HEAD_WIFE_ =(0(10000)100000))
margins hh_earn_type, at(dur=(1 6 12 18 24))
margins hh_earn_type, at(dur=(1(1)24))

logit dissolve_lag i.dur##i.hh_earn_type TAXABLE_HEAD_WIFE_ age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1 & hh_earn_type<4 & dur<=15, or
margins dur, dydx(hh_earn_type)

**Unpaid work
logit dissolve_lag i.dur i.housework_bkt if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Unpaid Coll) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.housework_bkt TAXABLE_HEAD_WIFE_ `controls' if inlist(IN_UNIT,1,2)  & cohort==3 & couple_educ_gp==1, or
outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Unpaid Coll+) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins housework_bkt

logit dissolve_lag i.dur##i.housework_bkt TAXABLE_HEAD_WIFE_ age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1 & housework_bkt<4 & dur<=15, or
margins dur, dydx(housework_bkt)

//// Alternate earnings
* No College
logit dissolve_lag i.dur earnings_ln if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
// marginscontplot earnings(logwt), var1(w(logw)) ci
logit dissolve_lag i.dur earnings_ln2 if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
logit dissolve_lag i.dur TAXABLE_HEAD_WIFE_ earnings_sq if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or // square not sig here
margins, at(TAXABLE_HEAD_WIFE_=(0(10000)100000))
logit dissolve_lag i.dur TAXABLE_HEAD_WIFE_ c.TAXABLE_HEAD_WIFE_#c.TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or // alt square
margins, at(TAXABLE_HEAD_WIFE_=(0(10000)100000))
logit dissolve_lag i.dur ib5.earnings_bucket if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
logit dissolve_lag i.dur earn1 earn2 earn3 earn4 if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
logit dissolve_lag i.dur earnx1 earnx2 earnx3 earnx4 if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
logit dissolve_lag i.dur knot1 knot2 knot3 if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
logit dissolve_lag i.dur knotx1 knotx2 if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or

logit dissolve_lag i.dur earnx1 earnx2 earnx3 earnx4 if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
margins, dydx(earnx1 earnx2 earnx3 earnx4)

logit dissolve_lag i.dur earnx1 earnx2 earnx3 earnx4 if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
margins, dydx(earnx1 earnx2 earnx3 earnx4)

logit dissolve_lag i.dur i.couple_educ_gp c.earnx1 c.earnx2 c.earnx3 c.earnx4 if inlist(IN_UNIT,1,2) & cohort==3, or
est store m0

logit dissolve_lag i.dur i.couple_educ_gp##(c.earnx1 c.earnx2 c.earnx3 c.earnx4) if inlist(IN_UNIT,1,2) & cohort==3, or
est store m1

lrtest m0 m1
est table m0 m1, stats(ll rank)

*College
logit dissolve_lag i.dur earnings_ln if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
logit dissolve_lag i.dur earnings_ln2 if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
logit dissolve_lag i.dur TAXABLE_HEAD_WIFE_ earnings_sq if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or // square *is* sig here
margins, at(TAXABLE_HEAD_WIFE_=(0(10000)150000))
logit dissolve_lag i.dur TAXABLE_HEAD_WIFE_ c.TAXABLE_HEAD_WIFE_#c.TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or // alt square
margins, at(TAXABLE_HEAD_WIFE_=(0(10000)100000))
logit dissolve_lag i.dur ib5.earnings_bucket if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
logit dissolve_lag i.dur earn1 earn2 earn3 earn4 if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
logit dissolve_lag i.dur earnx1 earnx2 earnx3 earnx4 if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
logit dissolve_lag i.dur knot1 knot2 knot3 if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or

///// Decide if want to use - all in one model, interactions

**No college
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur i.hh_earn_type i.housework_bkt TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
outreg2 using "$results/psid_marriage_dissolution_int.xls", sideway stats(coef pval) label ctitle(All - No) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

logit dissolve_lag i.dur i.hh_earn_type##i.housework_bkt TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or // bucketed
outreg2 using "$results/psid_marriage_dissolution_int.xls", sideway stats(coef pval) label ctitle(Interact - no) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

margins hh_earn_type#housework_bkt
marginsplot

logit dissolve_lag i.dur i.earn_type_hw TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or // still nothing sig
// okay when I change ref group, male bw less likely to dissolve than dual / male and female / male (so counter-normative)
// dual / woman (as predicted by economic necessity) also sig less likely to dissolve than both of these
margins earn_type_hw
margins, dydx(earn_type_hw)

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur c.female_earn_pct##c.wife_housework_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or // continuous - nothing sig
margins, at(female_earn_pct=(0(.25)1) wife_housework_pct=(0(0.25)1))
marginsplot

logit dissolve_lag i.dur i.division_bucket if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or // counter-normative = most likely to dissolve
logit dissolve_lag i.dur i.division_bucket TAXABLE_HEAD_WIFE_ age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or // though not true with controls
logit dissolve_lag i.dur i.division_bucket TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or // opposite of earlier - once I add earnings, effects go away - so it's probably because least income?

tabstat TAXABLE_HEAD_WIFE_ if cohort==3, by(division_bucket) stats(mean p50)
tabstat TAXABLE_HEAD_WIFE_ if cohort==3 & couple_educ_gp==0, by(division_bucket) stats(mean p50)

**College
logit dissolve_lag i.dur i.hh_earn_type i.housework_bkt TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
outreg2 using "$results/psid_marriage_dissolution_int.xls", sideway stats(coef pval) label ctitle(All - Coll) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_earn_type##i.housework_bkt TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
outreg2 using "$results/psid_marriage_dissolution_int.xls", sideway stats(coef pval) label ctitle(Interact - Coll) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

margins hh_earn_type#housework_bkt
marginsplot

logit dissolve_lag i.dur i.earn_type_hw TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
margins earn_type_hw
margins, dydx(earn_type_hw)

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur c.female_earn_pct##c.wife_housework_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or // continuous - marginally pos sig (the interaction term) - so when she does a lot of hw and paid work = high. use this instead??
margins, at(female_earn_pct=(0(.25)1) wife_housework_pct=(0(0.25)1))
marginsplot

logit dissolve_lag i.dur ib5.division_bucket if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or // "all other" sig more likely...
logit dissolve_lag i.dur i.division_bucket if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or // "all other" sig more likely...
logit dissolve_lag i.dur i.division_bucket TAXABLE_HEAD_WIFE_ age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or // still true
logit dissolve_lag i.dur ib4.division_bucket TAXABLE_HEAD_WIFE_ age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or // was wondering if "economic necessity" worse - bc of role strain. But seems like not

logit dissolve_lag i.dur ib4.division_bucket##i.overwork_head if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or // dual is marginally sig less likely to dissolve when husband does not overwork

/*
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur i.hh_earn_type if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or

logit dissolve_lag i.dur i.hh_earn_type TAXABLE_HEAD_WIFE_ age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or // both

logit dissolve_lag i.dur i.hh_earn_type TAXABLE_HEAD_WIFE_ age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth if inlist(IN_UNIT,1,2) & cohort_v2==1 & couple_educ_gp==0, or // alt cohort

logit dissolve_lag i.dur i.hh_earn_type age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or // just earn type

logit dissolve_lag i.dur TAXABLE_HEAD_WIFE_ age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or // just income. income always significant

logit dissolve_lag i.dur i.hh_earn_type TAXABLE_HEAD_WIFE_ age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region ever_cohab pre_marital_birth if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or // alt cohab (botH)

exploring
*/

********************************************************************************
********************************************************************************
**# Average Marginal Effects
********************************************************************************
********************************************************************************

********************************************************************************
* Logged earnings
********************************************************************************

/*
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"

////////// No College \\\\\\\\\\\/
** Total earnings
logit dissolve_lag i.dur earnings_ln `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
outreg2 using "$results/dissolution_margins.xls", sideway stats(coef pval) label ctitle(Earnings No+) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace
margins, dydx(*) post
outreg2 using "$results/dissolution_margins.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

**Paid work
logit dissolve_lag i.dur i.hh_earn_type earnings_ln  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
outreg2 using "$results/dissolution_margins.xls", sideway stats(coef pval) label ctitle(Paid No+) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/dissolution_margins.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

**Unpaid work
logit dissolve_lag i.dur i.housework_bkt earnings_ln `controls' if inlist(IN_UNIT,1,2)  & cohort==3 & couple_educ_gp==0, or
outreg2 using "$results/dissolution_margins.xls", sideway stats(coef pval) label ctitle(Unpaid No+) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/dissolution_margins.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

**Interaction
logit dissolve_lag i.dur ib5.earn_type_hw earnings_ln `controls' if inlist(IN_UNIT,1,2)  & cohort==3 & couple_educ_gp==0, or
outreg2 using "$results/dissolution_margins.xls", sideway stats(coef pval) label ctitle(Both No+) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/dissolution_margins.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

////////// College \\\\\\\\\\\/
** Total earnings
logit dissolve_lag i.dur earnings_ln `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
outreg2 using "$results/dissolution_margins.xls", sideway stats(coef pval) label ctitle(Earnings Coll+) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/dissolution_margins.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

**Paid work
logit dissolve_lag i.dur i.hh_earn_type earnings_ln  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
outreg2 using "$results/dissolution_margins.xls", sideway stats(coef pval) label ctitle(Paid Coll+) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/dissolution_margins.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

**Unpaid work
logit dissolve_lag i.dur i.housework_bkt earnings_ln `controls' if inlist(IN_UNIT,1,2)  & cohort==3 & couple_educ_gp==1, or
outreg2 using "$results/dissolution_margins.xls", sideway stats(coef pval) label ctitle(Unpaid Coll+) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/dissolution_margins.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

**Interaction
logit dissolve_lag i.dur ib5.earn_type_hw earnings_ln `controls' if inlist(IN_UNIT,1,2)  & cohort==3 & couple_educ_gp==1, or
outreg2 using "$results/dissolution_margins.xls", sideway stats(coef pval) label ctitle(Both Coll+) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/dissolution_margins.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

margins earn_type_hw, pwcompare level(90)
margins earn_type_hw, pwcompare(group) level(90)
*/

********************************************************************************
* Raw, 1000s of dollars (assumes linearity)
********************************************************************************

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"

////////// No College \\\\\\\\\\\/
*1. Continuous earnings ratio
logit dissolve_lag i.dur female_earn_pct earnings_1000s `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
outreg2 using "$results/dissolution_AMES_final.xls", sideway stats(coef pval) label ctitle(No 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace
margins, dydx(*) post
outreg2 using "$results/dissolution_AMES_final.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*2. Categorical indicator of Paid work
logit dissolve_lag i.dur i.hh_earn_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
outreg2 using "$results/dissolution_AMES_final.xls", sideway stats(coef pval) label ctitle(No 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/dissolution_AMES_final.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur i.hh_earn_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
margins hh_earn_type

*3A. Employment: no interaction
logit dissolve_lag i.dur i.ft_head i.ft_wife earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
outreg2 using "$results/dissolution_AMES_final.xls", sideway stats(coef pval) label ctitle(No 3a) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/dissolution_AMES_final.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*3B. Employment: interaction
logit dissolve_lag i.dur ib3.couple_work earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
outreg2 using "$results/dissolution_AMES_final.xls", sideway stats(coef pval) label ctitle(No 3b) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/dissolution_AMES_final.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*4. Total earnings
logit dissolve_lag i.dur earnings_1000s `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
outreg2 using "$results/dissolution_AMES_final.xls", sideway stats(coef pval) label ctitle(No 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/dissolution_AMES_final.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur earnings_1000s `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
margins, at(earnings_1000s=(0(10)100))

*5. Continuous Housework
logit dissolve_lag i.dur wife_housework_pct earnings_1000s `controls' if inlist(IN_UNIT,1,2)  & cohort==3 & couple_educ_gp==0, or
outreg2 using "$results/dissolution_AMES_final.xls", sideway stats(coef pval) label ctitle(No 5) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/dissolution_AMES_final.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*6. Categorical Housework
logit dissolve_lag i.dur i.housework_bkt earnings_1000s `controls' if inlist(IN_UNIT,1,2)  & cohort==3 & couple_educ_gp==0, or
outreg2 using "$results/dissolution_AMES_final.xls", sideway stats(coef pval) label ctitle(No 6) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/dissolution_AMES_final.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur i.housework_bkt earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
margins housework_bkt

*7. All 
logit dissolve_lag i.dur i.hh_earn_type i.housework_bkt earnings_1000s `controls' if inlist(IN_UNIT,1,2)  & cohort==3 & couple_educ_gp==0, or
outreg2 using "$results/dissolution_AMES_final.xls", sideway stats(coef pval) label ctitle(No 7) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/dissolution_AMES_final.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

////////// College \\\\\\\\\\\/
*1. Continuous earnings ratio
logit dissolve_lag i.dur female_earn_pct earnings_1000s `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
outreg2 using "$results/dissolution_AMES_final.xls", sideway stats(coef pval) label ctitle(Coll 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/dissolution_AMES_final.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*2. Categorical indicator of Paid work
logit dissolve_lag i.dur i.hh_earn_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
outreg2 using "$results/dissolution_AMES_final.xls", sideway stats(coef pval) label ctitle(Coll 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/dissolution_AMES_final.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur i.hh_earn_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
margins hh_earn_type

*3A. Employment: no interaction
logit dissolve_lag i.dur i.ft_head i.ft_wife earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
outreg2 using "$results/dissolution_AMES_final.xls", sideway stats(coef pval) label ctitle(Coll 3a) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/dissolution_AMES_final.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*3B. Employment: interaction
logit dissolve_lag i.dur ib3.couple_work earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
outreg2 using "$results/dissolution_AMES_final.xls", sideway stats(coef pval) label ctitle(Coll 3b) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/dissolution_AMES_final.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*4. Total earnings
logit dissolve_lag i.dur earnings_1000s `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
outreg2 using "$results/dissolution_AMES_final.xls", sideway stats(coef pval) label ctitle(Coll 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/dissolution_AMES_final.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur earnings_1000s `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
margins, at(earnings_1000s=(0(10)100))

*5. Continuous Housework
logit dissolve_lag i.dur wife_housework_pct earnings_1000s `controls' if inlist(IN_UNIT,1,2)  & cohort==3 & couple_educ_gp==1, or
outreg2 using "$results/dissolution_AMES_final.xls", sideway stats(coef pval) label ctitle(Coll 5) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/dissolution_AMES_final.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*6. Categorical Housework
logit dissolve_lag i.dur i.housework_bkt earnings_1000s `controls' if inlist(IN_UNIT,1,2)  & cohort==3 & couple_educ_gp==1, or
outreg2 using "$results/dissolution_AMES_final.xls", sideway stats(coef pval) label ctitle(Coll 6) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/dissolution_AMES_final.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur i.housework_bkt earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
margins housework_bkt

*7. All 
logit dissolve_lag i.dur i.hh_earn_type i.housework_bkt earnings_1000s `controls' if inlist(IN_UNIT,1,2)  & cohort==3 & couple_educ_gp==1, or
outreg2 using "$results/dissolution_AMES_final.xls", sideway stats(coef pval) label ctitle(Coll 7) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/dissolution_AMES_final.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

********************************************************************************
**# USE (actually) - this is in existing ASR paper
* Spline earnings
********************************************************************************
/* help
logit dissolve_lag i.dur knot1 knot2 knot3 if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
gen no_earnings=0
replace no_earnings=1 if earnings_1000s==0

logit dissolve_lag i.dur knot1 knot2 knot3 i.no_earnings if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur i.ft_head i.ft_wife knot1 knot2 knot3  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
margins, dydx(ft_head ft_wife)

logit dissolve_lag i.dur i.ft_head i.ft_wife knotx1 knotx2 `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
margins, dydx(ft_head ft_wife)
*/

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"

////////// No College \\\\\\\\\\\/
*1. Employment: no interaction
logit dissolve_lag i.dur i.ft_head i.ft_wife knot1 knot2 knot3  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
outreg2 using "$results/dissolution_AMES_alt_earn.xls", sideway stats(coef pval) label ctitle(No 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace
margins, dydx(*) post
outreg2 using "$results/dissolution_AMES_alt_earn.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*2. Categorical indicator of Paid work
logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
outreg2 using "$results/dissolution_AMES_alt_earn.xls", sideway stats(coef pval) label ctitle(No 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/dissolution_AMES_alt_earn.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*3. Total earnings
logit dissolve_lag i.dur knot1 knot2 knot3 `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
outreg2 using "$results/dissolution_AMES_alt_earn.xls", sideway stats(coef pval) label ctitle(No 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/dissolution_AMES_alt_earn.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*4. Categorical Housework
logit dissolve_lag i.dur i.housework_bkt knot1 knot2 knot3 `controls' if inlist(IN_UNIT,1,2)  & cohort==3 & couple_educ_gp==0, or
outreg2 using "$results/dissolution_AMES_alt_earn.xls", sideway stats(coef pval) label ctitle(No 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/dissolution_AMES_alt_earn.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

////////// College \\\\\\\\\\\/
*1. Employment: no interaction
logit dissolve_lag i.dur i.ft_head i.ft_wife knot1 knot2 knot3  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
outreg2 using "$results/dissolution_AMES_alt_earn.xls", sideway stats(coef pval) label ctitle(Coll 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/dissolution_AMES_alt_earn.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*2. Categorical indicator of Paid work
logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
outreg2 using "$results/dissolution_AMES_alt_earn.xls", sideway stats(coef pval) label ctitle(Coll 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/dissolution_AMES_alt_earn.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*3. Total earnings
logit dissolve_lag i.dur knot1 knot2 knot3 `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
outreg2 using "$results/dissolution_AMES_alt_earn.xls", sideway stats(coef pval) label ctitle(Coll 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/dissolution_AMES_alt_earn.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*4. Categorical Housework
logit dissolve_lag i.dur i.housework_bkt knot1 knot2 knot3 `controls' if inlist(IN_UNIT,1,2)  & cohort==3 & couple_educ_gp==1, or
outreg2 using "$results/dissolution_AMES_alt_earn.xls", sideway stats(coef pval) label ctitle(Coll 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/dissolution_AMES_alt_earn.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

********************************************************************************
**# Weight comparison (with other small tweaks asked for)
********************************************************************************
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"

// No College: Employment
logit dissolve_lag i.dur i.ft_head i.ft_wife knot1 knot2 knot3  `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0, or // original
margins, dydx(*) post
outreg2 using "$results/dissolution_weight_analysis.xls", sideway stats(coef se pval) ctitle(No1) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

logit dissolve_lag i.dur i.ft_head i.ft_wife knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq  `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0, or // add dummy for year change (+ new controls) -- okay so this makes ft_wife statistically sig
margins, dydx(*) post
outreg2 using "$results/dissolution_weight_analysis.xls", sideway stats(coef se pval) ctitle(No2) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.ft_head i.ft_wife knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq  `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0 [pweight=weight], or // adding weights - so ft_wife is marginally sig
margins, dydx(*) post
outreg2 using "$results/dissolution_weight_analysis.xls", sideway stats(coef se pval) ctitle(No3) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.ft_head i.ft_wife knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq  `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0 [pweight=weight_rescale], or // adding weights - so ft_wife is marginally sig
margins, dydx(*) post
outreg2 using "$results/dissolution_weight_analysis.xls", sideway stats(coef se pval) ctitle(No3b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// No College: DoL
logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3  `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0, or
margins, dydx(*) post
outreg2 using "$results/dissolution_weight_analysis.xls", sideway stats(coef se pval) ctitle(No4) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0, or // add dummy for year change - okay this one changes less
margins, dydx(*) post
outreg2 using "$results/dissolution_weight_analysis.xls", sideway stats(coef se pval) ctitle(No5) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0 [pweight=weight], or // add weights - okay so this makes results significant - dual earning has HIGHEST risk of divorce
margins, dydx(*) post
outreg2 using "$results/dissolution_weight_analysis.xls", sideway stats(coef se pval) ctitle(No6) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0 [pweight=weight_rescale], or // add weights - okay so this makes results significant - dual earning has HIGHEST risk of divorce
margins, dydx(*) post
outreg2 using "$results/dissolution_weight_analysis.xls", sideway stats(coef se pval) ctitle(No6b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// No College: Unpaid DoL
logit dissolve_lag i.dur i.housework_bkt knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0, or
margins, dydx(*) post
outreg2 using "$results/dissolution_weight_analysis.xls", sideway stats(coef se pval) ctitle(No7) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.housework_bkt knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0 [pweight=weight], or
margins, dydx(*) post
outreg2 using "$results/dissolution_weight_analysis.xls", sideway stats(coef se pval) ctitle(No8) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// College: Employment
logit dissolve_lag i.dur i.ft_head i.ft_wife knot1 knot2 knot3  `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==1, or
margins, dydx(*) post
outreg2 using "$results/dissolution_weight_analysis.xls", sideway stats(coef se pval) ctitle(Coll1) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.ft_head i.ft_wife knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq  `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==1, or // add dummy for year change (+ new controls) -- very similar, nothing changes
margins, dydx(*) post
outreg2 using "$results/dissolution_weight_analysis.xls", sideway stats(coef se pval) ctitle(Coll2) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.ft_head i.ft_wife knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq  `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==1 [pweight=weight], or // adding weights - now ft head = less likely and ft wife more likely
margins, dydx(*) post
outreg2 using "$results/dissolution_weight_analysis.xls", sideway stats(coef se pval) ctitle(Coll3) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.ft_head i.ft_wife knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq  `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==1 [pweight=weight_rescale], or // adding weights - now ft head = less likely and ft wife more likely
margins, dydx(*) post
outreg2 using "$results/dissolution_weight_analysis.xls", sideway stats(coef se pval) ctitle(Coll3b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// College: DoL
logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3  `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==1, or
margins, dydx(*) post
outreg2 using "$results/dissolution_weight_analysis.xls", sideway stats(coef se pval) ctitle(Coll4) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==1, or // add dummy for year change - okay this one changes less
margins, dydx(*) post
outreg2 using "$results/dissolution_weight_analysis.xls", sideway stats(coef se pval) ctitle(Coll5) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==1 [pweight=weight], or // add weights - now nothing sig
margins, dydx(*) post
outreg2 using "$results/dissolution_weight_analysis.xls", sideway stats(coef se pval) ctitle(Coll6) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==1 [pweight=weight_rescale], or // add weights - now nothing sig
margins, dydx(*) post
outreg2 using "$results/dissolution_weight_analysis.xls", sideway stats(coef se pval) ctitle(Coll6b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// College: Unpaid DoL
logit dissolve_lag i.dur i.housework_bkt knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==1, or
margins, dydx(*) post
outreg2 using "$results/dissolution_weight_analysis.xls", sideway stats(coef se pval) ctitle(Coll7) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.housework_bkt knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==1 [pweight=weight], or
margins, dydx(*) post
outreg2 using "$results/dissolution_weight_analysis.xls", sideway stats(coef se pval) ctitle(Coll8) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

********************************************************************************
* Alt earnings
********************************************************************************
/* logged*/
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur earnings_ln `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
margins, at(earnings_ln=(0(2)12))
margins, dydx(earnings_ln) post

logit dissolve_lag i.dur earnings_ln `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
margins, at(earnings_ln=(0(2)12))
margins, dydx(earnings_ln) post

logit dissolve_lag i.dur earnings_ln i.hh_earn_type `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
margins, dydx(earnings_ln) post
logit dissolve_lag i.dur earnings_ln i.hh_earn_type `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
margins, dydx(earnings_ln) post

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur c.earnings_ln##i.hh_earn_type `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0 & hh_earn_type<4, or
margins hh_earn_type, at(earnings_ln=(0(2)12))
marginsplot

logit dissolve_lag i.dur c.earnings_ln##i.hh_earn_type `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1 & hh_earn_type<4, or
margins hh_earn_type, at(earnings_ln=(0(2)12))
marginsplot

/* squared */
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur earnings_1000s c.earnings_1000s#c.earnings_1000s `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
margins, at(earnings_1000s=(0(10)100))
margins, dydx(earnings_1000s) at(earnings_1000s=(0(10)100))
marginsplot

logit dissolve_lag i.dur earnings_1000s c.earnings_1000s#c.earnings_1000s `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
margins, at(earnings_1000s=(0(10)100))
margins, dydx(earnings_1000s) at(earnings_1000s=(0(10)100))
marginsplot

/* grouped */
gen couple_earnings_gp=.
replace couple_earnings_gp=0 if earnings_1000s==0
replace couple_earnings_gp=1 if earnings_1000s>0 & earnings_1000s<10
replace couple_earnings_gp=2 if earnings_1000s>=10 & earnings_1000s<20
replace couple_earnings_gp=3 if earnings_1000s>=20 & earnings_1000s<30
replace couple_earnings_gp=4 if earnings_1000s>=30 & earnings_1000s<40
replace couple_earnings_gp=5 if earnings_1000s>=40 & earnings_1000s<50
replace couple_earnings_gp=6 if earnings_1000s>=50 & earnings_1000s<60
replace couple_earnings_gp=7 if earnings_1000s>=60 & earnings_1000s<70
replace couple_earnings_gp=8 if earnings_1000s>=70 & earnings_1000s<80
replace couple_earnings_gp=9 if earnings_1000s>=80 & earnings_1000s<90
replace couple_earnings_gp=10 if earnings_1000s>=90 & earnings_1000s<100
replace couple_earnings_gp=11 if earnings_1000s>=100 & earnings_1000s!=.

label define couple_earnings_gp 0 "$0" 1 "$10" 2 "$20" 3 "$30" 4 "$40" 5 "$50" 6 "$60" ///
7 "$70" 8 "$80" 9 "$90" 10 "$100" 11 ">$100"
label values couple_earnings_gp couple_earnings_gp

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur i.couple_earnings_gp `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
logit dissolve_lag i.dur ib3.couple_earnings_gp `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or // b3 is generally median
margins couple_earnings_gp
margins, dydx(couple_earnings_gp)

logit dissolve_lag i.dur ib3.couple_earnings_gp `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
logit dissolve_lag i.dur ib8.couple_earnings_gp `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
margins couple_earnings_gp
margins, dydx(couple_earnings_gp)

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur ib3.couple_earnings_gp##i.couple_educ_gp `controls' if inlist(IN_UNIT,1,2) & cohort==3, or // b3 is generally median
margins couple_earnings_gp#couple_educ_gp
marginsplot // yes okay this is exactly what I need for the curve argument
marginsplot, xtitle("Total Couple Earnings (grouped, $1000s)") ylabel(, angle(0))  ytitle("Probability of Marital Dissolution") plotregion(fcolor(white)) graphregion(fcolor(white)) title("")  xlabel(, angle(45) labsize(small)) legend(region(lcolor(white))) legend(size(small)) plot1opts(lcolor("blue")  msize("small") mcolor("blue"))  plot2opts(lcolor("pink") mcolor("pink") msize("small")) recast(line) recastci(rarea) ciopts(color(*.4)) //  ci2opts(lcolor("pink")) ci1opts(lcolor("blue"))

marginsplot, xtitle("Total Couple Earnings (grouped, $1000s)") ylabel(, angle(0))  ytitle("Probability of Marital Dissolution") plotregion(fcolor(white)) graphregion(fcolor(white)) title("")  xlabel(, angle(45) labsize(small)) legend(region(lcolor(white))) legend(size(small)) plot1opts(lcolor("blue")  msize("small") mcolor("blue"))  plot2opts(lcolor("pink") mcolor("pink") msize("small")) noci recast(line)

// okay so margins can do those fun horizontal plots : marginsplot, horizontal unique xline(0) recast(scatter) yscale(reverse)

/*Spline*/
mkspline earnings_1000s_1 30 earnings_1000s_2 80 earnings_1000s_3 = earnings_1000s
browse earnings_1000s*

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur earnings_1000s_1 earnings_1000s_2 earnings_1000s_3 `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
logit dissolve_lag i.dur earnings_1000s_1 earnings_1000s_2 earnings_1000s_3 `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or

/* does earnings as a control affect other vars?*/
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur i.ft_head i.ft_wife earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
margins, dydx(ft_head ft_wife)
logit dissolve_lag i.dur i.ft_head i.ft_wife ib5.earnings_bucket  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
margins, dydx(ft_head ft_wife)
logit dissolve_lag i.dur i.ft_head i.ft_wife earnx1 earnx2 earnx3 earnx4  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
margins, dydx(ft_head ft_wife)

/* Do i need to demonstrate better model fit?*/
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"

* Linear
logit dissolve_lag i.dur earnings_1000s `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
est store linear_no
logit dissolve_lag i.dur earnings_1000s `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
est store linear_coll
logit dissolve_lag i.dur i.couple_educ_gp##c.earnings_1000s `controls' if inlist(IN_UNIT,1,2) & cohort==3 
est store linear_int

* Grouped
logit dissolve_lag i.dur i.couple_earnings_gp `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
est store gp_no
logit dissolve_lag i.dur i.couple_earnings_gp `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
est store gp_coll
logit dissolve_lag i.dur i.couple_educ_gp##i.couple_earnings_gp `controls' if inlist(IN_UNIT,1,2) & cohort==3 
est store gp_int

* Logged
logit dissolve_lag i.dur earnings_ln `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
est store log_no
logit dissolve_lag i.dur earnings_ln `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
est store log_coll
logit dissolve_lag i.dur i.couple_educ_gp##c.earnings_ln `controls' if inlist(IN_UNIT,1,2) & cohort==3 
est store log_int

* Spline
logit dissolve_lag i.dur knot1 knot2 knot3 `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
est store spline_no
logit dissolve_lag i.dur knot1 knot2 knot3 `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
est store spline_coll
logit dissolve_lag i.dur i.couple_educ_gp##(c.knot1 c.knot2 c.knot3) `controls' if inlist(IN_UNIT,1,2) & cohort==3, or
est store spline_int

est table linear_no gp_no log_no spline_no, stats(ll rank)
lrtest spline_no linear_no // no diff
lrtest spline_no gp_no // no diff
lrtest spline_no log_no // spline better fit than log
lrtest linear_no gp_no // no diff
lrtest linear_no log_no // same degrees of freedom so can't test
lrtest gp_no log_no // grouped is also better

est table linear_coll gp_coll log_coll spline_coll, stats(ll rank)
lrtest spline_coll linear_coll // no diff
lrtest spline_coll gp_coll // no diff
lrtest spline_coll log_coll // no diff
lrtest linear_coll gp_coll // no diff
lrtest linear_coll log_coll // same degrees of freedom so can't test
lrtest gp_coll log_coll // no diff

est table linear_int gp_int log_int spline_int, stats(ll rank)
lrtest spline_int linear_int // no diff
lrtest spline_int gp_int // no diff
lrtest spline_int log_int // better

********************************************************************************
* Comparing ADC across models
* I think this might be comparing across coefficients NOT AMEs
********************************************************************************
// Total Earnings
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur earnings_1000s `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
margins, dydx(earnings_1000s) post
mlincom 1, stat(est se p) decimal(6) clear

logit dissolve_lag i.dur earnings_1000s `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
margins, dydx(earnings_1000s) post
mlincom 1, stat(est se p) decimal(6) add

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logistic dissolve_lag i.dur i.couple_educ_gp earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3
est store a
logistic dissolve_lag i.dur i.couple_educ_gp##c.earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3
est store b

lrtest a b // .0017

// Paid work: Group
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logistic dissolve_lag i.dur i.hh_earn_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0
est store m1
margins, dydx(hh_earn_type) post
mlincom 1, stat(est se p) clear
mlincom 2, stat(est se p) add

logistic dissolve_lag i.dur i.hh_earn_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1
est store m2
margins, dydx(hh_earn_type) post
mlincom 1, stat(est se p) add
mlincom 2, stat(est se p) add

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logistic dissolve_lag i.dur i.hh_earn_type i.couple_educ_gp earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 // R=.0494
testparm i.hh_earn_type // 0.44
est store m3

logistic dissolve_lag i.dur i.hh_earn_type i.couple_educ_gp i.hh_earn_type#i.couple_educ_gp earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 // R=.0503
testparm i.hh_earn_type#i.couple_educ_gp  // 0.15
est store m4

estimates table m3 m4, stats(N ll chi2 aic bic r2_a)
lrtest m3 m4 // 0.17

// Paid work: employment
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logistic dissolve_lag i.dur ib3.couple_work earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0
margins, dydx(couple_work) post
mlincom 1, stat(est se p) clear
mlincom 2, stat(est se p) add
mlincom 3, stat(est se p) add

logistic dissolve_lag i.dur ib3.couple_work earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1
margins, dydx(couple_work) post
mlincom 1, stat(est se p) add
mlincom 2, stat(est se p) add
mlincom 3, stat(est se p) add

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logistic dissolve_lag i.dur i.couple_work i.couple_educ_gp earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3
testparm i.couple_work //  .18
est store m5

logistic dissolve_lag i.dur i.couple_work i.couple_educ_gp i.couple_work#i.couple_educ_gp earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 //
testparm i.couple_work#i.couple_educ_gp  // .26
est store m6

lrtest m5 m6 // 0.27

// Paid Work: Continuous earnings ratio
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur female_earn_pct earnings_1000s `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
margins, dydx(female_earn_pct) post
mlincom 1, stat(est se p) decimal(6) clear

logit dissolve_lag i.dur female_earn_pct earnings_1000s `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
margins, dydx(female_earn_pct) post
mlincom 1, stat(est se p) decimal(6) add

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logistic dissolve_lag i.dur i.couple_educ_gp female_earn_pct earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3
est store c
logistic dissolve_lag i.dur i.couple_educ_gp##c.female_earn_pct earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3
est store d

lrtest c d

// Paid Work: Continuous hours ratio
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur female_hours_pct earnings_1000s `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
margins, dydx(female_hours_pct) post
mlincom 1, stat(est se p) decimal(6) clear

logit dissolve_lag i.dur female_hours_pct earnings_1000s `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
margins, dydx(female_hours_pct) post
mlincom 1, stat(est se p) decimal(6) add

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logistic dissolve_lag i.dur i.couple_educ_gp female_hours_pct earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3
est store e
logistic dissolve_lag i.dur i.couple_educ_gp##c.female_hours_pct earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3
est store f

lrtest e f 


// Unpaid work: Continuous housework
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur wife_housework_pct earnings_1000s `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
margins, dydx(wife_housework_pct) post
mlincom 1, stat(est se p) decimal(6) clear

logit dissolve_lag i.dur wife_housework_pct earnings_1000s `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
margins, dydx(wife_housework_pct) post
mlincom 1, stat(est se p) decimal(6) add

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logistic dissolve_lag i.dur i.couple_educ_gp wife_housework_pct earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3
est store g
logistic dissolve_lag i.dur i.couple_educ_gp##c.wife_housework_pct earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3
est store h

lrtest g h

// Unpaid work: Group
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logistic dissolve_lag i.dur i.housework_bkt earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0
margins, dydx(housework_bkt) post
mlincom 1, stat(est se p) clear
mlincom 2, stat(est se p) add

logistic dissolve_lag i.dur i.housework_bkt earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1
margins, dydx(housework_bkt) post
mlincom 1, stat(est se p) add
mlincom 2, stat(est se p) add

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logistic dissolve_lag i.dur i.housework_bkt i.couple_educ_gp earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3
est store m7

logistic dissolve_lag i.dur i.housework_bkt i.couple_educ_gp i.housework_bkt#i.couple_educ_gp earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 
est store m8

lrtest m7 m8

/// max code
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logistic dissolve_lag i.dur i.hh_earn_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0
margins, dydx(2.hh_earn_type) post
estimates store est1

logistic dissolve_lag i.dur i.hh_earn_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0
margins, dydx(3.hh_earn_type) post
estimates store est2

logistic dissolve_lag i.dur i.hh_earn_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1
margins, dydx(2.hh_earn_type) post
estimates store est3

logistic dissolve_lag i.dur i.hh_earn_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1
margins, dydx(3.hh_earn_type) post
estimates store est4

wtmarg est1 est3 // effects of dual / male -  between coll and no coll
wtmarg est2 est4 // effects of dual / female

coefplot est1 est2 est3 est4,  drop(_cons) nolabel xline(0) levels(90)
gr_edit plotregion1._xylines[1].style.editstyle linestyle(color(dimgray)) editcopy

********************************************************************************
**Other tests of comparison
********************************************************************************

*********************** Attempting suest AND other things
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled cohab_with_wife cohab_with_other pre_marital_birth"
logistic dissolve_lag i.dur i.hh_earn_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0
est store m1

logistic dissolve_lag i.dur i.hh_earn_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1
est store m2

suest m1 m2
test[m1_dissolve_lag]3.hh_earn_type = [m2_dissolve_lag]3.hh_earn_type  // but this is comparing beta coefficient?
margins, dydx(hh_earn_type) predict(equation(m1_dissolve_lag))
margins, dydx(hh_earn_type) predict(equation(m2_dissolve_lag))

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled cohab_with_wife cohab_with_other pre_marital_birth i.region"
logistic dissolve_lag i.dur i.hh_earn_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, vce(robust)
// margins, dydx(*) vce(unconditional)  post  - needed varlist
margins, dydx(hh_earn_type) vce(unconditional)  post 
est store m1

logistic dissolve_lag i.dur i.hh_earn_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, vce(robust)
margins, dydx(hh_earn_type) vce(unconditional)  post
est store m2

suest m1 m2, vce(robust) // cannot get this to work with AMEs

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
gsem (dissolve_lag <- i.dur i.hh_earn_type earnings_1000s `controls' if inlist(IN_UNIT,1,2) & cohort==3, logit), group(couple_educ_gp) ginvariant(none)
margins, dydx(hh_earn_type) over(couple_educ_gp) nose post  // okay but this doesn't match anything??
margins hh_earn_type, over(couple_educ_gp) nose // so these are just different estimates, like simmilar, but different. WHY? because alt way of estimating? I am just confused because the models themselves match below...
* mlincom 3-4, detail - not working, oh probably because I don't include se
* test  _b[3.hh_earn_type:0bn.couple_educ_gp] = _b[3.hh_earn_type:1.couple_educ_gp] - nor is this, oh probably because I don't include se

// wait okay is this treating as linear? is this now essentially an LPM?! i am so confused..., so am I just comparing across coefficients? okay I think its because in the example, there are 2 models but here i just have one, estimated by group. so I need to specify the group? so is this just an interaction now??? 
* to compare
logit dissolve_lag i.dur i.hh_earn_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0
margins hh_earn_type
logit dissolve_lag i.dur i.hh_earn_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1
margins hh_earn_type

********************************************************************************************
**# INTERACTION for LR tests and alternate WALD TESTS
/* https://www.statalist.org/forums/forum/general-stata-discussion/general/1700308-comparing-difference-in-average-marginal-effects-ame-between-stratified-samples? */
********************************************************************************************
*1. Paid Work
qui logit dissolve_lag i.couple_educ_gp##(i.dur i.hh_earn_type c.earnings_1000s c.age_mar_wife c.age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth) if inlist(IN_UNIT,1,2) & cohort==3
estimates store earn

qui logit dissolve_lag i.couple_educ_gp i.dur i.hh_earn_type c.earnings_1000s c.age_mar_wife c.age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth if inlist(IN_UNIT,1,2) & cohort==3
estimates store earn_noint

qui logit dissolve_lag i.dur c.age_mar_wife c.age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth i.couple_educ_gp##(i.hh_earn_type c.earnings_1000s) if inlist(IN_UNIT,1,2) & cohort==3
estimates store earn_v2

estimates table earn earn_noint earn_v2, stats(N ll rank)
lrtest earn_noint earn_v2

*2. Employment
qui logit dissolve_lag i.couple_educ_gp##(i.dur i.ft_wife i.ft_head c.earnings_1000s c.age_mar_wife c.age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth) if inlist(IN_UNIT,1,2) & cohort==3
est store employ

qui logit dissolve_lag i.couple_educ_gp i.dur i.ft_wife i.ft_head c.earnings_1000s c.age_mar_wife c.age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth if inlist(IN_UNIT,1,2) & cohort==3, or
est store employ_noint

estimates table employ employ_noint, stats(N ll rank)

*3. Earnings
qui logit dissolve_lag i.couple_educ_gp##(i.dur c.earnings_1000s c.age_mar_wife c.age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth) if inlist(IN_UNIT,1,2) & cohort==3
estimates store earnings

qui logit dissolve_lag i.couple_educ_gp i.dur c.earnings_1000s c.age_mar_wife c.age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth if inlist(IN_UNIT,1,2) & cohort==3
estimates store earnings_noint

estimates table earnings earnings_noint, stats(N ll rank)

*4. Unpaid Work
qui logit dissolve_lag i.couple_educ_gp##(i.dur i.housework_bkt c.earnings_1000s c.age_mar_wife c.age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth) if inlist(IN_UNIT,1,2) & cohort==3
estimates store unpaid

qui logit dissolve_lag i.couple_educ_gp i.dur i.housework_bkt c.earnings_1000s c.age_mar_wife c.age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth if inlist(IN_UNIT,1,2) & cohort==3
estimates store unpaid_noint

estimates table unpaid unpaid_noint, stats(N ll rank)

estimates table earn earn_noint employ employ_noint earnings earnings_noint unpaid unpaid_noint, stats(N ll rank)

*5. All Focal variables
qui logit dissolve_lag i.couple_educ_gp i.dur i.ft_wife i.ft_head i.housework_bkt c.earnings_1000s c.age_mar_wife c.age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth if inlist(IN_UNIT,1,2) & cohort==3, or
est store full_noint

qui logit dissolve_lag i.couple_educ_gp##(i.ft_wife i.ft_head i.housework_bkt c.earnings_1000s) i.dur c.age_mar_wife c.age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth if inlist(IN_UNIT,1,2) & cohort==3, or
est store full

estimates table full full_noint, stats(N ll rank)
lrtest full full_noint

*6. Alt tests
qui logit dissolve_lag i.couple_educ_gp i.dur i.hh_earn_type c.earnings_1000s c.age_mar_wife c.age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth if inlist(IN_UNIT,1,2) & cohort==3, or
est store test_noint

logit dissolve_lag i.couple_educ_gp i.hh_earn_type i.couple_educ_gp#i.hh_earn_type c.earnings_1000s i.dur c.age_mar_wife c.age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth if inlist(IN_UNIT,1,2) & cohort==3, or
est store test

estimates table test test_noint, stats(N ll rank)
lrtest test_noint test

testparm i.couple_educ_gp#i.hh_earn_type

/* Validation
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur i.hh_earn_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0
margins, dydx(hh_earn_type)
logit dissolve_lag i.dur i.hh_earn_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1
margins, dydx(hh_earn_type)

logit dissolve_lag i.dur i.hh_earn_type##i.couple_educ_gp earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3
margins, dydx(hh_earn_type) over(couple_educ_gp) // so these are different to above, why? okay wait do I have to interact EVERYTHING to make it the same?

qui logit dissolve_lag i.couple_educ_gp##(i.dur i.hh_earn_type c.earnings_1000s c.age_mar_wife c.age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth) if inlist(IN_UNIT,1,2) & cohort==3 // okay so yes have to interact LITERALLY everything. okay so I also need to think about this more - let EVERYTHING vary by education or just variables of interest?! this feels conceptual GAH.
estimates store earn
margins, dydx(hh_earn_type) over(couple_educ_gp) post

di  _b[3.hh_earn_type:0bn.couple_educ_gp] -  _b[3.hh_earn_type:1.couple_educ_gp]
test  _b[3.hh_earn_type:0bn.couple_educ_gp] = _b[3.hh_earn_type:1.couple_educ_gp] // okay so this is exactly the same as when I did it by hand. is it because the covariance is actually 0? 
mlincom 3-4, detail // also the same. 

qui logit dissolve_lag i.couple_educ_gp i.dur i.hh_earn_type c.earnings_1000s c.age_mar_wife c.age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth if inlist(IN_UNIT,1,2) & cohort==3
estimates store earn_noint

lrtest earn earn_noint
estimates table earn earn_noint, stats(N ll chi2 aic bic r2_a rank)
//https://stats.oarc.ucla.edu/other/mult-pkg/faq/general/faqhow-are-the-likelihood-ratio-wald-and-lagrange-multiplier-score-tests-different-andor-similar/
// https://www.socscistatistics.com/pvalues/chidistribution.aspx
// https://people.richland.edu/james/lecture/m170/tbl-chi.html

/*
mlincom (1-2)-(3-4), detail

 ( 1)  [2.hh_earn_type]0bn.couple_educ_gp - [2.hh_earn_type]1.couple_educ_gp -
       [3.hh_earn_type]0bn.couple_educ_gp + [3.hh_earn_type]1.couple_educ_gp = 0
*/

****This should match what i did by hand in Wald Test Table
qui logit dissolve_lag i.couple_educ_gp##(i.dur i.ft_wife i.ft_head c.knot1 c.knot2 c.knot3 c.age_mar_wife c.age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth) if inlist(IN_UNIT,1,2) & cohort==3
est store employ
margins, dydx(ft_wife) over(couple_educ_gp) post
mlincom 1-2, detail

est restore employ
margins, dydx(ft_head) over(couple_educ_gp) post
mlincom 1-2, detail

qui logit dissolve_lag i.dur  c.age_mar_wife c.age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth i.couple_educ_gp##(i.ft_wife i.ft_head c.knot1 c.knot2 c.knot3) if inlist(IN_UNIT,1,2) & cohort==3
est store employ2
margins, dydx(ft_wife) over(couple_educ_gp) post
mlincom 1-2, detail

est restore employ2
margins, dydx(ft_head) over(couple_educ_gp) post
mlincom 1-2, detail

qui logit dissolve_lag i.couple_educ_gp i.dur i.ft_wife i.ft_head c.knot1 c.knot2 c.knot3 c.age_mar_wife c.age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth if inlist(IN_UNIT,1,2) & cohort==3, or
est store noint
lrtest employ2 noint 

estimates table employ noint, stats(N ll chi2 aic bic r2_a)
*/

********************************************************************************************
**# Alternate models: interaction, but only key variables - for AMEs and Wald Test
********************************************************************************************
*1. Employment
qui logit dissolve_lag i.dur c.age_mar_wife c.age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth i.couple_educ_gp##(i.ft_wife i.ft_head c.knot1 c.knot2 c.knot3) if inlist(IN_UNIT,1,2) & cohort==3
est store employ

margins ft_wife, over(couple_educ_gp)
margins ft_head, over(couple_educ_gp)

margins, dydx(ft_wife) over(couple_educ_gp) post
mlincom 1-2, detail

est restore employ
margins, dydx(ft_head) over(couple_educ_gp) post
mlincom 1-2, detail

est restore employ
margins, dydx(*) over(couple_educ_gp) post
outreg2 using "$results/dissolution_AMES_int.xls", dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

qui logit dissolve_lag i.couple_educ_gp i.dur i.ft_wife i.ft_head c.knot1 c.knot2 c.knot3 c.age_mar_wife c.age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth if inlist(IN_UNIT,1,2) & cohort==3, or
est store employ_noint
lrtest employ employ_noint 

estimates table employ employ_noint, stats(N ll chi2 aic bic rank)

*2. Paid Work Type
qui logit dissolve_lag i.dur c.age_mar_wife c.age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth i.couple_educ_gp##(i.hh_earn_type c.knot1 c.knot2 c.knot3) if inlist(IN_UNIT,1,2) & cohort==3
est store paid
margins hh_earn_type, over(couple_educ_gp) nofvlab

margins, dydx(hh_earn_type) over(couple_educ_gp) post
mlincom 1-2, detail
mlincom 3-4, detail
mlincom (1-2)-(3-4), detail

est restore paid
margins, dydx(*) over(couple_educ_gp) post
outreg2 using "$results/dissolution_AMES_int.xls", dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

qui logit dissolve_lag i.couple_educ_gp i.dur i.hh_earn_type c.knot1 c.knot2 c.knot3 c.age_mar_wife c.age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth if inlist(IN_UNIT,1,2) & cohort==3, or
est store paid_noint

lrtest paid paid_noint 

*3. Earnings
qui logit dissolve_lag i.dur c.age_mar_wife c.age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth i.couple_educ_gp##(c.knot1 c.knot2 c.knot3) if inlist(IN_UNIT,1,2) & cohort==3
est store earn
margins, dydx(knot2) over(couple_educ_gp) post
mlincom 1-2, detail

est restore earn
margins, dydx(knot3) over(couple_educ_gp) post
mlincom 1-2, detail

est restore earn
// margins, at(knot2=(0(10)20)) at(knot3=(0(10)100)) over(couple_educ_gp) nofvlab
margins, at(knot2=(0 20)) at(knot3=(0 30 70)) over(couple_educ_gp) post
mlincom (3-1), detail // ($20-0, no college)
mlincom (4-2), detail
mlincom (3-1)-(4-2), detail

mlincom (7-5), detail // ($50-20, no college)
mlincom (8-6), detail
mlincom (7-5)-(8-6), detail

mlincom (9-5), detail // ($100-20, no college)
mlincom (10-6), detail
mlincom (9-5)-(10-6), detail

est restore earn
margins, dydx(*) over(couple_educ_gp) post
outreg2 using "$results/dissolution_AMES_int.xls", dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

qui logit dissolve_lag i.couple_educ_gp i.dur c.knot1 c.knot2 c.knot3 c.age_mar_wife c.age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth if inlist(IN_UNIT,1,2) & cohort==3, or
est store earn_noint

lrtest earn earn_noint

*4. Unpaid Work Type
qui logit dissolve_lag i.dur c.age_mar_wife c.age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth i.couple_educ_gp##(i.housework_bkt c.knot1 c.knot2 c.knot3) if inlist(IN_UNIT,1,2) & cohort==3
est store unpaid
margins housework_bkt, over(couple_educ_gp) nofvlab

margins, dydx(housework_bkt) over(couple_educ_gp) post
mlincom 1-2, detail
mlincom 3-4, detail

est restore unpaid
margins, dydx(*) over(couple_educ_gp) post
outreg2 using "$results/dissolution_AMES_int.xls", dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

qui logit dissolve_lag i.couple_educ_gp i.dur i.housework_bkt c.knot1 c.knot2 c.knot3 c.age_mar_wife c.age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth if inlist(IN_UNIT,1,2) & cohort==3, or
est store unpaid_noint

lrtest unpaid unpaid_noint 

********************************************************************************************
*Other checks - DNU
********************************************************************************************

*********************** 
* Linear probability models?

/*okay, I think when event is "rare" - prob close to 0 or 1, these are less good? and divorce is relatively rare in this sample. See King and Zeng 2001*/

logit dissolve_lag i.dur i.hh_earn_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0
margins hh_earn_type
/*
Dual Earner  |   .0825762    .006471    12.76   0.000     .0698932    .0952592
    Male BW  |   .0703171   .0038959    18.05   0.000     .0626812    .0779531
  Female BW  |   .0698832   .0083233     8.40   0.000     .0535698    .0861966
*/
margins, dydx(hh_earn_type)
/*
    Male BW  |   -.012259   .0075724    -1.62   0.105    -.0271007    .0025826
  Female BW  |   -.012693   .0107307    -1.18   0.237    -.0337247    .0083387
  
  coefficients
      Male BW  |  -.1760859   .1057899    -1.66   0.096    -.3834304    .0312585
    Female BW  |  -.1828173   .1584446    -1.15   0.249     -.493363    .1277284

*/

reg dissolve_lag i.dur i.hh_earn_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0
margins hh_earn_type
/*
Dual Earner  |   .0732728   .0055579    13.18   0.000     .0623779    .0841678
    Male BW  |   .0682522   .0038325    17.81   0.000     .0607395    .0757649
  Female BW  |   .0731283   .0083644     8.74   0.000     .0567317    .0895249
*/
margins, dydx(hh_earn_type)
/*
    Male BW  |  -.0050206   .0067555    -0.74   0.457    -.0182632    .0082219
  Female BW  |  -.0001446   .0100474    -0.01   0.989    -.0198403    .0195512
  
  matches coefficients
  
      Male BW  |  -.0050206   .0067555    -0.74   0.457    -.0182632    .0082219
    Female BW  |  -.0001446   .0100474    -0.01   0.989    -.0198403    .0195512

*/

/*from gsem above
2.hh_earn_type        |
       couple_educ_gp |
     Neither College  |  -.0099786
At Least One College  |   .0013729
----------------------+----------------------------------------------------------------
3.hh_earn_type        |
       couple_educ_gp |
     Neither College  |  -.0092288
At Least One College  |   .0118725
*/

logit dissolve_lag i.dur i.hh_earn_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1
reg dissolve_lag i.dur i.hh_earn_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1

***********************
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logistic dissolve_lag i.dur i.housework_bkt earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0
margins, dydx(2.housework_bkt) post
estimates store est1

logistic dissolve_lag i.dur i.housework_bkt earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0
margins, dydx(3.housework_bkt) post
estimates store est2

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logistic dissolve_lag i.dur i.housework_bkt earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1
margins, dydx(2.housework_bkt) post
estimates store est3

logistic dissolve_lag i.dur i.housework_bkt earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1
margins, dydx(3.housework_bkt) post
estimates store est4

wtmarg est1 est3 // effects of dual / female -  between coll and no coll
wtmarg est2 est4 // effects of dual / male

coefplot est1 est2 est3 est4,  drop(_cons) nolabel xline(0) levels(90)

*************
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logistic dissolve_lag i.dur i.ft_head i.ft_wife earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0
margins, dydx(ft_head) post
estimates store est5

logistic dissolve_lag i.dur i.ft_head i.ft_wife earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1
margins, dydx(ft_head) post
estimates store est6

logistic dissolve_lag i.dur i.ft_head i.ft_wife earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0
margins, dydx(ft_wife) post
estimates store est7

logistic dissolve_lag i.dur i.ft_head i.ft_wife earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1
margins, dydx(ft_wife) post
estimates store est8

wtmarg est5 est6
wtmarg est7 est8

*************
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logistic dissolve_lag i.dur earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0
margins, dydx(earnings_1000s) post
estimates store est9

logistic dissolve_lag i.dur earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1
margins, dydx(earnings_1000s) post
estimates store est10

wtmarg est9 est10

coefplot matrix(DIFF), ci(CI) 

coefplot est9 est10
coefplot est9 est10,  drop(_cons) levels(95 90) 
coefplot est9 est10,  drop(_cons) nolabel xline(0)


/*
suest m1 m2
test [m1]hh_earn_type=[m2]hh_earn_type
margins, dydx(hh_earn_type) post
mlincom 1
mlincom 2

// mlincom 1-2, stat(est se p) add

clonevar    dissolve_no = dissolve_lag
lab var     dissolve_no "M1 dissolve no college"
clonevar    dissolve_cl = dissolve_lag
lab var     dissolve_cl "M1 dissolve college"

codebook dissolve*, compact

gsem (dissolve_no <- i.dur i.hh_earn_type earnings_1000s if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, logit) ///
     (dissolve_cl <- i.dur i.hh_earn_type earnings_1000s if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, logit)

*/

********************************************************************************
**# Predicted Probabilities
********************************************************************************
/* confused
logit dissolve_lag i.dur i.ft_head i.ft_wife knot1 knot2 knot3 i.no_earnings `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
margins, dydx(ft_head ft_wife)

logit dissolve_lag i.dur i.ft_head i.ft_wife earnings_1000s i.no_earnings `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
margins, dydx(ft_head ft_wife)
*/

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"

qui logit dissolve_lag i.dur knot1 knot2 knot3 `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
margins, dydx(knot2 knot3)
qui logit dissolve_lag i.dur knot1 knot2 knot3 `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
margins, dydx(knot2 knot3)

////////// No College \\\\\\\\\\\/
*1. Continuous earnings ratio
qui logit dissolve_lag i.dur female_earn_pct knot1 knot2 knot3  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
margins, at(female_earn_pct=(0(.25)1))

*2. Categorical indicator of Paid work
qui logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3   `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
margins hh_earn_type
margins, dydx(hh_earn_type)

*3A. Employment: no interaction
qui logit dissolve_lag i.dur i.ft_head i.ft_wife knot1 knot2 knot3 `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
margins ft_head
margins ft_wife
margins, dydx(ft_head ft_wife)

*3B. Employment: interaction
qui logit dissolve_lag i.dur ib3.couple_work knot1 knot2 knot3  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
margins couple_work

*4. Total earnings
qui logit dissolve_lag i.dur knot1 knot2 knot3 `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
margins, dydx(knot2 knot3)
margins, at(knot2=(0(10)20)) at(knot3=(0(10)100))

*5. Continuous Housework
qui logit dissolve_lag i.dur wife_housework_pct knot1 knot2 knot3 `controls' if inlist(IN_UNIT,1,2)  & cohort==3 & couple_educ_gp==0, or
margins, at(wife_housework_pct=(0(.25)1))

*6. Categorical Housework
qui logit dissolve_lag i.dur i.housework_bkt knot1 knot2 knot3  `controls' if inlist(IN_UNIT,1,2)  & cohort==3 & couple_educ_gp==0, or
margins housework_bkt
margins, dydx(housework_bkt)

////////// College \\\\\\\\\\\/
*1. Continuous earnings ratio
qui logit dissolve_lag i.dur female_earn_pct knot1 knot2 knot3 `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
margins, at(female_earn_pct=(0(.25)1))

*2. Categorical indicator of Paid work
qui logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
margins hh_earn_type
margins, dydx(hh_earn_type)

*3A. Employment: no interaction
qui logit dissolve_lag i.dur i.ft_head i.ft_wife knot1 knot2 knot3 `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
margins ft_head
margins ft_wife
margins, dydx(ft_head ft_wife)

*3B. Employment: interaction
qui logit dissolve_lag i.dur ib3.couple_work knot1 knot2 knot3  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
margins couple_work

*4. Total earnings
qui logit dissolve_lag i.dur knot1 knot2 knot3  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
margins, at(knot2=(0(10)20)) at(knot3=(0(10)100))

*5. Continuous Housework
qui logit dissolve_lag i.dur wife_housework_pct knot1 knot2 knot3 `controls' if inlist(IN_UNIT,1,2)  & cohort==3 & couple_educ_gp==1, or
margins, at(wife_housework_pct=(0(.25)1))

*6. Categorical Housework
qui logit dissolve_lag i.dur i.housework_bkt knot1 knot2 knot3  `controls' if inlist(IN_UNIT,1,2)  & cohort==3 & couple_educ_gp==1, or
margins housework_bkt
margins, dydx(housework_bkt)

********************************************************************************
* Figures
********************************************************************************
*Panel 1A
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logistic dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0 & hh_earn_type <4
margins, dydx(hh_earn_type) level(90) post
estimates store est1

logistic dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1 & hh_earn_type <4
margins, dydx(hh_earn_type) level(90) post
estimates store est2

coefplot est1 est2,  drop(_cons) nolabel xline(0) levels(90)

*Panel 1B
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logistic dissolve_lag i.dur i.ft_head 1.ft_wife knot1 knot2 knot3  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0
estimates store no_b
margins, dydx(ft_head) level(90) post
estimates store est3

estimates restore no_b
margins, dydx(ft_wife) level(90) post
estimates store est4

logistic dissolve_lag i.dur i.ft_head 1.ft_wife knot1 knot2 knot3  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1
estimates store coll_b
margins, dydx(ft_head) level(90) post
estimates store est5

estimates restore coll_b
margins, dydx(ft_wife) level(90) post
estimates store est6

coefplot est3 est4 est5 est6,  drop(_cons) nolabel xline(0) levels(90)

*Panel 1C
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logistic dissolve_lag i.dur i.housework_bkt knot1 knot2 knot3  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0 & housework_bkt <4
margins, dydx(housework_bkt) level(90) post
estimates store est7

logistic dissolve_lag i.dur i.housework_bkt knot1 knot2 knot3 `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1 & housework_bkt <4
margins, dydx(housework_bkt) level(90) post
estimates store est8

coefplot est7 est8,  drop(_cons) nolabel xline(0) levels(90)

coefplot est3 est4 est5 est6 est1 est2 est7 est8,  drop(_cons) nolabel xline(0) levels(90)

coefplot (est3, offset(.20) label(No College)) (est4, offset(.20) nokey) (est5, offset(-.20) label(College)) (est6, offset(-.20) nokey) (est1, offset(.20) nokey) (est2, offset(-.20) nokey) (est7, offset(.20) nokey) (est8, offset(-.20) nokey), drop(_cons 0.ft_head) xline(0) levels(90) base xtitle(Average Marginal Effect Relative to Reference Group) ///
coeflabels(1.hh_earn_type = "Dual-Earner" 2.hh_earn_type = "Male-Breadwinner" 3.hh_earn_type = "Female-Breadwinner" 1.ft_head = "Husband Employed FT" 1.ft_wife = "Wife Employed FT" 1.housework_bkt = "Dual-Housework" 2.housework_bkt = "Female-Housework" 3.housework_bkt = "Male-Housework") ///
 headings(1.ft_head= "{bf:Employment Status (M1)}"   1.hh_earn_type = "{bf:Paid Work Arrangement (M2)}"   1.housework_bkt = "{bf:Unpaid Work Arrangement (M4)}")
 
 // legend(off)
 // legend(order(1 "No College" 2 "College") rows(1))
// useful: https://repec.sowi.unibe.ch/stata/coefplot/labelling.html

********************************************************************************
********************************************************************************
**# For PAA Final Paper: supplemental analysis - education ref group
********************************************************************************
********************************************************************************
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"

********************************************************************************
////////// Her education \\\\\\\\\\\/
********************************************************************************
*** No College
*1. Continuous earnings ratio
logit dissolve_lag i.dur female_earn_pct knot1 knot2 knot3 `controls' if inlist(IN_UNIT,1,2) & cohort==3 & college_complete_wife==0, or
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", sideway stats(coef pval) label ctitle(Hers No 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*2. Categorical indicator of Paid work
logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & college_complete_wife==0, or
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", sideway stats(coef pval) label ctitle(Hers No 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*3A. Employment: no interaction
logit dissolve_lag i.dur i.ft_head i.ft_wife knot1 knot2 knot3  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & college_complete_wife==0, or
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", sideway stats(coef pval) label ctitle(Hers No 3a) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*3B. Employment: interaction
logit dissolve_lag i.dur ib3.couple_work knot1 knot2 knot3  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & college_complete_wife==0, or
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", sideway stats(coef pval) label ctitle(Hers No 3b) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*4. Total earnings
logit dissolve_lag i.dur knot1 knot2 knot3 `controls' if inlist(IN_UNIT,1,2) & cohort==3 & college_complete_wife==0, or
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", sideway stats(coef pval) label ctitle(Hers No 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*5. Continuous Housework
logit dissolve_lag i.dur wife_housework_pct knot1 knot2 knot3 `controls' if inlist(IN_UNIT,1,2)  & cohort==3 & college_complete_wife==0, or
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", sideway stats(coef pval) label ctitle(Hers No 5) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*6. Categorical Housework
logit dissolve_lag i.dur i.housework_bkt knot1 knot2 knot3 `controls' if inlist(IN_UNIT,1,2)  & cohort==3 & college_complete_wife==0, or
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", sideway stats(coef pval) label ctitle(Hers No 6) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*** College
*1. Continuous earnings ratio
logit dissolve_lag i.dur female_earn_pct knot1 knot2 knot3 `controls' if inlist(IN_UNIT,1,2) & cohort==3 & college_complete_wife==1, or
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", sideway stats(coef pval) label ctitle(Hers Coll 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*2. Categorical indicator of Paid work
logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & college_complete_wife==1, or
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", sideway stats(coef pval) label ctitle(Hers Coll 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*3A. Employment: no interaction
logit dissolve_lag i.dur i.ft_head i.ft_wife knot1 knot2 knot3  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & college_complete_wife==1, or
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", sideway stats(coef pval) label ctitle(Hers Coll 3a) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*3B. Employment: interaction
logit dissolve_lag i.dur ib3.couple_work knot1 knot2 knot3  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & college_complete_wife==1, or
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", sideway stats(coef pval) label ctitle(Hers Coll 3b) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*4. Total earnings
logit dissolve_lag i.dur knot1 knot2 knot3 `controls' if inlist(IN_UNIT,1,2) & cohort==3 & college_complete_wife==1, or
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", sideway stats(coef pval) label ctitle(Hers Coll 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*5. Continuous Housework
logit dissolve_lag i.dur wife_housework_pct knot1 knot2 knot3 `controls' if inlist(IN_UNIT,1,2)  & cohort==3 & college_complete_wife==1, or
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", sideway stats(coef pval) label ctitle(Hers Coll 5) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*6. Categorical Housework
logit dissolve_lag i.dur i.housework_bkt knot1 knot2 knot3 `controls' if inlist(IN_UNIT,1,2)  & cohort==3 & college_complete_wife==1, or
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", sideway stats(coef pval) label ctitle(Hers Coll 6) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

********************************************************************************
////////// His education \\\\\\\\\\\/
********************************************************************************
*** No College
*1. Continuous earnings ratio
logit dissolve_lag i.dur female_earn_pct knot1 knot2 knot3 `controls' if inlist(IN_UNIT,1,2) & cohort==3 & college_complete_head==0, or
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", sideway stats(coef pval) label ctitle(His No 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*2. Categorical indicator of Paid work
logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & college_complete_head==0, or
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", sideway stats(coef pval) label ctitle(His No 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*3A. Employment: no interaction
logit dissolve_lag i.dur i.ft_head i.ft_wife knot1 knot2 knot3  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & college_complete_head==0, or
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", sideway stats(coef pval) label ctitle(His No 3a) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*3B. Employment: interaction
logit dissolve_lag i.dur ib3.couple_work knot1 knot2 knot3  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & college_complete_head==0, or
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", sideway stats(coef pval) label ctitle(His No 3b) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*4. Total earnings
logit dissolve_lag i.dur knot1 knot2 knot3 `controls' if inlist(IN_UNIT,1,2) & cohort==3 & college_complete_head==0, or
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", sideway stats(coef pval) label ctitle(His No 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*5. Continuous Housework
logit dissolve_lag i.dur wife_housework_pct knot1 knot2 knot3 `controls' if inlist(IN_UNIT,1,2)  & cohort==3 & college_complete_head==0, or
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", sideway stats(coef pval) label ctitle(His No 5) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*6. Categorical Housework
logit dissolve_lag i.dur i.housework_bkt knot1 knot2 knot3 `controls' if inlist(IN_UNIT,1,2)  & cohort==3 & college_complete_head==0, or
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", sideway stats(coef pval) label ctitle(His No 6) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*** College
*1. Continuous earnings ratio
logit dissolve_lag i.dur female_earn_pct knot1 knot2 knot3 `controls' if inlist(IN_UNIT,1,2) & cohort==3 & college_complete_head==1, or
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", sideway stats(coef pval) label ctitle(His Coll 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*2. Categorical indicator of Paid work
logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & college_complete_head==1, or
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", sideway stats(coef pval) label ctitle(His Coll 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*3A. Employment: no interaction
logit dissolve_lag i.dur i.ft_head i.ft_wife knot1 knot2 knot3  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & college_complete_head==1, or
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", sideway stats(coef pval) label ctitle(His Coll 3a) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*3B. Employment: interaction
logit dissolve_lag i.dur ib3.couple_work knot1 knot2 knot3  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & college_complete_head==1, or
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", sideway stats(coef pval) label ctitle(His Coll 3b) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*4. Total earnings
logit dissolve_lag i.dur knot1 knot2 knot3 `controls' if inlist(IN_UNIT,1,2) & cohort==3 & college_complete_head==1, or
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", sideway stats(coef pval) label ctitle(His Coll 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*5. Continuous Housework
logit dissolve_lag i.dur wife_housework_pct knot1 knot2 knot3 `controls' if inlist(IN_UNIT,1,2)  & cohort==3 & college_complete_head==1, or
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", sideway stats(coef pval) label ctitle(His Coll 5) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*6. Categorical Housework
logit dissolve_lag i.dur i.housework_bkt knot1 knot2 knot3 `controls' if inlist(IN_UNIT,1,2)  & cohort==3 & college_complete_head==1, or
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", sideway stats(coef pval) label ctitle(His Coll 6) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_educ_supp.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

********************************************************************************
********************************************************************************
* For PAA Final Paper: supplemental analysis - alternate indicators
********************************************************************************
********************************************************************************
/* Note: 5/9/23, I have moved most of these up to the main analysis */

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"

////////// No College \\\\\\\\\\\/
**Paid hours (instead of earnings)
logit dissolve_lag i.dur female_hours_pct earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
outreg2 using "$results/psid_marriage_dissolution_supp.xls", sideway stats(coef pval) label ctitle(Paid No+) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_supp.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

logit dissolve_lag i.dur i.hh_hours_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
outreg2 using "$results/psid_marriage_dissolution_supp.xls", sideway stats(coef pval) label ctitle(Paid No+) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_supp.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

**Interaction
logit dissolve_lag i.dur ib5.earn_type_hw earnings_1000s `controls' if inlist(IN_UNIT,1,2)  & cohort==3 & couple_educ_gp==0, or
outreg2 using "$results/dissolution_margins_alt.xls", sideway stats(coef pval) label ctitle(Both No+) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/dissolution_margins_alt.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

margins earn_type_hw, pwcompare
margins earn_type_hw, pwcompare(group)
margins earn_type_hw, pwcompare level(90)
margins earn_type_hw, pwcompare(group) level(90)


/*
** Continuous earnings ratio - no total earnings
logit dissolve_lag i.dur female_earn_pct `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
outreg2 using "$results/psid_marriage_dissolution_supp.xls", sideway stats(coef pval) label ctitle(Earnings No) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_supp.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

**Employment  - no total earnings
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur i.ft_head i.ft_wife `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
outreg2 using "$results/psid_marriage_dissolution_supp.xls", sideway stats(coef pval) label ctitle(Employment No) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_supp.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

**Continuous Housework  - no total earnings
logit dissolve_lag i.dur wife_housework_pct `controls' if inlist(IN_UNIT,1,2)  & cohort==3 & couple_educ_gp==0, or
outreg2 using "$results/psid_marriage_dissolution_supp.xls", sideway stats(coef pval) label ctitle(Unpaid No) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_supp.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

**Paid hours (instead of earnings)  - no total earnings
logit dissolve_lag i.dur female_hours_pct  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
outreg2 using "$results/psid_marriage_dissolution_supp.xls", sideway stats(coef pval) label ctitle(Paid No) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_supp.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

logit dissolve_lag i.dur i.hh_hours_type  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
outreg2 using "$results/psid_marriage_dissolution_supp.xls", sideway stats(coef pval) label ctitle(Paid No) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_supp.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)
*/

////////// College \\\\\\\\\\\/

**Paid hours (instead of earnings)
logit dissolve_lag i.dur female_hours_pct earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
outreg2 using "$results/psid_marriage_dissolution_supp.xls", sideway stats(coef pval) label ctitle(Paid Coll) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_supp.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

logit dissolve_lag i.dur i.hh_hours_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
outreg2 using "$results/psid_marriage_dissolution_supp.xls", sideway stats(coef pval) label ctitle(Paid Coll) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_supp.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

**Interaction
logit dissolve_lag i.dur ib5.earn_type_hw earnings_1000s `controls' if inlist(IN_UNIT,1,2)  & cohort==3 & couple_educ_gp==1, or
outreg2 using "$results/dissolution_margins_alt.xls", sideway stats(coef pval) label ctitle(Both Coll+) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/dissolution_margins_alt.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

margins earn_type_hw, pwcompare
margins earn_type_hw, pwcompare(group)
margins earn_type_hw, pwcompare level(90)
margins earn_type_hw, pwcompare(group) level(90)

**# End supplemental analyses

********************************************************************************
/* weights?!*/
********************************************************************************

browse survey_yr COR_IMM_WT_ CORE_WEIGHT_ CROSS_SECTION_FAM_WT_ LONG_WT_ CROSS_SECTION_WT_
gen weight = .
replace weight = CORE_WEIGHT_ if survey_yr<=1992
replace weight = COR_IMM_WT_ if survey_yr>=1993
browse survey_yr COR_IMM_WT_ CORE_WEIGHT_ weight CROSS_SECTION_FAM_WT_ LONG_WT_ CROSS_SECTION_WT_
 
*rescale?
gen weight_rescale=.
forvalues y=1968/2019{
	capture summarize weight if survey_yr==`y'
	capture local rescalefactor`y' `r(N)'/`r(sum)'
	capture replace weight_rescale = weight*`rescalefactor`y'' if survey_yr==`y'
	summarize weight_rescale
	
}

browse survey_yr weight weight_rescale

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur i.hh_earn_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
margins, dydx(hh_earn_type)
/*
    Male BW  |   -.010365   .0075017    -1.38   0.167    -.0250681     .004338
  Female BW  |   -.009588   .0107638    -0.89   0.373    -.0306847    .0115087
*/

logit dissolve_lag i.dur i.hh_earn_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0 [pw=weight], or
margins, dydx(hh_earn_type)
/*
    Male BW  |  -.0165746   .0088763    -1.87   0.062    -.0339718    .0008226
  Female BW  |  -.0254233    .011351    -2.24   0.025    -.0476708   -.0031757
*/

logit dissolve_lag i.dur i.hh_earn_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0 [pw=weight_rescale], or
margins, dydx(hh_earn_type)
/* 
   Male BW  |  -.0161444   .0089422    -1.81   0.071    -.0336709    .0013821
  Female BW  |  -.0268314   .0112764    -2.38   0.017    -.0489327     -.00473
*/

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur i.hh_earn_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
margins, dydx(hh_earn_type)
/*
    Male BW  |   .0014146   .0048482     0.29   0.770    -.0080877     .010917
  Female BW  |    .012233   .0073427     1.67   0.096    -.0021583    .0266244
*/

logit dissolve_lag i.dur i.hh_earn_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1 [pw=weight], or
margins, dydx(hh_earn_type)
/*
    Male BW  |  -.0053786    .005321    -1.01   0.312    -.0158076    .0050505
  Female BW  |    .009137   .0085956     1.06   0.288    -.0077101     .025984
*/

logit dissolve_lag i.dur i.hh_earn_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1 [pw=weight_rescale], or
margins, dydx(hh_earn_type)
/*
    Male BW  |  -.0052945   .0052302    -1.01   0.311    -.0155456    .0049565
  Female BW  |    .010723   .0088483     1.21   0.226    -.0066195    .0280654
*/

*rescale BY education?
gen weight_rescale_no=.
forvalues y=1968/2019{
	capture summarize weight if survey_yr==`y' & couple_educ_gp==0
	capture local rescalefactor`y' `r(N)'/`r(sum)'
	capture replace weight_rescale_no = weight*`rescalefactor`y'' if survey_yr==`y' & couple_educ_gp==0
	summarize weight_rescale_no if couple_educ_gp==0
	
}

gen weight_rescale_coll=.
forvalues y=1968/2019{
	capture summarize weight if survey_yr==`y' & couple_educ_gp==1
	capture local rescalefactor`y' `r(N)'/`r(sum)'
	capture replace weight_rescale_coll = weight*`rescalefactor`y'' if survey_yr==`y' & couple_educ_gp==1
	summarize weight_rescale_coll if couple_educ_gp==1
	
}

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur i.hh_earn_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
margins, dydx(hh_earn_type)
logit dissolve_lag i.dur i.hh_earn_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0 [pw=weight_rescale_no], or
margins, dydx(hh_earn_type)
/*
    Male BW  |  -.0160083    .008972    -1.78   0.074    -.0335931    .0015766
  Female BW  |  -.0270901   .0112719    -2.40   0.016    -.0491826   -.0049977
*/

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur i.hh_earn_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
margins, dydx(hh_earn_type)
logit dissolve_lag i.dur i.hh_earn_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1 [pw=weight_rescale_coll], or
margins, dydx(hh_earn_type)
/*
    Male BW  |  -.0051513   .0052537    -0.98   0.327    -.0154484    .0051458
  Female BW  |   .0103768   .0087921     1.18   0.238    -.0068554     .027609
*/

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur i.hh_earn_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0 [pw=weight_rescale]
margins, dydx(hh_earn_type)
logit dissolve_lag i.dur i.hh_earn_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1 [pw=weight_rescale]
margins, dydx(hh_earn_type)

qui logit dissolve_lag i.couple_educ_gp##(i.dur i.hh_earn_type c.earnings_1000s c.age_mar_wife c.age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth) if inlist(IN_UNIT,1,2) & cohort==3 [pw=weight_rescale] // okay so yes have to interact LITERALLY everything. okay so I also need to think about this more - let EVERYTHING vary by education or just variables of interest?! this feels conceptual GAH.
margins, dydx(hh_earn_type) over(couple_educ_gp) post

di  _b[3.hh_earn_type:0bn.couple_educ_gp] -  _b[3.hh_earn_type:1.couple_educ_gp]
test  _b[3.hh_earn_type:0bn.couple_educ_gp] = _b[3.hh_earn_type:1.couple_educ_gp] // okay so this is exactly the same as when I did it by hand. is it because the covariance is actually 0? 
mlincom 3-4, detail // okay so this is even MORE sig with the weights

di  _b[2.hh_earn_type:0bn.couple_educ_gp] -  _b[2.hh_earn_type:1.couple_educ_gp]
test  _b[2.hh_earn_type:0bn.couple_educ_gp] = _b[2.hh_earn_type:1.couple_educ_gp] 
mlincom 1-2, detail // despite the significance for the less educated, the cross-group diffs are still not sig at all.

** IM SO CONFUSED

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur i.ft_wife i.ft_head earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0
margins, dydx(ft_wife)
margins, dydx(ft_head)

logit dissolve_lag i.dur i.ft_wife i.ft_head earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0 [pw=weight_rescale] // okay so these change less than above
margins, dydx(ft_wife)
margins, dydx(ft_head)

logit dissolve_lag i.dur i.ft_wife i.ft_head earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1
margins, dydx(ft_wife)
margins, dydx(ft_head)

logit dissolve_lag i.dur i.ft_wife i.ft_head earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1 [pw=weight_rescale]
margins, dydx(ft_wife) // this is now sig at p<0.05
margins, dydx(ft_head)

qui logit dissolve_lag i.couple_educ_gp##(i.dur i.ft_wife i.ft_head c.earnings_1000s c.age_mar_wife c.age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth) if inlist(IN_UNIT,1,2) & cohort==3 [pw=weight_rescale] 
est store employ
margins, dydx(ft_wife) over(couple_educ_gp) post
mlincom 1-2, detail

est restore employ
margins, dydx(ft_head) over(couple_educ_gp) post
mlincom 1-2, detail // k p is now .10

/* test
summarize weight if survey_yr==1990
local rescalefactor1990 `r(N)'/`r(sum)'
display `rescalefactor1990'
replace weight_rescale = weight*`rescalefactor1990' if survey_yr==1990
summarize weight if survey_yr==1990
summarize weight_rescale if survey_yr==1990
*/
 
********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
* For PAA Extended Abstract
********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
** No College
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"

logit dissolve_lag i.dur female_hours_pct if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or //  continuous paid hours - discrete time
outreg2 using "$results/psid_marriage_dissolution_nocoll_paa.xls", sideway stats(coef pval) label ctitle(1 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace
logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or //  continuous paid hours - discrete time
outreg2 using "$results/psid_marriage_dissolution_nocoll_paa.xls", sideway stats(coef pval) label ctitle(1 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_hours_type if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_nocoll_paa.xls", sideway stats(coef pval) label ctitle(2 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.hh_hours_type TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_nocoll_paa.xls", sideway stats(coef pval) label ctitle(2 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur wife_housework_pct if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_nocoll_paa.xls", sideway stats(coef pval) label ctitle(3 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur wife_housework_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_nocoll_paa.xls", sideway stats(coef pval) label ctitle(3 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.housework_bkt if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_nocoll_paa.xls", sideway stats(coef pval) label ctitle(4 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.housework_bkt TAXABLE_HEAD_WIFE_ `controls' if inlist(IN_UNIT,1,2)  & cohort==3 & couple_educ_gp==0, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_nocoll_paa.xls", sideway stats(coef pval) label ctitle(4 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur female_earn_pct if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or //  continuous earnings
outreg2 using "$results/psid_marriage_dissolution_nocoll_paa.xls", sideway stats(coef pval) label ctitle(5 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur female_earn_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or //  continuous earnings
outreg2 using "$results/psid_marriage_dissolution_nocoll_paa.xls", sideway stats(coef pval) label ctitle(5 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_earn_type if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or // earnings - bucketed
outreg2 using "$results/psid_marriage_dissolution_nocoll_paa.xls", sideway stats(coef pval) label ctitle(6 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.hh_earn_type TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or // earnings - bucketed
outreg2 using "$results/psid_marriage_dissolution_nocoll_paa.xls", sideway stats(coef pval) label ctitle(6 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.ft_head i.ft_wife if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or // employment
outreg2 using "$results/psid_marriage_dissolution_nocoll_paa.xls", sideway stats(coef pval) label ctitle(7 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.ft_head i.ft_wife TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or // employment
outreg2 using "$results/psid_marriage_dissolution_nocoll_paa.xls", sideway stats(coef pval) label ctitle(7 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hours_diff_bkt if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or // dissimilarity - paid hoUrs
outreg2 using "$results/psid_marriage_dissolution_nocoll_paa.xls", sideway stats(coef pval) label ctitle(8 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.hours_diff_bkt TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or // dissimilarity - paid hoUrs
outreg2 using "$results/psid_marriage_dissolution_nocoll_paa.xls", sideway stats(coef pval) label ctitle(8 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hw_diff_bkt if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or // dissimilarity - UNpaid hoUrs
outreg2 using "$results/psid_marriage_dissolution_nocoll_paa.xls", sideway stats(coef pval) label ctitle(9 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.hw_diff_bkt TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or // dissimilarity - UNpaid hoUrs
outreg2 using "$results/psid_marriage_dissolution_nocoll_paa.xls", sideway stats(coef pval) label ctitle(9 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hours_type_hw if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or // combo
outreg2 using "$results/psid_marriage_dissolution_nocoll_paa.xls", sideway stats(coef pval) label ctitle(10 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.hours_type_hw TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or // dcombo
outreg2 using "$results/psid_marriage_dissolution_nocoll_paa.xls", sideway stats(coef pval) label ctitle(10 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled REGION_ cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur i.housework_bkt TAXABLE_HEAD_WIFE_ `controls' if inlist(IN_UNIT,1,2)  & cohort==3 & couple_educ_gp==0, or // housework - bucketed
margins housework_bkt

* margins for charts
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled REGION_ cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or //  continuous paid hours - discrete time
margins, at(female_hours_pct=(0 (.1) 1 ))

logit dissolve_lag i.dur female_earn_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or //  continuous earnings
margins, at(female_earn_pct=(0 (.1) 1 ))

logit dissolve_lag i.dur wife_housework_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or //  continuous housework
margins, at(wife_housework_pct=(0 (.1) 1 ))

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled REGION_ cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur c.female_hours_pct##c.wife_housework_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or // interaction
margins, at(wife_housework_pct=(0 (.25) 1 ) female_hours_pct=(0 (.25) 1 ))
marginsplot

logit dissolve_lag i.dur i.hh_hours_type##i.housework_bkt TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0 & housework_bkt <4, or // interaction
margins hh_hours_type#housework_bkt
marginsplot

* Splitting into who has degree
logit dissolve_lag i.dur i.no_college_bkd if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==0, or // okay so no differences here
logit dissolve_lag i.dur ib3.no_college_bkd if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==0, or

** College
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled REGION_ cohab_with_wife cohab_with_other pre_marital_birth"

logit dissolve_lag i.dur female_hours_pct if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or //  continuous paid hours - discrete time
outreg2 using "$results/psid_marriage_dissolution_college_paa.xls", sideway stats(coef pval) label ctitle(1 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace
logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or //  continuous paid hours - discrete time
outreg2 using "$results/psid_marriage_dissolution_college_paa.xls", sideway stats(coef pval) label ctitle(1 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled REGION_ cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or //  continuous paid hours - discrete time
margins, at(female_hours_pct=(0 (.1) 1 ))


logit dissolve_lag i.dur i.hh_hours_type if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_college_paa.xls", sideway stats(coef pval) label ctitle(2 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.hh_hours_type TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_college_paa.xls", sideway stats(coef pval) label ctitle(2 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur wife_housework_pct if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_college_paa.xls", sideway stats(coef pval) label ctitle(3 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur wife_housework_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_college_paa.xls", sideway stats(coef pval) label ctitle(3 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.housework_bkt if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_college_paa.xls", sideway stats(coef pval) label ctitle(4 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.housework_bkt TAXABLE_HEAD_WIFE_ `controls' if inlist(IN_UNIT,1,2)  & cohort==3 & couple_educ_gp==1, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_college_paa.xls", sideway stats(coef pval) label ctitle(4 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur female_earn_pct if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or //  continuous earnings
outreg2 using "$results/psid_marriage_dissolution_college_paa.xls", sideway stats(coef pval) label ctitle(5 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur female_earn_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or //  continuous earnings
outreg2 using "$results/psid_marriage_dissolution_college_paa.xls", sideway stats(coef pval) label ctitle(5 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_earn_type if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or // earnings - bucketed
outreg2 using "$results/psid_marriage_dissolution_college_paa.xls", sideway stats(coef pval) label ctitle(6 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.hh_earn_type TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or // earnings - bucketed
outreg2 using "$results/psid_marriage_dissolution_college_paa.xls", sideway stats(coef pval) label ctitle(6 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled REGION_ cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur ib3.hh_earn_type TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or // earnings - bucketed
margins hh_earn_type

logit dissolve_lag i.dur i.ft_head i.ft_wife if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or // employment
outreg2 using "$results/psid_marriage_dissolution_college_paa.xls", sideway stats(coef pval) label ctitle(7 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.ft_head i.ft_wife TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or // employment
outreg2 using "$results/psid_marriage_dissolution_college_paa.xls", sideway stats(coef pval) label ctitle(7 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hours_diff_bkt if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or // dissimilarity - paid hoUrs
outreg2 using "$results/psid_marriage_dissolution_college_paa.xls", sideway stats(coef pval) label ctitle(8 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.hours_diff_bkt TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or // dissimilarity - paid hoUrs
outreg2 using "$results/psid_marriage_dissolution_college_paa.xls", sideway stats(coef pval) label ctitle(8 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hw_diff_bkt if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or // dissimilarity - UNpaid hoUrs
outreg2 using "$results/psid_marriage_dissolution_college_paa.xls", sideway stats(coef pval) label ctitle(9 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.hw_diff_bkt TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or // dissimilarity - UNpaid hoUrs
outreg2 using "$results/psid_marriage_dissolution_college_paa.xls", sideway stats(coef pval) label ctitle(9 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hours_type_hw if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or // combo
outreg2 using "$results/psid_marriage_dissolution_college_paa.xls", sideway stats(coef pval) label ctitle(10 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.hours_type_hw TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or // dcombo
outreg2 using "$results/psid_marriage_dissolution_college_paa.xls", sideway stats(coef pval) label ctitle(10 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* margins for charts
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled REGION_ cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or //  continuous paid hours - discrete time
margins, at(female_hours_pct=(0 (.1) 1 ))

logit dissolve_lag i.dur female_earn_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or //  continuous earnings
margins, at(female_earn_pct=(0 (.1) 1 ))

logit dissolve_lag i.dur wife_housework_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or //  continuous housework
margins, at(wife_housework_pct=(0 (.1) 1 ))

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled REGION_ cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur c.female_hours_pct##c.wife_housework_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or // interaction
margins, at(wife_housework_pct=(0 (.25) 1 ) female_hours_pct=(0 (.25) 1 ))
marginsplot

logit dissolve_lag i.dur i.hh_hours_type##i.housework_bkt TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1 & housework_bkt <4 & hh_hours_type <4, or // interaction
margins hh_hours_type#housework_bkt
marginsplot

********************************************************************************
* Structural factors (do these need to BE MULTI-LEVEL??)
********************************************************************************
merge m:1 STATE_ using "$temp\state_division.dta"
drop _merge
merge m:1 survey_yr division using "$temp\gss_region_year.dta", keepusing(no_gender_egal no_working_mom_egal coll_gender_egal coll_working_mom_egal all_gender_egal all_working_mom_egal)
drop if _merge==2
drop _merge
merge m:1 survey_yr STATE_ using "$temp\state_min_wage.dta", keepusing(min_wage above_fed combined_fed federal)
drop if _merge==2
drop _merge

* Paid leave
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled REGION_ cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_  `controls' if cohort==3 & couple_educ_gp==1 & time_leave==0, or
logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_  `controls' if cohort==3 & couple_educ_gp==1 & time_leave==1, or
logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_  `controls' if cohort==3 & couple_educ_gp==1 & paid_leave_state==0, or
logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_  `controls' if cohort==3 & couple_educ_gp==1 & paid_leave_state==1, or
logit dissolve_lag i.dur c.female_hours_pct##i.paid_leave_state TAXABLE_HEAD_WIFE_  `controls' if cohort==3 & couple_educ_gp==1, or
margins paid_leave_state, at(female_hours_pct=(0(.25)1)) // this is kind of interesting
marginsplot

logit dissolve_lag i.dur c.female_hours_pct##i.paid_leave_state TAXABLE_HEAD_WIFE_  `controls' if cohort==3 & couple_educ_gp==0, or
margins paid_leave_state, at(female_hours_pct=(0(.25)1)) // okay so WAY less dramatic than college-educated.
marginsplot

logit dissolve_lag i.dur c.female_hours_pct##i.time_leave TAXABLE_HEAD_WIFE_  `controls' i.STATE_ if cohort==3 & couple_educ_gp==1, or
margins time_leave, at(female_hours_pct=(0(.25)1))
marginsplot

* chart
logit dissolve_lag i.dur c.female_hours_pct##i.time_leave TAXABLE_HEAD_WIFE_ survey_yr `controls' i.STATE_ i.cohort all_gender_egal if couple_educ_gp==1, or // okay this interesting also - more dramatic in non-paid-leave states
margins time_leave, at(female_hours_pct=(0(.25)1))
marginsplot

logit dissolve_lag i.dur i.paid_leave_state i.ft_head i.ft_wife i.ft_head#i.paid_leave_state i.ft_wife#i.paid_leave_state TAXABLE_HEAD_WIFE_  `controls' if couple_educ_gp==1 & cohort==3, or // okay ft_head here is ALSO interesting
margins paid_leave_state#ft_wife
marginsplot

margins paid_leave_state#ft_head
marginsplot

* chart
logit dissolve_lag i.dur c.female_hours_pct##i.time_leave TAXABLE_HEAD_WIFE_  `controls' i.STATE_ all_gender_egal i.cohort if couple_educ_gp==0, or // though is also true for less-educated
margins time_leave, at(female_hours_pct=(0(.25)1))
marginsplot

* chart
logit dissolve_lag i.dur c.wife_housework_pct##i.time_leave TAXABLE_HEAD_WIFE_  `controls' i.STATE_ all_gender_egal i.cohort if couple_educ_gp==0, or
margins time_leave, at(wife_housework_pct=(0(.25)1))
marginsplot

* chart
logit dissolve_lag i.dur c.wife_housework_pct##i.time_leave TAXABLE_HEAD_WIFE_  `controls' i.STATE_ all_gender_egal i.cohort if couple_educ_gp==1, or
margins time_leave, at(wife_housework_pct=(0(.25)1))
marginsplot


logit dissolve_lag i.dur c.female_hours_pct##i.paid_leave_state TAXABLE_HEAD_WIFE_  `controls' if cohort==3, or
margins paid_leave_state, at(female_hours_pct=(0(.25)1)) // this is kind of interesting
logit dissolve_lag i.dur c.female_hours_pct##i.time_leave TAXABLE_HEAD_WIFE_  `controls' if cohort==3, or
margins time_leave, at(female_hours_pct=(0(.25)1))

*Min wage
logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_  `controls' if cohort==3 & couple_educ_gp==1 & above_fed==0, or
logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_  `controls' if cohort==3 & couple_educ_gp==1 & above_fed==1, or

logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_  `controls' if cohort==3 & couple_educ_gp==0 & above_fed==0, or // wait okay this is wild - when minimum wage is not above federal, her earnings are not associated - but when they ARE (below) - they have a negative association!
logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_  `controls' if cohort==3 & couple_educ_gp==0 & above_fed==1, or

* chart
logit dissolve_lag i.dur c.female_hours_pct##i.above_fed TAXABLE_HEAD_WIFE_ all_gender_egal `controls' STATE_ if couple_educ_gp==0 & cohort==3, or // more dramatic when I control for attitudes
margins above_fed, at(female_hours_pct=(0(.25)1))
marginsplot

/*
gen combined_fed2 = combined_fed + 1
logit dissolve_lag i.dur c.female_hours_pct##i.combined_fed2 TAXABLE_HEAD_WIFE_ all_gender_egal `controls' STATE_ if couple_educ_gp==0 & cohort==3, or // more dramatic when I control for attitudes
margins combined_fed2, at(female_hours_pct=(0(.25)1))
marginsplot
*/

melogit dissolve_lag i.dur c.female_hours_pct##i.above_fed TAXABLE_HEAD_WIFE_ all_gender_egal if couple_educ_gp==0 & cohort==3 || STATE_:, or // do I need multilevel models?? seems very similar
margins above_fed, at(female_hours_pct=(0(.25)1))
marginsplot

* chart
logit dissolve_lag i.dur c.female_hours_pct##i.above_fed TAXABLE_HEAD_WIFE_ all_gender_egal `controls' STATE_ if couple_educ_gp==1 & cohort==3, or // AND min wage does not matter for college-educated.
margins above_fed, at(female_hours_pct=(0(.25)1))
marginsplot

logit dissolve_lag i.dur c.female_earn_pct##i.above_fed TAXABLE_HEAD_WIFE_  `controls' all_gender_egal STATE_ if couple_educ_gp==0 & cohort==3, or // true for earnings, but hours seems slightly more dramatic
margins above_fed, at(female_earn_pct=(0(.25)1))
marginsplot

logit dissolve_lag i.dur c.female_earn_pct##i.above_fed TAXABLE_HEAD_WIFE_  `controls' STATE_ if couple_educ_gp==1 & cohort==3, or // AND min wage does not matter for college-educated.
margins above_fed, at(female_earn_pct=(0(.25)1))
marginsplot

* chart
logit dissolve_lag i.dur c.wife_housework_pct##i.above_fed TAXABLE_HEAD_WIFE_  `controls' STATE_ all_gender_egal if couple_educ_gp==0 & cohort==3, or // housework is opposite trend but not sig
margins above_fed, at(wife_housework_pct=(0(.25)1))
marginsplot

* chart
logit dissolve_lag i.dur c.wife_housework_pct##i.above_fed TAXABLE_HEAD_WIFE_  `controls' STATE_  all_gender_egal if couple_educ_gp==1 & cohort==3, or // not sig, but almost implies opposite - like when min wage high, she should not do all of the housework
margins above_fed, at(wife_housework_pct=(0(.25)1))
marginsplot

** attitudes
// tabstat all_gender_egal, by(above_fed) -- yeah so attitudes correlated with min wage states, ofc
sum all_gender_egal
gen gender_egal_mean=.
replace gender_egal_mean=0 if all_gender_egal <= `r(mean)'
replace gender_egal_mean=1 if all_gender_egal > `r(mean)' & all_gender_egal!=.

logit dissolve_lag i.dur c.female_hours_pct##c.all_gender_egal TAXABLE_HEAD_WIFE_  `controls' STATE_ above_fed if couple_educ_gp==0 & cohort==3, or
margins, at(female_hours_pct=(0(.25)1) all_gender_egal=(.30(.1).80)) // think this indicates attitudes not as important?
marginsplot

logit dissolve_lag i.dur c.female_hours_pct##c.no_gender_egal TAXABLE_HEAD_WIFE_  `controls' STATE_ if couple_educ_gp==0 & cohort==3, or // just less-educated attitudes - same trends
margins, at(female_hours_pct=(0(.25)1) no_gender_egal=(.30(.1).80)) // think this indicates attitudes not as important?
marginsplot

*chart
logit dissolve_lag i.dur c.female_hours_pct##i.gender_egal_mean TAXABLE_HEAD_WIFE_ `controls' STATE_ above_fed if couple_educ_gp==0 & cohort==3, or // still not sig, though do have diff slopes
margins gender_egal_mean, at(female_hours_pct=(0(.25)1))
marginsplot

logit dissolve_lag i.dur c.female_hours_pct##c.all_gender_egal TAXABLE_HEAD_WIFE_  `controls' STATE_ if couple_educ_gp==1 & cohort==3, or // not sig, but slightly worse in lower egal
margins, at(female_hours_pct=(0(.25)1) all_gender_egal=(.30(.1).80))
marginsplot

logit dissolve_lag i.dur c.female_hours_pct##c.coll_gender_egal TAXABLE_HEAD_WIFE_  `controls' STATE_ if couple_educ_gp==1 & cohort==3, or // def no interaction here
margins, at(female_hours_pct=(0(.25)1) coll_gender_egal=(.30(.1).80))
marginsplot

*chart
logit dissolve_lag i.dur c.female_hours_pct##i.gender_egal_mean TAXABLE_HEAD_WIFE_ `controls' STATE_ if couple_educ_gp==1 & cohort==3, or // def no interaction
margins gender_egal_mean, at(female_hours_pct=(0(.25)1))
marginsplot

logit dissolve_lag i.dur c.wife_housework_pct##c.all_gender_egal TAXABLE_HEAD_WIFE_  `controls' STATE_ above_fed if couple_educ_gp==0 & cohort==3, or
margins, at(wife_housework_pct=(0(.25)1) all_gender_egal=(.30(.1).80))
marginsplot

logit dissolve_lag i.dur c.wife_housework_pct##c.all_gender_egal TAXABLE_HEAD_WIFE_  `controls' STATE_ if couple_educ_gp==1 & cohort==3, or 
margins, at(wife_housework_pct=(0(.25)1) all_gender_egal=(.30(.1).80))
marginsplot

*chart
logit dissolve_lag i.dur c.wife_housework_pct##i.gender_egal_mean TAXABLE_HEAD_WIFE_ `controls' STATE_ above_fed if couple_educ_gp==0 & cohort==3, or
margins gender_egal_mean, at(wife_housework_pct=(0(.25)1))
marginsplot

*chart
logit dissolve_lag i.dur c.wife_housework_pct##i.gender_egal_mean TAXABLE_HEAD_WIFE_ `controls' STATE_ above_fed if couple_educ_gp==1 & cohort==3, or
margins gender_egal_mean, at(wife_housework_pct=(0(.25)1))
marginsplot


// region lookup for gss: "$temp\state_division.dta"
// gss data: "$temp\gss_region_year.dta"
// min wage data: "$temp\state_min_wage.dta"
 
********************************************************************************
* Other models
********************************************************************************
* Splitting into who has degree
logit dissolve_lag i.dur i.college_bkd if cohort==3 & inlist(IN_UNIT,1,2), or
logit dissolve_lag i.dur ib1.college_bkd if cohort==3 & inlist(IN_UNIT,1,2), or
/// interesting - all college couples less likely to divorce than non-college. BUT both college is less likely than either husband or wife. kinda Schwartz and Han I guess - but somewhat contrary to rest of findings where equality is less stabilizing. education cultural, the division of labor power? 

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled REGION_ cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_  `controls' if cohort==3 & couple_educ_gp==1, or //  continuous paid hours - discrete time
logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_  `controls' if cohort==3 & college_bkd==1, or //  both
logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_  `controls' if cohort==3 & college_bkd==2, or //  wife
logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_  `controls' if cohort==3 & college_bkd==3, or //  husband

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled REGION_ cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur i.hh_earn_type TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or // earnings - bucketed
logit dissolve_lag i.dur i.hh_earn_type TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & college_bkd==1, or //  both
logit dissolve_lag i.dur i.hh_earn_type TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & college_bkd==2, or //  wife
logit dissolve_lag i.dur i.hh_earn_type TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & college_bkd==3, or //  husband

** Overall
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled REGION_ cohab_with_wife cohab_with_other pre_marital_birth i.couple_educ_gp"

logit dissolve_lag i.dur female_hours_pct if inlist(IN_UNIT,1,2) & cohort==3, or //  continuous paid hours - discrete time
outreg2 using "$results/psid_marriage_dissolution_total_paa.xls", sideway stats(coef pval) label ctitle(1 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace
logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3, or //  continuous paid hours - discrete time
outreg2 using "$results/psid_marriage_dissolution_total_paa.xls", sideway stats(coef pval) label ctitle(1 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_hours_type if inlist(IN_UNIT,1,2) & cohort==3, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_total_paa.xls", sideway stats(coef pval) label ctitle(2 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.hh_hours_type TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_total_paa.xls", sideway stats(coef pval) label ctitle(2 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur wife_housework_pct if inlist(IN_UNIT,1,2) & cohort==3, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_total_paa.xls", sideway stats(coef pval) label ctitle(3 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur wife_housework_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_total_paa.xls", sideway stats(coef pval) label ctitle(3 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.housework_bkt if inlist(IN_UNIT,1,2) & cohort==3, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_total_paa.xls", sideway stats(coef pval) label ctitle(4 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.housework_bkt TAXABLE_HEAD_WIFE_ `controls' if inlist(IN_UNIT,1,2)  & cohort==3, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_total_paa.xls", sideway stats(coef pval) label ctitle(4 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur female_earn_pct if inlist(IN_UNIT,1,2) & cohort==3, or //  continuous earnings
outreg2 using "$results/psid_marriage_dissolution_total_paa.xls", sideway stats(coef pval) label ctitle(5 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur female_earn_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3, or //  continuous earnings
outreg2 using "$results/psid_marriage_dissolution_total_paa.xls", sideway stats(coef pval) label ctitle(5 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_earn_type if inlist(IN_UNIT,1,2) & cohort==3, or // earnings - bucketed
outreg2 using "$results/psid_marriage_dissolution_total_paa.xls", sideway stats(coef pval) label ctitle(6 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.hh_earn_type TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3, or // earnings - bucketed
outreg2 using "$results/psid_marriage_dissolution_total_paa.xls", sideway stats(coef pval) label ctitle(6 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.ft_head i.ft_wife if inlist(IN_UNIT,1,2) & cohort==3, or // employment
outreg2 using "$results/psid_marriage_dissolution_total_paa.xls", sideway stats(coef pval) label ctitle(7 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.ft_head i.ft_wife TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3, or // employment
outreg2 using "$results/psid_marriage_dissolution_total_paa.xls", sideway stats(coef pval) label ctitle(7 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append


********************************************************************************
* Exploration
********************************************************************************
/*
logit dissolve_lag i.dur i.hours_type_hw if inlist(IN_UNIT,1,2) & inlist(cohort_alt,3,4), or // dual as ref
logit dissolve_lag i.dur ib5.hours_type_hw if inlist(IN_UNIT,1,2) & inlist(cohort_alt,3,4), or // male BW / female HM as ref

logit dissolve_lag i.dur i.hours_type_hw if inlist(IN_UNIT,1,2) & inlist(cohort_alt,3,4) & couple_educ_gp==0, or // dual as ref
logit dissolve_lag i.dur i.hours_type_hw if inlist(IN_UNIT,1,2) & inlist(cohort_alt,3,4) & couple_educ_gp==1, or // dual as ref

logit dissolve_lag i.dur i.earn_type_hw if inlist(IN_UNIT,1,2) & inlist(cohort_alt,3,4) & couple_educ_gp==0, or // dual as ref
margins earn_type_hw
marginsplot
logit dissolve_lag i.dur i.earn_type_hw if inlist(IN_UNIT,1,2) & inlist(cohort_alt,3,4) & couple_educ_gp==1, or // dual as ref
margins earn_type_hw
marginsplot

logit dissolve_lag i.dur i.hours_diff_bkt if inlist(IN_UNIT,1,2) & inlist(cohort_alt,3,4) & couple_educ_gp==0, or // dual as ref
logit dissolve_lag i.dur i.hours_diff_bkt if inlist(IN_UNIT,1,2) & inlist(cohort_alt,3,4) & couple_educ_gp==1, or // dual as ref
*/

********************************************************************************
* No College
********************************************************************************
** Cohort A (1990-1999)
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled REGION_ cohab_with_wife cohab_with_other pre_marital_birth"

logit dissolve_lag i.dur female_hours_pct if inlist(IN_UNIT,1,2) & cohort_alt==3 & couple_educ_gp==0, or //  continuous paid hours - discrete time
outreg2 using "$results/psid_marriage_dissolution_nocoll_A.xls", sideway stats(coef pval) label ctitle(1 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace
logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort_alt==3 & couple_educ_gp==0, or //  continuous paid hours - discrete time
outreg2 using "$results/psid_marriage_dissolution_nocoll_A.xls", sideway stats(coef pval) label ctitle(1 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_hours_type if inlist(IN_UNIT,1,2) & cohort_alt==3 & couple_educ_gp==0, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_nocoll_A.xls", sideway stats(coef pval) label ctitle(2 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.hh_hours_type TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort_alt==3 & couple_educ_gp==0, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_nocoll_A.xls", sideway stats(coef pval) label ctitle(2 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur wife_housework_pct if inlist(IN_UNIT,1,2) & cohort_alt==3 & couple_educ_gp==0, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_nocoll_A.xls", sideway stats(coef pval) label ctitle(3 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur wife_housework_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort_alt==3 & couple_educ_gp==0, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_nocoll_A.xls", sideway stats(coef pval) label ctitle(3 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.housework_bkt if inlist(IN_UNIT,1,2) & cohort_alt==3 & couple_educ_gp==0, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_nocoll_A.xls", sideway stats(coef pval) label ctitle(4 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.housework_bkt TAXABLE_HEAD_WIFE_ `controls' if inlist(IN_UNIT,1,2)  & cohort_alt==3 & couple_educ_gp==0, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_nocoll_A.xls", sideway stats(coef pval) label ctitle(4 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur female_earn_pct if inlist(IN_UNIT,1,2) & cohort_alt==3 & couple_educ_gp==0, or //  continuous earnings
outreg2 using "$results/psid_marriage_dissolution_nocoll_A.xls", sideway stats(coef pval) label ctitle(5 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur female_earn_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort_alt==3 & couple_educ_gp==0, or //  continuous earnings
outreg2 using "$results/psid_marriage_dissolution_nocoll_A.xls", sideway stats(coef pval) label ctitle(5 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_earn_type if inlist(IN_UNIT,1,2) & cohort_alt==3 & couple_educ_gp==0, or // earnings - bucketed
outreg2 using "$results/psid_marriage_dissolution_nocoll_A.xls", sideway stats(coef pval) label ctitle(6 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.hh_earn_type TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort_alt==3 & couple_educ_gp==0, or // earnings - bucketed
outreg2 using "$results/psid_marriage_dissolution_nocoll_A.xls", sideway stats(coef pval) label ctitle(6 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.ft_head i.ft_wife if inlist(IN_UNIT,1,2) & cohort_alt==3 & couple_educ_gp==0, or // employment
outreg2 using "$results/psid_marriage_dissolution_nocoll_A.xls", sideway stats(coef pval) label ctitle(7 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.ft_head i.ft_wife TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort_alt==3 & couple_educ_gp==0, or // employment
outreg2 using "$results/psid_marriage_dissolution_nocoll_A.xls", sideway stats(coef pval) label ctitle(7 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

** Cohort B (2000-2014)
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled REGION_ cohab_with_wife cohab_with_other pre_marital_birth"

logit dissolve_lag i.dur female_hours_pct if inlist(IN_UNIT,1,2) & cohort_alt==4 & couple_educ_gp==0, or //  continuous paid hours - discrete time
outreg2 using "$results/psid_marriage_dissolution_nocoll_B.xls", sideway stats(coef pval) label ctitle(1 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace
logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort_alt==4 & couple_educ_gp==0, or //  continuous paid hours - discrete time
outreg2 using "$results/psid_marriage_dissolution_nocoll_B.xls", sideway stats(coef pval) label ctitle(1 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_hours_type if inlist(IN_UNIT,1,2) & cohort_alt==4 & couple_educ_gp==0, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_nocoll_B.xls", sideway stats(coef pval) label ctitle(2 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.hh_hours_type TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort_alt==4 & couple_educ_gp==0, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_nocoll_B.xls", sideway stats(coef pval) label ctitle(2 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur wife_housework_pct if inlist(IN_UNIT,1,2) & cohort_alt==4 & couple_educ_gp==0, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_nocoll_B.xls", sideway stats(coef pval) label ctitle(3 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur wife_housework_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort_alt==4 & couple_educ_gp==0, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_nocoll_B.xls", sideway stats(coef pval) label ctitle(3 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.housework_bkt if inlist(IN_UNIT,1,2) & cohort_alt==4 & couple_educ_gp==0, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_nocoll_B.xls", sideway stats(coef pval) label ctitle(4 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.housework_bkt TAXABLE_HEAD_WIFE_ `controls' if inlist(IN_UNIT,1,2)  & cohort_alt==4 & couple_educ_gp==0, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_nocoll_B.xls", sideway stats(coef pval) label ctitle(4 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur female_earn_pct if inlist(IN_UNIT,1,2) & cohort_alt==4 & couple_educ_gp==0, or //  continuous earnings
outreg2 using "$results/psid_marriage_dissolution_nocoll_B.xls", sideway stats(coef pval) label ctitle(5 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur female_earn_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort_alt==4 & couple_educ_gp==0, or //  continuous earnings
outreg2 using "$results/psid_marriage_dissolution_nocoll_B.xls", sideway stats(coef pval) label ctitle(5 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_earn_type if inlist(IN_UNIT,1,2) & cohort_alt==4 & couple_educ_gp==0, or // earnings - bucketed
outreg2 using "$results/psid_marriage_dissolution_nocoll_B.xls", sideway stats(coef pval) label ctitle(6 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.hh_earn_type TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort_alt==4 & couple_educ_gp==0, or // earnings - bucketed
outreg2 using "$results/psid_marriage_dissolution_nocoll_B.xls", sideway stats(coef pval) label ctitle(6 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.ft_head i.ft_wife if inlist(IN_UNIT,1,2) & cohort_alt==4 & couple_educ_gp==0, or // employment
outreg2 using "$results/psid_marriage_dissolution_nocoll_B.xls", sideway stats(coef pval) label ctitle(7 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.ft_head i.ft_wife TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort_alt==4 & couple_educ_gp==0, or // employment
outreg2 using "$results/psid_marriage_dissolution_nocoll_B.xls", sideway stats(coef pval) label ctitle(7 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

** Cohort C (1990-2014)
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled REGION_ cohab_with_wife cohab_with_other pre_marital_birth"

logit dissolve_lag i.dur female_hours_pct if inlist(IN_UNIT,1,2) & inlist(cohort_alt,3,4) & couple_educ_gp==0, or //  continuous paid hours - discrete time
outreg2 using "$results/psid_marriage_dissolution_nocoll_C.xls", sideway stats(coef pval) label ctitle(1 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace
logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & inlist(cohort_alt,3,4) & couple_educ_gp==0, or //  continuous paid hours - discrete time
outreg2 using "$results/psid_marriage_dissolution_nocoll_C.xls", sideway stats(coef pval) label ctitle(1 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_hours_type if inlist(IN_UNIT,1,2) & inlist(cohort_alt,3,4) & couple_educ_gp==0, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_nocoll_C.xls", sideway stats(coef pval) label ctitle(2 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.hh_hours_type TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & inlist(cohort_alt,3,4) & couple_educ_gp==0, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_nocoll_C.xls", sideway stats(coef pval) label ctitle(2 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur wife_housework_pct if inlist(IN_UNIT,1,2) & inlist(cohort_alt,3,4) & couple_educ_gp==0, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_nocoll_C.xls", sideway stats(coef pval) label ctitle(3 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur wife_housework_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & inlist(cohort_alt,3,4) & couple_educ_gp==0, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_nocoll_C.xls", sideway stats(coef pval) label ctitle(3 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.housework_bkt if inlist(IN_UNIT,1,2) & inlist(cohort_alt,3,4) & couple_educ_gp==0, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_nocoll_C.xls", sideway stats(coef pval) label ctitle(4 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.housework_bkt TAXABLE_HEAD_WIFE_ `controls' if inlist(IN_UNIT,1,2)  & inlist(cohort_alt,3,4) & couple_educ_gp==0, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_nocoll_C.xls", sideway stats(coef pval) label ctitle(4 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur female_earn_pct if inlist(IN_UNIT,1,2) & inlist(cohort_alt,3,4) & couple_educ_gp==0, or //  continuous earnings
outreg2 using "$results/psid_marriage_dissolution_nocoll_C.xls", sideway stats(coef pval) label ctitle(5 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur female_earn_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & inlist(cohort_alt,3,4) & couple_educ_gp==0, or //  continuous earnings
outreg2 using "$results/psid_marriage_dissolution_nocoll_C.xls", sideway stats(coef pval) label ctitle(5 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_earn_type if inlist(IN_UNIT,1,2) & inlist(cohort_alt,3,4) & couple_educ_gp==0, or // earnings - bucketed
outreg2 using "$results/psid_marriage_dissolution_nocoll_C.xls", sideway stats(coef pval) label ctitle(6 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.hh_earn_type TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & inlist(cohort_alt,3,4) & couple_educ_gp==0, or // earnings - bucketed
outreg2 using "$results/psid_marriage_dissolution_nocoll_C.xls", sideway stats(coef pval) label ctitle(6 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.ft_head i.ft_wife if inlist(IN_UNIT,1,2) & inlist(cohort_alt,3,4) & couple_educ_gp==0, or // employment
outreg2 using "$results/psid_marriage_dissolution_nocoll_C.xls", sideway stats(coef pval) label ctitle(7 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.ft_head i.ft_wife TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & inlist(cohort_alt,3,4) & couple_educ_gp==0, or // employment
outreg2 using "$results/psid_marriage_dissolution_nocoll_C.xls", sideway stats(coef pval) label ctitle(7 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

********************************************************************************
* College
********************************************************************************
** Cohort A (1990-1999)
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled REGION_ cohab_with_wife cohab_with_other pre_marital_birth"

logit dissolve_lag i.dur female_hours_pct if inlist(IN_UNIT,1,2) & cohort_alt==3 & couple_educ_gp==1, or //  continuous paid hours - discrete time
outreg2 using "$results/psid_marriage_dissolution_college_A.xls", sideway stats(coef pval) label ctitle(1 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace
logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort_alt==3 & couple_educ_gp==1, or //  continuous paid hours - discrete time
outreg2 using "$results/psid_marriage_dissolution_college_A.xls", sideway stats(coef pval) label ctitle(1 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_hours_type if inlist(IN_UNIT,1,2) & cohort_alt==3 & couple_educ_gp==1, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_college_A.xls", sideway stats(coef pval) label ctitle(2 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.hh_hours_type TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort_alt==3 & couple_educ_gp==1, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_college_A.xls", sideway stats(coef pval) label ctitle(2 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur wife_housework_pct if inlist(IN_UNIT,1,2) & cohort_alt==3 & couple_educ_gp==1, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_college_A.xls", sideway stats(coef pval) label ctitle(3 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur wife_housework_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort_alt==3 & couple_educ_gp==1, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_college_A.xls", sideway stats(coef pval) label ctitle(3 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.housework_bkt if inlist(IN_UNIT,1,2) & cohort_alt==3 & couple_educ_gp==1, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_college_A.xls", sideway stats(coef pval) label ctitle(4 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.housework_bkt TAXABLE_HEAD_WIFE_ `controls' if inlist(IN_UNIT,1,2)  & cohort_alt==3 & couple_educ_gp==1, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_college_A.xls", sideway stats(coef pval) label ctitle(4 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur female_earn_pct if inlist(IN_UNIT,1,2) & cohort_alt==3 & couple_educ_gp==1, or //  continuous earnings
outreg2 using "$results/psid_marriage_dissolution_college_A.xls", sideway stats(coef pval) label ctitle(5 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur female_earn_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort_alt==3 & couple_educ_gp==1, or //  continuous earnings
outreg2 using "$results/psid_marriage_dissolution_college_A.xls", sideway stats(coef pval) label ctitle(5 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_earn_type if inlist(IN_UNIT,1,2) & cohort_alt==3 & couple_educ_gp==1, or // earnings - bucketed
outreg2 using "$results/psid_marriage_dissolution_college_A.xls", sideway stats(coef pval) label ctitle(6 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.hh_earn_type TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort_alt==3 & couple_educ_gp==1, or // earnings - bucketed
outreg2 using "$results/psid_marriage_dissolution_college_A.xls", sideway stats(coef pval) label ctitle(6 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.ft_head i.ft_wife if inlist(IN_UNIT,1,2) & cohort_alt==3 & couple_educ_gp==1, or // employment
outreg2 using "$results/psid_marriage_dissolution_college_A.xls", sideway stats(coef pval) label ctitle(7 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.ft_head i.ft_wife TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort_alt==3 & couple_educ_gp==1, or // employment
outreg2 using "$results/psid_marriage_dissolution_college_A.xls", sideway stats(coef pval) label ctitle(7 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

** Cohort B (2000-2014)
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled REGION_ cohab_with_wife cohab_with_other pre_marital_birth"

logit dissolve_lag i.dur female_hours_pct if inlist(IN_UNIT,1,2) & cohort_alt==4 & couple_educ_gp==1, or //  continuous paid hours - discrete time
outreg2 using "$results/psid_marriage_dissolution_college_B.xls", sideway stats(coef pval) label ctitle(1 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace
logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort_alt==4 & couple_educ_gp==1, or //  continuous paid hours - discrete time
outreg2 using "$results/psid_marriage_dissolution_college_B.xls", sideway stats(coef pval) label ctitle(1 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_hours_type if inlist(IN_UNIT,1,2) & cohort_alt==4 & couple_educ_gp==1, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_college_B.xls", sideway stats(coef pval) label ctitle(2 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.hh_hours_type TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort_alt==4 & couple_educ_gp==1, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_college_B.xls", sideway stats(coef pval) label ctitle(2 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur wife_housework_pct if inlist(IN_UNIT,1,2) & cohort_alt==4 & couple_educ_gp==1, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_college_B.xls", sideway stats(coef pval) label ctitle(3 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur wife_housework_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort_alt==4 & couple_educ_gp==1, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_college_B.xls", sideway stats(coef pval) label ctitle(3 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.housework_bkt if inlist(IN_UNIT,1,2) & cohort_alt==4 & couple_educ_gp==1, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_college_B.xls", sideway stats(coef pval) label ctitle(4 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.housework_bkt TAXABLE_HEAD_WIFE_ `controls' if inlist(IN_UNIT,1,2)  & cohort_alt==4 & couple_educ_gp==1, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_college_B.xls", sideway stats(coef pval) label ctitle(4 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur female_earn_pct if inlist(IN_UNIT,1,2) & cohort_alt==4 & couple_educ_gp==1, or //  continuous earnings
outreg2 using "$results/psid_marriage_dissolution_college_B.xls", sideway stats(coef pval) label ctitle(5 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur female_earn_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort_alt==4 & couple_educ_gp==1, or //  continuous earnings
outreg2 using "$results/psid_marriage_dissolution_college_B.xls", sideway stats(coef pval) label ctitle(5 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_earn_type if inlist(IN_UNIT,1,2) & cohort_alt==4 & couple_educ_gp==1, or // earnings - bucketed
outreg2 using "$results/psid_marriage_dissolution_college_B.xls", sideway stats(coef pval) label ctitle(6 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.hh_earn_type TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort_alt==4 & couple_educ_gp==1, or // earnings - bucketed
outreg2 using "$results/psid_marriage_dissolution_college_B.xls", sideway stats(coef pval) label ctitle(6 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.ft_head i.ft_wife if inlist(IN_UNIT,1,2) & cohort_alt==4 & couple_educ_gp==1, or // employment
outreg2 using "$results/psid_marriage_dissolution_college_B.xls", sideway stats(coef pval) label ctitle(7 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.ft_head i.ft_wife TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort_alt==4 & couple_educ_gp==1, or // employment
outreg2 using "$results/psid_marriage_dissolution_college_B.xls", sideway stats(coef pval) label ctitle(7 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

** Cohort C (1990-2014)
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled REGION_ cohab_with_wife cohab_with_other pre_marital_birth"

logit dissolve_lag i.dur female_hours_pct if inlist(IN_UNIT,1,2) & inlist(cohort_alt,3,4) & couple_educ_gp==1, or //  continuous paid hours - discrete time
outreg2 using "$results/psid_marriage_dissolution_college_C.xls", sideway stats(coef pval) label ctitle(1 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace
logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & inlist(cohort_alt,3,4) & couple_educ_gp==1, or //  continuous paid hours - discrete time
outreg2 using "$results/psid_marriage_dissolution_college_C.xls", sideway stats(coef pval) label ctitle(1 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_hours_type if inlist(IN_UNIT,1,2) & inlist(cohort_alt,3,4) & couple_educ_gp==1, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_college_C.xls", sideway stats(coef pval) label ctitle(2 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.hh_hours_type TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & inlist(cohort_alt,3,4) & couple_educ_gp==1, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_college_C.xls", sideway stats(coef pval) label ctitle(2 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur wife_housework_pct if inlist(IN_UNIT,1,2) & inlist(cohort_alt,3,4) & couple_educ_gp==1, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_college_C.xls", sideway stats(coef pval) label ctitle(3 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur wife_housework_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & inlist(cohort_alt,3,4) & couple_educ_gp==1, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_college_C.xls", sideway stats(coef pval) label ctitle(3 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.housework_bkt if inlist(IN_UNIT,1,2) & inlist(cohort_alt,3,4) & couple_educ_gp==1, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_college_C.xls", sideway stats(coef pval) label ctitle(4 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.housework_bkt TAXABLE_HEAD_WIFE_ `controls' if inlist(IN_UNIT,1,2)  & inlist(cohort_alt,3,4) & couple_educ_gp==1, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_college_C.xls", sideway stats(coef pval) label ctitle(4 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur female_earn_pct if inlist(IN_UNIT,1,2) & inlist(cohort_alt,3,4) & couple_educ_gp==1, or //  continuous earnings
outreg2 using "$results/psid_marriage_dissolution_college_C.xls", sideway stats(coef pval) label ctitle(5 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur female_earn_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & inlist(cohort_alt,3,4) & couple_educ_gp==1, or //  continuous earnings
outreg2 using "$results/psid_marriage_dissolution_college_C.xls", sideway stats(coef pval) label ctitle(5 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_earn_type if inlist(IN_UNIT,1,2) & inlist(cohort_alt,3,4) & couple_educ_gp==1, or // earnings - bucketed
outreg2 using "$results/psid_marriage_dissolution_college_C.xls", sideway stats(coef pval) label ctitle(6 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.hh_earn_type TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & inlist(cohort_alt,3,4) & couple_educ_gp==1, or // earnings - bucketed
outreg2 using "$results/psid_marriage_dissolution_college_C.xls", sideway stats(coef pval) label ctitle(6 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.ft_head i.ft_wife if inlist(IN_UNIT,1,2) & inlist(cohort_alt,3,4) & couple_educ_gp==1, or // employment
outreg2 using "$results/psid_marriage_dissolution_college_C.xls", sideway stats(coef pval) label ctitle(7 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.ft_head i.ft_wife TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & inlist(cohort_alt,3,4) & couple_educ_gp==1, or // employment
outreg2 using "$results/psid_marriage_dissolution_college_C.xls", sideway stats(coef pval) label ctitle(7 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append



logit dissolve_lag i.dur ib3.bw_type if inlist(IN_UNIT,1,2) & inlist(cohort_alt,3,4) & couple_educ_gp==1, or
logit dissolve_lag i.dur ib3.bw_type if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or
logit dissolve_lag i.dur i.dual_hw if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or


********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
**# Over historical time models
********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************

********************************************************************************
* These are the same models from the main paper
********************************************************************************

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"

////////// No College \\\\\\\\\\\/
*1. Employment: no interaction
logit dissolve_lag i.dur i.ft_head i.ft_wife earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort_v2==0 & couple_educ_gp==0, or
outreg2 using "$results/psid_marriage_dissolution_hist.xls", sideway stats(coef pval) label ctitle(No 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_hist.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*2. Categorical indicator of Paid work
logit dissolve_lag i.dur i.hh_earn_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort_v2==0 & couple_educ_gp==0, or
outreg2 using "$results/psid_marriage_dissolution_hist.xls", sideway stats(coef pval) label ctitle(No 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_hist.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*3. Total earnings
logit dissolve_lag i.dur earnings_1000s `controls' if inlist(IN_UNIT,1,2) & cohort_v2==0 & couple_educ_gp==0, or
outreg2 using "$results/psid_marriage_dissolution_hist.xls", sideway stats(coef pval) label ctitle(No 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_hist.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*4. Categorical Housework
logit dissolve_lag i.dur i.housework_bkt earnings_1000s `controls' if inlist(IN_UNIT,1,2)  & cohort_v2==0 & couple_educ_gp==0, or
outreg2 using "$results/psid_marriage_dissolution_hist.xls", sideway stats(coef pval) label ctitle(No 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_hist.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

////////// College \\\\\\\\\\\/
*1. Employment: no interaction
logit dissolve_lag i.dur i.ft_head i.ft_wife earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort_v2==0 & couple_educ_gp==1, or
outreg2 using "$results/psid_marriage_dissolution_hist.xls", sideway stats(coef pval) label ctitle(Coll 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_hist.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*2. Categorical indicator of Paid work
logit dissolve_lag i.dur i.hh_earn_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort_v2==0 & couple_educ_gp==1, or
outreg2 using "$results/psid_marriage_dissolution_hist.xls", sideway stats(coef pval) label ctitle(Coll 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_hist.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*3. Total earnings
logit dissolve_lag i.dur earnings_1000s `controls' if inlist(IN_UNIT,1,2) & cohort_v2==0 & couple_educ_gp==1, or
outreg2 using "$results/psid_marriage_dissolution_hist.xls", sideway stats(coef pval) label ctitle(Coll 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_hist.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*4. Categorical Housework
logit dissolve_lag i.dur i.housework_bkt earnings_1000s `controls' if inlist(IN_UNIT,1,2)  & cohort_v2==0 & couple_educ_gp==1, or
outreg2 using "$results/psid_marriage_dissolution_hist.xls", sideway stats(coef pval) label ctitle(Coll 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(*) post
outreg2 using "$results/psid_marriage_dissolution_hist.xls", ctitle(margins) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

// earnings linearity?
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur ib3.couple_earnings_gp##i.couple_educ_gp `controls' if inlist(IN_UNIT,1,2) & cohort_v2==0, or // b3 is generally median
margins couple_earnings_gp#couple_educ_gp
marginsplot // yes okay this is exactly what I need for the curve argument
marginsplot, xtitle("Total Couple Earnings (grouped, $1000s)") ylabel(, angle(0))  ytitle("Probability of Marital Dissolution") plotregion(fcolor(white)) graphregion(fcolor(white)) title("")  xlabel(, angle(45) labsize(small)) legend(region(lcolor(white))) legend(size(small)) plot1opts(lcolor("blue")  msize("small") mcolor("blue"))  plot2opts(lcolor("pink") mcolor("pink") msize("small")) recast(line) recastci(rarea) ciopts(color(*.4)) //  ci2opts(lcolor("pink")) ci1opts(lcolor("blue"))

// so much more linear here. use earnings_1000s not the knots

********************************************************************************
* Figure for historical data
********************************************************************************
*Panel 1A
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logistic dissolve_lag i.dur i.hh_earn_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort_v2==0 & couple_educ_gp==0 & hh_earn_type <4
margins, dydx(hh_earn_type) level(90) post
estimates store est1_hist

logistic dissolve_lag i.dur i.hh_earn_type earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort_v2==0 & couple_educ_gp==1 & hh_earn_type <4
margins, dydx(hh_earn_type) level(90) post
estimates store est2_hist

*Panel 1B
logistic dissolve_lag i.dur i.ft_head 1.ft_wife earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort_v2==0 & couple_educ_gp==0
estimates store no_b_hist
margins, dydx(ft_head) level(90) post
estimates store est3_hist

estimates restore no_b_hist
margins, dydx(ft_wife) level(90) post
estimates store est4_hist

logistic dissolve_lag i.dur i.ft_head 1.ft_wife earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort_v2==0 & couple_educ_gp==1
estimates store coll_b_hist
margins, dydx(ft_head) level(90) post
estimates store est5_hist

estimates restore coll_b_hist
margins, dydx(ft_wife) level(90) post
estimates store est6_hist

*Panel 1C
logistic dissolve_lag i.dur i.housework_bkt earnings_1000s  `controls' if inlist(IN_UNIT,1,2) & cohort_v2==0 & couple_educ_gp==0 & housework_bkt <4
margins, dydx(housework_bkt) level(90) post
estimates store est7_hist

logistic dissolve_lag i.dur i.housework_bkt earnings_1000s `controls' if inlist(IN_UNIT,1,2) & cohort_v2==0 & couple_educ_gp==1 & housework_bkt <4
margins, dydx(housework_bkt) level(90) post
estimates store est8_hist

coefplot (est3_hist, offset(.20) label(No College)) (est4_hist, offset(.20) nokey) (est5_hist, offset(-.20) label(College)) (est6_hist, offset(-.20) nokey) (est1_hist, offset(.20) nokey) (est2_hist, offset(-.20) nokey) (est7_hist, offset(.20) nokey) (est8_hist, offset(-.20) nokey), drop(_cons 0.ft_head) xline(0) levels(90) base xtitle(Average Marginal Effect Relative to Reference Group) ///
coeflabels(1.hh_earn_type = "Dual-Earner" 2.hh_earn_type = "Male-Breadwinner" 3.hh_earn_type = "Female-Breadwinner" 1.ft_head = "Husband Employed FT" 1.ft_wife = "Wife Employed FT" 1.housework_bkt = "Dual-Housework" 2.housework_bkt = "Female-Housework" 3.housework_bkt = "Male-Housework") ///
 headings(1.ft_head= "{bf:Employment Status (M1)}"   1.hh_earn_type = "{bf:Paid Work Arrangement (M2)}"   1.housework_bkt = "{bf:Unpaid Work Arrangement (M4)}")
 
*** Instead of separate by time, doing separate by education, with time combined
**No College
coefplot (est3_hist, offset(.20) label(Historical)) (est4_hist, offset(.20) nokey) (est3, offset(-.20) label(Present)) (est4, offset(-.20) nokey) (est1_hist, offset(.20) nokey) (est1, offset(-.20) nokey) (est7_hist, offset(.20) nokey) (est7, offset(-.20) nokey), drop(_cons 0.ft_head) xline(0) levels(90) base xtitle(Average Marginal Effect Relative to Reference Group) ///
coeflabels(1.hh_earn_type = "Dual-Earner" 2.hh_earn_type = "Male-Breadwinner" 3.hh_earn_type = "Female-Breadwinner" 1.ft_head = "Husband Employed FT" 1.ft_wife = "Wife Employed FT" 1.housework_bkt = "Dual-Housework" 2.housework_bkt = "Female-Housework" 3.housework_bkt = "Male-Housework") ///
 headings(1.ft_head= "{bf:Employment Status (M1)}"   1.hh_earn_type = "{bf:Paid Work Arrangement (M2)}"   1.housework_bkt = "{bf:Unpaid Work Arrangement (M4)}")
 
 **College
coefplot (est5_hist, offset(.20) label(Historical)) (est6_hist, offset(.20) nokey) (est5, offset(-.20) label(Present)) (est6, offset(-.20) nokey) (est2_hist, offset(.20) nokey) (est2, offset(-.20) nokey) (est8_hist, offset(.20) nokey) (est8, offset(-.20) nokey), drop(_cons 0.ft_head) xline(0) levels(90) base xtitle(Average Marginal Effect Relative to Reference Group) ///
coeflabels(1.hh_earn_type = "Dual-Earner" 2.hh_earn_type = "Male-Breadwinner" 3.hh_earn_type = "Female-Breadwinner" 1.ft_head = "Husband Employed FT" 1.ft_wife = "Wife Employed FT" 1.housework_bkt = "Dual-Housework" 2.housework_bkt = "Female-Housework" 3.housework_bkt = "Male-Housework") ///
 headings(1.ft_head= "{bf:Employment Status (M1)}"   1.hh_earn_type = "{bf:Paid Work Arrangement (M2)}"   1.housework_bkt = "{bf:Unpaid Work Arrangement (M4)}")
 

/*
********************************************************************************
* Overall models
********************************************************************************
** Cohort 1 (1970s)
logit dissolve_lag i.dur female_hours_pct i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==1, or //  continuous paid hours - discrete time
outreg2 using "$results/psid_marriage_dissolution_overall_1970s.xls", sideway stats(coef pval) label ctitle(1 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace
logit dissolve_lag i.dur female_hours_pct i.couple_educ_gp TAXABLE_HEAD_WIFE_ `controls' if inlist(IN_UNIT,1,2) & cohort==1, or //  continuous paid hours - with controls
outreg2 using "$results/psid_marriage_dissolution_overall_1970s.xls", sideway stats(coef pval) label ctitle(1 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_hours_3070 i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==1, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_overall_1970s.xls", sideway stats(coef pval) label ctitle(2 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.hh_hours_3070 i.couple_educ_gp TAXABLE_HEAD_WIFE_ `controls'  if inlist(IN_UNIT,1,2) & cohort==1, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_overall_1970s.xls", sideway stats(coef pval) label ctitle(2 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur wife_housework_pct i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==1, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_overall_1970s.xls", sideway stats(coef pval) label ctitle(3 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur wife_housework_pct i.couple_educ_gp TAXABLE_HEAD_WIFE_ `controls'  if inlist(IN_UNIT,1,2) & cohort==1, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_overall_1970s.xls", sideway stats(coef pval) label ctitle(3 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.housework_bkt i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==1, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_overall_1970s.xls", sideway stats(coef pval) label ctitle(4 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.housework_bkt i.couple_educ_gp TAXABLE_HEAD_WIFE_ `controls'  if inlist(IN_UNIT,1,2) & cohort==1, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_overall_1970s.xls", sideway stats(coef pval) label ctitle(4 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur female_hours_pct wife_housework_pct TAXABLE_HEAD_WIFE_ i.couple_educ_gp if inlist(IN_UNIT,1,2) & cohort==1, or // continuous paid and unpaid hours income coefficients to use
outreg2 using "$results/psid_marriage_dissolution_overall_1970s.xls", sideway stats(coef pval) label ctitle(5 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur female_hours_pct wife_housework_pct TAXABLE_HEAD_WIFE_ i.couple_educ_gp `controls' if inlist(IN_UNIT,1,2) & cohort==1, or // continuous paid and unpaid hours income coefficients to use
outreg2 using "$results/psid_marriage_dissolution_overall_1970s.xls", sideway stats(coef pval) label ctitle(5 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

** Cohort 2 (1980s)
logit dissolve_lag i.dur female_hours_pct i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==2, or //  continuous paid hours - discrete time
outreg2 using "$results/psid_marriage_dissolution_overall_1980s.xls", sideway stats(coef pval) label ctitle(1 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace
logit dissolve_lag i.dur female_hours_pct i.couple_educ_gp TAXABLE_HEAD_WIFE_ age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled REGION_ cohab_with_wife cohab_with_other pre_marital_birth if inlist(IN_UNIT,1,2) & cohort==2, or //  continuous paid hours - with controls
outreg2 using "$results/psid_marriage_dissolution_overall_1980s.xls", sideway stats(coef pval) label ctitle(1 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_hours_3070 i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==2, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_overall_1980s.xls", sideway stats(coef pval) label ctitle(2 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.hh_hours_3070 i.couple_educ_gp TAXABLE_HEAD_WIFE_ age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled REGION_ cohab_with_wife cohab_with_other pre_marital_birth if inlist(IN_UNIT,1,2) & cohort==2, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_overall_1980s.xls", sideway stats(coef pval) label ctitle(2 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur wife_housework_pct i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==2, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_overall_1980s.xls", sideway stats(coef pval) label ctitle(3 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur wife_housework_pct i.couple_educ_gp TAXABLE_HEAD_WIFE_ age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled REGION_ cohab_with_wife cohab_with_other pre_marital_birth if inlist(IN_UNIT,1,2) & cohort==2, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_overall_1980s.xls", sideway stats(coef pval) label ctitle(3 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.housework_bkt i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==2, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_overall_1980s.xls", sideway stats(coef pval) label ctitle(4 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.housework_bkt i.couple_educ_gp TAXABLE_HEAD_WIFE_ age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled REGION_ cohab_with_wife cohab_with_other pre_marital_birth if inlist(IN_UNIT,1,2) & cohort==2, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_overall_1980s.xls", sideway stats(coef pval) label ctitle(4 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur female_hours_pct wife_housework_pct TAXABLE_HEAD_WIFE_ i.couple_educ_gp if inlist(IN_UNIT,1,2) & cohort==2, or // continuous paid and unpaid hours income coefficients to use
outreg2 using "$results/psid_marriage_dissolution_overall_1980s.xls", sideway stats(coef pval) label ctitle(5 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur female_hours_pct wife_housework_pct TAXABLE_HEAD_WIFE_ i.couple_educ_gp age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled REGION_ cohab_with_wife cohab_with_other pre_marital_birth if inlist(IN_UNIT,1,2) & cohort==2, or // continuous paid and unpaid hours income coefficients to use
outreg2 using "$results/psid_marriage_dissolution_overall_1980s.xls", sideway stats(coef pval) label ctitle(5 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

** Cohort 3 (1990s)
logit dissolve_lag i.dur female_hours_pct i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==3, or //  continuous paid hours - discrete time
outreg2 using "$results/psid_marriage_dissolution_overall_1990s.xls", sideway stats(coef pval) label ctitle(1 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace
logit dissolve_lag i.dur female_hours_pct i.couple_educ_gp TAXABLE_HEAD_WIFE_ age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled REGION_ cohab_with_wife cohab_with_other pre_marital_birth if inlist(IN_UNIT,1,2) & cohort==3, or //  continuous paid hours - with controls
outreg2 using "$results/psid_marriage_dissolution_overall_1990s.xls", sideway stats(coef pval) label ctitle(1 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_hours_3070 i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==3, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_overall_1990s.xls", sideway stats(coef pval) label ctitle(2 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.hh_hours_3070 i.couple_educ_gp TAXABLE_HEAD_WIFE_ age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled REGION_ cohab_with_wife cohab_with_other pre_marital_birth if inlist(IN_UNIT,1,2) & cohort==3, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_overall_1990s.xls", sideway stats(coef pval) label ctitle(2 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur wife_housework_pct i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==3, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_overall_1990s.xls", sideway stats(coef pval) label ctitle(3 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur wife_housework_pct i.couple_educ_gp TAXABLE_HEAD_WIFE_ age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled REGION_ cohab_with_wife cohab_with_other pre_marital_birth if inlist(IN_UNIT,1,2) & cohort==3, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_overall_1990s.xls", sideway stats(coef pval) label ctitle(3 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.housework_bkt i.couple_educ_gp TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==3, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_overall_1990s.xls", sideway stats(coef pval) label ctitle(4 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.housework_bkt i.couple_educ_gp TAXABLE_HEAD_WIFE_ age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled REGION_ cohab_with_wife cohab_with_other pre_marital_birth if inlist(IN_UNIT,1,2) & cohort==3, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_overall_1990s.xls", sideway stats(coef pval) label ctitle(4 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur female_hours_pct wife_housework_pct TAXABLE_HEAD_WIFE_ i.couple_educ_gp if inlist(IN_UNIT,1,2) & cohort==3, or // continuous paid and unpaid hours income coefficients to use
outreg2 using "$results/psid_marriage_dissolution_overall_1990s.xls", sideway stats(coef pval) label ctitle(5 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur female_hours_pct wife_housework_pct TAXABLE_HEAD_WIFE_ i.couple_educ_gp age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled REGION_ cohab_with_wife cohab_with_other pre_marital_birth if inlist(IN_UNIT,1,2) & cohort==3, or // continuous paid and unpaid hours income coefficients to use
outreg2 using "$results/psid_marriage_dissolution_overall_1990s.xls", sideway stats(coef pval) label ctitle(5 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append


********************************************************************************
* No College
********************************************************************************
** Cohort 1 (1970s)
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled REGION_ cohab_with_wife cohab_with_other pre_marital_birth"

logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==1 & couple_educ_gp==0, or //  continuous paid hours - discrete time
outreg2 using "$results/psid_marriage_dissolution_nocoll_1970s.xls", sideway stats(coef pval) label ctitle(1 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace
logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==1 & couple_educ_gp==0, or //  continuous paid hours - discrete time
outreg2 using "$results/psid_marriage_dissolution_nocoll_1970s.xls", sideway stats(coef pval) label ctitle(1 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_hours_3070 TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==1 & couple_educ_gp==0, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_nocoll_1970s.xls", sideway stats(coef pval) label ctitle(2 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.hh_hours_3070 TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==1 & couple_educ_gp==0, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_nocoll_1970s.xls", sideway stats(coef pval) label ctitle(2 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur wife_housework_pct TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==1 & couple_educ_gp==0, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_nocoll_1970s.xls", sideway stats(coef pval) label ctitle(3 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur wife_housework_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==1 & couple_educ_gp==0, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_nocoll_1970s.xls", sideway stats(coef pval) label ctitle(3 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) appen

logit dissolve_lag i.dur i.housework_bkt TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==1 & couple_educ_gp==0, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_nocoll_1970s.xls", sideway stats(coef pval) label ctitle(4 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.housework_bkt TAXABLE_HEAD_WIFE_ `controls' if inlist(IN_UNIT,1,2)  & cohort==1 & couple_educ_gp==0, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_nocoll_1970s.xls", sideway stats(coef pval) label ctitle(4 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur female_hours_pct wife_housework_pct TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==1 & couple_educ_gp==0, or // continuous paid and unpaid hours income coefficients to use
outreg2 using "$results/psid_marriage_dissolution_nocoll_1970s.xls", sideway stats(coef pval) label ctitle(5 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur female_hours_pct wife_housework_pct TAXABLE_HEAD_WIFE_ `controls' if inlist(IN_UNIT,1,2) & cohort==1 & couple_educ_gp==0, or // continuous paid and unpaid hours income coefficients to use
outreg2 using "$results/psid_marriage_dissolution_nocoll_1970s.xls", sideway stats(coef pval) label ctitle(5 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

** Cohort 2 (1980s)
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled REGION_ cohab_with_wife cohab_with_other pre_marital_birth"

logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==2 & couple_educ_gp==0, or //  continuous paid hours - discrete time
outreg2 using "$results/psid_marriage_dissolution_nocoll_1980s.xls", sideway stats(coef pval) label ctitle(1 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace
logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_ `controls' if inlist(IN_UNIT,1,2)  & cohort==2 & couple_educ_gp==0, or //  continuous paid hours - discrete time
outreg2 using "$results/psid_marriage_dissolution_nocoll_1980s.xls", sideway stats(coef pval) label ctitle(1 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_hours_3070 TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==2 & couple_educ_gp==0, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_nocoll_1980s.xls", sideway stats(coef pval) label ctitle(2 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.hh_hours_3070 TAXABLE_HEAD_WIFE_ `controls' if inlist(IN_UNIT,1,2)  & cohort==2 & couple_educ_gp==0, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_nocoll_1980s.xls", sideway stats(coef pval) label ctitle(2 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur wife_housework_pct TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==2 & couple_educ_gp==0, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_nocoll_1980s.xls", sideway stats(coef pval) label ctitle(3 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur wife_housework_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==2 & couple_educ_gp==0, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_nocoll_1980s.xls", sideway stats(coef pval) label ctitle(3 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) appen

logit dissolve_lag i.dur i.housework_bkt TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==2 & couple_educ_gp==0, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_nocoll_1980s.xls", sideway stats(coef pval) label ctitle(4 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.housework_bkt TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2)  & cohort==2 & couple_educ_gp==0, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_nocoll_1980s.xls", sideway stats(coef pval) label ctitle(4 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur female_hours_pct wife_housework_pct TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==2 & couple_educ_gp==0, or // continuous paid and unpaid hours income coefficients to use
outreg2 using "$results/psid_marriage_dissolution_nocoll_1980s.xls", sideway stats(coef pval) label ctitle(5 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur female_hours_pct wife_housework_pct TAXABLE_HEAD_WIFE_ `controls' if inlist(IN_UNIT,1,2) & cohort==2 & couple_educ_gp==0, or // continuous paid and unpaid hours income coefficients to use
outreg2 using "$results/psid_marriage_dissolution_nocoll_1980s.xls", sideway stats(coef pval) label ctitle(5 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

** Cohort 3 (1990s)
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled REGION_ cohab_with_wife cohab_with_other pre_marital_birth"

logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or //  continuous paid hours - discrete time
outreg2 using "$results/psid_marriage_dissolution_nocoll_1990.xls", sideway stats(coef pval) label ctitle(1 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace
logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_ `controls'  if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or //  continuous paid hours - discrete time
outreg2 using "$results/psid_marriage_dissolution_nocoll_1990.xls", sideway stats(coef pval) label ctitle(1 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_hours_3070 TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_nocoll_1990.xls", sideway stats(coef pval) label ctitle(2 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.hh_hours_3070 TAXABLE_HEAD_WIFE_ `controls' if inlist(IN_UNIT,1,2)  & cohort==3 & couple_educ_gp==0, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_nocoll_1990.xls", sideway stats(coef pval) label ctitle(2 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur wife_housework_pct TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_nocoll_1990.xls", sideway stats(coef pval) label ctitle(3 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur wife_housework_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2)  & cohort==3 & couple_educ_gp==0, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_nocoll_1990.xls", sideway stats(coef pval) label ctitle(3 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) appen

logit dissolve_lag i.dur i.housework_bkt TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_nocoll_1990.xls", sideway stats(coef pval) label ctitle(4 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.housework_bkt TAXABLE_HEAD_WIFE_ `controls' if inlist(IN_UNIT,1,2)  & cohort==3 & couple_educ_gp==0, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_nocoll_1990.xls", sideway stats(coef pval) label ctitle(4 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur female_hours_pct wife_housework_pct TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or // continuous paid and unpaid hours income coefficients to use
outreg2 using "$results/psid_marriage_dissolution_nocoll_1990.xls", sideway stats(coef pval) label ctitle(5 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur female_hours_pct wife_housework_pct TAXABLE_HEAD_WIFE_ `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or // continuous paid and unpaid hours income coefficients to use
outreg2 using "$results/psid_marriage_dissolution_nocoll_1990.xls", sideway stats(coef pval) label ctitle(5 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// for future comparison: employment
logit dissolve_lag i.dur i.ft_head if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or // men's
logit dissolve_lag i.dur i.ft_head TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or // men's
logit dissolve_lag i.dur i.ft_wife if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or // women's
logit dissolve_lag i.dur i.ft_wife TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or // women's
logit dissolve_lag i.dur i.ft_head i.ft_wife if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or // both
logit dissolve_lag i.dur i.ft_head i.ft_wife TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or // both

logit dissolve_lag i.dur i.ft_head if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or // men's
logit dissolve_lag i.dur i.ft_head TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or // men's
logit dissolve_lag i.dur i.ft_wife if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or // women's
logit dissolve_lag i.dur i.ft_wife TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or // women's
logit dissolve_lag i.dur i.ft_head i.ft_wife if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or // both
logit dissolve_lag i.dur i.ft_head i.ft_wife TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or // both

********************************************************************************
* College
********************************************************************************
** Cohort 1 (1970s)
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled REGION_ cohab_with_wife cohab_with_other pre_marital_birth"

logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==1 & couple_educ_gp==1, or //  continuous paid hours - discrete time
outreg2 using "$results/psid_marriage_dissolution_college_1970s.xls", sideway stats(coef pval) label ctitle(1 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace
logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==1 & couple_educ_gp==1, or //  continuous paid hours - discrete time
outreg2 using "$results/psid_marriage_dissolution_college_1970s.xls", sideway stats(coef pval) label ctitle(1 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_hours_3070 TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==1 & couple_educ_gp==1, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_college_1970s.xls", sideway stats(coef pval) label ctitle(2 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.hh_hours_3070 TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==1 & couple_educ_gp==1, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_college_1970s.xls", sideway stats(coef pval) label ctitle(2 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur wife_housework_pct TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==1 & couple_educ_gp==1, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_college_1970s.xls", sideway stats(coef pval) label ctitle(3 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur wife_housework_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==1 & couple_educ_gp==1, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_college_1970s.xls", sideway stats(coef pval) label ctitle(3 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) appen

logit dissolve_lag i.dur i.housework_bkt TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==1 & couple_educ_gp==1, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_college_1970s.xls", sideway stats(coef pval) label ctitle(4 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.housework_bkt TAXABLE_HEAD_WIFE_ `controls' if inlist(IN_UNIT,1,2)  & cohort==1 & couple_educ_gp==1, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_college_1970s.xls", sideway stats(coef pval) label ctitle(4 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur female_hours_pct wife_housework_pct TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==1 & couple_educ_gp==1, or // continuous paid and unpaid hours income coefficients to use
outreg2 using "$results/psid_marriage_dissolution_college_1970s.xls", sideway stats(coef pval) label ctitle(5 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur female_hours_pct wife_housework_pct TAXABLE_HEAD_WIFE_ `controls' if inlist(IN_UNIT,1,2) & cohort==1 & couple_educ_gp==1, or // continuous paid and unpaid hours income coefficients to use
outreg2 using "$results/psid_marriage_dissolution_college_1970s.xls", sideway stats(coef pval) label ctitle(5 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

** Cohort 2 (1980s)
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled REGION_ cohab_with_wife cohab_with_other pre_marital_birth"

logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==2 & couple_educ_gp==1, or //  continuous paid hours - discrete time
outreg2 using "$results/psid_marriage_dissolution_college_1980s.xls", sideway stats(coef pval) label ctitle(1 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace
logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_ `controls' if inlist(IN_UNIT,1,2)  & cohort==2 & couple_educ_gp==1, or //  continuous paid hours - discrete time
outreg2 using "$results/psid_marriage_dissolution_college_1980s.xls", sideway stats(coef pval) label ctitle(1 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_hours_3070 TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==2 & couple_educ_gp==1, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_college_1980s.xls", sideway stats(coef pval) label ctitle(2 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.hh_hours_3070 TAXABLE_HEAD_WIFE_ `controls' if inlist(IN_UNIT,1,2)  & cohort==2 & couple_educ_gp==1, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_college_1980s.xls", sideway stats(coef pval) label ctitle(2 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur wife_housework_pct TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==2 & couple_educ_gp==1, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_college_1980s.xls", sideway stats(coef pval) label ctitle(3 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur wife_housework_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==2 & couple_educ_gp==1, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_college_1980s.xls", sideway stats(coef pval) label ctitle(3 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) appen

logit dissolve_lag i.dur i.housework_bkt TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==2 & couple_educ_gp==1, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_college_1980s.xls", sideway stats(coef pval) label ctitle(4 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.housework_bkt TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2)  & cohort==2 & couple_educ_gp==1, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_college_1980s.xls", sideway stats(coef pval) label ctitle(4 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur female_hours_pct wife_housework_pct TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==2 & couple_educ_gp==1, or // continuous paid and unpaid hours income coefficients to use
outreg2 using "$results/psid_marriage_dissolution_college_1980s.xls", sideway stats(coef pval) label ctitle(5 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur female_hours_pct wife_housework_pct TAXABLE_HEAD_WIFE_ `controls' if inlist(IN_UNIT,1,2) & cohort==2 & couple_educ_gp==1, or // continuous paid and unpaid hours income coefficients to use
outreg2 using "$results/psid_marriage_dissolution_college_1980s.xls", sideway stats(coef pval) label ctitle(5 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

** Cohort 3 (1990s)
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled REGION_ cohab_with_wife cohab_with_other pre_marital_birth"

logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or //  continuous paid hours - discrete time
outreg2 using "$results/psid_marriage_dissolution_college_1990.xls", sideway stats(coef pval) label ctitle(1 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace
logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_ `controls'  if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or //  continuous paid hours - discrete time
outreg2 using "$results/psid_marriage_dissolution_college_1990.xls", sideway stats(coef pval) label ctitle(1 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_hours_3070 TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_college_1990.xls", sideway stats(coef pval) label ctitle(2 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.hh_hours_3070 TAXABLE_HEAD_WIFE_ `controls' if inlist(IN_UNIT,1,2)  & cohort==3 & couple_educ_gp==1, or // paid hours - bucketed
outreg2 using "$results/psid_marriage_dissolution_college_1990.xls", sideway stats(coef pval) label ctitle(2 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur wife_housework_pct TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_college_1990.xls", sideway stats(coef pval) label ctitle(3 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur wife_housework_pct TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2)  & cohort==3 & couple_educ_gp==1, or //  continuous housework
outreg2 using "$results/psid_marriage_dissolution_college_1990.xls", sideway stats(coef pval) label ctitle(3 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) appen

logit dissolve_lag i.dur i.housework_bkt TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_college_1990.xls", sideway stats(coef pval) label ctitle(4 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur i.housework_bkt TAXABLE_HEAD_WIFE_ `controls' if inlist(IN_UNIT,1,2)  & cohort==3 & couple_educ_gp==1, or // housework - bucketed
outreg2 using "$results/psid_marriage_dissolution_college_1990.xls", sideway stats(coef pval) label ctitle(4 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur female_hours_pct wife_housework_pct TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or // continuous paid and unpaid hours income coefficients to use
outreg2 using "$results/psid_marriage_dissolution_college_1990.xls", sideway stats(coef pval) label ctitle(5 Base) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
logit dissolve_lag i.dur female_hours_pct wife_housework_pct TAXABLE_HEAD_WIFE_ `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or // continuous paid and unpaid hours income coefficients to use
outreg2 using "$results/psid_marriage_dissolution_college_1990.xls", sideway stats(coef pval) label ctitle(5 Controls) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// should not have both births in same model because they are essentially inverse. if anything, pre-marital birth flag, then do they have a child together as another flag (might not be their first); look into this
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled REGION_ cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or //  continuous paid hours - discrete time
logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_ `controls'  if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or //  continuous paid hours - discrete time

logit dissolve_lag i.dur i.hh_hours_3070 TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, or // paid hours - bucketed
logit dissolve_lag i.dur i.hh_hours_3070 TAXABLE_HEAD_WIFE_ `controls' if inlist(IN_UNIT,1,2)  & cohort==3 & couple_educ_gp==1, or // paid hours - bucketed
*/

**Testing time interaction (college)
logit dissolve_lag i.dur c.female_hours_pct##i.cohort TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & couple_educ_gp==1 & cohort <4, or //  continuous paid hours - discrete time
margins, at(cohort=(1 2 3) female_hours_pct=(.1(.2).9))

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled REGION_ cohab_with_wife cohab_with_other pre_marital_birth post_marital_birth"

logit dissolve_lag i.dur i.hh_hours_3070##i.cohort TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & couple_educ_gp==1 & cohort <4 & hh_hours_3070<4, or // paid hours - bucketed
margins cohort#hh_hours_3070
marginsplot

**Testing interaction in 1990s with class
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled REGION_ cohab_with_wife cohab_with_other pre_marital_birth post_marital_birth"

logit dissolve_lag i.dur c.female_hours_pct##i.couple_educ_gp TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3, or //  continuous paid hours - discrete time
margins, at(couple_educ_gp=(0 1) female_hours_pct=(.1(.2).9))
marginsplot

logit dissolve_lag i.dur i.hh_hours_3070##i.couple_educ_gp TAXABLE_HEAD_WIFE_  `controls' if inlist(IN_UNIT,1,2) & cohort==3 & hh_hours_3070<4, or // paid hours - bucketed
margins couple_educ_gp#hh_hours_3070
marginsplot


**** Race differences
logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_ i.couple_educ_gp if inlist(IN_UNIT,1,2) & cohort==3 & race_wife==1, or //  continuous paid hours - discrete time
logit dissolve_lag i.dur i.hh_hours_3070 TAXABLE_HEAD_WIFE_ i.couple_educ_gp if inlist(IN_UNIT,1,2) & cohort==3 & race_wife==1, or // paid hours - bucketed
logit dissolve_lag i.dur wife_housework_pct TAXABLE_HEAD_WIFE_ i.couple_educ_gp if inlist(IN_UNIT,1,2) & cohort==3 & race_wife==1, or //  continuous housework
logit dissolve_lag i.dur i.housework_bkt TAXABLE_HEAD_WIFE_ i.couple_educ_gp if inlist(IN_UNIT,1,2) & cohort==3 & race_wife==1, or // housework - bucketed

logit dissolve_lag i.dur female_hours_pct TAXABLE_HEAD_WIFE_ i.couple_educ_gp if inlist(IN_UNIT,1,2) & cohort==3 & race_wife==2, or //  continuous paid hours - discrete time
// for blacks, WITHOUT controls, female hours are actually stabilizing
logit dissolve_lag i.dur i.hh_hours_3070 TAXABLE_HEAD_WIFE_ i.couple_educ_gp if inlist(IN_UNIT,1,2) & cohort==3 & race_wife==2, or // paid hours - bucketed
// AND without controls, male BW = more risk
logit dissolve_lag i.dur wife_housework_pct TAXABLE_HEAD_WIFE_ i.couple_educ_gp if inlist(IN_UNIT,1,2) & cohort==3 & race_wife==2, or //  continuous housework
logit dissolve_lag i.dur i.housework_bkt TAXABLE_HEAD_WIFE_ i.couple_educ_gp if inlist(IN_UNIT,1,2) & cohort==3 & race_wife==2, or // housework - bucketed

logit dissolve_lag i.dur i.hh_hours_3070 TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0 & race_wife==2, or // paid hours - bucketed
logit dissolve_lag i.dur i.hh_hours_3070 TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0 & race_wife==1, or // paid hours - bucketed

logit dissolve_lag i.dur i.hh_hours_3070 TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1 & race_wife==2, or // paid hours - bucketed
logit dissolve_lag i.dur i.hh_hours_3070 TAXABLE_HEAD_WIFE_ if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1 & race_wife==1, or // paid hours - bucketed


********************************************************************************
**# Misc
********************************************************************************

tab hh_earn_type_bkd, sum(TAXABLE_HEAD_WIFE_)

// margins for figure
local controls "i.race_head i.same_race i.children i.either_enrolled TAXABLE_HEAD_WIFE_ i.religion_head age_mar_head age_mar_wife"
logit dissolve_lag dur i.hh_earn_type_bkd `controls' if couple_educ_gp==0 & inlist(IN_UNIT,1,2), or
margins hh_earn_type_bkd

logit dissolve_lag dur i.hh_earn_type_bkd `controls' if couple_educ_gp==1 & inlist(IN_UNIT,1,2), or
margins hh_earn_type_bkd

/* Power analysis */
* Need sample sizes - okay get from descriptives below
tab couple_educ_gp if inlist(IN_UNIT,1,2) & cohort==3
tab couple_educ_gp hh_earn_type if inlist(IN_UNIT,1,2) & cohort==3

power twomeans -0.0104 0.0014 ,n(7859) // male bw
power twomeans -0.0096 0.0122, n(1959) // female bw - lol 7% power
power twomeans -0.0096 0.0122 // would need n of ~60,000 for this difference

power twomeans 0.0110 0.0076, n(7992) // wife FT - if means, 5%
power twoproportions 0.0110 0.0076, n(7992) // wife FT - if proportions, 35%

********************************************************************************
*Quick descriptives for proposal
********************************************************************************
/*/ total
tab couple_educ_gp
unique id, by(couple_educ_gp)

tab couple_educ_gp hh_earn_type_bkd, row
tab couple_educ_gp hh_earn_type_mar, row
tabstat female_earn_pct, by(couple_educ_gp)
tab couple_educ_gp ft_head, row
tab couple_educ_gp ft_wife, row
tabstat wife_housework_pct, by(couple_educ_gp)
tabstat TAXABLE_HEAD_WIFE_, by(couple_educ_gp) stat(mean p50)
tabstat dur, by(couple_educ_gp) stat(mean p50)
tabstat age_mar_head, by(couple_educ_gp) stat(mean p50)
tabstat age_mar_wife, by(couple_educ_gp) stat(mean p50)
 
 
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
tabstat dur if dissolve_lag==1, by(couple_educ_gp) stat(mean p50)
tabstat age_mar_head if dissolve_lag==1, by(couple_educ_gp) stat(mean p50)
tabstat age_mar_wife if dissolve_lag==1, by(couple_educ_gp) stat(mean p50)

// pie chart
tab couple_educ_gp hh_earn_type_bkd if dissolve_lag==1, row nofreq
tab couple_educ_gp hh_earn_type_bkd if dissolve_lag==0, row nofreq // intact for ref
 */
 
********************************************************************************
**# Descriptive statistics

*PAA Final
********************************************************************************
// all
// restrictions on models: inlist(IN_UNIT,1,2) & cohort==3
tab hh_earn_type, gen(earn_type)
tab housework_bkt, gen(hw_type)
tab couple_work, gen(couple_work)

tab couple_educ_gp if cohort==3 & inlist(IN_UNIT,1,2)
unique id if cohort==3 & inlist(IN_UNIT,1,2), by(couple_educ_gp) // unique couples
unique id if cohort==3 & inlist(IN_UNIT,1,2) & dissolve_lag==1, by(couple_educ_gp) // dissolutions

tabstat female_earn_pct  if cohort==3 & inlist(IN_UNIT,1,2), by(couple_educ_gp)
tab couple_educ_gp hh_earn_type if cohort==3 & inlist(IN_UNIT,1,2), row
tab couple_educ_gp couple_work if cohort==3 & inlist(IN_UNIT,1,2), row
tab couple_educ_gp ft_head if cohort==3 & inlist(IN_UNIT,1,2), row
tab couple_educ_gp ft_wife if cohort==3 & inlist(IN_UNIT,1,2), row
tabstat wife_housework_pct if cohort==3 & inlist(IN_UNIT,1,2), by(couple_educ_gp)
tab couple_educ_gp housework_bkt if cohort==3 & inlist(IN_UNIT,1,2), row
tabstat TAXABLE_HEAD_WIFE_ if cohort==3 & inlist(IN_UNIT,1,2), by(couple_educ_gp) stat(mean p50)
tabstat earnings_wife if cohort==3 & inlist(IN_UNIT,1,2), by(couple_educ_gp) stat(mean p50)
tabstat earnings_head if cohort==3 & inlist(IN_UNIT,1,2), by(couple_educ_gp) stat(mean p50)
tabstat housework_wife if cohort==3 & inlist(IN_UNIT,1,2), by(couple_educ_gp) stat(mean p50)
tabstat housework_head if cohort==3 & inlist(IN_UNIT,1,2), by(couple_educ_gp) stat(mean p50)

ttest TAXABLE_HEAD_WIFE_ if cohort==3 & inlist(IN_UNIT,1,2), by(couple_educ_gp)
ttest female_earn_pct if cohort==3 & inlist(IN_UNIT,1,2), by(couple_educ_gp)
tab couple_educ_gp hh_earn_type if cohort==3 & inlist(IN_UNIT,1,2), chi2
ttest earn_type1 if cohort==3 & inlist(IN_UNIT,1,2), by(couple_educ_gp)
ttest earn_type2 if cohort==3 & inlist(IN_UNIT,1,2), by(couple_educ_gp)
ttest earn_type3 if cohort==3 & inlist(IN_UNIT,1,2), by(couple_educ_gp)
ttest ft_head if cohort==3 & inlist(IN_UNIT,1,2), by(couple_educ_gp)
ttest ft_wife if cohort==3 & inlist(IN_UNIT,1,2), by(couple_educ_gp)

ttest couple_work1 if cohort==3 & inlist(IN_UNIT,1,2), by(couple_educ_gp)
ttest couple_work2 if cohort==3 & inlist(IN_UNIT,1,2), by(couple_educ_gp)
ttest couple_work3 if cohort==3 & inlist(IN_UNIT,1,2), by(couple_educ_gp)
ttest couple_work4 if cohort==3 & inlist(IN_UNIT,1,2), by(couple_educ_gp)

ttest wife_housework_pct if cohort==3 & inlist(IN_UNIT,1,2), by(couple_educ_gp)
tab couple_educ_gp housework_bkt if cohort==3 & inlist(IN_UNIT,1,2), chi2
ttest hw_type1 if cohort==3 & inlist(IN_UNIT,1,2), by(couple_educ_gp)
ttest hw_type2 if cohort==3 & inlist(IN_UNIT,1,2), by(couple_educ_gp)
ttest hw_type3 if cohort==3 & inlist(IN_UNIT,1,2), by(couple_educ_gp)

ttest earnings_wife if cohort==3 & inlist(IN_UNIT,1,2), by(couple_educ_gp)
ttest earnings_head if cohort==3 & inlist(IN_UNIT,1,2), by(couple_educ_gp)
ttest housework_wife if cohort==3 & inlist(IN_UNIT,1,2), by(couple_educ_gp)
ttest housework_head if cohort==3 & inlist(IN_UNIT,1,2), by(couple_educ_gp)

tabstat dur if cohort==3 & inlist(IN_UNIT,1,2), by(couple_educ_gp) stat(mean p50)
tabstat age_mar_head if cohort==3 & inlist(IN_UNIT,1,2), by(couple_educ_gp) stat(mean p50)
tabstat age_mar_wife if cohort==3 & inlist(IN_UNIT,1,2), by(couple_educ_gp) stat(mean p50)
tab race_head couple_educ_gp if cohort==3 & inlist(IN_UNIT,1,2), col nofreq
tab couple_educ_gp same_race if cohort==3 & inlist(IN_UNIT,1,2), row nofreq
tab couple_educ_gp either_enrolled if cohort==3 & inlist(IN_UNIT,1,2), row nofreq
tab couple_educ_gp cohab_with_wife if cohort==3 & inlist(IN_UNIT,1,2), row nofreq
tab couple_educ_gp cohab_with_other if cohort==3 & inlist(IN_UNIT,1,2), row nofreq
tab couple_educ_gp pre_marital_birth if cohort==3 & inlist(IN_UNIT,1,2), row nofreq


// dissolved
tabstat female_earn_pct  if cohort==3 & inlist(IN_UNIT,1,2) & dissolve_lag==1, by(couple_educ_gp)
tab couple_educ_gp hh_earn_type if cohort==3 & inlist(IN_UNIT,1,2) & dissolve_lag==1, row
tab couple_educ_gp couple_work if cohort==3 & inlist(IN_UNIT,1,2) & dissolve_lag==1, row
tab couple_educ_gp ft_head if cohort==3 & inlist(IN_UNIT,1,2) & dissolve_lag==1, row
tab couple_educ_gp ft_wife if cohort==3 & inlist(IN_UNIT,1,2) & dissolve_lag==1, row
tabstat wife_housework_pct if cohort==3 & inlist(IN_UNIT,1,2) & dissolve_lag==1, by(couple_educ_gp)
tab couple_educ_gp housework_bkt if cohort==3 & inlist(IN_UNIT,1,2) & dissolve_lag==1, row
tabstat TAXABLE_HEAD_WIFE_ if cohort==3 & inlist(IN_UNIT,1,2) & dissolve_lag==1, by(couple_educ_gp) stat(mean p50)
tabstat earnings_wife if cohort==3 & inlist(IN_UNIT,1,2) & dissolve_lag==1, by(couple_educ_gp) stat(mean p50)
tabstat earnings_head if cohort==3 & inlist(IN_UNIT,1,2) & dissolve_lag==1, by(couple_educ_gp) stat(mean p50)
tabstat housework_wife if cohort==3 & inlist(IN_UNIT,1,2) & dissolve_lag==1, by(couple_educ_gp) stat(mean p50)
tabstat housework_head if cohort==3 & inlist(IN_UNIT,1,2) & dissolve_lag==1, by(couple_educ_gp) stat(mean p50)

tabstat dur if cohort==3 & inlist(IN_UNIT,1,2) & dissolve_lag==1, by(couple_educ_gp) stat(mean p50)
tabstat age_mar_head if cohort==3 & inlist(IN_UNIT,1,2) & dissolve_lag==1, by(couple_educ_gp) stat(mean p50)
tabstat age_mar_wife if cohort==3 & inlist(IN_UNIT,1,2) & dissolve_lag==1, by(couple_educ_gp) stat(mean p50)
tab race_head couple_educ_gp if cohort==3 & inlist(IN_UNIT,1,2) & dissolve_lag==1, col nofreq
tab couple_educ_gp same_race if cohort==3 & inlist(IN_UNIT,1,2) & dissolve_lag==1, row nofreq
tab couple_educ_gp either_enrolled if cohort==3 & inlist(IN_UNIT,1,2) & dissolve_lag==1, row nofreq
tab couple_educ_gp cohab_with_wife if cohort==3 & inlist(IN_UNIT,1,2) & dissolve_lag==1, row nofreq
tab couple_educ_gp cohab_with_other if cohort==3 & inlist(IN_UNIT,1,2) & dissolve_lag==1, row nofreq
tab couple_educ_gp pre_marital_birth if cohort==3 & inlist(IN_UNIT,1,2) & dissolve_lag==1, row nofreq

/// tests - dissolved v. not by group
** No College
ttest TAXABLE_HEAD_WIFE_ if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==0, by(dissolve_lag)
ttest female_earn_pct if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==0, by(dissolve_lag)
tab dissolve_lag hh_earn_type if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==0, chi2
ttest earn_type1 if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==0, by(dissolve_lag)
ttest earn_type2 if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==0, by(dissolve_lag)
ttest earn_type3 if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==0, by(dissolve_lag)
ttest earnings_wife if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==0, by(dissolve_lag)
ttest earnings_head if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==0, by(dissolve_lag)
ttest couple_work1 if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==0, by(dissolve_lag)
ttest couple_work2 if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==0, by(dissolve_lag)
ttest couple_work3 if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==0, by(dissolve_lag)
ttest couple_work4 if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==0, by(dissolve_lag)
ttest ft_head if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==0, by(dissolve_lag)
ttest ft_wife if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==0, by(dissolve_lag)

ttest wife_housework_pct if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==0, by(dissolve_lag)
tab dissolve_lag housework_bkt if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==0, chi2
ttest hw_type1 if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==0, by(dissolve_lag)
ttest hw_type2 if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==0, by(dissolve_lag)
ttest hw_type3 if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==0, by(dissolve_lag)
ttest housework_wife if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==0, by(dissolve_lag)
ttest housework_head if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==0, by(dissolve_lag)

** College
ttest TAXABLE_HEAD_WIFE_ if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==1, by(dissolve_lag)
ttest female_earn_pct if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==1, by(dissolve_lag)
tab dissolve_lag hh_earn_type if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==1, chi2
ttest earn_type1 if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==1, by(dissolve_lag)
ttest earn_type2 if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==1, by(dissolve_lag)
ttest earn_type3 if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==1, by(dissolve_lag)
ttest earnings_wife if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==1, by(dissolve_lag)
ttest earnings_head if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==1, by(dissolve_lag)
ttest couple_work1 if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==1, by(dissolve_lag)
ttest couple_work2 if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==1, by(dissolve_lag)
ttest couple_work3 if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==1, by(dissolve_lag)
ttest couple_work4 if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==1, by(dissolve_lag)
ttest ft_head if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==1, by(dissolve_lag)
ttest ft_wife if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==1, by(dissolve_lag)

ttest wife_housework_pct if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==1, by(dissolve_lag)
tab dissolve_lag housework_bkt if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==1, chi2
ttest hw_type1 if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==1, by(dissolve_lag)
ttest hw_type2 if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==1, by(dissolve_lag)
ttest hw_type3 if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==1, by(dissolve_lag)
ttest housework_wife if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==1, by(dissolve_lag)
ttest housework_head if cohort==3 & inlist(IN_UNIT,1,2) & couple_educ_gp==1, by(dissolve_lag)

********************************************************************************
*PAA Abstract
********************************************************************************
// all
 
tab couple_educ_gp if cohort==3
unique id if cohort==3, by(couple_educ_gp) // unique couples
unique id if cohort==3 & dissolve_lag==1, by(couple_educ_gp) // dissolutions

tabstat female_hours_pct  if cohort==3, by(couple_educ_gp)
tab couple_educ_gp hh_hours_type if cohort==3, row
tabstat female_earn_pct  if cohort==3, by(couple_educ_gp)
tab couple_educ_gp hh_earn_type if cohort==3, row
tabstat wife_housework_pct if cohort==3, by(couple_educ_gp)
tab couple_educ_gp housework_bkt if cohort==3, row
tab couple_educ_gp ft_head if cohort==3, row
tab couple_educ_gp ft_wife if cohort==3, row
tabstat TAXABLE_HEAD_WIFE_ if cohort==3, by(couple_educ_gp) stat(mean p50)

// dissolved
tabstat female_hours_pct  if cohort==3 & dissolve_lag==1, by(couple_educ_gp)
tab couple_educ_gp hh_hours_type if cohort==3 & dissolve_lag==1, row
tabstat female_earn_pct  if cohort==3 & dissolve_lag==1, by(couple_educ_gp)
tab couple_educ_gp hh_earn_type if cohort==3 & dissolve_lag==1, row
tabstat wife_housework_pct if cohort==3 & dissolve_lag==1, by(couple_educ_gp)
tab couple_educ_gp housework_bkt if cohort==3 & dissolve_lag==1, row
tab couple_educ_gp ft_head if cohort==3 & dissolve_lag==1, row
tab couple_educ_gp ft_wife if cohort==3 & dissolve_lag==1, row
tabstat TAXABLE_HEAD_WIFE_ if cohort==3 & dissolve_lag==1, by(couple_educ_gp) stat(mean p50)


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



********************************************************************************
* Robustness check on women's earnings
********************************************************************************
tabstat dur if inlist(IN_UNIT,1,2) & cohort==3, by(ever_dissolve) stats(mean p50)
tabstat dur if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, by(ever_dissolve) stats(mean p50)
tabstat dur if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==1, by(ever_dissolve) stats(mean p50)

preserve
collapse (median) female_earn_pct earnings_wife earnings_head if inlist(IN_UNIT,1,2) & cohort==3, by(dur couple_educ_gp)
twoway (line female_earn_pct dur if dur <=20 & couple_educ_gp==0) (line female_earn_pct dur if dur <=20 & couple_educ_gp==1), legend(on order(1 "No College" 2 "College"))
// graph export "$results\earn_pct_education.jpg", as(jpg) name("Graph") quality(90) replace
restore

preserve
collapse (median) female_earn_pct earnings_wife earnings_head if inlist(IN_UNIT,1,2) & cohort==3, by(dur couple_educ_gp ever_dissolve)
twoway (line female_earn_pct dur if dur <=20 & couple_educ_gp==0 & ever_dissolve==0) (line female_earn_pct dur if dur <=20 & couple_educ_gp==0 & ever_dissolve==1) (line female_earn_pct dur if dur <=20 & couple_educ_gp==1 & ever_dissolve==0) (line female_earn_pct dur if dur <=20 & couple_educ_gp==1 & ever_dissolve==1), legend(on order(1 "NC - Intact" 2 "NC - Dissolved" 3 "Coll - Intact" 4 "Coll-Dissolved"))

twoway (line female_earn_pct dur if dur <=10 & couple_educ_gp==0 & ever_dissolve==0) (line female_earn_pct dur if dur <=10 & couple_educ_gp==0 & ever_dissolve==1) (line female_earn_pct dur if dur <=10 & couple_educ_gp==1 & ever_dissolve==0) (line female_earn_pct dur if dur <=10 & couple_educ_gp==1 & ever_dissolve==1), legend(on order(1 "NC - Intact" 2 "NC - Dissolved" 3 "Coll - Intact" 4 "Coll-Dissolved"))

twoway (line earnings_wife dur if dur <=10 & couple_educ_gp==0 & ever_dissolve==0) (line earnings_wife dur if dur <=10 & couple_educ_gp==0 & ever_dissolve==1) (line earnings_wife dur if dur <=10 & couple_educ_gp==1 & ever_dissolve==0) (line earnings_wife dur if dur <=10 & couple_educ_gp==1 & ever_dissolve==1), legend(on order(1 "NC - Intact" 2 "NC - Dissolved" 3 "Coll - Intact" 4 "Coll-Dissolved"))
// graph export "$results\earn_pct_educ_x_dissolved.jpg", as(jpg) name("Graph") quality(90) replace
restore

// to standardize on TIME TO DIVORCE
by id: egen rel_end_temp= max(survey_yr) if rel_end_all==9998
replace rel_end_all = rel_end_temp if rel_end_all==9998

gen transition_dur=.
replace transition_dur = survey_yr-rel_end_all
replace transition_dur = dur if transition_dur==. // should be all those intact

// browse id dur transition_dur survey_yr rel_end_all

preserve
collapse (median) female_earn_pct earnings_wife earnings_head if inlist(IN_UNIT,1,2) & cohort==3, by(transition_dur ever_dissolve couple_educ_gp)

twoway (line female_earn_pct transition_dur if ever_dissolve==1 & couple_educ_gp==0 & transition_dur<=0 & transition_dur>=-15) (line female_earn_pct transition_dur if ever_dissolve==1 & couple_educ_gp==1 & transition_dur<=0 & transition_dur>=-15), legend(on order(1 "Dissolved, Non" 2 "Dissolved, College"))

twoway (line earnings_wife transition_dur if ever_dissolve==1 & couple_educ_gp==0 & transition_dur<=0 & transition_dur>=-15) (line earnings_wife transition_dur if ever_dissolve==1 & couple_educ_gp==1 & transition_dur<=0 & transition_dur>=-15), legend(on order(1 "Dissolved, Non" 2 "Dissolved, College"))

twoway (line earnings_head transition_dur if ever_dissolve==1 & couple_educ_gp==0 & transition_dur<=0 & transition_dur>=-15) (line earnings_head transition_dur if ever_dissolve==1 & couple_educ_gp==1 & transition_dur<=0 & transition_dur>=-15), legend(on order(1 "Dissolved, Non" 2 "Dissolved, College"))

restore

unique id if ever_dissolve==1 & couple_educ_gp==1 & inlist(IN_UNIT,1,2) & cohort==3, by(dur)

/*
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
*/

