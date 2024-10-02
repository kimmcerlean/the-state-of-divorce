********************************************************************************
* Table for descriptive statistics
* descriptive statistics.do
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

gen cohort_v3=.
replace cohort_v3=0 if inrange(rel_start_all,1970,1994)
replace cohort_v3=1 if inrange(rel_start_all,1995,2014)

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

// create binary home ownership variable
gen home_owner=0
replace home_owner=1 if HOUSE_STATUS_==1

// create dummy variable for interval length
gen interval=.
replace interval=1 if inrange(survey_yr,1968,1997)
replace interval=2 if inrange(survey_yr,1999,2019)

// need to combine weight variables
gen weight=.
replace weight=CORE_WEIGHT_ if inrange(survey_yr,1968,1992)
replace weight=COR_IMM_WT_ if inrange(survey_yr,1993,2019)

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

// race x educ
gen race_x_educ_head=.
replace race_x_educ_head=1 if race_head==1 & college_complete_head==0 // white no college
replace race_x_educ_head=2 if race_head==1 & college_complete_head==1 // white college
replace race_x_educ_head=3 if race_head==2 & college_complete_head==0 // black no college
replace race_x_educ_head=4 if race_head==2 & college_complete_head==1 // black college

label define race_x_educ 1 "White No" 2 "White Coll" 3 "Black No" 4 "Black Coll"
label values race_x_educ_head race_x_educ

********************************************************************************
**# Table starts here: over time
********************************************************************************
keep if inlist(cohort_v3,0,1) & inlist(IN_UNIT,0,1,2)
tab hh_earn_type, gen(earn_type)
tab housework_bkt, gen(hw_type)
tab couple_educ_gp, gen(couple_educ)

putexcel set "$results/Table1_Descriptives_time", replace
putexcel B1:C1 = "Total", merge border(bottom)
putexcel D1:E1 = "No College", merge border(bottom)
putexcel F1:G1 = "College-Educated", merge border(bottom)
putexcel B2 = ("Early") C2 = ("Late") D2 = ("Early") E2 = ("Late") F2 = ("Early") G2 = ("Late") , border(bottom)
putexcel A3 = "Unique Couples"
putexcel A4 = "% Dissolved"

// Means
putexcel A5 = "Wife's share of earnings"
putexcel A6 = "Dual Earning HH"
putexcel A7 = "Male Breadwinner"
putexcel A8 = "Female Breadwinner"
putexcel A9 = "Wife's share of unpaid hours"
putexcel A10 = "Equal"
putexcel A11 = "Female Primary"
putexcel A12 = "Male Primary"
putexcel A13 = "Average marital duration"
putexcel A14 = "Age at marriage (wife)"
putexcel A15 = "Age at marriage (husband)"
putexcel A16 = "Couple owns home"
putexcel A17 = "Couple has children"
putexcel A18 = "Average number of children"
putexcel A19 = "Cohabited prior to marriage"
putexcel A20 = "Had premarital birth"
putexcel A21 = "No College Degree"
putexcel A22 = "College Degree"

local meanvars_ovrl "female_earn_pct earn_type1 earn_type2 earn_type3 wife_housework_pct hw_type1 hw_type2 hw_type3 dur age_mar_wife age_mar_head home_owner  children NUM_CHILDREN_  cohab_with_wife pre_marital_birth couple_educ1 couple_educ2 "
local meanvars "female_earn_pct earn_type1 earn_type2 earn_type3 wife_housework_pct hw_type1 hw_type2 hw_type3 dur age_mar_wife age_mar_head home_owner  children NUM_CHILDREN_ cohab_with_wife pre_marital_birth"


// Overall: early
forvalues w=1/18{
	local row=`w'+4
	local var: word `w' of `meanvars_ovrl'
	mean `var' if cohort_v3==0
	matrix t`var'= e(b)
	putexcel B`row' = matrix(t`var'), nformat(#.#%)
}


// Overall: late
forvalues w=1/18{
	local row=`w'+4
	local var: word `w' of `meanvars_ovrl'
	mean `var' if cohort_v3==1
	matrix t`var'= e(b)
	putexcel C`row' = matrix(t`var'), nformat(#.#%)
}


**By education:


// No college degree: early
forvalues w=1/16{
	local row=`w'+4
	local var: word `w' of `meanvars'
	mean `var' if couple_educ_gp==0 & cohort_v3==0
	matrix t`var'= e(b)
	putexcel D`row' = matrix(t`var'), nformat(#.#%)
}

// No college degree: late
forvalues w=1/16{
	local row=`w'+4
	local var: word `w' of `meanvars'
	mean `var' if couple_educ_gp==0 & cohort_v3==1
	matrix t`var'= e(b)
	putexcel E`row' = matrix(t`var'), nformat(#.#%)
}

// College degree: early
forvalues w=1/16{
	local row=`w'+4
	local var: word `w' of `meanvars'
	mean `var' if couple_educ_gp==1 & cohort_v3==0
	matrix t`var'= e(b)
	putexcel F`row' = matrix(t`var'), nformat(#.#%)
}

// College degree: late
forvalues w=1/16{
	local row=`w'+4
	local var: word `w' of `meanvars'
	mean `var' if couple_educ_gp==1 & cohort_v3==1
	matrix t`var'= e(b)
	putexcel G`row' = matrix(t`var'), nformat(#.#%)
}

// uniques
*Overall
unique id if cohort_v3==0
unique id if cohort_v3==0 & dissolve_lag==1
unique id if cohort_v3==1
unique id if cohort_v3==1 & dissolve_lag==1

*No College
unique id if cohort_v3==0 & couple_educ_gp==0
unique id if cohort_v3==0 & dissolve_lag==1 & couple_educ_gp==0
unique id if cohort_v3==1 & couple_educ_gp==0
unique id if cohort_v3==1 & dissolve_lag==1 & couple_educ_gp==0

*College
unique id if cohort_v3==0 & couple_educ_gp==1
unique id if cohort_v3==0 & dissolve_lag==1 & couple_educ_gp==1
unique id if cohort_v3==1 & couple_educ_gp==1
unique id if cohort_v3==1 & dissolve_lag==1 & couple_educ_gp==1

********************************************************************************
**# Table starts here: when just focused on contemporary marriages
********************************************************************************
keep if cohort==3 & inlist(IN_UNIT,0,1,2) // for ease just remove those not in sample

tab hh_earn_type, gen(earn_type)
tab housework_bkt, gen(hw_type)
tab couple_work, gen(couple_work)
tab educ_wife, gen(educ_wife)
tab educ_head, gen(educ_head)
tab interval, gen(interval)
tab race_x_educ_head, gen(race_x_educ_head)

gen wife_white=0
replace wife_white=1 if race_wife==1
gen wife_black=0
replace wife_black=1 if race_wife==2

gen head_white=0
replace head_white=1 if race_head==1
gen head_black=0
replace head_black=1 if race_head==2

putexcel set "$results/Table1_Descriptives", replace
putexcel B1:C1 = "Total", merge border(bottom)
putexcel D1:E1 = "No College", merge border(bottom)
putexcel F1:G1 = "College-Educated", merge border(bottom)
putexcel B2 = ("All") C2 = ("Dissolved") D2 = ("All") E2 = ("Dissolved") F2 = ("All") G2 = ("Dissolved") , border(bottom)
putexcel A3 = "Unique Couples"
putexcel A4 = "% Dissolved"

// Means
putexcel A6 = "Wife's share of earnings"
putexcel A7 = "Husband employed full-time"
putexcel A8 = "Wife employed full-time"
putexcel A9 = "Dual Earning HH"
putexcel A10 = "Male Breadwinner"
putexcel A11 = "Female Breadwinner"
putexcel A12 = "Wife's share of unpaid hours"
putexcel A13 = "Equal"
putexcel A14 = "Female Primary"
putexcel A15 = "Male Primary"
putexcel A16 = "Wife's race: White"
putexcel A17 = "Wife's race: Black"
putexcel A18 = "Husband's race: White"
putexcel A19 = "Husband's race: Black"
putexcel A20 = "Wife's education: LTHS"
putexcel A21 = "Wife's education: hs"
putexcel A22 = "Wife's education: some college"
putexcel A23 = "Wife's education: college"
putexcel A24 = "husband's education: LTHS"
putexcel A25 = "husband's education: hs"
putexcel A26 = "husband's education: some college"
putexcel A27 = "husband's education: college"
putexcel A28 = "Husband: White No"
putexcel A29 = "Husband: White College"
putexcel A30 = "Husband: Black No"
putexcel A31 = "Husband: Black College"
putexcel A32 = "Wife's age at marriage"
putexcel A33 = "Husband's age at marriage"
putexcel A34 = "Husband Wife Cohabit"
putexcel A35 = "Other Premarital Cohabit"
putexcel A36 = "First Birth Premarital"
putexcel A37 = "Number of children"
putexcel A38 = "Interval"
putexcel A39 = "Interval: 1"
putexcel A40 = "Interval: 2"


// Medians
putexcel A42 = "Median Couple Earnings"
putexcel A43 = "Wife's annual earnings (median)"
putexcel A44 = "Husband's annual earnings (median)"
putexcel A45 = "Wife's weekly housework hours (median)"
putexcel A46 = "Husband's weekly housework hours (median)"

local meanvars "female_earn_pct ft_head ft_wife earn_type1 earn_type2 earn_type3 wife_housework_pct hw_type1 hw_type2 hw_type3 wife_white wife_black head_white head_black educ_wife1 educ_wife2 educ_wife3 educ_wife4 educ_head1 educ_head2 educ_head3 educ_head4 race_x_educ_head1 race_x_educ_head2 race_x_educ_head3 race_x_educ_head4 age_mar_wife age_mar_head cohab_with_wife cohab_with_other pre_marital_birth NUM_CHILDREN_ interval interval1 interval2"

local medianvars "TAXABLE_HEAD_WIFE_ earnings_wife earnings_head housework_wife housework_head"

// Overall: mean
forvalues w=1/35{
	local row=`w'+5
	local var: word `w' of `meanvars'
	mean `var'
	matrix t`var'= e(b)
	putexcel B`row' = matrix(t`var'), nformat(#.#%)
}

// Overall: median
forvalues w=1/5{
	local row=`w'+41
	local var: word `w' of `medianvars'
	sum `var', detail
	putexcel B`row' = `r(p50)', nformat(###,###)
}


// those who transitioned; value when dissolve_lag==1
forvalues w=1/35{
	local row=`w'+5
	local var: word `w' of `meanvars'
	mean `var' if dissolve_lag==1
	matrix t`var'= e(b)
	putexcel C`row' = matrix(t`var'), nformat(#.#%)
}

forvalues w=1/5{
	local row=`w'+41
	local var: word `w' of `medianvars'
	sum `var' if dissolve_lag==1, detail
	putexcel C`row' = `r(p50)', nformat(###,###)
}

**By education:


// No college degree: overall
forvalues w=1/35{
	local row=`w'+5
	local var: word `w' of `meanvars'
	mean `var' if couple_educ_gp==0
	matrix t`var'= e(b)
	putexcel D`row' = matrix(t`var'), nformat(#.#%)
}

forvalues w=1/5{
	local row=`w'+41
	local var: word `w' of `medianvars'
	sum `var' if couple_educ_gp==0, detail
	putexcel D`row' = `r(p50)', nformat(###,###)
}


// No college degree: those who transitioned
forvalues w=1/35{
	local row=`w'+5
	local var: word `w' of `meanvars'
	mean `var' if dissolve_lag==1 & couple_educ_gp==0
	matrix t`var'= e(b)
	putexcel E`row' = matrix(t`var'), nformat(#.#%)
}

forvalues w=1/5{
	local row=`w'+41
	local var: word `w' of `medianvars'
	sum `var' if dissolve_lag==1 & couple_educ_gp==0, detail
	putexcel E`row' = `r(p50)', nformat(###,###)
}

// College degree: overall
forvalues w=1/35{
	local row=`w'+5
	local var: word `w' of `meanvars'
	mean `var' if couple_educ_gp==1
	matrix t`var'= e(b)
	putexcel F`row' = matrix(t`var'), nformat(#.#%)
}

forvalues w=1/5{
	local row=`w'+41
	local var: word `w' of `medianvars'
	sum `var' if couple_educ_gp==1, detail
	putexcel F`row' = `r(p50)', nformat(###,###)
}


// College degree: those who transitioned
forvalues w=1/35{
	local row=`w'+5
	local var: word `w' of `meanvars'
	mean `var' if dissolve_lag==1 & couple_educ_gp==1
	matrix t`var'= e(b)
	putexcel G`row' = matrix(t`var'), nformat(#.#%)
}

forvalues w=1/5{
	local row=`w'+41
	local var: word `w' of `medianvars'
	sum `var' if dissolve_lag==1 & couple_educ_gp==1, detail
	putexcel G`row' = `r(p50)', nformat(###,###)
}


********************************************************************************
**# Weighted
********************************************************************************
svyset [pweight=weight]

putexcel set "$results/Table1_Descriptives_weighted", replace
putexcel B1:C1 = "Total", merge border(bottom)
putexcel D1:E1 = "No College", merge border(bottom)
putexcel F1:G1 = "College-Educated", merge border(bottom)
putexcel B2 = ("All") C2 = ("Dissolved") D2 = ("All") E2 = ("Dissolved") F2 = ("All") G2 = ("Dissolved") , border(bottom)
putexcel A3 = "Unique Couples"
putexcel A4 = "% Dissolved"

// Means
putexcel A6 = "Wife's share of earnings"
putexcel A7 = "Husband employed full-time"
putexcel A8 = "Wife employed full-time"
putexcel A9 = "Dual Earning HH"
putexcel A10 = "Male Breadwinner"
putexcel A11 = "Female Breadwinner"
putexcel A12 = "Wife's share of unpaid hours"
putexcel A13 = "Equal"
putexcel A14 = "Female Primary"
putexcel A15 = "Male Primary"
putexcel A16 = "Wife's race: White"
putexcel A17 = "Wife's race: Black"
putexcel A18 = "Husband's race: White"
putexcel A19 = "Husband's race: Black"
putexcel A20 = "Wife's education: LTHS"
putexcel A21 = "Wife's education: hs"
putexcel A22 = "Wife's education: some college"
putexcel A23 = "Wife's education: college"
putexcel A24 = "husband's education: LTHS"
putexcel A25 = "husband's education: hs"
putexcel A26 = "husband's education: some college"
putexcel A27 = "husband's education: college"
putexcel A28 = "Husband: White No"
putexcel A29 = "Husband: White College"
putexcel A30 = "Husband: Black No"
putexcel A31 = "Husband: Black College"
putexcel A32 = "Wife's age at marriage"
putexcel A33 = "Husband's age at marriage"
putexcel A34 = "Husband Wife Cohabit"
putexcel A35 = "Other Premarital Cohabit"
putexcel A36 = "First Birth Premarital"
putexcel A37 = "Number of children"
putexcel A38 = "Interval"
putexcel A39 = "Interval: 1"
putexcel A40 = "Interval: 2"

/* Medians
putexcel A42 = "Median Couple Earnings"
putexcel A43 = "Wife's annual earnings (median)"
putexcel A44 = "Husband's annual earnings (median)"
putexcel A45 = "Wife's weekly housework hours (median)"
putexcel A46 = "Husband's weekly housework hours (median)"
*/

local meanvars "female_earn_pct ft_head ft_wife earn_type1 earn_type2 earn_type3 wife_housework_pct hw_type1 hw_type2 hw_type3 wife_white wife_black head_white head_black educ_wife1 educ_wife2 educ_wife3 educ_wife4 educ_head1 educ_head2 educ_head3 educ_head4 race_x_educ_head1 race_x_educ_head2 race_x_educ_head3 race_x_educ_head4 age_mar_wife age_mar_head cohab_with_wife cohab_with_other pre_marital_birth NUM_CHILDREN_ interval interval1 interval2"

local medianvars "TAXABLE_HEAD_WIFE_ earnings_wife earnings_head housework_wife housework_head"


// Overall: mean
forvalues w=1/35{
	local row=`w'+5
	local var: word `w' of `meanvars'
	svy: mean `var'
	matrix t`var'= e(b)
	putexcel B`row' = matrix(t`var'), nformat(#.#%)
}


// those who transitioned; value when dissolve_lag==1
forvalues w=1/35{
	local row=`w'+5
	local var: word `w' of `meanvars'
	svy: mean `var' if dissolve_lag==1
	matrix t`var'= e(b)
	putexcel C`row' = matrix(t`var'), nformat(#.#%)
}


**By education:


// No college degree: overall
forvalues w=1/35{
	local row=`w'+5
	local var: word `w' of `meanvars'
	svy: mean `var' if couple_educ_gp==0
	matrix t`var'= e(b)
	putexcel D`row' = matrix(t`var'), nformat(#.#%)
}


// No college degree: those who transitioned
forvalues w=1/35{
	local row=`w'+5
	local var: word `w' of `meanvars'
	svy: mean `var' if dissolve_lag==1 & couple_educ_gp==0
	matrix t`var'= e(b)
	putexcel E`row' = matrix(t`var'), nformat(#.#%)
}


// College degree: overall
forvalues w=1/35{
	local row=`w'+5
	local var: word `w' of `meanvars'
	svy: mean `var' if couple_educ_gp==1
	matrix t`var'= e(b)
	putexcel F`row' = matrix(t`var'), nformat(#.#%)
}


// College degree: those who transitioned
forvalues w=1/35{
	local row=`w'+5
	local var: word `w' of `meanvars'
	svy: mean `var' if dissolve_lag==1 & couple_educ_gp==1
	matrix t`var'= e(b)
	putexcel G`row' = matrix(t`var'), nformat(#.#%)
}


