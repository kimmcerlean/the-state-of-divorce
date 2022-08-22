********************************************************************************
* Getting PSID sample for union dissolution
* create_variables.do
* Kim McErlean
********************************************************************************

use "$data_tmp\PSID_long_individs_relationships.dta", clear

tab relationship_type dissolve
tab relationship_start dissolve if relationship_type==2 // checking against table A1 in Schwartz and Han 2014
unique relationship_type id if relationship_type==2, by(relationship_start)
unique id if relationship_type==2 & dissolve==1, by(relationship_start)

// education
gen college_wife= 0 
replace college_wife=1 if COLLEGE_WIFE_ == 1
gen college_head = 0
replace college_head = 1 if COLLEGE_HEAD_ == 1

recode EDUC_WIFE_ (0/11=1) (12=2) (13/15=3) (16/17=4) (99=.), gen(educ_wife)
recode EDUC_HEAD_ (0/11=1) (12=2) (13/15=3) (16/17=4) (99=.), gen(educ_head)

label define educ 1 "LTHS" 2 "HS" 3 "Some College" 4 "College"
label values educ_wife educ_head educ

gen college_complete_wife=0
replace college_complete_wife=1 if educ_wife==4
gen college_complete_head=0
replace college_complete_head=1 if educ_head==4

gen couple_educ_gp=0
replace couple_educ_gp=1 if (college_complete_wife==1 | college_complete_head==1)

label define couple_educ 0 "Neither College" 1 "At Least One College"
label values couple_educ_gp couple_educ

gen educ_type=.
replace educ_type=1 if educ_head > educ_wife
replace educ_type=2 if educ_head < educ_wife
replace educ_type=3 if educ_head == educ_wife

label define educ_type 1 "Hyper" 2 "Hypo" 3 "Homo"
label values educ_type educ_type

logit dissolve i.educ_type if relationship_type==2, or // Schwartz and Han validation; homo should be sig less, no diff hyper and hypo
// okay matches, homo coefficient also v similar, though hyper isn't - potentially because no controls yet

// income / structure
browse id survey_yr FAMILY_INTERVIEW_NUM_ TAXABLE_HEAD_WIFE_ TOTAL_FAMILY_INCOME_ EMPLOY_STATUS1_HEAD_ LABOR_INCOME_HEAD_ WAGES_HEAD_  LABOR_INCOME_WIFE_ WAGES_WIFE_ WAGES_WIFE_PRE_ WAGES1_WIFE_ SALARY_WIFE_ AMOUNTEARN_*_WIFE_

	// to use: WAGES_HEAD_ WAGES_WIFE_

inspect WAGES_WIFE_ if EMPLOY_STATUS1_WIFE_==1 // okay, this is okay, no missing
inspect WAGES_HEAD_ if EMPLOY_STATUS1_HEAD_==1 // okay, this is okay, no missing

// validate this is aligning with total
browse id survey_yr FAMILY_INTERVIEW_NUM_ TAXABLE_HEAD_WIFE_ TOTAL_FAMILY_INCOME_ EMPLOY_STATUS1_HEAD_ WAGES_HEAD_ EMPLOY_STATUS1_WIFE_ WAGES_WIFE_

	// helpful note: 2001, id 23 and 70 are both in family 285, head + wife = total taxable, family income is greater, so someone else must be working.
	
gen female_earn_pct = WAGES_WIFE_/(WAGES_WIFE_ + WAGES_HEAD_)

gen hh_earn_type_bkd=.
replace hh_earn_type_bkd=1 if female_earn_pct >=.4000 & female_earn_pct <=.6000
replace hh_earn_type_bkd=2 if female_earn_pct < .4000 & female_earn_pct > 0
replace hh_earn_type_bkd=3 if female_earn_pct ==0
replace hh_earn_type_bkd=4 if female_earn_pct > .6000 & female_earn_pct <=1
replace hh_earn_type_bkd=5 if WAGES_HEAD_==0 & WAGES_WIFE_==0

label define earn_type_bkd 1 "Dual Earner" 2 "Male Primary" 3 "Male Sole" 4 "Female BW" 5 "No Earners"
label values hh_earn_type_bkd earn_type_bkd

sort id survey_yr
gen hh_earn_type_lag=.
replace hh_earn_type_lag=hh_earn_type_bkd[_n-1] if id==id[_n-1]
label values hh_earn_type_lag earn_type_bkd

browse id survey_yr WAGES_HEAD_ WAGES_WIFE_ hh_earn_type_bkd hh_earn_type_lag
//browse WAGES_WIFE_ WAGES_HEAD_ TAXABLE_HEAD_WIFE_ female_earn_pct if female_earn_pct>1

// alternate specification to try
gen hh_earnings_3070=.
replace hh_earnings_3070=1 if female_earn_pct >=.3000 & female_earn_pct <=.7000
replace hh_earnings_3070=2 if female_earn_pct <.3000
replace hh_earnings_3070=3 if female_earn_pct >.7000
replace hh_earnings_3070=4 if (WAGES_HEAD_==0 & WAGES_WIFE_==0) | female_earn_pct==.

label define hh_earnings_3070 1 "Dual Earner" 2 "Male BW" 3 "Female BW" 4 "No Earners"
label values hh_earnings_3070 hh_earnings_3070

sort id survey_yr
gen hh_earnings_3070_lag=.
replace hh_earnings_3070_lag=hh_earnings_3070[_n-1] if id==id[_n-1]
label values hh_earnings_3070_lag hh_earnings_3070

browse id survey_yr WAGES_HEAD_ WAGES_WIFE_ hh_earnings_3070 hh_earnings_3070_lag

gen female_earn_pct_lag=.
replace female_earn_pct_lag=female_earn_pct[_n-1] if id==id[_n-1]

// hours instead of earnings	
gen female_hours_pct = WEEKLY_HRS_WIFE_/(WEEKLY_HRS_WIFE_ + WEEKLY_HRS_HEAD_)

gen hh_hours_3070=.
replace hh_hours_3070=1 if female_hours_pct >=.3000 & female_hours_pct <=.7000
replace hh_hours_3070=2 if female_hours_pct <.3000
replace hh_hours_3070=3 if female_hours_pct >.7000
replace hh_hours_3070=4 if (WEEKLY_HRS_HEAD_==0 & WEEKLY_HRS_WIFE_==0) | female_hours_pct==.

label define hh_hours_3070 1 "Dual Earner" 2 "Male BW" 3 "Female BW" 4 "No Earners"
label values hh_hours_3070 hh_hours_3070

sort id survey_yr
gen hh_hours_3070_lag=.
replace hh_hours_3070_lag=hh_hours_3070[_n-1] if id==id[_n-1]
label values hh_hours_3070_lag hh_hours_3070

gen female_hours_pct_lag=.
replace female_hours_pct_lag=female_hours_pct[_n-1] if id==id[_n-1]

browse id survey_yr WEEKLY_HRS_HEAD_ WEEKLY_HRS_WIFE_ female_hours_pct female_hours_pct_lag hh_hours_3070 hh_hours_3070_lag
	
	
// restrict to working age (18-55) - at time of marriage or all? check what others do - Killewald said ages 18-55
browse id survey_yr AGE_ AGE_REF_ AGE_SPOUSE_ RELATION_
keep if (AGE_REF_>=18 & AGE_REF_<=55) &  (AGE_SPOUSE_>=18 & AGE_SPOUSE_<=55)

// employment
browse id survey_yr EMPLOY_STATUS1_HEAD_ EMPLOY_STATUS2_HEAD_ EMPLOY_STATUS3_HEAD_ EMPLOY_STATUS1_WIFE_ EMPLOY_STATUS2_WIFE_ EMPLOY_STATUS3_WIFE_
gen employ1_head=0
replace employ1_head=1 if EMPLOY_STATUS1_HEAD_==1
gen employ2_head=0
replace employ2_head=1 if EMPLOY_STATUS2_HEAD_==1
gen employ3_head=0
replace employ3_head=1 if EMPLOY_STATUS3_HEAD_==1
egen employed_head=rowtotal(employ1_head employ2_head employ3_head)

gen employ1_wife=0
replace employ1_wife=1 if EMPLOY_STATUS1_WIFE_==1
gen employ2_wife=0
replace employ2_wife=1 if EMPLOY_STATUS2_WIFE_==1
gen employ3_wife=0
replace employ3_wife=1 if EMPLOY_STATUS3_WIFE_==1
egen employed_wife=rowtotal(employ1_wife employ2_wife employ3_wife)

// browse id survey_yr employed_head employed_wife HOURS_WK_HEAD_ TOTAL_HOURS_HEAD_ TOTAL_HOURS_WIFE_ TOTAL_WEEKS_HEAD_ TOTAL_WEEK_WIFE_ // okay so have for husband, but don't have weekly hours for wife. I do have total weeks but seems to be 0 always (or could divide...)

// problem is this employment is NOW not last year. I want last year? use if wages = employ=yes, then no? (or hours)
gen employed_ly_head=0
replace employed_ly_head=1 if WAGES_HEAD_ > 0 & WAGES_HEAD_!=.

gen employed_ly_wife=0
replace employed_ly_wife=1 if WAGES_WIFE_ > 0 & WAGES_WIFE_!=.

browse id survey_yr employed_ly_head employed_ly_wife WEEKLY_HRS_HEAD_ WEEKLY_HRS_WIFE_ WAGES_HEAD_ WAGES_WIFE_

gen ft_pt_head=0
replace ft_pt_head=1 if employed_ly_head==1 & WEEKLY_HRS_HEAD_ >0 & WEEKLY_HRS_HEAD_<=35
replace ft_pt_head=2 if employed_ly_head==1 & WEEKLY_HRS_HEAD_>35 & WEEKLY_HRS_HEAD_!=.

gen ft_pt_wife=0
replace ft_pt_wife=1 if employed_ly_wife==1 & WEEKLY_HRS_WIFE_ >0 & WEEKLY_HRS_WIFE_<=35
replace ft_pt_wife=2 if employed_ly_wife==1 & WEEKLY_HRS_WIFE_>35 & WEEKLY_HRS_WIFE_!=.

label define ft_pt 0 "Not Employed" 1 "PT" 2 "FT"
label values ft_pt_head ft_pt_wife ft_pt

gen ft_head=0
replace ft_head=1 if ft_pt_head==2

gen ft_wife=0
replace ft_wife=1 if ft_pt_wife==2

// first need to figure out how to keep only one respondent per HH. really doesn't matter gender of who I keep, because all variables are denoted by head / wife, NOT respondent.
bysort survey_yr FAMILY_INTERVIEW_NUM_ : egen per_id = rank(id)
browse survey_yr FAMILY_INTERVIEW_NUM_  id per_id

browse survey_yr FAMILY_INTERVIEW_NUM_ per_id id if inlist(id,1922,1947)

keep if per_id==1

// adding other controls right now, using same as SIPP analysis
gen either_enrolled=0
replace either_enrolled = 1 if ENROLLED_WIFE_==1 | ENROLLED_HEAD_==1

//race
drop if RACE_1_WIFE_==9 | RACE_1_HEAD_==9

browse id survey_yr RACE_1_WIFE_ RACE_2_WIFE_ RACE_3_WIFE_ RACE_1_HEAD_ RACE_2_HEAD_ RACE_3_HEAD_ RACE_4_HEAD_
gen race_wife=RACE_1_WIFE_
recode race_wife(5/7=5)
replace race_wife=6 if RACE_2_WIFE_!=0

gen race_head=RACE_1_HEAD_
recode race_head(5/7=5)
replace race_head=6 if RACE_2_HEAD_!=0

label define race 1 "White" 2 "Black" 3 "Indian" 4 "Asian" 5 "Other" 6 "Multi-racial"
label values race_wife race_head race

// need to figure out ethnicity

gen same_race=0
replace same_race=1 if race_head==race_wife

gen children=0
replace children=1 if NUM_CHILDREN_>=1

gen metro=(METRO_==1) // a lot of missing, don't use for now, control for STATE_ for now

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

// also age at relationship start
browse id survey_yr relationship_start BIRTH_YR_ AGE_REF_ AGE_SPOUSE_
gen yr_born_head = survey_yr - AGE_REF_
gen yr_born_wife = survey_yr-AGE_SPOUSE_

gen age_mar_head = relationship_start -  yr_born_head
gen age_mar_wife = relationship_start -  yr_born_wife

browse id survey_yr yr_born_head yr_born_wife relationship_start age_mar_head age_mar_wife AGE_REF_ AGE_SPOUSE_

save "$data_keep\PSID_relationships_post2000.dta", replace