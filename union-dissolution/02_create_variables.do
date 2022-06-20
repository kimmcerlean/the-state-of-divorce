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
	
gen female_earn_pct = WAGES_WIFE_/(TAXABLE_HEAD_WIFE_)

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

// also make sure you only keep ONE RECORD per HH - challenging since family id changes every time. see if there is a unified ID i can use. doing in next step

save "$data_keep\PSID_relationships_post2000.dta", replace