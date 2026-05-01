********************************************************************************
* Project: Work-family policy and divorce
* Recoding couple-level sample
* recodes.do
* Code owner: Kimberly McErlean
********************************************************************************

use "$temp/PSID_all_unions.dta", clear // created in step 3

********************************************************************************
**# Variable Recodes
********************************************************************************
// Education

/*
educ1 until 1990, but educ started 1975, okay but then a gap until 1991? wife not asked 1969-1971 - might be able to fill in if she is in sample either 1968 or 1972? (match to the id). also look at yrs education (missing 69 and 74?)

codes are also different between the two, use educ1 until 1990, then educ 1991 post
early educ:
0. cannot read
1. 0-5th grade
2. 6-8th grade
3. 9-11 grade
4/5. 12 grade
6. college no degree
7/8. college / advanced degree
9. dk

later educ: years of education

I end up using code adapted from SHELF
*/

* clean up intermediary variables
label values YRS_EDUCATION_INDV .

gen hs_head=.
replace hs_head=1 if inlist(HS_GRAD_HEAD_,1,2)
replace hs_head=0 if HS_GRAD_HEAD_==3

gen hs_wife=.
replace hs_wife=1 if inlist(HS_GRAD_WIFE_,1,2)
replace hs_wife=0 if HS_GRAD_WIFE_==3

gen attended_college_head=.
replace attended_college_head= 0 if ATTENDED_COLLEGE_HEAD_==5
replace attended_college_head= 1 if ATTENDED_COLLEGE_HEAD_==1

gen attended_college_wife=.
replace attended_college_wife= 0 if ATTENDED_COLLEGE_WIFE_==5
replace attended_college_wife= 1 if ATTENDED_COLLEGE_WIFE_==1

gen completed_college_head=.
replace completed_college_head= 0 if COLLEGE_HEAD_==5
replace completed_college_head= 1 if COLLEGE_HEAD_==1
replace completed_college_head= 0 if attended_college_head==0

gen completed_college_wife=.
replace completed_college_wife= 0 if COLLEGE_WIFE_==5
replace completed_college_wife= 1 if COLLEGE_WIFE_==1
replace completed_college_wife= 0 if attended_college_wife==0

gen completed_college_indv=.
replace completed_college_indv= 0 if COLLEGE_INDV_==5
replace completed_college_indv= 1 if COLLEGE_INDV_==1

gen college_degree_head=.
replace college_degree_head=0 if HIGHEST_DEGREE_HEAD_==0
replace college_degree_head=1 if HIGHEST_DEGREE_HEAD_==1 // associates
replace college_degree_head=2 if inrange(HIGHEST_DEGREE_HEAD_,2,6) // bachelor's plus

gen college_degree_wife=.
replace college_degree_wife=0 if HIGHEST_DEGREE_WIFE_==0
replace college_degree_wife=1 if HIGHEST_DEGREE_WIFE_==1 // associates
replace college_degree_wife=2 if inrange(HIGHEST_DEGREE_WIFE_,2,6) // bachelor's plus

label define degree 0 "No Coll" 1 "Assoc" 2 "BA+"
label values college_degree_head college_degree_wife

tab attended_college_head completed_college_head, m
tab completed_college_head college_degree_head, m

replace NEW_HEAD_YEAR = 1900+NEW_HEAD_YEAR if NEW_HEAD_YEAR>0 & NEW_HEAD_YEAR<100
replace NEW_WIFE_YEAR = 1900+NEW_WIFE_YEAR if NEW_WIFE_YEAR>0 & NEW_WIFE_YEAR<100

recode EDUC1_WIFE_ (1/3=1)(4/5=2)(6=3)(7/8=4)(9=.)(0=.), gen(educ_wife_early)
recode EDUC1_HEAD_ (0/3=1)(4/5=2)(6=3)(7/8=4)(9=.), gen(educ_head_early)
recode EDUC_WIFE_ (1/11=1) (12=2) (13/15=3) (16/17=4) (99=.)(0=.), gen(educ_wife_1975)
recode EDUC_HEAD_ (0/11=1) (12=2) (13/15=3) (16/17=4) (99=.), gen(educ_head_1975)
recode YRS_EDUCATION_INDV (1/11=1) (12=2) (13/15=3) (16/17=4) (98/99=.)(0=.), gen(educ_completed) // okay this is hard to use because head / wife ONLY recorded against those specific ones so they don't always have values here

label define educ 1 "LTHS" 2 "HS" 3 "Some College" 4 "College"
label values educ_wife_early educ_head_early educ_wife_1975 educ_head_1975 educ_completed educ

browse unique_id survey_yr in_sample relation YRS_EDUCATION_INDV educ_completed educ_head_early educ_head_1975 hs_head HS_GRAD_HEAD attended_college_head completed_college_head college_degree_head BACHELOR_YR_HEAD_ YR_EDUC_UPD_HEAD_ NEW_HEAD_ NEW_HEAD_YEAR if relation==1 // using head right now to wrap my head around

* create final education variables
gen educ_head_est=. // this is more accurate
replace educ_head_est=1 if hs_head==0
replace educ_head_est=2 if hs_head==1 & attended_college_head==0
replace educ_head_est=3 if hs_head==1 & attended_college_head==1 & completed_college_head==0
replace educ_head_est=3 if completed_college_head==1 & college_degree_head==1
replace educ_head_est=4 if completed_college_head==1 & college_degree_head==2

gen educ_head=.
replace educ_head=educ_head_early if inrange(survey_yr,1968,1990)
replace educ_head=educ_head_1975 if inrange(survey_yr,1991,2021)

tab educ_head educ_head_est, m
tab educ_completed educ_head_est if relation==1, m
tab educ_head educ_completed if educ_head_est==., m
tab YRS_EDUCATION_INDV educ_head_est if relation==1, m

replace educ_head_est = educ_completed if educ_head_est==. & educ_completed!=. & relation==1
replace educ_head_est = educ_head if educ_head_est==. & educ_head!=.

browse unique_id survey_yr educ_head educ_completed educ_head_est YRS_EDUCATION_INDV  hs_head attended_college_head completed_college_head college_degree_head if relation==1 

gen educ_wife_est=.  // this is more accurate
replace educ_wife_est=1 if hs_wife==0
replace educ_wife_est=2 if hs_wife==1 & attended_college_wife==0
replace educ_wife_est=3 if hs_wife==1 & attended_college_wife==1 & completed_college_wife==0
replace educ_wife_est=3 if completed_college_wife==1 & college_degree_wife==1
replace educ_wife_est=4 if completed_college_wife==1 & college_degree_wife==2

gen educ_wife=.
replace educ_wife=educ_wife_early if inrange(survey_yr,1968,1990)
replace educ_wife=educ_wife_1975 if inrange(survey_yr,1991,2021)
tab survey_yr educ_wife, m 

replace educ_wife_est = educ_completed if educ_wife_est==. & educ_completed!=. & relation==2
replace educ_wife_est = educ_wife if educ_wife_est==. & educ_wife!=.

tab educ_wife educ_wife_est, m
tab educ_completed educ_wife_est if relation==2, m
tab educ_wife educ_completed if educ_wife_est==. & relation==2, m
tab YRS_EDUCATION_INDV educ_wife_est if relation==2, m

label values educ_head educ_wife educ_head_est educ_wife_est educ

gen college_wife=.
replace college_wife=0 if inrange(educ_wife_est,1,3)
replace college_wife=1 if educ_wife_est==4

gen college_head=.
replace college_head=0 if inrange(educ_head_est,1,3)
replace college_head=1 if educ_head_est==4
tab college_degree_head college_head, m

gen college_indv=.
replace college_indv=0 if inrange(educ_completed,1,3)
replace college_indv=1 if educ_completed==4

gen couple_educ_gp=.
replace couple_educ_gp=0 if college_wife==0 & college_head==0
replace couple_educ_gp=1 if (college_wife==1 | college_head==1)

label define couple_educ 0 "Neither College" 1 "At Least One College"
label values couple_educ_gp couple_educ

gen educ_type=.
replace educ_type=1 if couple_educ_gp==0
replace educ_type=2 if college_wife==0 & college_head==1
replace educ_type=3 if college_wife==1 & college_head==0
replace educ_type=4 if college_wife==1 & college_head==1

label define educ_type 1 "Neither College" 2 "His College" 3 "Her College" 4 "Both College"
label values educ_type educ_type

// income and division of paid labor
browse unique_id survey_yr FAMILY_INTERVIEW_NUM_ TAXABLE_T1_HEAD_WIFE TOTAL_INCOME_T1_FAMILY LABOR_INCOME_T1_HEAD WAGES_ALT_T1_HEAD WAGES_T1_HEAD LABOR_INCOME_T2_HEAD LABOR_INCOME_T1_WIFE_ WAGES_T1_WIFE_  LABOR_INCOME_T2_WIFE_

	// to use: WAGES_HEAD_ WAGES_WIFE_ -- wife not asked until 1993? okay labor income??
	// wages and labor income asked for head whole time. labor income wife 1968-1993, wages for wife, 1993 onwards

gen earnings_t1_wife=.
replace earnings_t1_wife = LABOR_INCOME_T1_WIFE_ if inrange(survey_yr,1968,1993)
replace earnings_t1_wife = WAGES_T1_WIFE_ if inrange(survey_yr,1994,2021)
replace earnings_t1_wife=. if earnings_t1_wife== 9999999

gen earnings_t1_head=.
replace earnings_t1_head = LABOR_INCOME_T1_HEAD if inrange(survey_yr,1968,1993)
replace earnings_t1_head = WAGES_T1_HEAD if inrange(survey_yr,1994,2021)
replace earnings_t1_head=. if earnings_t1_head== 9999999

egen couple_earnings_t1 = rowtotal(earnings_t1_wife earnings_t1_head)
browse unique_id survey_yr TAXABLE_T1_HEAD_WIFE couple_earnings_t1 earnings_t1_wife earnings_t1_head
	
gen female_earn_pct_t1 = earnings_t1_wife/(couple_earnings_t1)

gen hh_earn_type_t1=.
replace hh_earn_type_t1=1 if female_earn_pct_t1 >=.4000 & female_earn_pct_t1 <=.6000
replace hh_earn_type_t1=2 if female_earn_pct_t1 < .4000 & female_earn_pct_t1 >=0
replace hh_earn_type_t1=3 if female_earn_pct_t1 > .6000 & female_earn_pct_t1 <=1
replace hh_earn_type_t1=4 if earnings_t1_head==0 & earnings_t1_wife==0

label define hh_earn_type 1 "Dual Earner" 2 "Male BW" 3 "Female BW" 4 "No Earners"
label values hh_earn_type_t1 hh_earn_type

sort unique_id survey_yr

gen hh_earn_type_t=.
replace hh_earn_type_t=hh_earn_type_t1[_n+1] if unique_id==unique_id[_n+1] & wave==wave[_n+1]-1
label values hh_earn_type_t hh_earn_type

gen female_earn_pct_t=.
replace female_earn_pct_t=female_earn_pct_t1[_n+1] if unique_id==unique_id[_n+1] & wave==wave[_n+1]-1

browse unique_id survey_yr wave earnings_t1_head earnings_t1_wife hh_earn_type_t1 hh_earn_type_t female_earn_pct_t1 female_earn_pct_t

// hours instead of earnings	
browse unique_id survey_yr WEEKLY_HRS1_T1_WIFE_ WEEKLY_HRS_T1_WIFE_ WEEKLY_HRS1_T1_HEAD_ WEEKLY_HRS_T1_HEAD_

gen weekly_hrs_t1_wife = .
replace weekly_hrs_t1_wife = WEEKLY_HRS1_T1_WIFE_ if survey_yr > 1969 & survey_yr <1994
replace weekly_hrs_t1_wife = WEEKLY_HRS_T1_WIFE_ if survey_yr >=1994
replace weekly_hrs_t1_wife = 0 if inrange(survey_yr,1968,1969) & inlist(WEEKLY_HRS1_T1_WIFE_,9,0)
replace weekly_hrs_t1_wife = 10 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_WIFE_ ==1
replace weekly_hrs_t1_wife = 27 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_WIFE_ ==2
replace weekly_hrs_t1_wife = 35 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_WIFE_ ==3
replace weekly_hrs_t1_wife = 40 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_WIFE_ ==4
replace weekly_hrs_t1_wife = 45 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_WIFE_ ==5
replace weekly_hrs_t1_wife = 48 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_WIFE_ ==6
replace weekly_hrs_t1_wife = 55 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_WIFE_ ==7
replace weekly_hrs_t1_wife = 60 if inrange(survey_yr,1968,1969)  & WEEKLY_HRS1_T1_WIFE_ ==8
replace weekly_hrs_t1_wife=. if weekly_hrs_t1_wife==999

gen weekly_hrs_t1_head = .
replace weekly_hrs_t1_head = WEEKLY_HRS1_T1_HEAD_ if survey_yr > 1969 & survey_yr <1994
replace weekly_hrs_t1_head = WEEKLY_HRS_T1_HEAD_ if survey_yr >=1994
replace weekly_hrs_t1_head = 0 if inrange(survey_yr,1968,1969) & inlist(WEEKLY_HRS1_T1_HEAD_,9,0)
replace weekly_hrs_t1_head = 10 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_HEAD_ ==1
replace weekly_hrs_t1_head = 27 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_HEAD_ ==2
replace weekly_hrs_t1_head = 35 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_HEAD_ ==3
replace weekly_hrs_t1_head = 40 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_HEAD_ ==4
replace weekly_hrs_t1_head = 45 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_HEAD_ ==5
replace weekly_hrs_t1_head = 48 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_HEAD_ ==6
replace weekly_hrs_t1_head = 55 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_HEAD_ ==7
replace weekly_hrs_t1_head = 60 if inrange(survey_yr,1968,1969)  & WEEKLY_HRS1_T1_HEAD_ ==8
replace weekly_hrs_t1_head=. if weekly_hrs_t1_head==999

egen couple_hours_t1 = rowtotal(weekly_hrs_t1_wife weekly_hrs_t1_head)
gen female_hours_pct_t1 = weekly_hrs_t1_wife/couple_hours_t1

gen hh_hours_type_t1=.
replace hh_hours_type_t1=1 if female_hours_pct_t1 >=.4000 & female_hours_pct_t1 <=.6000
replace hh_hours_type_t1=2 if female_hours_pct_t1 <.4000
replace hh_hours_type_t1=3 if female_hours_pct_t1 >.6000 & female_hours_pct_t1!=.
replace hh_hours_type_t1=4 if weekly_hrs_t1_wife==0 & weekly_hrs_t1_head==0

label define hh_hours_type 1 "Dual Earner" 2 "Male BW" 3 "Female BW" 4 "No Earners"
label values hh_hours_type_t1 hh_hours_type

gen hh_hours_type_t=.
replace hh_hours_type_t=hh_hours_type_t1[_n+1] if unique_id==unique_id[_n+1] & wave==wave[_n+1]-1
label values hh_hours_type_t hh_hours_type

gen female_hours_pct_t=.
replace female_hours_pct_t=female_hours_pct_t1[_n+1] if unique_id==unique_id[_n+1] & wave==wave[_n+1]-1

// housework hours - not totally sure if accurate prior to 1976
browse unique_id survey_yr RELATION_ HOUSEWORK_HEAD_ HOUSEWORK_WIFE_ HOUSEWORK_INDV_ TOTAL_HOUSEWORK_T1_HW MOST_HOUSEWORK_T1 // total and most HW stopped after 1974

gen housework_head = HOUSEWORK_HEAD_
replace housework_head = (HOUSEWORK_HEAD_/52) if inrange(survey_yr,1968,1974)
replace housework_head = HOUSEWORK_INDV_ if relationship==1 & inrange(survey_yr,1968,1974) & HOUSEWORK_INDV_!=.
replace housework_head=. if inlist(housework_head,998,999)

gen housework_wife = HOUSEWORK_WIFE_
replace housework_wife = (HOUSEWORK_WIFE_/52) if inrange(survey_yr,1968,1974)
replace housework_wife = HOUSEWORK_INDV_ if relationship==2 & inrange(survey_yr,1968,1974) & HOUSEWORK_INDV_!=.
replace housework_wife=. if inlist(housework_wife,998,999)

gen total_housework_weekly = TOTAL_HOUSEWORK_T1_HW / 52

browse unique_id survey_yr relationship housework_head housework_wife HOUSEWORK_HEAD_ HOUSEWORK_WIFE_ HOUSEWORK_INDV_

egen couple_housework_t1 = rowtotal (housework_wife housework_head)
browse id survey_yr housework_head housework_wife couple_housework total_housework_weekly TOTAL_HOUSEWORK_T1_HW MOST_HOUSEWORK_T1

gen wife_housework_pct_t = housework_wife / couple_housework_t

gen housework_bkt_t=.
replace housework_bkt_t=1 if wife_housework_pct_t >=.4000 & wife_housework_pct_t <=.6000
replace housework_bkt_t=2 if wife_housework_pct_t >.6000 & wife_housework_pct_t!=.
replace housework_bkt_t=3 if wife_housework_pct_t <.4000
replace housework_bkt_t=4 if housework_wife==0 & housework_head==0

label define housework_bkt 1 "Dual HW" 2 "Female Primary" 3 "Male Primary" 4 "NA"
label values housework_bkt_t housework_bkt

sort unique_id survey_yr
gen housework_bkt_t1=.
replace housework_bkt_t1=housework_bkt_t[_n-1] if unique_id==unique_id[_n-1] & wave==wave[_n-1]+1
label values housework_bkt_t1 housework_bkt

gen wife_housework_pct_t1=.
replace wife_housework_pct_t1=wife_housework_pct_t[_n-1] if unique_id==unique_id[_n-1] & wave==wave[_n-1]+1

// browse id survey_yr housework_bkt_t housework_bkt_t1 wife_housework_pct_t wife_housework_pct_t1

// combined indicator of paid and unpaid, using HOURS
gen hours_housework_t=.
replace hours_housework_t=1 if hh_hours_type_t==1 & housework_bkt_t==1 // dual both (egal)
replace hours_housework_t=2 if hh_hours_type_t==1 & housework_bkt_t==2 // dual earner, female HM (second shift)
replace hours_housework_t=3 if hh_hours_type_t==2 & housework_bkt_t==2 // male BW, female HM (traditional)
replace hours_housework_t=4 if hh_hours_type_t==3 & housework_bkt_t==3 // female BW, male HM (counter-traditional)
replace hours_housework_t=5 if hours_housework_t==. & hh_hours_type_t!=. & housework_bkt_t!=. // all others

label define combined_dol 1 "Egal" 2 "Second Shift" 3 "Traditional" 4 "Counter Traditional" 5 "All others"
label values hours_housework_t combined_dol 

gen hours_housework_t1=.
replace hours_housework_t1=1 if hh_hours_type_t1==1 & housework_bkt_t1==1 // dual both (egal)
replace hours_housework_t1=2 if hh_hours_type_t1==1 & housework_bkt_t1==2 // dual earner, female HM (second shift)
replace hours_housework_t1=3 if hh_hours_type_t1==2 & housework_bkt_t1==2 // male BW, female HM (traditional)
replace hours_housework_t1=4 if hh_hours_type_t1==3 & housework_bkt_t1==3 // female BW, male HM (counter-traditional)
replace hours_housework_t1=5 if hours_housework_t1==. & hh_hours_type_t1!=. & housework_bkt_t1!=. // all others

label values hours_housework_t1 combined_dol 

// now EARNINGS
gen earn_housework_t=.
replace earn_housework_t=1 if hh_earn_type_t==1 & housework_bkt_t==1 // dual both (egal)
replace earn_housework_t=2 if hh_earn_type_t==1 & housework_bkt_t==2 // dual earner, female HM (second shift)
replace earn_housework_t=3 if hh_earn_type_t==2 & housework_bkt_t==2 // male BW, female HM (traditional)
replace earn_housework_t=4 if hh_earn_type_t==3 & housework_bkt_t==3 // female BW, male HM (counter-traditional)
replace earn_housework_t=5 if earn_housework_t==. & hh_earn_type_t!=. & housework_bkt_t!=. // all others

label values earn_housework_t combined_dol 

gen earn_housework_t1=.
replace earn_housework_t1=1 if hh_earn_type_t1==1 & housework_bkt_t1==1 // dual both (egal)
replace earn_housework_t1=2 if hh_earn_type_t1==1 & housework_bkt_t1==2 // dual earner, female HM (second shift)
replace earn_housework_t1=3 if hh_earn_type_t1==2 & housework_bkt_t1==2 // male BW, female HM (traditional)
replace earn_housework_t1=4 if hh_earn_type_t1==3 & housework_bkt_t1==3 // female BW, male HM (counter-traditional)
replace earn_housework_t1=5 if earn_housework_t1==. & hh_earn_type_t1!=. & housework_bkt_t1!=. // all others

label values earn_housework_t1 combined_dol 

/* turning the bucket interactions into variables to interact over time
gen hours_housework=.
replace hours_housework=1 if hh_hours_3070==1 & housework_bkt==1 // dual both (egal)
replace hours_housework=2 if hh_hours_3070==1 & housework_bkt==2 // dual earner, female HM (neotraditional)
replace hours_housework=3 if hh_hours_3070==2 & housework_bkt==1 // male BW, dual HW (mm not sure)
replace hours_housework=4 if hh_hours_3070==2 & housework_bkt==2 // male BW, female HM (conventional)
replace hours_housework=5 if hh_hours_3070==3 & housework_bkt==1 // female BW, dual HW (gender-atypical)
replace hours_housework=6 if hh_hours_3070==3 & housework_bkt==2 // female BW, female HM (undoing gender)
replace hours_housework=7 if housework_bkt==3  // all where male does more housework (gender-atypical)
replace hours_housework=8 if hh_hours_3070==4  // no earners

label define hours_housework 1 "Egal" 2 "Neotraditional" 3 "Male BW, dual HW" 4 "Conventional" 5 "Gender-atypical" 6 "Undoing gender" 7 "Male HW dominant" 8 "No Earners"
label values hours_housework hours_housework 
*/

// employment
browse id survey_yr EMPLOY_STATUS_HEAD_ EMPLOY_STATUS1_HEAD_ EMPLOY_STATUS2_HEAD_ EMPLOY_STATUS3_HEAD_ EMPLOY_STATUS_WIFE_ EMPLOY_STATUS1_WIFE_ EMPLOY_STATUS2_WIFE_ EMPLOY_STATUS3_WIFE_
// not numbered until 1994; 1-3 arose in 1994. codes match
// wife not asked until 1976?

gen employ_head=.
replace employ_head=0 if inrange(EMPLOY_STATUS_HEAD_,2,9)
replace employ_head=1 if EMPLOY_STATUS_HEAD_==1
gen employ1_head=.
replace employ1_head=0 if inrange(EMPLOY_STATUS1_HEAD_,2,8)
replace employ1_head=1 if EMPLOY_STATUS1_HEAD_==1
gen employ2_head=.
replace employ2_head=0 if EMPLOY_STATUS2_HEAD_==0 | inrange(EMPLOY_STATUS2_HEAD_,2,8)
replace employ2_head=1 if EMPLOY_STATUS2_HEAD_==1
gen employ3_head=.
replace employ3_head=0 if EMPLOY_STATUS3_HEAD_==0 | inrange(EMPLOY_STATUS3_HEAD_,2,8)
replace employ3_head=1 if EMPLOY_STATUS3_HEAD_==1

browse employ_head employ1_head employ2_head employ3_head
egen employed_head=rowtotal(employ_head employ1_head employ2_head employ3_head), missing
replace employed_head=1 if employed_head==2

gen employ_wife=.
replace employ_wife=0 if inrange(EMPLOY_STATUS_WIFE_,2,9)
replace employ_wife=1 if EMPLOY_STATUS_WIFE_==1
gen employ1_wife=.
replace employ1_wife=0 if inrange(EMPLOY_STATUS1_WIFE_,2,8)
replace employ1_wife=1 if EMPLOY_STATUS1_WIFE_==1
gen employ2_wife=.
replace employ2_wife=0 if EMPLOY_STATUS2_WIFE_==0 | inrange(EMPLOY_STATUS2_WIFE_,2,8)
replace employ2_wife=1 if EMPLOY_STATUS2_WIFE_==1
gen employ3_wife=.
replace employ3_wife=0 if EMPLOY_STATUS3_WIFE_==0 | inrange(EMPLOY_STATUS3_WIFE_,2,8)
replace employ3_wife=1 if EMPLOY_STATUS3_WIFE_==1

egen employed_wife=rowtotal(employ_wife employ1_wife employ2_wife employ3_wife), missing
replace employed_wife=1 if employed_wife==2

browse id survey_yr employed_head employed_wife employ_head employ1_head employ_wife employ1_wife

/* problem is this employment is NOW not last year. I want last year? use if wages = employ=yes, then no? (or hours)
gen employed_ly_head=0
replace employed_ly_head=1 if earnings_head > 0 & earnings_head!=.

gen employed_ly_wife=0
replace employed_ly_wife=1 if earnings_wife > 0 & earnings_wife!=.

browse id survey_yr employed_ly_head employed_ly_wife WEEKLY_HRS_HEAD_ WEEKLY_HRS1_HEAD_ WEEKLY_HRS_WIFE_ WEEKLY_HRS1_WIFE_ earnings_head earnings_wife
// weekly_hrs not asked until 1994, was something asked PRIOR? i think I maybe didn't pull in GAH, use weekly_hrs1 prior to 1994 / 2001 is last yr
// okay wife bucketed 1968, real all other years? (1 or 2=PT; 3/8-FT)

gen ft_pt_head_pre=0
replace ft_pt_head_pre=1 if employed_ly_head==1 & WEEKLY_HRS1_HEAD_ >0 & WEEKLY_HRS1_HEAD_<=35
replace ft_pt_head_pre=2 if employed_ly_head==1 & WEEKLY_HRS1_HEAD_>35 & WEEKLY_HRS1_HEAD_!=.

gen ft_pt_head_post=0
replace ft_pt_head_post=1 if employed_ly_head==1 & WEEKLY_HRS_HEAD_ >0 & WEEKLY_HRS_HEAD_<=35
replace ft_pt_head_post=2 if employed_ly_head==1 & WEEKLY_HRS_HEAD_>35 & WEEKLY_HRS_HEAD_!=.

gen ft_pt_wife_pre=0
replace ft_pt_wife_pre=1 if employed_ly_wife==1 & WEEKLY_HRS1_WIFE_ >0 & WEEKLY_HRS1_WIFE_<=35 & survey_yr!=1968
replace ft_pt_wife_pre=2 if employed_ly_wife==1 & WEEKLY_HRS1_WIFE_>35 & WEEKLY_HRS1_WIFE_!=. & survey_yr!=1968
replace ft_pt_wife_pre=1 if employed_ly_wife==1 & inlist(WEEKLY_HRS1_WIFE_,1,2) & survey_yr==1968
replace ft_pt_wife_pre=2 if employed_ly_wife==1 & inrange(WEEKLY_HRS1_WIFE_,3,8) & survey_yr==1968

gen ft_pt_wife_post=0
replace ft_pt_wife_post=1 if employed_ly_wife==1 & WEEKLY_HRS_WIFE_ >0 & WEEKLY_HRS_WIFE_<=35
replace ft_pt_wife_post=2 if employed_ly_wife==1 & WEEKLY_HRS_WIFE_>35 & WEEKLY_HRS_WIFE_!=.

label define ft_pt 0 "Not Employed" 1 "PT" 2 "FT"
label values ft_pt_head_pre ft_pt_head_post ft_pt_wife_pre ft_pt_wife_post ft_pt

gen ft_pt_head=.
replace ft_pt_head = ft_pt_head_pre if inrange(survey_yr,1968,1993)
replace ft_pt_head = ft_pt_head_post if inrange(survey_yr,1994,2019)

gen ft_pt_wife=.
replace ft_pt_wife = ft_pt_wife_pre if inrange(survey_yr,1968,1993)
replace ft_pt_wife = ft_pt_wife_post if inrange(survey_yr,1994,2019)

label values ft_pt_head ft_pt_wife ft_pt

gen ft_head=0
replace ft_head=1 if ft_pt_head==2

gen ft_wife=0
replace ft_wife=1 if ft_pt_wife==2
*/

gen ft_pt_t1_head=.
replace ft_pt_t1_head = 0 if weekly_hrs_t1_head==0
replace ft_pt_t1_head = 1 if weekly_hrs_t1_head > 0 & weekly_hrs_t1_head<35
replace ft_pt_t1_head = 2 if weekly_hrs_t1_head >= 35 & weekly_hrs_t1_head < 999

gen ft_pt_t1_wife=.
replace ft_pt_t1_wife = 0 if weekly_hrs_t1_wife==0
replace ft_pt_t1_wife = 1 if weekly_hrs_t1_wife > 0 & weekly_hrs_t1_wife<35
replace ft_pt_t1_wife = 2 if weekly_hrs_t1_wife >= 35 & weekly_hrs_t1_wife < 999

label define ft_pt 0 "Not Employed" 1 "PT" 2 "FT"
label values ft_pt_t1_head ft_pt_t1_wife ft_pt

gen ft_t1_head=0
replace ft_t1_head=1 if ft_pt_t1_head==2
replace ft_t1_head=. if ft_pt_t1_head==.

gen ft_t1_wife=0
replace ft_t1_wife=1 if ft_pt_t1_wife==2
replace ft_t1_wife=. if ft_pt_t1_wife==.

// adding other controls right now, using same as SIPP analysis
gen either_enrolled=0
replace either_enrolled = 1 if ENROLLED_WIFE_==1 | ENROLLED_HEAD_==1

//race
drop if RACE_1_WIFE_==9 | RACE_1_HEAD_==9

browse id survey_yr RACE_1_WIFE_ RACE_2_WIFE_ RACE_3_WIFE_ RACE_1_HEAD_ RACE_2_HEAD_ RACE_3_HEAD_ RACE_4_HEAD_
// wait race of wife not asked until 1985?! that's wild. also need to see if codes changed in between. try to fill in historical for wife if in survey in 1985 and prior.
/*
1968-1984: 1=White; 2=Negro; 3=PR or Mexican; 7=Other
1985-1989: 1=White; 2=Black; 3=Am Indian 4=Asian 7=Other; 8 =more than 2
1990-2003: 1=White; 2=Black; 3=Am India; 4=Asian; 5=Latino; 6=Other; 7=Other
2005-2019: 1=White; 2=Black; 3=Am India; 4=Asian; 5=Native Hawaiian/Pac Is; 7=Other
*/

gen race_1_head_rec=.
replace race_1_head_rec=1 if RACE_1_HEAD_==1
replace race_1_head_rec=2 if RACE_1_HEAD_==2
replace race_1_head_rec=3 if (inrange(survey_yr,1985,2019) & RACE_1_HEAD_==3)
replace race_1_head_rec=4 if (inrange(survey_yr,1985,2019) & RACE_1_HEAD_==4)
replace race_1_head_rec=5 if (inrange(survey_yr,1968,1984) & RACE_1_HEAD_==3) | (inrange(survey_yr,1990,2003) & RACE_1_HEAD_==5)
replace race_1_head_rec=6 if RACE_1_HEAD_==7 | (inrange(survey_yr,1990,2003) & RACE_1_HEAD_==6) | (inrange(survey_yr,2005,2019) & RACE_1_HEAD_==5) | (inrange(survey_yr,1985,1989) & RACE_1_HEAD_==8)

gen race_2_head_rec=.
replace race_2_head_rec=1 if RACE_2_HEAD_==1
replace race_2_head_rec=2 if RACE_2_HEAD_==2
replace race_2_head_rec=3 if (inrange(survey_yr,1985,2019) & RACE_2_HEAD_==3)
replace race_2_head_rec=4 if (inrange(survey_yr,1985,2019) & RACE_2_HEAD_==4)
replace race_2_head_rec=5 if (inrange(survey_yr,1968,1984) & RACE_2_HEAD_==3) | (inrange(survey_yr,1990,2003) & RACE_2_HEAD_==5)
replace race_2_head_rec=6 if RACE_2_HEAD_==7 | (inrange(survey_yr,1990,2003) & RACE_2_HEAD_==6) | (inrange(survey_yr,2005,2019) & RACE_2_HEAD_==5) | (inrange(survey_yr,1985,1989) & RACE_2_HEAD_==8)

gen race_3_head_rec=.
replace race_3_head_rec=1 if RACE_3_HEAD_==1
replace race_3_head_rec=2 if RACE_3_HEAD_==2
replace race_3_head_rec=3 if (inrange(survey_yr,1985,2019) & RACE_3_HEAD_==3)
replace race_3_head_rec=4 if (inrange(survey_yr,1985,2019) & RACE_3_HEAD_==4)
replace race_3_head_rec=5 if (inrange(survey_yr,1968,1984) & RACE_3_HEAD_==3) | (inrange(survey_yr,1990,2003) & RACE_3_HEAD_==5)
replace race_3_head_rec=6 if RACE_3_HEAD_==7 | (inrange(survey_yr,1990,2003) & RACE_3_HEAD_==6) | (inrange(survey_yr,2005,2019) & RACE_3_HEAD_==5) | (inrange(survey_yr,1985,1989) & RACE_3_HEAD_==8)

gen race_4_head_rec=.
replace race_4_head_rec=1 if RACE_4_HEAD_==1
replace race_4_head_rec=2 if RACE_4_HEAD_==2
replace race_4_head_rec=3 if (inrange(survey_yr,1985,2019) & RACE_4_HEAD_==3)
replace race_4_head_rec=4 if (inrange(survey_yr,1985,2019) & RACE_4_HEAD_==4)
replace race_4_head_rec=5 if (inrange(survey_yr,1968,1984) & RACE_4_HEAD_==3) | (inrange(survey_yr,1990,2003) & RACE_4_HEAD_==5)
replace race_4_head_rec=6 if RACE_4_HEAD_==7 | (inrange(survey_yr,1990,2003) & RACE_4_HEAD_==6) | (inrange(survey_yr,2005,2019) & RACE_4_HEAD_==5) | (inrange(survey_yr,1985,1989) & RACE_4_HEAD_==8)

gen race_1_wife_rec=.
replace race_1_wife_rec=1 if RACE_1_WIFE_==1
replace race_1_wife_rec=2 if RACE_1_WIFE_==2
replace race_1_wife_rec=3 if (inrange(survey_yr,1985,2019) & RACE_1_WIFE_==3)
replace race_1_wife_rec=4 if (inrange(survey_yr,1985,2019) & RACE_1_WIFE_==4)
replace race_1_wife_rec=5 if (inrange(survey_yr,1968,1984) & RACE_1_WIFE_==3) | (inrange(survey_yr,1990,2003) & RACE_1_WIFE_==5)
replace race_1_wife_rec=6 if RACE_1_WIFE_==7 | (inrange(survey_yr,1990,2003) & RACE_1_WIFE_==6) | (inrange(survey_yr,2005,2019) & RACE_1_WIFE_==5) | (inrange(survey_yr,1985,1989) & RACE_1_WIFE_==8)

gen race_2_wife_rec=.
replace race_2_wife_rec=1 if RACE_2_WIFE_==1
replace race_2_wife_rec=2 if RACE_2_WIFE_==2
replace race_2_wife_rec=3 if (inrange(survey_yr,1985,2019) & RACE_2_WIFE_==3)
replace race_2_wife_rec=4 if (inrange(survey_yr,1985,2019) & RACE_2_WIFE_==4)
replace race_2_wife_rec=5 if (inrange(survey_yr,1968,1984) & RACE_2_WIFE_==3) | (inrange(survey_yr,1990,2003) & RACE_2_WIFE_==5)
replace race_2_wife_rec=6 if RACE_2_WIFE_==7 | (inrange(survey_yr,1990,2003) & RACE_2_WIFE_==6) | (inrange(survey_yr,2005,2019) & RACE_2_WIFE_==5) | (inrange(survey_yr,1985,1989) & RACE_2_WIFE_==8)

gen race_3_wife_rec=.
replace race_3_wife_rec=1 if RACE_3_WIFE_==1
replace race_3_wife_rec=2 if RACE_3_WIFE_==2
replace race_3_wife_rec=3 if (inrange(survey_yr,1985,2019) & RACE_3_WIFE_==3)
replace race_3_wife_rec=4 if (inrange(survey_yr,1985,2019) & RACE_3_WIFE_==4)
replace race_3_wife_rec=5 if (inrange(survey_yr,1968,1984) & RACE_3_WIFE_==3) | (inrange(survey_yr,1990,2003) & RACE_3_WIFE_==5)
replace race_3_wife_rec=6 if RACE_3_WIFE_==7 | (inrange(survey_yr,1990,2003) & RACE_3_WIFE_==6) | (inrange(survey_yr,2005,2019) & RACE_3_WIFE_==5) | (inrange(survey_yr,1985,1989) & RACE_3_WIFE_==8)

gen race_4_wife_rec=.
replace race_4_wife_rec=1 if RACE_4_WIFE_==1
replace race_4_wife_rec=2 if RACE_4_WIFE_==2
replace race_4_wife_rec=3 if (inrange(survey_yr,1985,2021) & RACE_4_WIFE_==3)
replace race_4_wife_rec=4 if (inrange(survey_yr,1985,2021) & RACE_4_WIFE_==4)
replace race_4_wife_rec=5 if (inrange(survey_yr,1968,1984) & RACE_4_WIFE_==3) | (inrange(survey_yr,1990,2003) & RACE_4_WIFE_==5)
replace race_4_wife_rec=6 if RACE_4_WIFE_==7 | (inrange(survey_yr,1990,2003) & RACE_4_WIFE_==6) | (inrange(survey_yr,2005,2021) & RACE_4_WIFE_==5) | (inrange(survey_yr,1985,1989) & RACE_4_WIFE_==8)

browse unique_id race_1_head_rec race_2_head_rec race_3_head_rec race_4_head_rec

// based on first mention (that is one option they use in SHELF)
gen race_wife=race_1_wife_rec
replace race_wife=7 if race_2_wife_rec!=.

gen race_head=race_1_head_rec
replace race_head=7 if race_2_head_rec!=.

label define race 1 "White" 2 "Black" 3 "Indian" 4 "Asian" 5 "Latino" 6 "Other" 7 "Multi-racial"
label values race_wife race_head race

// ethnicity
gen hispanic_head=.
replace hispanic_head=0 if HISPANICITY_HEAD_==0
replace hispanic_head=1 if inrange(HISPANICITY_HEAD_,1,7)

gen hispanic_wife=.
replace hispanic_wife=0 if HISPANICITY_WIFE_==0
replace hispanic_wife=1 if inrange(HISPANICITY_WIFE_,1,7)

tab race_head hispanic_head, m

// combined
gen raceth_head=.
replace raceth_head=1 if race_head==1 & (hispanic_head==0 | hispanic_head==.)
replace raceth_head=2 if race_head==2
replace raceth_head=3 if hispanic_head==1 & race_head!=2 // hispanic, non-black
replace raceth_head=3 if race_head==5 & (hispanic_head==0 | hispanic_head==.)
replace raceth_head=4 if race_head==4 & (hispanic_head==0 | hispanic_head==.)
replace raceth_head=5 if inlist(race_head,3,6,7) & (hispanic_head==0 | hispanic_head==.)

tab raceth_head, m
tab race_head raceth_head, m

gen raceth_wife=.
replace raceth_wife=1 if race_wife==1 & (hispanic_wife==0 | hispanic_wife==.)
replace raceth_wife=2 if race_wife==2
replace raceth_wife=3 if hispanic_wife==1 & race_wife!=2 // hispanic, non-black
replace raceth_wife=3 if race_wife==5 & (hispanic_wife==0 | hispanic_wife==.)
replace raceth_wife=4 if race_wife==4 & (hispanic_wife==0 | hispanic_wife==.)
replace raceth_wife=5 if inlist(race_wife,3,6,7) & (hispanic_wife==0 | hispanic_wife==.)

label define raceth 1 "NH White" 2 "Black" 3 "Hispanic" 4 "NH Asian" 5 "NH Other"
labe values raceth_head raceth_wife raceth

// figure out how to make time invariant, re: SHELF
tab raceth_head in_sample, m
tab raceth_wife in_sample, m
browse unique_id survey_yr raceth_head raceth_wife

// bysort unique_id: egen raceth_head_fixed = median(raceth_head)
bysort unique_id: egen raceth_head_fixed = mode(raceth_head) // majority
tab raceth_head_fixed, m
gen last_race_head=raceth_head if survey_yr==last_survey_yr // tie break with last reported
bysort unique_id (last_race_head): replace last_race_head = last_race_head[1]
sort unique_id survey_yr
browse unique_id survey_yr last_survey_yr raceth_head raceth_head_fixed last_race_head
replace raceth_head_fixed=last_race_head if raceth_head_fixed==.
tab raceth_head if raceth_head_fixed==., m

bysort unique_id: egen raceth_wife_fixed = mode(raceth_wife) // majority
tab raceth_wife_fixed, m
gen last_race_wife=raceth_wife if survey_yr==last_survey_yr // tie break with last reported
bysort unique_id (last_race_wife): replace last_race_wife = last_race_wife[1]
sort unique_id survey_yr
browse unique_id survey_yr last_survey_yr raceth_wife raceth_wife_fixed last_race_wife
replace raceth_wife_fixed=last_race_wife if raceth_wife_fixed==.

// realizing - I shouldn't do this this way becauase the head / wife can change over time (one reason that head / wife might seemingly change over time rather than data errors for the same person)

// if partners same race
gen same_race=0
replace same_race=1 if raceth_head==raceth_wife & raceth_head!=.

// any children - need to get more specific
label values NUM_CHILDREN_ .

gen children=.
replace children=0 if NUM_CHILDREN_==0
replace children=1 if NUM_CHILDREN_>=1 & NUM_CHILDREN_!=.

bysort unique_id: egen children_ever = max(NUM_CHILDREN_)
replace children_ever=1 if children_ever>0

sort unique_id survey_yr
browse unique_id survey_yr NUM_CHILDREN_ children_ever

browse survey_yr unique_id NUM_CHILDREN_ NUM_BIRTHS FIRST_BIRTH_YR BIRTHS_T1_HEAD_ BIRTHS_T1_WIFE_ BIRTHS_T1_BOTH_

gen when_first_birth = FIRST_BIRTH_YR
replace when_first_birth =. if FIRST_BIRTH_YR==9999 & (NUM_BIRTHS==0 | NUM_BIRTHS==98)
replace when_first_birth =. if FIRST_BIRTH_YR==9999 & NUM_BIRTHS==99 & children_ever==0
replace when_first_birth = survey_yr if NUM_BIRTHS==99 & NUM_CHILDREN_ > 0 & NUM_CHILDREN_[_n-1]==0 & unique_id==unique_id[_n-1]
bysort unique_id: egen first_birth_check = min(when_first_birth)
// browse unique_id when_first_birth first_birth_check
replace when_first_birth = first_birth_check if when_first_birth==9999 & first_birth_check!=9999 & first_birth_check!=.

sort unique_id survey_yr
// browse unique_id survey_yr when_first_birth FIRST_BIRTH_YR NUM_BIRTHS NUM_CHILDREN_ AGE_OLDEST_CHILD_ first_birth_calc if when_first_birth==9999

gen first_birth_calc = survey_yr - AGE_OLDEST_CHILD_ if survey_yr==1969  & when_first_birth==9999 & AGE_OLDEST_CHILD_!=.
bysort unique_id (first_birth_calc): replace first_birth_calc = first_birth_calc[1]
replace when_first_birth = first_birth_calc if when_first_birth==9999 & first_birth_calc!=.
gen first_birth_calc2 = survey_yr - AGE_YOUNG_CHILD_ if when_first_birth==9999 & AGE_YOUNG_CHILD_!=. // use youngest if do not have oldest, use minimum
drop first_birth_check
bysort unique_id: egen first_birth_check = min(first_birth_calc2)
replace when_first_birth = first_birth_check if when_first_birth==9999 & first_birth_check!=9999 & first_birth_check!=.

sort unique_id survey_yr
browse unique_id survey_yr when_first_birth FIRST_BIRTH_YR first_birth_check children_ever NUM_BIRTHS NUM_CHILDREN_ rel_start_yr_couple

gen pre_marital_birth=0
replace pre_marital_birth=1 if when_first_birth < rel_start_yr_couple & when_first_birth!=.

gen post_marital_birth=0
replace post_marital_birth=1 if when_first_birth >= rel_start_yr_couple & when_first_birth<=rel_end_yr_couple & when_first_birth!=. // needs to be IN marriage years, okay barely changed it

// urbanicity
gen metro=(METRO_==1) // a lot of missing, don't use for now

// religion
tabstat RELIGION_WIFE_ RELIGION_HEAD_, by(survey_yr) // just to get a sense of when asked to start.
label values RELIGION_WIFE_ RELIGION_HEAD_ . // these values are v wrong
/* head was 1970-1977, 1979-2021. wife was 1976, 1985-2021
Okay, but some weird things with how asked: 
In 1979, when this question was reinstated in the questionnaire, values were not brought forward for families with unchanged Heads since 1977.
For those cases with the same Heads from 1977 through the present, please use 1977 religious preference, V5617
So, most missings after 1977 can be interpreted as no new head, so use 1977 value? Is this another that might help if I edit once I have the variables assigned to the focal person?
Okay, but I *think* starting in 1985, was asked to everyone again? Because number of 0s goes down and the note is gone. okay, carried forward again starting 1986.
So carry through 1977-1984 if in sample and same head / partner?

The codes changed wildly over the years?
1970-1984 - 0: No or Other, 1: Baptist, 2: Methodist, 3: Episcopalian, 4: Presbyterian, 5: Lutheran, 6: Unitarian, Mormon, and related, 7: Other Protestant, 8: Catholic, 9: Jewish
1985-1987 - 0: None, 1: Roman Catholic, 2: Jewish, 3: Baptist, 4: Lutheran, 5: Methodist, 6: Presbyterian, 7: Episcopalian, 8: Protestant unspecified, 9: Other Protestant, 10: Other non-Christian, 11: LDS, 12: Jehvah's Witnesses
13: Greek Orthodox, 14: "Christian", 15: Unitarian, 16: Christian Science, 17: 7th day Adventist, 18: Pentecostal, 19: Amish, 20: Quaker, 99: NA/DK
-- in 1987, the label specifically says None, atheist, agnostic
1988-1993 - 0: None, atheist, agnostic, 1: Roman Catholic, 2: Jewish, 3: Baptist, 4: Lutheran, 5: Methodist, 6: Presbyterian, 7: Episcopalian, 8: Protestant unspecified, 9: Other Protestant, 10: Other non-Christian, 11: LDS, 12: Jehvah's Witnesses
13: Greek Orthodox, 14: "Christian", 15: Unitarian, 16: Christian Science, 17: 7th day Adventist, 18: Pentecostal, 19: Amish, 20: Quaker, 21: Church of God, 22: United Church of Christ, 23: Reformed, 24: Disciples of Christ, 25: CHurches of Christ, 97: Other, 99: NA/DK
-- so, up to 20 is the same as above, just added 21-25.
1994-2017 - 0: None, 1: Catholic, 2: Jewish, 8: Protestant unspecified, 10: Other non-Christian, 13: Greek Orthodox, 97: Other, 98: DK, 99: NA // so these large categories do match above in terms of coding (like 8 is the same, 13, etc. just way less groups)
-- In 1994, DENOMINATION was added as a separate question, so all of the detail goes to a separate question (which I don't believe I pulled in at the moment). so, I guess decide if that is worth adding.
2019-2021 - 0: Inapp (no partner), 1: None, 2: Atheist, 3: Agnostic, 4: Roman Catholic, 5: Greek Orthodox, 6: Baptist, 7: Episcopalian, 8: Jehovah's Witness, 9: Lutheran, 10: Methodist, 11: Pentecostal, 12: Presbyterian, 13: Protestant unspecified, 14: Christian, unspecified, 15: Christian, non-denominational, 16: Jewish, 17: Muslim, 18: Buddhist, 19: Other non-christian, 20: Other protestant, 21: LDS, 22: Unitarian, 23: Christian Science, 24: Adventist, 25: Amish, 26: Quaker, 27: Church of God, 28: United Church of Christ, 29: Reformed, 30: Disciples of Christ, 31: Churches of Christ, 97: Other, 98: DK, 99: NA
-- lol so DENOMINATION ends in 2017 and is integrated BACK to this question lord and the codes change AGAIN.

Denomination
1994-2017 - 0: None, atheist, agnostic, not Protestant OR no spouse (this is a lot in one), 3: Baptist, 4: Lutheran, 5: Methodist, 6: Presbyterian, 7: Episcopalian, 8: Protestant unspecified, 9: Other Protestant, 11: LDS, 12: Jehovah's witness, 14: Christian, 15: Unitarian, 16: Christian Science, 17: Adventist, 18: Pentecostal, 19: Amish, 20: Quaker, 21: Church of God, 22: United Church of Christ, 23: Reformed, 24: Disciples of Christ, 25: CHurches of Christ, 97: Other, 98: DK, 99: NA
-- so, I think aligns with how asked 1985-1993. I think if I combine the two I actually get all the same codes 0-25 (that's why some are missing)

This might be helpful: https://www.pewresearch.org/religion/2015/05/12/appendix-b-classification-of-protestant-denominations/
https://en.wikipedia.org/wiki/Protestantism_in_the_United_States#Mainline_Protestantism
https://www.thegospelcoalition.org/blogs/trevin-wax/quick-guide-christian-denominations/ - the big three are Eastern Orthodox; Catholic; Protestant
https://truthandgracecounseling.com/understanding-the-difference-between-evangelical-and-mainline-protestant-churches/
https://woollyscreamsmiracle.wordpress.com/evangelical-vs-mainline-protestant-denominations-an-overview/
-- ideally could have evangelical Protestantism, mainline Protestantism and historically black Protestantism
-- okay no because these denominations spain mainline and evangelical in their classification
*/
tab DENOMINATION_HEAD_ RELIGION_HEAD_ if inrange(survey_yr,1994,2017), m col // want to clarify how these map on so I can decide what catgories to use. so all of these are protestant denominations??

browse unique_id survey_yr RELIGION_HEAD_ DENOMINATION_HEAD_ RELIGION_WIFE_ DENOMINATION_WIFE_

gen religion_head=.
replace religion_head=0 if inrange(survey_yr,1970,1984) & RELIGION_HEAD_==0 // no religion
replace religion_head=0 if inrange(survey_yr,1985,1993) & RELIGION_HEAD_==0
replace religion_head=0 if inrange(survey_yr,1994,2017) & RELIGION_HEAD_==0
replace religion_head=0 if inrange(survey_yr,2019,2021) & inlist(RELIGION_HEAD_,1,2,3)
replace religion_head=1 if inrange(survey_yr,1970,1984) & RELIGION_HEAD_==8 // catholic
replace religion_head=1 if inrange(survey_yr,1985,1993) & RELIGION_HEAD_==1
replace religion_head=1 if inrange(survey_yr,1994,2017) & RELIGION_HEAD_==1
replace religion_head=1 if inrange(survey_yr,2019,2021) & RELIGION_HEAD_==4
replace religion_head=2 if inrange(survey_yr,1970,1984) & RELIGION_HEAD_==9 // jewish
replace religion_head=2 if inrange(survey_yr,1985,1993) & RELIGION_HEAD_==2
replace religion_head=2 if inrange(survey_yr,1994,2017) & RELIGION_HEAD_==2
replace religion_head=2 if inrange(survey_yr,2019,2021) & RELIGION_HEAD_==16
replace religion_head=3 if inrange(survey_yr,1970,1984) & RELIGION_HEAD_==1 // baptist
replace religion_head=3 if inrange(survey_yr,1985,1993) & RELIGION_HEAD_==3
replace religion_head=3 if inrange(survey_yr,1994,2017) & RELIGION_HEAD_==8 & DENOMINATION_HEAD_==3
replace religion_head=3 if inrange(survey_yr,2019,2021) & RELIGION_HEAD_==6
replace religion_head=4 if inrange(survey_yr,1970,1984) & inlist(RELIGION_HEAD_,2,3,4,5) // mainline protestant
replace religion_head=4 if inrange(survey_yr,1985,1993) & inlist(RELIGION_HEAD_,4,5,6,7,22,23,24)
replace religion_head=4 if inrange(survey_yr,1994,2017) & RELIGION_HEAD_==8 & inlist(DENOMINATION_HEAD_,4,5,6,7,22,23,24)
replace religion_head=4 if inrange(survey_yr,2019,2021) & inlist(RELIGION_HEAD_,7,9,10,12,28,29,30)
// replace religion_head=5 if inrange(survey_yr,1970,1984) & inlist(RELIGION_HEAD_,) // evangelical protestant - none in first waves
replace religion_head=5 if inrange(survey_yr,1985,1993) & inlist(RELIGION_HEAD_,17,18,21,25)
replace religion_head=5 if inrange(survey_yr,1994,2017) & RELIGION_HEAD_==8 & inlist(DENOMINATION_HEAD_,17,18,21,25)
replace religion_head=5 if inrange(survey_yr,2019,2021) & inlist(RELIGION_HEAD_,11,24,27,31)
replace religion_head=6 if inrange(survey_yr,1970,1984) & RELIGION_HEAD_==7 // other protestant
replace religion_head=6 if inrange(survey_yr,1985,1993) & inlist(RELIGION_HEAD_,8,9,19,20)
replace religion_head=6 if inrange(survey_yr,1994,2017) & RELIGION_HEAD_==8 & inlist(DENOMINATION_HEAD_,8,9,19,20,97,98,99)
replace religion_head=6 if inrange(survey_yr,2019,2021) & inlist(RELIGION_HEAD_,13,20,25,26)
// replace religion_head=7 if inrange(survey_yr,1970,1984) & inlist(RELIGION_HEAD_,) // eastern orthodox
replace religion_head=7 if inrange(survey_yr,1985,1993) & RELIGION_HEAD_==13
replace religion_head=7 if inrange(survey_yr,1994,2017) & RELIGION_HEAD_==13
replace religion_head=7 if inrange(survey_yr,2019,2021) & RELIGION_HEAD_==5
replace religion_head=8 if inrange(survey_yr,1970,1984) & RELIGION_HEAD_==6 // other christian
replace religion_head=8 if inrange(survey_yr,1985,1993) & inlist(RELIGION_HEAD_,11,12,14,15,16)
replace religion_head=8 if inrange(survey_yr,1994,2017) & inlist(DENOMINATION_HEAD_,11,12,14,15,16)
replace religion_head=8 if inrange(survey_yr,2019,2021) & inlist(RELIGION_HEAD_,8,14,15,21,22,23)
// replace religion_head=9 if inrange(survey_yr,1970,1984) & inlist(RELIGION_HEAD_,) // other non-christian
replace religion_head=9 if inrange(survey_yr,1985,1993) & RELIGION_HEAD_==10
replace religion_head=9 if inrange(survey_yr,1994,2017) & RELIGION_HEAD_==10
replace religion_head=9 if inrange(survey_yr,2019,2021) & inlist(RELIGION_HEAD_,17,18,19)
// replace religion_head=10 if inrange(survey_yr,1970,1984) & inlist(RELIGION_HEAD_,) // other other
replace religion_head=10 if inrange(survey_yr,1985,1993) & RELIGION_HEAD_==97
replace religion_head=10 if inrange(survey_yr,1994,2017) & RELIGION_HEAD_==97
replace religion_head=10 if inrange(survey_yr,2019,2021) & RELIGION_HEAD_==97
// replace religion_head=. if inrange(survey_yr,1970,1984) & inlist(RELIGION_HEAD_,) // missing
replace religion_head=. if inrange(survey_yr,1985,1993) & RELIGION_HEAD_==99
replace religion_head=. if inrange(survey_yr,1994,2017) & inlist(RELIGION_HEAD_,98,99)
replace religion_head=. if inrange(survey_yr,2019,2021) & inlist(RELIGION_HEAD_,98,99)

gen religion_wife=.
replace religion_wife=0 if inrange(survey_yr,1970,1984) & RELIGION_WIFE_==0 // no religion
replace religion_wife=0 if inrange(survey_yr,1985,1993) & RELIGION_WIFE_==0
replace religion_wife=0 if inrange(survey_yr,1994,2017) & RELIGION_WIFE_==0
replace religion_wife=0 if inrange(survey_yr,2019,2021) & inlist(RELIGION_WIFE_,1,2,3)
replace religion_wife=1 if inrange(survey_yr,1970,1984) & RELIGION_WIFE_==8 // catholic
replace religion_wife=1 if inrange(survey_yr,1985,1993) & RELIGION_WIFE_==1
replace religion_wife=1 if inrange(survey_yr,1994,2017) & RELIGION_WIFE_==1
replace religion_wife=1 if inrange(survey_yr,2019,2021) & RELIGION_WIFE_==4
replace religion_wife=2 if inrange(survey_yr,1970,1984) & RELIGION_WIFE_==9 // jewish
replace religion_wife=2 if inrange(survey_yr,1985,1993) & RELIGION_WIFE_==2
replace religion_wife=2 if inrange(survey_yr,1994,2017) & RELIGION_WIFE_==2
replace religion_wife=2 if inrange(survey_yr,2019,2021) & RELIGION_WIFE_==16
replace religion_wife=3 if inrange(survey_yr,1970,1984) & RELIGION_WIFE_==1 // baptist
replace religion_wife=3 if inrange(survey_yr,1985,1993) & RELIGION_WIFE_==3
replace religion_wife=3 if inrange(survey_yr,1994,2017) & RELIGION_WIFE_==8 & DENOMINATION_WIFE_==3
replace religion_wife=3 if inrange(survey_yr,2019,2021) & RELIGION_WIFE_==6
replace religion_wife=4 if inrange(survey_yr,1970,1984) & inlist(RELIGION_WIFE_,2,3,4,5) // mainline protestant
replace religion_wife=4 if inrange(survey_yr,1985,1993) & inlist(RELIGION_WIFE_,4,5,6,7,22,23,24)
replace religion_wife=4 if inrange(survey_yr,1994,2017) & RELIGION_WIFE_==8 & inlist(DENOMINATION_WIFE_,4,5,6,7,22,23,24)
replace religion_wife=4 if inrange(survey_yr,2019,2021) & inlist(RELIGION_WIFE_,7,9,10,12,28,29,30)
// replace religion_wife=5 if inrange(survey_yr,1970,1984) & inlist(RELIGION_WIFE_,) // evangelical protestant - none in first waves
replace religion_wife=5 if inrange(survey_yr,1985,1993) & inlist(RELIGION_WIFE_,17,18,21,25)
replace religion_wife=5 if inrange(survey_yr,1994,2017) & RELIGION_WIFE_==8 & inlist(DENOMINATION_WIFE_,17,18,21,25)
replace religion_wife=5 if inrange(survey_yr,2019,2021) & inlist(RELIGION_WIFE_,11,24,27,31)
replace religion_wife=6 if inrange(survey_yr,1970,1984) & RELIGION_WIFE_==7 // other protestant
replace religion_wife=6 if inrange(survey_yr,1985,1993) & inlist(RELIGION_WIFE_,8,9,19,20)
replace religion_wife=6 if inrange(survey_yr,1994,2017) & RELIGION_WIFE_==8 & inlist(DENOMINATION_WIFE_,8,9,19,20,97,98,99)
replace religion_wife=6 if inrange(survey_yr,2019,2021) & inlist(RELIGION_WIFE_,13,20,25,26)
// replace religion_wife=7 if inrange(survey_yr,1970,1984) & inlist(RELIGION_WIFE_,) // eastern orthodox
replace religion_wife=7 if inrange(survey_yr,1985,1993) & RELIGION_WIFE_==13
replace religion_wife=7 if inrange(survey_yr,1994,2017) & RELIGION_WIFE_==13
replace religion_wife=7 if inrange(survey_yr,2019,2021) & RELIGION_WIFE_==5
replace religion_wife=8 if inrange(survey_yr,1970,1984) & RELIGION_WIFE_==6 // other christian
replace religion_wife=8 if inrange(survey_yr,1985,1993) & inlist(RELIGION_WIFE_,11,12,14,15,16)
replace religion_wife=8 if inrange(survey_yr,1994,2017) & inlist(DENOMINATION_WIFE_,11,12,14,15,16)
replace religion_wife=8 if inrange(survey_yr,2019,2021) & inlist(RELIGION_WIFE_,8,14,15,21,22,23)
// replace religion_wife=9 if inrange(survey_yr,1970,1984) & inlist(RELIGION_WIFE_,) // other non-christian
replace religion_wife=9 if inrange(survey_yr,1985,1993) & RELIGION_WIFE_==10
replace religion_wife=9 if inrange(survey_yr,1994,2017) & RELIGION_WIFE_==10
replace religion_wife=9 if inrange(survey_yr,2019,2021) & inlist(RELIGION_WIFE_,17,18,19)
// replace religion_wife=10 if inrange(survey_yr,1970,1984) & inlist(RELIGION_WIFE_,) // other other
replace religion_wife=10 if inrange(survey_yr,1985,1993) & RELIGION_WIFE_==97
replace religion_wife=10 if inrange(survey_yr,1994,2017) & RELIGION_WIFE_==97
replace religion_wife=10 if inrange(survey_yr,2019,2021) & RELIGION_WIFE_==97
// replace religion_wife=. if inrange(survey_yr,1970,1984) & inlist(RELIGION_WIFE_,) // missing
replace religion_wife=. if inrange(survey_yr,1985,1993) & RELIGION_WIFE_==99
replace religion_wife=. if inrange(survey_yr,1994,2017) & inlist(RELIGION_WIFE_,98,99)
replace religion_wife=. if inrange(survey_yr,2019,2021) & inlist(RELIGION_WIFE_,98,99)

label define religion 0 "No religion" 1 "Catholic" 2 "Jewish" 3 "Baptist" 4 "Mainline Protestant" 5 "Evangelical Protestant" 6 "Other Protestant" 7 "Eastern Orthodox" 8 "Other Christian" 9 "Other Non-Christian" 10 "Other Other"
label values religion_head religion_wife religion
tab religion_head, m
tab RELIGION_HEAD_ religion_head, m

tab religion_wife, m
tab RELIGION_WIFE_ religion_wife, m // much more missing - not asked consistently until 1985 (I drop those anyway)

// figuring out costs - creating these variables for posterity (come from other code), but I don't use these
browse id survey_yr HOUSE_STATUS_ RENT_COST_V1_ RENT_COST_V2_ TOTAL_HOUSING_ MORTGAGE_COST_ HOUSE_VALUE_  CHILDCARE_COSTS_

gen total_annual_rent=.
replace total_annual_rent = RENT_COST_V1_ if survey_yr <=1993
replace total_annual_rent = RENT_COST_V2_ * 12 if survey_yr > 1994

browse id survey_yr total_annual_rent RENT_COST_V1_ RENT_COST_V2_ if HOUSE_STATUS_==5

browse id survey_yr  TOTAL_HOUSING_ MORTGAGE_COST_ HOUSE_VALUE_  if HOUSE_STATUS_==1

egen total_annual_rent_std = std(total_annual_rent) if HOUSE_STATUS_==5
egen HOUSE_VALUE_std = std(HOUSE_VALUE_) if HOUSE_STATUS_==1
// browse total_annual_rent total_annual_rent_std HOUSE_VALUE_ HOUSE_VALUE_std

gen housing_costs_use=.
replace housing_costs_use = total_annual_rent_std if HOUSE_STATUS_==5
replace housing_costs_use = HOUSE_VALUE_std if HOUSE_STATUS_==1

replace CHILDCARE_COSTS_=. if inlist(CHILDCARE_COSTS_,99998,99999,999998,999999)

// other joint investments
* wealth - can I interpolate in between the years I don't have?
browse id survey_yr WEALTH_NO_EQUITY_ WEALTH_EQUITY_

bysort unique_id: ipolate WEALTH_NO_EQUITY_ survey_yr if survey_yr>=1984 & survey_yr<=2021, gen(WEALTH_NO_EQUITY_i)
bysort unique_id: ipolate WEALTH_EQUITY_ survey_yr if survey_yr>=1984 & survey_yr<=2021, gen(WEALTH_EQUITY_i)

* same with vehicle ownership and value
browse id survey_yr VEHICLE_OWN_ VEHICLE_VALUE_
bysort unique_id: ipolate VEHICLE_OWN_ survey_yr, gen(VEHICLE_OWN_i) // so this only works if there is another observed value. so, for some, there is just a last value, so it doesn't carry that through. can I add tht in? oh is that where ipolate works?
bysort unique_id: ipolate VEHICLE_OWN_ survey_yr, gen(VEHICLE_OWN_e) epolate
browse id survey_yr VEHICLE_OWN_*

replace VEHICLE_OWN_e=1 if VEHICLE_OWN_e>=1 & VEHICLE_OWN_e<5 // so if changes from 1 to 5, fills in, let's set all to 1
replace VEHICLE_OWN_e=1 if VEHICLE_OWN_e<=1 // negatives also seem to be because blanks turn to 1
replace VEHICLE_OWN_e=5 if VEHICLE_OWN_e>=5 & VEHICLE_OWN_e<100  // make these nos - if no, or dk, or anything in between

	// bysort id: egen vehicle_own_last = mode(VEHICLE_OWN_)
	// browse id survey_yr VEHICLE_OWN_ vehicle_own_last VEHICLE_OWN_i
	// replace VEHICLE_OWN_i = vehicle_own_last if VEHICLE_OWN_i==.

bysort unique_id: ipolate VEHICLE_VALUE_ survey_yr if survey_yr>=1984 & survey_yr<=2021, gen(VEHICLE_VALUE_i)

* other assets - okay let's just use wealth
tabstat DIVIDENDS_JOINT_ DIVIDENDS_HEAD_ DIVIDENDS_WIFE_, by(survey_yr)
browse id survey_yr DIVIDENDS_JOINT_ DIVIDENDS_HEAD_ DIVIDENDS_WIFE_ // okay joint not that useful - not many say yes and only since 2003. very few WIFEs also have dividends... okay let's not use these

tabstat INTEREST_JOINT_ BANK_ASSETS_ OTHER_ASSETS_ STOCKS_MF_, by(survey_yr)

// indicators of hardship
tabstat WELFARE_HEAD_1_  WELFARE_HEAD_2_ WELFARE_WIFE_1_ WELFARE_WIFE_2_ WELFARE_JOINT_, by(survey_yr) // 1 and 2 have overlapping years, see if they match, then decide which to use - these feel very not matched?!
browse id survey_yr WELFARE_HEAD_1_  WELFARE_HEAD_2_ WELFARE_WIFE_1_ WELFARE_WIFE_2_ WELFARE_JOINT_ // 1 and 2 have overlapping years, see if they match, then decide which to use - okay incidence is SO LOW. this also doesn't feel right??

gen receive_welfare=.
replace receive_welfare=0 if WELFARE_JOINT_==0 & survey_yr <=1985 // only indicator at this time
replace receive_welfare=0 if WELFARE_HEAD_1_==0 & WELFARE_WIFE_1_==0 & survey_yr >1985 & survey_yr < 1993
replace receive_welfare=0 if WELFARE_HEAD_2_==0 & WELFARE_WIFE_2_==0 & survey_yr >1985 & survey_yr >=1993
replace receive_welfare=1 if WELFARE_JOINT_>0 & WELFARE_JOINT_!=. & survey_yr <=1985 // only indicator at this time
replace receive_welfare=1 if ((WELFARE_HEAD_1_>0 & WELFARE_HEAD_1_!=.) | (WELFARE_WIFE_1_>0 & WELFARE_WIFE_1_!=.)) & survey_yr >1985 & survey_yr < 1993
replace receive_welfare=1 if ((WELFARE_HEAD_2_>0 & WELFARE_HEAD_2_!=.) | (WELFARE_WIFE_2_>0 & WELFARE_WIFE_2_!=.))  & survey_yr >1985 & survey_yr >=1993

gen receive_transfers=.
replace receive_transfers=0 if TRANSFER_INCOME_==0
replace receive_transfers=1 if TRANSFER_INCOME_>0 &TRANSFER_INCOME_!=.

// also age at relationship start
replace year_birth = survey_yr - AGE_INDV if year_birth==. & AGE_INDV!=999
browse unique_id partner_unique_id survey_yr relation SEX year_birth  AGE_INDV AGE_HEAD_ AGE_WIFE_

inspect AGE_HEAD_
inspect AGE_WIFE_ // sometimes 0 - since this is time invariant, think I can fill in.

gen yr_born_head_0 = survey_yr - AGE_HEAD_ if AGE_HEAD_!=0 & AGE_HEAD!=9999
gen yr_born_wife_0 = survey_yr - AGE_WIFE_ if AGE_WIFE_!=0 & AGE_WIFE!=9999

bysort unique_id partner_unique_id: egen yr_born_head = min(yr_born_head_0)
bysort unique_id partner_unique_id: egen yr_born_wife = min(yr_born_wife_0)

browse unique_id partner_unique_id survey_yr relation SEX year_birth  AGE_INDV AGE_HEAD_ AGE_WIFE_ yr_born_head yr_born_head_0 yr_born_wife yr_born_wife_0

gen age_mar_head = rel_start_yr_couple -  yr_born_head
replace age_mar_head = . if inrange(age_mar_head,900,1000)
gen age_mar_wife = rel_start_yr_couple -  yr_born_wife
replace age_mar_wife = . if inrange(age_mar_wife,900,1000)

drop if age_mar_head < 0 | age_mar_wife < 0

********************************************************************************
**# Cohab history specifically (for control variables)
********************************************************************************

// merge cohabitation history for head
merge m:1 unique_id using "$temp/PSID_partner_history.dta", keepusing(MX8* partner_unique_id*) // I AM DUMB - it shouldn;t all match because it is only people who ever had A COHABITING partner, not others. // partner_1968_id* partner_per_num* - not sure why I wasn't using unique ID befre...

drop if _merge==2 // people not in my sample
gen ever_cohab_head = 1 if _merge==3
replace ever_cohab_head=0 if ever_cohab_head==.
drop _merge
tab current_rel_type ever_cohab_head, m

foreach var in MX8* partner_unique_id*{
	rename `var' `var'_head
}

rename partner_unique_id_head partner_unique_id

// k now trying to match on PARTNER id to get HER history
merge m:1 partner_unique_id using "$temp\PSID_partner_history.dta", keepusing(MX8* partner_unique_id*) // less matches but I am not surprised about this (i don't think??
// merge m:1 spouse_per_num_all spouse_id_all // why am I confused? which should I match on? Okay, partner ID comes from family matrix, spouse comes from marital history. problem with marital history is that it's not comprehensive bc doesn't cover cohab. nor does it cover certain relationships prior to 1985, so use partner unique id
// inspect spouse_per_num_all if marital_status_updated==1
// inspect spouse_per_num_all if marital_status_updated==2

drop if _merge==2 // people not in my sample
gen ever_cohab_wife = 1 if _merge==3
replace ever_cohab_wife=0 if ever_cohab_wife==.
drop _merge
tab current_rel_type ever_cohab_wife, m

tab ever_cohab_wife ever_cohab_head, m

foreach var in MX8* partner_unique_id*{
	rename `var' `var'_wife
}

rename partner_unique_id*_head_wife partner_unique_id*_head
rename partner_unique_id_wife partner_unique_id

// tmp save

sort unique_id survey_yr
browse unique_id partner_unique_id survey_yr marital_status_updated current_rel_number ever_cohab_head rel_start_yr_couple rel_end_yr_couple partner_unique_id*_head partner_unique_id*_wife // okay not all of these make sense anymore if I keep cohabitors, but the pre / other ones do. should I only make the "with wife" ones for married couples? well, these are all pre rel, so think might only end up applying to married? can check at end
drop if rel_start_yr_couple==. // small amount - this will mess up below code and also I can't use these couples anyway, so drop here
// example of someone who should have a cohab other: 5559001

// okay, now that I am adding cohab, this is a little trickier - because actually, if I observed their transition from cohab to marriage, they are not captured here, because the relationship start date is for cohab, not marriage.

forvalues y=1968/1997{
	capture gen cohab_`y'_with_partner_head=0
	replace cohab_`y'_with_partner_head=1 if `y' < rel_start_yr_couple & partner_unique_id`y'_head==partner_unique_id
	replace cohab_`y'_with_partner_head=1 if `y' < transition_year & partner_unique_id`y'_head==partner_unique_id & ever_transition==1
	
	capture gen cohab_`y'_with_partner_wife=0
	replace cohab_`y'_with_partner_wife=1 if `y' < rel_start_yr_couple & partner_unique_id`y'_wife==unique_id
	replace cohab_`y'_with_partner_wife=1 if `y' < transition_year & partner_unique_id`y'_wife==unique_id & ever_transition==1
}

forvalues y=1999(2)2021{
	capture gen cohab_`y'_with_partner_head=0
	replace cohab_`y'_with_partner_head=1 if `y' < rel_start_yr_couple & partner_unique_id`y'_head==partner_unique_id
	replace cohab_`y'_with_partner_head=1 if `y' < transition_year & partner_unique_id`y'_head==partner_unique_id & ever_transition==1
		
	capture gen cohab_`y'_with_partner_wife=0
	replace cohab_`y'_with_partner_wife=1 if `y' < rel_start_yr_couple & partner_unique_id`y'_wife==unique_id
	replace cohab_`y'_with_partner_wife=1 if `y' < transition_year & partner_unique_id`y'_wife==unique_id & ever_transition==1
}

forvalues y=1968/1997{
	gen cohab_`y'_other_head=0
	replace cohab_`y'_other_head=1 if `y' < rel_start_yr_couple & partner_unique_id`y'_head!=partner_unique_id & partner_unique_id!=. & partner_unique_id`y'_head!=.
	
	gen cohab_`y'_other_wife=0
	replace cohab_`y'_other_wife=1 if `y' < rel_start_yr_couple & partner_unique_id`y'_wife!=unique_id & unique_id!=. & partner_unique_id`y'_wife!=.
}

forvalues y=1999(2)2021{
	gen cohab_`y'_other_head=0
	replace cohab_`y'_other_head=1 if `y' < rel_start_yr_couple & partner_unique_id`y'_head!=partner_unique_id & partner_unique_id!=. & partner_unique_id`y'_head!=.
	
	gen cohab_`y'_other_wife=0
	replace cohab_`y'_other_wife=1 if `y' < rel_start_yr_couple & partner_unique_id`y'_wife!=unique_id & unique_id!=. & partner_unique_id`y'_wife!=.
}


forvalues y=1968/1997{
	gen cohab_`y'_after_head=0
	replace cohab_`y'_after_head=1 if `y' > rel_end_yr_couple & partner_unique_id`y'_head!=.
	
	gen cohab_`y'_after_wife=0
	replace cohab_`y'_after_wife=1 if `y' > rel_end_yr_couple & partner_unique_id`y'_wife!=.
}

forvalues y=1999(2)2021{
	gen cohab_`y'_after_head=0
	replace cohab_`y'_after_head=1 if `y' > rel_end_yr_couple & partner_unique_id`y'_head!=.
	
	gen cohab_`y'_after_wife=0
	replace cohab_`y'_after_wife=1 if `y' > rel_end_yr_couple & partner_unique_id`y'_wife!=.
}

browse unique_id partner_unique_id survey_yr current_rel_type ever_cohab_head partner_unique_id rel_start_yr_couple rel_end_yr_couple cohab_*_with_partner_head cohab_*_other_head partner_unique_id*_head

egen cohab_with_partner_head = rowtotal(cohab_*_with_partner_head)
replace cohab_with_partner_head = 1 if cohab_with_partner_head > 1 & cohab_with_partner_head!=.

egen cohab_with_partner_wife = rowtotal(cohab_*_with_partner_wife)
replace cohab_with_partner_wife = 1 if cohab_with_partner_wife > 1 & cohab_with_partner_wife!=.

tab cohab_with_partner_head cohab_with_partner_wife, m
tab ever_transition cohab_with_partner_head, m // so this variable is not that useful at the moment given we have added cohab, so let's come back to this
tab marital_status_updated cohab_with_partner_head, m row

egen cohab_with_other_head = rowtotal(cohab_*_other_head)
replace cohab_with_other_head = 1 if cohab_with_other_head > 1 & cohab_with_other_head!=.

egen cohab_with_other_wife = rowtotal(cohab_*_other_wife)
replace cohab_with_other_wife = 1 if cohab_with_other_wife > 1 & cohab_with_other_wife!=.

tab cohab_with_other_head cohab_with_other_wife, m 

egen cohab_after_head = rowtotal(cohab_*_after_head)
replace cohab_after_head = 1 if cohab_after_head > 1 & cohab_after_head!=.

egen cohab_after_wife = rowtotal(cohab_*_after_wife)
replace cohab_after_wife = 1 if cohab_after_wife > 1 & cohab_after_wife!=.

tab ever_cohab_head cohab_with_partner_head
tab ever_cohab_head cohab_with_other_head
tab ever_cohab_head cohab_after_head

browse unique_id partner_unique_id survey_yr rel_start_yr_couple current_rel_type ever_cohab_head cohab_with_other_head cohab_with_partner_head cohab_after_head
tab rel_start_yr_couple if ever_cohab_head==1 & cohab_with_other_head==0 & cohab_with_partner_head==0 & cohab_after_head==0 & current_rel_type==20 // okay few actully not accounted for, and most are relationships started before PSID

// current challenge to address? 
tab MARITAL_PAIRS dissolve, m // so - i think there will be problems with both partners info if not recorded as being in a marital pair
tab MARITAL_PAIRS current_rel_type, m // okay so i did fix this for marriage (a key goal of last time) - it's mostly cohab
sort unique_id survey_yr 
browse unique_id partner_unique_id survey_yr current_rel_type rel_start_yr_couple rel_end_yr_couple how_end_couple dissolve MARITAL_PAIRS_ weekly_hrs_t1_wife weekly_hrs_t1_head // a lot of these feel like 1 year rels - let's validate with below
tab weekly_hrs_t1_wife if MARITAL_PAIRS==0, m // yeah these are like 99% zero. So, do I drop them? move up the dissolve?
tab in_marital_history if dissolve==1 & MARITAL_PAIRS_==0, m // is in marital history over or under reprsented - like is that more or less contribuitng to this problem? okay yes, so ALL in marital history. does it depend like month of survey v. month of dissolve? because marital history updated retrospetively so like maybe when answered 2005 survey, they were together and living together in month 8, but then then divorce month 10 - marital history will update, but there will still be interview data for wife because she was there at time of survey. so it's probably people who divorced later v. earler in year - so I want last full year of data I can get, whenever that is? technically I do have months in marital history... but it really doesn't matter like I don't get that info either way...
 // do I have to lag alll info though? or like JUST update year of dissolution? replace year of dissolution with all prior values of wife variables?
 
// relationship_duration
gen dur = survey_yr - rel_start_yr_couple
browse unique_id partner_unique_id survey_yr rel_start_yr_couple dur
tab dur, m

bysort unique_id partner_unique_id: egen min_dur = min(dur)
bysort unique_id partner_unique_id: egen max_dur = max(dur)

tab dur current_rel_type if MARITAL_PAIRS==0 // mostly first year (which makes sense if not yet married at time of interview OR first yr cohabitors (except - they should not be in my file yet)? 
tab max_dur if MARITAL_PAIRS==0 // then yes, many are 1 year rels
tab dur max_dur if MARITAL_PAIRS==0, m

// okay yes: 
// tab MARITAL_PAIRS if min_dur == dur & current_rel_type==22, m // 4327
// tab MARITAL_PAIRS  // out of 4602 total that are 0

sort unique_id survey_yr
browse unique_id partner_unique_id survey_yr dur min_dur max_dur current_rel_type rel_start_yr_couple rel_end_yr_couple how_end_couple dissolve MARITAL_PAIRS_ weekly_hrs_t1_wife weekly_hrs_t1_head

save "$created_data/PSID_union_sample_rec.dta", replace
