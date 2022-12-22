********************************************************************************
* Getting PSID sample for transition from cohabitation to marriage
* cohabitation_sample.do
* Kim McErlean
********************************************************************************

use "$data_tmp\PSID_full_long.dta", clear // created in step 1

// need to replace all "id" with INTERVIEW_NUM_, okay wait no unique ID
// BUT interview_num_ to match later
********************************************************************************
* First clean up to get a sense of WHO is even eligible
********************************************************************************

browse survey_yr main_per_id id unique_id INTERVIEW_NUM_ INTERVIEW_NUM_ SEQ_NUMBER_ RELATION_ FIRST_MARRIAGE_YR_START MARITAL_PAIRS_ COUPLE_STATUS_REF_

drop if survey_yr <1985 // 1983 is first time you could identify cohab, using this as "recent" cutoff

gen relationship=0
replace relationship=1 if inrange(MARITAL_PAIRS_,1,4)

sort id survey_yr
browse survey_yr unique_id id main_per_id SEQ_NUMBER_ relationship RELATION_ FIRST_MARRIAGE_YR_START  // so unique_id and id are different numbers, but same people

bysort unique_id (SEQ_NUMBER_): egen in_sample=max(SEQ_NUMBER_)

drop if in_sample==0 // people with NO DATA in any year
drop if SEQ_NUMBER_==0 // won't have data because not in that year -- like SIPP, how do I know if last year is because divorced or last year in sample? right now individual level file, so fine - this is JUST last year in sample at the moment

gen relation_gp = . // this is an INDIVIDUAL measure
replace relation_gp = 1 if RELATION_== 10
replace relation_gp = 2 if RELATION_== 20
replace relation_gp = 3 if inlist(RELATION_, 22, 88)
replace relation_gp = 4 if inrange(RELATION_,30,87) | inrange(RELATION_,89,99)

label define relation_gp 1 "Head" 2 "Spouse" 3 "Partner" 4 "Other"
label values relation_gp relation_gp

browse id unique_id survey_yr main_per_id relationship RELATION_ COUPLE_STATUS_

gen relationship_type_head=. // this is a HH measure
replace relationship_type_head=0 if inlist(COUPLE_STATUS_REF,3,5)
replace relationship_type_head=1 if COUPLE_STATUS_REF_==1
replace relationship_type_head=2 if inlist(COUPLE_STATUS_REF,2,4)

label define relationship_type_head 0 "Single" 1 "Married" 2 "Cohab"
label values relationship_type_head relationship_type_head 

// Restrict to couples, since we only have for ref / head, i basically just drop if NOT that person or relationship? but then need to remove heads without a partner
drop if relation_gp==4
drop if relationship_type_head==0
* some partners not getting picked up as MARITAL PAIRS - is it because they are ST (aka first year not "permanent"). yes, exactly, FIRST YEAR cohabitors (and then heads) are not considered "marital pairs" so need to use the relationship_type_head measure

*********************************************
* Variable recodes
*********************************************
// education
browse survey_yr id  EDUC1_WIFE_ EDUC_WIFE_ EDUC1_HEAD_ EDUC_HEAD_
// educ1 until 1990, but educ started 1975, okay but then a gap until 1991? wife not asked 1969-1971 - might be able to fill in if she is in sample either 1968 or 1972? (match to the id)
// codes are also different between the two, use educ1 until 1990, then educ 1991 post

recode EDUC1_WIFE_ (1/3=1)(4/5=2)(6=3)(7/8=4)(9=.)(0=1), gen(educ_wife_early)
recode EDUC1_HEAD_ (1/3=1)(4/5=2)(6=3)(7/8=4)(9=.)(0=1), gen(educ_head_early)
recode EDUC_WIFE_ (0/11=1) (12=2) (13/15=3) (16/17=4) (99=.), gen(educ_wife_1975)
recode EDUC_HEAD_ (0/11=1) (12=2) (13/15=3) (16/17=4) (99=.), gen(educ_head_1975)

label define educ 1 "LTHS" 2 "HS" 3 "Some College" 4 "College"
label values educ_wife_early educ_head_early educ_wife_1975 educ_head_1975 educ

gen educ_wife=.
replace educ_wife=educ_wife_early if inrange(survey_yr,1968,1990)
replace educ_wife=educ_wife_1975 if inrange(survey_yr,1991,2019)

gen educ_head=.
replace educ_head=educ_head_early if inrange(survey_yr,1968,1990)
replace educ_head=educ_head_1975 if inrange(survey_yr,1991,2019)

label values educ_wife educ_head educ

	// trying to fill in missing wife years when possible
	browse id survey_yr educ_wife if inlist(id,3,12,25,117)
	bysort id (educ_wife): replace educ_wife=educ_wife[1] if educ_wife==.


gen college_complete_wife=0
replace college_complete_wife=1 if educ_wife==4
gen college_complete_head=0
replace college_complete_head=1 if educ_head==4

gen couple_educ_gp=0
replace couple_educ_gp=1 if (college_complete_wife==1 | college_complete_head==1)

label define couple_educ 0 "Neither College" 1 "At Least One College"
label values couple_educ_gp couple_educ

gen educ_type=.
replace educ_type=1 if educ_head > educ_wife & educ_head!=. & educ_wife!=.
replace educ_type=2 if educ_head < educ_wife & educ_head!=. & educ_wife!=.
replace educ_type=3 if educ_head == educ_wife & educ_head!=. & educ_wife!=.

label define educ_type 1 "Hyper" 2 "Hypo" 3 "Homo"
label values educ_type educ_type

// income / structure
browse id survey_yr FAMILY_INTERVIEW_NUM_ TAXABLE_HEAD_WIFE_ TOTAL_FAMILY_INCOME_ LABOR_INCOME_HEAD_ WAGES_HEAD_  LABOR_INCOME_WIFE_ WAGES_WIFE_ 

	// to use: WAGES_HEAD_ WAGES_WIFE_ -- wife not asked until 1993? okay labor income??
	// wages and labor income asked for head whole time. labor income wife 1968-1993, wages for wife, 1993 onwards

gen earnings_wife=.
replace earnings_wife = LABOR_INCOME_WIFE_ if inrange(survey_yr,1968,1993)
replace earnings_wife = WAGES_WIFE_ if inrange(survey_yr,1994,2019)

gen earnings_head=.
replace earnings_head = LABOR_INCOME_HEAD_ if inrange(survey_yr,1968,1993)
replace earnings_head = WAGES_HEAD_ if inrange(survey_yr,1994,2019)

egen couple_earnings = rowtotal(earnings_wife earnings_head)

browse id survey_yr earnings_wife earnings_head couple_earnings

// employment
browse id survey_yr EMPLOY_STATUS_HEAD_ EMPLOY_STATUS1_HEAD_ EMPLOY_STATUS2_HEAD_ EMPLOY_STATUS3_HEAD_ EMPLOY_STATUS_WIFE_ EMPLOY_STATUS1_WIFE_ EMPLOY_STATUS2_WIFE_ EMPLOY_STATUS3_WIFE_
// not numbered until 1994; 1-3 arose in 1994. codes match
// wife not asked until 1976?

gen employ_head=0
replace employ_head=1 if EMPLOY_STATUS_HEAD_==1
gen employ1_head=0
replace employ1_head=1 if EMPLOY_STATUS1_HEAD_==1
gen employ2_head=0
replace employ2_head=1 if EMPLOY_STATUS2_HEAD_==1
gen employ3_head=0
replace employ3_head=1 if EMPLOY_STATUS3_HEAD_==1
egen employed_head=rowtotal(employ_head employ1_head employ2_head employ3_head)
replace employed_head = 1 if employed_head==2

gen employ_wife=0
replace employ_wife=1 if EMPLOY_STATUS_WIFE_==1
gen employ1_wife=0
replace employ1_wife=1 if EMPLOY_STATUS1_WIFE_==1
gen employ2_wife=0
replace employ2_wife=1 if EMPLOY_STATUS2_WIFE_==1
gen employ3_wife=0
replace employ3_wife=1 if EMPLOY_STATUS3_WIFE_==1
egen employed_wife=rowtotal(employ_wife employ1_wife employ2_wife employ3_wife)
replace employed_wife = 1 if employed_wife==2

browse id survey_yr employed_head employed_wife employ_head employ1_head employ_wife employ1_wife

// problem is this employment is NOW not last year. I want last year? use if wages = employ=yes, then no? (or hours)
gen employed_ly_head=0
replace employed_ly_head=1 if earnings_head > 0 & earnings_head!=.

gen employed_ly_wife=0
replace employed_ly_wife=1 if earnings_wife > 0 & earnings_wife!=.

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

gen race_wife=race_1_wife_rec
replace race_wife=7 if race_2_wife_rec!=.

gen race_head=race_1_head_rec
replace race_head=7 if race_2_head_rec!=.

label define race 1 "White" 2 "Black" 3 "Indian" 4 "Asian" 5 "Latino" 6 "Other" 7 "Multi-racial"
label values race_wife race_head race

// wife - not asked until 1985, need to figure out
	browse id survey_yr race_wife if inlist(id,3,12,16)
	bysort id (race_wife): replace race_wife=race_wife[1] if race_wife==.

// need to figure out ethnicity  okay DUH - see Cooke 2006 - bc only in sample in 1990-1995, otherwise there are actually no Hispanics in sample DUH

gen same_race=0
replace same_race=1 if race_head==race_wife & race_head!=.

// any children - need to get more specific
gen children=0
replace children=1 if NUM_CHILDREN_>=1

bysort unique_id: egen children_ever = max(NUM_CHILDREN_)

sort unique_id survey_yr
browse unique_id survey_yr NUM_CHILDREN_ children_ever


// housework hours - not totally sure if accurate prior to 1976
browse id survey_yr HOUSEWORK_HEAD_ HOUSEWORK_WIFE_ TOTAL_HOUSEWORK_HW_ MOST_HOUSEWORK_

gen housework_head = HOUSEWORK_HEAD_
replace housework_head = (HOUSEWORK_HEAD_/52) if inrange(survey_yr,1968,1974)
gen housework_wife = HOUSEWORK_WIFE_
replace housework_wife = (HOUSEWORK_WIFE_/52) if inrange(survey_yr,1968,1974)

browse id survey_yr housework_head housework_wife TOTAL_HOUSEWORK_HW_ MOST_HOUSEWORK_
gen wife_housework_pct = housework_wife / (housework_wife + housework_head)

gen housework_bkt=.
replace housework_bkt=1 if wife_housework_pct >=.4000 & wife_housework_pct <=.6000
replace housework_bkt=2 if wife_housework_pct >.6000 & wife_housework_pct!=.
replace housework_bkt=3 if wife_housework_pct <.4000
replace housework_bkt=4 if (housework_wife==0 | housework_wife==.) & (housework_head==0 | housework_head==.)

label define housework_bkt 1 "Dual HW" 2 "Female Primary" 3 "Male Primary" 4 "NA"
label values housework_bkt housework_bkt


gen labor_income = 0
replace labor_income = 1 if OFUM_LABOR_INCOME_!=.

//inspect OFUM_LABOR_INCOME_ if RELATION_==10

*********************************************
* New / updated variables to fill in for those in ST cohabitations who don't have data as "wives"
*********************************************
recode YRS_EDUCATION_ (1/11=1) (12=2) (13/15=3) (16/17=4) (98/99=.) (0=.), gen(education)
label define education 1 "LTHS" 2 "HS" 3 "Some College" 4 "College"
label values education education // okay there is still a LOT of missing specifically for the 88s GAH - I am feeling like more and more this is a bad idea, but do two views - 1 all (2005+), then 2 just LT cohabitors (maybe same time, maybe 1990 / 2000+)

gen college=0
replace college=1 if education==4
replace college=. if education==.

recode EMPLOYMENT_ (0=.) (1=1) (2/9=0), gen (employed)

gen num_marriages_wife_all_tmp = NUM_MARRIED_WIFE_
replace num_marriages_wife_all_tmp = NUM_MARRIED if RELATION_==88

gen first_marriage_wife_all_tmp = FIRST_MARRIAGE_YR_WIFE_
replace first_marriage_wife_all_tmp = FIRST_MARRIAGE_YR_START if RELATION_==88

gen last_marriage_wife_all_tmp = LAST_MARRIAGE_YR_WIFE_
replace last_marriage_wife_all_tmp = RECENT_MARRIAGE_YR_START if RELATION_==88

gen num_births_wife_all_tmp = BIRTH_SPOUSE_
replace num_births_wife_all_tmp = NUM_BIRTHS if RELATION_==88

gen age_wife_all_tmp = AGE_SPOUSE_
replace age_wife_all_tmp = AGE_ if RELATION_==88

gen sex_wife_all_tmp = SEX_WIFE_ // for some reason, this variable has a lot missing, so will use for all
replace sex_wife_all_tmp = SEX if inlist(RELATION_,20,22,88)

gen educ_wife_all_tmp = educ_wife
replace educ_wife_all_tmp = education if RELATION_==88
label values educ_wife_all_tmp education

gen employed_wife_all_tmp = employed_wife
replace employed_wife_all_tmp = employed if RELATION_==88

gen earnings_wife_all_tmp = earnings_wife
replace earnings_wife_all_tmp = OFUM_LABOR_INCOME_ if RELATION_==88

gen college_wife_all_tmp = college_complete_wife
replace college_wife_all_tmp = college if RELATION_==88

// how do I get this info on the row of head?? match ids with a couple_status_ref of 4, use id where relation is 88, but then match on family id? or main per id?
sort survey_yr FAMILY_INTERVIEW_NUM_
browse id unique_id survey_yr main_per_id FAMILY_INTERVIEW_NUM_ FAMILY_ID_SO_ RELATION_ educ_wife education educ_wife_all educ_head if COUPLE_STATUS_REF_==4
browse id unique_id survey_yr main_per_id FAMILY_INTERVIEW_NUM_ FAMILY_ID_SO_ RELATION_ AGE_SPOUSE_ age_wife_all AGE_ if COUPLE_STATUS_REF_==4

bysort survey_yr FAMILY_INTERVIEW_NUM_: egen educ_wife_all = max(educ_wife_all_tmp)
browse id unique_id survey_yr main_per_id FAMILY_INTERVIEW_NUM_ FAMILY_ID_SO_ RELATION_ educ_wife education educ_wife_all educ_wife_all_tmp educ_head if COUPLE_STATUS_REF_==4
label values educ_wife_all education

foreach var in num_marriages_wife_all first_marriage_wife_all last_marriage_wife_all num_births_wife_all age_wife_all sex_wife_all employed_wife_all earnings_wife_all college_wife_all {
	bysort survey_yr FAMILY_INTERVIEW_NUM_: egen `var' = max(`var'_tmp)
	drop `var'_tmp
}

egen couple_earnings_all = rowtotal(earnings_head earnings_wife_all) if COUPLE_STATUS_REF_==4
replace couple_earnings_all = couple_earnings if inlist(COUPLE_STATUS_REF_,1,2)

browse earnings_head earnings_wife earnings_wife_all couple_earnings couple_earnings_all COUPLE_STATUS_REF_

*do a bysort then max? (since all at 0, everything will be higher? AND want missing if it is?)

// key independent variable
** need to make sure i make this ACTUALLY female
drop if SEX_HEAD == sex_wife_all // want to be different sex for this purpose

gen earnings_male=.
replace earnings_male=earnings_head if SEX_HEAD_==1
replace earnings_male=earnings_wife if sex_wife_all==1

gen earnings_female=.
replace earnings_female=earnings_head if SEX_HEAD_==2
replace earnings_female=earnings_wife if sex_wife_all==2

// browse earnings_male earnings_female earnings_head earnings_wife couple_earnings_all SEX_HEAD_ sex_wife_all

gen female_earn_pct = earnings_female/(couple_earnings_all)

gen hh_earn_type=.
replace hh_earn_type=1 if female_earn_pct >=.4000 & female_earn_pct <=.6000
replace hh_earn_type=2 if female_earn_pct < .4000 & female_earn_pct >=0
replace hh_earn_type=3 if female_earn_pct > .6000 & female_earn_pct <=1
replace hh_earn_type=4 if earnings_head==0 & earnings_wife_all==0

label define hh_earn_type 1 "Dual Earner" 2 "Male BW" 3 "Female BW" 4 "No Earners"
label values hh_earn_type hh_earn_type

gen hh_earn_type_bkd=.
replace hh_earn_type_bkd=1 if female_earn_pct >=.4000 & female_earn_pct <=.6000
replace hh_earn_type_bkd=2 if female_earn_pct < .4000 & female_earn_pct > 0
replace hh_earn_type_bkd=3 if female_earn_pct ==0
replace hh_earn_type_bkd=4 if female_earn_pct > .6000 & female_earn_pct <=1
replace hh_earn_type_bkd=5 if earnings_head==0 & earnings_wife_all==0

label define earn_type_bkd 1 "Dual Earner" 2 "Male Primary" 3 "Male Sole" 4 "Female BW" 5 "No Earners"
label values hh_earn_type_bkd earn_type_bkd

/*
NUM_MARRIED_WIFE_	NUM_MARRIED
FIRST_MARRIAGE_YR_WIFE_	FIRST_MARRIAGE_YR_START
LAST_MARRIAGE_YR_WIFE_	RECENT_MARRIAGE_YR_START
BIRTH_SPOUSE_	NUM_BIRTHS
AGE_SPOUSE_	AGE_
SEX_WIFE_	SEX
educ_wife	education
employed_wife	employed
earnings_wife	OFUM_LABOR_INCOME_
college_complete_wife	college
*/

*********************************************
* Start / end dates
*********************************************
browse id FAMILY_INTERVIEW_NUM_ main_per_id survey_yr
sort id survey_yr

// do I need to do a lookup --- somewhere? main file? to get first and last year of each individual? (i did that in SIPP i think?)
egen first_couple_yr
egen last_couple_yr

save "$data_keep\PSID_allunions_coupled.dta", replace

/*
*********************************************
* More restrictions
*********************************************
* just cohabitors (or the year they marry)

* one record per household

* 2005+ (when labor income is asked)

// Then to figure out how to keep only one respondent per HH. really doesn't matter gender of who I keep, because all variables are denoted by head / wife, NOT respondent. BUT is it confusing if the head is not male (since most are?) okay why are there so many missing for WIFE VARIABLES?! did I do something wrong? is it to do with short-term cohabitors? also some to do with variable name changes?

bysort survey_yr FAMILY_INTERVIEW_NUM_ : egen per_id = rank(unique_id)
browse survey_yr FAMILY_INTERVIEW_NUM_  unique_id per_id
keep if per_id==1
*/
