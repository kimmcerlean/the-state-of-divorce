********************************************************************************
* Getting PSID sample for union dissolution
* create_comparison_sample.do
* Kim McErlean
********************************************************************************

* Instead of just keeping those with relationship start 20000 - keeping those later, to try to validate other findings (e.g. Killewald, Schwartz and GP). Since just marriage, can also do prior to 1983?

use "$data_tmp\PSID_full_long.dta", clear // created in other step 1

// need to replace all "id" with INTERVIEW_NUM_, okay wait no unique ID
// BUT interview_num_ to match later
********************************************************************************
* First clean up to get a sense of WHO is even eligible
********************************************************************************

browse survey_yr main_per_id id unique_id INTERVIEW_NUM_ SEQ_NUMBER_ RELATION_ FIRST_MARRIAGE_YR_START MARITAL_PAIRS_

// drop if survey_yr <1983 // first time you could identify cohab

gen relationship=0
replace relationship=1 if inrange(MARITAL_PAIRS_,1,4)

browse survey_yr unique_id main_per_id SEQ_NUMBER_ relationship RELATION_ FIRST_MARRIAGE_YR_START 

bysort unique_id (SEQ_NUMBER_): egen in_sample=max(SEQ_NUMBER_)

drop if in_sample==0 // people with NO DATA in any year
drop if SEQ_NUMBER_==0 // won't have data because not in that year -- like SIPP, how do I know if last year is because divorced or last year in sample? right now individual level file, so fine - this is JUST last year in sample at the moment

browse survey_yr INTERVIEW_NUM_ main_per_id relationship RELATION_ FIRST_MARRIAGE_YR_START FIRST_MARRIAGE_YR_HEAD_ FIRST_MARRIAGE_YR_END RECENT_MARRIAGE_YR_START 

browse id survey_yr relationship MARITAL_STATUS_HEAD_
gen relationship_yr = survey_yr if relationship==1
sort id survey_yr
gen enter_rel=0
replace enter_rel=1 if relationship==1 & relationship[_n-1]==0 & unique_id==unique_id[_n-1]
replace enter_rel=1 if relationship_yr==1968 // since can't transition, but call this "relationship 1"

gen exit_rel=0
sort id survey_yr
replace exit_rel=1 if relationship==1 & relationship[_n+1]==0 & unique_id==unique_id[_n+1]
// replace exit_rel=1 if relationship==0 & relationship[_n-1]==1 & id==id[_n-1]

browse unique_id survey_yr relationship enter_rel MARITAL_STATUS_HEAD_ exit_rel

gen relationship_start = survey_yr if enter_rel==1
bysort unique_id: egen marrno=rank(relationship_start)
browse unique_id survey_yr MARITAL_STATUS_HEAD_ enter_rel relationship_start FIRST_MARRIAGE_YR_START marrno

gen rel1_start=.
// replace rel1_start = FIRST_MARRIAGE_YR_START if FIRST_MARRIAGE_YR_START <=2019
replace rel1_start=relationship_start if marrno==1 // & FIRST_MARRIAGE_YR_START==9999
bysort unique_id (rel1_start): replace rel1_start=rel1_start[1]
gen rel2_start=.
replace rel2_start=relationship_start if marrno==2 // & RECENT_MARRIAGE_YR_START ==9999
bysort unique_id (rel2_start): replace rel2_start=rel2_start[1]
gen rel3_start=.
replace rel3_start=relationship_start if marrno==3
bysort unique_id (rel3_start): replace rel3_start=rel3_start[1]
gen rel4_start=.
replace rel4_start=relationship_start if marrno==4
bysort unique_id (rel4_start): replace rel4_start=rel4_start[1]

sort unique_id survey_yr
browse unique_id survey_yr MARITAL_STATUS_HEAD_ enter_rel relationship_start rel1_start FIRST_MARRIAGE_YR_START rel2_start marrno

browse unique_id survey_yr MARITAL_STATUS_HEAD_ enter_rel relationship_start rel1_start FIRST_MARRIAGE_YR_START rel2_start marrno id_spouse1 per_no_spouse1 yr_married1 id_spouse2 per_no_spouse2 yr_married2 in_marital_history MARITAL_PAIRS_


gen relationship_end = survey_yr if exit_rel==1
bysort unique_id: egen exitno=rank(relationship_end)
browse unique_id survey_yr MARITAL_STATUS_HEAD_ enter_rel relationship_start exitno

gen rel1_end=.
replace rel1_end=relationship_end if exitno==1
bysort unique_id (rel1_end): replace rel1_end=rel1_end[1]
gen rel2_end=.
replace rel2_end=relationship_end if exitno==2
bysort unique_id (rel2_end): replace rel2_end=rel2_end[1]
gen rel3_end=.
replace rel3_end=relationship_end if exitno==3
bysort unique_id (rel3_end): replace rel3_end=rel3_end[1]
gen rel4_end=.
replace rel4_end=relationship_end if exitno==4
bysort unique_id (rel4_end): replace rel4_end=rel4_end[1]

browse unique_id survey_yr relationship enter_rel marrno rel1_start rel1_end rel2_start rel2_end
sort unique_id survey_yr
browse unique_id survey_yr rel1_start rel1_end rel2_start rel2_end in_marital_history yr_married1 status1 yr_end1 yr_married2 status2 yr_end2 // figuring out how to combine gah

replace rel1_start=yr_married1 if in_marital_history==1
replace rel2_start=yr_married2 if in_marital_history==1
replace rel3_start=yr_married3 if in_marital_history==1
replace rel4_start=yr_married4 if in_marital_history==1
replace rel1_end=yr_end1 if in_marital_history==1
replace rel2_end=yr_end2 if in_marital_history==1
replace rel3_end=yr_end3 if in_marital_history==1
replace rel4_end=yr_end4 if in_marital_history==1

browse unique_id survey_yr rel1_start rel1_end rel2_start rel2_end in_marital_history yr_married1 status1 yr_end1 yr_married2 status2 yr_end2 // figuring out how to combine gah
replace rel1_end=. if rel1_end==9999
replace rel2_end=. if rel2_end==9999
replace rel3_end=. if rel3_end==9999
replace rel4_end=. if rel4_end==9999

gen rel_start_all=.
replace rel_start_all = rel1_start if survey_yr>=rel1_start & survey_yr <=rel1_end
replace rel_start_all = rel2_start if survey_yr>=rel2_start & survey_yr <=rel2_end
replace rel_start_all = rel3_start if survey_yr>=rel3_start & survey_yr <=rel3_end
replace rel_start_all = rel4_start if survey_yr>=rel4_start & survey_yr <=rel4_end

gen rel_end_all=.
replace rel_end_all = rel1_end if survey_yr>=rel1_start & survey_yr <=rel1_end
replace rel_end_all = rel2_end if survey_yr>=rel2_start & survey_yr <=rel2_end
replace rel_end_all = rel3_end if survey_yr>=rel3_start & survey_yr <=rel3_end
replace rel_end_all = rel4_end if survey_yr>=rel4_end & survey_yr <=rel4_end

gen status_all=.
replace status_all = status1 if survey_yr>=rel1_start & survey_yr <=rel1_end
replace status_all = status2 if survey_yr>=rel2_start & survey_yr <=rel2_end
replace status_all = status3 if survey_yr>=rel3_start & survey_yr <=rel3_end
replace status_all = status4 if survey_yr>=rel4_end & survey_yr <=rel4_end

gen spouse_id_all=.
replace spouse_id_all = id_spouse1 if survey_yr>=rel1_start & survey_yr <=rel1_end
replace spouse_id_all = id_spouse2 if survey_yr>=rel2_start & survey_yr <=rel2_end
replace spouse_id_all = id_spouse3 if survey_yr>=rel3_start & survey_yr <=rel3_end
replace spouse_id_all = id_spouse4 if survey_yr>=rel4_end & survey_yr <=rel4_end

gen spouse_per_num_all=.
replace spouse_per_num_all = per_no_spouse1 if survey_yr>=rel1_start & survey_yr <=rel1_end
replace spouse_per_num_all = per_no_spouse2 if survey_yr>=rel2_start & survey_yr <=rel2_end
replace spouse_per_num_all = per_no_spouse3 if survey_yr>=rel3_start & survey_yr <=rel3_end
replace spouse_per_num_all = per_no_spouse4 if survey_yr>=rel4_end & survey_yr <=rel4_end

label values status_all status

browse id survey_yr relationship marrno  rel_start_all rel_end_all rel1_start rel1_end rel2_start rel2_end spouse_id_all spouse_per_num_all id_spouse1 id_spouse2 per_no_spouse1 per_no_spouse2

gen relationship_order=.

forvalues r=1/4{
	replace relationship_order=`r' if survey_yr>=rel`r'_start & survey_yr <=rel`r'_end
}

browse unique_id survey_yr relationship relationship_order rel_start_all rel_end_all rel1_start rel1_end rel2_start rel2_end

bysort unique_id: egen last_survey_yr = max(survey_yr)

sort unique_id survey_yr
browse unique_id survey_yr relationship rel_start_all rel_end_all exit_rel status1 status2

browse unique_id survey_yr rel_start_all rel_end_all  status_all exit_rel last_survey_yr MARRIAGE_UPDATE MARITAL_STATUS_REF_ MARITAL_STATUS_HEAD_

gen dissolve=0
replace dissolve=1 if survey_yr >=rel_end_all & (inrange(status_all,4,7) & in_marital_history==1)
replace dissolve=1 if exit_rel==1 & inlist(MARITAL_STATUS_HEAD_[_n+1],2,4,5) & unique_id == unique_id[_n+1] & in_marital_history==0
replace dissolve=1 if exit_rel==1 & (inrange(status_all,4,7) & in_marital_history==1)
replace dissolve=1 if exit_rel[_n+1]==1 & dissolve[_n+1]==0 & (inrange(status_all,4,7) & in_marital_history==1) & unique_id==unique_id[_n+1]

browse id survey_yr relationship rel_start_all rel_end_all dissolve exit_rel status_all MARITAL_STATUS_HEAD_ if inlist(id,2009,2986,2992) // so the survey yr GREATER than part isn't working for people who dissolve in an off year - like 2008. so 2007 not getting flagged as end? 
browse id survey_yr relationship rel_start_all rel_end_all dissolve exit_rel status_all MARITAL_STATUS_HEAD_ if id==2009

sort id survey_yr
browse id survey_yr relationship rel_start_all rel_end_all dissolve exit_rel status_all MARITAL_STATUS_HEAD_

********************************************************************************
* Restrict to anyone in a relationship
********************************************************************************
gen total_relationship=relationship
replace total_relationship=1 if dissolve==1
keep if total_relationship==1 
browse id survey_yr relationship rel_start_all rel_end_all dissolve exit_rel status_all MARITAL_STATUS_HEAD_

unique id if inrange(status_all,4,7) // 6060
unique id if dissolve==1 // 7660

// trying to identify if married or cohabiting. .. need relation_?
// see fAQ, some might be labeled bf / gf in first year coresiding (code 88)
// might need to alter code for cohab because wouldn't be in marital history, so I may have over-wrote some cohab above. 
// will this as now for married, but for cohab, go back to original code and then ONLY KEEP cohab - will only be accurate for cohab and this file only for marriages? but here at least need to remove some marriages?
// egen year_family=concat(survey_yr FAMILY_INTERVIEW_NUM_), punct(_)
keep if inlist(RELATION_,1,2,10,20,22)

bysort survey_yr FAMILY_INTERVIEW_NUM_ (RELATION_): egen either_cohab=max(RELATION_)
sort survey_yr FAMILY_INTERVIEW_NUM_ id

// drop if NUM_MARRIED==98

browse relationship dissolve MARITAL_STATUS_REF_ MARITAL_STATUS_HEAD_ COUPLE_STATUS_REF_ // marital status_head_ = MARRIED OR COHAB IWTH NO DISTINGUISH, marital_status_ref = official - so if DIVROCED, put as cohab?

browse id survey_yr relationship dissolve MARITAL_STATUS_REF_

gen relationship_type=0
replace relationship_type=1 if NUM_MARRIED_2019==0
replace relationship_type=2 if NUM_MARRIED_2019>=1
replace relationship_type=1 if either_cohab==22
replace relationship_type=1 if inrange(MARITAL_STATUS_REF_,2,9)
sort id survey_yr

label define relationship_type 1 "Cohab" 2 "Married"
label values relationship_type relationship_type

tab MARITAL_STATUS_REF_ relationship_type
tab relationship_type RELATION_
tab relationship_type in_marital_history // should they automatically be married if in here since that is the dates I used?? confused...

replace relationship_type=2 if relationship==0 & dissolve==1 & relationship_type[_n-1]==2 & unique_id==unique_id[_n-1]
tab relationship_type in_marital_history // should they automatically be married if in here since that is the dates I used?? confused...
tab relationship_type relationship // should they automatically be married if in here since that is the dates I used?? confused...
drop if survey_yr > rel_end_all & rel_end_all!=.

browse id survey_yr relationship relationship_type rel_start_all rel_end_all status_all dissolve exit_rel  MARITAL_STATUS_HEAD_
browse id survey_yr relationship relationship_type rel_start_all rel_end_all status_all dissolve exit_rel  MARITAL_STATUS_HEAD_ if in_marital_history==1

// drop if relationship_type==1
tab RELATION_ // okay pretty equal numbers.

sort id survey_yr
gen dur = survey_yr - rel_start_all
browse id survey_yr relationship_type rel_start_all rel_end_all dissolve dur
browse id survey_yr relationship_type rel_start_all rel_end_all dissolve dur if FAMILY_INTERVIEW_NUM_ == 7439

gen reltype1 = relationship_type if survey_yr>=rel1_start & survey_yr <=rel1_end
bysort unique_id (reltype1): replace reltype1=reltype1[1]
gen reltype2 = relationship_type if survey_yr>=rel2_start & survey_yr <=rel2_end
bysort unique_id (reltype2): replace reltype2=reltype2[1]
gen reltype3 = relationship_type if survey_yr>=rel3_start & survey_yr <=rel3_end
bysort unique_id (reltype3): replace reltype3=reltype3[1]
gen reltype4 = relationship_type if survey_yr>=rel4_start & survey_yr <=rel4_end
bysort unique_id (reltype4): replace reltype4=reltype4[1]
label values reltype* relationship_type

sort unique_id survey_yr
browse unique_id survey_yr relationship_type rel_start_all rel_end_all reltype*

egen ct_unions=rownonmiss(reltype1 reltype2 reltype3 reltype4)
egen ct_marriages=anycount(reltype1 reltype2 reltype3 reltype4), values(2)
egen ct_cohab=anycount(reltype1 reltype2 reltype3 reltype4), values(1)

browse id survey_yr relationship_type reltype* ct_unions ct_marriages ct_cohab

gen marriage_order=.

forvalues r=1/4{
	replace marriage_order=`r' if survey_yr>=rel`r'_start & survey_yr <=rel`r'_end & relationship_type==2 // gah it's labelling as 2 if two in order, not one.
}

sort unique_id survey_yr
gen marriage_order_real = marriage_order
replace marriage_order_real = ct_marriages if marriage_order > ct_marriages & marriage_order!=. & ct_marriages!=. // think this isn't perfect if three relationships, but sufficient for now

browse unique_id survey_yr relationship_type relationship_order marriage_order marriage_order_real rel_start_all rel_end_all ct_unions ct_marriages ct_cohab 

save "$data_tmp\PSID_all_unions.dta", replace

********************************************************************************
**# Recodes
********************************************************************************
// first need to figure out how to keep only one respondent per HH. really doesn't matter gender of who I keep, because all variables are denoted by head / wife, NOT respondent.
bysort survey_yr FAMILY_INTERVIEW_NUM_ : egen per_id = rank(unique_id)
browse survey_yr FAMILY_INTERVIEW_NUM_  unique_id per_id

browse survey_yr FAMILY_INTERVIEW_NUM_ per_id unique_id if inlist(unique_id,12,13)

keep if per_id==1

unique id, by(rel_start_all) // can I get this to match S&H?
unique id if dissolve==1, by(rel_start_all)
 
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
	
gen female_earn_pct = earnings_wife/(couple_earnings)

gen hh_earn_type=.
replace hh_earn_type=1 if female_earn_pct >=.4000 & female_earn_pct <=.6000
replace hh_earn_type=2 if female_earn_pct < .4000 & female_earn_pct >=0
replace hh_earn_type=3 if female_earn_pct > .6000 & female_earn_pct <=1
replace hh_earn_type=4 if earnings_head==0 & earnings_wife==0

label define hh_earn_type 1 "Dual Earner" 2 "Male BW" 3 "Female BW" 4 "No Earners"
label values hh_earn_type hh_earn_type

gen hh_earn_type_bkd=.
replace hh_earn_type_bkd=1 if female_earn_pct >=.4000 & female_earn_pct <=.6000
replace hh_earn_type_bkd=2 if female_earn_pct < .4000 & female_earn_pct > 0
replace hh_earn_type_bkd=3 if female_earn_pct ==0
replace hh_earn_type_bkd=4 if female_earn_pct > .6000 & female_earn_pct <=1
replace hh_earn_type_bkd=5 if earnings_head==0 & earnings_wife==0

label define earn_type_bkd 1 "Dual Earner" 2 "Male Primary" 3 "Male Sole" 4 "Female BW" 5 "No Earners"
label values hh_earn_type_bkd earn_type_bkd

sort id survey_yr
gen hh_earn_type_bkd_lag=.
replace hh_earn_type_bkd_lag=hh_earn_type_bkd[_n-1] if unique_id==unique_id[_n-1]
label values hh_earn_type_bkd_lag earn_type_bkd

gen hh_earn_type_lag=.
replace hh_earn_type_lag=hh_earn_type[_n-1] if unique_id==unique_id[_n-1]
label values hh_earn_type_lag hh_earn_type

gen female_earn_pct_lag=.
replace female_earn_pct_lag=female_earn_pct[_n-1] if unique_id==unique_id[_n-1]

browse id survey_yr earnings_head earnings_wife hh_earn_type_bkd hh_earn_type_lag

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
replace hh_earnings_3070_lag=hh_earnings_3070[_n-1] if unique_id==unique_id[_n-1]
label values hh_earnings_3070_lag hh_earnings_3070

browse id survey_yr WAGES_HEAD_ WAGES_WIFE_ hh_earnings_3070 hh_earnings_3070_lag

// hours instead of earnings	
browse id survey_yr WEEKLY_HRS1_WIFE_ WEEKLY_HRS_WIFE_ WEEKLY_HRS1_HEAD_ WEEKLY_HRS_HEAD_

gen weekly_hrs_wife = .
replace weekly_hrs_wife = WEEKLY_HRS1_WIFE_ if survey_yr > 1969 & survey_yr <1994
replace weekly_hrs_wife = WEEKLY_HRS_WIFE_ if survey_yr >=1994
replace weekly_hrs_wife = 0 if inrange(survey_yr,1968,1969) & inlist(WEEKLY_HRS1_WIFE_,9,0)
replace weekly_hrs_wife = 10 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_WIFE_ ==1
replace weekly_hrs_wife = 27 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_WIFE_ ==2
replace weekly_hrs_wife = 35 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_WIFE_ ==3
replace weekly_hrs_wife = 40 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_WIFE_ ==4
replace weekly_hrs_wife = 45 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_WIFE_ ==5
replace weekly_hrs_wife = 48 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_WIFE_ ==6
replace weekly_hrs_wife = 55 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_WIFE_ ==7
replace weekly_hrs_wife = 60 if inrange(survey_yr,1968,1969)  & WEEKLY_HRS1_WIFE_ ==8

gen weekly_hrs_head = .
replace weekly_hrs_head = WEEKLY_HRS1_HEAD_ if survey_yr > 1969 & survey_yr <1994
replace weekly_hrs_head = WEEKLY_HRS_HEAD_ if survey_yr >=1994
replace weekly_hrs_head = 0 if inrange(survey_yr,1968,1969) & inlist(WEEKLY_HRS1_HEAD_,9,0)
replace weekly_hrs_head = 10 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_HEAD_ ==1
replace weekly_hrs_head = 27 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_HEAD_ ==2
replace weekly_hrs_head = 35 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_HEAD_ ==3
replace weekly_hrs_head = 40 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_HEAD_ ==4
replace weekly_hrs_head = 45 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_HEAD_ ==5
replace weekly_hrs_head = 48 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_HEAD_ ==6
replace weekly_hrs_head = 55 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_HEAD_ ==7
replace weekly_hrs_head = 60 if inrange(survey_yr,1968,1969)  & WEEKLY_HRS1_HEAD_ ==8

gen female_hours_pct = weekly_hrs_wife/(weekly_hrs_wife + weekly_hrs_head)

gen hh_hours_type=.
replace hh_hours_type=1 if female_hours_pct >=.4000 & female_hours_pct <=.6000
replace hh_hours_type=2 if female_hours_pct <.4000
replace hh_hours_type=3 if female_hours_pct >.6000 & female_hours_pct!=.
replace hh_hours_type=4 if (WEEKLY_HRS_HEAD_==0 & WEEKLY_HRS_WIFE_==0) | female_hours_pct==.

label define hh_hours_type 1 "Dual Earner" 2 "Male BW" 3 "Female BW" 4 "No Earners"
label values hh_hours_type hh_hours_type

gen hh_hours_3070=.
replace hh_hours_3070=1 if female_hours_pct >=.3000 & female_hours_pct <=.7000
replace hh_hours_3070=2 if female_hours_pct <.3000
replace hh_hours_3070=3 if female_hours_pct >.7000
replace hh_hours_3070=4 if (WEEKLY_HRS_HEAD_==0 & WEEKLY_HRS_WIFE_==0) | female_hours_pct==.

label define hh_hours_3070 1 "Dual Earner" 2 "Male BW" 3 "Female BW" 4 "No Earners"
label values hh_hours_3070 hh_hours_3070

sort unique_id survey_yr
gen hh_hours_3070_lag=.
replace hh_hours_3070_lag=hh_hours_3070[_n-1] if unique_id==unique_id[_n-1]
label values hh_hours_3070_lag hh_hours_3070

gen hh_hours_type_lag=.
replace hh_hours_type_lag=hh_hours_type[_n-1] if unique_id==unique_id[_n-1]
label values hh_hours_type_lag hh_hours_type

gen female_hours_pct_lag=.
replace female_hours_pct_lag=female_hours_pct[_n-1] if unique_id==unique_id[_n-1]

browse id survey_yr WEEKLY_HRS_HEAD_ WEEKLY_HRS_WIFE_ female_hours_pct female_hours_pct_lag hh_hours_3070 hh_hours_3070_lag
	
// restrict to working age (18-55) - at time of marriage or all? check what others do - Killewald said ages 18-55 - others have different restrictions, table this part for now
/*
browse id survey_yr AGE_ AGE_REF_ AGE_SPOUSE_ RELATION_
keep if (AGE_REF_>=18 & AGE_REF_<=55) &  (AGE_SPOUSE_>=18 & AGE_SPOUSE_<=55)
*/

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

gen employ_wife=0
replace employ_wife=1 if EMPLOY_STATUS_WIFE_==1
gen employ1_wife=0
replace employ1_wife=1 if EMPLOY_STATUS1_WIFE_==1
gen employ2_wife=0
replace employ2_wife=1 if EMPLOY_STATUS2_WIFE_==1
gen employ3_wife=0
replace employ3_wife=1 if EMPLOY_STATUS3_WIFE_==1
egen employed_wife=rowtotal(employ_wife employ1_wife employ2_wife employ3_wife)

browse id survey_yr employed_head employed_wife employ_head employ1_head employ_wife employ1_wife

// problem is this employment is NOW not last year. I want last year? use if wages = employ=yes, then no? (or hours)
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

browse survey_yr unique_id NUM_CHILDREN_ NUM_BIRTHS FIRST_BIRTH_YR BIRTHS_REF_ BIRTH_SPOUSE_ BIRTHS_BOTH_
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
browse unique_id survey_yr when_first_birth rel_start_all

gen pre_marital_birth=0
replace pre_marital_birth=1 if when_first_birth < rel_start_all & when_first_birth!=.

gen post_marital_birth=0
replace post_marital_birth=1 if when_first_birth >= rel_start_all & when_first_birth<=rel_end_all & when_first_birth!=. // needs to be IN marriage years, okay barely changed it

// urbanicity
gen metro=(METRO_==1) // a lot of missing, don't use for now, control for STATE_ for now

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

sort id survey_yr
gen housework_bkt_lag=.
replace housework_bkt_lag=housework_bkt[_n-1] if unique_id==unique_id[_n-1]
label values housework_bkt_lag housework_bkt

gen wife_hw_pct_lag=.
replace wife_hw_pct_lag=wife_housework_pct[_n-1] if unique_id==unique_id[_n-1]

// combined indicator of paid + unpaid hours
gen division_labor=.
replace division_labor=1 if hh_hours_3070==1 & housework_bkt==1 // both 30-70%
replace division_labor=2 if hh_hours_3070==1 & housework_bkt==2 // dual employment; female homemaker
replace division_labor=3 if hh_hours_3070==2 & wife_housework_pct <.7000 // Male BW earner; female housework < 70
replace division_labor=4 if hh_hours_3070==2 & housework_bkt==2 // Male 70%+ hours; female 70%+ housework
replace division_labor=5 if hh_hours_3070==3 & housework_bkt==2 // female 70%+ hours + housework
replace division_labor=6 if (hh_hours_3070==3 & wife_housework_pct <.7000) | (hh_hours_3070==1 & housework_bkt==3) // (Female 70%+ hours & 0-70% HW) OR (Women economic 30-70%; HW 0-30%)
replace division_labor=7 if hh_hours_3070==4

label define division 1 "Dual" 2 "Dual - Female HW" 3 "Gender-specialized" 4 "Male BW" 5 "Female All" 6 "Counter-traditional" 7 "No Earners"
label values division_labor division

sort id survey_yr
gen division_labor_lag=.
replace division_labor_lag=division_labor[_n-1] if unique_id==unique_id[_n-1]
label values division_labor_lag division

// turning the bucket interactions into variables to interact over time
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

// browse id survey_yr hh_hours_3070 housework_bkt wife_housework_pct division_labor if division_labor==.
// most of the missing are the years they didn't ask housework so need to figure that out. leave for now? 1968, 1975, 1982 have nothing

/* religion is new, but think I need to add given historical research. coding changes between 1984 and 1985, then again between 1994 and 1995 Need to recode, so tabling for now, not important for these purposes anyway
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
*/

// figuring out costs
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

// also age at relationship start
browse id survey_yr rel_start_all FIRST_MARRIAGE_YR_START BIRTH_YR_ AGE_REF_ AGE_SPOUSE_
sort id survey_yr
gen age_spouse=AGE_SPOUSE_
replace age_spouse=AGE_SPOUSE_[_n-1]+1 if MARITAL_PAIRS_==0 & unique_id==unique_id[_n-1]

//browse id survey_yr rel_start_all FIRST_MARRIAGE_YR_START BIRTH_YR_ AGE_REF_ AGE_SPOUSE_ age_spouse if MARITAL_PAIRS_==0 // some not updating- do they not have prior records? - are they all relationships that started PRIOR to 1968?
tab rel_start_all if age_spouse==0
browse id survey_yr rel_start_all rel_end_all FIRST_MARRIAGE_YR_START BIRTH_YR_ AGE_REF_ AGE_SPOUSE_ age_spouse if id == 32701

// for now - setting to 2 years younger than partner.
replace age_spouse = AGE_REF_ - 2 if age_spouse==0 // i am creating year born - but need to figure out if that is also provided. but I guess same challenge - if she isn't in HH and there is only one record, I won't know??

gen yr_born_head = survey_yr - AGE_REF_
gen yr_born_wife = survey_yr-age_spouse

gen age_mar_head = rel_start_all -  yr_born_head
gen age_mar_wife = rel_start_all -  yr_born_wife

drop if age_mar_head < 0 | age_mar_wife < 0

browse id survey_yr yr_born_head yr_born_wife rel_start_all age_mar_head age_mar_wife AGE_REF_ AGE_SPOUSE_
browse id survey_yr yr_born_head yr_born_wife rel_start_all age_mar_head age_mar_wife AGE_REF_ AGE_SPOUSE_ if dissolve==1
browse id survey_yr yr_born_head yr_born_wife rel_start_all rel_end_all age_mar_head age_mar_wife AGE_REF_ AGE_SPOUSE_ MARITAL_PAIRS_ if id==84

browse id survey_yr rel_start_all rel_end_all status_all dissolve MARITAL_PAIRS_
browse id survey_yr rel_end_all dissolve in_marital_history dur MARITAL_PAIRS_ if dissolve==1
tab in_marital_history , m
tab in_marital_history if dissolve==1 & MARITAL_PAIRS_==0,m // is in marital history over or under reprsented - like is that more or less contribuitng to this problem? okay yes, so ALL in marital history. does it depend like month of survey v. month of dissolve? because marital history updated retrospetively so like maybe when answered 2005 survey, they were together and living together in month 8, but then then divorce month 10 - marital history will update, but there will still be interview data for wife because she was there at time of survey. so it's probably people who divorced later v. earler in year - so I want last full year of data I can get, whenever that is? technically I do have months in marital history... but it really doesn't matter like I don't get that info either way...
 // do I have to lag alll info though? or like JUST update year of dissolution? replace year of dissolution with all prior values of wife variables?
 
// merge cohabitation history for head
// merge 1:1 survey_yr main_per_id INTERVIEW_NUM_ using "$data_tmp\PSID_partner_history.dta", keepusing(MX8* partner_1968_id* partner_per_num*) // lol now only 724 matched
// lol okay match rate SUPER low here when WIDE- 1000
merge m:1 unique_id using "$data_tmp\PSID_partner_history.dta", keepusing(MX8* partner_1968_id* partner_per_num*) // still only 48000 which seems low (when wide). okay try LONG -- okay this was my best bet... I AM DUMB - it shouldn;t all match because it is only people who ever had A COHABITING partner, not others.

drop if _merge==2 // people not in my sample
gen ever_cohab_head = 1 if _merge==3
replace ever_cohab_head=0 if ever_cohab_head==.
drop _merge

foreach var in MX8* partner_1968_id* partner_per_num*{
	rename `var' `var'_head
}

// k now trying to match on PARTNER id to get HER history
merge m:1 spouse_per_num_all spouse_id_all using "$data_tmp\PSID_partner_history.dta", keepusing(MX8* partner_1968_id* partner_per_num*) // less matches but I am not surprised about this (i don't think??

drop if _merge==2 // people not in my sample
gen ever_cohab_wife = 1 if _merge==3
replace ever_cohab_wife=0 if ever_cohab_wife==.
drop _merge

foreach var in MX8* partner_1968_id* partner_per_num*{
	rename `var' `var'_wife
}

rename partner_1968_id*_head_wife partner_1968_id*_head
rename partner_per_num*_head_wife partner_per_num*_head

save "$data_keep\PSID_union_validation_sample.dta", replace

**# Pivot to marriage only file

keep if relationship_type==2

// okay create cohabitation variables
sort unique_id survey_yr
browse unique_id survey_yr ever_cohab_head spouse_per_num_all spouse_id_all rel_start_all rel_end_all partner_1968_id*_head partner_per_num*_head

forvalues y=1968/1997{
	gen cohab_`y'_with_wife=0
	replace cohab_`y'_with_wife=1 if `y' <= rel_start_all & partner_1968_id`y'_head==spouse_id_all & partner_per_num`y'_head==spouse_per_num_all
}

forvalues y=1999(2)2019{
	gen cohab_`y'_with_wife=0
	replace cohab_`y'_with_wife=1 if `y' <= rel_start_all & partner_1968_id`y'_head==spouse_id_all & partner_per_num`y'_head==spouse_per_num_all
}

forvalues y=1968/1997{
	gen cohab_`y'_other=0
	replace cohab_`y'_other=1 if `y' <= rel_start_all & ((partner_1968_id`y'_head!=spouse_id_all & spouse_id_all!=. & partner_1968_id`y'_head!=.) | (partner_per_num`y'_head!=spouse_per_num_all & spouse_per_num_all!=. & partner_per_num`y'_head!=.))
	}

forvalues y=1999(2)2019{
	gen cohab_`y'_other=0
	replace cohab_`y'_other=1 if `y' <= rel_start_all & ((partner_1968_id`y'_head!=spouse_id_all & spouse_id_all!=. & partner_1968_id`y'_head!=.) | (partner_per_num`y'_head!=spouse_per_num_all & spouse_per_num_all!=. & partner_per_num`y'_head!=.))
}


forvalues y=1968/1997{
	gen cohab_`y'_after=0
	replace cohab_`y'_after=1 if `y' >= rel_end_all & partner_1968_id`y'_head!=. & partner_per_num`y'_head!=.
	}

forvalues y=1999(2)2019{
	gen cohab_`y'_after=0
	replace cohab_`y'_after=1 if `y' >= rel_end_all & partner_1968_id`y'_head!=. & partner_per_num`y'_head!=.
}

browse unique_id survey_yr ever_cohab_head spouse_per_num_all spouse_id_all rel_start_all rel_end_all cohab_*_with_wife cohab_*_other partner_1968_id*_head partner_per_num*_head

egen cohab_with_wife = rowtotal(cohab_*_with_wife)
replace cohab_with_wife = 1 if cohab_with_wife > 1 & cohab_with_wife!=.

egen cohab_with_other = rowtotal(cohab_*_other)
replace cohab_with_other = 1 if cohab_with_other > 1 & cohab_with_other!=.

egen cohab_after = rowtotal(cohab_*_after)
replace cohab_after = 1 if cohab_after > 1 & cohab_after!=.

browse unique_id ever_cohab_head cohab_with_other cohab_with_wife // about 14000 of ever cohab not accounted for - only 44 other, so put as other? OR are they missing marital history and I need to figure out?

browse unique_id survey_yr ever_cohab_head spouse_per_num_all spouse_id_all rel_start_all rel_end_all partner_1968_id*_head partner_per_num*_head if ever_cohab_head==1 & cohab_with_other==0 & cohab_with_wife==0 & cohab_after==0

tab rel_start_all if ever_cohab_head==1 & cohab_with_other==0 & cohab_with_wife==0 & cohab_after==0 // okay 90%+ are relationships started in 1968, which I exclude anyway because I don't think all of these are accurate

save "$data_keep\PSID_marriage_validation_sample.dta", replace

tab MARITAL_PAIRS_ if dissolve==1 // 35% have no spouse in year of dissolution - so all of my partner variables are moot.
browse id survey_yr earnings_wife earnings_head female_earn_pct female_earn_pct_lag ft_head ft_wife educ_wife educ_head educ_type age_mar_wife MARITAL_PAIRS_ if dissolve==1 & MARITAL_PAIRS_==0
browse id survey_yr rel_start_all rel_end_all dissolve earnings_wife earnings_head female_earn_pct female_earn_pct_lag ft_head ft_wife educ_wife educ_head educ_type age_mar_wife MARITAL_PAIRS_
tab hh_earn_type_bkd if dissolve==1 & MARITAL_PAIRS_==0
tab hh_earn_type_lag if dissolve==1 & MARITAL_PAIRS_==0 // much more distributed. but do lag for ALL or just if marital_pairs = 0 when dissolve ==1?

// one problem - any wife MISSING on educ is automatically HYPO - that is a small percentage, but still.
// I think earnings missing in year when marital pairs are 0, I think educ for wife is filled in, because educ wife not asked every year, and I manually filled in -which Ithink is also why "first educ" type isn't different - because this isn't 100% time varying.

/*
sort id survey_yr
foreach var in SEX_WIFE_ HRLY_RATE_WIFE_ ENROLLED_WIFE_ RELIGION_WIFE_ WEEKLY_HRS_WIFE_ TAXABLE_HEAD_WIFE_ WAGE_RATE_WIFE_ educ_wife college_complete_wife earnings_wife employ_wife employ1_wife employ2_wife employ3_wife employed_wife employed_ly_wife ft_pt_wife_pre ft_pt_wife_post ft_pt_wife ft_wife race_1_wife_rec race_2_wife_rec race_3_wife_rec race_wife yr_born_wife age_mar_wife female_earn_pct educ_type couple_educ_gp couple_earnings hh_earn_type_bkd either_enrolled same_race{
	replace `var'=`var'[_n-1] if MARITAL_PAIRS_==0 & id==id[_n-1] // & (`var'==0 | `var'==.) // deleted this because want to OVERWWRITE whatever is there for some created variables, so might not be 0
}

// some created variables - like couple_earnings, should I take from year prior? or recreate? I guess I want in  year prior if that's last full year for both - don't want to use like husband info from one year and wife from another....
// this is also where wide would help - can just take year prior?

// WAIT - do I also need to update the same husband variables?! because want the husband and wife info to come from same year, right? so right now, husband info will come from year wife isn't there an dwife will come frm year prior. okay - actually move UP the dissolve to the prior row? so it's there the last year they are both living together? try this then need to remove this extraneous row
*/

// id 749 as example.
sort id survey_yr
gen dissolve_lag = dissolve
replace dissolve_lag = 1 if dissolve==0 & dissolve[_n+1]==1 & id == id[_n+1] & MARITAL_PAIRS_[_n+1]==0
bysort id marriage_order_real: egen ever_dissolve=max(dissolve_lag)
sort id survey_yr
tab status_all ever_dissolve

// end dates STILL seem wrong
browse id survey_yr rel_start_all rel_end_all exit_rel status_all ever_dissolve dissolve_lag dissolve dur MARITAL_PAIRS_ // if inlist(id, 2009,2986, 2992)


tab dissolve // 6391 -- okay now I think there are too many here OMG
tab dissolve_lag // 6425
// was going to do drop if dissolve==1 & dissolve_lag==1 but for 754 - that is valid, but NOT valid for 749
drop if dissolve==1 & dissolve_lag==1 & MARITAL_PAIRS_==0 & (rel_start_all==rel_start_all[_n-1])  // k do has to be part of same relationship. i wonder if also my code update above fixed this...
// see 1121, 20961 as example of it working - okay yes it did.
tab dissolve_lag // 5423 // is this sketchy it got a lot lower??

// eventually also drop people where only one row and NO wife info - aka marital_pairs==0 - because can't include.
tab dur if MARITAL_PAIRS_ ==0
sort id survey_yr
browse id survey_yr rel_start_all rel_end_all dur earnings_wife MARITAL_PAIRS_ dissolve dissolve_lag if id==1877
bysort id: egen num_years = count(survey_yr)
sort id survey_yr
tab num_years if MARITAL_PAIRS_ ==0 // k mostly 1
browse id survey_yr rel_start_all rel_end_all dur num_years if dissolve==1 & MARITAL_PAIRS_==0
browse id survey_yr rel_start_all rel_end_all dur num_years if dur==.
browse id survey_yr num_years rel_start_all rel_end_all dur earnings_wife MARITAL_PAIRS_ dissolve dissolve_lag if id==87
browse id survey_yr num_years rel_start_all rel_end_all dur earnings_wife MARITAL_PAIRS_ dissolve dissolve_lag if id==376
browse id survey_yr num_years rel_start_all rel_end_all dur earnings_wife MARITAL_PAIRS_ dissolve dissolve_lag if id==5614
// okay some people also missing relationship start and end info for years here - so want to drop, but think this is going to ruin my dissolve_lag (as in the example above)

drop if dur==0 | dur==.
drop if num_years==1
drop if MARITAL_PAIRS_==0
drop if SEX_HEAD_==2
save "$data_keep\PSID_marriage_recoded_sample.dta", replace