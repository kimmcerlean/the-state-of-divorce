********************************************************************************
* Getting PSID sample for union dissolution
* create_sample.do
* Kim McErlean
********************************************************************************


********************************************************************************
* First reshape the data to be long, I think this will be less overwhelming / 
* easier to see which variables tracked consistently and how
********************************************************************************

use "$PSID\PSID_full_renamed.dta", clear
rename X1968_PERSON_NUM_1968 main_per_id

// merge on marital history
merge 1:1 main_per_id INTERVIEW_NUM_1968 using "$data_keep\marital_history_wide.dta" // is this going to change now that I have updated? I think I had both of the variables but they were in wrong columns, so I still have main_per_id and interview number, they are just different? OR do I need to rename in marital history file??? oh no...

gen in_marital_history=0
replace in_marital_history=1 if _merge==3
drop _merge

/* this isn't helping
// trying to merge cohab history sooner?
merge 1:1 main_per_id INTERVIEW_NUM_1968 using "$data_tmp\PSID_partner_history.dta" // when I merge this on here, 16000 linked
*/

// i have no clue what this is but it is wrong
egen family_intvw_num=rowmin(FAMILY_INTERVIEW_NUM*) // not working because inconsistent years
browse family_intvw_num FAMILY_INTERVIEW_NUM*

gen unique_id = (main_per_id*1000) + INTERVIEW_NUM_1968
browse main_per_id INTERVIEW_NUM_1968 unique_id

gen id=_n

local reshape_vars "RELEASE_ INTERVIEW_NUM_ X1968_PERSON_NUM_ RELATION_ AGE_ MARITAL_PAIRS_ MOVED_ YRS_EDUCATION_ TYPE_OF_INCOME_ TOTAL_MONEY_INCOME_ ANNUAL_WORK_HRS_ RELEASE_NUM2_ FAMILY_COMPOSITION_ AGE_REF_ AGE_SPOUSE_ SEX_HEAD_ AGE_YOUNG_CHILD_ RESPONDENT_WHO_ RACE_1_HEAD_ EMPLOY_STATUS_HEAD_ MARITAL_STATUS_HEAD_ WIDOW_LENGTH_HEAD_ WAGES_HEAD_ FAMILY_INTERVIEW_NUM_ FATHER_EDUC_HEAD_ WAGE_RATE_HEAD_ WAGE_RATE_WIFE_ REGION_ NUM_CHILDREN_ CORE_WEIGHT_ TOTAL_HOURS_HEAD_ TOTAL_HOURS_WIFE_ LABOR_INCOME_HEAD_ LABOR_INCOME_WIFE_ TOTAL_FAMILY_INCOME_ SEQ_NUMBER_ RESPONDENT_ FAMILY_ID_SO_ COMPOSITION_CHANGE_ NEW_REF_ HRLY_RATE_HEAD_ RELIGION_HEAD_ NEW_SPOUSE_ FATHER_EDUC_WIFE_ MOTHER_EDUC_WIFE_ MOTHER_EDUC_HEAD_ TYPE_TAXABLE_INCOME_ OFUM_TAXABLE_INCOME_ COLLEGE_HEAD_ COLLEGE_WIFE_ SALARY_TYPE_HEAD_ FIRST_MARRIAGE_YR_WIFE_ RELIGION_WIFE_ WORK_MONEY_WIFE_ EMPLOY_STATUS_WIFE_ SALARY_TYPE_WIFE_ HRLY_RATE_WIFE_ RESEPONDENT_WIFE_ WORK_MONEY_HEAD_ MARITAL_STATUS_REF_ EVER_MARRIED_HEAD_ EMPLOYMENT_ STUDENT_ BIRTH_YR_ COUPLE_STATUS_REF_ RELATION_TO_HEAD_ NUM_MARRIED_HEAD_ FIRST_MARRIAGE_YR_HEAD_ FIRST_MARRIAGE_END_HEAD_ FIRST_WIDOW_YR_HEAD_ FIRST_DIVORCE_YR_HEAD_ FIRST_SEPARATED_YR_HEAD_ LAST_MARRIAGE_YR_HEAD_ LAST_WIDOW_YR_HEAD_ LAST_DIVORCE_YR_HEAD_ LAST_SEPARATED_YR_HEAD_ FAMILY_STRUCTURE_HEAD_ RACE_2_HEAD_ NUM_MARRIED_WIFE_ FIRST_MARRIAGE_END_WIFE_ FIRST_WIDOW_YR_WIFE_ FIRST_DIVORCE_YR_WIFE_ FIRST_SEPARATED_YR_WIFE_ LAST_MARRIAGE_YR_WIFE_ LAST_WIDOW_YR_WIFE_ LAST_DIVORCE_YR_WIFE_ LAST_SEPARATED_YR_WIFE_ FAMILY_STRUCTURE_WIFE_ RACE_1_WIFE_ RACE_2_WIFE_ STATE_ BIRTHS_REF_ BIRTH_SPOUSE_ BIRTHS_BOTH_ OFUM_LABOR_INCOME_ RELEASE_NUM_ SALARY_HEAD_ SALARY_WIFE_ EMPLOY_STATUS1_HEAD_ EMPLOY_STATUS2_HEAD_ EMPLOY_STATUS3_HEAD_ EMPLOY_STATUS1_WIFE_ EMPLOY_STATUS2_WIFE_ EMPLOY_STATUS3_WIFE_ RACE_3_WIFE_ RACE_3_HEAD_ RACE_4_HEAD_ COR_IMM_WT_ ETHNIC_WIFE_ ETHNIC_HEAD_ CROSS_SECTION_FAM_WT_ LONG_WT_ CROSS_SECTION_WT_ EARNINGS_2YRLAG_ AMOUNTEARN_1_HEAD_ TOTAL_WEEKS_HEAD_ TOTAL_HOURS2_HEAD_ AMOUNTEARN_1_WIFE_ TOTAL_WEEK_WIFE_ TOTAL_HOURS2_WIFE_ HOURS_WK_HEAD_ NUM_JOBS_ BACHELOR_YR_ ENROLLED_ COLLEGE_ SEX_WIFE_ BACHELOR_YR_WIFE_ ENROLLED_WIFE_ BACHELOR_YR_HEAD_ ENROLLED_HEAD_ WAGES1_WIFE_ METRO_ CURRENTLY_WORK_HEAD_ CURRENTLY_WORK_WIFE_ AMOUNTEARN_2_HEAD_ AMOUNTEARN_3_HEAD_ AMOUNTEARN_4_HEAD_ AMOUNTEARN_2_WIFE_ AMOUNTEARN_3_WIFE_ AMOUNTEARN_4_WIFE_ TAXABLE_HEAD_WIFE_ WAGES_WIFE_ WAGES_HEAD_PRE_ WAGES_WIFE_PRE_ EDUC1_HEAD_ EDUC_HEAD_ EDUC1_WIFE_ EDUC_WIFE_ WEEKLY_HRS_HEAD_ WEEKLY_HRS_WIFE_ WEEKLY_HRS1_HEAD_ WEEKLY_HRS1_WIFE_ TOTAL_HOUSEWORK_HW_ HOUSEWORK_WIFE_ HOUSEWORK_HEAD_ MOST_HOUSEWORK_ AGE_OLDEST_CHILD_"

reshape long `reshape_vars', i(id unique_id) j(survey_yr)

save "$data_tmp\PSID_full_long.dta", replace

browse id survey_yr yr_married1 status1 yr_end1 yr_married2 status2 yr_end2 FIRST_MARRIAGE_YR_START in_marital_history // ensuring marital history makes sense

********************************************************************************
* First clean up to get a sense of WHO is even eligible
********************************************************************************

browse survey_yr id main_per_id SEQ_NUMBER_ RELATION_ FIRST_MARRIAGE_YR_START MARITAL_PAIRS_

drop if survey_yr <1983 // first time you could identify cohab

gen relationship=0
replace relationship=1 if inrange(MARITAL_PAIRS_,1,4)

browse survey_yr id main_per_id SEQ_NUMBER_ relationship RELATION_ FIRST_MARRIAGE_YR_START 

bysort id (SEQ_NUMBER_): egen in_sample=max(SEQ_NUMBER_)

drop if in_sample==0 // people with NO DATA in any year
drop if SEQ_NUMBER_==0 // won't have data because not in that year -- like SIPP, how do I know if last year is because divorced or last year in sample? right now individual level file, so fine - this is JUST last year in sample at the moment

browse survey_yr id main_per_id SEQ_NUMBER_ relationship RELATION_ FIRST_MARRIAGE_YR_START 

bysort id: egen relationship_start=min(survey_yr) if relationship==1
bysort id: egen relationship_end=max(survey_yr) if relationship==1
bysort id: egen last_survey_yr = max(survey_yr)

browse id survey_yr relationship relationship_start relationship_end last_survey_yr SEQ_NUMBER_ MARITAL_PAIRS_
gen dissolve=.
replace dissolve=0 if relationship==1
replace dissolve=1 if relationship_end < last_survey_yr & relationship_end==survey_yr

browse id survey_yr relationship relationship_start relationship_end last_survey_yr dissolve SEQ_NUMBER_ MARITAL_PAIRS_

********************************************************************************
* Restrict to anyone in a relationship that started after 2000
********************************************************************************
keep if relationship==1 & relationship_start >=2000
browse id survey_yr relationship relationship_start relationship_end last_survey_yr dissolve SEQ_NUMBER_ MARITAL_PAIRS_

// trying to identify if married or cohabiting. .. need relation_?
drop if RELATION_ >25

sort survey_yr FAMILY_INTERVIEW_NUM_ id

browse id main_per_id survey_yr FAMILY_INTERVIEW_NUM_ relationship RELATION_ relationship_start relationship_end dissolve if FAMILY_INTERVIEW_NUM_ ==46
browse id main_per_id survey_yr FAMILY_INTERVIEW_NUM_ relationship RELATION_ relationship_start relationship_end dissolve if id==36496

egen year_family=concat(survey_yr FAMILY_INTERVIEW_NUM_), punct(_)

sort id survey_yr FAMILY_INTERVIEW_NUM_
browse id survey_yr FAMILY_INTERVIEW_NUM_ relationship RELATION_ relationship_start relationship_end dissolve

// not everyone has two rows though per relationship, it seems? so confused, and in those cases, not sure how to identify cohab. use number of times married? if 0 =cohab, anytime more than that = married?

browse id survey_yr FAMILY_INTERVIEW_NUM_ relationship relationship_start relationship_end dissolve NUM_MARRIED
drop if NUM_MARRIED==98

gen relationship_type=0
replace relationship_type=1 if NUM_MARRIED==0
replace relationship_type=2 if NUM_MARRIED>=1

label define relationship_type 1 "Cohab" 2 "Married"
label values relationship_type relationship_type

browse id survey_yr FAMILY_INTERVIEW_NUM_ relationship relationship_type RELATION_ relationship_start relationship_end dissolve NUM_MARRIED

browse id survey_yr FAMILY_INTERVIEW_NUM_ relationship relationship_type RELATION_ relationship_start relationship_end dissolve NUM_MARRIED if relationship_type==2 & RELATION_==22 // hmm might be cohabiting but have been married. but i want FIRST unions. so also only keep if num_married is 1 or less?
browse id survey_yr FAMILY_INTERVIEW_NUM_ relationship relationship_type RELATION_ relationship_start relationship_end dissolve NUM_MARRIED if id==218
browse id survey_yr FAMILY_INTERVIEW_NUM_ relationship relationship_type RELATION_ relationship_start relationship_end dissolve NUM_MARRIED if FAMILY_INTERVIEW_NUM_==1850 // good example of like, yes for id 351 i can easily change based on relation, but how do I change for ref person?
bysort survey_yr FAMILY_INTERVIEW_NUM_ (RELATION_): egen either_cohab=max(RELATION_)
browse id survey_yr FAMILY_INTERVIEW_NUM_ relationship relationship_type either_cohab RELATION_ relationship_start relationship_end dissolve if FAMILY_INTERVIEW_NUM_==1850

replace relationship_type=1 if either_cohab==22

// is it possible to go from cohab to marry? will i capture that well? like will ids change or does this work?

keep if NUM_MARRIED<=1
tab RELATION_ relationship_type

sort id survey_yr
browse id survey_yr relationship_type relationship_start relationship_end dissolve 

gen dur = survey_yr - relationship_start

save "$data_tmp\PSID_long_individs_relationships.dta", replace