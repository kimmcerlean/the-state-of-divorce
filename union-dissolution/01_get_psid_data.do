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

/*
 merge to get info on CDS eligiblity - added this to main file
merge 1:1 main_per_id INTERVIEW_NUM_1968 using "T:\data\PSID\cds_eligibility.dta", keepusing(CDS_*)
drop if _merge==2
drop _merge
*/

/* this isn't helping
// trying to merge cohab history sooner?
merge 1:1 main_per_id INTERVIEW_NUM_1968 using "$data_tmp\PSID_partner_history.dta" // when I merge this on here, 16000 linked
*/

// i have no clue what this is but it is wrong
egen family_intvw_num=rowmin(FAMILY_INTERVIEW_NUM*) // not working because inconsistent years
browse family_intvw_num FAMILY_INTERVIEW_NUM*

gen unique_id = (main_per_id*1000) + INTERVIEW_NUM_1968
browse main_per_id INTERVIEW_NUM_1968 unique_id

merge 1:1 unique_id using "T:\data\PSID\strata.dta", keepusing(stratum cluster)
drop if _merge==2
drop _merge

gen id=_n

local reshape_vars "RELEASE_ INTERVIEW_NUM_ X1968_PERSON_NUM_ RELATION_ AGE_ MARITAL_PAIRS_ MOVED_ YRS_EDUCATION_ TYPE_OF_INCOME_ TOTAL_MONEY_INCOME_ ANNUAL_WORK_HRS_ RELEASE_NUM2_ FAMILY_COMPOSITION_ AGE_REF_ AGE_SPOUSE_ SEX_HEAD_ AGE_YOUNG_CHILD_ RESPONDENT_WHO_ RACE_1_HEAD_ EMPLOY_STATUS_HEAD_ MARITAL_STATUS_HEAD_ WIDOW_LENGTH_HEAD_ WAGES_HEAD_ FAMILY_INTERVIEW_NUM_ FATHER_EDUC_HEAD_ WAGE_RATE_HEAD_ WAGE_RATE_WIFE_ REGION_ NUM_CHILDREN_ CORE_WEIGHT_ TOTAL_HOURS_HEAD_ TOTAL_HOURS_WIFE_ LABOR_INCOME_HEAD_ LABOR_INCOME_WIFE_ TOTAL_FAMILY_INCOME_ SEQ_NUMBER_ RESPONDENT_ FAMILY_ID_SO_ COMPOSITION_CHANGE_ NEW_REF_ HRLY_RATE_HEAD_ RELIGION_HEAD_ NEW_SPOUSE_ FATHER_EDUC_WIFE_ MOTHER_EDUC_WIFE_ MOTHER_EDUC_HEAD_ TYPE_TAXABLE_INCOME_ OFUM_TAXABLE_INCOME_ COLLEGE_HEAD_ COLLEGE_WIFE_ SALARY_TYPE_HEAD_ FIRST_MARRIAGE_YR_WIFE_ RELIGION_WIFE_ WORK_MONEY_WIFE_ EMPLOY_STATUS_WIFE_ SALARY_TYPE_WIFE_ HRLY_RATE_WIFE_ RESEPONDENT_WIFE_ WORK_MONEY_HEAD_ MARITAL_STATUS_REF_ EVER_MARRIED_HEAD_ EMPLOYMENT_ STUDENT_ BIRTH_YR_ COUPLE_STATUS_REF_ RELATION_TO_HEAD_ NUM_MARRIED_HEAD_ FIRST_MARRIAGE_YR_HEAD_ FIRST_MARRIAGE_END_HEAD_ FIRST_WIDOW_YR_HEAD_ FIRST_DIVORCE_YR_HEAD_ FIRST_SEPARATED_YR_HEAD_ LAST_MARRIAGE_YR_HEAD_ LAST_WIDOW_YR_HEAD_ LAST_DIVORCE_YR_HEAD_ LAST_SEPARATED_YR_HEAD_ FAMILY_STRUCTURE_HEAD_ RACE_2_HEAD_ NUM_MARRIED_WIFE_ FIRST_MARRIAGE_END_WIFE_ FIRST_WIDOW_YR_WIFE_ FIRST_DIVORCE_YR_WIFE_ FIRST_SEPARATED_YR_WIFE_ LAST_MARRIAGE_YR_WIFE_ LAST_WIDOW_YR_WIFE_ LAST_DIVORCE_YR_WIFE_ LAST_SEPARATED_YR_WIFE_ FAMILY_STRUCTURE_WIFE_ RACE_1_WIFE_ RACE_2_WIFE_ STATE_ BIRTHS_REF_ BIRTH_SPOUSE_ BIRTHS_BOTH_ OFUM_LABOR_INCOME_ RELEASE_NUM_ SALARY_HEAD_ SALARY_WIFE_ EMPLOY_STATUS1_HEAD_ EMPLOY_STATUS2_HEAD_ EMPLOY_STATUS3_HEAD_ EMPLOY_STATUS1_WIFE_ EMPLOY_STATUS2_WIFE_ EMPLOY_STATUS3_WIFE_ RACE_3_WIFE_ RACE_3_HEAD_ RACE_4_HEAD_ COR_IMM_WT_ ETHNIC_WIFE_ ETHNIC_HEAD_ CROSS_SECTION_FAM_WT_ LONG_WT_ CROSS_SECTION_WT_ EARNINGS_2YRLAG_ AMOUNTEARN_1_HEAD_ TOTAL_WEEKS_HEAD_ TOTAL_HOURS2_HEAD_ AMOUNTEARN_1_WIFE_ TOTAL_WEEK_WIFE_ TOTAL_HOURS2_WIFE_ HOURS_WK_HEAD_ NUM_JOBS_ BACHELOR_YR_ ENROLLED_ COLLEGE_ SEX_WIFE_ BACHELOR_YR_WIFE_ ENROLLED_WIFE_ BACHELOR_YR_HEAD_ ENROLLED_HEAD_ WAGES1_WIFE_ METRO_ CURRENTLY_WORK_HEAD_ CURRENTLY_WORK_WIFE_ AMOUNTEARN_2_HEAD_ AMOUNTEARN_3_HEAD_ AMOUNTEARN_4_HEAD_ AMOUNTEARN_2_WIFE_ AMOUNTEARN_3_WIFE_ AMOUNTEARN_4_WIFE_ TAXABLE_HEAD_WIFE_ WAGES_WIFE_ WAGES_HEAD_PRE_ WAGES_WIFE_PRE_ EDUC1_HEAD_ EDUC_HEAD_ EDUC1_WIFE_ EDUC_WIFE_ WEEKLY_HRS_HEAD_ WEEKLY_HRS_WIFE_ WEEKLY_HRS1_HEAD_ WEEKLY_HRS1_WIFE_ TOTAL_HOUSEWORK_HW_ HOUSEWORK_WIFE_ HOUSEWORK_HEAD_ MOST_HOUSEWORK_ AGE_OLDEST_CHILD_ CDS_ELIGIBLE_ CHILDCARE_COSTS_ RENT_COST_V1_ RENT_COST_V2_ TOTAL_HOUSING_ MORTGAGE_COST_ HOUSE_VALUE_ HOUSE_STATUS_ VEHICLE_OWN_ HEALTH_INSURANCE_FAM_ DIVIDENDS_JOINT_ DIVIDENDS_HEAD_ DIVIDENDS_SPOUSE_ INTEREST_JOINT_ POVERTY_THRESHOLD_ TRANSFER_INCOME_ WELFARE_HEAD_1_ WELFARE_HEAD_2_ WELFARE_SPOUSE_1_ WELFARE_SPOUSE_2_ WELFARE_JOINT_ FOOD_STAMPS_ BANK_ASSETS_ OTHER_ASSETS_ STOCKS_MF_ WEALTH_NO_EQUITY_ WEALTH_EQUITY_ VEHICLE_VALUE_"

reshape long `reshape_vars', i(id unique_id stratum cluster) j(survey_yr)

save "$data_tmp\PSID_full_long.dta", replace
replace SEQ_NUMBER_=0 if SEQ_NUMBER==.
bysort id (SEQ_NUMBER_): egen in_sample=max(SEQ_NUMBER_)

drop if in_sample==0 // people with NO DATA in any year
sort id survey_yr
browse survey_yr SEQ_NUMBER_ id
drop if SEQ_NUMBER_==0 // won't have data becausenot in that year -- like SIPP, how do I know if last year is because divorced or last year in sample? right now individual level file, so fine - this is JUST last year in sample at the moment. okay, but think the problem is, if you enter LATER, this is 0 in years prior? so maybe do min and max when seq number is not 0?? ah and missing for 1968 i am dumb

save "$data_tmp\PSID_data_long.dta", replace

bysort id (survey_yr): egen first_survey_yr = min(survey_yr)
bysort id (survey_yr): egen last_survey_yr = max(survey_yr)

browse id survey_yr first_survey_yr last_survey_yr

collapse (min) first_survey_yr (max) last_survey_yr, by(id)
save "$data_tmp\PSID_year_lookup.dta", replace

// browse id survey_yr yr_married1 status1 yr_end1 yr_married2 status2 yr_end2 FIRST_MARRIAGE_YR_START in_marital_history // ensuring marital history makes sense
