********************************************************************************
* Getting PSID sample for union dissolution
* create_sample.do
* Kim McErlean
********************************************************************************

********************************************************************************
* Take downloaded data, turn it into Stata format and rename
********************************************************************************
do "$PSID/J342304 (2021)/J342304.do" // note - this will need to be updated to wherever the raw data you downloaded is - this is directly provided by PSID
do "$PSID/J342304 (2021)/J342304_formats.do" // note - this will need to be updated to wherever the raw data you downloaded is - this is directly provided by PSID - also need to direct this file where to save: "$PSID\PSID_full.dta"
do "$code/x_rename_vars.do"
do "$code/x_marital_history.do"

********************************************************************************
* Then reshape the data to be long, I think this will be less overwhelming / 
* easier to see which variables tracked consistently and how
********************************************************************************
use "$PSID\PSID_full_renamed.dta", clear
browse X1968_PERSON_NUM_1968 X1968_INTERVIEW_NUM_1968 // 30001 = interview; 30002 = person number

rename X1968_PERSON_NUM_1968 main_per_id
rename X1968_INTERVIEW_NUM_1968 main_fam_id

gen unique_id = (main_fam_id*1000) + main_per_id // (ER30001 * 1000) + ER30002
browse main_per_id main_fam_id unique_id

// want to see if I can make this time-fixed sample status
recode main_fam_id (1/2999 = 1 "SRC cross-section") (3001/3441 = 2 "Immigrant 97") (3442/3511 = 3 "Immigrant 99") (4001/4851 = 4 "Immigrant 17/19") (5001/6999  = 5 "1968 Census") (7001/9043 = 6 "Latino 90") (9044/9308 = 7 "Latino 92"), gen(sample_type) 
tab sample_type, m

// merge on marital history
merge 1:1 unique_id using "$created_data/marital_history_wide.dta" // is this going to change now that I have updated? I think I had both of the variables but they were in wrong columns, so I still have main_per_id and interview number, they are just different? OR do I need to rename in marital history file??? oh no...

gen in_marital_history=0
replace in_marital_history=1 if _merge==3
drop _merge

// i have no clue what this is but it is wrong
// egen family_intvw_num=rowmin(FAMILY_INTERVIEW_NUM*) // not working because inconsistent years
// browse family_intvw_num FAMILY_INTERVIEW_NUM*

merge 1:1 unique_id using "$PSID/strata.dta", keepusing(stratum cluster)
drop if _merge==2
drop _merge

gen id=_n

local reshape_vars "RELEASE_ X1968_PERSON_NUM_ INTERVIEW_NUM_ RELATION_ AGE_INDV_ MARITAL_PAIRS_ MOVED_ YRS_EDUCATION_INDV_ TYPE_OF_INCOME_ TOTAL_MONEY_INCOME_ ANNUAL_HOURS_T1_INDV_ RELEASE_NUM2_ FAMILY_COMPOSITION_ AGE_HEAD_ AGE_WIFE_ SEX_HEAD_ AGE_YOUNG_CHILD_ RESPONDENT_WHO_ RACE_1_HEAD_ EMPLOY_STATUS_HEAD_ MARST_DEFACTO_HEAD_ WIDOW_LENGTH_HEAD_ WAGES_T1_HEAD_ FAMILY_INTERVIEW_NUM_ FATHER_EDUC_HEAD_ HRLY_RATE_T1_HEAD_ HRLY_RATE_T1_WIFE_ REGION_ NUM_CHILDREN_ CORE_WEIGHT_ ANNUAL_HOURS_T1_HEAD_ ANNUAL_HOURS_T1_WIFE_ LABOR_INCOME_T1_HEAD_ LABOR_INCOME_T1_WIFE_ TOTAL_INCOME_T1_FAMILY_ TAXABLE_T1_HEAD_WIFE_ EDUC1_HEAD_ EDUC1_WIFE_ WEEKLY_HRS1_T1_WIFE_ WEEKLY_HRS1_T1_HEAD_ TOTAL_HOUSEWORK_T1_HW_ RENT_COST_V1_ MORTGAGE_COST_ HOUSE_VALUE_ HOUSE_STATUS_ VEHICLE_OWN_ POVERTY_THRESHOLD_ SEQ_NUMBER_ RESPONDENT_ FAMILY_ID_SO_ COMPOSITION_CHANGE_ NEW_HEAD_ HOUSEWORK_WIFE_ HOUSEWORK_HEAD_ MOST_HOUSEWORK_T1_ AGE_OLDEST_CHILD_ FOOD_STAMPS_ HRLY_RATE_CURRENT_HEAD_ RELIGION_HEAD_ CHILDCARE_COSTS_ TRANSFER_INCOME_ WELFARE_JOINT_ NEW_WIFE_ FATHER_EDUC_WIFE_ MOTHER_EDUC_WIFE_ MOTHER_EDUC_HEAD_ TYPE_TAXABLE_INCOME_ TOTAL_INCOME_T1_INDV_ COLLEGE_HEAD_ COLLEGE_WIFE_ EDUC_HEAD_ EDUC_WIFE_ SALARY_TYPE_HEAD_ FIRST_MARRIAGE_YR_WIFE_ RELIGION_WIFE_ WORK_MONEY_WIFE_ EMPLOY_STATUS_WIFE_ SALARY_TYPE_WIFE_ HRLY_RATE_CURRENT_WIFE_ RESEPONDENT_WIFE_ WORK_MONEY_HEAD_ MARST_LEGAL_HEAD_ EVER_MARRIED_HEAD_ EMPLOYMENT_INDV_ STUDENT_T1_INDV_ BIRTH_YR_INDV_ COUPLE_STATUS_HEAD_ OTHER_ASSETS_ STOCKS_MF_ WEALTH_NO_EQUITY_ WEALTH_EQUITY_ VEHICLE_VALUE_ RELATION_TO_HEAD_ NUM_MARRIED_HEAD_ FIRST_MARRIAGE_YR_HEAD_ FIRST_MARRIAGE_END_HEAD_ FIRST_WIDOW_YR_HEAD_ FIRST_DIVORCE_YR_HEAD_ FIRST_SEPARATED_YR_HEAD_ LAST_MARRIAGE_YR_HEAD_ LAST_WIDOW_YR_HEAD_ LAST_DIVORCE_YR_HEAD_ LAST_SEPARATED_YR_HEAD_ FAMILY_STRUCTURE_HEAD_ RACE_2_HEAD_ NUM_MARRIED_WIFE_ FIRST_MARRIAGE_END_WIFE_ FIRST_WIDOW_YR_WIFE_ FIRST_DIVORCE_YR_WIFE_ FIRST_SEPARATED_YR_WIFE_ LAST_MARRIAGE_YR_WIFE_ LAST_WIDOW_YR_WIFE_ LAST_DIVORCE_YR_WIFE_ LAST_SEPARATED_YR_WIFE_ FAMILY_STRUCTURE_WIFE_ RACE_1_WIFE_ RACE_2_WIFE_ STATE_ BIRTHS_T1_HEAD_ BIRTHS_T1_WIFE_ BIRTHS_T1_BOTH_ WELFARE_HEAD_1_ WELFARE_WIFE_1_ LABOR_INCOME_T1_INDV_ RELEASE_NUM_ WAGES_CURRENT_HEAD_ WAGES_CURRENT_WIFE_ WAGES_T1_WIFE_ RENT_COST_V2_ DIVIDENDS_HEAD_ DIVIDENDS_WIFE_ WELFARE_HEAD_2_ WELFARE_WIFE_2_ EMPLOY_STATUS1_HEAD_ EMPLOY_STATUS2_HEAD_ EMPLOY_STATUS3_HEAD_ EMPLOY_STATUS1_WIFE_ EMPLOY_STATUS2_WIFE_ EMPLOY_STATUS3_WIFE_ RACE_3_WIFE_ RACE_3_HEAD_ WAGES_ALT_T1_HEAD_ WAGES_ALT_T1_WIFE_ WEEKLY_HRS_T1_HEAD_ WEEKLY_HRS_T1_WIFE_ RACE_4_HEAD_ COR_IMM_WT_ ETHNIC_WIFE_ ETHNIC_HEAD_ CROSS_SECTION_FAM_WT_ LONG_WT_ CROSS_SECTION_WT_ CDS_ELIGIBLE_ TOTAL_HOUSING_ HEALTH_INSURANCE_FAM_ BANK_ASSETS_ LABOR_INC_J1_T1_HEAD_ TOTAL_WEEKS_T1_HEAD_ ANNUAL_HOURS2_T1_HEAD_ LABOR_INC_J1_T1_WIFE_ TOTAL_WEEKS_T1_WIFE_ ANNUAL_HOURS2_T1_WIFE_ WEEKLY_HRS_T2_HEAD_ LABOR_INC_J2_T1_HEAD_ LABOR_INC_J3_T1_HEAD_ LABOR_INC_J4_T1_HEAD_ LABOR_INC_J2_T1_WIFE_ LABOR_INC_J3_T1_WIFE_ LABOR_INC_J4_T1_WIFE_ DIVIDENDS_JOINT_ INTEREST_JOINT_ NUM_JOBS_T1_INDV_ BACHELOR_YR_INDV_ STUDENT_CURRENT_INDV_ COLLEGE_INDV_ SEX_WIFE_ BACHELOR_YR_WIFE_ ENROLLED_WIFE_ BACHELOR_YR_HEAD_ ENROLLED_HEAD_ WAGES2_T1_WIFE_ METRO_ CURRENTLY_WORK_HEAD_ CURRENTLY_WORK_WIFE_ EMPLOY_STATUS_T2_HEAD_ EMPLOY_STATUS_T2_WIFE_ WEEKLY_HRS_T2_WIFE_ START_YR_EMPLOYER_HEAD_ START_YR_EMPLOYER_WIFE_ START_YR_CURRENT_HEAD_ START_YR_CURRENT_WIFE_ START_YR_PREV_HEAD_ START_YR_PREV_WIFE_ YRS_CURRENT_EMPLOY_HEAD_ YRS_CURRENT_EMPLOY_WIFE_ LABOR_INCOME_T2_HEAD_ LABOR_INCOME_T2_WIFE_ WEEKLY_HRS_T2_INDV_ LABOR_INCOME_T2_INDV_ HOUSEWORK_INDV_ RACE_4_WIFE_ HISPANICITY_WIFE_ HISPANICITY_HEAD_ CHILDCARE_HEAD_ CHILDCARE_WIFE_ ADULTCARE_HEAD_ ADULTCARE_WIFE_ TOTAL_INCOME_T2_FAMILY_ WEEKS_WORKED_T2_INDV_ FOLLOW_STATUS_ NUM_IN_HH_ NUM_NONFU_IN_HH_ NEW_WIFE_YEAR_ MOVED_YEAR_ MOVED_MONTH_ SPLITOFF_YEAR_ SPLITOFF_MONTH_ DATA_RECORD_TYPE_ SPLITOFF_ NEW_HEAD_YEAR_ HS_GRAD_HEAD_ ATTENDED_COLLEGE_HEAD_ HIGHEST_DEGREE_HEAD_ HS_GRAD_WIFE_ ATTENDED_COLLEGE_WIFE_ HIGHEST_DEGREE_WIFE_ WHERE_EDUC_HEAD_ FOREIGN_DEG_HEAD_ WHERE_EDUC_WIFE_ FOREIGN_DEG_WIFE_ YR_EDUC_UPD_HEAD_ YR_EDUC_UPD_WIFE_ DENOMINATION_HEAD_ DENOMINATION_WIFE_"

reshape long `reshape_vars', i(id unique_id stratum cluster) j(survey_yr)

save "$temp\PSID_full_long.dta", replace

replace SEQ_NUMBER_=0 if SEQ_NUMBER==.
bysort id (SEQ_NUMBER_): egen in_sample=max(SEQ_NUMBER_)

drop if in_sample==0 // people with NO DATA in any year
sort id survey_yr
browse survey_yr SEQ_NUMBER_ id
drop if SEQ_NUMBER_==0 // won't have data becausenot in that year -- like SIPP, how do I know if last year is because divorced or last year in sample? right now individual level file, so fine - this is JUST last year in sample at the moment. okay, but think the problem is, if you enter LATER, this is 0 in years prior? so maybe do min and max when seq number is not 0?? ah and missing for 1968 i am dumb

save "$temp\PSID_data_long.dta", replace

// browse id survey_yr yr_married1 status1 yr_end1 yr_married2 status2 yr_end2 FIRST_MARRIAGE_YR_START in_marital_history // ensuring marital history makes sense

bysort id (survey_yr): egen first_survey_yr = min(survey_yr)
bysort id (survey_yr): egen last_survey_yr = max(survey_yr)

browse id survey_yr first_survey_yr last_survey_yr

collapse (min) first_survey_yr (max) last_survey_yr, by(id)
save "$temp\PSID_year_lookup.dta", replace
