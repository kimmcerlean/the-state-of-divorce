********************************************************************************
* Getting PSID sample for union dissolution
* create_variables.do
* Kim McErlean
********************************************************************************

use "$data_tmp\PSID_long_individs_relationships.dta", clear

tab relationship_type dissolve
tab relationship_start dissolve if relationship_type==2 // checking against table A1 in Schwartz and Han 2014
unique relationship_type dissolve, by(relationship_start)

// education
gen college_wife= 0 
replace college_wife=1 if COLLEGE_WIFE_ == 1
gen college_head = 0
replace college_head = 1 if COLLEGE_HEAD_ == 1

browse id survey_yr FAMILY_INTERVIEW_NUM_ RELATION_ YRS_EDUCATION_ college_wife college_head EDUC_WIFE_ EDUC_HEAD_
	//yrs education should match head if relation==head, or wife if otherwise, use that to check?

// income / structure
browse id survey_yr FAMILY_INTERVIEW_NUM_ TAXABLE_HEAD_WIFE_ TOTAL_FAMILY_INCOME_ EMPLOY_STATUS1_HEAD_ LABOR_INCOME_HEAD_ WAGES_HEAD_  LABOR_INCOME_WIFE_ WAGES_WIFE_ WAGES_WIFE_PRE_ WAGES1_WIFE_ SALARY_WIFE_ AMOUNTEARN_*_WIFE_

	// to use: WAGES_HEAD_ WAGES_WIFE_

inspect WAGES_WIFE_ if EMPLOY_STATUS1_WIFE_==1 // okay, this is okay, no missing
inspect WAGES_HEAD_ if EMPLOY_STATUS1_HEAD_==1 // okay, this is okay, no missing

// validate this is aligning with total
browse id survey_yr FAMILY_INTERVIEW_NUM_ TAXABLE_HEAD_WIFE_ TOTAL_FAMILY_INCOME_ EMPLOY_STATUS1_HEAD_ WAGES_HEAD_ EMPLOY_STATUS1_WIFE_ WAGES_WIFE_

	// helpful note: 2001, id 23 and 70 are both in family 285, head + wife = total taxable, family income is greater, so someone else must be working.
	

// restrict to working age (18-55) - at time of marriage or all? check what others do - Killewald said ages 18-55