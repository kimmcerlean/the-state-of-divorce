********************************************************************************
* Getting PSID sample for union dissolution
* create_variables.do
* Kim McErlean
********************************************************************************

use "$data_tmp\PSID_long_individs_relationships.dta", clear

// education
COLLEGE_WIFE_ COLLEGE_HEAD_

// income / structure
browse EMPLOY_STATUS1_HEAD_ LABOR_INCOME_HEAD_ WAGES_HEAD_ EMPLOY_STATUS1_WIFE_ LABOR_INCOME_WIFE_ WAGES_WIFE_ TOTAL_FAMILY_INCOME_ SALARY_WIFE_ AMOUNTEARN_1_WIFE_

/// might need to pull in all amountearn and sum (just have 1, think there might be more?)

inspect WAGES_WIFE_ if EMPLOY_STATUS1_WIFE_==1 // why so many missing wages and all labor income missing - is something wrong?
inspect AMOUNTEARN_1_WIFE_ if EMPLOY_STATUS1_WIFE_==1 // so amount earn is more accurate, but is this complete???