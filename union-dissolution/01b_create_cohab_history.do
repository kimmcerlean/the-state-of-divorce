********************************************************************************
* Trying to get a file of all prior cohabitations for each partner to use as control variables
* create_cohab_history.do
* Kim McErlean
********************************************************************************

********************************************************************************
* Just cohabitation
********************************************************************************

use "$PSID\family_matrix_68_19.dta", clear // relationship matrix downloaded from PSID site

unique MX5 MX6 // should match the 82000 in other file? -- okay so it does. I am dumb because I restricted to only partners. omg this explains evertything

rename MX5 ego_1968_id 
rename MX6 ego_per_num
gen partner_1968_id = MX10 if MX8==22
gen partner_per_num = MX11 if MX8==22
gen unique_id = (ego_1968_id*1000) + ego_per_num // how they tell you to identify in main file
// egen ego_unique = concat(ego_1968_id ego_per_num), punct(_)
// egen partner_unique = concat(partner_1968_id partner_per_num), punct(_)
gen partner_unique_id = (partner_1968_id*1000) + partner_per_num

// try making specific variable to match E30002 that is 1968 id? but what if not in 1968??

keep if MX8==22

browse MX2 ego_1968_id ego_per_num unique_id partner_1968_id partner_per_num partner_unique_id MX8 // does unique_id track over years? or just 1 record per year? might this be wrong?

keep MX2 ego_1968_id ego_per_num unique_id partner_1968_id partner_per_num partner_unique_id MX8

// seeing if not working because needs to be LONG 
reshape wide partner_1968_id partner_per_num partner_unique_id MX8, i(ego_1968_id ego_per_num unique_id) j(MX2)

// for ego - will match on unique_id? need to figure out how to match partner, keep separate?
rename ego_1968_id main_per_id
rename ego_per_num INTERVIEW_NUM_

gen spouse_per_num_all = INTERVIEW_NUM_
gen spouse_id_all = main_per_id
gen INTERVIEW_NUM_1968 = INTERVIEW_NUM_

// okay so not JUST the ids, but also YEAR?!  unique MX2 main_per_id INTERVIEW_NUM_
// rename MX2 survey_yr

unique main_per_id INTERVIEW_NUM_1968

save "$data_tmp\PSID_partner_history.dta", replace // really this is just cohabitation NOT marriages.

********************************************************************************
* All relationships
********************************************************************************

use "$PSID\family_matrix_68_19.dta", clear // relationship matrix downloaded from PSID site

unique MX5 MX6 // should match the 82000 in other file? -- okay so it does. I am dumb because I restricted to only partners. omg this explains evertything

rename MX5 ego_1968_id 
rename MX6 ego_per_num
gen unique_id = (ego_1968_id*1000) + ego_per_num // how they tell you to identify in main file
// egen ego_unique = concat(ego_1968_id ego_per_num), punct(_)
// egen partner_unique = concat(partner_1968_id partner_per_num), punct(_)

// for now, will see if splitting types or keeping together makes sense, need to wrap my head around this file
gen cohab_1968_id = MX10 if MX8==22
gen cohab_per_num = MX11 if MX8==22
gen cohab_unique_id = (cohab_1968_id*1000) + cohab_per_num

gen spouse_1968_id = MX10 if MX8==20
gen spouse_per_num = MX11 if MX8==20
gen spouse_unique_id = (spouse_1968_id*1000) + spouse_per_num

gen partner_1968_id = MX10 if MX8==22 | MX8==20
gen partner_per_num = MX11 if MX8==22 | MX8==20
gen partner_unique_id = (partner_1968_id*1000) + partner_per_num

// try making specific variable to match E30002 that is 1968 id? but what if not in 1968??

keep if MX8==22 | MX8==20

browse MX2 ego_1968_id ego_per_num unique_id cohab_1968_id cohab_per_num cohab_unique_id spouse_1968_id spouse_per_num spouse_unique_id partner_1968_id partner_per_num partner_unique_id MX8 // does unique_id track over years? or just 1 record per year? might this be wrong?

keep MX2 ego_1968_id ego_per_num unique_id cohab_1968_id cohab_per_num cohab_unique_id spouse_1968_id spouse_per_num spouse_unique_id partner_1968_id partner_per_num partner_unique_id MX8

// seeing if not working because needs to be WIDE
*one person has two spouses, one seems to be an error so dropping that row
drop if ego_1968_id == 1821 & ego_per_num == 170 & MX2==1977 & partner_unique_id== 1821004
 
reshape wide cohab_1968_id cohab_per_num cohab_unique_id spouse_1968_id spouse_per_num spouse_unique_id partner_1968_id partner_per_num partner_unique_id MX8, i(ego_1968_id ego_per_num unique_id) j(MX2)

// for ego - will match on unique_id? need to figure out how to match partner, keep separate? what is happening here? I think it's because in other file, I have matched husband and wife so want to be able to first match on husband, then match on wife, so need two ids. Same id, but one name for use for husband and one for wife. don't think I need this for current purposes, but might need to rename the ego ones to match back to individual file.
rename ego_1968_id main_per_id
rename ego_per_num INTERVIEW_NUM_
gen INTERVIEW_NUM_1968 = INTERVIEW_NUM_

/*
gen spouse_per_num_all = INTERVIEW_NUM_
gen spouse_id_all = main_per_id
gen INTERVIEW_NUM_1968 = INTERVIEW_NUM_
*/

// okay so not JUST the ids, but also YEAR?!  unique MX2 main_per_id INTERVIEW_NUM_
// rename MX2 survey_yr

unique main_per_id INTERVIEW_NUM_1968

save "$data_tmp\PSID_union_history.dta", replace

browse main_per_id INTERVIEW_NUM_ unique_id MX8* partner_1968_id* partner_per_num* partner_unique_id*
// compare to this:  "$data_keep\PSID_union_history_created.dta"