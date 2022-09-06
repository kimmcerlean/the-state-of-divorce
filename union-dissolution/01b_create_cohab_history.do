********************************************************************************
* Trying to get a file of all prior cohabitations for each partner to use as control variables
* create_cohab_history.do
* Kim McErlean
********************************************************************************

use "$PSID\family_matrix_68_19.dta", clear // relationship matrix downloaded from PSID site

rename MX5 ego_1968_id 
rename MX6 ego_per_num
gen partner_1968_id = MX10 if MX8==22
gen partner_per_num = MX11 if MX8==22
egen ego_unique = concat(ego_1968_id ego_per_num), punct(_)
egen partner_unique = concat(partner_1968_id partner_per_num), punct(_)

keep if MX8==22

keep MX2 ego_1968_id ego_per_num ego_unique partner_1968_id partner_per_num partner_unique MX8

reshape wide partner_1968_id partner_per_num partner_unique MX8, i(ego_1968_id ego_per_num ego_unique) j(MX2)

rename ego_1968_id main_per_id
rename ego_per_num 

save "$data_tmp\PSID_partner_history.dta", replace

// merge on marital history
merge 1:1 main_per_id INTERVIEW_NUM_1968 using "$data_keep\marital_history_wide.dta"

gen in_marital_history=0
replace in_marital_history=1 if _merge==3
drop _merge

egen family_intvw_num=rowmin(FAMILY_INTERVIEW_NUM*) // not working because inconsistent years
browse family_intvw_num FAMILY_INTERVIEW_NUM*

gen unique_id = (family_intvw_num*1000) + main_per_id
browse unique_id family_intvw_num main_per_id

gen id=_n