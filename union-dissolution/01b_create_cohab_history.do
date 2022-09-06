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
