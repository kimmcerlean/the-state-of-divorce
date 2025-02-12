use "$PSID/mh85_21.dta", clear

/* first rename for ease*/
rename MH1 releaseno
rename MH2 main_fam_id
rename MH3 main_per_id
rename MH4 sex
rename MH5 mo_born
rename MH6 yr_born
rename MH7 id_spouse
rename MH8 per_no_spouse
rename MH9 marrno 
rename MH10 mo_married
rename MH11 yr_married
rename MH12 status
rename MH13 mo_widdiv
rename MH14 yr_widdiv
rename MH15 mo_sep
rename MH16 yr_sep
rename MH17 history
rename MH18 num_marriages
rename MH19 marital_status
rename MH20 num_records

label define status 1 "Intact" 3 "Widow" 4 "Divorce" 5 "Separation" 7 "Other" 8 "DK" 9 "Never Married"
label values status status

gen unique_id = (main_fam_id*1000) + main_per_id
// browse unique_id main_per_id main_fam_id

browse unique_id marrno status yr_widdiv yr_sep

egen yr_end = rowmin(yr_widdiv yr_sep)
browse unique_id marrno status yr_widdiv yr_sep yr_end

// this is currently LONG - one record per marriage. want to make WIDE

drop mo_born mo_widdiv yr_widdiv mo_sep yr_sep history
bysort unique_id: egen year_birth = min(yr_born)
drop yr_born

reshape wide id_spouse per_no_spouse mo_married yr_married status yr_end, i(unique_id main_per_id main_fam_id) j(marrno)
// gen INTERVIEW_NUM_1968 = fam_id

save "$created_data/marital_history_wide.dta", replace