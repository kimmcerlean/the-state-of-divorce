********************************************************************************
* Trying to get a file of all prior cohabitations for each partner to use as control variables
* create_cohab_history.do
* Kim McErlean
********************************************************************************

********************************************************************************
* Just cohabitation
********************************************************************************

use "$PSID/family_matrix_68_21.dta", clear // relationship matrix downloaded from PSID site

unique MX5 MX6 // should match the 82000 in other file? -- okay so it does. I am dumb because I restricted to only partners. omg this explains evertything

rename MX5 ego_1968_id 
rename MX6 ego_per_num
recode MX7 (1=1)(2=2)(3/8=3)(9=2)(10=1)(11/19=3)(20/22=2)(23/87=3)(88=2)(89/120=3), gen(ego_rel) // ego relationship to ref. because also only really useful if one is reference person bc otherwise i don't get a ton of info about them
recode MX12 (1=1)(2=2)(3/8=3)(9=2)(10=1)(11/19=3)(20/22=2)(23/87=3)(88=2)(89/120=3), gen(alter_rel) // alter relationship to ref

label define rels 1 "Ref" 2 "Spouse/Partner" 3 "Other"
label values ego_rel alter_rel rels

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
rename ego_1968_id main_fam_id
rename ego_per_num main_per_id

// browse main_fam_id main_per_id unique_id

gen spouse_id_all = main_fam_id // spouse_fam_id
gen spouse_per_num_all = main_per_id // spouse_per_id
gen partner_unique_id = unique_id
// gen INTERVIEW_NUM_1968 = INTERVIEW_NUM_

// okay so not JUST the ids, but also YEAR?!  unique MX2 main_per_id INTERVIEW_NUM_
// rename MX2 survey_yr

unique main_per_id main_fam_id

save "$temp/PSID_partner_history.dta", replace // really this is just cohabitation NOT marriages.

********************************************************************************
* All relationships
********************************************************************************

use "$PSID/family_matrix_68_21.dta", clear // relationship matrix downloaded from PSID site

unique MX5 MX6 // should match the 82000 in other file? -- okay so it does. I am dumb because I restricted to only partners. omg this explains evertything

rename MX5 ego_1968_id 
rename MX6 ego_per_num
gen unique_id = (ego_1968_id*1000) + ego_per_num // how they tell you to identify in main file
// egen ego_unique = concat(ego_1968_id ego_per_num), punct(_)
// egen partner_unique = concat(partner_1968_id partner_per_num), punct(_)

recode MX7 (1=1)(2=2)(3/8=3)(9=2)(10=1)(11/19=3)(20/22=2)(23/87=3)(88=2)(89/120=3), gen(ego_rel) // ego relationship to ref. because also only really useful if one is reference person bc otherwise i don't get a ton of info about them
recode MX12 (1=1)(2=2)(3/8=3)(9=2)(10=1)(11/19=3)(20/22=2)(23/87=3)(88=2)(89/120=3), gen(alter_rel) // alter relationship to ref

label define rels 1 "Ref" 2 "Spouse/Partner" 3 "Other"
label values ego_rel alter_rel rels

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

keep if MX8==22 | MX8==20 // spouses or partners

browse MX2 ego_1968_id ego_per_num ego_rel unique_id cohab_1968_id cohab_per_num cohab_unique_id spouse_1968_id spouse_per_num spouse_unique_id partner_1968_id partner_per_num partner_unique_id alter_rel MX8 // does unique_id track over years? or just 1 record per year? might this be wrong?

keep MX2 ego_1968_id ego_per_num unique_id cohab_1968_id cohab_per_num cohab_unique_id spouse_1968_id spouse_per_num spouse_unique_id partner_1968_id partner_per_num partner_unique_id MX8 ego_rel alter_rel

// seeing if not working because needs to be WIDE
*one person has two spouses, one seems to be an error so dropping that row
drop if ego_1968_id == 1821 & ego_per_num == 170 & MX2==1977 & partner_unique_id== 1821004
 
reshape wide ego_rel cohab_1968_id cohab_per_num cohab_unique_id spouse_1968_id spouse_per_num spouse_unique_id partner_1968_id partner_per_num partner_unique_id alter_rel MX8, i(ego_1968_id ego_per_num unique_id) j(MX2)

// for ego - will match on unique_id? need to figure out how to match partner, keep separate? what is happening here? I think it's because in other file, I have matched husband and wife so want to be able to first match on husband, then match on wife, so need two ids. Same id, but one name for use for husband and one for wife. don't think I need this for current purposes, but might need to rename the ego ones to match back to individual file.
rename ego_1968_id main_fam_id
rename ego_per_num main_per_id
// gen INTERVIEW_NUM_1968 = INTERVIEW_NUM_

/*
gen spouse_per_num_all = INTERVIEW_NUM_
gen spouse_id_all = main_per_id
gen INTERVIEW_NUM_1968 = INTERVIEW_NUM_
*/

// okay so not JUST the ids, but also YEAR?!  unique MX2 main_per_id INTERVIEW_NUM_
// rename MX2 survey_yr

unique main_per_id main_fam_id

save "$temp/PSID_union_history.dta", replace

browse main_per_id main_fam_id unique_id MX8* partner_1968_id* partner_per_num* partner_unique_id*
// compare to this:  "$data_keep\PSID_union_history_created.dta"

* Now want to reshape back to long so I can merge info on
drop cohab_1968_id* cohab_per_num* cohab_unique_id* spouse_1968_id* spouse_per_num* spouse_unique_id*

reshape long MX8 ego_rel alter_rel partner_1968_id partner_per_num partner_unique_id, i(main_per_id main_fam_id unique_id) j(year)

// want to get relationship order
unique partner_unique_id, by(unique_id) gen(rel_num)
drop rel_num

egen couple_num = group(unique_id partner_unique_id)

//https://www.statalist.org/forums/forum/general-stata-discussion/general/1437910-trying-to-rank-numbers-without-gaps
sort unique_id year
by unique_id: egen rank = rank(partner_unique_id), track
egen help_var = group(unique_id rank)

bysort unique_id (rank): gen rel_num = sum(rank != rank[_n-1]) if rank != .

// now do same thing specifically for MARRIAGE order
sort unique_id year
by unique_id: egen marr_rank = rank(partner_unique_id) if MX8==20, track
egen marr_help_var = group(unique_id marr_rank)

bysort unique_id (marr_rank): gen marr_num = sum(marr_rank != marr_rank[_n-1]) if marr_rank != .

drop rank help_var marr_rank marr_help_var

rename year survey_yr // to match to later files

save "$temp/PSID_relationship_list_tomatch.dta", replace
/// so this file, because it was reshaped frm wide to long, actually contains records even for non-sample years for all unique ids
// so there are 42 records per unique id and it is keyed on unique id and survey_yr

********************************************************************************
**# Need to figure out if I can get true relationship start and end dates for
*** cohabitations that started during panel
*** Won't work with family matrix bc only in that file if in sample BUT we
*** need that file bc contains info on cohab, so need to merge info
********************************************************************************
// can loosely use the life course cohab history (though not sure that incorporates family matrix)
use "$temp/PSID_full_long.dta", clear // created in step 1
	// this file also keyed on unique_id and survey_yr, and has 42 records per unique id
	// key difference - there are more unique IDs here, for two reasons: 1. includes people never in sample and 2. above only has people ever partnered
egen wave = group(survey_yr) // this will make years consecutive, easier for laters

merge 1:1 unique_id survey_yr using "$temp/PSID_relationship_list_tomatch.dta", keepusing(MX8 ego_rel alter_rel partner_unique_id rel_num marr_num)
tab MARITAL_PAIRS_ _merge, m // so those in marital pairs have basically perfect match
drop if _merge==2
drop _merge

rename MX8 rel_type

gen has_psid_gene=0
replace has_psid_gene = 1 if inlist(SAMPLE,1,2)

gen in_sample=.
replace in_sample=0 if SEQ_NUMBER_==0 | inrange(SEQ_NUMBER_,60,90)
replace in_sample=1 if inrange(SEQ_NUMBER_,1,59)
replace in_sample=0 if survey_yr==1968 & RELATION_==0 // no seq number in 1968
replace in_sample=1 if survey_yr==1968 & RELATION_!=0 // no seq number in 1968

label define sample 0 "not sample" 1 "original sample" 2 "born-in" 3 "moved in" 4 "joint inclusion" 5 "followable nonsample parent" 6 "nonsample elderly"
label values SAMPLE sample

gen hh_status_=.
replace hh_status_=0 if SEQ_NUMBER_==0 
replace hh_status_=0 if survey_yr==1968 & RELATION_==0 // no seq number in 1968
replace hh_status_=1 if inrange(SEQ_NUMBER_,1,20) // in sample
replace hh_status_=1 if survey_yr==1968 & inrange(RELATION_,1,9) // no seq number in 1968
replace hh_status_=2 if inrange(SEQ_NUMBER_,51,59) // institutionalized
replace hh_status_=3 if inrange(SEQ_NUMBER_,71,80) // new HH 
replace hh_status_=4 if inrange(SEQ_NUMBER_,81,89) // died
label define hh_status 0 "not in sample" 1 "in sample" 2 "institutionalized" 3 "new hh" 4 "died"
label values hh_status_ hh_status

gen relationship=.
replace relationship=0 if RELATION_==0
replace relationship=1 if inlist(RELATION_,1,10)
replace relationship=2 if inlist(RELATION_,2,20,22,88)
replace relationship=3 if inrange(RELATION_,23,87) | inrange(RELATION_,90,98) | inrange(RELATION_,3,9)
label define relationship 0 "not in sample" 1 "head" 2 "partner" 3 "other"
label values relationship relationship

gen partnered=.
replace partnered=0 if in_sample==1 & MARITAL_PAIRS_==0
replace partnered=1 if in_sample==1 & inrange(MARITAL_PAIRS_,1,4)

tab relationship rel_type, m
tab partnered rel_type, m
tab in_sample rel_type, m
replace rel_type = 0 if rel_type==. & in_sample==1 // only want to identify relationship transitions if in sample bc if it's dropout or entrance, we don't know true info

bysort unique_id: egen first_survey_yr= min(survey_yr) if in_sample==1
bysort unique_id (first_survey_yr): replace first_survey_yr=first_survey_yr[1]
tab first_survey_yr, m
bysort unique_id: egen last_survey_yr= max(survey_yr) if in_sample==1
bysort unique_id (last_survey_yr): replace last_survey_yr=last_survey_yr[1]
tab last_survey_yr, m

sort unique_id survey_yr
browse unique_id survey_yr first_survey_yr last_survey_yr has_psid_gene in_sample hh_status SAMPLE partnered rel_type relationship partner_unique_id
// browse unique_id survey_yr first_survey_yr last_survey_yr has_psid_gene in_sample hh_status SAMPLE partnered rel_type relationship partner_unique_id if partnered==0 & rel_type!=.
// browse unique_id survey_yr first_survey_yr last_survey_yr has_psid_gene in_sample hh_status SAMPLE partnered rel_type relationship partner_unique_id if inlist(unique_id, 4006, 4170, 4041, 4207, 57183, 57030, 5971170, 5971021) 
// ah okay, these are mostly first-yr cohabitors - so these are identified as partners in the matrix, but not in main file. this might be problematic bc the PSID doesn't collect any info on these people... BUT this is the true rel start, then ...
tab RELATION_ if partnered==0 & rel_type!=.

// relationship transitions - OBSERVED
sort unique_id wave
// start rel - observed
gen rel_start=0
replace rel_start=1 if inlist(rel_type,20,22) & rel_type[_n-1]==0 & unique_id==unique_id[_n-1] & wave==wave[_n-1]+1

gen marriage_start=0 // from unpartnered, NOT cohabiting
replace marriage_start=1 if rel_type==20 & rel_type[_n-1]==0 & unique_id==unique_id[_n-1] & wave==wave[_n-1]+1

gen cohab_start=0
replace cohab_start=1 if rel_type==22 & rel_type[_n-1]==0 & unique_id==unique_id[_n-1] & wave==wave[_n-1]+1

// end rel
gen rel_end=0
replace rel_end=1 if inlist(rel_type,20,22) & rel_type[_n+1]==0 & unique_id==unique_id[_n+1] & wave==wave[_n+1]-1

gen marriage_end=0
replace marriage_end=1 if rel_type==20 & rel_type[_n+1]==0 & unique_id==unique_id[_n+1] & wave==wave[_n+1]-1

gen cohab_end=0
replace cohab_end=1 if rel_type==22 & rel_type[_n+1]==0 & unique_id==unique_id[_n+1] & wave==wave[_n+1]-1

browse unique_id survey_yr rel_start marriage_start cohab_start rel_end marriage_end cohab_end first_survey_yr last_survey_yr has_psid_gene in_sample hh_status SAMPLE partnered rel_type relationship partner_unique_id yr_married1 yr_married2 yr_married3

// now turn these into years - let's start with all relationships
gen entered_in_rel = .
replace entered_in_rel = 0 if survey_yr == first_survey_yr & rel_type==0
replace entered_in_rel = 1 if survey_yr == first_survey_yr & inlist(rel_type,20,22)
	// think I need to fill these in temporarily but want a flag
	bysort unique_id (entered_in_rel): replace entered_in_rel=entered_in_rel[1]
		
gen relationship_start = survey_yr if rel_start==1
replace relationship_start = survey_yr if relationship_start==. & survey_yr == first_survey_yr & inlist(rel_type,20,22)
gen relationship_end = survey_yr if rel_end==1

bysort unique_id: egen relno=rank(relationship_start)
tab relno, m
bysort unique_id: egen exitno=rank(relationship_end)
tab exitno, m

forvalues r=1/6{
	gen rel`r'_start=.
	replace rel`r'_start=relationship_start if relno==`r' 
	bysort unique_id (rel`r'_start): replace rel`r'_start=rel`r'_start[1]

	gen rel`r'_end=.
	replace rel`r'_end=relationship_end if exitno==`r'
	bysort unique_id (rel`r'_end): replace rel`r'_end=rel`r'_end[1]
	
	gen rel`r'_type=.
	replace rel`r'_type = rel_type if relno==`r'
	bysort unique_id (rel`r'_type): replace rel`r'_type=rel`r'_type[1]	
}

sort unique_id survey_yr
browse unique_id partner_unique_id survey_yr in_sample hh_status partnered rel_type rel1_start rel1_end rel2_start rel2_end rel3_start rel3_end yr_married1 yr_end1 yr_married2 yr_end2 yr_married3 yr_end3 rel_start marriage_start cohab_start rel_end marriage_end cohab_end has_psid_gene 

tab rel1_start rel1_type, m

// just marriage
gen marriage_start_yr = survey_yr if marriage_start==1
replace marriage_start_yr = survey_yr if survey_yr == first_survey_yr & rel_type==20
gen marriage_end_yr = survey_yr if marriage_end==1

bysort unique_id: egen marrno=rank(marriage_start_yr)
tab marrno, m
bysort unique_id: egen marr_exitno=rank(marriage_end_yr)
tab marr_exitno, m

forvalues r=1/6{
	gen marr`r'_start=.
	replace marr`r'_start=marriage_start_yr if marrno==`r' 
	bysort unique_id (marr`r'_start): replace marr`r'_start=marr`r'_start[1]

	gen marr`r'_end=.
	replace marr`r'_end=marriage_end_yr if marr_exitno==`r'
	bysort unique_id (marr`r'_end): replace marr`r'_end=marr`r'_end[1]
}

// just cohabitation
gen cohab_start_yr = survey_yr if cohab_start==1
replace cohab_start_yr = survey_yr if survey_yr == first_survey_yr & rel_type==22
gen cohab_end_yr = survey_yr if cohab_end==1

bysort unique_id: egen cohno=rank(cohab_start_yr)
tab cohno, m
bysort unique_id: egen coh_exitno=rank(cohab_end_yr)
tab coh_exitno, m

forvalues r=1/6{
	gen coh`r'_start=.
	replace coh`r'_start=cohab_start_yr if cohno==`r' 
	bysort unique_id (coh`r'_start): replace coh`r'_start=coh`r'_start[1]

	gen coh`r'_end=.
	replace coh`r'_end=cohab_end_yr if coh_exitno==`r'
	bysort unique_id (coh`r'_end): replace coh`r'_end=coh`r'_end[1]
}

sort unique_id survey_yr
browse unique_id partner_unique_id survey_yr in_sample partnered rel_type rel1_start rel1_end rel2_start rel2_end rel3_start rel3_end marr1_start marr1_end marr2_start marr2_end coh1_start coh1_end coh2_start coh2_end yr_married1 yr_end1 yr_married2 yr_end2 yr_married3 yr_end3 rel_start marriage_start cohab_start rel_end marriage_end cohab_end has_psid_gene 

********************************************************************************
* Just get per unique
********************************************************************************
preserve

collapse 	(mean) rel1_start rel2_start rel3_start rel4_start rel5_start rel6_start rel1_end rel2_end rel3_end rel4_end rel5_end rel6_end rel1_type rel2_type rel3_type rel4_type rel5_type rel6_type /// created rel variables
					marr1_start marr2_start marr3_start marr4_start marr5_start marr6_start marr1_end marr2_end marr3_end marr4_end marr5_end marr6_end ///
					coh1_start coh2_start coh3_start coh4_start coh5_start coh6_start coh1_end coh2_end coh3_end coh4_end coh5_end coh6_end ///
					yr_married1 yr_married2 yr_married3 yr_married4 yr_married5 yr_married6 yr_married7 yr_married8 yr_married9 yr_married12 yr_married13 /// marital history variables
					yr_end1 yr_end2 yr_end3 yr_end4 yr_end5 yr_end6 yr_end7 yr_end8 yr_end9 yr_end12 yr_end13  ///
					status1 status2 status3 status4 status5 status6 status7 status8 status9 status12 status13 ///
					first_survey_yr last_survey_yr YR_NONRESPONSE_FIRST YR_NONRESPONSE_RECENT ///
			(max) 	partnered relno marrno cohno /// get a sense of ever partnered
, by(unique_id has_psid_gene in_marital_history num_marriages entered_in_rel)

gen partner_unique_id = unique_id // for later matching

save "$temp/psid_relationship_history.dta", replace

restore

********************************************************************************
* Now attempt to create master history (follow either GSOEP or my old code here)
********************************************************************************
use "$temp/psid_relationship_history.dta", clear

tab rel1_start partnered, m // do most ever partnered people at least have rel1 start date? yes 

// what both of those start with is reshaping to long, but on relationship number. an interesting approach I took in old file is, instead of leave in separate columns (as I do for GSOEP also), I move these created variables into marital history.
// I do wonder - can I do this? so if IN marital history - i prioritize your marital history marriage info and my cohab info (so put them into same column and then rerank so in order). if not in marital history, I actually just use the entire created history? (already in order?). let's reshape as is to explore, but may revisit

// so there are three scenarios. let's actually create variables to flag which one they are in...
tab cohno, m

gen ever_cohab = .
replace ever_cohab = 0 if cohno==.
replace ever_cohab = 1 if inrange(cohno,1,10)

tab in_marital_history ever_cohab, m
gen history_flag = .
replace history_flag = 1 if in_marital_history==1 & ever_cohab==0
replace history_flag = 2 if in_marital_history==1 & ever_cohab==1
replace history_flag = 3 if in_marital_history==0

label define history_flag 1 "History, No Cohab" 2 "History, Cohab" 3 "No History"
label values history_flag history_flag
tab history_flag, m

// 1a. In marital history, no cohabs observed: use marital history as is
// 1b. In marital history, observed at least one cohab: add cohab info to marital history
// 2. Not in marital history, doesn't really matter if have cohab or not - use my rel variables?

reshape long rel@_start rel@_end rel@_type marr@_start marr@_end coh@_start coh@_end ///
yr_married yr_end status, i(unique_id) j(relationship)

tab num_marriages marrno
tab cohno, m

browse unique_id history_flag relno relationship rel_start rel_end rel_type yr_married yr_end marr_start marr_end coh_start coh_end
// 4006 good example of history no cohab - my marriage dates off by 1 year, so need to use real marital history data but my cohab data
// and 4008 - there is a marriage in history that I do not even observe
// 2156002 good example of marriage in history who enters in rel, need marital history for true start date

// okay, think I like the approach of putting all in same column, but want to create a few things while long
rename yr_married mh_yr_married
rename yr_end mh_yr_end
rename status mh_status
gen mh_rel_type = .
replace mh_rel_type = 20 if mh_yr_married!=.

reshape wide rel@_start rel@_end rel@_type marr@_start marr@_end coh@_start coh@_end ///
mh_yr_married mh_yr_end mh_status mh_rel_type, i(unique_id) j(relationship)

// create master indicator that is based on above rel type
forvalues r=1/9{
	gen master_rel_start`r'=.
	gen master_rel_end`r'=.
	gen master_rel_type`r'=.
	
	replace master_rel_start`r' = mh_yr_married`r' if in_marital_history==1 // start here - because I will still fill in marriages as 1-13 for those in history, i'll just ADD ON cohab
	replace master_rel_end`r' = mh_yr_end`r' if in_marital_history==1 // start here - because I will still fill in marriages as 1-13 for those in history, i'll just ADD ON cohab
	replace master_rel_type`r' = mh_rel_type`r' if in_marital_history==1 // start here - because I will still fill in marriages as 1-13 for those in history, i'll just ADD ON cohab

	replace master_rel_start`r' = rel`r'_start if in_marital_history==0
	replace master_rel_end`r' = rel`r'_end if in_marital_history==0
	replace master_rel_type`r' = rel`r'_type if in_marital_history==0	
}

forvalues r=12/13{
	gen master_rel_start`r'=.
	gen master_rel_end`r'=.
	gen master_rel_type`r'=.
	
	replace master_rel_start`r' = mh_yr_married`r' if in_marital_history==1 // start here - because I will still fill in marriages as 1-13 for those in history, i'll just ADD ON cohab
	replace master_rel_end`r' = mh_yr_end`r' if in_marital_history==1 // start here - because I will still fill in marriages as 1-13 for those in history, i'll just ADD ON cohab
	replace master_rel_type`r' = mh_rel_type`r' if in_marital_history==1 // start here - because I will still fill in marriages as 1-13 for those in history, i'll just ADD ON cohab

	replace master_rel_start`r' = rel`r'_start if in_marital_history==0
	replace master_rel_end`r' = rel`r'_end if in_marital_history==0
	replace master_rel_type`r' = rel`r'_type if in_marital_history==0	
}


gen master_rel_start14 = coh1_start if history_flag==2
gen master_rel_start15 = coh2_start if history_flag==2
gen master_rel_start16 = coh3_start if history_flag==2
gen master_rel_start17 = coh4_start if history_flag==2
gen master_rel_start18 = coh5_start if history_flag==2
gen master_rel_start19 = coh6_start if history_flag==2

gen master_rel_end14 = coh1_end if history_flag==2
gen master_rel_end15 = coh2_end if history_flag==2
gen master_rel_end16 = coh3_end if history_flag==2
gen master_rel_end17 = coh4_end if history_flag==2
gen master_rel_end18 = coh5_end if history_flag==2
gen master_rel_end19 = coh6_end if history_flag==2

gen master_rel_type14 = 22 if history_flag==2 & coh1_start != .
gen master_rel_type15 = 22 if history_flag==2 & coh2_start != .
gen master_rel_type16 = 22 if history_flag==2 & coh3_start != .
gen master_rel_type17 = 22 if history_flag==2 & coh4_start != .
gen master_rel_type18 = 22 if history_flag==2 & coh5_start != .
gen master_rel_type19 = 22 if history_flag==2 & coh6_start != .

label define type 20 "Marriage" 22 "Cohab"
forvalues r=1/19{
	capture label values master_rel_type`r' type 
}

browse unique_id history_flag master_rel_start* master_rel_type*

reshape long master_rel_start master_rel_end master_rel_type ///
rel@_start rel@_end rel@_type marr@_start marr@_end coh@_start coh@_end ///
mh_yr_married mh_yr_end mh_status mh_rel_type, i(unique_id) j(relationship)

browse unique_id relationship history_flag master_rel_start master_rel_end master_rel_type

// now actually rank relationships (following x_create_cohab_sample)
gen master_rel_end_orig=master_rel_end
replace master_rel_end=9999 if master_rel_end==. & master_rel_start!=.
drop if master_rel_start==9998

by unique_id: egen rank_start = rank(master_rel_start)
by unique_id: egen rank_end = rank(master_rel_end)
by unique_id: egen max_rank_start = max(rank_start)
// egen rank_avg = rowmean(rank_start rank_end)
// by unique_id: egen rank = rank(master_rel_start master_rel_end)

browse unique_id relationship history_flag max_rank_start rank_start rank_end master_rel_start master_rel_end master_rel_type
// browse unique_id relationship history_flag max_rank_start rank_start rank_end master_rel_start master_rel_end master_rel_type if rank_start==2.5
// browse unique_id relationship history_flag max_rank_start rank_start rank_end master_rel_start master_rel_end master_rel_type if inlist(unique_id,5186, 351183)

// I want to prio start date, then end date, and i want to break ties using rel type...
gen rel_rank = .
replace rel_rank = rank_start if inlist(rank_start, 1,2,3,4,5,6,7,8,9,10) // whole numbers
replace rel_rank = 1 if rank_start==1.5 & master_rel_type==22 // prio cohab
replace rel_rank = 2 if rank_start==1.5 & master_rel_type==20

forvalues y=2.5(1)7.5{
	local a = `y' - 0.5
	local b = `y' + 0.5
	replace rel_rank = `a' if rank_start==`y' & master_rel_type==22 // prio cohab
	replace rel_rank = `b' if rank_start==`y' & master_rel_type==20
}

inspect rank_start
inspect rel_rank
tab rank_start rel_rank, m
tab rel_rank rank_end, m

browse unique_id relationship history_flag rel_rank rank_start rank_end master_rel_start master_rel_end master_rel_type
// browse unique_id relationship history_flag rel_rank rank_start rank_end master_rel_start master_rel_end master_rel_type if rel_rank==. & rank_end!=.
// browse unique_id relationship history_flag rel_rank rank_start rank_end master_rel_start master_rel_end master_rel_type if inlist(unique_id, 1335034, 2432031, 5548010)

replace rel_rank = rank_end if rel_rank==. & inlist(rank_end, 1,2,3,4,5,6,7,8,9,10) // e.g. 4034

tab master_rel_start rel_rank, m
drop if rel_rank==. // let's get rid of these extra rows

bysort unique_id rel_rank: egen duplicate_rank = count(rel_rank)
tab duplicate_rank, m 
// browse unique_id entered_in_rel history_flag rel_rank rank_start rank_end master_rel_start master_rel_end master_rel_type if duplicate_rank==2
// browse unique_id entered_in_rel history_flag rel_rank rank_start rank_end master_rel_start master_rel_end master_rel_type if inlist(unique_id,105181,546003,931174,1480001,1530001,1684195,2001001,2177003,2194021,2269002,2288002,2435183,5419002,5539172,5712173,5747002)
// tab master_rel_start if duplicate_rank>1, m // yeah, these are mostly 9998. Actually just drop those? I did that in other version of this file // fixed this
replace rel_rank = rank_end if duplicate_rank==2 // this should work
bysort unique_id rel_rank: egen duplicate_rank_v2 = count(rel_rank)
replace rel_rank = rank_end if duplicate_rank_v2==2

keep unique_id entered_in_rel history_flag rel_rank  master_rel_start master_rel_end master_rel_type
sort unique_id rel_rank

reshape wide master_rel_start master_rel_end master_rel_type, i(unique_id) j(rel_rank)

save "$created_data/psid_master_relationship_history_wide.dta", replace
