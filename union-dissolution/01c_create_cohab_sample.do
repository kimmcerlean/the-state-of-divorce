********************************************************************************
* Getting PSID sample for union dissolution
* create_cohab_sample.do
* Kim McErlean
********************************************************************************
// This file is adapted from 1a - need to ensure I included cohabitation properly, not just marriage. Still unsure if "marital" history includes cohabitations

use "$data_tmp\PSID_full_long.dta", clear // created in other step 1

// need to replace all "id" with INTERVIEW_NUM_, okay wait no unique ID
// BUT interview_num_ to match later

********************************************************************************
********************************************************************************
* APPROACH 1
* First, just attempting to create relationship file for all relationships,
* marriages and cohabitations
********************************************************************************
********************************************************************************

browse survey_yr main_per_id id unique_id INTERVIEW_NUM_ SEQ_NUMBER_ RELATION_ FIRST_MARRIAGE_YR_START MARITAL_PAIRS_

// drop if survey_yr <1983 // first time you could identify cohab

gen relationship=0
replace relationship=1 if inrange(MARITAL_PAIRS_,1,4)

browse survey_yr unique_id main_per_id SEQ_NUMBER_ relationship RELATION_ FIRST_MARRIAGE_YR_START 

bysort unique_id (SEQ_NUMBER_): egen in_sample=max(SEQ_NUMBER_)

drop if in_sample==0 // people with NO DATA in any year
drop if SEQ_NUMBER_==0 // won't have data because not in that year -- like SIPP, how do I know if last year is because divorced or last year in sample? right now individual level file, so fine - this is JUST last year in sample at the moment

browse survey_yr INTERVIEW_NUM_ main_per_id relationship RELATION_ FIRST_MARRIAGE_YR_START FIRST_MARRIAGE_YR_HEAD_ FIRST_MARRIAGE_YR_END RECENT_MARRIAGE_YR_START 

browse id survey_yr relationship MARITAL_STATUS_HEAD_
gen relationship_yr = survey_yr if relationship==1
sort id survey_yr
gen enter_rel=0
replace enter_rel=1 if relationship==1 & relationship[_n-1]==0 & unique_id==unique_id[_n-1]
replace enter_rel=1 if relationship_yr==1968 // since can't transition, but call this "built relationship 1"

gen exit_rel=0
sort id survey_yr
replace exit_rel=1 if relationship==1 & relationship[_n+1]==0 & unique_id==unique_id[_n+1]
// replace exit_rel=1 if relationship==0 & relationship[_n-1]==1 & id==id[_n-1]

browse unique_id survey_yr relationship enter_rel MARITAL_STATUS_HEAD_ exit_rel

gen relationship_start = survey_yr if enter_rel==1
bysort unique_id: egen built_rel_no=rank(relationship_start) // this is flawed if have multiple marriages prior to survey starting
browse unique_id survey_yr MARITAL_STATUS_HEAD_ enter_rel relationship_start FIRST_MARRIAGE_YR_START built_rel_no

gen built_start_rel1=.
// replace rel1_start = FIRST_MARRIAGE_YR_START if FIRST_MARRIAGE_YR_START <=2019
replace built_start_rel1=relationship_start if built_rel_no==1 // & FIRST_MARRIAGE_YR_START==9999
bysort unique_id (built_start_rel1): replace built_start_rel1=built_start_rel1[1]
gen built_start_rel2=.
replace built_start_rel2=relationship_start if built_rel_no==2 // & RECENT_MARRIAGE_YR_START ==9999
bysort unique_id (built_start_rel2): replace built_start_rel2=built_start_rel2[1]
gen built_start_rel3=.
replace built_start_rel3=relationship_start if built_rel_no==3
bysort unique_id (built_start_rel3): replace built_start_rel3=built_start_rel3[1]
gen built_start_rel4=.
replace built_start_rel4=relationship_start if built_rel_no==4
bysort unique_id (built_start_rel4): replace built_start_rel4=built_start_rel4[1]
gen built_start_rel5=.
replace built_start_rel5=relationship_start if built_rel_no==5
bysort unique_id (built_start_rel5): replace built_start_rel5=built_start_rel5[1]

sort unique_id survey_yr
browse unique_id survey_yr MARITAL_STATUS_HEAD_ enter_rel relationship_start FIRST_MARRIAGE_YR_START built_start_rel1 built_start_rel2 built_start_rel3 built_rel_no  yr_married1  yr_married2 yr_married3 yr_married4 in_marital_history MARITAL_PAIRS_ // id_spouse1 per_no_spouse1 id_spouse2 per_no_spouse2

gen relationship_end = survey_yr if exit_rel==1
bysort unique_id: egen built_exitno=rank(relationship_end)
browse unique_id survey_yr MARITAL_STATUS_HEAD_ enter_rel relationship_start built_exitno

gen built_end_rel1=.
replace built_end_rel1=relationship_end if built_exitno==1
bysort unique_id (built_end_rel1): replace built_end_rel1=built_end_rel1[1]
gen built_end_rel2=.
replace built_end_rel2=relationship_end if built_exitno==2
bysort unique_id (built_end_rel2): replace built_end_rel2=built_end_rel2[1]
gen built_end_rel3=.
replace built_end_rel3=relationship_end if built_exitno==3
bysort unique_id (built_end_rel3): replace built_end_rel3=built_end_rel3[1]
gen built_end_rel4=.
replace built_end_rel4=relationship_end if built_exitno==4
bysort unique_id (built_end_rel4): replace built_end_rel4=built_end_rel4[1]
gen built_end_rel5=.
replace built_end_rel5=relationship_end if built_exitno==5
bysort unique_id (built_end_rel5): replace built_end_rel5=built_end_rel5[1]

sort unique_id survey_yr
browse unique_id survey_yr relationship enter_rel exit_rel built_start_rel1 built_end_rel1 built_start_rel2 built_end_rel2
browse unique_id survey_yr built_start_rel1 built_end_rel1 built_start_rel2 built_end_rel2 in_marital_history yr_married1 status1 yr_end1 yr_married2 status2 yr_end2 yr_married3 status3 yr_end3 // figuring out how to combine gah

preserve

collapse (mean) built_start_rel1 built_start_rel2 built_start_rel3 built_start_rel4 built_start_rel5 built_end_rel1 built_end_rel2 built_end_rel3 built_end_rel4 built_end_rel5 yr_married1 yr_married2 yr_married3 yr_married4 yr_married5 yr_married6 yr_married7 yr_married8 yr_married9 yr_end1 yr_end2 yr_end3 yr_end4 yr_end5 yr_end6 yr_end7 yr_end8 yr_end9 status1 status2 status3 status4 status5 status6 status7 status8 status9, by(unique_id)

// reshape long built_start_rel built_end_rel yr_married yr_end status, i(unique_id) j(relationship)
// okay moving the built variables to slots 10-15

gen yr_married11 = built_start_rel1
gen yr_married12 = built_start_rel2
gen yr_married13 = built_start_rel3
gen yr_married14 = built_start_rel4
gen yr_married15 = built_start_rel5

gen yr_end11 = built_end_rel1
gen yr_end12 = built_end_rel2
gen yr_end13 = built_end_rel3
gen yr_end14 = built_end_rel4
gen yr_end15 = built_end_rel5

drop built_start_rel*  built_end_rel*
reshape long yr_married yr_end status, i(unique_id) j(relationship)

drop if yr_married==.
drop if yr_married==9998 | yr_end==9998 // dk

gen yr_end_full=yr_end
replace yr_end=9999 if yr_end==. // so it will rank (and in marital history, this is what is given)

by unique_id: egen rank_start = rank(yr_married)
by unique_id: egen rank_end = rank(yr_end)
egen rank_avg = rowmean(rank_start rank_end)

sort unique_id rank_avg relationship 

// so put them all at halfway point if like 1.25/1.75
recode rank_avg (1.25/1.75=1.5) (2.25/2.75=2.5) (3.25/3.75=3.5) (4.25/4.75=4.5) (5.25/5.75=5.5) (6.25/6.75=6.5) (7.25/7.75=7.5) (8.25/8.75=8.5) (9.25/9.75=9.5), gen(rank_avg_dup)

*https://www.stata.com/support/faqs/data-management/duplicate-observations/
// by unique_id rank_avg:  gen dup = cond(_N==1,0,_n) // this is mostly good and then can drop dup==2, there are a few non-integers that aren't duplicates need to figure those out
sort unique_id rank_avg_dup relationship 
by unique_id rank_avg_dup:  gen dup = cond(_N==1,0,_n)

drop if dup>=2

* still a few not catching if years are off bc of recorded date v. where it is recorded when I subtract.
* think it mostly affects those that have a maximum rank of 2 - bc it's one relationship, but looks like 2 because of dates?
by unique_id: egen rank_max = max(rank_avg)
gen flag=0
replace flag=1 if rank_max==2

gen same_rel=0
replace same_rel=1 if (yr_married==yr_married[_n-1]+1 | yr_married==yr_married[_n-1]+2) & (yr_end==yr_end[_n-1]+1 | yr_end==yr_end[_n-1]+2) & unique_id==unique_id[_n-1] // & flag==1
replace same_rel=1 if (yr_married==yr_married[_n+1]-1 | yr_married==yr_married[_n+1]-2) & (yr_end==yr_end[_n+1]-1 | yr_end==yr_end[_n+1]-2) & unique_id==unique_id[_n+1] // & flag==1

* want to keep the marital history one? so
drop if same_rel==1 & status==. // bc means not in marital history. will there always be one in marital history? it seems

* need to rerank
by unique_id: egen relationship_real = rank(yr_married)
browse unique_id relationship relationship_real yr_married yr_end status
drop if status==. & inlist(relationship_real,1.5,2.5,3.5,4.5,5.5,6.5) // intact, but I had them ending (prob bc censoring) so using the one attached to marital history

drop relationship_real
by unique_id: egen relationship_real = rank(yr_married), unique // few people started, ended, started all in same year

// need to rename bc the current variables are named yr_married and yr_end
gen relationship_start = yr_married
gen relationship_end = yr_end
gen relationship_status = status

keep unique_id relationship_start relationship_end relationship_status relationship_real
reshape wide relationship_start relationship_end relationship_status , i(unique_id) j(relationship_real)

save "$data_keep\PSID_union_history_created.dta", replace

/*
this only works for marriages
replace rel1_start=yr_married1 if in_marital_history==1
replace rel2_start=yr_married2 if in_marital_history==1
replace rel3_start=yr_married3 if in_marital_history==1
replace rel4_start=yr_married4 if in_marital_history==1
replace rel1_end=yr_end1 if in_marital_history==1
replace rel2_end=yr_end2 if in_marital_history==1
replace rel3_end=yr_end3 if in_marital_history==1
replace rel4_end=yr_end4 if in_marital_history==1

browse unique_id survey_yr rel1_start rel1_end rel2_start rel2_end in_marital_history yr_married1 status1 yr_end1 yr_married2 status2 yr_end2 // figuring out how to combine gah
replace rel1_end=. if rel1_end==9999
replace rel2_end=. if rel2_end==9999
replace rel3_end=. if rel3_end==9999
replace rel4_end=. if rel4_end==9999
*/

********************************************************************************
**# Now, merge relationship information back on file 
** 	so can create information I need
********************************************************************************
* restore // will this work? or just start from first file?

use "$data_tmp\PSID_full_long.dta", clear // created in other step 1

gen relationship=0
replace relationship=1 if inrange(MARITAL_PAIRS_,1,4)

bysort unique_id (SEQ_NUMBER_): egen in_sample=max(SEQ_NUMBER_)
drop if in_sample==0 // people with NO DATA in any year
drop if SEQ_NUMBER_==0 // won't have data because not in that year -- like SIPP, how do I know if last year is because divorced or last year in sample? right now individual level file, so fine - this is JUST last year in sample at the moment

merge m:1 unique_id using "$data_keep\PSID_union_history_created.dta" // I think people not matched in master are those with NO relationships. that would make sense because I deleted them frm the file.

gen rel_start_all=.
replace rel_start_all = relationship_start1 if survey_yr>=relationship_start1 & survey_yr <=relationship_end1
replace rel_start_all = relationship_start2 if survey_yr>=relationship_start2 & survey_yr <=relationship_end2
replace rel_start_all = relationship_start3 if survey_yr>=relationship_start3 & survey_yr <=relationship_end3
replace rel_start_all = relationship_start4 if survey_yr>=relationship_start4 & survey_yr <=relationship_end4
replace rel_start_all = relationship_start5 if survey_yr>=relationship_start5 & survey_yr <=relationship_end5
replace rel_start_all = relationship_start6 if survey_yr>=relationship_start6 & survey_yr <=relationship_end6
replace rel_start_all = relationship_start7 if survey_yr>=relationship_start7 & survey_yr <=relationship_end7
replace rel_start_all = relationship_start8 if survey_yr>=relationship_start8 & survey_yr <=relationship_end8

gen rel_end_all=.
replace rel_end_all = relationship_end1 if survey_yr>=relationship_start1 & survey_yr <=relationship_end1
replace rel_end_all = relationship_end2 if survey_yr>=relationship_start2 & survey_yr <=relationship_end2
replace rel_end_all = relationship_end3 if survey_yr>=relationship_start3 & survey_yr <=relationship_end3
replace rel_end_all = relationship_end4 if survey_yr>=relationship_start4 & survey_yr <=relationship_end4
replace rel_end_all = relationship_end5 if survey_yr>=relationship_start5 & survey_yr <=relationship_end5
replace rel_end_all = relationship_end6 if survey_yr>=relationship_start6 & survey_yr <=relationship_end6
replace rel_end_all = relationship_end7 if survey_yr>=relationship_start7 & survey_yr <=relationship_end7
replace rel_end_all = relationship_end8 if survey_yr>=relationship_start8 & survey_yr <=relationship_end8

gen status_all=.
replace status_all = relationship_status1 if survey_yr>=relationship_start1 & survey_yr <=relationship_end1
replace status_all = relationship_status2 if survey_yr>=relationship_start2 & survey_yr <=relationship_end2
replace status_all = relationship_status3 if survey_yr>=relationship_start3 & survey_yr <=relationship_end3
replace status_all = relationship_status4 if survey_yr>=relationship_start4 & survey_yr <=relationship_end4
replace status_all = relationship_status5 if survey_yr>=relationship_start5 & survey_yr <=relationship_end5
replace status_all = relationship_status6 if survey_yr>=relationship_start6 & survey_yr <=relationship_end6
replace status_all = relationship_status7 if survey_yr>=relationship_start7 & survey_yr <=relationship_end7
replace status_all = relationship_status8 if survey_yr>=relationship_start8 & survey_yr <=relationship_end8

label values status_all status

gen relationship_order=.
forvalues r=1/8{
	replace relationship_order=`r' if survey_yr>=relationship_start`r' & survey_yr <=relationship_end`r'
}

browse unique_id survey_yr MARITAL_PAIRS_ relationship relationship_order rel_start_all rel_end_all relationship_start1 relationship_end1 relationship_start2 relationship_start2 relationship_status1

/* 
**also only works for marriage, but might need this info at some point halp**
gen spouse_id_all=.
replace spouse_id_all = id_spouse1 if survey_yr>=rel1_start & survey_yr <=rel1_end
replace spouse_id_all = id_spouse2 if survey_yr>=rel2_start & survey_yr <=rel2_end
replace spouse_id_all = id_spouse3 if survey_yr>=rel3_start & survey_yr <=rel3_end
replace spouse_id_all = id_spouse4 if survey_yr>=rel4_end & survey_yr <=rel4_end

gen spouse_per_num_all=.
replace spouse_per_num_all = per_no_spouse1 if survey_yr>=rel1_start & survey_yr <=rel1_end
replace spouse_per_num_all = per_no_spouse2 if survey_yr>=rel2_start & survey_yr <=rel2_end
replace spouse_per_num_all = per_no_spouse3 if survey_yr>=rel3_start & survey_yr <=rel3_end
replace spouse_per_num_all = per_no_spouse4 if survey_yr>=rel4_end & survey_yr <=rel4_end
*/

bysort unique_id: egen first_survey_yr = min(survey_yr)
bysort unique_id: egen last_survey_yr = max(survey_yr)

sort unique_id survey_yr
browse unique_id survey_yr FAMILY_INTERVIEW_NUM_ main_per_id rel_start_all rel_end_all status_all last_survey_yr MARRIAGE_UPDATE MARITAL_STATUS_REF_ MARITAL_STATUS_HEAD_ // exit_rel X1968_PERSON_NUM_

//// stopped here because very confused; think I need to match couples? did I ever do that later? or no bc all the information is stored against head / spouse anyway?
/*
**This all matches 1a - removing rest for now, because think I need a new approach**
gen dissolve=0
replace dissolve=1 if survey_yr >=rel_end_all & (inrange(status_all,4,7) & in_marital_history==1)
replace dissolve=1 if exit_rel==1 & inlist(MARITAL_STATUS_HEAD_[_n+1],2,4,5) & unique_id == unique_id[_n+1] & in_marital_history==0
replace dissolve=1 if exit_rel==1 & (inrange(status_all,4,7) & in_marital_history==1)
replace dissolve=1 if exit_rel[_n+1]==1 & dissolve[_n+1]==0 & (inrange(status_all,4,7) & in_marital_history==1) & unique_id==unique_id[_n+1]

browse id survey_yr relationship rel_start_all rel_end_all dissolve exit_rel status_all MARITAL_STATUS_HEAD_ if inlist(id,2009,2986,2992) // so the survey yr GREATER than part isn't working for people who dissolve in an off year - like 2008. so 2007 not getting flagged as end? 
browse id survey_yr relationship rel_start_all rel_end_all dissolve exit_rel status_all MARITAL_STATUS_HEAD_ if id==2009

sort id survey_yr
browse id survey_yr relationship rel_start_all rel_end_all dissolve exit_rel status_all MARITAL_STATUS_HEAD_

********************************************************************************
* Restrict to anyone in a relationship
* From FAQ:
* As a corollary, to select individuals who have been either Reference Persons or Spouses/Partners, yearly Sequence Numbers must equal 1 or 2 and yearly Relationships to Reference Person must be in the range 1, 2, 10, 20, or 22. Once that subset is made and family data are merged, information about an individual can be found in Reference Person variables (Reference Person's work hours, Reference Person's labor income, etc.) when his or her Relationship to Reference Person=1 or 10. When Relationship to Reference Person is 2, 20, or 22, then his or her information is found in variables about the Spouse/Partner.
********************************************************************************
gen total_relationship=relationship
replace total_relationship=1 if dissolve==1
keep if total_relationship==1 
browse id survey_yr relationship rel_start_all rel_end_all dissolve exit_rel status_all MARITAL_STATUS_HEAD_
*/

********************************************************************************
********************************************************************************
**# APPROACH 2
* Start with relationship list, merge on rest of information
********************************************************************************
********************************************************************************
* First create a file with just the variables I want / need
use "$data_tmp\PSID_full_long.dta", clear // created in other step 1
drop MOTHER_MARITALSTATUSB1 MARRIAGE_UPDATE  COMPOSITION_CHANGE_ RESPONDENT_ ///
id_spouse6 per_no_spouse6 mo_married6 yr_married6 status6 yr_end6 id_spouse7 ///
per_no_spouse7 mo_married7 yr_married7 status7 yr_end7 id_spouse8 per_no_spouse8 mo_married8 yr_married8 status8 yr_end8 id_spouse9 /// 
per_no_spouse9 mo_married9 yr_married9 status9 yr_end9 id_spouse12 per_no_spouse12 mo_married12 yr_married12 status12 yr_end12 ///
id_spouse13 per_no_spouse13 mo_married13 yr_married13 status13 yr_end13 id_spouse98 per_no_spouse98 mo_married98 yr_married98 ///
status98 yr_end98 id_spouse99 per_no_spouse99 mo_married99 yr_married99 status99 yr_end99 releaseno RELEASE_NUM2_ STUDENT_

save "$data_tmp\PSID_long_tomerge.dta", replace

* Now merge
use "$data_tmp\PSID_relationship_list_tomatch.dta", clear // created in step 1b
rename year survey_yr

merge 1:1 main_per_id unique_id survey_yr using "$data_tmp\PSID_long_tomerge.dta"
drop if _merge==2
drop _merge

// want to get people's first and last year in survey (Based on seq_number), then remove records when not in survey
replace SEQ_NUMBER_=1 if survey_yr==1968 & RELATION_ !=0 // didn't exist as variable yet. wait so how do I know if people existed in 1968? this doesn't work
replace SEQ_NUMBER_=0 if survey_yr==1968 & RELATION_ ==0 
//  browse if inrange(SEQ_NUMBER_,71,80) // need to figure out if someone can be 71 in one HH but 1 in another, I am confused if these people should be in sample or not
browse unique_id survey_yr INTERVIEW_NUM_1968 FAMILY_INTERVIEW_NUM_ INTERVIEW_NUM_ main_per_id SEQ_NUMBER_ AGE_ if inlist(unique_id,2170, 4007, 7047) // need a variable to see what is being collected

gen in_sample = 0
replace in_sample=1 if inrange(SEQ_NUMBER_,1,20) // | inrange(SEQ_NUMBER_,71,80) - think I actually need to remove these people because don't have relationship matrix info

gen survey_yr_sample=.
replace survey_yr_sample = survey_yr if in_sample==1

bysort unique_id: egen first_survey_yr = min(survey_yr_sample)
bysort unique_id: egen last_survey_yr = max(survey_yr_sample)

browse unique_id main_per_id survey_yr SEQ_NUMBER_ survey_yr_sample first_survey_yr last_survey_yr  AGE_

keep if survey_yr>=first_survey_yr & survey_yr <=last_survey_yr

// start to get some relationship information and prep to drop non-relationship years
rename MX8 rel_type
browse main_per_id survey_yr unique_id partner_unique_id first_survey_yr last_survey_yr rel_type

/*
// Want to get number of relationships (and ideally, start and end years?)
// did this also at bottom of 1b and I think that one is more accurate
sort unique_id survey_yr
by unique_id: egen relationship_number = rank(partner_unique_id), track
*/

bysort unique_id rel_num: egen rel_start = min(survey_yr) if rel_num!=.
bysort unique_id rel_num: egen marriage_start = min(survey_yr) if rel_num!=. & rel_type==20
bysort unique_id rel_num: egen rel_end = max(survey_yr)  if rel_num!=.
bysort unique_id: egen first_rel_start = min(rel_start)
bysort unique_id: egen first_marr_start = min(marriage_start)

browse main_per_id survey_yr unique_id partner_unique_id rel_num rel_start marriage_start rel_end rel_type first_rel_start first_marr_start FIRST_MARRIAGE_YR_START yr_married1
gen master_marriage_start=FIRST_MARRIAGE_YR_START if FIRST_MARRIAGE_YR_START!=9999
replace master_marriage_start = first_marr_start if master_marriage_start==.

// okay I think I might need to use that other relationship file i made in 1b to get true start and end
merge m:1 unique_id using "$data_keep\PSID_union_history_created.dta"
drop if _merge==2 
drop _merge

gen rel_num_track=.
forvalues y=1/8{
	replace rel_num_track=`y' if survey_yr >=relationship_start`y' & survey_yr <=relationship_end`y'
}

browse unique_id partner_unique_id survey_yr  rel_num rel_num_track relationship_start1 relationship_start2 relationship_start3

gen start_this_rel=.
forvalues y=1/8{
	replace start_this_rel=relationship_start`y' if rel_num_track==`y'
}

browse unique_id partner_unique_id survey_yr rel_num start_this_rel relationship_start1 relationship_start2 relationship_start3

//validate
gen in_relationship=0
replace in_relationship=1 if partner_unique_id !=.

browse  main_per_id survey_yr unique_id partner_unique_id in_relationship MARITAL_PAIRS_ COUPLE_STATUS_REF_
tab in_relationship MARITAL_PAIRS_, m // general alignment. look at those where marital_pairs says yes but not in_relationship. first-year cohabitors
browse if in_relationship==0 &  MARITAL_PAIRS_!=0 // okay most of these are mover-outs (based on sequence). I think I maybe should have removed the mover outs? but have data on ref / head even if that person specifically not in household? BUT if I don't have relationship info, then useless, right?

save "$created_data\PSID_relationship_file.dta", replace

* move this to later - do eventually need to figure out how to only keep one respondent per relationship and ideally so all the genders align with head / wife but that part confuses me
/*
// first need to figure out how to keep only one respondent per HH. really doesn't matter gender of who I keep, because all variables are denoted by head / wife, NOT respondent.
bysort survey_yr FAMILY_INTERVIEW_NUM_ : egen per_id = rank(unique_id)
browse survey_yr FAMILY_INTERVIEW_NUM_  unique_id per_id

browse survey_yr FAMILY_INTERVIEW_NUM_ per_id unique_id if inlist(unique_id,12,13)

keep if per_id==1
*/

********************************************************************************
**# Recodes
* do this before I restrict sample further
********************************************************************************
// do I need to restrict to only people who are ref / ref wife or partner?

// education
browse survey_yr unique_id partner_unique_id EDUC1_WIFE_ EDUC_WIFE_ EDUC1_HEAD_ EDUC_HEAD_
// educ1 until 1990, but educ started 1975, okay but then a gap until 1991? wife not asked 1969-1971 - might be able to fill in if she is in sample either 1968 or 1972? (match to the id)
// codes are also different between the two, use educ1 until 1990, then educ 1991 post

recode EDUC1_WIFE_ (1/3=1)(4/5=2)(6=3)(7/8=4)(9=.)(0=1), gen(educ_wife_early)
recode EDUC1_HEAD_ (1/3=1)(4/5=2)(6=3)(7/8=4)(9=.)(0=1), gen(educ_head_early)
recode EDUC_WIFE_ (0/11=1) (12=2) (13/15=3) (16/17=4) (99=.), gen(educ_wife_1975)
recode EDUC_HEAD_ (0/11=1) (12=2) (13/15=3) (16/17=4) (99=.), gen(educ_head_1975)

label define educ 1 "LTHS" 2 "HS" 3 "Some College" 4 "College"
label values educ_wife_early educ_head_early educ_wife_1975 educ_head_1975 educ

gen educ_wife=.
replace educ_wife=educ_wife_early if inrange(survey_yr,1968,1990)
replace educ_wife=educ_wife_1975 if inrange(survey_yr,1991,2019)

gen educ_head=.
replace educ_head=educ_head_early if inrange(survey_yr,1968,1990)
replace educ_head=educ_head_1975 if inrange(survey_yr,1991,2019)

label values educ_wife educ_head educ

// browse survey_yr unique_id partner_unique_id RELATION_ educ_wife_early educ_wife_1975 educ_wife educ_head

	// trying to fill in missing wife years when possible
	browse id survey_yr educ_wife if inlist(id,3,12,25,117)
	bysort id (educ_wife): replace educ_wife=educ_wife[1] if educ_wife==.


gen college_complete_wife=0
replace college_complete_wife=1 if educ_wife==4
gen college_complete_head=0
replace college_complete_head=1 if educ_head==4

/*
gen couple_educ_gp=0
replace couple_educ_gp=1 if (college_complete_wife==1 | college_complete_head==1)

label define couple_educ 0 "Neither College" 1 "At Least One College"
label values couple_educ_gp couple_educ

gen educ_type=.
replace educ_type=1 if educ_head > educ_wife & educ_head!=. & educ_wife!=.
replace educ_type=2 if educ_head < educ_wife & educ_head!=. & educ_wife!=.
replace educ_type=3 if educ_head == educ_wife & educ_head!=. & educ_wife!=.

label define educ_type 1 "Hyper" 2 "Hypo" 3 "Homo"
label values educ_type educ_type
*/

// income / structure
browse id survey_yr FAMILY_INTERVIEW_NUM_ TAXABLE_HEAD_WIFE_ TOTAL_FAMILY_INCOME_ LABOR_INCOME_HEAD_ WAGES_HEAD_  LABOR_INCOME_WIFE_ WAGES_WIFE_ 

	// to use: WAGES_HEAD_ WAGES_WIFE_ -- wife not asked until 1993? okay labor income??
	// wages and labor income asked for head whole time. labor income wife 1968-1993, wages for wife, 1993 onwards

gen earnings_wife=.
replace earnings_wife = LABOR_INCOME_WIFE_ if inrange(survey_yr,1968,1993)
replace earnings_wife = WAGES_WIFE_ if inrange(survey_yr,1994,2019)

gen earnings_head=.
replace earnings_head = LABOR_INCOME_HEAD_ if inrange(survey_yr,1968,1993)
replace earnings_head = WAGES_HEAD_ if inrange(survey_yr,1994,2019)

/* moving this down because not right here
egen couple_earnings = rowtotal(earnings_wife earnings_head)

browse id survey_yr earnings_wife earnings_head couple_earnings

gen female_earn_pct = earnings_wife/(couple_earnings)

gen hh_earn_type=.
replace hh_earn_type=1 if female_earn_pct >=.4000 & female_earn_pct <=.6000
replace hh_earn_type=2 if female_earn_pct < .4000 & female_earn_pct >=0
replace hh_earn_type=3 if female_earn_pct > .6000 & female_earn_pct <=1
replace hh_earn_type=4 if earnings_head==0 & earnings_wife==0

label define hh_earn_type 1 "Dual Earner" 2 "Male BW" 3 "Female BW" 4 "No Earners"
label values hh_earn_type hh_earn_type

sort id survey_yr

gen hh_earn_type_lag=.
replace hh_earn_type_lag=hh_earn_type[_n-1] if unique_id==unique_id[_n-1]
label values hh_earn_type_lag hh_earn_type

gen female_earn_pct_lag=.
replace female_earn_pct_lag=female_earn_pct[_n-1] if unique_id==unique_id[_n-1]

browse id survey_yr earnings_head earnings_wife hh_earn_type hh_earn_type_lag
*/

// hours instead of earnings	
browse id survey_yr WEEKLY_HRS1_WIFE_ WEEKLY_HRS_WIFE_ WEEKLY_HRS1_HEAD_ WEEKLY_HRS_HEAD_

gen weekly_hrs_wife = .
replace weekly_hrs_wife = WEEKLY_HRS1_WIFE_ if survey_yr > 1969 & survey_yr <1994
replace weekly_hrs_wife = WEEKLY_HRS_WIFE_ if survey_yr >=1994
replace weekly_hrs_wife = 0 if inrange(survey_yr,1968,1969) & inlist(WEEKLY_HRS1_WIFE_,9,0)
replace weekly_hrs_wife = 10 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_WIFE_ ==1
replace weekly_hrs_wife = 27 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_WIFE_ ==2
replace weekly_hrs_wife = 35 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_WIFE_ ==3
replace weekly_hrs_wife = 40 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_WIFE_ ==4
replace weekly_hrs_wife = 45 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_WIFE_ ==5
replace weekly_hrs_wife = 48 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_WIFE_ ==6
replace weekly_hrs_wife = 55 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_WIFE_ ==7
replace weekly_hrs_wife = 60 if inrange(survey_yr,1968,1969)  & WEEKLY_HRS1_WIFE_ ==8

gen weekly_hrs_head = .
replace weekly_hrs_head = WEEKLY_HRS1_HEAD_ if survey_yr > 1969 & survey_yr <1994
replace weekly_hrs_head = WEEKLY_HRS_HEAD_ if survey_yr >=1994
replace weekly_hrs_head = 0 if inrange(survey_yr,1968,1969) & inlist(WEEKLY_HRS1_HEAD_,9,0)
replace weekly_hrs_head = 10 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_HEAD_ ==1
replace weekly_hrs_head = 27 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_HEAD_ ==2
replace weekly_hrs_head = 35 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_HEAD_ ==3
replace weekly_hrs_head = 40 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_HEAD_ ==4
replace weekly_hrs_head = 45 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_HEAD_ ==5
replace weekly_hrs_head = 48 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_HEAD_ ==6
replace weekly_hrs_head = 55 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_HEAD_ ==7
replace weekly_hrs_head = 60 if inrange(survey_yr,1968,1969)  & WEEKLY_HRS1_HEAD_ ==8

gen female_hours_pct = weekly_hrs_wife/(weekly_hrs_wife + weekly_hrs_head)

gen hh_hours_type=.
replace hh_hours_type=1 if female_hours_pct >=.4000 & female_hours_pct <=.6000
replace hh_hours_type=2 if female_hours_pct <.4000
replace hh_hours_type=3 if female_hours_pct >.6000 & female_hours_pct!=.
replace hh_hours_type=4 if (WEEKLY_HRS_HEAD_==0 & WEEKLY_HRS_WIFE_==0) | female_hours_pct==.

label define hh_hours_type 1 "Dual Earner" 2 "Male BW" 3 "Female BW" 4 "No Earners"
label values hh_hours_type hh_hours_type

gen hh_hours_type_lag=.
replace hh_hours_type_lag=hh_hours_type[_n-1] if unique_id==unique_id[_n-1]
label values hh_hours_type_lag hh_hours_type

gen female_hours_pct_lag=.
replace female_hours_pct_lag=female_hours_pct[_n-1] if unique_id==unique_id[_n-1]

browse id survey_yr WEEKLY_HRS_HEAD_ WEEKLY_HRS_WIFE_ female_hours_pct female_hours_pct_lag 
	
// restrict to working age (18-55) - at time of marriage or all? check what others do - Killewald said ages 18-55 - others have different restrictions, table this part for now
/*
browse id survey_yr AGE_ AGE_REF_ AGE_SPOUSE_ RELATION_
keep if (AGE_REF_>=18 & AGE_REF_<=55) &  (AGE_SPOUSE_>=18 & AGE_SPOUSE_<=55)
*/

// employment
browse id survey_yr EMPLOY_STATUS_HEAD_ EMPLOY_STATUS1_HEAD_ EMPLOY_STATUS2_HEAD_ EMPLOY_STATUS3_HEAD_ EMPLOY_STATUS_WIFE_ EMPLOY_STATUS1_WIFE_ EMPLOY_STATUS2_WIFE_ EMPLOY_STATUS3_WIFE_
// not numbered until 1994; 1-3 arose in 1994. codes match
// wife not asked until 1976?

gen employ_head=0
replace employ_head=1 if EMPLOY_STATUS_HEAD_==1
gen employ1_head=0
replace employ1_head=1 if EMPLOY_STATUS1_HEAD_==1
gen employ2_head=0
replace employ2_head=1 if EMPLOY_STATUS2_HEAD_==1
gen employ3_head=0
replace employ3_head=1 if EMPLOY_STATUS3_HEAD_==1
egen employed_head=rowtotal(employ_head employ1_head employ2_head employ3_head)

gen employ_wife=0
replace employ_wife=1 if EMPLOY_STATUS_WIFE_==1
gen employ1_wife=0
replace employ1_wife=1 if EMPLOY_STATUS1_WIFE_==1
gen employ2_wife=0
replace employ2_wife=1 if EMPLOY_STATUS2_WIFE_==1
gen employ3_wife=0
replace employ3_wife=1 if EMPLOY_STATUS3_WIFE_==1
egen employed_wife=rowtotal(employ_wife employ1_wife employ2_wife employ3_wife)

browse id survey_yr employed_head employed_wife employ_head employ1_head employ_wife employ1_wife

// problem is this employment is NOW not last year. I want last year? use if wages = employ=yes, then no? (or hours)
gen employed_ly_head=0
replace employed_ly_head=1 if earnings_head > 0 & earnings_head!=.

gen employed_ly_wife=0
replace employed_ly_wife=1 if earnings_wife > 0 & earnings_wife!=.

browse id survey_yr employed_ly_head employed_ly_wife WEEKLY_HRS_HEAD_ WEEKLY_HRS1_HEAD_ WEEKLY_HRS_WIFE_ WEEKLY_HRS1_WIFE_ earnings_head earnings_wife
// weekly_hrs not asked until 1994, was something asked PRIOR? i think I maybe didn't pull in GAH, use weekly_hrs1 prior to 1994 / 2001 is last yr
// okay wife bucketed 1968, real all other years? (1 or 2=PT; 3/8-FT)

gen ft_pt_head_pre=0
replace ft_pt_head_pre=1 if employed_ly_head==1 & WEEKLY_HRS1_HEAD_ >0 & WEEKLY_HRS1_HEAD_<=35
replace ft_pt_head_pre=2 if employed_ly_head==1 & WEEKLY_HRS1_HEAD_>35 & WEEKLY_HRS1_HEAD_!=.

gen ft_pt_head_post=0
replace ft_pt_head_post=1 if employed_ly_head==1 & WEEKLY_HRS_HEAD_ >0 & WEEKLY_HRS_HEAD_<=35
replace ft_pt_head_post=2 if employed_ly_head==1 & WEEKLY_HRS_HEAD_>35 & WEEKLY_HRS_HEAD_!=.

gen ft_pt_wife_pre=0
replace ft_pt_wife_pre=1 if employed_ly_wife==1 & WEEKLY_HRS1_WIFE_ >0 & WEEKLY_HRS1_WIFE_<=35 & survey_yr!=1968
replace ft_pt_wife_pre=2 if employed_ly_wife==1 & WEEKLY_HRS1_WIFE_>35 & WEEKLY_HRS1_WIFE_!=. & survey_yr!=1968
replace ft_pt_wife_pre=1 if employed_ly_wife==1 & inlist(WEEKLY_HRS1_WIFE_,1,2) & survey_yr==1968
replace ft_pt_wife_pre=2 if employed_ly_wife==1 & inrange(WEEKLY_HRS1_WIFE_,3,8) & survey_yr==1968

gen ft_pt_wife_post=0
replace ft_pt_wife_post=1 if employed_ly_wife==1 & WEEKLY_HRS_WIFE_ >0 & WEEKLY_HRS_WIFE_<=35
replace ft_pt_wife_post=2 if employed_ly_wife==1 & WEEKLY_HRS_WIFE_>35 & WEEKLY_HRS_WIFE_!=.

label define ft_pt 0 "Not Employed" 1 "PT" 2 "FT"
label values ft_pt_head_pre ft_pt_head_post ft_pt_wife_pre ft_pt_wife_post ft_pt

gen ft_pt_head=.
replace ft_pt_head = ft_pt_head_pre if inrange(survey_yr,1968,1993)
replace ft_pt_head = ft_pt_head_post if inrange(survey_yr,1994,2019)

gen ft_pt_wife=.
replace ft_pt_wife = ft_pt_wife_pre if inrange(survey_yr,1968,1993)
replace ft_pt_wife = ft_pt_wife_post if inrange(survey_yr,1994,2019)

label values ft_pt_head ft_pt_wife ft_pt

gen ft_head=0
replace ft_head=1 if ft_pt_head==2

gen ft_wife=0
replace ft_wife=1 if ft_pt_wife==2

// adding other controls right now, using same as SIPP analysis
gen either_enrolled=0
replace either_enrolled = 1 if ENROLLED_WIFE_==1 | ENROLLED_HEAD_==1

//race
drop if RACE_1_WIFE_==9 | RACE_1_HEAD_==9

browse id survey_yr RACE_1_WIFE_ RACE_2_WIFE_ RACE_3_WIFE_ RACE_1_HEAD_ RACE_2_HEAD_ RACE_3_HEAD_ RACE_4_HEAD_
// wait race of wife not asked until 1985?! that's wild. also need to see if codes changed in between. try to fill in historical for wife if in survey in 1985 and prior.
/*
1968-1984: 1=White; 2=Negro; 3=PR or Mexican; 7=Other
1985-1989: 1=White; 2=Black; 3=Am Indian 4=Asian 7=Other; 8 =more than 2
1990-2003: 1=White; 2=Black; 3=Am India; 4=Asian; 5=Latino; 6=Other; 7=Other
2005-2019: 1=White; 2=Black; 3=Am India; 4=Asian; 5=Native Hawaiian/Pac Is; 7=Other
*/


gen race_1_head_rec=.
replace race_1_head_rec=1 if RACE_1_HEAD_==1
replace race_1_head_rec=2 if RACE_1_HEAD_==2
replace race_1_head_rec=3 if (inrange(survey_yr,1985,2019) & RACE_1_HEAD_==3)
replace race_1_head_rec=4 if (inrange(survey_yr,1985,2019) & RACE_1_HEAD_==4)
replace race_1_head_rec=5 if (inrange(survey_yr,1968,1984) & RACE_1_HEAD_==3) | (inrange(survey_yr,1990,2003) & RACE_1_HEAD_==5)
replace race_1_head_rec=6 if RACE_1_HEAD_==7 | (inrange(survey_yr,1990,2003) & RACE_1_HEAD_==6) | (inrange(survey_yr,2005,2019) & RACE_1_HEAD_==5) | (inrange(survey_yr,1985,1989) & RACE_1_HEAD_==8)

gen race_2_head_rec=.
replace race_2_head_rec=1 if RACE_2_HEAD_==1
replace race_2_head_rec=2 if RACE_2_HEAD_==2
replace race_2_head_rec=3 if (inrange(survey_yr,1985,2019) & RACE_2_HEAD_==3)
replace race_2_head_rec=4 if (inrange(survey_yr,1985,2019) & RACE_2_HEAD_==4)
replace race_2_head_rec=5 if (inrange(survey_yr,1968,1984) & RACE_2_HEAD_==3) | (inrange(survey_yr,1990,2003) & RACE_2_HEAD_==5)
replace race_2_head_rec=6 if RACE_2_HEAD_==7 | (inrange(survey_yr,1990,2003) & RACE_2_HEAD_==6) | (inrange(survey_yr,2005,2019) & RACE_2_HEAD_==5) | (inrange(survey_yr,1985,1989) & RACE_2_HEAD_==8)

gen race_3_head_rec=.
replace race_3_head_rec=1 if RACE_3_HEAD_==1
replace race_3_head_rec=2 if RACE_3_HEAD_==2
replace race_3_head_rec=3 if (inrange(survey_yr,1985,2019) & RACE_3_HEAD_==3)
replace race_3_head_rec=4 if (inrange(survey_yr,1985,2019) & RACE_3_HEAD_==4)
replace race_3_head_rec=5 if (inrange(survey_yr,1968,1984) & RACE_3_HEAD_==3) | (inrange(survey_yr,1990,2003) & RACE_3_HEAD_==5)
replace race_3_head_rec=6 if RACE_3_HEAD_==7 | (inrange(survey_yr,1990,2003) & RACE_3_HEAD_==6) | (inrange(survey_yr,2005,2019) & RACE_3_HEAD_==5) | (inrange(survey_yr,1985,1989) & RACE_3_HEAD_==8)

gen race_4_head_rec=.
replace race_4_head_rec=1 if RACE_4_HEAD_==1
replace race_4_head_rec=2 if RACE_4_HEAD_==2
replace race_4_head_rec=3 if (inrange(survey_yr,1985,2019) & RACE_4_HEAD_==3)
replace race_4_head_rec=4 if (inrange(survey_yr,1985,2019) & RACE_4_HEAD_==4)
replace race_4_head_rec=5 if (inrange(survey_yr,1968,1984) & RACE_4_HEAD_==3) | (inrange(survey_yr,1990,2003) & RACE_4_HEAD_==5)
replace race_4_head_rec=6 if RACE_4_HEAD_==7 | (inrange(survey_yr,1990,2003) & RACE_4_HEAD_==6) | (inrange(survey_yr,2005,2019) & RACE_4_HEAD_==5) | (inrange(survey_yr,1985,1989) & RACE_4_HEAD_==8)

gen race_1_wife_rec=.
replace race_1_wife_rec=1 if RACE_1_WIFE_==1
replace race_1_wife_rec=2 if RACE_1_WIFE_==2
replace race_1_wife_rec=3 if (inrange(survey_yr,1985,2019) & RACE_1_WIFE_==3)
replace race_1_wife_rec=4 if (inrange(survey_yr,1985,2019) & RACE_1_WIFE_==4)
replace race_1_wife_rec=5 if (inrange(survey_yr,1968,1984) & RACE_1_WIFE_==3) | (inrange(survey_yr,1990,2003) & RACE_1_WIFE_==5)
replace race_1_wife_rec=6 if RACE_1_WIFE_==7 | (inrange(survey_yr,1990,2003) & RACE_1_WIFE_==6) | (inrange(survey_yr,2005,2019) & RACE_1_WIFE_==5) | (inrange(survey_yr,1985,1989) & RACE_1_WIFE_==8)

gen race_2_wife_rec=.
replace race_2_wife_rec=1 if RACE_2_WIFE_==1
replace race_2_wife_rec=2 if RACE_2_WIFE_==2
replace race_2_wife_rec=3 if (inrange(survey_yr,1985,2019) & RACE_2_WIFE_==3)
replace race_2_wife_rec=4 if (inrange(survey_yr,1985,2019) & RACE_2_WIFE_==4)
replace race_2_wife_rec=5 if (inrange(survey_yr,1968,1984) & RACE_2_WIFE_==3) | (inrange(survey_yr,1990,2003) & RACE_2_WIFE_==5)
replace race_2_wife_rec=6 if RACE_2_WIFE_==7 | (inrange(survey_yr,1990,2003) & RACE_2_WIFE_==6) | (inrange(survey_yr,2005,2019) & RACE_2_WIFE_==5) | (inrange(survey_yr,1985,1989) & RACE_2_WIFE_==8)

gen race_3_wife_rec=.
replace race_3_wife_rec=1 if RACE_3_WIFE_==1
replace race_3_wife_rec=2 if RACE_3_WIFE_==2
replace race_3_wife_rec=3 if (inrange(survey_yr,1985,2019) & RACE_3_WIFE_==3)
replace race_3_wife_rec=4 if (inrange(survey_yr,1985,2019) & RACE_3_WIFE_==4)
replace race_3_wife_rec=5 if (inrange(survey_yr,1968,1984) & RACE_3_WIFE_==3) | (inrange(survey_yr,1990,2003) & RACE_3_WIFE_==5)
replace race_3_wife_rec=6 if RACE_3_WIFE_==7 | (inrange(survey_yr,1990,2003) & RACE_3_WIFE_==6) | (inrange(survey_yr,2005,2019) & RACE_3_WIFE_==5) | (inrange(survey_yr,1985,1989) & RACE_3_WIFE_==8)

gen race_wife=race_1_wife_rec
replace race_wife=7 if race_2_wife_rec!=.

gen race_head=race_1_head_rec
replace race_head=7 if race_2_head_rec!=.

label define race 1 "White" 2 "Black" 3 "Indian" 4 "Asian" 5 "Latino" 6 "Other" 7 "Multi-racial"
label values race_wife race_head race

// wife - not asked until 1985, need to figure out
	browse id survey_yr race_wife if inlist(id,3,12,16)
	bysort id (race_wife): replace race_wife=race_wife[1] if race_wife==.

// need to figure out ethnicity  okay DUH - see Cooke 2006 - bc only in sample in 1990-1995, otherwise there are actually no Hispanics in sample DUH

gen same_race=0
replace same_race=1 if race_head==race_wife & race_head!=.

// any children - need to get more specific
gen children=0
replace children=1 if NUM_CHILDREN_>=1

bysort unique_id: egen children_ever = max(NUM_CHILDREN_)

sort unique_id survey_yr
browse unique_id survey_yr NUM_CHILDREN_ children_ever

browse survey_yr unique_id NUM_CHILDREN_ NUM_BIRTHS FIRST_BIRTH_YR BIRTHS_REF_ BIRTH_SPOUSE_ BIRTHS_BOTH_
gen when_first_birth = FIRST_BIRTH_YR
replace when_first_birth =. if FIRST_BIRTH_YR==9999 & (NUM_BIRTHS==0 | NUM_BIRTHS==98)
replace when_first_birth =. if FIRST_BIRTH_YR==9999 & NUM_BIRTHS==99 & children_ever==0
replace when_first_birth = survey_yr if NUM_BIRTHS==99 & NUM_CHILDREN_ > 0 & NUM_CHILDREN_[_n-1]==0 & unique_id==unique_id[_n-1]
bysort unique_id: egen first_birth_check = min(when_first_birth)
// browse unique_id when_first_birth first_birth_check
replace when_first_birth = first_birth_check if when_first_birth==9999 & first_birth_check!=9999 & first_birth_check!=.

sort unique_id survey_yr
// browse unique_id survey_yr when_first_birth FIRST_BIRTH_YR NUM_BIRTHS NUM_CHILDREN_ AGE_OLDEST_CHILD_ first_birth_calc if when_first_birth==9999

gen first_birth_calc = survey_yr - AGE_OLDEST_CHILD_ if survey_yr==1969  & when_first_birth==9999 & AGE_OLDEST_CHILD_!=.
bysort unique_id (first_birth_calc): replace first_birth_calc = first_birth_calc[1]
replace when_first_birth = first_birth_calc if when_first_birth==9999 & first_birth_calc!=.
gen first_birth_calc2 = survey_yr - AGE_YOUNG_CHILD_ if when_first_birth==9999 & AGE_YOUNG_CHILD_!=. // use youngest if do not have oldest, use minimum
drop first_birth_check
bysort unique_id: egen first_birth_check = min(first_birth_calc2)
replace when_first_birth = first_birth_check if when_first_birth==9999 & first_birth_check!=9999 & first_birth_check!=.

sort unique_id survey_yr
browse unique_id survey_yr when_first_birth master_marriage_start

gen pre_marital_birth=0
replace pre_marital_birth=1 if when_first_birth < master_marriage_start & when_first_birth!=. // this will not be 100% accurate for people missing marital history - bc I might not know date of first marriage if prior to survey

/*
gen post_marital_birth=0
replace post_marital_birth=1 if when_first_birth >= master_marriage_start & when_first_birth<=rel_end_all & when_first_birth!=. // needs to be IN marriage years, okay barely changed it - need to figure out how to do this for a given relationship. come back to this
*/

// urbanicity
gen metro=(METRO_==1) // a lot of missing, don't use for now, control for STATE_ for now

// housework hours - not totally sure if accurate prior to 1976
browse id survey_yr HOUSEWORK_HEAD_ HOUSEWORK_WIFE_ TOTAL_HOUSEWORK_HW_ MOST_HOUSEWORK_

gen housework_head = HOUSEWORK_HEAD_
replace housework_head = (HOUSEWORK_HEAD_/52) if inrange(survey_yr,1968,1974)
gen housework_wife = HOUSEWORK_WIFE_
replace housework_wife = (HOUSEWORK_WIFE_/52) if inrange(survey_yr,1968,1974)

browse id survey_yr housework_head housework_wife TOTAL_HOUSEWORK_HW_ MOST_HOUSEWORK_
gen wife_housework_pct = housework_wife / (housework_wife + housework_head)

gen housework_bkt=.
replace housework_bkt=1 if wife_housework_pct >=.4000 & wife_housework_pct <=.6000
replace housework_bkt=2 if wife_housework_pct >.6000 & wife_housework_pct!=.
replace housework_bkt=3 if wife_housework_pct <.4000
replace housework_bkt=4 if (housework_wife==0 | housework_wife==.) & (housework_head==0 | housework_head==.)

label define housework_bkt 1 "Dual HW" 2 "Female Primary" 3 "Male Primary" 4 "NA"
label values housework_bkt housework_bkt

sort id survey_yr
gen housework_bkt_lag=.
replace housework_bkt_lag=housework_bkt[_n-1] if unique_id==unique_id[_n-1]
label values housework_bkt_lag housework_bkt

gen wife_hw_pct_lag=.
replace wife_hw_pct_lag=wife_housework_pct[_n-1] if unique_id==unique_id[_n-1]

/* religion is new, but think I need to add given historical research. coding changes between 1984 and 1985, then again between 1994 and 1995 Need to recode, so tabling for now, not important for these purposes anyway
label define update_religion  ///
       1 "Catholic"  ///
       2 "Jewish"  ///
       8 "Protestant unspecified"  ///
      10 "Other non-Christian: Muslim, Rastafarian, etc."  ///
      13 "Greek/Russian/Eastern Orthodox"  ///
      97 "Other"  ///
      98 "DK"  ///
      99 "NA; refused"  ///
       0 "None"

recode RELIGION_HEAD_ (3/7=97)(9=97)(11/12=97)(14/31=97), gen(religion_head)
recode RELIGION_WIFE_ (3/7=97)(9=97)(11/12=97)(14/31=97), gen(religion_wife)
	   
label values religion_head religion_wife update_religion
*/

// also age at relationship start
browse unique_id survey_yr AGE_ BIRTH_YR_ AGE_REF_ AGE_SPOUSE_
replace AGE_SPOUSE_ = . if AGE_SPOUSE_==0

gen yr_born = survey_yr - AGE_ - 1
gen yr_born_head = survey_yr - AGE_REF_ - 1
gen yr_born_wife = survey_yr - AGE_SPOUSE_ - 1

/* move this later, right now I do not have restricted to just relationships and just heads and wife, so this will not be accurate. I could do year rel started - year born also?
gen age_mar_head = rel_start_all -  yr_born_head
gen age_mar_wife = rel_start_all -  yr_born_wife

drop if age_mar_head < 0 | age_mar_wife < 0
*/

save "$created_data\PSID_relationship_file.dta", replace // same name as above, now just recodes added. Still not restricted to any relationships or just head / wife

********************************************************************************
**# Want to create a COHABITATION analysis file
********************************************************************************
// first, remove non-relationship rows
browse survey_yr unique_id partner_unique_id rel_type
drop if rel_type==.

// see what happens if I use individual education instead of ref / head
gen college=0
replace college=1 if inlist(YRS_EDUCATION_,16,17)
	// tab college college_complete_head if ego_rel==1
	// tab college college_complete_wife if ego_rel==2
	
recode YRS_EDUCATION_ (0/11=1)(12=2)(13/15=3)(16/17=4), gen(educ)
replace educ=. if educ==99
browse main_per_id survey_yr unique_id partner_unique_id AGE_ college

gen employed=0
replace employed=1 if EMPLOYMENT_ == 1	
	
// then, need to restrict to just relationships where either head or wife / partner because that is where the bulk of the variables are
// Per Schneider et al 2018, think I need to include OFUMs, so don't do this for now
browse survey_yr unique_id partner_unique_id rel_type ego_rel alter_rel RELATION_
recode RELATION_ (1=1)(2=2)(3/7=3)(8=2)(9=3)(10=1)(20/22=2)(30/83=3)(88/92=2)(95/99=3), gen(relation) // oh I also put the bf/gf in as 2 so that is part of the problem, but if they are 88, they actually don't get information
recode RELATION_ (1=1)(2=2)(3/7=4)(8=3)(9=4)(10=1)(20/22=2)(30/83=4)(88/92=3)(95/99=4), gen(relation_v2)

tab ego_rel relation
** keep if inlist(ego_rel,1,2) & inlist(alter_rel,1,2)

// figure out income (need additional from CNEF - perhaps impute for now?)
browse unique_id survey_yr RELATION_ earnings_head earnings_wife TOTAL_MONEY_INCOME_ OFUM_LABOR_INCOME_ OFUM_TAXABLE_INCOME_ EARNINGS_2YRLAG_ employed 
	// if also head, is their income recorded against OFUM AND income for head?! or just ofum?! is this why so many are 0? bc recorded against head?! this is so confusing. okay yes, their income is recorded both, but that doesn't seem true in later years.  when they switched BACK to labor income? after 2005?
	// but it does look like recorded in OFUMs for first year cohabitors  - see 1473173
gen income=.
replace income = earnings_head if relation_v2==1
replace income = earnings_wife if relation_v2==2
replace income = TOTAL_MONEY_INCOME_ if inlist(relation_v2,3,4) & survey_yr >=1968 & survey_yr <=1974
replace income = OFUM_TAXABLE_INCOME_ if inlist(relation_v2,3,4) & survey_yr >=1975 & survey_yr <=1990
replace income = OFUM_LABOR_INCOME_ if inlist(relation_v2,3,4) & ((survey_yr >=1991 & survey_yr <=1993) | (survey_yr >=2005 & survey_yr <=2019))
browse unique_id partner_unique_id survey_yr relation_v2 income earnings_head earnings_wife TOTAL_MONEY_INCOME_ OFUM_LABOR_INCOME_ OFUM_TAXABLE_INCOME_ EARNINGS_2YRLAG_ employed 

** Need to save a file so I can do a vlookup of partner information
//  unique unique_id partner_unique_id survey_yr
//  unique unique_id survey_yr

preserve

collapse (mean) income educ college employed yr_born start_this_rel, by(unique_id survey_yr)
rename unique_id partner_unique_id
gen partner_income = income
gen partner_educ = educ
gen partner_college = college
gen partner_employed = employed
gen partner_yr_born = yr_born
gen partner_rel_start = start_this_rel

save "$temp\PSID_partner_info_lookup.dta", replace

restore

// one record that is not unique partner_survey
bysort partner_unique_id survey_yr : egen record_id = rank(survey_yr)
keep if record_id==1

unique partner_unique_id survey_yr
merge 1:1 partner_unique_id survey_yr using "$temp\PSID_partner_info_lookup.dta", keepusing(partner_*)
drop if _merge==2
drop _merge

browse unique_id partner_unique_id survey_yr income partner_income earnings_head earnings_wife  // okay so just need to fill this in and make sure the gender are right THEN create actual variables

gen income_man=.
replace income_man=income if SEX==1
replace income_man=partner_income if SEX==2

gen income_woman=.
replace income_woman=income if SEX==2
replace income_woman=partner_income if SEX==1

gen employed_man=.
replace employed_man=employed if SEX==1
replace employed_man=partner_employed if SEX==2

gen employed_woman=.
replace employed_woman=employed if SEX==2
replace employed_woman=partner_employed if SEX==1

gen college_man=.
replace college_man=college if SEX==1
replace college_man=partner_college if SEX==2

gen college_woman=.
replace college_woman=college if SEX==2
replace college_woman=partner_college if SEX==1

browse unique_id partner_unique_id survey_yr sex income_man income_woman earnings_head earnings_wife income partner_income

gen couple_educ_gp=0
replace couple_educ_gp=1 if (college_man==1 | college_woman==1)

label define couple_educ 0 "Neither College" 1 "At Least One College"
label values couple_educ_gp couple_educ

// key variable
egen couple_earnings = rowtotal(income_woman income_man)

gen female_earn_pct = income_woman/(couple_earnings)

gen hh_earn_type=.
replace hh_earn_type=1 if female_earn_pct >=.4000 & female_earn_pct <=.6000
replace hh_earn_type=2 if female_earn_pct < .4000 & female_earn_pct >=0
replace hh_earn_type=3 if female_earn_pct > .6000 & female_earn_pct <=1
replace hh_earn_type=4 if income_man==0 & income_woman==0

label define hh_earn_type 1 "Dual Earner" 2 "Male BW" 3 "Female BW" 4 "No Earners"
label values hh_earn_type hh_earn_type

sort unique_id survey_yr

gen hh_earn_type_lag=.
replace hh_earn_type_lag=hh_earn_type[_n-1] if unique_id==unique_id[_n-1]
label values hh_earn_type_lag hh_earn_type

gen female_earn_pct_lag=.
replace female_earn_pct_lag=female_earn_pct[_n-1] if unique_id==unique_id[_n-1]

browse unique_id survey_yr income_man income_woman hh_earn_type hh_earn_type_lag

save "$created_data\PSID_relationship_file.dta", replace // same name as above, now just partner info added. Still not restricted to any relationships or just head / wife

********************************************************************************
* Create rest of file and sample restrictions
********************************************************************************

// drop those who entered in relationship (so first survey yr is the first relationship year) - so like a bunch will have start date of 1968 if they don't have history. need to figure out who has history and also if partner and person don't have same first survey year. 
rename start_this_rel main_rel_start
browse unique_id partner_unique_id survey_yr first_survey_yr rel_num main_rel_start partner_rel_start rel_start MARITAL_PAIRS_

egen start_this_rel = rowmin(main_rel_start partner_rel_start rel_start)
browse unique_id partner_unique_id survey_yr start_this_rel main_rel_start partner_rel_start rel_start MARITAL_PAIRS_

gen rel_start_flag=0
replace rel_start_flag=1 if start_this_rel == first_survey_yr

// signal marital transition
browse survey_yr unique_id partner_unique_id rel_type
sort unique_id survey_yr
gen marr_trans=0
replace marr_trans=1 if rel_type==20 & rel_type[_n-1]==22 & unique_id==unique_id[_n-1] & partner_unique_id==partner_unique_id[_n-1]

browse survey_yr unique_id partner_unique_id rel_type marr_trans MARITAL_PAIRS_
keep if rel_type==22 | marr_trans==1

// still need to figure out how to drop one person in HH (do I ever have a case where I just have one person?)
// FAMILY_INTERVIEW_NUM_ is the specific year, main_per_id is HH in 1968, so the problem is multiple couples have same main_per_id if they later moved out.
browse survey_yr unique_id partner_unique_id main_per_id

sort survey_yr FAMILY_INTERVIEW_NUM_
browse survey_yr FAMILY_INTERVIEW_NUM_ unique_id partner_unique_id

bysort survey_yr FAMILY_INTERVIEW_NUM_ : egen couple_per_id = rank(unique_id)
browse survey_yr FAMILY_INTERVIEW_NUM_ unique_id partner_unique_id couple_per_id

keep if couple_per_id==1

// other recodes needed
gen age_mar_head = rel_start -  yr_born_head
gen age_mar_wife = rel_start -  yr_born_wife // still a lot of missing
browse main_per_id survey_yr unique_id partner_unique_id rel_num rel_start yr_born_head age_mar_head

tab MARITAL_PAIRS_ // still a bunch recorded as no spouse, which is confusing. are these first year spouses? yes, it seems like they are first year cohabitors, because it is primarily the first year of the relationship
browse survey_yr unique_id partner_unique_id rel_type marr_trans MARITAL_PAIRS_

// validating hh_earn_type seems real now
tab hh_earn_type
tab hh_earn_type if MARITAL_PAIRS_ == 0

tabstat AGE_, by(MARITAL_PAIRS_) // think this is Schneider's point - younger

// need to figure out vars when head is not male. Right now my variables created above assume that - this is very confusing
tab SEX_HEAD_ // 1 = male
tab MARITAL_PAIRS_ SEX_HEAD_ , row // okay yes, the head can be either sex when 1st year cohabitors, so that will affect the created variables. need to figure this out once I use OFUM / individual data to actually get employment / earnings.

gen dur=survey_yr - start_this_rel // this is not true duration. need to account for marital history if I do have it. 
//  browse main_per_id survey_yr unique_id partner_unique_id relationship_number rel_start dur

/*to revisit
drop if dur==0 | dur==.
drop if num_years==1
drop if MARITAL_PAIRS_==0
drop if SEX_HEAD_==2
*/

save "$created_data\PSID_cohab_sample.dta", replace


/* I am dumb - see FAQ, male partner / husband is always ref.

// can I do this with a loop and stubs? but some are head v. ref or spouse v. wife and a mix of upper and lower case. there is prob a way to do this but whatever
gen age_mar_man=.
replace age_mar_man = age_mar_head if SEX_HEAD_==1 // so if sex of head is male, head var = male
replace age_mar_man = age_mar_wife if SEX_HEAD_==2 // if sex of head is female, the wife var = male
gen age_mar_woman=.
replace age_mar_woman = age_mar_wife if SEX_HEAD_==1
replace age_mar_woman = age_mar_head if SEX_HEAD_==2

gen age_man=.
replace age_man = AGE_REF_ if SEX_HEAD_==1 
replace age_man = AGE_SPOUSE_ if SEX_HEAD_==2
gen age_woman=.
replace age_woman = AGE_SPOUSE_ if SEX_HEAD_==1
replace age_woman = AGE_REF_ if SEX_HEAD_==2

gen births_man=.
replace births_man = BIRTHS_REF_ if SEX_HEAD_==1 
replace births_man = BIRTH_SPOUSE_ if SEX_HEAD_==2
gen births_woman=.
replace births_woman = BIRTH_SPOUSE_ if SEX_HEAD_==1
replace births_woman = BIRTHS_REF_ if SEX_HEAD_==2

gen educ_man=.
replace educ_man = educ_head if SEX_HEAD_==1 
replace educ_man = educ_wife if SEX_HEAD_==2
gen educ_woman=.
replace educ_woman = educ_wife if SEX_HEAD_==1
replace educ_woman = educ_head if SEX_HEAD_==2

gen college_man=.
replace college_man = college_complete_head if SEX_HEAD_==1 
replace college_man = college_complete_wife if SEX_HEAD_==2
gen college_woman=.
replace college_woman = college_complete_wife if SEX_HEAD_==1
replace college_woman = college_complete_head if SEX_HEAD_==2

gen earnings_man=.
replace earnings_man = earnings_head if SEX_HEAD_==1 
replace earnings_man = earnings_wife if SEX_HEAD_==2
gen earnings_woman=.
replace earnings_woman = earnings_wife if SEX_HEAD_==1
replace earnings_woman = earnings_head if SEX_HEAD_==2

gen employed_man=.
replace employed_man = employed_head if SEX_HEAD_==1 
replace employed_man = employed_wife if SEX_HEAD_==2
gen employed_woman=.
replace employed_woman = employed_wife if SEX_HEAD_==1
replace employed_woman = employed_head if SEX_HEAD_==2

gen employed_ly_man=.
replace employed_ly_man = employed_ly_head if SEX_HEAD_==1 
replace employed_ly_man = employed_ly_wife if SEX_HEAD_==2
gen employed_ly_woman=.
replace employed_ly_woman = employed_ly_wife if SEX_HEAD_==1
replace employed_ly_woman = employed_ly_head if SEX_HEAD_==2

gen family_structure_man=.
replace family_structure_man = FAMILY_STRUCTURE_HEAD_ if SEX_HEAD_==1 
replace family_structure_man = FAMILY_STRUCTURE_WIFE_ if SEX_HEAD_==2
gen family_structure_woman=.
replace family_structure_woman = FAMILY_STRUCTURE_WIFE_ if SEX_HEAD_==1
replace family_structure_woman = FAMILY_STRUCTURE_HEAD_ if SEX_HEAD_==2

gen father_educ_man=.
replace father_educ_man = FATHER_EDUC_HEAD_ if SEX_HEAD_==1 
replace father_educ_man = FATHER_EDUC_WIFE_ if SEX_HEAD_==2
gen father_educ_woman=.
replace father_educ_woman = FATHER_EDUC_WIFE_ if SEX_HEAD_==1
replace father_educ_woman = FATHER_EDUC_HEAD_ if SEX_HEAD_==2

gen mother_educ_man=.
replace mother_educ_man = MOTHER_EDUC_HEAD_ if SEX_HEAD_==1 
replace mother_educ_man = MOTHER_EDUC_WIFE_ if SEX_HEAD_==2
gen mother_educ_woman=.
replace mother_educ_woman = MOTHER_EDUC_WIFE_ if SEX_HEAD_==1
replace mother_educ_woman = MOTHER_EDUC_HEAD_ if SEX_HEAD_==2

gen ft_man=.
replace ft_man = ft_head if SEX_HEAD_==1 
replace ft_man = ft_wife if SEX_HEAD_==2
gen ft_woman=.
replace ft_woman = ft_wife if SEX_HEAD_==1
replace ft_woman = ft_head if SEX_HEAD_==2

gen ft_pt_man=.
replace ft_pt_man = ft_pt_head if SEX_HEAD_==1 
replace ft_pt_man = ft_pt_wife if SEX_HEAD_==2
gen ft_pt_woman=.
replace ft_pt_woman = ft_pt_wife if SEX_HEAD_==1
replace ft_pt_woman = ft_pt_head if SEX_HEAD_==2

gen housework_man=.
replace housework_man = housework_head if SEX_HEAD_==1 
replace housework_man = housework_wife if SEX_HEAD_==2
gen housework_woman=.
replace housework_woman = housework_wife if SEX_HEAD_==1
replace housework_woman = housework_head if SEX_HEAD_==2

gen race_man=.
replace race_man = race_head if SEX_HEAD_==1 
replace race_man = race_wife if SEX_HEAD_==2
gen race_woman=.
replace race_woman = race_wife if SEX_HEAD_==1
replace race_woman = race_head if SEX_HEAD_==2

gen religion_man=.
replace religion_man = RELIGION_HEAD_ if SEX_HEAD_==1 
replace religion_man = RELIGION_WIFE_ if SEX_HEAD_==2
gen religion_woman=.
replace religion_woman = RELIGION_WIFE_ if SEX_HEAD_==1
replace religion_woman = RELIGION_HEAD_ if SEX_HEAD_==2

gen sex_man=.
replace sex_man = SEX_HEAD_ if SEX_HEAD_==1 
replace sex_man = SEX_WIFE_ if SEX_HEAD_==2
gen sex_woman=.
replace sex_woman = SEX_WIFE_ if SEX_HEAD_==1
replace sex_woman = SEX_HEAD_ if SEX_HEAD_==2

gen weekly_hrs_man=.
replace weekly_hrs_man = weekly_hrs_head if SEX_HEAD_==1 
replace weekly_hrs_man = weekly_hrs_wife if SEX_HEAD_==2
gen weekly_hrs_woman=.
replace weekly_hrs_woman = weekly_hrs_wife if SEX_HEAD_==1
replace weekly_hrs_woman = weekly_hrs_head if SEX_HEAD_==2

gen yr_born_man=.
replace yr_born_man = yr_born_head if SEX_HEAD_==1 
replace yr_born_man = yr_born_wife if SEX_HEAD_==2
gen yr_born_woman=.
replace yr_born_woman = yr_born_wife if SEX_HEAD_==1
replace yr_born_woman = yr_born_head if SEX_HEAD_==2

gen num_married_man=.
replace num_married_man = NUM_MARRIED_HEAD_ if SEX_HEAD_==1 
replace num_married_man = NUM_MARRIED_WIFE_ if SEX_HEAD_==2
gen num_married_woman=.
replace num_married_woman = NUM_MARRIED_WIFE_ if SEX_HEAD_==1
replace num_married_woman = NUM_MARRIED_HEAD_ if SEX_HEAD_==2

gen first_marriage_yr_man=.
replace first_marriage_yr_man = FIRST_MARRIAGE_YR_HEAD_ if SEX_HEAD_==1 
replace first_marriage_yr_man = FIRST_MARRIAGE_YR_WIFE_ if SEX_HEAD_==2
gen first_marriage_yr_woman=.
replace first_marriage_yr_woman = FIRST_MARRIAGE_YR_WIFE_ if SEX_HEAD_==1
replace first_marriage_yr_woman = FIRST_MARRIAGE_YR_HEAD_ if SEX_HEAD_==2

gen first_marriage_end_man=.
replace first_marriage_end_man = FIRST_MARRIAGE_END_HEAD_ if SEX_HEAD_==1 
replace first_marriage_end_man = FIRST_MARRIAGE_END_WIFE_ if SEX_HEAD_==2
gen first_marriage_end_woman=.
replace first_marriage_end_woman = FIRST_MARRIAGE_END_WIFE_ if SEX_HEAD_==1
replace first_marriage_end_woman = FIRST_MARRIAGE_END_HEAD_ if SEX_HEAD_==2

gen first_divorce_yr_man=.
replace first_divorce_yr_man = FIRST_DIVORCE_YR_HEAD_ if SEX_HEAD_==1 
replace first_divorce_yr_man = FIRST_DIVORCE_YR_WIFE_ if SEX_HEAD_==2
gen first_divorce_yr_woman=.
replace first_divorce_yr_woman = FIRST_DIVORCE_YR_WIFE_ if SEX_HEAD_==1
replace first_divorce_yr_woman = FIRST_DIVORCE_YR_HEAD_ if SEX_HEAD_==2

gen first_separated_yr_man=.
replace first_separated_yr_man = FIRST_SEPARATED_YR_HEAD_ if SEX_HEAD_==1 
replace first_separated_yr_man = FIRST_SEPARATED_YR_WIFE_ if SEX_HEAD_==2
gen first_separated_yr_woman=.
replace first_separated_yr_woman = FIRST_SEPARATED_YR_WIFE_ if SEX_HEAD_==1
replace first_separated_yr_woman = FIRST_SEPARATED_YR_HEAD_ if SEX_HEAD_==2

gen first_widow_yr_man=.
replace first_widow_yr_man = FIRST_WIDOW_YR_HEAD_ if SEX_HEAD_==1 
replace first_widow_yr_man = FIRST_WIDOW_YR_WIFE_ if SEX_HEAD_==2
gen first_widow_yr_woman=.
replace first_widow_yr_woman = FIRST_WIDOW_YR_WIFE_ if SEX_HEAD_==1
replace first_widow_yr_woman = FIRST_WIDOW_YR_HEAD_ if SEX_HEAD_==2

gen last_marriage_yr_man=.
replace last_marriage_yr_man = LAST_MARRIAGE_YR_HEAD_ if SEX_HEAD_==1 
replace last_marriage_yr_man = LAST_MARRIAGE_YR_WIFE_ if SEX_HEAD_==2
gen last_marriage_yr_woman=.
replace last_marriage_yr_woman = LAST_MARRIAGE_YR_WIFE_ if SEX_HEAD_==1
replace last_marriage_yr_woman = LAST_MARRIAGE_YR_HEAD_ if SEX_HEAD_==2

gen last_divorce_yr_man=.
replace last_divorce_yr_man = LAST_DIVORCE_YR_HEAD_ if SEX_HEAD_==1 
replace last_divorce_yr_man = LAST_DIVORCE_YR_WIFE_ if SEX_HEAD_==2
gen last_divorce_yr_woman=.
replace last_divorce_yr_woman = LAST_DIVORCE_YR_WIFE_ if SEX_HEAD_==1
replace last_divorce_yr_woman = LAST_DIVORCE_YR_HEAD_ if SEX_HEAD_==2

gen last_separated_yr_man=.
replace last_separated_yr_man = LAST_SEPARATED_YR_HEAD_ if SEX_HEAD_==1 
replace last_separated_yr_man = LAST_SEPARATED_YR_WIFE_ if SEX_HEAD_==2
gen last_separated_yr_woman=.
replace last_separated_yr_woman = LAST_SEPARATED_YR_WIFE_ if SEX_HEAD_==1
replace last_separated_yr_woman = LAST_SEPARATED_YR_HEAD_ if SEX_HEAD_==2

gen last_widow_yr_man=.
replace last_widow_yr_man = LAST_WIDOW_YR_HEAD_ if SEX_HEAD_==1 
replace last_widow_yr_man = LAST_WIDOW_YR_WIFE_ if SEX_HEAD_==2
gen last_widow_yr_woman=.
replace last_widow_yr_woman = LAST_WIDOW_YR_WIFE_ if SEX_HEAD_==1
replace last_widow_yr_woman = LAST_WIDOW_YR_HEAD_ if SEX_HEAD_==2

// okay NOW make the independent variables I need
*/