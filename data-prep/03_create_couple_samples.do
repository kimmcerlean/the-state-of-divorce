********************************************************************************
* Project: Work-family policy and divorce
* Getting PSID sample for union dissolution
* create_couple_samples.do
* Code owner: Kimberly McErlean
********************************************************************************

use "$temp/PSID_full_long.dta", clear // created in step 1
egen wave = group(survey_yr) // this will make years consecutive, easier for later

label define sample 0 "not sample" 1 "original sample" 2 "born-in" 3 "moved in" 4 "joint inclusion" 5 "followable nonsample parent" 6 "nonsample elderly"
label values SAMPLE sample
gen has_psid_gene=0
replace has_psid_gene = 1 if inlist(SAMPLE,1,2)

********************************************************************************
* First organize data and get relationship status / history
********************************************************************************

browse survey_yr main_per_id id unique_id INTERVIEW_NUM_ SEQ_NUMBER_ RELATION_ FIRST_MARRIAGE_YR_START MARITAL_PAIRS_

// drop if survey_yr <1983 // first time you could identify cohab

gen relationship=0
replace relationship=1 if inrange(MARITAL_PAIRS_,1,4)

browse survey_yr unique_id main_per_id SEQ_NUMBER_ relationship RELATION_ FIRST_MARRIAGE_YR_START 

replace SEQ_NUMBER_= 0 if survey_yr==1968 & RELATION_==0 // no seq number in 1968, make it 0 if not in sample based on response to RELATION
bysort unique_id (SEQ_NUMBER_): egen in_sample=max(SEQ_NUMBER_)

drop if in_sample==0 // people with NO DATA in any year
drop if SEQ_NUMBER_==0 // won't have data because not in that year -- this is JUST last year in sample at the moment

// attempt to add marital status based on if head or partner (bc can apply head's marital status if their partner)
label define marr_defacto 1 "Partnered" 2 "Single" 3 "Widowed" 4 "Divorced" 5 "Separated"
label values MARST_DEFACTO_HEAD_ marr_defacto

label define marr_legal 1 "Married" 2 "Single" 3 "Widowed" 4 "Divorced" 5 "Separated"
label values MARST_LEGAL_HEAD_ marr_legal

gen relation = .
replace relation = 1 if inlist(RELATION_,1,10)
replace relation = 2 if inlist(RELATION_,2,20,22)
replace relation = 3 if !inlist(RELATION_,1,2,10,20,22)
label define relation 1 "Head" 2 "Partner" 3 "Other"
label values relation relation

tab RELATION_ relation, m

gen cohab_est_head=0
replace cohab_est_head=1 if MARST_DEFACTO_HEAD_==1 & inlist(MARST_LEGAL_HEAD_,2,3,4,5) // will only apply after 1977

gen marital_status_updated=.
replace marital_status_updated=1 if MARST_DEFACTO_HEAD_==1 & cohab_est_head==0
replace marital_status_updated=2 if MARST_DEFACTO_HEAD_==1 & cohab_est_head==1
replace marital_status_updated=3 if MARST_DEFACTO_HEAD_==2
replace marital_status_updated=4 if MARST_DEFACTO_HEAD_==3
replace marital_status_updated=5 if MARST_DEFACTO_HEAD_==4
replace marital_status_updated=6 if MARST_DEFACTO_HEAD_==5

label define marital_status_updated 1 "Married (or pre77)" 2 "Partnered" 3 "Single" 4 "Widowed" 5 "Divorced" 6 "Separated"
label values marital_status_updated marital_status_updated

browse unique_id survey_yr relationship relation marital_status_updated FIRST_MARRIAGE_YR_START FIRST_MARRIAGE_YR_END RECENT_MARRIAGE_YR_START
replace marital_status_updated = . if relation==3 // this marital status doesn't apply if you are not head or their partner
tab marital_status_updated relationship, m col

// see if I can also make use of info from the relationship history file - because I think that can also distinguish marriage and cohab?
merge 1:1 unique_id survey_yr using "$temp/PSID_relationship_list_tomatch.dta", keepusing(MX8 partner_unique_id rel_num marr_num)
drop if _merge==2
tab relationship _merge, m
drop _merge

rename rel_num matrix_rel_num // just so I know where I got it
rename marr_num matrix_marr_num

tab MX8 relationship, m col // okay, yes, very few rows missing here
tab marital_status_updated MX8 if relationship==1, m

replace marital_status_updated = 1 if MX8 == 20 & marital_status_updated==1
replace marital_status_updated = 2 if MX8 == 22 & marital_status_updated==1 // move the cohab I wasn't sure about
replace marital_status_updated = 1 if MX8 == 20 & marital_status_updated==. // fill in missing
replace marital_status_updated = 2 if MX8 == 22 & marital_status_updated==. 

tab marital_status_updated relationship, m col

// relationship transitions - okay replacing original code with the new history I just created (bc previous code was just for marriage and need cohab)
merge m:1 unique_id using "$created_data/psid_master_relationship_history_wide.dta" // remember kim, before you panic, this file is restricted to ONLY PEOPLE with relationships ever
drop if _merge==2
tab relationship _merge, m row // a few people in relationships don't have matches, but pretty good (it's less than 1% non-match rate)
drop _merge
tab yr_married1 if history_flag==., m // confirm - those missing on history flag (aka no record in rel history wide) do not have any observed marriage dates in marital history - and this is true. this validates that my history at least covers those in a relationship at some point.

browse unique_id survey_yr relationship MARITAL_PAIRS_ MARST_DEFACTO_HEAD_ marital_status_updated
gen relationship_yr = survey_yr if relationship==1
sort unique_id survey_yr
gen enter_rel=0
replace enter_rel=1 if relationship==1 & relationship[_n-1]==0 & unique_id==unique_id[_n-1]
replace enter_rel=1 if relationship_yr==1968 // since can't transition, but call this "relationship 1"

gen exit_rel=0
sort id survey_yr
replace exit_rel=1 if relationship==1 & relationship[_n+1]==0 & unique_id==unique_id[_n+1]
// replace exit_rel=1 if relationship==0 & relationship[_n-1]==1 & id==id[_n-1]

browse unique_id survey_yr relationship MX8 partner_unique_id enter_rel exit_rel master_rel_start1 master_rel_end1 master_rel_type1 master_rel_start2 master_rel_end2 master_rel_type2 master_rel_start3 master_rel_end3 master_rel_type3

tab marital_status_updated MX8
tab MX8 master_rel_type1, m

gen current_rel_number=.
forvalues r=1/10{
	capture replace current_rel_number = `r' if survey_yr >= master_rel_start`r' & survey_yr<= master_rel_end`r' & MX8 == master_rel_type`r' // prio next relationship bc end dates of 9999 causing problems
	// yes - bc current rel_type is based on start type-  so couples who transition frm cohab to marriage will not match (that is currently happening)
	capture replace current_rel_number = `r' if current_rel_number==. & survey_yr >= master_rel_start`r' & survey_yr<= master_rel_end`r' // & MX8!=. // don't need to match, but need to be observed in a rel
}

tab current_rel_number relationship, m col
tab current_rel_number MX8, m col
tab matrix_rel_num current_rel_number, m // matrix rel num doesn't include relationships prior to survey, so makes sense some of those are lower than here
// browse unique_id survey_yr relationship MX8 partner_unique_id enter_rel exit_rel current_rel_number master_rel_start1 master_rel_end1 master_rel_type1 master_rel_start2 master_rel_end2 master_rel_type2 master_rel_start3 master_rel_end3 master_rel_type3

// want to also make a marriage version bc want to keep first marriages for that sample
	// browse unique_id survey_yr in_marital_history relationship MX8 partner_unique_id current_rel_number yr_married1 yr_end1 yr_married2 yr_end2 yr_married3 yr_end3
	// can't use mater rel type because the relationships are not ranked just for marriage. SInce I later drop people with early relationship starts, it is fine to use marital history (bc all of the people in my sample have to be in marital history)
gen current_marr_number=.
forvalues r=1/13{
	capture replace current_marr_number = `r' if survey_yr >= yr_married`r' & survey_yr<= yr_end`r' & MX8 == 20 // use MX8 or the relationship type in history? I think prio what is observed in PSID...
}

tab current_rel_number current_marr_number, m
tab matrix_marr_num current_marr_number, m
tab current_marr_number relationship, m col
tab current_marr_number MX8, m col
tab num_marriages current_marr_number, m

// browse unique_id survey_yr in_marital_history relationship MX8 partner_unique_id current_rel_number current_marr_number yr_married1 yr_end1 yr_married2 yr_end2 yr_married3 yr_end3 master_rel_start1 master_rel_end1 master_rel_type1 master_rel_start2 master_rel_end2 master_rel_type2 master_rel_start3 master_rel_end3 master_rel_type3

gen rel_start_all=.
gen rel_end_all=.
gen rel_type_all=.
gen rel_status_all=.

forvalues r=1/10{
	replace rel_start_all = master_rel_start`r' if current_rel_number==`r'
	replace rel_end_all = master_rel_end`r' if current_rel_number==`r'
	replace rel_type_all = master_rel_type`r' if current_rel_number==`r'
	replace rel_status_all = master_rel_how_end`r' if current_rel_number==`r'
}

label values rel_type_all type
label values rel_status_all how_rel_end
tab MX8 rel_type_all, m // some are off because transitioned cohab to marriage and mine only captures 1st rel - we are going to update htis the way I normally do with the min / max rel dates. but doing this at a couple-level so can attempt to fill in true start / end dates (if one has non-missing info)
tab MX8 rel_status_all, m // this is def not perfect, especially for cohab - intact not at all labelled yet

browse unique_id survey_yr relationship MX8 partner_unique_id rel_start_all rel_end_all rel_type_all rel_status_all enter_rel exit_rel master_rel_start1 master_rel_end1 master_rel_type1 master_rel_how_end1 master_rel_start2 master_rel_end2 master_rel_type2 master_rel_how_end2 master_rel_start3 master_rel_end3 master_rel_type3

// attempt to fill in 9999s / missings using transition info created above
tab rel_start_all MX8, m
tab rel_end_all MX8, m col

gen rel_start_est = survey_yr if enter_rel==1
bysort unique_id partner_unique_id (rel_start_est): replace rel_start_est=rel_start_est[1] if partner_unique_id!=.

gen rel_end_est = survey_yr if exit_rel==1
bysort unique_id partner_unique_id (rel_end_est): replace rel_end_est=rel_end_est[1] if partner_unique_id!=.

sort unique_id survey_yr
browse unique_id survey_yr relationship MX8 partner_unique_id rel_start_all rel_end_all rel_type_all enter_rel exit_rel rel_start_est rel_end_est
browse unique_id survey_yr relationship MX8 partner_unique_id rel_start_all rel_end_all rel_type_all enter_rel exit_rel rel_start_est rel_end_est if (rel_start_all==. | rel_end_all==. | rel_end_all==9999) & MX8!=.

gen current_rel_start = rel_start_all // retain original
replace current_rel_start = rel_start_est if current_rel_start==. & rel_start_est!=.

gen current_rel_end = rel_end_all // retain original
replace current_rel_end = rel_end_est if (current_rel_end==. | current_rel_end==9999) & rel_end_est!=.

browse unique_id survey_yr relationship MX8 partner_unique_id current_rel_start current_rel_end rel_start_all rel_end_all rel_type_all enter_rel exit_rel rel_start_est rel_end_est

tab current_rel_start MX8, m
tab current_rel_end MX8, m col

bysort unique_id: egen first_survey_yr = min(survey_yr)
bysort unique_id: egen last_survey_yr = max(survey_yr)
bysort unique_id partner_unique_id: egen first_couple_yr = min(survey_yr) if partner_unique_id!=. // possibly can help figure out attrition?
bysort unique_id partner_unique_id: egen last_couple_yr = max(survey_yr) if partner_unique_id!=.

tab current_rel_end rel_status_all, m // also could all 9999s be proxy for attrit? that is true for marital history, not sure otherwise

sort unique_id survey_yr
browse unique_id partner_unique_id survey_yr in_marital_history MX8 rel_status_all current_rel_start current_rel_end first_survey_yr last_survey_yr first_couple_yr last_couple_yr

gen current_rel_status_est = rel_status_all
replace current_rel_status_est = 0 if current_rel_status_est==. & last_survey_yr == last_couple_yr & MX8!=.
replace current_rel_status_est = 1 if current_rel_status_est==. & last_survey_yr != last_couple_yr & MX8!=. // breakup if person continues in survey past relationship end

label values current_rel_status_est how_rel_end
browse unique_id partner_unique_id survey_yr in_marital_history MX8 current_rel_status_est rel_status_all current_rel_start current_rel_end first_survey_yr last_survey_yr first_couple_yr last_couple_yr
tab current_rel_end current_rel_status_est, m

// oh yeah, add in info on transitions between cohab and marriage
sort unique_id survey_yr
browse unique_id survey_yr wave partner_unique_id MX8

gen marr_trans=0
replace marr_trans=1 if MX8 == 20 & MX8[_n-1]==22 & unique_id==unique_id[_n-1] & partner_unique_id==partner_unique_id[_n-1] & wave==wave[_n-1]+1

bysort unique_id partner_unique_id: egen ever_transition = max(marr_trans)

gen transition_year = survey_yr if marr_trans==1
bysort unique_id partner_unique_id (transition_year): replace transition_year=transition_year[1] if partner_unique_id!=. 

sort unique_id survey_yr
browse unique_id survey_yr wave partner_unique_id MX8  marr_trans ever_transition transition_year

rename MX8 current_rel_type

********************************************************************************
* take intermission to match on partners so we a. use same info across partners, 
* where possible, and b. can fill in maybe rel start / end if one partner has
* and the other does not
********************************************************************************

preserve

keep unique_id survey_yr current_rel_type current_rel_start current_rel_end current_rel_status_est rel_status_all ever_transition transition_year entered_in_rel has_psid_gene in_marital_history history_flag first_survey_yr last_survey_yr first_couple_yr last_couple_yr

rename unique_id partner_unique_id
foreach var in current_rel_type current_rel_start current_rel_end current_rel_status_est rel_status_all ever_transition transition_year entered_in_rel has_psid_gene in_marital_history history_flag first_survey_yr last_survey_yr first_couple_yr last_couple_yr{
	rename `var' `var'_sp
}

save "$temp/psid_partner_rel_info.dta", replace

restore

merge m:1 partner_unique_id survey_yr using "$temp/psid_partner_rel_info.dta"
drop if _merge==2
tab current_rel_type _merge, m row // okay yes all partnered rows match (I haven't yet restricted to couples)
drop _merge

tab current_rel_type current_rel_type_sp, m // match, which makes sense bc this info came from matrix
tab entered_in_rel entered_in_rel_sp, m // might be able to recover some data from here
// basically, the problems are if a. both entered in marriage and neither in marital history or b. both entered already cohab - bc they can enter in rel but I still have correct info...
tab current_rel_type entered_in_rel, m
tab entered_in_rel in_marital_history if current_rel_type==20, m
tab in_marital_history in_marital_history_sp, m
tab history_flag history_flag_sp, m
tab has_psid_gene has_psid_gene_sp, m
tab ever_transition ever_transition_sp, m // this is congruent also (I guess makes sense if also came from matrix info)
tab current_rel_status_est current_rel_status_est_sp, m // these are all over the place - so ideally can use teh one that is NOT estimated
tab rel_status_all rel_status_all_sp, m // these are all quite all over the place...

gen long partner_1 = cond(unique_id < partner_unique_id, unique_id, partner_unique_id) if partner_unique_id!=.
gen long partner_2 = cond(unique_id < partner_unique_id, partner_unique_id, unique_id) if partner_unique_id!=.
egen long couple_id = group(partner_1 partner_2)

browse unique_id partner_unique_id survey_yr partner_1 partner_2 couple_id

unique unique_id partner_unique_id if partner_unique_id!=.
unique couple_id if partner_unique_id!=.

browse couple_id unique_id partner_unique_id survey_yr in_marital_history in_marital_history_sp entered_in_rel entered_in_rel_sp current_rel_type current_rel_start current_rel_start_sp current_rel_end current_rel_end_sp rel_status_all rel_status_all_sp current_rel_status_est current_rel_status_est_sp ever_transition transition_year 

gen couple = 0
replace couple = 1 if inlist(current_rel_type, 20,22)
tab current_rel_start couple, m

tab couple relationship, m // the couple yes, relationship no are the first year cohabitors (bc in family matrix they are considered partners already). for now, I am filling in their info, but they are not considered "wife" so i actually can't even use first year cohabitors, unless I impute, which i then need to think about (check Brines and Joyner?)

gen flag = 0
replace flag = 1 if master_rel_type1 == 20 & in_marital_history==0 & in_marital_history_sp == 0 & entered_in_rel==1 & entered_in_rel_sp == 1 // want to check those couples where neither partner is in marital history and both entered in relationship - does this mean left censored?
replace flag = 1 if master_rel_type1 == 22 & entered_in_rel==1 & entered_in_rel_sp == 1 // if first rel is cohab - it doesn't matter if neitehr in marital history?
tab couple flag, m row // still a small percentage once adjusted for cohab, BUT affects cohab more (ofc)
tab current_rel_type flag, m row

// I first want to get current info to match across couples. also - like some info might match while other info does not...
gen rel_start_yr_couple = .
gen rel_end_yr_couple = .
gen how_end_couple = .

	// like first - if they match, okay great
	replace rel_start_yr_couple = current_rel_start if couple==1 & current_rel_start == current_rel_start_sp & current_rel_start!=.
	replace rel_end_yr_couple = current_rel_end if couple==1 & current_rel_end == current_rel_end_sp & current_rel_end!=.
	replace how_end_couple = current_rel_status_est if couple==1 & current_rel_status_est == current_rel_status_est_sp & current_rel_status_est!=.
	
	// if they don't match, and the relationship is a marriage, use the partner in marital history (if both not)
	replace rel_start_yr_couple = current_rel_start if rel_start_yr_couple==. & couple==1 & current_rel_type==20 & in_marital_history==1 & in_marital_history_sp==0
	replace rel_start_yr_couple = current_rel_start_sp if rel_start_yr_couple==. & couple==1 & current_rel_type==20 & in_marital_history==0 & in_marital_history_sp==1
	
	replace rel_end_yr_couple = current_rel_end if rel_end_yr_couple==. & couple==1 & current_rel_type==20 & in_marital_history==1 & in_marital_history_sp==0
	replace rel_end_yr_couple = current_rel_end_sp if rel_end_yr_couple==. & couple==1 & current_rel_type==20 & in_marital_history==0 & in_marital_history_sp==1
	
	replace how_end_couple = current_rel_status_est if how_end_couple==. & couple==1 & current_rel_type==20 & in_marital_history==1 & in_marital_history_sp==0
	replace how_end_couple = current_rel_status_est_sp if how_end_couple==. & couple==1 & current_rel_type==20 & in_marital_history==0 & in_marital_history_sp==1
	
	// widowhood is a specific problem as usually the one that dies disappears so only one of them gets widow info
	replace rel_end_yr_couple = current_rel_end if rel_end_yr_couple==. & couple==1 & rel_status_all==2 & rel_status_all_sp==. // want the info of the person recorded as WIDOWHOOD
	replace rel_end_yr_couple = current_rel_end if rel_end_yr_couple==. & couple==1 & rel_status_all==2 & rel_status_all_sp==0 // want the info of the person recorded as WIDOWHOOD
	replace rel_end_yr_couple = current_rel_end_sp if rel_end_yr_couple==. & couple==1 & rel_status_all==. & rel_status_all_sp==2
	replace rel_end_yr_couple = current_rel_end_sp if rel_end_yr_couple==. & couple==1 & rel_status_all==0 & rel_status_all_sp==2

	replace how_end_couple = 2 if how_end_couple==. & ((rel_status_all==2 & rel_status_all_sp==.) | (rel_status_all==. & rel_status_all_sp==2))
	replace how_end_couple = 2 if how_end_couple==. & ((rel_status_all==2 & rel_status_all_sp==0) | (rel_status_all==0 & rel_status_all_sp==2))
	
	// next prioritize those with psid gene
	replace rel_start_yr_couple = current_rel_start if rel_start_yr_couple==. & couple==1 & has_psid_gene==1 & has_psid_gene_sp==0
	replace rel_start_yr_couple = current_rel_start_sp if rel_start_yr_couple==. & couple==1 & has_psid_gene==0 & has_psid_gene_sp==1
	
	replace rel_end_yr_couple = current_rel_end if rel_end_yr_couple==. & couple==1 & has_psid_gene==1 & has_psid_gene_sp==0
	replace rel_end_yr_couple = current_rel_end_sp if rel_end_yr_couple==. & couple==1 & has_psid_gene==0 & has_psid_gene_sp==1
	
	replace how_end_couple = current_rel_status_est if how_end_couple==. & couple==1 & has_psid_gene==1 & has_psid_gene_sp==0
	replace how_end_couple = current_rel_status_est_sp if how_end_couple==. & couple==1 & has_psid_gene==0 & has_psid_gene_sp==1
	
	// not many but if someone missing and other not - use the non missing
	replace rel_start_yr_couple = current_rel_start if rel_start_yr_couple==. & couple==1 & current_rel_start!=. & current_rel_start_sp==.
	replace rel_start_yr_couple = current_rel_start_sp if rel_start_yr_couple==. & couple==1 & current_rel_start==. & current_rel_start_sp!=.
	
	replace rel_end_yr_couple = current_rel_end if rel_end_yr_couple==. & couple==1 & !inlist(current_rel_end,.,9998,9999) & inlist(current_rel_end_sp,.,9998,9999)
	replace rel_end_yr_couple = current_rel_end_sp if rel_end_yr_couple==. & couple==1 & inlist(current_rel_end,.,9998,9999) & !inlist(current_rel_end_sp,.,9998,9999)
	
	replace rel_end_yr_couple = 9999 if rel_end_yr_couple==. & couple==1 & ((current_rel_end==9999 & current_rel_end_sp==.) | (current_rel_end==. & current_rel_end_sp==9999))
	replace rel_end_yr_couple = 9999 if rel_end_yr_couple==. & couple==1 & how_end_couple==0 & current_rel_end==. & current_rel_end_sp==.
		// tab how_end_couple if couple==1 & current_rel_end==. & current_rel_end_sp==.
		
	// with no other info, use first start date and last end date
	egen min_start = rowmin(current_rel_start current_rel_start_sp)
	egen max_end = rowmax(current_rel_end current_rel_end_sp)
	// tab min_start if rel_start_yr_couple==. & couple==1, m
	// tab max_end if rel_end_yr_couple==. & couple==1, m 
	
	replace rel_start_yr_couple = min_start if rel_start_yr_couple==. & couple==1
	replace rel_end_yr_couple = max_end if rel_end_yr_couple==. & couple==1
	
	// think widowhood > breakup and breakup > intact
	replace how_end_couple = 2 if how_end_couple==. & ((current_rel_status_est==2 & current_rel_status_est_sp==1) | (current_rel_status_est==1 & current_rel_status_est_sp==2))
	replace how_end_couple = 1 if how_end_couple==. & ((current_rel_status_est==1 & current_rel_status_est_sp==0) | (current_rel_status_est==0 & current_rel_status_est_sp==1))
		
	label values how_end_couple how_rel_end
	
browse couple_id unique_id partner_unique_id survey_yr in_marital_history in_marital_history_sp entered_in_rel entered_in_rel_sp has_psid_gene has_psid_gene_sp current_rel_type rel_start_yr_couple rel_end_yr_couple how_end_couple current_rel_start current_rel_start_sp current_rel_end current_rel_end_sp rel_status_all rel_status_all_sp current_rel_status_est current_rel_status_est_sp last_survey_yr last_survey_yr_sp

tab rel_start_yr_couple if couple==1 & (current_rel_start!=. | current_rel_start_sp!=.), m
tab rel_end_yr_couple if  couple==1 & (current_rel_end!=. | current_rel_end_sp!=.), m
tab how_end_couple if  couple==1 & (current_rel_status_est!=. | current_rel_status_est_sp!=.), m
tab current_rel_status_est current_rel_status_est_sp if how_end_couple==. & couple==1, m

// but THEN need to adjust for transitions from marriage to cohabitation also...
sort unique_id survey_yr
browse couple_id unique_id partner_unique_id survey_yr ever_transition transition_year current_rel_type current_rel_number rel_start_yr_couple rel_end_yr_couple how_end_couple 

rename rel_start_yr_couple rel_start_yr_couple_unadj
rename rel_end_yr_couple rel_end_yr_couple_unadj
rename current_rel_number current_rel_number_unadj

unique unique_id partner_unique_id
unique couple_id 
unique couple_id rel_start_yr_couple_unadj rel_end_yr_couple_unadj // current_rel_number_unadj

bysort unique_id partner_unique_id: egen rel_start_yr_couple = min(rel_start_yr_couple_unadj) if partner_unique_id!=.
bysort unique_id partner_unique_id: egen rel_end_yr_couple = max(rel_end_yr_couple_unadj) if partner_unique_id!=.
bysort unique_id partner_unique_id: egen current_rel_number = min(current_rel_number_unadj) if partner_unique_id!=. 

inspect  rel_start_yr_couple  rel_start_yr_couple_unadj
inspect  rel_end_yr_couple  rel_end_yr_couple_unadj
inspect  current_rel_number  current_rel_number_unadj

sort couple_id partner_unique_id survey_yr

browse couple_id unique_id partner_unique_id survey_yr ever_transition transition_year current_rel_type current_rel_number rel_start_yr_couple rel_end_yr_couple how_end_couple current_rel_number_unadj rel_start_yr_couple_unadj rel_end_yr_couple_unadj

// probably need to also update "how end" - because we want info from the LAST partnership not the first (like cohab will probably say break up but married will say intact)
browse couple_id unique_id partner_unique_id survey_yr ever_transition transition_year current_rel_type current_rel_number rel_start_yr_couple rel_end_yr_couple how_end_couple current_rel_number_unadj rel_start_yr_couple_unadj rel_end_yr_couple_unadj if ever_transition==1
tab current_rel_type how_end_couple if ever_transition==1, row // pretty accurate

gen how_end_marriage = how_end_couple if current_rel_type==20 & ever_transition==1
bysort unique_id partner_unique_id (how_end_marriage): replace how_end_marriage=how_end_marriage[1] if ever_transition==1
tab how_end_marriage if ever_transition==1, m

tab how_end_couple how_end_marriage if ever_transition==1, row m
replace how_end_couple = how_end_marriage if ever_transition==1

// temp save - before I restrict to just couples
save "$temp/PSID_couples_compiled.dta", replace

********************************************************************************
**# Create outcome
********************************************************************************

sort unique_id survey_yr
tab rel_end_yr_couple current_rel_type, m // because I had to estimate cohab ends - most are recorded in a survey year - so this is where I always get confused, where is the dissolve recorded? I am predicting divorce by NEXT survey wave. because I need couple-level info as well. The way I set up exit rel is this way - observed together this year and not next year - so it is predicting by next wave
tab how_end_couple exit_rel, m row

browse couple_id unique_id partner_unique_id survey_yr current_rel_type current_rel_number rel_start_yr_couple rel_end_yr_couple how_end_couple marital_status_updated exit_rel

// use current rel type and marital status
gen dissolve = .
replace dissolve = 0 if couple==1
replace dissolve = 1 if inlist(current_rel_type,20,22) & current_rel_type[_n+1]==. & unique_id == unique_id[_n+1] & how_end_couple==1
replace dissolve=1 if marital_status_updated==1 & marital_status_updated[_n+1]==5 & unique_id == unique_id[_n+1] & how_end_couple==1
replace dissolve=1 if marital_status_updated==1 & marital_status_updated[_n+1]==6 & unique_id == unique_id[_n+1] & how_end_couple==1
replace dissolve=1 if marital_status_updated==2 & marital_status_updated[_n+1]==5 & unique_id == unique_id[_n+1] & how_end_couple==1
replace dissolve=1 if marital_status_updated==2 & marital_status_updated[_n+1]==6 & unique_id == unique_id[_n+1] & how_end_couple==1

// then end dates
replace dissolve = 1 if survey_yr == rel_end_yr_couple & how_end_couple==1
forvalues y=1/13{ // need this for people we don't observe in next year
	capture replace dissolve=1 if survey_yr == yr_end`y' & how_end_couple==1 // don't want WIDOWHOOD
	capture replace dissolve=1 if survey_yr == (yr_end`y'-1) & how_end_couple==1 & inlist(yr_end`y',1998,2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2020) // I think biennial years are causing problems, so update to prior year
	
	capture replace dissolve=1 if survey_yr == master_rel_end`y' & how_end_couple==1 // don't want WIDOWHOOD
	capture replace dissolve=1 if survey_yr == (master_rel_end`y'-1) & how_end_couple==1 & inlist(master_rel_end`y',1998,2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2020) // I think biennial years are causing problems, so update to prior year

}

// then fix these small issues with duplicated recorded years
replace dissolve = 0 if dissolve==1 & dissolve[_n-1]==1 & current_rel_type==. & inlist(current_rel_type[_n-1],20,22) & unique_id==unique_id[_n-1]
replace dissolve = 0 if dissolve==1 & dissolve[_n-1]==1 & inlist(marital_status_updated,5,6) & inlist(marital_status_updated[_n-1],1,2) & unique_id==unique_id[_n-1] 

tab dissolve, m
tab how_end_couple dissolve, m
tab marital_status_updated dissolve, m row
tab current_rel_type dissolve, m row
tab exit_rel dissolve, m // many of these discrepancies are bc of exits that aren't breakups, like widowhood and others are dissolve but not exit rel because I did not observe next year
tab exit_rel dissolve if how_end_couple==1, m

browse couple_id unique_id partner_unique_id survey_yr current_rel_type current_rel_number rel_start_yr_couple rel_end_yr_couple how_end_couple marital_status_updated exit_rel dissolve

********************************************************************************
**# Restrict to anyone in a relationship
********************************************************************************

gen total_relationship= 0 
replace total_relationship = 1 if relationship==1 | couple == 1 // for now, let's keep first yr cohabitors (the relationship v. couple discrepancies)
replace total_relationship = 1 if dissolve == 1 // need to keep year of dissolution - if I did this right (to predict by next wave), this should actually be fine

keep if total_relationship==1 

browse couple_id unique_id partner_unique_id survey_yr current_rel_type current_rel_number rel_start_yr_couple rel_end_yr_couple how_end_couple marital_status_updated exit_rel dissolve

unique unique_id if dissolve==1 // 14596
unique unique_id if how_end_couple==1 // 14972

tab relation current_rel_type, m
keep if inlist(relation,1,2) // this restricts to head / partner; we need this because otherwise don't get necessary info (e.g. housework)

tab current_rel_type flag, m row // left censored (not relevant here because I restrict a. to marriages and b. to people late enough that all are in marital history, e.g. past 1985)
browse couple_id unique_id partner_unique_id survey_yr current_rel_type relation

save "$temp/PSID_all_unions.dta", replace // this is now restricted to just couples, both cohab and married
