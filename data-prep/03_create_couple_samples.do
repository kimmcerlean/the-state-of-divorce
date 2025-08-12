********************************************************************************
* Getting PSID sample for union dissolution
* create_couple_samples.do
* Kim McErlean
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
drop if SEQ_NUMBER_==0 // won't have data because not in that year -- like SIPP, how do I know if last year is because divorced or last year in sample? right now individual level file, so fine - this is JUST last year in sample at the moment

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
// browse couple_id unique_id partner_unique_id survey_yr current_rel_type rel_start_yr_couple rel_end_yr_couple how_end_couple current_rel_start current_rel_start_sp current_rel_end current_rel_end_sp rel_status_all rel_status_all_sp current_rel_status_est current_rel_status_est_sp last_survey_yr last_survey_yr_sp if how_end_couple==. & couple==1
// browse couple_id unique_id partner_unique_id survey_yr current_rel_type rel_start_yr_couple rel_end_yr_couple how_end_couple current_rel_start current_rel_start_sp current_rel_end current_rel_end_sp rel_status_all rel_status_all_sp current_rel_status_est current_rel_status_est_sp last_survey_yr last_survey_yr_sp if inlist(unique_id, 75001,75002,2326174,2326171,5065001,5065002)

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
tab current_rel_type how_end_couple if ever_transition==1, row // I actually feel like this is not terrible inaccurate?

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
	// replace rel_end=1 if inlist(rel_type,20,22) & rel_type[_n+1]==0 & unique_id==unique_id[_n+1] & wave==wave[_n+1]-1 // yes observed together in this wave - NOT together next wave
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
// replace dissolve = 1 if survey_yr == last_couple_yr & rel_end_yr_couple==9999 & how_end_couple==1 // for those with missing end date - do I do this?

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

/*

Okay, I wanted to explore the data because the cohab creates a lot of chaos, so tested three ways of classifying so I can ensure I am really capturing divorces - moving the proper code above (mostly dissolve v1 with a few small edits based on marital status that are used in dissolve)

// current method (from when I did marriage - but adapted for cohab)
gen dissolve=.
replace dissolve=0 if couple==1
replace dissolve=1 if marital_status_updated==1 & marital_status_updated[_n+1]==5 & unique_id == unique_id[_n+1]  // & wave == wave[_n+1]-1 - okay, if the next time they appear, they are divorced, then this is probably fine? // married to divorce
replace dissolve=1 if marital_status_updated==1 & marital_status_updated[_n+1]==6 & unique_id == unique_id[_n+1] & how_end_couple!=0 // need to not capture temporary separations // married to separated
replace dissolve=1 if marital_status_updated==2 & marital_status_updated[_n+1]==3 & unique_id == unique_id[_n+1]  // cohab to single
replace dissolve=1 if marital_status_updated==2 & marital_status_updated[_n+1]==5 & unique_id == unique_id[_n+1] // cohab to divorce - shouldn't be possible, but jic - okay divorce does seem to be used. oh duh - if divorced, then partnered, then breakup- they prob are considered divorced? okay, so these are all possible. basically just not widowed
replace dissolve=1 if marital_status_updated==2 & marital_status_updated[_n+1]==6 & unique_id == unique_id[_n+1] & how_end_couple!=0  // cohab to separation - same here

replace dissolve = 1 if survey_yr == rel_end_yr_couple & how_end_couple==1
forvalues y=1/13{ // this is from marriage code, I guess use it for now...also add in the master rel_type so I have cohab
	capture replace dissolve=1 if survey_yr == yr_end`y' & how_end_couple==1 // don't want WIDOWHOOD
	capture replace dissolve=1 if survey_yr == (yr_end`y'-1) & how_end_couple==1 & inlist(yr_end`y',1998,2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2020) // I think biennial years are causing problems, so update to prior year
	
	capture replace dissolve=1 if survey_yr == master_rel_end`y' & how_end_couple==1 // don't want WIDOWHOOD
	capture replace dissolve=1 if survey_yr == (master_rel_end`y'-1) & how_end_couple==1 & inlist(master_rel_end`y',1998,2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2020) // I think biennial years are causing problems, so update to prior year

}

// replace dissolve = 1 if survey_yr == last_couple_yr & rel_end_yr_couple==9999 & how_end_couple==1 // for those with missing end date - do I do this? I think we have no idea if it ended then though or like...years in future....
replace dissolve = 0 if dissolve==1 & dissolve[_n-1]==1 & inlist(marital_status_updated,5,6) & inlist(marital_status_updated[_n-1],1,2) & unique_id==unique_id[_n-1] // there are people with two diff years recording as dissolved. putting it next to partnered one so I have both partners' info
replace dissolve = 0 if dissolve==1 & dissolve[_n-1]==1 & inlist(marital_status_updated,3,5,6) & marital_status_updated[_n-1]==2 & unique_id==unique_id[_n-1]

browse unique_id survey_yr marital_status_updated current_rel_type dissolve rel_start_yr_couple rel_end_yr_couple how_end_couple 

tab dissolve, m
tab how_end_couple dissolve, m
tab marital_status_updated dissolve, m row
tab exit_rel dissolve, m // many of these discrepancies are bc of exits that aren't breakups, like widowhood and others are dissolve but not exit rel because I did not observe next year
tab exit_rel dissolve if how_end_couple==1, m

// alt method - use current rel type instead of marital status
gen dissolve_v1 = .
replace dissolve_v1 = 0 if couple==1
replace dissolve_v1 = 1 if inlist(current_rel_type,20,22) & current_rel_type[_n+1]==. & unique_id == unique_id[_n+1] & how_end_couple==1

replace dissolve_v1 = 1 if survey_yr == rel_end_yr_couple & how_end_couple==1
forvalues y=1/13{ // need this for people we don't observe in next year
	capture replace dissolve_v1=1 if survey_yr == yr_end`y' & how_end_couple==1 // don't want WIDOWHOOD
	capture replace dissolve_v1=1 if survey_yr == (yr_end`y'-1) & how_end_couple==1 & inlist(yr_end`y',1998,2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2020) // I think biennial years are causing problems, so update to prior year
	
	capture replace dissolve_v1=1 if survey_yr == master_rel_end`y' & how_end_couple==1 // don't want WIDOWHOOD
	capture replace dissolve_v1=1 if survey_yr == (master_rel_end`y'-1) & how_end_couple==1 & inlist(master_rel_end`y',1998,2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2020) // I think biennial years are causing problems, so update to prior year

}

// replace dissolve_v1 = 1 if survey_yr == last_couple_yr & rel_end_yr_couple==9999 & how_end_couple==1 // for those with missing end date - do I do this?
replace dissolve_v1 = 0 if dissolve_v1==1 & dissolve_v1[_n-1]==1 & current_rel_type==. & inlist(current_rel_type[_n-1],20,22) & unique_id==unique_id[_n-1]

tab dissolve dissolve_v1, m

// another alt method - especially now that I created robust history...the small problem is just people with missing end date (about 4%)
tab rel_end_yr_couple how_end_couple, m
browse unique_id survey_yr marital_status_updated current_rel_type dissolve rel_start_yr_couple rel_end_yr_couple how_end_couple first_couple_yr last_couple_yr if rel_end_yr_couple==9999 & how_end_couple==1

gen dissolve_v2 = . // okay definitely not this one - it's because of timing of rel end + survey
replace dissolve_v2 = 0 if couple==1
replace dissolve_v2 = 1 if survey_yr == rel_end_yr_couple & how_end_couple==1
replace dissolve_v2 = 1 if survey_yr == (rel_end_yr_couple - 1) & how_end_couple==1 & inlist(rel_end_yr_couple,1998,2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2020) 
replace dissolve_v2 = 1 if survey_yr == last_couple_yr & rel_end_yr_couple==9999 & how_end_couple==1 // for those with missing end date

tab dissolve dissolve_v1, m row // dissolve_v1 has more 1s in general but some also don't match. my hesitation with original dissolve now that I use cohab is that marital status is not as accurate, especially also if not the head or partner (though, I eventually get rid of these people) - so yes this is one reason (e.g. 4191)
// I think a lot of these are first-yr cohabitors maybe also (e.g. 4210) - so, a lot of these discrepancies will go away anyway. but technically, I think dissolve_V1 is more accurate. need to just figure out when dissolve = 1 and v1 = 0
	// tab dissolve dissolve_v1 if current_rel_type==20, m cell
	// tab dissolve dissolve_v1 if current_rel_type==22, m cell // so yeah, discrepancies much larger for cohab, which we know cohab will cause problems because this is all observational not based on history
	// tab dissolve dissolve_v1 if survey_yr == last_couple_yr & rel_end_yr_couple==9999 & how_end_couple==1, m
	// tab dissolve dissolve_v1 if survey_yr == rel_end_yr_couple & how_end_couple==1, m
tab how_end_couple if dissolve==1 & dissolve_v1==0, m // okay yeah, some widowhood accidentally captured (I think because widowhood is weirdly recorded for the person who died)
tab dissolve dissolve_v2, m row // okay, this one is capturing way less than dissolve orig - especially for marriages (weirdly) - I think this is really for the interrupted relationships?
tab dissolve_v1 dissolve_v2, m row // so dissolve v2 is capturing the least - bc had to be observed in breakup year?  but some might only be observed year before depending on when in year breakup happened
// one reason is interruptions (eg 4006) - but that person, for example, I think did truly break up and get back together

browse couple_id unique_id partner_unique_id survey_yr current_rel_type current_rel_number rel_start_yr_couple rel_end_yr_couple how_end_couple marital_status_updated exit_rel dissolve dissolve_v1 dissolve_v2
*/

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
keep if inlist(relation,1,2) // this restricts to head / partner. but I am just thinking - do I actually to restrict to people ALWAYS head or partner? so I dont remove random years? how often do people change...
// some people enter not as head / partner. but this is why I control for duration - so not left censored in the sense of I have no idea
// reminder: this is how I denoted left censor. will leave for now, but should test with and without left censoring...
tab current_rel_type flag, m row
browse couple_id unique_id partner_unique_id survey_yr current_rel_type relation

save "$temp/PSID_all_unions.dta", replace // this is now restricted to just couples, both cohab and married

********************************************************************************
**# Recodes
********************************************************************************
// Education

/*
educ1 until 1990, but educ started 1975, okay but then a gap until 1991? wife not asked 1969-1971 - might be able to fill in if she is in sample either 1968 or 1972? (match to the id). also look at yrs education (missing 69 and 74?)

codes are also different between the two, use educ1 until 1990, then educ 1991 post
early educ:
0. cannot read
1. 0-5th grade
2. 6-8th grade
3. 9-11 grade
4/5. 12 grade
6. college no degree
7/8. college / advanced degree
9. dk

later educ: years of education
*/

* clean up intermediary variables
label values YRS_EDUCATION_INDV .

gen hs_head=.
replace hs_head=1 if inlist(HS_GRAD_HEAD_,1,2)
replace hs_head=0 if HS_GRAD_HEAD_==3

gen hs_wife=.
replace hs_wife=1 if inlist(HS_GRAD_WIFE_,1,2)
replace hs_wife=0 if HS_GRAD_WIFE_==3

gen attended_college_head=.
replace attended_college_head= 0 if ATTENDED_COLLEGE_HEAD_==5
replace attended_college_head= 1 if ATTENDED_COLLEGE_HEAD_==1

gen attended_college_wife=.
replace attended_college_wife= 0 if ATTENDED_COLLEGE_WIFE_==5
replace attended_college_wife= 1 if ATTENDED_COLLEGE_WIFE_==1

gen completed_college_head=.
replace completed_college_head= 0 if COLLEGE_HEAD_==5
replace completed_college_head= 1 if COLLEGE_HEAD_==1
replace completed_college_head= 0 if attended_college_head==0

gen completed_college_wife=.
replace completed_college_wife= 0 if COLLEGE_WIFE_==5
replace completed_college_wife= 1 if COLLEGE_WIFE_==1
replace completed_college_wife= 0 if attended_college_wife==0

gen completed_college_indv=.
replace completed_college_indv= 0 if COLLEGE_INDV_==5
replace completed_college_indv= 1 if COLLEGE_INDV_==1

gen college_degree_head=.
replace college_degree_head=0 if HIGHEST_DEGREE_HEAD_==0
replace college_degree_head=1 if HIGHEST_DEGREE_HEAD_==1 // associates
replace college_degree_head=2 if inrange(HIGHEST_DEGREE_HEAD_,2,6) // bachelor's plus

gen college_degree_wife=.
replace college_degree_wife=0 if HIGHEST_DEGREE_WIFE_==0
replace college_degree_wife=1 if HIGHEST_DEGREE_WIFE_==1 // associates
replace college_degree_wife=2 if inrange(HIGHEST_DEGREE_WIFE_,2,6) // bachelor's plus

label define degree 0 "No Coll" 1 "Assoc" 2 "BA+"
label values college_degree_head college_degree_wife

tab attended_college_head completed_college_head, m
tab completed_college_head college_degree_head, m

replace NEW_HEAD_YEAR = 1900+NEW_HEAD_YEAR if NEW_HEAD_YEAR>0 & NEW_HEAD_YEAR<100
replace NEW_WIFE_YEAR = 1900+NEW_WIFE_YEAR if NEW_WIFE_YEAR>0 & NEW_WIFE_YEAR<100

recode EDUC1_WIFE_ (1/3=1)(4/5=2)(6=3)(7/8=4)(9=.)(0=.), gen(educ_wife_early)
recode EDUC1_HEAD_ (0/3=1)(4/5=2)(6=3)(7/8=4)(9=.), gen(educ_head_early)
recode EDUC_WIFE_ (1/11=1) (12=2) (13/15=3) (16/17=4) (99=.)(0=.), gen(educ_wife_1975)
recode EDUC_HEAD_ (0/11=1) (12=2) (13/15=3) (16/17=4) (99=.), gen(educ_head_1975)
recode YRS_EDUCATION_INDV (1/11=1) (12=2) (13/15=3) (16/17=4) (98/99=.)(0=.), gen(educ_completed) // okay this is hard to use because head / wife ONLY recorded against those specific ones so they don't always have values here

label define educ 1 "LTHS" 2 "HS" 3 "Some College" 4 "College"
label values educ_wife_early educ_head_early educ_wife_1975 educ_head_1975 educ_completed educ

browse unique_id survey_yr in_sample relation YRS_EDUCATION_INDV educ_completed educ_head_early educ_head_1975 hs_head HS_GRAD_HEAD attended_college_head completed_college_head college_degree_head BACHELOR_YR_HEAD_ YR_EDUC_UPD_HEAD_ NEW_HEAD_ NEW_HEAD_YEAR if relation==1 // using head right now to wrap my head around

* create final education variables
gen educ_head_est=. // this is more accurate
replace educ_head_est=1 if hs_head==0
replace educ_head_est=2 if hs_head==1 & attended_college_head==0
replace educ_head_est=3 if hs_head==1 & attended_college_head==1 & completed_college_head==0
replace educ_head_est=3 if completed_college_head==1 & college_degree_head==1
replace educ_head_est=4 if completed_college_head==1 & college_degree_head==2

gen educ_head=.
replace educ_head=educ_head_early if inrange(survey_yr,1968,1990)
replace educ_head=educ_head_1975 if inrange(survey_yr,1991,2021)

tab educ_head educ_head_est, m
tab educ_completed educ_head_est if relation==1, m
tab educ_head educ_completed if educ_head_est==., m
tab YRS_EDUCATION_INDV educ_head_est if relation==1, m

replace educ_head_est = educ_completed if educ_head_est==. & educ_completed!=. & relation==1
replace educ_head_est = educ_head if educ_head_est==. & educ_head!=.

browse unique_id survey_yr educ_head educ_completed educ_head_est YRS_EDUCATION_INDV  hs_head attended_college_head completed_college_head college_degree_head if relation==1 

gen educ_wife_est=.  // this is more accurate
replace educ_wife_est=1 if hs_wife==0
replace educ_wife_est=2 if hs_wife==1 & attended_college_wife==0
replace educ_wife_est=3 if hs_wife==1 & attended_college_wife==1 & completed_college_wife==0
replace educ_wife_est=3 if completed_college_wife==1 & college_degree_wife==1
replace educ_wife_est=4 if completed_college_wife==1 & college_degree_wife==2

gen educ_wife=.
replace educ_wife=educ_wife_early if inrange(survey_yr,1968,1990)
replace educ_wife=educ_wife_1975 if inrange(survey_yr,1991,2021)
tab survey_yr educ_wife, m 

replace educ_wife_est = educ_completed if educ_wife_est==. & educ_completed!=. & relation==2
replace educ_wife_est = educ_wife if educ_wife_est==. & educ_wife!=.

tab educ_wife educ_wife_est, m
tab educ_completed educ_wife_est if relation==2, m
tab educ_wife educ_completed if educ_wife_est==. & relation==2, m
tab YRS_EDUCATION_INDV educ_wife_est if relation==2, m

label values educ_head educ_wife educ_head_est educ_wife_est educ

gen college_wife=.
replace college_wife=0 if inrange(educ_wife_est,1,3)
replace college_wife=1 if educ_wife_est==4

gen college_head=.
replace college_head=0 if inrange(educ_head_est,1,3)
replace college_head=1 if educ_head_est==4
tab college_degree_head college_head, m

gen college_indv=.
replace college_indv=0 if inrange(educ_completed,1,3)
replace college_indv=1 if educ_completed==4

gen couple_educ_gp=.
replace couple_educ_gp=0 if college_wife==0 & college_head==0
replace couple_educ_gp=1 if (college_wife==1 | college_head==1)

label define couple_educ 0 "Neither College" 1 "At Least One College"
label values couple_educ_gp couple_educ

gen educ_type=.
replace educ_type=1 if couple_educ_gp==0
replace educ_type=2 if college_wife==0 & college_head==1
replace educ_type=3 if college_wife==1 & college_head==0
replace educ_type=4 if college_wife==1 & college_head==1

label define educ_type 1 "Neither College" 2 "His College" 3 "Her College" 4 "Both College"
label values educ_type educ_type

// income and division of paid labor
browse unique_id survey_yr FAMILY_INTERVIEW_NUM_ TAXABLE_T1_HEAD_WIFE TOTAL_INCOME_T1_FAMILY LABOR_INCOME_T1_HEAD WAGES_ALT_T1_HEAD WAGES_T1_HEAD LABOR_INCOME_T2_HEAD LABOR_INCOME_T1_WIFE_ WAGES_T1_WIFE_  LABOR_INCOME_T2_WIFE_

	// to use: WAGES_HEAD_ WAGES_WIFE_ -- wife not asked until 1993? okay labor income??
	// wages and labor income asked for head whole time. labor income wife 1968-1993, wages for wife, 1993 onwards

gen earnings_t1_wife=.
replace earnings_t1_wife = LABOR_INCOME_T1_WIFE_ if inrange(survey_yr,1968,1993)
replace earnings_t1_wife = WAGES_T1_WIFE_ if inrange(survey_yr,1994,2021)
replace earnings_t1_wife=. if earnings_t1_wife== 9999999

gen earnings_t1_head=.
replace earnings_t1_head = LABOR_INCOME_T1_HEAD if inrange(survey_yr,1968,1993)
replace earnings_t1_head = WAGES_T1_HEAD if inrange(survey_yr,1994,2021)
replace earnings_t1_head=. if earnings_t1_head== 9999999

egen couple_earnings_t1 = rowtotal(earnings_t1_wife earnings_t1_head)
browse unique_id survey_yr TAXABLE_T1_HEAD_WIFE couple_earnings_t1 earnings_t1_wife earnings_t1_head
	
gen female_earn_pct_t1 = earnings_t1_wife/(couple_earnings_t1)

gen hh_earn_type_t1=.
replace hh_earn_type_t1=1 if female_earn_pct_t1 >=.4000 & female_earn_pct_t1 <=.6000
replace hh_earn_type_t1=2 if female_earn_pct_t1 < .4000 & female_earn_pct_t1 >=0
replace hh_earn_type_t1=3 if female_earn_pct_t1 > .6000 & female_earn_pct_t1 <=1
replace hh_earn_type_t1=4 if earnings_t1_head==0 & earnings_t1_wife==0

label define hh_earn_type 1 "Dual Earner" 2 "Male BW" 3 "Female BW" 4 "No Earners"
label values hh_earn_type_t1 hh_earn_type

sort unique_id survey_yr

gen hh_earn_type_t=.
replace hh_earn_type_t=hh_earn_type_t1[_n+1] if unique_id==unique_id[_n+1] & wave==wave[_n+1]-1
label values hh_earn_type_t hh_earn_type

gen female_earn_pct_t=.
replace female_earn_pct_t=female_earn_pct_t1[_n+1] if unique_id==unique_id[_n+1] & wave==wave[_n+1]-1

browse unique_id survey_yr wave earnings_t1_head earnings_t1_wife hh_earn_type_t1 hh_earn_type_t female_earn_pct_t1 female_earn_pct_t

// hours instead of earnings	
browse unique_id survey_yr WEEKLY_HRS1_T1_WIFE_ WEEKLY_HRS_T1_WIFE_ WEEKLY_HRS1_T1_HEAD_ WEEKLY_HRS_T1_HEAD_

gen weekly_hrs_t1_wife = .
replace weekly_hrs_t1_wife = WEEKLY_HRS1_T1_WIFE_ if survey_yr > 1969 & survey_yr <1994
replace weekly_hrs_t1_wife = WEEKLY_HRS_T1_WIFE_ if survey_yr >=1994
replace weekly_hrs_t1_wife = 0 if inrange(survey_yr,1968,1969) & inlist(WEEKLY_HRS1_T1_WIFE_,9,0)
replace weekly_hrs_t1_wife = 10 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_WIFE_ ==1
replace weekly_hrs_t1_wife = 27 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_WIFE_ ==2
replace weekly_hrs_t1_wife = 35 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_WIFE_ ==3
replace weekly_hrs_t1_wife = 40 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_WIFE_ ==4
replace weekly_hrs_t1_wife = 45 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_WIFE_ ==5
replace weekly_hrs_t1_wife = 48 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_WIFE_ ==6
replace weekly_hrs_t1_wife = 55 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_WIFE_ ==7
replace weekly_hrs_t1_wife = 60 if inrange(survey_yr,1968,1969)  & WEEKLY_HRS1_T1_WIFE_ ==8
replace weekly_hrs_t1_wife=. if weekly_hrs_t1_wife==999

gen weekly_hrs_t1_head = .
replace weekly_hrs_t1_head = WEEKLY_HRS1_T1_HEAD_ if survey_yr > 1969 & survey_yr <1994
replace weekly_hrs_t1_head = WEEKLY_HRS_T1_HEAD_ if survey_yr >=1994
replace weekly_hrs_t1_head = 0 if inrange(survey_yr,1968,1969) & inlist(WEEKLY_HRS1_T1_HEAD_,9,0)
replace weekly_hrs_t1_head = 10 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_HEAD_ ==1
replace weekly_hrs_t1_head = 27 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_HEAD_ ==2
replace weekly_hrs_t1_head = 35 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_HEAD_ ==3
replace weekly_hrs_t1_head = 40 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_HEAD_ ==4
replace weekly_hrs_t1_head = 45 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_HEAD_ ==5
replace weekly_hrs_t1_head = 48 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_HEAD_ ==6
replace weekly_hrs_t1_head = 55 if inrange(survey_yr,1968,1969) & WEEKLY_HRS1_T1_HEAD_ ==7
replace weekly_hrs_t1_head = 60 if inrange(survey_yr,1968,1969)  & WEEKLY_HRS1_T1_HEAD_ ==8
replace weekly_hrs_t1_head=. if weekly_hrs_t1_head==999

egen couple_hours_t1 = rowtotal(weekly_hrs_t1_wife weekly_hrs_t1_head)
gen female_hours_pct_t1 = weekly_hrs_t1_wife/couple_hours_t1

gen hh_hours_type_t1=.
replace hh_hours_type_t1=1 if female_hours_pct_t1 >=.4000 & female_hours_pct_t1 <=.6000
replace hh_hours_type_t1=2 if female_hours_pct_t1 <.4000
replace hh_hours_type_t1=3 if female_hours_pct_t1 >.6000 & female_hours_pct_t1!=.
replace hh_hours_type_t1=4 if weekly_hrs_t1_head==0 & weekly_hrs_t1_head==0

label define hh_hours_type 1 "Dual Earner" 2 "Male BW" 3 "Female BW" 4 "No Earners"
label values hh_hours_type_t1 hh_hours_type

gen hh_hours_type_t=.
replace hh_hours_type_t=hh_hours_type_t1[_n+1] if unique_id==unique_id[_n+1] & wave==wave[_n+1]-1
label values hh_hours_type_t hh_hours_type

gen female_hours_pct_t=.
replace female_hours_pct_t=female_hours_pct_t1[_n+1] if unique_id==unique_id[_n+1] & wave==wave[_n+1]-1

// housework hours - not totally sure if accurate prior to 1976
browse unique_id survey_yr RELATION_ HOUSEWORK_HEAD_ HOUSEWORK_WIFE_ HOUSEWORK_INDV_ TOTAL_HOUSEWORK_T1_HW MOST_HOUSEWORK_T1 // total and most HW stopped after 1974

gen housework_head = HOUSEWORK_HEAD_
replace housework_head = (HOUSEWORK_HEAD_/52) if inrange(survey_yr,1968,1974)
replace housework_head = HOUSEWORK_INDV_ if relationship==1 & inrange(survey_yr,1968,1974) & HOUSEWORK_INDV_!=.
replace housework_head=. if inlist(housework_head,998,999)

gen housework_wife = HOUSEWORK_WIFE_
replace housework_wife = (HOUSEWORK_WIFE_/52) if inrange(survey_yr,1968,1974)
replace housework_wife = HOUSEWORK_INDV_ if relationship==2 & inrange(survey_yr,1968,1974) & HOUSEWORK_INDV_!=.
replace housework_wife=. if inlist(housework_wife,998,999)

gen total_housework_weekly = TOTAL_HOUSEWORK_T1_HW / 52

browse unique_id survey_yr relationship housework_head housework_wife HOUSEWORK_HEAD_ HOUSEWORK_WIFE_ HOUSEWORK_INDV_

egen couple_housework_t1 = rowtotal (housework_wife housework_head)
browse id survey_yr housework_head housework_wife couple_housework total_housework_weekly TOTAL_HOUSEWORK_T1_HW MOST_HOUSEWORK_T1

gen wife_housework_pct_t = housework_wife / couple_housework_t

gen housework_bkt_t=.
replace housework_bkt_t=1 if wife_housework_pct_t >=.4000 & wife_housework_pct_t <=.6000
replace housework_bkt_t=2 if wife_housework_pct_t >.6000 & wife_housework_pct_t!=.
replace housework_bkt_t=3 if wife_housework_pct_t <.4000
replace housework_bkt_t=4 if housework_wife==0 & housework_head==0

label define housework_bkt 1 "Dual HW" 2 "Female Primary" 3 "Male Primary" 4 "NA"
label values housework_bkt_t housework_bkt

sort unique_id survey_yr
gen housework_bkt_t1=.
replace housework_bkt_t1=housework_bkt_t[_n-1] if unique_id==unique_id[_n-1] & wave==wave[_n-1]+1
label values housework_bkt_t1 housework_bkt

gen wife_housework_pct_t1=.
replace wife_housework_pct_t1=wife_housework_pct_t[_n-1] if unique_id==unique_id[_n-1] & wave==wave[_n-1]+1

// browse id survey_yr housework_bkt_t housework_bkt_t1 wife_housework_pct_t wife_housework_pct_t1

// combined indicator of paid and unpaid, using HOURS
gen hours_housework_t=.
replace hours_housework_t=1 if hh_hours_type_t==1 & housework_bkt_t==1 // dual both (egal)
replace hours_housework_t=2 if hh_hours_type_t==1 & housework_bkt_t==2 // dual earner, female HM (second shift)
replace hours_housework_t=3 if hh_hours_type_t==2 & housework_bkt_t==2 // male BW, female HM (traditional)
replace hours_housework_t=4 if hh_hours_type_t==3 & housework_bkt_t==3 // female BW, male HM (counter-traditional)
replace hours_housework_t=5 if hours_housework_t==. & hh_hours_type_t!=. & housework_bkt_t!=. // all others

label define combined_dol 1 "Egal" 2 "Second Shift" 3 "Traditional" 4 "Counter Traditional" 5 "All others"
label values hours_housework_t combined_dol 

gen hours_housework_t1=.
replace hours_housework_t1=1 if hh_hours_type_t1==1 & housework_bkt_t1==1 // dual both (egal)
replace hours_housework_t1=2 if hh_hours_type_t1==1 & housework_bkt_t1==2 // dual earner, female HM (second shift)
replace hours_housework_t1=3 if hh_hours_type_t1==2 & housework_bkt_t1==2 // male BW, female HM (traditional)
replace hours_housework_t1=4 if hh_hours_type_t1==3 & housework_bkt_t1==3 // female BW, male HM (counter-traditional)
replace hours_housework_t1=5 if hours_housework_t1==. & hh_hours_type_t1!=. & housework_bkt_t1!=. // all others

label values hours_housework_t1 combined_dol 

// now EARNINGS
gen earn_housework_t=.
replace earn_housework_t=1 if hh_earn_type_t==1 & housework_bkt_t==1 // dual both (egal)
replace earn_housework_t=2 if hh_earn_type_t==1 & housework_bkt_t==2 // dual earner, female HM (second shift)
replace earn_housework_t=3 if hh_earn_type_t==2 & housework_bkt_t==2 // male BW, female HM (traditional)
replace earn_housework_t=4 if hh_earn_type_t==3 & housework_bkt_t==3 // female BW, male HM (counter-traditional)
replace earn_housework_t=5 if earn_housework_t==. & hh_earn_type_t!=. & housework_bkt_t!=. // all others

label values earn_housework_t combined_dol 

gen earn_housework_t1=.
replace earn_housework_t1=1 if hh_earn_type_t1==1 & housework_bkt_t1==1 // dual both (egal)
replace earn_housework_t1=2 if hh_earn_type_t1==1 & housework_bkt_t1==2 // dual earner, female HM (second shift)
replace earn_housework_t1=3 if hh_earn_type_t1==2 & housework_bkt_t1==2 // male BW, female HM (traditional)
replace earn_housework_t1=4 if hh_earn_type_t1==3 & housework_bkt_t1==3 // female BW, male HM (counter-traditional)
replace earn_housework_t1=5 if earn_housework_t1==. & hh_earn_type_t1!=. & housework_bkt_t1!=. // all others

label values earn_housework_t1 combined_dol 

/* turning the bucket interactions into variables to interact over time
gen hours_housework=.
replace hours_housework=1 if hh_hours_3070==1 & housework_bkt==1 // dual both (egal)
replace hours_housework=2 if hh_hours_3070==1 & housework_bkt==2 // dual earner, female HM (neotraditional)
replace hours_housework=3 if hh_hours_3070==2 & housework_bkt==1 // male BW, dual HW (mm not sure)
replace hours_housework=4 if hh_hours_3070==2 & housework_bkt==2 // male BW, female HM (conventional)
replace hours_housework=5 if hh_hours_3070==3 & housework_bkt==1 // female BW, dual HW (gender-atypical)
replace hours_housework=6 if hh_hours_3070==3 & housework_bkt==2 // female BW, female HM (undoing gender)
replace hours_housework=7 if housework_bkt==3  // all where male does more housework (gender-atypical)
replace hours_housework=8 if hh_hours_3070==4  // no earners

label define hours_housework 1 "Egal" 2 "Neotraditional" 3 "Male BW, dual HW" 4 "Conventional" 5 "Gender-atypical" 6 "Undoing gender" 7 "Male HW dominant" 8 "No Earners"
label values hours_housework hours_housework 
*/

// employment
browse id survey_yr EMPLOY_STATUS_HEAD_ EMPLOY_STATUS1_HEAD_ EMPLOY_STATUS2_HEAD_ EMPLOY_STATUS3_HEAD_ EMPLOY_STATUS_WIFE_ EMPLOY_STATUS1_WIFE_ EMPLOY_STATUS2_WIFE_ EMPLOY_STATUS3_WIFE_
// not numbered until 1994; 1-3 arose in 1994. codes match
// wife not asked until 1976?

gen employ_head=.
replace employ_head=0 if inrange(EMPLOY_STATUS_HEAD_,2,9)
replace employ_head=1 if EMPLOY_STATUS_HEAD_==1
gen employ1_head=.
replace employ1_head=0 if inrange(EMPLOY_STATUS1_HEAD_,2,8)
replace employ1_head=1 if EMPLOY_STATUS1_HEAD_==1
gen employ2_head=.
replace employ2_head=0 if EMPLOY_STATUS2_HEAD_==0 | inrange(EMPLOY_STATUS2_HEAD_,2,8)
replace employ2_head=1 if EMPLOY_STATUS2_HEAD_==1
gen employ3_head=.
replace employ3_head=0 if EMPLOY_STATUS3_HEAD_==0 | inrange(EMPLOY_STATUS3_HEAD_,2,8)
replace employ3_head=1 if EMPLOY_STATUS3_HEAD_==1

browse employ_head employ1_head employ2_head employ3_head
egen employed_head=rowtotal(employ_head employ1_head employ2_head employ3_head), missing
replace employed_head=1 if employed_head==2

gen employ_wife=.
replace employ_wife=0 if inrange(EMPLOY_STATUS_WIFE_,2,9)
replace employ_wife=1 if EMPLOY_STATUS_WIFE_==1
gen employ1_wife=.
replace employ1_wife=0 if inrange(EMPLOY_STATUS1_WIFE_,2,8)
replace employ1_wife=1 if EMPLOY_STATUS1_WIFE_==1
gen employ2_wife=.
replace employ2_wife=0 if EMPLOY_STATUS2_WIFE_==0 | inrange(EMPLOY_STATUS2_WIFE_,2,8)
replace employ2_wife=1 if EMPLOY_STATUS2_WIFE_==1
gen employ3_wife=.
replace employ3_wife=0 if EMPLOY_STATUS3_WIFE_==0 | inrange(EMPLOY_STATUS3_WIFE_,2,8)
replace employ3_wife=1 if EMPLOY_STATUS3_WIFE_==1

egen employed_wife=rowtotal(employ_wife employ1_wife employ2_wife employ3_wife), missing
replace employed_wife=1 if employed_wife==2

browse id survey_yr employed_head employed_wife employ_head employ1_head employ_wife employ1_wife

/* problem is this employment is NOW not last year. I want last year? use if wages = employ=yes, then no? (or hours)
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
*/

gen ft_pt_t1_head=.
replace ft_pt_t1_head = 0 if weekly_hrs_t1_head==0
replace ft_pt_t1_head = 1 if weekly_hrs_t1_head > 0 & weekly_hrs_t1_head<35
replace ft_pt_t1_head = 2 if weekly_hrs_t1_head >= 35 & weekly_hrs_t1_head < 999

gen ft_pt_t1_wife=.
replace ft_pt_t1_wife = 0 if weekly_hrs_t1_wife==0
replace ft_pt_t1_wife = 1 if weekly_hrs_t1_wife > 0 & weekly_hrs_t1_wife<35
replace ft_pt_t1_wife = 2 if weekly_hrs_t1_wife >= 35 & weekly_hrs_t1_wife < 999

label define ft_pt 0 "Not Employed" 1 "PT" 2 "FT"
label values ft_pt_t1_head ft_pt_t1_wife ft_pt

gen ft_t1_head=0
replace ft_t1_head=1 if ft_pt_t1_head==2
replace ft_t1_head=. if ft_pt_t1_head==.

gen ft_t1_wife=0
replace ft_t1_wife=1 if ft_pt_t1_wife==2
replace ft_t1_wife=. if ft_pt_t1_wife==.

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

gen race_4_wife_rec=.
replace race_4_wife_rec=1 if RACE_4_WIFE_==1
replace race_4_wife_rec=2 if RACE_4_WIFE_==2
replace race_4_wife_rec=3 if (inrange(survey_yr,1985,2021) & RACE_4_WIFE_==3)
replace race_4_wife_rec=4 if (inrange(survey_yr,1985,2021) & RACE_4_WIFE_==4)
replace race_4_wife_rec=5 if (inrange(survey_yr,1968,1984) & RACE_4_WIFE_==3) | (inrange(survey_yr,1990,2003) & RACE_4_WIFE_==5)
replace race_4_wife_rec=6 if RACE_4_WIFE_==7 | (inrange(survey_yr,1990,2003) & RACE_4_WIFE_==6) | (inrange(survey_yr,2005,2021) & RACE_4_WIFE_==5) | (inrange(survey_yr,1985,1989) & RACE_4_WIFE_==8)

browse unique_id race_1_head_rec race_2_head_rec race_3_head_rec race_4_head_rec

// based on first mention (that is one option they use in SHELF)
gen race_wife=race_1_wife_rec
replace race_wife=7 if race_2_wife_rec!=.

gen race_head=race_1_head_rec
replace race_head=7 if race_2_head_rec!=.

label define race 1 "White" 2 "Black" 3 "Indian" 4 "Asian" 5 "Latino" 6 "Other" 7 "Multi-racial"
label values race_wife race_head race

// ethnicity
gen hispanic_head=.
replace hispanic_head=0 if HISPANICITY_HEAD_==0
replace hispanic_head=1 if inrange(HISPANICITY_HEAD_,1,7)

gen hispanic_wife=.
replace hispanic_wife=0 if HISPANICITY_WIFE_==0
replace hispanic_wife=1 if inrange(HISPANICITY_WIFE_,1,7)

tab race_head hispanic_head, m

// combined
gen raceth_head=.
replace raceth_head=1 if race_head==1 & (hispanic_head==0 | hispanic_head==.)
replace raceth_head=2 if race_head==2
replace raceth_head=3 if hispanic_head==1 & race_head!=2 // hispanic, non-black
replace raceth_head=3 if race_head==5 & (hispanic_head==0 | hispanic_head==.)
replace raceth_head=4 if race_head==4 & (hispanic_head==0 | hispanic_head==.)
replace raceth_head=5 if inlist(race_head,3,6,7) & (hispanic_head==0 | hispanic_head==.)

tab raceth_head, m
tab race_head raceth_head, m

gen raceth_wife=.
replace raceth_wife=1 if race_wife==1 & (hispanic_wife==0 | hispanic_wife==.)
replace raceth_wife=2 if race_wife==2
replace raceth_wife=3 if hispanic_wife==1 & race_wife!=2 // hispanic, non-black
replace raceth_wife=3 if race_wife==5 & (hispanic_wife==0 | hispanic_wife==.)
replace raceth_wife=4 if race_wife==4 & (hispanic_wife==0 | hispanic_wife==.)
replace raceth_wife=5 if inlist(race_wife,3,6,7) & (hispanic_wife==0 | hispanic_wife==.)

label define raceth 1 "NH White" 2 "Black" 3 "Hispanic" 4 "NH Asian" 5 "NH Other"
labe values raceth_head raceth_wife raceth

// figure out how to make time invariant, re: SHELF
tab raceth_head in_sample, m
tab raceth_wife in_sample, m
browse unique_id survey_yr raceth_head raceth_wife

// bysort unique_id: egen raceth_head_fixed = median(raceth_head)
bysort unique_id: egen raceth_head_fixed = mode(raceth_head) // majority
tab raceth_head_fixed, m
gen last_race_head=raceth_head if survey_yr==last_survey_yr // tie break with last reported
bysort unique_id (last_race_head): replace last_race_head = last_race_head[1]
sort unique_id survey_yr
browse unique_id survey_yr last_survey_yr raceth_head raceth_head_fixed last_race_head
replace raceth_head_fixed=last_race_head if raceth_head_fixed==.
tab raceth_head if raceth_head_fixed==., m

bysort unique_id: egen raceth_wife_fixed = mode(raceth_wife) // majority
tab raceth_wife_fixed, m
gen last_race_wife=raceth_wife if survey_yr==last_survey_yr // tie break with last reported
bysort unique_id (last_race_wife): replace last_race_wife = last_race_wife[1]
sort unique_id survey_yr
browse unique_id survey_yr last_survey_yr raceth_wife raceth_wife_fixed last_race_wife
replace raceth_wife_fixed=last_race_wife if raceth_wife_fixed==.

// realizing - I shouldn't do this this way becauase the head / wife can change over time (one reason that head / wife might seemingly change over time rather than data errors for the same person)

// if partners same race
gen same_race=0
replace same_race=1 if raceth_head==raceth_wife & raceth_head!=.

// any children - need to get more specific
label values NUM_CHILDREN_ .

gen children=.
replace children=0 if NUM_CHILDREN_==0
replace children=1 if NUM_CHILDREN_>=1 & NUM_CHILDREN_!=.

bysort unique_id: egen children_ever = max(NUM_CHILDREN_)
replace children_ever=1 if children_ever>0

sort unique_id survey_yr
browse unique_id survey_yr NUM_CHILDREN_ children_ever

browse survey_yr unique_id NUM_CHILDREN_ NUM_BIRTHS FIRST_BIRTH_YR BIRTHS_T1_HEAD_ BIRTHS_T1_WIFE_ BIRTHS_T1_BOTH_

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
browse unique_id survey_yr when_first_birth FIRST_BIRTH_YR first_birth_check children_ever NUM_BIRTHS NUM_CHILDREN_ rel_start_yr_couple

gen pre_marital_birth=0
replace pre_marital_birth=1 if when_first_birth < rel_start_yr_couple & when_first_birth!=.

gen post_marital_birth=0
replace post_marital_birth=1 if when_first_birth >= rel_start_yr_couple & when_first_birth<=rel_end_yr_couple & when_first_birth!=. // needs to be IN marriage years, okay barely changed it

// urbanicity
gen metro=(METRO_==1) // a lot of missing, don't use for now

// religion
tabstat RELIGION_WIFE_ RELIGION_HEAD_, by(survey_yr) // just to get a sense of when asked to start.
label values RELIGION_WIFE_ RELIGION_HEAD_ . // these values are v wrong
/* head was 1970-1977, 1979-2021. wife was 1976, 1985-2021
Okay, but some weird things with how asked: 
In 1979, when this question was reinstated in the questionnaire, values were not brought forward for families with unchanged Heads since 1977.
For those cases with the same Heads from 1977 through the present, please use 1977 religious preference, V5617
So, most missings after 1977 can be interpreted as no new head, so use 1977 value? Is this another that might help if I edit once I have the variables assigned to the focal person?
Okay, but I *think* starting in 1985, was asked to everyone again? Because number of 0s goes down and the note is gone. okay, carried forward again starting 1986.
So carry through 1977-1984 if in sample and same head / partner?

The codes changed wildly over the years?
1970-1984 - 0: No or Other, 1: Baptist, 2: Methodist, 3: Episcopalian, 4: Presbyterian, 5: Lutheran, 6: Unitarian, Mormon, and related, 7: Other Protestant, 8: Catholic, 9: Jewish
1985-1987 - 0: None, 1: Roman Catholic, 2: Jewish, 3: Baptist, 4: Lutheran, 5: Methodist, 6: Presbyterian, 7: Episcopalian, 8: Protestant unspecified, 9: Other Protestant, 10: Other non-Christian, 11: LDS, 12: Jehvah's Witnesses
13: Greek Orthodox, 14: "Christian", 15: Unitarian, 16: Christian Science, 17: 7th day Adventist, 18: Pentecostal, 19: Amish, 20: Quaker, 99: NA/DK
-- in 1987, the label specifically says None, atheist, agnostic
1988-1993 - 0: None, atheist, agnostic, 1: Roman Catholic, 2: Jewish, 3: Baptist, 4: Lutheran, 5: Methodist, 6: Presbyterian, 7: Episcopalian, 8: Protestant unspecified, 9: Other Protestant, 10: Other non-Christian, 11: LDS, 12: Jehvah's Witnesses
13: Greek Orthodox, 14: "Christian", 15: Unitarian, 16: Christian Science, 17: 7th day Adventist, 18: Pentecostal, 19: Amish, 20: Quaker, 21: Church of God, 22: United Church of Christ, 23: Reformed, 24: Disciples of Christ, 25: CHurches of Christ, 97: Other, 99: NA/DK
-- so, up to 20 is the same as above, just added 21-25.
1994-2017 - 0: None, 1: Catholic, 2: Jewish, 8: Protestant unspecified, 10: Other non-Christian, 13: Greek Orthodox, 97: Other, 98: DK, 99: NA // so these large categories do match above in terms of coding (like 8 is the same, 13, etc. just way less groups)
-- In 1994, DENOMINATION was added as a separate question, so all of the detail goes to a separate question (which I don't believe I pulled in at the moment). so, I guess decide if that is worth adding.
2019-2021 - 0: Inapp (no partner), 1: None, 2: Atheist, 3: Agnostic, 4: Roman Catholic, 5: Greek Orthodox, 6: Baptist, 7: Episcopalian, 8: Jehovah's Witness, 9: Lutheran, 10: Methodist, 11: Pentecostal, 12: Presbyterian, 13: Protestant unspecified, 14: Christian, unspecified, 15: Christian, non-denominational, 16: Jewish, 17: Muslim, 18: Buddhist, 19: Other non-christian, 20: Other protestant, 21: LDS, 22: Unitarian, 23: Christian Science, 24: Adventist, 25: Amish, 26: Quaker, 27: Church of God, 28: United Church of Christ, 29: Reformed, 30: Disciples of Christ, 31: Churches of Christ, 97: Other, 98: DK, 99: NA
-- lol so DENOMINATION ends in 2017 and is integrated BACK to this question lord and the codes change AGAIN.

Denomination
1994-2017 - 0: None, atheist, agnostic, not Protestant OR no spouse (this is a lot in one), 3: Baptist, 4: Lutheran, 5: Methodist, 6: Presbyterian, 7: Episcopalian, 8: Protestant unspecified, 9: Other Protestant, 11: LDS, 12: Jehovah's witness, 14: Christian, 15: Unitarian, 16: Christian Science, 17: Adventist, 18: Pentecostal, 19: Amish, 20: Quaker, 21: Church of God, 22: United Church of Christ, 23: Reformed, 24: Disciples of Christ, 25: CHurches of Christ, 97: Other, 98: DK, 99: NA
-- so, I think aligns with how asked 1985-1993. I think if I combine the two I actually get all the same codes 0-25 (that's why some are missing)

This might be helpful: https://www.pewresearch.org/religion/2015/05/12/appendix-b-classification-of-protestant-denominations/
https://en.wikipedia.org/wiki/Protestantism_in_the_United_States#Mainline_Protestantism
https://www.thegospelcoalition.org/blogs/trevin-wax/quick-guide-christian-denominations/ - the big three are Eastern Orthodox; Catholic; Protestant
https://truthandgracecounseling.com/understanding-the-difference-between-evangelical-and-mainline-protestant-churches/
https://woollyscreamsmiracle.wordpress.com/evangelical-vs-mainline-protestant-denominations-an-overview/
-- ideally could have evangelical Protestantism, mainline Protestantism and historically black Protestantism
-- okay no because these denominations spain mainline and evangelical in their classification
*/
tab DENOMINATION_HEAD_ RELIGION_HEAD_ if inrange(survey_yr,1994,2017), m col // want to clarify how these map on so I can decide what catgories to use. so all of these are protestant denominations??

browse unique_id survey_yr RELIGION_HEAD_ DENOMINATION_HEAD_ RELIGION_WIFE_ DENOMINATION_WIFE_

gen religion_head=.
replace religion_head=0 if inrange(survey_yr,1970,1984) & RELIGION_HEAD_==0 // no religion
replace religion_head=0 if inrange(survey_yr,1985,1993) & RELIGION_HEAD_==0
replace religion_head=0 if inrange(survey_yr,1994,2017) & RELIGION_HEAD_==0
replace religion_head=0 if inrange(survey_yr,2019,2021) & inlist(RELIGION_HEAD_,1,2,3)
replace religion_head=1 if inrange(survey_yr,1970,1984) & RELIGION_HEAD_==8 // catholic
replace religion_head=1 if inrange(survey_yr,1985,1993) & RELIGION_HEAD_==1
replace religion_head=1 if inrange(survey_yr,1994,2017) & RELIGION_HEAD_==1
replace religion_head=1 if inrange(survey_yr,2019,2021) & RELIGION_HEAD_==4
replace religion_head=2 if inrange(survey_yr,1970,1984) & RELIGION_HEAD_==9 // jewish
replace religion_head=2 if inrange(survey_yr,1985,1993) & RELIGION_HEAD_==2
replace religion_head=2 if inrange(survey_yr,1994,2017) & RELIGION_HEAD_==2
replace religion_head=2 if inrange(survey_yr,2019,2021) & RELIGION_HEAD_==16
replace religion_head=3 if inrange(survey_yr,1970,1984) & RELIGION_HEAD_==1 // baptist
replace religion_head=3 if inrange(survey_yr,1985,1993) & RELIGION_HEAD_==3
replace religion_head=3 if inrange(survey_yr,1994,2017) & RELIGION_HEAD_==8 & DENOMINATION_HEAD_==3
replace religion_head=3 if inrange(survey_yr,2019,2021) & RELIGION_HEAD_==6
replace religion_head=4 if inrange(survey_yr,1970,1984) & inlist(RELIGION_HEAD_,2,3,4,5) // mainline protestant
replace religion_head=4 if inrange(survey_yr,1985,1993) & inlist(RELIGION_HEAD_,4,5,6,7,22,23,24)
replace religion_head=4 if inrange(survey_yr,1994,2017) & RELIGION_HEAD_==8 & inlist(DENOMINATION_HEAD_,4,5,6,7,22,23,24)
replace religion_head=4 if inrange(survey_yr,2019,2021) & inlist(RELIGION_HEAD_,7,9,10,12,28,29,30)
// replace religion_head=5 if inrange(survey_yr,1970,1984) & inlist(RELIGION_HEAD_,) // evangelical protestant - none in first waves
replace religion_head=5 if inrange(survey_yr,1985,1993) & inlist(RELIGION_HEAD_,17,18,21,25)
replace religion_head=5 if inrange(survey_yr,1994,2017) & RELIGION_HEAD_==8 & inlist(DENOMINATION_HEAD_,17,18,21,25)
replace religion_head=5 if inrange(survey_yr,2019,2021) & inlist(RELIGION_HEAD_,11,24,27,31)
replace religion_head=6 if inrange(survey_yr,1970,1984) & RELIGION_HEAD_==7 // other protestant
replace religion_head=6 if inrange(survey_yr,1985,1993) & inlist(RELIGION_HEAD_,8,9,19,20)
replace religion_head=6 if inrange(survey_yr,1994,2017) & RELIGION_HEAD_==8 & inlist(DENOMINATION_HEAD_,8,9,19,20,97,98,99)
replace religion_head=6 if inrange(survey_yr,2019,2021) & inlist(RELIGION_HEAD_,13,20,25,26)
// replace religion_head=7 if inrange(survey_yr,1970,1984) & inlist(RELIGION_HEAD_,) // eastern orthodox
replace religion_head=7 if inrange(survey_yr,1985,1993) & RELIGION_HEAD_==13
replace religion_head=7 if inrange(survey_yr,1994,2017) & RELIGION_HEAD_==13
replace religion_head=7 if inrange(survey_yr,2019,2021) & RELIGION_HEAD_==5
replace religion_head=8 if inrange(survey_yr,1970,1984) & RELIGION_HEAD_==6 // other christian
replace religion_head=8 if inrange(survey_yr,1985,1993) & inlist(RELIGION_HEAD_,11,12,14,15,16)
replace religion_head=8 if inrange(survey_yr,1994,2017) & inlist(DENOMINATION_HEAD_,11,12,14,15,16)
replace religion_head=8 if inrange(survey_yr,2019,2021) & inlist(RELIGION_HEAD_,8,14,15,21,22,23)
// replace religion_head=9 if inrange(survey_yr,1970,1984) & inlist(RELIGION_HEAD_,) // other non-christian
replace religion_head=9 if inrange(survey_yr,1985,1993) & RELIGION_HEAD_==10
replace religion_head=9 if inrange(survey_yr,1994,2017) & RELIGION_HEAD_==10
replace religion_head=9 if inrange(survey_yr,2019,2021) & inlist(RELIGION_HEAD_,17,18,19)
// replace religion_head=10 if inrange(survey_yr,1970,1984) & inlist(RELIGION_HEAD_,) // other other
replace religion_head=10 if inrange(survey_yr,1985,1993) & RELIGION_HEAD_==97
replace religion_head=10 if inrange(survey_yr,1994,2017) & RELIGION_HEAD_==97
replace religion_head=10 if inrange(survey_yr,2019,2021) & RELIGION_HEAD_==97
// replace religion_head=. if inrange(survey_yr,1970,1984) & inlist(RELIGION_HEAD_,) // missing
replace religion_head=. if inrange(survey_yr,1985,1993) & RELIGION_HEAD_==99
replace religion_head=. if inrange(survey_yr,1994,2017) & inlist(RELIGION_HEAD_,98,99)
replace religion_head=. if inrange(survey_yr,2019,2021) & inlist(RELIGION_HEAD_,98,99)

gen religion_wife=.
replace religion_wife=0 if inrange(survey_yr,1970,1984) & RELIGION_WIFE_==0 // no religion
replace religion_wife=0 if inrange(survey_yr,1985,1993) & RELIGION_WIFE_==0
replace religion_wife=0 if inrange(survey_yr,1994,2017) & RELIGION_WIFE_==0
replace religion_wife=0 if inrange(survey_yr,2019,2021) & inlist(RELIGION_WIFE_,1,2,3)
replace religion_wife=1 if inrange(survey_yr,1970,1984) & RELIGION_WIFE_==8 // catholic
replace religion_wife=1 if inrange(survey_yr,1985,1993) & RELIGION_WIFE_==1
replace religion_wife=1 if inrange(survey_yr,1994,2017) & RELIGION_WIFE_==1
replace religion_wife=1 if inrange(survey_yr,2019,2021) & RELIGION_WIFE_==4
replace religion_wife=2 if inrange(survey_yr,1970,1984) & RELIGION_WIFE_==9 // jewish
replace religion_wife=2 if inrange(survey_yr,1985,1993) & RELIGION_WIFE_==2
replace religion_wife=2 if inrange(survey_yr,1994,2017) & RELIGION_WIFE_==2
replace religion_wife=2 if inrange(survey_yr,2019,2021) & RELIGION_WIFE_==16
replace religion_wife=3 if inrange(survey_yr,1970,1984) & RELIGION_WIFE_==1 // baptist
replace religion_wife=3 if inrange(survey_yr,1985,1993) & RELIGION_WIFE_==3
replace religion_wife=3 if inrange(survey_yr,1994,2017) & RELIGION_WIFE_==8 & DENOMINATION_WIFE_==3
replace religion_wife=3 if inrange(survey_yr,2019,2021) & RELIGION_WIFE_==6
replace religion_wife=4 if inrange(survey_yr,1970,1984) & inlist(RELIGION_WIFE_,2,3,4,5) // mainline protestant
replace religion_wife=4 if inrange(survey_yr,1985,1993) & inlist(RELIGION_WIFE_,4,5,6,7,22,23,24)
replace religion_wife=4 if inrange(survey_yr,1994,2017) & RELIGION_WIFE_==8 & inlist(DENOMINATION_WIFE_,4,5,6,7,22,23,24)
replace religion_wife=4 if inrange(survey_yr,2019,2021) & inlist(RELIGION_WIFE_,7,9,10,12,28,29,30)
// replace religion_wife=5 if inrange(survey_yr,1970,1984) & inlist(RELIGION_WIFE_,) // evangelical protestant - none in first waves
replace religion_wife=5 if inrange(survey_yr,1985,1993) & inlist(RELIGION_WIFE_,17,18,21,25)
replace religion_wife=5 if inrange(survey_yr,1994,2017) & RELIGION_WIFE_==8 & inlist(DENOMINATION_WIFE_,17,18,21,25)
replace religion_wife=5 if inrange(survey_yr,2019,2021) & inlist(RELIGION_WIFE_,11,24,27,31)
replace religion_wife=6 if inrange(survey_yr,1970,1984) & RELIGION_WIFE_==7 // other protestant
replace religion_wife=6 if inrange(survey_yr,1985,1993) & inlist(RELIGION_WIFE_,8,9,19,20)
replace religion_wife=6 if inrange(survey_yr,1994,2017) & RELIGION_WIFE_==8 & inlist(DENOMINATION_WIFE_,8,9,19,20,97,98,99)
replace religion_wife=6 if inrange(survey_yr,2019,2021) & inlist(RELIGION_WIFE_,13,20,25,26)
// replace religion_wife=7 if inrange(survey_yr,1970,1984) & inlist(RELIGION_WIFE_,) // eastern orthodox
replace religion_wife=7 if inrange(survey_yr,1985,1993) & RELIGION_WIFE_==13
replace religion_wife=7 if inrange(survey_yr,1994,2017) & RELIGION_WIFE_==13
replace religion_wife=7 if inrange(survey_yr,2019,2021) & RELIGION_WIFE_==5
replace religion_wife=8 if inrange(survey_yr,1970,1984) & RELIGION_WIFE_==6 // other christian
replace religion_wife=8 if inrange(survey_yr,1985,1993) & inlist(RELIGION_WIFE_,11,12,14,15,16)
replace religion_wife=8 if inrange(survey_yr,1994,2017) & inlist(DENOMINATION_WIFE_,11,12,14,15,16)
replace religion_wife=8 if inrange(survey_yr,2019,2021) & inlist(RELIGION_WIFE_,8,14,15,21,22,23)
// replace religion_wife=9 if inrange(survey_yr,1970,1984) & inlist(RELIGION_WIFE_,) // other non-christian
replace religion_wife=9 if inrange(survey_yr,1985,1993) & RELIGION_WIFE_==10
replace religion_wife=9 if inrange(survey_yr,1994,2017) & RELIGION_WIFE_==10
replace religion_wife=9 if inrange(survey_yr,2019,2021) & inlist(RELIGION_WIFE_,17,18,19)
// replace religion_wife=10 if inrange(survey_yr,1970,1984) & inlist(RELIGION_WIFE_,) // other other
replace religion_wife=10 if inrange(survey_yr,1985,1993) & RELIGION_WIFE_==97
replace religion_wife=10 if inrange(survey_yr,1994,2017) & RELIGION_WIFE_==97
replace religion_wife=10 if inrange(survey_yr,2019,2021) & RELIGION_WIFE_==97
// replace religion_wife=. if inrange(survey_yr,1970,1984) & inlist(RELIGION_WIFE_,) // missing
replace religion_wife=. if inrange(survey_yr,1985,1993) & RELIGION_WIFE_==99
replace religion_wife=. if inrange(survey_yr,1994,2017) & inlist(RELIGION_WIFE_,98,99)
replace religion_wife=. if inrange(survey_yr,2019,2021) & inlist(RELIGION_WIFE_,98,99)

label define religion 0 "No religion" 1 "Catholic" 2 "Jewish" 3 "Baptist" 4 "Mainline Protestant" 5 "Evangelical Protestant" 6 "Other Protestant" 7 "Eastern Orthodox" 8 "Other Christian" 9 "Other Non-Christian" 10 "Other Other"
label values religion_head religion_wife religion
tab religion_head, m
tab RELIGION_HEAD_ religion_head, m

tab religion_wife, m
tab RELIGION_WIFE_ religion_wife, m // much more missing - not asked consistently until 1985 (I drop those anyway)

// figuring out costs - creating these variables for posterity (come from other code), but I don't use these
browse id survey_yr HOUSE_STATUS_ RENT_COST_V1_ RENT_COST_V2_ TOTAL_HOUSING_ MORTGAGE_COST_ HOUSE_VALUE_  CHILDCARE_COSTS_

gen total_annual_rent=.
replace total_annual_rent = RENT_COST_V1_ if survey_yr <=1993
replace total_annual_rent = RENT_COST_V2_ * 12 if survey_yr > 1994

browse id survey_yr total_annual_rent RENT_COST_V1_ RENT_COST_V2_ if HOUSE_STATUS_==5

browse id survey_yr  TOTAL_HOUSING_ MORTGAGE_COST_ HOUSE_VALUE_  if HOUSE_STATUS_==1

egen total_annual_rent_std = std(total_annual_rent) if HOUSE_STATUS_==5
egen HOUSE_VALUE_std = std(HOUSE_VALUE_) if HOUSE_STATUS_==1
// browse total_annual_rent total_annual_rent_std HOUSE_VALUE_ HOUSE_VALUE_std

gen housing_costs_use=.
replace housing_costs_use = total_annual_rent_std if HOUSE_STATUS_==5
replace housing_costs_use = HOUSE_VALUE_std if HOUSE_STATUS_==1

replace CHILDCARE_COSTS_=. if inlist(CHILDCARE_COSTS_,99998,99999,999998,999999)

// other joint investments
* wealth - can I interpolate in between the years I don't have?
browse id survey_yr WEALTH_NO_EQUITY_ WEALTH_EQUITY_

bysort unique_id: ipolate WEALTH_NO_EQUITY_ survey_yr if survey_yr>=1984 & survey_yr<=2021, gen(WEALTH_NO_EQUITY_i)
bysort unique_id: ipolate WEALTH_EQUITY_ survey_yr if survey_yr>=1984 & survey_yr<=2021, gen(WEALTH_EQUITY_i)

* same with vehicle ownership and value
browse id survey_yr VEHICLE_OWN_ VEHICLE_VALUE_
bysort unique_id: ipolate VEHICLE_OWN_ survey_yr, gen(VEHICLE_OWN_i) // so this only works if there is another observed value. so, for some, there is just a last value, so it doesn't carry that through. can I add tht in? oh is that where ipolate works?
bysort unique_id: ipolate VEHICLE_OWN_ survey_yr, gen(VEHICLE_OWN_e) epolate
browse id survey_yr VEHICLE_OWN_*

replace VEHICLE_OWN_e=1 if VEHICLE_OWN_e>=1 & VEHICLE_OWN_e<5 // so if changes from 1 to 5, fills in, let's set all to 1
replace VEHICLE_OWN_e=1 if VEHICLE_OWN_e<=1 // negatives also seem to be because blanks turn to 1
replace VEHICLE_OWN_e=5 if VEHICLE_OWN_e>=5 & VEHICLE_OWN_e<100  // make these nos - if no, or dk, or anything in between

	// bysort id: egen vehicle_own_last = mode(VEHICLE_OWN_)
	// browse id survey_yr VEHICLE_OWN_ vehicle_own_last VEHICLE_OWN_i
	// replace VEHICLE_OWN_i = vehicle_own_last if VEHICLE_OWN_i==.

bysort unique_id: ipolate VEHICLE_VALUE_ survey_yr if survey_yr>=1984 & survey_yr<=2021, gen(VEHICLE_VALUE_i)

* other assets - okay let's just use wealth
tabstat DIVIDENDS_JOINT_ DIVIDENDS_HEAD_ DIVIDENDS_WIFE_, by(survey_yr)
browse id survey_yr DIVIDENDS_JOINT_ DIVIDENDS_HEAD_ DIVIDENDS_WIFE_ // okay joint not that useful - not many say yes and only since 2003. very few WIFEs also have dividends... okay let's not use these

tabstat INTEREST_JOINT_ BANK_ASSETS_ OTHER_ASSETS_ STOCKS_MF_, by(survey_yr)

// indicators of hardship
tabstat WELFARE_HEAD_1_  WELFARE_HEAD_2_ WELFARE_WIFE_1_ WELFARE_WIFE_2_ WELFARE_JOINT_, by(survey_yr) // 1 and 2 have overlapping years, see if they match, then decide which to use - these feel very not matched?!
browse id survey_yr WELFARE_HEAD_1_  WELFARE_HEAD_2_ WELFARE_WIFE_1_ WELFARE_WIFE_2_ WELFARE_JOINT_ // 1 and 2 have overlapping years, see if they match, then decide which to use - okay incidence is SO LOW. this also doesn't feel right??

gen receive_welfare=.
replace receive_welfare=0 if WELFARE_JOINT_==0 & survey_yr <=1985 // only indicator at this time
replace receive_welfare=0 if WELFARE_HEAD_1_==0 & WELFARE_WIFE_1_==0 & survey_yr >1985 & survey_yr < 1993
replace receive_welfare=0 if WELFARE_HEAD_2_==0 & WELFARE_WIFE_2_==0 & survey_yr >1985 & survey_yr >=1993
replace receive_welfare=1 if WELFARE_JOINT_>0 & WELFARE_JOINT_!=. & survey_yr <=1985 // only indicator at this time
replace receive_welfare=1 if ((WELFARE_HEAD_1_>0 & WELFARE_HEAD_1_!=.) | (WELFARE_WIFE_1_>0 & WELFARE_WIFE_1_!=.)) & survey_yr >1985 & survey_yr < 1993
replace receive_welfare=1 if ((WELFARE_HEAD_2_>0 & WELFARE_HEAD_2_!=.) | (WELFARE_WIFE_2_>0 & WELFARE_WIFE_2_!=.))  & survey_yr >1985 & survey_yr >=1993

gen receive_transfers=.
replace receive_transfers=0 if TRANSFER_INCOME_==0
replace receive_transfers=1 if TRANSFER_INCOME_>0 &TRANSFER_INCOME_!=.

// also age at relationship start
replace year_birth = survey_yr - AGE_INDV if year_birth==. & AGE_INDV!=999
browse unique_id partner_unique_id survey_yr relation SEX year_birth  AGE_INDV AGE_HEAD_ AGE_WIFE_

inspect AGE_HEAD_
inspect AGE_WIFE_ // sometimes 0 - since this is time invariant, think I can fill in.

gen yr_born_head_0 = survey_yr - AGE_HEAD_ if AGE_HEAD_!=0 & AGE_HEAD!=9999
gen yr_born_wife_0 = survey_yr - AGE_WIFE_ if AGE_WIFE_!=0 & AGE_WIFE!=9999

bysort unique_id partner_unique_id: egen yr_born_head = min(yr_born_head_0)
bysort unique_id partner_unique_id: egen yr_born_wife = min(yr_born_wife_0)

browse unique_id partner_unique_id survey_yr relation SEX year_birth  AGE_INDV AGE_HEAD_ AGE_WIFE_ yr_born_head yr_born_head_0 yr_born_wife yr_born_wife_0

gen age_mar_head = rel_start_yr_couple -  yr_born_head
replace age_mar_head = . if inrange(age_mar_head,900,1000)
gen age_mar_wife = rel_start_yr_couple -  yr_born_wife
replace age_mar_wife = . if inrange(age_mar_wife,900,1000)

drop if age_mar_head < 0 | age_mar_wife < 0

// merge cohabitation history for head
merge m:1 unique_id using "$temp/PSID_partner_history.dta", keepusing(MX8* partner_unique_id*) // I AM DUMB - it shouldn;t all match because it is only people who ever had A COHABITING partner, not others. // partner_1968_id* partner_per_num* - not sure why I wasn't using unique ID befre...

drop if _merge==2 // people not in my sample
gen ever_cohab_head = 1 if _merge==3
replace ever_cohab_head=0 if ever_cohab_head==.
drop _merge
tab current_rel_type ever_cohab_head, m

foreach var in MX8* partner_unique_id*{
	rename `var' `var'_head
}

rename partner_unique_id_head partner_unique_id

// k now trying to match on PARTNER id to get HER history
merge m:1 partner_unique_id using "$temp\PSID_partner_history.dta", keepusing(MX8* partner_unique_id*) // less matches but I am not surprised about this (i don't think??
// merge m:1 spouse_per_num_all spouse_id_all // why am I confused? which should I match on? Okay, partner ID comes from family matrix, spouse comes from marital history. problem with marital history is that it's not comprehensive bc doesn't cover cohab. nor does it cover certain relationships prior to 1985, so use partner unique id
// inspect spouse_per_num_all if marital_status_updated==1
// inspect spouse_per_num_all if marital_status_updated==2

drop if _merge==2 // people not in my sample
gen ever_cohab_wife = 1 if _merge==3
replace ever_cohab_wife=0 if ever_cohab_wife==.
drop _merge
tab current_rel_type ever_cohab_wife, m

tab ever_cohab_wife ever_cohab_head, m

foreach var in MX8* partner_unique_id*{
	rename `var' `var'_wife
}

rename partner_unique_id*_head_wife partner_unique_id*_head
rename partner_unique_id_wife partner_unique_id

// tmp save

// now create the cohab history variables (this used to come later but moved up)
sort unique_id survey_yr
browse unique_id partner_unique_id survey_yr marital_status_updated current_rel_number ever_cohab_head rel_start_yr_couple rel_end_yr_couple partner_unique_id*_head partner_unique_id*_wife // okay not all of these make sense anymore if I keep cohabitors, but the pre / other ones do. should I only make the "with wife" ones for married couples? well, these are all pre rel, so think might only end up applying to married? can check at end
drop if rel_start_yr_couple==. // small amount - this will mess up below code and also I can't use these couples anyway, so drop here
// example of someone who should have a cohab other: 5559001

// okay, now that I am adding cohab, this is a little trickier - because actually, if I observed their transition from cohab to marriage, they are not captured here, because the relationship start date is for cohab, not marriage.

forvalues y=1968/1997{
	capture gen cohab_`y'_with_partner_head=0
	replace cohab_`y'_with_partner_head=1 if `y' < rel_start_yr_couple & partner_unique_id`y'_head==partner_unique_id
	replace cohab_`y'_with_partner_head=1 if `y' < transition_year & partner_unique_id`y'_head==partner_unique_id & ever_transition==1
	
	capture gen cohab_`y'_with_partner_wife=0
	replace cohab_`y'_with_partner_wife=1 if `y' < rel_start_yr_couple & partner_unique_id`y'_wife==unique_id
	replace cohab_`y'_with_partner_wife=1 if `y' < transition_year & partner_unique_id`y'_wife==unique_id & ever_transition==1
}

forvalues y=1999(2)2021{
	capture gen cohab_`y'_with_partner_head=0
	replace cohab_`y'_with_partner_head=1 if `y' < rel_start_yr_couple & partner_unique_id`y'_head==partner_unique_id
	replace cohab_`y'_with_partner_head=1 if `y' < transition_year & partner_unique_id`y'_head==partner_unique_id & ever_transition==1
		
	capture gen cohab_`y'_with_partner_wife=0
	replace cohab_`y'_with_partner_wife=1 if `y' < rel_start_yr_couple & partner_unique_id`y'_wife==unique_id
	replace cohab_`y'_with_partner_wife=1 if `y' < transition_year & partner_unique_id`y'_wife==unique_id & ever_transition==1
}

forvalues y=1968/1997{
	gen cohab_`y'_other_head=0
	replace cohab_`y'_other_head=1 if `y' < rel_start_yr_couple & partner_unique_id`y'_head!=partner_unique_id & partner_unique_id!=. & partner_unique_id`y'_head!=.
	
	gen cohab_`y'_other_wife=0
	replace cohab_`y'_other_wife=1 if `y' < rel_start_yr_couple & partner_unique_id`y'_wife!=unique_id & unique_id!=. & partner_unique_id`y'_wife!=.
}

forvalues y=1999(2)2021{
	gen cohab_`y'_other_head=0
	replace cohab_`y'_other_head=1 if `y' < rel_start_yr_couple & partner_unique_id`y'_head!=partner_unique_id & partner_unique_id!=. & partner_unique_id`y'_head!=.
	
	gen cohab_`y'_other_wife=0
	replace cohab_`y'_other_wife=1 if `y' < rel_start_yr_couple & partner_unique_id`y'_wife!=unique_id & unique_id!=. & partner_unique_id`y'_wife!=.
}


forvalues y=1968/1997{
	gen cohab_`y'_after_head=0
	replace cohab_`y'_after_head=1 if `y' > rel_end_yr_couple & partner_unique_id`y'_head!=.
	
	gen cohab_`y'_after_wife=0
	replace cohab_`y'_after_wife=1 if `y' > rel_end_yr_couple & partner_unique_id`y'_wife!=.
}

forvalues y=1999(2)2021{
	gen cohab_`y'_after_head=0
	replace cohab_`y'_after_head=1 if `y' > rel_end_yr_couple & partner_unique_id`y'_head!=.
	
	gen cohab_`y'_after_wife=0
	replace cohab_`y'_after_wife=1 if `y' > rel_end_yr_couple & partner_unique_id`y'_wife!=.
}

browse unique_id partner_unique_id survey_yr current_rel_type ever_cohab_head partner_unique_id rel_start_yr_couple rel_end_yr_couple cohab_*_with_partner_head cohab_*_other_head partner_unique_id*_head

egen cohab_with_partner_head = rowtotal(cohab_*_with_partner_head)
replace cohab_with_partner_head = 1 if cohab_with_partner_head > 1 & cohab_with_partner_head!=.

egen cohab_with_partner_wife = rowtotal(cohab_*_with_partner_wife)
replace cohab_with_partner_wife = 1 if cohab_with_partner_wife > 1 & cohab_with_partner_wife!=.

tab cohab_with_partner_head cohab_with_partner_wife, m
tab ever_transition cohab_with_partner_head, m // so this variable is not that useful at the moment given we have added cohab, so let's come back to this
tab marital_status_updated cohab_with_partner_head, m row

egen cohab_with_other_head = rowtotal(cohab_*_other_head)
replace cohab_with_other_head = 1 if cohab_with_other_head > 1 & cohab_with_other_head!=.

egen cohab_with_other_wife = rowtotal(cohab_*_other_wife)
replace cohab_with_other_wife = 1 if cohab_with_other_wife > 1 & cohab_with_other_wife!=.

tab cohab_with_other_head cohab_with_other_wife, m 

egen cohab_after_head = rowtotal(cohab_*_after_head)
replace cohab_after_head = 1 if cohab_after_head > 1 & cohab_after_head!=.

egen cohab_after_wife = rowtotal(cohab_*_after_wife)
replace cohab_after_wife = 1 if cohab_after_wife > 1 & cohab_after_wife!=.

tab ever_cohab_head cohab_with_partner_head
tab ever_cohab_head cohab_with_other_head
tab ever_cohab_head cohab_after_head

browse unique_id partner_unique_id survey_yr rel_start_yr_couple current_rel_type ever_cohab_head cohab_with_other_head cohab_with_partner_head cohab_after_head
tab rel_start_yr_couple if ever_cohab_head==1 & cohab_with_other_head==0 & cohab_with_partner_head==0 & cohab_after_head==0 & current_rel_type==20 // okay few actully not accounted for, and most are relationships started before PSID

// current challenge to address? 
tab MARITAL_PAIRS dissolve, m // so - i think there will be problems with both partners info if not recorded as being in a marital pair
tab MARITAL_PAIRS current_rel_type, m // okay so i did fix this for marriage (a key goal of last time) - it's mostly cohab
sort unique_id survey_yr 
browse unique_id partner_unique_id survey_yr current_rel_type rel_start_yr_couple rel_end_yr_couple how_end_couple dissolve MARITAL_PAIRS_ weekly_hrs_t1_wife weekly_hrs_t1_head // a lot of these feel like 1 year rels - let's validate with below
tab weekly_hrs_t1_wife if MARITAL_PAIRS==0, m // yeah these are like 99% zero. So, do I drop them? move up the dissolve?
tab in_marital_history if dissolve==1 & MARITAL_PAIRS_==0, m // is in marital history over or under reprsented - like is that more or less contribuitng to this problem? okay yes, so ALL in marital history. does it depend like month of survey v. month of dissolve? because marital history updated retrospetively so like maybe when answered 2005 survey, they were together and living together in month 8, but then then divorce month 10 - marital history will update, but there will still be interview data for wife because she was there at time of survey. so it's probably people who divorced later v. earler in year - so I want last full year of data I can get, whenever that is? technically I do have months in marital history... but it really doesn't matter like I don't get that info either way...
 // do I have to lag alll info though? or like JUST update year of dissolution? replace year of dissolution with all prior values of wife variables?
 
// relationship_duration
gen dur = survey_yr - rel_start_yr_couple
browse unique_id partner_unique_id survey_yr rel_start_yr_couple dur
tab dur, m

bysort unique_id partner_unique_id: egen min_dur = min(dur)
bysort unique_id partner_unique_id: egen max_dur = max(dur)

tab dur current_rel_type if MARITAL_PAIRS==0 // mostly first year (which makes sense if not yet married at time of interview OR first yr cohabitors (except - they should not be in my file yet)? 
tab max_dur if MARITAL_PAIRS==0 // then yes, many are 1 year rels
tab dur max_dur if MARITAL_PAIRS==0, m

// okay yes: 
// tab MARITAL_PAIRS if min_dur == dur & current_rel_type==22, m // 4327
// tab MARITAL_PAIRS  // out of 4602 total that are 0

sort unique_id survey_yr
browse unique_id partner_unique_id survey_yr dur min_dur max_dur current_rel_type rel_start_yr_couple rel_end_yr_couple how_end_couple dissolve MARITAL_PAIRS_ weekly_hrs_t1_wife weekly_hrs_t1_head

save "$created_data/PSID_union_sample_rec.dta", replace

/* some QA-ing
use "$created_data/PSID_union_validation_sample.dta", clear
browse unique_id partner_unique_id survey_yr rel_start_yr_couple rel_end_yr_couple marital_status_updated yr_married1 yr_married2 yr_married3 if inlist(unique_id, 13004, 99031, 355003, 796173, 1822170, 2409176, 5610030, 6494003)
*/

********************************************************************************
**# General sample restrictions - okay most of these actually happen later
* Let's revisit this - do I want to do age / cohort here or next file?
* See notes below on what to make sure I revisit (note: 8/8/25)
********************************************************************************
// first need to figure out how to keep only one respondent per HH. really doesn't matter gender of who I keep, because all variables are denoted by head / wife, NOT respondent.
// this is also where the marital pairs going to cause problems - bc then there aren't couples to match. AND we lack info. so - to avoid problems down the line - just drop now

// let's drop them actually
drop if MARITAL_PAIRS== 0 // this removes first year of some cohab relationships (bc I fixed this in previous iterations of code, that is all who is captured here still) - so in effect, also removes any relationships only observed for 1 year that broke up (which - I already drop later anyway)

unique unique_id partner_unique_id, by(current_rel_type)
unique couple_id, by(current_rel_type) // that also makes these more congruent as just half of each other (20271)

// now deduplicate
bysort survey_yr FAMILY_INTERVIEW_NUM_ : egen per_id = rank(unique_id)
tab per_id, m

sort unique_id survey_yr
browse unique_id partner_unique_id survey_yr FAMILY_INTERVIEW_NUM_ couple_id partner_1 partner_2 per_id

tab per_id if unique_id == partner_1 // okay so these would lead to same conclusion
tab per_id if unique_id == partner_2

keep if per_id==1
unique couple_id, by(current_rel_type) // now still have 20271

// also delete records after the end of the relationship 
sort unique_id survey_yr
browse unique_id partner_unique_id survey_yr rel_start_yr_couple rel_end_yr_couple marital_status_updated dissolve how_end_couple

gen rel_over_flag=0
replace rel_over_flag = 1 if survey_yr > rel_end_yr_couple & rel_end_yr_couple!=.
tab rel_over_flag, m // very small amount
tab current_rel_type rel_over_flag, m row
tab how_end_couple rel_over_flag, m row // let's leave if intact

// browse unique_id partner_unique_id survey_yr rel_start_yr_couple rel_end_yr_couple marital_status_updated dissolve how_end_couple if rel_over_flag==1
// browse unique_id partner_unique_id survey_yr rel_start_yr_couple rel_end_yr_couple marital_status_updated dissolve how_end_couple rel_over_flag if inlist(unique_id,41170,41178,656031,656176,6115050,6115206) // people with discontinuous years. these are confusing, so let's just drop because we don't know if considered breakup or not. so treat as however coded (bc again, many based on KM compiled history...). but let's see if would get covered below...

// alt way
gen end_year = survey_yr if dissolve==1
bysort unique_id partner_unique_id (end_year): replace end_year=end_year[1]
sort unique_id survey_yr

gen rel_over_flag_alt=0
replace rel_over_flag_alt = 1 if survey_yr > end_year & end_year!=.
tab rel_over_flag_alt, m 
tab rel_over_flag rel_over_flag_alt, m // actually very little overlap

browse unique_id partner_unique_id survey_yr current_rel_type rel_start_yr_couple rel_end_yr_couple dissolve end_year how_end_couple rel_over_flag rel_over_flag_alt

drop if rel_over_flag==1 & how_end_couple==1
drop if rel_over_flag_alt==1 // this one I *definitely* want to do - bc this is the problem of which year divorce observed v. recorded (if between waves)
	
// restrict to working age (18-55) - at time of marriage or all? check what others do - Killewald said ages 18-55 - others have different restrictions, table this part for now, also need to figure out the ages a bit more
/*
browse id survey_yr AGE_ AGE_REF_ AGE_SPOUSE_ RELATION_
keep if (AGE_REF_>=18 & AGE_REF_<=55) &  (AGE_SPOUSE_>=18 & AGE_SPOUSE_<=55)

// sample things that need to happen later
1. age
2. first relationship (figure out how to handle this if both marriage and cohab observed) - and how to handle transitions to marriage
3. rel start year
4. left-censoring (unknown start date) - aka flag==1
*/

// final sample checks / outcome variable checks
bysort unique_id partner_unique_id: egen ever_dissolve=max(dissolve)
sort unique_id survey_yr
tab ever_dissolve dissolve, m
tab how_end_couple ever_dissolve, m row // okay pretty close - I think some I have as breakup but I don't have close enough to year of divorce for me to feel comfortable calling it a dissolve in that year

tab dissolve
tab exit_rel // okay but this includes widows so makes sense it's higher
tab dissolve exit_rel, m // key area to investigate is where dissolve is 1 but exit is not. I think sometimes we just didn't observe a transition out, so had to use end date
tab how_end_couple exit_rel, m
tab how_end_couple dissolve, m

sort unique_id survey_yr
bysort unique_id partner_unique_id: egen num_years = count(survey_yr) // this is diff to max dur because based on observed years not true
tab num_years, m

gen total_dur = rel_end_yr_couple - rel_start_yr_couple + 1  if rel_end_yr_couple <= 2021 // also a little different to max dur because this is again based on observed
replace total_dur = end_year - rel_start_yr_couple + 1 if total_dur==. & end_year!=.
tab total_dur current_rel_type, m col
tab max_dur current_rel_type, m col
tab total_dur, m
tab total_dur if max_dur==0, m
tab max_dur if total_dur==0, m // think rationately here is that I can't observe a transition to divorce if partnered for one year - BUT should they still be in sample? (this is also a V small amount of people). like I think this is one couple lol

sort unique_id survey_yr
browse unique_id partner_unique_id survey_yr rel_start_yr_couple rel_end_yr_couple dur num_years total_dur min_dur max_dur

drop if total_dur==0
drop if dur < 0
// drop if num_years==1 // want to keep
// drop if MARITAL_PAIRS_==0 // moved up
drop if SEX_HEAD_==2 | SEX_HEAD_==0 // best way to proxy same-gender (sex_wife not consistently asked)
drop if SEX_WIFE_==1

save "$created_data/PSID_union_sample_dedup.dta", replace
// save "$created_data/PSID_marriage_recoded_sample.dta", replace // old file name, for reference
 
/* Checks comparing to old data as a sense check (this is left over from marriage sample)
* missing respondents - bc of not first marriage
// browse unique_id partner_unique_id survey_yr rel_start_yr_couple rel_end_yr_couple marital_status_updated yr_married1 yr_married2 yr_married3 matrix_marr_num if inlist(unique_id, 13004, 99031, 355003, 796173, 1822170, 2409176, 5610030, 6494003)

* incorrectly classified as dissolved - bc of temp separations
// browse unique_id partner_unique_id survey_yr marital_status_updated dissolve dissolve_v0 rel_start_yr_couple rel_end_yr_couple last_survey_yr yr_married1 yr_end1 yr_married2 yr_end1 yr_married3 yr_end3 matrix_marr_num if inlist(unique_id, 356030, 409032, 677032, 423032, 916032, 951030, 2707033, 6165006, 5576031, 5623036, 5628032)

* new respondents (not in original file)
// browse unique_id partner_unique_id survey_yr marital_status_updated dissolve dissolve_v0 rel_start_yr_couple rel_end_yr_couple matrix_marr_num relationship_order marr_no_estimated last_survey_yr yr_married1 yr_end1 yr_married2 yr_end1 yr_married3 yr_end3 if inlist(unique_id, 4033, 46030, 47033, 245033, 280033, 497032, 519030, 1241030, 1241033, 2876031, 2901031, 5994006, 5994008, 6822006)
*/