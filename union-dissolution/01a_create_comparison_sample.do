********************************************************************************
* Getting PSID sample for union dissolution
* create_comparison_sample.do
* Kim McErlean
********************************************************************************

* Instead of just keeping those with relationship start 20000 - keeping those later, to try to validate other findings (e.g. Killewald, Schwartz and GP). Since just marriage, can also do prior to 1983?

use "$data_tmp\PSID_full_long.dta", clear // created in other step 1

********************************************************************************
* First clean up to get a sense of WHO is even eligible
********************************************************************************

browse survey_yr id main_per_id SEQ_NUMBER_ RELATION_ FIRST_MARRIAGE_YR_START MARITAL_PAIRS_

// drop if survey_yr <1983 // first time you could identify cohab

gen relationship=0
replace relationship=1 if inrange(MARITAL_PAIRS_,1,4)

browse survey_yr id main_per_id SEQ_NUMBER_ relationship RELATION_ FIRST_MARRIAGE_YR_START 

bysort id (SEQ_NUMBER_): egen in_sample=max(SEQ_NUMBER_)

drop if in_sample==0 // people with NO DATA in any year
drop if SEQ_NUMBER_==0 // won't have data because not in that year -- like SIPP, how do I know if last year is because divorced or last year in sample? right now individual level file, so fine - this is JUST last year in sample at the moment

browse survey_yr id main_per_id SEQ_NUMBER_ relationship RELATION_ FIRST_MARRIAGE_YR_START 

bysort id: egen relationship_start=min(survey_yr) if relationship==1
bysort id: egen relationship_end=max(survey_yr) if relationship==1
bysort id: egen last_survey_yr = max(survey_yr)

browse id survey_yr relationship relationship_start relationship_end last_survey_yr SEQ_NUMBER_ MARITAL_PAIRS_
gen dissolve=.
replace dissolve=0 if relationship==1
replace dissolve=1 if relationship_end < last_survey_yr & relationship_end==survey_yr

browse id survey_yr relationship relationship_start relationship_end last_survey_yr dissolve SEQ_NUMBER_ MARITAL_PAIRS_

********************************************************************************
* Restrict to anyone in a relationship
********************************************************************************
keep if relationship==1
browse id survey_yr relationship relationship_start relationship_end last_survey_yr dissolve SEQ_NUMBER_ MARITAL_PAIRS_

// trying to identify if married or cohabiting. .. need relation_?
egen year_family=concat(survey_yr FAMILY_INTERVIEW_NUM_), punct(_)
bysort survey_yr FAMILY_INTERVIEW_NUM_ (RELATION_): egen either_cohab=max(RELATION_)
// keep if inlist(RELATION_,1,2,10,20) // remove only those classified as head or wife. Before 1983, couldnt' distinguish cohab. some of those marked 10 might be in cohab, need to account for that

sort survey_yr FAMILY_INTERVIEW_NUM_ id

drop if NUM_MARRIED==98

gen relationship_type=0
replace relationship_type=1 if NUM_MARRIED==0
replace relationship_type=2 if NUM_MARRIED>=1
replace relationship_type=1 if either_cohab==22

label define relationship_type 1 "Cohab" 2 "Married"
label values relationship_type relationship_type

keep if inlist(RELATION_,1,2,10,20)

browse id survey_yr FAMILY_INTERVIEW_NUM_ relationship relationship_type RELATION_ relationship_start relationship_end dissolve NUM_MARRIED
tab relationship_type RELATION_

drop if relationship_type==1
tab RELATION_ // okay pretty equal numbers.

// keep if NUM_MARRIED<=1 - I don't know if this variable applies to all years - right not asked until at least 1985
keep if inlist(NUM_MARRIED,1,99)

sort id survey_yr
browse id survey_yr relationship_type relationship_start relationship_end dissolve 

gen dur = survey_yr - relationship_start

save "$data_tmp\PSID_all_marriages.dta", replace