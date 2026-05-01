********************************************************************************
* Project: Work-family policy and divorce
* Deduplicating so one record per couple
* deduplicate.do
* Code owner: Kimberly McErlean
********************************************************************************

use  "$created_data/PSID_union_sample_rec.dta", clear // created step 4

// if not identified in a marital pair, won't have head / wife info
drop if MARITAL_PAIRS== 0 // this removes first year of some cohab relationships (bc I fixed this in previous iterations of code, that is all who is captured here still) - so in effect, also removes any relationships only observed for 1 year that broke up (which - I already drop later anyway)

unique unique_id partner_unique_id, by(current_rel_type)
unique couple_id, by(current_rel_type) // that also makes these more congruent as just half of each other (20271)

********************************************************************************
* now deduplicate
********************************************************************************
// keep only one respondent per HH. really doesn't matter gender of who I keep, because all variables are denoted by head / wife, NOT respondent.

bysort survey_yr FAMILY_INTERVIEW_NUM_ : egen per_id = rank(unique_id)
tab per_id, m

sort unique_id survey_yr
browse unique_id partner_unique_id survey_yr FAMILY_INTERVIEW_NUM_ couple_id partner_1 partner_2 per_id

tab per_id if unique_id == partner_1 // okay so these would lead to same conclusion
tab per_id if unique_id == partner_2

keep if per_id==1
unique couple_id, by(current_rel_type) // now still have 20271

********************************************************************************
* also delete records after the end of the relationship 
********************************************************************************
sort unique_id survey_yr
browse unique_id partner_unique_id survey_yr rel_start_yr_couple rel_end_yr_couple marital_status_updated dissolve how_end_couple

gen rel_over_flag=0
replace rel_over_flag = 1 if survey_yr > rel_end_yr_couple & rel_end_yr_couple!=.
tab rel_over_flag, m // very small amount
tab current_rel_type rel_over_flag, m row
tab how_end_couple rel_over_flag, m row // let's leave if intact

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

********************************************************************************
* final sample checks / outcome variable checks
********************************************************************************
bysort unique_id partner_unique_id: egen ever_dissolve=max(dissolve)
sort unique_id survey_yr
tab ever_dissolve dissolve, m
tab how_end_couple ever_dissolve, m row // okay pretty close - I think some I have as breakup but I don't have close enough to year of divorce for me to feel comfortable calling it a dissolve in that year

tab dissolve
tab exit_rel // okay but this includes widows so makes sense it's higher
tab dissolve exit_rel, m // key area to investigate is where dissolve is 1 but exit is not. sometimes we just didn't observe a transition out, so had to use end date
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
tab max_dur if total_dur==0, m // I can't observe a transition to divorce if partnered for one year - BUT should they still be in sample? (this is also a V small amount of people, like one couple)

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

/*
// sample things that need to happen later
1. age
2. first relationship (figure out how to handle this if both marriage and cohab observed) - and how to handle transitions to marriage
3. rel start year
4. left-censoring (unknown start date) - aka flag==1
*/
 
 /* moving this info from other file on how to handle transitioners
Gonalons-Pons and Gangl (SIPP):
Cohabiting couples who marry during the survey are included and contribute observations to both the cohabiting and married samples. For instance, if a couple is first observed as cohabiting and they get married and separate, this separation will be recorded as a marital separation. However, because we only follow couples for four years, this is a rare sequence of events.
Oh, but they don't include union duration at all because not measured in EU-SILC (isn't that true in SIPP as well?)
They do an entire simulation exercise to understand the implications of left censoring and left truncation (but this is slightly different to my concerns at the moment)

Brines and Joyner (PSID):
Worth noting, their footnote 2 is good for discussing the types of cohabitors measured in the PSID
"Of these, 96 couples contributed to both types of unions; we control for these couples in our analysis of marital disruption" - so they also let couples just contribute based on whatever type of relationship they are in at the time?
Okay so yes, become censored if they marry "Unions are censored if they remain intact through the end of the observation period or if, in the case of cohabitation, they are legalized"
THey estimate all models separately based on union type
But they also don't specify how they handle duration for married couples

Kamp Dush et al (NLSY)
"Note that cohabiting unions that became marriages were treated as continuing cohabiting unions."
*/