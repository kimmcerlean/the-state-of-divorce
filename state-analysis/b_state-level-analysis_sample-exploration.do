********************************************************************************
* Adding in state-level data
* state-level-analysis.do
* Kim McErlean
********************************************************************************
// ssc install marginscontplot - have to make sure you have this pckage
// ssc install firthlogit, replace

* This file is created as part of the Social Forces R&R
* Given reviewer comments, trying to figure out several things:
	** Include cohabitors?
	** Retain focus on parents (and if so - change sample in any way)
	** Some questions about measures included in scale
* Eventually - will go back to main step b with final included analyses

********************************************************************************
* First just get data and do final sample restrictions
********************************************************************************
use "$created_data/PSID_union_sample_dedup.dta", clear // created in step 3 in main folder

// Final sample restrictions
tab matrix_marr_num, m
tab matrix_rel_num, m
tab current_rel_number current_marr_number, m // this includes historical relationships. BUT - need to figure out how to do this across cohab and marriage. likee ...how do I want to keep people? also - if couples transition to marriage, where do I start their clock? I am leaning towards them contributing to cohab when cohab (and then marriage is essentially a censoring event) and married when married - but yeah, do I need two clocks then?

/*
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

browse unique_id survey_yr partner_unique_id in_marital_history current_rel_type current_rel_number current_marr_number rel_start_yr_couple rel_end_yr_couple

// keep if matrix_marr_num==1
keep if current_rel_number==1 | current_marr_number==1
keep if (AGE_HEAD_>=18 & AGE_HEAD_<=55) &  (AGE_WIFE_>=18 & AGE_WIFE_<=55)

gen age_flag_2554=0
replace age_flag_2554=1 if (AGE_HEAD_>=25 & AGE_HEAD_<=54) &  (AGE_WIFE_>=25 & AGE_WIFE_<=54)

gen age_flag_2055=0
replace age_flag_2055=1 if (AGE_HEAD_>=20 & AGE_HEAD_<=55) &  (AGE_WIFE_>=20 & AGE_WIFE_<=55)

keep if inrange(rel_start_yr_couple,1995,2014) // for married sample - if the point of 2014 is to avoid short durations, for married sample, do I also need to restrict on year of transition to marriage? (oh - for married sample, probably at least control for that?)
	tab transition_year, m
keep if inlist(IN_UNIT,0,1,2)
drop if survey_yr==2021 // until I figure out what to do about covid year. I did add the policy measures, now need to figure out if it makes sense to keep for other reasons
drop if STATE_==11 // DC is missing a lot of state variables, so need to remove.
drop if STATE_==0
drop if STATE_==99

// okay, let's create marriage specific start year and duration for now
gen marriage_start_yr = rel_start_yr_couple
replace marriage_start_yr = transition_year if ever_transition==1

gen marr_dur = survey_yr - marriage_start_yr // duh these are negative for the cohab years
tab marr_dur, m
tab marr_dur if current_rel_type==20, m

	// need to group long durations because of collinearity as well. Even worse of a problem for restricting to parents of young children, so needs to be closer to 15+
	gen marr_dur_raw = marr_dur
	replace marr_dur = 16 if marr_dur>=16 & marr_dur<1000

browse unique_id survey_yr partner_unique_id ever_transition current_rel_type current_rel_number current_marr_number rel_start_yr_couple rel_end_yr_couple dur marr_dur marriage_start_yr transition_year

// because I am estimating cohab based on observed transitions, there are not many in odd numbers, and some small durs becoming collinear. Need to aggregate in some wave (make based on wave?)
browse unique_id survey_yr wave current_rel_type rel_start_yr_couple dur
tab dur if current_rel_type==22, m
gen coh_dur = dur
replace coh_dur = dur - 1 if inlist(dur,1,3,5,7,9,11,13)
replace coh_dur = 15 if dur >=15 & dur < =100

gen cohab_flag = .
replace cohab_flag = 0 if current_rel_type==20
replace cohab_flag = flag if current_rel_type==22

// okay, need to create a combined duration indicator? so I can use for when I combine the samples (bc want dur to restart for marriage so treat as two different relationship contributions?? So can't use existing plain dur variable). Is this the right logic?
browse unique_id survey_yr wave current_rel_type rel_start_yr_couple marriage_start_yr dur marr_dur coh_dur ever_transition

gen combined_dur = .
replace combined_dur = coh_dur if current_rel_type==22
replace combined_dur = marr_dur if current_rel_type==20

// let's get a sense of sample NOW  - this needs to be revisited based on how I want to think about relationship order...but just want an initial sense
unique unique_id partner_unique_id, by(current_rel_type)
unique unique_id partner_unique_id if flag==0, by(current_rel_type) // no left-censor. this removes shockingless less than expected (think bc of using the later years)
unique unique_id partner_unique_id if dissolve==1, by(current_rel_type)
unique unique_id partner_unique_id if flag==0 & dissolve==1, by(current_rel_type) // real concern is # of divorces - except i guess bc cohab is more unstable, this might not actually be as bad as I think??
// unique unique_id partner_unique_id if children_under6==1, by(current_rel_type) // I wonder if THIS is the problem...
// unique unique_id partner_unique_id if children_under6==1 & dissolve==1, by(current_rel_type) // there are 230 divorces here for cohab, and 417 for married couples - so really not like...that much less?
// tab current_rel_type children_under6, row m // and actually - almost equally likely to have a kid under 6, regardless of relationship type..

// drop those with no earnings or housework hours the whole time
bysort unique_id: egen min_type = min(hh_earn_type_t1) // since no earners is 4, if the minimum is 4, means that was it the whole time
label values min_type hh_earn_type
sort unique_id survey_yr
browse unique_id survey_yr min_type hh_earn_type_t1

tab min_type // okay very few people had no earnings whole time
drop if min_type ==4

bysort id: egen min_hw_type = min(housework_bkt_t) // since no earners is 4, if the minimum is 4, means that was it the whole time
label values min_hw_type housework_bkt
sort unique_id survey_yr
browse unique_id survey_yr min_hw_type housework_bkt_t

tab min_hw_type // same here
drop if min_hw_type ==4

********************************************************************************
* Some variables that should be created
********************************************************************************
// controls for ref so I know they are lagged. most of these are fixed anyway
// local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner knot1 knot2 knot3 i.couple_educ_gp i.moved_last2 i.couple_joint_religion" 

// the cohab indicators need to be couple level
tab cohab_with_partner_head cohab_with_partner_wife, m
gen cohab_with_partner = 0
replace cohab_with_partner = 1 if cohab_with_partner_head==1 | cohab_with_partner_wife==1

tab cohab_with_other_head cohab_with_other_wife, m
gen cohab_with_other = 0
replace cohab_with_other = 1 if cohab_with_other_head == 1 | cohab_with_other_wife== 1
tab cohab_with_other, m // is this too low? I think this was problematic before I added cohab relationships - but probably not surprising we don't have rel details on prior cohab? since this is only what is observed in SIPP, so def too lows

// indicator if we observed at start of marriage - might want to use some measures in marriage year, but want to see how much that reduces sample
 // I moved this to earlier step
browse unique_id partner_unique_id survey_yr rel_start_all dur min_dur total_dur

// create joint religion indicator
tab religion_head religion_wife

gen couple_joint_religion=.
replace couple_joint_religion = 0 if religion_head==0 & religion_wife==0
replace couple_joint_religion = 1 if religion_head==1 & religion_wife==1
replace couple_joint_religion = 2 if inlist(religion_head,3,4,5,6) & inlist(religion_wife,3,4,5,6)
// replace couple_joint_religion = 3 if (religion_head==1 & religion_wife!=1 & religion_wife!=.) | (religion_head!=1 & religion_head!=. & religion_wife==1)
replace couple_joint_religion = 3 if (religion_head==1 & religion_wife!=1) | (religion_head!=1 & religion_wife==1)
replace couple_joint_religion = 4 if ((religion_head==0 & religion_wife!=0) | (religion_head!=0 & religion_wife==0)) & couple_joint_religion==.
replace couple_joint_religion = 5 if inlist(religion_head,2,7,8,9,10) & inlist(religion_wife,2,7,8,9,10)
replace couple_joint_religion = 5 if couple_joint_religion==. & religion_head!=. & religion_wife!=. 
// tab religion_head religion_wife if couple_joint_religion==.

label define couple_joint_religion 0 "Both None" 1 "Both Catholic" 2 "Both Protestant" 3 "One Catholic" 4 "One No Religion" 5 "Other"
label values couple_joint_religion couple_joint_religion

quietly unique couple_joint_religion if couple_joint_religion!=., by(unique_id) gen(relig_change)
bysort unique_id (relig_change): replace relig_change=relig_change[1]
tab relig_change, m

browse unique_id survey_yr rel_start_all couple_joint_religion religion_head religion_wife relig_change

* create t-1 version
sort id survey_yr
gen couple_joint_religion_t1=.
replace couple_joint_religion_t1=couple_joint_religion[_n-1] if unique_id==unique_id[_n-1] & wave==wave[_n-1]+1
label values couple_joint_religion_t1 couple_joint_religion

replace couple_joint_religion_t1=couple_joint_religion if couple_joint_religion_t1==. & relig_change==1 // if religion same whole time, fill in t-1 with t
replace couple_joint_religion_t1=couple_joint_religion if couple_joint_religion_t1==. & couple_joint_religion==couple_joint_religion_t1[_n+1] & unique_id==unique_id[_n+1]

browse unique_id survey_yr rel_start_all couple_joint_religion couple_joint_religion_t1

* create start of marriage version
gen couple_joint_religion_mar = couple_joint_religion if dur==min_dur & inlist(min_dur,0,1,2)
bysort unique_id (couple_joint_religion_mar): replace couple_joint_religion_mar=couple_joint_religion_mar[1]
replace couple_joint_religion_mar = couple_joint_religion if couple_joint_religion_mar ==. & relig_change==1

label values couple_joint_religion_mar couple_joint_religion

// browse unique_id survey_yr rel_start_all dur min_dur couple_joint_religion couple_joint_religion_mar

// create lagged education measures
quietly unique educ_type if educ_type!=., by(unique_id) gen(educ_type_chg)
bysort unique_id (educ_type_chg): replace educ_type_chg=educ_type_chg[1]
tab educ_type_chg, m

sort unique_id survey_yr
browse unique_id survey_yr rel_start_all min_dur couple_educ_gp educ_type educ_type_chg educ_wife_est educ_head_est 

* t-1
gen couple_educ_gp_t1=.
replace couple_educ_gp_t1=couple_educ_gp[_n-1] if unique_id==unique_id[_n-1] & wave==wave[_n-1]+1
replace couple_educ_gp_t1=couple_educ_gp if couple_educ_gp_t1==. & educ_type_chg==1 // if same whole time, fill in t-1 with t

label values couple_educ_gp_t1 couple_educ

gen educ_type_t1=.
replace educ_type_t1=educ_type[_n-1] if unique_id==unique_id[_n-1] & wave==wave[_n-1]+1
replace educ_type_t1=educ_type if educ_type_t1==. & educ_type_chg==1 // if same whole time, fill in t-1 with t

label values educ_type_t1 educ_type

browse unique_id survey_yr rel_start_all couple_educ_gp couple_educ_gp_t1 educ_type educ_type_t1 educ_type_chg educ_wife_est educ_head_est 

* start of marriage
gen couple_educ_gp_mar = couple_educ_gp if dur==min_dur & inlist(min_dur,0,1,2)
bysort unique_id (couple_educ_gp_mar): replace couple_educ_gp_mar=couple_educ_gp_mar[1]
replace couple_educ_gp_mar = couple_educ_gp if couple_educ_gp_mar ==. & educ_type_chg==1

label values couple_educ_gp_t1 couple_educ

gen educ_type_mar = educ_type if dur==min_dur & inlist(min_dur,0,1,2)
bysort unique_id (educ_type_mar): replace educ_type_mar=educ_type_mar[1]
replace educ_type_mar = educ_type if educ_type_mar ==. & educ_type_chg==1

label values educ_type_t1 couple_educ

// fix region
gen region = REGION_
replace region = . if inlist(REGION_,0,9)
label define region 1 "Northeast" 2 "North Central" 3 "South" 4 "West" 5 "Alaska,Hawaii" 6 "Foreign"
label values region region

// Migration status - either control or actually remove
label values STATE_ MOVED_ .
quietly unique STATE_ if STATE_!=., by(unique_id) gen(state_change)
bysort unique_id (state_change): replace state_change=state_change[1]
tab state_change, m

sort unique_id survey_yr 
browse unique_id partner_unique_id survey_yr STATE_ state_change MOVED_ MOVED_YEAR_

gen moved_states = .
replace moved_states = 0 if STATE_==STATE_[_n-1] & unique_id==unique_id[_n-1] & wave==wave[_n-1]+1
replace moved_states = 0 if state_change==1
replace moved_states = 1 if STATE_!=STATE_[_n-1] & unique_id==unique_id[_n-1] & wave==wave[_n-1]+1
replace moved_states = 0 if moved_states==. & state_change!=0 // remaining are first observations
tab moved_states, m

browse unique_id partner_unique_id survey_yr STATE_ state_change moved_states rel_start_all MOVED_ MOVED_YEAR_
tab state_change moved_states, m

gen moved_states_lag = .
replace moved_states_lag = 0 if STATE_==STATE_[_n+1] & unique_id==unique_id[_n+1] & wave==wave[_n+1]-1
replace moved_states_lag = 0 if state_change==1
replace moved_states_lag = 1 if STATE_!=STATE_[_n+1] & unique_id==unique_id[_n+1] & wave==wave[_n+1]-1
replace moved_states_lag = 0 if moved_states_lag==. & STATE_==STATE_[_n-1] & unique_id==unique_id[_n-1] & wave==wave[_n-1]+1 // last survey waves
replace moved_states_lag = 0 if moved_states_lag==. & state_change!=0 // remaining are last observations

tab moved_states_lag, m
tab moved_states moved_states_lag, m
tab state_change moved_states_lag, m

browse unique_id partner_unique_id survey_yr STATE_ state_change moved_states moved_states_lag rel_start_all dissolve MOVED_ MOVED_YEAR_

gen moved_last2=.
replace moved_last2 = 0 if moved_states_lag==0 & moved_states==0
replace moved_last2 = 1 if moved_states_lag==1 | moved_states==1

// splitting the college group into who has a degree. also considering advanced degree as higher than college -- this currently only works for cohort 3. I think for college - the specific years matter to split advanced, but for no college - distinguishing between grades less relevant?
// moved this to earlier step

// alternative DoL measures
// more discrete measures of work contributions
input group
.10
.20
.30
.40
.50
.60
.70
.80
1
end

xtile female_hours_bucket_t1 = female_hours_pct_t1, cut(group)
browse female_hours_bucket_t1 female_hours_pct_t1 weekly_hrs_t1_wife ft_pt_t1_wife weekly_hrs_t1_head ft_pt_t1_head

// overwork
gen overwork_t1_head = 0
replace overwork_t1_head =1 if weekly_hrs_t1_head >50 & weekly_hrs_t1_head<=200 // used by Cha 2013

gen overwork_t1_wife = 0 
replace overwork_t1_wife = 1 if weekly_hrs_t1_wife > 50 & weekly_hrs_t1_wife<=200

gen bw_type_t1=.
replace bw_type_t1=1 if inlist(ft_pt_t1_head,1,2) & ft_pt_t1_wife==0
replace bw_type_t1=2 if ft_pt_t1_head==2 & ft_pt_t1_wife==1
replace bw_type_t1=3 if (ft_pt_t1_head==2 & ft_pt_t1_wife==2) | (ft_pt_t1_wife==1 & ft_pt_t1_head==1)
replace bw_type_t1=4 if ft_pt_t1_head==1 & ft_pt_t1_wife==2
replace bw_type_t1=5 if ft_pt_t1_head==0 & inlist(ft_pt_t1_wife,1,2)

label define bw_type 1 "Male BW" 2 "Male and a half" 3 "Dual" 4 "Female and a half" 5 "Female BW"
label values bw_type_t1 bw_type

gen bw_type_t1_gp=.
replace bw_type_t1_gp=1 if ft_t1_head==1 & ft_t1_wife==1
replace bw_type_t1_gp=2 if ft_t1_head==1 & ft_t1_wife==0
replace bw_type_t1_gp=3 if ft_t1_head==0 & ft_t1_wife==1
replace bw_type_t1_gp=4 if ft_t1_head==0 & ft_t1_wife==0

label define bw_type_gp 1 "Both FT" 2 "Male FT" 3 "Female FT"  4 "Neither FT"
label values bw_type_t1_gp bw_type_gp

gen bw_type_t1_gp_alt=.
replace bw_type_t1_gp_alt=1 if bw_type_t1==3
replace bw_type_t1_gp_alt=2 if inlist(bw_type_t1,1,2)
replace bw_type_t1_gp_alt=3 if inlist(bw_type_t1,4,5)
replace bw_type_t1_gp_alt=4 if ft_pt_t1_wife==0 & ft_pt_t1_head==0

label define bw_type_gp_alt 1 "Dual" 2 "Male BW" 3 "Female BW"  4 "Neither works"
label values bw_type_t1_gp_alt bw_type_gp_alt

/* combined
* employment
gen hours_type_hw_t1=.
replace hours_type_hw_t1=1 if bw_type==3 & housework_bkt==1
replace hours_type_hw_t1=2 if bw_type==3 & housework_bkt==2
replace hours_type_hw_t1=3 if bw_type==3 & housework_bkt==3
replace hours_type_hw_t1=4 if inlist(bw_type,1,2) & housework_bkt==1
replace hours_type_hw_t1=5 if inlist(bw_type,1,2) & housework_bkt==2
replace hours_type_hw_t1=6 if inlist(bw_type,1,2) & housework_bkt==3
replace hours_type_hw_t1=7 if inlist(bw_type,4,5) & housework_bkt==1
replace hours_type_hw_t1=8 if inlist(bw_type,4,5) & housework_bkt==2
replace hours_type_hw_t1=9 if inlist(bw_type,4,5) & housework_bkt==3

label define hours_type_hw 1 "Dual: Equal" 2 "Dual: Woman" 3 "Dual: Man" 4 "Male BW: Equal" 5 "Male BW: Woman" 6 "Male BW: Man" 7 "Female BW: Equal" 8 "Female BW: Woman" 9 "Female BW: Man"
label values hours_type_hw_t1 hours_type_hw

* earnings
gen earn_type_hw=.
replace earn_type_hw=1 if hh_earn_type==1 & housework_bkt==1
replace earn_type_hw=2 if hh_earn_type==1 & housework_bkt==2
replace earn_type_hw=3 if hh_earn_type==1 & housework_bkt==3
replace earn_type_hw=4 if hh_earn_type==2 & housework_bkt==1
replace earn_type_hw=5 if hh_earn_type==2 & housework_bkt==2
replace earn_type_hw=6 if hh_earn_type==2 & housework_bkt==3
replace earn_type_hw=7 if hh_earn_type==3 & housework_bkt==1
replace earn_type_hw=8 if hh_earn_type==3 & housework_bkt==2
replace earn_type_hw=9 if hh_earn_type==3 & housework_bkt==3

label define earn_type_hw 1 "Dual: Equal" 2 "Dual: Woman" 3 "Dual: Man" 4 "Male BW: Equal" 5 "Male BW: Woman" 6 "Male BW: Man" 7 "Female BW: Equal" 8 "Female BW: Woman" 9 "Female BW: Man"
label values earn_type_hw earn_type_hw
*/

gen division_bucket_t1=5
replace division_bucket_t1 = 1 if hh_earn_type_t1== 1 & housework_bkt_t== 1 // dual, dual
replace division_bucket_t1 = 2 if hh_earn_type_t1== 2 & housework_bkt_t== 2 // male bw, female hw
replace division_bucket_t1 = 3 if hh_earn_type_t1== 3 & housework_bkt_t== 3 // female bw, male hw
replace division_bucket_t1 = 4 if hh_earn_type_t1== 1 & housework_bkt_t== 2 // dual, female hw
replace division_bucket_t1 = . if hh_earn_type_t1==. | housework_bkt_t==.

label define division_bucket 1 "Dual" 2 "Traditional" 3 "Counter-traditional" 4 "Second shift" 5 "All Other"
label values division_bucket_t1 division_bucket

gen hh_earn_type_t1_alt=.
replace hh_earn_type_t1_alt=hh_earn_type_t1
replace hh_earn_type_t1_alt=4 if inlist(ft_pt_t1_head,0,1) & inlist(ft_pt_t1_wife,0,1) // neither works FT
label define hh_earn_type_t1_alt 1 "Dual" 2 "Male BW" 3 "Female BW" 4 "Neither FT"

* hours
gen division_bucket_hrs_t1=5
replace division_bucket_hrs_t1 = 1 if hh_hours_type_t1== 1 & housework_bkt_t== 1 // dual, dual
replace division_bucket_hrs_t1 = 2 if hh_hours_type_t1== 2 & housework_bkt_t== 2 // male bw, female hw
replace division_bucket_hrs_t1 = 3 if hh_hours_type_t1== 3 & housework_bkt_t== 3 // female bw, male hw
replace division_bucket_hrs_t1 = 4 if hh_hours_type_t1== 1 & housework_bkt_t== 2 // dual, female hw
replace division_bucket_hrs_t1 = . if hh_hours_type_t1== . | housework_bkt_t== .

label values division_bucket_hrs_t1 division_bucket

* hours - need to put counter-traditional in all others because it's not estimating on its own for most data views
gen division_bucket_hrs_gp_t1=4
replace division_bucket_hrs_gp_t1 = 1 if hh_hours_type_t1== 1 & housework_bkt_t== 1 // dual, dual
replace division_bucket_hrs_gp_t1 = 2 if hh_hours_type_t1== 2 & housework_bkt_t== 2 // male bw, female hw
replace division_bucket_hrs_gp_t1 = 4 if hh_hours_type_t1== 3 & housework_bkt_t== 3 // female bw, male hw
replace division_bucket_hrs_gp_t1 = 3 if hh_hours_type_t1== 1 & housework_bkt_t== 2 // dual, female hw
replace division_bucket_hrs_gp_t1 = . if hh_hours_type_t1== . | housework_bkt_t== .

label define div_gp 1 "Dual" 2 "Traditional" 3 "Second shift" 4 "All Other"
label values division_bucket_hrs_gp_t1 div_gp

// this doesn't capture OVERWORK
sum weekly_hrs_t1_head if ft_pt_t1_head==2, detail
sum weekly_hrs_t1_wife if ft_pt_t1_wife==2, detail

// dissimilarity
* paid work hours
gen hours_diff_t1 = weekly_hrs_t1_head - weekly_hrs_t1_wife
browse hours_diff_t1 weekly_hrs_t1_head weekly_hrs_t1_wife

gen hours_diff_t1_bkt = .
replace hours_diff_t1_bkt = 1 if hours_diff_t1 <=10 & hours_diff_t1 >=-10
replace hours_diff_t1_bkt = 2 if hours_diff_t1 >10 & hours_diff_t1 <=150
replace hours_diff_t1_bkt = 3 if hours_diff_t1 <-10 & hours_diff_t1 >=-150

label define hours_diff_bkt 1 "Similar" 2 "Skew Male" 3 "Skew Female"
label values hours_diff_t1_bkt hours_diff_bkt 

browse hours_diff_t1_bkt hours_diff_t1

* hw hours
sort unique_id survey_yr
gen housework_t1_wife=.
replace housework_t1_wife=housework_wife[_n-1] if unique_id==unique_id[_n-1] & wave==wave[_n-1]+1

gen housework_t1_head=.
replace housework_t1_head=housework_head[_n-1] if unique_id==unique_id[_n-1] & wave==wave[_n-1]+1

// browse unique_id survey_yr housework_wife housework_t1_wife housework_head housework_t1_head housework_bkt_t housework_bkt_t1

gen hw_diff_t1 = housework_t1_wife - housework_t1_head
browse hw_diff_t1 housework_t1_wife housework_t1_head

gen hw_diff_t1_bkt = .
replace hw_diff_t1_bkt = 1 if hw_diff_t1 <=10 & hw_diff_t1 >=-10
replace hw_diff_t1_bkt = 2 if hw_diff_t1 >10 & hw_diff_t1 <=150
replace hw_diff_t1_bkt = 3 if hw_diff_t1 <-10 & hw_diff_t1 >=-150

label define hw_diff_bkt 1 "Similar" 2 "Skew Female" 3 "Skew Male"
label values hw_diff_t1_bkt hw_diff_bkt 

browse hw_diff_t1_bkt hw_diff_t1

// test spline at 0.5
mkspline earn_ratio1 0.5 earn_ratio2 = female_earn_pct_t1
browse female_earn_pct_t1 earn_ratio1 earn_ratio2 

mkspline hrs_ratio1 0.5 hrs_ratio2 = female_hours_pct_t1
browse female_hours_pct_t1 hrs_ratio1 hrs_ratio2

// alternate earnings measures
*Convert to 1000s
gen earnings_1000s = couple_earnings_t1 / 1000

*log
gen earnings_total = couple_earnings_t1 + 1 
gen earnings_ln = ln(earnings_total)
* browse TAXABLE_T1_HEAD_WIFE_ couple_earnings_t1

*square
gen earnings_sq = couple_earnings_t1 * couple_earnings_t1

* groups
gen earnings_bucket_t1=.
replace earnings_bucket_t1 = 0 if couple_earnings_t1 <=0
replace earnings_bucket_t1 = 1 if couple_earnings_t1 > 0 		& couple_earnings_t1 <=10000
replace earnings_bucket_t1 = 2 if couple_earnings_t1 > 10000 	& couple_earnings_t1 <=20000
replace earnings_bucket_t1 = 3 if couple_earnings_t1 > 20000 	& couple_earnings_t1 <=30000
replace earnings_bucket_t1 = 4 if couple_earnings_t1 > 30000 	& couple_earnings_t1 <=40000
replace earnings_bucket_t1 = 5 if couple_earnings_t1 > 40000 	& couple_earnings_t1 <=50000
replace earnings_bucket_t1 = 6 if couple_earnings_t1 > 50000 	& couple_earnings_t1 <=60000
replace earnings_bucket_t1 = 7 if couple_earnings_t1 > 60000 	& couple_earnings_t1 <=70000
replace earnings_bucket_t1 = 8 if couple_earnings_t1 > 70000 	& couple_earnings_t1 <=80000
replace earnings_bucket_t1 = 9 if couple_earnings_t1 > 80000 	& couple_earnings_t1 <=90000
replace earnings_bucket_t1 = 10 if couple_earnings_t1 > 90000 	& couple_earnings_t1 <=100000
replace earnings_bucket_t1 = 11 if couple_earnings_t1 > 100000 & couple_earnings_t1 <=150000
replace earnings_bucket_t1 = 12 if couple_earnings_t1 > 150000 & couple_earnings_t1 !=.

label define earnings_bucket_t1 0 "0" 1 "0-10000" 2 "10000-20000" 3 "20000-30000" 4 "30000-40000" 5 "40000-50000" 6 "50000-60000" 7 "60000-70000" ///
8 "70000-80000" 9 "80000-90000" 10 "90000-100000" 11 "100000-150000" 12 "150000+"
label values earnings_bucket_t1 earnings_bucket_t1

*Spline
mkspline knot1 0 knot2 20 knot3 = earnings_1000s

// want to create time-invariant indicator of hh type in first year of marriage (but need to make sure it's year both spouses in hh) - some started in of year gah. use DUR? or rank years and use first rank? (actually is that a better duration?) well, this then doesn't mean it's year of marriage if not observed, so it should be blank
bysort unique_id (survey_yr): egen yr_rank=rank(survey_yr)
// browse unique_id survey_yr rel_start_all dur yr_rank hh_earn_type_t1

gen hh_earn_type_mar = hh_earn_type_t if dur==min_dur & inlist(min_dur,0,1,2)
bysort unique_id (hh_earn_type_mar): replace hh_earn_type_mar=hh_earn_type_mar[1]
label values hh_earn_type_mar hh_earn_type

gen hh_hours_type_mar = hh_hours_type_t if dur==min_dur & inlist(min_dur,0,1,2)
bysort unique_id (hh_hours_type_mar): replace hh_hours_type_mar=hh_hours_type_mar[1]
label values hh_hours_type_mar hh_hours_type

sort unique_id survey_yr
browse id survey_yr rel_start_all yr_rank dur hh_earn_type_t hh_earn_type_mar hh_hours_type_t hh_hours_type_mar

/* okay rolling change in female earn pct - absolute or relative?! absolute for now...
sort id survey_yr
gen female_earn_pct_chg = (female_earn_pct-female_earn_pct[_n-1]) if id==id[_n-1]
browse id survey_yr rel_start_all female_earn_pct female_earn_pct_chg
*/

// alt cohab
gen ever_cohab=0
replace ever_cohab=1 if cohab_with_partner==1 | cohab_with_other==1

// categorical for number of children
recode NUM_CHILDREN_ (0=0)(1=1)(2=2)(3/13=3), gen(num_children)
label define num_children 0 "None" 1 "1 Child" 2 "2 Children" 3 "3+ Children"
label values num_children num_children

// square age of marriage
gen age_mar_head_sq = age_mar_head * age_mar_head
gen age_mar_wife_sq = age_mar_wife * age_mar_wife

// create binary home ownership variable
gen home_owner=0
replace home_owner=1 if HOUSE_STATUS_==1

* ever home_owner
bysort unique_id: egen ever_home_owner = max(home_owner)
tab home_owner, m
tab ever_home_owner, m
tab ever_home_owner home_owner, m

* t-1
gen home_owner_t1=.
replace home_owner_t1=home_owner[_n-1] if unique_id==unique_id[_n-1] & wave==wave[_n-1]+1
replace home_owner_t1 = 0 if home_owner_t1==. & home_owner==0 // in theory, can only change one direction? well, that is probably not true, but assuming if next observation is a 0, the one prior also is. still quite a lot missing. it is possible there is a change in home ownership status specifically upon the transition to marriage so don't really want to make a lot of assumptions.

// browse unique_id survey_yr home_owner home_owner_t1 ever_home_owner HOUSE_STATUS_

// lag school enrollment
gen either_enrolled_t1=.
replace either_enrolled_t1=either_enrolled[_n-1] if unique_id==unique_id[_n-1] & wave==wave[_n-1]+1
replace either_enrolled_t1 = 0 if either_enrolled_t1==. & either_enrolled==0 

// create new variable for having kids under 6 in household.
// also doing some exploring and considering other ways to measure
tab AGE_YOUNG_CHILD_ if children==1 // okay, this is an interesting statistic: of those with children in the HH, 68% have a child under 6 (so it is the majority experience)

gen children_under6=0
replace children_under6=1 if children==1 & AGE_YOUNG_CHILD_ < 6

tab dur children_under6, row

bysort unique_id: egen num_years_child = count(survey_yr) if children==1
bysort unique_id (num_years_child): replace num_years_child=num_years_child[1]
replace num_years_child = 0 if num_years_child==. & children_ever==0

bysort unique_id: egen num_years_child_u6 = count(survey_yr) if children_under6==1
bysort unique_id (num_years_child_u6): replace num_years_child_u6=num_years_child_u6[1]
replace num_years_child_u6 = 0 if num_years_child_u6==. & children_ever==0
tab AGE_YOUNG_CHILD_ if num_years_child_u6==.
replace num_years_child_u6 = 0 if num_years_child_u6==. 
tab num_years_child_u6 children_under6, m
tabstat num_years_child_u6 if num_years_child_u6!=0
tabstat num_years if num_years_child_u6!=0

gen prop_years_childu6 = num_years_child_u6 / num_years
tabstat prop_years_childu6 if num_years_child_u6!=0

gen dur_childu6 = dur if children_under6==1
bysort unique_id: egen childu6_cutoff = max(dur_childu6)
bysort unique_id: egen childu6_start = min(dur_childu6)
sort unique_id survey_yr

// browse unique_id dur children_under6 childu6_start childu6_cutoff

// rel start to no more kids under 6
gen children_under6_flag = 0
replace children_under6_flag = 1 if dur <= childu6_cutoff & childu6_cutoff!=.
tab children_under6_flag, m

// one year prior to first kid under 6 to kids under 6
gen children_under6_flag_alt = 0
replace children_under6_flag_alt = 1 if dur >= (childu6_start-2) & dur <= childu6_cutoff & childu6_cutoff!=.
tab children_under6_flag_alt, m

tab children_under6_flag children_under6, m row
tab children_under6_flag_alt children_under6, m row
tab children_under6_flag children_under6_flag_alt, m row

sort unique_id survey_yr
browse unique_id partner_unique_id survey_yr rel_start_all min_dur dur childu6_cutoff children_under6_flag children_under6 AGE_YOUNG_CHILD_ num_years num_years_child_u6 prop_years_childu6 children num_children 

	// also create other cut-offs for robustness
	* this is would be like pre-high-school
	gen children_under14=0
	replace children_under14=1 if children==1 & AGE_YOUNG_CHILD_ < 14
	
	tab children_under14 children_under6_flag, m

// create dummy variable for interval length
gen interval=.
replace interval=1 if inrange(survey_yr,1968,1997)
replace interval=2 if inrange(survey_yr,1999,2021)

********************************************************************************
* Last checks and data cleaning / set-up
********************************************************************************

* Final sample restrictions based on missing values and division of labor to divide
// missing value inspect
inspect age_mar_wife // 18
inspect age_mar_head // 3
// inspect raceth_head
inspect raceth_head_fixed // 39
inspect same_race // 0
inspect either_enrolled // 0
inspect region // 0 
inspect STATE_ // 0
inspect cohab_with_partner // 0
inspect cohab_with_other // 0 
inspect pre_marital_birth // 0
inspect home_owner // 0
inspect couple_joint_religion // this has the most missing (502 - about 3.5%)
inspect earnings_bucket_t1 // 0
inspect couple_earnings_t1 // 0 (underlying variable)
inspect couple_educ_gp // 360 (2.5%)
inspect moved_last2 // 0
inspect num_children // 0
inspect hh_hours_type_t1 // 0
inspect housework_bkt_t // 196
inspect division_bucket_hrs_t1 // 196

// create flag
gen any_missing = 0
replace any_missing = 1 if age_mar_wife==. | age_mar_head==. | raceth_head_fixed==. | couple_joint_religion ==. | couple_educ_gp==. | housework_bkt_t==. | division_bucket_hrs_t1==.
tab any_missing , m // this is closer to 7% Is this too high?

// flag for no paid or unpaid labor to observe
gen no_labor = 0 
replace no_labor = 1 if hh_hours_type_t1==4 | housework_bkt_t ==4
tab no_labor, m

tab any_missing no_labor, m // so this is about 10%.

// some states cannot be estimated on their own so are being dropped - do I group these? Need to figure out what to do...
// cc: https://academicweb.nd.edu/~rwilliam/stats3/RareEvents.pdf - do I need to consider firthlogit??
// for married couples, it's 30, 35, 44, 50, 54
// for cohab, it's 2, 23, 30, 31, 35, 38, 44, 46, 49, 50, 54, 56 // is this too many?

* Small things needed for analysis (run through this so you have controls and for figures)
set scheme cleanplots

global controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion i.num_children"  // i.region knot1 knot2 knot3 

/* QA
preserve
collapse (max) rel_start_all rel_end_all dissolve dissolve_v0 outcome end_year survey_yr (count) num_yrs=survey_yr, by(unique_id partner_unique_id)
restore
*/

********************************************************************************
**# Merge onto policy data
********************************************************************************
local scale_vars "structural_familism structural_factor cc_pct_income_orig prek_enrolled_public cc_pct_served policy_lib_all policy_lib_econ policy_lib_soc gender_factor_reg fepresch_reg fechld_reg fefam_reg preschool_egal_reg working_mom_egal_reg genderroles_egal_reg avg_egal_reg fepresch_state fechld_state fefam_state gender_factor_state preschool_egal_state working_mom_egal_state genderroles_egal_state avg_egal_state evang_lds_rate married_dual_earn_rate married_pure_male_bw_rate married_women_emp_rate_wt maternal_u5_employment_wt min_amt_above_fed unemployment_percap wba_max high_inc_prem_pct low_inc_prem_pct earn_ratio married_earn_ratio welfare_all paid_leave abortion_protected educ_spend_percap headstart_pct headstart_pct_totalpop earlyhs_pct earlyhs_pct_totalpop total_headstart_pct total_headstart_pct_totalpop diffusion policy_group_v1 policy_group_v2 policy_group_v3" 

rename STATE_ state_fips
rename survey_yr year

//merge m:1 state_fips year using "$raw_state_data/structural_familism_jue25_int.dta" // merging on this file for now to explore - so commenting out the below while I sort this out
merge m:1 state_fips year using "$created_data/scale_refresh.dta", keepusing(`scale_vars')
drop if _merge==2
drop _merge

foreach var in `scale_vars'{
	rename `var' `var'_t
}

gen year_t1 = year - 1
merge m:1 year_t1 state_fips using "$created_data/scale_refresh.dta", keepusing(`scale_vars')
drop if _merge==2
drop _merge

foreach var in `scale_vars'{
	rename `var' `var'_t1
}

/*
local scale_vars "structural_familism structural_familism_v0 paid_leave_st paid_leave_length_st prek_enrolled_public_st min_amt_above_fed_st min_above_fed_st earn_ratio_neg_st unemployment_percap_st abortion_protected_st welfare_all_st paid_leave paid_leave_length prek_enrolled_public min_amt_above_fed min_above_fed earn_ratio_neg unemployment_percap abortion_protected welfare_all sf_centered" // f1 

merge m:1 state_fips year using "$state_data/structural_familism.dta", keepusing(`scale_vars')
drop if _merge==2
drop _merge

foreach var in `scale_vars'{
	rename `var' `var'_t
}

// rename structural_familism structural_familism_t
capture drop year_t1 year_t2

gen year_t1 = year - 1
merge m:1 year_t1 state_fips using "$state_data/structural_familism.dta", keepusing(`scale_vars')
drop if _merge==2
drop _merge

foreach var in `scale_vars'{
	rename `var' `var'_t1
}
// rename structural_familism structural_familism_t1

label values state_fips .
sort unique_id year
browse unique_id year state_fips structural_familism_t structural_familism_t1 structural_familism_v0_t structural_familism_v0_t1

// do I center the main data or here? in main data, no real need to center (already essentially at 0)
gen sf_centered_alt_t=.
sum structural_familism_t, detail
replace sf_centered_alt_t = structural_familism_t - `r(mean)'

gen sf_centered_alt_t1=.
sum structural_familism_t1, detail
replace sf_centered_alt_t1 = structural_familism_t1 - `r(mean)'

browse structural_familism_t structural_familism_v0_t sf_centered_t sf_centered_alt_t
sum structural_familism_t sf_centered_t sf_centered_alt_t, detail

alpha paid_leave_length_st_t1 prek_enrolled_public_st_t1 min_amt_above_fed_st_t1 earn_ratio_neg_st_t1 unemployment_percap_st_t1 abortion_protected_st_t1 welfare_all_st_t1 // structural familism. 0.70 0.716 now
alpha paid_leave_st_t1 prek_enrolled_public_st_t1 min_above_fed_st_t1 earn_ratio_neg_st_t1 unemployment_percap_st_t1 abortion_protected_st_t1 welfare_all_st_t1 
alpha paid_leave_st_t1 prek_enrolled_public_st_t1 min_amt_above_fed_st_t1 earn_ratio_neg_st_t1 unemployment_percap_st_t1 abortion_protected_st_t1 welfare_all_st_t1 
*/

********************************************************************************
********************************************************************************
********************************************************************************
**# Analysis starts
********************************************************************************
********************************************************************************
********************************************************************************

********************************************************************************
* Overall trends
********************************************************************************
logit dissolve i.dur i.couple_educ_gp i.current_rel_type, or
logit dissolve i.dur i.couple_educ_gp##i.current_rel_type, or

// Got some questions about earnings, revisit this
* Parents
logit dissolve i.dur i.hh_earn_type_t1 knot1 knot2 knot3 if children_under6==1 & hh_earn_type_t1 <4 , or
logit dissolve i.dur i.hh_earn_type_t1 earnings_ln if children_under6==1 & hh_earn_type_t1 <4 , or
margins, at(earnings_ln=(0(1)12))
marginsplot

logit dissolve i.dur i.hh_earn_type_t1 earnings_1000s if children_under6==1 & hh_earn_type_t1 <4 , or
margins, at(earnings_1000s=(0(10)200))
marginsplot

logit dissolve i.dur i.hh_earn_type_t1 earnings_1000s c.earnings_1000s#c.earnings_1000s if children_under6==1 & hh_earn_type_t1 <4 , or
margins, at(earnings_1000s=(0(10)200))
marginsplot

logit dissolve i.dur i.hh_earn_type_t1 i.earnings_bucket_t1 if children_under6==1 & hh_earn_type_t1 <4 , or
margins i.earnings_bucket_t1
marginsplot

* All
logit dissolve i.dur i.hh_earn_type_t1 knot1 knot2 knot3 if hh_earn_type_t1 <4 , or
logit dissolve i.dur i.hh_earn_type_t1 earnings_ln if hh_earn_type_t1 <4 , or
margins, at(earnings_ln=(0(1)12))
marginsplot

logit dissolve i.dur i.hh_earn_type_t1 earnings_1000s if hh_earn_type_t1 <4 , or
margins, at(earnings_1000s=(0(10)200))
marginsplot

logit dissolve i.dur i.hh_earn_type_t1 earnings_1000s c.earnings_1000s#c.earnings_1000s if hh_earn_type_t1 <4 , or
margins, at(earnings_1000s=(0(10)200))
marginsplot

logit dissolve i.dur i.hh_earn_type_t1 i.earnings_bucket_t1 if hh_earn_type_t1 <4 , or
margins i.earnings_bucket_t1
marginsplot

logit dissolve i.dur i.hh_earn_type_t1 earnings_1000s c.earnings_1000s#c.earnings_1000s c.earnings_1000s#c.earnings_1000s#c.earnings_1000s  if hh_earn_type_t1 <4 , or
margins, at(earnings_1000s=(0(10)200))
marginsplot

////////////////////////////////////////////////////////////////////////////////
**# // Let's first explore basic trends across different samples
////////////////////////////////////////////////////////////////////////////////

********************************************************************************
********************************************************************************
********************************************************************************
**# Married Couples
********************************************************************************
********************************************************************************
********************************************************************************

********************************************************************************
* Total Sample
********************************************************************************
logit dissolve i.marr_dur c.structural_familism_t i.hh_earn_type_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(hh_earn_type_t1) level(95) post
estimates store est1a

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(hh_hours_type_t1) level(95) post
estimates store est2a

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(division_bucket_t1) level(95) post
estimates store est3a

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(division_bucket_hrs_t1) level(95) post
estimates store est4a

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(structural_familism_t) post
estimates store est5a

coefplot est1a est2a est3a est4a,  drop(_cons) nolabel xline(0) levels(95) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners" 2.hh_earn_type_t1 = "Male Breadwinner" 3.hh_earn_type_t1 = "Female Breadwinner" 4.hh_earn_type_t1 = "No Earners" 1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" 2.division_bucket_t1 = "Traditional" 3.division_bucket_t1 = "Counter Traditional" 4.division_bucket_t1 = "Her Second Shift" 5.division_bucket_t1 = "All Others") ///
 headings(2.hh_hours_type_t1= "{bf:Division of Work Hours}" 2.hh_earn_type_t1= `"{bf:Division of Earnings}"' 2.division_bucket_hrs_t1 = `"{bf:Combined (hours)}"' 2.division_bucket_t1 = `"{bf:Combined (earnings)}"')

 // just hours and maybe add structural support
coefplot (est2a, offset(.20) nokey) (est4a, offset(-.20) nokey) (est5a, nokey lcolor("black") mcolor("black") ciopts(color("black"))) , drop(_cons) xline(0) levels(95) base xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners"  1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" structural_familism_t = "Structural Support Scale") ///
 headings(1.hh_hours_type_t1= `"{bf:Division of Work Hours}"'  1.division_bucket_hrs_t1 = `"{bf:Combined Paid and Unpaid Labor}"' structural_familism_t = `"{bf:Structural Support for Working Families}"')
 

********************************************************************************
* All parents
********************************************************************************
logit dissolve i.marr_dur c.structural_familism_t i.hh_earn_type_t1 $controls if children==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(hh_earn_type_t1) level(95) post
estimates store est1b

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 $controls if children==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(hh_hours_type_t1) level(95) post
estimates store est2b

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_t1 $controls if children==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(division_bucket_t1) level(95) post
estimates store est3b

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls if children==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(division_bucket_hrs_t1) level(95) post
estimates store est4b

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls if children==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(structural_familism_t) post
estimates store est5b

coefplot est1b est2b est3b est4b,  drop(_cons) nolabel xline(0) levels(95) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners" 2.hh_earn_type_t1 = "Male Breadwinner" 3.hh_earn_type_t1 = "Female Breadwinner" 4.hh_earn_type_t1 = "No Earners" 1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" 2.division_bucket_t1 = "Traditional" 3.division_bucket_t1 = "Counter Traditional" 4.division_bucket_t1 = "Her Second Shift" 5.division_bucket_t1 = "All Others") ///
 headings(2.hh_hours_type_t1= "{bf:Division of Work Hours}" 2.hh_earn_type_t1= `"{bf:Division of Earnings}"' 2.division_bucket_hrs_t1 = `"{bf:Combined (hours)}"' 2.division_bucket_t1 = `"{bf:Combined (earnings)}"')
 
 // just hours and maybe add structural support
coefplot (est2b, offset(.20) nokey) (est4b, offset(-.20) nokey) (est5b, nokey lcolor("black") mcolor("black") ciopts(color("black"))) , drop(_cons) xline(0) levels(95) base xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners"  1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" structural_familism_t = "Structural Support Scale") ///
 headings(1.hh_hours_type_t1= `""{bf:Division of Work Hours}" "{it:(Model 1)}""'  1.division_bucket_hrs_t1 = `""{bf:Combined Paid and Unpaid Labor}" "{it:(Model 2)}""' structural_familism_t = `""{bf:Structural Support for Working Families}" "{it:(Model 2)}"')
 
********************************************************************************
* All Parents of children under the age of 6
********************************************************************************
logit dissolve i.marr_dur c.structural_familism_t i.hh_earn_type_t1 $controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(hh_earn_type_t1) level(95) post
estimates store est1c

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 $controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(hh_hours_type_t1) level(95) post
estimates store est2c

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_t1 $controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(division_bucket_t1) level(95) post
estimates store est3c

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(division_bucket_hrs_t1) level(95) post
estimates store est4c

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(structural_familism_t) post
estimates store est5c

coefplot est1c est2c est3c est4c,  drop(_cons) nolabel xline(0) levels(95) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners" 2.hh_earn_type_t1 = "Male Breadwinner" 3.hh_earn_type_t1 = "Female Breadwinner" 4.hh_earn_type_t1 = "No Earners" 1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" 2.division_bucket_t1 = "Traditional" 3.division_bucket_t1 = "Counter Traditional" 4.division_bucket_t1 = "Her Second Shift" 5.division_bucket_t1 = "All Others") ///
 headings(2.hh_hours_type_t1= "{bf:Division of Work Hours}" 2.hh_earn_type_t1= `"{bf:Division of Earnings}"' 2.division_bucket_hrs_t1 = `"{bf:Combined (hours)}"' 2.division_bucket_t1 = `"{bf:Combined (earnings)}"')

 // just hours and maybe add structural support
coefplot (est2c, offset(.20) nokey) (est4c, offset(-.20) nokey) (est5c, nokey lcolor("black") mcolor("black") ciopts(color("black"))) , drop(_cons) xline(0) levels(95) base xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners"  1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" structural_familism_t = "Structural Support Scale") ///
 headings(1.hh_hours_type_t1= `"{bf:Division of Work Hours}"'  1.division_bucket_hrs_t1 = `"{bf:Combined Paid and Unpaid Labor}"' structural_familism_t = `"{bf:Structural Support for Working Families}"')

********************************************************************************
* All Parents of children under the age of 6 - start = dur 0 
********************************************************************************
tab children_under6_flag children_under6, m 

logit dissolve i.marr_dur c.structural_familism_t i.hh_earn_type_t1 $controls if children_under6_flag==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(hh_earn_type_t1) level(95) post
estimates store est1d

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 $controls if children_under6_flag==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(hh_hours_type_t1) level(95) post
estimates store est2d

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_t1 $controls if children_under6_flag==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(division_bucket_t1) level(95) post
estimates store est3d

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls if children_under6_flag==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(division_bucket_hrs_t1) level(95) post
estimates store est4d

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 $controls if children_under6_flag==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(structural_familism_t) post
estimates store est5d

coefplot est1d est2d est3d est4d,  drop(_cons) nolabel xline(0) levels(95) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners" 2.hh_earn_type_t1 = "Male Breadwinner" 3.hh_earn_type_t1 = "Female Breadwinner" 4.hh_earn_type_t1 = "No Earners" 1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" 2.division_bucket_t1 = "Traditional" 3.division_bucket_t1 = "Counter Traditional" 4.division_bucket_t1 = "Her Second Shift" 5.division_bucket_t1 = "All Others") ///
 headings(2.hh_hours_type_t1= "{bf:Division of Work Hours}" 2.hh_earn_type_t1= `"{bf:Division of Earnings}"' 2.division_bucket_hrs_t1 = `"{bf:Combined (hours)}"' 2.division_bucket_t1 = `"{bf:Combined (earnings)}"')

 // just hours and maybe add structural support
coefplot (est2d, offset(.20) nokey) (est4d, offset(-.20) nokey) (est5d, nokey lcolor("black") mcolor("black") ciopts(color("black"))) , drop(_cons) xline(0) levels(95) base xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners"  1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" structural_familism_t = "Structural Support Scale") ///
 headings(1.hh_hours_type_t1= `"{bf:Division of Work Hours}"'  1.division_bucket_hrs_t1 = `"{bf:Combined Paid and Unpaid Labor}"' structural_familism_t = `"{bf:Structural Support for Working Families}"')


********************************************************************************
********************************************************************************
********************************************************************************
**# Cohabiting Couples
********************************************************************************
********************************************************************************
********************************************************************************

********************************************************************************
* Total Sample
********************************************************************************
logit dissolve i.dur c.structural_familism_t i.hh_earn_type_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(hh_earn_type_t1) level(95) post
estimates store est6a

logit dissolve i.dur c.structural_familism_t i.hh_hours_type_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(hh_hours_type_t1) level(95) post
estimates store est7a

logit dissolve i.dur c.structural_familism_t i.division_bucket_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(division_bucket_t1) level(95) post
estimates store est8a

logit dissolve i.dur c.structural_familism_t i.division_bucket_hrs_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(division_bucket_hrs_t1) level(95) post
estimates store est9a

logit dissolve i.dur c.structural_familism_t i.division_bucket_hrs_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(structural_familism_t) post
estimates store est10a

coefplot est6a est7a est8a est9a,  drop(_cons) nolabel xline(0) levels(95) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners" 2.hh_earn_type_t1 = "Male Breadwinner" 3.hh_earn_type_t1 = "Female Breadwinner" 4.hh_earn_type_t1 = "No Earners" 1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" 2.division_bucket_t1 = "Traditional" 3.division_bucket_t1 = "Counter Traditional" 4.division_bucket_t1 = "Her Second Shift" 5.division_bucket_t1 = "All Others") ///
 headings(2.hh_hours_type_t1= "{bf:Division of Work Hours}" 2.hh_earn_type_t1= `"{bf:Division of Earnings}"' 2.division_bucket_hrs_t1 = `"{bf:Combined (hours)}"' 2.division_bucket_t1 = `"{bf:Combined (earnings)}"')

 // just hours and maybe add structural support
coefplot (est7a, offset(.20) nokey) (est9a, offset(-.20) nokey) (est10a, nokey lcolor("black") mcolor("black") ciopts(color("black"))) , drop(_cons) xline(0) levels(95) base xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners"  1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" structural_familism_t = "Structural Support Scale") ///
 headings(1.hh_hours_type_t1= `"{bf:Division of Work Hours}"'  1.division_bucket_hrs_t1 = `"{bf:Combined Paid and Unpaid Labor}"' structural_familism_t = `"{bf:Structural Support for Working Families}"')

********************************************************************************
* All parents
********************************************************************************
logit dissolve i.dur c.structural_familism_t i.hh_earn_type_t1 $controls if children==1 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(hh_earn_type_t1) level(95) post
estimates store est6b

logit dissolve i.dur c.structural_familism_t i.hh_hours_type_t1 $controls if children==1 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(hh_hours_type_t1) level(95) post
estimates store est7b

logit dissolve i.dur c.structural_familism_t i.division_bucket_t1 $controls if children==1 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(division_bucket_t1) level(95) post
estimates store est8b

logit dissolve i.dur c.structural_familism_t i.division_bucket_hrs_t1 $controls if children==1 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(division_bucket_hrs_t1) level(95) post
estimates store est9b

logit dissolve i.dur c.structural_familism_t i.division_bucket_hrs_t1 $controls if children==1 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(structural_familism_t) post
estimates store est10b

coefplot est6b est7b est8b est9b,  drop(_cons) nolabel xline(0) levels(95) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners" 2.hh_earn_type_t1 = "Male Breadwinner" 3.hh_earn_type_t1 = "Female Breadwinner" 4.hh_earn_type_t1 = "No Earners" 1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" 2.division_bucket_t1 = "Traditional" 3.division_bucket_t1 = "Counter Traditional" 4.division_bucket_t1 = "Her Second Shift" 5.division_bucket_t1 = "All Others") ///
 headings(2.hh_hours_type_t1= "{bf:Division of Work Hours}" 2.hh_earn_type_t1= `"{bf:Division of Earnings}"' 2.division_bucket_hrs_t1 = `"{bf:Combined (hours)}"' 2.division_bucket_t1 = `"{bf:Combined (earnings)}"')

 // just hours and maybe add structural support
coefplot (est7b, offset(.20) nokey) (est9b, offset(-.20) nokey) (est10b, nokey lcolor("black") mcolor("black") ciopts(color("black"))) , drop(_cons) xline(0) levels(95) base xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners"  1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" structural_familism_t = "Structural Support Scale") ///
 headings(1.hh_hours_type_t1= `"{bf:Division of Work Hours}"'  1.division_bucket_hrs_t1 = `"{bf:Combined Paid and Unpaid Labor}"' structural_familism_t = `"{bf:Structural Support for Working Families}"')
 
********************************************************************************
* All Parents of children under the age of 6
********************************************************************************
logit dissolve i.dur c.structural_familism_t i.hh_earn_type_t1 $controls if children_under6==1 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(hh_earn_type_t1) level(95) post
estimates store est6c

logit dissolve i.dur c.structural_familism_t i.hh_hours_type_t1 $controls if children_under6==1 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(hh_hours_type_t1) level(95) post
estimates store est7c

logit dissolve i.dur c.structural_familism_t i.division_bucket_t1 $controls if children_under6==1 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(division_bucket_t1) level(95) post
estimates store est8c

logit dissolve i.dur c.structural_familism_t i.division_bucket_hrs_t1 $controls if children_under6==1 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(division_bucket_hrs_t1) level(95) post
estimates store est9c

logit dissolve i.dur c.structural_familism_t i.division_bucket_hrs_t1 $controls if children_under6==1 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(structural_familism_t) post
estimates store est10c

coefplot est6c est7c est8c est9c,  drop(_cons) nolabel xline(0) levels(95) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners" 2.hh_earn_type_t1 = "Male Breadwinner" 3.hh_earn_type_t1 = "Female Breadwinner" 4.hh_earn_type_t1 = "No Earners" 1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" 2.division_bucket_t1 = "Traditional" 3.division_bucket_t1 = "Counter Traditional" 4.division_bucket_t1 = "Her Second Shift" 5.division_bucket_t1 = "All Others") ///
 headings(2.hh_hours_type_t1= "{bf:Division of Work Hours}" 2.hh_earn_type_t1= `"{bf:Division of Earnings}"' 2.division_bucket_hrs_t1 = `"{bf:Combined (hours)}"' 2.division_bucket_t1 = `"{bf:Combined (earnings)}"')

 // just hours and maybe add structural support
coefplot (est7c, offset(.20) nokey) (est9c, offset(-.20) nokey) (est10c, nokey lcolor("black") mcolor("black") ciopts(color("black"))) , drop(_cons) xline(0) levels(95) base xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners"  1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" structural_familism_t = "Structural Support Scale") ///
 headings(1.hh_hours_type_t1= `"{bf:Division of Work Hours}"'  1.division_bucket_hrs_t1 = `"{bf:Combined Paid and Unpaid Labor}"' structural_familism_t = `"{bf:Structural Support for Working Families}"')

********************************************************************************
* All Parents of children under the age of 6 - start = dur 0 
********************************************************************************
logit dissolve i.dur c.structural_familism_t i.hh_earn_type_t1 $controls if children_under6_flag==1 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(hh_earn_type_t1) level(95) post
estimates store est6d

logit dissolve i.dur c.structural_familism_t i.hh_hours_type_t1 $controls if children_under6_flag==1 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(hh_hours_type_t1) level(95) post
estimates store est7d

logit dissolve i.dur c.structural_familism_t i.division_bucket_t1 $controls if children_under6_flag==1 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(division_bucket_t1) level(95) post
estimates store est8d

logit dissolve i.dur c.structural_familism_t i.division_bucket_hrs_t1 $controls if children_under6_flag==1 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(division_bucket_hrs_t1) level(95) post
estimates store est9d

logit dissolve i.dur c.structural_familism_t i.division_bucket_hrs_t1 $controls if children_under6_flag==1 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(structural_familism_t) post
estimates store est10d

coefplot est6d est7d est8d est9d,  drop(_cons) nolabel xline(0) levels(95) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners" 2.hh_earn_type_t1 = "Male Breadwinner" 3.hh_earn_type_t1 = "Female Breadwinner" 4.hh_earn_type_t1 = "No Earners" 1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" 2.division_bucket_t1 = "Traditional" 3.division_bucket_t1 = "Counter Traditional" 4.division_bucket_t1 = "Her Second Shift" 5.division_bucket_t1 = "All Others") ///
 headings(2.hh_hours_type_t1= "{bf:Division of Work Hours}" 2.hh_earn_type_t1= `"{bf:Division of Earnings}"' 2.division_bucket_hrs_t1 = `"{bf:Combined (hours)}"' 2.division_bucket_t1 = `"{bf:Combined (earnings)}"')

 // just hours and maybe add structural support
coefplot (est7d, offset(.20) nokey) (est9d, offset(-.20) nokey) (est10d, nokey lcolor("black") mcolor("black") ciopts(color("black"))) , drop(_cons) xline(0) levels(95) base xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners"  1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" structural_familism_t = "Structural Support Scale") ///
 headings(1.hh_hours_type_t1= `"{bf:Division of Work Hours}"'  1.division_bucket_hrs_t1 = `"{bf:Combined Paid and Unpaid Labor}"' structural_familism_t = `"{bf:Structural Support for Working Families}"')
 
 
********************************************************************************
* Charts across marriage and cohab
********************************************************************************
** Total Sample
coefplot (est2a, offset(.20) nokey) (est4a, offset(-.20) nokey), bylabel("Married") || ///
		est7a est9a, bylabel("Cohabitors"), ///
, drop(_cons) xline(0) levels(95) base xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners"  1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others") ///
 headings(1.hh_hours_type_t1= `"{bf:Division of Work Hours}"'  1.division_bucket_hrs_t1 = `"{bf:Combined Division of Labor}"') byopts(rows(1) xrescale)


** Parents of Kids under age of 6
coefplot (est2c, offset(.20) nokey) (est4c, offset(-.20) nokey), bylabel("Married") || ///
		est7c est9c, bylabel("Cohabitors"), ///
, drop(_cons) xline(0) levels(95) base xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners"  1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others") ///
 headings(1.hh_hours_type_t1= `"{bf:Division of Work Hours}"'  1.division_bucket_hrs_t1 = `"{bf:Combined Division of Labor}"') byopts(rows(1) xrescale)

 
////////////////////////////////////////////////////////////////////////////////
**# Now let's look at individual scale indicators across different samples
////////////////////////////////////////////////////////////////////////////////


********************************************************************************
* Married Couples
********************************************************************************

*************
* Continuous
*************

local scale_vars_cont "structural_familism_t structural_factor_t policy_lib_all_t prek_enrolled_public_t min_amt_above_fed_t earn_ratio_t unemployment_percap_t welfare_all_t cc_pct_income_orig_t cc_pct_served_t headstart_pct_t earlyhs_pct_t total_headstart_pct_t educ_spend_percap_t genderroles_egal_reg_t avg_egal_reg_t married_women_emp_rate_wt_t married_pure_male_bw_rate_t evang_lds_rate_t diffusion_t"

foreach var in `scale_vars_cont'{
	
* Total sample
logit dissolve i.marr_dur c.`var' i.hh_hours_type_t1 c.`var'#i.hh_hours_type_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum `var', detail
margins, dydx(hh_hours_type_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_marriage.xls", ctitle(all `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.`var' i.division_bucket_hrs_t1 c.`var'#i.division_bucket_hrs_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum `var', detail
margins, dydx(division_bucket_hrs_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_marriage.xls", ctitle(all `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* All Parents
logit dissolve i.marr_dur c.`var' i.hh_hours_type_t1 c.`var'#i.hh_hours_type_t1 $controls if children==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum `var', detail
margins, dydx(hh_hours_type_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_marriage.xls", ctitle(par `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.`var' i.division_bucket_hrs_t1 c.`var'#i.division_bucket_hrs_t1 $controls if children==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum `var', detail
margins, dydx(division_bucket_hrs_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_marriage.xls", ctitle(par `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Kids under 6
logit dissolve i.marr_dur c.`var' i.hh_hours_type_t1 c.`var'#i.hh_hours_type_t1 $controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum `var', detail
margins, dydx(hh_hours_type_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_marriage.xls", ctitle(u6 `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.`var' i.division_bucket_hrs_t1 c.`var'#i.division_bucket_hrs_t1 $controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum `var', detail
margins, dydx(division_bucket_hrs_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_marriage.xls", ctitle(u6 `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Childfree couples
logit dissolve i.marr_dur c.`var' i.hh_hours_type_t1 c.`var'#i.hh_hours_type_t1 $controls if children==0 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum `var', detail
margins, dydx(hh_hours_type_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_marriage.xls", ctitle(cf `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.`var' i.division_bucket_hrs_t1 c.`var'#i.division_bucket_hrs_t1 $controls if children==0 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum `var', detail
margins, dydx(division_bucket_hrs_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_marriage.xls", ctitle(cf `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

}

*************
* Binary
*************
// for the binary variables - I cannot get counter-traditional to estimate on its own. In some cases, I also cannot get all others to estimate on own. Considering "no labor" as all others for these purposes and using this new variable: division_bucket_hrs_gp_t1 - where counter-trad is grouped into all other

local scale_vars_binary "paid_leave_t abortion_protected_t"

foreach var in `scale_vars_binary'{
	
* Total sample
logit dissolve i.marr_dur i.`var' i.hh_hours_type_t1 i.`var'#i.hh_hours_type_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(2.hh_hours_type_t1) at(`var'=(0 1)) post // 3.hh_hours_type_t1
outreg2 using "$results/scale_exploration_marriage.xls", ctitle(all `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur i.`var' i.division_bucket_hrs_gp_t1 i.`var'#i.division_bucket_hrs_gp_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(division_bucket_hrs_gp_t1) at(`var'=(0 1)) post
outreg2 using "$results/scale_exploration_marriage.xls", ctitle(all `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* All Parents
logit dissolve i.marr_dur i.`var' i.hh_hours_type_t1 i.`var'#i.hh_hours_type_t1 $controls if children==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(2.hh_hours_type_t1) at(`var'=(0 1)) post //  3.hh_hours_type_t1
outreg2 using "$results/scale_exploration_marriage.xls", ctitle(par `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur i.`var' i.division_bucket_hrs_gp_t1 i.`var'#i.division_bucket_hrs_gp_t1 $controls if children==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0, or // & no_labor==0, or
margins, dydx(division_bucket_hrs_gp_t1) at(`var'=(0 1)) post
outreg2 using "$results/scale_exploration_marriage.xls", ctitle(par `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Kids under 6
logit dissolve i.marr_dur i.`var' i.hh_hours_type_t1 i.`var'#i.hh_hours_type_t1 $controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(2.hh_hours_type_t1) at(`var'=(0 1)) post // 3.hh_hours_type_t1
outreg2 using "$results/scale_exploration_marriage.xls", ctitle(u6 `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur i.`var' i.division_bucket_hrs_gp_t1 i.`var'#i.division_bucket_hrs_gp_t1 $controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0, or // & no_labor==0, or
margins, dydx(division_bucket_hrs_gp_t1) at(`var'=(0 1)) post
outreg2 using "$results/scale_exploration_marriage.xls", ctitle(u6 `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Childfree couples
logit dissolve i.marr_dur i.`var' i.hh_hours_type_t1 i.`var'#i.hh_hours_type_t1 $controls if children==0 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(2.hh_hours_type_t1) at(`var'=(0 1)) post // 3.hh_hours_type_t1
outreg2 using "$results/scale_exploration_marriage.xls", ctitle(cf `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur i.`var' i.division_bucket_hrs_gp_t1 i.`var'#i.division_bucket_hrs_gp_t1 $controls if children==0 & current_rel_type==20 & marr_dur>=0 & any_missing==0, or // & no_labor==0, or
margins, dydx(division_bucket_hrs_gp_t1) at(`var'=(0 1)) post
outreg2 using "$results/scale_exploration_marriage.xls", ctitle(cf `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
}

*************
* Categorical
*************
// here also need to combine 3 and 5

local scale_vars_cat "policy_group_v1_t  policy_group_v2_t"

foreach var in `scale_vars_cat'{
	
* Total sample
logit dissolve i.marr_dur i.`var' i.hh_hours_type_t1 i.`var'#i.hh_hours_type_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(hh_hours_type_t1) at(`var'=(1 2 3 4)) post
outreg2 using "$results/scale_exploration_marriage.xls", ctitle(all `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur i.`var' i.division_bucket_hrs_gp_t1 i.`var'#i.division_bucket_hrs_gp_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(division_bucket_hrs_gp_t1) at(`var'=(1 2 3 4)) post
outreg2 using "$results/scale_exploration_marriage.xls", ctitle(all `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* All Parents
logit dissolve i.marr_dur i.`var' i.hh_hours_type_t1 i.`var'#i.hh_hours_type_t1 $controls if children==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(2.hh_hours_type_t1) at(`var'=(1 2 3 4)) post
outreg2 using "$results/scale_exploration_marriage.xls", ctitle(par `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur i.`var' i.division_bucket_hrs_gp_t1 i.`var'#i.division_bucket_hrs_gp_t1 $controls if children==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(division_bucket_hrs_gp_t1) at(`var'=(1 2 3 4)) post
outreg2 using "$results/scale_exploration_marriage.xls", ctitle(par `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Kids under 6
logit dissolve i.marr_dur i.`var' i.hh_hours_type_t1 i.`var'#i.hh_hours_type_t1 $controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(2.hh_hours_type_t1) at(`var'=(1 2 3 4)) post
outreg2 using "$results/scale_exploration_marriage.xls", ctitle(u6 `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur i.`var' i.division_bucket_hrs_gp_t1 i.`var'#i.division_bucket_hrs_gp_t1 $controls if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(division_bucket_hrs_gp_t1) at(`var'=(1 2 3 4)) post
outreg2 using "$results/scale_exploration_marriage.xls", ctitle(u6 `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Childfree couples
logit dissolve i.marr_dur i.`var' i.hh_hours_type_t1 i.`var'#i.hh_hours_type_t1 $controls if children==0 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(2.hh_hours_type_t1) at(`var'=(1 2 3 4)) post
outreg2 using "$results/scale_exploration_marriage.xls", ctitle(cf `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur i.`var' i.division_bucket_hrs_gp_t1 i.`var'#i.division_bucket_hrs_gp_t1 $controls if children==0 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(division_bucket_hrs_gp_t1) at(`var'=(1 2 3 4)) post
outreg2 using "$results/scale_exploration_marriage.xls", ctitle(cf `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
}

*************
* Can't use FE
*************
// state-level attitudes - can't have fixed effects. want to see if effect direction same as regional (then can justify using regional bc time varying)
local scale_vars_nofe "genderroles_egal_state_t avg_egal_state_t"

local nofe "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion i.num_children"  // i.region knot1 knot2 knot3 

foreach var in `scale_vars_nofe'{
	
* Total sample
logit dissolve i.marr_dur c.`var' i.hh_hours_type_t1 c.`var'#i.hh_hours_type_t1 `nofe' if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum `var', detail
margins, dydx(hh_hours_type_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_marriage.xls", ctitle(all `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.`var' i.division_bucket_hrs_t1 c.`var'#i.division_bucket_hrs_t1 `nofe' if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum `var', detail
margins, dydx(division_bucket_hrs_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_marriage.xls", ctitle(all `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* All Parents
logit dissolve i.marr_dur c.`var' i.hh_hours_type_t1 c.`var'#i.hh_hours_type_t1 `nofe' if children==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum `var', detail
margins, dydx(hh_hours_type_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_marriage.xls", ctitle(par `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.`var' i.division_bucket_hrs_t1 c.`var'#i.division_bucket_hrs_t1 `nofe' if children==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum `var', detail
margins, dydx(division_bucket_hrs_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_marriage.xls", ctitle(par `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Kids under 6
logit dissolve i.marr_dur c.`var' i.hh_hours_type_t1 c.`var'#i.hh_hours_type_t1 `nofe' if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum `var', detail
margins, dydx(hh_hours_type_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_marriage.xls", ctitle(u6 `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.`var' i.division_bucket_hrs_t1 c.`var'#i.division_bucket_hrs_t1 `nofe' if children_under6==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum `var', detail
margins, dydx(division_bucket_hrs_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_marriage.xls", ctitle(u6 `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Childfree couples
logit dissolve i.marr_dur c.`var' i.hh_hours_type_t1 c.`var'#i.hh_hours_type_t1 `nofe' if children==0 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum `var', detail
margins, dydx(hh_hours_type_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_marriage.xls", ctitle(cf `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.marr_dur c.`var' i.division_bucket_hrs_t1 c.`var'#i.division_bucket_hrs_t1 `nofe' if children==0 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum `var', detail
margins, dydx(division_bucket_hrs_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_marriage.xls", ctitle(cf `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

}

*************
* Miscellaneous
*************
// this is mostly incorporated above, just leaving for reference

// do I actually need to do childfree SEPARATELY?! let's use the policy liberalism as a test because quite broad (and has moderation effect similar to my original scale)
// because isn't *this* really the test of the effect of parenthood? Like total sample results mask the differences (if there are any)
logit dissolve i.marr_dur c.policy_lib_all i.hh_hours_type_t1 c.policy_lib_all#i.hh_hours_type_t1 $controls if children==1 & hh_hours_type_t1 < 4 & current_rel_type==20 & marr_dur>=0, or
sum policy_lib_all, detail
margins, dydx(hh_hours_type_t1) at(policy_lib_all=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

logit dissolve i.marr_dur c.policy_lib_all i.hh_hours_type_t1 c.policy_lib_all#i.hh_hours_type_t1 $controls if children==0 & hh_hours_type_t1 < 4 & current_rel_type==20 & marr_dur>=0, or
sum policy_lib_all, detail
margins, dydx(hh_hours_type_t1) at(policy_lib_all=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

// just under 6
logit dissolve i.marr_dur c.policy_lib_all i.hh_hours_type_t1 c.policy_lib_all#i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4 & current_rel_type==20 & marr_dur>=0, or
sum policy_lib_all, detail
margins, dydx(hh_hours_type_t1) at(policy_lib_all=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

logit dissolve i.marr_dur c.policy_lib_all i.hh_hours_type_t1 c.policy_lib_all#i.hh_hours_type_t1 $controls if children_under6==0 & hh_hours_type_t1 < 4 & current_rel_type==20 & marr_dur>=0, or
sum policy_lib_all, detail
margins, dydx(hh_hours_type_t1) at(policy_lib_all=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

// is there also a possibility this is an age effect? (This is spurred by Hook and Li 2025). I think I just need to better understand WHY - is it truly the presence of children? Like I think something like this probably needs to go into appendix?
tab AGE_HEAD_ children_under6, row
twoway (histogram AGE_HEAD_ if children_under6==0, width(1) color(pink%30)) (histogram AGE_HEAD_ if children_under6==1, width(1) color(blue%30)), legend(order(1 "No Kids u6" 2 "Kids u6") rows(1) position(6))
twoway (histogram AGE_WIFE_ if children_under6==0, width(1) color(pink%30)) (histogram AGE_WIFE_ if children_under6==1, width(1) color(blue%30)), legend(order(1 "No Kids u6" 2 "Kids u6") rows(1) position(6))

// just those under 40 (this is cutoff Hook and Li use - their goals are different, but actually quite aligns with the distro anyway)
logit dissolve i.marr_dur c.policy_lib_all i.hh_hours_type_t1 c.policy_lib_all#i.hh_hours_type_t1 $controls if AGE_HEAD_<=40 & hh_hours_type_t1 < 4 & current_rel_type==20 & marr_dur>=0, or // somewhat more moderation, but nothing sig.
sum policy_lib_all, detail
margins, dydx(hh_hours_type_t1) at(policy_lib_all=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))	

logit dissolve i.marr_dur c.min_amt_above_fed i.hh_hours_type_t1 c.min_amt_above_fed#i.hh_hours_type_t1 $controls if AGE_HEAD_<=40 & hh_hours_type_t1 < 4 & current_rel_type==20 & marr_dur>=0, or // this one more similar to overall sample, though, not kids
sum min_amt_above_fed, detail
margins, dydx(hh_hours_type_t1) at(min_amt_above_fed=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))	

//// quick exploration of VERY quick bck of the envelope indicator... (v1 = montez policy liberalism; v2 = kim's structural familism)
// married, total sample
logit dissolve i.marr_dur i.policy_group_v1 i.hh_hours_type_t1 i.policy_group_v1#i.hh_hours_type_t1 $controls if hh_hours_type_t1 < 4 & current_rel_type==20 & marr_dur>=0, or
margins policy_group_v1, dydx(hh_hours_type_t1) // male BW is significantly negative in trad / no

logit dissolve i.marr_dur i.policy_group_v2 i.hh_hours_type_t1 i.policy_group_v2#i.hh_hours_type_t1 $controls if hh_hours_type_t1 < 4 & current_rel_type==20 & marr_dur>=0, or
margins policy_group_v2, dydx(hh_hours_type_t1) // see here, it is diff - male BW sig negative with trad attitudes, regardless of policy (but only marginally sig)

// married, young kids
logit dissolve i.marr_dur i.policy_group_v1 i.hh_hours_type_t1 i.policy_group_v1#i.hh_hours_type_t1 $controls if children_under6==1 &  hh_hours_type_t1 < 4 & current_rel_type==20 & marr_dur>=0, or
margins policy_group_v1, dydx(hh_hours_type_t1) // effects even stronger here

logit dissolve i.marr_dur i.policy_group_v2 i.hh_hours_type_t1 i.policy_group_v2#i.hh_hours_type_t1 $controls if children_under6==1 &  hh_hours_type_t1 < 4 & current_rel_type==20 & marr_dur>=0, or
margins policy_group_v2, dydx(hh_hours_type_t1) // okay - only the trad, no sig here (trad but supportive is neg but not sig, egal no is also neg but not sig)

// cohab, total sample
logit dissolve i.coh_dur i.policy_group_v1 i.hh_hours_type_t1 i.policy_group_v1#i.hh_hours_type_t1 $controls if hh_hours_type_t1 < 4 & current_rel_type==22, or
margins policy_group_v1, dydx(hh_hours_type_t1) // opposite findings - male BW always positively associated with divorce, but only sig when egal supportive

logit dissolve i.coh_dur i.policy_group_v2 i.hh_hours_type_t1 i.policy_group_v2#i.hh_hours_type_t1 $controls if hh_hours_type_t1 < 4 & current_rel_type==22, or
margins policy_group_v2, dydx(hh_hours_type_t1) // nothing sig here (but male BW = always positive)

// cohab, young kids
logit dissolve i.coh_dur i.policy_group_v1 i.hh_hours_type_t1 i.policy_group_v1#i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4 & current_rel_type==22, or
margins policy_group_v1, dydx(hh_hours_type_t1) // okay here male BW not always positive. I think we're running into sample bc trad no v. negative but not sig (only 796 observations totalS)

logit dissolve i.coh_dur i.policy_group_v2 i.hh_hours_type_t1 i.policy_group_v2#i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4 & current_rel_type==22, or
margins policy_group_v2, dydx(hh_hours_type_t1) // same here (but confusing things happening with egal + support here

********************************************************************************
**# Cohabiting Couples
********************************************************************************

*************
* Continuous
*************
local scale_vars_cont "structural_familism_t structural_factor_t policy_lib_all_t prek_enrolled_public_t min_amt_above_fed_t earn_ratio_t unemployment_percap_t welfare_all_t cc_pct_income_orig_t cc_pct_served_t headstart_pct_t earlyhs_pct_t total_headstart_pct_t educ_spend_percap_t genderroles_egal_reg_t avg_egal_reg_t married_women_emp_rate_wt_t married_pure_male_bw_rate_t evang_lds_rate_t diffusion_t"

foreach var in `scale_vars_cont'{
	
* Total sample
logit dissolve i.coh_dur c.`var' i.hh_hours_type_t1 c.`var'#i.hh_hours_type_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum `var', detail
margins, dydx(hh_hours_type_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_cohab.xls", ctitle(all `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.coh_dur c.`var' i.division_bucket_hrs_t1 c.`var'#i.division_bucket_hrs_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum `var', detail
margins, dydx(division_bucket_hrs_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_cohab.xls", ctitle(all `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* All Parents
logit dissolve i.coh_dur c.`var' i.hh_hours_type_t1 c.`var'#i.hh_hours_type_t1 $controls if children==1 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum `var', detail
margins, dydx(hh_hours_type_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_cohab.xls", ctitle(par `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.coh_dur c.`var' i.division_bucket_hrs_t1 c.`var'#i.division_bucket_hrs_t1 $controls if children==1 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum `var', detail
margins, dydx(division_bucket_hrs_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_cohab.xls", ctitle(par `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Kids under 6
logit dissolve i.coh_dur c.`var' i.hh_hours_type_t1 c.`var'#i.hh_hours_type_t1 $controls if children_under6==1 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum `var', detail
margins, dydx(hh_hours_type_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_cohab.xls", ctitle(u6 `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.coh_dur c.`var' i.division_bucket_hrs_t1 c.`var'#i.division_bucket_hrs_t1 $controls if children_under6==1 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum `var', detail
margins, dydx(division_bucket_hrs_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_cohab.xls", ctitle(u6 `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

*Childfree couples
logit dissolve i.coh_dur c.`var' i.hh_hours_type_t1 c.`var'#i.hh_hours_type_t1 $controls if children==0 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum `var', detail
margins, dydx(hh_hours_type_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_cohab.xls", ctitle(cf `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.coh_dur c.`var' i.division_bucket_hrs_t1 c.`var'#i.division_bucket_hrs_t1 $controls if children==0 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum `var', detail
margins, dydx(division_bucket_hrs_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_cohab.xls", ctitle(cf `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

}

*************
* Binary
*************
local scale_vars_binary "paid_leave_t abortion_protected_t"

foreach var in `scale_vars_binary'{
	
* Total sample
logit dissolve i.coh_dur i.`var' i.hh_hours_type_t1 i.`var'#i.hh_hours_type_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(2.hh_hours_type_t1 3.hh_hours_type_t1) at(`var'=(0 1)) post
outreg2 using "$results/scale_exploration_cohab.xls", ctitle(all `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.coh_dur i.`var' i.division_bucket_hrs_gp_t1 i.`var'#i.division_bucket_hrs_gp_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(division_bucket_hrs_gp_t1) at(`var'=(0 1)) post
outreg2 using "$results/scale_exploration_cohab.xls", ctitle(all `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* All Parents
logit dissolve i.coh_dur i.`var' i.hh_hours_type_t1 i.`var'#i.hh_hours_type_t1 $controls if children==1 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(2.hh_hours_type_t1) at(`var'=(0 1)) post // 3.hh_hours_type_t1
outreg2 using "$results/scale_exploration_cohab.xls", ctitle(par `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.coh_dur i.`var' i.division_bucket_hrs_gp_t1 i.`var'#i.division_bucket_hrs_gp_t1 $controls if children==1 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(division_bucket_hrs_gp_t1) at(`var'=(0 1)) post
outreg2 using "$results/scale_exploration_cohab.xls", ctitle(par `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Kids under 6
logit dissolve i.coh_dur i.`var' i.hh_hours_type_t1 i.`var'#i.hh_hours_type_t1 $controls if children_under6==1 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(2.hh_hours_type_t1) at(`var'=(0 1)) post // 3.hh_hours_type_t1
outreg2 using "$results/scale_exploration_cohab.xls", ctitle(u6 `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.coh_dur i.`var' i.division_bucket_hrs_gp_t1 i.`var'#i.division_bucket_hrs_gp_t1 $controls if children_under6==1 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(division_bucket_hrs_gp_t1) at(`var'=(0 1)) post
outreg2 using "$results/scale_exploration_cohab.xls", ctitle(u6 `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Childfree couples
logit dissolve i.coh_dur i.`var' i.hh_hours_type_t1 i.`var'#i.hh_hours_type_t1 $controls if children==0 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(2.hh_hours_type_t1) at(`var'=(0 1)) post // 3.hh_hours_type_t1
outreg2 using "$results/scale_exploration_cohab.xls", ctitle(cf `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.coh_dur i.`var' i.division_bucket_hrs_gp_t1 i.`var'#i.division_bucket_hrs_gp_t1 $controls if children==0 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(division_bucket_hrs_gp_t1) at(`var'=(0 1)) post
outreg2 using "$results/scale_exploration_cohab.xls", ctitle(cf `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
}

*************
* Categorical
*************
local scale_vars_cat "policy_group_v1_t  policy_group_v2_t"

foreach var in `scale_vars_cat'{
	
* Total sample
logit dissolve i.coh_dur i.`var' i.hh_hours_type_t1 i.`var'#i.hh_hours_type_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(hh_hours_type_t1) at(`var'=(1 2 3 4)) post
outreg2 using "$results/scale_exploration_cohab.xls", ctitle(all `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.coh_dur i.`var' i.division_bucket_hrs_gp_t1 i.`var'#i.division_bucket_hrs_gp_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(division_bucket_hrs_gp_t1) at(`var'=(1 2 3 4)) post
outreg2 using "$results/scale_exploration_cohab.xls", ctitle(all `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* All Parents
logit dissolve i.coh_dur i.`var' i.hh_hours_type_t1 i.`var'#i.hh_hours_type_t1 $controls if children==1 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(2.hh_hours_type_t1) at(`var'=(1 2 3 4)) post
outreg2 using "$results/scale_exploration_cohab.xls", ctitle(par `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.coh_dur i.`var' i.division_bucket_hrs_gp_t1 i.`var'#i.division_bucket_hrs_gp_t1 $controls if children==1 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(division_bucket_hrs_gp_t1) at(`var'=(1 2 3 4)) post
outreg2 using "$results/scale_exploration_cohab.xls", ctitle(par `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Kids under 6
logit dissolve i.coh_dur i.`var' i.hh_hours_type_t1 i.`var'#i.hh_hours_type_t1 $controls if children_under6==1 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(2.hh_hours_type_t1) at(`var'=(1 2 3 4)) post
outreg2 using "$results/scale_exploration_cohab.xls", ctitle(u6 `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.coh_dur i.`var' i.division_bucket_hrs_gp_t1 i.`var'#i.division_bucket_hrs_gp_t1 $controls if children_under6==1 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(division_bucket_hrs_gp_t1) at(`var'=(1 2 3 4)) post
outreg2 using "$results/scale_exploration_cohab.xls", ctitle(u6 `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Childfree couples
logit dissolve i.coh_dur i.`var' i.hh_hours_type_t1 i.`var'#i.hh_hours_type_t1 $controls if children==0 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(2.hh_hours_type_t1) at(`var'=(1 2 3 4)) post
outreg2 using "$results/scale_exploration_cohab.xls", ctitle(cf `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.coh_dur i.`var' i.division_bucket_hrs_gp_t1 i.`var'#i.division_bucket_hrs_gp_t1 $controls if children==0 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(division_bucket_hrs_gp_t1) at(`var'=(1 2 3 4)) post
outreg2 using "$results/scale_exploration_cohab.xls", ctitle(cf `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
}

*************
* Can't use FE
*************
// state-level attitudes - can't have fixed effects. want to see if effect direction same as regional (then can justify using regional bc time varying)
local scale_vars_nofe "genderroles_egal_state_t avg_egal_state_t"

local nofe "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion i.num_children"  // i.region knot1 knot2 knot3 

foreach var in `scale_vars_nofe'{
	
* Total sample
logit dissolve i.coh_dur c.`var' i.hh_hours_type_t1 c.`var'#i.hh_hours_type_t1 `nofe' if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum `var', detail
margins, dydx(hh_hours_type_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_cohab.xls", ctitle(all `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.coh_dur c.`var' i.division_bucket_hrs_t1 c.`var'#i.division_bucket_hrs_t1 `nofe' if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum `var', detail
margins, dydx(division_bucket_hrs_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_cohab.xls", ctitle(all `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* All Parents
logit dissolve i.coh_dur c.`var' i.hh_hours_type_t1 c.`var'#i.hh_hours_type_t1 `nofe' if children==1 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum `var', detail
margins, dydx(hh_hours_type_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_cohab.xls", ctitle(par `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.coh_dur c.`var' i.division_bucket_hrs_t1 c.`var'#i.division_bucket_hrs_t1 `nofe' if children==1 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum `var', detail
margins, dydx(division_bucket_hrs_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_cohab.xls", ctitle(par `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Kids under 6
logit dissolve i.coh_dur c.`var' i.hh_hours_type_t1 c.`var'#i.hh_hours_type_t1 `nofe' if children_under6==1 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum `var', detail
margins, dydx(hh_hours_type_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_cohab.xls", ctitle(u6 `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.coh_dur c.`var' i.division_bucket_hrs_t1 c.`var'#i.division_bucket_hrs_t1 `nofe' if children_under6==1 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum `var', detail
margins, dydx(division_bucket_hrs_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_cohab.xls", ctitle(u6 `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Childfree couples
logit dissolve i.coh_dur c.`var' i.hh_hours_type_t1 c.`var'#i.hh_hours_type_t1 `nofe' if children==0 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum `var', detail
margins, dydx(hh_hours_type_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_cohab.xls", ctitle(cf `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.coh_dur c.`var' i.division_bucket_hrs_t1 c.`var'#i.division_bucket_hrs_t1 `nofe' if children==0 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum `var', detail
margins, dydx(division_bucket_hrs_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_cohab.xls", ctitle(cf `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

}

********************************************************************************
**# Combined Married + Cohab
********************************************************************************
// Note (8/22/25): forgot to exclude potentially left-censored cohabitations the first time. I have not yet rerun these results (instead focusing on figures below) - so married is done and cleaned; cohab is repulled but not cleaned / organized; this is not yet repulled

// should control for relationship type, so need a new set of controls

global combo_controls "i.current_rel_type age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion i.num_children" // cohab_with_partner cohab_with_other // these causing problems right now when combined

*************
* Continuous
*************

local scale_vars_cont "structural_familism_t structural_factor_t policy_lib_all_t prek_enrolled_public_t min_amt_above_fed_t earn_ratio_t unemployment_percap_t welfare_all_t cc_pct_income_orig_t cc_pct_served_t headstart_pct_t earlyhs_pct_t total_headstart_pct_t educ_spend_percap_t genderroles_egal_reg_t avg_egal_reg_t married_women_emp_rate_wt_t married_pure_male_bw_rate_t evang_lds_rate_t diffusion_t"

foreach var in `scale_vars_cont'{
	
* Total sample
logit dissolve i.combined_dur c.`var' i.hh_hours_type_t1 c.`var'#i.hh_hours_type_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum `var', detail
margins, dydx(hh_hours_type_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_combined.xls", ctitle(all `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.combined_dur c.`var' i.division_bucket_hrs_t1 c.`var'#i.division_bucket_hrs_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum `var', detail
margins, dydx(division_bucket_hrs_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_combined.xls", ctitle(all `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* All Parents
logit dissolve i.combined_dur c.`var' i.hh_hours_type_t1 c.`var'#i.hh_hours_type_t1 $combo_controls if children==1 & combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum `var', detail
margins, dydx(hh_hours_type_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_combined.xls", ctitle(par `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.combined_dur c.`var' i.division_bucket_hrs_t1 c.`var'#i.division_bucket_hrs_t1 $combo_controls if children==1 & combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum `var', detail
margins, dydx(division_bucket_hrs_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_combined.xls", ctitle(par `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Kids under 6
logit dissolve i.combined_dur c.`var' i.hh_hours_type_t1 c.`var'#i.hh_hours_type_t1 $combo_controls if children_under6==1 & combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum `var', detail
margins, dydx(hh_hours_type_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_combined.xls", ctitle(u6 `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.combined_dur c.`var' i.division_bucket_hrs_t1 c.`var'#i.division_bucket_hrs_t1 $combo_controls if children_under6==1 & combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum `var', detail
margins, dydx(division_bucket_hrs_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_combined.xls", ctitle(u6 `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Childfree couples
logit dissolve i.combined_dur c.`var' i.hh_hours_type_t1 c.`var'#i.hh_hours_type_t1 $combo_controls if children==0 & combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum `var', detail
margins, dydx(hh_hours_type_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_combined.xls", ctitle(cf `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.combined_dur c.`var' i.division_bucket_hrs_t1 c.`var'#i.division_bucket_hrs_t1 $combo_controls if children==0 & combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum `var', detail
margins, dydx(division_bucket_hrs_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_combined.xls", ctitle(cf `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

}

*************
* Binary
*************
// for the binary variables - I cannot get counter-traditional to estimate on its own. In some cases, I also cannot get all others to estimate on own. Using this new variable: division_bucket_hrs_gp_t1 - where counter-trad is grouped into all other

local scale_vars_binary "paid_leave_t abortion_protected_t"

foreach var in `scale_vars_binary'{
	
* Total sample
logit dissolve i.combined_dur i.`var' i.hh_hours_type_t1 i.`var'#i.hh_hours_type_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(2.hh_hours_type_t1) at(`var'=(0 1)) post // 3.hh_hours_type_t1
outreg2 using "$results/scale_exploration_combined.xls", ctitle(all `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.combined_dur i.`var' i.division_bucket_hrs_gp_t1 i.`var'#i.division_bucket_hrs_gp_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(division_bucket_hrs_gp_t1) at(`var'=(0 1)) post
outreg2 using "$results/scale_exploration_combined.xls", ctitle(all `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* All Parents
logit dissolve i.combined_dur i.`var' i.hh_hours_type_t1 i.`var'#i.hh_hours_type_t1 $combo_controls if children==1 & combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(2.hh_hours_type_t1) at(`var'=(0 1)) post //  3.hh_hours_type_t1
outreg2 using "$results/scale_exploration_combined.xls", ctitle(par `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.combined_dur i.`var' i.division_bucket_hrs_gp_t1 i.`var'#i.division_bucket_hrs_gp_t1 $combo_controls if children==1 & combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(division_bucket_hrs_gp_t1) at(`var'=(0 1)) post
outreg2 using "$results/scale_exploration_combined.xls", ctitle(par `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Kids under 6
logit dissolve i.combined_dur i.`var' i.hh_hours_type_t1 i.`var'#i.hh_hours_type_t1 $combo_controls if children_under6==1 & combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(2.hh_hours_type_t1) at(`var'=(0 1)) post // 3.hh_hours_type_t1
outreg2 using "$results/scale_exploration_combined.xls", ctitle(u6 `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.combined_dur i.`var' i.division_bucket_hrs_gp_t1 i.`var'#i.division_bucket_hrs_gp_t1 $combo_controls if children_under6==1 & combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(division_bucket_hrs_gp_t1) at(`var'=(0 1)) post
outreg2 using "$results/scale_exploration_combined.xls", ctitle(u6 `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Childfree couples
logit dissolve i.combined_dur i.`var' i.hh_hours_type_t1 i.`var'#i.hh_hours_type_t1 $combo_controls if children==0 & combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(2.hh_hours_type_t1) at(`var'=(0 1)) post // 3.hh_hours_type_t1
outreg2 using "$results/scale_exploration_combined.xls", ctitle(cf `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.combined_dur i.`var' i.division_bucket_hrs_gp_t1 i.`var'#i.division_bucket_hrs_gp_t1 $combo_controls if children==0 & combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(division_bucket_hrs_gp_t1) at(`var'=(0 1)) post
outreg2 using "$results/scale_exploration_combined.xls", ctitle(cf `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
}

*************
* Categorical
*************
// here also need to combine 3 and 5

local scale_vars_cat "policy_group_v1_t  policy_group_v2_t"

foreach var in `scale_vars_cat'{
	
* Total sample
logit dissolve i.combined_dur i.`var' i.hh_hours_type_t1 i.`var'#i.hh_hours_type_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(hh_hours_type_t1) at(`var'=(1 2 3 4)) post
outreg2 using "$results/scale_exploration_combined.xls", ctitle(all `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.combined_dur i.`var' i.division_bucket_hrs_gp_t1 i.`var'#i.division_bucket_hrs_gp_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(division_bucket_hrs_gp_t1) at(`var'=(1 2 3 4)) post
outreg2 using "$results/scale_exploration_combined.xls", ctitle(all `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* All Parents
logit dissolve i.combined_dur i.`var' i.hh_hours_type_t1 i.`var'#i.hh_hours_type_t1 $combo_controls if children==1 & combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(2.hh_hours_type_t1) at(`var'=(1 2 3 4)) post
outreg2 using "$results/scale_exploration_combined.xls", ctitle(par `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.combined_dur i.`var' i.division_bucket_hrs_gp_t1 i.`var'#i.division_bucket_hrs_gp_t1 $combo_controls if children==1 & combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(division_bucket_hrs_gp_t1) at(`var'=(1 2 3 4)) post
outreg2 using "$results/scale_exploration_combined.xls", ctitle(par `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Kids under 6
logit dissolve i.combined_dur i.`var' i.hh_hours_type_t1 i.`var'#i.hh_hours_type_t1 $combo_controls if children_under6==1 & combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(2.hh_hours_type_t1) at(`var'=(1 2 3 4)) post
outreg2 using "$results/scale_exploration_combined.xls", ctitle(u6 `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.combined_dur i.`var' i.division_bucket_hrs_gp_t1 i.`var'#i.division_bucket_hrs_gp_t1 $combo_controls if children_under6==1 & combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(division_bucket_hrs_gp_t1) at(`var'=(1 2 3 4)) post
outreg2 using "$results/scale_exploration_combined.xls", ctitle(u6 `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Childfree couples
logit dissolve i.combined_dur i.`var' i.hh_hours_type_t1 i.`var'#i.hh_hours_type_t1 $combo_controls if children==0 & combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(2.hh_hours_type_t1) at(`var'=(1 2 3 4)) post
outreg2 using "$results/scale_exploration_combined.xls", ctitle(cf `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.combined_dur i.`var' i.division_bucket_hrs_gp_t1 i.`var'#i.division_bucket_hrs_gp_t1 $combo_controls if children==0 & combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(division_bucket_hrs_gp_t1) at(`var'=(1 2 3 4)) post
outreg2 using "$results/scale_exploration_combined.xls", ctitle(cf `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
}


*************
* Can't use FE
*************
// state-level attitudes - can't have fixed effects. want to see if effect direction same as regional (then can justify using regional bc time varying)
local scale_vars_nofe "genderroles_egal_state_t avg_egal_state_t"

local nofe "i.current_rel_type age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion i.num_children"  // i.region knot1 knot2 knot3 

foreach var in `scale_vars_nofe'{
	
* Total sample
logit dissolve i.combined_dur c.`var' i.hh_hours_type_t1 c.`var'#i.hh_hours_type_t1 `nofe' if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum `var', detail
margins, dydx(hh_hours_type_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_combined.xls", ctitle(all `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.combined_dur c.`var' i.division_bucket_hrs_t1 c.`var'#i.division_bucket_hrs_t1 `nofe' if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum `var', detail
margins, dydx(division_bucket_hrs_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_combined.xls", ctitle(all `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* All Parents
logit dissolve i.combined_dur c.`var' i.hh_hours_type_t1 c.`var'#i.hh_hours_type_t1 `nofe' if children==1 & combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum `var', detail
margins, dydx(hh_hours_type_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_combined.xls", ctitle(par `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.combined_dur c.`var' i.division_bucket_hrs_t1 c.`var'#i.division_bucket_hrs_t1 `nofe' if children==1 & combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum `var', detail
margins, dydx(division_bucket_hrs_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_combined.xls", ctitle(par `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Kids under 6
logit dissolve i.combined_dur c.`var' i.hh_hours_type_t1 c.`var'#i.hh_hours_type_t1 `nofe' if children_under6==1 & combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum `var', detail
margins, dydx(hh_hours_type_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_combined.xls", ctitle(u6 `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.combined_dur c.`var' i.division_bucket_hrs_t1 c.`var'#i.division_bucket_hrs_t1 `nofe' if children_under6==1 & combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum `var', detail
margins, dydx(division_bucket_hrs_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_combined.xls", ctitle(u6 `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Childfree couples
logit dissolve i.combined_dur c.`var' i.hh_hours_type_t1 c.`var'#i.hh_hours_type_t1 `nofe' if children==0 & combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum `var', detail
margins, dydx(hh_hours_type_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_combined.xls", ctitle(cf `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve i.combined_dur c.`var' i.division_bucket_hrs_t1 c.`var'#i.division_bucket_hrs_t1 `nofe' if children==0 & combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum `var', detail
margins, dydx(division_bucket_hrs_t1) at(`var'=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/scale_exploration_combined.xls", ctitle(cf `var') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

}

////////////////////////////////////////////////////////////////////////////////
**# // Now pulling some figures for each of my options
////////////////////////////////////////////////////////////////////////////////

********************************************************************************
* Basic descriptives
********************************************************************************

// sample sizes for the different models
* Total sample
tab current_rel_type if  any_missing==0 & no_labor==0 & cohab_flag==0
	// married
	unique unique_id partner_unique_id if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0
	unique unique_id partner_unique_id if dissolve==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0
	
	// cohab
	unique unique_id partner_unique_id if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0
	unique unique_id partner_unique_id if dissolve==1 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0
	
	// combined
	unique unique_id partner_unique_id if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0
	unique unique_id partner_unique_id if dissolve==1 & combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0
	
* All Parents
tab current_rel_type if  any_missing==0 & no_labor==0 & cohab_flag==0 & children==1
	unique unique_id partner_unique_id if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & children==1
	unique unique_id partner_unique_id if dissolve==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & children==1
	
	// cohab
	unique unique_id partner_unique_id if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0 & children==1
	unique unique_id partner_unique_id if dissolve==1 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0  & children==1
	
	// combined
	unique unique_id partner_unique_id if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0  & children==1
	unique unique_id partner_unique_id if dissolve==1 & combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0  & children==1
	
* Parents of kids under 6
tab current_rel_type if  any_missing==0 & no_labor==0 & cohab_flag==0 & children_under6==1
	unique unique_id partner_unique_id if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & children_under6==1
	unique unique_id partner_unique_id if dissolve==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & children_under6==1
	
	// cohab
	unique unique_id partner_unique_id if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0 & children_under6==1
	unique unique_id partner_unique_id if dissolve==1 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0  & children_under6==1
	
	// combined
	unique unique_id partner_unique_id if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0  & children_under6==1
	unique unique_id partner_unique_id if dissolve==1 & combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0  & children_under6==1
	
* Childfree couples
tab current_rel_type if  any_missing==0 & no_labor==0 & cohab_flag==0 & children==0
	unique unique_id partner_unique_id if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & children==0
	unique unique_id partner_unique_id if dissolve==1 & current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & children==0
	
	// cohab
	unique unique_id partner_unique_id if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0 & children==0
	unique unique_id partner_unique_id if dissolve==1 & current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0  & children==0
	
	// combined
	unique unique_id partner_unique_id if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0  & children==0
	unique unique_id partner_unique_id if dissolve==1 & combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0  & children==0
	
// distribution across states
tab state_fips current_rel_type if any_missing==0 & no_labor==0 & cohab_flag==0, col
tab state_fips current_rel_type if any_missing==0 & no_labor==0 & cohab_flag==0, col nofreq
tab state_fips current_rel_type if any_missing==0 & no_labor==0 & cohab_flag==0, row nofreq

********************************************************************************
**# Option 1: Diffusion v. Policy
********************************************************************************
/*
Base models (from above) - for total sample only
// Married
logit dissolve i.marr_dur c.`var' i.hh_hours_type_t1 c.`var'#i.hh_hours_type_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or

logit dissolve i.marr_dur c.`var' i.division_bucket_hrs_t1 c.`var'#i.division_bucket_hrs_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or

// Cohab
logit dissolve i.coh_dur c.`var' i.hh_hours_type_t1 c.`var'#i.hh_hours_type_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or

logit dissolve i.coh_dur c.`var' i.division_bucket_hrs_t1 c.`var'#i.division_bucket_hrs_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or

// Combined
logit dissolve i.combined_dur c.`var' i.hh_hours_type_t1 c.`var'#i.hh_hours_type_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or

logit dissolve i.combined_dur c.`var' i.division_bucket_hrs_t1 c.`var'#i.division_bucket_hrs_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
*/

**************
* Married Couples
**************
** Structural Familism
// Paid Labor
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_m2

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(3.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_m3

coefplot (est_m2, mcolor(navy) ciopts(color(navy)) label("Male BW")) (est_m3, label("Female BW")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical))

// Combined Division of Labor
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_m2a

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(3.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_m3a

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(4.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_m4a

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(5.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_m5a

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins division_bucket_hrs_t1, at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) 

coefplot (est_m2a, mcolor(blue) ciopts(color(blue)) label("Traditional")) (est_m3a, label("Counter-Traditional")) (est_m4a, label("Second Shift"))  (est_m5a, label("All Others")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical)) xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) legend(position(bottom) rows(1))

** Diffusion of Gender Revolution
// Paid Labor
logit dissolve i.marr_dur c.diffusion_t i.hh_hours_type_t1 c.diffusion_t#i.hh_hours_type_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum diffusion_t, detail
margins, dydx(2.hh_hours_type_t1) at(diffusion_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_m2b

logit dissolve i.marr_dur c.diffusion_t i.hh_hours_type_t1 c.diffusion_t#i.hh_hours_type_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum diffusion_t, detail
margins, dydx(3.hh_hours_type_t1) at(diffusion_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_m3b

coefplot (est_m2b, mcolor(navy) ciopts(color(navy)) label("Male BW")) (est_m3b, label("Female BW")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) groups(?._at = "{bf:Diffusion of Gender Revolution: Percentiles}", angle(vertical))

// Combined Division of Labor
logit dissolve i.marr_dur c.diffusion_t i.division_bucket_hrs_t1 c.diffusion_t#i.division_bucket_hrs_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum diffusion_t, detail
margins, dydx(2.division_bucket_hrs_t1) at(diffusion_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_m2c

logit dissolve i.marr_dur c.diffusion_t i.division_bucket_hrs_t1 c.diffusion_t#i.division_bucket_hrs_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum diffusion_t, detail
margins, dydx(3.division_bucket_hrs_t1) at(diffusion_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_m3c

logit dissolve i.marr_dur c.diffusion_t i.division_bucket_hrs_t1 c.diffusion_t#i.division_bucket_hrs_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum diffusion_t, detail
margins, dydx(4.division_bucket_hrs_t1) at(diffusion_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_m4c

logit dissolve i.marr_dur c.diffusion_t i.division_bucket_hrs_t1 c.diffusion_t#i.division_bucket_hrs_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum diffusion_t, detail
margins, dydx(5.division_bucket_hrs_t1) at(diffusion_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_m5c

logit dissolve i.marr_dur c.diffusion_t i.division_bucket_hrs_t1 c.diffusion_t#i.division_bucket_hrs_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum diffusion_t, detail
margins division_bucket_hrs_t1, at(diffusion_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) 

coefplot (est_m2c, mcolor(blue) ciopts(color(blue)) label("Traditional")) (est_m3c, label("Counter-Traditional")) (est_m4c, label("Second Shift"))  (est_m5c, label("All Others")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") groups(?._at = "{bf:Diffusion of Gender Revolution: Percentiles}", angle(vertical)) xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) legend(position(bottom) rows(1))

** JUST attitudes
// Paid Labor
logit dissolve i.marr_dur c.avg_egal_reg_t i.hh_hours_type_t1 c.avg_egal_reg_t#i.hh_hours_type_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum avg_egal_reg_t, detail
margins, dydx(2.hh_hours_type_t1) at(avg_egal_reg_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_m2d

logit dissolve i.marr_dur c.avg_egal_reg_t i.hh_hours_type_t1 c.avg_egal_reg_t#i.hh_hours_type_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum avg_egal_reg_t, detail
margins, dydx(3.hh_hours_type_t1) at(avg_egal_reg_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_m3d

coefplot (est_m2d, mcolor(navy) ciopts(color(navy)) label("Male BW")) (est_m3d, label("Female BW")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) groups(?._at = "{bf:% of Population with Egal Attitudes: Percentiles}", angle(vertical))

// Combined Division of Labor
logit dissolve i.marr_dur c.avg_egal_reg_t i.division_bucket_hrs_t1 c.avg_egal_reg_t#i.division_bucket_hrs_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum avg_egal_reg_t, detail
margins, dydx(2.division_bucket_hrs_t1) at(avg_egal_reg_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_m2e

logit dissolve i.marr_dur c.avg_egal_reg_t i.division_bucket_hrs_t1 c.avg_egal_reg_t#i.division_bucket_hrs_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum avg_egal_reg_t, detail
margins, dydx(3.division_bucket_hrs_t1) at(avg_egal_reg_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_m3e

logit dissolve i.marr_dur c.avg_egal_reg_t i.division_bucket_hrs_t1 c.avg_egal_reg_t#i.division_bucket_hrs_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum avg_egal_reg_t, detail
margins, dydx(4.division_bucket_hrs_t1) at(avg_egal_reg_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_m4e

logit dissolve i.marr_dur c.avg_egal_reg_t i.division_bucket_hrs_t1 c.avg_egal_reg_t#i.division_bucket_hrs_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum avg_egal_reg_t, detail
margins, dydx(5.division_bucket_hrs_t1) at(avg_egal_reg_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_m5e

logit dissolve i.marr_dur c.avg_egal_reg_t i.division_bucket_hrs_t1 c.avg_egal_reg_t#i.division_bucket_hrs_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum avg_egal_reg_t, detail
margins division_bucket_hrs_t1, at(avg_egal_reg_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) 

coefplot (est_m2e, mcolor(blue) ciopts(color(blue)) label("Traditional")) (est_m3e, label("Counter-Traditional")) (est_m4e, label("Second Shift"))  (est_m5e, label("All Others")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") groups(?._at = "{bf:% of Population with Egal Attitudes: Percentiles}", angle(vertical)) xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) legend(position(bottom) rows(1))

** 2x2 indicator (I guess use structural familism if that is what is above - but then wait, I might be using a diff diffusion indicator oops - okay updated this to be the diffusion x structural
// Paid Labor
logit dissolve i.marr_dur i.policy_group_v3_t i.hh_hours_type_t1 i.policy_group_v3_t#i.hh_hours_type_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
// margins, dydx(hh_hours_type_t1) at(policy_group_v3_t=(1 2 3 4)) 
margins, dydx(2.hh_hours_type_t1) at(policy_group_v3_t=(1 2 3 4)) post
estimates store est_m2f

logit dissolve i.marr_dur i.policy_group_v3_t i.hh_hours_type_t1 i.policy_group_v3_t#i.hh_hours_type_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(3.hh_hours_type_t1) at(policy_group_v3_t=(1 2 3 4)) post
estimates store est_m3f

coefplot (est_m2f, mcolor(navy) ciopts(color(navy)) label("Male BW")) (est_m3f, label("Female BW")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) coeflabels(1._at = "Egal, Support" 2._at = "Egal, No Support" 3._at = "Trad, Support" 4._at = "Trad, No Support") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) groups(?._at = "{bf: Policy x Diffusion}", angle(vertical))

// Combined DoL
logit dissolve i.marr_dur i.policy_group_v3_t i.division_bucket_hrs_gp_t1 i.policy_group_v3_t#i.division_bucket_hrs_gp_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
// margins, dydx(division_bucket_hrs_gp_t1) at(policy_group_v3_t=(1 2 3 4))
margins, dydx(2.division_bucket_hrs_gp_t1) at(policy_group_v3_t=(1 2 3 4)) level(90) post
estimates store est_m2g

logit dissolve i.marr_dur i.policy_group_v3_t i.division_bucket_hrs_gp_t1 i.policy_group_v3_t#i.division_bucket_hrs_gp_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(3.division_bucket_hrs_gp_t1) at(policy_group_v3_t=(1 2 3 4)) level(90) post
estimates store est_m3g

logit dissolve i.marr_dur i.policy_group_v3_t i.division_bucket_hrs_gp_t1 i.policy_group_v3_t#i.division_bucket_hrs_gp_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
margins, dydx(4.division_bucket_hrs_gp_t1) at(policy_group_v3_t=(1 2 3 4)) level(90) post
estimates store est_m4g

coefplot (est_m2g, mcolor(blue) ciopts(color(blue)) label("Traditional")) (est_m3g,  mcolor(gray) ciopts(color(gray)) label("Second Shift"))  (est_m4g, mcolor(black) ciopts(color(black)) label("All Others")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) coeflabels(1._at = "Egal, Support" 2._at = "Egal, No Support" 3._at = "Trad, Support" 4._at = "Trad, No Support") groups(?._at = "{bf:Policy x Diffusion}", angle(vertical)) xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) legend(position(bottom) rows(1))

// Alt Figures - across metrics
coefplot (est_m2, lcolor("navy") mcolor("navy") ciopts(color("navy")) label("Male BW")) (est_m3, lcolor("eltblue") mcolor("eltblue") ciopts(color("eltblue")) label("Female BW")) , bylabel("Structural Support")  || ///
		(est_m2b) (est_m3b) , bylabel("Diffusion of Gender Revolution"), ///
, drop(_cons) xline(0, lcolor(red)) levels(95) base xtitle(Average Marginal Effects Relative to Dual Earning, size(small)) ///
coeflabels(1._at = "5th Pct" 2._at = "25th Pct" 3._at = "50th Pct" 4._at = "75th Pct" 5._at = "95th Pct") ///
legend(position(bottom) rows(1)) groups(?._at = "{bf:Scale Value}", angle(vertical)) 

coefplot (est_m2a, lcolor("blue") mcolor("blue") ciopts(color("blue")) label("Traditional")) (est_m3a, lcolor("eltblue") mcolor("eltblue") ciopts(color("eltblue")) label("Counter-Trad")) (est_m4a, lcolor("gs8") mcolor("gs8") ciopts(color("gs8")) label("Second Shift")) (est_m5a, lcolor("black") mcolor("black") ciopts(color("black")) label("All Others")), bylabel("Structural Support")  || ///
		est_m2c est_m3c est_m4c est_m5c, bylabel("Diffusion of Gender Revolution"), ///
, drop(_cons) xline(0, lcolor(red)) levels(95) base xtitle(Average Marginal Effects Relative to Egalitarian, size(small)) ///
coeflabels(1._at = "5th Pct" 2._at = "25th Pct" 3._at = "50th Pct" 4._at = "75th Pct" 5._at = "95th Pct") ///
legend(position(bottom) rows(1)) groups(?._at = "{bf:Scale Value}", angle(vertical)) 

**************
* Cohabiting Couples
**************
** Structural Familism
// Paid Labor
logit dissolve i.coh_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum structural_familism_t, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_c2

logit dissolve i.coh_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum structural_familism_t, detail
margins, dydx(3.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_c3

coefplot (est_c2, mcolor(navy) ciopts(color(navy)) label("Male BW")) (est_c3, label("Female BW")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical))

// Combined Division of Labor
logit dissolve i.coh_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum structural_familism_t, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_c2a

logit dissolve i.coh_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum structural_familism_t, detail
margins, dydx(3.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_c3a

logit dissolve i.coh_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum structural_familism_t, detail
margins, dydx(4.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_c4a

logit dissolve i.coh_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum structural_familism_t, detail
margins, dydx(5.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_c5a

logit dissolve i.coh_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum structural_familism_t, detail
margins division_bucket_hrs_t1, at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) 

coefplot (est_c2a, mcolor(blue) ciopts(color(blue)) label("Traditional")) (est_c3a, label("Counter-Traditional")) (est_c4a, label("Second Shift"))  (est_c5a, label("All Others")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical)) xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) legend(position(bottom) rows(1))

** Diffusion of Gender Revolution
// Paid Labor
logit dissolve i.coh_dur c.diffusion_t i.hh_hours_type_t1 c.diffusion_t#i.hh_hours_type_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum diffusion_t, detail
margins, dydx(2.hh_hours_type_t1) at(diffusion_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_c2b

logit dissolve i.coh_dur c.diffusion_t i.hh_hours_type_t1 c.diffusion_t#i.hh_hours_type_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum diffusion_t, detail
margins, dydx(3.hh_hours_type_t1) at(diffusion_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_c3b

coefplot (est_c2b, mcolor(navy) ciopts(color(navy)) label("Male BW")) (est_c3b, label("Female BW")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) groups(?._at = "{bf:Diffusion of Gender Revolution: Percentiles}", angle(vertical))

// Combined Division of Labor
logit dissolve i.coh_dur c.diffusion_t i.division_bucket_hrs_t1 c.diffusion_t#i.division_bucket_hrs_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum diffusion_t, detail
margins, dydx(2.division_bucket_hrs_t1) at(diffusion_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_c2c

logit dissolve i.coh_dur c.diffusion_t i.division_bucket_hrs_t1 c.diffusion_t#i.division_bucket_hrs_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum diffusion_t, detail
margins, dydx(3.division_bucket_hrs_t1) at(diffusion_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_c3c

logit dissolve i.coh_dur c.diffusion_t i.division_bucket_hrs_t1 c.diffusion_t#i.division_bucket_hrs_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum diffusion_t, detail
margins, dydx(4.division_bucket_hrs_t1) at(diffusion_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_c4c

logit dissolve i.coh_dur c.diffusion_t i.division_bucket_hrs_t1 c.diffusion_t#i.division_bucket_hrs_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum diffusion_t, detail
margins, dydx(5.division_bucket_hrs_t1) at(diffusion_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_c5c

logit dissolve i.coh_dur c.diffusion_t i.division_bucket_hrs_t1 c.diffusion_t#i.division_bucket_hrs_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum diffusion_t, detail
margins division_bucket_hrs_t1, at(diffusion_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) 

coefplot (est_c2c, mcolor(blue) ciopts(color(blue)) label("Traditional")) (est_c3c, label("Counter-Traditional")) (est_c4c, label("Second Shift"))  (est_c5c, label("All Others")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") groups(?._at = "{bf:Diffusion of Gender Revolution: Percentiles}", angle(vertical)) xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) legend(position(bottom) rows(1))

** JUST attitudes
// Paid Labor
logit dissolve i.coh_dur c.avg_egal_reg_t i.hh_hours_type_t1 c.avg_egal_reg_t#i.hh_hours_type_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum avg_egal_reg_t, detail
margins, dydx(2.hh_hours_type_t1) at(avg_egal_reg_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_c2d

logit dissolve i.coh_dur c.avg_egal_reg_t i.hh_hours_type_t1 c.avg_egal_reg_t#i.hh_hours_type_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum avg_egal_reg_t, detail
margins, dydx(3.hh_hours_type_t1) at(avg_egal_reg_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_c3d

coefplot (est_c2d, mcolor(navy) ciopts(color(navy)) label("Male BW")) (est_c3d, label("Female BW")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) groups(?._at = "{bf:% of Population with Egal Attitudes: Percentiles}", angle(vertical))

// Combined Division of Labor
logit dissolve i.coh_dur c.avg_egal_reg_t i.division_bucket_hrs_t1 c.avg_egal_reg_t#i.division_bucket_hrs_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum avg_egal_reg_t, detail
margins, dydx(2.division_bucket_hrs_t1) at(avg_egal_reg_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_c2e

logit dissolve i.coh_dur c.avg_egal_reg_t i.division_bucket_hrs_t1 c.avg_egal_reg_t#i.division_bucket_hrs_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum avg_egal_reg_t, detail
margins, dydx(3.division_bucket_hrs_t1) at(avg_egal_reg_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_c3e

logit dissolve i.coh_dur c.avg_egal_reg_t i.division_bucket_hrs_t1 c.avg_egal_reg_t#i.division_bucket_hrs_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum avg_egal_reg_t, detail
margins, dydx(4.division_bucket_hrs_t1) at(avg_egal_reg_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_c4e

logit dissolve i.coh_dur c.avg_egal_reg_t i.division_bucket_hrs_t1 c.avg_egal_reg_t#i.division_bucket_hrs_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum avg_egal_reg_t, detail
margins, dydx(5.division_bucket_hrs_t1) at(avg_egal_reg_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_c5e

logit dissolve i.coh_dur c.avg_egal_reg_t i.division_bucket_hrs_t1 c.avg_egal_reg_t#i.division_bucket_hrs_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum avg_egal_reg_t, detail
margins division_bucket_hrs_t1, at(avg_egal_reg_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) 

coefplot (est_c2e, mcolor(blue) ciopts(color(blue)) label("Traditional")) (est_c3e, label("Counter-Traditional")) (est_c4e, label("Second Shift"))  (est_c5e, label("All Others")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") groups(?._at = "{bf:% of Population with Egal Attitudes: Percentiles}", angle(vertical)) xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) legend(position(bottom) rows(1))

** 2x2 indicator (I guess use structural familism if that is what is above - but then wait, I might be using a diff diffusion indicator oops - okay attitudes v. diffusion- but individual effects nearly identical so I think it is okay
// Paid Labor
logit dissolve i.coh_dur i.policy_group_v3_t i.hh_hours_type_t1 i.policy_group_v3_t#i.hh_hours_type_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
// margins, dydx(hh_hours_type_t1) at(policy_group_v3_t=(1 2 3 4)) 
margins, dydx(2.hh_hours_type_t1) at(policy_group_v3_t=(1 2 3 4)) post
estimates store est_c2f

logit dissolve i.coh_dur i.policy_group_v3_t i.hh_hours_type_t1 i.policy_group_v3_t#i.hh_hours_type_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
// margins, dydx(3.hh_hours_type_t1) at(policy_group_v3_t=(1 3 4)) post // female bw x egal / no is not estimating, so this is not working...
// estimates store est_c3f

coefplot (est_c2f, mcolor(navy) ciopts(color(navy)) label("Male BW")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) coeflabels(1._at = "Egal, Support" 2._at = "Egal, No Support" 3._at = "Trad, Support" 4._at = "Trad, No Support") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) groups(?._at = "{bf: Policy x Diffusion}", angle(vertical)) //  (est_c3f, label("Female BW"))

// Combined DoL
logit dissolve i.coh_dur i.policy_group_v3_t i.division_bucket_hrs_gp_t1 i.policy_group_v3_t#i.division_bucket_hrs_gp_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
// margins, dydx(division_bucket_hrs_gp_t1) at(policy_group_v3_t=(1 2 3 4))
margins, dydx(2.division_bucket_hrs_gp_t1) at(policy_group_v3_t=(1 2 3 4)) level(90) post
estimates store est_c2g

logit dissolve i.coh_dur i.policy_group_v3_t i.division_bucket_hrs_gp_t1 i.policy_group_v3_t#i.division_bucket_hrs_gp_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(3.division_bucket_hrs_gp_t1) at(policy_group_v3_t=(1 2 3 4)) level(90) post
estimates store est_c3g

logit dissolve i.coh_dur i.policy_group_v3_t i.division_bucket_hrs_gp_t1 i.policy_group_v3_t#i.division_bucket_hrs_gp_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(4.division_bucket_hrs_gp_t1) at(policy_group_v3_t=(1 2 3 4)) level(90) post
estimates store est_c4g

coefplot (est_c2g, mcolor(blue) ciopts(color(blue)) label("Traditional")) (est_c3g,  mcolor(gray) ciopts(color(gray)) label("Second Shift"))  (est_c4g, mcolor(black) ciopts(color(black)) label("All Others")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) coeflabels(1._at = "Egal, Support" 2._at = "Egal, No Support" 3._at = "Trad, Support" 4._at = "Trad, No Support") groups(?._at = "{bf:Policy x Diffusion}", angle(vertical)) xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) legend(position(bottom) rows(1))

// Alt Figures - across metrics
coefplot (est_c2, lcolor("navy") mcolor("navy") ciopts(color("navy")) label("Male BW")) (est_c3, lcolor("eltblue") mcolor("eltblue") ciopts(color("eltblue")) label("Female BW")) , bylabel("Structural Support")  || ///
		(est_c2b) (est_c3b) , bylabel("Diffusion of Gender Revolution"), ///
, drop(_cons) xline(0, lcolor(red)) levels(95) base xtitle(Average Marginal Effects Relative to Dual Earning, size(small)) ///
coeflabels(1._at = "5th Pct" 2._at = "25th Pct" 3._at = "50th Pct" 4._at = "75th Pct" 5._at = "95th Pct") ///
legend(position(bottom) rows(1)) groups(?._at = "{bf:Scale Value}", angle(vertical)) 

**************
* Combined Couples
**************
** Structural Familism
// Paid Labor
logit dissolve i.combined_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum structural_familism_t, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_x2

logit dissolve i.combined_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum structural_familism_t, detail
margins, dydx(3.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_x3

coefplot (est_x2, mcolor(navy) ciopts(color(navy)) label("Male BW")) (est_x3, label("Female BW")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical))

// Combined Division of Labor
logit dissolve i.combined_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum structural_familism_t, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_x2a

logit dissolve i.combined_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum structural_familism_t, detail
margins, dydx(3.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_x3a

logit dissolve i.combined_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum structural_familism_t, detail
margins, dydx(4.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_x4a

logit dissolve i.combined_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum structural_familism_t, detail
margins, dydx(5.division_bucket_hrs_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_x5a

logit dissolve i.combined_dur c.structural_familism_t i.division_bucket_hrs_t1 c.structural_familism_t#i.division_bucket_hrs_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum structural_familism_t, detail
margins division_bucket_hrs_t1, at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) 

coefplot (est_x2a, mcolor(blue) ciopts(color(blue)) label("Traditional")) (est_x3a, label("Counter-Traditional")) (est_x4a, label("Second Shift"))  (est_x5a, label("All Others")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical)) xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) legend(position(bottom) rows(1))

** Diffusion of Gender Revolution
// Paid Labor
logit dissolve i.combined_dur c.diffusion_t i.hh_hours_type_t1 c.diffusion_t#i.hh_hours_type_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum diffusion_t, detail
margins, dydx(2.hh_hours_type_t1) at(diffusion_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_x2b

logit dissolve i.combined_dur c.diffusion_t i.hh_hours_type_t1 c.diffusion_t#i.hh_hours_type_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum diffusion_t, detail
margins, dydx(3.hh_hours_type_t1) at(diffusion_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_x3b

coefplot (est_x2b, mcolor(navy) ciopts(color(navy)) label("Male BW")) (est_x3b, label("Female BW")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) groups(?._at = "{bf:Diffusion of Gender Revolution: Percentiles}", angle(vertical))

// Combined Division of Labor
logit dissolve i.combined_dur c.diffusion_t i.division_bucket_hrs_t1 c.diffusion_t#i.division_bucket_hrs_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum diffusion_t, detail
margins, dydx(2.division_bucket_hrs_t1) at(diffusion_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_x2c

logit dissolve i.combined_dur c.diffusion_t i.division_bucket_hrs_t1 c.diffusion_t#i.division_bucket_hrs_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum diffusion_t, detail
margins, dydx(3.division_bucket_hrs_t1) at(diffusion_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_x3c

logit dissolve i.combined_dur c.diffusion_t i.division_bucket_hrs_t1 c.diffusion_t#i.division_bucket_hrs_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum diffusion_t, detail
margins, dydx(4.division_bucket_hrs_t1) at(diffusion_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_x4c

logit dissolve i.combined_dur c.diffusion_t i.division_bucket_hrs_t1 c.diffusion_t#i.division_bucket_hrs_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum diffusion_t, detail
margins, dydx(5.division_bucket_hrs_t1) at(diffusion_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_x5c

logit dissolve i.combined_dur c.diffusion_t i.division_bucket_hrs_t1 c.diffusion_t#i.division_bucket_hrs_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum diffusion_t, detail
margins division_bucket_hrs_t1, at(diffusion_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) 

coefplot (est_x2c, mcolor(blue) ciopts(color(blue)) label("Traditional")) (est_x3c, label("Counter-Traditional")) (est_x4c, label("Second Shift"))  (est_x5c, label("All Others")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") groups(?._at = "{bf:Diffusion of Gender Revolution: Percentiles}", angle(vertical)) xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) legend(position(bottom) rows(1))

** JUST attitudes
// Paid Labor
logit dissolve i.combined_dur c.avg_egal_reg_t i.hh_hours_type_t1 c.avg_egal_reg_t#i.hh_hours_type_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum avg_egal_reg_t, detail
margins, dydx(2.hh_hours_type_t1) at(avg_egal_reg_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_x2d

logit dissolve i.combined_dur c.avg_egal_reg_t i.hh_hours_type_t1 c.avg_egal_reg_t#i.hh_hours_type_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum avg_egal_reg_t, detail
margins, dydx(3.hh_hours_type_t1) at(avg_egal_reg_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_x3d

coefplot (est_x2d, mcolor(navy) ciopts(color(navy)) label("Male BW")) (est_x3d, label("Female BW")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) groups(?._at = "{bf:% of Population with Egal Attitudes: Percentiles}", angle(vertical))

// Combined Division of Labor
logit dissolve i.combined_dur c.avg_egal_reg_t i.division_bucket_hrs_t1 c.avg_egal_reg_t#i.division_bucket_hrs_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum avg_egal_reg_t, detail
margins, dydx(2.division_bucket_hrs_t1) at(avg_egal_reg_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_x2e

logit dissolve i.combined_dur c.avg_egal_reg_t i.division_bucket_hrs_t1 c.avg_egal_reg_t#i.division_bucket_hrs_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum avg_egal_reg_t, detail
margins, dydx(3.division_bucket_hrs_t1) at(avg_egal_reg_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_x3e

logit dissolve i.combined_dur c.avg_egal_reg_t i.division_bucket_hrs_t1 c.avg_egal_reg_t#i.division_bucket_hrs_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum avg_egal_reg_t, detail
margins, dydx(4.division_bucket_hrs_t1) at(avg_egal_reg_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_x4e

logit dissolve i.combined_dur c.avg_egal_reg_t i.division_bucket_hrs_t1 c.avg_egal_reg_t#i.division_bucket_hrs_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum avg_egal_reg_t, detail
margins, dydx(5.division_bucket_hrs_t1) at(avg_egal_reg_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_x5e

logit dissolve i.combined_dur c.avg_egal_reg_t i.division_bucket_hrs_t1 c.avg_egal_reg_t#i.division_bucket_hrs_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum avg_egal_reg_t, detail
margins division_bucket_hrs_t1, at(avg_egal_reg_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) 

coefplot (est_x2e, mcolor(blue) ciopts(color(blue)) label("Traditional")) (est_x3e, label("Counter-Traditional")) (est_x4e, label("Second Shift"))  (est_x5e, label("All Others")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") groups(?._at = "{bf:% of Population with Egal Attitudes: Percentiles}", angle(vertical)) xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) legend(position(bottom) rows(1))

** 2x2 indicator (I guess use structural familism if that is what is above - but then wait, I might be using a diff diffusion indicator oops - okay attitudes v. diffusion- but individual effects nearly identical so I think it is okay
// Paid Labor
logit dissolve i.combined_dur i.policy_group_v3_t i.hh_hours_type_t1 i.policy_group_v3_t#i.hh_hours_type_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
// margins, dydx(hh_hours_type_t1) at(policy_group_v3_t=(1 2 3 4)) 
margins, dydx(2.hh_hours_type_t1) at(policy_group_v3_t=(1 2 3 4)) post
estimates store est_x2f

logit dissolve i.combined_dur i.policy_group_v3_t i.hh_hours_type_t1 i.policy_group_v3_t#i.hh_hours_type_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(3.hh_hours_type_t1) at(policy_group_v3_t=(1 2 3 4)) post // female bw x egal / no is not estimating, so this is not working...
estimates store est_x3f

coefplot (est_x2f, mcolor(navy) ciopts(color(navy)) label("Male BW")) (est_x3f, label("Female BW")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) coeflabels(1._at = "Egal, Support" 2._at = "Egal, No Support" 3._at = "Trad, Support" 4._at = "Trad, No Support") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) groups(?._at = "{bf: Policy x Diffusion}", angle(vertical)) //  

// Combined DoL
logit dissolve i.combined_dur i.policy_group_v3_t i.division_bucket_hrs_gp_t1 i.policy_group_v3_t#i.division_bucket_hrs_gp_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
// margins, dydx(division_bucket_hrs_gp_t1) at(policy_group_v3_t=(1 2 3 4))
margins, dydx(2.division_bucket_hrs_gp_t1) at(policy_group_v3_t=(1 2 3 4)) level(90) post
estimates store est_x2g

logit dissolve i.combined_dur i.policy_group_v3_t i.division_bucket_hrs_gp_t1 i.policy_group_v3_t#i.division_bucket_hrs_gp_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(3.division_bucket_hrs_gp_t1) at(policy_group_v3_t=(1 2 3 4)) level(90) post
estimates store est_x3g

logit dissolve i.combined_dur i.policy_group_v3_t i.division_bucket_hrs_gp_t1 i.policy_group_v3_t#i.division_bucket_hrs_gp_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
margins, dydx(4.division_bucket_hrs_gp_t1) at(policy_group_v3_t=(1 2 3 4)) level(90) post
estimates store est_x4g

coefplot (est_x2g, mcolor(blue) ciopts(color(blue)) label("Traditional")) (est_x3g,  mcolor(gray) ciopts(color(gray)) label("Second Shift"))  (est_x4g, mcolor(black) ciopts(color(black)) label("All Others")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) coeflabels(1._at = "Egal, Support" 2._at = "Egal, No Support" 3._at = "Trad, Support" 4._at = "Trad, No Support") groups(?._at = "{bf:Policy x Diffusion}", angle(vertical)) xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) legend(position(bottom) rows(1))

// Alt Figures - across metrics
coefplot (est_x2, lcolor("navy") mcolor("navy") ciopts(color("navy")) label("Male BW")) (est_x3, lcolor("eltblue") mcolor("eltblue") ciopts(color("eltblue")) label("Female BW")) , bylabel("Structural Support")  || ///
		(est_x2b) (est_x3b) , bylabel("Diffusion of Gender Revolution"), ///
, drop(_cons) xline(0, lcolor(red)) levels(95) base xtitle(Average Marginal Effects Relative to Dual Earning, size(small)) ///
coeflabels(1._at = "5th Pct" 2._at = "25th Pct" 3._at = "50th Pct" 4._at = "75th Pct" 5._at = "95th Pct") ///
legend(position(bottom) rows(1)) groups(?._at = "{bf:Scale Value}", angle(vertical)) 

// Alt Figures - across groups, with 2x2 policy indicator
coefplot (est_m2f, lcolor("navy") mcolor("navy") ciopts(color("navy")) label("Male BW")) (est_m3f, lcolor("eltblue") mcolor("eltblue") ciopts(color("eltblue")) label("Female BW")) , bylabel("Married Couples")  || ///
		est_c2f, bylabel("Cohabitors") || ///
		est_x2f est_x3f, bylabel("All Couples"), ///
, drop(_cons) xline(0, lcolor(red)) levels(95) base xtitle(Average Marginal Effects Relative to Dual Earning, size(small)) ///
coeflabels(1._at = "Egal, Support" 2._at = "Egal, No Support" 3._at = "Trad, Support" 4._at = "Trad, No Support")  ///
legend(position(bottom) rows(1)) groups(?._at = "{bf:Policy x Diffusion}", angle(vertical)) byopts(rows(1) xrescale)
// est_c3f - couldn't use cohab x female BW

********************************************************************************
**# Option 3: Parental Status
********************************************************************************

// Ideally want four panels (1) total sample; (2) parents; (3) parents <6; (childfree) - then one panel per sample
// For now - JUST use structural support measure
// oh - there is probably a way to do this in Stata instead of myself as well (like I've done for education - see divorce over time code)

**************
* Married Couples
**************
** Total Sample
// Paid Labor
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_m2

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(3.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_m3

// Combined Division of Labor
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(2.division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_m2a

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(3.division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_m3a

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0, or
sum structural_familism_t, detail
margins, dydx(4.division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_m4a

** All Parents
// Paid Labor
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & children==1, or
sum structural_familism_t, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_m2_p

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & children==1, or
sum structural_familism_t, detail
margins, dydx(3.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_m3_p

// Combined Division of Labor
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & children==1, or
sum structural_familism_t, detail
margins, dydx(2.division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_m2a_p

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & children==1, or
sum structural_familism_t, detail
margins, dydx(3.division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_m3a_p

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & children==1, or
sum structural_familism_t, detail
margins, dydx(4.division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_m4a_p


** Parents < 6
// Paid Labor
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & children_under6==1, or
sum structural_familism_t, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_m2_u

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & children_under6==1, or
sum structural_familism_t, detail
margins, dydx(3.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_m3_u

// Combined Division of Labor
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & children_under6==1, or
sum structural_familism_t, detail
margins, dydx(2.division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_m2a_u

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & children_under6==1, or
sum structural_familism_t, detail
margins, dydx(3.division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_m3a_u

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & children_under6==1, or
sum structural_familism_t, detail
margins, dydx(4.division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_m4a_u

** Childfree
// Paid Labor
logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & children==0, or
sum structural_familism_t, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_m2_c

logit dissolve i.marr_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & children==0, or
sum structural_familism_t, detail
margins, dydx(3.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_m3_c

// Combined Division of Labor
logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & children==0, or
sum structural_familism_t, detail
margins, dydx(2.division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_m2a_c

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & children==0, or
sum structural_familism_t, detail
margins, dydx(3.division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_m3a_c

logit dissolve i.marr_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $controls if current_rel_type==20 & marr_dur>=0 & any_missing==0 & no_labor==0 & children==0, or
sum structural_familism_t, detail
margins, dydx(4.division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_m4a_c

// Figure: Paid Labor
coefplot (est_m2, lcolor("navy") mcolor("navy") ciopts(color("navy")) label("Male BW")) (est_m3, lcolor("eltblue") mcolor("eltblue") ciopts(color("eltblue")) label("Female BW")) , bylabel("Total")  || ///
		(est_m2_p) (est_m3_p) , bylabel("Parents")  || ///
		(est_m2_u) (est_m3_u) , bylabel("Under 6")  || ///
		(est_m2_c) (est_m3_c) , bylabel("Childfree"), ///
, drop(_cons) xline(0, lcolor(red)) levels(95) base xtitle(Average Marginal Effects Relative to Dual Earning, size(small)) ///
coeflabels(1._at = "5th Pct" 2._at = "25th Pct" 3._at = "50th Pct" 4._at = "75th Pct" 5._at = "95th Pct") ///
legend(position(bottom) rows(1)) groups(?._at = "{bf:Structural Support}", angle(vertical)) 

// Figure: Combined Labor
coefplot (est_m2a, lcolor("blue") mcolor("blue") ciopts(color("blue")) label("Traditional")) (est_m3a, lcolor("gs8") mcolor("gs8") ciopts(color("gs8")) label("Second Shift")) (est_m4a, lcolor("black") mcolor("black") ciopts(color("black")) label("All Others")), bylabel("Total")  || ///
		est_m2a_p est_m3a_p est_m4a_p, bylabel("Parents")  || ///
		est_m2a_u est_m3a_u est_m4a_p, bylabel("Under 6")  || ///
		est_m2a_c est_m3a_c est_m4a_p, bylabel("Childfree"), ///
, drop(_cons) xline(0, lcolor(red)) levels(95) base xtitle(Average Marginal Effects Relative to Egalitarian, size(small)) ///
coeflabels(1._at = "5th Pct" 2._at = "25th Pct" 3._at = "50th Pct" 4._at = "75th Pct" 5._at = "95th Pct") ///
legend(position(bottom) rows(1)) groups(?._at = "{bf:Structural Support}", angle(vertical)) 

**************
* Cohabiting Couples
**************
** Total Sample
// Paid Labor
logit dissolve i.coh_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum structural_familism_t, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_c2

logit dissolve i.coh_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum structural_familism_t, detail
margins, dydx(3.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_c3

// Combined Division of Labor
logit dissolve i.coh_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum structural_familism_t, detail
margins, dydx(2.division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_c2a

logit dissolve i.coh_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum structural_familism_t, detail
margins, dydx(3.division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_c3a

logit dissolve i.coh_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum structural_familism_t, detail
margins, dydx(4.division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_c4a

** All Parents
// Paid Labor
logit dissolve i.coh_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0 & children==1, or
sum structural_familism_t, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_c2_p

logit dissolve i.coh_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0 & children==1, or
sum structural_familism_t, detail
margins, dydx(3.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_c3_p

// Combined Division of Labor
logit dissolve i.coh_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0 & children==1, or
sum structural_familism_t, detail
margins, dydx(2.division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_c2a_p

logit dissolve i.coh_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0 & children==1, or
sum structural_familism_t, detail
margins, dydx(3.division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_c3a_p

logit dissolve i.coh_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0 & children==1, or
sum structural_familism_t, detail
margins, dydx(4.division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_c4a_p

** Parents < 6
// Paid Labor
logit dissolve i.coh_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0 & children_under6==1, or
sum structural_familism_t, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_c2_u

logit dissolve i.coh_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0 & children_under6==1, or
sum structural_familism_t, detail
margins, dydx(3.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_c3_u

// Combined Division of Labor
logit dissolve i.coh_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0 & children_under6==1, or
sum structural_familism_t, detail
margins, dydx(2.division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_c2a_u

logit dissolve i.coh_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0 & children_under6==1, or
sum structural_familism_t, detail
margins, dydx(3.division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_c3a_u

logit dissolve i.coh_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0 & children_under6==1, or
sum structural_familism_t, detail
margins, dydx(4.division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_c4a_u

** Childfree
// Paid Labor
logit dissolve i.coh_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0 & children==0, or
sum structural_familism_t, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_c2_c

logit dissolve i.coh_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0 & children==0, or
sum structural_familism_t, detail
margins, dydx(3.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_c3_c

// Combined Division of Labor
logit dissolve i.coh_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0 & children==0, or
sum structural_familism_t, detail
margins, dydx(2.division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_c2a_c

logit dissolve i.coh_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0 & children==0, or
sum structural_familism_t, detail
margins, dydx(3.division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_c3a_c

logit dissolve i.coh_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $controls if current_rel_type==22 & any_missing==0 & no_labor==0 & cohab_flag==0 & children==0, or
sum structural_familism_t, detail
margins, dydx(4.division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_c4a_c

// Figure: Paid Labor
coefplot (est_c2, lcolor("navy") mcolor("navy") ciopts(color("navy")) label("Male BW")) (est_c3, lcolor("eltblue") mcolor("eltblue") ciopts(color("eltblue")) label("Female BW")) , bylabel("Total")  || ///
		(est_c2_p) (est_c3_p) , bylabel("Parents")  || ///
		(est_c2_u) (est_c3_u) , bylabel("Under 6")  || ///
		(est_c2_c) (est_c3_c) , bylabel("Childfree"), ///
, drop(_cons) xline(0, lcolor(red)) levels(95) base xtitle(Average Marginal Effects Relative to Dual Earning, size(small)) ///
coeflabels(1._at = "5th Pct" 2._at = "25th Pct" 3._at = "50th Pct" 4._at = "75th Pct" 5._at = "95th Pct") ///
legend(position(bottom) rows(1)) groups(?._at = "{bf:Structural Support}", angle(vertical)) 

// Figure: Combined Labor
coefplot (est_c2a, lcolor("blue") mcolor("blue") ciopts(color("blue")) label("Traditional")) (est_c3a, lcolor("gs8") mcolor("gs8") ciopts(color("gs8")) label("Second Shift")) (est_c4a, lcolor("black") mcolor("black") ciopts(color("black")) label("All Others")), bylabel("Total")  || ///
		est_c2a_p est_c3a_p est_c4a_p, bylabel("Parents")  || ///
		est_c2a_u est_c3a_u est_c4a_p, bylabel("Under 6")  || ///
		est_c2a_c est_c3a_c est_c4a_p, bylabel("Childfree"), ///
, drop(_cons) xline(0, lcolor(red)) levels(95) base xtitle(Average Marginal Effects Relative to Egalitarian, size(small)) ///
coeflabels(1._at = "5th Pct" 2._at = "25th Pct" 3._at = "50th Pct" 4._at = "75th Pct" 5._at = "95th Pct") ///
legend(position(bottom) rows(1)) groups(?._at = "{bf:Structural Support}", angle(vertical)) 

**************
* Combined Couples
**************
** Total Sample
// Paid Labor
logit dissolve i.combined_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum structural_familism_t, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_x2

logit dissolve i.combined_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum structural_familism_t, detail
margins, dydx(3.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_x3

// Combined Division of Labor
logit dissolve i.combined_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum structural_familism_t, detail
margins, dydx(2.division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_x2a

logit dissolve i.combined_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum structural_familism_t, detail
margins, dydx(3.division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_x3a

logit dissolve i.combined_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0, or
sum structural_familism_t, detail
margins, dydx(4.division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_x4a

** All Parents
// Paid Labor
logit dissolve i.combined_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0 & children==1, or
sum structural_familism_t, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_x2_p

logit dissolve i.combined_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0 & children==1, or
sum structural_familism_t, detail
margins, dydx(3.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_x3_p

// Combined Division of Labor
logit dissolve i.combined_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0 & children==1, or
sum structural_familism_t, detail
margins, dydx(2.division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_x2a_p

logit dissolve i.combined_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0 & children==1, or
sum structural_familism_t, detail
margins, dydx(3.division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_x3a_p

logit dissolve i.combined_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0 & children==1, or
sum structural_familism_t, detail
margins, dydx(4.division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_x4a_p

** Parents < 6
// Paid Labor
logit dissolve i.combined_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0 & children_under6==1, or
sum structural_familism_t, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_x2_u

logit dissolve i.combined_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0 & children_under6==1, or
sum structural_familism_t, detail
margins, dydx(3.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_x3_u

// Combined Division of Labor
logit dissolve i.combined_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0 & children_under6==1, or
sum structural_familism_t, detail
margins, dydx(2.division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_x2a_u

logit dissolve i.combined_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0 & children_under6==1, or
sum structural_familism_t, detail
margins, dydx(3.division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_x3a_u

logit dissolve i.combined_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0 & children_under6==1, or
sum structural_familism_t, detail
margins, dydx(4.division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_x4a_u

** Childfree
// Paid Labor
logit dissolve i.combined_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0 & children==0, or
sum structural_familism_t, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_x2_c

logit dissolve i.combined_dur c.structural_familism_t i.hh_hours_type_t1 c.structural_familism_t#i.hh_hours_type_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0 & children==0, or
sum structural_familism_t, detail
margins, dydx(3.hh_hours_type_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_x3_c

// Combined Division of Labor
logit dissolve i.combined_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0 & children==0, or
sum structural_familism_t, detail
margins, dydx(2.division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_x2a_c

logit dissolve i.combined_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0 & children==0, or
sum structural_familism_t, detail
margins, dydx(3.division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_x3a_c

logit dissolve i.combined_dur c.structural_familism_t i.division_bucket_hrs_gp_t1 c.structural_familism_t#i.division_bucket_hrs_gp_t1 $combo_controls if combined_dur>=0 & any_missing==0 & no_labor==0 & cohab_flag==0 & children==0, or
sum structural_familism_t, detail
margins, dydx(4.division_bucket_hrs_gp_t1) at(structural_familism_t=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_x4a_c

// Figure: Paid Labor
coefplot (est_x2, lcolor("navy") mcolor("navy") ciopts(color("navy")) label("Male BW")) (est_x3, lcolor("eltblue") mcolor("eltblue") ciopts(color("eltblue")) label("Female BW")) , bylabel("Total")  || ///
		(est_x2_p) (est_x3_p) , bylabel("Parents")  || ///
		(est_x2_u) (est_x3_u) , bylabel("Under 6")  || ///
		(est_x2_c) (est_x3_c) , bylabel("Childfree"), ///
, drop(_cons) xline(0, lcolor(red)) levels(95) base xtitle(Average Marginal Effects Relative to Dual Earning, size(small)) ///
coeflabels(1._at = "5th Pct" 2._at = "25th Pct" 3._at = "50th Pct" 4._at = "75th Pct" 5._at = "95th Pct") ///
legend(position(bottom) rows(1)) groups(?._at = "{bf:Structural Support}", angle(vertical)) 

// Figure: Combined Labor
coefplot (est_x2a, lcolor("blue") mcolor("blue") ciopts(color("blue")) label("Traditional")) (est_x3a, lcolor("gs8") mcolor("gs8") ciopts(color("gs8")) label("Second Shift")) (est_x4a, lcolor("black") mcolor("black") ciopts(color("black")) label("All Others")), bylabel("Total")  || ///
		est_x2a_p est_x3a_p est_x4a_p, bylabel("Parents")  || ///
		est_x2a_u est_x3a_u est_x4a_p, bylabel("Under 6")  || ///
		est_x2a_c est_x3a_c est_x4a_p, bylabel("Childfree"), ///
, drop(_cons) xline(0, lcolor(red)) levels(95) base xtitle(Average Marginal Effects Relative to Egalitarian, size(small)) ///
coeflabels(1._at = "5th Pct" 2._at = "25th Pct" 3._at = "50th Pct" 4._at = "75th Pct" 5._at = "95th Pct") ///
legend(position(bottom) rows(1)) groups(?._at = "{bf:Structural Support}", angle(vertical)) 

********************************************************************************
**# Option 2: Marriage v. Cohabitation
********************************************************************************
// Will show main effects (I pull these results above)
// Then moderation of current indicator just for paid labor comparing married v. cohab - total sample and parents
// Moving this down because all of these estimates are already stored, just need two figures

// 1. Total Sample
coefplot (est_m2, lcolor("navy") mcolor("navy") ciopts(color("navy")) label("Male BW")) (est_m3, lcolor("eltblue") mcolor("eltblue") ciopts(color("eltblue")) label("Female BW")) , bylabel("Married")  || ///
		(est_c2) (est_c3) , bylabel("Cohabitors"), ///
, drop(_cons) xline(0, lcolor(red)) levels(95) base xtitle(Average Marginal Effects Relative to Dual Earning, size(small)) ///
coeflabels(1._at = "5th Pct" 2._at = "25th Pct" 3._at = "50th Pct" 4._at = "75th Pct" 5._at = "95th Pct") ///
legend(position(bottom) rows(1)) groups(?._at = "{bf:Structural Support}", angle(vertical)) byopts(xrescale)

// 2. Parents
coefplot (est_m2_u, lcolor("navy") mcolor("navy") ciopts(color("navy")) label("Male BW")) (est_m3_u, lcolor("eltblue") mcolor("eltblue") ciopts(color("eltblue")) label("Female BW")) , bylabel("Married")  || ///
		(est_c2_u) (est_c3_u) , bylabel("Cohabitors"), ///
, drop(_cons) xline(0, lcolor(red)) levels(95) base xtitle(Average Marginal Effects Relative to Dual Earning, size(small)) ///
coeflabels(1._at = "5th Pct" 2._at = "25th Pct" 3._at = "50th Pct" 4._at = "75th Pct" 5._at = "95th Pct") ///
legend(position(bottom) rows(1)) groups(?._at = "{bf:Structural Support}", angle(vertical)) byopts(xrescale)