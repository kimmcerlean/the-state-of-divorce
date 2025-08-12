********************************************************************************
* Adding in state-level data
* state-level-analysis.do
* Kim McErlean
********************************************************************************
// ssc install marginscontplot - have to make sure you have this pckage

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

browse unique_id survey_yr partner_unique_id ever_transition current_rel_type current_rel_number current_marr_number rel_start_yr_couple rel_end_yr_couple dur marr_dur marriage_start_yr transition_year

// let's get a sense of sample NOW  - this needs to be revisited based on how I want to think about relationship order...but just want an initial sense
unique unique_id partner_unique_id, by(current_rel_type)
unique unique_id partner_unique_id if flag==0, by(current_rel_type) // no left-censor. this removes shockingless less than expected (think bc of using the later years)
unique unique_id partner_unique_id if dissolve==1, by(current_rel_type)
unique unique_id partner_unique_id if flag==0 & dissolve==1, by(current_rel_type) // real concern is # of divorces - except i guess bc cohab is more unstable, this might not actually be as bad as I think??

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

// missing value inspect
inspect age_mar_wife
inspect age_mar_head
inspect raceth_head
inspect raceth_head_fixed
inspect same_race
inspect either_enrolled
inspect region
inspect cohab_with_partner 
inspect cohab_with_other
inspect pre_marital_birth
inspect home_owner
inspect couple_joint_religion // this has the most missing (about 3.5%)

/* QA
preserve
collapse (max) rel_start_all rel_end_all dissolve dissolve_v0 outcome end_year survey_yr (count) num_yrs=survey_yr, by(unique_id partner_unique_id)
restore
*/

********************************************************************************
**# Merge onto policy data
********************************************************************************
rename STATE_ state_fips
rename survey_yr year

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

set scheme cleanplots

alpha paid_leave_length_st_t1 prek_enrolled_public_st_t1 min_amt_above_fed_st_t1 earn_ratio_neg_st_t1 unemployment_percap_st_t1 abortion_protected_st_t1 welfare_all_st_t1 // structural familism. 0.70 0.716 now
alpha paid_leave_st_t1 prek_enrolled_public_st_t1 min_above_fed_st_t1 earn_ratio_neg_st_t1 unemployment_percap_st_t1 abortion_protected_st_t1 welfare_all_st_t1 
alpha paid_leave_st_t1 prek_enrolled_public_st_t1 min_amt_above_fed_st_t1 earn_ratio_neg_st_t1 unemployment_percap_st_t1 abortion_protected_st_t1 welfare_all_st_t1 

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
// Let's first explore basic trends across different samples
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
logit dissolve i.marr_dur c.structural_familism_t1 i.hh_earn_type_t1 $controls if hh_earn_type_t1 < 4 & current_rel_type==20 & marr_dur>=0, or
margins, dydx(hh_earn_type_t1) level(95) post
estimates store est1a

logit dissolve i.marr_dur c.structural_familism_t1 i.hh_hours_type_t1 $controls if hh_hours_type_t1 < 4 & current_rel_type==20 & marr_dur>=0, or
margins, dydx(hh_hours_type_t1) level(95) post
estimates store est2a

logit dissolve i.marr_dur c.structural_familism_t1 i.division_bucket_t1 $controls if current_rel_type==20 & marr_dur>=0, or
margins, dydx(division_bucket_t1) level(95) post
estimates store est3a

logit dissolve i.marr_dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if current_rel_type==20 & marr_dur>=0, or
margins, dydx(division_bucket_hrs_t1) level(95) post
estimates store est4a

logit dissolve i.marr_dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if current_rel_type==20 & marr_dur>=0, or
margins, dydx(structural_familism_t1) post
estimates store est5a

coefplot est1a est2a est3a est4a,  drop(_cons) nolabel xline(0) levels(95) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners" 2.hh_earn_type_t1 = "Male Breadwinner" 3.hh_earn_type_t1 = "Female Breadwinner" 4.hh_earn_type_t1 = "No Earners" 1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" 2.division_bucket_t1 = "Traditional" 3.division_bucket_t1 = "Counter Traditional" 4.division_bucket_t1 = "Her Second Shift" 5.division_bucket_t1 = "All Others") ///
 headings(2.hh_hours_type_t1= "{bf:Division of Work Hours}" 2.hh_earn_type_t1= `"{bf:Division of Earnings}"' 2.division_bucket_hrs_t1 = `"{bf:Combined (hours)}"' 2.division_bucket_t1 = `"{bf:Combined (earnings)}"')

 // just hours and maybe add structural support
coefplot (est2a, offset(.20) nokey) (est4a, offset(-.20) nokey) (est5a, nokey lcolor("black") mcolor("black") ciopts(color("black"))) , drop(_cons) xline(0) levels(95) base xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners"  1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" structural_familism_t1 = "Structural Support Scale") ///
 headings(1.hh_hours_type_t1= `""{bf:Division of Work Hours}" "{it:(Model 1)}""'  1.division_bucket_hrs_t1 = `""{bf:Combined Paid and Unpaid Labor}" "{it:(Model 2)}""' structural_familism_t1 = `""{bf:Structural Support for Working Families}" "{it:(Model 2)}"')
 

********************************************************************************
* All parents
********************************************************************************
logit dissolve i.marr_dur c.structural_familism_t1 i.hh_earn_type_t1 $controls if children==1 & hh_earn_type_t1 < 4 & current_rel_type==20 & marr_dur>=0, or
margins, dydx(hh_earn_type_t1) level(95) post
estimates store est1b

logit dissolve i.marr_dur c.structural_familism_t1 i.hh_hours_type_t1 $controls if children==1 & hh_hours_type_t1 < 4 & current_rel_type==20 & marr_dur>=0, or
margins, dydx(hh_hours_type_t1) level(95) post
estimates store est2b

logit dissolve i.marr_dur c.structural_familism_t1 i.division_bucket_t1 $controls if children==1 & current_rel_type==20 & marr_dur>=0, or
margins, dydx(division_bucket_t1) level(95) post
estimates store est3b

logit dissolve i.marr_dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children==1 & current_rel_type==20 & marr_dur>=0, or
margins, dydx(division_bucket_hrs_t1) level(95) post
estimates store est4b

logit dissolve i.marr_dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children==1 & current_rel_type==20 & marr_dur>=0, or
margins, dydx(structural_familism_t1) post
estimates store est5b

coefplot est1b est2b est3b est4b,  drop(_cons) nolabel xline(0) levels(95) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners" 2.hh_earn_type_t1 = "Male Breadwinner" 3.hh_earn_type_t1 = "Female Breadwinner" 4.hh_earn_type_t1 = "No Earners" 1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" 2.division_bucket_t1 = "Traditional" 3.division_bucket_t1 = "Counter Traditional" 4.division_bucket_t1 = "Her Second Shift" 5.division_bucket_t1 = "All Others") ///
 headings(2.hh_hours_type_t1= "{bf:Division of Work Hours}" 2.hh_earn_type_t1= `"{bf:Division of Earnings}"' 2.division_bucket_hrs_t1 = `"{bf:Combined (hours)}"' 2.division_bucket_t1 = `"{bf:Combined (earnings)}"')
 
 // just hours and maybe add structural support
coefplot (est2b, offset(.20) nokey) (est4b, offset(-.20) nokey) (est5b, nokey lcolor("black") mcolor("black") ciopts(color("black"))) , drop(_cons) xline(0) levels(95) base xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners"  1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" structural_familism_t1 = "Structural Support Scale") ///
 headings(1.hh_hours_type_t1= `""{bf:Division of Work Hours}" "{it:(Model 1)}""'  1.division_bucket_hrs_t1 = `""{bf:Combined Paid and Unpaid Labor}" "{it:(Model 2)}""' structural_familism_t1 = `""{bf:Structural Support for Working Families}" "{it:(Model 2)}"')
 
********************************************************************************
* All Parents of children under the age of 6
********************************************************************************
logit dissolve i.marr_dur c.structural_familism_t1 i.hh_earn_type_t1 $controls if children_under6==1 & hh_earn_type_t1 < 4 & current_rel_type==20 & marr_dur>=0, or
margins, dydx(hh_earn_type_t1) level(95) post
estimates store est1c

logit dissolve i.marr_dur c.structural_familism_t1 i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4 & current_rel_type==20 & marr_dur>=0, or
margins, dydx(hh_hours_type_t1) level(95) post
estimates store est2c

logit dissolve i.marr_dur c.structural_familism_t1 i.division_bucket_t1 $controls if children_under6==1 & current_rel_type==20 & marr_dur>=0, or
margins, dydx(division_bucket_t1) level(95) post
estimates store est3c

logit dissolve i.marr_dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children_under6==1 & current_rel_type==20 & marr_dur>=0, or
margins, dydx(division_bucket_hrs_t1) level(95) post
estimates store est4c

logit dissolve i.marr_dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children_under6==1 & current_rel_type==20 & marr_dur>=0, or
margins, dydx(structural_familism_t1) post
estimates store est5c

coefplot est1c est2c est3c est4c,  drop(_cons) nolabel xline(0) levels(95) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners" 2.hh_earn_type_t1 = "Male Breadwinner" 3.hh_earn_type_t1 = "Female Breadwinner" 4.hh_earn_type_t1 = "No Earners" 1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" 2.division_bucket_t1 = "Traditional" 3.division_bucket_t1 = "Counter Traditional" 4.division_bucket_t1 = "Her Second Shift" 5.division_bucket_t1 = "All Others") ///
 headings(2.hh_hours_type_t1= "{bf:Division of Work Hours}" 2.hh_earn_type_t1= `"{bf:Division of Earnings}"' 2.division_bucket_hrs_t1 = `"{bf:Combined (hours)}"' 2.division_bucket_t1 = `"{bf:Combined (earnings)}"')

 // just hours and maybe add structural support
coefplot (est2c, offset(.20) nokey) (est4c, offset(-.20) nokey) (est5c, nokey lcolor("black") mcolor("black") ciopts(color("black"))) , drop(_cons) xline(0) levels(95) base xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners"  1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" structural_familism_t1 = "Structural Support Scale") ///
 headings(1.hh_hours_type_t1= `""{bf:Division of Work Hours}" "{it:(Model 1)}""'  1.division_bucket_hrs_t1 = `""{bf:Combined Paid and Unpaid Labor}" "{it:(Model 2)}""' structural_familism_t1 = `""{bf:Structural Support for Working Families}" "{it:(Model 2)}"')

********************************************************************************
* All Parents of children under the age of 6 - start = dur 0 
********************************************************************************
tab children_under6_flag children_under6, m 

logit dissolve i.marr_dur c.structural_familism_t1 i.hh_earn_type_t1 $controls if children_under6_flag==1 & hh_earn_type_t1 < 4 & current_rel_type==20 & marr_dur>=0, or
margins, dydx(hh_earn_type_t1) level(95) post
estimates store est1d

logit dissolve i.marr_dur c.structural_familism_t1 i.hh_hours_type_t1 $controls if children_under6_flag==1 & hh_hours_type_t1 < 4 & current_rel_type==20 & marr_dur>=0, or
margins, dydx(hh_hours_type_t1) level(95) post
estimates store est2d

logit dissolve i.marr_dur c.structural_familism_t1 i.division_bucket_t1 $controls if children_under6_flag==1 & current_rel_type==20 & marr_dur>=0, or
margins, dydx(division_bucket_t1) level(95) post
estimates store est3d

logit dissolve i.marr_dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children_under6_flag==1 & current_rel_type==20 & marr_dur>=0, or
margins, dydx(division_bucket_hrs_t1) level(95) post
estimates store est4d

logit dissolve i.marr_dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children_under6_flag==1 & current_rel_type==20 & marr_dur>=0, or
margins, dydx(structural_familism_t1) post
estimates store est5d

coefplot est1d est2d est3d est4d,  drop(_cons) nolabel xline(0) levels(95) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners" 2.hh_earn_type_t1 = "Male Breadwinner" 3.hh_earn_type_t1 = "Female Breadwinner" 4.hh_earn_type_t1 = "No Earners" 1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" 2.division_bucket_t1 = "Traditional" 3.division_bucket_t1 = "Counter Traditional" 4.division_bucket_t1 = "Her Second Shift" 5.division_bucket_t1 = "All Others") ///
 headings(2.hh_hours_type_t1= "{bf:Division of Work Hours}" 2.hh_earn_type_t1= `"{bf:Division of Earnings}"' 2.division_bucket_hrs_t1 = `"{bf:Combined (hours)}"' 2.division_bucket_t1 = `"{bf:Combined (earnings)}"')

 // just hours and maybe add structural support
coefplot (est2d, offset(.20) nokey) (est4d, offset(-.20) nokey) (est5d, nokey lcolor("black") mcolor("black") ciopts(color("black"))) , drop(_cons) xline(0) levels(95) base xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners"  1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" structural_familism_t1 = "Structural Support Scale") ///
 headings(1.hh_hours_type_t1= `""{bf:Division of Work Hours}" "{it:(Model 1)}""'  1.division_bucket_hrs_t1 = `""{bf:Combined Paid and Unpaid Labor}" "{it:(Model 2)}""' structural_familism_t1 = `""{bf:Structural Support for Working Families}" "{it:(Model 2)}"')


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
logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 $controls if hh_earn_type_t1 < 4 & current_rel_type==22, or
margins, dydx(hh_earn_type_t1) level(95) post
estimates store est6a

logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 $controls if hh_hours_type_t1 < 4 & current_rel_type==22, or
margins, dydx(hh_hours_type_t1) level(95) post
estimates store est7a

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_t1 $controls if current_rel_type==22, or
margins, dydx(division_bucket_t1) level(95) post
estimates store est8a

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if current_rel_type==22, or
margins, dydx(division_bucket_hrs_t1) level(95) post
estimates store est9a

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if current_rel_type==22, or
margins, dydx(structural_familism_t1) post
estimates store est10a

coefplot est6a est7a est8a est9a,  drop(_cons) nolabel xline(0) levels(95) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners" 2.hh_earn_type_t1 = "Male Breadwinner" 3.hh_earn_type_t1 = "Female Breadwinner" 4.hh_earn_type_t1 = "No Earners" 1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" 2.division_bucket_t1 = "Traditional" 3.division_bucket_t1 = "Counter Traditional" 4.division_bucket_t1 = "Her Second Shift" 5.division_bucket_t1 = "All Others") ///
 headings(2.hh_hours_type_t1= "{bf:Division of Work Hours}" 2.hh_earn_type_t1= `"{bf:Division of Earnings}"' 2.division_bucket_hrs_t1 = `"{bf:Combined (hours)}"' 2.division_bucket_t1 = `"{bf:Combined (earnings)}"')

 // just hours and maybe add structural support
coefplot (est7a, offset(.20) nokey) (est9a, offset(-.20) nokey) (est10a, nokey lcolor("black") mcolor("black") ciopts(color("black"))) , drop(_cons) xline(0) levels(95) base xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners"  1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" structural_familism_t1 = "Structural Support Scale") ///
 headings(1.hh_hours_type_t1= `""{bf:Division of Work Hours}" "{it:(Model 1)}""'  1.division_bucket_hrs_t1 = `""{bf:Combined Paid and Unpaid Labor}" "{it:(Model 2)}""' structural_familism_t1 = `""{bf:Structural Support for Working Families}" "{it:(Model 2)}"')
 

********************************************************************************
* All parents
********************************************************************************
logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 $controls if children==1 & hh_earn_type_t1 < 4 & current_rel_type==22, or
margins, dydx(hh_earn_type_t1) level(95) post
estimates store est6b

logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 $controls if children==1 & hh_hours_type_t1 < 4 & current_rel_type==22, or
margins, dydx(hh_hours_type_t1) level(95) post
estimates store est7b

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_t1 $controls if children==1 & current_rel_type==22, or
margins, dydx(division_bucket_t1) level(95) post
estimates store est8b

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children==1 & current_rel_type==22, or
margins, dydx(division_bucket_hrs_t1) level(95) post
estimates store est9b

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children==1 & current_rel_type==22, or
margins, dydx(structural_familism_t1) post
estimates store est10b

coefplot est6b est7b est8b est9b,  drop(_cons) nolabel xline(0) levels(95) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners" 2.hh_earn_type_t1 = "Male Breadwinner" 3.hh_earn_type_t1 = "Female Breadwinner" 4.hh_earn_type_t1 = "No Earners" 1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" 2.division_bucket_t1 = "Traditional" 3.division_bucket_t1 = "Counter Traditional" 4.division_bucket_t1 = "Her Second Shift" 5.division_bucket_t1 = "All Others") ///
 headings(2.hh_hours_type_t1= "{bf:Division of Work Hours}" 2.hh_earn_type_t1= `"{bf:Division of Earnings}"' 2.division_bucket_hrs_t1 = `"{bf:Combined (hours)}"' 2.division_bucket_t1 = `"{bf:Combined (earnings)}"')

 // just hours and maybe add structural support
coefplot (est7b, offset(.20) nokey) (est9b, offset(-.20) nokey) (est10b, nokey lcolor("black") mcolor("black") ciopts(color("black"))) , drop(_cons) xline(0) levels(95) base xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners"  1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" structural_familism_t1 = "Structural Support Scale") ///
 headings(1.hh_hours_type_t1= `""{bf:Division of Work Hours}" "{it:(Model 1)}""'  1.division_bucket_hrs_t1 = `""{bf:Combined Paid and Unpaid Labor}" "{it:(Model 2)}""' structural_familism_t1 = `""{bf:Structural Support for Working Families}" "{it:(Model 2)}"')
 
********************************************************************************
* All Parents of children under the age of 6
********************************************************************************
logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 $controls if children_under6==1 & hh_earn_type_t1 < 4 & current_rel_type==22, or
margins, dydx(hh_earn_type_t1) level(95) post
estimates store est6c

logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4 & current_rel_type==22, or
margins, dydx(hh_hours_type_t1) level(95) post
estimates store est7c

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_t1 $controls if children_under6==1 & current_rel_type==22, or
margins, dydx(division_bucket_t1) level(95) post
estimates store est8c

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children_under6==1 & current_rel_type==22, or
margins, dydx(division_bucket_hrs_t1) level(95) post
estimates store est9c

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children_under6==1 & current_rel_type==22, or
margins, dydx(structural_familism_t1) post
estimates store est10c

coefplot est6c est7c est8c est9c,  drop(_cons) nolabel xline(0) levels(95) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners" 2.hh_earn_type_t1 = "Male Breadwinner" 3.hh_earn_type_t1 = "Female Breadwinner" 4.hh_earn_type_t1 = "No Earners" 1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" 2.division_bucket_t1 = "Traditional" 3.division_bucket_t1 = "Counter Traditional" 4.division_bucket_t1 = "Her Second Shift" 5.division_bucket_t1 = "All Others") ///
 headings(2.hh_hours_type_t1= "{bf:Division of Work Hours}" 2.hh_earn_type_t1= `"{bf:Division of Earnings}"' 2.division_bucket_hrs_t1 = `"{bf:Combined (hours)}"' 2.division_bucket_t1 = `"{bf:Combined (earnings)}"')

 // just hours and maybe add structural support
coefplot (est7c, offset(.20) nokey) (est9c, offset(-.20) nokey) (est10c, nokey lcolor("black") mcolor("black") ciopts(color("black"))) , drop(_cons) xline(0) levels(95) base xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners"  1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" structural_familism_t1 = "Structural Support Scale") ///
 headings(1.hh_hours_type_t1= `""{bf:Division of Work Hours}" "{it:(Model 1)}""'  1.division_bucket_hrs_t1 = `""{bf:Combined Paid and Unpaid Labor}" "{it:(Model 2)}""' structural_familism_t1 = `""{bf:Structural Support for Working Families}" "{it:(Model 2)}"')

********************************************************************************
* All Parents of children under the age of 6 - start = dur 0 
********************************************************************************
logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 $controls if children_under6_flag==1 & hh_earn_type_t1 < 4 & current_rel_type==22, or
margins, dydx(hh_earn_type_t1) level(95) post
estimates store est6d

logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 $controls if children_under6_flag==1 & hh_hours_type_t1 < 4 & current_rel_type==22, or
margins, dydx(hh_hours_type_t1) level(95) post
estimates store est7d

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_t1 $controls if children_under6_flag==1 & current_rel_type==22, or
margins, dydx(division_bucket_t1) level(95) post
estimates store est8d

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children_under6_flag==1 & current_rel_type==22, or
margins, dydx(division_bucket_hrs_t1) level(95) post
estimates store est9d

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children_under6_flag==1 & current_rel_type==22, or
margins, dydx(structural_familism_t1) post
estimates store est10d

coefplot est6d est7d est8d est9d,  drop(_cons) nolabel xline(0) levels(95) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners" 2.hh_earn_type_t1 = "Male Breadwinner" 3.hh_earn_type_t1 = "Female Breadwinner" 4.hh_earn_type_t1 = "No Earners" 1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" 2.division_bucket_t1 = "Traditional" 3.division_bucket_t1 = "Counter Traditional" 4.division_bucket_t1 = "Her Second Shift" 5.division_bucket_t1 = "All Others") ///
 headings(2.hh_hours_type_t1= "{bf:Division of Work Hours}" 2.hh_earn_type_t1= `"{bf:Division of Earnings}"' 2.division_bucket_hrs_t1 = `"{bf:Combined (hours)}"' 2.division_bucket_t1 = `"{bf:Combined (earnings)}"')

 // just hours and maybe add structural support
coefplot (est7d, offset(.20) nokey) (est9d, offset(-.20) nokey) (est10d, nokey lcolor("black") mcolor("black") ciopts(color("black"))) , drop(_cons) xline(0) levels(95) base xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners"  1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" structural_familism_t1 = "Structural Support Scale") ///
 headings(1.hh_hours_type_t1= `""{bf:Division of Work Hours}" "{it:(Model 1)}""'  1.division_bucket_hrs_t1 = `""{bf:Combined Paid and Unpaid Labor}" "{it:(Model 2)}""' structural_familism_t1 = `""{bf:Structural Support for Working Families}" "{it:(Model 2)}"')


********************************************************************************
********************************************************************************
**# MAIN MODELS (by parental status)
********************************************************************************
********************************************************************************
global controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion i.num_children"  // i.region knot1 knot2 knot3 

********************************************************************************
* Parents of children under the age of 6
********************************************************************************
/* Main Effects */
*Earnings
logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 $controls if children_under6==1 & hh_earn_type_t1!=4, or
outreg2 using "$results/dissolution_AMES_familism_parents.xls", sideway stats(coef pval) label ctitle(Parents 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

* Hours
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1!=4, or
outreg2 using "$results/dissolution_AMES_familism_parents.xls", sideway stats(coef pval) label ctitle(Parents 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

*Combined Earnings
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_t1 $controls if children_under6==1, or
outreg2 using "$results/dissolution_AMES_familism_parents.xls", sideway stats(coef pval) label ctitle(Parents 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Combined Hours
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children_under6==1, or
outreg2 using "$results/dissolution_AMES_familism_parents.xls", sideway stats(coef pval) label ctitle(Parents 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

/* Interactions */
*Earnings
logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 c.structural_familism_t1#i.hh_earn_type_t1 $controls if children_under6==1 & hh_earn_type_t1 < 4, or
outreg2 using "$results/dissolution_AMES_familism_parents.xls", sideway stats(coef pval) label ctitle(Parents 1a) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum structural_familism_t1, detail
margins, dydx(hh_earn_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_parents.xls", ctitle(Parents 1b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*Hours
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4, or
outreg2 using "$results/dissolution_AMES_familism_parents.xls", sideway stats(coef pval) label ctitle(Parents 2a) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum structural_familism_t1, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_parents.xls", ctitle(Parents 2b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*Combined Earnings
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_t1 c.structural_familism_t1#i.division_bucket_t1 $controls if children_under6==1, or
outreg2 using "$results/dissolution_AMES_familism_parents.xls", sideway stats(coef pval) label ctitle(Parents 3a) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum structural_familism_t1, detail
margins, dydx(division_bucket_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_parents.xls", ctitle(Parents 3b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*Combined Hours
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under6==1, or
outreg2 using "$results/dissolution_AMES_familism_parents.xls", sideway stats(coef pval) label ctitle(Parents 4a) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum structural_familism_t1, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_parents.xls", ctitle(Parents 4b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* First 10 years of marriage (robustness)
logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 c.structural_familism_t1#i.hh_earn_type_t1 $controls if children_under6==1 & hh_earn_type_t1 < 4 & dur<=10, or
// outreg2 using "$results/dissolution_AMES_familism_parents.xls", sideway stats(coef pval) label ctitle(dur) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum structural_familism_t1, detail
margins, dydx(hh_earn_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
// outreg2 using "$results/dissolution_AMES_familism_parents.xls", ctitle(dur) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

/*
* Alt DoL (robustness) - "under"-earners. ignoring for now
logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1_alt c.structural_familism_t1#i.hh_earn_type_t1_alt $controls if children_under6==1, or
outreg2 using "$results/dissolution_AMES_familism_parents.xls", sideway stats(coef pval) label ctitle(Parents 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum structural_familism_t1, detail
margins, dydx(hh_earn_type_t1_alt) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_parents.xls", ctitle(alt dol) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)
*/

// Predicted Probabilities for Key Models
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion"

logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1!=4, or
margins hh_hours_type_t1

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children_under6==1, or
margins division_bucket_hrs_t1
margins, at(structural_familism_t1=(-10(1)15))

********************************************************************************
* All parents
********************************************************************************
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion"  // i.num_children i.region knot1 knot2 knot3 

/* Main Effects */
*Earnings
logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 $controls if children==1 & hh_earn_type_t1!=4, or
outreg2 using "$results/dissolution_AMES_familism_parents_all.xls", sideway stats(coef pval) label ctitle(All Par 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

* Hours
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 $controls if children==1 & hh_hours_type_t1!=4, or
outreg2 using "$results/dissolution_AMES_familism_parents_all.xls", sideway stats(coef pval) label ctitle(All Par 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

*Combined Earnings
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_t1 $controls if children==1, or
outreg2 using "$results/dissolution_AMES_familism_parents_all.xls", sideway stats(coef pval) label ctitle(All Par 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Combined Hours
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children==1, or
outreg2 using "$results/dissolution_AMES_familism_parents_all.xls", sideway stats(coef pval) label ctitle(All Par 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

/* Interactions */
*Earnings
logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 c.structural_familism_t1#i.hh_earn_type_t1 $controls if children==1 & hh_earn_type_t1 < 4, or
outreg2 using "$results/dissolution_AMES_familism_parents_all.xls", sideway stats(coef pval) label ctitle(All Par 1a) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum structural_familism_t1, detail
margins, dydx(hh_earn_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_parents_all.xls", ctitle(All Par 1b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*Hours
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if children==1 & hh_hours_type_t1 < 4, or
outreg2 using "$results/dissolution_AMES_familism_parents_all.xls", sideway stats(coef pval) label ctitle(All Par 2a) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum structural_familism_t1, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_parents_all.xls", ctitle(All Par 2b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*Combined Earnings
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_t1 c.structural_familism_t1#i.division_bucket_t1 $controls if children==1, or
outreg2 using "$results/dissolution_AMES_familism_parents_all.xls", sideway stats(coef pval) label ctitle(All Par 3a) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum structural_familism_t1, detail
margins, dydx(division_bucket_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_parents_all.xls", ctitle(All Par 3b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*Combined Hours
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children==1, or
outreg2 using "$results/dissolution_AMES_familism_parents_all.xls", sideway stats(coef pval) label ctitle(All Par 4a) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum structural_familism_t1, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_parents_all.xls", ctitle(All Par 4b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

********************************************************************************
* Total sample
********************************************************************************
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion i.num_children" 

/* Main Effects */
*Earnings
logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 $controls if hh_earn_type_t1!=4, or
outreg2 using "$results/dissolution_AMES_familism_sample_all.xls", sideway stats(coef pval) label ctitle(All 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

* Hours
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 $controls if hh_hours_type_t1!=4, or
outreg2 using "$results/dissolution_AMES_familism_sample_all.xls", sideway stats(coef pval) label ctitle(All 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

*Combined Earnings
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_t1 $controls, or
outreg2 using "$results/dissolution_AMES_familism_sample_all.xls", sideway stats(coef pval) label ctitle(All 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Combined Hours
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls, or
outreg2 using "$results/dissolution_AMES_familism_sample_all.xls", sideway stats(coef pval) label ctitle(All 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

/* Interactions */
*Earnings
logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 c.structural_familism_t1#i.hh_earn_type_t1 $controls if hh_earn_type_t1 < 4, or
outreg2 using "$results/dissolution_AMES_familism_sample_all.xls", sideway stats(coef pval) label ctitle(All 1a) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum structural_familism_t1, detail
margins, dydx(hh_earn_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_sample_all.xls", ctitle(All 1b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*Hours
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if hh_hours_type_t1 < 4, or
outreg2 using "$results/dissolution_AMES_familism_sample_all.xls", sideway stats(coef pval) label ctitle(All 2a) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum structural_familism_t1, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_sample_all.xls", ctitle(All 2b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*Combined Earnings
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_t1 c.structural_familism_t1#i.division_bucket_t1 $controls, or
outreg2 using "$results/dissolution_AMES_familism_sample_all.xls", sideway stats(coef pval) label ctitle(All 3a) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum structural_familism_t1, detail
margins, dydx(division_bucket_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_sample_all.xls", ctitle(All 3b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*Combined Hours
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls, or
outreg2 using "$results/dissolution_AMES_familism_sample_all.xls", sideway stats(coef pval) label ctitle(All 4a) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum structural_familism_t1, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_sample_all.xls", ctitle(All 4b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

********************************************************************************
********************************************************************************
**# * Figures to use (ASR and SF submission)
********************************************************************************
********************************************************************************

********************************************************************************
* Main Effects
********************************************************************************

local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion" 

logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 $controls if children_under6==1 & hh_earn_type_t1 < 4, or
margins, dydx(hh_earn_type_t1) level(95) post
estimates store est1

logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4, or
margins, dydx(hh_hours_type_t1) level(95) post
estimates store est2

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_t1 $controls if children_under6==1, or
margins, dydx(division_bucket_t1) level(95) post
estimates store est3

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children_under6==1, or
margins, dydx(division_bucket_hrs_t1) level(95) post
estimates store est4

coefplot est1 est2 est3 est4,  drop(_cons) nolabel xline(0) levels(95)

set scheme cleanplots

coefplot (est1, offset(.20) nokey) (est2, offset(.20) nokey) (est3, offset(-.20) nokey) (est4, offset(-.20) nokey), drop(_cons) xline(0) levels(95) base xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) ///
coeflabels(2.hh_earn_type_t1 = "Male Breadwinner" 3.hh_earn_type_t1 = "Female Breadwinner" 4.hh_earn_type_t1 = "No Earners" 2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners"  1.division_bucket_t1 = "Egalitarian" 2.division_bucket_t1 = "Traditional" 3.division_bucket_t1 = "Counter Traditional" 4.division_bucket_t1 = "Her Second Shift" 5.division_bucket_t1 = "All Others" 1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others") ///
 headings(1.hh_earn_type_t1 = "{bf:Division of Earnings}" 1.hh_hours_type_t1= "{bf:Division of Work Hours}"  1.division_bucket_t1 = "{bf:Combined (Earnings)}"  1.division_bucket_hrs_t1 = "{bf:Combined (Hours)}")
 // (est3, offset(-.20) label(College)) 
 // coefplot (est1, offset(.20) nokey lcolor("dkgreen") mcolor("dkgreen") ciopts(color("dkgreen")))
 
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion" 

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children_under6==1, or
margins, at(structural_familism_t1=(-5(1)10))
marginsplot, ytitle(Predicted Probability of Divorce) xtitle(Structural Support for Working Families) plot1opts(lcolor("black") mcolor("black")) ci1opts(color("black"))
margins, dydx(structural_familism_t1) post
estimates store esta

 // just hours and maybe add structural support
coefplot (est2, offset(.20) nokey) (est4, offset(-.20) nokey) (esta, nokey lcolor("black") mcolor("black") ciopts(color("black"))) , drop(_cons) xline(0) levels(95) base xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners"  1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" structural_familism_t1 = "Structural Support Scale") ///
 headings(1.hh_hours_type_t1= `""{bf:Division of Work Hours}" "{it:(Model 1)}""'  1.division_bucket_hrs_t1 = `""{bf:Combined Paid and Unpaid Labor}" "{it:(Model 2)}""' structural_familism_t1 = `""{bf:Structural Support for Working Families}" "{it:(Model 2)}"')

********************************************************************************
* Earnings Interaction
********************************************************************************
/* parents of kids under 6 */
*Predicted Probabilities (no CI) -- see if I can do what Mize 2019 does (Figure 14) - make dotted when not significant
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion"  // i.num_children 

logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 c.structural_familism_t1#i.hh_earn_type_t1 $controls if children_under6==1 & hh_earn_type_t1 < 4, or
sum structural_familism_t1, detail
margins hh_earn_type_t1, at(structural_familism_t1=(`r(min)'(1)`r(max)'))
marginsplot, xtitle("Structural Support for Dual-Earning") ytitle("Predicted Probability of Marital Dissolution") title("") legend(position(6) ring(3) rows(1)) noci recast(line) xlabel(#10) plot2opts(lcolor("navy") mcolor("navy")) plot3opts(lcolor("ltblue") mcolor("ltblue"))   // plot1opts(lcolor("gray") mcolor("gray")) xlabel(-3.12 "5th" -0.64 "25th" 1.27 "50th" 3.57 "75th" 12.48 "95th") ci1opts(color("navy")) ci2opts(color("ltblue")) yscale(range(-.1 .1)) ylabel(-.1(.05).1, angle(0))

* AMEs with CI
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion" 

logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 c.structural_familism_t1#i.hh_earn_type_t1 $controls if children_under6==1 & hh_earn_type_t1 < 4, or
sum structural_familism_t1, detail
margins, dydx(hh_earn_type_t1) at(structural_familism_t1=(`r(min)'(1)`r(max)')) // level(90)
marginsplot, xtitle("Structural Support for Dual-Earning") yline(0,lcolor(red))  ytitle("Average Marginal Effects (relative to dual-earning)") title("") legend(position(6) ring(3) order(1 "Male BW" 2 "Female BW") rows(1)) xlabel(#10) recast(line) recastci(rarea) plot1opts(lcolor("navy") mcolor("navy")) ci1opts(color(navy%70)) plot2opts(lcolor("bluishgray") mcolor("ltblue")) ci2opts(color(ltblue%40)) //  yscale(range(-.1 .1)) ylabel(-.1(.05).1, angle(0))

/* all parents */
*Predicted Probabilities (no CI)
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion" // i.num_children 

logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 c.structural_familism_t1#i.hh_earn_type_t1 $controls if children==1 & hh_earn_type_t1 < 4, or
sum structural_familism_t1, detail
margins hh_earn_type_t1, at(structural_familism_t1=(`r(min)'(1)`r(max)'))
marginsplot, xtitle("Structural Support for Dual-Earning") ytitle("Predicted Probability of Marital Dissolution") title("") legend(position(6) ring(3) rows(1)) noci recast(line) xlabel(#10) plot2opts(lcolor("navy") mcolor("navy")) plot3opts(lcolor("ltblue") mcolor("ltblue")) 

* AMEs with CI
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion" 

logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 c.structural_familism_t1#i.hh_earn_type_t1 $controls if children==1 & hh_earn_type_t1 < 4, or
sum structural_familism_t1, detail
margins, dydx(hh_earn_type_t1) at(structural_familism_t1=(`r(min)'(1)`r(max)')) // level(90)
marginsplot, xtitle("Structural Support for Dual-Earning") yline(0,lcolor(red))  ytitle("Average Marginal Effects (relative to dual-earning)") title("") legend(position(6) ring(3) order(1 "Male BW" 2 "Female BW") rows(1)) xlabel(#10) recast(line) recastci(rarea) plot1opts(lcolor("navy") mcolor("navy")) ci1opts(color(navy%70)) plot2opts(lcolor("bluishgray") mcolor("ltblue")) ci2opts(color(ltblue%40))

/* total sample */
*Predicted Probabilities (no CI)
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion i.num_children" 

logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 c.structural_familism_t1#i.hh_earn_type_t1 $controls if hh_earn_type_t1 < 4, or
sum structural_familism_t1, detail
margins hh_earn_type_t1, at(structural_familism_t1=(`r(min)'(1)`r(max)'))
marginsplot, xtitle("Structural Support for Dual-Earning") ytitle("Predicted Probability of Marital Dissolution") title("") legend(position(6) ring(3) rows(1)) noci recast(line) xlabel(#10) plot2opts(lcolor("navy") mcolor("navy")) plot3opts(lcolor("ltblue") mcolor("ltblue")) 

* AMEs with CI
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion i.num_children" 

logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 c.structural_familism_t1#i.hh_earn_type_t1 $controls if hh_earn_type_t1 < 4, or
sum structural_familism_t1, detail
margins, dydx(hh_earn_type_t1) at(structural_familism_t1=(`r(min)'(1)`r(max)')) // level(90)
marginsplot, xtitle("Structural Support for Dual-Earning") yline(0,lcolor(red))  ytitle("Average Marginal Effects (relative to dual-earning)") title("") legend(position(6) ring(3) order(1 "Male BW" 2 "Female BW") rows(1)) xlabel(#10) recast(line) recastci(rarea) plot1opts(lcolor("navy") mcolor("navy")) ci1opts(color(navy%70)) plot2opts(lcolor("bluishgray") mcolor("ltblue")) ci2opts(color(ltblue%40))

********************************************************************************
**# Hours instead of earnings
********************************************************************************
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion" 

/* parents of kids under 6 */
* Predicted Probabilities
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4, or

sum structural_familism_t1, detail
margins hh_hours_type_t1, at(structural_familism_t1=(`r(min)'(1)`r(max)'))
marginsplot, xtitle("Structural Support for Dual-Earning") ytitle("Predicted Probability of Marital Dissolution") title("") legend(position(6) ring(3) rows(1)) noci recast(line) xlabel(#10) plot2opts(lcolor("navy") mcolor("navy")) plot3opts(lcolor("ltblue") mcolor("ltblue"))  

// AMEs
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion" 

logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4, or

sum structural_familism_t1, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t1=(`r(min)'(1)`r(max)')) // level(90)
marginsplot, xtitle("Structural Support for Dual-Earning") yline(0,lcolor(red))  ytitle("Average Marginal Effects (relative to dual-earning)") title("") legend(position(6) ring(3) order(1 "Male BW" 2 "Female BW") rows(1)) xlabel(#10) recast(line) recastci(rarea) plot1opts(lcolor("navy") mcolor("navy")) ci1opts(color(navy%70)) plot2opts(lcolor("bluishgray") mcolor("ltblue")) ci2opts(color(ltblue%40))

// Test alternate interaction charts
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion" 

logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4, or
sum structural_familism_t1, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est5

logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4, or
sum structural_familism_t1, detail
margins, dydx(3.hh_hours_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est6

coefplot (est5, mcolor(navy) ciopts(color(navy)) label("Male BW")) (est6, label("Female BW")),  drop(_cons) nolabel xline(0) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) headings(1._at = "{bf:Structural Support: Percentiles}", nogap offset(0.30))

coefplot (est5, mcolor(navy) ciopts(color(navy)) label("Male BW")) (est6, label("Female BW")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical))

// trying to get predicted prob
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion" 

logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4, or
sum structural_familism_t1, detail
margins 1.hh_hours_type_t1, at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est7

logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4, or
sum structural_familism_t1, detail
margins 2.hh_hours_type_t1, at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est8

logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4, or
sum structural_familism_t1, detail
margins 3.hh_hours_type_t1, at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est9

logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4, or
sum structural_familism_t1, detail
margins hh_hours_type_t1, at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est10

coefplot est7 est8 est9
coefplot est10

********************************************************************************
**# Combined paid and unpaid labor: based on earnings
********************************************************************************
/* parents of kids under 6 */
* Predicted probabilities
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion" 

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_t1 c.structural_familism_t1#i.division_bucket_t1 $controls if children_under6==1, or
sum structural_familism_t1, detail
margins division_bucket_t1, at(structural_familism_t1=(`r(min)'(1)`r(max)'))
marginsplot, xtitle("Structural Support for Dual-Earning") ytitle("Predicted Probability of Marital Dissolution") title("") legend(position(6) ring(3) rows(1)) noci recast(line) xlabel(#10) plot2opts(lcolor("blue") mcolor("blue")) plot3opts(lcolor("ltblue") mcolor("ltblue")) plot4opts(lcolor("navy") mcolor("navy"))   plot5opts(lcolor("gs8") mcolor("gs8")) 

* AMEs with CI
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion"  

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_t1 c.structural_familism_t1#i.division_bucket_t1 $controls if children_under6==1, or
sum structural_familism_t1, detail
margins, dydx(division_bucket_t1) at(structural_familism_t1=(`r(min)'(1)`r(max)'))
marginsplot, xtitle("Structural Support for Dual-Earning") yline(0,lcolor(red))  ytitle("Average Marginal Effects (relative to dual-earning)") title("") legend(position(6) ring(3) order(1 "Traditional" 2 "Counter" 3 "Second Shift" 4 "Other") rows(1)) xlabel(#10) recast(line)  recastci(rarea) plot1opts(lcolor("blue") mcolor("blue")) ci1opts(color(blue%40)) plot2opts(lcolor("bluishgray") mcolor("ltblue")) ci2opts(color(none)) plot3opts(lcolor("green") mcolor("green")) ci3opts(color(green%40)) plot4opts(lcolor("gs12") mcolor("gs12")) ci4opts(color(none)) // recastci(rarea)


********************************************************************************
**# Combined paid and unpaid labor: based on hours
********************************************************************************
/* parents of kids under 6 */
* Predicted probabilities
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion" 

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under6==1, or // & division_bucket_hrs_t1!=3, or
sum structural_familism_t1, detail
margins division_bucket_hrs_t1, at(structural_familism_t1=(`r(min)'(1)`r(max)'))
marginsplot, xtitle("Structural Support for Dual-Earning") ytitle("Predicted Probability of Marital Dissolution") title("") legend(position(6) ring(3) rows(1)) noci recast(line) xlabel(#10) plot2opts(lcolor("blue") mcolor("blue")) plot3opts(lcolor("ltblue") mcolor("ltblue")) plot4opts(lcolor("gs8") mcolor("gs8")) plot5opts(lcolor("black") mcolor("black"))  

**AMEs with CIs
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion" 

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under6==1 & division_bucket_hrs_t1!=3, or
sum structural_familism_t1, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t1=(`r(min)'(1)`r(max)'))
marginsplot, xtitle("Structural Support for Dual-Earning") yline(0,lcolor(red))  ytitle("Average Marginal Effects (relative to dual-earning)") title("") legend(position(6) ring(3) order(1 "Traditional" 2 "Second Shift" 3 "Other") rows(1)) xlabel(#10) recast(line)  recastci(rarea) plot1opts(lcolor("blue") mcolor("blue")) ci1opts(color(blue%40)) plot2opts(lcolor("green") mcolor("green")) ci2opts(color(green%40)) plot3opts(lcolor("gs12") mcolor("gs12")) ci3opts(color(none)) // recastci(rarea) plot2opts(lcolor("bluishgray") mcolor("ltblue"))

// Test alternate interaction charts
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion" 

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under6==1, or
sum structural_familism_t1, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est10

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under6==1, or
sum structural_familism_t1, detail
margins, dydx(3.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est11

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under6==1, or
sum structural_familism_t1, detail
margins, dydx(4.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est12

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under6==1, or
sum structural_familism_t1, detail
margins, dydx(5.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est13

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under6==1, or
sum structural_familism_t1, detail
margins division_bucket_hrs_t1, at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) 

coefplot (est10, mcolor(blue) ciopts(color(blue)) label("Traditional")) (est11, label("Counter-Traditional")) (est12, label("Second Shift"))  (est13, label("All Others")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical)) xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) legend(position(bottom) rows(1))

********************************************************************************
**# Figures for alternative samples
********************************************************************************
*******************************
* All Parents
*******************************
** Main effects
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion" 

logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 $controls if children==1 & hh_hours_type_t1!=4, or
margins, dydx(hh_hours_type_t1) level(95) post
estimates store p_esta

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children==1 & hh_hours_type_t1!=4, or
margins, dydx(division_bucket_hrs_t1) level(95) post
estimates store p_estb

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children==1 & hh_hours_type_t1!=4, or
margins, dydx(structural_familism_t1) post
estimates store p_estc

* Interaction with Paid Labor
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if children==1 & hh_hours_type_t1 < 4, or
sum structural_familism_t1, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store p_est1

logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if children==1 & hh_hours_type_t1 < 4, or
sum structural_familism_t1, detail
margins, dydx(3.hh_hours_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store p_est2

** Combined DoL (Hours)
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children==1, or
sum structural_familism_t1, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store p_est3

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children==1, or
sum structural_familism_t1, detail
margins, dydx(3.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store p_est4

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children==1, or
sum structural_familism_t1, detail
margins, dydx(4.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store p_est5

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children==1, or
sum structural_familism_t1, detail
margins, dydx(5.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store p_est6

*******************************
* Total Sample
*******************************
** Main effects
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion" 

logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 $controls if hh_hours_type_t1!=4, or
margins, dydx(hh_hours_type_t1) level(95) post
estimates store t_esta

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if hh_hours_type_t1!=4, or
margins, dydx(division_bucket_hrs_t1) level(95) post
estimates store t_estb

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if hh_hours_type_t1!=4, or
margins, dydx(structural_familism_t1) post
estimates store t_estc

* Interaction with Paid Labor
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if hh_hours_type_t1 < 4, or
sum structural_familism_t1, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store t_est1

logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if hh_hours_type_t1 < 4, or
sum structural_familism_t1, detail
margins, dydx(3.hh_hours_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store t_est2

** Combined DoL (Hours)
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls, or
sum structural_familism_t1, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store t_est3

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls, or
sum structural_familism_t1, detail
margins, dydx(3.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store t_est4

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls, or
sum structural_familism_t1, detail
margins, dydx(4.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store t_est5

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls, or
sum structural_familism_t1, detail
margins, dydx(5.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store t_est6

*******************************
* Combined Figures
*******************************
* Main effects
/// coefplot (est2, offset(.20) nokey) (est4, offset(-.20) nokey) (esta, nokey lcolor("black") mcolor("black") ciopts(color("black"))), bylabel("Parents of Young Children")

coefplot (p_esta, offset(.20) nokey) (p_estb, offset(-.20) nokey) (p_estc, nokey lcolor("black") mcolor("black") ciopts(color("black"))), bylabel("All Parents")  || ///
(t_esta, offset(.20) nokey) (t_estb, offset(-.20) nokey) (t_estc, nokey lcolor("black") mcolor("black") ciopts(color("black"))), bylabel("Total Sample") || ///
, drop(_cons) xline(0) levels(95) base xtitle(Average Marginal Effects, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners"  1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" structural_familism_t1 = "Structural Support Scale") xsize(8) ///
 headings(1.hh_hours_type_t1= "{bf:Division of Work Hours}" 1.division_bucket_hrs_t1 = "{bf:Combined DoL}" structural_familism_t1 = "{bf:Structural Support}")
 
* Paid Labor
coefplot (p_est1, mcolor(navy) ciopts(color(navy)) label("Male BW")) (p_est2, label("Female BW")), bylabel("All Parents") || ///
		(t_est1, mcolor(navy) ciopts(color(navy)) label("Male BW")) (t_est2, label("Female BW")), bylabel("Total Sample") || ///
,  drop(_cons) nolabel xline(0, lcolor("red")) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical)) xsize(8)

* Combined p_est3
coefplot (p_est3, mcolor(blue) ciopts(color(blue)) label("Traditional")) (p_est4, label("Counter-Traditional")) (p_est5, label("Second Shift"))  (p_est6, label("All Others")), bylabel("All Parents") || ///
(t_est3, mcolor(blue) ciopts(color(blue)) label("Traditional")) (t_est4, label("Counter-Traditional")) (t_est5, label("Second Shift") mcolor(gs8) ciopts(color(gs8)))  (t_est6, label("All Others") mcolor(black) ciopts(color(black))), bylabel("Total Sample")  || ///
,  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical)) xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) legend(position(bottom) rows(1)) xsize(8)


********************************************************************************
********************************************************************************
**# Individual indicators of scale
********************************************************************************
********************************************************************************
// scale, for reference: paid_leave_st prek_enrolled_public_st min_amt_above_fed_st earn_ratio_neg_st unemployment_percap_st abortion_protected_st welfare_all_st

*****************
*Paid Work Hours
*****************
global controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion"  // i.num_children i.region knot1 knot2 knot3 

* Main model
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4, or
sum structural_familism_t1, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_scale_details.xls", ctitle(Parents main) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace
estimates store hrs_est1

* Paid Leave
logit dissolve i.dur i.paid_leave_t1 i.hh_hours_type_t1 i.paid_leave_t1#i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4, or
margins, dydx(2.hh_hours_type_t1) at(paid_leave_t1=(0 1)) post // had to update bc #3 is collinnear
outreg2 using "$results/dissolution_AMES_scale_details.xls", ctitle(parent paidleave) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
estimates store hrs_est2

* PreK Enrollment
logit dissolve i.dur c.prek_enrolled_public_t1 i.hh_hours_type_t1 c.prek_enrolled_public_t1#i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4, or
sum prek_enrolled_public_t1, detail
margins, dydx(hh_hours_type_t1) at(prek_enrolled_public_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_scale_details.xls", ctitle(parent prek) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
estimates store hrs_est3

* Min Wage
logit dissolve i.dur c.min_amt_above_fed_t1 i.hh_hours_type_t1 c.min_amt_above_fed_t1#i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4, or
sum min_amt_above_fed_t1, detail
margins, dydx(hh_hours_type_t1) at(min_amt_above_fed_t1=(`r(p5)' `r(p50)' `r(p75)' `r(p95)' `r(p99)')) post
outreg2 using "$results/dissolution_AMES_scale_details.xls", ctitle(parent minwage) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
estimates store hrs_est4

* Earnings Ratio
logit dissolve i.dur c.earn_ratio_neg_t1 i.hh_hours_type_t1 c.earn_ratio_neg_t1#i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4, or
sum earn_ratio_neg_t1, detail
margins, dydx(hh_hours_type_t1) at(earn_ratio_neg_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_scale_details.xls", ctitle(parent earnings) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
estimates store hrs_est5

* Unemployment Compensation
logit dissolve i.dur c.unemployment_percap_t1 i.hh_hours_type_t1 c.unemployment_percap_t1#i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4, or
sum unemployment_percap_t1, detail
margins, dydx(hh_hours_type_t1) at(unemployment_percap_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_scale_details.xls", ctitle(parent unemployment) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
estimates store hrs_est6

* Abortion protected
logit dissolve i.dur i.abortion_protected_t1 i.hh_hours_type_t1 i.abortion_protected_t1#i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4, or
// margins, dydx(2.hh_hours_type_t1) at(abortion_protected_t1=(0 1)) post // had to update bc #3 is collinnear
margins, dydx(hh_hours_type_t1) at(abortion_protected_t1=(0 1)) post // had to update bc #3 is collinnear
outreg2 using "$results/dissolution_AMES_scale_details.xls", ctitle(parent abortion) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
estimates store hrs_est7

* Welfare Expenditures
logit dissolve i.dur c.welfare_all_t1 i.hh_hours_type_t1 c.welfare_all_t1#i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4, or
sum welfare_all_t1, detail
margins, dydx(hh_hours_type_t1) at(welfare_all_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_scale_details.xls", ctitle(parent welfare) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
estimates store hrs_est8

// Percentiles
coefplot hrs_est1, bylabel("Structural Support") || hrs_est3, bylabel("Pre-K Enrollment")  || hrs_est4, bylabel("Relative Minimum Wage")  ///
		|| hrs_est5, bylabel("Earnings Ratio")  || hrs_est6, bylabel("Unemployment Compensation")  || hrs_est8, bylabel("Welfare Expenditures") ||,  ///
		drop(_cons) nolabel xline(0, lcolor("red"))  levels(95) ///
		coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") ///	
		xtitle(Average Marginal Effects of Male-BW Relative to Dual-Earning, size(small)) ///
		mcolor(navy) ciopts(color(navy))

// 0 and 1
coefplot hrs_est2, bylabel("Paid Leave") || hrs_est7, bylabel("Abortion Protected") ||, ///
		drop(_cons) nolabel xline(0, lcolor("red"))  levels(95) ///
		coeflabels(1._at = "No" 2._at = "Yes") ///	
		xtitle(Average Marginal Effects of Male-BW Relative to Dual-Earning, size(small)) ///
		mcolor(navy) ciopts(color(navy)) byopts(rows(2)) xsize(3) 

*****************
*Combined DoL
*****************
* Main Model
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under6==1, or
sum structural_familism_t1, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_scale_details.xls", ctitle(Parents 4b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
estimates store dol_est1

* Paid Leave
logit dissolve i.dur i.paid_leave_t1 i.division_bucket_hrs_t1 i.paid_leave_t1#i.division_bucket_hrs_t1 $controls if children_under6==1, or
margins, dydx(2.division_bucket_hrs_t1 4.division_bucket_hrs_t1 5.division_bucket_hrs_t1) at(paid_leave_t1=(0 1)) post
outreg2 using "$results/dissolution_AMES_scale_details.xls", ctitle(parent paidleave) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
estimates store dol_est2

* PreK Enrollment
logit dissolve i.dur c.prek_enrolled_public_t1 i.division_bucket_hrs_t1 c.prek_enrolled_public_t1#i.division_bucket_hrs_t1 $controls if children_under6==1, or
sum prek_enrolled_public_t1, detail
margins, dydx(division_bucket_hrs_t1) at(prek_enrolled_public_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_scale_details.xls", ctitle(parent prek) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
estimates store dol_est3

* Min Wage
logit dissolve i.dur c.min_amt_above_fed_t1 i.division_bucket_hrs_t1 c.min_amt_above_fed_t1#i.division_bucket_hrs_t1 $controls if children_under6==1, or
sum min_amt_above_fed_t1, detail
margins, dydx(division_bucket_hrs_t1) at(min_amt_above_fed_t1=(`r(p5)' `r(p50)' `r(p75)' `r(p95)' `r(p99)')) post
outreg2 using "$results/dissolution_AMES_scale_details.xls", ctitle(parent minwage) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
estimates store dol_est4

* Earnings Ratio
logit dissolve i.dur c.earn_ratio_neg_t1 i.division_bucket_hrs_t1 c.earn_ratio_neg_t1#i.division_bucket_hrs_t1 $controls if children_under6==1, or
sum earn_ratio_neg_t1, detail
margins, dydx(division_bucket_hrs_t1) at(earn_ratio_neg_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_scale_details.xls", ctitle(parent earnings) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
estimates store dol_est5

* Unemployment Compensation
logit dissolve i.dur c.unemployment_percap_t1 i.division_bucket_hrs_t1 c.unemployment_percap_t1#i.division_bucket_hrs_t1 $controls if children_under6==1, or
sum unemployment_percap_t1, detail
margins, dydx(division_bucket_hrs_t1) at(unemployment_percap_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_scale_details.xls", ctitle(parent unemployment) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
estimates store dol_est6

* Abortion protected
logit dissolve i.dur i.abortion_protected_t1 i.division_bucket_hrs_t1 i.abortion_protected_t1#i.division_bucket_hrs_t1 $controls if children_under6==1, or
margins, dydx(2.division_bucket_hrs_t1 4.division_bucket_hrs_t1 5.division_bucket_hrs_t1) at(abortion_protected_t1=(0 1)) post // had to update bc #3 is collinnear
outreg2 using "$results/dissolution_AMES_scale_details.xls", ctitle(parent abortion) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
estimates store dol_est7

* Welfare Expenditures
logit dissolve i.dur c.welfare_all_t1 i.division_bucket_hrs_t1 c.welfare_all_t1#i.division_bucket_hrs_t1 $controls if children_under6==1, or
sum welfare_all_t1, detail
margins, dydx(division_bucket_hrs_t1) at(welfare_all_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_scale_details.xls", ctitle(parent welfare) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
estimates store dol_est8

// Percentiles
coefplot dol_est1, bylabel("Structural Support") || dol_est3, bylabel("Pre-K Enrollment")  || dol_est4, bylabel("Relative Minimum Wage")  ///
		|| dol_est5, bylabel("Earnings Ratio")  || dol_est6, bylabel("Unemployment Compensation")  || dol_est8, bylabel("Welfare Expenditures") ||,  ///
		drop(_cons) nolabel xline(0, lcolor("red"))  levels(95) ///
		coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") ///	
		xtitle(Average Marginal Effects of Traditional Relative to Egalitarian, size(small)) ///
		mcolor(blue) ciopts(color(blue))

// 0 and 1
coefplot dol_est2, bylabel("Paid Leave") || dol_est7, bylabel("Abortion Protected") ||, ///
		drop(_cons) nolabel xline(0, lcolor("red"))  levels(95) ///
		coeflabels(1._at = "No" 2._at = "Yes") ///	
		xtitle(Average Marginal Effects of Traditional Relative to Egalitarian, size(small)) ///
		mcolor(blue) ciopts(color(blue)) byopts(rows(2)) xsize(3) 

********************************************************************************
********************************************************************************
**# Results by level of education
********************************************************************************
********************************************************************************

*******************************
* College
*******************************
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion" 

/* Main Effects */
* Hours
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1!=4 & couple_educ_gp==1, or
outreg2 using "$results/dissolution_AMES_familism_educ.xls", sideway stats(coef pval) label ctitle(Coll 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

* Combined Hours
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children_under6==1 & hh_hours_type_t1!=4 & couple_educ_gp==1, or
outreg2 using "$results/dissolution_AMES_familism_educ.xls", sideway stats(coef pval) label ctitle(Coll 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

/* Interactions */
*Hours
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4  & couple_educ_gp==1, or
outreg2 using "$results/dissolution_AMES_familism_educ.xls", sideway stats(coef pval) label ctitle(Coll 1a) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum structural_familism_t1, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_educ.xls", ctitle(Coll 1b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*Combined Hours
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under6==1  & hh_hours_type_t1!=4 & couple_educ_gp==1, or
outreg2 using "$results/dissolution_AMES_familism_educ.xls", sideway stats(coef pval) label ctitle(Coll 2a) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum structural_familism_t1, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_educ.xls", ctitle(Coll 2b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*******************************
* No College
*******************************
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion" 

/* Main Effects */
* Hours
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1!=4 & couple_educ_gp==0, or
outreg2 using "$results/dissolution_AMES_familism_educ.xls", sideway stats(coef pval) label ctitle(No Coll 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Combined Hours
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children_under6==1 & hh_hours_type_t1!=4 & couple_educ_gp==0, or
outreg2 using "$results/dissolution_AMES_familism_educ.xls", sideway stats(coef pval) label ctitle(No Coll 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

/* Interactions */
*Hours
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4  & couple_educ_gp==0, or
outreg2 using "$results/dissolution_AMES_familism_educ.xls", sideway stats(coef pval) label ctitle(No Coll 1a) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum structural_familism_t1, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_educ.xls", ctitle(No Coll 1b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*Combined Hours
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under6==1  & hh_hours_type_t1!=4 & couple_educ_gp==0, or
outreg2 using "$results/dissolution_AMES_familism_educ.xls", sideway stats(coef pval) label ctitle(No Coll 2a) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum structural_familism_t1, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_educ.xls", ctitle(No Coll 2b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)


********************************************************************************
* Figures
********************************************************************************
*******************************
* College
*******************************
** Main effects
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion" 

logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1!=4 & couple_educ_gp==1, or
margins, dydx(hh_hours_type_t1) level(95) post
estimates store c_esta

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children_under6==1 & hh_hours_type_t1!=4 & couple_educ_gp==1, or
margins, dydx(division_bucket_hrs_t1) level(95) post
estimates store c_estb

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children_under6==1 & hh_hours_type_t1!=4 & couple_educ_gp==1, or
margins, dydx(structural_familism_t1) post
estimates store c_estc

 // just hours and maybe add structural support
coefplot (c_esta, offset(.20) nokey) (c_estb, offset(-.20) nokey) (c_estc, nokey lcolor("black") mcolor("black") ciopts(color("black"))) , drop(_cons) xline(0) levels(95) base xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners"  1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" structural_familism_t1 = "Structural Support Scale") ///
 headings(1.hh_hours_type_t1= `""{bf:Division of Work Hours}" "{it:(Model 1)}""'  1.division_bucket_hrs_t1 = `""{bf:Combined Paid and Unpaid Labor}" "{it:(Model 2)}""' structural_familism_t1 = `""{bf:Structural Support for Working Families}" "{it:(Model 2)}"')
 
** Hours
// Predicted Probabilities
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion" 

logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4 & couple_educ_gp==1, or

sum structural_familism_t1, detail
margins hh_hours_type_t1, at(structural_familism_t1=(`r(min)'(1)`r(max)'))
marginsplot, xtitle("Structural Support for Dual-Earning") ytitle("Predicted Probability of Marital Dissolution") title("") legend(position(6) ring(3) rows(1)) noci recast(line) xlabel(#10) plot2opts(lcolor("navy") mcolor("navy")) plot3opts(lcolor("ltblue") mcolor("ltblue"))  

// AMEs
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion" 

logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4 & couple_educ_gp==1, or

sum structural_familism_t1, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t1=(`r(min)'(1)`r(max)')) // level(90)
marginsplot, xtitle("Structural Support for Dual-Earning") yline(0,lcolor(red))  ytitle("Average Marginal Effects (relative to dual-earning)") title("") legend(position(6) ring(3) order(1 "Male BW" 2 "Female BW") rows(1)) xlabel(#10) recast(line) recastci(rarea) plot1opts(lcolor("navy") mcolor("navy")) ci1opts(color(navy%70)) plot2opts(lcolor("bluishgray") mcolor("ltblue")) ci2opts(color(ltblue%40))

// Test alternate interaction charts
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion" 

logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4 & couple_educ_gp==1, or
sum structural_familism_t1, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est5c

logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4 & couple_educ_gp==1, or
sum structural_familism_t1, detail
margins, dydx(3.hh_hours_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est6c

coefplot (est5c, mcolor(navy) ciopts(color(navy)) label("Male BW")) (est6c, label("Female BW")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical))

** Combined DoL (Hours)
// Test alternate interaction charts
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion" 

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under6==1 & couple_educ_gp==1, or
sum structural_familism_t1, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est10c

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under6==1 & couple_educ_gp==1, or
sum structural_familism_t1, detail
margins, dydx(3.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est11c

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under6==1 & couple_educ_gp==1, or
sum structural_familism_t1, detail
margins, dydx(4.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est12c

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under6==1 & couple_educ_gp==1, or
sum structural_familism_t1, detail
margins, dydx(5.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est13c

coefplot (est10c, mcolor(blue) ciopts(color(blue)) label("Traditional")) (est12c, label("Second Shift") mcolor(gs8) ciopts(color(gs8)))  (est13c, label("All Others") mcolor(black) ciopts(color(black))),  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical)) xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) legend(position(bottom) rows(1)) // (est11c, label("Counter-Traditional"))


*******************************
* No College
*******************************
** Main effects
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion" 

logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1!=4 & couple_educ_gp==0, or
margins, dydx(hh_hours_type_t1) level(95) post
estimates store n_esta

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children_under6==1 & hh_hours_type_t1!=4 & couple_educ_gp==0, or
margins, dydx(division_bucket_hrs_t1) level(95) post
estimates store n_estb

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children_under6==1 & hh_hours_type_t1!=4 & couple_educ_gp==0, or
margins, dydx(structural_familism_t1) post
estimates store n_estc

 // just hours and maybe add structural support
coefplot (n_esta, offset(.20) nokey) (n_estb, offset(-.20) nokey) (n_estc, nokey lcolor("black") mcolor("black") ciopts(color("black"))) , drop(_cons) xline(0) levels(95) base xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners"  1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" structural_familism_t1 = "Structural Support Scale") ///
 headings(1.hh_hours_type_t1= `""{bf:Division of Work Hours}" "{it:(Model 1)}""'  1.division_bucket_hrs_t1 = `""{bf:Combined Paid and Unpaid Labor}" "{it:(Model 2)}""' structural_familism_t1 = `""{bf:Structural Support for Working Families}" "{it:(Model 2)}"')
 
** Hours
// Predicted Probabilities
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion" 

logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4 & couple_educ_gp==0, or

sum structural_familism_t1, detail
margins hh_hours_type_t1, at(structural_familism_t1=(`r(min)'(1)`r(max)'))
marginsplot, xtitle("Structural Support for Dual-Earning") ytitle("Predicted Probability of Marital Dissolution") title("") legend(position(6) ring(3) rows(1)) noci recast(line) xlabel(#10) plot2opts(lcolor("navy") mcolor("navy")) plot3opts(lcolor("ltblue") mcolor("ltblue"))  

// AMEs
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion" 

logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4 & couple_educ_gp==0, or

sum structural_familism_t1, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t1=(`r(min)'(1)`r(max)')) // level(90)
marginsplot, xtitle("Structural Support for Dual-Earning") yline(0,lcolor(red))  ytitle("Average Marginal Effects (relative to dual-earning)") title("") legend(position(6) ring(3) order(1 "Male BW" 2 "Female BW") rows(1)) xlabel(#10) recast(line) recastci(rarea) plot1opts(lcolor("navy") mcolor("navy")) ci1opts(color(navy%70)) plot2opts(lcolor("bluishgray") mcolor("ltblue")) ci2opts(color(ltblue%40))

// Test alternate interaction charts
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion" 

logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4 & couple_educ_gp==0, or
sum structural_familism_t1, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est5n

logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4 & couple_educ_gp==0, or
sum structural_familism_t1, detail
margins, dydx(3.hh_hours_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est6n

coefplot (est5n, mcolor(navy) ciopts(color(navy)) label("Male BW")) (est6n, label("Female BW")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical))

** Combined DoL (Hours)
// Test alternate interaction charts
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion" 

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under6==1 & couple_educ_gp==0, or
sum structural_familism_t1, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est10n

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under6==1 & couple_educ_gp==0, or
sum structural_familism_t1, detail
margins, dydx(3.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est11n

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under6==1 & couple_educ_gp==0, or
sum structural_familism_t1, detail
margins, dydx(4.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est12n

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under6==1 & couple_educ_gp==0, or
sum structural_familism_t1, detail
margins, dydx(5.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est13n

coefplot (est10n, mcolor(blue) ciopts(color(blue)) label("Traditional")) (est11n, label("Counter-Traditional")) (est12n, label("Second Shift"))  (est13n, label("All Others")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical)) xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) legend(position(bottom) rows(1))

*******************************
* Combined Figures
*******************************
* Main effects
coefplot (n_esta, offset(.20) nokey) (n_estb, offset(-.20) nokey) (n_estc, nokey lcolor("black") mcolor("black") ciopts(color("black"))), bylabel("No College Degree")  || ///
(c_esta, offset(.20) nokey) (c_estb, offset(-.20) nokey) (c_estc, nokey lcolor("black") mcolor("black") ciopts(color("black"))), bylabel("One Partner Has College Degree") || ///
, drop(_cons) xline(0) levels(95) base xtitle(Average Marginal Effects, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners"  1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" structural_familism_t1 = "Structural Support Scale") xsize(8) ///
 headings(1.hh_hours_type_t1= "{bf:Division of Work Hours}" 1.division_bucket_hrs_t1 = "{bf:Combined DoL}" structural_familism_t1 = "{bf:Structural Support}")
 
* Paid Labor
coefplot (est5n, mcolor(navy) ciopts(color(navy)) label("Male BW")) (est6n, label("Female BW")), bylabel("No College Degree") || ///
		(est5c, mcolor(navy) ciopts(color(navy)) label("Male BW")) (est6c, label("Female BW")), bylabel("One Partner Has College  Degree") || ///
,  drop(_cons) nolabel xline(0, lcolor("red")) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical)) xsize(8)

* Combined DoL
coefplot (est10n, mcolor(blue) ciopts(color(blue)) label("Traditional")) (est11n, label("Counter-Traditional")) (est12n, label("Second Shift"))  (est13n, label("All Others")), bylabel("No College Degree") || ///
(est10c, mcolor(blue) ciopts(color(blue)) label("Traditional")) (est11c, label("Counter-Traditional")) (est12c, label("Second Shift") mcolor(gs8) ciopts(color(gs8)))  (est13c, label("All Others") mcolor(black) ciopts(color(black))), bylabel("One Partner Has College  Degree")  || ///
,  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical)) xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) legend(position(bottom) rows(1)) xsize(8)


********************************************************************************
********************************************************************************
**# Other analyses / metrics
********************************************************************************
********************************************************************************

********************************************************************************
* Median centered
********************************************************************************
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner knot1 knot2 knot3 i.couple_educ_gp"  // i.num_children i.region

/* Parents */
*Baseline model
logit dissolve i.dur c.sf_centered_alt i.hh_earn_type $controls if children_under6==1 & hh_earn_type < 4, or
outreg2 using "$results/dissolution_AMES_familism_centered.xls", sideway stats(coef pval) label ctitle(Parents 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

* Structural familism
logit dissolve i.dur c.sf_centered_alt i.hh_earn_type c.sf_centered_alt#i.hh_earn_type $controls if children_under6==1 & hh_earn_type < 4, or
outreg2 using "$results/dissolution_AMES_familism_centered.xls", sideway stats(coef pval) label ctitle(Parents 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum sf_centered_alt, detail
margins, dydx(hh_earn_type) at(sf_centered_alt=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_centered.xls", ctitle(parent familism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* All parents (robustness)
logit dissolve i.dur c.sf_centered_alt i.hh_earn_type c.sf_centered_alt#i.hh_earn_type $controls if children==1 & hh_earn_type < 4, or
sum sf_centered_alt, detail
margins, dydx(hh_earn_type) at(sf_centered_alt=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_centered.xls", ctitle(allparent familism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Full sample (robustness)
logit dissolve i.dur c.sf_centered_alt i.hh_earn_type c.sf_centered_alt#i.hh_earn_type i.num_children $controls if hh_earn_type < 4, or
sum sf_centered_alt, detail
margins, dydx(hh_earn_type) at(sf_centered_alt=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_centered.xls", ctitle(all familism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

********************************************************************************
* Mixed effects
* Not using because they are exactly the same 
********************************************************************************
/*
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.race_head i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner knot1 knot2 knot3 i.couple_educ_gp"  // i.num_children i.region

/* Mixed effects */
*Baseline model
melogit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 $controls if children_under6==1 & hh_earn_type_t1 < 4 || state_fips: , or
outreg2 using "$results/dissolution_AMES_melogit.xls", sideway stats(coef pval) label ctitle(Parents 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

* Structural familism
melogit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 c.structural_familism_t1#i.hh_earn_type_t1 $controls if children_under6==1 & hh_earn_type_t1 < 4 || state_fips: , or
outreg2 using "$results/dissolution_AMES_melogit.xls", sideway stats(coef pval) label ctitle(Parents 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum structural_familism_t1, detail
margins, dydx(hh_earn_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_melogit.xls", ctitle(parent familism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Paid Leave
melogit dissolve i.dur i.paid_leave i.hh_earn_type_t1 i.paid_leave#i.hh_earn_type_t1 $controls if children_under6==1 & hh_earn_type_t1 < 4 || state_fips: , or
margins, dydx(hh_earn_type_t1) at(paid_leave=(0 1)) post
outreg2 using "$results/dissolution_AMES_melogit.xls", ctitle(parent paidleave) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* PreK Enrollment
melogit dissolve i.dur c.prek_enrolled_public i.hh_earn_type_t1 c.prek_enrolled_public#i.hh_earn_type_t1 $controls if children_under6==1 & hh_earn_type_t1 < 4 || state_fips: , or
sum prek_enrolled_public, detail
margins, dydx(hh_earn_type_t1) at(prek_enrolled_public=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_melogit.xls", ctitle(parent prek) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Min Wage
melogit dissolve i.dur i.min_above_fed i.hh_earn_type_t1 i.min_above_fed#i.hh_earn_type_t1 $controls if children_under6==1 & hh_earn_type_t1 < 4 || state_fips: , or
margins, dydx(hh_earn_type_t1) at(min_above_fed=(0 1)) post
outreg2 using "$results/dissolution_AMES_melogit.xls", ctitle(parent minwage) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Earnings Ratio
melogit dissolve i.dur c.earn_ratio i.hh_earn_type_t1 c.earn_ratio#i.hh_earn_type_t1 $controls if children_under6==1 & hh_earn_type_t1 < 4 || state_fips: , or
sum earn_ratio, detail
margins, dydx(hh_earn_type_t1) at(earn_ratio=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_melogit.xls", ctitle(parent earnings) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Unemployment Compensation
melogit dissolve i.dur c.unemployment_percap i.hh_earn_type_t1 c.unemployment_percap#i.hh_earn_type_t1 $controls if children_under6==1 & hh_earn_type_t1 < 4 || state_fips: , or
sum unemployment_percap, detail
margins, dydx(hh_earn_type_t1) at(unemployment_percap=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_melogit.xls", ctitle(parent unemployment) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Abortion protected
melogit dissolve i.dur i.abortion_protected i.hh_earn_type_t1 i.abortion_protected#i.hh_earn_type_t1 $controls if children_under6==1 & hh_earn_type_t1 < 4 || state_fips: , or
margins, dydx(2.hh_earn_type_t1) at(abortion_protected=(0 1)) post // had to update bc #3 is collinnear
outreg2 using "$results/dissolution_AMES_melogit.xls", ctitle(parent abortion) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Welfare Expenditures
melogit dissolve i.dur c.welfare_all i.hh_earn_type_t1 c.welfare_all#i.hh_earn_type_t1 $controls if children_under6==1 & hh_earn_type_t1 < 4 || state_fips: , or
sum welfare_all, detail
margins, dydx(hh_earn_type_t1) at(welfare_all=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_melogit.xls", ctitle(parent welfare) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* All parents (robustness)
melogit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 c.structural_familism_t1#i.hh_earn_type_t1 $controls if children==1 & hh_earn_type_t1 < 4 || state_fips: , or
sum structural_familism_t1, detail
margins, dydx(hh_earn_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_melogit.xls", ctitle(allparent familism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Full sample (robustness)
melogit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 c.structural_familism_t1#i.hh_earn_type_t1 i.num_children $controls if hh_earn_type_t1 < 4 || state_fips: , or
sum structural_familism_t1, detail
margins, dydx(hh_earn_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_melogit.xls", ctitle(all familism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* First 10 years of marriage (robustness)
melogit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 c.structural_familism_t1#i.hh_earn_type_t1 $controls if children_under6==1 & hh_earn_type_t1 < 4 & dur<=10 || state_fips: , or
outreg2 using "$results/dissolution_AMES_melogit.xls", sideway stats(coef pval) label ctitle(dur) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum structural_familism_t1, detail
margins, dydx(hh_earn_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_melogit.xls", ctitle(dur) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Alt DoL (robustness)
melogit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1_alt c.structural_familism_t1#i.hh_earn_type_t1_alt $controls if children_under6==1 || state_fips: , or
outreg2 using "$results/dissolution_AMES_melogit.xls", sideway stats(coef pval) label ctitle(Parents 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum structural_familism_t1, detail
margins, dydx(hh_earn_type_t1_alt) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_melogit.xls", ctitle(alt dol) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)
*/

********************************************************************************
********************************************************************************
**# Descriptive statistics
********************************************************************************
********************************************************************************
// for ref: local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_partner cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion" // i.num_children

tab hh_earn_type_t1, gen(earn_type)
tab hh_hours_type_t1, gen(hours_type)
// tab division_bucket_t1, gen(combined_earn)
tab division_bucket_hrs_t1, gen(combined_hours)
tab raceth_head_fixed, gen(race_head)
tab couple_joint_religion, gen(religion)

putexcel set "$results/Table1_Descriptives_chapter3", replace
putexcel B1:C1 = "Parents of children under 6", merge border(bottom)
putexcel D1:E1 = "Total Sample", merge border(bottom)
putexcel B2 = ("All") C2 = ("Dissolved") D2 = ("All") E2 = ("Dissolved")
putexcel A3 = "Unique Couples"

putexcel A4 = "Dual Earning HH (Hours)"
putexcel A5 = "Male Breadwinner (Hours)"
putexcel A6 = "Female Breadwinner (Hours)"
putexcel A7 = "Dual Earning HH ($)"
putexcel A8 = "Male Breadwinner ($)"
putexcel A9 = "Female Breadwinner ($)"
putexcel A10 = "Egalitarian"
putexcel A11 = "Traditional"
putexcel A12 = "Counter-Traditional"
putexcel A13 = "Second Shift"
putexcel A14 = "All Other"
putexcel A15 = "Structural Support for Working Families"
putexcel A16 = "Relationship Duration"
putexcel A17 = "Husband's age at marriage"
putexcel A18 = "Wife's age at marriage"
putexcel A19 = "Total Couple Earnings"
putexcel A20 = "At least one partner has college degree"
putexcel A21 = "Couple owns home"
putexcel A22 = "Husband's Race: NH White"
putexcel A23 = "Husband's Race: Black"
putexcel A24 = "Husband's Race: Hispanic"
putexcel A25 = "Husband's Race: NH Asian"
putexcel A26 = "Husband's Race: NH Other"
putexcel A27 = "Husband and wife same race"
putexcel A28 = "Either partner enrolled in school"
putexcel A29 = "Husband Wife Cohabit"
putexcel A30 = "Other Premarital Cohabit"
putexcel A31 = "First Birth Premarital"
putexcel A32 = "Religion: Both No Religion"
putexcel A33 = "Religion: Both Catholic"
putexcel A34 = "Religion: Both Protestant"
putexcel A35 = "Religion: One Catholic"
putexcel A36 = "Religion: One No Religion"
putexcel A37 = "Religion: Other"
putexcel A38 = "Moved Within 2 Survey Waves"

local meanvars "hours_type1 hours_type2 hours_type3 earn_type1 earn_type2 earn_type3 combined_hours1 combined_hours2 combined_hours3 combined_hours4 combined_hours5 structural_familism_t1 dur age_mar_head age_mar_wife couple_earnings_t1 couple_educ_gp home_owner race_head1 race_head2 race_head3 race_head4 race_head5 same_race either_enrolled cohab_with_partner cohab_with_other pre_marital_birth religion1 religion2 religion3 religion4 religion5 religion6 moved_last2"

// Parents
forvalues w=1/35{
	local row=`w'+3
	local var: word `w' of `meanvars'
	mean `var' if children_under6==1
	matrix t`var'= e(b)
	putexcel B`row' = matrix(t`var'), nformat(#.#%)
}

// those who dissolved; value when dissolve==1
forvalues w=1/35{
	local row=`w'+3
	local var: word `w' of `meanvars' 
	mean `var' if dissolve==1 & children_under6==1
	matrix t`var'= e(b)
	putexcel C`row' = matrix(t`var'), nformat(#.#%)
}


// All couples
forvalues w=1/35{
	local row=`w'+3
	local var: word `w' of `meanvars'
	mean `var'
	matrix t`var'= e(b)
	putexcel D`row' = matrix(t`var'), nformat(#.#%)
}

// those who dissolved; value when dissolve==1
forvalues w=1/35{
	local row=`w'+3
	local var: word `w' of `meanvars'
	mean `var' if dissolve==1
	matrix t`var'= e(b)
	putexcel E`row' = matrix(t`var'), nformat(#.#%)
}

unique unique_id if children_under6==1
unique unique_id if children_under6==1 & dissolve==1
unique unique_id 
unique unique_id if dissolve==1
