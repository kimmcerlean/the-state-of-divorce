********************************************************************************
* Edit 9/10/25
* Also archiving this file. This is for SF revision, but this was JUST exploring
* robustness for married sample. These are now integrated into new main files
* that include cohbitation

********************************************************************************
* Robustness checks on main analysis (for SF revision)
* robustness_checks.do
* Kim McErlean
********************************************************************************
// ssc install marginscontplot - have to make sure you have this pckage

********************************************************************************
* First just get data and do all the restrictions that are in file 3
********************************************************************************

use "$created_data/PSID_marriage_recoded_sample.dta", clear // created in step 3 in main folder

gen cohort=.
replace cohort=1 if inrange(rel_start_all,1969,1979)
replace cohort=2 if inrange(rel_start_all,1980,1989)
replace cohort=3 if inrange(rel_start_all,1990,2010)
replace cohort=4 if inrange(rel_start_all,2011,2021)

// Final sample restrictions
// keep if cohort==3, need to just use filters so I don't have to keep using and updating the data
// need to decide - ALL MARRIAGES or just first? - killewald restricts to just first, so does cooke. My validation is MUCH BETTER against those with first marraiges only...
tab matrix_marr_num, m
tab matrix_rel_num, m
tab relationship_order, m // more accurate

// keep if matrix_marr_num==1
keep if relationship_order==1
keep if (AGE_HEAD_>=18 & AGE_HEAD_<=55) &  (AGE_WIFE_>=18 & AGE_WIFE_<=55)

gen age_flag_2554=0
replace age_flag_2554=1 if (AGE_HEAD_>=25 & AGE_HEAD_<=54) &  (AGE_WIFE_>=25 & AGE_WIFE_<=54)

gen age_flag_2055=0
replace age_flag_2055=1 if (AGE_HEAD_>=20 & AGE_HEAD_<=55) &  (AGE_WIFE_>=20 & AGE_WIFE_<=55)

keep if inrange(rel_start_all,1995,2014) // extend to 2016? if I keep 2021?
keep if inlist(IN_UNIT,0,1,2)
drop if survey_yr==2021 // until I figure out what to do about covid year. I did add the policy measures, now need to figure out if it makes sense to keep for other reasons
drop if STATE_==11 // DC is missing a lot of state variables, so need to remove.
drop if STATE_==0
drop if STATE_==99

// final check of sample / end dates BUT this won't work when biennial?
tab rel_end_all status_all,m 
tab rel_end_all dissolve,m col
tab end_year status_all, m

sort unique_id survey_yr
browse unique_id partner_unique_id survey_yr rel_start_all rel_end_all end_year status_all yr_end1 status1 yr_end2 status2 dur dissolve dissolve_v0 ever_dissolve

gen outcome = dissolve
replace outcome=1 if dissolve==0 & survey_yr == (rel_end_all-1) & inlist(rel_end_all,1998,2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2020) // I do think biennial are causing problems.
replace outcome = 0 if dissolve==1 & survey_yr !=rel_end_all & survey_yr!=(rel_end_all-1)

tab outcome dissolve, m
tab rel_end_all status_all, m col
unique unique_id if inlist(status_all,4,5) // 841 here 
unique unique_id if outcome==1 // 651 here. but not all couples observed in their year of breakup which I think is the problem 
unique unique_id if dissolve==1 // 838 here. This is like, I did adjust for them not being observed together in last year, so this s the final year observed together, hence why there are more. in outcome, they are not accounted for. I think this one is right, but maybe robustness with both? And, basically matches those with status of dissolved, so they should all be accounted for here
unique unique_id if inlist(status_all,4,5) & dissolve==1
unique unique_id if !inlist(status_all,4,5) & dissolve==1
tab marital_status_updated dissolve, m

gen end_year_match=.
replace end_year_match = 0 if dissolve==1 & rel_end_all!=end_year & rel_end_all!=.
replace end_year_match = 1 if dissolve==1 & rel_end_all==end_year & rel_end_all!=.
tab dissolve end_year_match, m row

browse unique_id partner_unique_id survey_yr marital_status_updated rel_start_all rel_end_all end_year end_year_match dissolve outcome status_all yr_end1 status1 yr_end2 status2 dur dissolve_v0 ever_dissolve

// some QAing
// browse unique_id partner_unique_id survey_yr marital_status_updated dissolve dissolve_v0 outcome rel_start_all rel_end_all last_survey_yr yr_married1 yr_end1 yr_married2 yr_end1 yr_married3 yr_end3 matrix_marr_num if inlist(unique_id, 356030, 409032, 677032, 423032, 916032, 951030, 2707033, 6165006)

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
// local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_wife cohab_with_other pre_marital_birth i.interval i.home_owner knot1 knot2 knot3 i.couple_educ_gp i.moved_last2 i.couple_joint_religion" 

// indicator if we observed at start of marriage - might want to use some measures in marriage year, but want to see how much that reduces sample
bysort unique_id partner_unique_id: egen min_dur = min(dur)
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
replace ever_cohab=1 if cohab_with_wife==1 | cohab_with_other==1

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
inspect cohab_with_wife 
inspect cohab_with_other
inspect pre_marital_birth
inspect home_owner
inspect couple_joint_religion // this has quite a bit of missing

// indicators of paid leave
gen paid_leave_state=0
replace paid_leave_state=1 if inlist(STATE_,6,34,36,44)

gen time_leave=.
replace time_leave=0 if STATE_==6 & survey_yr < 2004
replace time_leave=0 if STATE_==34 & survey_yr < 2009
replace time_leave=0 if STATE_==36 & survey_yr < 2014
replace time_leave=0 if STATE_==44 & survey_yr < 2018
replace time_leave=1 if STATE_==6 & survey_yr >= 2004
replace time_leave=1 if STATE_==34 & survey_yr >= 2009
replace time_leave=1 if STATE_==36 & survey_yr >= 2014
replace time_leave=1 if STATE_==44 & survey_yr >= 2018

// minimum wage
gen min_wage=0
replace min_wage=1 if inlist(STATE_,2,4,5,6,8,9,10,11,12,15,17,23,24,25,26,27,29,30,31,34,35,36,39,41,44,46,50,53,54)

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
**# Main figures from submission
********************************************************************************
********************************************************************************
********************************************************************************
global controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_wife cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion" 

global controls2 "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_wife cohab_with_other pre_marital_birth i.interval i.home_owner i.earnings_bucket_t1 i.couple_educ_gp i.moved_last2 i.couple_joint_religion i.num_children AGE_YOUNG_CHILD_" 

// Main effects charts
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4, or
margins, dydx(hh_hours_type_t1) level(95) post
estimates store est2

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children_under6==1, or
margins, dydx(division_bucket_hrs_t1) level(95) post
estimates store est4

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children_under6==1, or
margins, at(structural_familism_t1=(-5(1)10))
marginsplot, ytitle(Predicted Probability of Divorce) xtitle(Structural Support for Working Families) plot1opts(lcolor("black") mcolor("black")) ci1opts(color("black"))
margins, dydx(structural_familism_t1) post
estimates store esta

coefplot (est2, offset(.20) nokey) (est4, offset(-.20) nokey) (esta, nokey lcolor("black") mcolor("black") ciopts(color("black"))) , drop(_cons) xline(0) levels(95) base xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners"  1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" structural_familism_t1 = "Structural Support Scale") ///
 headings(1.hh_hours_type_t1= `""{bf:Division of Work Hours}" "{it:(Model 1)}""'  1.division_bucket_hrs_t1 = `""{bf:Combined Paid and Unpaid Labor}" "{it:(Model 2)}""' structural_familism_t1 = `""{bf:Structural Support for Working Families}" "{it:(Model 2)}"')

// Interaction charts
* Division of labor
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

* Combined DoL
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

// all parents
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 $controls if children==1 & hh_hours_type_t1!=4, or
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children==1, or
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if children==1 & hh_hours_type_t1 < 4, or
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children==1, or

// total sample
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 $controls if hh_hours_type_t1!=4, or
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls, or
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if hh_hours_type_t1 < 4, or
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls, or

********************************************************************************
********************************************************************************
********************************************************************************
**# Robustness checks start
********************************************************************************
********************************************************************************
********************************************************************************

********************************************************************************
* Some descriptive metric things
********************************************************************************
// selection into parental status
tab couple_educ_gp children_under6, row
tab educ_type children_under6, row
tab educ_wife_est children_under6, row

tab couple_educ_gp pre_marital_birth, row // okay so premarital birth is what I'd expect. just doesn't align for having child under 6
tab educ_type pre_marital_birth, row
tab educ_wife_est pre_marital_birth, row

// timing of first birth relative to relationship start - shotgun marriages?
browse unique_id year rel_start_all FIRST_BIRTH_YR when_first_birth first_birth_calc first_birth_calc2 // okay, should use when_first_birth

gen birth_timing = when_first_birth - rel_start_all
tab birth_timing, m
tab birth_timing dissolve, row
tab birth_timing pre_marital_birth, m

// divorce by child age - do people actually divorce if they have a kid under 6?
tab children_under6 dissolve, row // okay, in this crude view, actually quite similar...
tab AGE_YOUNG_CHILD_ dissolve, row

logit dissolve i.dur i.children_under6 $controls // okay, once adjusted, though, the divorce rate is sig. lower...
margins children_under6 // okay but effect size is really small. so, I think it's just sample size?

********************************************************************************
**# Alt age ranges (might replace all models)
********************************************************************************

****************************************
* 20-55
****************************************
tab children_under6 age_flag_2055, m row

// Main effects charts
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4 & age_flag_2055==1, or
margins, dydx(hh_hours_type_t1) level(95) post
estimates store age20a

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children_under6==1 & age_flag_2055==1, or
margins, dydx(division_bucket_hrs_t1) level(95) post
estimates store age20b

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children_under6==1 & age_flag_2055==1, or
margins, at(structural_familism_t1=(-5(1)10))
marginsplot, ytitle(Predicted Probability of Divorce) xtitle(Structural Support for Working Families) plot1opts(lcolor("black") mcolor("black")) ci1opts(color("black"))
margins, dydx(structural_familism_t1) post
estimates store age20c

coefplot (age20a, offset(.20) nokey) (age20b, offset(-.20) nokey) (age20c, nokey lcolor("black") mcolor("black") ciopts(color("black"))) , drop(_cons) xline(0) levels(95) base xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners"  1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" structural_familism_t1 = "Structural Support Scale") ///
 headings(1.hh_hours_type_t1= `""{bf:Division of Work Hours}" "{it:(Model 1)}""'  1.division_bucket_hrs_t1 = `""{bf:Combined Paid and Unpaid Labor}" "{it:(Model 2)}""' structural_familism_t1 = `""{bf:Structural Support for Working Families}" "{it:(Model 2)}"')

// Interaction charts
* Division of labor
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4 & age_flag_2055==1, or
sum structural_familism_t1, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store age20d

logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4 & age_flag_2055==1, or
sum structural_familism_t1, detail
margins, dydx(3.hh_hours_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store age20e

coefplot (age20d, mcolor(navy) ciopts(color(navy)) label("Male BW")) (age20e, label("Female BW")),  drop(_cons) nolabel xline(0) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) headings(1._at = "{bf:Structural Support: Percentiles}", nogap offset(0.30))

coefplot (age20d, mcolor(navy) ciopts(color(navy)) label("Male BW")) (age20e, label("Female BW")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical))

* Combined DoL
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under6==1 & age_flag_2055==1,  or
sum structural_familism_t1, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store age20f

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under6==1 & age_flag_2055==1, or
sum structural_familism_t1, detail
margins, dydx(3.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store age20g

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under6==1 & age_flag_2055==1, or
sum structural_familism_t1, detail
margins, dydx(4.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store age20h

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under6==1 & age_flag_2055==1, or
sum structural_familism_t1, detail
margins, dydx(5.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store age20i

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under6==1 & age_flag_2055==1, or
sum structural_familism_t1, detail
margins division_bucket_hrs_t1, at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) 
marginsplot

coefplot (age20f, mcolor(blue) ciopts(color(blue)) label("Traditional")) (age20g, label("Counter-Traditional")) (age20h, label("Second Shift"))  (age20i, label("All Others")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical)) xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) legend(position(bottom) rows(1))

// all parents
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 $controls if children==1 & hh_hours_type_t1!=4 & age_flag_2055==1, or
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children==1 & age_flag_2055==1, or
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if children==1 & hh_hours_type_t1 < 4 & age_flag_2055==1, or
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children==1 & age_flag_2055==1, or

// total sample
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 $controls if hh_hours_type_t1!=4 & age_flag_2055==1, or
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if age_flag_2055==1, or
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if hh_hours_type_t1 < 4 & age_flag_2055==1, or
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if age_flag_2055==1, or

****************************************
* 25-54
****************************************
tab children_under6 age_flag_2554, m row

// Main effects charts
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4 & age_flag_2554==1, or
margins, dydx(hh_hours_type_t1) level(95) post
estimates store age25a

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children_under6==1 & age_flag_2554==1, or
margins, dydx(division_bucket_hrs_t1) level(95) post
estimates store age25b

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children_under6==1 & age_flag_2554==1, or
margins, at(structural_familism_t1=(-5(1)10))
marginsplot, ytitle(Predicted Probability of Divorce) xtitle(Structural Support for Working Families) plot1opts(lcolor("black") mcolor("black")) ci1opts(color("black"))
margins, dydx(structural_familism_t1) post
estimates store age25c

coefplot (age25a, offset(.20) nokey) (age25b, offset(-.20) nokey) (age25c, nokey lcolor("black") mcolor("black") ciopts(color("black"))) , drop(_cons) xline(0) levels(95 90) base xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners"  1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" structural_familism_t1 = "Structural Support Scale") ///
 headings(1.hh_hours_type_t1= `""{bf:Division of Work Hours}" "{it:(Model 1)}""'  1.division_bucket_hrs_t1 = `""{bf:Combined Paid and Unpaid Labor}" "{it:(Model 2)}""' structural_familism_t1 = `""{bf:Structural Support for Working Families}" "{it:(Model 2)}"')

// Interaction charts
* Division of labor
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4 & age_flag_2554==1, or
sum structural_familism_t1, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store age25d

logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if children_under6==1 & hh_hours_type_t1 < 4 & age_flag_2554==1, or
sum structural_familism_t1, detail
margins, dydx(3.hh_hours_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store age25e

coefplot (age25d, mcolor(navy) ciopts(color(navy)) label("Male BW")) (age25e, label("Female BW")),  drop(_cons) nolabel xline(0) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) headings(1._at = "{bf:Structural Support: Percentiles}", nogap offset(0.30))

coefplot (age25d, mcolor(navy) ciopts(color(navy)) label("Male BW")) (age25e, label("Female BW")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical))

* Combined DoL
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under6==1 & age_flag_2554==1,  or
sum structural_familism_t1, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store age25f

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under6==1 & age_flag_2554==1, or
sum structural_familism_t1, detail
margins, dydx(3.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store age25g

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under6==1 & age_flag_2554==1, or
sum structural_familism_t1, detail
margins, dydx(4.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store age25h

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under6==1 & age_flag_2554==1, or
sum structural_familism_t1, detail
margins, dydx(5.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store age25i

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under6==1 & age_flag_2554==1, or
sum structural_familism_t1, detail
margins division_bucket_hrs_t1, at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) 
marginsplot

coefplot (age25f, mcolor(blue) ciopts(color(blue)) label("Traditional")) (age25g, label("Counter-Traditional")) (age25h, label("Second Shift"))  (age25i, label("All Others")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical)) xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) legend(position(bottom) rows(1))

// all parents
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 $controls if children==1 & hh_hours_type_t1!=4 & age_flag_2554==1, or
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children==1 & age_flag_2554==1 & division_bucket_hrs_t1!=3, or
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if children==1 & hh_hours_type_t1 < 4 & age_flag_2554==1, or
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children==1 & age_flag_2554==1 & division_bucket_hrs_t1!=3, or

// total sample
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 $controls if hh_hours_type_t1!=4 & age_flag_2554==1, or
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if age_flag_2554==1, or
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if hh_hours_type_t1 < 4 & age_flag_2554==1, or
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if age_flag_2554==1 & division_bucket_hrs_t1!=3, or

********************************************************************************
**# Alt child age things (might replace all models)
********************************************************************************
// let's actually use the age flag of 20 also. the results are so similar and this is easy.

****************************************
* Follow from rel start until MAX DURATION there is a child under 6
****************************************
// Main effects charts
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 $controls if children_under6_flag==1 & hh_hours_type_t1 < 4 & age_flag_2055==1, or
margins, dydx(hh_hours_type_t1) level(95) post
estimates store child6a

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children_under6_flag==1 & age_flag_2055==1, or
margins, dydx(division_bucket_hrs_t1) level(95) post
estimates store child6b

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children_under6_flag==1 & age_flag_2055==1, or
margins, at(structural_familism_t1=(-5(1)10))
marginsplot, ytitle(Predicted Probability of Divorce) xtitle(Structural Support for Working Families) plot1opts(lcolor("black") mcolor("black")) ci1opts(color("black"))
margins, dydx(structural_familism_t1) post
estimates store child6c

coefplot (child6a, offset(.20) nokey) (child6b, offset(-.20) nokey) (child6c, nokey lcolor("black") mcolor("black") ciopts(color("black"))) , drop(_cons) xline(0) levels(95) base xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners"  1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" structural_familism_t1 = "Structural Support Scale") ///
 headings(1.hh_hours_type_t1= `""{bf:Division of Work Hours}" "{it:(Model 1)}""'  1.division_bucket_hrs_t1 = `""{bf:Combined Paid and Unpaid Labor}" "{it:(Model 2)}""' structural_familism_t1 = `""{bf:Structural Support for Working Families}" "{it:(Model 2)}"')

// Interaction charts
* Division of labor
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if children_under6_flag==1 & hh_hours_type_t1 < 4 & age_flag_2055==1, or
sum structural_familism_t1, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store child6d

logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if children_under6_flag==1 & hh_hours_type_t1 < 4 & age_flag_2055==1, or
sum structural_familism_t1, detail
margins, dydx(3.hh_hours_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store child6e

coefplot (child6d, mcolor(navy) ciopts(color(navy)) label("Male BW")) (child6e, label("Female BW")),  drop(_cons) nolabel xline(0) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) headings(1._at = "{bf:Structural Support: Percentiles}", nogap offset(0.30))

coefplot (child6d, mcolor(navy) ciopts(color(navy)) label("Male BW")) (child6e, label("Female BW")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical))

* Combined DoL
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under6_flag==1 & age_flag_2055==1,  or
sum structural_familism_t1, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store child6f

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under6_flag==1 & age_flag_2055==1, or
sum structural_familism_t1, detail
margins, dydx(3.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store child6g

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under6_flag==1 & age_flag_2055==1, or
sum structural_familism_t1, detail
margins, dydx(4.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store child6h

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under6_flag==1 & age_flag_2055==1, or
sum structural_familism_t1, detail
margins, dydx(5.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store child6i

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under6_flag==1 & age_flag_2055==1, or
sum structural_familism_t1, detail
margins division_bucket_hrs_t1, at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) 
marginsplot

coefplot (child6f, mcolor(blue) ciopts(color(blue)) label("Traditional")) (child6g, label("Counter-Traditional")) (child6h, label("Second Shift"))  (child6i, label("All Others")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical)) xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) legend(position(bottom) rows(1))

****************************************
* Follow from 1 year prior to last year child under 6
****************************************
// Main effects charts
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 $controls if children_under6_flag_alt==1 & hh_hours_type_t1 < 4 & age_flag_2055==1, or
margins, dydx(hh_hours_type_t1) level(95) post
estimates store child6xa

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children_under6_flag_alt==1 & age_flag_2055==1, or
margins, dydx(division_bucket_hrs_t1) level(95) post
estimates store child6xb

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children_under6_flag_alt==1 & age_flag_2055==1, or
margins, at(structural_familism_t1=(-5(1)10))
marginsplot, ytitle(Predicted Probability of Divorce) xtitle(Structural Support for Working Families) plot1opts(lcolor("black") mcolor("black")) ci1opts(color("black"))
margins, dydx(structural_familism_t1) post
estimates store child6xc

coefplot (child6xa, offset(.20) nokey) (child6xb, offset(-.20) nokey) (child6xc, nokey lcolor("black") mcolor("black") ciopts(color("black"))) , drop(_cons) xline(0) levels(95) base xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners"  1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" structural_familism_t1 = "Structural Support Scale") ///
 headings(1.hh_hours_type_t1= `""{bf:Division of Work Hours}" "{it:(Model 1)}""'  1.division_bucket_hrs_t1 = `""{bf:Combined Paid and Unpaid Labor}" "{it:(Model 2)}""' structural_familism_t1 = `""{bf:Structural Support for Working Families}" "{it:(Model 2)}"')

// Interaction charts
* Division of labor
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if children_under6_flag_alt==1 & hh_hours_type_t1 < 4 & age_flag_2055==1, or
sum structural_familism_t1, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store child6xd

logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if children_under6_flag_alt==1 & hh_hours_type_t1 < 4 & age_flag_2055==1, or
sum structural_familism_t1, detail
margins, dydx(3.hh_hours_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store child6xe

coefplot (child6xd, mcolor(navy) ciopts(color(navy)) label("Male BW")) (child6xe, label("Female BW")),  drop(_cons) nolabel xline(0) levels(95 90) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) headings(1._at = "{bf:Structural Support: Percentiles}", nogap offset(0.30))

coefplot (child6xd, mcolor(navy) ciopts(color(navy)) label("Male BW")) (child6xe, label("Female BW")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical))

* Combined DoL
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under6_flag_alt==1 & age_flag_2055==1,  or
sum structural_familism_t1, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store child6xf

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under6_flag_alt==1 & age_flag_2055==1, or
sum structural_familism_t1, detail
margins, dydx(3.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store child6xg

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under6_flag_alt==1 & age_flag_2055==1, or
sum structural_familism_t1, detail
margins, dydx(4.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store child6xh

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under6_flag_alt==1 & age_flag_2055==1, or
sum structural_familism_t1, detail
margins, dydx(5.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store child6xi

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under6_flag_alt==1 & age_flag_2055==1, or
sum structural_familism_t1, detail
margins division_bucket_hrs_t1, at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) 
marginsplot

coefplot (child6xf, mcolor(blue) ciopts(color(blue)) label("Traditional")) (child6xg, label("Counter-Traditional")) (child6xh, label("Second Shift"))  (child6xi, label("All Others")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical)) xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) legend(position(bottom) rows(1))

****************************************
* Cut off of 14 instead (pre high school)
****************************************
// Main effects charts
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 $controls if children_under14==1 & hh_hours_type_t1 < 4 & age_flag_2055==1, or
margins, dydx(hh_hours_type_t1) level(95) post
estimates store child14a

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children_under14==1 & age_flag_2055==1, or
margins, dydx(division_bucket_hrs_t1) level(95) post
estimates store child14b

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls if children_under14==1 & age_flag_2055==1, or
margins, at(structural_familism_t1=(-5(1)10))
marginsplot, ytitle(Predicted Probability of Divorce) xtitle(Structural Support for Working Families) plot1opts(lcolor("black") mcolor("black")) ci1opts(color("black"))
margins, dydx(structural_familism_t1) post
estimates store child14c

coefplot (child14a, offset(.20) nokey) (child14b, offset(-.20) nokey) (child14c, nokey lcolor("black") mcolor("black") ciopts(color("black"))) , drop(_cons) xline(0) levels(95) base xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners"  1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" structural_familism_t1 = "Structural Support Scale") ///
 headings(1.hh_hours_type_t1= `""{bf:Division of Work Hours}" "{it:(Model 1)}""'  1.division_bucket_hrs_t1 = `""{bf:Combined Paid and Unpaid Labor}" "{it:(Model 2)}""' structural_familism_t1 = `""{bf:Structural Support for Working Families}" "{it:(Model 2)}"')

// Interaction charts
* Division of labor
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if children_under14==1 & hh_hours_type_t1 < 4 & age_flag_2055==1, or
sum structural_familism_t1, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store child14d

logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls if children_under14==1 & hh_hours_type_t1 < 4 & age_flag_2055==1, or
sum structural_familism_t1, detail
margins, dydx(3.hh_hours_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store child14e

coefplot (child14d, mcolor(navy) ciopts(color(navy)) label("Male BW")) (child14e, label("Female BW")),  drop(_cons) nolabel xline(0) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) headings(1._at = "{bf:Structural Support: Percentiles}", nogap offset(0.30))

coefplot (child14d, mcolor(navy) ciopts(color(navy)) label("Male BW")) (child14e, label("Female BW")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical))

* Combined DoL
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under14==1 & age_flag_2055==1,  or
sum structural_familism_t1, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store child14f

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under14==1 & age_flag_2055==1, or
sum structural_familism_t1, detail
margins, dydx(3.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store child14g

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under14==1 & age_flag_2055==1, or
sum structural_familism_t1, detail
margins, dydx(4.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store child14h

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under14==1 & age_flag_2055==1, or
sum structural_familism_t1, detail
margins, dydx(5.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store child14i

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls if children_under14==1 & age_flag_2055==1, or
sum structural_familism_t1, detail
margins division_bucket_hrs_t1, at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) 
marginsplot

coefplot (child14f, mcolor(blue) ciopts(color(blue)) label("Traditional")) (child14g, label("Counter-Traditional")) (child14h, label("Second Shift"))  (child14i, label("All Others")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical)) xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) legend(position(bottom) rows(1))

****************************************
* Restrict to non-parents prior to marriage (this will be a select sample I know)
****************************************
// Main effects charts
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 $controls2 if pre_marital_birth==0 & hh_hours_type_t1 < 4 & age_flag_2055==1, or
margins, dydx(hh_hours_type_t1) level(95) post
estimates store pre_a

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls2 if pre_marital_birth==0 & age_flag_2055==1, or
margins, dydx(division_bucket_hrs_t1) level(95) post
estimates store pre_b

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 $controls2 if pre_marital_birth==0 & age_flag_2055==1, or
margins, at(structural_familism_t1=(-5(1)10))
marginsplot, ytitle(Predicted Probability of Divorce) xtitle(Structural Support for Working Families) plot1opts(lcolor("black") mcolor("black")) ci1opts(color("black"))
margins, dydx(structural_familism_t1) post
estimates store pre_c

coefplot (pre_a, offset(.20) nokey) (pre_b, offset(-.20) nokey) (pre_c, nokey lcolor("black") mcolor("black") ciopts(color("black"))) , drop(_cons) xline(0) levels(95) base xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) ///
coeflabels(2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner" 4.hh_hours_type_t1 = "No Earners"  1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others" structural_familism_t1 = "Structural Support Scale") ///
 headings(1.hh_hours_type_t1= `""{bf:Division of Work Hours}" "{it:(Model 1)}""'  1.division_bucket_hrs_t1 = `""{bf:Combined Paid and Unpaid Labor}" "{it:(Model 2)}""' structural_familism_t1 = `""{bf:Structural Support for Working Families}" "{it:(Model 2)}"')

// Interaction charts
* Division of labor
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls2 if pre_marital_birth==0 & hh_hours_type_t1 < 4 & age_flag_2055==1, or
sum structural_familism_t1, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store pre_d

logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls2 if pre_marital_birth==0 & hh_hours_type_t1 < 4 & age_flag_2055==1, or
sum structural_familism_t1, detail
margins, dydx(3.hh_hours_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store pre_e

coefplot (pre_d, mcolor(navy) ciopts(color(navy)) label("Male BW")) (pre_e, label("Female BW")),  drop(_cons) nolabel xline(0) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) headings(1._at = "{bf:Structural Support: Percentiles}", nogap offset(0.30))

coefplot (pre_d, mcolor(navy) ciopts(color(navy)) label("Male BW")) (pre_e, label("Female BW")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical))

* Combined DoL
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls2 if pre_marital_birth==0 & age_flag_2055==1,  or
sum structural_familism_t1, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store pre_f

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls2 if pre_marital_birth==0 & age_flag_2055==1, or
sum structural_familism_t1, detail
margins, dydx(3.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store pre_g

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls2 if pre_marital_birth==0 & age_flag_2055==1, or
sum structural_familism_t1, detail
margins, dydx(4.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store pre_h

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls2 if pre_marital_birth==0 & age_flag_2055==1, or
sum structural_familism_t1, detail
margins, dydx(5.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store pre_i

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls2 if pre_marital_birth==0 & age_flag_2055==1, or
sum structural_familism_t1, detail
margins division_bucket_hrs_t1, at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) 
marginsplot

coefplot (pre_f, mcolor(blue) ciopts(color(blue)) label("Traditional")) (pre_g, label("Counter-Traditional")) (pre_h, label("Second Shift"))  (pre_i, label("All Others")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical)) xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) legend(position(bottom) rows(1))



********************************************************************************
**# Did I do education interaction for total sample?
********************************************************************************

****************************************
* Non college educated
****************************************

// Interaction charts
* Division of labor
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls2 if age_flag_2055==1 & hh_hours_type_t1 < 4 & couple_educ_gp==0, or
sum structural_familism_t1, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_n1

logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls2 if age_flag_2055==1 & hh_hours_type_t1 < 4 & couple_educ_gp==0, or
sum structural_familism_t1, detail
margins, dydx(3.hh_hours_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_n2

coefplot (est_n1, mcolor(navy) ciopts(color(navy)) label("Male BW")) (est_n2, label("Female BW")),  drop(_cons) nolabel xline(0) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) headings(1._at = "{bf:Structural Support: Percentiles}", nogap offset(0.30))

* Combined DoL
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls2 if age_flag_2055==1 & couple_educ_gp==0, or
sum structural_familism_t1, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_n3

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls2 if age_flag_2055==1 & couple_educ_gp==0, or
sum structural_familism_t1, detail
margins, dydx(3.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_n4

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls2 if age_flag_2055==1 & couple_educ_gp==0, or
sum structural_familism_t1, detail
margins, dydx(4.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_n5

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls2 if age_flag_2055==1 & couple_educ_gp==0, or
sum structural_familism_t1, detail
margins, dydx(5.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_n6

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls2 if age_flag_2055==1 & couple_educ_gp==0, or
sum structural_familism_t1, detail
margins division_bucket_hrs_t1, at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) 

coefplot (est_n3, mcolor(blue) ciopts(color(blue)) label("Traditional")) (est_n4, label("Counter-Traditional")) (est_n5, label("Second Shift"))  (est_n6, label("All Others")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical)) xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) legend(position(bottom) rows(1))

///////////////// Other child measure
// Interaction charts
* Division of labor
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls2 if children_under6_flag==1 & hh_hours_type_t1 < 4 & couple_educ_gp==0, or
sum structural_familism_t1, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_n1

logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls2 if children_under6_flag==1 & hh_hours_type_t1 < 4 & couple_educ_gp==0, or
sum structural_familism_t1, detail
margins, dydx(3.hh_hours_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_n2

coefplot (est_n1, mcolor(navy) ciopts(color(navy)) label("Male BW")) (est_n2, label("Female BW")),  drop(_cons) nolabel xline(0) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) headings(1._at = "{bf:Structural Support: Percentiles}", nogap offset(0.30))

* Combined DoL
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls2 if children_under6_flag==1 & couple_educ_gp==0, or
sum structural_familism_t1, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_n3

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls2 if children_under6_flag==1 & couple_educ_gp==0, or
sum structural_familism_t1, detail
margins, dydx(3.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_n4

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls2 if children_under6_flag==1 & couple_educ_gp==0, or
sum structural_familism_t1, detail
margins, dydx(4.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_n5

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls2 if children_under6_flag==1 & couple_educ_gp==0, or
sum structural_familism_t1, detail
margins, dydx(5.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_n6

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls2 if children_under6_flag==1 & couple_educ_gp==0, or
sum structural_familism_t1, detail
margins division_bucket_hrs_t1, at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) 

coefplot (est_n3, mcolor(blue) ciopts(color(blue)) label("Traditional")) (est_n4, label("Counter-Traditional")) (est_n5, label("Second Shift"))  (est_n6, label("All Others")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical)) xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) legend(position(bottom) rows(1))

****************************************
* College educated
****************************************

// Interaction charts
* Division of labor
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls2 if age_flag_2055==1 & hh_hours_type_t1 < 4 & couple_educ_gp==1, or
sum structural_familism_t1, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_c1

logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls2 if age_flag_2055==1 & hh_hours_type_t1 < 4 & couple_educ_gp==1, or
sum structural_familism_t1, detail
margins, dydx(3.hh_hours_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_c2

coefplot (est_c1, mcolor(navy) ciopts(color(navy)) label("Male BW")) (est_c2, label("Female BW")),  drop(_cons) nolabel xline(0) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) headings(1._at = "{bf:Structural Support: Percentiles}", nogap offset(0.30))

* Combined DoL
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls2 if age_flag_2055==1 & couple_educ_gp==1, or
sum structural_familism_t1, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_c3

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls2 if age_flag_2055==1 & couple_educ_gp==1, or
sum structural_familism_t1, detail
margins, dydx(3.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_c4

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls2 if age_flag_2055==1 & couple_educ_gp==1, or
sum structural_familism_t1, detail
margins, dydx(4.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_c5

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls2 if age_flag_2055==1 & couple_educ_gp==1, or
sum structural_familism_t1, detail
margins, dydx(5.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_c6

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls2 if age_flag_2055==1 & couple_educ_gp==1, or
sum structural_familism_t1, detail
margins division_bucket_hrs_t1, at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) 

coefplot (est_c3, mcolor(blue) ciopts(color(blue)) label("Traditional")) (est_c4, label("Counter-Traditional")) (est_c5, label("Second Shift"))  (est_c6, label("All Others")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical)) xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) legend(position(bottom) rows(1))

//////////////// Other child measure
// Interaction charts
* Division of labor
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls2 if children_under6_flag==1 & hh_hours_type_t1 < 4 & couple_educ_gp==1, or
sum structural_familism_t1, detail
margins, dydx(2.hh_hours_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_c1

logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 $controls2 if children_under6_flag==1 & hh_hours_type_t1 < 4 & couple_educ_gp==1, or
sum structural_familism_t1, detail
margins, dydx(3.hh_hours_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
estimates store est_c2

coefplot (est_c1, mcolor(navy) ciopts(color(navy)) label("Male BW")) (est_c2, label("Female BW")),  drop(_cons) nolabel xline(0) levels(95) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") xtitle(Average Marginal Effect Relative to Dual-Earning, size(small)) legend(position(bottom) rows(1)) headings(1._at = "{bf:Structural Support: Percentiles}", nogap offset(0.30))

* Combined DoL
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls2 if children_under6_flag==1 & couple_educ_gp==1, or
sum structural_familism_t1, detail
margins, dydx(2.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_c3

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls2 if children_under6_flag==1 & couple_educ_gp==1, or
sum structural_familism_t1, detail
margins, dydx(3.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_c4

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls2 if children_under6_flag==1 & couple_educ_gp==1, or
sum structural_familism_t1, detail
margins, dydx(4.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_c5

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls2 if children_under6_flag==1 & couple_educ_gp==1, or
sum structural_familism_t1, detail
margins, dydx(5.division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) level(90) post
estimates store est_c6

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 $controls2 if children_under6_flag==1 & couple_educ_gp==1, or
sum structural_familism_t1, detail
margins division_bucket_hrs_t1, at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) 

coefplot (est_c3, mcolor(blue) ciopts(color(blue)) label("Traditional")) (est_c4, label("Counter-Traditional")) (est_c5, label("Second Shift"))  (est_c6, label("All Others")),  drop(_cons) nolabel xline(0, lcolor("red")) levels(90) coeflabels(1._at = "5th Percentile" 2._at = "25th Percentile" 3._at = "50th Percentile" 4._at = "75th Percentile" 5._at = "95th Percentile") groups(?._at = "{bf:Structural Support: Percentiles}", angle(vertical)) xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) legend(position(bottom) rows(1))
