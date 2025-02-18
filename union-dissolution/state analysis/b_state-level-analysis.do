********************************************************************************
* Adding in state-level data
* state-level-analysis.do
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

keep if matrix_marr_num==1
keep if (AGE_HEAD_>=18 & AGE_HEAD_<=55) &  (AGE_WIFE_>=18 & AGE_WIFE_<=55)

keep if inrange(rel_start_all,1995,2016) // extend to 2016? if I keep 2021?
keep if inlist(IN_UNIT,0,1,2)
// drop if survey_yr==2021 // until I figure out what to do about covid year. I did add the policy measures, now need to figure out if it makes sense to keep for other reasons
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
unique unique_id if inlist(status_all,4,5) // 926 here 
unique unique_id if outcome==1 // 709 here. but not all couples observed in their year of breakup which I think is the problem 
unique unique_id if dissolve==1 // 976 here. This is like, I did adjust for them not being observed together in last year, so this s the final year observed together, hence why there are more. in outcome, they are not accounted for. I think this one is right, but maybe robustness with both?

browse unique_id partner_unique_id survey_yr rel_start_all rel_end_all end_year dissolve outcome status_all yr_end1 status1 yr_end2 status2 dur dissolve_v0 ever_dissolve

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

browse unique_id partner_unique_id survey_yr STATE_ state_change moved_states moved_states_lag rel_start_all dissolve_lag MOVED_ MOVED_YEAR_

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

// create new variable for having kids under 6 in household
gen children_under6=0
replace children_under6=1 if children==1 & AGE_YOUNG_CHILD_ < 6

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

********************************************************************************
**# Merge onto policy data
********************************************************************************
/*
statemin: minimum wage - 2017, I updated up until 2020
masssociallib_est: attitudes - 2014
policysociallib_est: social policy - 2014
policyeconlib_est: economic policy - 2014
unemployment: unemployment rate - 2017, I updated up until 2020
state_cpi_bfh_est: state CPI - 2010
fed min: "T:\Research Projects\State data\data_keep\fed_min.dta"
new policy file: "T:\Research Projects\State data\data_keep\final_measures.dta"
*/

rename STATE_ state_fips
rename survey_yr year

/* Not using these measures
merge m:1 state_fips year using "$state_data/cspp_data_1985_2019.dta", keepusing(statemin masssociallib_est policysociallib_est policyeconlib_est unemployment state_cpi_bfh_est pollib_median)

drop if _merge==2
drop _merge

merge m:1 year using "$state_data/fed_min.dta"
drop if _merge==2
drop _merge

merge m:1 state_fips year using "$state_data/final_measures.dta"
drop if _merge==2
drop _merge

merge m:1 state_fips year using "$state_data/state_lca.dta"
drop if _merge==2
drop _merge
*/

merge m:1 state_fips year using "$state_data/structural_familism.dta"
drop if _merge==2
drop _merge
rename structural_familism structural_familism_t
rename f1 f1_t
drop year_t1 year_t2

gen year_t1 = year - 1
merge m:1 year_t1 state_fips using "$state_data/structural_familism.dta", keepusing(structural_familism f1)
drop if _merge==2
drop _merge
rename structural_familism structural_familism_t1
rename f1 f1_t1

label values state_fips .
sort unique_id year
browse unique_id year state_fips structural_familism_t structural_familism_t1

// do I center the main data or here? in main data, no real need to center (already essentially at 0)
gen sf_centered_alt_t=.
sum structural_familism_t, detail
replace sf_centered_alt_t = structural_familism_t - `r(mean)'

gen sf_centered_alt_t1=.
sum structural_familism_t1, detail
replace sf_centered_alt_t1 = structural_familism_t1 - `r(mean)'

browse structural_familism_t sf_centered sf_centered_alt_t
sum structural_familism_t sf_centered sf_centered_alt_t, detail

// various variables I tested and am not using
/*
gen above_fed_min=0
replace above_fed_min=1 if statemin>fed_min & statemin!=.
replace above_fed_min=. if statemin==.


gen state_cpi = (state_cpi_bfh_est*100) + 100

gen above_fed_cpi=0
replace above_fed_cpi=1 if state_cpi>fed_cpi & state_cpi!=.
replace above_fed_cpi=. if state_cpi==.

gen social_policy=0
replace social_policy=1 if policysociallib_est>0 & policysociallib_est!=.

xtile social_policy_gp = policysociallib_est, nq(5)
tabstat policysociallib_est, by(social_policy_gp)
// pctile social_policy_score = policysociallib_est, nq(5) genp(social_policy_gp)

xtile liberal_attitudes_gp = masssociallib_est, nq(5)
tabstat masssociallib_est, by(liberal_attitudes_gp)

// is there enough variation?
recode disapproval (2.154=1) (2.20/2.21=2) (2.2405=3) (2.27/2.29=4) (2.3935=5), gen(disapproval_bkt)

// creating four categorical variable of sexism and attitudes
sum structural_sexism
gen sexism_scale=0
replace sexism_scale=1 if structural_sexism > `r(mean)'
replace sexism_scale=. if structural_sexism==.

sum structural_familism
gen familism_scale=0
replace familism_scale=1 if structural_familism > `r(mean)'
replace familism_scale=. if structural_familism==.

gen familism_scale_det=.
sum structural_familism, detail
replace familism_scale_det=1 if structural_familism < `r(p25)'
replace familism_scale_det=2 if structural_familism >= `r(p25)' & structural_familism <=`r(p75)'
replace familism_scale_det=3 if structural_familism > `r(p75)' & structural_familism!=.
tabstat structural_familism, by(familism_scale_det)

sum gender_mood
gen gender_scale=0
replace gender_scale=1 if gender_mood > `r(mean)'
replace gender_scale=. if gender_mood==.

tab sexism_scale gender_scale // these are inverse remember kim
tab familism_scale gender_scale // these both go in the same direction
tab familism_scale sexism_scale // opposite

gen state_cat = .
replace state_cat=1 if familism_scale==0 & gender_scale == 0 // both trad
replace state_cat=2 if familism_scale==0 & gender_scale == 1 //  trad families, egal att
replace state_cat=3 if familism_scale==1 & gender_scale == 0 // egal families, trad att
replace state_cat=4 if familism_scale==1 & gender_scale == 1 // both good

label define state_cat 1 "Both Trad" 2 "Policy Trad" 3 "Policy Support" 4 "Both Good"
label values state_cat state_cat

rename f1 family_factor

// aggregate attitudinal measure
* One just average current
egen regional_attitudes_pct = rowmean(genderroles_egal working_mom_egal preschool_egal)
egen regional_attitudes_mean = rowmean(fepresch fechld fefam)

pwcorr regional_attitudes_pct regional_attitudes_mean // .9247

* PCA (following Pessin) - need to do at raw mean, not percentage? - okay but for now, I only pulled in percentage gah, but i have okay
* these don't all go same way gah do with pct for now
alpha genderroles_egal working_mom_egal preschool_egal // 0.7360 - matches Pessin (0.74). she centered it not rescaled?
factor genderroles_egal working_mom_egal preschool_egal, ipf
predict f1
rename f1 regional_attitudes_factor
pwcorr regional_attitudes_pct regional_attitudes_factor // .9680

// rescale?
sum regional_attitudes_factor
gen regional_attitudes_scaled=(regional_attitudes_factor - r(min)) /  (r(max) - r(min))
sum regional_attitudes_scaled

alpha unemployment_st child_pov_st gini_st // economic uncertainty. 0.54
alpha earn_ratio_st lfp_ratio_st pov_ratio_st pctmaleleg_st no_paid_leave_st no_dv_gun_law_st senate_rep_st // structural sexism. 0.61

*/

set scheme cleanplots

alpha paid_leave_length_st prek_enrolled_public_st min_amt_above_fed_st earn_ratio_neg_st unemployment_percap_st abortion_protected_st welfare_all_st // structural familism. 0.70 0.716 now

********************************************************************************
********************************************************************************
********************************************************************************
**# Analysis starts
* Overall trends
********************************************************************************
********************************************************************************
********************************************************************************

logit outcome i.dur i.couple_educ_gp, or

logit outcome i.dur i.couple_educ_gp##i.state_fips, or nocons // prob gonna be a lot of collinearity / inability to estimate with small states?
outreg2 using "$results/state_test.xls", sideway stats(coef pval) label dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

forvalues s=1/56{
	capture logistic outcome i.dur i.couple_educ_gp if state_fips==`s'
	capture estimates store m`s'
} 

estimates dir
estimates table *, keep(i.couple_educ_gp) eform b p

estout * using "$results/state_test.xls", replace keep(1.couple_educ_gp) eform cells(b p)

// estout *, eform cells(b(keep(i.couple_educ_gp)) p(keep(i.couple_educ_gp)))

/* test
logit dissolve_lag i.dur i.couple_educ_gp if STATE_==1, or
logit dissolve_lag i.dur i.couple_educ_gp if STATE_==2, or // won't estimate
logit dissolve_lag i.dur i.couple_educ_gp if STATE_==3, or // no obs
logit dissolve_lag i.dur i.couple_educ_gp if STATE_==4, or // will estimate
logit dissolve_lag i.dur i.couple_educ_gp if STATE_==5, or // also will estimate
*/

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

********************************************************************************
********************************************************************************
**# MAIN MODELS (by parental status)
********************************************************************************
********************************************************************************

local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled_t1 i.state_fips cohab_with_wife cohab_with_other pre_marital_birth i.interval i.home_owner_t1 earnings_bucket_t1 i.couple_educ_gp_t1 i.moved_last2 i.couple_joint_religion_t1"  // i.num_children i.region knot1 knot2 knot3 

********************************************************************************
* Parents of children under the age of 6
********************************************************************************
/* Main Effects */
*Earnings
logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 `controls' if children_under6==1 & hh_earn_type_t1 < 4, or
outreg2 using "$results/dissolution_AMES_familism_parents.xls", sideway stats(coef pval) label ctitle(Parents 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

* Hours
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 `controls' if children_under6==1 & hh_hours_type_t1 < 4, or
outreg2 using "$results/dissolution_AMES_familism_parents.xls", sideway stats(coef pval) label ctitle(Parents 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

*Combined Earnings
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_t1 `controls' if children_under6==1, or
outreg2 using "$results/dissolution_AMES_familism_parents.xls", sideway stats(coef pval) label ctitle(Parents 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Combined Hours
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 `controls' if children_under6==1, or
outreg2 using "$results/dissolution_AMES_familism_parents.xls", sideway stats(coef pval) label ctitle(Parents 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

/* Interactions */
*Earnings
logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 c.structural_familism_t1#i.hh_earn_type_t1 `controls' if children_under6==1 & hh_earn_type_t1 < 4, or
outreg2 using "$results/dissolution_AMES_familism_parents.xls", sideway stats(coef pval) label ctitle(Parents 1a) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum structural_familism_t1, detail
margins, dydx(hh_earn_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_parents.xls", ctitle(Parents 1b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*Hours
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 `controls' if children_under6==1 & hh_hours_type_t1 < 4, or
outreg2 using "$results/dissolution_AMES_familism_parents.xls", sideway stats(coef pval) label ctitle(Parents 2a) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum structural_familism_t1, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_parents.xls", ctitle(Parents 2b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*Combined Earnings
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_t1 c.structural_familism_t1#i.division_bucket_t1 `controls' if children_under6==1, or
outreg2 using "$results/dissolution_AMES_familism_parents.xls", sideway stats(coef pval) label ctitle(Parents 3a) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum structural_familism_t1, detail
margins, dydx(division_bucket_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_parents.xls", ctitle(Parents 3b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*Combined Hours
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 `controls' if children_under6==1, or
outreg2 using "$results/dissolution_AMES_familism_parents.xls", sideway stats(coef pval) label ctitle(Parents 4a) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum structural_familism_t1, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_parents.xls", ctitle(Parents 4b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* First 10 years of marriage (robustness)
logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 c.structural_familism_t1#i.hh_earn_type_t1 `controls' if children_under6==1 & hh_earn_type_t1 < 4 & dur<=10, or
// outreg2 using "$results/dissolution_AMES_familism_parents.xls", sideway stats(coef pval) label ctitle(dur) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum structural_familism_t1, detail
margins, dydx(hh_earn_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
// outreg2 using "$results/dissolution_AMES_familism_parents.xls", ctitle(dur) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

/*
* Alt DoL (robustness) - "under"-earners. ignoring for now
logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1_alt c.structural_familism_t1#i.hh_earn_type_t1_alt `controls' if children_under6==1, or
outreg2 using "$results/dissolution_AMES_familism_parents.xls", sideway stats(coef pval) label ctitle(Parents 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum structural_familism_t1, detail
margins, dydx(hh_earn_type_t1_alt) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_parents.xls", ctitle(alt dol) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)
*/


********************************************************************************
* All parents
********************************************************************************
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled_t1 i.state_fips cohab_with_wife cohab_with_other pre_marital_birth i.interval i.home_owner_t1 earnings_bucket_t1 i.couple_educ_gp_t1 i.moved_last2 i.couple_joint_religion_t1"  // i.num_children i.region knot1 knot2 knot3 

/* Main Effects */
*Earnings
logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 `controls' if children==1 & hh_earn_type_t1 < 4, or
outreg2 using "$results/dissolution_AMES_familism_parents_all.xls", sideway stats(coef pval) label ctitle(All Par 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

* Hours
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 `controls' if children==1 & hh_hours_type_t1 < 4, or
outreg2 using "$results/dissolution_AMES_familism_parents_all.xls", sideway stats(coef pval) label ctitle(All Par 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

*Combined Earnings
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_t1 `controls' if children==1, or
outreg2 using "$results/dissolution_AMES_familism_parents_all.xls", sideway stats(coef pval) label ctitle(All Par 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Combined Hours
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 `controls' if children==1, or
outreg2 using "$results/dissolution_AMES_familism_parents_all.xls", sideway stats(coef pval) label ctitle(All Par 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

/* Interactions */
*Earnings
logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 c.structural_familism_t1#i.hh_earn_type_t1 `controls' if children==1 & hh_earn_type_t1 < 4, or
outreg2 using "$results/dissolution_AMES_familism_parents_all.xls", sideway stats(coef pval) label ctitle(All Par 1a) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum structural_familism_t1, detail
margins, dydx(hh_earn_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_parents_all.xls", ctitle(All Par 1b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*Hours
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 `controls' if children==1 & hh_hours_type_t1 < 4, or
outreg2 using "$results/dissolution_AMES_familism_parents_all.xls", sideway stats(coef pval) label ctitle(All Par 2a) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum structural_familism_t1, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_parents_all.xls", ctitle(All Par 2b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*Combined Earnings
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_t1 c.structural_familism_t1#i.division_bucket_t1 `controls' if children==1, or
outreg2 using "$results/dissolution_AMES_familism_parents_all.xls", sideway stats(coef pval) label ctitle(All Par 3a) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum structural_familism_t1, detail
margins, dydx(division_bucket_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_parents_all.xls", ctitle(All Par 3b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*Combined Hours
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 `controls' if children==1, or
outreg2 using "$results/dissolution_AMES_familism_parents_all.xls", sideway stats(coef pval) label ctitle(All Par 4a) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum structural_familism_t1, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_parents_all.xls", ctitle(All Par 4b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

********************************************************************************
* Total sample
********************************************************************************
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled_t1 i.state_fips cohab_with_wife cohab_with_other pre_marital_birth i.interval i.home_owner_t1 earnings_bucket_t1 i.couple_educ_gp_t1 i.moved_last2 i.couple_joint_religion_t1 i.num_children" 

/* Main Effects */
*Earnings
logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 `controls' if hh_earn_type_t1 < 4, or
outreg2 using "$results/dissolution_AMES_familism_sample_all.xls", sideway stats(coef pval) label ctitle(All 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

* Hours
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 `controls' if hh_hours_type_t1 < 4, or
outreg2 using "$results/dissolution_AMES_familism_sample_all.xls", sideway stats(coef pval) label ctitle(All 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

*Combined Earnings
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_t1 `controls', or
outreg2 using "$results/dissolution_AMES_familism_sample_all.xls", sideway stats(coef pval) label ctitle(All 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Combined Hours
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 `controls', or
outreg2 using "$results/dissolution_AMES_familism_sample_all.xls", sideway stats(coef pval) label ctitle(All 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

/* Interactions */
*Earnings
logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 c.structural_familism_t1#i.hh_earn_type_t1 `controls' if hh_earn_type_t1 < 4, or
outreg2 using "$results/dissolution_AMES_familism_sample_all.xls", sideway stats(coef pval) label ctitle(All 1a) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum structural_familism_t1, detail
margins, dydx(hh_earn_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_sample_all.xls", ctitle(All 1b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*Hours
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 `controls' if hh_hours_type_t1 < 4, or
outreg2 using "$results/dissolution_AMES_familism_sample_all.xls", sideway stats(coef pval) label ctitle(All 2a) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum structural_familism_t1, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_sample_all.xls", ctitle(All 2b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*Combined Earnings
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_t1 c.structural_familism_t1#i.division_bucket_t1 `controls', or
outreg2 using "$results/dissolution_AMES_familism_sample_all.xls", sideway stats(coef pval) label ctitle(All 3a) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum structural_familism_t1, detail
margins, dydx(division_bucket_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_sample_all.xls", ctitle(All 3b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

*Combined Hours
logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 `controls', or
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

local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled_t1 i.state_fips cohab_with_wife cohab_with_other pre_marital_birth i.interval i.home_owner_t1 earnings_bucket_t1 i.couple_educ_gp_t1 i.moved_last2 i.couple_joint_religion_t1" 

logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 `controls' if children_under6==1 & hh_earn_type_t1 < 4, or
margins, dydx(hh_earn_type_t1) level(95) post
estimates store est1

logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 `controls' if children_under6==1 & hh_hours_type_t1 < 4, or
margins, dydx(hh_hours_type_t1) level(95) post
estimates store est2

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_t1 `controls' if children_under6==1, or
margins, dydx(division_bucket_t1) level(95) post
estimates store est3

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 `controls' if children_under6==1, or
margins, dydx(division_bucket_hrs_t1) level(95) post
estimates store est4

coefplot est1 est2 est3 est4,  drop(_cons) nolabel xline(0) levels(95)

set scheme cleanplots

coefplot (est1, offset(.20) nokey) (est2, offset(.20) nokey) (est3, offset(-.20) nokey) (est4, offset(-.20) nokey), drop(_cons) xline(0) levels(90) base xtitle(Average Marginal Effect Relative to Egalitarian Arrangement, size(small)) ///
coeflabels(2.hh_earn_type_t1 = "Male Breadwinner" 3.hh_earn_type_t1 = "Female Breadwinner" 2.hh_hours_type_t1 = "Male Breadwinner" 3.hh_hours_type_t1 = "Female Breadwinner"  1.division_bucket_t1 = "Egalitarian" 2.division_bucket_t1 = "Traditional" 3.division_bucket_t1 = "Counter Traditional" 4.division_bucket_t1 = "Her Second Shift" 5.division_bucket_t1 = "All Others" 1.division_bucket_hrs_t1 = "Egalitarian" 2.division_bucket_hrs_t1 = "Traditional" 3.division_bucket_hrs_t1 = "Counter Traditional" 4.division_bucket_hrs_t1 = "Her Second Shift" 5.division_bucket_hrs_t1 = "All Others") ///
 headings(1.hh_earn_type_t1 = "{bf:Division of Earnings}" 1.hh_hours_type_t1= "{bf:Division of Work Hours}"  1.division_bucket_t1 = "{bf:Combined (Earnings)}"  1.division_bucket_hrs_t1 = "{bf:Combined (Hours)}")
 // (est3, offset(-.20) label(College)) 
 // coefplot (est1, offset(.20) nokey lcolor("dkgreen") mcolor("dkgreen") ciopts(color("dkgreen")))
 
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled_t1 i.state_fips cohab_with_wife cohab_with_other pre_marital_birth i.interval i.home_owner_t1 earnings_bucket_t1 i.couple_educ_gp_t1 i.moved_last2 i.couple_joint_religion_t1" 

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 `controls' if children_under6==1, or
margins, at(structural_familism_t1=(-5(1)10))
marginsplot, ytitle(Predicted Probability of Divorce) xtitle(Structural Support for Working Families) // splot1opts(lcolor("eltgreen") mcolor("eltgreen")) ci1opts(color("eltgreen"))
margins, dydx(structural_familism_t1)

********************************************************************************
* Earnings Interaction
********************************************************************************
/* parents of kids under 6 */
*Predicted Probabilities (no CI) -- see if I can do what Mize 2019 does (Figure 14) - make dotted when not significant
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled_t1 i.state_fips cohab_with_wife cohab_with_other pre_marital_birth i.interval i.home_owner_t1 earnings_bucket_t1 i.couple_educ_gp_t1 i.moved_last2 i.couple_joint_religion_t1" // i.num_children 

logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 c.structural_familism_t1#i.hh_earn_type_t1 `controls' if children_under6==1 & hh_earn_type_t1 < 4, or
sum structural_familism_t1, detail
margins hh_earn_type_t1, at(structural_familism_t1=(`r(min)'(1)`r(max)'))
marginsplot, xtitle("Structural Support for Dual-Earning") ytitle("Predicted Probability of Marital Dissolution") title("") legend(position(6) ring(3) rows(1)) noci recast(line) xlabel(#10) plot2opts(lcolor("navy") mcolor("navy")) plot3opts(lcolor("ltblue") mcolor("ltblue"))   // plot1opts(lcolor("gray") mcolor("gray")) xlabel(-3.12 "5th" -0.64 "25th" 1.27 "50th" 3.57 "75th" 12.48 "95th") ci1opts(color("navy")) ci2opts(color("ltblue")) yscale(range(-.1 .1)) ylabel(-.1(.05).1, angle(0))

* AMEs with CI
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled_t1 i.state_fips cohab_with_wife cohab_with_other pre_marital_birth i.interval i.home_owner_t1 earnings_bucket_t1 i.couple_educ_gp_t1 i.moved_last2 i.couple_joint_religion_t1" 

logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 c.structural_familism_t1#i.hh_earn_type_t1 `controls' if children_under6==1 & hh_earn_type_t1 < 4, or
sum structural_familism_t1, detail
margins, dydx(hh_earn_type_t1) at(structural_familism_t1=(`r(min)'(1)`r(max)')) // level(90)
marginsplot, xtitle("Structural Support for Dual-Earning") yline(0,lcolor(red))  ytitle("Average Marginal Effects (relative to dual-earning)") title("") legend(position(6) ring(3) order(1 "Male BW" 2 "Female BW") rows(1)) xlabel(#10) recast(line) recastci(rarea) plot1opts(lcolor("navy") mcolor("navy")) ci1opts(color(navy%70)) plot2opts(lcolor("bluishgray") mcolor("ltblue")) ci2opts(color(ltblue%40)) //  yscale(range(-.1 .1)) ylabel(-.1(.05).1, angle(0))

/* all parents */
*Predicted Probabilities (no CI)
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled_t1 i.state_fips cohab_with_wife cohab_with_other pre_marital_birth i.interval i.home_owner_t1 earnings_bucket_t1 i.couple_educ_gp_t1 i.moved_last2 i.couple_joint_religion_t1" // i.num_children 

logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 c.structural_familism_t1#i.hh_earn_type_t1 `controls' if children==1 & hh_earn_type_t1 < 4, or
sum structural_familism_t1, detail
margins hh_earn_type_t1, at(structural_familism_t1=(`r(min)'(1)`r(max)'))
marginsplot, xtitle("Structural Support for Dual-Earning") ytitle("Predicted Probability of Marital Dissolution") title("") legend(position(6) ring(3) rows(1)) noci recast(line) xlabel(#10) plot2opts(lcolor("navy") mcolor("navy")) plot3opts(lcolor("ltblue") mcolor("ltblue")) 

* AMEs with CI
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled_t1 i.state_fips cohab_with_wife cohab_with_other pre_marital_birth i.interval i.home_owner_t1 earnings_bucket_t1 i.couple_educ_gp_t1 i.moved_last2 i.couple_joint_religion_t1" 

logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 c.structural_familism_t1#i.hh_earn_type_t1 `controls' if children==1 & hh_earn_type_t1 < 4, or
sum structural_familism_t1, detail
margins, dydx(hh_earn_type_t1) at(structural_familism_t1=(`r(min)'(1)`r(max)')) // level(90)
marginsplot, xtitle("Structural Support for Dual-Earning") yline(0,lcolor(red))  ytitle("Average Marginal Effects (relative to dual-earning)") title("") legend(position(6) ring(3) order(1 "Male BW" 2 "Female BW") rows(1)) xlabel(#10) recast(line) recastci(rarea) plot1opts(lcolor("navy") mcolor("navy")) ci1opts(color(navy%70)) plot2opts(lcolor("bluishgray") mcolor("ltblue")) ci2opts(color(ltblue%40))

/* total sample */
*Predicted Probabilities (no CI)
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled_t1 i.state_fips cohab_with_wife cohab_with_other pre_marital_birth i.interval i.home_owner_t1 earnings_bucket_t1 i.couple_educ_gp_t1 i.moved_last2 i.couple_joint_religion_t1 i.num_children" 

logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 c.structural_familism_t1#i.hh_earn_type_t1 `controls' if hh_earn_type_t1 < 4, or
sum structural_familism_t1, detail
margins hh_earn_type_t1, at(structural_familism_t1=(`r(min)'(1)`r(max)'))
marginsplot, xtitle("Structural Support for Dual-Earning") ytitle("Predicted Probability of Marital Dissolution") title("") legend(position(6) ring(3) rows(1)) noci recast(line) xlabel(#10) plot2opts(lcolor("navy") mcolor("navy")) plot3opts(lcolor("ltblue") mcolor("ltblue")) 

* AMEs with CI
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled_t1 i.state_fips cohab_with_wife cohab_with_other pre_marital_birth i.interval i.home_owner_t1 earnings_bucket_t1 i.couple_educ_gp_t1 i.moved_last2 i.couple_joint_religion_t1 i.num_children" 

logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 c.structural_familism_t1#i.hh_earn_type_t1 `controls' if hh_earn_type_t1 < 4, or
sum structural_familism_t1, detail
margins, dydx(hh_earn_type_t1) at(structural_familism_t1=(`r(min)'(1)`r(max)')) // level(90)
marginsplot, xtitle("Structural Support for Dual-Earning") yline(0,lcolor(red))  ytitle("Average Marginal Effects (relative to dual-earning)") title("") legend(position(6) ring(3) order(1 "Male BW" 2 "Female BW") rows(1)) xlabel(#10) recast(line) recastci(rarea) plot1opts(lcolor("navy") mcolor("navy")) ci1opts(color(navy%70)) plot2opts(lcolor("bluishgray") mcolor("ltblue")) ci2opts(color(ltblue%40))

********************************************************************************
**# Hours instead of earnings
********************************************************************************
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled_t1 i.state_fips cohab_with_wife cohab_with_other pre_marital_birth i.interval i.home_owner_t1 earnings_bucket_t1 i.couple_educ_gp_t1 i.moved_last2 i.couple_joint_religion_t1"

/* parents of kids under 6 */
* Predicted Probabilities
logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 `controls' if children_under6==1 & hh_hours_type_t1 < 4, or

sum structural_familism_t1, detail
margins hh_hours_type_t1, at(structural_familism_t1=(`r(min)'(1)`r(max)'))
marginsplot, xtitle("Structural Support for Dual-Earning") ytitle("Predicted Probability of Marital Dissolution") title("") legend(position(6) ring(3) rows(1)) noci recast(line) xlabel(#10) plot2opts(lcolor("navy") mcolor("navy")) plot3opts(lcolor("ltblue") mcolor("ltblue"))  

// AMEs
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled_t1 i.state_fips cohab_with_wife cohab_with_other pre_marital_birth i.interval i.home_owner_t1 earnings_bucket_t1 i.couple_educ_gp_t1 i.moved_last2 i.couple_joint_religion_t1"

logit dissolve i.dur c.structural_familism_t1 i.hh_hours_type_t1 c.structural_familism_t1#i.hh_hours_type_t1 `controls' if children_under6==1 & hh_hours_type_t1 < 4, or

sum structural_familism_t1, detail
margins, dydx(hh_hours_type_t1) at(structural_familism_t1=(`r(min)'(1)`r(max)')) // level(90)
marginsplot, xtitle("Structural Support for Dual-Earning") yline(0,lcolor(red))  ytitle("Average Marginal Effects (relative to dual-earning)") title("") legend(position(6) ring(3) order(1 "Male BW" 2 "Female BW") rows(1)) xlabel(#10) recast(line) recastci(rarea) plot1opts(lcolor("navy") mcolor("navy")) ci1opts(color(navy%70)) plot2opts(lcolor("bluishgray") mcolor("ltblue")) ci2opts(color(ltblue%40))


********************************************************************************
**# Combined paid and unpaid labor: based on earnings
********************************************************************************
/* parents of kids under 6 */
* Predicted probabilities
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled_t1 i.state_fips cohab_with_wife cohab_with_other pre_marital_birth i.interval i.home_owner_t1 earnings_bucket_t1 i.couple_educ_gp_t1 i.moved_last2 i.couple_joint_religion_t1"

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_t1 c.structural_familism_t1#i.division_bucket_t1 `controls' if children_under6==1, or
sum structural_familism_t1, detail
margins division_bucket_t1, at(structural_familism_t1=(`r(min)'(1)`r(max)'))
marginsplot, xtitle("Structural Support for Dual-Earning") ytitle("Predicted Probability of Marital Dissolution") title("") legend(position(6) ring(3) rows(1)) noci recast(line) xlabel(#10) plot2opts(lcolor("blue") mcolor("blue")) plot3opts(lcolor("ltblue") mcolor("ltblue")) plot4opts(lcolor("navy") mcolor("navy"))   plot5opts(lcolor("gs8") mcolor("gs8")) 

* AMEs with CI
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled_t1 i.state_fips cohab_with_wife cohab_with_other pre_marital_birth i.interval i.home_owner_t1 earnings_bucket_t1 i.couple_educ_gp_t1 i.moved_last2 i.couple_joint_religion_t1" 

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_t1 c.structural_familism_t1#i.division_bucket_t1 `controls' if children_under6==1, or
sum structural_familism_t1, detail
margins, dydx(division_bucket_t1) at(structural_familism_t1=(`r(min)'(1)`r(max)'))
marginsplot, xtitle("Structural Support for Dual-Earning") yline(0,lcolor(red))  ytitle("Average Marginal Effects (relative to dual-earning)") title("") legend(position(6) ring(3) order(1 "Traditional" 2 "Counter" 3 "Second Shift" 4 "Other") rows(1)) xlabel(#10) recast(line)  recastci(rarea) plot1opts(lcolor("blue") mcolor("blue")) ci1opts(color(blue%40)) plot2opts(lcolor("bluishgray") mcolor("ltblue")) ci2opts(color(none)) plot3opts(lcolor("green") mcolor("green")) ci3opts(color(green%40)) plot4opts(lcolor("gs12") mcolor("gs12")) ci4opts(color(none)) // recastci(rarea)


********************************************************************************
**# Combined paid and unpaid labor: based on hours
********************************************************************************
/* parents of kids under 6 */
* Predicted probabilities
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled_t1 i.state_fips cohab_with_wife cohab_with_other pre_marital_birth i.interval i.home_owner_t1 earnings_bucket_t1 i.couple_educ_gp_t1 i.moved_last2 i.couple_joint_religion_t1" 

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 `controls' if children_under6==1 & division_bucket_hrs_t1!=3, or
sum structural_familism_t1, detail
margins division_bucket_hrs_t1, at(structural_familism_t1=(`r(min)'(1)`r(max)'))
marginsplot, xtitle("Structural Support for Dual-Earning") ytitle("Predicted Probability of Marital Dissolution") title("") legend(position(6) ring(3) rows(1)) noci recast(line) xlabel(#10) plot2opts(lcolor("blue") mcolor("blue")) plot3opts(lcolor("navy") mcolor("navy")) plot4opts(lcolor("gs8") mcolor("gs8"))  // plot5opts(lcolor("gs8") mcolor("gs8"))  

**AMEs with CIs
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled_t1 i.state_fips cohab_with_wife cohab_with_other pre_marital_birth i.interval i.home_owner_t1 earnings_bucket_t1 i.couple_educ_gp_t1 i.moved_last2 i.couple_joint_religion_t1" 

logit dissolve i.dur c.structural_familism_t1 i.division_bucket_hrs_t1 c.structural_familism_t1#i.division_bucket_hrs_t1 `controls' if children_under6==1 & division_bucket_hrs_t1!=3, or
sum structural_familism_t1, detail
margins, dydx(division_bucket_hrs_t1) at(structural_familism_t1=(`r(min)'(1)`r(max)'))
marginsplot, xtitle("Structural Support for Dual-Earning") yline(0,lcolor(red))  ytitle("Average Marginal Effects (relative to dual-earning)") title("") legend(position(6) ring(3) order(1 "Traditional" 2 "Second Shift" 3 "Other") rows(1)) xlabel(#10) recast(line)  recastci(rarea) plot1opts(lcolor("blue") mcolor("blue")) ci1opts(color(blue%40)) plot2opts(lcolor("green") mcolor("green")) ci2opts(color(green%40)) plot3opts(lcolor("gs12") mcolor("gs12")) ci3opts(color(none)) // recastci(rarea) plot2opts(lcolor("bluishgray") mcolor("ltblue"))


********************************************************************************
********************************************************************************
**# Individual indicators of scale
********************************************************************************
********************************************************************************
* Paid Leave
logit dissolve i.dur i.paid_leave_length i.hh_earn_type_t1 i.paid_leave_length#i.hh_earn_type_t1 `controls' if children_under6==1 & hh_earn_type_t1 < 4, or
margins, dydx(hh_earn_type_t1) at(paid_leave_length=(0 6 12)) post
outreg2 using "$results/dissolution_AMES_familism_parents.xls", ctitle(parent paidleave) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* PreK Enrollment
logit dissolve i.dur c.prek_enrolled_public i.hh_earn_type_t1 c.prek_enrolled_public#i.hh_earn_type_t1 `controls' if children_under6==1 & hh_earn_type_t1 < 4, or
sum prek_enrolled_public, detail
margins, dydx(hh_earn_type_t1) at(prek_enrolled_public=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_parents.xls", ctitle(parent prek) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Min Wage
logit dissolve i.dur c.min_amt_above_fed i.hh_earn_type_t1 c.min_amt_above_fed#i.hh_earn_type_t1 `controls' if children_under6==1 & hh_earn_type_t1 < 4, or
margins, dydx(hh_earn_type_t1) at(min_amt_above_fed=(0(1)5)) post
outreg2 using "$results/dissolution_AMES_familism_parents.xls", ctitle(parent minwage) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Earnings Ratio
logit dissolve i.dur c.earn_ratio i.hh_earn_type_t1 c.earn_ratio#i.hh_earn_type_t1 `controls' if children_under6==1 & hh_earn_type_t1 < 4, or
sum earn_ratio, detail
margins, dydx(hh_earn_type_t1) at(earn_ratio=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_parents.xls", ctitle(parent earnings) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Unemployment Compensation
logit dissolve i.dur c.unemployment_percap i.hh_earn_type_t1 c.unemployment_percap#i.hh_earn_type_t1 `controls' if children_under6==1 & hh_earn_type_t1 < 4, or
sum unemployment_percap, detail
margins, dydx(hh_earn_type_t1) at(unemployment_percap=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_parents.xls", ctitle(parent unemployment) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Abortion protected
logit dissolve i.dur i.abortion_protected i.hh_earn_type_t1 i.abortion_protected#i.hh_earn_type_t1 `controls' if children_under6==1 & hh_earn_type_t1 < 4, or
margins, dydx(2.hh_earn_type_t1) at(abortion_protected=(0 1)) post // had to update bc #3 is collinnear
outreg2 using "$results/dissolution_AMES_familism_parents.xls", ctitle(parent abortion) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Welfare Expenditures
logit dissolve i.dur c.welfare_all i.hh_earn_type_t1 c.welfare_all#i.hh_earn_type_t1 `controls' if children_under6==1 & hh_earn_type_t1 < 4, or
sum welfare_all, detail
margins, dydx(hh_earn_type_t1) at(welfare_all=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_parents.xls", ctitle(parent welfare) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

********************************************************************************
********************************************************************************
**# Results by level of education
********************************************************************************
********************************************************************************

local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.race_head i.same_race i.either_enrolled i.state_fips cohab_with_wife cohab_with_other pre_marital_birth  i.num_children i.interval i.home_owner knot1 knot2 knot3"

/* College x Parental Status */
logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 c.structural_familism_t1#i.hh_earn_type_t1 `controls' if children_under6==1 & couple_educ_gp==1 & hh_earn_type_t1 < 4, or
outreg2 using "$results/dissolution_AMES_familism_educ.xls", sideway stats(coef pval) label ctitle(Coll Par) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace
sum structural_familism_t1, detail
margins, dydx(hh_earn_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_educ.xls", ctitle(col parent) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 c.structural_familism_t1#i.hh_earn_type_t1 `controls' if children_under6==0 & couple_educ_gp==1 & hh_earn_type_t1 < 4, or
outreg2 using "$results/dissolution_AMES_familism_educ.xls", sideway stats(coef pval) label ctitle(Coll No) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum structural_familism_t1, detail
margins, dydx(hh_earn_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_educ.xls", ctitle(col no parent) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

/* No College x Parental Status */
logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 c.structural_familism_t1#i.hh_earn_type_t1 `controls' if children_under6==1 & couple_educ_gp==0 & hh_earn_type_t1 < 4, or
outreg2 using "$results/dissolution_AMES_familism_educ.xls", sideway stats(coef pval) label ctitle(No Par) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum structural_familism_t1, detail
margins, dydx(hh_earn_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_educ.xls", ctitle(no parent) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 c.structural_familism_t1#i.hh_earn_type_t1 `controls' if children_under6==0 & couple_educ_gp==0 & hh_earn_type_t1 < 4, or
outreg2 using "$results/dissolution_AMES_familism_educ.xls", sideway stats(coef pval) label ctitle(No No) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum structural_familism_t1, detail
margins, dydx(hh_earn_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_educ.xls", ctitle(no no parent) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

********************************************************************************
* Figures
********************************************************************************
// Non-college-educated (parents of kids under 6)
*Predicted Probabilities (no CI)
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_wife cohab_with_other pre_marital_birth i.interval i.home_owner knot1 knot2 knot3 i.couple_educ_gp"  

logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 c.structural_familism_t1#i.hh_earn_type_t1 `controls' if children_under6==1 & hh_earn_type_t1 < 4 & state_fips!=11 & couple_educ_gp==0, or
sum structural_familism_t1, detail
margins hh_earn_type_t1, at(structural_familism_t1=(`r(min)'(1)`r(max)'))
marginsplot, xtitle("Structural Support for Dual-Earning") ytitle("Predicted Probability of Marital Dissolution") title("") legend(position(6) ring(3) rows(1)) noci recast(line) xlabel(#10) plot2opts(lcolor("navy") mcolor("navy")) plot3opts(lcolor("ltblue") mcolor("ltblue")) 

* AMEs with CI
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_wife cohab_with_other pre_marital_birth i.interval i.home_owner knot1 knot2 knot3 i.couple_educ_gp"  

logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 c.structural_familism_t1#i.hh_earn_type_t1 `controls' if children_under6==1 & hh_earn_type_t1 < 4 & state_fips!=11 & couple_educ_gp==0, or
sum structural_familism_t1, detail
margins, dydx(hh_earn_type_t1) at(structural_familism_t1=(`r(min)'(1)`r(max)')) 
marginsplot, xtitle("Structural Support for Dual-Earning") yline(0,lcolor(red))  ytitle("Average Marginal Effects (relative to dual-earning)") title("") legend(position(6) ring(3) order(1 "Male BW" 2 "Female BW") rows(1)) xlabel(#10) recast(line) recastci(rarea) plot1opts(lcolor("navy") mcolor("navy")) ci1opts(color(navy%70)) plot2opts(lcolor("bluishgray") mcolor("ltblue")) ci2opts(color(ltblue%70)) 

// College-educated (parents of kids under 6)
*Predicted Probabilities (no CI)
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_wife cohab_with_other pre_marital_birth i.interval i.home_owner knot1 knot2 knot3 i.couple_educ_gp"  

logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 c.structural_familism_t1#i.hh_earn_type_t1 `controls' if children_under6==1 & hh_earn_type_t1 < 4 & state_fips!=11 & couple_educ_gp==1, or
sum structural_familism_t1, detail
margins hh_earn_type_t1, at(structural_familism_t1=(`r(min)'(1)`r(max)'))
marginsplot, xtitle("Structural Support for Dual-Earning") ytitle("Predicted Probability of Marital Dissolution") title("") legend(position(6) ring(3) rows(1)) noci recast(line) xlabel(#10) plot2opts(lcolor("navy") mcolor("navy")) plot3opts(lcolor("ltblue") mcolor("ltblue")) 

* AMEs with CI
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_wife cohab_with_other pre_marital_birth i.interval i.home_owner knot1 knot2 knot3 i.couple_educ_gp"  

logit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 c.structural_familism_t1#i.hh_earn_type_t1 `controls' if children_under6==1 & hh_earn_type_t1 < 4 & state_fips!=11 & couple_educ_gp==1, or
sum structural_familism_t1, detail
margins, dydx(hh_earn_type_t1) at(structural_familism_t1=(`r(min)'(1)`r(max)')) 
marginsplot, xtitle("Structural Support for Dual-Earning") yline(0,lcolor(red))  ytitle("Average Marginal Effects (relative to dual-earning)") title("") legend(position(6) ring(3) order(1 "Male BW" 2 "Female BW") rows(1)) xlabel(#10) recast(line) recastci(rarea) plot1opts(lcolor("navy") mcolor("navy")) ci1opts(color(navy%70)) plot2opts(lcolor("bluishgray") mcolor("ltblue")) ci2opts(color(ltblue%70)) 

********************************************************************************
********************************************************************************
**# Other analyses / metrics
********************************************************************************
********************************************************************************

********************************************************************************
* Median centered
********************************************************************************
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.raceth_head_fixed i.same_race i.either_enrolled i.state_fips cohab_with_wife cohab_with_other pre_marital_birth i.interval i.home_owner knot1 knot2 knot3 i.couple_educ_gp"  // i.num_children i.region

/* Parents */
*Baseline model
logit dissolve i.dur c.sf_centered_alt i.hh_earn_type `controls' if children_under6==1 & hh_earn_type < 4, or
outreg2 using "$results/dissolution_AMES_familism_centered.xls", sideway stats(coef pval) label ctitle(Parents 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

* Structural familism
logit dissolve i.dur c.sf_centered_alt i.hh_earn_type c.sf_centered_alt#i.hh_earn_type `controls' if children_under6==1 & hh_earn_type < 4, or
outreg2 using "$results/dissolution_AMES_familism_centered.xls", sideway stats(coef pval) label ctitle(Parents 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum sf_centered_alt, detail
margins, dydx(hh_earn_type) at(sf_centered_alt=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_centered.xls", ctitle(parent familism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* All parents (robustness)
logit dissolve i.dur c.sf_centered_alt i.hh_earn_type c.sf_centered_alt#i.hh_earn_type `controls' if children==1 & hh_earn_type < 4, or
sum sf_centered_alt, detail
margins, dydx(hh_earn_type) at(sf_centered_alt=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_centered.xls", ctitle(allparent familism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Full sample (robustness)
logit dissolve i.dur c.sf_centered_alt i.hh_earn_type c.sf_centered_alt#i.hh_earn_type i.num_children `controls' if hh_earn_type < 4, or
sum sf_centered_alt, detail
margins, dydx(hh_earn_type) at(sf_centered_alt=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism_centered.xls", ctitle(all familism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

********************************************************************************
* Mixed effects
* Not using because they are exactly the same 
********************************************************************************
/*
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.race_head i.same_race i.either_enrolled i.state_fips cohab_with_wife cohab_with_other pre_marital_birth i.interval i.home_owner knot1 knot2 knot3 i.couple_educ_gp"  // i.num_children i.region

/* Mixed effects */
*Baseline model
melogit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 `controls' if children_under6==1 & hh_earn_type_t1 < 4 || state_fips: , or
outreg2 using "$results/dissolution_AMES_melogit.xls", sideway stats(coef pval) label ctitle(Parents 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

* Structural familism
melogit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 c.structural_familism_t1#i.hh_earn_type_t1 `controls' if children_under6==1 & hh_earn_type_t1 < 4 || state_fips: , or
outreg2 using "$results/dissolution_AMES_melogit.xls", sideway stats(coef pval) label ctitle(Parents 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum structural_familism_t1, detail
margins, dydx(hh_earn_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_melogit.xls", ctitle(parent familism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Paid Leave
melogit dissolve i.dur i.paid_leave i.hh_earn_type_t1 i.paid_leave#i.hh_earn_type_t1 `controls' if children_under6==1 & hh_earn_type_t1 < 4 || state_fips: , or
margins, dydx(hh_earn_type_t1) at(paid_leave=(0 1)) post
outreg2 using "$results/dissolution_AMES_melogit.xls", ctitle(parent paidleave) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* PreK Enrollment
melogit dissolve i.dur c.prek_enrolled_public i.hh_earn_type_t1 c.prek_enrolled_public#i.hh_earn_type_t1 `controls' if children_under6==1 & hh_earn_type_t1 < 4 || state_fips: , or
sum prek_enrolled_public, detail
margins, dydx(hh_earn_type_t1) at(prek_enrolled_public=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_melogit.xls", ctitle(parent prek) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Min Wage
melogit dissolve i.dur i.min_above_fed i.hh_earn_type_t1 i.min_above_fed#i.hh_earn_type_t1 `controls' if children_under6==1 & hh_earn_type_t1 < 4 || state_fips: , or
margins, dydx(hh_earn_type_t1) at(min_above_fed=(0 1)) post
outreg2 using "$results/dissolution_AMES_melogit.xls", ctitle(parent minwage) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Earnings Ratio
melogit dissolve i.dur c.earn_ratio i.hh_earn_type_t1 c.earn_ratio#i.hh_earn_type_t1 `controls' if children_under6==1 & hh_earn_type_t1 < 4 || state_fips: , or
sum earn_ratio, detail
margins, dydx(hh_earn_type_t1) at(earn_ratio=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_melogit.xls", ctitle(parent earnings) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Unemployment Compensation
melogit dissolve i.dur c.unemployment_percap i.hh_earn_type_t1 c.unemployment_percap#i.hh_earn_type_t1 `controls' if children_under6==1 & hh_earn_type_t1 < 4 || state_fips: , or
sum unemployment_percap, detail
margins, dydx(hh_earn_type_t1) at(unemployment_percap=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_melogit.xls", ctitle(parent unemployment) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Abortion protected
melogit dissolve i.dur i.abortion_protected i.hh_earn_type_t1 i.abortion_protected#i.hh_earn_type_t1 `controls' if children_under6==1 & hh_earn_type_t1 < 4 || state_fips: , or
margins, dydx(2.hh_earn_type_t1) at(abortion_protected=(0 1)) post // had to update bc #3 is collinnear
outreg2 using "$results/dissolution_AMES_melogit.xls", ctitle(parent abortion) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Welfare Expenditures
melogit dissolve i.dur c.welfare_all i.hh_earn_type_t1 c.welfare_all#i.hh_earn_type_t1 `controls' if children_under6==1 & hh_earn_type_t1 < 4 || state_fips: , or
sum welfare_all, detail
margins, dydx(hh_earn_type_t1) at(welfare_all=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_melogit.xls", ctitle(parent welfare) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* All parents (robustness)
melogit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 c.structural_familism_t1#i.hh_earn_type_t1 `controls' if children==1 & hh_earn_type_t1 < 4 || state_fips: , or
sum structural_familism_t1, detail
margins, dydx(hh_earn_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_melogit.xls", ctitle(allparent familism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Full sample (robustness)
melogit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 c.structural_familism_t1#i.hh_earn_type_t1 i.num_children `controls' if hh_earn_type_t1 < 4 || state_fips: , or
sum structural_familism_t1, detail
margins, dydx(hh_earn_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_melogit.xls", ctitle(all familism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* First 10 years of marriage (robustness)
melogit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1 c.structural_familism_t1#i.hh_earn_type_t1 `controls' if children_under6==1 & hh_earn_type_t1 < 4 & dur<=10 || state_fips: , or
outreg2 using "$results/dissolution_AMES_melogit.xls", sideway stats(coef pval) label ctitle(dur) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
sum structural_familism_t1, detail
margins, dydx(hh_earn_type_t1) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_melogit.xls", ctitle(dur) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Alt DoL (robustness)
melogit dissolve i.dur c.structural_familism_t1 i.hh_earn_type_t1_alt c.structural_familism_t1#i.hh_earn_type_t1_alt `controls' if children_under6==1 || state_fips: , or
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
// for ref: local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth i.interval i.home_owner knot1 knot2 knot3 i.couple_educ_gp"  // i.num_children

tab hh_earn_type, gen(earn_type)
tab race_head, gen(race_head)
tab region, gen(region)

putexcel set "$results/Table1_Descriptives_chapter3", replace
putexcel B1:C1 = "Parents of children under 6", merge border(bottom)
putexcel D1:E1 = "Total Sample", merge border(bottom)
putexcel B2 = ("All") C2 = ("Dissolved") D2 = ("All") E2 = ("Dissolved")
putexcel A3 = "Unique Couples"

putexcel A4 = "Dual Earning HH"
putexcel A5 = "Male Breadwinner"
putexcel A6 = "Female Breadwinner"
putexcel A7 = "Structural Support for Working Families"
putexcel A8 = "Total Couple Earnings"
putexcel A9 = "At least one partner has college degree"
putexcel A10 = "Couple owns home"
putexcel A11 = "Husband's age at marriage"
putexcel A12 = "Wife's age at marriage"
putexcel A13 = "Husband's Race: White"
putexcel A14 = "Husband's Race: Black"
putexcel A15 = "Husband's Race: Indian"
putexcel A16 = "Husband's Race: Asian"
putexcel A17 = "Husband's Race: Latino"
putexcel A18 = "Husband's Race: Other"
putexcel A19 = "Husband's Race: Multiracial"
putexcel A20 = "Husband and wife same race"
putexcel A21 = "Either partner enrolled in school"
putexcel A22 = "Region: Northeast"
putexcel A23 = "Region: North Central"
putexcel A24 = "Region: South"
putexcel A25 = "Region: West"
putexcel A26 = "Region: Alaska, Hawaii"
putexcel A27 = "Husband Wife Cohabit"
putexcel A28 = "Other Premarital Cohabit"
putexcel A29 = "First Birth Premarital"

local meanvars "earn_type1 earn_type2 earn_type3 structural_familism couple_earnings couple_educ_gp home_owner age_mar_head age_mar_wife race_head1 race_head2 race_head3 race_head4 race_head5 race_head6 race_head7 same_race either_enrolled region1 region2 region3 region4 region5 cohab_with_wife cohab_with_other pre_marital_birth"

// Parents
forvalues w=1/26{
	local row=`w'+3
	local var: word `w' of `meanvars'
	mean `var' if children_under6==1
	matrix t`var'= e(b)
	putexcel B`row' = matrix(t`var'), nformat(#.#%)
}

// those who dissolved; value when outcome==1
forvalues w=1/26{
	local row=`w'+3
	local var: word `w' of `meanvars' 
	mean `var' if outcome==1 & children_under6==1
	matrix t`var'= e(b)
	putexcel C`row' = matrix(t`var'), nformat(#.#%)
}


// All couples
forvalues w=1/26{
	local row=`w'+3
	local var: word `w' of `meanvars'
	mean `var'
	matrix t`var'= e(b)
	putexcel D`row' = matrix(t`var'), nformat(#.#%)
}

// those who dissolved; value when outcome==1
forvalues w=1/26{
	local row=`w'+3
	local var: word `w' of `meanvars'
	mean `var' if outcome==1
	matrix t`var'= e(b)
	putexcel E`row' = matrix(t`var'), nformat(#.#%)
}

mean dur if children_under6==1
mean dur if children_under6==1 & outcome==1
mean dur
mean dur if outcome==1

unique id if children_under6==1
unique id if children_under6==1 & outcome==1
unique id 
unique id if outcome==1

********************************************************************************
**# * Does it matter how "male-BW" and "dual-earning" are operationalized?
** (NOT updated past this point - Feb 2025)
********************************************************************************
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth i.interval i.home_owner knot1 knot2 knot3 i.couple_educ_gp"  // i.num_children

* current def
logit dissolve_lag i.dur c.structural_familism i.hh_earn_type c.structural_familism#i.hh_earn_type `controls' if children_under6==1 & hh_earn_type < 4, or
sum structural_familism, detail
margins, dydx(hh_earn_type) at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
sum structural_familism, detail
margins hh_earn_type, at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
marginsplot

* alt type
logit dissolve_lag i.dur c.structural_familism i.bw_type_gp c.structural_familism#i.bw_type_gp `controls' if children_under6==1, or
sum structural_familism, detail
margins, dydx(bw_type_gp) at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
sum structural_familism, detail
margins bw_type_gp, at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
marginsplot

* alt type
logit dissolve_lag i.dur c.structural_familism i.bw_type_gp_alt c.structural_familism#i.bw_type_gp_alt `controls' if children_under6==1, or
sum structural_familism, detail
margins, dydx(bw_type_gp_alt) at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
sum structural_familism, detail
margins bw_type_gp_alt, at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
marginsplot

* alt type 2
logit dissolve_lag i.dur c.structural_familism ib3.bw_type c.structural_familism#ib3.bw_type `controls' if children_under6==1, or
sum structural_familism, detail
margins, dydx(bw_type) at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

* just men's FT employment
logit dissolve_lag i.dur c.structural_familism i.ft_head c.structural_familism#i.ft_head `controls' if children_under6==1, or
sum structural_familism, detail
margins, dydx(ft_head) at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
sum structural_familism, detail
margins ft_head, at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
marginsplot

* men's earnings

* just women's FT employment
logit dissolve_lag i.dur c.structural_familism i.ft_wife c.structural_familism#i.ft_wife `controls' if children_under6==1, or
sum structural_familism, detail
margins, dydx(ft_wife) at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
sum structural_familism, detail
margins ft_wife, at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
marginsplot

* women's earnings

tab ft_head hh_earn_type, row
tab ft_head hh_earn_type, col // see, this is the problem - 93% of dual-earning couples have a husband working FT. and 95% of male BW do. so yes, obviously less male BW within FT head being 0 but FT employment of the head tells us v. little about whether or not they are male BW or dual-earning, which is why i don't like this definition. it's like his employment is a given - but whose employment is bringing home the most money - aka perhaps being prioritized? because if we say both employed FT - it's v. likely her work is not comparable to his, so it's like hard to say if ideologically they are seen as equal. whereas money might tell us that? use the motherhood wage penalty as motivation? both FT masks the actual dynamics of types of employment and earnings and flex work and such. also like Phil Cohen's argument - men are usually SOLE provider while women aren't.

tab ft_wife hh_earn_type, row
tab ft_wife hh_earn_type, col // okay so in "female BW" households - men are still 50% likely to work FT. but in male BW households, women are only 35% likely to work FT. This is really Phil Cohen's article I think. so if we assume both partners are working - money is a better distinguisher? (to Oppenheimer's point). and that is the GENDER nuance too. like it isn't just WORK but the resources provided from work. Also Gupta and such has some of this. like the gendered meaning of earnings. I tihnk this relates to Gerson / Pedulla as well - equity.

tab bw_type_gp hh_earn_type, row // like only 50% of "both FT" are considered dual-earning. 36% are male BW in terms of money
tab bw_type_gp hh_earn_type, col


********************************************************************************
**# State-chart
********************************************************************************
gen dual_earners=0
replace dual_earners=1 if hh_earn_type==1

gen dual_earners_div=0
replace dual_earners_div=1 if hh_earn_type==1 & dissolve_lag==1

gen male_bw=0
replace male_bw=1 if hh_earn_type==2

gen male_bw_div=0
replace male_bw_div=1 if hh_earn_type==2 & dissolve_lag==1

gen female_bw=0
replace female_bw=1 if hh_earn_type==3

gen female_bw_div=0
replace female_bw_div=1 if hh_earn_type==3 & dissolve_lag==1

preserve

collapse 	(mean) structural_familism															///
			(sum) dual_earners dual_earners_div male_bw male_bw_div female_bw female_bw_div, ///
			by(state_fips state)
		
gen dual_rate = dual_earners_div / dual_earners
gen male_rate = male_bw_div / male_bw
gen female_rate = female_bw_div / female_bw

browse

restore

tab hh_earn_type dissolve_lag if state_fips==17, row

********************************************************************************
********************************************************************************
**# Margins: using percentiles for "high" and "low" to get figures
********************************************************************************
********************************************************************************

local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.race_head i.same_race i.either_enrolled i.state_fips cohab_with_wife cohab_with_other pre_marital_birth  i.num_children i.interval i.home_owner knot1 knot2 knot3"

// Total Sample
logit dissolve_lag i.dur c.structural_familism i.hh_earn_type c.structural_familism#i.hh_earn_type `controls' i.couple_educ_gp if hh_earn_type < 4 & state_fips!=11, or
margins, dydx(hh_earn_type) at(structural_familism=(-5(1)10))
sum structural_familism, detail
margins hh_earn_type, at(structural_familism=(`r(p25)' `r(p75)'))
sum structural_familism, detail
margins, dydx(hh_earn_type) at(structural_familism=(`r(p25)' `r(p75)'))

sum structural_familism, detail
margins, dydx(hh_earn_type) at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
marginsplot, xtitle("Structural Familism Scale: percentiles") yline(0,lcolor(gs3))  ytitle("Average Marginal Effects: Marital Dissolution") title("") legend(position(6) ring(3) order(1 "Male BW" 2 "Female BW") rows(1)) xlabel(-3.12 "5th" -0.64 "25th" 1.27 "50th" 3.57 "75th" 12.48 "95th") yscale(range(-.1 .1)) ylabel(-.1(.05).1, angle(0))

// No College
logit dissolve_lag i.dur c.structural_familism i.hh_earn_type c.structural_familism#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 & state_fips!=11, or
margins, dydx(hh_earn_type) at(structural_familism=(-5(1)10))
sum structural_familism, detail
margins hh_earn_type, at(structural_familism=(`r(p25)' `r(p75)'))
sum structural_familism, detail
margins, dydx(hh_earn_type) at(structural_familism=(`r(p25)' `r(p75)'))

sum structural_familism, detail
margins, dydx(hh_earn_type) at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
marginsplot, xtitle("Structural Familism Scale: percentiles") yline(0,lcolor(gs3))  ytitle("Average Marginal Effects: Marital Dissolution") title("") legend(position(6) ring(3) order(1 "Male BW" 2 "Female BW") rows(1)) xlabel(-3.12 "5th" -0.64 "25th" 1.27 "50th" 3.57 "75th" 12.48 "95th") yscale(range(-.1 .1)) ylabel(-.1(.05).1, angle(0))

// College
logit dissolve_lag i.dur c.structural_familism i.hh_earn_type c.structural_familism#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 & state_fips!=11, or
sum structural_familism, detail
margins hh_earn_type, at(structural_familism=(`r(p25)' `r(p75)'))
sum structural_familism, detail
margins, dydx(hh_earn_type) at(structural_familism=(`r(p25)' `r(p75)'))

sum structural_familism, detail
margins, dydx(hh_earn_type) at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
marginsplot, xtitle("Structural Familism Scale: percentiles") yline(0,lcolor(gs3))  ytitle("Average Marginal Effects: Marital Dissolution") title("") legend(position(6) ring(3) order(1 "Male BW" 2 "Female BW") rows(1)) xlabel(-3.12 "5th" -0.64 "25th" 1.27 "50th" 3.57 "75th" 12.48 "95th") yscale(range(-.1 .1)) ylabel(-.1(.05).1, angle(0))


// Parents
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.race_head i.same_race i.either_enrolled i.state_fips cohab_with_wife cohab_with_other pre_marital_birth i.interval i.home_owner knot1 knot2 knot3 i.couple_educ_gp"  // i.num_children

logit dissolve_lag i.dur c.structural_familism i.hh_earn_type c.structural_familism#i.hh_earn_type `controls' if children_under6==1 & hh_earn_type < 4 & state_fips!=11, or
sum structural_familism, detail
margins, dydx(hh_earn_type) at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
marginsplot, xtitle("Structural Familism Scale: percentiles") yline(0,lcolor(gs3))  ytitle("Average Marginal Effects: Marital Dissolution") title("") legend(position(6) ring(3) order(1 "Male BW" 2 "Female BW") rows(1)) xlabel(-3.12 "5th" -0.64 "25th" 1.27 "50th" 3.57 "75th" 12.48 "95th") plot1opts(lcolor("navy") mcolor("navy")) ci1opts(color("navy")) plot2opts(lcolor("ltblue") mcolor("ltblue")) ci2opts(color("ltblue")) // recastci(rarea) yscale(range(-.1 .1)) ylabel(-.1(.05).1, angle(0))

// NOT Parents
logit dissolve_lag i.dur c.structural_familism i.hh_earn_type c.structural_familism#i.hh_earn_type `controls' if children_under6==0 & hh_earn_type < 4 & state_fips!=11, or
sum structural_familism, detail
margins, dydx(hh_earn_type) at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
marginsplot, xtitle("Structural Familism Scale: percentiles") yline(0,lcolor(gs3))  ytitle("Average Marginal Effects: Marital Dissolution") title("") legend(position(6) ring(3) order(1 "Male BW" 2 "Female BW") rows(1)) xlabel(-3.12 "5th" -0.64 "25th" 1.27 "50th" 3.57 "75th" 12.48 "95th") plot1opts(lcolor("navy") mcolor("navy")) ci1opts(color("navy")) plot2opts(lcolor("ltblue") mcolor("ltblue")) ci2opts(color("ltblue"))// yscale(range(-.1 .1)) ylabel(-.1(.05).1, angle(0))s

// Parents + college
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.race_head i.same_race i.either_enrolled i.state_fips cohab_with_wife cohab_with_other pre_marital_birth i.interval i.home_owner knot1 knot2 knot3"

logit dissolve_lag i.dur c.structural_familism i.hh_earn_type c.structural_familism#i.hh_earn_type `controls' if children_under6==1 & couple_educ_gp==0 & hh_earn_type < 4 & state_fips!=11, or
sum structural_familism, detail
margins, dydx(hh_earn_type) at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
marginsplot, xtitle("Structural Familism Scale: percentiles") yline(0,lcolor(gs3))  ytitle("Average Marginal Effects: Marital Dissolution") title("") legend(position(6) ring(3) order(1 "Male BW" 2 "Female BW") rows(1)) xlabel(-3.12 "5th" -0.64 "25th" 1.27 "50th" 3.57 "75th" 12.48 "95th") plot1opts(lcolor("navy") mcolor("navy")) ci1opts(color("navy")) plot2opts(lcolor("ltblue") mcolor("ltblue")) ci2opts(color("ltblue")) // yscale(range(-.1 .1)) ylabel(-.1(.05).1, angle(0))

logit dissolve_lag i.dur c.structural_familism i.hh_earn_type c.structural_familism#i.hh_earn_type `controls' if children_under6==1 & couple_educ_gp==1 & hh_earn_type < 4 & state_fips!=11, or
sum structural_familism, detail
margins, dydx(hh_earn_type) at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
marginsplot, xtitle("Structural Familism Scale: percentiles") yline(0,lcolor(gs3))  ytitle("Average Marginal Effects: Marital Dissolution") title("") legend(position(6) ring(3) order(1 "Male BW" 2 "Female BW") rows(1)) xlabel(-3.12 "5th" -0.64 "25th" 1.27 "50th" 3.57 "75th" 12.48 "95th") plot1opts(lcolor("navy") mcolor("navy")) ci1opts(color("navy")) plot2opts(lcolor("ltblue") mcolor("ltblue")) ci2opts(color("ltblue")) // yscale(range(-.1 .1)) ylabel(-.1(.05).1, angle(0))

// NOT parents + college
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.race_head i.same_race i.either_enrolled i.state_fips cohab_with_wife cohab_with_other pre_marital_birth i.interval i.home_owner knot1 knot2 knot3"

logit dissolve_lag i.dur c.structural_familism i.hh_earn_type c.structural_familism#i.hh_earn_type `controls' if children_under6==0 & couple_educ_gp==0 & hh_earn_type < 4 & state_fips!=11, or
sum structural_familism, detail
margins, dydx(hh_earn_type) at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
marginsplot, xtitle("Structural Familism Scale: percentiles") yline(0,lcolor(gs3))  ytitle("Average Marginal Effects: Marital Dissolution") title("") legend(position(6) ring(3) order(1 "Male BW" 2 "Female BW") rows(1)) xlabel(-3.12 "5th" -0.64 "25th" 1.27 "50th" 3.57 "75th" 12.48 "95th") plot1opts(lcolor("navy") mcolor("navy")) ci1opts(color("navy")) plot2opts(lcolor("ltblue") mcolor("ltblue")) ci2opts(color("ltblue")) // yscale(range(-.1 .1)) ylabel(-.1(.05).1, angle(0))

logit dissolve_lag i.dur c.structural_familism i.hh_earn_type c.structural_familism#i.hh_earn_type `controls' if children_under6==0 & couple_educ_gp==1 & hh_earn_type < 4 & state_fips!=11, or
sum structural_familism, detail
margins, dydx(hh_earn_type) at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
marginsplot, xtitle("Structural Familism Scale: percentiles") yline(0,lcolor(gs3))  ytitle("Average Marginal Effects: Marital Dissolution") title("") legend(position(6) ring(3) order(1 "Male BW" 2 "Female BW") rows(1)) xlabel(-3.12 "5th" -0.64 "25th" 1.27 "50th" 3.57 "75th" 12.48 "95th") plot1opts(lcolor("navy") mcolor("navy")) ci1opts(color("navy")) plot2opts(lcolor("ltblue") mcolor("ltblue")) ci2opts(color("ltblue")) // yscale(range(-.1 .1)) ylabel(-.1(.05).1, angle(0))

********************************************************************************
**# DISSERTATION CHARTS
********************************************************************************

* Parents of kids under 6
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.race_head i.same_race i.either_enrolled i.state_fips cohab_with_wife cohab_with_other pre_marital_birth i.interval i.home_owner knot1 knot2 knot3 i.couple_educ_gp"  // i.num_children

logit dissolve_lag i.dur c.structural_familism i.hh_earn_type c.structural_familism#i.hh_earn_type `controls' if children_under6==1 & hh_earn_type < 4 & state_fips!=11, or
sum structural_familism, detail
margins hh_earn_type, at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
marginsplot, xtitle("Structural Support for Dual-Earning: Percentiles") ytitle("Predicted Probability of Marital Dissolution") title("") xlabel(-3.12 "5th" -0.64 "25th" 1.27 "50th" 3.57 "75th" 12.48 "95th") legend(position(6) ring(3) rows(1))  // plot1opts(lcolor("navy") mcolor("navy")) ci1opts(color("navy")) plot2opts(lcolor("ltblue") mcolor("ltblue")) ci2opts(color("ltblue")) yscale(range(-.1 .1)) ylabel(-.1(.05).1, angle(0))

* All parents
logit dissolve_lag i.dur c.structural_familism i.hh_earn_type c.structural_familism#i.hh_earn_type `controls' if children==1 & hh_earn_type < 4, or
sum structural_familism, detail
margins hh_earn_type, at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
marginsplot, xtitle("Structural Support for Dual-Earning: Percentiles") ytitle("Predicted Probability of Marital Dissolution") title("") xlabel(-3.12 "5th" -0.64 "25th" 1.27 "50th" 3.57 "75th" 12.48 "95th") legend(position(6) ring(3) rows(1))  // plot1opts(lcolor("navy") mcolor("navy")) ci1opts(color("navy")) plot2opts(lcolor("ltblue") mcolor("ltblue")) ci2opts(color("ltblue")) yscale(range(-.1 .1)) ylabel(-.1(.05).1, angle(0))

* Total sample
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.race_head i.same_race i.either_enrolled i.state_fips cohab_with_wife cohab_with_other pre_marital_birth i.interval i.home_owner knot1 knot2 knot3 i.couple_educ_gp i.num_children" 

logit dissolve_lag i.dur c.structural_familism i.hh_earn_type c.structural_familism#i.hh_earn_type `controls' if hh_earn_type < 4, or
sum structural_familism, detail
margins hh_earn_type, at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
marginsplot, xtitle("Structural Support for Dual-Earning: Percentiles") ytitle("Predicted Probability of Marital Dissolution") title("") xlabel(-3.12 "5th" -0.64 "25th" 1.27 "50th" 3.57 "75th" 12.48 "95th") legend(position(6) ring(3) rows(1))  // plot1opts(lcolor("navy") mcolor("navy")) ci1opts(color("navy")) plot2opts(lcolor("ltblue") mcolor("ltblue")) ci2opts(color("ltblue")) yscale(range(-.1 .1)) ylabel(-.1(.05).1, angle(0))


********************************************************************************
********************************************************************************
********************************************************************************
**# * Models to use - NOT restricted to just parents
* (Generally from SDA, but moving up / redoing to keep things separate and organized)
********************************************************************************
********************************************************************************
********************************************************************************

********************************************************************************
* Models that match primary divorce paper to validate
********************************************************************************

//* Main findings (to match Chapter 2) *//
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth  i.num_children i.interval i.home_owner knot1 knot2 knot3"

** Total sample
logit dissolve_lag i.dur i.hh_earn_type i.couple_educ_gp `controls', or
margins, dydx(hh_earn_type)

logit dissolve_lag i.dur i.housework_bkt i.couple_educ_gp `controls', or
margins, dydx(housework_bkt)

** College-educated
logit dissolve_lag i.dur i.hh_earn_type  `controls' if couple_educ_gp==1, or
margins, dydx(hh_earn_type)

logit dissolve_lag i.dur i.housework_bkt `controls' if couple_educ_gp==1, or
margins, dydx(housework_bkt)

** Non-college-educated
logit dissolve_lag i.dur i.hh_earn_type  `controls' if couple_educ_gp==0, or
margins, dydx(hh_earn_type)

logit dissolve_lag i.dur i.housework_bkt `controls' if couple_educ_gp==0, or
margins, dydx(housework_bkt)

//* Does structural familism generally predict dissolution? *//
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth  i.num_children i.interval i.home_owner knot1 knot2 knot3"

logit dissolve_lag i.dur c.structural_familism, or // it does PRIOR to controls, so controls prob picking some of that up
logit dissolve_lag i.dur c.structural_familism i.couple_educ_gp, or
margins, at(structural_familism=(-6(1)10))
margins, at(structural_familism=(-6(2)10))
marginsplot, xtitle("Structural Familism Scale") yline(0,lcolor(gs3)) ylabel(, angle(0))  ytitle("Predicted Probability of Marital Dissolution") title("")

logit dissolve_lag i.dur c.structural_familism i.couple_educ_gp `controls', or // it does PRIOR to controls, so controls prob picking some of that up
margins, at(structural_familism=(-6(1)10))
marginsplot, xtitle("Structural Familism Scale") yline(0,lcolor(gs3)) ylabel(, angle(0))  ytitle("Predicted Probability of Marital Dissolution") title("")

********************************************************************************
* Adding interactions of variables now
********************************************************************************
//* STRUCTURAL FAMILISM *//
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth  i.num_children i.interval i.home_owner knot1 knot2 knot3"

*Overall
logit dissolve_lag i.dur c.structural_familism i.hh_earn_type c.structural_familism#i.hh_earn_type `controls' i.couple_educ_gp if hh_earn_type < 4, or
margins, dydx(hh_earn_type) at(structural_familism=(-5(1)10))
marginsplot, xtitle("Structural Familism Scale") yline(0,lcolor(gs3)) yscale(range(-.1 .15)) ylabel(-.1(.05).15, angle(0))  ytitle("Average Marginal Effects: Marital Dissolution") title("") legend(position(6) ring(3) order(1 "Male BW" 2 "Female BW") rows(1))

logit dissolve_lag i.dur c.structural_familism i.housework_bkt c.structural_familism#i.housework_bkt `controls' i.couple_educ_gp if housework_bkt < 4, or
margins, dydx(housework_bkt) at(structural_familism=(-5(1)10))
marginsplot, xtitle("Structural Familism Scale") yline(0,lcolor(gs3)) yscale(range(-.1 .3)) ylabel(-.1(.1).3, angle(0)) ytitle("Average Marginal Effects: Marital Dissolution") title("") legend(position(6) ring(3) order(1 "Female Housework" 2 "Male Housework") rows(1))

*No College
logit dissolve_lag i.dur c.structural_familism i.hh_earn_type c.structural_familism#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4, or
margins, dydx(hh_earn_type) at(structural_familism=(-5(1)10))
marginsplot, xtitle("Structural Familism Scale") yline(0,lcolor(gs3)) yscale(range(-.1 .15)) ylabel(-.1(.05).15, angle(0))  ytitle("Average Marginal Effects: Marital Dissolution") title("") legend(position(6) ring(3) order(1 "Male BW" 2 "Female BW") rows(1))

logit dissolve_lag i.dur c.structural_familism i.housework_bkt c.structural_familism#i.housework_bkt `controls' if couple_educ_gp==0 & housework_bkt < 4, or
margins, dydx(housework_bkt) at(structural_familism=(-5(1)10))
marginsplot, xtitle("Structural Familism Scale") yline(0,lcolor(gs3)) yscale(range(-.1 .3)) ylabel(-.1(.1).3, angle(0)) ytitle("Average Marginal Effects: Marital Dissolution") title("") legend(position(6) ring(3) order(1 "Female Housework" 2 "Male Housework") rows(1))

*College
logit dissolve_lag i.dur c.structural_familism i.hh_earn_type c.structural_familism#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4, or
margins, dydx(hh_earn_type) at(structural_familism=(-5(1)10))
marginsplot, xtitle("Structural Familism Scale") yline(0,lcolor(gs3)) yscale(range(-.1 .15)) ylabel(-.1(.05).15, angle(0))  ytitle("Average Marginal Effects: Marital Dissolution") title("") legend(position(6) ring(3) order(1 "Male BW" 2 "Female BW") rows(1))

logit dissolve_lag i.dur c.structural_familism i.housework_bkt c.structural_familism#i.housework_bkt `controls' if couple_educ_gp==1 & housework_bkt < 4, or
margins, dydx(housework_bkt) at(structural_familism=(-5(1)10))
marginsplot, xtitle("Structural Familism Scale") yline(0,lcolor(gs3)) yscale(range(-.1 .3)) ylabel(-.1(.1).3, angle(0)) ytitle("Average Marginal Effects: Marital Dissolution") title("") legend(position(6) ring(3) order(1 "Female Housework" 2 "Male Housework") rows(1))


//* INDIVIDUAL COMPONENTS *//
// egen structural_familism= rowtotal(paid_leave_st prek_enrolled_public_st min_above_fed_st earn_ratio_neg_st unemployment_percap_st abortion_protected_st welfare_all_st)

local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth  i.num_children i.interval i.home_owner knot1 knot2 knot3 i.couple_educ_gp"

/* Total Sample*/
* Structural familism
logit dissolve_lag i.dur c.structural_familism i.hh_earn_type c.structural_familism#i.hh_earn_type `controls' if hh_earn_type < 4, or
outreg2 using "$results/dissolution_AMES_familism.xls", sideway stats(coef pval) label ctitle(1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace
margins, dydx(hh_earn_type) at(structural_familism=(-5(1)10))
sum structural_familism, detail
margins, dydx(hh_earn_type) at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(familism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Paid Leave
logit dissolve_lag i.dur i.paid_leave i.hh_earn_type i.paid_leave#i.hh_earn_type `controls' if hh_earn_type < 4, or
margins, dydx(hh_earn_type) at(paid_leave=(0 1)) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(paidleave) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* PreK Enrollment
logit dissolve_lag i.dur c.prek_enrolled_public i.hh_earn_type c.prek_enrolled_public#i.hh_earn_type `controls' if hh_earn_type < 4, or
margins, dydx(hh_earn_type) at(prek_enrolled_public=(.10(.10).50))
sum prek_enrolled_public, detail
margins, dydx(hh_earn_type) at(prek_enrolled_public=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(prek) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Min Wage
logit dissolve_lag i.dur i.min_above_fed i.hh_earn_type i.min_above_fed#i.hh_earn_type `controls' if hh_earn_type < 4, or
margins, dydx(hh_earn_type) at(min_above_fed=(0 1)) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(minwage) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Earnings Ratio
logit dissolve_lag i.dur c.earn_ratio i.hh_earn_type c.earn_ratio#i.hh_earn_type `controls' if hh_earn_type < 4, or
margins, dydx(hh_earn_type) at(earn_ratio=(1(.1)1.5))
sum earn_ratio, detail
margins, dydx(hh_earn_type) at(earn_ratio=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(earnings) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Unemployment Compensation
logit dissolve_lag i.dur c.unemployment_percap i.hh_earn_type c.unemployment_percap#i.hh_earn_type `controls' if hh_earn_type < 4, or
margins, dydx(hh_earn_type) at(unemployment_percap=(50(50)500))
sum unemployment_percap, detail
margins, dydx(hh_earn_type) at(unemployment_percap=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(unemployment) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Abortion protected
logit dissolve_lag i.dur i.abortion_protected i.hh_earn_type i.abortion_protected#i.hh_earn_type `controls' if hh_earn_type < 4, or
margins, dydx(hh_earn_type) at(abortion_protected=(0 1)) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(abortion) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Welfare Expenditures
logit dissolve_lag i.dur c.welfare_all i.hh_earn_type c.welfare_all#i.hh_earn_type `controls' if hh_earn_type < 4, or
margins, dydx(hh_earn_type) at(welfare_all=(500(500)2500))
sum welfare_all, detail
margins, dydx(hh_earn_type) at(welfare_all=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(welfare) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* General State Policy Liberalism (from CSPP - use to compare to familism results)
logit dissolve_lag i.dur c.pollib_median i.hh_earn_type c.pollib_median#i.hh_earn_type `controls' if hh_earn_type < 4, or
margins, dydx(hh_earn_type) at(pollib_median=(-2(1)3))
sum pollib_median, detail
margins, dydx(hh_earn_type) at(pollib_median=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(liberalism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth  i.num_children i.interval i.home_owner knot1 knot2 knot3"

/* No College */
* Structural familism - to test
logit dissolve_lag i.dur c.structural_familism i.hh_earn_type c.structural_familism#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4, or
outreg2 using "$results/dissolution_AMES_familism.xls", sideway stats(coef pval) label ctitle(No 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(hh_earn_type) at(structural_familism=(-5(1)10))
sum structural_familism, detail
margins, dydx(hh_earn_type) at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(no familism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Paid Leave
logit dissolve_lag i.dur i.paid_leave i.hh_earn_type i.paid_leave#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4, or
margins, dydx(hh_earn_type) at(paid_leave=(0 1)) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(no paidleave) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* PreK Enrollment
logit dissolve_lag i.dur c.prek_enrolled_public i.hh_earn_type c.prek_enrolled_public#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4, or
margins, dydx(hh_earn_type) at(prek_enrolled_public=(.10(.10).50))
sum prek_enrolled_public, detail
margins, dydx(hh_earn_type) at(prek_enrolled_public=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(no prek) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Min Wage
logit dissolve_lag i.dur i.min_above_fed i.hh_earn_type i.min_above_fed#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4, or
margins, dydx(hh_earn_type) at(min_above_fed=(0 1)) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(no minwage) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Earnings Ratio
logit dissolve_lag i.dur c.earn_ratio i.hh_earn_type c.earn_ratio#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4, or
margins, dydx(hh_earn_type) at(earn_ratio=(1(.1)1.5))
sum earn_ratio, detail
margins, dydx(hh_earn_type) at(earn_ratio=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(no earnings) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Unemployment Compensation
logit dissolve_lag i.dur c.unemployment_percap i.hh_earn_type c.unemployment_percap#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4, or
margins, dydx(hh_earn_type) at(unemployment_percap=(50(50)500))
sum unemployment_percap, detail
margins, dydx(hh_earn_type) at(unemployment_percap=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(no unemployment) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Abortion protected
logit dissolve_lag i.dur i.abortion_protected i.hh_earn_type i.abortion_protected#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4, or
margins, dydx(hh_earn_type) at(abortion_protected=(0 1)) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(no abortion) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Welfare Expenditures
logit dissolve_lag i.dur c.welfare_all i.hh_earn_type c.welfare_all#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4, or
margins, dydx(hh_earn_type) at(welfare_all=(500(500)2500))
sum welfare_all, detail
margins, dydx(hh_earn_type) at(welfare_all=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(no welfare) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* General State Policy Liberalism (from CSPP - use to compare to familism results)
logit dissolve_lag i.dur c.pollib_median i.hh_earn_type c.pollib_median#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4, or
margins, dydx(hh_earn_type) at(pollib_median=(-2(1)3))
sum pollib_median, detail
margins, dydx(hh_earn_type) at(pollib_median=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(no liberalism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)


/* College */
* Structural familism - to test
logit dissolve_lag i.dur c.structural_familism i.hh_earn_type c.structural_familism#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4, or
outreg2 using "$results/dissolution_AMES_familism.xls", sideway stats(coef pval) label ctitle(Coll 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(hh_earn_type) at(structural_familism=(-5(1)10))
sum structural_familism, detail
margins, dydx(hh_earn_type) at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(col familism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Paid Leave
logit dissolve_lag i.dur i.paid_leave i.hh_earn_type i.paid_leave#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4, or
margins, dydx(hh_earn_type) at(paid_leave=(0 1)) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(col paidleave) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* PreK Enrollment
logit dissolve_lag i.dur c.prek_enrolled_public i.hh_earn_type c.prek_enrolled_public#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4, or
margins, dydx(hh_earn_type) at(prek_enrolled_public=(.10(.10).50))
sum prek_enrolled_public, detail
margins, dydx(hh_earn_type) at(prek_enrolled_public=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(col prek) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Min Wage
logit dissolve_lag i.dur i.min_above_fed i.hh_earn_type i.min_above_fed#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4, or
margins, dydx(hh_earn_type) at(min_above_fed=(0 1)) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(col minwage) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Earnings Ratio
logit dissolve_lag i.dur c.earn_ratio i.hh_earn_type c.earn_ratio#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4, or
margins, dydx(hh_earn_type) at(earn_ratio=(1(.1)1.5))
sum earn_ratio, detail
margins, dydx(hh_earn_type) at(earn_ratio=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(col earnings) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Unemployment Compensation
logit dissolve_lag i.dur c.unemployment_percap i.hh_earn_type c.unemployment_percap#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4, or
margins, dydx(hh_earn_type) at(unemployment_percap=(50(50)500))
sum unemployment_percap, detail
margins, dydx(hh_earn_type) at(unemployment_percap=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(col unemployment) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Abortion protected
logit dissolve_lag i.dur i.abortion_protected i.hh_earn_type i.abortion_protected#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4, or
margins, dydx(2.hh_earn_type) at(abortion_protected=(0 1)) post // had to update bc #3 is collinnear
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(col abortion) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Welfare Expenditures
logit dissolve_lag i.dur c.welfare_all i.hh_earn_type c.welfare_all#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4, or
margins, dydx(hh_earn_type) at(welfare_all=(500(500)2500))
sum welfare_all, detail
margins, dydx(hh_earn_type) at(welfare_all=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(col welfare) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* General State Policy Liberalism (from CSPP - use to compare to familism results)
logit dissolve_lag i.dur c.pollib_median i.hh_earn_type c.pollib_median#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4, or
margins, dydx(hh_earn_type) at(pollib_median=(-2(1)3))
sum pollib_median, detail
margins, dydx(hh_earn_type) at(pollib_median=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(col liberalism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

/* College Breakdowns */
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth  i.num_children i.interval i.home_owner knot1 knot2 knot3"

* Structural familism
logit dissolve_lag i.dur c.structural_familism i.hh_earn_type c.structural_familism#i.hh_earn_type `controls' if couple_educ_detail==1 & hh_earn_type < 4, or // both college
sum structural_familism, detail
margins, dydx(hh_earn_type) at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(both familism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

logit dissolve_lag i.dur c.structural_familism i.hh_earn_type c.structural_familism#i.hh_earn_type `controls' if couple_educ_detail==2 & hh_earn_type < 4, or // her college college
sum structural_familism, detail
margins, dydx(hh_earn_type) at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(her familism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

logit dissolve_lag i.dur c.structural_familism i.hh_earn_type c.structural_familism#i.hh_earn_type `controls' if couple_educ_detail==3 & hh_earn_type < 4, or // him college college
sum structural_familism, detail
margins, dydx(hh_earn_type) at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(him familism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* General State Policy Liberalism (from CSPP - use to compare to familism results)
logit dissolve_lag i.dur c.pollib_median i.hh_earn_type c.pollib_median#i.hh_earn_type `controls' if couple_educ_detail==1 & hh_earn_type < 4, or // both college
sum pollib_median, detail
margins, dydx(hh_earn_type) at(pollib_median=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(both liberalism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

logit dissolve_lag i.dur c.pollib_median i.hh_earn_type c.pollib_median#i.hh_earn_type `controls' if couple_educ_detail==2 & hh_earn_type < 4, or // her college college
sum pollib_median, detail
margins, dydx(hh_earn_type) at(pollib_median=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(her liberalism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

logit dissolve_lag i.dur c.pollib_median i.hh_earn_type c.pollib_median#i.hh_earn_type `controls' if couple_educ_detail==3 & hh_earn_type < 4, or // him college college
sum pollib_median, detail
margins, dydx(hh_earn_type) at(pollib_median=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(him liberalism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)


********************************************************************************
********************************************************************************
********************************************************************************
**# For ASA Paper
********************************************************************************
********************************************************************************
********************************************************************************

********************************************************************************
* Just policy variables -> do they predict dissolution?
********************************************************************************
/*
attitudes: disapproval genderroles_egal working_mom_egal preschool_egal
	margins, at(disapproval=(2.1(.10)2.4))
	margins, at(genderroles_egal=(0.56(.04)0.72))
	margins, at(working_mom_egal=(0.66(.02)0.72))
	margins, at(preschool_egal=(0.58(.02)0.64))
regional_attitudes_factor
	margins, at(regional_attitudes_factor=(-2.0(1)2.0))
min_above_fed: binary yes / no
	margins, at(min_above_fed=(0 1))
unemployment: continuous
	margins, at(unemployment=(3(2)11))
cc_pct_income: continuous
	margins, at(cc_pct_income=(0.05(.10)0.35))
paid_leave: binary yes / no
	margins, at(paid_leave=(0 1))
senate_dems: higher = more dems; .88 correlation with house
	margins, at(senate_dems=(0(.10)0.8))
cc_subsidies: higher = more eligible kids served
	margins, at(cc_subsidies=(0.05(.10)0.45))
LCA: predclass
	margins, at(predclass=(1(1)4))
*/

log using "$logdir/policy_dissolution.log", replace

// By education
forvalues g=0/1{
	qui melogit dissolve_lag i.dur disapproval if couple_educ_gp==`g' || state_fips:, or
	margins, dydx(disapproval)
	margins, at(disapproval=(2.1(.10)2.4))

	qui melogit dissolve_lag i.dur regional_attitudes_factor if couple_educ_gp==`g' || state_fips:, or
	margins, dydx(regional_attitudes_factor)
	margins, at(regional_attitudes_factor=(-2.0(1)2.0))
	
	qui melogit dissolve_lag i.dur genderroles_egal if couple_educ_gp==`g' || state_fips:, or
	margins, dydx(genderroles_egal)
	margins, at(genderroles_egal=(0.56(.04)0.72))
	
	qui melogit dissolve_lag i.dur working_mom_egal if couple_educ_gp==`g' || state_fips:, or
	margins, dydx(working_mom_egal)
	margins, at(working_mom_egal=(0.66(.02)0.72))
	
	qui melogit dissolve_lag i.dur preschool_egal if couple_educ_gp==`g' || state_fips:, or
	margins, dydx(preschool_egal)
	margins, at(preschool_egal=(0.58(.02)0.64))
	
	qui melogit dissolve_lag i.dur i.min_above_fed if couple_educ_gp==`g' || state_fips:, or
	margins, dydx(min_above_fed)
	margins, at(min_above_fed=(0 1))
	
	qui melogit dissolve_lag i.dur unemployment if couple_educ_gp==`g' || state_fips:, or
	margins, dydx(unemployment)
	margins, at(unemployment=(3(2)11))	
	
	qui melogit dissolve_lag i.dur cc_pct_income if couple_educ_gp==`g' || state_fips:, or
	margins, dydx(cc_pct_income)
	margins, at(cc_pct_income=(0.05(.10)0.35))
	
	qui melogit dissolve_lag i.dur i.predclass if couple_educ_gp==`g' || state_fips:, or
	margins, dydx(predclass)
	margins, at(predclass=(1(1)4))
	
	qui melogit dissolve_lag i.dur senate_dems if couple_educ_gp==`g' || state_fips:, or
	margins, dydx(senate_dems)
	margins, at(senate_dems=(0(.10)0.8))	
	
	qui melogit dissolve_lag i.dur cc_subsidies if couple_educ_gp==`g' || state_fips:, or
	margins, dydx(cc_subsidies)
	margins, at(cc_subsidies=(0.05(.10)0.45))
	
	qui melogit dissolve_lag i.dur i.paid_leave if couple_educ_gp==`g' || state_fips:, or
	margins, dydx(paid_leave)
	margins, at(paid_leave=(0 1))
}

// Overall
 qui melogit dissolve_lag i.dur disapproval  || state_fips:, or
 margins, dydx(disapproval)
 margins, at(disapproval=(2.1(.10)2.4))
 
 qui melogit dissolve_lag i.dur regional_attitudes_factor || state_fips:, or
 margins, dydx(regional_attitudes_factor)
 margins, at(regional_attitudes_factor=(-2.0(1)2.0))
 
 qui melogit dissolve_lag i.dur genderroles_egal  || state_fips:, or
 margins, dydx(genderroles_egal)
 margins, at(genderroles_egal=(0.56(.04)0.72))
 
 qui melogit dissolve_lag i.dur working_mom_egal  || state_fips:, or
 margins, dydx(working_mom_egal)
 margins, at(working_mom_egal=(0.66(.02)0.72))
 
 qui melogit dissolve_lag i.dur preschool_egal  || state_fips:, or
 margins, dydx(preschool_egal)
 margins, at(preschool_egal=(0.58(.02)0.64))
 
 qui melogit dissolve_lag i.dur i.min_above_fed  || state_fips:, or
 margins, dydx(min_above_fed)
 margins, at(min_above_fed=(0 1))
 
 qui melogit dissolve_lag i.dur unemployment  || state_fips:, or
 margins, dydx(unemployment)
 margins, at(unemployment=(3(2)11)) 
 
 qui melogit dissolve_lag i.dur cc_pct_income  || state_fips:, or
 margins, dydx(cc_pct_income)
 margins, at(cc_pct_income=(0.05(.10)0.35))
 
 qui melogit dissolve_lag i.dur i.predclass  || state_fips:, or
 margins, dydx(predclass)
 margins, at(predclass=(1(1)4))
 
 qui melogit dissolve_lag i.dur senate_dems  || state_fips:, or
 margins, dydx(senate_dems)
 margins, at(senate_dems=(0(.10)0.8)) 
 
 qui melogit dissolve_lag i.dur cc_subsidies  || state_fips:, or
 margins, dydx(cc_subsidies)
 margins, at(cc_subsidies=(0.05(.10)0.45))
 
 qui melogit dissolve_lag i.dur i.paid_leave  || state_fips:, or
 margins, dydx(paid_leave)
 margins, at(paid_leave=(0 1))

log close

********************************************************************************
* Just looking for state variation atm
********************************************************************************
////////// No College \\\\\\\\\\\/

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
forvalues s=1/56{
	capture logistic dissolve_lag i.dur earnings_1000s `controls' if couple_educ_gp==0 & state_fips==`s'
	capture margins, dydx(earnings_1000s) post
	capture estimates store no_a`s'

	capture logistic dissolve_lag i.dur i.hh_earn_type earnings_1000s `controls' if couple_educ_gp==0 & state_fips==`s'
	capture margins, dydx(hh_earn_type) post
	capture estimates store no_b`s'
	
	capture logistic dissolve_lag i.dur i.ft_head i.ft_wife earnings_1000s `controls' if couple_educ_gp==0 & state_fips==`s'
	capture margins, dydx(ft_head ft_wife) post
	capture estimates store no_c`s'
	
} 

estimates dir
estimates table *, b p
estout no_* using "$results/state_no_college.xls", replace cells(b p)

////////// College \\\\\\\\\\\/

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
forvalues s=1/56{
	capture logistic dissolve_lag i.dur earnings_1000s `controls' if couple_educ_gp==1 & state_fips==`s'
	capture margins, dydx(earnings_1000s) post
	capture estimates store coll_a`s'

	capture logistic dissolve_lag i.dur i.hh_earn_type earnings_1000s `controls' if couple_educ_gp==1 & state_fips==`s'
	capture margins, dydx(hh_earn_type) post
	capture estimates store coll_b`s'
	
	capture logistic dissolve_lag i.dur i.ft_head i.ft_wife earnings_1000s `controls' if couple_educ_gp==1 & state_fips==`s'
	capture margins, dydx(ft_head ft_wife) post
	capture estimates store coll_c`s'
	
} 

estout coll_* using "$results/state_college.xls", replace cells(b p)


********************************************************************************
**# MODELS WITH INTERACTIONS
********************************************************************************
// log using "$logdir/policy_interactions_all.log", replace
// log using "$logdir/policy_interactions_all.log", append

********************************************************************************
* Interactions: Paid Work Arrangement - from ASA
********************************************************************************
log using "$logdir/policy_interactions_paid.log", replace
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth knot1 knot2 knot3"

/* No College */

**attitude summary
melogit dissolve_lag i.dur c.disapproval i.hh_earn_type c.disapproval#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(disapproval=(2.1(.10)2.4))

**regional attitudes: factor var
melogit dissolve_lag i.dur c.regional_attitudes_factor i.hh_earn_type c.regional_attitudes_factor#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(regional_attitudes_factor=(-2.0(1)2.0))

	**regional attitudes: gender roles
	melogit dissolve_lag i.dur c.genderroles_egal i.hh_earn_type c.genderroles_egal#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 || state_fips:, or
	margins, dydx(hh_earn_type) at(genderroles_egal=(0.56(.04)0.72))

	**regional attitudes: working mom
	melogit dissolve_lag i.dur c.working_mom_egal i.hh_earn_type c.working_mom_egal#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 || state_fips:, or
	margins, dydx(hh_earn_type) at(working_mom_egal=(0.66(.02)0.72))

	**regional attitudes: preschool
	melogit dissolve_lag i.dur c.preschool_egal i.hh_earn_type c.preschool_egal#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 || state_fips:, or
	margins, dydx(hh_earn_type) at(preschool_egal=(0.58(.02)0.64))

** Minimum wage
melogit dissolve_lag i.dur i.min_above_fed i.hh_earn_type i.min_above_fed#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(min_above_fed=(0 1))

	*Trying continuous
	melogit dissolve_lag i.dur statemin i.hh_earn_type c.statemin#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 || state_fips:, or
	margins, dydx(hh_earn_type) at(statemin=(4(2)10))

**% democrats in senate
melogit dissolve_lag i.dur c.senate_dems i.hh_earn_type c.senate_dems#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(senate_dems=(0(.10)0.8))

**Paid Leave
melogit dissolve_lag i.dur i.paid_leave i.hh_earn_type i.paid_leave#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(paid_leave=(0 1))

**Childcare costs
melogit dissolve_lag i.dur c.cc_pct_income i.hh_earn_type c.cc_pct_income#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(cc_pct_income=(0.05(.10)0.35))

**State Latent Class
melogit dissolve_lag i.dur i.predclass i.hh_earn_type i.predclass#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(predclass=(1(1)4))

**Unemployment
melogit dissolve_lag i.dur c.unemployment i.hh_earn_type c.unemployment#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(unemployment=(3(2)11))

**Childcare subsidies
melogit dissolve_lag i.dur c.cc_subsidies i.hh_earn_type c.cc_subsidies#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(cc_subsidies=(0.05(.10)0.45))

**Structural Sexism
melogit dissolve_lag i.dur c.structural_sexism i.hh_earn_type c.structural_sexism#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(structural_sexism=(-9(1)5))

* Alt attitudes
melogit dissolve_lag i.dur c.gender_mood i.hh_earn_type c.gender_mood#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(gender_mood=(50(5)75))

/* Tabling for now
**Unemployment compensation
melogit dissolve_lag i.dur c.unemployment_comp i.hh_earn_type c.unemployment_comp#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(unemployment_comp=(200(200)800))

**Prek-12 education spending
melogit dissolve_lag i.dur c.educ_spend i.hh_earn_type c.educ_spend#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(educ_spend=(4000(1000)9000))

**Right to Work
melogit dissolve_lag i.dur i.right2work i.hh_earn_type i.right2work#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(right2work=(0 1))
*/

/* College */

**attitude summary
melogit dissolve_lag i.dur c.disapproval i.hh_earn_type c.disapproval#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(disapproval=(2.1(.10)2.4))

**regional attitudes: factor var
melogit dissolve_lag i.dur c.regional_attitudes_factor i.hh_earn_type c.regional_attitudes_factor#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(regional_attitudes_factor=(-2.0(1)2.0))

	**regional attitudes: gender roles
	melogit dissolve_lag i.dur c.genderroles_egal i.hh_earn_type c.genderroles_egal#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 || state_fips:, or
	margins, dydx(hh_earn_type) at(genderroles_egal=(0.56(.04)0.72))

	**regional attitudes: working mom
	melogit dissolve_lag i.dur c.working_mom_egal i.hh_earn_type c.working_mom_egal#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 || state_fips:, or
	margins, dydx(hh_earn_type) at(working_mom_egal=(0.66(.02)0.72))

	**regional attitudes: preschool
	melogit dissolve_lag i.dur c.preschool_egal i.hh_earn_type c.preschool_egal#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 || state_fips:, or
	margins, dydx(hh_earn_type) at(preschool_egal=(0.58(.02)0.64))

** Minimum wage
melogit dissolve_lag i.dur i.min_above_fed i.hh_earn_type i.min_above_fed#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(min_above_fed=(0 1))

	*Trying continuous
	melogit dissolve_lag i.dur statemin i.hh_earn_type c.statemin#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 || state_fips:, or
	margins, dydx(hh_earn_type) at(statemin=(4(2)10))

**% democrats in senate
melogit dissolve_lag i.dur c.senate_dems i.hh_earn_type c.senate_dems#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(senate_dems=(0(.10)0.8))

**Paid Leave
melogit dissolve_lag i.dur i.paid_leave i.hh_earn_type i.paid_leave#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(paid_leave=(0 1))

**Childcare costs
melogit dissolve_lag i.dur c.cc_pct_income i.hh_earn_type c.cc_pct_income#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(cc_pct_income=(0.05(.10)0.35))

**State Latent Class
melogit dissolve_lag i.dur i.predclass i.hh_earn_type i.predclass#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(predclass=(1(1)4))

**Unemployment
melogit dissolve_lag i.dur c.unemployment i.hh_earn_type c.unemployment#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(unemployment=(3(2)11))

**Childcare subsidies
melogit dissolve_lag i.dur c.cc_subsidies i.hh_earn_type c.cc_subsidies#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(cc_subsidies=(0.05(.10)0.45))

* Structural sexism
melogit dissolve_lag i.dur c.structural_sexism i.hh_earn_type c.structural_sexism#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(structural_sexism=(-9(1)5))

* Alt attitudes
melogit dissolve_lag i.dur c.gender_mood i.hh_earn_type c.gender_mood#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(gender_mood=(50(5)75))

/* Tabling for now
**Unemployment compensation
melogit dissolve_lag i.dur c.unemployment_comp i.hh_earn_type c.unemployment_comp#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(unemployment_comp=(200(200)800))

**Prek-12 education spending
melogit dissolve_lag i.dur c.educ_spend i.hh_earn_type c.educ_spend#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(educ_spend=(4000(1000)9000))

**Right to Work
melogit dissolve_lag i.dur i.right2work i.hh_earn_type i.right2work#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(right2work=(0 1))

*/

log close

********************************************************************************
**# Interactions: for SDA
********************************************************************************
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth knot1 knot2 knot3"
logit dissolve_lag i.dur i.hh_earn_type `controls' i.couple_educ_gp if hh_earn_type < 4, or
logit dissolve_lag i.dur i.ft_head i.ft_wife `controls' i.couple_educ_gp if hh_earn_type < 4, or

// test 
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth knot1 knot2 knot3"
melogit dissolve_lag i.dur c.structural_sexism i.hh_earn_type c.structural_sexism#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 || state_fips:, or
logit dissolve_lag i.dur c.structural_sexism i.hh_earn_type c.structural_sexism#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4, or
logit dissolve_lag i.dur i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4, or

set scheme cleanplots

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth knot1 knot2 knot3"
logit dissolve_lag i.dur i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4, or
margins, dydx(hh_earn_type)
logit dissolve_lag i.dur i.ft_head i.ft_wife `controls' if couple_educ_gp==0 & hh_earn_type < 4, or
margins, dydx(ft_head ft_wife)

logit dissolve_lag i.dur i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4, or
margins, dydx(hh_earn_type)
logit dissolve_lag i.dur ib3.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4, or
margins, dydx(hh_earn_type)
logit dissolve_lag i.dur i.ft_head i.ft_wife `controls' if couple_educ_gp==1 & hh_earn_type < 4, or
margins, dydx(ft_head ft_wife)

// temp code - can it be as simple as this?!
log using "$logdir/policy_interactions_sexism.log", replace

//* Does structural familism generally predict dissolution? *//

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth knot1 knot2 knot3"
logit dissolve_lag i.dur c.structural_familism if state_fips!=11, or // it does PRIOR to controls, so controls prob picking some of that up
logit dissolve_lag i.dur c.structural_familism i.couple_educ_gp if state_fips!=11, or
margins, at(structural_familism=(-6(1)10))
margins, at(structural_familism=(-6(2)10))
marginsplot, xtitle("Structural Familism Scale") yline(0,lcolor(gs3)) ylabel(, angle(0))  ytitle("Predicted Probability of Marital Dissolution") title("")

logit dissolve_lag i.dur c.structural_familism `controls' i.state_fips if state_fips!=11, or

logit dissolve_lag i.dur c.structural_familism if state_fips!=11 & couple_educ_gp==0, or
logit dissolve_lag i.dur c.structural_familism `controls'  if state_fips!=11 & couple_educ_gp==0, or // so again, only prior to controls
logit dissolve_lag i.dur c.structural_familism if state_fips!=11 & couple_educ_gp==1, or // doesn't even here without controls
logit dissolve_lag i.dur c.structural_familism `controls'  if state_fips!=11 & couple_educ_gp==1, or

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth knot1 knot2 knot3"
logit dissolve_lag i.dur c.structural_sexism if state_fips!=11, or // true for sexism as well, interesting
logit dissolve_lag i.dur c.structural_sexism `controls'  if state_fips!=11, or

logit dissolve_lag i.dur c.structural_familism if state_fips!=11 & couple_educ_gp==0, or // no association
logit dissolve_lag i.dur c.structural_familism if state_fips!=11 & couple_educ_gp==1, or // not sig
logit dissolve_lag i.dur i.couple_educ_gp##c.structural_familism if state_fips!=11, or

//* Paid Work *//

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth knot1 knot2 knot3"
* Structural sexism
logit dissolve_lag i.dur c.structural_sexism i.hh_earn_type c.structural_sexism#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 & state_fips!=11, or
margins, dydx(hh_earn_type) at(structural_sexism=(-9(2)5))

// marginsplot, xtitle("Structural Sexism Scale") yline(0,lcolor(gs3)) ylabel(, angle(0))  ytitle("Average Marginal Effects: Marital Dissolution") plotregion(fcolor(white)) graphregion(fcolor(white)) title("")  legend(region(lcolor(white))) legend(size(small)) plot1opts(lcolor("blue")  msize("small") mcolor("blue"))  plot2opts(lcolor("pink") mcolor("pink") msize("small")) ciopts(color(*.4)) //  ci2opts(lcolor("pink")) ci1opts(lcolor("blue")) xlabel(, angle(0) labsize(small))

marginsplot, xtitle("Structural Sexism Scale") yline(0,lcolor(gs3)) ylabel(, angle(0))  ytitle("Average Marginal Effects: Marital Dissolution") title("") legend(position(6) ring(3) order(1 "Male BW" 2 "Female BW") rows(1))
// graph query, schemes

* Structural familism
logit dissolve_lag i.dur c.structural_familism i.hh_earn_type c.structural_familism#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 & state_fips!=11, or
margins, dydx(hh_earn_type) at(structural_familism=(-5(1)10))
marginsplot, xtitle("Structural Familism Scale") yline(0,lcolor(gs3)) yscale(range(-.1 .15)) ylabel(-.1(.05).15, angle(0))  ytitle("Average Marginal Effects: Marital Dissolution") title("") legend(position(6) ring(3) order(1 "Male BW" 2 "Female BW") rows(1))

logit dissolve_lag i.dur c.structural_familism_v0 i.hh_earn_type c.structural_familism_v0#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 & state_fips!=11, or
margins, dydx(hh_earn_type) at(structural_familism_v0=(-6(2)10))
marginsplot, xtitle("Structural Familism Scale") yline(0,lcolor(gs3)) yscale(range(-.1 .15)) ylabel(-.1(.05).15, angle(0))  ytitle("Average Marginal Effects: Marital Dissolution") title("") legend(position(6) ring(3) order(1 "Male BW" 2 "Female BW") rows(1))

* Control for attitudes
logit dissolve_lag i.dur c.structural_familism i.hh_earn_type c.structural_familism#i.hh_earn_type `controls' c.gender_mood if couple_educ_gp==0 & hh_earn_type < 4 & state_fips!=11, or
margins, dydx(hh_earn_type) at(structural_familism=(-5(1)10))
marginsplot, xtitle("Structural Familism Scale") yline(0,lcolor(gs3)) yscale(range(-.1 .15)) ylabel(-.1(.05).15, angle(0))  ytitle("Average Marginal Effects: Marital Dissolution") title("") legend(position(6) ring(3) order(1 "Male BW" 2 "Female BW") rows(1))

* Alt attitudes
logit dissolve_lag i.dur c.gender_mood i.hh_earn_type c.gender_mood#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 & state_fips!=11, or
margins, dydx(hh_earn_type) at(gender_mood=(50(5)75))
marginsplot, xtitle("Supportive Gender Role Attitudes") yline(0,lcolor(gs3)) yscale(range(-.1 .15)) ylabel(-.1(.05).15, angle(0))  ytitle("Average Marginal Effects: Marital Dissolution") title("") legend(position(6) ring(3) order(1 "Male BW" 2 "Female BW") rows(1))

* Combo
logit dissolve_lag i.dur i.state_cat i.hh_earn_type i.state_cat#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4  & state_fips!=11, or
margins, dydx(hh_earn_type) at(state_cat=(1(1)4))

* Structural sexism
logit dissolve_lag i.dur c.structural_sexism i.hh_earn_type c.structural_sexism#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 & state_fips!=11, or
margins, dydx(hh_earn_type) at(structural_sexism=(-9(2)5))
marginsplot, xtitle("Structural Sexism Scale") yline(0,lcolor(gs3)) ylabel(, angle(0))  ytitle("Average Marginal Effects: Marital Dissolution") title("") legend(position(6) ring(3) order(1 "Male BW" 2 "Female BW") rows(1))

* Structural familism
logit dissolve_lag i.dur c.structural_familism i.hh_earn_type c.structural_familism#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 & state_fips!=11, or
margins, dydx(hh_earn_type) at(structural_familism=(-5(1)10))
marginsplot, xtitle("Structural Familism Scale") yline(0,lcolor(gs3)) yscale(range(-.1 .15)) ylabel(-.1(.05).15, angle(0))  ytitle("Average Marginal Effects: Marital Dissolution") title("") legend(position(6) ring(3) order(1 "Male BW" 2 "Female BW") rows(1))

* Control for attitudes
logit dissolve_lag i.dur c.structural_familism i.hh_earn_type c.structural_familism#i.hh_earn_type `controls' c.gender_mood if couple_educ_gp==1 & hh_earn_type < 4 & state_fips!=11, or
margins, dydx(hh_earn_type) at(structural_familism=(-5(1)10))
marginsplot, xtitle("Structural Familism Scale") yline(0,lcolor(gs3)) yscale(range(-.1 .15)) ylabel(-.1(.05).15, angle(0))  ytitle("Average Marginal Effects: Marital Dissolution") title("") legend(position(6) ring(3) order(1 "Male BW" 2 "Female BW") rows(1))

* Alt attitudes
logit dissolve_lag i.dur c.gender_mood i.hh_earn_type c.gender_mood#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4  & state_fips!=11, or
margins, dydx(hh_earn_type) at(gender_mood=(50(5)75))
marginsplot, xtitle("Supportive Gender Role Attitudes") yline(0,lcolor(gs3)) yscale(range(-.1 .15)) ylabel(-.1(.05).15, angle(0)) ytitle("Average Marginal Effects: Marital Dissolution") title("") legend(position(6) ring(3) order(1 "Male BW" 2 "Female BW") rows(1))

* Combo
logit dissolve_lag i.dur i.state_cat i.hh_earn_type i.state_cat#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 & state_fips!=11, or
margins, dydx(hh_earn_type) at(state_cat=(1(1)4))

//* Unpaid Work *//

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth knot1 knot2 knot3"
* Structural sexism
logit dissolve_lag i.dur c.structural_sexism i.housework_bkt c.structural_sexism#i.housework_bkt `controls' if couple_educ_gp==0 & housework_bkt < 4 & state_fips!=11, or
margins, dydx(housework_bkt) at(structural_sexism=(-9(2)5))
marginsplot, xtitle("Structural Sexism Scale") yline(0,lcolor(gs3)) ylabel(, angle(0))  ytitle("Average Marginal Effects: Marital Dissolution") title("") legend(position(6) ring(3) order(1 "Female Housework" 2 "Male Housework") rows(1))

* Structural familism
logit dissolve_lag i.dur c.structural_familism i.housework_bkt c.structural_familism#i.housework_bkt `controls' if couple_educ_gp==0 & housework_bkt < 4 & state_fips!=11, or
margins, dydx(housework_bkt) at(structural_familism=(-6(2)10))
marginsplot, xtitle("Structural Familism Scale") yline(0,lcolor(gs3)) yscale(range(-.1 .3)) ylabel(-.1(.1).3, angle(0)) ytitle("Average Marginal Effects: Marital Dissolution") title("") legend(position(6) ring(3) order(1 "Female Housework" 2 "Male Housework") rows(1))

* Alt attitudes
logit dissolve_lag i.dur c.gender_mood i.housework_bkt c.gender_mood#i.housework_bkt `controls' if couple_educ_gp==0 & housework_bkt < 4 & state_fips!=11, or
margins, dydx(housework_bkt) at(gender_mood=(50(5)75))
marginsplot, xtitle("Supportive Gender Role Attitudes") yline(0,lcolor(gs3)) yscale(range(-.1 .3)) ylabel(-.1(.1).3, angle(0))  ytitle("Average Marginal Effects: Marital Dissolution") title("") legend(position(6) ring(3) order(1 "Female Housework" 2 "Male Housework") rows(1))

* Combo
logit dissolve_lag i.dur i.state_cat i.housework_bkt i.state_cat#i.housework_bkt `controls' if couple_educ_gp==0 & housework_bkt < 4  & state_fips!=11, or
margins, dydx(housework_bkt) at(state_cat=(1(1)4))

* Structural sexism
logit dissolve_lag i.dur c.structural_sexism i.housework_bkt c.structural_sexism#i.housework_bkt `controls' if couple_educ_gp==1 & housework_bkt < 4 & state_fips!=11, or
margins, dydx(housework_bkt) at(structural_sexism=(-9(2)5))
marginsplot, xtitle("Structural Sexism Scale") yline(0,lcolor(gs3)) ylabel(, angle(0))  ytitle("Average Marginal Effects: Marital Dissolution") title("") legend(position(6) ring(3) order(1 "Female Housework" 2 "Male Housework") rows(1))

* Structural familism
logit dissolve_lag i.dur c.structural_familism i.housework_bkt c.structural_familism#i.housework_bkt `controls' if couple_educ_gp==1 & housework_bkt < 4 & state_fips!=11, or
margins, dydx(housework_bkt) at(structural_familism=(-6(2)10))
marginsplot, xtitle("Structural Familism Scale") yline(0,lcolor(gs3)) yscale(range(-.1 .3)) ylabel(-.1(.1).3, angle(0))  ytitle("Average Marginal Effects: Marital Dissolution") title("") legend(position(6) ring(3) order(1 "Female Housework" 2 "Male Housework") rows(1))

* Alt attitudes
logit dissolve_lag i.dur c.gender_mood i.housework_bkt c.gender_mood#i.housework_bkt `controls' if couple_educ_gp==1 & housework_bkt < 4  & state_fips!=11, or
margins, dydx(housework_bkt) at(gender_mood=(50(5)75))
marginsplot, xtitle("Supportive Gender Role Attitudes") yline(0,lcolor(gs3)) yscale(range(-.1 .3)) ylabel(-.1(.1).3, angle(0))  ytitle("Average Marginal Effects: Marital Dissolution") title("") legend(position(6) ring(3) order(1 "Female Housework" 2 "Male Housework") rows(1))

* Combo
logit dissolve_lag i.dur i.state_cat i.housework_bkt i.state_cat#i.housework_bkt `controls' if couple_educ_gp==1 & housework_bkt < 4 & state_fips!=11, or
margins, dydx(housework_bkt) at(state_cat=(1(1)4))

// get correlation of sexism and attitudes
pwcorr gender_mood structural_familism
pwcorr structural_sexism structural_familism
pwcorr structural_sexism gender_mood // negatively correlated which makes sense, MORe structrual sexism = LESS support for women

log close

// for figure
tabstat structural_familism structural_sexism gender_mood, by(state)

// test continuous indicators or is that too much?
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth knot1 knot2 knot3"
logit dissolve_lag i.dur c.structural_familism c.female_earn_pct c.structural_familism#c.female_earn_pct `controls' if couple_educ_gp==0 & hh_earn_type < 4 & state_fips!=11, or
margins, at(structural_familism=(-6(2)10) female_earn_pct=(0(.25)1))
marginsplot

logit dissolve_lag i.dur c.structural_familism c.female_earn_pct c.structural_familism#c.female_earn_pct `controls' if couple_educ_gp==1 & hh_earn_type < 4 & state_fips!=11, or
margins, at(structural_familism=(-6(2)10) female_earn_pct=(0(.25)1))
marginsplot

// *IS EMPLOYMENT LESS ANNOYING KIMMM *//
log using "$logdir/policy_interactions_sexism.log", append

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth knot1 knot2 knot3"
* Structural familism
logit dissolve_lag i.dur c.structural_familism i.ft_head i.ft_wife c.structural_familism#i.ft_head c.structural_familism#i.ft_wife  `controls' if couple_educ_gp==0 & state_fips!=11, or
margins, dydx(ft_head ft_wife) at(structural_familism=(-6(2)10))
marginsplot, xtitle("Structural Familism Scale") yline(0,lcolor(gs3)) yscale(range(-.1 .15)) ylabel(-.1(.05).15, angle(0))  ytitle("Average Marginal Effects: Marital Dissolution") title("") legend(position(6) ring(3) order(1 "Husband Employment" 2 "Wife Employment") rows(1))

logit dissolve_lag i.dur c.structural_familism i.ft_head i.ft_wife c.structural_familism#i.ft_head c.structural_familism#i.ft_wife  `controls' if couple_educ_gp==1 & state_fips!=11, or
margins, dydx(ft_head ft_wife) at(structural_familism=(-6(2)10))
marginsplot, xtitle("Structural Familism Scale") yline(0,lcolor(gs3)) yscale(range(-.1 .15)) ylabel(-.1(.05).15, angle(0))  ytitle("Average Marginal Effects: Marital Dissolution") title("") legend(position(6) ring(3) order(1 "Husband Employment" 2 "Wife Employment") rows(1))

* Alt attitudes
logit dissolve_lag i.dur c.gender_mood i.ft_head i.ft_wife c.gender_mood#i.ft_head c.gender_mood#i.ft_wife `controls' if couple_educ_gp==0 & state_fips!=11, or
margins, dydx(ft_head ft_wife)  at(gender_mood=(50(5)75))
marginsplot, xtitle("Supportive Gender Role Attitudes") yline(0,lcolor(gs3)) yscale(range(-.1 .15)) ylabel(-.1(.05).15, angle(0))  ytitle("Average Marginal Effects: Marital Dissolution") title("") legend(position(6) ring(3) order(1 "Husband Employment" 2 "Wife Employment") rows(1))

logit dissolve_lag i.dur c.gender_mood i.ft_head i.ft_wife c.gender_mood#i.ft_head c.gender_mood#i.ft_wife `controls' if couple_educ_gp==1 & state_fips!=11, or
margins, dydx(ft_head ft_wife)  at(gender_mood=(50(5)75))
marginsplot, xtitle("Supportive Gender Role Attitudes") yline(0,lcolor(gs3)) yscale(range(-.1 .15)) ylabel(-.1(.05).15, angle(0))  ytitle("Average Marginal Effects: Marital Dissolution") title("") legend(position(6) ring(3) order(1 "Husband Employment" 2 "Wife Employment") rows(1))

log close


********************************************************************************
********************************************************************************
********************************************************************************
**# Try here (10/5/23)
********************************************************************************
********************************************************************************
********************************************************************************

tabstat structural_familism, by(state)
tabstat economic_challenges, by(state)
tabstat dissolve_lag, by(state)
tab couple_educ_gp hh_earn_type if hh_earn_type<4, row chi2

logit dissolve_lag i.dur c.structural_familism if state_fips!=11, or
logit dissolve_lag i.dur c.structural_familism_alt if state_fips!=11, or

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled cohab_with_wife cohab_with_other pre_marital_birth knot1 knot2 knot3 c.gender_mood"
logit dissolve_lag i.dur c.structural_familism_alt `controls' if state_fips!=11, or
logit dissolve_lag i.dur c.structural_familism_alt `controls' if state_fips!=11 & couple_educ_gp==0, or
logit dissolve_lag i.dur c.structural_familism_alt `controls' if state_fips!=11 & couple_educ_gp==1, or

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled cohab_with_wife cohab_with_other pre_marital_birth knot1 knot2 knot3 c.gender_mood"
logit dissolve_lag i.dur i.hh_earn_type `controls' if state_fips!=11 & couple_educ_gp==1 & hh_earn_type<=4, or
logit dissolve_lag i.dur ib2.hh_earn_type `controls' structural_familism if state_fips!=11 & couple_educ_gp==1 & hh_earn_type<4, or
logit dissolve_lag i.dur i.hh_earn_type##i.familism_scale `controls' if state_fips!=11 & couple_educ_gp==1 & hh_earn_type<4, or
margins, dydx(hh_earn_type) at(familism_scale=(0 1))

logit dissolve_lag i.dur i.hh_earn_type##i.familism_scale `controls' if state_fips!=11 & couple_educ_gp==0 & hh_earn_type<4, or
margins, dydx(hh_earn_type) at(familism_scale=(0 1))

logit dissolve_lag i.dur i.hh_earn_type##i.familism_scale_det `controls' if state_fips!=11 & couple_educ_gp==1 & hh_earn_type<4, or
margins, dydx(hh_earn_type) at(familism_scale_det=(1 2 3))

// main figures: familism
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled cohab_with_wife cohab_with_other pre_marital_birth knot1 knot2 knot3 c.gender_mood"
logit dissolve_lag i.dur c.structural_familism i.hh_earn_type c.structural_familism#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 & state_fips!=11, or
margins, dydx(hh_earn_type) at(structural_familism=(-5(1)10))
marginsplot, xtitle("Structural Familism Scale") yline(0,lcolor(gs3)) yscale(range(-.1 .1)) ylabel(-.1(.05).1, angle(0))  ytitle("Average Marginal Effects: Marital Dissolution") title("") legend(position(6) ring(3) order(1 "Male BW" 2 "Female BW") rows(1)) plot1opts(lcolor("191 87 0") mcolor("191 87 0")) ci1opts(color("191 87 0")) plot2opts(lcolor("0 95 134") mcolor("0 95 134")) ci2opts(color("0 95 134")) // plot3opts(lcolor("248 151 31") mcolor("248 151 31")) ci3opts(color("248 151 31")) 

logit dissolve_lag i.dur c.structural_familism i.hh_earn_type c.structural_familism#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 & state_fips!=11, or
margins, dydx(hh_earn_type) at(structural_familism=(-5(1)10))
marginsplot, xtitle("Structural Familism Scale") yline(0,lcolor(gs3)) yscale(range(-.1 .1)) ylabel(-.1(.05).1, angle(0))  ytitle("Average Marginal Effects: Marital Dissolution") title("") legend(position(6) ring(3) order(1 "Male BW" 2 "Female BW") rows(1)) plot1opts(lcolor("191 87 0") mcolor("191 87 0")) ci1opts(color("191 87 0")) plot2opts(lcolor("0 95 134") mcolor("0 95 134")) ci2opts(color("0 95 134")) // plot3opts(lcolor("248 151 31") mcolor("248 151 31")) ci3opts(color("248 151 31")) 

// main figures: economics
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled cohab_with_wife cohab_with_other pre_marital_birth knot1 knot2 knot3 c.gender_mood"
logit dissolve_lag i.dur c.economic_challenges i.hh_earn_type c.economic_challenges#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 & state_fips!=11, or
margins, dydx(hh_earn_type) at(economic_challenges=(-3(1)5))
marginsplot, xtitle("Economic Inequality") yline(0,lcolor(gs3)) yscale(range(-.1 .1)) ylabel(-.1(.05).1, angle(0))  ytitle("Average Marginal Effects: Marital Dissolution") title("") legend(position(6) ring(3) order(1 "Male BW" 2 "Female BW") rows(1)) plot1opts(lcolor("191 87 0") mcolor("191 87 0")) ci1opts(color("191 87 0")) plot2opts(lcolor("0 95 134") mcolor("0 95 134")) ci2opts(color("0 95 134")) // plot3opts(lcolor("248 151 31") mcolor("248 151 31")) ci3opts(color("248 151 31")) 

logit dissolve_lag i.dur c.economic_challenges i.hh_earn_type c.economic_challenges#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 & state_fips!=11, or
margins, dydx(hh_earn_type) at(economic_challenges=(-3(1)5))
marginsplot, xtitle("Economic Inequality") yline(0,lcolor(gs3)) yscale(range(-.1 .1)) ylabel(-.1(.05).1, angle(0))  ytitle("Average Marginal Effects: Marital Dissolution") title("") legend(position(6) ring(3) order(1 "Male BW" 2 "Female BW") rows(1)) plot1opts(lcolor("191 87 0") mcolor("191 87 0")) ci1opts(color("191 87 0")) plot2opts(lcolor("0 95 134") mcolor("0 95 134")) ci2opts(color("0 95 134")) // plot3opts(lcolor("248 151 31") mcolor("248 151 31")) ci3opts(color("248 151 31")) 


********************************************************************************
**# Outreg: primary interactions
********************************************************************************

// *Paid Work: All variables in scale individually *//
	// egen structural_familism= rowtotal(min_above_fed_st paid_leave_st senate_dems_st welfare_all_st educ_spend_percap_st earn_ratio_neg_st)
	
// old controls: local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth knot1 knot2 knot3 c.gender_mood"

local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth i.num_children i.interval knot1 knot2 knot3 c.gender_mood"

/* No College */
* Structural familism - to test
logit dissolve_lag i.dur c.structural_familism i.hh_earn_type c.structural_familism#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 & state_fips!=11, or
outreg2 using "$results/dissolution_AMES_familism.xls", sideway stats(coef pval) label ctitle(No 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace
margins, dydx(hh_earn_type) at(structural_familism=(-5(1)10)) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(no familism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Min Wage
logit dissolve_lag i.dur i.min_above_fed i.hh_earn_type i.min_above_fed#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 & state_fips!=11, or
margins, dydx(hh_earn_type) at(min_above_fed=(0 1)) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(no minwage) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Paid Leave
logit dissolve_lag i.dur i.paid_leave i.hh_earn_type i.paid_leave#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 & state_fips!=11, or
margins, dydx(hh_earn_type) at(paid_leave=(0 1)) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(no paidleave) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Senate Dems
logit dissolve_lag i.dur c.senate_dems i.hh_earn_type c.senate_dems#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 & state_fips!=11, or
margins, dydx(hh_earn_type) at(senate_dems=(.20(.10).80)) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(no dems) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Welfare Expenditures
logit dissolve_lag i.dur c.welfare_all i.hh_earn_type c.welfare_all#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 & state_fips!=11, or
margins, dydx(hh_earn_type) at(welfare_all=(500(500)2500)) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(no welfare) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Education Expenditures
logit dissolve_lag i.dur c.educ_spend_percap i.hh_earn_type c.educ_spend_percap#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 & state_fips!=11, or
margins, dydx(hh_earn_type) at(educ_spend_percap=(1000(200)2000)) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(no educ) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Earnings Ratio
logit dissolve_lag i.dur c.parent_earn_ratio i.hh_earn_type c.parent_earn_ratio#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 & state_fips!=11, or
margins, dydx(hh_earn_type) at(parent_earn_ratio=(1(.2)2)) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(no earnings) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Structural sexism
logit dissolve_lag i.dur c.structural_sexism i.hh_earn_type c.structural_sexism#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 & state_fips!=11, or
margins, dydx(hh_earn_type) at(structural_sexism=(-8(2)4)) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(no sexism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Attitudes
logit dissolve_lag i.dur c.gender_mood i.hh_earn_type c.gender_mood#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 & state_fips!=11, or
margins, dydx(hh_earn_type) at(gender_mood=(50(5)75)) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(no attitudes) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Economic challenges
logit dissolve_lag i.dur c.economic_challenges i.hh_earn_type c.economic_challenges#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 & state_fips!=11, or
outreg2 using "$results/dissolution_AMES_familism.xls", sideway stats(coef pval) label ctitle(No 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(hh_earn_type) at(economic_challenges=(-3(1)5)) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(no economic) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Unemployment
logit dissolve_lag i.dur c.unemployment i.hh_earn_type c.unemployment#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 & state_fips!=11, or
margins, dydx(hh_earn_type) at(unemployment=(2(2)10)) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(no unemploy) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Child poverty
logit dissolve_lag i.dur c.child_pov i.hh_earn_type c.child_pov#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 & state_fips!=11, or
margins, dydx(hh_earn_type) at(child_pov=(.10(.05).30)) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(no pov) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Gini
logit dissolve_lag i.dur c.gini i.hh_earn_type c.gini#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 & state_fips!=11, or
margins, dydx(hh_earn_type) at(gini=(.55(.05).70)) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(no gini) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

/* Both in model
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth knot1 knot2 knot3 c.gender_mood"
logit dissolve_lag i.dur c.economic_challenges structural_familism i.hh_earn_type c.economic_challenges#i.hh_earn_type c.structural_familism#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 & state_fips!=11, or
sum structural_familism, detail
margins, dydx(hh_earn_type) at(structural_familism=(`r(p25)' `r(p75)'))
// margins, dydx(hh_earn_type) at(structural_familism=(`r(p10)' `r(p90)'))
margins hh_earn_type, at(structural_familism=(`r(p25)' `r(p75)'))
sum economic_challenges, detail
margins, dydx(hh_earn_type) at(economic_challenges=(`r(p25)' `r(p75)'))
margins hh_earn_type, at(economic_challenges=(`r(p25)' `r(p75)'))
margins, dydx(hh_earn_type) at(structural_familism=(-5(1)10))
margins, dydx(hh_earn_type) at(economic_challenges=(-3(1)5))
*/

/* College */
* Structural familism - to test
logit dissolve_lag i.dur c.structural_familism i.hh_earn_type c.structural_familism#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 & state_fips!=11, or
outreg2 using "$results/dissolution_AMES_familism.xls", sideway stats(coef pval) label ctitle(Coll 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(hh_earn_type) at(structural_familism=(-5(1)10)) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(col familism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Min Wage
logit dissolve_lag i.dur i.min_above_fed i.hh_earn_type i.min_above_fed#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 & state_fips!=11, or
margins, dydx(hh_earn_type) at(min_above_fed=(0 1)) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(col minwage) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Paid Leave
logit dissolve_lag i.dur i.paid_leave i.hh_earn_type i.paid_leave#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 & state_fips!=11, or
margins, dydx(hh_earn_type) at(paid_leave=(0 1)) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(col paidleave) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Senate Dems
logit dissolve_lag i.dur c.senate_dems i.hh_earn_type c.senate_dems#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 & state_fips!=11, or
margins, dydx(hh_earn_type) at(senate_dems=(.20(.10).80)) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(col dems) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Welfare Expenditures
logit dissolve_lag i.dur c.welfare_all i.hh_earn_type c.welfare_all#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 & state_fips!=11, or
margins, dydx(hh_earn_type) at(welfare_all=(500(500)2500)) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(col welfare) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Education Expenditures
logit dissolve_lag i.dur c.educ_spend_percap i.hh_earn_type c.educ_spend_percap#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 & state_fips!=11, or
margins, dydx(hh_earn_type) at(educ_spend_percap=(1000(200)2000)) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(col educ) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Earnings Ratio
logit dissolve_lag i.dur c.parent_earn_ratio i.hh_earn_type c.parent_earn_ratio#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 & state_fips!=11, or
margins, dydx(hh_earn_type) at(parent_earn_ratio=(1(.2)2)) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(col earnings) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Structural sexism
logit dissolve_lag i.dur c.structural_sexism i.hh_earn_type c.structural_sexism#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 & state_fips!=11, or
margins, dydx(hh_earn_type) at(structural_sexism=(-8(2)4)) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(col sexism) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Attitudes
logit dissolve_lag i.dur c.gender_mood i.hh_earn_type c.gender_mood#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 & state_fips!=11, or
margins, dydx(hh_earn_type) at(gender_mood=(50(5)75)) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(col attitudes) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Economic challenges
logit dissolve_lag i.dur c.economic_challenges i.hh_earn_type c.economic_challenges#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 & state_fips!=11, or
outreg2 using "$results/dissolution_AMES_familism.xls", sideway stats(coef pval) label ctitle(Coll 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
margins, dydx(hh_earn_type) at(economic_challenges=(-3(1)5)) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(coll economic) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Unemployment
logit dissolve_lag i.dur c.unemployment i.hh_earn_type c.unemployment#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 & state_fips!=11, or
margins, dydx(hh_earn_type) at(unemployment=(2(2)10)) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(coll unemploy) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Child poverty
logit dissolve_lag i.dur c.child_pov i.hh_earn_type c.child_pov#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 & state_fips!=11, or
margins, dydx(hh_earn_type) at(child_pov=(.10(.05).30)) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(coll pov) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

* Gini
logit dissolve_lag i.dur c.gini i.hh_earn_type c.gini#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 & state_fips!=11, or
margins, dydx(hh_earn_type) at(gini=(.55(.05).70)) post
outreg2 using "$results/dissolution_AMES_familism.xls", ctitle(coll gini) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

/* Both in model
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth knot1 knot2 knot3 c.gender_mood"
logit dissolve_lag i.dur c.economic_challenges structural_familism i.hh_earn_type c.economic_challenges#i.hh_earn_type c.structural_familism#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 & state_fips!=11, or
sum structural_familism, detail
margins hh_earn_type, at(structural_familism=(`r(p25)' `r(p75)'))
sum economic_challenges, detail
margins hh_earn_type, at(economic_challenges=(`r(p25)' `r(p75)'))
margins, dydx(hh_earn_type) at(structural_familism=(-5(1)10))
margins, dydx(hh_earn_type) at(economic_challenges=(-3(1)5))
*/

/* Margins: using percentiles for "high" and "low"*/
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth i.num_children i.interval knot1 knot2 knot3 c.gender_mood"

logit dissolve_lag i.dur c.structural_familism i.hh_earn_type c.structural_familism#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 & state_fips!=11, or
margins, dydx(hh_earn_type) at(structural_familism=(-5(1)10))
sum structural_familism, detail
margins hh_earn_type, at(structural_familism=(`r(p25)' `r(p75)'))
sum structural_familism, detail
margins, dydx(hh_earn_type) at(structural_familism=(`r(p25)' `r(p75)'))

logit dissolve_lag i.dur c.economic_challenges i.hh_earn_type c.economic_challenges#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 & state_fips!=11, or
sum economic_challenges, detail
margins hh_earn_type, at(economic_challenges=(`r(p25)' `r(p75)'))
sum economic_challenges, detail
margins, dydx(hh_earn_type) at(economic_challenges=(`r(p25)' `r(p75)'))

logit dissolve_lag i.dur c.structural_familism i.hh_earn_type c.structural_familism#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 & state_fips!=11, or
sum structural_familism, detail
margins hh_earn_type, at(structural_familism=(`r(p25)' `r(p75)'))
sum structural_familism, detail
margins, dydx(hh_earn_type) at(structural_familism=(`r(p25)' `r(p75)'))

logit dissolve_lag i.dur c.economic_challenges i.hh_earn_type c.economic_challenges#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 & state_fips!=11, or
sum economic_challenges, detail
margins hh_earn_type, at(economic_challenges=(`r(p25)' `r(p75)'))
sum economic_challenges, detail
margins, dydx(hh_earn_type) at(economic_challenges=(`r(p25)' `r(p75)'))

/* Just the measures*/
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth i.num_children i.interval knot1 knot2 knot3 c.gender_mood"

logit dissolve_lag i.dur c.structural_familism `controls' if couple_educ_gp==0 & hh_earn_type < 4 & state_fips!=11, or
logit dissolve_lag i.dur c.economic_challenges `controls' if couple_educ_gp==0 & hh_earn_type < 4 & state_fips!=11, or
logit dissolve_lag i.dur c.structural_familism `controls' if couple_educ_gp==1 & hh_earn_type < 4 & state_fips!=11, or
logit dissolve_lag i.dur c.economic_challenges `controls' if couple_educ_gp==1 & hh_earn_type < 4 & state_fips!=11, or

logit dissolve_lag i.dur c.structural_familism `controls' if couple_educ_gp==0 & state_fips!=11, or
logit dissolve_lag i.dur c.economic_challenges `controls' if couple_educ_gp==0 & state_fips!=11, or
logit dissolve_lag i.dur c.structural_familism `controls' if couple_educ_gp==1 & state_fips!=11, or
logit dissolve_lag i.dur c.economic_challenges `controls' if couple_educ_gp==1 & state_fips!=11, or

********************************************************************************
**# Does structural familism OR attitudes predict DoL?
********************************************************************************
mlogit hh_earn_type i.dur i.couple_educ_gp i.children if hh_earn_type < 4 & state_fips!=11, rrr // so yes, college-educated more likely to be dual-earning and female-BW than male BW
margins couple_educ_gp

mlogit hh_earn_type i.dur i.couple_educ_gp if hh_earn_type < 4 & state_fips!=11, rrr baseoutcome(1)

local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth i.num_children i.interval knot1 knot2 knot3 c.gender_mood"

// Familism
mlogit hh_earn_type i.dur structural_familism i.children if hh_earn_type < 4 & state_fips!=11, rrr // when higher, more likely to be dual / female BW than male BW
margins, at(structural_familism=(-5(5)10)) // post
outreg2 using "$results/policy_DOL.xls", ctitle(total) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

mlogit hh_earn_type i.dur structural_familism i.children if hh_earn_type < 4 & state_fips!=11 & couple_educ_gp==0, rrr // no diffs
margins, at(structural_familism=(-5(5)10)) // post
outreg2 using "$results/policy_DOL.xls", ctitle(no) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

mlogit hh_earn_type i.dur structural_familism i.children if hh_earn_type < 4 & state_fips!=11 & couple_educ_gp==1, rrr // really the trends here
margins, at(structural_familism=(-5(5)10)) // post
outreg2 using "$results/policy_DOL.xls", ctitle(coll) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

mlogit hh_earn_type i.dur structural_familism i.children if hh_earn_type < 4 & state_fips!=11 & couple_educ_gp==0, rrr // no association
margins, at(structural_familism=(-5(5)10))
marginsplot, xtitle("Structural Familism Scale") ylabel(, angle(0))  ytitle("Probability of Given Division of Labor") title("") legend(position(6) ring(3) order(1 "Dual Earner" 2 "Male BW" 3 "Female BW") rows(1)) // plot2opts(lcolor("191 87 0") mcolor("191 87 0")) ci2opts(color("191 87 0")) plot3opts(lcolor("0 95 134") mcolor("0 95 134")) ci3opts(color("0 95 134")) plot1opts(lcolor(gray) mcolor(gray)) ci1opts(color(gray)) 
// plot1opts(lcolor("248 151 31") mcolor("248 151 31")) ci1opts(color("248 151 31")) 

mlogit hh_earn_type i.dur structural_familism i.children if hh_earn_type < 4 & state_fips!=11 & couple_educ_gp==1, rrr // really the trends here
margins, at(structural_familism=(-5(5)10))
marginsplot, xtitle("Structural Familism Scale") ylabel(, angle(0))  ytitle("Probability of Given Division of Labor") title("") legend(position(6) ring(3) order(1 "Dual Earner" 2 "Male BW" 3 "Female BW") rows(1)) // plot2opts(lcolor("191 87 0") mcolor("191 87 0")) ci2opts(color("191 87 0")) plot3opts(lcolor("0 95 134") mcolor("0 95 134")) ci3opts(color("0 95 134")) plot1opts(lcolor(gray) mcolor(gray)) ci1opts(color(gray)) 

// Sexism
mlogit hh_earn_type i.dur structural_sexism i.children if hh_earn_type < 4 & state_fips!=11, rrr // makes sense - when higher, more likely to be male BW and less likely to be others
margins, at(structural_sexism=(-10(5)5)) post
outreg2 using "$results/policy_DOL.xls", ctitle(total) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

mlogit hh_earn_type i.dur structural_sexism i.children if hh_earn_type < 4 & state_fips!=11 & couple_educ_gp==0, rrr // directional but not sig
margins, at(structural_sexism=(-10(5)5)) post
outreg2 using "$results/policy_DOL.xls", ctitle(no) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

mlogit hh_earn_type i.dur structural_sexism i.children if hh_earn_type < 4 & state_fips!=11 & couple_educ_gp==1, rrr // most sig for female / male BW, not dual earning (only marginal)
margins, at(structural_sexism=(-10(5)5)) post
outreg2 using "$results/policy_DOL.xls", ctitle(coll) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)


// Attitudes
mlogit hh_earn_type i.dur gender_mood i.children if hh_earn_type < 4 & state_fips!=11, rrr // same results for familism. higher = more dual and female BW
margins, at(gender_mood=(55(10)75)) post
outreg2 using "$results/policy_DOL.xls", ctitle(total) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

mlogit hh_earn_type i.dur gender_mood i.children if hh_earn_type < 4 & state_fips!=11 & couple_educ_gp==0, rrr // okay atttitudes actually sig here
margins, at(gender_mood=(55(10)75)) post
outreg2 using "$results/policy_DOL.xls", ctitle(no) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

mlogit hh_earn_type i.dur gender_mood i.children if hh_earn_type < 4 & state_fips!=11 & couple_educ_gp==1, rrr
margins, at(gender_mood=(55(10)75)) post
outreg2 using "$results/policy_DOL.xls", ctitle(coll) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

mlogit hh_earn_type i.dur gender_mood i.children if hh_earn_type < 4 & state_fips!=11 & couple_educ_gp==0, rrr
margins, at(gender_mood=(55(10)75))
marginsplot, xtitle("Gender Equality Mood") xlabel(55(10)75, format(%15.0gc)) ylabel(, angle(0))  ytitle("Probability of Given Division of Labor") title("") legend(position(6) ring(3) order(1 "Dual Earner" 2 "Male BW" 3 "Female BW") rows(1)) plot1opts(lcolor("191 87 0") mcolor("191 87 0")) ci1opts(color("191 87 0")) plot2opts(lcolor("0 95 134") mcolor("0 95 134")) ci2opts(color("0 95 134")) plot3opts(lcolor("248 151 31") mcolor("248 151 31")) ci3opts(color("248 151 31")) 

mlogit hh_earn_type i.dur gender_mood i.children if hh_earn_type < 4 & state_fips!=11 & couple_educ_gp==1, rrr
margins, at(gender_mood=(55(10)75))
marginsplot, xtitle("Gender Equality Mood") xlabel(55(10)75, format(%15.0gc)) ylabel(, angle(0))  ytitle("Probability of Given Division of Labor") title("") legend(position(6) ring(3) order(1 "Dual Earner" 2 "Male BW" 3 "Female BW") rows(1)) plot1opts(lcolor("191 87 0") mcolor("191 87 0")) ci1opts(color("191 87 0")) plot2opts(lcolor("0 95 134") mcolor("0 95 134")) ci2opts(color("0 95 134")) plot3opts(lcolor("248 151 31") mcolor("248 151 31")) ci3opts(color("248 151 31")) 

//same models
mlogit hh_earn_type i.dur structural_familism gender_mood i.children if hh_earn_type < 4 & state_fips!=11 & couple_educ_gp==0, rrr // only gender mood predictive

mlogit hh_earn_type i.dur structural_familism gender_mood i.children if hh_earn_type < 4 & state_fips!=11 & couple_educ_gp==1, rrr // dual-earning = gender mood; female BW = structural
margins, at(structural_familism=(-5(5)10))
marginsplot, xtitle("Structural Familism Scale") ylabel(, angle(0))  ytitle("Probability of Given Division of Labor") title("") legend(position(6) ring(3) order(1 "Dual Earner" 2 "Male BW" 3 "Female BW") rows(1)) plot1opts(lcolor("191 87 0") mcolor("191 87 0")) ci1opts(color("191 87 0")) plot2opts(lcolor("0 95 134") mcolor("0 95 134")) ci2opts(color("0 95 134")) plot3opts(lcolor("248 151 31") mcolor("248 151 31")) ci3opts(color("248 151 31")) 

margins, at(gender_mood=(55(10)75))
marginsplot, xtitle("Gender Equality Mood") xlabel(55(10)75, format(%15.0gc)) ylabel(, angle(0))  ytitle("Probability of Given Division of Labor") title("") legend(position(6) ring(3) order(1 "Dual Earner" 2 "Male BW" 3 "Female BW") rows(1)) plot1opts(lcolor("191 87 0") mcolor("191 87 0")) ci1opts(color("191 87 0")) plot2opts(lcolor("0 95 134") mcolor("0 95 134")) ci2opts(color("0 95 134")) plot3opts(lcolor("248 151 31") mcolor("248 151 31")) ci3opts(color("248 151 31")) 

// Economic challenges
mlogit hh_earn_type i.dur economic_challenges i.children if hh_earn_type < 4 & state_fips!=11, rrr // when higher, more likely female BW than male BW, but dual = no change
margins, at(economic_challenges=(-3(1)5))
outreg2 using "$results/policy_DOL.xls", ctitle(total) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

mlogit hh_earn_type i.dur economic_challenges i.children if hh_earn_type < 4 & state_fips!=11 & couple_educ_gp==0, rrr // more likely to be female BW, but it comes from DUAL not male?! this is the eemrgency BW story?!
margins, at(economic_challenges=(-3(1)5))
outreg2 using "$results/policy_DOL.xls", ctitle(no) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

mlogit hh_earn_type i.dur economic_challenges i.children if hh_earn_type < 4 & state_fips!=11 & couple_educ_gp==1, rrr // no diffs
margins, at(economic_challenges=(-3(1)5))
outreg2 using "$results/policy_DOL.xls", ctitle(coll) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

mlogit hh_earn_type i.dur economic_challenges i.children if hh_earn_type < 4 & state_fips!=11 & couple_educ_gp==0, rrr // counterintuive - female BW more likely when more challenges
margins, at(economic_challenges=(-3(2)5))
marginsplot, xtitle("Economic Uncertainty") ylabel(, angle(0))  ytitle("Probability of Given Division of Labor") title("") legend(position(6) ring(3) order(1 "Dual Earner" 2 "Male BW" 3 "Female BW") rows(1)) // plot2opts(lcolor("191 87 0") mcolor("191 87 0")) ci2opts(color("191 87 0")) plot3opts(lcolor("0 95 134") mcolor("0 95 134")) ci3opts(color("0 95 134")) plot1opts(lcolor(gray) mcolor(gray)) ci1opts(color(gray)) 
// plot1opts(lcolor("248 151 31") mcolor("248 151 31")) ci1opts(color("248 151 31")) 

mlogit hh_earn_type i.dur economic_challenges i.children if hh_earn_type < 4 & state_fips!=11 & couple_educ_gp==1, rrr // no real association here
margins, at(economic_challenges=(-3(2)5))
marginsplot, xtitle("Economic Uncertainty") ylabel(, angle(0))  ytitle("Probability of Given Division of Labor") title("") legend(position(6) ring(3) order(1 "Dual Earner" 2 "Male BW" 3 "Female BW") rows(1)) //  plot2opts(lcolor("191 87 0") mcolor("191 87 0")) ci2opts(color("191 87 0")) plot3opts(lcolor("0 95 134") mcolor("0 95 134")) ci3opts(color("0 95 134")) plot1opts(lcolor(gray) mcolor(gray)) ci1opts(color(gray)) 

// Structural familism and economic in same model
mlogit hh_earn_type i.dur economic_challenges structural_familism i.children if hh_earn_type < 4 & state_fips!=11 & couple_educ_gp==0, rrr // here, only economic challenges sig, and the effect is w female BW
margins, at(economic_challenges=(-3(2)5))
marginsplot, xtitle("Economic Uncertainty") ylabel(, angle(0))  ytitle("Probability of Given Division of Labor") title("") legend(position(6) ring(3) order(1 "Dual Earner" 2 "Male BW" 3 "Female BW") rows(1)) plot2opts(lcolor("191 87 0") mcolor("191 87 0")) ci2opts(color("191 87 0")) plot3opts(lcolor("0 95 134") mcolor("0 95 134")) ci3opts(color("0 95 134")) plot1opts(lcolor(gray) mcolor(gray)) ci1opts(color(gray)) 
// plot1opts(lcolor("248 151 31") mcolor("248 151 31")) ci1opts(color("248 151 31")) 

margins, at(structural_familism=(-5(5)10))
marginsplot, xtitle("Structural Familism Scale") ylabel(, angle(0))  ytitle("Probability of Given Division of Labor") title("") legend(position(6) ring(3) order(1 "Dual Earner" 2 "Male BW" 3 "Female BW") rows(1)) plot2opts(lcolor("191 87 0") mcolor("191 87 0")) ci2opts(color("191 87 0")) plot3opts(lcolor("0 95 134") mcolor("0 95 134")) ci3opts(color("0 95 134")) plot1opts(lcolor(gray) mcolor(gray)) ci1opts(color(gray)) 


mlogit hh_earn_type i.dur economic_challenges structural_familism i.children if hh_earn_type < 4 & state_fips!=11 & couple_educ_gp==1, rrr // here, structural familism higher = more female AND dual. economic challenges = less dual (but not sig for female BW)
margins, at(economic_challenges=(-3(2)5))
marginsplot, xtitle("Economic Uncertainty") ylabel(, angle(0))  ytitle("Probability of Given Division of Labor") title("") legend(position(6) ring(3) order(1 "Dual Earner" 2 "Male BW" 3 "Female BW") rows(1)) plot2opts(lcolor("191 87 0") mcolor("191 87 0")) ci2opts(color("191 87 0")) plot3opts(lcolor("0 95 134") mcolor("0 95 134")) ci3opts(color("0 95 134")) plot1opts(lcolor(gray) mcolor(gray)) ci1opts(color(gray)) 

margins, at(structural_familism=(-5(5)10))
marginsplot, xtitle("Structural Familism Scale") ylabel(, angle(0))  ytitle("Probability of Given Division of Labor") title("") legend(position(6) ring(3) order(1 "Dual Earner" 2 "Male BW" 3 "Female BW") rows(1)) plot2opts(lcolor("191 87 0") mcolor("191 87 0")) ci2opts(color("191 87 0")) plot3opts(lcolor("0 95 134") mcolor("0 95 134")) ci3opts(color("0 95 134")) plot1opts(lcolor(gray) mcolor(gray)) ci1opts(color(gray)) 

// alt
mlogit hh_earn_type i.dur regional_attitudes_factor if hh_earn_type < 4 & state_fips!=11, rrr // only sig for dual
margins, at(regional_attitudes_factor=(-2.0(2)2.0))
marginsplot

mlogit hh_earn_type i.dur regional_attitudes_factor if hh_earn_type < 4 & state_fips!=11 & couple_educ_gp==0, rrr // same
margins, at(regional_attitudes_factor=(-2.0(2)2.0))
marginsplot

mlogit hh_earn_type i.dur regional_attitudes_factor if hh_earn_type < 4 & state_fips!=11 & couple_educ_gp==1, rrr // nothing sig
margins, at(regional_attitudes_factor=(-2.0(2)2.0))
marginsplot

********************************************************************************
**# Do policies predict divorce
********************************************************************************
// By education
forvalues g=0/1{
	qui logit dissolve_lag i.dur structural_familism if couple_educ_gp==`g', or
	outreg2 using "$results/divorce_totals.xls", ctitle(fam `g') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)
	margins, at(structural_familism=(-5(1)10)) post
	outreg2 using "$results/divorce_totals_margins.xls", ctitle(fam `g') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

	qui logit dissolve_lag i.dur i.min_above_fed if couple_educ_gp==`g', or
	outreg2 using "$results/divorce_totals.xls", ctitle(wage `g') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
	margins, at(min_above_fed=(0 1)) post
	outreg2 using "$results/divorce_totals_margins.xls", ctitle(wage `g') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

	qui logit dissolve_lag i.dur i.paid_leave if couple_educ_gp==`g', or
	outreg2 using "$results/divorce_totals.xls", ctitle(leave `g') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
	margins, at(paid_leave=(0 1)) post
	outreg2 using "$results/divorce_totals_margins.xls", ctitle(leave `g') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)
	
	qui logit dissolve_lag i.dur senate_dems if couple_educ_gp==`g', or
	outreg2 using "$results/divorce_totals.xls", ctitle(dems `g') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
	margins, at(senate_dems=(.20(.10).80)) post
	outreg2 using "$results/divorce_totals_margins.xls", ctitle(dems `g') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)
	
	qui logit dissolve_lag i.dur welfare_all if couple_educ_gp==`g', or
	outreg2 using "$results/divorce_totals.xls", ctitle(welfare `g') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
	margins, at(welfare_all=(500(500)2500)) post
	outreg2 using "$results/divorce_totals_margins.xls", ctitle(welfare `g') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)

	qui logit dissolve_lag i.dur educ_spend_percap if couple_educ_gp==`g', or
	outreg2 using "$results/divorce_totals.xls", ctitle(educ `g') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
	margins, at(educ_spend_percap=(1000(200)2000)) post
	outreg2 using "$results/divorce_totals_margins.xls", ctitle(educ `g') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)
	
	qui logit dissolve_lag i.dur parent_earn_ratio if couple_educ_gp==`g', or
	outreg2 using "$results/divorce_totals.xls", ctitle(ratio `g') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
	margins, at(parent_earn_ratio=(1(.2)2)) post
	outreg2 using "$results/divorce_totals_margins.xls", ctitle(ratio `g') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)
	
	qui logit dissolve_lag i.dur structural_sexism if couple_educ_gp==`g', or
	outreg2 using "$results/divorce_totals.xls", ctitle(sexism `g') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
	margins, at(structural_sexism=(-8(2)4)) post
	outreg2 using "$results/divorce_totals_margins.xls", ctitle(sexism `g') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)
	
	qui logit dissolve_lag i.dur gender_mood if couple_educ_gp==`g', or
	outreg2 using "$results/divorce_totals.xls", ctitle(mood `g') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
	margins, at(gender_mood=(50(5)75)) post
	outreg2 using "$results/divorce_totals_margins.xls", ctitle(mood `g') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)
	//
	qui logit dissolve_lag i.dur economic_challenges if couple_educ_gp==`g', or
	outreg2 using "$results/divorce_totals.xls", ctitle(economic `g') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
	margins, at(economic_challenges=(-3(1)5)) post
	outreg2 using "$results/divorce_totals_margins.xls", ctitle(economic `g') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)
	
	qui logit dissolve_lag i.dur unemployment if couple_educ_gp==`g', or
	outreg2 using "$results/divorce_totals.xls", ctitle(unemploy `g') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
	margins, at(unemployment=(2(2)10)) post
	outreg2 using "$results/divorce_totals_margins.xls", ctitle(unemploy `g') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)
	
	qui logit dissolve_lag i.dur child_pov if couple_educ_gp==`g', or
	outreg2 using "$results/divorce_totals.xls", ctitle(pov `g') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
	margins, at(child_pov=(.10(.05).30)) post
	outreg2 using "$results/divorce_totals_margins.xls", ctitle(pov `g') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)
	
	qui logit dissolve_lag i.dur gini if couple_educ_gp==`g', or
	outreg2 using "$results/divorce_totals.xls", ctitle(gini `g') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
	margins, at(gini=(.55(.05).70)) post
	outreg2 using "$results/divorce_totals_margins.xls", ctitle(gini `g') dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +)
}

**# Old things below here

********************************************************************************
* Overall models (for H1 and H2)
********************************************************************************
log using "$logdir/policy_overall_paid.log", replace
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth knot1 knot2 knot3"

**attitude summary
melogit dissolve_lag i.dur c.disapproval i.hh_earn_type c.disapproval#i.hh_earn_type `controls' if hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(disapproval=(2.1(.10)2.4))

**regional attitudes: factor var
melogit dissolve_lag i.dur c.regional_attitudes_factor i.hh_earn_type c.regional_attitudes_factor#i.hh_earn_type `controls' if hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(regional_attitudes_factor=(-2.0(1)2.0))

** Minimum wage
melogit dissolve_lag i.dur i.min_above_fed i.hh_earn_type i.min_above_fed#i.hh_earn_type `controls' if hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(min_above_fed=(0 1))

**% democrats in senate
melogit dissolve_lag i.dur c.senate_dems i.hh_earn_type c.senate_dems#i.hh_earn_type `controls' if hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(senate_dems=(0(.10)0.8))

**Paid Leave
melogit dissolve_lag i.dur i.paid_leave i.hh_earn_type i.paid_leave#i.hh_earn_type `controls' if hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(paid_leave=(0 1))

**Childcare costs
melogit dissolve_lag i.dur c.cc_pct_income i.hh_earn_type c.cc_pct_income#i.hh_earn_type `controls' if hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(cc_pct_income=(0.05(.10)0.35))

**State Latent Class
melogit dissolve_lag i.dur i.predclass i.hh_earn_type i.predclass#i.hh_earn_type `controls' if hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(predclass=(1(1)4))

**Unemployment
melogit dissolve_lag i.dur c.unemployment i.hh_earn_type c.unemployment#i.hh_earn_type `controls' if hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(unemployment=(3(2)11))

**Childcare subsidies
melogit dissolve_lag i.dur c.cc_subsidies i.hh_earn_type c.cc_subsidies#i.hh_earn_type `controls' if hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(cc_subsidies=(0.05(.10)0.45))

log close

********************************************************************************
* Interactions: Paid Work Arrangement controlling for attitudes
********************************************************************************
log using "$logdir/policy_interactions_paid.log", append
local controls "c.regional_attitudes_factor age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth knot1 knot2 knot3"

/* No College */
** Minimum wage
melogit dissolve_lag i.dur i.min_above_fed i.hh_earn_type i.min_above_fed#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(min_above_fed=(0 1))

**% democrats in senate
melogit dissolve_lag i.dur c.senate_dems i.hh_earn_type c.senate_dems#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(senate_dems=(0(.10)0.8))

**Paid Leave
melogit dissolve_lag i.dur i.paid_leave i.hh_earn_type i.paid_leave#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(paid_leave=(0 1))

**Childcare costs
melogit dissolve_lag i.dur c.cc_pct_income i.hh_earn_type c.cc_pct_income#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(cc_pct_income=(0.05(.10)0.35))

**State Latent Class
melogit dissolve_lag i.dur i.predclass i.hh_earn_type i.predclass#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(predclass=(1(1)4))

**Unemployment
melogit dissolve_lag i.dur c.unemployment i.hh_earn_type c.unemployment#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(unemployment=(3(2)11))

**Childcare subsidies
melogit dissolve_lag i.dur c.cc_subsidies i.hh_earn_type c.cc_subsidies#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(cc_subsidies=(0.05(.10)0.45))

/* College */
** Minimum wage
melogit dissolve_lag i.dur i.min_above_fed i.hh_earn_type i.min_above_fed#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(min_above_fed=(0 1))

**% democrats in senate
melogit dissolve_lag i.dur c.senate_dems i.hh_earn_type c.senate_dems#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(senate_dems=(0(.10)0.8))

**Paid Leave
melogit dissolve_lag i.dur i.paid_leave i.hh_earn_type i.paid_leave#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(paid_leave=(0 1))

**Childcare costs
melogit dissolve_lag i.dur c.cc_pct_income i.hh_earn_type c.cc_pct_income#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(cc_pct_income=(0.05(.10)0.35))

**State Latent Class
melogit dissolve_lag i.dur i.predclass i.hh_earn_type i.predclass#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(predclass=(1(1)4))

**Unemployment
melogit dissolve_lag i.dur c.unemployment i.hh_earn_type c.unemployment#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(unemployment=(3(2)11))

**Childcare subsidies
melogit dissolve_lag i.dur c.cc_subsidies i.hh_earn_type c.cc_subsidies#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(cc_subsidies=(0.05(.10)0.45))

**test multiple interactions - the above just adds attitudes but NOT the interaction of attitudes and hh earn type. Do i need the INTERACTION? testing with some variables here
local controls "c.regional_attitudes_factor age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth knot1 knot2 knot3"

melogit dissolve_lag i.dur c.senate_dems i.hh_earn_type c.senate_dems#i.hh_earn_type c.regional_attitudes_factor#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(senate_dems=(0(.10)0.8))
margins, dydx(hh_earn_type) at(regional_attitudes_factor=(-2.0(1)2.0))

melogit dissolve_lag i.dur i.paid_leave i.hh_earn_type i.paid_leave#i.hh_earn_type c.regional_attitudes_factor#i.hh_earn_type  `controls' if couple_educ_gp==0 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(paid_leave=(0 1))
margins, dydx(hh_earn_type) at(regional_attitudes_factor=(-2.0(1)2.0))

melogit dissolve_lag i.dur c.senate_dems i.hh_earn_type c.senate_dems#i.hh_earn_type c.regional_attitudes_factor#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(senate_dems=(0(.10)0.8))
margins, dydx(hh_earn_type) at(regional_attitudes_factor=(-2.0(1)2.0))

melogit dissolve_lag i.dur i.paid_leave i.hh_earn_type i.paid_leave#i.hh_earn_type c.regional_attitudes_factor#i.hh_earn_type  `controls' if couple_educ_gp==1 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(paid_leave=(0 1))
margins, dydx(hh_earn_type) at(regional_attitudes_factor=(-2.0(1)2.0))


log close

// Overall // 
log using "$logdir/policy_overall_paid.log", append
local controls "c.regional_attitudes_factor age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth knot1 knot2 knot3"

** Minimum wage
melogit dissolve_lag i.dur i.min_above_fed i.hh_earn_type i.min_above_fed#i.hh_earn_type `controls' if hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(min_above_fed=(0 1))

**% democrats in senate
melogit dissolve_lag i.dur c.senate_dems i.hh_earn_type c.senate_dems#i.hh_earn_type `controls' if hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(senate_dems=(0(.10)0.8))

**Paid Leave
melogit dissolve_lag i.dur i.paid_leave i.hh_earn_type i.paid_leave#i.hh_earn_type `controls' if hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(paid_leave=(0 1))

**Childcare costs
melogit dissolve_lag i.dur c.cc_pct_income i.hh_earn_type c.cc_pct_income#i.hh_earn_type `controls' if hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(cc_pct_income=(0.05(.10)0.35))

**State Latent Class
melogit dissolve_lag i.dur i.predclass i.hh_earn_type i.predclass#i.hh_earn_type `controls' if hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(predclass=(1(1)4))

**Unemployment
melogit dissolve_lag i.dur c.unemployment i.hh_earn_type c.unemployment#i.hh_earn_type `controls' if hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(unemployment=(3(2)11))

**Childcare subsidies
melogit dissolve_lag i.dur c.cc_subsidies i.hh_earn_type c.cc_subsidies#i.hh_earn_type `controls' if hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(cc_subsidies=(0.05(.10)0.45))

log close

********************************************************************************
* Interactions: Unpaid Work Arrangement
********************************************************************************
log using "$logdir/policy_interactions_unpaid.log", replace
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth knot1 knot2 knot3"

/* No College */

**attitude summary
melogit dissolve_lag i.dur c.disapproval i.housework_bkt c.disapproval#i.housework_bkt `controls' if couple_educ_gp==0 & housework_bkt < 4 || state_fips:, or
margins, dydx(housework_bkt) at(disapproval=(2.1(.10)2.4))

	**regional attitudes: gender roles
	melogit dissolve_lag i.dur c.genderroles_egal i.housework_bkt c.genderroles_egal#i.housework_bkt `controls' if couple_educ_gp==0 & housework_bkt < 4 || state_fips:, or
	margins, dydx(housework_bkt) at(genderroles_egal=(0.56(.04)0.72))

	**regional attitudes: working mom
	melogit dissolve_lag i.dur c.working_mom_egal i.housework_bkt c.working_mom_egal#i.housework_bkt `controls' if couple_educ_gp==0 & housework_bkt < 4 || state_fips:, or
	margins, dydx(housework_bkt) at(working_mom_egal=(0.66(.02)0.72))

	**regional attitudes: preschool
	melogit dissolve_lag i.dur c.preschool_egal i.housework_bkt c.preschool_egal#i.housework_bkt `controls' if couple_educ_gp==0 & housework_bkt < 4 || state_fips:, or
	margins, dydx(housework_bkt) at(preschool_egal=(0.58(.02)0.64))

** Minimum wage
melogit dissolve_lag i.dur i.min_above_fed i.housework_bkt i.min_above_fed#i.housework_bkt `controls' if couple_educ_gp==0 & housework_bkt < 4 || state_fips:, or
margins, dydx(housework_bkt) at(min_above_fed=(0 1))

**Unemployment compensation
melogit dissolve_lag i.dur c.unemployment_comp i.housework_bkt c.unemployment_comp#i.housework_bkt `controls' if couple_educ_gp==0 & housework_bkt < 4 || state_fips:, or
margins, dydx(housework_bkt) at(unemployment_comp=(200(200)800))

**% democrats in senate
melogit dissolve_lag i.dur c.senate_dems i.housework_bkt c.senate_dems#i.housework_bkt `controls' if couple_educ_gp==0 & housework_bkt < 4 || state_fips:, or
margins, dydx(housework_bkt) at(senate_dems=(0(.10)0.8))

**Paid Leave
melogit dissolve_lag i.dur i.paid_leave i.housework_bkt i.paid_leave#i.housework_bkt `controls' if couple_educ_gp==0 & housework_bkt < 4 || state_fips:, or
margins, dydx(housework_bkt) at(paid_leave=(0 1))

**Right to Work
melogit dissolve_lag i.dur i.right2work i.housework_bkt i.right2work#i.housework_bkt `controls' if couple_educ_gp==0 & housework_bkt < 4 || state_fips:, or
margins, dydx(housework_bkt) at(right2work=(0 1))

**Childcare costs
melogit dissolve_lag i.dur c.cc_pct_income i.housework_bkt c.cc_pct_income#i.housework_bkt `controls' if couple_educ_gp==0 & housework_bkt < 4 || state_fips:, or
margins, dydx(housework_bkt) at(cc_pct_income=(0.05(.10)0.35))

**Prek-12 education spending
melogit dissolve_lag i.dur c.educ_spend i.housework_bkt c.educ_spend#i.housework_bkt `controls' if couple_educ_gp==0 & housework_bkt < 4 || state_fips:, or
margins, dydx(housework_bkt) at(educ_spend=(4000(1000)9000))

**State Latent Class
melogit dissolve_lag i.dur i.predclass i.housework_bkt i.predclass#i.housework_bkt `controls' if couple_educ_gp==0 & housework_bkt < 4 || state_fips:, or
margins, dydx(housework_bkt) at(predclass=(1(1)4))


/* College */

**attitude summary
melogit dissolve_lag i.dur c.disapproval i.housework_bkt c.disapproval#i.housework_bkt `controls' if couple_educ_gp==1 & housework_bkt < 4 || state_fips:, or
margins, dydx(housework_bkt) at(disapproval=(2.1(.10)2.4))

	**regional attitudes: gender roles
	melogit dissolve_lag i.dur c.genderroles_egal i.housework_bkt c.genderroles_egal#i.housework_bkt `controls' if couple_educ_gp==1 & housework_bkt < 4 || state_fips:, or
	margins, dydx(housework_bkt) at(genderroles_egal=(0.56(.04)0.72))

	**regional attitudes: working mom
	melogit dissolve_lag i.dur c.working_mom_egal i.housework_bkt c.working_mom_egal#i.housework_bkt `controls' if couple_educ_gp==1 & housework_bkt < 4 || state_fips:, or
	margins, dydx(housework_bkt) at(working_mom_egal=(0.66(.02)0.72))

	**regional attitudes: preschool
	melogit dissolve_lag i.dur c.preschool_egal i.housework_bkt c.preschool_egal#i.housework_bkt `controls' if couple_educ_gp==1 & housework_bkt < 4 || state_fips:, or
	margins, dydx(housework_bkt) at(preschool_egal=(0.58(.02)0.64))

** Minimum wage
melogit dissolve_lag i.dur i.min_above_fed i.housework_bkt i.min_above_fed#i.housework_bkt `controls' if couple_educ_gp==1 & housework_bkt < 4 || state_fips:, or
margins, dydx(housework_bkt) at(min_above_fed=(0 1))

**Unemployment compensation
melogit dissolve_lag i.dur c.unemployment_comp i.housework_bkt c.unemployment_comp#i.housework_bkt `controls' if couple_educ_gp==1 & housework_bkt < 4 || state_fips:, or
margins, dydx(housework_bkt) at(unemployment_comp=(200(200)800))

**% democrats in senate
melogit dissolve_lag i.dur c.senate_dems i.housework_bkt c.senate_dems#i.housework_bkt `controls' if couple_educ_gp==1 & housework_bkt < 4 || state_fips:, or
margins, dydx(housework_bkt) at(senate_dems=(0(.10)0.8))

**Paid Leave
melogit dissolve_lag i.dur i.paid_leave i.housework_bkt i.paid_leave#i.housework_bkt `controls' if couple_educ_gp==1 & housework_bkt < 4 || state_fips:, or
margins, dydx(housework_bkt) at(paid_leave=(0 1))

**Right to Work
melogit dissolve_lag i.dur i.right2work i.housework_bkt i.right2work#i.housework_bkt `controls' if couple_educ_gp==1 & housework_bkt < 4 || state_fips:, or
margins, dydx(housework_bkt) at(right2work=(0 1))

**Childcare costs
melogit dissolve_lag i.dur c.cc_pct_income i.housework_bkt c.cc_pct_income#i.housework_bkt `controls' if couple_educ_gp==1 & housework_bkt < 4 || state_fips:, or
margins, dydx(housework_bkt) at(cc_pct_income=(0.05(.10)0.35))

**Prek-12 education spending
melogit dissolve_lag i.dur c.educ_spend i.housework_bkt c.educ_spend#i.housework_bkt `controls' if couple_educ_gp==1 & housework_bkt < 4 || state_fips:, or
margins, dydx(housework_bkt) at(educ_spend=(4000(1000)9000))

**State Latent Class
melogit dissolve_lag i.dur i.predclass i.housework_bkt i.predclass#i.housework_bkt `controls' if couple_educ_gp==1 & housework_bkt < 4 || state_fips:, or
margins, dydx(housework_bkt) at(predclass=(1(1)4))

log close

/*
********************************************************************************
* Interactions: Employment variables
********************************************************************************
log using "$logdir/policy_interactions.log", replace

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth knot1 knot2 knot3"

/* No College */

** Minimum wage
melogit dissolve_lag i.dur i.min_above_fed i.ft_head i.ft_wife i.min_above_fed#i.ft_wife i.min_above_fed#i.ft_head `controls' if couple_educ_gp==0 || state_fips:, or
// margins min_above_fed#ft_head
// margins min_above_fed#ft_wife
// margins, dydx(ft_head ft_wife) over(min_above_fed) // this isn't quite matching the above. which is right?!
margins, dydx(ft_head ft_wife) at(min_above_fed=(0 1))
// margins, dydx(ft_head#min_above_fed)

**Rent affordability
melogit dissolve_lag i.dur c.rent_afford i.ft_head i.ft_wife c.rent_afford#i.ft_wife c.rent_afford#i.ft_head `controls' if couple_educ_gp==0 || state_fips:, or
margins, dydx(ft_head ft_wife) at(rent_afford=(0.25(.05)0.45))

**Unemployment
melogit dissolve_lag i.dur c.unemployment i.ft_head i.ft_wife c.unemployment#i.ft_wife c.unemployment#i.ft_head `controls' if couple_educ_gp==0 || state_fips:, or
margins, dydx(ft_head ft_wife) at(unemployment=(3(2)11))

**Gender LFP gap
melogit dissolve_lag i.dur c.gender_lfp_gap_nocoll i.ft_head i.ft_wife c.gender_lfp_gap_nocoll#i.ft_wife c.gender_lfp_gap_nocoll#i.ft_head `controls' if couple_educ_gp==0 || state_fips:, or
margins, dydx(ft_head ft_wife) at(gender_lfp_gap_nocoll=(0.60(.05)0.85))

**Paid Leave
melogit dissolve_lag i.dur i.paid_leave i.ft_head i.ft_wife i.paid_leave#i.ft_wife i.paid_leave#i.ft_head `controls' if couple_educ_gp==0 || state_fips:, or
margins, dydx(ft_head ft_wife) at(paid_leave=(0 1))

**Childcare subsidies
melogit dissolve_lag i.dur c.cc_subsidies i.ft_head i.ft_wife c.cc_subsidies#i.ft_wife c.cc_subsidies#i.ft_head `controls' if couple_educ_gp==0 || state_fips:, or
margins, dydx(ft_head ft_wife) at(cc_subsidies=(0.05(.10)0.45))

**% democrats in senate
melogit dissolve_lag i.dur c.senate_dems i.ft_head i.ft_wife c.senate_dems#i.ft_wife c.senate_dems#i.ft_head `controls' if couple_educ_gp==0 || state_fips:, or
margins, dydx(ft_head ft_wife) at(senate_dems=(0(.10)0.8))

**attitude summary
melogit dissolve_lag i.dur c.disapproval i.ft_head i.ft_wife c.disapproval#i.ft_wife c.disapproval#i.ft_head `controls' if couple_educ_gp==0 || state_fips:, or
margins, dydx(ft_head ft_wife) at(disapproval=(2.1(.10)2.4))

**attitude summary - alt
melogit dissolve_lag i.dur i.disapproval_bkt i.ft_head i.ft_wife i.disapproval_bkt#i.ft_wife i.disapproval_bkt#i.ft_head `controls' if couple_educ_gp==0 || state_fips:, or
margins, dydx(ft_head ft_wife) at(disapproval_bkt=(1(1)5))


/* College */

** Minimum wage
melogit dissolve_lag i.dur i.min_above_fed i.ft_head i.ft_wife i.min_above_fed#i.ft_wife i.min_above_fed#i.ft_head `controls' if couple_educ_gp==1 || state_fips:, or
margins, dydx(ft_head ft_wife) at(min_above_fed=(0 1))

**Rent affordability
melogit dissolve_lag i.dur c.rent_afford i.ft_head i.ft_wife c.rent_afford#i.ft_wife c.rent_afford#i.ft_head `controls' if couple_educ_gp==1 || state_fips:, or
margins, dydx(ft_head ft_wife) at(rent_afford=(0.25(.05)0.45))

**Unemployment
melogit dissolve_lag i.dur c.unemployment i.ft_head i.ft_wife c.unemployment#i.ft_wife c.unemployment#i.ft_head `controls' if couple_educ_gp==1 || state_fips:, or
margins, dydx(ft_head ft_wife) at(unemployment=(3(2)11))

**Gender LFP gap
melogit dissolve_lag i.dur c.gender_lfp_gap_nocoll i.ft_head i.ft_wife c.gender_lfp_gap_nocoll#i.ft_wife c.gender_lfp_gap_nocoll#i.ft_head `controls' if couple_educ_gp==1 || state_fips:, or
margins, dydx(ft_head ft_wife) at(gender_lfp_gap_nocoll=(0.60(.05)0.85))

**Paid Leave
melogit dissolve_lag i.dur i.paid_leave i.ft_head i.ft_wife i.paid_leave#i.ft_wife i.paid_leave#i.ft_head `controls' if couple_educ_gp==1 || state_fips:, or
margins, dydx(ft_head ft_wife) at(paid_leave=(0 1))

**Childcare subsidies
melogit dissolve_lag i.dur c.cc_subsidies i.ft_head i.ft_wife c.cc_subsidies#i.ft_wife c.cc_subsidies#i.ft_head `controls' if couple_educ_gp==1 || state_fips:, or
margins, dydx(ft_head ft_wife) at(cc_subsidies=(0.05(.10)0.45))

**% democrats in senate
melogit dissolve_lag i.dur c.senate_dems i.ft_head i.ft_wife c.senate_dems#i.ft_wife c.senate_dems#i.ft_head `controls' if couple_educ_gp==1 || state_fips:, or
margins, dydx(ft_head ft_wife) at(senate_dems=(0(.10)0.8))

**attitude summary
melogit dissolve_lag i.dur c.disapproval i.ft_head i.ft_wife c.disapproval#i.ft_wife c.disapproval#i.ft_head `controls' if couple_educ_gp==1 || state_fips:, or
margins, dydx(ft_head ft_wife) at(disapproval=(2.1(.10)2.4))

log close

********************************************************************************
* Interactions: Earnings
********************************************************************************
log using "$logdir/policy_interactions_earnings.log", replace

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth"

/* No College */

** Minimum wage
melogit dissolve_lag i.dur i.min_above_fed c.knot1 c.knot2 c.knot3 i.min_above_fed#c.knot2 i.min_above_fed#c.knot3 `controls' if couple_educ_gp==0 || state_fips:, or
margins, dydx(knot2 knot3) at(min_above_fed=(0 1))

**Rent affordability
melogit dissolve_lag i.dur c.rent_afford c.knot2 c.knot3 c.rent_afford#c.knot2 c.rent_afford#c.knot3 `controls' if couple_educ_gp==0 || state_fips:, or
margins, dydx(knot2 knot3) at(rent_afford=(0.25(.05)0.45))

**Unemployment
melogit dissolve_lag i.dur c.unemployment c.knot2 c.knot3 c.unemployment#c.knot2 c.unemployment#c.knot3 `controls' if couple_educ_gp==0 || state_fips:, or
margins, dydx(knot2 knot3) at(unemployment=(3(2)11))

**Gender LFP gap
melogit dissolve_lag i.dur c.gender_lfp_gap_nocoll c.knot2 c.knot3 c.gender_lfp_gap_nocoll#c.knot2 c.gender_lfp_gap_nocoll#c.knot3 `controls' if couple_educ_gp==0 || state_fips:, or
margins, dydx(knot2 knot3) at(gender_lfp_gap_nocoll=(0.60(.05)0.85))

**Paid Leave
melogit dissolve_lag i.dur i.paid_leave c.knot2 c.knot3 i.paid_leave#c.knot2 i.paid_leave#c.knot3 `controls' if couple_educ_gp==0 || state_fips:, or
margins, dydx(knot2 knot3) at(paid_leave=(0 1))

**Childcare subsidies
melogit dissolve_lag i.dur c.cc_subsidies c.knot2 c.knot3 c.cc_subsidies#c.knot2 c.cc_subsidies#c.knot3  `controls' if couple_educ_gp==0 || state_fips:, or
margins, dydx(knot2 knot3) at(cc_subsidies=(0.05(.10)0.45))

**% democrats in senate
melogit dissolve_lag i.dur c.senate_dems c.knot2 c.knot3 c.senate_dems#c.knot2 c.senate_dems#c.knot3 `controls' if couple_educ_gp==0 || state_fips:, or
margins, dydx(knot2 knot3) at(senate_dems=(0(.10)0.8))

**attitude summary
melogit dissolve_lag i.dur c.disapproval c.knot2 c.knot3 c.disapproval#c.knot2 c.disapproval#c.knot3 `controls' if couple_educ_gp==0 || state_fips:, or
margins, dydx(knot2 knot3) at(disapproval=(2.1(.10)2.4))

/* College */

** Minimum wage
melogit dissolve_lag i.dur i.min_above_fed c.knot1 c.knot2 c.knot3 i.min_above_fed#c.knot2 i.min_above_fed#c.knot3 `controls' if couple_educ_gp==1 || state_fips:, or
margins, dydx(knot2 knot3) at(min_above_fed=(0 1))

**Rent affordability
melogit dissolve_lag i.dur c.rent_afford c.knot2 c.knot3 c.rent_afford#c.knot2 c.rent_afford#c.knot3 `controls' if couple_educ_gp==1 || state_fips:, or
margins, dydx(knot2 knot3) at(rent_afford=(0.25(.05)0.45))

**Unemployment
melogit dissolve_lag i.dur c.unemployment c.knot2 c.knot3 c.unemployment#c.knot2 c.unemployment#c.knot3 `controls' if couple_educ_gp==1 || state_fips:, or
margins, dydx(knot2 knot3) at(unemployment=(3(2)11))

**Gender LFP gap
melogit dissolve_lag i.dur c.gender_lfp_gap_nocoll c.knot2 c.knot3 c.gender_lfp_gap_nocoll#c.knot2 c.gender_lfp_gap_nocoll#c.knot3 `controls' if couple_educ_gp==1 || state_fips:, or
margins, dydx(knot2 knot3) at(gender_lfp_gap_nocoll=(0.60(.05)0.85))

**Paid Leave - not converging
melogit dissolve_lag i.dur i.paid_leave c.knot2 c.knot3 i.paid_leave#c.knot2 i.paid_leave#c.knot3 `controls' if couple_educ_gp==1 || state_fips:, or
margins, dydx(knot2 knot3) at(paid_leave=(0 1))

**Childcare subsidies
melogit dissolve_lag i.dur c.cc_subsidies c.knot2 c.knot3 c.cc_subsidies#c.knot2 c.cc_subsidies#c.knot3  `controls' if couple_educ_gp==1 || state_fips:, or
margins, dydx(knot2 knot3) at(cc_subsidies=(0.05(.10)0.45))

**% democrats in senate
melogit dissolve_lag i.dur c.senate_dems c.knot2 c.knot3 c.senate_dems#c.knot2 c.senate_dems#c.knot3 `controls' if couple_educ_gp==1 || state_fips:, or
margins, dydx(knot2 knot3) at(senate_dems=(0(.10)0.8))

**attitude summary
melogit dissolve_lag i.dur c.disapproval c.knot2 c.knot3 c.disapproval#c.knot2 c.disapproval#c.knot3 `controls' if couple_educ_gp==1 || state_fips:, or
margins, dydx(knot2 knot3) at(disapproval=(2.1(.10)2.4))

log close
*/


********************************************************************************
********************************************************************************
********************************************************************************
**# Preliminary analysis
********************************************************************************
********************************************************************************
********************************************************************************

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth"
//// state variables
* Min wage
melogit dissolve_lag i.dur i.above_fed_min if couple_educ_gp==0 || state_fips:, or
outreg2 using "$results/psid_dissolution_state.xls", sideway stats(coef pval) label ctitle(Wage No) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

logit dissolve_lag i.dur i.above_fed_min if couple_educ_gp==0, or // okay these are essentially the same
melogit dissolve_lag i.dur i.above_fed_min if couple_educ_gp==1 || state_fips:, or
outreg2 using "$results/psid_dissolution_state.xls", sideway stats(coef pval) label ctitle(Wage Coll) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Cost of Living
//melogit dissolve_lag i.dur state_cpi_bfh_est if couple_educ_gp==0 || state_fips:, or
//melogit dissolve_lag i.dur state_cpi_bfh_est if couple_educ_gp==1 || state_fips:, or

melogit dissolve_lag i.dur i.above_fed_cpi if couple_educ_gp==0 || state_fips:, or // missing some data past 2010
outreg2 using "$results/psid_dissolution_state.xls", sideway stats(coef pval) label ctitle(CPI No) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

melogit dissolve_lag i.dur i.above_fed_cpi  if couple_educ_gp==1 || state_fips:, or
outreg2 using "$results/psid_dissolution_state.xls", sideway stats(coef pval) label ctitle(CPI Coll) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Social Policy
melogit dissolve_lag i.dur policysociallib_est if couple_educ_gp==0 || state_fips:, or
outreg2 using "$results/psid_dissolution_state.xls", sideway stats(coef pval) label ctitle(Soc-Policy No) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

melogit dissolve_lag i.dur policysociallib_est if couple_educ_gp==1 || state_fips:, or
outreg2 using "$results/psid_dissolution_state.xls", sideway stats(coef pval) label ctitle(Soc-Policy Coll) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

//melogit dissolve_lag i.dur i.social_policy if couple_educ_gp==0 || state_fips:, or
//melogit dissolve_lag i.dur i.social_policy if couple_educ_gp==1 || state_fips:, or
//melogit dissolve_lag i.dur i.social_policy_gp if couple_educ_gp==0 || state_fips:, or
//melogit dissolve_lag i.dur i.social_policy_gp if couple_educ_gp==1 || state_fips:, or

* Economic Policy
melogit dissolve_lag i.dur policyeconlib_est if couple_educ_gp==0 || state_fips:, or
outreg2 using "$results/psid_dissolution_state.xls", sideway stats(coef pval) label ctitle(Econ-Policy No) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

melogit dissolve_lag i.dur policyeconlib_est if couple_educ_gp==1 || state_fips:, or
outreg2 using "$results/psid_dissolution_state.xls", sideway stats(coef pval) label ctitle(Econ-Policy Coll) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

* Attitudes
melogit dissolve_lag i.dur masssociallib_est if couple_educ_gp==0 || state_fips:, or
outreg2 using "$results/psid_dissolution_state.xls", sideway stats(coef pval) label ctitle(Lib Attitudes No) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

melogit dissolve_lag i.dur masssociallib_est if couple_educ_gp==1 || state_fips:, or
outreg2 using "$results/psid_dissolution_state.xls", sideway stats(coef pval) label ctitle(Lib Attitudes Coll) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

melogit dissolve_lag i.dur i.liberal_attitudes_gp if couple_educ_gp==0 || state_fips:, or
melogit dissolve_lag i.dur i.liberal_attitudes_gp if couple_educ_gp==1 || state_fips:, or

////////// No College \\\\\\\\\\\/
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth"

** Total earnings
melogit dissolve_lag i.dur c.TAXABLE_HEAD_WIFE_##i.above_fed_min `controls' if couple_educ_gp==0 || state_fips:, or
outreg2 using "$results/psid_dissolution_state_nocoll.xls", sideway stats(coef pval) label ctitle(Earnings 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

// logit dissolve_lag i.dur c.TAXABLE_HEAD_WIFE_##i.above_fed_min age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth if couple_educ_gp==0, or
margins, at(TAXABLE_HEAD_WIFE_ =(0(10000)100000) above_fed_min =(0 1))
marginsplot

melogit dissolve_lag i.dur c.TAXABLE_HEAD_WIFE_##i.above_fed_cpi `controls' if couple_educ_gp==0 || state_fips:, or
outreg2 using "$results/psid_dissolution_state_nocoll.xls", sideway stats(coef pval) label ctitle(Earnings 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

//margins, at(TAXABLE_HEAD_WIFE_ =(0(10000)100000) above_fed_cpi =(0 1))
//marginsplot

melogit dissolve_lag i.dur c.TAXABLE_HEAD_WIFE_##c.policysociallib_est `controls' if couple_educ_gp==0 || state_fips:, or
outreg2 using "$results/psid_dissolution_state_nocoll.xls", sideway stats(coef pval) label ctitle(Earnings 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

//logit dissolve_lag i.dur c.TAXABLE_HEAD_WIFE_##c.policysociallib_est age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth if couple_educ_gp==0, or
//margins, at(TAXABLE_HEAD_WIFE_ =(0(10000)100000) policysociallib_est =(-2(1)2))
//marginsplot

melogit dissolve_lag i.dur c.TAXABLE_HEAD_WIFE_##c.policyeconlib_est `controls' if couple_educ_gp==0 || state_fips:, or
outreg2 using "$results/psid_dissolution_state_nocoll.xls", sideway stats(coef pval) label ctitle(Earnings 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

//logit dissolve_lag i.dur c.TAXABLE_HEAD_WIFE_##c.policyeconlib_est age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth if couple_educ_gp==0, or
//margins, at(TAXABLE_HEAD_WIFE_ =(0(10000)100000) policyeconlib_est =(-2(1)2))
//marginsplot

melogit dissolve_lag i.dur c.TAXABLE_HEAD_WIFE_##c.masssociallib_est `controls' if couple_educ_gp==0 || state_fips:, or
outreg2 using "$results/psid_dissolution_state_nocoll.xls", sideway stats(coef pval) label ctitle(Earnings 5) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

//logit dissolve_lag i.dur c.TAXABLE_HEAD_WIFE_##c.masssociallib_est age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth if couple_educ_gp==0, or
//margins, at(TAXABLE_HEAD_WIFE_ =(0(10000)100000) masssociallib_est =(-1(1)2))
//marginsplot

**Paid work
melogit dissolve_lag i.dur i.hh_earn_type##i.above_fed_min TAXABLE_HEAD_WIFE_  `controls' if couple_educ_gp==0 || state_fips:, or
outreg2 using "$results/psid_dissolution_state_nocoll.xls", sideway stats(coef pval) label ctitle(Paid 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

//logit dissolve_lag i.dur i.hh_earn_type##i.above_fed_min TAXABLE_HEAD_WIFE_ age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth if couple_educ_gp==0 & hh_earn_type<4, or
//margins hh_earn_type#above_fed_min
// marginsplot

melogit dissolve_lag i.dur i.hh_earn_type##i.above_fed_cpi TAXABLE_HEAD_WIFE_  `controls' if couple_educ_gp==0 || state_fips:, or
outreg2 using "$results/psid_dissolution_state_nocoll.xls", sideway stats(coef pval) label ctitle(Paid 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

melogit dissolve_lag i.dur i.hh_earn_type##c.policysociallib_est TAXABLE_HEAD_WIFE_  `controls' if couple_educ_gp==0 || state_fips:, or
outreg2 using "$results/psid_dissolution_state_nocoll.xls", sideway stats(coef pval) label ctitle(Paid 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

//logit dissolve_lag i.dur i.hh_earn_type##c.policysociallib_est TAXABLE_HEAD_WIFE_  age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth if couple_educ_gp==0 & hh_earn_type<4, or
//margins hh_earn_type, at(policysociallib_est =(-2(1)2))
//marginsplot

melogit dissolve_lag i.dur i.hh_earn_type##c.policyeconlib_est TAXABLE_HEAD_WIFE_  `controls' if couple_educ_gp==0 || state_fips:, or
outreg2 using "$results/psid_dissolution_state_nocoll.xls", sideway stats(coef pval) label ctitle(Paid 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

//logit dissolve_lag i.dur i.hh_earn_type##c.policyeconlib_est TAXABLE_HEAD_WIFE_  age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth if couple_educ_gp==0 & hh_earn_type<4, or
//margins hh_earn_type, at(policyeconlib_est =(-2(1)2))
//marginsplot


melogit dissolve_lag i.dur i.hh_earn_type##c.masssociallib_est TAXABLE_HEAD_WIFE_  `controls' if couple_educ_gp==0 || state_fips:, or
outreg2 using "$results/psid_dissolution_state_nocoll.xls", sideway stats(coef pval) label ctitle(Paid 5) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

//margins hh_earn_type#above_fed_min
//marginsplot

**Unpaid work
melogit dissolve_lag i.dur i.housework_bkt##i.above_fed_min TAXABLE_HEAD_WIFE_  `controls' if couple_educ_gp==0 || state_fips:, or
outreg2 using "$results/psid_dissolution_state_nocoll.xls", sideway stats(coef pval) label ctitle(Unpaid 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// logit dissolve_lag i.dur i.housework_bkt##i.above_fed_min TAXABLE_HEAD_WIFE_  age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth if couple_educ_gp==0 & housework_bkt<4, or
//margins housework_bkt#above_fed_min
//marginsplot

melogit dissolve_lag i.dur i.housework_bkt##i.above_fed_cpi TAXABLE_HEAD_WIFE_  `controls' if couple_educ_gp==0 || state_fips:, or
outreg2 using "$results/psid_dissolution_state_nocoll.xls", sideway stats(coef pval) label ctitle(Unpaid 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

melogit dissolve_lag i.dur i.housework_bkt##c.policysociallib_est TAXABLE_HEAD_WIFE_  `controls' if couple_educ_gp==0 || state_fips:, or
outreg2 using "$results/psid_dissolution_state_nocoll.xls", sideway stats(coef pval) label ctitle(Unpaid 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// logit dissolve_lag i.dur i.housework_bkt##c.policysociallib_est TAXABLE_HEAD_WIFE_  age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth if couple_educ_gp==0 & housework_bkt<4, or
//margins housework_bkt, at(policysociallib_est =(-2(1)2))
//marginsplot

melogit dissolve_lag i.dur i.housework_bkt##c.policyeconlib_est TAXABLE_HEAD_WIFE_  `controls' if couple_educ_gp==0 || state_fips:, or
outreg2 using "$results/psid_dissolution_state_nocoll.xls", sideway stats(coef pval) label ctitle(Unpaid 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// logit dissolve_lag i.dur i.housework_bkt##c.policyeconlib_est TAXABLE_HEAD_WIFE_  age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth if couple_educ_gp==0 & housework_bkt<4, or
//margins housework_bkt, at(policyeconlib_est =(-2(1)2))
//marginsplot


melogit dissolve_lag i.dur i.housework_bkt##c.masssociallib_est TAXABLE_HEAD_WIFE_  `controls' if couple_educ_gp==0 || state_fips:, or
outreg2 using "$results/psid_dissolution_state_nocoll.xls", sideway stats(coef pval) label ctitle(Unpaid 5) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append


////////// College \\\\\\\\\\\/
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth"

** Total earnings
melogit dissolve_lag i.dur c.TAXABLE_HEAD_WIFE_##i.above_fed_min `controls' if couple_educ_gp==1 || state_fips:, or
outreg2 using "$results/psid_dissolution_state_college.xls", sideway stats(coef pval) label ctitle(Earnings 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

//margins, at(TAXABLE_HEAD_WIFE_ =(0(10000)100000) above_fed_min =(0 1))
//marginsplot

melogit dissolve_lag i.dur c.TAXABLE_HEAD_WIFE_##i.above_fed_cpi `controls' if couple_educ_gp==1 || state_fips:, or
outreg2 using "$results/psid_dissolution_state_college.xls", sideway stats(coef pval) label ctitle(Earnings 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

//margins, at(TAXABLE_HEAD_WIFE_ =(0(10000)100000) above_fed_cpi =(0 1))
//marginsplot

melogit dissolve_lag i.dur c.TAXABLE_HEAD_WIFE_##c.policysociallib_est `controls' if couple_educ_gp==1 || state_fips:, or
outreg2 using "$results/psid_dissolution_state_college.xls", sideway stats(coef pval) label ctitle(Earnings 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

//logit dissolve_lag i.dur c.TAXABLE_HEAD_WIFE_##c.policysociallib_est age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth if couple_educ_gp==1, or
//margins, at(TAXABLE_HEAD_WIFE_ =(0(10000)100000) policysociallib_est =(-2(1)2))
//marginsplot

melogit dissolve_lag i.dur c.TAXABLE_HEAD_WIFE_##c.policyeconlib_est `controls' if couple_educ_gp==1 || state_fips:, or
outreg2 using "$results/psid_dissolution_state_college.xls", sideway stats(coef pval) label ctitle(Earnings 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

//logit dissolve_lag i.dur c.TAXABLE_HEAD_WIFE_##c.policyeconlib_est age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth if couple_educ_gp==1, or
//margins, at(TAXABLE_HEAD_WIFE_ =(0(10000)100000) policyeconlib_est =(-2(1)2))
//marginsplot

melogit dissolve_lag i.dur c.TAXABLE_HEAD_WIFE_##c.masssociallib_est `controls' if couple_educ_gp==1 || state_fips:, or
outreg2 using "$results/psid_dissolution_state_college.xls", sideway stats(coef pval) label ctitle(Earnings 5) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

//margins, at(TAXABLE_HEAD_WIFE_ =(0(10000)100000) masssociallib_est =(-1(1)2))
//marginsplot

**Paid work
melogit dissolve_lag i.dur i.hh_earn_type##i.above_fed_min TAXABLE_HEAD_WIFE_  `controls' if couple_educ_gp==1 || state_fips:, or
outreg2 using "$results/psid_dissolution_state_college.xls", sideway stats(coef pval) label ctitle(Paid 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

//logit dissolve_lag i.dur i.hh_earn_type##i.above_fed_min TAXABLE_HEAD_WIFE_ age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth if couple_educ_gp==1 & hh_earn_type<4, or
//margins hh_earn_type#above_fed_min
// marginsplot

melogit dissolve_lag i.dur i.hh_earn_type##i.above_fed_cpi TAXABLE_HEAD_WIFE_  `controls' if couple_educ_gp==1 || state_fips:, or
outreg2 using "$results/psid_dissolution_state_college.xls", sideway stats(coef pval) label ctitle(Paid 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

melogit dissolve_lag i.dur i.hh_earn_type##c.policysociallib_est TAXABLE_HEAD_WIFE_  `controls' if couple_educ_gp==1 || state_fips:, or
outreg2 using "$results/psid_dissolution_state_college.xls", sideway stats(coef pval) label ctitle(Paid 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

//logit dissolve_lag i.dur i.hh_earn_type##c.policysociallib_est TAXABLE_HEAD_WIFE_ age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth if couple_educ_gp==1 & hh_earn_type<4, or
//margins hh_earn_type, at(policysociallib_est =(-2(1)2))
// marginsplot

melogit dissolve_lag i.dur i.hh_earn_type##c.policyeconlib_est TAXABLE_HEAD_WIFE_  `controls' if couple_educ_gp==1 || state_fips:, or
outreg2 using "$results/psid_dissolution_state_college.xls", sideway stats(coef pval) label ctitle(Paid 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

//logit dissolve_lag i.dur i.hh_earn_type##c.policyeconlib_est TAXABLE_HEAD_WIFE_ age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth if couple_educ_gp==1 & hh_earn_type<4, or
//margins hh_earn_type, at(policyeconlib_est =(-2(1)2))
// marginsplot


melogit dissolve_lag i.dur i.hh_earn_type##c.masssociallib_est TAXABLE_HEAD_WIFE_  `controls' if couple_educ_gp==1 || state_fips:, or
outreg2 using "$results/psid_dissolution_state_college.xls", sideway stats(coef pval) label ctitle(Paid 5) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

//logit dissolve_lag i.dur i.hh_earn_type##c.masssociallib_est TAXABLE_HEAD_WIFE_ age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth if couple_educ_gp==1 & hh_earn_type<4, or
//margins hh_earn_type, at(masssociallib_est =(-1(1)2))
// marginsplot

**Unpaid work
melogit dissolve_lag i.dur i.housework_bkt##i.above_fed_min TAXABLE_HEAD_WIFE_  `controls' if couple_educ_gp==1 || state_fips:, or
outreg2 using "$results/psid_dissolution_state_college.xls", sideway stats(coef pval) label ctitle(Unpaid 1) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

melogit dissolve_lag i.dur i.housework_bkt##i.above_fed_cpi TAXABLE_HEAD_WIFE_  `controls' if couple_educ_gp==1 || state_fips:, or
outreg2 using "$results/psid_dissolution_state_college.xls", sideway stats(coef pval) label ctitle(Unpaid 2) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

melogit dissolve_lag i.dur i.housework_bkt##c.policysociallib_est TAXABLE_HEAD_WIFE_  `controls' if couple_educ_gp==1 || state_fips:, or
outreg2 using "$results/psid_dissolution_state_college.xls", sideway stats(coef pval) label ctitle(Unpaid 3) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

melogit dissolve_lag i.dur i.housework_bkt##c.policyeconlib_est TAXABLE_HEAD_WIFE_  `controls' if couple_educ_gp==1 || state_fips:, or
outreg2 using "$results/psid_dissolution_state_college.xls", sideway stats(coef pval) label ctitle(Unpaid 4) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

melogit dissolve_lag i.dur i.housework_bkt##c.masssociallib_est TAXABLE_HEAD_WIFE_  `controls' if couple_educ_gp==1 || state_fips:, or
outreg2 using "$results/psid_dissolution_state_college.xls", sideway stats(coef pval) label ctitle(Unpaid 5) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// paid leave
logit dissolve_lag i.dur i.hh_earn_type##i.paid_leave_state TAXABLE_HEAD_WIFE_ age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth if couple_educ_gp==0 & hh_earn_type<4, or
margins hh_earn_type#paid_leave_state
 marginsplot
 
logit dissolve_lag i.dur i.hh_earn_type##i.paid_leave_state TAXABLE_HEAD_WIFE_ age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth if couple_educ_gp==1 & hh_earn_type<4, or
margins hh_earn_type#paid_leave_state
 marginsplot
 
 logit dissolve_lag i.dur i.housework_bkt##i.paid_leave_state TAXABLE_HEAD_WIFE_ age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth if couple_educ_gp==1 & hh_earn_type<4, or
margins housework_bkt#paid_leave_state
 marginsplot
 
logit dissolve_lag i.dur i.hh_earn_type##i.time_leave TAXABLE_HEAD_WIFE_ age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth if couple_educ_gp==1 & hh_earn_type<4, or
margins hh_earn_type#time_leave
marginsplot

********************************************************************************
* Where do they reside?
********************************************************************************
gen college_pop = 1 if couple_educ_gp==1
gen no_college_pop = 1 if couple_educ_gp==0

tabstat policysociallib_est, by(state_fips)
tabstat policyeconlib_est, by(state_fips)
tabstat masssociallib_est, by(state_fips)

preserve
collapse (mean) policysociallib_est policyeconlib_est masssociallib_est (sum) college_pop no_college_pop, by(state_fips)
restore

tab social_policy
tab couple_educ_gp social_policy, row

tabstat policysociallib_est, by(couple_educ_gp)
// Neither College | -.1055554
// At Least One Col |   .129121

tabstat policysociallib_est if dissolve_lag==1, by(couple_educ_gp)
// Neither College | -.1897416
// At Least One Col | -.0548838


/********************************************************************************
********************************************************************************
********************************************************************************
* Models - old
********************************************************************************
********************************************************************************
********************************************************************************
// // outreg2 using "$results/psid_marriage_dissolution.xls", sideway stats(coef pval) label ctitle(Earnings No) dec(2) eform alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth"

** Minimum wage
// total earnings
logit dissolve_lag i.dur TAXABLE_HEAD_WIFE_ if couple_educ_gp==0, or
logit dissolve_lag i.dur statemin if couple_educ_gp==0, or // not sig on own
logit dissolve_lag i.dur c.TAXABLE_HEAD_WIFE_##c.statemin if couple_educ_gp==0, or // interaction is
margins, at(TAXABLE_HEAD_WIFE_ =(0(10000)100000) statemin =(4(2)10))
marginsplot

logit dissolve_lag i.dur TAXABLE_HEAD_WIFE_ if couple_educ_gp==1, or
logit dissolve_lag i.dur statemin if couple_educ_gp==1, or // not sig on own
logit dissolve_lag i.dur c.TAXABLE_HEAD_WIFE_##c.statemin if couple_educ_gp==1, or // not sig
margins, at(TAXABLE_HEAD_WIFE_ =(0(10000)100000) statemin =(4(2)10))
marginsplot

logit dissolve_lag i.dur TAXABLE_HEAD_WIFE_ `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or

// paid work
logit dissolve_lag i.dur i.hh_earn_type if couple_educ_gp==0, or
logit dissolve_lag i.dur i.hh_earn_type##c.statemin if couple_educ_gp==0, or // nothing sig still
margins hh_earn_type, at(statemin =(4(2)10))
marginsplot

logit dissolve_lag i.dur i.hh_earn_type##c.statemin TAXABLE_HEAD_WIFE_ if couple_educ_gp==0, or // state min on own is marginally positively associated with divorce (so among dual earners - the ref group?)
margins hh_earn_type, at(statemin =(4(2)10)) // this is interesting, but nothing sig
marginsplot

logit dissolve_lag i.dur i.hh_earn_type##c.statemin TAXABLE_HEAD_WIFE_ if couple_educ_gp==1, or // nothing sig
margins hh_earn_type, at(statemin =(4(2)10))
marginsplot

//Unpaid work
logit dissolve_lag i.dur i.housework_bkt TAXABLE_HEAD_WIFE_ if couple_educ_gp==0, or
logit dissolve_lag i.dur i.housework_bkt##c.statemin TAXABLE_HEAD_WIFE_ if couple_educ_gp==0 & housework_bkt!=4, or // male primary sig in interaction
margins housework_bkt, at(statemin =(4(2)10))
marginsplot

logit dissolve_lag i.dur i.housework_bkt TAXABLE_HEAD_WIFE_ if couple_educ_gp==1, or
logit dissolve_lag i.dur i.housework_bkt##c.statemin TAXABLE_HEAD_WIFE_ if couple_educ_gp==1 & housework_bkt!=4, or // nothing sig
margins housework_bkt, at(statemin =(4(2)10))
marginsplot

** Unemployment
// total earnings
logit dissolve_lag i.dur TAXABLE_HEAD_WIFE_ if couple_educ_gp==0, or
logit dissolve_lag i.dur unemployment if couple_educ_gp==0, or // is positive
logit dissolve_lag i.dur c.TAXABLE_HEAD_WIFE_##c.unemployment if couple_educ_gp==0, or // interaction not sig, but main effects are marginally
margins, at(TAXABLE_HEAD_WIFE_ =(0(10000)100000) unemployment =(3(2)9)) // okay so both independently shape divorce
marginsplot

logit dissolve_lag i.dur TAXABLE_HEAD_WIFE_ if couple_educ_gp==1, or
logit dissolve_lag i.dur unemployment if couple_educ_gp==1, or // no association
logit dissolve_lag i.dur c.TAXABLE_HEAD_WIFE_##c.unemployment if couple_educ_gp==1, or // interaction is sig, and so is main effect for total earnings
margins, at(TAXABLE_HEAD_WIFE_ =(0(10000)100000) unemployment =(3(2)9)) // income seems to matter, then, when unemployment is LOW
marginsplot

//Paid work
logit dissolve_lag i.dur i.hh_earn_type##c.unemployment TAXABLE_HEAD_WIFE_ if couple_educ_gp==0, or // main effect sig
margins hh_earn_type, at(unemployment =(3(2)9))
marginsplot

** Attitudes
// total earnings
logit dissolve_lag i.dur masssociallib_est if couple_educ_gp==0, or // no association
logit dissolve_lag i.dur c.TAXABLE_HEAD_WIFE_##c.masssociallib_est if couple_educ_gp==0, or // here, interaction sig (negative) and main effect = positive = so when more conservative, income does not matter, when liberal, income lowers divorce (or, when liberal, no income raises risk of divorce?)
margins, at(TAXABLE_HEAD_WIFE_ =(0(10000)100000) masssociallib_est =(-1(1)2))
marginsplot

logit dissolve_lag i.dur masssociallib_est if couple_educ_gp==1, or // not sig but directionally negative
logit dissolve_lag i.dur c.TAXABLE_HEAD_WIFE_##c.masssociallib_est if couple_educ_gp==1, or // nothing sig
margins, at(TAXABLE_HEAD_WIFE_ =(0(10000)100000) masssociallib_est =(-1(1)2))
marginsplot

//Paid work
logit dissolve_lag i.dur i.hh_earn_type##c.masssociallib_est TAXABLE_HEAD_WIFE_ if couple_educ_gp==0, or  // nothing sig
margins hh_earn_type, at(masssociallib_est =(-1(1)2)) 
marginsplot

logit dissolve_lag i.dur i.hh_earn_type##c.masssociallib_est TAXABLE_HEAD_WIFE_ if couple_educ_gp==1, or  // female BW main effect - so when more conservative, female BW is bad?
margins hh_earn_type, at(masssociallib_est =(-1(1)2)) 
marginsplot

//Unpaid work
logit dissolve_lag i.dur i.housework_bkt##c.masssociallib_est TAXABLE_HEAD_WIFE_ if couple_educ_gp==0 & housework_bkt!=4, or  // nothing sig
margins housework_bkt, at(masssociallib_est =(-1(1)2)) 
marginsplot

logit dissolve_lag i.dur i.housework_bkt##c.masssociallib_est TAXABLE_HEAD_WIFE_ if couple_educ_gp==1 & housework_bkt!=4, or // nothing sig
margins housework_bkt, at(masssociallib_est =(-1(1)2)) 
marginsplot

** Social policy
policysociallib_est

// total earnings
logit dissolve_lag i.dur policysociallib_est if couple_educ_gp==0, or // no association
logit dissolve_lag i.dur c.TAXABLE_HEAD_WIFE_##c.policysociallib_est if couple_educ_gp==0, or // main effect of income is negative
margins, at(TAXABLE_HEAD_WIFE_ =(0(10000)100000) policysociallib_est =(-2(1)2))
marginsplot

logit dissolve_lag i.dur policysociallib_est if couple_educ_gp==1, or // negative
logit dissolve_lag i.dur c.TAXABLE_HEAD_WIFE_##c.policysociallib_est if couple_educ_gp==1, or // interaction sig
margins, at(TAXABLE_HEAD_WIFE_ =(0(10000)100000) policysociallib_est =(-2(1)2)) // income more of a buffer in socially conservative places?
marginsplot

//Paid work
logit dissolve_lag i.dur i.hh_earn_type##c.policysociallib_est TAXABLE_HEAD_WIFE_ if couple_educ_gp==0, or  // main effect negative (so at dual earning?) and female BW interaction positive
margins hh_earn_type, at(policysociallib_est =(-2(1)2)) // so female BW bad when policy is liberal - so it IS necessity?!
marginsplot

logit dissolve_lag i.dur i.hh_earn_type##c.policysociallib_est TAXABLE_HEAD_WIFE_ if couple_educ_gp==1, or  // female BW main effect - so when more conservative, female BW is bad?
margins hh_earn_type, at(policysociallib_est =(-2(1)2)) 
marginsplot

//Unpaid work
logit dissolve_lag i.dur i.housework_bkt##c.policysociallib_est TAXABLE_HEAD_WIFE_ if couple_educ_gp==0 & housework_bkt!=4, or  // male primary main effect
margins housework_bkt, at(policysociallib_est =(-2(1)2)) 
marginsplot

logit dissolve_lag i.dur i.housework_bkt##c.policysociallib_est TAXABLE_HEAD_WIFE_ if couple_educ_gp==1 & housework_bkt!=4, or // nothing sig
margins housework_bkt, at(policysociallib_est =(-2(1)2)) 
marginsplot
*/