********************************************************************************
* Growth curves of female earnings % over marital duration
* growth_curves.do
* Kim McErlean
********************************************************************************

use "$data_keep\PSID_marriage_recoded_sample.dta", clear // created in 1a - no longer using my original order

gen cohort=.
replace cohort=1 if inrange(rel_start_all,1969,1989)
replace cohort=2 if inrange(rel_start_all,1990,2010)
replace cohort=3 if inrange(rel_start_all,2011,2019)

keep if cohort==2 // | cohort==3 // start with contemporary marriages - match divorce time frame
keep if marriage_order_real==1 // for now, just FIRST marriage
keep if (AGE_REF_>=18 & AGE_REF_<=55) &  (AGE_SPOUSE_>=18 & AGE_SPOUSE_<=55) // working age

rename STATE_ statefip
gen year = survey_yr
// merge m:1 year statefip using "T:/Research Projects/State data/data_keep/2010_2018_state_policies.dta", keepusing(cc_percent_served leave_policy leave_policy_score eitc_credit tanf_rate tanf_basic tanf_cc tanf_max cost_of_living20 tanf_max_cost abortion dems_legis women_legis) // adding in policy info
merge m:1 statefip using "$data_tmp\PSID_state_data.dta", keepusing(cost_living	living_wage	leave_policy_score sexism paid_leave) // trying new measures of policy, this is not yet updated for over time
drop if _merge==2
drop _merge


// also need to restrict to people who we observe from start. Some I have their start date, but not sure if in PSID whole time? so min dur = 0/1? Would I have done anything in file 1 that would remove early years of people? definitely removed if no partner, but that is still relevant here - need female earnings to get this...
bysort id: egen first_dur=min(dur)
keep if inlist(first_dur,1,2) // keeping one OR two because when survey shifted to biannual, year two may feasibly be the first time we can observe a full year of data?

// key splits:
* class: couple_educ_gp
* ever divorce: ever_dissolve - moved this to step 1a
sort id survey_yr
browse id survey_yr rel_start_all rel_end_all status_all hh_earn_type_bkd dissolve_lag ever_dissolve dur first_dur MARITAL_PAIRS_ if inlist(id,2009,2986,2992)
tab status_all ever_dissolve // lol this is very discordant... see IDs 2986, 2992 - okay got fixed, but still concerned on timing
* first child? for now - any children - think when doing actual curves, need to add a spline (or whatever - from DP class) at TIME of child: children - but 0 in years without, want EVER
by id: egen ever_children=max(children)
sort id survey_yr

// collapse by duration - do steps at a time
tab dur // when should I cut it off - did 1990 - 2019, so max is 30, but probably not a ton of people, so do 20? because even that is low.

// trying to divide by leave policy (as one example)
gen leave_policy_group=.
replace leave_policy_group=0 if leave_policy_score==0
replace leave_policy_group=1 if leave_policy_score>0 & leave_policy_score<=25
replace leave_policy_group=2 if leave_policy_score>25 & leave_policy_score<=80
replace leave_policy_group=3 if leave_policy_score>=85 & leave_policy_score!=.

gen leave_policy_group2=.
replace leave_policy_group2=0 if leave_policy_score==0
replace leave_policy_group2=1 if leave_policy_score>0 & leave_policy_score<=25
replace leave_policy_group2=2 if leave_policy_score>25 & leave_policy_score!=.

gen sexism_gp=.
replace sexism_gp=1 if sexism <=-2
replace sexism_gp=2 if sexism > -2 & sexism < 2
replace sexism_gp=3 if sexism >=2 & sexism!=.

label define sexism 1 "Low" 2 "Moderate" 3 "High"
label values sexism_gp sexism


********************************************************************************
* Exploratory plots
********************************************************************************

preserve
collapse (median) female_earn_pct, by(dur)
twoway line female_earn_pct dur if dur <=20
restore

preserve
collapse (median) female_earn_pct, by(dur couple_educ_gp ever_dissolve ever_children)
restore

preserve
collapse (median) female_earn_pct, by(dur couple_educ_gp)
twoway (line female_earn_pct dur if dur <=20 & couple_educ_gp==0) (line female_earn_pct dur if dur <=20 & couple_educ_gp==1), legend(on order(1 "No College" 2 "College"))
graph export "$results\earn_pct_education.jpg", as(jpg) name("Graph") quality(90) replace
restore

preserve
collapse (median) female_hours_pct, by(dur couple_educ_gp)
twoway (line female_hours_pct dur if dur <=20 & couple_educ_gp==0) (line female_hours_pct dur if dur <=20 & couple_educ_gp==1), legend(on order(1 "No College" 2 "College"))
restore

preserve
collapse (median) female_earn_pct if inrange(REGION_,1,4) & couple_educ_gp==1, by(dur REGION_)
twoway (line female_earn_pct dur if dur <=20 & REGION_==1) (line female_earn_pct dur if dur <=20 & REGION_==2) (line female_earn_pct dur if dur <=20 & REGION_==3) (line female_earn_pct dur if dur <=20 & REGION_==4), legend(on order(1 "Northeast" 2 "North Central" 3 "South" 4 "West"))
graph export "$results\earn_pct_region_college.jpg", as(jpg) name("Graph") quality(90) replace
restore

preserve
collapse (median) female_earn_pct if inrange(REGION_,1,4) & couple_educ_gp==1, by(dur STATE_)
twoway (line female_earn_pct dur if dur <=20 & STATE_==6) (line female_earn_pct dur if dur <=20 & STATE_==36) (line female_earn_pct dur if dur <=20 & STATE_==48) (line female_earn_pct dur if dur <=20 & STATE_==17) (line female_earn_pct dur if dur <=20 & STATE_==5), legend(on order(1 "California" 2 "New York" 3 "Texas" 4 "Illinois" 5 "Arkansas"))
graph export "$results\earn_pct_state_college.jpg", as(jpg) name("Graph") quality(90) replace
restore

preserve
collapse (median) female_earn_pct if couple_educ_gp==1, by(dur leave_policy_group)
twoway (line female_earn_pct dur if dur <=20 & leave_policy_group==0) (line female_earn_pct dur if dur <=20 & leave_policy_group==1) (line female_earn_pct dur if dur <=20 & leave_policy_group==2) (line female_earn_pct dur if dur <=20 & leave_policy_group==3), legend(on order(1 "None" 2 "Low" 3 "Medium" 4 "High"))
graph export "$results\earn_pct_policy_college.jpg", as(jpg) name("Graph") quality(90) replace
restore

preserve
collapse (median) female_earn_pct if year >=2011 & year<2019, by(dur leave_policy_group2)
twoway (line female_earn_pct dur if dur <=15 & leave_policy_group2==0) (line female_earn_pct dur if dur <=15 & leave_policy_group2==1) (line female_earn_pct dur if dur <=15 & leave_policy_group2==2), legend(on order(1 "Poor" 2 "Average" 3 "Good"))
graph export "$results\earn_pct_policy_all.jpg", as(jpg) name("Graph") quality(90) replace
restore

preserve
collapse (median) female_earn_pct, by(dur paid_leave couple_educ_gp)
twoway (line female_earn_pct dur if dur <=15 & paid_leave==0 & couple_educ_gp==0) (line female_earn_pct dur if dur <=15 & paid_leave==1 & couple_educ_gp==0) (line female_earn_pct dur if dur <=15 & paid_leave==0 & couple_educ_gp==1) (line female_earn_pct dur if dur <=15 & paid_leave==1 & couple_educ_gp==1) , legend(on order(1 "NC - No leave" 2 "NC - leave" 3 "Coll - no leave" 4 "Coll - leave"))
restore

preserve
collapse (median) female_earn_pct if couple_educ_gp==1 & ever_children==1, by(dur paid_leave)
twoway (line female_earn_pct dur if dur <=15 & paid_leave==0) (line female_earn_pct dur if dur <=15 & paid_leave==1), legend(on order(1 "No leave" 2 "Leave"))
restore

preserve
collapse (median) female_earn_pct if couple_educ_gp==1, by(dur paid_leave ever_children)
twoway (line female_earn_pct dur if dur <=15 & paid_leave==0 & ever_children==0) (line female_earn_pct dur if dur <=15 & paid_leave==1 & ever_children==0) (line female_earn_pct dur if dur <=15 & paid_leave==0 & ever_children==1) (line female_earn_pct dur if dur <=15 & paid_leave==1 & ever_children==1), legend(on order(1 "No leave - no kids" 2 "Leave - no kids" 3 "No leave" 4 "Leave"))
restore


preserve
collapse (median) female_earn_pct, by(dur sexism_gp)
twoway (line female_earn_pct dur if dur <=15 & sexism_gp==1) (line female_earn_pct dur if dur <=15 & sexism_gp==2) (line female_earn_pct dur if dur <=15 & sexism_gp==3), legend(on order(1 "Low" 2 "Medium" 3 "High"))
restore

preserve
collapse (median) female_earn_pct if couple_educ_gp==1 & ever_children==1, by(dur sexism_gp)
twoway (line female_earn_pct dur if dur <=15 & sexism_gp==1) (line female_earn_pct dur if dur <=15 & sexism_gp==2) (line female_earn_pct dur if dur <=15 & sexism_gp==3), legend(on order(1 "Low" 2 "Medium" 3 "High"))
restore


preserve
collapse (median) female_earn_pct if couple_educ_gp==1, by(dur sexism_gp)
twoway (line female_earn_pct dur if dur <=15 & sexism_gp==1) (line female_earn_pct dur if dur <=15 & sexism_gp==2) (line female_earn_pct dur if dur <=15 & sexism_gp==3), legend(on order(1 "Low" 2 "Medium" 3 "High"))
restore



preserve
collapse (median) female_earn_pct, by(dur sexism_gp couple_educ_gp)
twoway (line female_earn_pct dur if dur <=15 & sexism_gp==1 & couple_educ_gp==0) (line female_earn_pct dur if dur <=15 & sexism_gp==2 & couple_educ_gp==0) (line female_earn_pct dur if dur <=15 & sexism_gp==3 & couple_educ_gp==0) (line female_earn_pct dur if dur <=15 & sexism_gp==1 & couple_educ_gp==1) (line female_earn_pct dur if dur <=15 & sexism_gp==2 & couple_educ_gp==1) (line female_earn_pct dur if dur <=15 & sexism_gp==3 & couple_educ_gp==1) , legend(on order(1 "NC Low" 2 "NC Mod" 3 "NC High" 4 "Cll Low" 5 "Coll Mod" 6 "coll high"))
restore

preserve
collapse (median) female_earn_pct if ever_dissolve==0, by(dur couple_educ_gp)
twoway (line female_earn_pct dur if dur <=20 & couple_educ_gp==0) (line female_earn_pct dur if dur <=20 & couple_educ_gp==1), legend(on order(1 "No College" 2 "College"))
graph export "$results\earn_pct_education_intact.jpg", as(jpg) name("Graph") quality(90) replace
restore

preserve
collapse (median) female_earn_pct if ever_dissolve==1, by(dur couple_educ_gp)
twoway (line female_earn_pct dur if dur <=15 & couple_educ_gp==0) (line female_earn_pct dur if dur <=15 & couple_educ_gp==1), legend(on order(1 "No College" 2 "College"))
graph export "$results\earn_pct_education_ended.jpg", as(jpg) name("Graph") quality(90) replace
restore


preserve
collapse (median) female_earn_pct, by(dur ever_dissolve)
twoway (line female_earn_pct dur if dur <=20 & ever_dissolve==0) (line female_earn_pct dur if dur <=20 & ever_dissolve==1), legend(on order(1 "Intact" 2 "Dissolved"))
graph export "$results\earn_pct_dissolved.jpg", as(jpg) name("Graph") quality(90) replace
restore

preserve
collapse (median) female_earn_pct, by(dur ever_children)
twoway (line female_earn_pct dur if dur <=20 & ever_children==0) (line female_earn_pct dur if dur <=20 & ever_children==1), legend(on order(1 "No Children" 2 "Children"))
graph export "$results\earn_pct_children.jpg", as(jpg) name("Graph") quality(90) replace
restore

preserve
collapse (median) female_earn_pct, by(dur couple_educ_gp ever_dissolve)
twoway (line female_earn_pct dur if dur <=20 & couple_educ_gp==0 & ever_dissolve==0) (line female_earn_pct dur if dur <=20 & couple_educ_gp==0 & ever_dissolve==1) (line female_earn_pct dur if dur <=20 & couple_educ_gp==1 & ever_dissolve==0) (line female_earn_pct dur if dur <=20 & couple_educ_gp==1 & ever_dissolve==1), legend(on order(1 "NC - Intact" 2 "NC - Dissolved" 3 "Coll - Intact" 4 "Coll-Dissolved"))
twoway (line female_earn_pct dur if dur <=10 & couple_educ_gp==0 & ever_dissolve==0) (line female_earn_pct dur if dur <=10 & couple_educ_gp==0 & ever_dissolve==1) (line female_earn_pct dur if dur <=10 & couple_educ_gp==1 & ever_dissolve==0) (line female_earn_pct dur if dur <=10 & couple_educ_gp==1 & ever_dissolve==1), legend(on order(1 "NC - Intact" 2 "NC - Dissolved" 3 "Coll - Intact" 4 "Coll-Dissolved"))
graph export "$results\earn_pct_educ_x_dissolved.jpg", as(jpg) name("Graph") quality(90) replace
restore

preserve
collapse (median) female_earn_pct, by(dur couple_educ_gp ever_children)
twoway (line female_earn_pct dur if dur <=20 & couple_educ_gp==0 & ever_children==0) (line female_earn_pct dur if dur <=20 & couple_educ_gp==0 & ever_children==1) (line female_earn_pct dur if dur <=20 & couple_educ_gp==1 & ever_children==0) (line female_earn_pct dur if dur <=20 & couple_educ_gp==1 & ever_children==1), legend(on order(1 "NC - No Children" 2 "NC - Children" 3 "Coll - No Children" 4 "Coll-Children"))
graph export "$results\earn_pct_educ_x_children.jpg", as(jpg) name("Graph") quality(90) replace
restore

preserve
collapse (median) female_earn_pct if couple_educ_gp==1, by(dur ever_children)
twoway (line female_earn_pct dur if dur <=20 & ever_children==0) (line female_earn_pct dur if dur <=20 & ever_children==1), legend(on order(1 "No Children" 2 "Children"))
restore

preserve
collapse (median) female_earn_pct if ever_dissolve==0, by(dur couple_educ_gp ever_children)
twoway (line female_earn_pct dur if dur <=20 & couple_educ_gp==0 & ever_children==0) (line female_earn_pct dur if dur <=20 & couple_educ_gp==0 & ever_children==1) (line female_earn_pct dur if dur <=20 & couple_educ_gp==1 & ever_children==0) (line female_earn_pct dur if dur <=20 & couple_educ_gp==1 & ever_children==1), legend(on order(1 "NC - No Children" 2 "NC - Children" 3 "Coll - No Children" 4 "Coll-Children"))
graph export "$results\earn_pct_educ_x_children_intact.jpg", as(jpg) name("Graph") quality(90) replace
restore

********************************************************************************
* Growth curve attempts
********************************************************************************

// lol are these growth curves? (see assignment 3 and lecture 7 from Dan's class)
// also: https://stats.oarc.ucla.edu/stata/faq/linear-growth-models-xtmixed-vs-sem/
// and: https://data.princeton.edu/pop510/egm

mixed female_earn_pct dur|| id: dur // would I need to do durations in individuals in states??? (to add contextual?)
// baseline is 36.7% (constant), with each year of duration, goes down .135% (-.00135 is the coefficient)
margins, at(dur=(1(2)15)) // so is this how I graph the curve? am I allowed to make non-linear??
marginsplot

mixed female_earn_pct dur c.dur#c.dur || id: dur, covariance(unstructured) // this is curvilinear, so also probably add squared term
margins, at(dur=(1(2)15))
marginsplot

mixed female_earn_pct dur|| id: dur, cov(un) 
/* from assignment: There is also significant covariance, suggesting that, the higher the initial level of anxiety, the faster it
declines over time. This makes sense given the plot of women from question 1 – they start with higher
anxiety and see a steeper decline over time.
This is true in this as well - so college start higher and decline faster
*/


gen no_college=(couple_educ_gp==0)
gen college=(couple_educ_gp==1)
gen no_dur= no_college*dur
gen coll_dur=college*dur

mixed female_earn_pct c.dur##i.couple_educ_gp|| id: dur, cov(un) 
margins couple_educ_gp, at(dur=(1(2)19)) // so is this how I graph the curve? am I allowed to make non-linear??
marginsplot

gen dur_sq = dur * dur
mixed female_earn_pct dur_sq c.dur##i.couple_educ_gp|| id: dur, cov(un) 
margins couple_educ_gp, at(dur=(1(2)19)) // I am not 100% sure this totally worked as curvilinear? 
marginsplot

mixed female_earn_pct no_college college no_dur coll_dur, nocons || id: no_college college no_dur coll_dur, cov(ind) // okay does this work? is interaction needed, because of time scale?
mixed female_earn_pct no_college college no_dur coll_dur, nocons ||id: no_college no_dur, nocons cov(ind) ||id: college coll_dur, nocons cov(ind) var // from handout 4, p 10...if I change cov to indepdent, can't LR test

// so college start higher and decrease faster than no college. no college actually do not see sig decline over time?
// how do I graph this?? okay margins

mixed female_earn_pct dur ///
|| statefip: dur, covariance(unstructured) ///
|| id:  dur, covariance(unstructured) mle

// do i put all predictors in first level, regardless of what level measured at? I think if I put in first level, it is the difference between (have to do math) - if in all levels, it is the actual value? i don't know how it works if one predictor is one level and the other is another....
gen unpaid=(paid_leave==0)
gen paid_dur= paid_leave*dur
gen unpaid_dur=unpaid*dur

mixed female_earn_pct paid_leave unpaid paid_dur unpaid_dur, nocons ||statefip: paid_leave unpaid paid_dur unpaid_dur, cov(ind) || id: dur, cov(ind) 
mixed female_earn_pct paid_leave unpaid paid_dur unpaid_dur, nocons ||statefip: dur, cov(ind) || id: dur, cov(ind) 

mixed female_earn_pct c.dur##i.sexism_gp if couple_educ_gp==1 || statefip: c.dur##i.sexism_gp|| id: dur, cov(un) 
margins sexism_gp, at(dur=(1(2)19)) //
marginsplot

mixed female_earn_pct c.dur##i.ever_children if couple_educ_gp==1 ||statefip: dur || id: dur, cov(un) 
margins ever_children, at(dur=(1(2)19)) //
marginsplot

*********************************************************************
* Misc things
*********************************************************************

// to standardize on TIME TO DIVORCE
by id: egen rel_end_temp= max(survey_yr) if rel_end_all==9998
replace rel_end_all = rel_end_temp if rel_end_all==9998

gen transition_dur=.
replace transition_dur = survey_yr-rel_end_all
replace transition_dur = dur if transition_dur==. // should be all those intact

preserve
collapse (median) female_earn_pct, by(transition_dur ever_dissolve couple_educ_gp)

twoway (line female_earn_pct transition_dur if ever_dissolve==1 & couple_educ_gp==0 & transition_dur<=0 & transition_dur>=-15) (line female_earn_pct transition_dur if ever_dissolve==1 & couple_educ_gp==1 & transition_dur<=0 & transition_dur>=-15), legend(on order(1 "Dissolved, Non" 2 "Dissolved, College"))
graph export "$results\earn_pct_educ_x_dissolved_duration.jpg", as(jpg) name("Graph") quality(90) replace

restore


// also try to standardize on time pre and post first child??
browse id survey_yr rel_start_all rel_end_all status_all female_earn_pct children FIRST_BIRTH_YR NUM_CHILDREN_ BIRTHS_REF_ BIRTH_SPOUSE_ BIRTH_YR_ dur

gen first_birth_dur=.
replace first_birth_dur = survey_yr-FIRST_BIRTH_YR if ever_children==1
browse id survey_yr rel_start_all rel_end_all status_all female_earn_pct first_birth_dur children ever_children FIRST_BIRTH_YR dur if first_birth_dur < -1000 // eventually need to use FULL FILE (including like pre marriage) and see if I can get actual birth year when they transiton from 0 to 1, but that won't work right now, because i don't have full history

preserve
collapse (median) female_earn_pct if ever_children==1 & FIRST_BIRTH_YR!=9999, by(first_birth_dur couple_educ_gp)

twoway (line female_earn_pct first_birth_dur if couple_educ_gp==0 & first_birth_dur>=-10 & first_birth_dur<=20) (line female_earn_pct first_birth_dur if couple_educ_gp==1 & first_birth_dur>=-10 & first_birth_dur<=20), legend(on order(1 "Non" 2 "College"))
graph export "$results\earn_pct_educ_x_children_duration.jpg", as(jpg) name("Graph") quality(90) replace
restore

preserve
collapse (median) female_earn_pct if ever_children==1 & FIRST_BIRTH_YR!=9999 & ever_dissolve==0, by(first_birth_dur couple_educ_gp)

twoway (line female_earn_pct first_birth_dur if couple_educ_gp==0 & first_birth_dur>=-10 & first_birth_dur<=20) (line female_earn_pct first_birth_dur if couple_educ_gp==1 & first_birth_dur>=-10 & first_birth_dur<=20), legend(on order(1 "Non" 2 "College"))
graph export "$results\earn_pct_educ_x_children_duration_intact.jpg", as(jpg) name("Graph") quality(90) replace
restore

preserve
collapse (median) female_earn_pct if ever_children==1 & FIRST_BIRTH_YR!=9999 & ever_dissolve==1, by(first_birth_dur couple_educ_gp)

twoway (line female_earn_pct first_birth_dur if couple_educ_gp==0 & first_birth_dur>=-10 & first_birth_dur<=10) (line female_earn_pct first_birth_dur if couple_educ_gp==1 & first_birth_dur>=-10 & first_birth_dur<=10), legend(on order(1 "Non" 2 "College"))
graph export "$results\earn_pct_educ_x_children_duration)dissolve.jpg", as(jpg) name("Graph") quality(90) replace
restore