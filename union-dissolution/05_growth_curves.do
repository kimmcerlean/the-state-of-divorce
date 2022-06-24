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

keep if cohort==2 | cohort==3 // start with contemporary marriages
keep if marriage_order_real==1 // for now, just FIRST marriage
keep if (AGE_REF_>=18 & AGE_REF_<=55) &  (AGE_SPOUSE_>=18 & AGE_SPOUSE_<=55) // working age

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

preserve
collapse (median) female_earn_pct, by(dur couple_educ_gp ever_dissolve ever_children)
restore

preserve
collapse (median) female_earn_pct, by(dur couple_educ_gp)
twoway (line female_earn_pct dur if dur <=20 & couple_educ_gp==0) (line female_earn_pct dur if dur <=20 & couple_educ_gp==1), legend(on order(1 "No College" 2 "College"))
graph export "$results\earn_pct_education.jpg", as(jpg) name("Graph") quality(90) replace
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
collapse (median) female_earn_pct if ever_dissolve==0, by(dur couple_educ_gp ever_children)
twoway (line female_earn_pct dur if dur <=20 & couple_educ_gp==0 & ever_children==0) (line female_earn_pct dur if dur <=20 & couple_educ_gp==0 & ever_children==1) (line female_earn_pct dur if dur <=20 & couple_educ_gp==1 & ever_children==0) (line female_earn_pct dur if dur <=20 & couple_educ_gp==1 & ever_children==1), legend(on order(1 "NC - No Children" 2 "NC - Children" 3 "Coll - No Children" 4 "Coll-Children"))
graph export "$results\earn_pct_educ_x_children_intact.jpg", as(jpg) name("Graph") quality(90) replace
restore


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