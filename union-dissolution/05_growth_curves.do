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
* ever divorce: 
sort id survey_yr
browse id survey_yr rel_start_all rel_end_all status_all dur dissolve_lag MARITAL_PAIRS_
bysort id: egen ever_dissolve=max(dissolve_lag)
sort id survey_yr
browse id survey_yr rel_start_all rel_end_all status_all dissolve_lag ever_dissolve dur first_dur MARITAL_PAIRS_
tab status_all ever_dissolve // lol this is very discordant... see IDs 2986, 2992
* first child? for now - any children - think when doing actual curves, need to add a spline (or whatever - from DP class) at TIME of child: children

// collapse by duration
gen transition_month=.
replace transition_month=panelmonth if dissolved==1
bysort SSUID from_num to_num (transition_month): replace transition_month=transition_month[1]

sort SSUID from_num to_num panelmonth
browse SSUID from_num to_num panelmonth from_earnings to_earnings female_earn_pct dissolved transition_month

gen transition_dur=.
replace transition_dur = panelmonth-transition_month
replace transition_dur = dur if transition_dur==. // should be all those intact

browse SSUID from_num to_num panelmonth from_earnings to_earnings female_earn_pct dissolved ever_dissolved transition_month transition_dur dur

preserve

collapse (median) female_earn_pct, by(transition_dur ever_dissolved couple_educ_gp)

twoway (line female_earn_pct transition_dur if ever_dissolved==1 & couple_educ_gp==0) (line female_earn_pct transition_dur if ever_dissolved==1 & couple_educ_gp==1) ///
(line female_earn_pct transition_dur if ever_dissolved==0 & couple_educ_gp==0) (line female_earn_pct transition_dur if ever_dissolved==0 & couple_educ_gp==1), legend(on order(1 "Dissolved, Non" 2 "Dissolved, College" 3 "intact, non" 4 "intact, college"))

twoway (line female_earn_pct transition_dur if ever_dissolved==1 & couple_educ_gp==0 & transition_dur<=0) (line female_earn_pct transition_dur if ever_dissolved==1 & couple_educ_gp==1 & transition_dur<=0), legend(on order(1 "Dissolved, Non" 2 "Dissolved, College"))

twoway (line female_earn_pct transition_dur if ever_dissolved==0 & couple_educ_gp==0) (line female_earn_pct transition_dur if ever_dissolved==0 & couple_educ_gp==1), legend(on order(1 "Intact, Non" 2 "Intact, College"))