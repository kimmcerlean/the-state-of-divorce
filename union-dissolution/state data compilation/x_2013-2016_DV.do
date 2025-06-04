********************************************************************************
* State view of household types
* create_sample.do
* Kim McErlean
********************************************************************************

********************************************************************************
* DESCRIPTION
********************************************************************************
* This file sets up the directories and specifies file locations for the project
* It also checks for any required packages

********************************************************************************
* CREATE SAMPLE
********************************************************************************

use "$ACS\spouse_linked_2013_2016.dta", clear

* First restrict to only married couple households (hhtype==1)
keep if hhtype==1

*Then, only want the married couple in the HH (aka remove kids and other members)
keep if marst==1

*Only want main families(aka head of HH + spouse), not subfamilies
keep if subfam==0

*Drop same-sex couples
drop if sex==sex_sp

*Now have two records per couple. Decide = keep either the main person as all either female or male OR keep the head of household, and it's okay if gender mixed?
tab sex if relate==1
// I wonder if sex of householder corresponds to who is primary breadwinner?! something to explore
// decided if easier if all main and all _sp variables correspond to the same sex
keep if sex==1 // keeping males as reference group (more males than females are HHer)

* Making indicator of type of household economic structure
browse hhincome inctot ftotinc incwage incearn inctot_sp ftotinc_sp incwage_sp incearn_sp

gen couple_earnings = incearn + incearn_sp
gen female_earn_ratio = incearn_sp / couple_earnings

gen hh_earn_type=.
replace hh_earn_type = 0 if female_earn_ratio==.
replace hh_earn_type = 1 if female_earn_ratio==0
replace hh_earn_type = 2 if female_earn_ratio < 0.4 & female_earn_ratio > 0
replace hh_earn_type = 3 if female_earn_ratio >=0.4 & female_earn_ratio <=0.6
replace hh_earn_type = 4 if female_earn_ratio >=0.6 & female_earn_ratio!=.

label define hh_earn_type 0 "No Couple Earnings" 1 "Male Sole" 2 "Male Primary" 3 "Dual Earner" 4 "Female Primary"
label values hh_earn_type hh_earn_type

browse hhincome incearn incearn_sp couple_earnings female_earn_ratio hh_earn_type

recode educ (0/8=0) (10/11=1), gen(college)
recode educ_sp (0/8=0) (10/11=1), gen(college_sp)

gen couple_educ=.
replace couple_educ = 0 if college==0 & college_sp==0
replace couple_educ = 1 if college==1 | college_sp==1
label define couple_educ 0 "Neither College" 1 "At Least One College"
label values couple_educ couple_educ

browse college college_sp couple_educ

save "$created_data\married_households_2013_2016.dta", replace

** Getting state-level hh distribution
browse stateicp hh_earn_type couple_educ

tab hh_earn_type, gen(hh)

preserve

collapse 	(mean) 	hh1 hh2 hh3 hh4 hh5										///
			(sum)   hh1_ct=hh1 hh2_ct=hh2 hh3_ct=hh3 hh4_ct=hh4 hh5_ct=hh5,	/// 
			by(year statefip couple_educ)
			
save "$created_data\state_hh_distribution.dta", replace
export excel using "$\results\state_hh_distribution.xls", firstrow(variables)

restore