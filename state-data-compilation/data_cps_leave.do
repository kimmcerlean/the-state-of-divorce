********************************************************************************
* Getting state maternity leave usage
* data_cps_leave.do
* Kim McErlean
********************************************************************************

********************************************************************************
* CREATING DATA-SET
********************************************************************************

* decide - do leave out of ALL WORKERS OR just those who had a birth recently? 
* OR just parents?
* Can use those with children under 5? (That is a variable) - or age of youngest child (also a variable)
* The latter sample might be too small. do all.

use "$CPS\leave_state_2010_2019.dta", clear

// only keep those likely to be in work and in child-bearing years
keep if age >=20 & age<=45

recode empstat (1=0)(10/12=1)(21/22=2)(32/36=0), gen(employ)
label define employ 0 "Not in LF" 1 "Employed" 2 "Unemployed"
label values employ employ

gen paternal_leave=0
replace paternal_leave=1 if whyabsnt==9

gen childcare_prob=0
replace childcare_prob=1 if whyabsnt==7

// tab paternal_leave if absent==3

// parent variables
browse year serial pernum sex age nchlt5 yngch
gen children_under5=0
replace children_under5=1 if nchlt5 >=1

gen children_under2=0
replace children_under2=1 if yngch >=0 & yngch <=2

browse year serial pernum empstat absent whyabsnt payifabs earnweek
browse year serial pernum empstat absent whyabsnt payifabs earnweek if paternal_leave==1


// let's do women, men, total - then split by all versus those with children under 5 (to start). need a. those empoyed, b those employedwho took leave, then employed and a parent, then employed and a parent and then took leave
gen count=1

* Employment
gen employed_count=0
replace employed_count=1 if employ==1
gen women_employed=0
replace women_employed=1 if employ==1 & sex==2
gen men_employed=0
replace men_employed=1 if employ==1 & sex==1

* Employed and parent
gen employed_count_par=0
replace employed_count_par=1 if employ==1 & children_under5==1
gen women_employed_par=0
replace women_employed_par=1 if employ==1 & sex==2 & children_under5==1
gen men_employed_par=0
replace men_employed_par=1 if employ==1 & sex==1 & children_under5==1

* Employed and took leave
gen leave_count=0
replace leave_count=1 if paternal_leave==1
gen women_leave=0
replace women_leave=1 if paternal_leave==1 & sex==2
gen men_leave=0
replace men_leave=1 if paternal_leave==1 & sex==1

* Employed and took leave and paid
gen leave_count_paid=0
replace leave_count_paid=1 if paternal_leave==1 & payifabs==2
gen women_leave_paid=0
replace women_leave_paid=1 if paternal_leave==1 & sex==2 & payifabs==2
gen men_leave_paid=0
replace men_leave_paid=1 if paternal_leave==1 & sex==1 & payifabs==2
 
* Employed + parent + took leave
gen leave_count_par=0
replace leave_count_par=1 if paternal_leave==1 & children_under5==1
gen women_leave_par=0
replace women_leave_par=1 if paternal_leave==1 & sex==2 & children_under5==1
gen men_leave_par=0
replace men_leave_par=1 if paternal_leave==1 & sex==1 & children_under5==1

* Employed + parent + took leave and paid
gen leave_count_par_paid=0
replace leave_count_par_paid=1 if paternal_leave==1 & children_under5==1 & payifabs==2
gen women_leave_par_paid=0
replace women_leave_par_paid=1 if paternal_leave==1 & sex==2 & children_under5==1 & payifabs==2
gen men_leave_par_paid=0
replace men_leave_par_paid=1 if paternal_leave==1 & sex==1 & children_under5==1 & payifabs==2

* Employed + parent + had childcare issues
gen prob_count_par=0
replace prob_count_par=1 if childcare_prob==1 & children_under5==1
gen women_prob_par=0
replace women_prob_par=1 if childcare_prob==1 & sex==2 & children_under5==1
gen men_prob_par=0
replace men_prob_par=1 if childcare_prob==1 & sex==1 & children_under5==1

********************************************************************************
* AGGREGATE BY STATE AND YEAR
********************************************************************************
preserve

collapse 	(sum)  count employed_count women_employed men_employed employed_count_par 		///
					women_employed_par men_employed_par leave_count women_leave 			///
					men_leave leave_count_par women_leave_par men_leave_par children_under5	///
					leave_count_paid women_leave_paid men_leave_paid						///
					leave_count_par_paid women_leave_par_paid men_leave_par_paid			///
					prob_count_par women_prob_par men_prob_par,					 			///
			by(year statefip region)
			
**Calculated metrics
gen x_all_leave = leave_count / employed_count
gen x_women_leave = women_leave / women_employed
gen x_men_leave = men_leave / men_employed
gen x_all_leave_par = leave_count_par / employed_count_par
gen x_women_leave_par = women_leave_par / women_employed_par
gen x_men_leave_par= men_leave_par / men_employed_par
gen x_all_leave_par_paid = leave_count_par_paid / employed_count_par
gen x_women_leave_par_paid = women_leave_par_paid / women_employed_par
gen x_men_leave_par_paid= men_leave_par_paid / men_employed_par
gen x_all_ratio_paid = leave_count_par_paid / leave_count_par
gen x_women_ratio_paid = women_leave_par_paid / women_leave_par
gen x_men_ratio_paid = men_leave_par_paid / men_leave_par
gen x_all_prob = prob_count_par / employed_count_par
gen x_women_prob = women_prob_par / women_employed_par
gen x_men_prob = men_prob_par / men_employed_par

browse year statefip x_*

save "$created_data\2010_2019_state_leave_data.dta", replace
