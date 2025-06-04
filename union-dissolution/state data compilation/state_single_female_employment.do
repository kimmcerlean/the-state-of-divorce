********************************************************************************
* Employment trends for single female - do the same things that predict male BW also hinder female employment?
* state_single_female_employment.do
* Kim McErlean
********************************************************************************

use "$ACS\acs_state_2010_2018.dta", clear // from step 1

// tab age - is this file even adequate lol...
keep if sex==2  // only females
* keep if age >=25 & age <=60 // only working age
keep if age >=25 & age <=45 // reproductive age
// do I want NEVER married or divorced?
// keep if marst==6 // do never-married for now. divorced maybe have kids like a whole different calculus
*** okay, doing ALL women for now.

// education recode: college v not
recode educ (0/5=1)(6=2)(7/9=3)(10/11=4), gen(education)
label define education 1 "LTHS" 2 "HS" 3 "Some College" 4 "College"
label values education education 

gen college=0
replace college=1 if education==4

// employment. empstat is yes, no, not in LF. I want all yes as one
gen employed= 0
replace employed=1 if empstat==1

gen ft_employed=0
replace ft_employed=1 if empstat==1 & uhrswork>=35

save "$created_data\acs_single_females.dta", replace