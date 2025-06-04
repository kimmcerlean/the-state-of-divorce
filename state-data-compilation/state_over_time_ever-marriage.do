********************************************************************************
* Marriage trends by state and education over time
* state_over_time_ever-marriage.do
* Kim McErlean
********************************************************************************

use "$ACS\acs_1980_2019_marriage_state", clear

// denote who is ever married
gen ever_married=0
replace ever_married=1 if inrange(marst,1,5)

// education recode: college v not
recode educ (0/5=1)(6=2)(7/8=3)(10/11=4), gen(education)
label define education 1 "LTHS" 2 "HS" 3 "Some College" 4 "College"
label values education education 

gen college=0
replace college=1 if education==4

// flags to get counts for when I aggregate.
*total
gen total=1

*total college
* use college variable

*total no college
gen no_college=0
replace no_college=1 if college==0

*total ever-married
* use ever_married

*total ever-married college
gen ever_married_college=0
replace ever_married_college=1 if college==1 & ever_married==1

*total ever-married no college
gen ever_married_non=0
replace ever_married_non=1 if college==0 & ever_married==1

*women
gen women=0
replace women=1 if sex==2

*women college
gen women_college=0
replace women_college=1 if sex==2 & college==1

*women no college
gen women_non=0
replace women_non=1 if sex==2 & college==0

*women ever-married
gen women_ever_married=0
replace women_ever_married=1 if sex==2 & ever_married==1

*women ever-married college
gen women_college_ever_married=0
replace women_college_ever_married=1 if sex==2 & college==1 & ever_married==1

*women ever-married no college
gen women_non_ever_married=0
replace women_non_ever_married=1 if sex==2 & college==0 & ever_married==1

*men
gen men=0
replace men=1 if sex==1

*men college
gen men_college=0
replace men_college=1 if sex==1 & college==1

*men no college
gen men_non=0
replace men_non=1 if sex==1 & college==0

*men ever-married
gen men_ever_married=0
replace men_ever_married=1 if sex==1 & ever_married==1

*men ever-married college
gen men_college_ever_married=0
replace men_college_ever_married=1 if sex==1 & college==1 & ever_married==1

*men ever-married no college
gen men_non_ever_married=0
replace men_non_ever_married=1 if sex==1 & college==0 & ever_married==1

// aggregate

collapse (sum) 	total college no_college ever_married ever_married_college 			///
				ever_married_non women women_college women_non women_ever_married 	///
				women_college_ever_married women_non_ever_married men men_college 	///
				men_non men_ever_married men_college_ever_married					///
				men_non_ever_married,												///
by(statefip year)

// metrics
gen rt_total=			ever_married/total
gen rt_college=			ever_married_college / college
gen rt_non=				ever_married_non/no_college
gen rt_women= 			women_ever_married/women
gen rt_women_college =	women_college_ever_married/women_college
gen rt_women_non=		women_non_ever_married/women_non
gen rt_men			=	men_ever_married/men
gen rt_men_college = 	men_college_ever_married/men_college
gen rt_men_non = 		men_non_ever_married / men_non

save "$created_data\state_marriage_history.dta", replace

gen women_college_non_ratio = rt_women_non / rt_women_college

twoway (line rt_women_college year) (line rt_women_non year) if statefip==48
twoway (line rt_women_college year) (line rt_women_non year), by(statefip) ylabel(.6(.1)1)
twoway (line rt_women_college year) (line rt_women_non year), yscale(range(.6 1))

twoway (line rt_women_college year) (line rt_women_non year) if statefip<=12, by(statefip)
twoway (line rt_women_college year) (line rt_women_non year) if statefip>12 & statefip<=23, by(statefip)
twoway (line rt_women_college year) (line rt_women_non year) if statefip>23 & statefip<=34, by(statefip)
twoway (line rt_women_college year) (line rt_women_non year) if statefip>35 & statefip<=45, by(statefip)
twoway (line rt_women_college year) (line rt_women_non year) if statefip>45 & statefip<=56, by(statefip)

twoway (line women_college_non_ratio year), by(statefip)
twoway (line women_college_non_ratio year) if statefip<=12 & year>=2000, by(statefip)
twoway (line women_college_non_ratio year) if statefip>12 & statefip<=23 & year>=2000, by(statefip)
twoway (line women_college_non_ratio year) if statefip>23 & statefip<=34 & year>=2000, by(statefip)
twoway (line women_college_non_ratio year) if statefip>35 & statefip<=45 & year>=2000, by(statefip)
twoway (line women_college_non_ratio year) if statefip>45 & statefip<=56 & year>=2000, by(statefip)

twoway (line rt_women year), by(statefip)
twoway (line rt_women year) if statefip<=12 & year>=2000, by(statefip)
twoway (line rt_women year) if statefip>12 & statefip<=23 & year>=2000, by(statefip)
twoway (line rt_women year) if statefip>23 & statefip<=34 & year>=2000, by(statefip)
twoway (line rt_women year) if statefip>35 & statefip<=45 & year>=2000, by(statefip)
twoway (line rt_women year) if statefip>45 & statefip<=56 & year>=2000, by(statefip)
