********************************************************************************
* Marriage trends by state and education over time
* state_hh_type_married_last_year.do
* Kim McErlean
********************************************************************************

use "$ACS\acs_1980_2019_marriages_lastyr_state.dta", clear

// calculate year of marriage for 1980 to get if married in last year
gen yrborn = year - age
replace yrmarr=yrborn+agemarr if year==1980 & agemarr!=0

browse year serial pernum age yrborn agemarr marrinyr yrmarr
replace marrinyr=2 if year==1980 & inlist(yrmarr,1979,1980)
replace marrinyr=0 if marrinyr==.

keep if marrinyr==2
drop if inlist(marst,3,4,5)

// denote who got married
gen married=0
replace married=1 if marrinyr==2

// education recode: college v not
recode educ (0/5=1)(6=2)(7/9=3)(10/11=4), gen(education)
label define education 1 "LTHS" 2 "HS" 3 "Some College" 4 "College"
label values education education 

gen college=0
replace college=1 if education==4

** indicators of hh type among married couples who married in last year
browse year serial pernum incwage incwage_sp sex
egen couple_earnings=rowtotal(incwage incwage_sp)

gen wife_earnings=.
replace wife_earnings=incwage if sex==2
replace wife_earnings=incwage_sp if sex==1

gen husband_earnings=.
replace husband_earnings=incwage if sex==1
replace husband_earnings=incwage_sp if sex==2

browse year serial pernum incwage incwage_sp sex couple_earnings wife_earnings husband_earnings

gen wife_earn_ratio = wife_earnings / couple_earnings

gen hh_earn_type=.
replace hh_earn_type = 0 if wife_earn_ratio==.
replace hh_earn_type = 1 if wife_earn_ratio==0
replace hh_earn_type = 2 if wife_earn_ratio < 0.4 & wife_earn_ratio > 0
replace hh_earn_type = 3 if wife_earn_ratio >=0.4 & wife_earn_ratio <=0.6
replace hh_earn_type = 4 if wife_earn_ratio >=0.6 & wife_earn_ratio!=.

label define hh_earn_type 0 "No Couple Earnings" 1 "Male Sole" 2 "Male Primary" 3 "Dual Earner" 4 "Female Primary"
label values hh_earn_type hh_earn_type

// Getting counts of HHs to aggregate
* not everyone is pernum 1, creating a rank
bysort year serial: egen person = rank(pernum)
browse year serial person pernum

* Count of couples
gen hh_count=0
replace hh_count=1 if person==1

gen hh_count_non=0
replace hh_count_non=1 if person==1 & college==0

gen hh_count_coll=0
replace hh_count_coll=1 if person==1 & college==1

* Count married couples: dual earning
gen married_dual_count=0
replace married_dual_count=1 if person==1 & hh_earn_type==3

gen married_dual_count_non=0
replace married_dual_count_non=1 if person==1 & hh_earn_type==3 & college==0

gen married_dual_count_coll=0
replace married_dual_count_coll=1 if person==1 & hh_earn_type==3 & college==1

* Count married couples: male BW
gen married_malebw_count=0
replace married_malebw_count=1 if person==1 & inlist(hh_earn_type,1,2)

gen married_malebw_count_non=0
replace married_malebw_count_non=1 if person==1 & inlist(hh_earn_type,1,2) & college==0

gen married_malebw_count_coll=0
replace married_malebw_count_coll=1 if person==1 & inlist(hh_earn_type,1,2) & college==1

gen married_malesole_count=0
replace married_malesole_count=1 if person==1 & hh_earn_type==1

gen married_malesole_count_non=0
replace married_malesole_count_non=1 if person==1 & hh_earn_type==1 & college==0

gen married_malesole_count_coll=0
replace married_malesole_count_coll=1 if person==1 & hh_earn_type==1 & college==1

** Collapse by state
preserve
collapse (sum) 	hh_count hh_count_non hh_count_coll married_dual_count ///
				married_dual_count_non married_dual_count_coll ///
				married_malebw_count married_malebw_count_non ///
				married_malebw_count_coll married_malesole_count ///
				married_malesole_count_non married_malesole_count_coll, ///
by(statefip region year)

**Calculating rates
gen x_married_dual = married_dual_count / hh_count
gen x_married_malebw = married_malebw_count / hh_count
gen x_married_malesole = married_malesole_count / hh_count
gen x_married_dual_non = married_dual_count_non / hh_count_non
gen x_married_malebw_non = married_malebw_count_non / hh_count_non
gen x_married_malesole_non = married_malesole_count_non / hh_count_non
gen x_married_dual_coll = married_dual_count_coll / hh_count_coll
gen x_married_malebw_coll = married_malebw_count_coll / hh_count_coll
gen x_married_malesole_coll = married_malesole_count_coll / hh_count_coll

save "$created_data\state_married_last_yr_hhtype.dta", replace

restore

********************************************************************************
* Getting individual married in last year file to test multi-level models
********************************************************************************
keep if marrinyr==2

gen dual_earn=0
replace dual_earn=1 if hh_earn_type==3

gen male_bw=0
replace male_bw=1 if hh_earn_type==1 | hh_earn_type==2

save "$created_data\acs_married_last_yr.dta", replace

********************************************************************************
* State-level analysis
********************************************************************************
use "$created_data\state_married_last_yr_hhtype.dta", clear

gen year_real=year
replace year=2007 if year==1980

twoway (line x_married_dual_coll year) (line x_married_dual_non year) if statefip<=12, by(statefip)
twoway (line x_married_dual_coll year) (line x_married_dual_non year) if statefip>12 & statefip<=23, by(statefip)
twoway (line x_married_dual_coll year) (line x_married_dual_non year) if statefip>23 & statefip<=34, by(statefip)
twoway (line x_married_dual_coll year) (line x_married_dual_non year) if statefip>35 & statefip<=45, by(statefip)
twoway (line x_married_dual_coll year) (line x_married_dual_non year) if statefip>45 & statefip<=56, by(statefip)

twoway (line x_married_malebw_non year) (line x_married_dual_non year) if statefip<=12, by(statefip)
twoway (line x_married_malebw_non year) (line x_married_dual_non year) if statefip>12 & statefip<=23, by(statefip)
twoway (line x_married_malebw_non year) (line x_married_dual_non year) if statefip>23 & statefip<=34, by(statefip)
twoway (line x_married_malebw_non year) (line x_married_dual_non year) if statefip>35 & statefip<=45, by(statefip)
twoway (line x_married_malebw_non year) (line x_married_dual_non year) if statefip>45 & statefip<=56, by(statefip)

twoway (line x_married_malebw_coll year) (line x_married_dual_coll year) if statefip<=12, by(statefip)
twoway (line x_married_malebw_coll year) (line x_married_dual_coll year) if statefip>12 & statefip<=23, by(statefip)
twoway (line x_married_malebw_coll year) (line x_married_dual_coll year) if statefip>23 & statefip<=34, by(statefip)
twoway (line x_married_malebw_coll year) (line x_married_dual_coll year) if statefip>35 & statefip<=45, by(statefip)
twoway (line x_married_malebw_coll year) (line x_married_dual_coll year) if statefip>45 & statefip<=56, by(statefip)


twoway (bar x_married_dual_coll statefip, fcolor(%50)) (bar x_married_dual_non statefip, fcolor(%30)) if year==2008, xlabel(1(1)56, valuelabel labsize(tiny) angle(45))
twoway (line x_married_dual_coll statefip) (line x_married_dual_non statefip) if year==2008, xlabel(1(1)56, valuelabel labsize(tiny) angle(45))