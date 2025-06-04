********************************************************************************
* Getting divorcce rates by state and education
* data_acs_divorce.do
* Kim McErlean
********************************************************************************

********************************************************************************
* CREATING DATA-SET
********************************************************************************
use "$ACS\acs_divorce_state_5yr.dta", clear

recode educ (0/5=1)(6=2)(7/8=3)(10/11=4), gen(education)
label define education 1 "LTHS" 2 "HS" 3 "Some College" 4 "College"
label values education education

gen college=0
replace college=1 if education==4

drop if inlist(divinyr,0,8)

// variables to prep for summing
gen total=1
gen total_coll=0
replace total_coll=1 if college==1

gen total_no=0
replace total_no=1 if college==0

gen divorced=0
replace divorced=1 if divinyr==2

gen divorced_coll=0
replace divorced_coll=1 if divinyr==2 & college==1

gen divorced_no=0
replace divorced_no=1 if divinyr==2 & college==0


preserve

collapse (sum) total total_coll total_no divorced divorced_coll divorced_no, by(statefip)

save "$created_data\acs_divorce_states.dta", replace

restore