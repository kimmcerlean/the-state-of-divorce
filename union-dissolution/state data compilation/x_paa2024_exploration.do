use T:\data\ACS\acs_2000_2021.dta, clear

keep if year >=2008

gen college=0
replace college=1 if inlist(educ,10,11)