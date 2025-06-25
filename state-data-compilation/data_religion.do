********************************************************************************
* The file takes the decennial data from the ARDA and interpolates
* the missing years

* Data came from: 
* https://www.thearda.com/us-religion/maps/us-state-maps?color=orange&m1=1_1_100_1980
* https://thearda.com/data-archive/browse-category?cid=B-B&sort_by=Title
********************************************************************************

/*
// Only need to do this once

clear
import excel "G:\Other computers\My Laptop\Documents\Dissertation\Policy data\Religion\ARDA_From Website.xlsx", sheet("to interpolate") firstrow

save "$temp/state_religion.dta", replace
*/

use "$temp/state_religion.dta", clear

// drop if year==1980. confirmed that 1980 or not doesn't matter

// want to use these as final names
foreach var in evang_rate evang_num catholic_rate catholic_num lds_rate lds_num evang_lds_rate evang_lds_num relig_rate relig_num{
	rename `var' `var'_orig
}

// test with just one variable
sort state_fips year
bysort state_fips: ipolate evang_rate_orig year, gen(evang_rate)
bysort state_fips: ipolate evang_rate_orig year, gen(evang_rate_e) epolate // think I need this to get 2021 (ipolate, in theory, only does the IN-between years)

sort state_fips year
browse state_fips year evang_rate_orig evang_rate evang_rate_e

drop evang_rate evang_rate_e

// okay, yes, use epolate so we have 2021
foreach var in evang_rate evang_num catholic_rate catholic_num lds_rate lds_num evang_lds_rate evang_lds_num relig_rate relig_num{
	bysort state_fips: ipolate `var'_orig year, gen(`var') epolate 
}

// compare the interpolated rates to rates calculated from the population numbers
foreach var in evang catholic lds evang_lds relig{
	gen `var'_rate_calc = `var'_num / population
}

pwcorr evang_rate evang_rate_calc if year!=1980
pwcorr catholic_rate catholic_rate_calc if year!=1980 // okay so catholic are the least good?
pwcorr lds_rate lds_rate_calc
pwcorr evang_lds_rate evang_lds_rate_calc
pwcorr relig_rate relig_rate_calc // and because of that, these aren't that good?

browse state_fips year evang_rate evang_rate_calc  catholic_rate catholic_rate_calc relig_rate relig_rate_calc

save "$created_data/state_religion.dta", replace

export excel using "$created_data/state_religion", firstrow(variables) replace