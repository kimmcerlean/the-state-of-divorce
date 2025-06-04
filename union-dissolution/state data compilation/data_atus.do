********************************************************************************
* Combining licensing surveys
* data_licensing.do
* Kim McErlean
********************************************************************************

********************************************************************************
* CREATING MERGED DATA-SET
********************************************************************************

/*
ATUS respondents are interviewed only one time about how they spent their time on the previous day, where they were, and whom they were with.
Each of the ATUS microdata files contains useful information, but to produce most estimates, the files must be linked. All of the microdata files contain the variable TUCASEID, which is the ATUS identification number. TUACTIVITY_N (the activity line number) and TULINENO (the person line number) are two additional linking variables that can be used in conjunction with TUCASEID.
*/

use "$ATUS\atusresp-0319\atusresp_0319.dta", clear // respondent - There is one record for each ATUS respondent. id vars: tucaseid tulineno

keep tucaseid tulineno trhhchild tehruslt tehrusl1 trthh trsppres tufnwgtp tuyear tudiarydate tudiaryday
keep if tuyear>=2010 & tuyear<=2018
save "$temp/atus_resp_tolink.dta", replace

use "$ATUS\atussum-0319\atussum_0319.dta", clear // activity summary - There is one record for each ATUS respondent. id vars: tucaseid

keep  tucaseid tuyear tesex teage tudiaryday t020101-t029999 t030101-t030112 t030199 peeduca tufnwgtp
keep if tuyear>=2010 & tuyear<=2018

egen all_household=rowtotal(t020101-t029999)
egen all_housework=rowtotal(t020101-t020199)
egen all_childcare=rowtotal(t030101-t030199)

save "$temp/atus_sum_tolink.dta", replace

use "$ATUS\atuscps-0319\atuscps_0319.dta", clear // cps for characteristics. id vars: tucaseid tulineno

keep tucaseid tulineno hryear4 ptdtrace gestfips 
// keep if hryear4>=2010 & hryear4<=2018 -- keep all years because not sure if match

gen tuyear=hryear4

save "$temp/atus_cps_tolink.dta", replace

***Merge files
use "$temp/atus_resp_tolink.dta", clear
merge 1:1 tucaseid tuyear using "$temp/atus_sum_tolink.dta", keepusing(tesex teage peeduca all_household all_housework all_childcare)
drop _merge
merge 1:1 tucaseid tulineno using "$temp/atus_cps_tolink.dta", keepusing (ptdtrace gestfips hryear4)
// a lot of people are missing in CPS file...I wonder if year doesn't match... does match better when no use year..okay yes
drop if _merge==2 // just in CPS file, not in ATUS (didn't have to respond to ATUS)

*** create variables needed to aggregate - multiply time indicator by weight
// recode -1s to missing // wait okay missing or zero - bc there are the two ways to calculate - percent for total population or percent for just those who participate in activity; I think I want latter?
recode tehruslt (-4/-1=0)

//variables
gen household_wgt = all_household*tufnwgtp
gen housework_wgt = all_housework*tufnwgtp
gen childcare_wgt = all_childcare*tufnwgtp
gen trthh_wgt = trthh*tufnwgtp
gen work_wgt = tehruslt*tufnwgtp

gen household_valid = 1 if all_household > 0
gen housework_valid = 1 if all_housework > 0
gen childcare_valid = 1 if all_childcare > 0
gen trthh_valid = 1 if trthh > 0
gen work_valid = 1 if tehruslt > 0

foreach var in household_valid housework_valid childcare_valid trthh_valid work_valid{
	replace `var'=0 if `var'==.
}

// add label to denote weekend v weekday
fre tudiaryday
gen weekday=0
replace weekday=1 if inrange(tudiaryday,2,6)

tabstat all_household, by(weekday) // lower weekdays
tabstat all_housework, by(weekday) // lower weekdays
tabstat all_childcare, by(weekday) // very similar both
tabstat trthh, by(weekday) // lower weekdays
tabstat tehruslt, by(weekday) // this is for prior week and is in hours

tabstat all_household if household_valid==1, by(weekday) // lower weekdays
tabstat all_housework if housework_valid==1, by(weekday) // lower weekdays
tabstat all_childcare if childcare_valid==1, by(weekday) // lower weekdays
tabstat trthh if trthh_valid==1, by(weekday) // MUCH lower weekdays
tabstat tehruslt if work_valid==1, by(weekday) // duh this is for prior week and is in hours

foreach var in household_wgt housework_wgt childcare_wgt trthh_wgt work_wgt tufnwgtp tehruslt{
	gen `var'_male = `var' if tesex==1
	gen `var'_fem = `var' if tesex==2
}

// will replacing 0s as missing help in aggregation?? my struggle is there is 1 weight variable, but do I need to make a weight for each category? because also only goes in denominator if valid, but denominator will be different across variables - so create a new weight variable

local identifier "household housework childcare trthh work"
forvalues i=1/5{
	local var: word `i' of `identifier'
	gen `var'_wgt_male_valid = `var'_wgt_male if `var'_valid==1
	gen `var'_wgt_fem_valid = `var'_wgt_fem if `var'_valid==1
	gen calc_wgt_`var'_male = tufnwgtp if `var'_valid==1 & tesex==1
	gen calc_wgt_`var'_fem = tufnwgtp if `var'_valid==1 & tesex==2
}

browse tesex household_wgt household_wgt_male household_wgt_fem household_valid household_wgt_male_valid household_wgt_fem_valid tufnwgtp calc_wgt_household_male calc_wgt_household_fem

save "$created_data/atus_individual_level.dta", replace

********************************************************************************
* AGGREGATE TO STATE AND YEAR
********************************************************************************
preserve
// need to split by state, year, weekday v. weekend, male v. female

collapse ///
	(sum) household_wgt housework_wgt childcare_wgt trthh_wgt tufnwgtp ///
	household_wgt_male housework_wgt_male childcare_wgt_male trthh_wgt_male  tufnwgtp_male ///
	household_wgt_fem housework_wgt_fem childcare_wgt_fem trthh_wgt_fem tufnwgtp_fem ///
	household_wgt_male_valid housework_wgt_male_valid childcare_wgt_male_valid trthh_wgt_male_valid work_wgt_male_valid ///
	calc_wgt_household_male calc_wgt_housework_male calc_wgt_childcare_male calc_wgt_trthh_male calc_wgt_work_male ///
	household_wgt_fem_valid housework_wgt_fem_valid childcare_wgt_fem_valid trthh_wgt_fem_valid work_wgt_fem_valid ///
	calc_wgt_household_fem calc_wgt_housework_fem calc_wgt_childcare_fem calc_wgt_trthh_fem calc_wgt_work_fem ///
	(mean) work_wgt work_wgt_male work_wgt_fem tehruslt tehruslt_male tehruslt_fem, /// 
by(gestfips tuyear)

local identifier "household housework childcare trthh"

forvalues i=1/4{
	local var: word `i' of `identifier'
	gen `var'_hrs = (`var'_wgt / tufnwgtp) / 60
	gen `var'_hrs_male = (`var'_wgt_male / tufnwgtp) / 60
	gen `var'_hrs_fem = (`var'_wgt_fem / tufnwgtp) / 60
	gen `var'_hrs_male_valid = (`var'_wgt_male_valid / calc_wgt_`var'_male) / 60
	gen `var'_hrs_fem_valid = (`var'_wgt_fem_valid / calc_wgt_`var'_fem) / 60
}

save "$temp/atus_full_state_data.dta", replace

keep gestfips tuyear household_hrs_male_valid household_hrs_fem_valid housework_hrs_male_valid housework_hrs_fem_valid childcare_hrs_male_valid childcare_hrs_fem_valid trthh_hrs_male_valid trthh_hrs_fem_valid

// rename to match census
rename gestfips statefip
rename tuyear year

save "$created_data/atus_state_lookup.dta", replace

/*
The simplest way to generate an estimate about time use on an average day involves using the ATUS Activity Summary file. 
Those generating estimates using the module files in addition to the basic or additional ATUS files should use the module weights. For more information about how to calculate estimates, see Chapter 7 and Appendix J of the ATUS User's Guide (PDF).
*/


/* Measures - p 39 of user guide
Average hours per day. Tj , the average number of hours per day spent by a given
population engaging in activity j, is given by
fwgti*Tij / fwgt
where Tij is the amount of time spent in activity j by respondent i, and
fwgti is the final weight for respondent i. 
-- are activities in minutes or hours?

Average hours per day of participants. P Tj , the average number of hours spent per
day engaged in activity j by people who participated in that activity on that day, is
given by
fwgti*Tij*I / fwgt*I
where Tij is the amount of time spent in activity j by respondent i,
fwgti is the final weight for respondent i, and
Iij is an indicator that equals 1 if respondent i participated in activity j during the
reference day and 0 otherwise. 

okay yes in minutes, divide by 60 at end to get hours (user guide p40-41)
*/