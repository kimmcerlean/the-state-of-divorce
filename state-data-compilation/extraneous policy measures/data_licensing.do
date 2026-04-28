********************************************************************************
* Combining licensing survey files to get what I need
* data_licensing.do
* Kim McErlean
********************************************************************************

********************************************************************************
* CREATING MERGED DATA-SET
********************************************************************************
// figure out how to duplicate like I did GSS

use "$licensing/licensing_2011_all_state/ICPSR_34550/DS0001/34550-0001-Data.dta", clear // 2011
gen year=2011
egen total_centers=rowtotal(Q0004_0001 Q0004_0002 Q0004_0003 Q0004_0004)
egen total_capacity=rowtotal(Q0005_0001 Q0005_0002 Q0005_0003 Q0005_0004)

keep Q0001_0006 year total_centers total_capacity
rename Q0001_0006 STATE
save "$temp/licensing_2011.dta", replace

use "$licensing/licensing_2014_all_state/ICPSR_37026/DS0001/37026-0001-Data.dta", clear // 2014
gen year=2014
egen total_centers=rowtotal(C_NUMPROG14 F_NUMPROG14 LF_NUMPROG14 OTH_NUMPROG14)
egen total_capacity=rowtotal(C_CAPACITY14 F_CAPACITY14 LF_CAPACITY14 OTH_CAPACITY14)

keep STATE year total_centers total_capacity
save "$temp/licensing_2014.dta", replace

use "$licensing/licensing_2017_all_state/ICPSR_37700/DS0001/37700-0001-Data.dta", clear // 2017
gen year=2017
egen total_centers=rowtotal(C_NUMPROG17 F_NUMPROG17 LF_NUMPROG17 OTH_NUMPROG17)
egen total_capacity=rowtotal(C_CAPACITY17 F_CAPACITY17 LF_CAPACITY17 OTH_CAPACITY17)

keep STATE year total_centers total_capacity
save "$temp/licensing_2017.dta", replace

use "$temp/licensing_2011.dta", clear
append using "$temp/licensing_2014.dta"
append using "$temp/licensing_2017.dta"
merge m:1 STATE using "$temp/state_fip.dta", keepusing(statefip)
drop if _merge==2
drop _merge
save "$temp/licensing_incomplete.dta", replace

expand 2, gen(dupindicator)
recode year (2011=2010)(2014=2013)(2017=2018) if dupindicator==1
drop dupindicator
expand 2 if inlist(year,2014,2017), gen(dupindicator)
recode year (2014=2012)(2017=2016) if dupindicator==1
drop dupindicator
expand 2 if year==2017, gen(dupindicator)
recode year (2017=2015) if dupindicator==1
drop dupindicator

merge 1:1 year statefip using "$created_data\2010_2018_state_child_counts.dta", keepusing(count_all employed_all)
drop if _merge==2
drop _merge

gen childcare_gap = total_capacity / employed_all

save "$created_data/state_licensing.dta", replace

// save "$temp/state_fip.dta", replace
// save "T:/Research Projects/State data/data_tmp/state_fip.dta", replace