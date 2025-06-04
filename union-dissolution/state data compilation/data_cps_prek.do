********************************************************************************
* Getting state pre-K enrollment across 3-4 year olds
* data_cps_prek.do
* Kim McErlean
********************************************************************************

********************************************************************************
* CREATING DATA-SET
********************************************************************************
use "$CPS/cps_schoolenrollment_1990_2022.dta", clear

// edatt = currently in school
// edattly = enrolled last year
// edgrade - 11 = nursery part-day; 12 = nursery full-day
// edgrdly
// edpupr = public or private

tab age edatt // confirm universe starts at age 3

gen prek_enrollment=0
replace prek_enrollment=1 if inlist(edgrade,11,12)

tab age prek_enrollment, m
tab age prek_enrollment if edatt==1, m
tab edgrade if inlist(age,3,4)
tab prek_enrollment edpupr

gen prek_public=.
replace prek_public=0 if prek_enrollment==1 & edpupr==11
replace prek_public=1 if prek_enrollment==1 & edpupr==10

tab year prek_enrollment if inlist(age,3,4), row // validate against this report: https://aspe.hhs.gov/reports/trends-use-early-care-education-1995-2011-descriptive-analysis-child-care-arrangements-national-0

tab year prek_public if inlist(age,3,4), row // this measure is just those enrolled
tab year edpupr if inlist(age,3,4), row // okay that report is based on all, not just those enrolled

// do I need weights? 
browse year serial hwtfinl wtfinl edsuppwt

********************************************************************************
* AGGREGATE AT YEAR + STATE LEVEL
********************************************************************************
// first create variables needed to make aggregations

gen age_3=0
replace age_3=1 if age==3

gen age_4=0
replace age_4=1 if age==4

gen age_3_4=0
replace age_3_4=1 if inlist(age,3,4)

gen age_3_enrolled=0
replace age_3_enrolled=1 if age==3 & prek_enrollment==1

gen age_4_enrolled=0
replace age_4_enrolled=1 if age==4 & prek_enrollment==1

gen age_3_4_enrolled=0
replace age_3_4_enrolled=1 if inlist(age,3,4) & prek_enrollment==1

gen age_3_public=0
replace age_3_public=1 if age==3 & prek_enrollment==1 & prek_public==1

gen age_4_public=0
replace age_4_public=1 if age==4 & prek_enrollment==1 & prek_public==1

gen age_3_4_public=0
replace age_3_4_public=1 if inlist(age,3,4) & prek_enrollment==1 & prek_public==1

// is this how to weight? I have no idea...
foreach var in age_3 age_4 age_3_4 age_3_enrolled age_4_enrolled age_3_4_enrolled age_3_public age_4_public age_3_4_public{
	gen `var'_wt = `var' * edsuppwt
}

browse age_3 age_3_enrolled edsuppwt age_3_wt age_3_enrolled_wt

******************   
* Aggregate
******************   

preserve

collapse (sum) 	age_3 age_4 age_3_4 age_3_enrolled age_4_enrolled ///
				age_3_4_enrolled age_3_public age_4_public age_3_4_public, ///
				by(year)
				
restore


preserve

collapse (sum) 	age_3 age_4 age_3_4 age_3_enrolled age_4_enrolled ///
				age_3_4_enrolled age_3_public age_4_public age_3_4_public, ///
				by(year statefip)
				
restore

preserve

collapse (sum) 	age_3_wt age_4_wt age_3_4_wt age_3_enrolled_wt age_4_enrolled_wt ///
				age_3_4_enrolled_wt age_3_public_wt age_4_public_wt age_3_4_public_wt, ///
				by(year statefip)

restore