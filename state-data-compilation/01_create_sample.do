********************************************************************************
* State view of household types
* create_sample.do
* Kim McErlean
********************************************************************************

********************************************************************************
* CREATE VARIABLES NEEDED TO AGGREGATE
********************************************************************************

use "$ACS\acs_state_2010_2018.dta", clear
// one challenge - this file has multiple records per HH right?! so make sure all counts are DEDUPLICATED  - maybe turn file into wide by person? feel like I did this somewhere
// okay but then for like # of women or # of men - need individual records. so it's just like counts of households I need to sort out... okay base it on pernum 1 - give that person a flag

recode educ (0/5=1)(6=2)(7/8=3)(10/11=4), gen(education)
label define education 1 "LTHS" 2 "HS" 3 "Some College" 4 "College"
label values education education 

gen college=0
replace college=1 if education==4

// flags needed
* Count of households
browse year serial pernum
gen hh_count=0
replace hh_count=1 if pernum==1 // to count hhs, will flag pernum 1 only

gen hh_count_non=0
replace hh_count_non=1 if pernum==1 & college==0

	//validate
	tab hh_count
	unique year serial
	
* Count of households in poverty
gen hh_pov=0
replace hh_pov=1 if pernum==1 & poverty <100

* Count of female headed HHs
gen female_hh_count=0
replace female_hh_count=1 if inlist(hhtype,3,6,7) & pernum==1

* Count of female headed HHs in poverty
gen female_hh_pov=0
replace female_hh_pov=1 if inlist(hhtype,3,6,7) & pernum==1 & poverty<100

* Get income for just households with children to use to match to child care
gen hhincome_children=.
replace hhincome_children=hhincome if nchild>0 & pernum==1

* Woman age 25-64
gen woman_25_64=0
replace woman_25_64=1 if sex==2 & age>=25 & age <65

* Woman age 25-64: in labor force
gen woman_25_64_lf=0
replace woman_25_64_lf=1 if sex==2 & age>=25 & age <65 & labforce==2

* Woman age 25-64: employed
gen woman_25_64_emp=0
replace woman_25_64_emp=1 if sex==2 & age>=25 & age <65 & labforce==2 & empstat==1

* Woman age 25-64: employed part-time
gen woman_emp_pt=0
replace woman_emp_pt=1 if sex==2 & age>=25 & age <65 & labforce==2 & empstat==1 & uhrswork<35

* College educated Woman age 25-64
gen college_woman_25_64=0
replace college_woman_25_64=1 if sex==2 & age>=25 & age <65 & college==1

* College educated Woman age 25-64: in labor force
gen college_woman_25_64_lf=0
replace college_woman_25_64_lf=1 if sex==2 & age>=25 & age <65 & labforce==2 & college==1

* College educated Woman age 25-64: employed
gen college_woman_25_64_emp=0
replace college_woman_25_64_emp=1 if sex==2 & age>=25 & age <65 & labforce==2 & empstat==1 & college==1

* Non-college educated Woman age 25-64
gen non_woman_25_64=0
replace non_woman_25_64=1 if sex==2 & age>=25 & age <65 & college==0

* Non-college educated Woman age 25-64: in labor force
gen non_woman_25_64_lf=0
replace non_woman_25_64_lf=1 if sex==2 & age>=25 & age <65 & labforce==2 & college==0

* Non-college educated Woman age 25-64: employed
gen non_woman_25_64_emp=0
replace non_woman_25_64_emp=1 if sex==2 & age>=25 & age <65 & labforce==2 & empstat==1 & college==0

* Man age 25-64
gen man_25_64=0
replace man_25_64=1 if sex==1 & age>=25 & age <65

* Man age 25-64: in labor force
gen man_25_64_lf=0
replace man_25_64_lf=1 if sex==1 & age>=25 & age <65 & labforce==2

* Man age 25-64: employed
gen man_25_64_emp=0
replace man_25_64_emp=1 if sex==1 & age>=25 & age <65 & labforce==2 & empstat==1

* All age 25-64: employed
gen all_emp=0
replace all_emp=1 if age>=25 & age <65 & labforce==2 & empstat==1

* All age 25-64: part-time employed
gen all_emp_pt=0
replace all_emp_pt=1 if age>=25 & age <65 & labforce==2 & empstat==1 & uhrswork<35

* Woman CEOs
gen woman_ceo=0
replace woman_ceo=1 if sex==2 & age>=25 & age <65 & occ==0010

* Woman in management
gen woman_manage=0
replace woman_manage=1 if sex==2 & age>=25 & age <65 & inrange(occ,0010,0440)

// need to get outcome for JUST less educated couples; currently aggregated by all
* Count of married couples (hhtype)
gen married_hh_count=0
replace married_hh_count=1 if hhtype==1 & pernum==1

gen married_hh_count_non=0
replace married_hh_count_non=1 if hhtype==1 & pernum==1 & college==0

gen married_hh_count_coll=0
replace married_hh_count_coll=1 if hhtype==1 & pernum==1 & college==1

gen married_ly_count=0
replace married_ly_count=1 if marrinyr==2 & pernum==1

gen married_ly_count_non=0
replace married_ly_count_non=1 if marrinyr==2 & pernum==1 & college==0

gen married_ly_count_coll=0
replace married_ly_count_coll=1 if marrinyr==2 & pernum==1 & college==1

* Count married couples: dual earning
browse year serial pernum hhtype marst incwage sex relate
bysort year serial: egen couple_earnings = total(incwage) if relate==1 | relate==2
gen wife_earnings=.
replace wife_earnings=incwage if marst==1 & hhtype==1 & sex==2 & inlist(relate,1,2)
replace wife_earnings=incwage_sp if marst==1 & hhtype==1 & sex==1 & inlist(relate,1,2)

gen husband_earnings=.
replace husband_earnings=incwage if marst==1 & hhtype==1 & sex==1 & inlist(relate,1,2)
replace husband_earnings=incwage_sp if marst==1 & hhtype==1 & sex==2 & inlist(relate,1,2)

browse year serial pernum hhtype marst incwage sex relate couple_earnings wife_earnings husband_earnings

gen wife_earn_ratio = wife_earnings / couple_earnings

gen hh_earn_type=.
replace hh_earn_type = 0 if wife_earn_ratio==.
replace hh_earn_type = 1 if wife_earn_ratio==0
replace hh_earn_type = 2 if wife_earn_ratio < 0.4 & wife_earn_ratio > 0
replace hh_earn_type = 3 if wife_earn_ratio >=0.4 & wife_earn_ratio <=0.6
replace hh_earn_type = 4 if wife_earn_ratio >=0.6 & wife_earn_ratio!=.

label define hh_earn_type 0 "No Couple Earnings" 1 "Male Sole" 2 "Male Primary" 3 "Dual Earner" 4 "Female Primary"
label values hh_earn_type hh_earn_type

gen married_dual_count=0
replace married_dual_count=1 if hhtype==1 & pernum==1 & hh_earn_type==3

gen married_dual_count_ly=0
replace married_dual_count_ly=1 if pernum==1 & hh_earn_type==3 & marrinyr==2

gen married_dual_count_non=0
replace married_dual_count_non=1 if hhtype==1 & pernum==1 & hh_earn_type==3 & college==0

gen married_dual_count_coll=0
replace married_dual_count_coll=1 if hhtype==1 & pernum==1 & hh_earn_type==3 & college==1

gen married_dual_count_non_ly=0
replace married_dual_count_non_ly=1 if pernum==1 & hh_earn_type==3 & college==0 & marrinyr==2

gen married_dual_count_coll_ly=0
replace married_dual_count_coll_ly=1 if pernum==1 & hh_earn_type==3 & college==1 & marrinyr==2

* Count married couples: male BW
gen married_malebw_count=0
replace married_malebw_count=1 if hhtype==1 & pernum==1 & inlist(hh_earn_type,1,2)

gen married_malebw_count_ly=0
replace married_malebw_count_ly=1 if pernum==1 & inlist(hh_earn_type,1,2) & marrinyr==2

gen married_malebw_count_non=0
replace married_malebw_count_non=1 if hhtype==1 & pernum==1 & inlist(hh_earn_type,1,2) & college==0

gen married_malebw_count_non_ly=0
replace married_malebw_count_non_ly=1 if pernum==1 & inlist(hh_earn_type,1,2) & college==0 & marrinyr==2

gen married_malesole_count=0
replace married_malesole_count=1 if hhtype==1 & pernum==1 & hh_earn_type==1

gen married_malesole_count_non=0
replace married_malesole_count_non=1 if hhtype==1 & pernum==1 & hh_earn_type==1 & college==0

gen married_malebw_count_coll=0
replace married_malebw_count_coll=1 if hhtype==1 & pernum==1 & inlist(hh_earn_type,1,2) & college==1

gen married_malebw_count_coll_ly=0
replace married_malebw_count_coll_ly=1 if pernum==1 & inlist(hh_earn_type,1,2) & college==1 & marrinyr==2

// Variables to create
* Men's hourly earnings
browse year serial sex empstat incwage wkswork2 uhrswork
gen weeks_worked=0 // weeks worked is categorical; use mid-point
replace weeks_worked=6 if wkswork2==1 // 1-13
replace weeks_worked=20 if wkswork2==2 // 14-26
replace weeks_worked=33 if wkswork2==3 // 27-39
replace weeks_worked=44 if wkswork2==4 // 40-47
replace weeks_worked=49 if wkswork2==5 // 48-49
replace weeks_worked=52 if wkswork2==6 // 50-52

gen men_hourly = (incwage / weeks_worked / uhrswork) if sex==1 & empstat==1

* Women's hourly earnings
gen women_hourly = (incwage / weeks_worked / uhrswork) if sex==2 & empstat==1

browse year serial sex empstat incwage wkswork2 uhrswork men_hourly women_hourly

* Men's average hours
gen men_wk_hours=.
replace men_wk_hours = uhrswork if sex==1 & empstat==1

* Women's average hours
gen women_wk_hours=.
replace women_wk_hours = uhrswork if sex==2 & empstat==1

* HH income deciles

// Variables to remember I want
* Created hourly earnings
	* men_hourly
	* women_hourly
* Men's median weekly work hours - men_wk_hours
* Women's median weekly work hours - women_wk_hours
* Occupational segregation - revisit later
* Occupational autonomy - revisit later


********************************************************************************
* AGGREGATE BY STATE AND YEAR
********************************************************************************

** Getting state-level metrics
preserve

collapse 	(median) 	men_hourly women_hourly	men_wk_hours women_wk_hours					///
			(mean)		men_mean_hours=men_wk_hours women_mean_hours=women_wk_hours			///
			(sum)   	hh_count hh_count_non hh_pov female_hh_count female_hh_pov college	///
						woman_25_64 woman_25_64_lf woman_25_64_emp college_woman_25_64		///
						college_woman_25_64_lf college_woman_25_64_emp non_woman_25_64		///
						non_woman_25_64_lf non_woman_25_64_emp man_25_64 man_25_64_lf 		///
						man_25_64_emp woman_ceo woman_manage married_hh_count 				///
						married_dual_count married_malebw_count married_malesole_count 		/// 
						married_dual_count_non married_malebw_count_non						/// 
						married_malesole_count_non married_hh_count_non				 		/// 
						married_ly_count married_ly_count_non married_dual_count_ly 		///
						married_dual_count_non_ly married_malebw_count_ly					///
						married_malebw_count_non_ly married_malebw_count_coll				///
						married_malebw_count_coll_ly married_dual_count_coll_ly	 			///
						married_dual_count_coll married_ly_count_coll						///
						married_hh_count_coll all_emp all_emp_pt woman_emp_pt				///
			(p10) 		p10_hhincome=hhincome												///
			(p50)		p50_hhincome=hhincome hhincome_children								///
			(p90)		p90_hhincome=hhincome,												///
			by(year statefip region)
			
save "$created_data\2010_2018_state_data_main.dta", replace

gen college_women_employ = college_woman_25_64_emp / college_woman_25_64
gen non_women_employ = non_woman_25_64_emp / non_woman_25_64

**Calculated metrics
gen x_lfp_women = woman_25_64_lf / woman_25_64
gen x_employed_women = woman_25_64_emp / woman_25_64
gen x_employed_women_lf = woman_25_64_emp / woman_25_64_lf
gen x_women_wage_ratio = women_hourly / men_hourly
gen x_women_ceo = woman_ceo / woman_25_64_emp
gen x_women_manage = woman_manage / woman_25_64_emp
gen x_college_women = college_woman_25_64 / woman_25_64 // this won't be right because of age group; think need to get from different source anyway
gen x_sex_ratio = woman_25_64_emp / man_25_64_emp
gen x_educ_work_ratio = non_women_employ / college_women_employ
gen x_90_10_inequality = p90_hhincome / p10_hhincome
gen x_pov_rate = hh_pov / hh_count
gen x_fem_pov_rate = female_hh_pov / female_hh_count
gen x_married_dual = married_dual_count / married_hh_count
gen x_married_malebw = married_malebw_count / married_hh_count
gen x_married_malesole = married_malesole_count / married_hh_count
gen x_married_dual_non = married_dual_count_non / married_hh_count_non
gen x_married_malebw_non = married_malebw_count_non / married_hh_count_non
gen x_married_malesole_non = married_malesole_count_non / married_hh_count_non
gen x_married_dual_coll = married_dual_count_coll / married_hh_count_coll
gen x_married_malebw_coll = married_malebw_count_coll / married_hh_count_coll
// gen x_married_malesole_coll = married_malesole_count_coll / married_hh_count_coll
gen x_married_dual_ly = married_dual_count_ly / married_ly_count
gen x_married_malebw_ly = married_malebw_count_ly / married_ly_count
gen x_married_dual_non_ly = married_dual_count_non_ly / married_ly_count_non
gen x_married_malebw_non_ly = married_malebw_count_non_ly / married_ly_count_non
gen x_married_malebw_coll_ly = married_malebw_count_coll_ly / married_ly_count_coll
gen x_married_dual_coll_ly = married_dual_count_coll_ly / married_ly_count_coll
gen x_all_pt = all_emp_pt / all_emp 
gen x_women_pt = woman_emp_pt / woman_25_64_emp

browse year statefip x_*

save "$created_data\2010_2018_state_data_main.dta", replace

/// export excel using "$\results\state_hh_distribution.xls", firstrow(variables)

restore