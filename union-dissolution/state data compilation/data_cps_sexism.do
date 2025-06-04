use "$CPS/cps_1990_2022_structuralsexism.dta", clear

/*
recode educ (1/71=1)(72/73=2)(80/110=3)(111/125=4)(999=1), gen(education)
gen college=(education==4)
*/

gen employed=.
replace employed=0 if inlist(labforce,0,1)
replace employed=1 if inlist(empstat,10,12)
replace employed=2 if inlist(empstat,20,21,22)

label define employed 0 "NILF" 1 "Employed" 2 "Unemployed"
label values employed employed

gen ft_worker=0
replace ft_worker=1 if inlist(wkstat,10,11,14,15)

gen weekly_earn_ft=.
replace weekly_earn_ft = incwage/52 if ft_worker==1
// browse ft_worker weekly_earn_ft incwage

drop if age < 16 | age > 64

// to get data over time
gen total=1
gen men = (sex==1)
gen women = (sex==2)

gen mothers=0
replace mothers=1 if sex==2 & nchlt5!=0 // just want young children since more associated with employment

gen fathers=0
replace fathers=1 if sex==1 & nchlt5!=0

* for LFP ratio
gen men_lfp=0
replace men_lfp=1 if men==1 & labforce==2
gen women_lfp=0
replace women_lfp=1 if women==1 & labforce==2

gen men_emp=0
replace men_emp=1 if men==1 & employed==1
gen women_emp=0
replace women_emp=1 if women==1 & employed==1

gen mothers_emp=0
replace mothers_emp=1 if mothers==1 & employed==1

gen fathers_emp=0
replace fathers_emp=1 if fathers==1 & employed==1

* for earnings ratio
gen men_earn_ft=.
replace men_earn_ft = weekly_earn_ft if men==1
gen women_earn_ft=.
replace women_earn_ft = weekly_earn_ft if women==1

gen fathers_earn_ft=.
replace fathers_earn_ft = weekly_earn_ft if fathers==1
gen mothers_earn_ft=.
replace mothers_earn_ft = weekly_earn_ft if mothers==1

gen men_wage=.
replace men_wage=incwage if men==1
gen women_wage=.
replace women_wage=incwage if women==1

* for poverty ratio
gen men_pov=0
replace men_pov=1 if men==1 & offpov==1
gen women_pov=0
replace women_pov=1 if women==1 & offpov==1

gen child=0
replace child=1 if age <18

gen child_in_pov=0
replace child_in_pov=1 if child==1 & offpov==1

preserve

collapse 	(sum) 	total men women men_lfp women_lfp men_emp women_emp men_pov women_pov 			///
					mothers fathers mothers_emp fathers_emp	child child_in_pov						///
			(p50)	men_earn_ft women_earn_ft men_wage women_wage fathers_earn_ft mothers_earn_ft,  ///
					by(year statefip)

// create variables
gen men_lfp_rate = men_lfp / men
gen women_lfp_rate = women_lfp / women
gen maternal_employment = mothers_emp / mothers			
gen men_pov_rate = men_pov / men
gen women_pov_rate = women_pov / women

gen earn_ratio = men_earn_ft / 	women_earn_ft
gen parent_earn_ratio = fathers_earn_ft / mothers_earn_ft
gen lfp_ratio = men_lfp_rate /  women_lfp_rate
gen pov_ratio = men_pov_rate / women_pov_rate

gen child_pov_rate = child_in_pov / child

rename statefip state_fips

save "$created_data/sexism_measures_1990_2022.dta", replace
// T:/Research Projects/State data/data_keep/sexism_measures_1990_2019.dta

restore
