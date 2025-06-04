********************************************************************************
* Compiling some acs data
* data_acs.do
* Kim McErlean
********************************************************************************

********************************************************************************
* CREATING DATA-SET FOR CHILDCARE
********************************************************************************
use "$ACS\acs_2010_2018_children.dta", clear

browse serial pernum momloc poploc empstat_mom empstat_pop

gen parents_employed=0
replace parents_employed=1 if empstat_mom==1 & empstat_pop==1
replace parents_employed=1 if empstat_mom==1 & empstat_pop==.
replace parents_employed=1 if empstat_mom==. & empstat_pop==1

gen count=1

gen employed_all = parents_employed * perwt
gen count_all = count * perwt


preserve
collapse (sum) count parents_employed count_all employed_all, by(year statefip region)

save "$created_data\2010_2018_state_child_counts.dta", replace

restore

********************************************************************************
* CREATING DATA-SET FOR HOUSING
********************************************************************************
use "$ACS\acs_housing_1990_2021.dta", clear

// use rentgrs owncost hhincome -- remember the first two are MONTHLY kim, hh income is annual. so need to annualize
replace owncost=. if owncost==99999
replace rentgrs=. if ownershp!=2
replace rentgrs=. if ownershpd== 21

gen annual_rent = rentgrs * 12
gen annual_own = owncost * 12
gen annual_housing = .
replace annual_housing = annual_rent if annual_rent!=.
replace annual_housing = annual_own if annual_own!=.

gen rent_income = annual_rent / hhincome
gen own_income = annual_own / hhincome
gen housing_income = .
replace housing_income = rent_income if annual_rent!=.
replace housing_income = own_income if annual_own!=.

gen renters=0
replace renters=1 if annual_rent!=.
gen owners=0
replace owners=1 if annual_own!=.
gen house=0
replace house=1 if renters==1 | owners==1

gen rent_35pct=0
replace rent_35pct=1 if renters==1 & rent_income>=0.350000 & rent_income!=.

gen own_35pct=0
replace own_35pct=1 if owners==1 & own_income>=0.350000 & own_income!=.

gen house_35pct=0
replace house_35pct=1 if house==1 & housing_income>=0.350000 & housing_income!=.

preserve
collapse 	(sum) renters rent_35pct owners own_35pct house house_35pct ///
			(p50) rentgrs annual_rent owncost annual_own annual_housing ///
			(p50) rent_income own_income housing_income hhincome ///
			, by(year statefip region)
			
gen rent_afford = rent_35pct / renters
gen own_afford = own_35pct / owners
gen house_afford = house_35pct / house

save "$created_data\1990_2021_housing_costs.dta", replace

restore