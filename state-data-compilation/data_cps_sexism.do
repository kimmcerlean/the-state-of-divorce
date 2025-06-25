********************************************************************************
* Create structural support variable
* This file gets relevant data needed from the CPS to input into the scale
* create-state-varables.do
* Kim McErlean
********************************************************************************

use "$CPS/cps_1988_2023_structuralmeasure.dta", clear

/*
recode educ (1/71=1)(72/73=2)(80/110=3)(111/125=4)(999=1), gen(education)
gen college=(education==4)
*/


********************************************************************************
*Employment things
********************************************************************************

gen employed=.
replace employed=0 if inlist(labforce,0,1)
replace employed=1 if inlist(empstat,10,12)
replace employed=2 if inlist(empstat,20,21,22)

label define employed 0 "NILF" 1 "Employed" 2 "Unemployed"
label values employed employed

tab wkswork1 employed, m col

browse pernum year wkstat uhrsworkt uhrswork1 ahrsworkt
// https://cps.ipums.org/cps/hours_worked_variables_notes.shtml
// tabstat uhrsworkt uhrswork1 ahrsworkt, by(year) // only uhrswork1 and ahrsworkt available for all years. okay, but in early years, the uhrs one not asked unless in outgoing rotation group. okay so use wkstat
// tabstat uhrsworkt uhrswork1 ahrsworkt if uhrsworkt<900 & uhrswork1<900 & ahrsworkt<900, by(year)

gen ft_worker = 0
replace ft_worker = 1 if inlist(wkstat,10,11,12,13,21)

gen ft_worker_alt = 0
replace ft_worker_alt = 1 if uhrswork1 >=35 & uhrswork1<900

tab ft_worker ft_worker_alt, m cell // okay the one way set of overlap validates that the one I am using (not alt) is right

browse pernum year ft_worker ft_worker_alt wkstat uhrsworkt uhrswork1 ahrsworkt

gen pt_worker=0
replace pt_worker = 1 if inlist(wkstat,14,15,22,40,41,42)

gen weekly_earn_ft=.
replace weekly_earn_ft = incwage/52 if ft_worker==1
// INCWAGE indicates each respondent's total pre-tax wage and salary income--that is, money received as an employee--for the previous calendar year.
// In the CPS, individuals' employment status was determined on the basis of answers to a series of questions relating to their activities during the preceding week.
// so this is slightly problematic maybe - because wages are last year but employment is currently GAH
// browse ft_worker weekly_earn_ft incwage

// drop if age < 16 | age > 64 // this actually not going to work for child poverty? so just use this as filter below?s

// to get data over time
gen total=1

gen men = (sex==1)
gen women = (sex==2)

gen work_age_men = 1 if sex==1 & age>=16 & age <=64 // empstat asked of those 15+, but more than 90% of 15yos not in LF
replace work_age_men = 0 if work_age_men==.
gen work_age_women = 1 if sex==2 & age>=16 & age <=64
replace work_age_women = 0 if work_age_women==.

// will restrict all of these to working age
gen married_women=0
replace married_women=1 if sex==2 & inlist(marst,1,2) & age>=16 & age <=64

gen married_men=0
replace married_men=1 if sex==1 & inlist(marst,1,2) & age>=16 & age <=64

gen mothers=0
replace mothers=1 if sex==2 & nchild!=0 & age>=16 & age <=64

gen mothers_u5=0
replace mothers_u5=1 if sex==2 & nchlt5!=0  & age>=16 & age <=64 // just want young children since more associated with employment

gen fathers=0
replace fathers=1 if sex==1 & nchild!=0 & age>=16 & age <=64

gen fathers_u5=0
replace fathers_u5=1 if sex==1 & nchlt5!=0 & age>=16 & age <=64

* for LFP ratio
gen men_lfp=0
replace men_lfp=1 if work_age_men==1 & labforce==2
gen women_lfp=0
replace women_lfp=1 if work_age_women==1 & labforce==2

gen men_emp=0
replace men_emp=1 if work_age_men==1 & employed==1
gen women_emp=0
replace women_emp=1 if work_age_women==1 & employed==1

gen married_men_emp=0
replace married_men_emp=1 if married_men==1 & employed==1
gen married_women_emp=0
replace married_women_emp=1 if married_women==1 & employed==1

gen mothers_emp=0
replace mothers_emp=1 if mothers==1 & employed==1
gen mothers_u5_emp=0
replace mothers_u5_emp=1 if mothers_u5==1 & employed==1

gen fathers_emp=0
replace fathers_emp=1 if fathers==1 & employed==1
gen fathers_u5_emp=0
replace fathers_u5_emp=1 if fathers_u5==1 & employed==1

// women's PT status
gen women_pt_emp=0
replace women_pt_emp=1 if work_age_women==1 & employed==1 & pt_worker==1

gen married_women_pt_emp=0
replace married_women_pt_emp=1 if married_women==1 & employed==1 & pt_worker==1

gen mothers_pt_emp=0
replace mothers_pt_emp=1 if mothers==1 & employed==1 & pt_worker==1

gen mothers_u5_pt_emp=0
replace mothers_u5_pt_emp=1 if mothers_u5==1 & employed==1 & pt_worker==1

* for earnings ratio
gen men_earn_ft=.
replace men_earn_ft = weekly_earn_ft if work_age_men==1
gen women_earn_ft=.
replace women_earn_ft = weekly_earn_ft if work_age_women==1

gen married_men_earn_ft=.
replace married_men_earn_ft = weekly_earn_ft if married_men==1
gen married_women_earn_ft=.
replace married_women_earn_ft = weekly_earn_ft if married_women==1

gen men_wage=.
replace men_wage=incwage if work_age_men==1
gen women_wage=.
replace women_wage=incwage if work_age_women==1

gen fathers_earn_ft=.
replace fathers_earn_ft = weekly_earn_ft if fathers==1
gen mothers_earn_ft=.
replace mothers_earn_ft = weekly_earn_ft if mothers==1

gen fathers_u5_earn_ft=.
replace fathers_u5_earn_ft = weekly_earn_ft if fathers_u5==1
gen mothers_u5_earn_ft=.
replace mothers_u5_earn_ft = weekly_earn_ft if mothers_u5==1

// median HH income
// for cost percentage, they use "median income by state, married couple with children under the age of 18"
// example table B19126 from ACS 5-year: ""Median Family Income in the Past 12 Months (in 2023 Inflation-Adjusted Dollars) by Family Type by Presence of Own Children Under 18 Years"
tabstat hhincome, by(year) stats(p50 mean)
// tabstat hhincome [aweight=asecwt], by(year) stats(p50 mean)

tab year gqtype, m // not asked all years
tab ncouples, m // but need the couple to be the HHer
tab gqtype ncouples, m // so in years with overlap, husband / wife "Types" always have at least 1 couple. HOWEVER, there are sometimes couples even if not the HH type
// oh duh there is famkind - use this
tab gqtype famkind, m row
tab ncouples famkind, m

// nchild is also children of ANY AGE (NCHILD counts the number of own children (of any age or marital status) residing with each individual.)
// so there isn't an under 18 version. So I might need to do both of these things (married HH + children) using HH roster info?
// or do based on age of eldest child? like if the eldest child is under 18, then I can use nchild, but if not, need to figure it out...
tab eldch nchild, m
tab eldch if eldch!=99 // okay, about 70% have the eldest child under 18
tab age if famrel == 3, m

bysort serial year: egen nchild_u18 = count(pernum) if age < 18 & relate==301
bysort serial year (nchild_u18): replace nchild_u18 = nchild_u18[1]
replace nchild_u18 = 0 if nchild_u18==.
tab nchild_u18, m
tab nchild if eldch<18, m
tab nchild if yngch<18, m

sort year serial pernum
browse serial pernum year marst age nfams famkind famrel ncouples nchild_u18 nchild relate eldch yngch hhincome

gen married_fam_hh=0
replace married_fam_hh = 1 if famkind==1 & inrange(nchild_u18,1,20)
// browse serial pernum year married_fam_hh marst age nfams famkind famrel ncouples nchild_u18 nchild relate eldch yngch hhincome

unique serial year
unique serial year if pernum==1 // because i am doing median, I actually think I do need to restrict this based on just 1 record per HH. will just do pernum of 1 (gets same count)
unique serial year, by(married_fam_hh)
unique serial year if pernum==1, by(married_fam_hh)

********************************************************************************
* for poverty ratio / HS eligiblity
********************************************************************************

browse serial pernum year offpov offpovuniv offtotval offcutoff offreason poverty cutoff

tab poverty offpov

gen men_pov=0
replace men_pov=1 if men==1 & offpov==1
gen women_pov=0
replace women_pov=1 if women==1 & offpov==1

gen child_u18=0
replace child_u18=1 if age <18

gen child_u6=0
replace child_u6=1 if age <6

gen child_0to2=0
replace child_0to2=1 if age >=0 & age <=2

gen child_3to5=0
replace child_3to5=1 if age >=3 & age <=5

gen child_u18_in_pov=0
replace child_u18_in_pov=1 if child_u18==1 & offpov==1

gen child_u6_in_pov=0
replace child_u6_in_pov=1 if child_u6==1 & offpov==1

gen child_0to2_in_pov=0
replace child_0to2_in_pov=1 if child_0to2==1 & offpov==1

gen child_3to5_in_pov=0
replace child_3to5_in_pov=1 if child_3to5==1 & offpov==1

// tab year child_u6_in_pov
// tab year child_u6_in_pov [aweight=asecwt] // want to see if I can get these to match kids count - do I need weights?
// tab year child_u6_in_pov if child_u6==1 & inlist(offpov,1,2) [aweight=asecwt], row // okay do at least the percentages match? okay yes the percentages are better than the raw counts
// okay, the other option is taking their raw counts that go back further in time than their child poverty and applying the percentages? (instead of using the raw counts as base for HS eligiblity)

gen child_u6_in_pov_test = child_u6_in_pov * asecwt if hflag==. | hflag== 0 // see below notes - have to use only 5/8 or 3/8 sample for 2014 (Will do 5/8 for now); all other years are missing
tabstat child_u6_in_pov_test, by(year) stats(sum) // okay, THIS is actually quite close - so I do need to multiply all of my variables? let's create two versions?

* Other forms of assistance
// browse serial pernum year incwelfr srcwelfr incssi

tab srcwelfr, m
gen welfare_income = incwelfr if incwelfr > 0 & incwelfr < 900000 // okay so 0 is NONE, 999999 is truly not in universe (those under 15)
gen ssi_income = incssi if incssi > 0 & incssi < 900000

tabstat welfare_income, by(srcwelfr)
browse serial pernum age year welfare_income ssi_income srcwelfr srcwelfr_mom srcwelfr_mom2 srcwelfr_pop srcwelfr_pop2 incwelfr incssi

foreach var in mom mom2 pop pop2{
	gen ssi_`var' = 0
	replace ssi_`var' = 1 if incssi_`var' > 0 & incssi_`var' < 900000
}

browse serial pernum age year ssi_income incssi incssi_mom ssi_mom incssi_mom2 ssi_mom2 incssi_pop ssi_pop incssi_pop2 ssi_pop2

tab age srcwelf, m // okay, not asked to those under 15, so I should just make a HH version? OR, I pull in mom and dad variables to get this info for kids - like did mom or dad receive?
tab age srcwelfr_mom, m
tab age srcwelfr_pop, m

gen parent_tanf = 0
replace parent_tanf = 1 if inlist(srcwelfr_mom,1,3) | inlist(srcwelfr_mom2,1,3) | inlist(srcwelfr_pop,1,3) | inlist(srcwelfr_pop2,1,3)
tab parent_tanf, m
tab age parent_tanf, m

gen parent_ssi = 0
replace parent_ssi = 1 if ssi_mom==1 | ssi_mom2==1 | ssi_pop==1 | ssi_pop2==1
tab parent_ssi, m
tab age parent_ssi, m

gen child_u6_tanf=0
replace child_u6_tanf=1 if child_u6==1 & parent_tanf==1

gen child_0to2_tanf=0
replace child_0to2_tanf=1 if child_0to2==1 & parent_tanf==1

gen child_3to5_tanf=0
replace child_3to5_tanf=1 if child_3to5==1 & parent_tanf==1

gen child_u6_ssi=0
replace child_u6_ssi=1 if child_u6==1 & parent_ssi==1

gen child_0to2_ssi=0
replace child_0to2_ssi=1 if child_0to2==1 & parent_ssi==1

gen child_3to5_ssi=0
replace child_3to5_ssi=1 if child_3to5==1 & parent_ssi==1

tab child_u6_tanf child_u6_ssi, m
tab child_u6_in_pov child_u6_tanf, m

// okay eligibility is poverty OR assistance so:
gen child_u6_hs_elig=0
replace child_u6_hs_elig=1 if child_u6==1 & (offpov==1 | parent_tanf==1 | parent_ssi==1)

gen child_0to2_hs_elig=0
replace child_0to2_hs_elig=1 if child_0to2==1 & (offpov==1 | parent_tanf==1 | parent_ssi==1)

gen child_3to5_hs_elig=0
replace child_3to5_hs_elig=1 if child_3to5==1 & (offpov==1 | parent_tanf==1 | parent_ssi==1)


********************************************************************************
* Get HH economic status (male-BW or dual-earning)
* JUST married couples
********************************************************************************
// key challenge here is needing to make sure I do not overcount. do I need to do this separately so I can dedup?
// or can I use sex?
tab sex sex_sp if marst==1 // so I actually think I could just consider data from one spouse (and only consider married if hetero)
// or well if both the numerator and denominator are inflated by 2x, the ratio will actually be right? Am I dumb? Because I just need % not number?

tab marst marst_sp, m row //  okay, only primarily have this info if spouse present so married, in this case, will just be that
// inspect incwage_sp if marst==1
// inspect incwage_sp if marst==2
gen married = 0
replace married = 1 if marst==1 & sex!=sex_sp

browse serial pernum year marst incwage incwage_sp inctot inctot_sp  if married==1

egen couple_earnings=rowtotal(incwage incwage_sp)

gen wife_earnings=.
replace wife_earnings=incwage if sex==2
replace wife_earnings=incwage_sp if sex==1

gen husband_earnings=.
replace husband_earnings=incwage if sex==1
replace husband_earnings=incwage_sp if sex==2

// browse serial pernum year marst sex sex_sp wife_earnings husband_earnings incwage incwage_sp inctot inctot_sp  if married==1
gen wife_earn_ratio = wife_earnings / couple_earnings
tabstat wife_earn_ratio if married==1, by(year) // do % and trends make sense?
// inspect wife_earn_ratio if married==1
// tab couple_earnings if wife_earn_ratio==. & married==1
 
gen hh_earn_type=.
replace hh_earn_type = 0 if wife_earn_ratio==.
replace hh_earn_type = 1 if wife_earn_ratio==0
replace hh_earn_type = 2 if wife_earn_ratio < 0.4 & wife_earn_ratio > 0
replace hh_earn_type = 3 if wife_earn_ratio >=0.4 & wife_earn_ratio <=0.6
replace hh_earn_type = 4 if wife_earn_ratio >=0.6 & wife_earn_ratio!=.

label define hh_earn_type 0 "No Couple Earnings" 1 "Male Sole" 2 "Male Primary" 3 "Dual Earner" 4 "Female Primary"
label values hh_earn_type hh_earn_type

tab hh_earn_type if married==1, m // decide if male BW is male sole or all - see size. prob make version of both

gen married_male_bw = 0
replace married_male_bw = 1 if married==1 & inlist(hh_earn_type,1,2)

gen married_pure_male_bw = 0
replace married_pure_male_bw = 1 if married==1 & hh_earn_type==1

gen married_dual_earn = 0
replace married_dual_earn = 1 if married==1 & hh_earn_type==3

tab married married_male_bw, row m
tab married married_pure_male_bw, row m
tab married married_dual_earn, row m

********************************************************************************
* Create weighted versions of variables to use for more accurate counts
********************************************************************************

/* Note on weighting bc of redesign in 2014:
See: https://cps.ipums.org/cps/three_eighths.shtml
More specifically, the Census Bureau recommends using the 3/8 file (HFLAG=1) for comparing income estimates from ASEC 2014 with ASEC 2015 and beyond. Similarly, those looking to compare income estimates from ASEC 2014 with ASEC 2013 and prior should use the 5/8 file (HFLAG=0). In general, the choice of weights will depend on the particular analysis being undertaken.

So, I am planning to use both...
*/

// quickly validate all binary before I do this math
sum total men women work_age_men work_age_women mothers mothers_u5 fathers fathers_u5 men_lfp women_lfp men_emp women_emp mothers_emp mothers_u5_emp fathers_emp fathers_u5_emp women_pt_emp mothers_pt_emp mothers_u5_pt_emp men_pov women_pov child_u18 child_u6 child_0to2 child_3to5 child_u18_in_pov child_u6_in_pov child_0to2_in_pov child_3to5_in_pov parent_tanf parent_ssi child_u6_tanf child_0to2_tanf child_3to5_tanf child_u6_ssi child_0to2_ssi child_3to5_ssi child_u6_hs_elig child_0to2_hs_elig child_3to5_hs_elig married married_male_bw married_pure_male_bw married_dual_earn

foreach var in total men women work_age_men work_age_women mothers mothers_u5 fathers fathers_u5 men_lfp women_lfp men_emp women_emp mothers_emp mothers_u5_emp fathers_emp fathers_u5_emp women_pt_emp mothers_pt_emp mothers_u5_pt_emp men_pov women_pov child_u18 child_u6 child_0to2 child_3to5 child_u18_in_pov child_u6_in_pov child_0to2_in_pov child_3to5_in_pov parent_tanf parent_ssi child_u6_tanf child_0to2_tanf child_3to5_tanf child_u6_ssi child_0to2_ssi child_3to5_ssi child_u6_hs_elig child_0to2_hs_elig child_3to5_hs_elig married married_male_bw married_pure_male_bw married_dual_earn{
	gen `var'_wt = `var' * asecwt if hflag==. | hflag== 0 // using 5/8 sample for 2014 for now...
}

browse serial pernum year sex asecwt men women men_wt women_wt

********************************************************************************
**# Aggregate to state and year
********************************************************************************

preserve

collapse 	(sum) 	total men women work_age_men work_age_women mothers mothers_u5 ///
					fathers fathers_u5 men_lfp women_lfp men_emp women_emp mothers_emp ///
					mothers_u5_emp fathers_emp fathers_u5_emp women_pt_emp mothers_pt_emp ///
					mothers_u5_pt_emp men_pov women_pov child_u18 child_u6 child_0to2 ///
					child_3to5 child_u18_in_pov child_u6_in_pov child_0to2_in_pov ///
					child_3to5_in_pov parent_tanf parent_ssi child_u6_tanf child_0to2_tanf ///
					child_3to5_tanf child_u6_ssi child_0to2_ssi child_3to5_ssi child_u6_hs_elig ///
					child_0to2_hs_elig child_3to5_hs_elig married married_male_bw ///
					married_pure_male_bw married_dual_earn total_wt men_wt women_wt ///
					work_age_men_wt work_age_women_wt mothers_wt mothers_u5_wt fathers_wt ///
					fathers_u5_wt men_lfp_wt women_lfp_wt men_emp_wt women_emp_wt mothers_emp_wt ///
					mothers_u5_emp_wt fathers_emp_wt fathers_u5_emp_wt women_pt_emp_wt ///
					mothers_pt_emp_wt mothers_u5_pt_emp_wt men_pov_wt women_pov_wt child_u18_wt ///
					child_u6_wt child_0to2_wt child_3to5_wt child_u18_in_pov_wt child_u6_in_pov_wt ///
					child_0to2_in_pov_wt child_3to5_in_pov_wt parent_tanf_wt parent_ssi_wt ///
					child_u6_tanf_wt child_0to2_tanf_wt child_3to5_tanf_wt child_u6_ssi_wt ///
					child_0to2_ssi_wt child_3to5_ssi_wt child_u6_hs_elig_wt child_0to2_hs_elig_wt ///
					child_3to5_hs_elig_wt married_wt married_male_bw_wt married_pure_male_bw_wt ///
					married_dual_earn_wt ///
			(p50)	men_earn_ft women_earn_ft men_wage women_wage fathers_earn_ft ///
					mothers_earn_ft fathers_u5_earn_ft mothers_u5_earn_ft, /// 
					by(year statefip)

// tmp save
save "$created_data/sexism_measures_1988_2023.dta", replace

// create variables: regular
gen men_lfp_rate = men_lfp / work_age_men
gen women_lfp_rate = women_lfp / work_age_women
gen women_emp_rate = women_emp / work_age_women
gen maternal_employment = mothers_emp / mothers	
gen maternal_u5_employment = mothers_u5_emp / mothers
gen women_pt_rate = women_pt_emp / women_emp // is base all women or just employed? Just employed (see Hook and Paek. I thought more studies used this but maybe just them?)
// "We took the incidence of parttime employment among dependent employees as a percentage of women's total employment, using national definitions."
gen maternal_pt_rate = mothers_pt_emp / mothers_emp
gen maternal_u5_pt_rate = mothers_u5_pt_emp / mothers_u5_emp

gen men_pov_rate = men_pov / men
gen women_pov_rate = women_pov / women
gen child_u18_pov_rate = child_u18_in_pov / child_u18
gen child_u6_pov_rate = child_u6_in_pov / child_u6
gen child_0to2_pov_rate = child_0to2_in_pov / child_0to2
gen child_3to5_pov_rate = child_3to5_in_pov / child_3to5

gen child_u6_hs_elig_rate = child_u6_hs_elig / child_u6
gen child_0to2_hs_elig_rate = child_0to2_hs_elig / child_0to2
gen child_3to5_hs_elig_rate = child_3to5_hs_elig / child_3to5

gen married_dual_earn_rate = married_dual_earn / married
gen married_male_bw_rate = married_male_bw / married
gen married_pure_male_bw_rate = married_pure_male_bw / married

gen earn_ratio = women_earn_ft / men_earn_ft // (note 6/19/25: this used to be men / women, but in the scale, I want higher to be "better" for women, so I have updated)
gen parent_earn_ratio = mothers_earn_ft / fathers_earn_ft
gen parent_u5_earn_ratio = mothers_u5_earn_ft / fathers_u5_earn_ft
gen lfp_ratio = women_lfp_rate / men_lfp_rate 
gen pov_ratio = men_pov_rate / women_pov_rate

// create variables: weighted versions
gen men_lfp_rate_wt = men_lfp_wt / work_age_men_wt
gen women_lfp_rate_wt = women_lfp_wt / work_age_women_wt
gen women_emp_rate_wt = women_emp_wt / work_age_women_wt
gen maternal_employment_wt = mothers_emp_wt / mothers_wt
gen maternal_u5_employment_wt = mothers_u5_emp_wt / mothers_wt
gen women_pt_rate_wt = women_pt_emp_wt / women_emp_wt
gen maternal_pt_rate_wt = mothers_pt_emp_wt / mothers_emp_wt
gen maternal_u5_pt_rate_wt = mothers_u5_pt_emp_wt / mothers_u5_emp_wt

gen men_pov_rate_wt = men_pov_wt / men_wt
gen women_pov_rate_wt = women_pov_wt / women_wt
gen child_u18_pov_rate_wt = child_u18_in_pov_wt / child_u18_wt
gen child_u6_pov_rate_wt = child_u6_in_pov_wt / child_u6_wt
gen child_0to2_pov_rate_wt = child_0to2_in_pov_wt / child_0to2_wt
gen child_3to5_pov_rate_wt = child_3to5_in_pov_wt / child_3to5_wt

gen child_u6_hs_elig_rate_wt = child_u6_hs_elig_wt / child_u6_wt
gen child_0to2_hs_elig_rate_wt = child_0to2_hs_elig_wt / child_0to2_wt
gen child_3to5_hs_elig_rate_wt = child_3to5_hs_elig_wt / child_3to5_wt

gen married_dual_earn_rate_wt = married_dual_earn_wt / married_wt
gen married_male_bw_rate_wt = married_male_bw_wt / married_wt
gen married_pure_male_bw_rate_wt = married_pure_male_bw_wt / married_wt

rename statefip state_fips

save "$created_data/sexism_measures_1988_2023.dta", replace
// save "$created_data/sexism_measures_1990_2022.dta", replace
// T:/Research Projects/State data/data_keep/sexism_measures_1990_2019.dta

export excel using "$created_data/sexism_measures_1988_2023", firstrow(variables) replace

restore

********************************************************************************
**# Median income
********************************************************************************

** Need to deduplicate
keep if pernum==1
keep if married_fam_hh==1
unique serial year pernum

preserve

collapse 	(p50)	hhincome [aweight=asecwt], /// 
					by(year statefip)

// save
save "$created_data/cps_medianfaminc_1988_2023.dta", replace

restore

