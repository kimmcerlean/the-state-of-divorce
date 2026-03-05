********************************************************************************
* Getting CPS data to compare cohabitors in PSID (not representative) to cohabitors
* in CPS (should be representative)
* cps_rel_type_descriptives.do
* Kim McErlean
********************************************************************************
// a few challenges. FIrst, I am not sure if I should use marrinyr or marital status
// upon reflection, marital status - because I have the current population of married couples, not just newlyweds
// so, i have more flexibility to go back before 2008 bc marrinyr not until 2008
// BUT year married also not until 2008. I don't want people married in lke 1977, because they aren't in my PSID sample
// so - do I restrict all to 2008 OR see in 2008+, how many marriages "recent" v. old to estimate effects - if like 50/50. prob risky. if not, could possibly use more expansive (right not pulled in starting 2000 so could use that to start to evaluate)
// problem is - is it weirder to match policy to do a currently married analysis? 
// more intuitive to link policy in year of marriage? I guess again, these are different questions and my PSID = all currently married (with some restrictions) so this is most aligned?

use "$ACS\acs_2000_2021_marriage-selection.dta", clear

// this file is very large so let's try to immediately drop some irrelevant people. Problem is the samples will differ - if I want to estimate marriage, I need never married and married in last year. if I want to estimate current married population, i need currently married. so let's actually split this file here to make it easier later?

********************************************************************************
********************************************************************************
********************************************************************************
**# * Based on currently married population
********************************************************************************
********************************************************************************
********************************************************************************
// so here - can restrict just based on marital status
tab marst
fre marst

keep if marst==1

// marriage order
tab marrno
keep if marrno==1 | marrno==. // for now - bc not asked until 2008

// same v. different sex
tab sex, m
tab sex_sp, m
tab sex sex_sp, m
drop if sex==sex_sp // need different gender couples

* Other sample descriptives to align with PSID
// age
tab age, m
tab age_sp, m
keep if (age>=18 & age<=55) &  (age>=18 & age_sp<=55)

// WHAT ELSE - is that it actually LOL it's mostly about marriage order and then PSID specific things that don't apply here.

// let's explore the year marriage thing - decide if can use more expansive time frame or need to use 2008 and restrict to contemporary marriages
tabstat yrmarr, by(year) // so the average distance is like 20+ years so this isn't a great sign... even in 2008, average is 1985 which is much before my sample
// once i remove older ages, it's better. more like 15 years. so that means NOW the average is basically my current PSID sample (aka 1995+)
tab yrmarr
gen current_marr_yr = .
replace current_marr_yr = 0 if yrmarr < 2008 & yrmarr!=. // have to use 2008 because that's when this asked
replace current_marr_yr = 1 if yrmarr >= 2008 & yrmarr!=.
tab current_marr_yr // so i only have data 2008 + and with that sample, only 27%+ are aligned to those years.

gen sample_marr_yr = .
replace sample_marr_yr = 0 if yrmarr < 1995 & yrmarr!=. // with data on 2008+ only - what percent THERE is in my sample (because the further back I go, the less will be since 1995 also but what am I starting with? aka the maximum)
replace sample_marr_yr = 1 if yrmarr >= 1995 & yrmarr!=.
tab sample_marr_yr // so here we are at 70% plus. 

// okay, I have frm 2000 onward. I think I def can't go back to 1995, will be too many old marriages. Could go back a bit; this feels reasonable. let's see. it's 33% missing so even if all of those were shifted, it'd be like 45 / 55 (55 NOT in my years which is obv an over-estimation)

// do I need to deduplicate? if so would just keep women. let's come back to this later

save "$ACS\acs_2000_2021_marriage-composition.dta", replace

********************************************************************************
* Merge to policy data
********************************************************************************
use "$ACS\acs_2000_2021_marriage-composition.dta", clear

// remember I don't use DC and I stop at 2019 - so could drop those (though I have the policy variables for this whole time period)
tab migrate1, m
tab migrate1d, m

gen state_mover=.
replace state_mover=0 if inlist(migrate1, 1,2)
replace state_mover=1 if inlist(migrate1, 3,4)

// variables to create for t-1 (will use that for married in last year). BUT when I do this I also need to use state LAST YEAR not this year
browse serial pernum year migrate1 state_mover statefip migplac1
gen state_t1 = statefip if state_mover==0
replace state_t1 = migplac1 if state_mover==1
label values state_t1 migplac1_lbl

browse serial pernum year migrate1 state_mover statefip migplac1 state_t1

rename statefip state_fips

local scale_vars "structural_familism structural_factor avg_egal_reg married_pure_male_bw_rate married_women_emp_rate_wt married_earn_ratio women_college_rate_wt broad_policy family_investment" 

// First do T
merge m:1 state_fips year using "$created_data/scale_refresh.dta", keepusing(`scale_vars')
	// tab state_fips _merge // it's DC that is master only (which makes sense)
drop if _merge==2
drop _merge

foreach var in `scale_vars'{
	rename `var' `var'_t
}

// Then T-1
gen year_t1 = year - 1 // so in 2019, i want policy from 2018
merge m:1 year_t1 state_t1 using "$created_data/scale_refresh.dta", keepusing(`scale_vars')
		// tab state_t1 _merge // here they could have lived abroad a year ago so there are more non-matches (now DC + Abroad)
		// tab migrate1 _merge
		// tab state_t1 if _merge==1 & migrate1 !=4
drop if _merge==2
drop _merge

foreach var in `scale_vars'{
	rename `var' `var'_t1
}

save "$ACS\acs_2000_2021_marriage-composition.dta", replace // save with merged policy data so I don't need to redo this

********************************************************************************
* Make some descriptive variables
********************************************************************************
*realizing some of these variables now assume I have restricted to women. right now I do still have two records per HH. Should I just formally drop here? let's explore...
// sort year serial pernum
// browse serial pernum year sex sex_sp educ educ_sp

// education
// recode educd (0/61=1)(62/64=2)(65/81=3)(101/116=4), gen(education)
recode educ (0/5=1)(6=2)(7/9=3)(10/11=4), gen(education)
recode educ_sp (0/5=1)(6=2)(7/9=3)(10/11=4), gen(education_sp)

label define education 1 "LTHS" 2 "HS" 3 "Some College" 4 "College"
label values education education_sp education

gen college=0
replace college=1 if education==4
gen college_sp=0
replace college_sp=1 if education_sp==4

tab college college_sp, m

gen educ_type=.
replace educ_type = 1 if college == 0 & college_sp == 0
replace educ_type = 2 if college == 0 & college_sp == 1
replace educ_type = 3 if college == 1 & college_sp == 0
replace educ_type = 4 if college == 1 & college_sp == 1

label define educ_type 1 "Neither College" 2 "Him College" 3 "Her College" 4 "Both College" // this currently assumes just women. should I formally drop?
label values educ_type educ_type

tab educ_type, m

// race
gen race_gp = .
replace race_gp = 1 if race == 100
replace race_gp = 2 if race == 200
replace race_gp = 3 if race == 300
replace race_gp = 4 if inrange(race,650,652)
replace race_gp = 5 if race == 700
replace race_gp = 6 if inrange(race,801,830)

gen race_gp_sp = .
replace race_gp_sp = 1 if race_sp == 100
replace race_gp_sp = 2 if race_sp == 200
replace race_gp_sp = 3 if race_sp == 300
replace race_gp_sp = 4 if inrange(race_sp,650,652)
replace race_gp_sp = 5 if race_sp == 700
replace race_gp_sp = 6 if inrange(race_sp,801,830)

label define race_gp 1 "White" 2 "Black" 3 "American Indian" 4 "Asian / Pac Is" 5 "Other Single Race" 6 "Multi-racial"
label values race_gp race_gp_sp race_gp

gen hispanic=.
replace hispanic=0 if hispan==0
replace hispanic=1 if inrange(hispan,100,612)

gen hispanic_sp=.
replace hispanic_sp=0 if hispan_sp==0
replace hispanic_sp=1 if inrange(hispan_sp,100,612)

tab race_gp hispanic, m

// this matches PSID
gen raceth = .
replace raceth = 1 if race_gp==1 & (hispanic==0 | hispanic==.) // NH White
replace raceth = 2 if race_gp==2 // Black
replace raceth = 3 if hispanic==1 & race_gp!=2  // Hispanic
replace raceth = 4 if race_gp==4 & (hispanic==0 | hispanic==.) // NH Asian
replace raceth = 5 if inlist(race_gp,3,5,6) & (hispanic==0 | hispanic==.) // NH Other (including Amer Indian, Other, and Multiracial)

gen raceth_sp = .
replace raceth_sp = 1 if race_gp_sp==1 & (hispanic_sp==0 | hispanic_sp==.) // NH White
replace raceth_sp = 2 if race_gp_sp==2 // Black
replace raceth_sp = 3 if hispanic_sp==1 & race_gp_sp!=2  // Hispanic
replace raceth_sp = 4 if race_gp_sp==4 & (hispanic_sp==0 | hispanic_sp==.) // NH Asian
replace raceth_sp = 5 if inlist(race_gp_sp,3,5,6) & (hispanic_sp==0 | hispanic_sp==.) // NH Other (including Amer Indian, Other, and Multiracial)

label define raceth 1 "NH White" 2 "Black" 3 "Hispanic" 4 "NH Asian" 5 "NH Other"
label values raceth raceth_sp raceth

gen same_raceth = 0
replace same_raceth = 1 if raceth == raceth_sp

// employment status
fre labforce
fre empstat

gen employed = 0
replace employed = 1 if inrange(empstat,10,12)

gen employed_sp = 0
replace employed_sp = 1 if inrange(empstat_sp,10,12)

tab labforce employed, m
gen employed_det = 0 if labforce==0 | labforce==1
replace employed_det = 1 if labforce==2 & employed==0
replace employed_det = 2 if labforce==2 & employed==1

gen employed_det_sp = 0 if labforce_sp==0 | labforce_sp==1
replace employed_det_sp = 1 if labforce_sp==2 & employed_sp==0
replace employed_det_sp = 2 if labforce_sp==2 & employed_sp==1

label define employed_det 0 "not in LF" 1 "unemployed" 2 "employed"
label values employed_det employed_det_sp employed_det

// work hours? income?

// include age as well, maybe kids in home?
sum age age_sp nchlt5 nchild

// should I estimate differently by gender? also if I am doing currently married - I need to deduplicate? but married in last year do I also?

********************************************************************************
********************************************************************************
********************************************************************************
**# * Based on marital transitions
********************************************************************************
********************************************************************************
********************************************************************************
use "$ACS\acs_2000_2021_marriage-selection.dta", clear

keep if year>=2008

tab marst marrinyr, m

keep if marst==6 | marrinyr==1

save "$ACS\acs_2008_2021_marriage-entrance.dta", replace
