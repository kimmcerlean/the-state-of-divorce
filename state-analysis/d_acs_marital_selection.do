********************************************************************************
* Project: Work-family policy and divorce
* See if state-policy context affects selection into marriage
* (and thus possibly the effect of state level variable)
* acs_marital_selection.do
* Code owner: Kimberly McErlean
********************************************************************************

// a few challenges. FIrst, I am not sure if I should use marrinyr or marital status
// upon reflection, marital status - because I have the current population of married couples, not just newlyweds
// so, i have more flexibility to go back before 2008 bc marrinyr not until 2008
// BUT year married also not until 2008. I don't want people married in lke 1977, because they aren't in my PSID sample
// so - do I restrict all to 2008 OR see in 2008+, how many marriages "recent" v. old to estimate effects - if like 50/50. prob risky. if not, could possibly use more expansive (right not pulled in starting 2000 so could use that to start to evaluate)
// problem is - is it weirder to match policy to do a currently married analysis? 
// more intuitive to link policy in year of marriage? I guess again, these are different questions and my PSID = all currently married (with some restrictions) so this is most aligned?


********************************************************************************
********************************************************************************
********************************************************************************
**# * Compared to never married
********************************************************************************
********************************************************************************
********************************************************************************

********************************************************************************
********************************************************************************
* Women
********************************************************************************
********************************************************************************

use "$ACS\acs_2000_2021_marriage-selection.dta", clear

keep if sex==2 // let's start with women
keep if age>=18 & age<=55

gen currently_married = .
replace currently_married = 1 if marst==1
replace currently_married = 0 if inrange(marst,2,6)
tab marst currently_married, m

recode educd (0/61=1)(62/64=2)(65/81=3)(101/116=4), gen(education)
label define education 1 "LTHS" 2 "HS" 3 "Some College" 4 "College"
label values education education

gen college=0
replace college=1 if education==4

tab race hispan

gen hispanic=.
replace hispanic=0 if hispan==0
replace hispanic=1 if inrange(hispan,1,4)
tab hispan hispanic, m

tab race hispanic

gen race_gp = .
replace race_gp=1 if race==1 & hispanic==0
replace race_gp=2 if race==2
replace race_gp=3 if hispanic==1 & race!=2
replace race_gp=4 if hispanic==0 & inrange(race,3,9)

label define race_gp 1 "NH White" 2 "Black" 3 "Hispanic" 4 "NH Other"
label values race_gp race_gp

// merge on to policy data - think I only need t in this case?
rename statefip state_fips

local scale_vars "structural_familism structural_factor avg_egal_reg married_pure_male_bw_rate married_women_emp_rate_wt married_earn_ratio women_college_rate_wt evang_rate broad_policy family_investment" 

merge m:1 state_fips year using "$created_data/scale_refresh.dta", keepusing(`scale_vars')
	// tab state_fips _merge
drop if _merge==2
drop _merge

save "$ACS\acs_2000_2021_women-marriage.dta", replace
// use "$ACS\acs_2000_2021_women-marriage.dta", clear

global macro_controls "women_college_rate_wt married_women_emp_rate_wt avg_egal_reg married_pure_male_bw_rate evang_rate"

********************************************************************************
* Education
********************************************************************************

logistic currently_married college
logistic currently_married structural_familism // so better policy = less marriage (aligns with idea that marriage declining in more urban areas prob right?)
sum structural_familism, detail
margins, at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

logistic currently_married i.college c.structural_familism i.college#c.structural_familism // c.age
sum structural_familism, detail
margins, dydx(college) at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
marginsplot
sum structural_familism, detail
margins college, at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
marginsplot

	// Figure to pull for paper use potentially (will pull 6 to decide between) - ugh decide if scale should be 100 or this is fine?
	logistic currently_married i.college c.structural_familism i.college#c.structural_familism
	sum structural_familism, detail
	margins college, at(structural_familism=(`r(p5)' (1) `r(p95)'))
	marginsplot, xtitle("Structural Support for Working Families") ytitle("Predicted Probability of Being Currently Married") title("") legend(position(6) ring(3) rows(1) order(3 "No College Degree" 4 "College Degree")) recast(line) recastci(rarea) xlabel(#10) plot1opts(lcolor("black") mcolor("black") lpattern(dash))  plot2opts(lcolor("black") mcolor("black")) ci1opts(color(gray%50)) ci2opts(color(gray%50)) // noci
	graph export "$results/women_current_marriage_NOcontrols.png", replace
	
	// is this better?
	marginsplot, xtitle("Structural Support for Working Families") ytitle("Predicted Probability of Being Currently Married") title("") legend(position(6) ring(3) rows(1) order(3 "No College Degree" 4 "College Degree")) recast(line) recastci(rarea) xlabel(#10) plot1opts(lcolor("black") mcolor("black") lpattern(dash))  plot2opts(lcolor("black") mcolor("black")) ci1opts(color(gray%50)) ci2opts(color(gray%50))  yscale(range(0 1)) ylabel(0(.1)1)

logistic currently_married i.college c.structural_familism i.college#c.structural_familism /// in this model, main effect of SF goes away - possibly captured by women's education rates?
women_college_rate_wt i.state_fips i.year age i.race i.hispan incwage i.empstat i.nchild
sum structural_familism, detail
margins college, at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
sum structural_familism, detail
margins, dydx(college) at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) // this is helpful to confirm sig
sum structural_familism, detail
margins, at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

	// Figure to pull for paper use potentially (will pull 6 to decide between) - ugh decide if scale should be 100 or this is fine?
	logistic currently_married i.college c.structural_familism i.college#c.structural_familism /// in this model, main effect of SF goes away - possibly captured by women's education rates?
	women_college_rate_wt i.state_fips i.year age i.race i.hispan incwage i.empstat i.nchild
	sum structural_familism, detail
	margins college, at(structural_familism=(`r(p5)' (1) `r(p95)'))
	marginsplot, xtitle("Structural Support for Working Families") ytitle("Predicted Probability of Being Currently Married") title("") legend(position(6) ring(3) rows(1) order(3 "No College Degree" 4 "College Degree")) recast(line) recastci(rarea) xlabel(#10) plot1opts(lcolor("black") mcolor("black") lpattern(dash))  plot2opts(lcolor("black") mcolor("black")) ci1opts(color(gray%50)) ci2opts(color(gray%50)) // noci
	graph export "$results/women_current_marriage_controls.png", replace
	
	// is this better?
	marginsplot, xtitle("Structural Support for Working Families") ytitle("Predicted Probability of Being Currently Married") title("") legend(position(6) ring(3) rows(1) order(3 "No College Degree" 4 "College Degree")) recast(line) recastci(rarea) xlabel(#10) plot1opts(lcolor("black") mcolor("black") lpattern(dash))  plot2opts(lcolor("black") mcolor("black")) ci1opts(color(gray%50)) ci2opts(color(gray%50))  yscale(range(0 1)) ylabel(0(.1)1)

logistic currently_married i.college c.structural_familism i.college#c.structural_familism /// let's do JUST demo controls
age i.race i.hispan incwage i.empstat i.nchild
sum structural_familism, detail
margins college, at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
sum structural_familism, detail
margins, dydx(college) at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) // this is helpful to confirm sig
sum structural_familism, detail
margins, at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

// can I figure out which is driving the policy effect down?
logistic currently_married i.college c.structural_familism i.college#c.structural_familism /// just state fips
i.state_fips
sum structural_familism, detail
margins college, at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

logistic currently_married i.college c.structural_familism i.college#c.structural_familism /// just state fips AND year
i.state_fips i.year
sum structural_familism, detail
margins college, at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

logistic currently_married i.college c.structural_familism i.college#c.structural_familism /// just year
i.year

logistic currently_married i.college c.structural_familism i.college#c.structural_familism /// just educ rate
women_college_rate_wt
sum structural_familism, detail
margins college, at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

***************
* For paper
***************

logistic currently_married i.college c.structural_familism i.college#c.structural_familism ///
$macro_controls i.state_fips i.year age i.race i.hispan incwage i.empstat i.nchild
sum structural_familism, detail
margins college, at(structural_familism=(`r(p5)' (1) `r(p95)'))
marginsplot, xtitle("Structural Support for Working Families") ytitle("Predicted Probability of Being Currently Married") title("") legend(position(6) ring(3) rows(1) order(3 "No College Degree" 4 "College Degree")) recast(line) recastci(rarea) xlabel(#10) plot1opts(lcolor("black") mcolor("black") lpattern(dash))  plot2opts(lcolor("black") mcolor("black")) ci1opts(color(gray%50)) ci2opts(color(gray%50))  yscale(range(0 1)) ylabel(0(.1)1)
graph export "$results/women_current_marriage_controls_vF.png", replace
	

// might need to export full models - a useful comparison could be effect size - like is this effect size similar to or smaller than other correlates like age or employment status? I think it will be *smaller* - but I also think these are just small differences it should be fine; they are just significant because i have millions of people. i think what i am putting in excel is fine for now. can come back to this.

********************************************************************************
* Race
********************************************************************************
// check the same for race??
logistic currently_married i.race_gp

// no controls
logistic currently_married i.race_gp c.structural_familism i.race_gp#c.structural_familism if race_gp!=4
sum structural_familism, detail
margins race_gp, at(structural_familism=(`r(p5)' (1) `r(p95)'))
marginsplot, xtitle("Structural Support for Working Families") ytitle("Predicted Probability of Being Currently Married") title("") legend(position(6) ring(3) rows(1) order(4 "NH White" 5 "Black" 6 "Hispanic")) recast(line) recastci(rarea) xlabel(#10) plot1opts(lcolor("black") mcolor("black") lpattern(solid))  plot2opts(lcolor("black") mcolor("black") lpattern(dash))  plot3opts(lcolor("black") mcolor("black") lpattern(dot))  ci1opts(color(gray%50)) ci2opts(color(gray%50)) ci3opts(color(gray%50)) // yscale(range(0 1)) ylabel(0(.1)1) plot4opts(lcolor("gs12") mcolor("gs12")) ci4opts(color(gray%50))  
marginsplot, xtitle("Structural Support for Working Families") ytitle("Predicted Probability of Being Currently Married") title("") legend(position(6) ring(3) rows(1) order(4 "NH White" 5 "Black" 6 "Hispanic")) recast(line) recastci(rarea) xlabel(#10) plot1opts(lcolor("black") mcolor("black") lpattern(solid))  plot2opts(lcolor("black") mcolor("black") lpattern(dash))  plot3opts(lcolor("black") mcolor("black") lpattern(dot)) ci1opts(color(gray%50)) ci2opts(color(gray%50)) ci3opts(color(gray%50)) yscale(range(0 1)) ylabel(0(.1)1) //  plot4opts(lcolor("gs12") mcolor("gs12")) ci4opts(color(gray%50)) 
graph export "$results/women_current_marriage_race_NOcontrols.png", replace

// with controls. wait why am i not doing all?? was it too much?
logistic currently_married i.race_gp c.structural_familism i.race_gp#c.structural_familism /// 
women_college_rate_wt i.state_fips i.year age i.education i.hispan incwage i.empstat i.nchild if race_gp!=4 // replace race with education as control DUH
sum structural_familism, detail
margins race_gp, at(structural_familism=(`r(p5)' (1) `r(p95)'))
marginsplot, xtitle("Structural Support for Working Families") ytitle("Predicted Probability of Being Currently Married") title("") legend(position(6) ring(3) rows(1) order(4 "NH White" 5 "Black" 6 "Hispanic")) recast(line) recastci(rarea) xlabel(#10) plot1opts(lcolor("black") mcolor("black") lpattern(solid))  plot2opts(lcolor("black") mcolor("black") lpattern(dash))  plot3opts(lcolor("black") mcolor("black") lpattern(dot)) ci1opts(color(gray%50)) ci2opts(color(gray%50)) ci3opts(color(gray%50)) yscale(range(0 1)) ylabel(0(.1)1) //  plot4opts(lcolor("gs12") mcolor("gs12")) ci4opts(color(gray%50)) 
graph export "$results/women_current_marriage_race_controls.png", replace

***************
* For paper
***************

logistic currently_married i.race_gp c.structural_familism i.race_gp#c.structural_familism /// 
$macro_controls i.state_fips i.year age i.education i.hispan incwage i.empstat i.nchild if inlist(race_gp,1,2) // just Black/White to reflect PSID sample
sum structural_familism, detail
margins race_gp, at(structural_familism=(`r(p5)' (1) `r(p95)'))
marginsplot, xtitle("Structural Support for Working Families") ytitle("Predicted Probability of Being Currently Married") title("") legend(position(6) ring(3) rows(1) order(3 "NH White" 4 "Black")) recast(line) recastci(rarea) xlabel(#10) plot1opts(lcolor("black") mcolor("black") lpattern(dash))  plot2opts(lcolor("black") mcolor("black")) ci1opts(color(gray%50)) ci2opts(color(gray%50))  yscale(range(0 1)) ylabel(0(.1)1)
graph export "$results/women_current_marriage_race_controls_vF.png", replace

// save "$ACS\acs_2000_2021_women-marriage.dta", replace

********************************************************************************
********************************************************************************
* Men
********************************************************************************
********************************************************************************
log using "$logdir/acs_men_currentmarriage.log", replace // doing this so I can run and revisit but not sure it will fit in window so export log

use "$ACS\acs_2000_2021_marriage-selection.dta", clear

keep if sex==1
keep if age>=18 & age<=55

gen currently_married = .
replace currently_married = 1 if marst==1
replace currently_married = 0 if inrange(marst,2,6)
tab marst currently_married, m

recode educd (0/61=1)(62/64=2)(65/81=3)(101/116=4), gen(education)
label define education 1 "LTHS" 2 "HS" 3 "Some College" 4 "College"
label values education education

gen college=0
replace college=1 if education==4

tab race hispan

gen hispanic=.
replace hispanic=0 if hispan==0
replace hispanic=1 if inrange(hispan,1,4)
tab hispan hispanic, m

tab race hispanic

gen race_gp = .
replace race_gp=1 if race==1 & hispanic==0
replace race_gp=2 if race==2
replace race_gp=3 if hispanic==1 & race!=2
replace race_gp=4 if hispanic==0 & inrange(race,3,9)

label define race_gp 1 "NH White" 2 "Black" 3 "Hispanic" 4 "NH Other"
label values race_gp race_gp

tab race_gp, m

// merge on to policy data - think I only need t in this case?
rename statefip state_fips

local scale_vars "structural_familism structural_factor avg_egal_reg married_pure_male_bw_rate married_women_emp_rate_wt married_earn_ratio women_college_rate_wt evang_rate broad_policy family_investment" 

merge m:1 state_fips year using "$created_data/scale_refresh.dta", keepusing(`scale_vars')
	tab state_fips _merge
drop if _merge==2
drop _merge

save "$ACS\acs_2000_2021_men-marriage.dta", replace
// use "$ACS\acs_2000_2021_men-marriage.dta", clear

global macro_controls "women_college_rate_wt married_women_emp_rate_wt avg_egal_reg married_pure_male_bw_rate evang_rate"

********************************************************************************
* Education
********************************************************************************

logistic currently_married college
logistic currently_married structural_familism // so better policy = less marriage (aligns with idea that marriage declining in more urban areas prob right?)
sum structural_familism, detail
margins, at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

logistic currently_married i.college c.structural_familism i.college#c.structural_familism // c.age
sum structural_familism, detail
margins college, at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
sum structural_familism, detail
margins, dydx(college) at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

	// potential figure
	logistic currently_married i.college c.structural_familism i.college#c.structural_familism 
	sum structural_familism, detail
	margins college, at(structural_familism=(`r(p5)' (1) `r(p95)'))
	marginsplot, xtitle("Structural Support for Working Families") ytitle("Predicted Probability of Being Currently Married") title("") legend(position(6) ring(3) rows(1) order(3 "No College Degree" 4 "College Degree")) recast(line) recastci(rarea) xlabel(#10) plot1opts(lcolor("black") mcolor("black") lpattern(dash))  plot2opts(lcolor("black") mcolor("black")) ci1opts(color(gray%50)) ci2opts(color(gray%50)) // noci
	graph export "$results/men_current_marriage_NOcontrols.png", replace
	
	// is this scale better?
	marginsplot, xtitle("Structural Support for Working Families") ytitle("Predicted Probability of Being Currently Married") title("") legend(position(6) ring(3) rows(1) order(3 "No College Degree" 4 "College Degree")) recast(line) recastci(rarea) xlabel(#10) plot1opts(lcolor("black") mcolor("black") lpattern(dash))  plot2opts(lcolor("black") mcolor("black")) ci1opts(color(gray%50)) ci2opts(color(gray%50)) yscale(range(0 1)) ylabel(0(.1)1)

logistic currently_married i.college c.structural_familism i.college#c.structural_familism /// in this model, main effect of SF goes away - possibly captured by women's education rates?
women_college_rate_wt i.state_fips i.year age i.race i.hispan incwage i.empstat i.nchild
sum structural_familism, detail
margins college, at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
sum structural_familism, detail
margins, dydx(college) at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) // this is helpful to confirm sig
sum structural_familism, detail
margins, at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

	// potential figure
	logistic currently_married i.college c.structural_familism i.college#c.structural_familism /// in this model, main effect of SF goes away - possibly captured by women's education rates?
	women_college_rate_wt i.state_fips i.year age i.race i.hispan incwage i.empstat i.nchild
	sum structural_familism, detail
	margins college, at(structural_familism=(`r(p5)' (1) `r(p95)'))
	marginsplot, xtitle("Structural Support for Working Families") ytitle("Predicted Probability of Being Currently Married") title("") legend(position(6) ring(3) rows(1) order(3 "No College Degree" 4 "College Degree")) recast(line) recastci(rarea) xlabel(#10) plot1opts(lcolor("black") mcolor("black") lpattern(dash))  plot2opts(lcolor("black") mcolor("black")) ci1opts(color(gray%50)) ci2opts(color(gray%50)) // noci
	graph export "$results/men_current_marriage_controls.png", replace

	// is this scale better?
	marginsplot, xtitle("Structural Support for Working Families") ytitle("Predicted Probability of Being Currently Married") title("") legend(position(6) ring(3) rows(1) order(3 "No College Degree" 4 "College Degree")) recast(line) recastci(rarea) xlabel(#10) plot1opts(lcolor("black") mcolor("black") lpattern(dash))  plot2opts(lcolor("black") mcolor("black")) ci1opts(color(gray%50)) ci2opts(color(gray%50)) yscale(range(0 1)) ylabel(0(.1)1)
		
logistic currently_married i.college c.structural_familism i.college#c.structural_familism /// let's do JUST demo controls
age i.race i.hispan incwage i.empstat i.nchild
sum structural_familism, detail
margins college, at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
sum structural_familism, detail
margins, dydx(college) at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) // this is helpful to confirm sig
sum structural_familism, detail
margins, at(structural_familism=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

***************
* For paper
***************

logistic currently_married i.college c.structural_familism i.college#c.structural_familism ///
$macro_controls i.state_fips i.year age i.race i.hispan incwage i.empstat i.nchild
sum structural_familism, detail
margins college, at(structural_familism=(`r(p5)' (1) `r(p95)'))
marginsplot, xtitle("Structural Support for Working Families") ytitle("Predicted Probability of Being Currently Married") title("") legend(position(6) ring(3) rows(1) order(3 "No College Degree" 4 "College Degree")) recast(line) recastci(rarea) xlabel(#10) plot1opts(lcolor("black") mcolor("black") lpattern(dash))  plot2opts(lcolor("black") mcolor("black")) ci1opts(color(gray%50)) ci2opts(color(gray%50))  yscale(range(0 1)) ylabel(0(.1)1)
graph export "$results/men_current_marriage_controls_vF.png", replace

********************************************************************************
* Race
********************************************************************************

// check the same for race??
logistic currently_married i.race_gp

// no controls
logistic currently_married i.race_gp c.structural_familism i.race_gp#c.structural_familism if race_gp!=4
sum structural_familism, detail
margins race_gp, at(structural_familism=(`r(p5)' (1) `r(p95)'))
marginsplot, xtitle("Structural Support for Working Families") ytitle("Predicted Probability of Being Currently Married") title("") legend(position(6) ring(3) rows(1) order(4 "NH White" 5 "Black" 6 "Hispanic")) recast(line) recastci(rarea) xlabel(#10) plot1opts(lcolor("black") mcolor("black") lpattern(solid))  plot2opts(lcolor("black") mcolor("black") lpattern(dash))  plot3opts(lcolor("black") mcolor("black") lpattern(dot))  ci1opts(color(gray%50)) ci2opts(color(gray%50)) ci3opts(color(gray%50)) // yscale(range(0 1)) ylabel(0(.1)1) plot4opts(lcolor("gs12") mcolor("gs12")) ci4opts(color(gray%50))  
marginsplot, xtitle("Structural Support for Working Families") ytitle("Predicted Probability of Being Currently Married") title("") legend(position(6) ring(3) rows(1) order(4 "NH White" 5 "Black" 6 "Hispanic")) recast(line) recastci(rarea) xlabel(#10) plot1opts(lcolor("black") mcolor("black") lpattern(solid))  plot2opts(lcolor("black") mcolor("black") lpattern(dash))  plot3opts(lcolor("black") mcolor("black") lpattern(dot)) ci1opts(color(gray%50)) ci2opts(color(gray%50)) ci3opts(color(gray%50)) yscale(range(0 1)) ylabel(0(.1)1) //  plot4opts(lcolor("gs12") mcolor("gs12")) ci4opts(color(gray%50)) 
graph export "$results/men_current_marriage_race_NOcontrols.png", replace

// holding off here for now (3/13/26)
// with controls. wait why am i not doing all?? was it too much?
logistic currently_married i.race_gp c.structural_familism i.race_gp#c.structural_familism /// 
women_college_rate_wt i.state_fips i.year age i.education i.hispan incwage i.empstat i.nchild if race_gp!=4 // replace race with education as control DUH
sum structural_familism, detail
margins race_gp, at(structural_familism=(`r(p5)' (1) `r(p95)'))
marginsplot, xtitle("Structural Support for Working Families") ytitle("Predicted Probability of Being Currently Married") title("") legend(position(6) ring(3) rows(1) order(4 "NH White" 5 "Black" 6 "Hispanic")) recast(line) recastci(rarea) xlabel(#10) plot1opts(lcolor("black") mcolor("black") lpattern(solid))  plot2opts(lcolor("black") mcolor("black") lpattern(dash))  plot3opts(lcolor("black") mcolor("black") lpattern(dot)) ci1opts(color(gray%50)) ci2opts(color(gray%50)) ci3opts(color(gray%50)) yscale(range(0 1)) ylabel(0(.1)1) //  plot4opts(lcolor("gs12") mcolor("gs12")) ci4opts(color(gray%50)) 
graph export "$results/men_current_marriage_race_controls.png", replace

***************
* For paper
***************

logistic currently_married i.race_gp c.structural_familism i.race_gp#c.structural_familism /// 
$macro_controls i.state_fips i.year age i.education i.hispan incwage i.empstat i.nchild if inlist(race_gp,1,2) // just Black/White to reflect PSID sample
sum structural_familism, detail
margins race_gp, at(structural_familism=(`r(p5)' (1) `r(p95)'))
marginsplot, xtitle("Structural Support for Working Families") ytitle("Predicted Probability of Being Currently Married") title("") legend(position(6) ring(3) rows(1) order(3 "NH White" 4 "Black")) recast(line) recastci(rarea) xlabel(#10) plot1opts(lcolor("black") mcolor("black") lpattern(dash))  plot2opts(lcolor("black") mcolor("black")) ci1opts(color(gray%50)) ci2opts(color(gray%50))  yscale(range(0 1)) ylabel(0(.1)1)
graph export "$results/men_current_marriage_race_controls_vF.png", replace

log close

********************************************************************************
********************************************************************************
********************************************************************************
**# Let's export coefficients so I have
********************************************************************************
********************************************************************************
********************************************************************************

log using "$logdir/acs_marriage_coefficients_vF.log", replace

********************************************************************************
* Women
********************************************************************************

use "$ACS\acs_2000_2021_women-marriage.dta", clear

global macro_controls "women_college_rate_wt married_women_emp_rate_wt avg_egal_reg married_pure_male_bw_rate evang_rate"

// education
logistic currently_married i.college c.structural_familism i.college#c.structural_familism ///
$macro_controls i.state_fips i.year age i.race i.hispan incwage i.empstat i.nchild

// race
logistic currently_married i.race_gp c.structural_familism i.race_gp#c.structural_familism /// 
$macro_controls i.state_fips i.year age i.education i.hispan incwage i.empstat i.nchild if inlist(race_gp,1,2) 


********************************************************************************
* Men 
********************************************************************************

use "$ACS\acs_2000_2021_men-marriage.dta", clear

global macro_controls "women_college_rate_wt married_women_emp_rate_wt avg_egal_reg married_pure_male_bw_rate evang_rate"

// education
logistic currently_married i.college c.structural_familism i.college#c.structural_familism ///
$macro_controls i.state_fips i.year age i.race i.hispan incwage i.empstat i.nchild

// race
logistic currently_married i.race_gp c.structural_familism i.race_gp#c.structural_familism /// 
$macro_controls i.state_fips i.year age i.education i.hispan incwage i.empstat i.nchild if inlist(race_gp,1,2) 

log close

********************************************************************************
********************************************************************************
********************************************************************************
**# * Based on marital transitions
********************************************************************************
********************************************************************************
********************************************************************************
use "$ACS\acs_2000_2021_marriage-selection.dta", clear

keep if year>=2008

fre marrinyr
tab marst marrinyr, m
tab age marrinyr
drop if age < 15 // not asked below age of 15
tab age marrinyr, row

keep if age>=20 & age<=49 // want to try to restrict to 1st marriages, acknowledging men prob have higher age range [older prob likely to be remarriage]

keep if marst==6 | marrinyr==2

gen married=.
replace married = 0 if marrinyr==1
replace married = 1 if marrinyr==2

recode educd (0/61=1)(62/64=2)(65/81=3)(101/116=4), gen(education)
label define education 1 "LTHS" 2 "HS" 3 "Some College" 4 "College"
label values education education

gen college=0
replace college=1 if education==4

tab race hispan

gen hispanic=.
replace hispanic=0 if hispan==0
replace hispanic=1 if inrange(hispan,1,4)
tab hispan hispanic, m

tab race hispanic

gen race_gp = .
replace race_gp=1 if race==1 & hispanic==0
replace race_gp=2 if race==2
replace race_gp=3 if hispanic==1 & race!=2
replace race_gp=4 if hispanic==0 & inrange(race,3,9)

label define race_gp 1 "NH White" 2 "Black" 3 "Hispanic" 4 "NH Other"
label values race_gp race_gp

** merge on to policy data - here, I think I need to do t AND t-1 [and probably USE t-1 given it's married in last year]
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

local scale_vars "structural_familism structural_factor avg_egal_reg married_pure_male_bw_rate married_women_emp_rate_wt married_earn_ratio women_college_rate_wt evang_rate broad_policy family_investment" 

// First do T
merge m:1 state_fips year using "$created_data/scale_refresh.dta", keepusing(`scale_vars')
	tab state_fips _merge
drop if _merge==2
drop _merge

foreach var in `scale_vars'{
	rename `var' `var'_t
}

// Then T-1
gen year_t1 = year - 1 // so in 2019, i want policy from 2018

merge m:1 year_t1 state_t1 using "$created_data/scale_refresh.dta", keepusing(`scale_vars')
		tab state_t1 _merge // here they could have lived abroad a year ago so there are more non-matches (now DC + Abroad)
		tab migrate1 _merge
		tab state_t1 if _merge==1 & migrate1 !=4
drop if _merge==2
drop _merge

foreach var in `scale_vars'{
	rename `var' `var'_t1
}

save "$ACS\acs_2008_2021_marriage-entrance.dta", replace

** Models just for women
logistic married college if sex==2
logistic married structural_familism_t1 if sex==2 // so better policy = less marriage (as with currently married trends). I think I thought htis would reduce that here bc of age BUT this still doesn't account for age specific rates aka age structure of states so if marriage is geting later in states with more  policy that will reflect age structure of eligible population and that is still not addressed here. do AGE specific rates? let's reflect on this.
sum structural_familism_t1, detail
margins, at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

logistic married i.college c.structural_familism_t1 i.college#c.structural_familism_t1 if sex==2 // c.age
sum structural_familism_t1, detail
margins college, at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
sum structural_familism_t1, detail
margins, dydx(college) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

	// potential figure
	logistic married i.college c.structural_familism_t1 i.college#c.structural_familism_t1 if sex==2 // c.age
	sum structural_familism_t1, detail
	margins college, at(structural_familism_t1=(`r(p5)' (1) `r(p95)'))
	marginsplot, xtitle("Structural Support for Working Families") ytitle("Predicted Probability of Marriage in Last Year") title("") legend(position(6) ring(3) rows(1) order(3 "No College Degree" 4 "College Degree")) recast(line) recastci(rarea) xlabel(#10) plot1opts(lcolor("black") mcolor("black") lpattern(dash))  plot2opts(lcolor("black") mcolor("black")) ci1opts(color(gray%50)) ci2opts(color(gray%50)) // noci
	graph export "$results/women_newly_married_NOcontrols.png", replace

// is adding age sufficient to address age structure? do I need to interact age with something?
logistic married i.college c.structural_familism_t1 i.college#c.structural_familism_t1 c.age if sex==2
sum structural_familism_t1, detail
margins college, at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
sum structural_familism_t1, detail
margins, dydx(college) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

// full model
logistic married i.college c.structural_familism_t1 i.college#c.structural_familism_t1 /// 
women_college_rate_wt_t1 i.state_fips i.year age i.race i.hispan incwage i.empstat i.nchild if sex==2 
sum structural_familism_t1, detail
margins college, at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))
sum structural_familism_t1, detail
margins, dydx(college) at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)')) // this is helpful to confirm sig
sum structural_familism_t1, detail
margins, at(structural_familism_t1=(`r(p5)' `r(p25)' `r(p50)' `r(p75)' `r(p95)'))

	// potential figure
	logistic married i.college c.structural_familism_t1 i.college#c.structural_familism_t1 /// 
	women_college_rate_wt_t1 i.state_fips i.year age i.race i.hispan incwage i.empstat i.nchild if sex==2 
	sum structural_familism_t1, detail
	margins college, at(structural_familism_t1=(`r(p5)' (1) `r(p95)'))
	marginsplot, xtitle("Structural Support for Working Families") ytitle("Predicted Probability of Marriage in Last Year") title("") legend(position(6) ring(3) rows(1) order(3 "No College Degree" 4 "College Degree")) recast(line) recastci(rarea) xlabel(#10) plot1opts(lcolor("black") mcolor("black") lpattern(dash))  plot2opts(lcolor("black") mcolor("black")) ci1opts(color(gray%50)) ci2opts(color(gray%50)) // noci
	graph export "$results/women_newly_married_controls.png", replace
	
	// no this really doesn't work lol
	// marginsplot, xtitle("Structural Support for Working Families") ytitle("Predicted Probability of Marriage in Last Year") title("") legend(position(6) ring(3) rows(1) order(3 "No College Degree" 4 "College Degree")) recast(line) recastci(rarea) xlabel(#10) plot1opts(lcolor("black") mcolor("black") lpattern(dash))  plot2opts(lcolor("black") mcolor("black")) ci1opts(color(gray%50)) ci2opts(color(gray%50)) yscale(range(0 1)) ylabel(0(.1)1)

** Explore if RACE results same
logistic married i.race_gp if sex==2

// no controls
// without controls, the story for race IS a little different than for education. I think also the coefficient for black is larger than for college. BUT the differences are, in fact, smaller when policy is more supportive. this is more driven by the effects on white v. black. like marriage rates decline for all as policy increases - the declines are steeper for whites. so, i guess that DOES change the composition of the population. (less support = bigger gaps, so less blacks respresented). let's see if changes once controls added in next model [because then can say can control away]
logistic married i.race_gp c.structural_familism_t1 i.race_gp#c.structural_familism_t1 if sex==2 & race_gp!=4 // c.age
sum structural_familism_t1, detail
margins race_gp, at(structural_familism_t1=(`r(p5)' (1) `r(p95)'))
marginsplot, xtitle("Structural Support for Working Families") ytitle("Predicted Probability of Marriage in Last Year") title("") legend(position(6) ring(3) rows(1) order(4 "NH White" 5 "Black" 6 "Hispanic")) recast(line) recastci(rarea) xlabel(#10) plot1opts(lcolor("black") mcolor("black") lpattern(solid))  plot2opts(lcolor("black") mcolor("black") lpattern(dash))  plot3opts(lcolor("black") mcolor("black") lpattern(dot))  ci1opts(color(gray%50)) ci2opts(color(gray%50)) ci3opts(color(gray%50)) // yscale(range(0 1)) ylabel(0(.1)1) // noci
graph export "$results/women_newly_married_race_NOcontrols.png", replace

// with controls - okay interesting, so here, white and hispanic become further apart but no gap (hispanic marriage rate declines drastically once adjusted). and then no real change with policy BUT for black women now their risk marriage INCREASEs with policy so the gap becomes narrower with whites. i still don't know that, esp with controls, it's that dramatic - it's like 1ppt. I guess for a base of BLACK it's quite large but for the GAP it's quite small. but there is a more definitely skew here than for education. like in the long-run, not sure dramatic, but okay. it's interesting because the effects don't exist for CURRENTLY married JUST newly married. let's reflect on this
logistic married i.race_gp c.structural_familism_t1 i.race_gp#c.structural_familism_t1 /// 
women_college_rate_wt_t1 i.state_fips i.year age i.education i.hispan incwage i.empstat i.nchild if sex==2 & race_gp!=4 // replace race control with education
sum structural_familism_t1, detail
margins race_gp, at(structural_familism_t1=(`r(p5)' (1) `r(p95)'))
marginsplot, xtitle("Structural Support for Working Families") ytitle("Predicted Probability of Marriage in Last Year") title("") legend(position(6) ring(3) rows(1) order(4 "NH White" 5 "Black" 6 "Hispanic")) recast(line) recastci(rarea) xlabel(#10) plot1opts(lcolor("black") mcolor("black") lpattern(solid))  plot2opts(lcolor("black") mcolor("black") lpattern(dash))  plot3opts(lcolor("black") mcolor("black") lpattern(dot))  ci1opts(color(gray%50)) ci2opts(color(gray%50)) ci3opts(color(gray%50)) // yscale(range(0 1)) ylabel(0(.1)1) // noci 
graph export "$results/women_newly_married_race_controls.png", replace

