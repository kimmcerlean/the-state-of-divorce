********************************************************************************
* Marriage trends by state and education over time
* state_over_time_marriage-last-yr.do
* Kim McErlean
********************************************************************************

use "$ACS\acs_1980_2019_marriages_lastyr_state.dta", clear

// calculate year of marriage for 1980 to get if married in last year
gen yrborn = year - age
replace yrmarr=yrborn+agemarr if year==1980 & agemarr!=0

browse year serial pernum age yrborn agemarr marrinyr yrmarr
replace marrinyr=2 if year==1980 & inlist(yrmarr,1979,1980)
replace marrinyr=0 if marrinyr==.

// getting base - never-married adults over the age of 15. browse year serial pernum marst marrinyr
// is this the base, should it be transition rate by education? OR out of those who married in past year (so that is the base), what type of HH was it? or both...
keep if age >=15
keep if marst==6 | marrinyr==2

// ugh duh, marrinyr not asked until 2008 - so does limit understanding. 1980 also asked age at marriage so can be calculated
keep if year==1980 | year >=2008

// denote who got married
gen married=0
replace married=1 if marrinyr==2

// education recode: college v not
recode educ (0/5=1)(6=2)(7/8=3)(10/11=4), gen(education)
label define education 1 "LTHS" 2 "HS" 3 "Some College" 4 "College"
label values education education 

gen college=0
replace college=1 if education==4

regress married i.statefip if year>=2008, nocons
margins statefip
marginsplot, xlabel(1(1)56, labsize(tiny) angle(45))

regress married i.statefip if year>=2008 & college==0, nocons
margins statefip
marginsplot, xlabel(1(1)56, labsize(tiny) angle(45))

regress married i.statefip if year>=2008 & college==1, nocons
margins statefip
marginsplot, xlabel(1(1)56, labsize(tiny) angle(45))

regress married i.statefip i.college i.statefip#i.college if year>=2008, nocons
margins statefip#college
marginsplot, xlabel(1(1)56, labsize(tiny) angle(45))


// flags to get counts for when I aggregate.
*total
gen total=1

*total college
* use college variable

*total no college
gen no_college=0
replace no_college=1 if college==0

*total who married
* use married

*total married college
gen married_college=0
replace married_college=1 if college==1 & married==1

*total married no college
gen married_non=0
replace married_non=1 if college==0 & married==1

*women
gen women=0
replace women=1 if sex==2

*women college
gen women_college=0
replace women_college=1 if sex==2 & college==1

*women no college
gen women_non=0
replace women_non=1 if sex==2 & college==0

*women married
gen women_married=0
replace women_married=1 if sex==2 & married==1

*women married college
gen women_college_married=0
replace women_college_married=1 if sex==2 & college==1 & married==1

*women married no college
gen women_non_married=0
replace women_non_married=1 if sex==2 & college==0 & married==1

*men
gen men=0
replace men=1 if sex==1

*men college
gen men_college=0
replace men_college=1 if sex==1 & college==1

*men no college
gen men_non=0
replace men_non=1 if sex==1 & college==0

*men married
gen men_married=0
replace men_married=1 if sex==1 & married==1

*men married college
gen men_college_married=0
replace men_college_married=1 if sex==1 & college==1 & married==1

*men married no college
gen men_non_married=0
replace men_non_married=1 if sex==1 & college==0 & married==1

// aggregate
preserve

collapse (sum) 	total college no_college married married_college 			///
				married_non women women_college women_non women_married 	///
				women_college_married women_non_married men men_college 	///
				men_non men_married men_college_married					///
				men_non_married,												///
by(statefip region year)

// metrics
gen rt_total=			married/total
gen rt_college=			married_college / college
gen rt_non=				married_non/no_college
gen rt_women= 			women_married/women
gen rt_women_college =	women_college_married/women_college
gen rt_women_non=		women_non_married/women_non
gen rt_men			=	men_married/men
gen rt_men_college = 	men_college_married/men_college
gen rt_men_non = 		men_non_married / men_non

save "$created_data\state_married_last_yr_history.dta", replace

// restore

********************************************************************************
* State-level analysis
********************************************************************************

gen women_college_non_ratio = rt_women_non / rt_women_college
bysort statefip (year): egen avg_ratio = mean(women_college_non_ratio) 

// to make graphs easier, replacing 1980 as 2007
gen year_real=year
replace year=2007 if year==1980

twoway (line rt_women_college year) (line rt_women_non year) if statefip==48
twoway (line rt_women_college year) (line rt_women_non year), by(statefip) ylabel(.6(.1)1)
twoway (line rt_women_college year) (line rt_women_non year), yscale(range(.6 1))

twoway (line rt_women_college year) (line rt_women_non year) if statefip<=12, by(statefip)
twoway (line rt_women_college year) (line rt_women_non year) if statefip>12 & statefip<=23, by(statefip)
twoway (line rt_women_college year) (line rt_women_non year) if statefip>23 & statefip<=34, by(statefip)
twoway (line rt_women_college year) (line rt_women_non year) if statefip>35 & statefip<=45, by(statefip)
twoway (line rt_women_college year) (line rt_women_non year) if statefip>45 & statefip<=56, by(statefip)

twoway (line women_college_non_ratio year), by(statefip)
twoway (line women_college_non_ratio year) if statefip<=12, by(statefip)
twoway (line women_college_non_ratio year) if statefip>12 & statefip<=23, by(statefip)
twoway (line women_college_non_ratio year) if statefip>23 & statefip<=34, by(statefip)
twoway (line women_college_non_ratio year) if statefip>35 & statefip<=45, by(statefip)
twoway (line women_college_non_ratio year) if statefip>45 & statefip<=56, by(statefip)

twoway (line rt_women year) if statefip<=12 & year>=2008, by(statefip)
twoway (line rt_women year) if statefip>12 & statefip<=23 & year>=2008, by(statefip)
twoway (line rt_women year) if statefip>23 & statefip<=34 & year>=2008, by(statefip)
twoway (line rt_women year) if statefip>35 & statefip<=45 & year>=2008, by(statefip)
twoway (line rt_women year) if statefip>45 & statefip<=56 & year>=2008, by(statefip)

twoway (bar rt_women_college statefip, fcolor(%50)) (bar rt_women_non statefip, fcolor(%30)) if year==2008, xlabel(1(1)56, valuelabel labsize(tiny) angle(45))

browse statefip year rt_women_college rt_women_non women_college_non_ratio

// long-term change
gen start_ratio=women_college_non_ratio if year==2007
bysort statefip (start_ratio): replace start_ratio=start_ratio[1]

gen end_ratio=women_college_non_ratio if year==2018
bysort statefip (end_ratio): replace end_ratio=end_ratio[1]

sort statefip year
gen ratio_change = (end_ratio-start_ratio) / start_ratio
browse statefip year women_college_non_ratio start_ratio end_ratio ratio_change

twoway bar ratio_change statefip, xlabel(1(1)56, valuelabel labsize(tiny) angle(45))

// short-term change
gen start_ratio_st=women_college_non_ratio if year==2008
bysort statefip (start_ratio_st): replace start_ratio_st=start_ratio_st[1]

sort statefip year
gen ratio_change_st = (end_ratio-start_ratio_st) / start_ratio_st
browse statefip year women_college_non_ratio start_ratio start_ratio_st end_ratio ratio_change ratio_change_st

twoway bar ratio_change statefip, xlabel(1(1)56, valuelabel labsize(tiny) angle(45))
twoway bar ratio_change_st statefip, xlabel(1(1)56, valuelabel labsize(tiny) angle(45))

twoway bar avg_ratio statefip, xlabel(1(1)56, valuelabel labsize(tiny) angle(45))

/// adding in incidence of hh type across all current marriages in a state

merge 1:1 statefip year using "$created_data\state_all_marriages_hhtype.dta", keepusing(x_married_dual x_married_malebw x_married_malesole x_married_dual_non x_married_malebw_non x_married_malesole_non x_married_dual_coll x_married_malebw_coll x_married_malesole_coll)
drop if _merge==2 // extra years
drop _merge

twoway (line rt_women year) (line x_married_dual year, yaxis(2)) if statefip<=12 & statefip!=11 & year>=2008, by(statefip)
twoway (line rt_women year) (line x_married_dual year, yaxis(2)) if statefip>12 & statefip<=23 & year>=2008, by(statefip)
twoway (line rt_women year) (line x_married_dual year, yaxis(2)) if statefip>23 & statefip<=34 & year>=2008, by(statefip)
twoway (line rt_women year) (line x_married_dual year, yaxis(2)) if statefip>35 & statefip<=45 & year>=2008, by(statefip)
twoway (line rt_women year) (line x_married_dual year, yaxis(2)) if statefip>45 & statefip<=56 & year>=2008, by(statefip)

// are states with an inverse association states with a higher proportion of non-college educated? think I need to account for this in some way.

twoway (line rt_women_non year) (line x_married_dual year, yaxis(2)) if statefip<=12 & statefip!=11 & year>=2009, by(statefip)
twoway (line rt_women_non year) (line x_married_dual year, yaxis(2)) if statefip>12 & statefip<=23 & year>=2009, by(statefip)
twoway (line rt_women_non year) (line x_married_dual year, yaxis(2)) if statefip>23 & statefip<=34 & year>=2009, by(statefip)
twoway (line rt_women_non year) (line x_married_dual year, yaxis(2)) if statefip>35 & statefip<=45 & year>=2009, by(statefip)
twoway (line rt_women_non year) (line x_married_dual year, yaxis(2)) if statefip>45 & statefip<=56 & year>=2009, by(statefip)

twoway (line rt_women_college year) (line x_married_dual year, yaxis(2)) if statefip<=12 & statefip!=11 & year>=2009, by(statefip)
twoway (line rt_women_college year) (line x_married_dual year, yaxis(2)) if statefip>12 & statefip<=23 & year>=2009, by(statefip)
twoway (line rt_women_college year) (line x_married_dual year, yaxis(2)) if statefip>23 & statefip<=34 & year>=2009, by(statefip)
twoway (line rt_women_college year) (line x_married_dual year, yaxis(2)) if statefip>35 & statefip<=45 & year>=2009, by(statefip)
twoway (line rt_women_college year) (line x_married_dual year, yaxis(2)) if statefip>45 & statefip<=56 & year>=2009, by(statefip)

// should I lag?
// sort statefip year
gen x_married_dual_lag=.
replace x_married_dual_lag=x_married_dual[_n-1] if statefip==statefip[_n-1]

browse statefip year x_married_dual x_married_dual_lag

twoway (line rt_women year) (line x_married_dual_lag year, yaxis(2)) if statefip<=12 & statefip!=11 & year>=2009, by(statefip)
twoway (line rt_women year) (line x_married_dual_lag year, yaxis(2)) if statefip>12 & statefip<=23 & year>=2009, by(statefip)
twoway (line rt_women year) (line x_married_dual_lag year, yaxis(2)) if statefip>23 & statefip<=34 & year>=2009, by(statefip)
twoway (line rt_women year) (line x_married_dual_lag year, yaxis(2)) if statefip>35 & statefip<=45 & year>=2009, by(statefip)
twoway (line rt_women year) (line x_married_dual_lag year, yaxis(2)) if statefip>45 & statefip<=56 & year>=2009, by(statefip)

// are states with an inverse association states with a higher proportion of non-college educated? think I need to account for this in some way.

twoway (line rt_women_non year) (line x_married_dual_lag year, yaxis(2)) if statefip<=12 & statefip!=11 & year>=2009, by(statefip)
twoway (line rt_women_non year) (line x_married_dual_lag year, yaxis(2)) if statefip>12 & statefip<=23 & year>=2009, by(statefip)
twoway (line rt_women_non year) (line x_married_dual_lag year, yaxis(2)) if statefip>23 & statefip<=34 & year>=2009, by(statefip)
twoway (line rt_women_non year) (line x_married_dual_lag year, yaxis(2)) if statefip>35 & statefip<=45 & year>=2009, by(statefip)
twoway (line rt_women_non year) (line x_married_dual_lag year, yaxis(2)) if statefip>45 & statefip<=56 & year>=2009, by(statefip)

twoway (line rt_women_college year) (line x_married_dual_lag year, yaxis(2)) if statefip<=12 & statefip!=11 & year>=2009, by(statefip)
twoway (line rt_women_college year) (line x_married_dual_lag year, yaxis(2)) if statefip>12 & statefip<=23 & year>=2009, by(statefip)
twoway (line rt_women_college year) (line x_married_dual_lag year, yaxis(2)) if statefip>23 & statefip<=34 & year>=2009, by(statefip)
twoway (line rt_women_college year) (line x_married_dual_lag year, yaxis(2)) if statefip>35 & statefip<=45 & year>=2009, by(statefip)
twoway (line rt_women_college year) (line x_married_dual_lag year, yaxis(2)) if statefip>45 & statefip<=56 & year>=2009, by(statefip)

********************************************************************************
** Very Basic FE models
********************************************************************************
/*
xtset [panelvars] [timevars]
xtreg depvar [indepvars] [if] [in] [weight] , fe [FE_options]
*/

regress rt_women x_married_dual i.year if year >=2008

/* negative without fixed effects

--------------------------------------------------------------------------------
      rt_women | Coefficient  Std. err.      t    P>|t|     [95% conf. interval]
---------------+----------------------------------------------------------------
x_married_dual |  -.2042358   .0261222    -7.82   0.000     -.255538   -.1529336
               |
          year |
         2009  |  -.0023405   .0026418    -0.89   0.376    -.0075289    .0028478
         2010  |  -.0067028   .0026489    -2.53   0.012     -.011905   -.0015006
         2011  |  -.0135708    .002664    -5.09   0.000    -.0188028   -.0083388
         2012  |  -.0129754   .0026648    -4.87   0.000    -.0182089   -.0077419
         2013  |   -.013813   .0026602    -5.19   0.000    -.0190375   -.0085885
         2014  |  -.0106985   .0026659    -4.01   0.000     -.015934   -.0054629
         2015  |  -.0109958   .0026594    -4.13   0.000    -.0162188   -.0057729
         2016  |  -.0108098   .0026496    -4.08   0.000    -.0160135   -.0056061
         2017  |  -.0120985   .0026466    -4.57   0.000    -.0172962   -.0069007
         2018  |  -.0132978   .0026468    -5.02   0.000    -.0184959   -.0080997
         2019  |  -.0158243   .0026528    -5.97   0.000    -.0210342   -.0106144
               |
         _cons |   .1057636   .0048737    21.70   0.000      .096192    .1153352
--------------------------------------------------------------------------------
*/

xtset statefip year // unclear if I put both state and year in there... and then do I put year in model as well?
xtreg rt_women x_married_dual i.year if year >=2008, fe

/* positive with


--------------------------------------------------------------------------------
      rt_women | Coefficient  Std. err.      t    P>|t|     [95% conf. interval]
---------------+----------------------------------------------------------------
x_married_dual |   .1502606   .0345387     4.35   0.000     .0824164    .2181049
               |
          year |
         2009  |  -.0011528   .0010726    -1.07   0.283    -.0032596     .000954
         2010  |  -.0038233   .0011026    -3.47   0.001    -.0059891   -.0016575
         2011  |    -.00876   .0011648    -7.52   0.000     -.011048   -.0064721
         2012  |  -.0080889   .0011678    -6.93   0.000    -.0103828   -.0057951
         2013  |  -.0094078   .0011494    -8.18   0.000    -.0116656     -.00715
         2014  |  -.0057057    .001172    -4.87   0.000    -.0080079   -.0034035
         2015  |  -.0066804   .0011462    -5.83   0.000    -.0089319   -.0044289
         2016  |  -.0078045   .0011058    -7.06   0.000    -.0099766   -.0056324
         2017  |  -.0096355    .001093    -8.82   0.000    -.0117824   -.0074886
         2018  |  -.0107964   .0010938    -9.87   0.000     -.012945   -.0086479
         2019  |  -.0123417    .001119   -11.03   0.000    -.0145397   -.0101437
               |
         _cons |   .0446698       .006     7.45   0.000     .0328841    .0564555
---------------+----------------------------------------------------------------
       sigma_u |  .01433572
       sigma_e |  .00538454
           rho |  .87636452   (fraction of variance due to u_i)
--------------------------------------------------------------------------------
*/

xtreg rt_women x_married_dual_lag i.year if year >=2009, fe
// x_married_dual_lag |   .0249457   .0368051     0.68   0.498    -.0473664    .0972578
// lag not significant

xtreg rt_women_college x_married_dual i.year if year >=2008, fe
// x_married_dual |   .2898443   .1022321     2.84   0.005     .0890304    .4906581


xtreg rt_women_non x_married_dual i.year if year >=2008, fe
// x_married_dual |   .0763899   .0343703     2.22   0.027     .0088765    .1439034

// can I interpret this as effects stronger for more educated? test interaction somehow?! but this is not individual level data.... how to do that?!

**also see if male BW has opposite associations based on education?! or like negative?!
xtreg rt_women x_married_malebw i.year if year >=2008, fe
// okay not significant: so this helps? x_married_malebw |   .0239242   .0264543     0.90   0.366    -.0280399    .0758883

xtreg rt_women_college x_married_malebw i.year if year >=2008, fe
// negative but not significant. x_married_malebw |  -.0961521   .0774977    -1.24   0.215    -.2483805    .0560762

xtreg rt_women_non x_married_malebw i.year if year >=2008, fe
// okay yes - marginally sig positive for less educated, so THIS aligns with the individual level findings: x_married_malebw |   .0444258   .0259493     1.71   0.087    -.0065463    .0953979
// think this can also be corroborated in SIPP decomp - rate v composition? is rate WEAKENING?! - can this partially explain change over time?!

// has there been change in association over time. do I have enough data to do this? like did dual earning not used to predict for less educated and now does? (hence why so weak)??
	// --- interact time? can I do that? but think that's normal regression? or mixed effects panel
	
regress rt_women x_married_dual i.year i.year#c.x_married_dual if year >=2008 // if i am interpreting right, is going from negative to positive over time, but not significant. in 2008, sig negative association - becomes positive but not significant by 2019?
margins, at(x_married_dual=(0.10(.02)0.28) year=(2008(1)2019))
marginsplot

regress rt_women_college x_married_dual i.year i.year#c.x_married_dual if year >=2008 // always positive, but gets less positive over time?
margins, at(x_married_dual=(0.10(.02)0.28) year=(2008(1)2019))
marginsplot

regress rt_women_non x_married_dual i.year i.year#c.x_married_dual if year >=2008 // okay also starts very negative, but gets positive over time?
margins, at(x_married_dual=(0.10(.02)0.28) year=(2008(1)2019))
marginsplot

regress rt_women_non x_married_malebw i.year i.year#c.x_married_malebw if year >=2008 // was positive - gets negative?
margins, at(x_married_malebw=(0.35(.02)0.57) year=(2008(1)2019))
marginsplot

xtreg rt_women x_married_dual i.year if year >=2008, re