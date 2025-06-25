 
use "$data/gss_regional_attitudes_1972_2024.dta", clear
drop if YEAR < 1990

********************************************************************************
**# Recodes for analysis
********************************************************************************

recode EDUC (0/11=1) (12=2) (13/15=3) (16/20=4), gen(education)
label define educ 1 "LTHS" 2 "HS" 3 "Some College" 4 "College +"
label values education educ
drop if education <0

gen college=(education==4)

fre FECHLD // (1/2=agree)(3/4=disagree) - agree = egal (" A working mother can establish just as warm and secure a relationship with her children as a mother who does not work")
fre FEPRESCH // (1/2=agree)(3/4=disagree) - DISagree = egal ("A preschool child is likely to suffer if his or her mother works.")
fre FEFAM // (1/2=agree)(3/4=disagree) - DISagree = egal ("It is much better for everyone involved if the man is the achiever outside the home and the woman takes care of the home and family.")
fre FEPOL // (1=agree)(2=disagree) - DISagree = egal ("Most men are better suited emotionally for politics than are most women.")

recode FECHLD (-100/-98=.)(1/2=1)(3/4=2), gen(work_children)
recode FEPRESCH (-100/-98=.)(1/2=1)(3/4=2), gen(work_preschool)
recode FEFAM (-100/-98=.)(1/2=1)(3/4=2), gen(gendered_roles)
recode FEPOL (-100/-98=.), gen(women_politics)

/* not needed anything bc 100/97 already recoded now to diff types of missing
recode FECHLD (-100/-98=.), gen(FECHLD_M)
label values FECHLD_M   GSP005X

recode FEPRESCH (-100/-98=.), gen(FEPRESCH_M)
label values FEPRESCH_M   GSP006X

recode FEFAM (-100/-98=.), gen( )
label values FEFAM_M  GSP007X

recode FEPOL (-100/-98=.), gen(FEPOL_M)
label values FEPOL_M   GSP004X
*/

label define scale 1 "Agree" 2 "Disagree"
label values work_children work_preschool gendered_roles women_politics scale

// so then these become proportion of people who are egal
gen working_mom_egal=.
replace working_mom_egal=1 if work_children==1 // agree = egal
replace working_mom_egal=0 if work_children==2

gen gender_egal=.
replace gender_egal=1 if gendered_roles==2 // disagree = egal
replace gender_egal=0 if gendered_roles==1

gen preschool_egal=.
replace preschool_egal=1 if work_preschool==2 // disagree = egal
replace preschool_egal=0 if work_preschool==1

// want to recode so higher score = always more egal. Mainly just FECHLD that needs to change (currently lower = more egal)
recode FECHLD (1=4)(2=3)(3=2)(4=1), gen(FECHLD_rev)
label define rev_scale 1 "strongly disagree" 2 "disagree" 3 "agree" 4 "strongly agree"
label values FECHLD_rev rev_scale
// tab FECHLD FECHLD_rev

tabstat FECHLD FECHLD_rev FEPRESCH FEFAM FEPOL

// for when I collapse - want to see cell sizes
gen records=1

// tab YEAR gendered_roles, row nofreq
// tab YEAR gender_egal, row nofreq
// tab YEAR gendered_roles if college==0, row nofreq
// tab YEAR gendered_roles if college==1, row nofreq


// factor analysis of scale as alt indicator
/* Pessin 2018 for methods reference:
All the available surveys were pooled and a principal-factor analysis was carried out to obtain a unique index where higher
scores represented more egalitarian gender attitudes (Cronbach's alpha = 0.74; see Appendix
B for more details). To fill the missing years before 1977 and between 1977 and 2010, an
interpolation was carried out. Additional analyses showed that the results were not sensitive
to the interpolation of the gender index 
*/

alpha FECHLD_rev FEPRESCH FEFAM // 0.7252
factor FECHLD_rev FEPRESCH FEFAM, ipf // but this tends to be more common...
// rotate, varimax
predict f1
rename f1 gender_factor_ipf

factor FECHLD_rev FEPRESCH FEFAM, pcf // see: https://www.statalist.org/forums/forum/general-stata-discussion/general/1500686-principal-factor-vs-principal-component-factor-option-for-factor-analysis and Acock 2016
predict f1
rename f1 gender_factor_pcf

browse YEAR FECHLD_rev FEPRESCH FEFAM gender_factor_ipf gender_factor_pcf
pwcorr gender_factor_ipf gender_factor_pcf // almost perfectly correlated

pwcorr gender_factor_ipf FECHLD_rev FEPRESCH FEFAM 
pwcorr gender_factor_pcf FECHLD_rev FEPRESCH FEFAM 

** this is from when I really cared about educ differences
// save "T:\data\GSS\GSS - Chapter 3\gss_year_region_educ.dta", replace
save "$data/gss_year_region_educ.dta", replace
;

log using "$logdir/regional_gss.log", replace

foreach var in work_children work_preschool gendered_roles women_politics{
	display "total"
	tab REGION `var', row nofreq
	display "no college"
	tab REGION `var' if college==0, row nofreq
	display "college"
	tab REGION `var' if college==1, row nofreq
}

log close

preserve 
collapse (mean) gender_egal working_mom_egal, by(REGION YEAR college)
restore

preserve
collapse (mean) gender_egal working_mom_egal FEFAM_M, by(YEAR college)
restore

********************************************************************************
**# Exports to use
********************************************************************************

preserve
keep if YEAR > = 1990
collapse 	(mean) gender_egal working_mom_egal preschool_egal FECHLD_rev FEPRESCH FEFAM gender_factor_ipf ///
			(sum) records, by(REGION) // keeping just these two because HIGHER = more egal (the other variable is coded the other way)
// save "T:\Research Projects\State data\data_keep\region_gss_total.dta", replace
save "$created_data/region_gss_total.dta", replace
restore

preserve
keep if YEAR > = 1990
collapse 	(mean) gender_egal working_mom_egal preschool_egal FECHLD_rev FEPRESCH FEFAM gender_factor_ipf ///
			(sum) records, by(REGION YEAR) // keeping just these two because HIGHER = more egal (the other variable is coded the other way)

sum records // okay so the min is 60 so that makes me feel okay (Ruppanner says 50 is their min at a state level
save "$created_data/region_gss_by-year.dta", replace
restore


preserve
keep if YEAR > = 1990 & YEAR <= 2018 // can I replicate my old numbers? okay yes basically
collapse 	(mean) gender_egal working_mom_egal preschool_egal FECHLD_rev FEPRESCH FEFAM gender_factor_ipf ///
			(sum) records, by(REGION) // keeping just these two because HIGHER = more egal (the other variable is coded the other way)
browse
restore

use "$created_data/region_gss_by-year.dta", clear
export excel using "$created_data/region_gss_by-year", firstrow(variables) replace