********************************************************************************
* The file takes the Excel with all of the compiled measures and
* interpolates / extrapolates variables with missing info. This includes:
	* Childcare costs (prior to 2010)
	* CCDF subsidy info (prior to 1999)
	* GSS regional attitude data (mostly odd years 1995-2021)
********************************************************************************

// first, turn excel into stata file
clear
import excel "G:/Other computers/My Laptop/Documents/Dissertation/Policy data/Structural support measure/State Measures - updated 0625.xlsx", sheet("import") firstrow

save "G:/Other computers/My Laptop/Documents/Dissertation/Policy data/Structural support measure/structural_familism_june25.dta", replace
save "$created_data/structural_familism_june25.dta", replace

********************************************************************************
* now, do interpolation
********************************************************************************
use "$created_data/structural_familism_june25.dta", clear

** Start with attitudes
// want to use these as final names
foreach var in fepresch_reg	fechld_reg fefam_reg gender_factor_reg preschool_egal_reg working_mom_egal_reg genderroles_egal_reg{
	rename `var' `var'_orig
}

sort state_fips year
bysort state_fips: ipolate gender_factor_reg_orig year, gen(gender_factor_reg) epolate // think I need this to get 2021 (ipolate, in theory, only does the IN-between years)

sort state_fips year
browse state_fips year gender_factor_reg_orig gender_factor_reg

// do for all variables
foreach var in fepresch_reg	fechld_reg fefam_reg preschool_egal_reg working_mom_egal_reg genderroles_egal_reg{
	bysort state_fips: ipolate `var'_orig year, gen(`var') epolate 
}

sort state_fips year
browse state_fips year fepresch_reg	fepresch_reg_orig fechld_reg fechld_reg_orig fefam_reg fefam_reg_orig gender_factor_reg gender_factor_reg_orig preschool_egal_reg preschool_egal_reg_orig working_mom_egal_reg working_mom_egal_reg_orig genderroles_egal_reg genderroles_egal_reg_orig

// let's also average the %ages? Because I kind of want just one variable?
pwcorr preschool_egal_reg working_mom_egal_reg genderroles_egal_reg
pwcorr preschool_egal_state working_mom_egal_state genderroles_egal_state
egen avg_egal_reg = rowmean(preschool_egal_reg working_mom_egal_reg genderroles_egal_reg)
egen avg_egal_state = rowmean(preschool_egal_state working_mom_egal_state genderroles_egal_state)

// browse state_fips year avg_egal_reg preschool_egal_reg working_mom_egal_reg genderroles_egal_reg avg_egal_state preschool_egal_state working_mom_egal_state genderroles_egal_state

** Childcare costs
foreach var in cc_cost cc_pct_income{ // not sure I need to interpolate the percent of income or use the calculation, so let's test both
	rename `var' `var'_orig
}

foreach var in cc_cost cc_pct_income{
	bysort state_fips: ipolate `var'_orig year, gen(`var') epolate 
}

gen cc_pct_calc = cc_cost / med_married_income
pwcorr cc_pct_calc cc_pct_income // okay r=0.80

sort state_fips year
browse state_fips year cc_cost cc_cost_orig cc_pct_calc cc_pct_income cc_pct_income_orig // okay, not this isn't working. so, need to either do myself based on a rolling average OR just do a robustness with this linked just to time period covered.
// OR I guess a third option is use as time invariant? let's come back to this

// what about a regression on year instead?
bysort state_fips: regress cc_pct_income_orig year
// by state_fips: predict cc_pct_est

regress cc_pct_income_orig year i.state_fips
predict cc_pct_est

browse state_fips year cc_pct_income_orig cc_pct_est cc_pct_income

** Childcare subsidies - I am getting confused here because the reason data didn't exist prior to 1999 is because this didn't exist prior to 1999, right? so actually this doesn't make sense to do?
// yeah the CCDF was only created by PRWORA in 1996, but the block grant program was created in 1990
// cc: https://bipartisanpolicy.org/download/?file=/wp-content/uploads/2021/05/B.-1-History-of-Federal-Funding.pdf and https://www.congress.gov/crs-product/R44528
// okay especially given how bad the extrapolation is for many years outside of the range, we are not going to do this.

save "G:/Other computers/My Laptop/Documents/Dissertation/Policy data/Structural support measure/structural_familism_june25_int.dta", replace
save "$created_data/structural_familism_june25_int.dta", replace

********************************************************************************
* Some data exploration while here
********************************************************************************

browse state_fips year unemployment_comp unemployment_percap wba_max ui_max
pwcorr unemployment_comp unemployment_percap wba_max ui_max
// ui appears to be wba*26 (half a year)
// sometimes, unemployment comp (not percap) = wba. so, I wonder if unemployment comp is not what I think it is.

/*

             | unemp~mp unemp~ap  wba_max   ui_max
-------------+------------------------------------
unemploym~mp |   1.0000 
unemploym~ap |   0.4419   1.0000 
     wba_max |   0.8098   0.5554   1.0000 
      ui_max |   0.7979   0.5432   0.9878   1.0000 
*/

tab right2work rtw // almost perfectly aligned, but some right2work 0s are 1s in rtw - let's use Montez version if I decide to use...but not sure

tab preempt_total preempt_fairsched
tab preempt_fairsched, m // okay I don't think there are enough 1s here
tab preempt_total, m

browse state_fips year high_inc_prem_raw high_inc_prem_pct low_inc_prem_raw low_inc_prem_pct
pwcorr high_inc_prem_pct low_inc_prem_pct 

// which employment rates to use (if any)
pwcorr 	women_emp_rate_wt married_women_emp_rate_wt maternal_employment_wt maternal_u5_employment_wt ///
		women_pt_rate married_women_pt_rate_wt maternal_pt_rate maternal_u5_pt_rate
// so within total or pt rates - all the categories generally correlated (ofc not perfectly), BUT PT employment not well correlated to total employment, so is a diff construct I think?


// childcare
browse state_fips year cc_pct_served headstart_pct headstart_pct_totalpop earlyhs_pct earlyhs_pct_totalpop total_headstart_pct total_headstart_pct_totalpop prek_enrolled prek_enrolled_public

pwcorr headstart_pct headstart_pct_totalpop 
pwcorr earlyhs_pct earlyhs_pct_totalpop 
pwcorr total_headstart_pct total_headstart_pct_totalpop

pwcorr cc_pct_served headstart_pct headstart_pct_totalpop earlyhs_pct earlyhs_pct_totalpop total_headstart_pct total_headstart_pct_totalpop prek_enrolled prek_enrolled_public
pwcorr headstart_pct headstart_pct_totalpop earlyhs_pct earlyhs_pct_totalpop total_headstart_pct total_headstart_pct_totalpop prek_enrolled prek_enrolled_public // remove cc_pct_served bc not until 1999, so this will cover more years - oh I am dumb, it uses the full info where available, so JUST the comparisons with cc pct served are truncated

