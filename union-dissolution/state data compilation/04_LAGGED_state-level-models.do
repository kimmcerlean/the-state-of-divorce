********************************************************************************
* Pull together all data sources
* compiled data.do
* Kim McErlean
********************************************************************************
* First pull in file with IVs and shift then forward two years
use "$temp/2010_2018_compiled_dataset_factor.dta", clear // created in step 2, saving new copy to use as lagged
drop x_lfp_women x_employed_women x_employed_women_lf x_women_wage_ratio x_women_ceo x_women_manage x_college_women x_sex_ratio x_educ_work_ratio x_90_10_inequality x_pov_rate x_fem_pov_rate x_married_dual x_married_malebw x_married_malesole x_married_dual_non x_married_malebw_non x_married_malesole_non x_married_dual_ly x_married_malebw_ly x_married_dual_non_ly x_married_malebw_non_ly x_married_malebw_coll_ly x_married_dual_coll_ly // drop all DVS want to use them as is.

drop if year > 2016
recode year (2010=2012) (2011=2013) (2012=2014) (2013=2015) (2014=2016) (2015=2017) (2016=2018)

* merge with ACS file with outcomes
merge 1:1 year statefip using "$created_data\2010_2018_state_data_main.dta", keepusing(x_*) 
drop if _merge==2 // because of the xtra years
drop _merge

regress x_married_malebw_non_ly fepresch infant_care_pct pct_workers_paid_leave eitc_credit f_tanf women_legis dems_legis x_pov_rate leave_policy_score abortion
outreg2 using "$results/state_level_malebw.xls", sideway stats(coef pval) label ctitle(No College - Lag LY) dec(5) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

regress x_married_malebw_non fepresch infant_care_pct pct_workers_paid_leave eitc_credit f_tanf women_legis dems_legis x_pov_rate leave_policy_score abortion
outreg2 using "$results/state_level_malebw.xls", sideway stats(coef pval) label ctitle(No College - Lag All) dec(5) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

regress x_married_malebw_coll_ly fepresch infant_care_pct pct_workers_paid_leave eitc_credit f_tanf women_legis dems_legis x_pov_rate leave_policy_score abortion
outreg2 using "$results/state_level_malebw.xls", sideway stats(coef pval) label ctitle(College - Lag LY) dec(5) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

regress x_married_malebw_coll fepresch infant_care_pct pct_workers_paid_leave eitc_credit f_tanf women_legis dems_legis x_pov_rate leave_policy_score abortion
outreg2 using "$results/state_level_malebw.xls", sideway stats(coef pval) label ctitle(College - Lag All) dec(5) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append
