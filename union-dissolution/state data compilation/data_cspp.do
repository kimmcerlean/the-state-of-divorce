// import delimited "C:\Users\km48547\Downloads\correlates2-6.csv", bindquote(strict) varnames(1)
/*
import delimited "T:\Research Projects\State data\correlates2-6.csv", bindquote(strict) varnames(1) 
save "$temp\cspp_data_all.dta", replace
*/

********************************************************************************
* Clean data, keep what I want
********************************************************************************
use "$temp\cspp_data_all.dta", clear

keep st	stateno	state	state_fips	state_icpsr	year fcpi earned_income_taxcredit avgec_low avgec_mid avgec_high avgsoc_low avgsoc_mid vavgec_low st_ec st_soc pollib_lower pollib_upper pollib_median innovatescore_boehmkeskinner policypriorityscore soc_capital state_culture citi6013 pid ideo pro_welfare anti_welfare neutral_welfare pro_race anti_race neutral_race mothersaid adc timelim z_tanf_initialelig z_tanf_maxpayment z_tanf_paymentsperfamily equalpay genderrights_state_eras gender_discrimination_laws gender_gap const_protect contr_eq foca funds_avail abortion_medicaid gayhate gaymarban sstmrrge gayrights_hatecrimes w_gayrights_civilunions_marriage w_gayrights_employment_discrimit w_gayrights_public_accomodations exec_order ndl_employment ndl_housing ndl_public_accomm ndl_credit ndl_state_employees anti_discrimination fhpriv fhpub fhurb fairemp gminraw gminwag gunion gleave labor_minwage_abovefed z_labor_unemployment_compensatio pubhouen frent regulation_housing_directstateai regulation_housing_federal_aid regulation_rent_control inflation masseconlib_est masssociallib_est policysociallib_est policyeconlib_est gayempnondisc value_mean esi uniondensity public_unionmem privatemem pretgini posttgini unionpriv welfpc1000 k12ed welfare medical miscellaneous highered naturalresources publicsafety highways acir med_hhinc_int cath_adherents_prop lib dem_p welfinal approvefinal sentimentfinal advocacygroup gendermood mlabor wlabor ineqp_a median_inc ineqp_disagg_a coincident epu_state epu_national epu_composite perc_college debt_inc_ratio_low_yr replacement_ratio1_yr disinc unemployment statemin ccpi housing_prices_quar bfh_cpi_multiplier state_cpi_bfh_est sqli sen_dem_prop_all sen_rep_prop_all hs_dem_prop_all hs_rep_prop_all hou_chamber sen_chamber h_chamber_sd s_chamber_sd ranney1_sen_dem_prop ranney2_hs_dem_prop ranney_4yrs citi6013 pid ideo democrat republican liberal conservative z_labor_unemployment_compensatio right2work public_unionmem public_unionmem_demeaned privatemem unionpriv



foreach var in fcpi earned_income_taxcredit avgec_low avgec_mid avgec_high avgsoc_low avgsoc_mid vavgec_low st_ec st_soc pollib_lower pollib_upper pollib_median innovatescore_boehmkeskinner policypriorityscore soc_capital state_culture citi6013 pid ideo pro_welfare anti_welfare neutral_welfare pro_race anti_race neutral_race mothersaid adc timelim z_tanf_initialelig z_tanf_maxpayment z_tanf_paymentsperfamily equalpay genderrights_state_eras gender_discrimination_laws gender_gap const_protect contr_eq foca funds_avail abortion_medicaid gayhate gaymarban sstmrrge gayrights_hatecrimes w_gayrights_civilunions_marriage w_gayrights_employment_discrimit w_gayrights_public_accomodations exec_order ndl_employment ndl_housing ndl_public_accomm ndl_credit ndl_state_employees anti_discrimination fhpriv fhpub fhurb fairemp gminraw gminwag gunion gleave labor_minwage_abovefed z_labor_unemployment_compensatio pubhouen frent regulation_housing_directstateai regulation_housing_federal_aid regulation_rent_control inflation masseconlib_est masssociallib_est policysociallib_est policyeconlib_est gayempnondisc value_mean esi uniondensity public_unionmem privatemem pretgini posttgini unionpriv welfpc1000 k12ed welfare medical miscellaneous highered naturalresources publicsafety highways acir med_hhinc_int cath_adherents_prop lib dem_p welfinal approvefinal sentimentfinal advocacygroup gendermood mlabor wlabor ineqp_a median_inc ineqp_disagg_a coincident epu_state epu_national epu_composite perc_college debt_inc_ratio_low_yr replacement_ratio1_yr disinc unemployment statemin ccpi housing_prices_quar bfh_cpi_multiplier state_cpi_bfh_est sqli sen_dem_prop_all sen_rep_prop_all hs_dem_prop_all hs_rep_prop_all hou_chamber sen_chamber h_chamber_sd s_chamber_sd ranney1_sen_dem_prop ranney2_hs_dem_prop ranney_4yrs democrat republican liberal conservative right2work public_unionmem_demeaned{
	replace `var' ="" if `var'== "NA"
	destring `var', replace
}


label variable fcpi "State cost of living, index year 2007"
label variable earned_income_taxcredit "Earned income tax credit"
label variable avgec_low "Mean economic liberalism-low income respondents"
label variable avgec_mid "Mean economic liberalism-mid income respondents"
label variable avgec_high "Mean economic liberalism-high income respondents"
label variable avgsoc_low "Mean social liberalism-low income respondents"
label variable avgsoc_mid "Mean social liberalism-mid income respondents"
label variable vavgec_low "Mean economic liberalism-low income voters"
label variable st_ec "Mean economic liberalism-all survey respondents"
label variable st_soc "Mean social liberalism-all survey respondents"
label variable pollib_lower "State policy liberalism score-lower bounds"
label variable pollib_upper "State policy liberalism score-upper bounds"
label variable pollib_median "State policy liberalism score-median"
label variable innovatescore_boehmkeskinner "Policy innovativeness score"
label variable policypriorityscore "State policy priority score"
label variable soc_capital "Social capital"
label variable state_culture "State culture type"
label variable citi6013 "Citizen ideology measure"
label variable pid "State party identification score"
label variable ideo "State ideology score"
label variable pro_welfare "Pro-welfare opinion"
label variable anti_welfare "Anti-welfare opinion"
label variable neutral_welfare "Neutral welfare opinion"
label variable pro_race "Pro-race opinion"
label variable anti_race "Anti-race opinion"
label variable neutral_race "Neutral race opinion"
label variable mothersaid "Mothers' aid cash assistance program"
label variable adc "Aid to Dependent Children (Social Security)"
label variable timelim "Time limits on welfare benefits"
label variable z_tanf_initialelig "TANF-initial eligibility level"
label variable z_tanf_maxpayment "TANF-max payment"
label variable z_tanf_paymentsperfamily "TANF-average payments per family"
label variable equalpay "Equal pay for females"
label variable genderrights_state_eras "State equal rights law"
label variable gender_discrimination_laws "Gender discrimination laws (post-1964)"
label variable gender_gap "Gender pay gap"
label variable const_protect "The state constitution protects access to abortion"
label variable contr_eq "Contraceptive equality"
label variable foca "Codifies Roe v. Wade in state"
label variable funds_avail "Requires contraceptive equality"
label variable abortion_medicaid "Medicaid for abortion"
label variable gayhate "Laws establishing anti-LGBT hate crimes"
label variable gaymarban "Constitutional amendment banning gay marriage"
label variable sstmrrge "State marriage laws"
label variable gayrights_hatecrimes "Hate crimes ban"
label variable w_gayrights_civilunions_marriage "Civil unions and gay marriage"
label variable w_gayrights_employment_discrimit "Employment discrimination protections for gays"
label variable w_gayrights_public_accomodations "Ban on discrimination against gays in public accommodations"
label variable exec_order "Governor issued an executive order prohibiting discrimination"
label variable ndl_employment "State has passed a law prohibiting discrimination in employment"
label variable ndl_housing "State has passed a law prohibiting discrimination in housing"
label variable ndl_public_accomm "State has passed a law prohibiting discrimination in public accommodations"
label variable ndl_credit "State has passed a law prohibiting discrimination in credit and lending practices"
label variable ndl_state_employees "State has passed a law prohibiting discrimination against state employees"
label variable anti_discrimination "Anti- discrimination law (later updated in 1995 law)"
label variable fhpriv "Fair housing (private housing)"
label variable fhpub "Fair housing (public housing)"
label variable fhurb "Fair housing (urban renewal areas)"
label variable fairemp "Fair employment laws"
label variable gminraw "Minimum wage rate"
label variable gminwag "For states with higher than federal minimum wage"
label variable gunion "Employees represented by unions as percentage of those employed, annual average"
label variable gleave "Paid family leave insurance program"
label variable labor_minwage_abovefed "Minimum wage above federal level"
label variable z_labor_unemployment_compensatio "Unemployment compensation"
label variable pubhouen "Public housing (enabling legislation)"
label variable frent "Local rent control laws exist"
label variable regulation_housing_directstateai "Urban housing-direct state aid"
label variable regulation_housing_federal_aid "Urban housing- enabling federal aid"
label variable regulation_rent_control "Rent control prohibition"
label variable inflation "Annual inflation rate in the state and year"
label variable masseconlib_est "Economic liberalism of state residents"
label variable masssociallib_est "Social liberalism of state residents"
label variable policysociallib_est "Estimated social policy liberalism"
label variable policyeconlib_est "Estimated economic policy liberalism"
label variable gayempnondisc "Gay employment nondiscrimination law"
label variable value_mean "Support for same-sex marriage"
label variable esi "Economic Security Index"
label variable uniondensity "Union Strength"
label variable public_unionmem "Public sector workers that are union members"
label variable privatemem "Private sector union density"
label variable pretgini "Pre-Transfer Gini"
label variable posttgini "Post-Transfer Gini"
label variable unionpriv "Union Membership"
label variable welfpc1000 "Welfare Spending"
label variable k12ed "Education spending"
label variable welfare "Welfare spending"
label variable medical "Public health spending"
label variable miscellaneous "Other spending"
label variable highered "Higher education spending"
label variable naturalresources "Natural resources spending"
label variable publicsafety "Police and prison spending"
label variable highways "Highway spending"
label variable acir "Budget stringency"
label variable med_hhinc_int "Median Household Income in 2016 dollars"
label variable cath_adherents_prop "Proportion of Catholics"
label variable lib "Dynamic measure of state opinion on ideology"
label variable dem_p "Dynamic measure of state opinion on partisanship"
label variable welfinal "Dynamic measure of state opinion on welfare spending"
label variable approvefinal "Dynamic measure of state opinion on presidential approval"
label variable sentimentfinal "Dynamic measure of state opinion on consumer sentiment"
label variable advocacygroup "Ideological advocacy groups"
label variable gendermood "Gender equality policy mood"
label variable mlabor "Men labor participation"
label variable wlabor "Women labor participation"
label variable ineqp_a "Perception of inequality"
label variable median_inc "Median income"
label variable ineqp_disagg_a "Perception of inequality (disaggregated)"
label variable coincident "State economic health index"
label variable epu_state "State and local sources of economic policy uncertainy"
label variable epu_national "National and international sources of economic policy uncertainy"
label variable epu_composite "Composite index that captures both state + local and national + international sources of economic policy uncertainty"
label variable perc_college "College degree"
label variable debt_inc_ratio_low_yr "Debt-to-income ratio"
label variable replacement_ratio1_yr "Unemployment insurance benefit replacement rate"
label variable disinc "Disposable income"
label variable unemployment "Employment status of the civilian noninstitutional population"
label variable unemployment "Employment status of the civilian noninstitutional population"
label variable statemin "State minimum wage"
label variable ccpi "Consumer price index, all items, annual"
label variable housing_prices_quar "Quarterly housing price index"
label variable bfh_cpi_multiplier "Change in state consumer price index"
label variable state_cpi_bfh_est "State consumer price index"
label variable sqli "Quality of life index ranking"
label variable sen_dem_prop_all "Proportion of Democrats in the senate"
label variable sen_rep_prop_all "Proportion of Republicans in the senate"
label variable hs_dem_prop_all "Proportion of Democrats in the house"
label variable hs_rep_prop_all "Proportion of Republicans in the house"
label variable hou_chamber "State house ideological median"
label variable sen_chamber "State senate ideological median"
label variable h_chamber_sd "State house ideological heterogeneity"
label variable s_chamber_sd "State senate ideological heterogeneity"
label variable ranney1_sen_dem_prop "Proportion of Democratic state senators"
label variable ranney2_hs_dem_prop "Proportion of Democratic state house members"
label variable ranney_4yrs "Four-year moving average of proportion of Democratic control in state senate, house, governor, and government"
label variable democrat "Democratic identifiers"
label variable republican "Republican identifiers"
label variable liberal "Liberal identifiers"
label variable conservative "Conservative identifiers"


preserve
keep if year >=1985 & year <=2019

// recode * ("NA"=)

browse state year ccpi state_cpi_bfh_est
browse state year ranney1_sen_dem_prop ranney2_hs_dem_prop ranney_4yrs democrat republican liberal conservative
browse year state z_labor_unemployment_compensatio right2work public_unionmem public_unionmem_demeaned privatemem unionpriv
tabstat  public_unionmem, by(right2work)
browse year state state_fips z_labor_unemployment_compensatio right2work

save "$created_data\cspp_data_1985_2019.dta", replace
restore

keep if year >=1970 & year <=2019
save "$created_data\cspp_data_1970_2019.dta", replace


/* some things for over time trends
statemin: minimum wage - 2017, I updated up until 2020
masssociallib_est: attitudes - 2014
policysociallib_est: social policy - 2014
policyeconlib_est: economic policy - 2014
unemployment: unemployment rate - 2017, I updated up until 2020
state_cpi_bfh_est: state CPI - 2010
*/

preserve

collapse (mean) statemin masssociallib_est policysociallib_est policyeconlib_est unemployment state_cpi_bfh_est, by(year)

// browse state year gminraw statemin gleave pollib_median policysociallib_est masssociallib_est policyeconlib_est masseconlib_est gendermood  pro_welfare pro_race uniondensity unionpriv pretgini posttgini welfpc1000 welfare ineqp_a epu_state inflation unemployment ccpi state_cpi_bfh_est fcpi housing_prices_quar sqli

pwcorr policysociallib_est policyeconlib_est // yes
pwcorr masseconlib_est masssociallib_est // not really
pwcorr policysociallib_est masssociallib_est // yes
pwcorr policyeconlib_est masseconlib_est // moderately

/* to use:
statemin: minimum wage
masssociallib_est: attitudes
policysociallib_est: social policy
policyeconlib_est: economic policy
unemployment: unemployment rate (but need to figure out why missing for 1995)
state_cpi_bfh_est: state CPI
*/

