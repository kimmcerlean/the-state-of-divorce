********************************************************************************
* Create structural support variable
* create-state-varables.do
* Kim McErlean
********************************************************************************

********************************************************************************
**# Scale for first Social Forces submission
********************************************************************************
use "$structural/structural_support_2021.dta", clear

drop if state_fips==11 // DC doesn't have a lot of these variables

// first recode so all in the same direction - want higher to be MORE support
gen earn_ratio_neg = 0-earn_ratio // bc was calculated as men's wages over women's, so currently, above 1 = men higher, want opposite
gen parent_earn_ratio_neg = 0-parent_earn_ratio
// gen welfare_neg = 0-welfare_all
// gen welfare_cash_neg = 0 -welfare_cash_asst

foreach var in min_wage min_above_fed min_amt_above_fed paid_leave paid_leave_length earn_ratio earn_ratio_neg parent_earn_ratio parent_earn_ratio_neg welfare_all welfare_cash_asst abortion_protected unemployment_percap prek_enrolled prek_enrolled_public{
	sum `var'
	gen `var'_st = (`var'- `r(mean)') / `r(sd)'
}

** Current
alpha paid_leave_st prek_enrolled_public_st min_above_fed_st earn_ratio_neg_st unemployment_percap_st abortion_protected_st welfare_all_st
egen structural_familism_v0 = rowtotal(paid_leave_st prek_enrolled_public_st min_above_fed_st earn_ratio_neg_st unemployment_percap_st abortion_protected_st welfare_all_st)

** Explore new measure based on reviewers
alpha paid_leave_st prek_enrolled_public_st min_amt_above_fed_st earn_ratio_neg_st unemployment_percap_st abortion_protected_st welfare_all_st // replace min wage binary with indicator of amount above
alpha paid_leave_length_st prek_enrolled_public_st min_amt_above_fed_st earn_ratio_neg_st unemployment_percap_st abortion_protected_st welfare_all_st // replace paid leave binary with length
alpha paid_leave_length_st prek_enrolled_public_st min_amt_above_fed_st parent_earn_ratio_neg_st unemployment_percap_st abortion_protected_st welfare_all_st

egen structural_familism = rowtotal(paid_leave_st prek_enrolled_public_st min_amt_above_fed_st earn_ratio_neg_st unemployment_percap_st abortion_protected_st welfare_all_st) // for not including 2019/2021 - going to just use paid leave bc length really doesn't vary until after 2018 and I don't want to overexaggerate differences

// summarize inputs: base variable
tabstat paid_leave prek_enrolled_public min_amt_above_fed earn_ratio_neg unemployment_percap abortion_protected welfare_all structural_familism, stats(min max mean sd)

// summarize inputs: standardized
tabstat paid_leave_st prek_enrolled_public_st min_amt_above_fed_st earn_ratio_neg_st unemployment_percap_st abortion_protected_st welfare_all_st, stats(min max mean sd)

// egen structural_familism = rowtotal(paid_leave_length_st prek_enrolled_public_st min_amt_above_fed_st earn_ratio_neg_st unemployment_percap_st abortion_protected_st welfare_all_st)

pwcorr structural_familism_v0 structural_familism // 0.9821 correlation
browse state_fips year structural_familism structural_familism_v0 paid_leave_length prek_enrolled_public min_amt_above_fed earn_ratio_neg unemployment_percap abortion_protected welfare_all

sum structural_familism_v0, detail
sum structural_familism, detail

** test centering the variable as well
gen sf_centered=.
sum structural_familism, detail
// replace sf_centered = structural_familism - `r(p50)'
replace sf_centered = structural_familism - `r(mean)'

sum structural_familism sf_centered, detail // okay it is basically already mean centered, so it is fine?
browse structural_familism sf_centered

tabstat structural_familism paid_leave_length prek_enrolled_public min_amt_above_fed earn_ratio_neg unemployment_percap abortion_protected welfare_all, by(state_name)
tabstat structural_familism paid_leave_length prek_enrolled_public min_amt_above_fed earn_ratio_neg unemployment_percap abortion_protected welfare_all if year<=2019, by(state_name)

**Second approach: Factor-based
factor paid_leave_length_st prek_enrolled_public_st min_amt_above_fed_st earn_ratio_neg_st unemployment_percap_st abortion_protected_st welfare_all_st, ipf
predict f1
pwcorr f1 structural_familism // so if I make a factor variable, also VERY correlated (~0.95)
// browse structural_familism f1
rename f1 structural_factor

**Factor based for fertility
alpha paid_leave_length_st prek_enrolled_public_st earn_ratio_neg_st welfare_all_st
factor paid_leave_length_st prek_enrolled_public_st earn_ratio_neg_st welfare_all_st, ipf
predict f1
rename f1 fertility_factor

alpha paid_leave_length_st prek_enrolled_public_st parent_earn_ratio_neg_st min_amt_above_fed_st welfare_all_st abortion_protected_st
factor paid_leave_length_st prek_enrolled_public_st parent_earn_ratio_neg_st min_amt_above_fed_st welfare_all_st abortion_protected_st, ipf
predict f1
rename f1 fertility_factor_det

tabstat structural_familism fertility_factor_det paid_leave_length prek_enrolled_public min_amt_above_fed parent_earn_ratio_neg abortion_protected welfare_all, by(state_name)

save "$state_data/structural_familism.dta", replace

********************************************************************************
********************************************************************************
********************************************************************************
**# Exploring other scale options
********************************************************************************
********************************************************************************
********************************************************************************

// use "$structural/structural_familism_june25_int.dta", clear
use "$created_data/structural_familism_june25_int.dta", clear

/*
Work-family policy: explicit
- paid_leave
- cc_pct_income
- prek_enrolled_public
- cc_pct_served
- headstart_pct_totalpop
- earlyhs_pct_totalpop

Work-family policy: implicit
- policy_lib_all
- abortion_protected
- married_women_pt_rate_wt
- maternal_u5_pt_rate

Normative diffusion of gender revolution
- gender_factor_reg
- avg_egal_reg
- gender_factor_state
- avg_egal_state
- evang_lds_rate

Behavioral diffusion of gender revolution
- married_dual_earn_rate
- married_pure_male_bw_rate
- married_women_emp_rate_wt
- maternal_u5_employment_wt

Women's economic dependence on marriage (some of these could arguably be explicit work-family policy)
- min_amt_above_fed
- unemployment_percap
- wba_max
- high_inc_prem_pct
- low_inc_prem_pct
- married_earn_ratio
- welfare_all
*/

// any variables still need to be recoded so higher = more support fro gender egalitarianism? (I tried to update most of this in underlying data). although, with factor analysis, at least, it will reverse code for you? so maybe I don't need to myself?
// browse state_fips year cc_pct_income_orig evang_lds_rate low_inc_prem_pct
gen cc_pct_inc_neg = 0 - cc_pct_income_orig // higher = bad
gen married_wom_pt_rate_neg = 0 - married_women_pt_rate_wt // is higher pt good or bad?
gen mom_u5_pt_rate_neg = 0 - maternal_u5_pt_rate // is higher pt good or bad?
gen non_evang_lds_rate = 1 - evang_lds_rate
gen non_pure_male_bw_rate = 1 - married_pure_male_bw_rate // not *quite* a direct foil to dual-earn percent, so should recode
// high_inc_prem_pct // negative = more of a premium, so less negative = I guess better, so is this actually fine?
// low_inc_prem_pct

// then standardize variables
foreach var in paid_leave cc_pct_income prek_enrolled_public cc_pct_served headstart_pct_totalpop earlyhs_pct_totalpop policy_lib_all abortion_protected married_women_pt_rate_wt maternal_u5_pt_rate gender_factor_reg avg_egal_reg gender_factor_state avg_egal_state evang_lds_rate married_dual_earn_rate married_pure_male_bw_rate married_women_emp_rate_wt maternal_u5_employment_wt min_amt_above_fed unemployment_percap wba_max high_inc_prem_pct low_inc_prem_pct earn_ratio married_earn_ratio welfare_all cc_pct_inc_neg married_wom_pt_rate_neg mom_u5_pt_rate_neg non_evang_lds_rate non_pure_male_bw_rate{
	sum `var'
	gen `var'_st = (`var'- `r(mean)') / `r(sd)'
}

// duh - let's just make the basic original scale for now as well for some robustness / preliminary exploration
alpha paid_leave_st prek_enrolled_public_st min_amt_above_fed_st earn_ratio_st unemployment_percap_st abortion_protected_st welfare_all_st

egen structural_familism = rowtotal(paid_leave_st prek_enrolled_public_st min_amt_above_fed_st earn_ratio_st unemployment_percap_st abortion_protected_st welfare_all_st)
pwcorr structural_familism policy_lib_all policy_lib_econ policy_lib_soc

// try factor based on current also
factor paid_leave_st prek_enrolled_public_st min_amt_above_fed_st earn_ratio_st unemployment_percap_st abortion_protected_st welfare_all_st, ipf
predict f1
rename f1 structural_factor
pwcorr structural_factor structural_familism // so if I make a factor variable, also VERY correlated (~0.95)

gen year_t1 = year // to get t-1 measures

save "$temp/data_for_scale.dta", replace

********************************************************************************
**# Exploring factor analysis
********************************************************************************
// https://libguides.princeton.edu/factor
// https://www.theanalysisfactor.com/the-fundamental-difference-between-principal-component-analysis-and-factor-analysis/
// https://stats.oarc.ucla.edu/spss/seminars/introduction-to-factor-analysis/a-practical-introduction-to-factor-analysis/

/* From rotate guide: https://www.stata.com/manuals/mvrotate.pdf#mvrotate
The default factor solution is rather poor from the perspective of a "simple structure", namely, that
variables should have high loadings on few (one) factors and factors should ideally have only low and
high values.

UCLA: https://stats.oarc.ucla.edu/stata/faq/how-can-i-perform-a-factor-analysis-with-categorical-or-categorical-and-continuous-variables/
Better for continuous variables, realizing some of these (With high uniqueness) are dichotomous - paid leave and abortion
*/

// feel like variables that could be dropped include those with uniqueness above 0.6
// those with loadings below 0.3

use "$temp/data_for_scale.dta", clear

// paid_leave_st cc_pct_income_st prek_enrolled_public_st cc_pct_served_st headstart_pct_totalpop_st earlyhs_pct_totalpop_st policy_lib_all_st abortion_protected_st married_women_pt_rate_wt_st maternal_u5_pt_rate_st gender_factor_reg_st avg_egal_reg_st gender_factor_state_st avg_egal_state_st evang_lds_rate_st married_dual_earn_rate_st married_pure_male_bw_rate_st married_women_emp_rate_wt_st maternal_u5_employment_wt_st min_amt_above_fed_st unemployment_percap_st wba_max_st high_inc_prem_pct_st low_inc_prem_pct_st married_earn_ratio_st welfare_all_st
pwcorr gender_factor_reg_st avg_egal_reg_st

alpha paid_leave_st prek_enrolled_public_st headstart_pct_totalpop_st earlyhs_pct_totalpop_st // cc_pct_income_st cc_pct_served_st
alpha paid_leave_st prek_enrolled_public_st headstart_pct_totalpop_st earlyhs_pct_totalpop_st cc_pct_served_st

factor paid_leave_st prek_enrolled_public_st earlyhs_pct_totalpop_st policy_lib_all_st abortion_protected_st maternal_u5_pt_rate_st avg_egal_reg_st non_evang_lds_rate_st married_dual_earn_rate_st married_women_emp_rate_wt_st min_amt_above_fed_st wba_max_st high_inc_prem_pct_st low_inc_prem_pct_st married_earn_ratio_st welfare_all_st, ipf
// Warning: Solution is a Heywood case; that is, invalid or boundary values of uniqueness.
// This is because of policy_lib_all, which makes sense if that is already an amalgamation of policies

factor paid_leave_st prek_enrolled_public_st earlyhs_pct_totalpop_st abortion_protected_st maternal_u5_pt_rate_st avg_egal_reg_st non_evang_lds_rate_st married_dual_earn_rate_st married_women_emp_rate_wt_st min_amt_above_fed_st wba_max_st high_inc_prem_pct_st low_inc_prem_pct_st married_earn_ratio_st welfare_all_st, ipf // policy_lib_all_st

// uniqueness depends on # of factors. This is yielding MANY factors, so let's update
factor paid_leave_st prek_enrolled_public_st earlyhs_pct_totalpop_st abortion_protected_st maternal_u5_pt_rate_st avg_egal_reg_st non_evang_lds_rate_st married_dual_earn_rate_st married_women_emp_rate_wt_st min_amt_above_fed_st wba_max_st high_inc_prem_pct_st low_inc_prem_pct_st married_earn_ratio_st welfare_all_st, ipf factors(2) blanks(0.2) // also - don't display extremely low factors. okay but when I do this, uniqueness goes up quite significantly for many...
loadingplot, xlab(-1(.2)1) ylab(0(.2)1.2) aspect(1) yline(0) xline(0)
 
rotate, varimax // so in theory, should more easily be able to see that some variables load high on one factor and low on another and vice versa
loadingplot, xlab(-1(.2)1) ylab(-.2(.2)1.2) aspect(1) yline(0) xline(0)
estat rotatecompare
//rotate, quartimax

estat kmo // is it worth doing a factor analysis? "small values meaning that overall the variables have too little in common to warrant a factor analysis". >0.9 is "marvelous" but feel like above 0.7 to 0.8 is okay

// do I actually want standardized or non-standardized??
// Unstandardized is a Heywood case - bc of maternal employment. but the general outcome is similar
factor paid_leave prek_enrolled_public earlyhs_pct_totalpop abortion_protected maternal_u5_pt_rate avg_egal_reg non_evang_lds_rate married_dual_earn_rate married_women_emp_rate_wt min_amt_above_fed wba_max high_inc_prem_pct low_inc_prem_pct married_earn_ratio welfare_all, ipf factors(2) blanks(0.2) 
rotate, varimax

// polychoric (https://stats.oarc.ucla.edu/stata/faq/how-can-i-perform-a-factor-analysis-with-categorical-or-categorical-and-continuous-variables/)
polychoric paid_leave prek_enrolled_public earlyhs_pct_totalpop abortion_protected maternal_u5_pt_rate avg_egal_reg non_evang_lds_rate married_dual_earn_rate married_women_emp_rate_wt min_amt_above_fed wba_max high_inc_prem_pct low_inc_prem_pct married_earn_ratio welfare_all

display r(sum_w)
global N = r(sum_w)

matrix r = r(R)
factormat r, n($N) factors(3) forcepsd // okay I am getting this error: r not positive (semi)definite
// (collinear variables specified)
// okay these are like...very different
// okay I actually wonder if married dual earn rate and married women employment rate are too collinear (bc women's employment needs to be high)

// okay, I am doing all of this. Is there a world where I use my ORIGINAL scale (maybe with small variable and / or construction tweaks) and I just control for other possible factors - including attitudes, dual earning rate, religiosity, women's employment rate, etc? I am possibly losing the plot a bit
// MAYBE make a factor for like "Diffusion" of gender rev, but that's it?
alpha avg_egal_reg_st non_evang_lds_rate_st married_dual_earn_rate_st married_women_emp_rate_wt_st // 0.6116
alpha avg_egal_reg_st non_evang_lds_rate_st non_pure_male_bw_rate_st married_women_emp_rate_wt_st //  0.6616
alpha gender_factor_reg_st non_evang_lds_rate_st non_pure_male_bw_rate_st married_women_emp_rate_wt_st //   0.6531
pwcorr avg_egal_reg_st gender_factor_reg_st fepresch_reg fechld_reg fefam_reg preschool_egal_reg working_mom_egal_reg genderroles_egal_reg non_evang_lds_rate_st non_pure_male_bw_rate_st married_dual_earn_rate_st married_women_emp_rate_wt_st // so all positively correlated - but maybe not as much as I thought? Like attitudes and behavior (male BW) not highly correlated

factor avg_egal_reg_st non_evang_lds_rate_st non_pure_male_bw_rate_st married_women_emp_rate_wt_st , ipf // okay but attitudes and religiosity quite unique. so attitudinal diffusion + behavioral diffusion might not be the same?
predict f1
rename f1 diffusion

pwcorr diffusion avg_egal_reg_st non_evang_lds_rate_st non_pure_male_bw_rate_st married_women_emp_rate_wt_st
pwcorr diffusion avg_egal_reg policy_lib_all policy_lib_soc // like - they are not really correlated at all....I mean, they are positive, but it's clear they are not at all the same...

// should I try this 2x2 indicator again??
sum avg_egal_reg, det
gen egal_attitudes = 0 if avg_egal_reg < `r(p50)'
replace egal_attitudes = 1 if avg_egal_reg >= `r(p50)'

sum diffusion, det
gen diffusion_yn = 0 if diffusion < `r(p50)'
replace diffusion_yn = 1 if diffusion >= `r(p50)'

tab egal_attitudes diffusion_yn, m row // actually less congruent than policy

sum policy_lib_all, det
gen policy_support = 0 if policy_lib_all < `r(p50)'
replace policy_support = 1 if policy_lib_all >= `r(p50)'

sum structural_familism, det
gen family_support = 0 if structural_familism < `r(p50)'
replace family_support = 1 if structural_familism >= `r(p50)'

tab policy_support family_support, m row // def not super congruent either (but my scale is much more limited than theirs)

gen policy_group_v1 = . 
replace policy_group_v1 = 1 if egal_attitudes==1 & policy_support==1
replace policy_group_v1 = 2 if egal_attitudes==1 & policy_support==0
replace policy_group_v1 = 3 if egal_attitudes==0 & policy_support==1
replace policy_group_v1 = 4 if egal_attitudes==0 & policy_support==0

gen policy_group_v2 = . 
replace policy_group_v2 = 1 if egal_attitudes==1 & family_support==1
replace policy_group_v2 = 2 if egal_attitudes==1 & family_support==0
replace policy_group_v2 = 3 if egal_attitudes==0 & family_support==1
replace policy_group_v2 = 4 if egal_attitudes==0 & family_support==0

gen policy_group_v3 = . 
replace policy_group_v3 = 1 if diffusion_yn==1 & family_support==1
replace policy_group_v3 = 2 if diffusion_yn==1 & family_support==0
replace policy_group_v3 = 3 if diffusion_yn==0 & family_support==1
replace policy_group_v3 = 4 if diffusion_yn==0 & family_support==0

label define policy_group 1 "Egal, support" 2 "Egal, no" 3 "Traditional, support" 4 "Traditional, no"
label values policy_group_v1 policy_group_v2 policy_group_v3 policy_group

tab policy_group_v1, m // like...this is kind of a crazy distribution (in that it's roughly 25/25/25/25)
tab policy_group_v2, m // this actually makes a bit more sense - bc egal attitudes / supportive policy and trad attitudes / unsupportive policy are more clustered (which is what I would have expected...)
tab policy_group_v3, m // this is also very even, which is crazy...

save "$created_data/scale_refresh.dta", replace

********************************************************************************
**# Exploring PCA
********************************************************************************
// https://www.stata.com/manuals/mvpca.pdf
// I *think* this might actually be better robustness for testing how to better aggregate my variables in my original scale? rather than a simple addition?// 
/*
but is it problematic again that my data are mostly continuous or dichtomous, none of these are like...scales (which i feel like all of these methods assume)
(from manual):
"It does not assume that the data satisfy a specific statistical model, though it does require that the data be interval-level
data—otherwise taking linear combinations is meaningless."
*/

// original scale:
pca paid_leave_st prek_enrolled_public_st min_amt_above_fed_st married_earn_ratio_st unemployment_percap_st abortion_protected_st welfare_all_st
pca paid_leave prek_enrolled_public min_amt_above_fed married_earn_ratio unemployment_percap abortion_protected welfare_all // okay, exactly the same standardized or not

// new variables
pca paid_leave prek_enrolled_public earlyhs_pct_totalpop abortion_protected maternal_u5_pt_rate avg_egal_reg non_evang_lds_rate married_dual_earn_rate married_women_emp_rate_wt min_amt_above_fed wba_max high_inc_prem_pct low_inc_prem_pct married_earn_ratio welfare_all

********************************************************************************
**# Exploring LCA
********************************************************************************
// https://stats.oarc.ucla.edu/stata/dae/latent-class-analysis-stata-data-analysis-examples/
// okay, so I think my mix of variable types is one problem: Factor Analysis – Because the term "latent variable" is used, you might be tempted to use factor analysis, since that is a technique used with latent variables. However, factor analysis is used for continuous and usually normally distributed latent variables, where this latent variable, e.g., alcoholism, is categorical.
// so latent class = typically category; latent profile = typically continuous (see Stata presentation by Huber: https://stats.stackexchange.com/questions/339936/mixing-variable-types-in-latent-class-profile-analysis)
// there COULD also be a world where I make my continuous categorical using quartiles are something, but why reduce data?
// slide 6 is VERY HELPFUL: if I want to turn continuous observed to a continous latent construct, I use factor analysis.
// if you want your latent construact to be CATEGORICAL, you use LCA
// item response theory could also work for categorical observed variables

// mixing variable types:
// https://stats.stackexchange.com/questions/339936/mixing-variable-types-in-latent-class-profile-analysis
// https://www.statmodel.com/download/2006catcont1MBR.pdf
// https://hummedia.manchester.ac.uk/institutes/methods-manchester/docs/lsa.pdf

// original
paid_leave prek_enrolled_public min_amt_above_fed married_earn_ratio unemployment_percap abortion_protected welfare_all 
// variables for "diffusion"
avg_egal_reg non_evang_lds_rate married_dual_earn_rate married_women_emp_rate_wt

gsem (paid_leave abortion_protected <-  , logit) ///
(prek_enrolled_public_st min_amt_above_fed_st married_earn_ratio_st unemployment_percap_st ///
 welfare_all_st avg_egal_reg_st non_evang_lds_rate_st married_dual_earn_rate_st married_women_emp_rate_wt_st <-  , regress) ///
, lclass(C 3) nonrtolerance
estimates store class3

estimates stats class3 class4 class5 class6 // want low AIC /BIC

estimates restore class3
estat lcprob  // prob of being in each group
estat lcmean // values by group

// alt visuals
margins, predict(outcome(x) class(3)) ///
		predict(outcome(y) class(3)) 
		predict(outcome(z) class(3)) 
marginsplot

margins, predict(outcome(x) class(1)) ///
		predict(outcome(x) class(2)) ///
		predict(outcome(x) class(3))
marginsplot

/* their latent profile example
gsem (glucose insulin sspg <- _cons),    ///
     family(gaussian) link(identity)     ///
     lclass(C 3)
*/

// might need a latent STRUCTURE model (see what i downloaded)
// in stata: gllamm (http://www.gllamm.org/)

********************************************************************************
********************************************************************************
********************************************************************************
**# Old measures - no longer using
********************************************************************************
********************************************************************************
********************************************************************************

/* Resources for LCA
** https://www.stata.com/features/overview/latent-class-analysis/
** https://www.stata.com/features/latent-class-analysis/
** https://www.stata.com/meeting/uk18/slides/uk18_MacDonald.pdf
** https://stats.oarc.ucla.edu/stata/dae/latent-class-analysis-stata-data-analysis-examples/
Convergence problems
https://www.statalist.org/forums/forum/general-stata-discussion/general/1629031-not-achieving-convergence-in-latent-class-analysis
*/

/*
********************************************************************************
* Get data and create new variables
********************************************************************************

**Import state data:
use "$state_data/final_measures.dta", clear

keep if year >=1997
drop if state_fips==11

/*
attitudes: disapproval
market: min_above_fed (logit) unemployment_comp cc_pct_income
policy: senate_dems paid_leave (logit) right2work (logit) educ_spend
*/

gen egal_attitudes = 0
replace egal_attitudes=1 if disapproval < 2.21

sum unemployment_comp
gen good_unemployment=0
replace good_unemployment = 1 if unemployment_comp > (`r(mean)' + `r(sd)') 
replace good_unemployment=. if unemployment_comp==.

gen cc_afford=0
replace cc_afford=1 if cc_pct_income <=0.100000

gen liberal=0
replace liberal=1 if senate_dems > 0.50

sum educ_spend
gen high_educ_spend=0
replace high_educ_spend = 1 if educ_spend > (`r(mean)' + `r(sd)') 
replace high_educ_spend=. if educ_spend==.

********************************************************************************
* Attempt analysis
********************************************************************************

/* Failures */
/*
gsem (egal_attitudes min_above_fed good_unemployment cc_afford liberal paid_leave right2work high_educ_spend <-), logit lclass(C 3) // STILL not estimating - does this mean not enough variation?

gsem (egal_attitudes min_above_fed good_unemployment cc_afford liberal paid_leave right2work high_educ_spend <-), logit lclass(C 3) startvalues(randompr, draws(20) seed(15)) // not helping - I don't know enough but I am not sure that it's the starting values?

gsem (egal_attitudes min_above_fed good_unemployment cc_afford liberal paid_leave right2work high_educ_spend <-), logit lclass(C 3) nonrtolerance // with DC removed - right2work estimates, but cc_afford still doesn't I'm so confused
gsem (egal_attitudes min_above_fed good_unemployment cc_afford liberal paid_leave high_educ_spend <-), logit lclass(C 3) // took out right2work based on above, but still don't think helping
*/

/* Progress */
// worked sorta??
gsem (egal_attitudes min_above_fed liberal paid_leave right2work <-  , logit) ///
(unemployment_comp cc_pct_income educ_spend <-  , regress) ///
, lclass(C 3) nonrtolerance // this maybe worked?

estat lcprob
estat lcmean
estat lcgof // goodness of fit

// https://www.stata.com/manuals/semexample52g.pdf: how to test # of classes?
gsem (egal_attitudes min_above_fed liberal paid_leave  <-  , logit) ///
(unemployment_comp cc_pct_income educ_spend <-  , regress) ///
, lclass(C 3) nonrtolerance // this maybe worked?
estimates store class3

gsem (egal_attitudes min_above_fed liberal paid_leave  <-  , logit) ///
(unemployment_comp cc_pct_income educ_spend <-  , regress) ///
, lclass(C 4) nonrtolerance
estimates store class4

/*
gsem (egal_attitudes min_above_fed liberal paid_leave <-  , logit) ///
(unemployment_comp cc_pct_income educ_spend <-  , regress) ///
, lclass(C 5) nonrtolerance // feel like this didn't estimate, not adding a lot of incremental value either
estimates store class5
*/

gsem (egal_attitudes min_above_fed liberal paid_leave <-  , logit) ///
(unemployment_comp cc_pct_income educ_spend <-  , regress) ///
, lclass(C 6) nonrtolerance // removing DC maybe removed variation bc such an outlier? this didn't achieve convergence
estimates store class6

estimates stats class3 class4 class5 class6 // want low AIC /BIC

estimates restore class3
estat lcprob  // prob of being in each group
estat lcmean // values by group

estimates restore class4 // okay so this not really working either
estat lcprob  // prob of being in each group
estat lcmean // values by group

/* Trying to remove CC */
gsem (egal_attitudes min_above_fed liberal paid_leave  <-  , logit) ///
(unemployment_comp educ_spend <-  , regress) ///
, lclass(C 3) nonrtolerance // this maybe worked?
estimates store class3_alt

gsem (egal_attitudes min_above_fed liberal paid_leave  <-  , logit) ///
(unemployment_comp educ_spend <-  , regress) ///
, lclass(C 4) nonrtolerance
estimates store class4_alt

gsem (egal_attitudes min_above_fed liberal paid_leave <-  , logit) ///
(unemployment_comp educ_spend <-  , regress) ///
, lclass(C 5) nonrtolerance
estimates store class5_alt

estimates stats class3_alt class4_alt class5_alt // want low AIC /BIC

estimates restore class3_alt
estat lcprob  // prob of being in each group
estat lcmean // values by group

estimates restore class4_alt
estat lcprob  // prob of being in each group
estat lcmean // values by group

estimates restore class5_alt
estat lcprob  // prob of being in each group
estat lcmean // values by group

/* Kim test groups*/
egen policy_support = rowtotal(min_above_fed liberal paid_leave high_educ_spend good_unemployment)

gen policy_group = . 
replace policy_group = 1 if egal_attitudes==1 & policy_support >=2
replace policy_group = 2 if egal_attitudes==1 & policy_support < 2
replace policy_group = 3 if egal_attitudes==0 & policy_support >=2
replace policy_group = 4 if egal_attitudes==0 & policy_support < 2

label define policy_group 1 "Egal, support" 2 "Egal, no" 3 "Traditional, support" 4 "Traditional, no"
label values policy_group policy_group


********************************************************************************
* Make var to use?!
********************************************************************************

// predict which class a state is in to create a variable
estimates restore class4_alt

predict cpost*, classposteriorpr
egen max = rowmax(cpost*)
generate predclass = 1 if cpost1==max
replace predclass = 2 if cpost2==max
replace predclass = 3 if cpost3==max
replace predclass = 4 if cpost4==max

label define pred 1 "Trad No" 2 "Egal No" 3 "Trad Yes" 4 "Egal Yes"
label values predclass pred

tabulate predclass

/*

  predclass |      Freq.     Percent        Cum.
------------+-----------------------------------
          1 |        505       43.91       43.91
          2 |        170       14.78       58.70
          3 |        356       30.96       89.65
          4 |        119       10.35      100.00
------------+-----------------------------------
*/

tab predclass policy_group, row

browse state_name year predclass policy_group egal_attitudes min_above_fed liberal paid_leave high_educ_spend good_unemployment

save "$state_data/state_lca.dta", replace

********************************************************************************
**# Quick models - eventually merge with other file and make sure accurage but this is back of the envelope
********************************************************************************

use "$data_keep\PSID_marriage_recoded_sample.dta", clear

gen cohort=.
replace cohort=1 if inrange(rel_start_all,1969,1979)
replace cohort=2 if inrange(rel_start_all,1980,1989)
replace cohort=3 if inrange(rel_start_all,1990,2010)
replace cohort=4 if inrange(rel_start_all,2011,2019)

keep if cohort==3
keep if inlist(IN_UNIT,1,2)
keep if marriage_order_real==1
keep if (AGE_REF_>=18 & AGE_REF_<=55) &  (AGE_SPOUSE_>=18 & AGE_SPOUSE_<=55)

// alternate earnings measures
*Convert to 1000s
gen earnings_1000s = couple_earnings / 1000

*Spline
mkspline knot1 0 knot2 20 knot3 = earnings_1000s

rename STATE_ state_fips
rename survey_yr year

merge m:1 state_fips year using "T:\Research Projects\State data\data_keep\state_lca.dta"
drop if _merge==2
drop _merge

**Okay I moved these to the main file
log using "$logdir/policy_interactions_all.log", append

/* Paid */
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth knot1 knot2 knot3"
melogit dissolve_lag i.dur i.predclass i.hh_earn_type i.predclass#i.hh_earn_type `controls' if couple_educ_gp==0 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(predclass=(1(1)4))

melogit dissolve_lag i.dur i.predclass i.hh_earn_type i.predclass#i.hh_earn_type `controls' if couple_educ_gp==1 & hh_earn_type < 4 || state_fips:, or
margins, dydx(hh_earn_type) at(predclass=(1(1)4))

/* Unpaid*/
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.REGION_ cohab_with_wife cohab_with_other pre_marital_birth knot1 knot2 knot3"
melogit dissolve_lag i.dur i.predclass i.housework_bkt i.predclass#i.housework_bkt `controls' if couple_educ_gp==0 & housework_bkt < 4 || state_fips:, or
margins, dydx(housework_bkt) at(predclass=(1(1)4))

melogit dissolve_lag i.dur i.predclass i.housework_bkt i.predclass#i.housework_bkt `controls' if couple_educ_gp==1 & housework_bkt < 4 || state_fips:, or
margins, dydx(housework_bkt) at(predclass=(1(1)4))

log close


********************************************************************************
**# Creating structural sexism scale here so I can merge to file 6
********************************************************************************
// "T:\Research Projects\State data\data_keep\structural_sexism_raw.dta"

use  "$state_data/structural_sexism_raw.dta", clear
drop if statefip==11 // drop earlier so doesn't affect variable creation below

	sum earn_ratio
	gen earn_ratio_z = (earn_ratio - `r(mean)') / `r(sd)'
	
// need to flip flop some variables because some are better for women when higher and others are worse. I want HIGHER to be more struxtural sexism
// earn_ratio = fine, already men more, same with lfp and pov
gen pctmaleleg=1-pctfemaleleg // so becomes men
gen no_paid_leave = (paid_leave==0) // so becomes no
gen no_dv_gun_law = (dv_guns==0)
gen senate_rep = 1-senate_dems // becomes republican
gen house_rep = 1-house_dems // becomes republican
gen gender_mood_neg = 0-gender_mood // so higher become more negative now?

// okay 6 need to be changed to be less women friendly so actually let's flip flop others? but have to do in other file

// foreach var in earn_ratio lfp_ratio pov_ratio pctfemaleleg paid_leave senate_dems house_dems dv_guns gender_mood
foreach var in earn_ratio lfp_ratio pov_ratio pctfemaleleg paid_leave senate_dems house_dems dv_guns gender_mood pctmaleleg no_paid_leave no_dv_gun_law senate_rep house_rep gender_mood_neg{
	sum `var'
	gen `var'_st = (`var'- `r(mean)') / `r(sd)'
}

alpha earn_ratio_st lfp_ratio_st pov_ratio_st pctmaleleg_st no_paid_leave_st no_dv_gun_law_st senate_rep_st gender_mood_neg_st // 0.55
alpha earn_ratio_st lfp_ratio_st pov_ratio_st pctmaleleg_st no_paid_leave_st no_dv_gun_law_st senate_rep_st // bc don't actually want attitudes in here -  0.47

egen structural_sexism = rowtotal(earn_ratio_st lfp_ratio_st pov_ratio_st pctmaleleg_st no_paid_leave_st no_dv_gun_law_st senate_rep_st)

rename statefip state_fips

save "T:\Research Projects\State data\data_keep\structural_sexism.dta", replace

// try LCA?
drop if state_fips==11
 
gsem (no_paid_leave_st no_dv_gun_law_st <-  , logit) ///
(earn_ratio_st lfp_ratio_st pov_ratio_st pctmaleleg_st senate_rep_st gender_mood_neg_st<-  , regress) ///
, lclass(C 3) nonrtolerance 
estimates store class3

gsem (no_paid_leave_st no_dv_gun_law_st <-  , logit) ///
(earn_ratio_st lfp_ratio_st pov_ratio_st pctmaleleg_st senate_rep_st gender_mood_neg_st<-  , regress) ///
, lclass(C 4) nonrtolerance 
estimates store class4

gsem (no_paid_leave_st no_dv_gun_law_st <-  , logit) ///
(earn_ratio_st lfp_ratio_st pov_ratio_st pctmaleleg_st senate_rep_st gender_mood_neg_st<-  , regress) ///
, lclass(C 5) nonrtolerance 
estimates store class5

estimates stats class3 class4 class5 // want low AIC /BIC

estimates restore class3
estat lcprob  // prob of being in each group
estat lcmean // values by group

estimates restore class4 // okay so this not really working either
estat lcprob  // prob of being in each group
estat lcmean // values by groups*/

// wait these are wrong because I technically made all variables cotninuous, they aren't binary anymore (but paid leave and gun laws would only have 2 options anyway)


********************************************************************************
**# Structural family measure instead
********************************************************************************
/*
/* scale instead, to match structural sexism people?*/
// First recode so all are in the same direction (higher = more support)
use "$state_data/final_measures.dta", clear
merge 1:1 year state_fips using "$state_data/structural_sexism.dta"
drop _merge

merge 1:1 year state_fips using "$state_data/sexism_measures_1990_2019.dta", keepusing(maternal_employment parent_earn_ratio)
drop if _merge==2
drop _merge
drop paid_leave_st

drop if state_fips==11 // DC doesn't have a lot of these variables

gen gdp_per_cap = gdp / population

// first recode so all in the same direction - want higher to be MORE support
gen unemployment_neg = 0-unemployment
gen cc_pct_income_neg = 0-cc_pct_income
gen earn_ratio_neg = 0-earn_ratio
gen parent_earn_ratio_neg = 0-parent_earn_ratio
gen min_below_fed = (min_above_fed==0)
gen child_pov_neg = 0-child_pov
gen welfare_neg = 0-welfare_all
gen welfare_cash_neg = 0 -welfare_cash_asst
gen gdp_neg = 0-gdp
gen gdp_percap_neg = 0 - gdp_per_cap

// earn_ratio lfp_ratio pov_ratio pctfemaleleg paid_leave senate_dems house_dems dv_guns gender_mood pctmaleleg no_paid_leave no_dv_gun_law senate_rep house_rep gender_mood_neg

foreach var in unemployment_neg min_above_fed paid_leave cc_pct_income_neg earn_ratio_neg cc_subsidies unemployment cc_pct_income min_below_fed child_pov child_pov_neg min_wage welfare_all welfare_cash_asst welfare_neg welfare_cash_neg ccdf_direct ccdf_per_cap cc_served_percap cc_pct_served educ_spend educ_spend_percap parent_earn_ratio parent_earn_ratio_neg maternal_employment gdp gdp_neg gdp_per_cap gdp_percap_neg gini gender_discrimin_ban equalpay contraceptive_coverage abortion_protected unemployment_percap prek_enrolled prek_enrolled_public{
	sum `var'
	gen `var'_st = (`var'- `r(mean)') / `r(sd)'
}

/*
alpha unemployment_neg_st min_above_fed_st paid_leave_st cc_pct_income_neg_st // 0.40
alpha unemployment_neg_st min_above_fed_st paid_leave_st cc_pct_income_neg_st earn_ratio_neg_st // 0.45
alpha unemployment_neg_st min_above_fed_st paid_leave_st cc_pct_income_neg_st earn_ratio_neg_st senate_dems_st // 0.51
alpha unemployment_neg_st min_above_fed_st paid_leave_st cc_pct_income_neg_st earn_ratio_neg_st senate_dems_st child_pov_neg_st // 0.50
alpha unemployment_neg_st min_above_fed_st paid_leave_st cc_pct_income_neg_st child_pov_neg_st senate_dems_st  // 0.47
alpha unemployment_neg_st min_above_fed_st paid_leave_st cc_pct_income_neg_st earn_ratio_neg_st senate_dems_st, asis
alpha unemployment_neg_st min_above_fed_st paid_leave_st cc_pct_income_neg_st earn_ratio_neg_st senate_dems_st gender_mood_st // 0.56
alpha unemployment_neg_st min_above_fed_st paid_leave_st cc_pct_income_neg_st earn_ratio_neg_st senate_dems_st cc_subsidies_st // 0.52

alpha unemployment child_pov min_wage // lol do not cluster - bc not standard

alpha unemployment_st child_pov_st min_wage_st // 0.36
alpha unemployment_st child_pov_st min_above_fed_st // 0.36
alpha paid_leave_st cc_pct_income_st senate_dems_st // 0.35
alpha paid_leave_st welfare_all_st senate_dems_st // 0.30
alpha paid_leave_st welfare_cash_asst_st senate_dems_st // 0.31
alpha paid_leave_st welfare_cash_asst_st senate_dems_st cc_pct_income_st // 0.41

alpha unemployment_st child_pov_st min_wage_st paid_leave_st cc_pct_income_st senate_dems_st // 0.41
alpha unemployment_st child_pov_st min_above_fed_st paid_leave_st cc_pct_income_st senate_dems_st // 0.47
alpha unemployment_st child_pov_st min_below_fed_st no_paid_leave_st cc_pct_income_st senate_rep_st // 0.47
alpha unemployment_st child_pov_st min_below_fed_st no_paid_leave_st senate_rep_st welfare_cash_asst_st // 0.44
alpha unemployment_st child_pov_st min_below_fed_st no_paid_leave_st cc_pct_income_st senate_rep_st welfare_cash_asst_st // 0.49

* Feels like unemployment and cc costs are problem
alpha earn_ratio_st child_pov_st min_below_fed_st no_paid_leave_st senate_rep_st welfare_cash_neg_st // 0.43
alpha unemployment_st child_pov_st min_below_fed_st no_paid_leave_st senate_rep_st welfare_cash_neg_st // 0.44
alpha lfp_ratio_st child_pov_st min_below_fed_st no_paid_leave_st senate_rep_st welfare_cash_neg_st // 0.45

alpha unemployment_neg_st child_pov_neg_st min_above_fed_st paid_leave_st senate_dems_st welfare_cash_asst_st // 0.44
alpha unemployment_neg_st child_pov_neg_st min_above_fed_st paid_leave_st senate_dems_st welfare_cash_asst_st, asis // 0.33
alpha unemployment_neg_st child_pov_neg_st min_above_fed_st paid_leave_st welfare_cash_asst_st, asis // 0.33

pwcorr  unemployment_st child_pov_st min_above_fed_st paid_leave_st cc_pct_income_st senate_dems_st
pwcorr welfare_all_st welfare_cash_asst_st

alpha min_above_fed_st paid_leave_st senate_dems_st welfare_cash_asst_st ccdf_per_cap_st  // 0.47
alpha min_above_fed_st paid_leave_st senate_dems_st welfare_cash_asst_st ccdf_per_cap_st earn_ratio_st // 0.48
alpha min_above_fed_st paid_leave_st senate_dems_st welfare_cash_asst_st ccdf_per_cap_st unemployment_neg_st // 0.49
alpha min_above_fed_st paid_leave_st senate_dems_st welfare_cash_asst_st ccdf_per_cap_st earn_ratio_st unemployment_neg_st // 0.49
alpha min_above_fed_st paid_leave_st senate_dems_st welfare_all_st ccdf_per_cap_st  // 0.53
alpha min_above_fed_st paid_leave_st senate_dems_st welfare_all_st ccdf_per_cap_st earn_ratio_neg_st // 0.57
alpha min_above_fed_st paid_leave_st senate_dems_st welfare_all_st educ_spend_st earn_ratio_neg_st // 0.65
alpha min_above_fed_st paid_leave_st senate_dems_st welfare_all_st educ_spend_percap_st  // 0.64
alpha min_above_fed_st paid_leave_st senate_dems_st welfare_all_st educ_spend_percap_st earn_ratio_neg_st child_pov_neg_st // 0.64
alpha min_above_fed_st paid_leave_st senate_dems_st welfare_all_st educ_spend_percap_st ccdf_per_cap_st earn_ratio_neg_st // 0.67
alpha min_above_fed_st paid_leave_st senate_dems_st welfare_all_st educ_spend_percap_st ccdf_per_cap_st // 0.64

alpha min_above_fed_st paid_leave_st senate_dems_st welfare_all_st ccdf_per_cap_st unemployment_neg_st // 0.52

alpha min_above_fed_st paid_leave_st senate_dems_st welfare_cash_asst_st ccdf_per_cap_st  child_pov_neg_st // 0.44
alpha min_above_fed_st paid_leave_st senate_dems_st welfare_cash_asst_st ccdf_per_cap_st cc_served_percap_st // 0.43
alpha min_above_fed_st paid_leave_st senate_dems_st welfare_cash_asst_st ccdf_per_cap_st cc_pct_served_st // 0.45

// worried the negative coding is messing this up, let's try to do higher = less support
alpha unemployment_st min_below_fed_st no_paid_leave_st cc_pct_income_st senate_rep_st earn_ratio_st // it;s still reversing the sign so even though higher unemployment = arguably "worse" it seems as though it needs to be negative to go in alpha

*/

// using: familism
alpha min_above_fed_st paid_leave_st senate_dems_st welfare_all_st educ_spend_percap_st earn_ratio_neg_st  // 0.67
alpha min_above_fed_st paid_leave_st senate_dems_st welfare_all_st educ_spend_percap_st parent_earn_ratio_neg_st  // 0.65
alpha min_above_fed_st paid_leave_st senate_dems_st welfare_all_st educ_spend_percap_st maternal_employment_st parent_earn_ratio_neg_st // 0.62

alpha min_above_fed_st paid_leave_st senate_dems_st welfare_all_st educ_spend_percap_st parent_earn_ratio_neg_st // 0.65
alpha min_above_fed_st paid_leave_st senate_dems_st welfare_all_st educ_spend_percap_st // 0.64
alpha paid_leave_st senate_dems_st welfare_all_st educ_spend_percap_st // 0.52 - this is without minimum wage

// take 2
alpha senate_dems_st paid_leave_st ccdf_per_cap_st educ_spend_percap_st // 0.33 without new variables
alpha senate_dems_st paid_leave_st ccdf_per_cap_st educ_spend_percap_st gender_discrimin_ban_st equalpay_st contraceptive_coverage_st abortion_protected_st // 0.54 adding all now
alpha senate_dems_st paid_leave_st ccdf_per_cap_st educ_spend_percap_st equalpay_st contraceptive_coverage_st abortion_protected_st // 0.55 
alpha senate_dems_st paid_leave_st ccdf_per_cap_st educ_spend_percap_st equalpay_st contraceptive_coverage_st abortion_protected_st if year>=1999 & year<=2010 // 0.52 - only data for years where all variables there

alpha senate_dems_st paid_leave_st ccdf_per_cap_st educ_spend_percap_st equalpay_st contraceptive_coverage_st abortion_protected_st min_above_fed_st // 0.64 so min wage does make a difference
alpha senate_dems_st paid_leave_st ccdf_per_cap_st educ_spend_percap_st equalpay_st contraceptive_coverage_st abortion_protected_st welfare_all_st // 0.66 as does welfare spending
alpha senate_dems_st paid_leave_st ccdf_per_cap_st educ_spend_percap_st equalpay_st contraceptive_coverage_st abortion_protected_st min_above_fed_st welfare_all_st // 0.72 - so both is really good. but is that too many?
alpha senate_dems_st paid_leave_st ccdf_per_cap_st educ_spend_percap_st equalpay_st contraceptive_coverage_st abortion_protected_st min_above_fed_st welfare_all_st, std // matters less bc already standardized
alpha senate_dems paid_leave ccdf_per_cap educ_spend_percap equalpay contraceptive_coverage abortion_protected min_above_fed welfare_all
alpha senate_dems paid_leave ccdf_per_cap educ_spend_percap equalpay contraceptive_coverage abortion_protected min_above_fed welfare_all, std // oh so I don't actually need to standardize, could just specify here?

// adding item tells you what alpha will be if that item is removed - so can try to improve. might also want to remove those with low "item-rest" correlation meaning that variable doesn't correlate well with the scale if that item isn't in there
alpha senate_dems_st paid_leave_st ccdf_per_cap_st educ_spend_percap_st equalpay_st contraceptive_coverage_st abortion_protected_st min_above_fed_st welfare_all_st, item // remove senate dems? would improve alpha and is the most generic, so probably less tied to my goals?
alpha paid_leave_st ccdf_per_cap_st educ_spend_percap_st equalpay_st contraceptive_coverage_st abortion_protected_st min_above_fed_st welfare_all_st, item  // 0.75
alpha paid_leave_st educ_spend_percap_st equalpay_st contraceptive_coverage_st abortion_protected_st min_above_fed_st welfare_all_st, item  // 0.76

// okay maybe actually final?! (3/15/24)
alpha paid_leave_st prek_enrolled_public_st abortion_protected_st welfare_all_st // family vars
alpha min_above_fed_st parent_earn_ratio_neg_st unemployment_percap_st // work vars
alpha min_above_fed_st earn_ratio_neg_st unemployment_percap_st // work vars

alpha paid_leave_st prek_enrolled_public_st min_above_fed_st earn_ratio_neg_st unemployment_percap_st abortion_protected_st, item // now 0.61
alpha paid_leave_st prek_enrolled_public_st min_above_fed_st earn_ratio_neg_st unemployment_percap_st abortion_protected_st welfare_all_st, item // 0.71
alpha paid_leave_st prek_enrolled_public_st min_above_fed_st earn_ratio_neg_st unemployment_percap_st contraceptive_coverage_st abortion_protected_st welfare_all_st, item // 0.75

/// you can also just create a scale using alpha - see help alpha, instead of creating like I do below?

// using: economic
alpha unemployment_st child_pov_st // 0.59
alpha unemployment_st child_pov_st min_below_fed_st // 0.35
alpha unemployment_st child_pov_st gdp_percap_neg_st // 0.51
alpha unemployment_st child_pov_st cc_pct_income_st // 0.38
alpha unemployment_st child_pov_st cc_pct_income_st gdp_percap_neg_st // 0.46
alpha unemployment_st child_pov_st gini_st // 0.46
alpha unemployment_st child_pov_st gini_st gdp_percap_neg_st // 0.43
alpha unemployment_st child_pov_st gini_st parent_earn_ratio_neg_st // 0.41 - okay so the economic ones have generally low reliably

alpha unemployment_st child_pov_st cc_pct_income_st, std item

// just childcare
alpha ccdf_per_cap_st cc_pct_served_st paid_leave_st educ_spend_percap_st, item
alpha cc_pct_served_st paid_leave_st, item

**USE
egen structural_familism = rowtotal(paid_leave_st prek_enrolled_public_st min_above_fed_st earn_ratio_neg_st unemployment_percap_st abortion_protected_st welfare_all_st)

// test centering the variable as well
gen sf_centered=.
sum structural_familism, detail
replace sf_centered = structural_familism - `r(p50)'

browse structural_familism sf_centered

/*
**OLD
egen structural_familism_v0 = rowtotal(unemployment_neg_st child_pov_neg_st min_above_fed_st paid_leave_st senate_dems_st welfare_cash_asst_st)
egen structural_familism = rowtotal(min_above_fed_st paid_leave_st senate_dems_st welfare_all_st educ_spend_percap_st parent_earn_ratio_neg_st)
egen structural_familism_alt = rowtotal(min_above_fed_st paid_leave_st senate_dems_st welfare_all_st educ_spend_percap_st)
*/

egen economic_challenges = rowtotal(unemployment_st child_pov_st gini_st)
egen economic_challenges_alt = rowtotal(unemployment_st child_pov_st)

// tabstat structural_familism_v0 structural_familism, by(state)
tabstat structural_familism, by(state)
tabstat structural_familism paid_leave prek_enrolled_public min_above_fed earn_ratio_neg unemployment_percap abortion_protected welfare_all, by(state)
browse state year structural_familism unemployment child_pov min_above_fed paid_leave senate_dems welfare_cash_asst
// pwcorr structural_familism_v0 structural_familism
pwcorr structural_familism structural_sexism // negative correlated which makes sense - sexism = bad, familism = good
pwcorr structural_familism economic_challenges // hmm positively correlated
pwcorr structural_familism economic_challenges_alt // no correlation

**Second approach: Factor-based
factor paid_leave_st prek_enrolled_public_st min_above_fed_st earn_ratio_neg_st unemployment_percap_st abortion_protected_st welfare_all_st, ipf  // ccdf_per_cap_st
predict f1
pwcorr f1 structural_familism // so if I make a factor variable, VERY correlated

factor unemployment_st child_pov_st min_above_fed_st paid_leave_st senate_dems_st welfare_cash_asst_st, ipf

factor unemployment_st child_pov_st gini_st, ipf 
rotate, varimax

gen year_t1 = year // to get t-1 measures

save "$state_data/structural_familism.dta", replace
*/

/*
** LCA
gsem (paid_leave_st min_above_fed_st <-  , logit) ///
(unemployment_st welfare_cash_asst_st child_pov_st senate_dems_st <-  , regress) ///
, lclass(C 3) nonrtolerance
estimates store class3

gsem (paid_leave_st min_above_fed_st <-  , logit) ///
(unemployment_st cc_pct_income_st child_pov_st senate_dems_st <-  , regress) ///
, lclass(C 4) nonrtolerance
estimates store class4

estimates stats class3 class4

estimates restore class4
estat lcprob  // prob of being in each group
estat lcmean // values by group
*/
/*

Latent class marginal probabilities                      Number of obs = 1,500

--------------------------------------------------------------
             |            Delta-method
             |     Margin   std. err.     [95% conf. interval]
-------------+------------------------------------------------
           C |
          1  |   .1165155   .0214499      .0805977    .1655577
          2  |   .0985991    .013543      .0750528    .1285063
          3  |   .6122655   .0249091      .5624704     .659821
          4  |   .1726198   .0192179      .1381309    .2135861
--------------------------------------------------------------

. estat lcmean // values by group

Latent class marginal means                              Number of obs = 1,500

----------------------------------------------------------------------------------
                 |            Delta-method
                 |     Margin   std. err.      z    P>|z|     [95% conf. interval]
-----------------+----------------------------------------------------------------
1                |
      paid_leave |   4.81e-06    .000166                      2.00e-35           1
   min_above_fed |   .0525084   .0246303                      .0205695    .1275801
 unemployment_st |    -.68355    .093877    -7.28   0.000    -.8675454   -.4995545
cc_pct_income_st |   -.651969    .081906    -7.96   0.000    -.8125018   -.4914363
   earn_ratio_st |   .4325581   .1062098     4.07   0.000     .2243907    .6407255
  senate_dems_st |  -1.323205   .1144297   -11.56   0.000    -1.547483   -1.098926
-----------------+----------------------------------------------------------------
2                |
      paid_leave |   1.09e-08   3.62e-07                      5.10e-37           1
   min_above_fed |   .0139215   .0167001                      .0012993    .1328541
 unemployment_st |   .2653678   .0983174     2.70   0.007     .0726692    .4580664
cc_pct_income_st |  -.9481219   .0959761    -9.88   0.000    -1.136232   -.7600122
   earn_ratio_st |   .9547793    .124517     7.67   0.000     .7107306    1.198828
  senate_dems_st |   .9661303   .0833795    11.59   0.000     .8027094    1.129551
-----------------+----------------------------------------------------------------
3                |
      paid_leave |   1.53e-08   5.31e-06                      8.4e-305           1
   min_above_fed |   .2702374    .023508                      .2266976    .3186932
 unemployment_st |   .0271925   .0418765     0.65   0.516    -.0548838    .1092689
cc_pct_income_st |   .0611587   .0452643     1.35   0.177    -.0275577    .1498751
   earn_ratio_st |  -.0423055   .0413351    -1.02   0.306    -.1233208    .0387097
  senate_dems_st |  -.1389419   .0423831    -3.28   0.001    -.2220112   -.0558725
-----------------+----------------------------------------------------------------
4                |
      paid_leave |   .1429623   .0257176                      .0995446    .2010882
   min_above_fed |   .8008109   .0375254                      .7171338    .8644147
 unemployment_st |   .2127441   .0775184     2.74   0.006     .0608108    .3646773
cc_pct_income_st |   .7658201   .0754308    10.15   0.000     .6179785    .9136617
   earn_ratio_st |  -.4795222    .065134    -7.36   0.000    -.6071825   -.3518619
  senate_dems_st |   1.110771   .0727979    15.26   0.000     .9680899    1.253452
----------------------------------------------------------------------------------
*/