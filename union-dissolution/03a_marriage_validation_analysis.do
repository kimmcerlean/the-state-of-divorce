********************************************************************************
* Analysis to compare to prior studies
* marriage_validation_analysis.do
* Kim McErlean
********************************************************************************

use "$data_keep\PSID_marriage_recoded_sample.dta", clear

tab rel_start_all
browse id survey_yr marriage_order rel_start_all rel1_start rel1_end rel2_start rel3_start FIRST_MARRIAGE_YR_START if rel_start_all==. // are these cohabitations? started in between marriage dates -- think this is why I think cohab not accurate - i am replacing this info with JUST marital history
// when I did all my cleanup in previous file - these went away

// weighted?! browse id survey_yr FAMILY_INTERVIEW_NUM_ CORE_WEIGHT_ 
gen weight = .
replace weight = CORE_WEIGHT_ if inrange(survey_yr,1968,1992)
replace weight = COR_IMM_WT_ if inrange(survey_yr,1993,2019)

********************************************************************************
* Schwartz and Han 2014 - okay when I fixed coding, no longer can validate
* historical, but can validate current. does that mean at least my current measurse
* are fine.
* it was either the censor widow or the split start / end dates. check i did both right
********************************************************************************
unique id, by(rel_start_all)
unique id if dissolve==1, by(rel_start_all)
unique id if survey_yr <=2009, by(rel_start_all)
unique id if dissolve==1 & survey_yr <=2009, by(rel_start_all)

gen educ_type_lag=.
sort id survey_yr
replace educ_type_lag=educ_type[_n-1] if id==id[_n-1]

label values educ_type_lag educ_type

bysort id: egen first_survey_yr = min(survey_yr)
gen first_educ_type = .
replace first_educ_type = educ_type if survey_yr == first_survey_yr
bysort id (first_educ_type): replace first_educ_type = first_educ_type[1]
label values first_educ_type educ_type

sort id survey_yr
browse id survey_yr educ_type educ_type_lag first_educ_type

gen cohort_sh=.
replace cohort_sh=1 if inrange(rel_start_all,1950,1967) // problem with 1950 marriages is that, if in 1968 PSID - biased towards LONGER marriages okay
replace cohort_sh=2 if inrange(rel_start_all,1969,1979) // worried about my pre-1970 marriages, seeing if even like 1970 matches
replace cohort_sh=3 if inrange(rel_start_all,2000,2010)

gen cohort_sh_detail=.
replace cohort_sh_detail=1 if inrange(rel_start_all,1970,1974)
replace cohort_sh_detail=2 if inrange(rel_start_all,1975,1979)
replace cohort_sh_detail=3 if inrange(rel_start_all,1980,1984)
replace cohort_sh_detail=4 if inrange(rel_start_all,1985,1989)
replace cohort_sh_detail=5 if inrange(rel_start_all,1990,1994)
replace cohort_sh_detail=6 if inrange(rel_start_all,1995,1999)
replace cohort_sh_detail=7 if inrange(rel_start_all,2000,2004)

gen in_sh_sample=1
replace in_sh_sample=0 if age_mar_wife<16 | age_mar_wife>40
replace in_sh_sample=0 if survey_yr > 2009
replace in_sh_sample=0 if SEX_HEAD_ ==2

sort id survey_yr
browse id survey_yr rel_start_all rel_end_all dissolve dissolve_lag in_sh_sample

//k their findings: cohort 1 - hypo sig worse than hyper, no diff with homo
// cohort 2 - hyper and hypo similar, homo = sig less
logit dissolve_lag i.educ_type if cohort_sh==1 & inlist(IN_UNIT,1,2), or // nothing sig
logit dissolve_lag i.educ_type if cohort_sh==2 & inlist(IN_UNIT,1,2), or // nothing sig
logit dissolve_lag i.educ_type if cohort_sh==3 & inlist(IN_UNIT,1,2), or // nothing sig

logit dissolve_lag i.educ_type if cohort_sh==1 & inlist(IN_UNIT,1,2) & in_sh_sample==1, or // with this, nothing sig - but makes more sense
logit dissolve_lag i.educ_type if cohort_sh==2 & inlist(IN_UNIT,1,2) & in_sh_sample==1, or
logit dissolve_lag i.educ_type if cohort_sh==3 & inlist(IN_UNIT,1,2) & in_sh_sample==1, or

logit dissolve_lag i.educ_type if cohort_sh==1 & inlist(IN_UNIT,1,2) & in_sh_sample==1 [pweight=weight], or // with this, nothing sig - but makes more sense
logit dissolve_lag i.educ_type if cohort_sh==2 & inlist(IN_UNIT,1,2) & in_sh_sample==1 [pweight=weight], or
logit dissolve_lag i.educ_type if cohort_sh==3 & inlist(IN_UNIT,1,2) & in_sh_sample==1 [pweight=weight], or

logit dissolve_lag i.first_educ_type if cohort_sh==1 & inlist(IN_UNIT,1,2), or //
logit dissolve_lag i.first_educ_type if cohort_sh==2 & inlist(IN_UNIT,1,2), or //
logit dissolve_lag i.first_educ_type if cohort_sh==3 & inlist(IN_UNIT,1,2), or // can get homo to be sig less here, but can't get hypo to be worse earlier

logit dissolve_lag i.first_educ_type if cohort_sh==1 & inlist(IN_UNIT,1,2) & in_sh_sample==1, or
logit dissolve_lag i.first_educ_type if cohort_sh==2 & inlist(IN_UNIT,1,2) & in_sh_sample==1, or
logit dissolve_lag i.first_educ_type if cohort_sh==3 & inlist(IN_UNIT,1,2) & in_sh_sample==1, or

local controls "dur i.race_head i.same_race i.children age_mar_head age_mar_wife marriage_order educ_head educ_wife ct_marriages"
logit dissolve_lag i.educ_type `controls' if cohort_sh==1 & inlist(IN_UNIT,1,2) [pweight=weight], or // nothing sig
logit dissolve_lag i.educ_type `controls' if cohort_sh==2 & inlist(IN_UNIT,1,2) [pweight=weight], or // nothing sig
logit dissolve_lag i.educ_type `controls' if cohort_sh==3 & inlist(IN_UNIT,1,2) [pweight=weight], or // nothing sig

local controls "dur i.race_head i.same_race i.children age_mar_head age_mar_wife marriage_order educ_head educ_wife ct_marriages"
logit dissolve_lag i.educ_type `controls' if cohort_sh==1 & inlist(IN_UNIT,1,2)  & in_sh_sample==1 [pweight=weight], or // hypo and homo all less than hyper
logit dissolve_lag i.educ_type `controls' if cohort_sh==2 & inlist(IN_UNIT,1,2)  & in_sh_sample==1 [pweight=weight], or // hypo and homo all less than hyper
logit dissolve_lag i.educ_type `controls' if cohort_sh==3 & inlist(IN_UNIT,1,2)  & in_sh_sample==1 [pweight=weight], or // hypo and homo all less than hyper

local controls "dur i.race_head i.same_race i.children age_mar_head age_mar_wife marriage_order educ_head educ_wife ct_marriages"
logit dissolve_lag i.first_educ_type `controls' if cohort_sh==1 & inlist(IN_UNIT,1,2)  & in_sh_sample==1 [pweight=weight], or // hypo and homo all less than hyper
logit dissolve_lag i.first_educ_type `controls' if cohort_sh==2 & inlist(IN_UNIT,1,2)  & in_sh_sample==1 [pweight=weight], or //
logit dissolve_lag i.first_educ_type `controls' if cohort_sh==3 & inlist(IN_UNIT,1,2)  & in_sh_sample==1 [pweight=weight], or // hypo and homo all less than hyper

local controls "dur i.race_head i.same_race i.children age_mar_head age_mar_wife marriage_order educ_head educ_wife"
logit dissolve i.educ_type_lag `controls' if cohort_sh==1 & inlist(IN_UNIT,1,2)  & in_sh_sample==1, or
logit dissolve i.educ_type_lag `controls' if cohort_sh==2 & inlist(IN_UNIT,1,2)  & in_sh_sample==1, or
logit dissolve i.educ_type_lag `controls' if cohort_sh==3 & inlist(IN_UNIT,1,2)  & in_sh_sample==1, or

local controls "dur i.race_head i.same_race i.children age_mar_head age_mar_wife marriage_order educ_head educ_wife"
logit dissolve_lag i.educ_type `controls' if cohort_sh_detail==1, or // nothing sig
logit dissolve_lag i.educ_type `controls' if cohort_sh_detail==2, or // hypo and homo both sig worse
logit dissolve_lag i.educ_type `controls' if cohort_sh_detail==3, or // hypo higher but not sig
logit dissolve_lag i.educ_type `controls' if cohort_sh_detail==4, or // nothing sig
logit dissolve_lag i.educ_type `controls' if cohort_sh_detail==5, or // hypo and homo both higher but not sig
logit dissolve_lag i.educ_type `controls' if cohort_sh_detail==6, or // hypo and homo both lower
logit dissolve_lag i.educ_type `controls' if cohort_sh_detail==7, or // hypo and homo both lower

local controls "dur i.race_head i.same_race i.children age_mar_head age_mar_wife marriage_order educ_head educ_wife"
logit dissolve_lag i.educ_type `controls' if cohort_sh_detail==1 & inlist(IN_UNIT,1,2) & in_sh_sample==1, or // nothing sig
logit dissolve_lag i.educ_type `controls' if cohort_sh_detail==2 & inlist(IN_UNIT,1,2) & in_sh_sample==1, or // hypo higher but not sig
logit dissolve_lag i.educ_type `controls' if cohort_sh_detail==3 & inlist(IN_UNIT,1,2) & in_sh_sample==1, or // hypo sig higher
logit dissolve_lag i.educ_type `controls' if cohort_sh_detail==4 & inlist(IN_UNIT,1,2) & in_sh_sample==1, or // nothing sig
logit dissolve_lag i.educ_type `controls' if cohort_sh_detail==5 & inlist(IN_UNIT,1,2) & in_sh_sample==1, or // hypo sig higher, homo higher but not sig
logit dissolve_lag i.educ_type `controls' if cohort_sh_detail==6 & inlist(IN_UNIT,1,2) & in_sh_sample==1, or // both lower, not sig
logit dissolve_lag i.educ_type `controls' if cohort_sh_detail==7 & inlist(IN_UNIT,1,2) & in_sh_sample==1, or // both lower, not sig

local controls "dur i.race_head i.same_race i.children age_mar_head age_mar_wife marriage_order educ_head educ_wife ct_marriages"
logit dissolve_lag i.educ_type `controls' if cohort_sh_detail==1 & inlist(IN_UNIT,1,2) & in_sh_sample==1 [pweight=weight], or // nothing sig - but all lowed
logit dissolve_lag i.educ_type `controls' if cohort_sh_detail==2 & inlist(IN_UNIT,1,2) & in_sh_sample==1 [pweight=weight], or // hypo higher but not sig
logit dissolve_lag i.educ_type `controls' if cohort_sh_detail==3 & inlist(IN_UNIT,1,2) & in_sh_sample==1 [pweight=weight], or // nothing sig - all lower
logit dissolve_lag i.educ_type `controls' if cohort_sh_detail==4 & inlist(IN_UNIT,1,2) & in_sh_sample==1 [pweight=weight], or // nothing sig - all higher
logit dissolve_lag i.educ_type `controls' if cohort_sh_detail==5 & inlist(IN_UNIT,1,2) & in_sh_sample==1 [pweight=weight], or // nothing sig - all higher
logit dissolve_lag i.educ_type `controls' if cohort_sh_detail==6 & inlist(IN_UNIT,1,2) & in_sh_sample==1 [pweight=weight], or // both lower, not sig
logit dissolve_lag i.educ_type `controls' if cohort_sh_detail==7 & inlist(IN_UNIT,1,2) & in_sh_sample==1 [pweight=weight], or // both lower, not sig

logit dissolve_lag i.educ_type i.educ_wife i.educ_head if cohort_sh_detail==1 & in_sh_sample==1, or // nothing sig but hypo highest
logit dissolve_lag i.educ_type i.educ_wife i.educ_head if cohort_sh_detail==6 & in_sh_sample==1, or // nothing sig
logit dissolve_lag i.educ_type i.educ_wife i.educ_head if cohort_sh_detail==7 & in_sh_sample==1, or // both lower but not sig

logit dissolve_lag i.educ_type i.educ_wife i.educ_head age_mar_wife if cohort_sh_detail==1, or // hypo higher but not sig
logit dissolve_lag i.educ_type i.educ_wife i.educ_head age_mar_wife if cohort_sh_detail==6, or // both lower but not sig
// I think DIVORCE is a lot more sensitive to controls, because there are just SO MANY THINGS correlated?

logit dissolve_lag i.educ_type age_mar_wife i.educ_type#i.educ_wife i.educ_head if cohort_sh_detail==1, or // honestly changes so dramatically based on what and how education of partner is included, which feels concerning - and like I dont;  understand their 6 way interaction, so is this why I acn't get?
logit dissolve_lag i.educ_type age_mar_wife i.educ_type#i.educ_wife i.educ_head if cohort_sh_detail==6, or
logit dissolve_lag i.educ_type age_mar_wife i.educ_type#i.educ_wife i.educ_head if cohort_sh_detail==7, or

logit dissolve_lag i.cohort_sh_detail##i.educ_type i.educ_wife i.educ_head  if inlist(IN_UNIT,1,2) [pweight=weight], or // slightly more sense except for last cohort
logit dissolve_lag i.cohort_sh_detail##i.educ_type i.educ_wife i.educ_head  if inlist(IN_UNIT,1,2) & in_sh_sample==1 [pweight=weight], or // slightly more sense except for last cohort

logit dissolve_lag i.cohort_sh_detail##i.educ_type i.educ_wife i.educ_head dur ct_marriages if inlist(IN_UNIT,1,2) & in_sh_sample==1 [pweight=weight], or // okay the more controls i add - the more the effects are going away...
logit dissolve_lag i.cohort_sh_detail##i.educ_type i.educ_wife i.educ_head if inlist(IN_UNIT,1,2) & in_sh_sample==1 & marriage_order_real==1 [pweight=weight], or // okay the more controls i add - the more the effects are going away...

local controls "dur i.race_head i.same_race i.children age_mar_head age_mar_wife marriage_order educ_head educ_wife ct_marriages"
logit dissolve_lag i.cohort_sh_detail##i.educ_type `controls' if inlist(IN_UNIT,1,2) [pweight=weight], or // is this same issue as Schwartz and GP - need to interact, not run separately? maybe in cohort one (main effects) hypo IS sig worse, but homo never gets sig lower in larter cohorts? is this because of interacton??
margins cohort_sh_detail##educ_type
marginsplot

local controls "dur i.race_head i.same_race i.children age_mar_head age_mar_wife marriage_order educ_head educ_wife ct_marriages"
logit dissolve_lag i.cohort_sh_detail##i.educ_type `controls' if in_sh_sample==1 [pweight=weight], or // makes more sense for early cohorts, not later - homo never better
logit dissolve_lag i.cohort_sh_detail##i.educ_type `controls' if in_sh_sample==1 & inlist(IN_UNIT,1,2) [pweight=weight], or

********************************************************************************
* Killewald 2016
********************************************************************************
gen cohort_k=.
replace cohort_k=1 if rel_start_all<=1974
replace cohort_k=2 if rel_start_all>=1975

gen in_age=0
replace in_age=1 if (AGE_REF_>=18 & AGE_REF_<=55) &  (AGE_SPOUSE_>=18 & AGE_SPOUSE_<=55)

browse survey_yr NUM_CHILDREN_ FAMILY_COMPOSITION_ TOTAL_FAMILY_INCOME_
gen family_composition = 2+NUM_CHILDREN_
gen economic_well_being = TOTAL_FAMILY_INCOME_ / (sqrt(family_composition))
browse survey_yr family_composition NUM_CHILDREN_ TOTAL_FAMILY_INCOME_ economic_well_being

logit dissolve_lag couple_earnings if in_age==1, or // this is sig negative, unlike Killewald
logit dissolve_lag economic_well_being if in_age==1, or // still sig negative
logit dissolve_lag ft_head if in_age==1, or // sig neg - same
logit dissolve_lag ft_wife if in_age==1, or // sig pos - same

local controls "dur i.race_head i.same_race i.children i.educ_wife i.educ_head age_mar_head age_mar_wife"
logit dissolve_lag couple_earnings `controls' if in_age==1 // this is sig negative, unlike Killewald, i thought maybe education explained it - but doesn't
logit dissolve_lag economic_well_being `controls' if in_age==1 // okay still sig negative.
logit dissolve_lag ft_head `controls' if in_age==1, or // sig neg - same
logit dissolve_lag ft_wife `controls' if in_age==1, or // sig pos - same

local controls "dur i.race_head i.same_race i.children i.educ_wife i.educ_head age_mar_head age_mar_wife"
logit dissolve_lag economic_well_being `controls' if in_age==1 & cohort_k==1, or // not sig - matches killewald
logit dissolve_lag couple_earnings `controls' if in_age==1 & cohort_k==1, or // not sig - matches killewald
logit dissolve_lag ft_head `controls' if in_age==1 & cohort_k==1, or // not sig, mathces killewald
logit dissolve_lag ft_wife `controls' if in_age==1 & cohort_k==1, or // sig positive - also doesn't match killwald, not sig, but odds ratio similar (1.30)

local controls "dur i.race_head i.same_race i.children i.educ_wife i.educ_head age_mar_head age_mar_wife"
logit dissolve_lag economic_well_being `controls' if in_age==1 & cohort_k==2, or // okay is sig negative here, again, unlike killewald - BUT i have more divorces in my sample because more years of data.. so that is totally possible.
logit dissolve_lag couple_earnings `controls' if in_age==1 & cohort_k==2, or // okay is sig negative here, again, unlike killewald - BUT i have more divorces in my sample because more years of data.. so that is totally possible.
logit dissolve_lag ft_head `controls' if in_age==1 & cohort_k==2, or // sig negative (OR = 0.79 - aligns with killewald that got more associated over time)
logit dissolve_lag ft_wife `controls' if in_age==1 & cohort_k==2, or // marginally sig (OR=1.09, again, like ALMOST EXACTLY killewald)

********************************************************************************
* Schwartz and Gonalons-Pons 2016
********************************************************************************
// did I remove higher order marriages?! that might also do it - CHECK

gen cohort_sgp=.
replace cohort_sgp=1 if inrange(rel_start_all,1969,1979)
replace cohort_sgp=2 if inrange(rel_start_all,1980,1989)
replace cohort_sgp=3 if inrange(rel_start_all,1990,1999)
replace cohort_sgp=4 if inrange(rel_start_all,2000,2009)

browse id survey_yr FIRST_MARRIAGE_YR_START FIRST_MARRIAGE_YR_WIFE_ FIRST_MARRIAGE_YR_HEAD_

gen cohort_sgp_alt=.
replace cohort_sgp_alt=1 if inrange(rel_start_all,1968,1979)
replace cohort_sgp_alt=2 if inrange(rel_start_all,1980,1989)
replace cohort_sgp_alt=3 if inrange(rel_start_all,1990,1999)
replace cohort_sgp_alt=4 if inrange(rel_start_all,2000,2009)

gen divorce_date = relationship_end if dissolve_lag==1
bysort id (divorce_date): replace divorce_date = divorce_date[1]
sort id survey_yr

gen in_div_sample=0
replace in_div_sample=1 if divorce_date <=2009 | divorce_date==.
replace in_div_sample=0 if age_mar_wife<16 | age_mar_wife>40 // restriction they used
replace in_div_sample=0 if SEX_HEAD_ ==2 // head NOT male when I assuming it is (small %)
replace in_div_sample=0 if inlist(SEX_WIFE_,0,1) // head NOT male when I assuming it is (small %)

gen in_survey_sample=0
replace in_survey_sample=1 if survey_yr <=2009
replace in_survey_sample=0 if age_mar_wife<16 | age_mar_wife>40 // restriction they used


/* weighted?! browse id survey_yr FAMILY_INTERVIEW_NUM_ CORE_WEIGHT_ 
gen weight = .
replace weight = CORE_WEIGHT_ if inrange(survey_yr,1968,1992)
replace weight = COR_IMM_WT_ if inrange(survey_yr,1993,2019)
*/

input group
.10
.20
.30
.40
.50
.60
.70
.80
1
end

xtile female_pct_bucket = female_earn_pct, cut(group)
browse female_pct_bucket female_earn_pct

// to mimic their table 2
input group2
.10
.50
.70
end

xtile female_pct_bucket2 = female_earn_pct, cut(group2)
browse female_pct_bucket2 female_earn_pct

logit dissolve_lag female_earn_pct, or // sig pos
logit dissolve_lag female_earn_pct if cohort_sgp==1, or // not sig
logit dissolve_lag female_earn_pct if cohort_sgp==3, or // not sig
logit dissolve_lag female_earn_pct if cohort_sgp==4, or // not sig.

logit dissolve_lag i.female_pct_bucket##i.cohort_sgp if cohort_sgp<4, or
margins female_pct_bucket#cohort_sgp
marginsplot
// okay actually pretty similar EXCEPT last bucket - this is STILL TRUE. Again, another thing - the more PSID data, the more divorces one gets... so it's plausible it has changed
// with controls (below) - more aligned, decently similar
local controls "dur i.race_head i.same_race i.children i.educ_wife i.educ_head age_mar_head age_mar_wife ct_marriages"
logit dissolve_lag i.female_pct_bucket##i.cohort_sgp `controls' if cohort_sgp<4, or
margins female_pct_bucket#cohort_sgp
marginsplot

// adding sample restrictions and weights
local controls "dur i.race_head i.same_race i.children i.educ_wife i.educ_head age_mar_head age_mar_wife ct_marriages"
logit dissolve_lag i.female_pct_bucket##i.cohort_sgp `controls' if cohort_sgp<4 & in_div_sample==1 & inlist(IN_UNIT,1,2) [pweight=weight], or
margins female_pct_bucket#cohort_sgp
marginsplot

local controls "dur i.race_head i.same_race i.children i.educ_wife i.educ_head age_mar_head age_mar_wife ct_marriages"
logit dissolve_lag dur i.female_pct_bucket2 `controls' if cohort_sgp==1, or // sig and pos
logit dissolve_lag dur i.female_pct_bucket2 `controls' if cohort_sgp==2, or // sig and pos
logit dissolve_lag dur i.female_pct_bucket2 `controls' if cohort_sgp==3, or // not sig
logit dissolve_lag dur i.female_pct_bucket2 `controls' if cohort_sgp==4, or // not sig - middle is higher like for them, but it's not REVERSING direction the way it is for them

// problem is also - not really not in sample, but in sample as INTACT, here I am specifically removing these marriages. okay also need to get rid of immigrant sample
local controls "dur i.race_head i.same_race i.children i.educ_wife i.educ_head age_mar_head age_mar_wife ct_marriages"
logit dissolve_lag dur i.female_pct_bucket2 `controls' if cohort_sgp==1 & in_div_sample==1 & inlist(IN_UNIT,1,2), or //pos and sig
logit dissolve_lag dur i.female_pct_bucket2 `controls' if cohort_sgp==2 & in_div_sample==1 & inlist(IN_UNIT,1,2), or // 3 and 4 sig
logit dissolve_lag dur i.female_pct_bucket2 `controls' if cohort_sgp==3 & in_div_sample==1 & inlist(IN_UNIT,1,2), or // nothing sig
logit dissolve_lag dur i.female_pct_bucket2 `controls' if cohort_sgp==4 & in_div_sample==1 & inlist(IN_UNIT,1,2), or // back to sig // this is most concerning

//this way removes from sample just after that year, right - so this will work. okay also need to get rid of immigrant sample
local controls "dur i.race_head i.same_race i.children i.educ_wife i.educ_head age_mar_head age_mar_wife ct_marriages"
logit dissolve_lag dur i.female_pct_bucket2 `controls' if cohort_sgp==1 & in_survey_sample==1 & inlist(IN_UNIT,1,2), or // sig and pos
logit dissolve_lag dur i.female_pct_bucket2 `controls' if cohort_sgp==2 & in_survey_sample==1 & inlist(IN_UNIT,1,2), or // 3 and 4 sig
logit dissolve_lag dur i.female_pct_bucket2 `controls' if cohort_sgp==3 & in_survey_sample==1 & inlist(IN_UNIT,1,2), or // nothing sig
logit dissolve_lag dur i.female_pct_bucket2 `controls' if cohort_sgp==4 & in_survey_sample==1 & inlist(IN_UNIT,1,2), or // nothing sig - but for them NOT sig

**************This is closest I have gotten**************
// so the survey sample thing is like - just a matter of history, those are now legit divorces so I should include, but does speak to need to WEIGHT as well as DROP IMMIGRANTS
local controls "dur i.race_head i.same_race i.children i.educ_wife i.educ_head age_mar_head age_mar_wife couple_earnings i.employed_ly_wife NUM_MARRIED" 
logit dissolve_lag dur i.female_pct_bucket2 `controls' if cohort_sgp==1 & inlist(IN_UNIT,1,2) & in_survey_sample==1 [pweight=weight], or // 1 and 2 sig - which I guess aligns (is this like the dependency thing - it's more a u than linear?)
logit dissolve_lag dur i.female_pct_bucket2 `controls' if cohort_sgp==2 & inlist(IN_UNIT,1,2) & in_survey_sample==1 [pweight=weight], or // nothing sig
logit dissolve_lag dur i.female_pct_bucket2 `controls' if cohort_sgp==3 & inlist(IN_UNIT,1,2) & in_survey_sample==1 [pweight=weight], or // nothing sig
logit dissolve_lag dur i.female_pct_bucket2 `controls' if cohort_sgp==4 & inlist(IN_UNIT,1,2) & in_survey_sample==1 [pweight=weight], or // okay here - 4 gets VERY LOW - like it does for them 0 but not sig, but isn't sig for them either...
**************This is closest I have gotten**************

**************This is now less close than above****************
local controls "dur i.race_head i.same_race i.children i.educ_wife i.educ_head age_mar_head age_mar_wife couple_earnings i.employed_ly_wife ct_marriages" // trying to get controls to match table 2
logit dissolve_lag i.cohort_sgp##i.female_pct_bucket2 `controls' if in_survey_sample==1 & inlist(IN_UNIT,1,2) [pweight=weight], or
margins female_pct_bucket2#cohort_sgp
marginsplot
**************This is now less close than above****************

local controls "dur i.race_head i.same_race i.children i.educ_wife i.educ_head age_mar_head age_mar_wife ct_marriages"
logit dissolve_lag i.female_pct_bucket `controls' if cohort_sgp==1 & in_survey_sample==1 & inlist(IN_UNIT,1,2) [pweight=weight], or // buckets 3-8 sig
logit dissolve_lag i.female_pct_bucket `controls' if cohort_sgp==2 & in_survey_sample==1 & inlist(IN_UNIT,1,2) [pweight=weight], or // buckets 5-8 sig
logit dissolve_lag i.female_pct_bucket `controls' if cohort_sgp==3 & in_survey_sample==1 & inlist(IN_UNIT,1,2) [pweight=weight], or // nothing sig
logit dissolve_lag i.female_pct_bucket `controls' if cohort_sgp==4 & in_survey_sample==1 & inlist(IN_UNIT,1,2) [pweight=weight], or // nothing sig but bucket 5 sig then declines after that? this is like mutual dependency?

/// this also generally aligns with their conclusions with less significance, but think also doing a lot of hacking for sample - and won't be so bad. also speaks to importance of CONTROL VARIABLES and honestly, in particular, first v. remarriage - definitely something there that is important to control for. or just filter...DECIDE

gen female_50 = .
replace female_50=0 if female_earn_pct <0.5
replace female_50=1 if female_earn_pct >=0.5 & female_earn_pct!=. 

local controls "dur i.race_head i.same_race i.children i.educ_wife i.educ_head age_mar_head age_mar_wife ct_marriages"
logit dissolve_lag i.female_50 `controls' if cohort_sgp==1, or //  sig pos (no assoc in theirs)
logit dissolve_lag i.female_50 `controls' if cohort_sgp==2, or // sig pos (no assoc in theirs)
logit dissolve_lag i.female_50 `controls' if cohort_sgp==3, or // no assoc (theirs is sig neg)
logit dissolve_lag i.female_50 `controls' if cohort_sgp==4, or // no assoc (theirs is sig neg - but they def acknowledge here not enough data )

local controls "dur i.race_head i.same_race i.children i.educ_wife i.educ_head age_mar_head age_mar_wife ct_marriages"
logit dissolve_lag i.cohort_sgp##i.female_50 `controls', or // interaction makes sig neg in cohort 3 and is sig pos in coh 1 - so aligns

local controls "dur i.race_head i.same_race i.children i.educ_wife i.educ_head age_mar_head age_mar_wife ct_marriages"
logit dissolve_lag i.cohort_sgp##i.female_50 `controls' if in_survey_sample==1 & inlist(IN_UNIT,1,2) [pweight=weight], or // sig pos at 1, not sig 2,3, then marg sig neg 4

// other checks
tab cohort_sgp_alt if in_div_sample==1 & inlist(IN_UNIT,1,2), sum(female_earn_pct)
tab cohort_sgp, sum(female_earn_pct)

tab cohort_sgp if earnings_wife>0 & earnings_head>0 & in_div_sample==1 & inlist(IN_UNIT,1,2), sum(female_earn_pct) // the "dual-earner"
tab cohort_sgp if earnings_head>0 & in_div_sample==1 & inlist(IN_UNIT,1,2), sum(female_earn_pct)

gen wife_earns_more=0
replace wife_earns_more=1 if earnings_wife>earnings_head

tab wife_earns_more if cohort_sgp_alt==1 & in_survey_sample==1 & inlist(IN_UNIT,1,2) // okay this percentage matches  like literally exactly
tab wife_earns_more if cohort_sgp_alt==1 & in_survey_sample==1 & inlist(IN_UNIT,1,2) , sum(female_earn_pct) // so does this
tab wife_earns_more if cohort_sgp_alt==1 // & in_survey_sample==1 & inlist(IN_UNIT,1,2) // okay this percentage matches
tab wife_earns_more if cohort_sgp_alt==1, sum(female_earn_pct) // so does this

********************************************************************************
* Cooke 2006
********************************************************************************
/* okay helpful on WEIGHT debate: see p 12
In both panels, new members marrying into the sample can be assigned
a weight of zero if they are not within the original sampling frame. For
this reason, unweighted data are used for the analysis, although a comparison of weighted and unweighted sample descriptive statistics indicates
that the two are highly similar.

In the data set, each year of a couple's marriage is a distinct observation - so they do use PY?  Constructing couple-years in
this way automatically incorporates the time-varying aspects of the independent variables, but also violates the assumption that error terms
not be correlated. Consequently, robust standard errors clustering on a
unique couple identification number are used.
*/

gen cohort_cooke=.
replace cohort_cooke=1 if inrange(rel_start_all,1985,1995) & marriage_order_real==1

gen in_cooke_sample=0
replace in_cooke_sample=1 if survey_yr <=1997 // these are such short marriages, no? like barely longer than 10 years and some even shorter

unique id if cohort_cooke==1 & in_cooke_sample==1 // they have 506 couples + 4204 couple years, lol why do I have 2000? is it because they only had peoople who also ANSWERED in these years, but I have the HISTORY of people?. 223 divorced so about half; i have 268 divorces, which seems low relative to my total number of couples...

gen wife_out_lf=0
replace wife_out_lf=1 if ft_pt_wife==0

logit dissolve_lag ft_wife if cohort_cooke==1, or // no association  (should be sig pos) - but i have WAY more dvorces than they do in this view since tracked to 2019 - the later the divorce, the less this is probably harmful?
logit dissolve_lag i.ft_pt_wife if cohort_cooke==1, or // pt sig lower than none
logit dissolve_lag ft_head if cohort_cooke==1, or // neg but not sig 
logit dissolve_lag female_earn_pct if cohort_cooke==1, or // not sig

logit dissolve_lag ft_wife if cohort_cooke==1 & in_cooke_sample==1, or // still not sig (but this aligns with Killewald's "later" cohort where this is no / small association past 1975) - okay but why is this so different to killewald? also other age restrictions I missed?
logit dissolve_lag i.ft_pt_wife if cohort_cooke==1 & in_cooke_sample==1, or // pt still good
logit dissolve_lag ft_head if cohort_cooke==1 & in_cooke_sample==1, or // sig neg
logit dissolve_lag female_earn_pct if cohort_cooke==1 & in_cooke_sample==1, or // no association
logit dissolve_lag wife_out_lf if cohort_cooke==1 & in_cooke_sample==1, or // marginally sig pos - okay does align, male BW more likely to divorce- but this is counter to the earnings being higher and more likely to divorce....
logit dissolve_lag i.hh_earn_type_bkd if cohort_cooke==1 & in_cooke_sample==1, or // nothing sig - all male marginally higher

local controls  "dur i.race_head i.same_race i.children i.educ_wife i.educ_head AGE_REF_ age_mar_wife couple_earnings"
logit dissolve_lag ft_wife `controls' if cohort_cooke==1 & in_cooke_sample==1, or // marginally sig positive - so opposite, they have, when wife out of labor force = more likely to divorce aka male BW couples more likely to divorce. okay wait this lumps part time and full time GAH
logit dissolve_lag i.ft_pt_wife `controls' if cohort_cooke==1 & in_cooke_sample==1, or // nothing sig
logit dissolve_lag ft_head `controls' if cohort_cooke==1 & in_cooke_sample==1, or // sig negative
logit dissolve_lag female_earn_pct `controls' if cohort_cooke==1 & in_cooke_sample==1, or // positive association - so this aligns
logit dissolve_lag wife_out_lf `controls' if cohort_cooke==1 & in_cooke_sample==1, or // no association once controls added - was sig (which aligned) without controls
logit dissolve_lag i.hh_earn_type_bkd `controls' if cohort_cooke==1 & in_cooke_sample==1, or // with controls, though - male sole LEAST likely to dissolve.

// test spline at 0.5
mkspline ratio1 0.5 ratio2 = female_earn_pct
browse female_earn_pct ratio1 ratio2 

local controls  "dur i.race_head i.same_race i.children i.educ_wife i.educ_head AGE_REF_ age_mar_wife couple_earnings"
logit dissolve_lag ratio1 ratio2 if cohort_cooke==1 & in_cooke_sample==1, or // not sig but definitely WORSE when above 50, and not as bad below 50
logit dissolve_lag ratio1 ratio2 `controls' if cohort_cooke==1 & in_cooke_sample==1, or  // lol with controls, both bad. think there is also something about controlling for couple earnings, which makes sense - but couple earnings are highly correlated with hh type - male BW = lower than dual earner

local controls  "dur i.race_head i.same_race i.children i.educ_wife i.educ_head AGE_REF_ age_mar_wife"
logit dissolve_lag ratio1 ratio2 `controls' if cohort_cooke==1 & in_cooke_sample==1, or  // yeah when I remove earnings - different

local controls  "dur i.race_head i.same_race i.children i.educ_wife i.educ_head AGE_REF_ age_mar_wife couple_earnings"
logit dissolve_lag wife_out_lf female_earn_pct `controls' if cohort_cooke==1 & in_cooke_sample==1, or  // when both in model (like they seem to do) - nothing sig. when couple earnigns added -female earn perecntage iss negatively assoicated. so this does speak to need ot ocnside education, beacuse also correlated? - like higher female percentage - to an extent = higher couple earnings, but then if she is primary earner, probably negative.

// how to disentangle total earnings + who is working + who contributes most??


/*
Together these results suggest that the male
breadwinner couples reinforced by policy are the most stable in West
Germany, whereas dual-earner couples are the most stable in the United
States provided a woman's earnings do not exceed her husband's
*/

// also they put some in same model, do that

sum female_earn_pct if in_cooke_sample==1 & cohort_cooke==1 // matches p 15
tab ft_pt_wife if in_cooke_sample==1 & cohort_cooke==1 // the 0s here are higher than them (25%)
tab employed_ly_wife if in_cooke_sample==1 & cohort_cooke==1 //closer?

********************************************************************************
* Brines and Joyner 1999
********************************************************************************

