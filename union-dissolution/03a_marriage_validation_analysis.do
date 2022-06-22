********************************************************************************
* Analysis to compare to prior studies
* marriage_validation_analysis.do
* Kim McErlean
********************************************************************************

use "$data_keep\PSID_marriage_validation_sample.dta", clear

tab rel_start_all
browse id survey_yr marriage_order rel_start_all rel1_start rel1_end rel2_start rel3_start FIRST_MARRIAGE_YR_START if rel_start_all==. // are these cohabitations? started in between marriage dates -- think this is why I think cohab not accurate - i am replacing this info with JUST marital history

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
browse id survey_yr rel_start_all rel_end_all dissolve in_sh_sample

//k their findings: cohort 1 - hypo sig worse than hyper, no diff with homo
// cohort 2 - hyper and hypo similar, homo = sig less
logit dissolve i.educ_type if cohort_sh==1 & inlist(IN_UNIT,1,2), or // hypo and homo sig less likely that hyper to dissolve in all - WHY?! more educated?
logit dissolve i.educ_type if cohort_sh==2 & inlist(IN_UNIT,1,2), or //
logit dissolve i.educ_type if cohort_sh==3 & inlist(IN_UNIT,1,2), or //

logit dissolve i.educ_type if cohort_sh==1 & inlist(IN_UNIT,1,2) & in_sh_sample==1, or // with this, nothing sig - but makes more sense
logit dissolve i.educ_type if cohort_sh==2 & inlist(IN_UNIT,1,2) & in_sh_sample==1, or
logit dissolve i.educ_type if cohort_sh==3 & inlist(IN_UNIT,1,2) & in_sh_sample==1, or


logit dissolve i.first_educ_type if cohort_sh==1 & inlist(IN_UNIT,1,2), or // still all sig less likely...
logit dissolve i.first_educ_type if cohort_sh==2 & inlist(IN_UNIT,1,2), or //
logit dissolve i.first_educ_type if cohort_sh==3 & inlist(IN_UNIT,1,2), or //

logit dissolve i.educ_type_lag if cohort_sh==1 & inlist(IN_UNIT,1,2), or // nothing sig
logit dissolve i.educ_type_lag if cohort_sh==2 & inlist(IN_UNIT,1,2), or // homo < hypo
logit dissolve i.educ_type_lag if cohort_sh==3 & inlist(IN_UNIT,1,2), or // homo<hypo


local controls "dur i.race_head i.same_race i.children age_mar_head age_mar_wife marriage_order educ_head educ_wife"
logit dissolve i.educ_type `controls' if cohort_sh==1 & inlist(IN_UNIT,1,2)  & in_sh_sample==1, or // nothing sig
logit dissolve i.educ_type `controls' if cohort_sh==2 & inlist(IN_UNIT,1,2)  & in_sh_sample==1, or // hypo worse, but not sig
logit dissolve i.educ_type `controls' if cohort_sh==3 & inlist(IN_UNIT,1,2)  & in_sh_sample==1, or // both less

local controls "dur i.race_head i.same_race i.children age_mar_head age_mar_wife marriage_order educ_head educ_wife"
logit dissolve i.first_educ_type `controls' if cohort_sh==1 & inlist(IN_UNIT,1,2)  & in_sh_sample==1, or
logit dissolve i.first_educ_type `controls' if cohort_sh==2 & inlist(IN_UNIT,1,2)  & in_sh_sample==1, or
logit dissolve i.first_educ_type `controls' if cohort_sh==3 & inlist(IN_UNIT,1,2)  & in_sh_sample==1, or

local controls "dur i.race_head i.same_race i.children age_mar_head age_mar_wife marriage_order educ_head educ_wife"
logit dissolve i.educ_type_lag `controls' if cohort_sh==1 & inlist(IN_UNIT,1,2)  & in_sh_sample==1, or
logit dissolve i.educ_type_lag `controls' if cohort_sh==2 & inlist(IN_UNIT,1,2)  & in_sh_sample==1, or
logit dissolve i.educ_type_lag `controls' if cohort_sh==3 & inlist(IN_UNIT,1,2)  & in_sh_sample==1, or

local controls "dur i.race_head i.same_race i.children age_mar_head age_mar_wife marriage_order educ_head educ_wife"
logit dissolve i.educ_type `controls' if cohort_sh==1 & inlist(IN_UNIT,1,2), or // nothing sig
logit dissolve i.educ_type `controls' if cohort_sh==2 & inlist(IN_UNIT,1,2), or // hypo sig worse
logit dissolve i.educ_type `controls' if cohort_sh==3 & inlist(IN_UNIT,1,2), or // nothing sig

local controls "dur i.race_head i.same_race i.children age_mar_head age_mar_wife marriage_order educ_head educ_wife"
logit dissolve i.educ_type `controls' if cohort_sh_detail==1, or
logit dissolve i.educ_type `controls' if cohort_sh_detail==2, or
logit dissolve i.educ_type `controls' if cohort_sh_detail==3, or
logit dissolve i.educ_type `controls' if cohort_sh_detail==4, or
logit dissolve i.educ_type `controls' if cohort_sh_detail==5, or
logit dissolve i.educ_type `controls' if cohort_sh_detail==6, or
logit dissolve i.educ_type `controls' if cohort_sh_detail==7, or

logit dissolve i.educ_type educ_wife age_mar_wife if cohort_sh_detail==1 & in_sh_sample==1, or // hypo sig worse
logit dissolve i.educ_type educ_wife age_mar_wife if cohort_sh_detail==7 & in_sh_sample==1, or // okay wait adjusting here - makes it look similar to old one? is this why need ot interact? i am losing my mind


foreach var in dur i.race_head i.same_race i.children age_mar_head age_mar_wife marriage_order educ_head educ_wife{
	logit dissolve `var' i.educ_type if cohort_sh_detail==1, or // hypo sig worse
}

// dur no, race no, age mar wife starts to get there as does educ_wife 

foreach var in dur i.race_head i.same_race i.children age_mar_head age_mar_wife marriage_order educ_head educ_wife{
	logit dissolve `var' i.educ_type if cohort_sh_detail==7, or // hypo sig worse
}

local controls "dur i.race_head i.same_race i.children age_mar_head age_mar_wife marriage_order educ_head educ_wife"
logit dissolve i.cohort_sh_detail##i.educ_type `controls' if inlist(IN_UNIT,1,2), or // is this same issue as Schwartz and GP - need to interact, not run separately? maybe in cohort one (main effects) hypo IS sig worse, but homo is also sig worse by later cohort? is this because of interacton??
margins cohort_sh_detail##educ_type
marginsplot
logit dissolve i.cohort_sh_detail##i.educ_type `controls' if inlist(IN_UNIT,1,2), or nocons

local controls "dur i.race_head i.same_race i.children age_mar_head age_mar_wife marriage_order educ_head educ_wife"
logit dissolve i.cohort_sh_detail##i.educ_type `controls' if in_sh_sample==1, nocons or

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

logit dissolve couple_earnings if in_age==1, or // this is sig negative, unlike Killewald
logit dissolve economic_well_being if in_age==1, or // still sig negative
logit dissolve ft_head if in_age==1, or // sig neg - same
logit dissolve ft_wife if in_age==1, or // sig pos - same

local controls "dur i.race_head i.same_race i.children i.educ_wife i.educ_head age_mar_head age_mar_wife"
logit dissolve couple_earnings `controls' if in_age==1 // this is sig negative, unlike Killewald, i thought maybe education explained it - but doesn't
logit dissolve economic_well_being `controls' if in_age==1 // okay still sig negative.
logit dissolve ft_head `controls' if in_age==1, or // sig neg - same
logit dissolve ft_wife `controls' if in_age==1, or // sig pos - same

local controls "dur i.race_head i.same_race i.children i.educ_wife i.educ_head age_mar_head age_mar_wife"
logit dissolve economic_well_being `controls' if in_age==1 & cohort_k==1, or // not sig - matches killewald
logit dissolve couple_earnings `controls' if in_age==1 & cohort_k==1, or // not sig - matches killewald
logit dissolve ft_head `controls' if in_age==1 & cohort_k==1, or // not sig, mathces killewald
logit dissolve ft_wife `controls' if in_age==1 & cohort_k==1, or // sig positive - also doesn't match killwald, not sig, but odds ratio similar (1.30) - literally matches, this precision (which she says - let me see if gets smaller)

local controls "dur i.race_head i.same_race i.children i.educ_wife i.educ_head age_mar_head age_mar_wife"
logit dissolve economic_well_being `controls' if in_age==1 & cohort_k==2, or // okay is sig negative here, again, unlike killewald - BUT i have more divorces in my sample because more years of data.. so that is totally possible.
logit dissolve couple_earnings `controls' if in_age==1 & cohort_k==2, or // okay is sig negative here, again, unlike killewald - BUT i have more divorces in my sample because more years of data.. so that is totally possible.
logit dissolve ft_head `controls' if in_age==1 & cohort_k==2, or // sig negative (OR = 0.72 - aligns with killewald that got more associated over time)
logit dissolve ft_wife `controls' if in_age==1 & cohort_k==2, or // not sig (OR=1.07, again, like ALMOST EXACTLY killewald)

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

gen divorce_date = relationship_end if dissolve==1
bysort id (divorce_date): replace divorce_date = divorce_date[1]
sort id survey_yr

gen in_div_sample=0
replace in_div_sample=1 if divorce_date <=2009 | divorce_date==.
replace in_div_sample=0 if age_mar_wife<16 | age_mar_wife>40 // restriction they used
replace in_div_sample=0 if SEX_HEAD_ ==2 // head NOT male when I assuming it is (small %)
replace in_div_sample=0 if inlist(SEX_WIFE_,0,1) // head NOT male when I assuming it is (small %)

browse id survey_yr rel_start_all relationship_end dissolve divorce_date in_div_sample

// weighted?! browse id survey_yr FAMILY_INTERVIEW_NUM_ CORE_WEIGHT_ 
gen weight = .
replace weight = CORE_WEIGHT_ if inrange(survey_yr,1968,1992)
replace weight = COR_IMM_WT_ if inrange(survey_yr,1993,2019)


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

xtile female_pct_bucket = female_earn_pct_lag, cut(group)
browse female_pct_bucket female_earn_pct_lag

// to mimic their table 2
input group2
.10
.50
.70
end

xtile female_pct_bucket2 = female_earn_pct_lag, cut(group2)
browse female_pct_bucket2 female_earn_pct_lag

logit dissolve female_earn_pct_lag, or // sig pos
logit dissolve female_earn_pct_lag if cohort_sgp==1, or // not sig
logit dissolve female_earn_pct_lag if cohort_sgp==3, or // not sig
logit dissolve female_earn_pct_lag if cohort_sgp==4, or // not sig.

logit dissolve i.female_pct_bucket##i.cohort_sgp if cohort_sgp<4, or
margins female_pct_bucket#cohort_sgp
marginsplot
// okay actually pretty similar EXCEPT last bucket. Again, another thing - the more PSID data, the more divorces one gets... so it's plausible it has changed
// with controls (below) - more aligned, decently similar
local controls "dur i.race_head i.same_race i.children i.educ_wife i.educ_head age_mar_head age_mar_wife"
logit dissolve i.female_pct_bucket##i.cohort_sgp `controls' if cohort_sgp<4, or
margins female_pct_bucket#cohort_sgp
marginsplot

// adding sample restrictions and weights
local controls "dur i.race_head i.same_race i.children i.educ_wife i.educ_head age_mar_head age_mar_wife"
logit dissolve i.female_pct_bucket##i.cohort_sgp `controls' if cohort_sgp<4 & in_div_sample==1 & inlist(IN_UNIT,1,2) [pweight=weight], or
margins female_pct_bucket#cohort_sgp
marginsplot

local controls "dur i.race_head i.same_race i.children i.educ_wife i.educ_head age_mar_head age_mar_wife"
logit dissolve dur i.female_pct_bucket2 `controls' if cohort_sgp==1, or // pos and sig
logit dissolve dur i.female_pct_bucket2 `controls' if cohort_sgp==2, or // pos and sig
logit dissolve dur i.female_pct_bucket2 `controls' if cohort_sgp==3, or // pos in 3 and 4
logit dissolve dur i.female_pct_bucket2 `controls' if cohort_sgp==4, or // 4=still sig


// problem is also - not really not in sample, but in sample as INTACT. okay also need to get rid of immigrant sample
local controls "dur i.race_head i.same_race i.children i.educ_wife i.educ_head age_mar_head age_mar_wife"
logit dissolve dur i.female_pct_bucket2 `controls' if cohort_sgp==1 & in_div_sample==1 & inlist(IN_UNIT,1,2), or // 2 sig, nothing else is
logit dissolve dur i.female_pct_bucket2 `controls' if cohort_sgp==2 & in_div_sample==1 & inlist(IN_UNIT,1,2), or // nothing sig
logit dissolve dur i.female_pct_bucket2 `controls' if cohort_sgp==3 & in_div_sample==1 & inlist(IN_UNIT,1,2), or // bothing sig
logit dissolve dur i.female_pct_bucket2 `controls' if cohort_sgp==4 & in_div_sample==1 & inlist(IN_UNIT,1,2), or // 4 sig

local controls "dur i.race_head i.same_race i.children i.educ_wife i.educ_head age_mar_head age_mar_wife couple_earnings i.employed_ly_wife NUM_MARRIED" 
logit dissolve dur i.female_pct_bucket2 `controls' if cohort_sgp==1 & inlist(IN_UNIT,1,2) [pweight=weight], or // 2 and3  sig
logit dissolve dur i.female_pct_bucket2 `controls' if cohort_sgp==2 & inlist(IN_UNIT,1,2) [pweight=weight], or // 3 and 4 sig
logit dissolve dur i.female_pct_bucket2 `controls' if cohort_sgp==3 & inlist(IN_UNIT,1,2) [pweight=weight], or // 3 sig
logit dissolve dur i.female_pct_bucket2 `controls' if cohort_sgp==4 & inlist(IN_UNIT,1,2) [pweight=weight], or // 4 sig


**************This is closest I have gotten**************
local controls "dur i.race_head i.same_race i.children i.educ_wife i.educ_head age_mar_head age_mar_wife couple_earnings i.employed_ly_wife NUM_MARRIED" // trying to get controls to match table 2
logit dissolve i.cohort_sgp##i.female_pct_bucket2 `controls' if in_div_sample==1 & inlist(IN_UNIT,1,2) [pweight=weight], or
/// also - is it because I am estimating separately and the association ispositive later, but NOT THE SAME?
// wait okay with the INTERACTION, is that actually right? - cohort 3, bucket 4, sig negative? cohort 4 is just off
margins female_pct_bucket2#cohort_sgp
marginsplot

local controls "dur i.race_head i.same_race i.children i.educ_wife i.educ_head age_mar_head age_mar_wife couple_earnings i.employed_ly_wife NUM_MARRIED" // trying to get controls to match table 2
logit dissolve i.cohort_sgp##i.female_pct_bucket2 `controls' if inlist(IN_UNIT,1,2) [pweight=weight], or
**************This is closest I have gotten**************

local controls "dur i.race_head i.same_race i.children i.educ_wife i.educ_head age_mar_head age_mar_wife"
logit dissolve i.female_pct_bucket `controls' if cohort_sgp==1 & in_div_sample==1 & inlist(IN_UNIT,1,2) [pweight=weight], or
logit dissolve i.female_pct_bucket `controls' if cohort_sgp==2 & in_div_sample==1 & inlist(IN_UNIT,1,2) [pweight=weight], or
logit dissolve i.female_pct_bucket `controls' if cohort_sgp==3 & in_div_sample==1 & inlist(IN_UNIT,1,2) [pweight=weight], or
logit dissolve i.female_pct_bucket `controls' if cohort_sgp==4 & in_div_sample==1 & inlist(IN_UNIT,1,2) [pweight=weight], or

// why did the buckets work to match the chart, but these estimates are NOWHERE CLOSE - it's definitely also the 80-100, is it because of 100%, but seems like they do include that?

gen female_50 = .
replace female_50=0 if female_earn_pct <0.5
replace female_50=1 if female_earn_pct >=0.5 & female_earn_pct!=. 

local controls "dur i.race_head i.same_race i.children i.educ_wife i.educ_head age_mar_head age_mar_wife"
logit dissolve i.female_50 `controls' if cohort_sgp==1, or //  sig pos (no assoc in theirs)
logit dissolve i.female_50 `controls' if cohort_sgp==2, or // sig pos (no assoc in theirs)
logit dissolve i.female_50 `controls' if cohort_sgp==3, or // no assoc (theirs is sig neg)
logit dissolve i.female_50 `controls' if cohort_sgp==4, or // sig pos (theirs is sig neg - but they def acknowledge here not enough data - but all of mine are more negative than theirs)

local controls "dur i.race_head i.same_race i.children i.educ_wife i.educ_head age_mar_head age_mar_wife"
logit dissolve i.cohort_sgp##i.female_50 `controls', or // okay but the interaction here doesn't help

local controls "dur i.race_head i.same_race i.children i.educ_wife i.educ_head age_mar_head age_mar_wife"
logit dissolve i.cohort_sgp##i.female_50 `controls' if in_div_sample==1 & inlist(IN_UNIT,1,2) [pweight=weight], or // with sample restrictions gets better, but still not sig the way theirs is. for 3 and 4

// other checks
tab cohort_sgp_alt if in_div_sample==1 & inlist(IN_UNIT,1,2), sum(female_earn_pct)
tab cohort_sgp, sum(female_earn_pct)

tab cohort_sgp if earnings_wife>0 & earnings_head>0 & in_div_sample==1 & inlist(IN_UNIT,1,2), sum(female_earn_pct) // the "dual-earner"
tab cohort_sgp if earnings_head>0 & in_div_sample==1 & inlist(IN_UNIT,1,2), sum(female_earn_pct)

gen wife_earns_more=0
replace wife_earns_more=1 if earnings_wife>earnings_head

tab wife_earns_more if cohort_sgp_alt==1 & in_div_sample==1 & inlist(IN_UNIT,1,2) // okay this percentage matches
tab wife_earns_more if cohort_sgp_alt==1 & in_div_sample==1 & inlist(IN_UNIT,1,2) , sum(female_earn_pct) // so does this
tab wife_earns_more if cohort_sgp_alt==1 // & in_div_sample==1 & inlist(IN_UNIT,1,2) // okay this percentage matches
tab wife_earns_more if cohort_sgp_alt==1, sum(female_earn_pct) // so does this

********************************************************************************
* Brines and Joyner 1999
********************************************************************************

********************************************************************************
* Cooke 2006
********************************************************************************
