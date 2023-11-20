
// No College: Employment
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"

logit dissolve_lag i.dur i.ft_head i.ft_wife knot1 knot2 knot3  `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0, or
margins, dydx(ft_head ft_wife)

logit dissolve_lag i.dur i.ft_head i.ft_wife knot1 knot2 knot3 i.num_children `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0, or // control for number of children
margins, dydx(ft_head ft_wife)

logit dissolve_lag i.dur i.ft_head i.ft_wife knot1 knot2 knot3 i.num_children age_mar_head_sq age_mar_wife_sq `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0, or // control for number of children + age marriage squared - okay so these are all similar
margins, dydx(ft_head ft_wife)

logit dissolve_lag i.dur i.ft_head i.ft_wife knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq  `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0, or // add dummy for year change (+ new controls) -- okay so this makes ft_wife statistically sig
margins, dydx(ft_head ft_wife)

logit dissolve_lag i.dur i.ft_head i.ft_wife knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq  `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0 [pweight=weight], or // adding weights - so ft_wife is marginally sig
margins, dydx(ft_head ft_wife)

logit dissolve_lag i.dur i.ft_head i.ft_wife knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq  `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0 [pweight=weight], or // adding up to 2014 - ft_wife is marginally sig
margins, dydx(ft_head ft_wife)

logit dissolve_lag i.dur i.ft_head i.ft_wife knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq  `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or // adding up to 2014, no weights - and back to nothing sig lol
margins, dydx(ft_head ft_wife)

// No College: DoL
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"

logit dissolve_lag i.dur i.hh_earn_type if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0, or // no controls
margins, dydx(hh_earn_type)

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3  `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0, or
margins, dydx(hh_earn_type)

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 i.num_children `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0, or // control for number of children
margins, dydx(hh_earn_type)

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 i.num_children age_mar_head_sq age_mar_wife_sq `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0, or // control for number of children + age marriage squared - generally similar
margins, dydx(hh_earn_type)

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0, or // add dummy for year change - okay this one changes less
margins, dydx(hh_earn_type)

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0 [pweight=weight], or // add weights - okay so this makes results significant - dual earning has HIGHEST risk of divorce
margins, dydx(hh_earn_type)

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0 [pweight=weight_rescale], or // rescaled weights
margins, dydx(hh_earn_type)

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0 [pweight=weight], or // adding up to 2014 - okay results get even stronger
margins, dydx(hh_earn_type)

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or // adding up to 2014, no weights - and back to nothing significant lol (but same directionally)
margins, dydx(hh_earn_type)

// College: Employment
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"

logit dissolve_lag i.dur i.ft_head i.ft_wife knot1 knot2 knot3  `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==1, or
margins, dydx(ft_head ft_wife)

logit dissolve_lag i.dur i.ft_head i.ft_wife knot1 knot2 knot3 i.num_children `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==1, or // control for number of children
margins, dydx(ft_head ft_wife)

logit dissolve_lag i.dur i.ft_head i.ft_wife knot1 knot2 knot3 i.num_children age_mar_head_sq age_mar_wife_sq `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==1, or // control for number of children + age marriage squared - okay so these are all similar
margins, dydx(ft_head ft_wife)

logit dissolve_lag i.dur i.ft_head i.ft_wife knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq  `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==1, or // add dummy for year change (+ new controls) -- very similar, nothing changes
margins, dydx(ft_head ft_wife)

logit dissolve_lag i.dur i.ft_head i.ft_wife knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq  `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==1 [pweight=weight], or // adding weights - now ft head = less likely and ft wife more likely
margins, dydx(ft_head ft_wife)

logit dissolve_lag i.dur i.ft_head i.ft_wife knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq  `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1 [pweight=weight], or // adding up to 2014. ft head no longer sig, ft wife is
margins, dydx(ft_head ft_wife)

logit dissolve_lag i.dur i.ft_head i.ft_wife knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq  `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1, or // adding up to 2014, no weights - okay not ft head sig (so back to original results)
margins, dydx(ft_head ft_wife)

// College: DoL
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3  `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==1, or
margins, dydx(hh_earn_type)

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 i.num_children `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==1, or // control for number of children
margins, dydx(hh_earn_type)

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 i.num_children age_mar_head_sq age_mar_wife_sq `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==1, or // control for number of children + age marriage squared - generally similar
margins, dydx(hh_earn_type)

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==1, or // add dummy for year change - okay this one changes less
margins, dydx(hh_earn_type)

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==1 [pweight=weight], or // add weights - now nothing sig
margins, dydx(hh_earn_type)

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1 [pweight=weight], or // adding up to 2014. nothing sig
margins, dydx(hh_earn_type)

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1, or // adding up to 2014, no weights - nothing sig
margins, dydx(hh_earn_type)

// alt indicator of BW
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur ib3.bw_type knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq  `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0, or
margins, dydx(bw_type)

logit dissolve_lag i.dur ib3.bw_type knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq  `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0 [pweight=weight], or
margins, dydx(bw_type)

// logit dissolve_lag i.dur i.bw_type_alt knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq  `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0, or
// margins, dydx(bw_type_alt)

// logit dissolve_lag i.dur i.bw_type_alt knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq  `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0 [pweight=weight], or
// margins, dydx(bw_type_alt)

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur ib3.bw_type knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq  `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==1, or
margins, dydx(bw_type)

logit dissolve_lag i.dur ib3.bw_type knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq  `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==1 [pweight=weight], or
margins, dydx(bw_type)

// combined
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth i.interval i.num_children age_mar_head_sq age_mar_wife_sq knot1 knot2 knot3"
logit dissolve_lag i.dur ib5.earn_type_hw `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or
margins earn_type_hw
marginsplot
margins, dydx(earn_type_hw)

logit dissolve_lag i.dur ib5.earn_type_hw  `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0 [pweight=weight], or
margins earn_type_hw
marginsplot
margins, dydx(earn_type_hw)

logit dissolve_lag i.dur ib5.earn_type_hw  `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1, or
margins earn_type_hw
marginsplot
margins, dydx(earn_type_hw)

logit dissolve_lag i.dur ib5.earn_type_hw  `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1 [pweight=weight], or
margins earn_type_hw
marginsplot
margins, dydx(earn_type_hw)

*********************************************** attempting to figure out weights
// No College: DoL
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0, or // no weights
margins, dydx(hh_earn_type)

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq AGE_REF_ AGE_SPOUSE_ `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0, or // no weights + age - yeah okay can't have age because age, duration, and age at marriage all relate to each other
margins, dydx(hh_earn_type)

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0 [pweight=weight], or // weights
margins, dydx(hh_earn_type)

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0 [iweight=weight], or // alt weight
margins, dydx(hh_earn_type)

local controls "age_mar_wife age_mar_head i.race_head i.race_wife i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0, or // alt controls - control for both husband and wife race
margins, dydx(hh_earn_type)

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0 [pweight=weight], or // alt controls - control for both husband and wife race
margins, dydx(hh_earn_type)

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 i.num_children age_mar_head_sq age_mar_wife_sq `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0, or // no weights - remove interval
margins, dydx(hh_earn_type)

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 i.num_children age_mar_head_sq age_mar_wife_sq `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0 [pweight=weight], or // weights - remove interval
margins, dydx(hh_earn_type)

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0, or // no weights - no controls
margins, dydx(hh_earn_type)

logit dissolve_lag i.dur i.hh_earn_type  knot1 knot2 knot3 if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0 [pweight=weight], or // weights - no controls
margins, dydx(hh_earn_type)

logit dissolve_lag i.dur i.hh_earn_type if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0, or // no weights - no controls
margins, dydx(hh_earn_type)

logit dissolve_lag i.dur i.hh_earn_type if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0 [pweight=weight], or // weights - no controls
margins, dydx(hh_earn_type)

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq `controls' if cohort==3 & couple_educ_gp==0, or // no weights - innclude immigrant refresh
margins, dydx(hh_earn_type)

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq `controls' if cohort==3 & couple_educ_gp==0 [pweight=weight], or // weights - innclude immigrant refresh
margins, dydx(hh_earn_type)

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth i.interval i.num_children age_mar_head_sq age_mar_wife_sq"
logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 `controls' if inlist(IN_UNIT,1,2) & cohort==3 & couple_educ_gp==0, or
margins, dydx(hh_earn_type)

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0, or
margins, dydx(hh_earn_type)

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0 [pweight=weight], or
margins, dydx(hh_earn_type)

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth i.interval i.num_children age_mar_head_sq age_mar_wife_sq"
logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 i.cds_sample `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0, or
margins, dydx(hh_earn_type)

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 i.cds_sample `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0 [pweight=weight], or
margins, dydx(hh_earn_type) // okay this is not making a difference

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 i.cds_sample `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0 [pweight=weight_adjust], or
margins, dydx(hh_earn_type)

********************************************************************************
* Race differences
********************************************************************************

local controls "age_mar_wife age_mar_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth i.couple_educ_gp i.interval i.num_children age_mar_head_sq age_mar_wife_sq"

// White
logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & race_head==1, or // no weights
margins, dydx(*) post
outreg2 using "$results/dissolution_race.xls", sideway stats(coef se pval) ctitle(White1a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & race_head==1 [pweight=weight], or // weights
margins, dydx(*) post
outreg2 using "$results/dissolution_race.xls", sideway stats(coef se pval) ctitle(White1b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur ft_head ft_wife knot1 knot2 knot3 `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & race_head==1, or // no weights
margins, dydx(*) post
outreg2 using "$results/dissolution_race.xls", sideway stats(coef se pval) ctitle(White2a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur ft_head ft_wife knot1 knot2 knot3 `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & race_head==1 [pweight=weight], or // weights
margins, dydx(*) post
outreg2 using "$results/dissolution_race.xls", sideway stats(coef se pval) ctitle(White2b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// Black
logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & race_head==2, or // no weights
margins, dydx(*) post
outreg2 using "$results/dissolution_race.xls", sideway stats(coef se pval) ctitle(Black1a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & race_head==2 [pweight=weight], or // weights
margins, dydx(*) post
outreg2 using "$results/dissolution_race.xls", sideway stats(coef se pval) ctitle(Black1b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur ft_head ft_wife knot1 knot2 knot3 `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & race_head==2, or // no weights
margins, dydx(*) post
outreg2 using "$results/dissolution_race.xls", sideway stats(coef se pval) ctitle(Black2a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur ft_head ft_wife knot1 knot2 knot3 `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & race_head==2 [pweight=weight], or // weights
margins, dydx(*) post
outreg2 using "$results/dissolution_race.xls", sideway stats(coef se pval) ctitle(Black2b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

local controls "age_mar_wife age_mar_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth i.couple_educ_gp i.interval i.num_children age_mar_head_sq age_mar_wife_sq"
logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & race_head==2 & hh_earn_type<4, or // no weights
margins, dydx(hh_earn_type)

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & race_head==2 & hh_earn_type<4 [pweight=weight], or // weights
margins, dydx(hh_earn_type)

local controls "age_mar_wife age_mar_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth i.couple_educ_gp i.interval i.num_children age_mar_head_sq age_mar_wife_sq"
logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & race_head==1 & couple_educ_gp==0, or
margins, dydx(*) post
outreg2 using "$results/dissolution_race.xls", sideway stats(coef se pval) ctitle(white_no) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & race_head==1 & couple_educ_gp==0 [pweight=weight], or
margins, dydx(*) post
outreg2 using "$results/dissolution_race.xls", sideway stats(coef se pval) ctitle(white_no_w) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & race_head==1 & couple_educ_gp==1, or
margins, dydx(*) post
outreg2 using "$results/dissolution_race.xls", sideway stats(coef se pval) ctitle(white_coll) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & race_head==1 & couple_educ_gp==1 [pweight=weight], or
margins, dydx(*) post
outreg2 using "$results/dissolution_race.xls", sideway stats(coef se pval) ctitle(white_coll_w) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & race_head==2 & couple_educ_gp==0, or
margins, dydx(*) post
outreg2 using "$results/dissolution_race.xls", sideway stats(coef se pval) ctitle(black_no) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & race_head==2 & couple_educ_gp==0 [pweight=weight], or
margins, dydx(*) post
outreg2 using "$results/dissolution_race.xls", sideway stats(coef se pval) ctitle(black_no_w) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & race_head==2 & couple_educ_gp==1, or
margins, dydx(*) post
outreg2 using "$results/dissolution_race.xls", sideway stats(coef se pval) ctitle(black_coll) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & race_head==2 & couple_educ_gp==1 [pweight=weight], or
margins, dydx(*) post
outreg2 using "$results/dissolution_race.xls", sideway stats(coef se pval) ctitle(black_coll_w) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

tab race_head hh_earn_type if inlist(race_head,1,2), row
tab race_head hh_earn_type if inlist(race_head,1,2) [aweight=weight], row

/// Total sample
local controls "age_mar_wife age_mar_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth i.couple_educ_gp i.interval i.num_children age_mar_head_sq age_mar_wife_sq"

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 `controls' if inlist(IN_UNIT,0,1,2) & cohort==3, or
margins, dydx(hh_earn_type)
logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 [pweight=weight], or
margins, dydx(hh_earn_type)
// here, nothing is significant, so hard to say if changes meaningful, so I guess the weights don't change in such a crazy way, like change direction of coefficient for female BW, but it's not sig anyway.

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 `controls' if inlist(IN_UNIT,0,1,2), or // these are virtually identical
margins, dydx(hh_earn_type)
logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 `controls' if inlist(IN_UNIT,0,1,2) [pweight=weight], or // these are virtally identical
margins, dydx(hh_earn_type)

// Another potentially informative exercise could be to create a crosstab of mean weight by education by division of labor variable
tabstat weight if inlist(IN_UNIT,0,1,2) & cohort==3, by(hh_earn_type)
tabstat weight if couple_educ_gp==0 & inlist(IN_UNIT,0,1,2) & cohort==3, by(hh_earn_type)
tabstat weight if couple_educ_gp==1 & inlist(IN_UNIT,0,1,2) & cohort==3, by(hh_earn_type)
tabstat weight if race_head==1 & inlist(IN_UNIT,0,1,2) & cohort==3, by(hh_earn_type)
tabstat weight if race_head==2 & inlist(IN_UNIT,0,1,2) & cohort==3, by(hh_earn_type)

tabstat AGE_REF_ if inlist(IN_UNIT,0,1,2) & cohort==3
tabstat AGE_REF_ if inlist(IN_UNIT,0,1,2) & cohort==3 [aweight=weight]


***************************** attempting to figure out at least "final model"
// thinking to add up to 2014 couples if I am going to redo the analysis
// also use the most flexible earnings specification instead of spline? (think a reviewer said that)
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth i.interval i.num_children age_mar_head_sq age_mar_wife_sq i.earnings_bucket"

logit dissolve_lag i.dur i.hh_earn_type `controls' i.couple_educ_gp if inlist(IN_UNIT,0,1,2) & cohort_v2==1, or // no weights
margins, dydx(hh_earn_type)

svy: logit dissolve_lag i.dur i.hh_earn_type `controls' i.couple_educ_gp if inlist(IN_UNIT,0,1,2) & cohort_v2==1, or // weights
margins, dydx(hh_earn_type)

logit dissolve_lag i.dur i.hh_earn_type `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or // no weights
margins, dydx(hh_earn_type)

svy: logit dissolve_lag i.dur i.hh_earn_type `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or // weights
margins, dydx(hh_earn_type)

logit dissolve_lag i.dur i.hh_earn_type `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1, or // no weights
margins, dydx(hh_earn_type)

svy: logit dissolve_lag i.dur i.hh_earn_type `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1, or // weights
margins, dydx(hh_earn_type)

local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth i.interval i.num_children age_mar_head_sq age_mar_wife_sq knot1 knot2 knot3"
logit dissolve_lag i.dur i.hh_earn_type `controls' i.couple_educ_gp if inlist(IN_UNIT,0,1,2) & cohort_v2==1, or // no weights
margins, dydx(hh_earn_type)

svy: logit dissolve_lag i.dur i.hh_earn_type `controls' i.couple_educ_gp if inlist(IN_UNIT,0,1,2) & cohort_v2==1, or // weights
margins, dydx(hh_earn_type) // so how I control for earnings also makes a difference?

********************************************************************************
* Earnings specifications
********************************************************************************
/// Division of Labor \\\
** Overall
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth i.interval i.num_children"

// continuous
logit dissolve_lag i.dur i.hh_earn_type `controls' earnings_1000s i.couple_educ_gp if inlist(IN_UNIT,0,1,2) & cohort_v2==1, or // no weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings.xls", sideway stats(coef se pval) ctitle(1a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

svy: logit dissolve_lag i.dur i.hh_earn_type `controls' earnings_1000s i.couple_educ_gp if inlist(IN_UNIT,0,1,2) & cohort_v2==1, or // weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings.xls", sideway stats(coef se pval) ctitle(1b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// discrete
logit dissolve_lag i.dur i.hh_earn_type `controls' i.earnings_bucket i.couple_educ_gp if inlist(IN_UNIT,0,1,2) & cohort_v2==1, or // no weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings.xls", sideway stats(coef se pval) ctitle(2a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

svy: logit dissolve_lag i.dur i.hh_earn_type `controls' i.earnings_bucket i.couple_educ_gp if inlist(IN_UNIT,0,1,2) & cohort_v2==1, or // weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings.xls", sideway stats(coef se pval) ctitle(2b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// logged
logit dissolve_lag i.dur i.hh_earn_type `controls' earnings_ln i.couple_educ_gp if inlist(IN_UNIT,0,1,2) & cohort_v2==1, or // no weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings.xls", sideway stats(coef se pval) ctitle(3a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

svy: logit dissolve_lag i.dur i.hh_earn_type `controls' earnings_ln i.couple_educ_gp if inlist(IN_UNIT,0,1,2) & cohort_v2==1, or // weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings.xls", sideway stats(coef se pval) ctitle(3b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// squared
logit dissolve_lag i.dur i.hh_earn_type `controls' earnings_1000s earnings_sq i.couple_educ_gp if inlist(IN_UNIT,0,1,2) & cohort_v2==1, or // no weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings.xls", sideway stats(coef se pval) ctitle(4a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

svy: logit dissolve_lag i.dur i.hh_earn_type `controls' earnings_1000s earnings_sq i.couple_educ_gp if inlist(IN_UNIT,0,1,2) & cohort_v2==1, or // weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings.xls", sideway stats(coef se pval) ctitle(4b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// spline
logit dissolve_lag i.dur i.hh_earn_type `controls' knot1 knot2 knot3 i.couple_educ_gp if inlist(IN_UNIT,0,1,2) & cohort_v2==1, or // no weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings.xls", sideway stats(coef se pval) ctitle(5a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

svy: logit dissolve_lag i.dur i.hh_earn_type `controls' knot1 knot2 knot3 i.couple_educ_gp if inlist(IN_UNIT,0,1,2) & cohort_v2==1, or // weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings.xls", sideway stats(coef se pval) ctitle(5b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

** No College
 local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth i.interval i.num_children"

// continuous
logit dissolve_lag i.dur i.hh_earn_type `controls' earnings_1000s if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or // no weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings.xls", sideway stats(coef) ctitle(No1a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

svy: logit dissolve_lag i.dur i.hh_earn_type `controls' earnings_1000s if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or // weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings.xls", sideway stats(coef) ctitle(1b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// discrete
logit dissolve_lag i.dur i.hh_earn_type `controls' i.earnings_bucket if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or // no weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings.xls", sideway stats(coef) ctitle(2a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

svy: logit dissolve_lag i.dur i.hh_earn_type `controls' i.earnings_bucket if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or // weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings.xls", sideway stats(coef) ctitle(2b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// logged
logit dissolve_lag i.dur i.hh_earn_type `controls' earnings_ln if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0 , or // no weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings.xls", sideway stats(coef) ctitle(3a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

svy: logit dissolve_lag i.dur i.hh_earn_type `controls' earnings_ln if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or // weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings.xls", sideway stats(coef) ctitle(3b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// squared
logit dissolve_lag i.dur i.hh_earn_type `controls' earnings_1000s earnings_sq if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or // no weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings.xls", sideway stats(coef) ctitle(4a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

svy: logit dissolve_lag i.dur i.hh_earn_type `controls' earnings_1000s earnings_sq if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or // weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings.xls", sideway stats(coef) ctitle(4b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// spline
logit dissolve_lag i.dur i.hh_earn_type `controls' knot1 knot2 knot3 if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or // no weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings.xls", sideway stats(coef) ctitle(5a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

svy: logit dissolve_lag i.dur i.hh_earn_type `controls' knot1 knot2 knot3 if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or // weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings.xls", sideway stats(coef) ctitle(5b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

**College
 local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth i.interval i.num_children"

// continuous
logit dissolve_lag i.dur i.hh_earn_type `controls' earnings_1000s if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1, or // no weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings.xls", sideway stats(coef) ctitle(Coll1a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

svy: logit dissolve_lag i.dur i.hh_earn_type `controls' earnings_1000s if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1, or // weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings.xls", sideway stats(coef) ctitle(1b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// discrete
logit dissolve_lag i.dur i.hh_earn_type `controls' i.earnings_bucket if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1, or // no weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings.xls", sideway stats(coef) ctitle(2a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

svy: logit dissolve_lag i.dur i.hh_earn_type `controls' i.earnings_bucket if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1, or // weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings.xls", sideway stats(coef) ctitle(2b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// logged
logit dissolve_lag i.dur i.hh_earn_type `controls' earnings_ln if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1 , or // no weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings.xls", sideway stats(coef) ctitle(3a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

svy: logit dissolve_lag i.dur i.hh_earn_type `controls' earnings_ln if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1, or // weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings.xls", sideway stats(coef) ctitle(3b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// squared
logit dissolve_lag i.dur i.hh_earn_type `controls' earnings_1000s earnings_sq if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1, or // no weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings.xls", sideway stats(coef) ctitle(4a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

svy: logit dissolve_lag i.dur i.hh_earn_type `controls' earnings_1000s earnings_sq if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1, or // weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings.xls", sideway stats(coef) ctitle(4b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// spline
logit dissolve_lag i.dur i.hh_earn_type `controls' knot1 knot2 knot3 if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1, or // no weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings.xls", sideway stats(coef) ctitle(5a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

svy: logit dissolve_lag i.dur i.hh_earn_type `controls' knot1 knot2 knot3 if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1, or // weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings.xls", sideway stats(coef) ctitle(5b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

/// Employment \\\
* Overall
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth i.interval i.num_children"

// continuous
logit dissolve_lag i.dur i.ft_head i.ft_wife `controls' earnings_1000s i.couple_educ_gp if inlist(IN_UNIT,0,1,2) & cohort_v2==1, or // no weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings_v2.xls", sideway stats(coef se pval) ctitle(1a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) replace

svy: logit dissolve_lag i.dur i.ft_head i.ft_wife `controls' earnings_1000s i.couple_educ_gp if inlist(IN_UNIT,0,1,2) & cohort_v2==1, or // weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings_v2.xls", sideway stats(coef se pval) ctitle(1b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// discrete
logit dissolve_lag i.dur i.ft_head i.ft_wife `controls' i.earnings_bucket i.couple_educ_gp if inlist(IN_UNIT,0,1,2) & cohort_v2==1, or // no weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings_v2.xls", sideway stats(coef se pval) ctitle(2a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

svy: logit dissolve_lag i.dur i.ft_head i.ft_wife `controls' i.earnings_bucket i.couple_educ_gp if inlist(IN_UNIT,0,1,2) & cohort_v2==1, or // weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings_v2.xls", sideway stats(coef se pval) ctitle(2b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// logged
logit dissolve_lag i.dur i.ft_head i.ft_wife `controls' earnings_ln i.couple_educ_gp if inlist(IN_UNIT,0,1,2) & cohort_v2==1, or // no weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings_v2.xls", sideway stats(coef se pval) ctitle(3a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

svy: logit dissolve_lag i.dur i.ft_head i.ft_wife `controls' earnings_ln i.couple_educ_gp if inlist(IN_UNIT,0,1,2) & cohort_v2==1, or // weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings_v2.xls", sideway stats(coef se pval) ctitle(3b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// squared
logit dissolve_lag i.dur i.ft_head i.ft_wife `controls' earnings_1000s earnings_sq i.couple_educ_gp if inlist(IN_UNIT,0,1,2) & cohort_v2==1, or // no weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings_v2.xls", sideway stats(coef se pval) ctitle(4a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

svy: logit dissolve_lag i.dur i.ft_head i.ft_wife `controls' earnings_1000s earnings_sq i.couple_educ_gp if inlist(IN_UNIT,0,1,2) & cohort_v2==1, or // weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings_v2.xls", sideway stats(coef se pval) ctitle(4b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// spline
logit dissolve_lag i.dur i.ft_head i.ft_wife `controls' knot1 knot2 knot3 i.couple_educ_gp if inlist(IN_UNIT,0,1,2) & cohort_v2==1, or // no weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings_v2.xls", sideway stats(coef se pval) ctitle(5a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

svy: logit dissolve_lag i.dur i.ft_head i.ft_wife `controls' knot1 knot2 knot3 i.couple_educ_gp if inlist(IN_UNIT,0,1,2) & cohort_v2==1, or // weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings_v2.xls", sideway stats(coef se pval) ctitle(5b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

** No College
 local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth i.interval i.num_children"

// continuous
logit dissolve_lag i.dur i.ft_head i.ft_wife `controls' earnings_1000s if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or // no weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings_v2.xls", sideway stats(coef) ctitle(No1a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

svy: logit dissolve_lag i.dur i.ft_head i.ft_wife `controls' earnings_1000s if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or // weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings_v2.xls", sideway stats(coef) ctitle(1b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// discrete
logit dissolve_lag i.dur i.ft_head i.ft_wife `controls' i.earnings_bucket if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or // no weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings_v2.xls", sideway stats(coef) ctitle(2a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

svy: logit dissolve_lag i.dur i.ft_head i.ft_wife `controls' i.earnings_bucket if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or // weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings_v2.xls", sideway stats(coef) ctitle(2b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// logged
logit dissolve_lag i.dur i.ft_head i.ft_wife `controls' earnings_ln if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0 , or // no weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings_v2.xls", sideway stats(coef) ctitle(3a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

svy: logit dissolve_lag i.dur i.ft_head i.ft_wife `controls' earnings_ln if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or // weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings_v2.xls", sideway stats(coef) ctitle(3b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// squared
logit dissolve_lag i.dur i.ft_head i.ft_wife `controls' earnings_1000s earnings_sq if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or // no weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings_v2.xls", sideway stats(coef) ctitle(4a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

svy: logit dissolve_lag i.dur i.ft_head i.ft_wife `controls' earnings_1000s earnings_sq if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or // weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings_v2.xls", sideway stats(coef) ctitle(4b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// spline
logit dissolve_lag i.dur i.ft_head i.ft_wife `controls' knot1 knot2 knot3 if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or // no weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings_v2.xls", sideway stats(coef) ctitle(5a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

svy: logit dissolve_lag i.dur i.ft_head i.ft_wife `controls' knot1 knot2 knot3 if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or // weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings_v2.xls", sideway stats(coef) ctitle(5b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

**College
 local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth i.interval i.num_children"

// continuous
logit dissolve_lag i.dur i.ft_head i.ft_wife `controls' earnings_1000s if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1, or // no weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings_v2.xls", sideway stats(coef) ctitle(Coll1a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

svy: logit dissolve_lag i.dur i.ft_head i.ft_wife `controls' earnings_1000s if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1, or // weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings_v2.xls", sideway stats(coef) ctitle(1b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// discrete
logit dissolve_lag i.dur i.ft_head i.ft_wife `controls' i.earnings_bucket if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1, or // no weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings_v2.xls", sideway stats(coef) ctitle(2a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

svy: logit dissolve_lag i.dur i.ft_head i.ft_wife `controls' i.earnings_bucket if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1, or // weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings_v2.xls", sideway stats(coef) ctitle(2b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// logged
logit dissolve_lag i.dur i.ft_head i.ft_wife `controls' earnings_ln if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1 , or // no weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings_v2.xls", sideway stats(coef) ctitle(3a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

svy: logit dissolve_lag i.dur i.ft_head i.ft_wife `controls' earnings_ln if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1, or // weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings_v2.xls", sideway stats(coef) ctitle(3b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// squared
logit dissolve_lag i.dur i.ft_head i.ft_wife `controls' earnings_1000s earnings_sq if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1, or // no weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings_v2.xls", sideway stats(coef) ctitle(4a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

svy: logit dissolve_lag i.dur i.ft_head i.ft_wife `controls' earnings_1000s earnings_sq if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1, or // weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings_v2.xls", sideway stats(coef) ctitle(4b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

// spline
logit dissolve_lag i.dur i.ft_head i.ft_wife `controls' knot1 knot2 knot3 if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1, or // no weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings_v2.xls", sideway stats(coef) ctitle(5a) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

svy: logit dissolve_lag i.dur i.ft_head i.ft_wife `controls' knot1 knot2 knot3 if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1, or // weights
margins, dydx(*) post
outreg2 using "$results/dissolution_earnings_v2.xls", sideway stats(coef) ctitle(5b) dec(4) alpha(0.001, 0.01, 0.05, 0.10) symbol(***, **, *, +) append

********************************************************************************
* More exploration
********************************************************************************

/// Help
 local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth i.interval i.num_children"

logit dissolve_lag i.dur i.ft_head i.ft_wife earnings_1000s i.ft_head#c.earnings_1000s i.ft_wife#c.earnings_1000s `controls'  if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or // no weights
margins ft_wife, at(earnings_1000s=(0(10)100))

logit dissolve_lag i.dur i.ft_head i.ft_wife i.earnings_bucket i.ft_head#i.earnings_bucket i.ft_wife#i.earnings_bucket `controls'  if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or // no weights
margins earnings_bucket#ft_wife
margins earnings_bucket#ft_head

logit dissolve_lag i.dur i.ft_head##i.ft_wife i.earnings_bucket `controls'  if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or // no weights
margins ft_head#ft_wife
marginsplot

logit dissolve_lag i.dur i.ft_head##i.ft_wife i.earnings_bucket `controls'  if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1, or // no weights
margins ft_head#ft_wife
marginsplot

logit dissolve_lag i.dur i.ft_pt_head##i.ft_pt_wife i.earnings_bucket `controls'  if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or // no weights
margins ft_pt_head#ft_pt_wife
marginsplot

/// Alt svyset

logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0, or
margins, dydx(hh_earn_type)

svyset [pweight=weight]
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
svy: logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0, or
margins, dydx(hh_earn_type)

svyset cluster [pweight=weight], strata(stratum)
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
svy: logit dissolve_lag i.dur i.hh_earn_type knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0, or
margins, dydx(hh_earn_type)

// no controls
logit dissolve_lag i.dur i.hh_earn_type if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or
margins, dydx(hh_earn_type)

logit dissolve_lag i.dur i.hh_earn_type if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1, or
margins, dydx(hh_earn_type)

// earnings
logit dissolve_lag i.dur i.earnings_bucket##i.couple_educ_gp if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & earnings_bucket!=0, or
margins earnings_bucket#couple_educ_gp
marginsplot

logit dissolve_lag i.dur i.couple_educ_gp if inlist(IN_UNIT,0,1,2) & cohort_v2==1, or
logit dissolve_lag i.dur i.couple_educ_gp i.earnings_bucket i.ft_head i.ft_wife if inlist(IN_UNIT,0,1,2) & cohort_v2==1, or


logit dissolve_lag i.dur i.earnings_bucket if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or
margins earnings_bucket
marginsplot

logit dissolve_lag i.dur i.earnings_bucket if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1 & earnings_bucket!=0, or
margins earnings_bucket
marginsplot

logit dissolve_lag i.dur i.couple_educ_gp##c.earnings_1000s if inlist(IN_UNIT,0,1,2) & cohort_v2==1, or
margins couple_educ_gp, at(earnings_1000s=(0(10)150))
marginsplot

logit dissolve_lag i.dur earnings_1000s earnings_sq if inlist(IN_UNIT,0,1,2) & cohort_v2==1, or
margins, at(earnings_1000s=(0(10)150))

logit dissolve_lag i.dur i.couple_educ_gp##c.earnings_1000s i.couple_educ_gp##c.earnings_sq if inlist(IN_UNIT,0,1,2) & cohort_v2==1, or
margins couple_educ_gp, at(earnings_1000s=(0(10)150))
marginsplot

logit dissolve_lag i.dur i.couple_educ_gp##c.earnings_ln if inlist(IN_UNIT,0,1,2) & cohort_v2==1, or
margins couple_educ_gp, at(earnings_ln=(8(1)12))
marginsplot

logit dissolve_lag i.dur c.female_earn_pct##c.wife_housework_pct if inlist(IN_UNIT,0,1,2) & cohort_v2==1, or
margins, at(female_earn_pct=(0(.25)1) wife_housework_pct=(0(.25)1))
marginsplot

********************************************************************************
**# Possible interactions of interest
********************************************************************************
** Housework / TIME BINDS

// okay *this* is interesting re: TIME BINDS - way to visualize: https://www.stata.com/stata-news/news32-1/spotlight/
logit dissolve_lag i.dur c.female_earn_pct##c.wife_housework_pct if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or // gender deviance neutralization
margins, at(female_earn_pct=(0(.25)1) wife_housework_pct=(0(.25)1))
marginsplot

logit dissolve_lag i.dur c.female_earn_pct##c.wife_housework_pct if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1, or // specialization?
margins, at(female_earn_pct=(0(.25)1) wife_housework_pct=(0(.25)1))
marginsplot

 local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth i.interval i.num_children knot1 knot2 knot3"
// No College
logit dissolve_lag i.dur c.female_earn_pct##c.wife_housework_pct `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or // gender deviance neutralization
margins, at(female_earn_pct=(0(.25)1) wife_housework_pct=(0(.25)1))
margins, at(wife_housework_pct=(0(.25)1) female_earn_pct=(0(.25)1))
marginsplot
margins, at(female_earn_pct=(0(.1)1) wife_housework_pct=(0(.1)1)) saving(nocollege_unpaid, replace)
use nocollege_unpaid, clear
twoway contour _margin _at2 _at3, scolor(green) ecolor(red) levels(10)
twoway contour _margin _at3 _at2, scolor(green) ecolor(red) levels(10)

logit dissolve_lag i.dur i.hh_earn_type##c.wife_housework_pct `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0 & hh_earn_type < 4, or
margins hh_earn_type, at(wife_housework_pct=(0(.25)1))
marginsplot

 logit dissolve_lag i.dur c.female_earn_pct##c.total_weekly_hrs `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or
margins, at(female_earn_pct=(0(.25)1) total_weekly_hrs=(0(20)80))
marginsplot

logit dissolve_lag i.dur c.female_earn_pct##c.weekly_hrs_head `controls'  if inlist(IN_UNIT,0,1,2) & cohort_v2==0 & couple_educ_gp==0, or 
margins, at(female_earn_pct=(0(.25)1) weekly_hrs_head=(0(10)80))
marginsplot

// College
 local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth i.interval i.num_children knot1 knot2 knot3"
 
logit dissolve_lag i.dur c.female_earn_pct##c.wife_housework_pct `controls'  if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1, or // specialization?
margins, at(female_earn_pct=(0(.1)1) wife_housework_pct=(0(.1)1))
marginsplot
margins, at(female_earn_pct=(0(.1)1) wife_housework_pct=(0(.1)1)) saving(college_unpaid, replace)
use college_unpaid, clear
twoway contour _margin _at2 _at3, scolor(green) ecolor(red) levels(10)

logit dissolve_lag i.dur i.hh_earn_type##c.wife_housework_pct `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1 & hh_earn_type < 4, or
margins hh_earn_type, at(wife_housework_pct=(0(.25)1))
marginsplot

logit dissolve_lag i.dur c.female_earn_pct##c.total_weekly_hrs `controls'  if inlist(IN_UNIT,0,1,2) & cohort_v2==0 & couple_educ_gp==1, or 
margins, at(female_earn_pct=(0(.25)1) total_weekly_hrs=(0(20)80))
marginsplot

logit dissolve_lag i.dur c.female_earn_pct##c.weekly_hrs_head `controls'  if inlist(IN_UNIT,0,1,2) & cohort_v2==0 & couple_educ_gp==1, or 
margins, at(female_earn_pct=(0(.25)1) weekly_hrs_head=(0(10)80))
marginsplot

** Earnings / Cost variables

logit dissolve_lag i.dur i.hh_earn_type##c.earnings_1000s i.couple_educ_gp if inlist(IN_UNIT,0,1,2) & cohort_v2==1, or
margins hh_earn_type, at(earnings_1000s=(0(10)150))
marginsplot

logit dissolve_lag i.dur i.hh_earn_type##c.earnings_1000s i.couple_educ_gp if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & hh_earn_type!=4, or
margins hh_earn_type, at(earnings_1000s=(0(10)150)) // this is interesting, but not significant I don't think - but does support the mutual dependence sort of model?
marginsplot

// No College

 local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth i.interval i.num_children i.HOUSE_STATUS_"
 
logit dissolve_lag i.dur i.hh_earn_type##c.earnings_1000s `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & hh_earn_type!=4 & couple_educ_gp==0, or
margins hh_earn_type, at(earnings_1000s=(0(10)150))
marginsplot

logit dissolve_lag i.dur c.female_earn_pct##c.earnings_1000s `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or
margins, at(earnings_1000s=(0(50)150) female_earn_pct=(0(.25)1))
marginsplot

logit dissolve_lag i.dur i.hh_earn_type##c.CHILDCARE_COSTS_ `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & hh_earn_type!=4 & couple_educ_gp==0, or
margins hh_earn_type, at(CHILDCARE_COSTS_=(0(1000)10000)) 
marginsplot

logit dissolve_lag i.dur i.hh_earn_type##c.CHILDCARE_COSTS_ `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & hh_earn_type!=4 & couple_educ_gp==0 & CHILDCARE_COSTS!=0, or
margins hh_earn_type, at(CHILDCARE_COSTS_=(100(1000)10000)) 
marginsplot

logit dissolve_lag i.dur c.female_earn_pct##c.CHILDCARE_COSTS_ `controls'  if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or
margins, at(CHILDCARE_COSTS_=(0(5000)10000) female_earn_pct=(0(.25)1))
marginsplot

logit dissolve_lag i.dur i.hh_earn_type##c.housing_costs_use `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & hh_earn_type!=4 & couple_educ_gp==0, or
margins hh_earn_type, at(housing_costs_use=(-.2(.1).2)) 
marginsplot

logit dissolve_lag i.dur i.hh_earn_type##c.total_annual_rent `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & hh_earn_type!=4 & couple_educ_gp==0 & HOUSE_STATUS_==5, or
margins hh_earn_type, at(total_annual_rent=(500(500)10000)) 
marginsplot

logit dissolve_lag i.dur i.hh_earn_type##c.HOUSE_VALUE_ `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & hh_earn_type!=4 & couple_educ_gp==0 & HOUSE_STATUS_==1, or
margins hh_earn_type, at(HOUSE_VALUE_=(10000(5000)40000)) 
marginsplot

logit dissolve_lag i.dur i.hh_earn_type##c.MORTGAGE_COST_  `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & hh_earn_type!=4 & couple_educ_gp==0 & HOUSE_STATUS_==1, or
margins hh_earn_type, at(MORTGAGE_COST_ =(1000(1000)15000)) 
marginsplot

logit dissolve_lag i.dur c.female_earn_pct##c.MORTGAGE_COST_ `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0 & HOUSE_STATUS_==1, or
margins, at(MORTGAGE_COST_=(1000(5000)16000) female_earn_pct=(0(.25)1))
marginsplot

// College
 local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth i.interval i.num_children i.HOUSE_STATUS_"
 
logit dissolve_lag i.dur i.hh_earn_type##c.earnings_1000s `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & hh_earn_type!=4 & couple_educ_gp==1, or
margins hh_earn_type, at(earnings_1000s=(0(10)150))
marginsplot

logit dissolve_lag i.dur c.female_earn_pct##c.earnings_1000s `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1, or
margins, at(earnings_1000s=(0(50)150) female_earn_pct=(0(.25)1))
marginsplot

logit dissolve_lag i.dur i.hh_earn_type##c.CHILDCARE_COSTS_ `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & hh_earn_type!=4 & couple_educ_gp==1, or
margins hh_earn_type, at(CHILDCARE_COSTS_=(0(1000)10000)) 
marginsplot

logit dissolve_lag i.dur i.hh_earn_type##c.housing_costs_use `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & hh_earn_type!=4 & couple_educ_gp==1, or
margins hh_earn_type, at(housing_costs_use=(-.2(.1).2)) 
marginsplot

logit dissolve_lag i.dur i.hh_earn_type##c.total_annual_rent `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & hh_earn_type!=4 & couple_educ_gp==1 & HOUSE_STATUS_==5, or
margins hh_earn_type, at(total_annual_rent=(500(500)10000)) 
marginsplot

logit dissolve_lag i.dur i.hh_earn_type##c.HOUSE_VALUE_ `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & hh_earn_type!=4 & couple_educ_gp==1 & HOUSE_STATUS_==1, or
margins hh_earn_type, at(HOUSE_VALUE_=(10000(5000)40000)) 
marginsplot

logit dissolve_lag i.dur i.hh_earn_type##c.MORTGAGE_COST_  `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & hh_earn_type!=4 & couple_educ_gp==1 & HOUSE_STATUS_==1, or
margins hh_earn_type, at(MORTGAGE_COST_ =(1000(1000)15000)) 
marginsplot

logit dissolve_lag i.dur c.female_earn_pct##c.MORTGAGE_COST_ `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1 & HOUSE_STATUS_==1, or
margins, at(MORTGAGE_COST_=(1000(5000)16000) female_earn_pct=(0(.25)1))
marginsplot

/// Splines instead? or alternate functional forms. I think Schwartz and GP might have used continous bucket? i am not sure
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth i.interval i.num_children age_mar_head_sq age_mar_wife_sq knot1 knot2 knot3"

logit dissolve_lag i.dur i.hh_earn_type `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or
margins, dydx(hh_earn_type)

logit dissolve_lag i.dur earn_pct1 earn_pct2 earn_pct3 `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or

logit dissolve_lag i.dur female_earn_pct `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or
margins, at(female_earn_pct=(0(.25)1))

logit dissolve_lag i.dur female_earn_pct earn_pct_sq `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or
margins, at(female_earn_pct=(0(.1)1))

logit dissolve_lag i.dur c.female_earn_pct##c.female_earn_pct `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or
margins, at(female_earn_pct=(0(.1)1))

logit dissolve_lag i.dur i.hh_earn_type `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1, or
margins, dydx(hh_earn_type)

logit dissolve_lag i.dur earn_pct1 earn_pct2 earn_pct3 `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1, or

logit dissolve_lag i.dur female_earn_pct `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1, or
margins, at(female_earn_pct=(0(.25)1))

logit dissolve_lag i.dur female_earn_pct earn_pct_sq `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1, or
margins, at(female_earn_pct=(0(.25)1))

logit dissolve_lag i.dur c.female_earn_pct##c.female_earn_pct `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1, or
margins, at(female_earn_pct=(0(.1)1))

logit dissolve_lag i.dur i.couple_educ_gp##c.female_earn_pct##c.female_earn_pct `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1, or
margins couple_educ_gp, at(female_earn_pct=(0(.1)1))

/// what is TEMPORAL turning point
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth i.interval i.num_children age_mar_head_sq age_mar_wife_sq knot1 knot2 knot3"

logit dissolve_lag i.dur c.female_earn_pct##i.cohort_alt `controls' if inlist(IN_UNIT,0,1,2) & cohort_alt<5, or
margins cohort_alt, at(female_earn_pct=(0(.25)1))
marginsplot

logit dissolve_lag i.dur c.female_earn_pct##i.cohort_alt `controls' if inlist(IN_UNIT,0,1,2) & couple_educ_gp==0, or
margins cohort_alt, at(female_earn_pct=(0(.25)1))
marginsplot

logit dissolve_lag i.dur c.female_earn_pct##i.cohort_alt `controls' if inlist(IN_UNIT,0,1,2) & couple_educ_gp==1, or
margins cohort_alt, at(female_earn_pct=(0(.25)1))
marginsplot

********************************************************************************
**# For memo for Kelly?
********************************************************************************

/// do my findings match Schwartz and GP? okay yes.
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth i.interval i.num_children age_mar_head_sq age_mar_wife_sq knot1 knot2 knot3"
logit dissolve_lag i.dur c.female_earn_pct `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or // no association, so this matches
margins, at(female_earn_pct=(0(.1)1))
marginsplot

logit dissolve_lag i.dur c.female_earn_pct##i.cohort_v2 `controls' if inlist(IN_UNIT,0,1,2) & couple_educ_gp==0, or // cohort change is sig.
margins cohort_v2, at(female_earn_pct=(0(.1)1))
marginsplot

logit dissolve_lag i.dur c.female_earn_pct `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1, or // positive, but not significant, which also matches
margins, at(female_earn_pct=(0(.1)1))
marginsplot

logit dissolve_lag i.dur c.female_earn_pct##i.cohort_v2 `controls' if inlist(IN_UNIT,0,1,2) & couple_educ_gp==1, or // pos in cohort 1 and no sig cohort change - so this matches
margins cohort_v2, at(female_earn_pct=(0(.1)1))
marginsplot


// Unpaid labor interactions
 local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth i.interval i.num_children knot1 knot2 knot3"
 
logit dissolve_lag i.dur c.female_earn_pct##c.wife_housework_pct `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or
margins, at(female_earn_pct=(0(.25)1) wife_housework_pct=(0(.25)1))
margins, at(wife_housework_pct=(0(.25)1) female_earn_pct=(0(.25)1))
marginsplot
margins, at(female_earn_pct=(0(.1)1) wife_housework_pct=(0(.1)1)) saving(nocollege_unpaid, replace)
// use nocollege_unpaid, clear
// twoway contour _margin _at2 _at3, scolor(green) ecolor(red) levels(10)
// twoway contour _margin _at3 _at2, scolor(green) ecolor(red) levels(10)
 
logit dissolve_lag i.dur c.female_earn_pct##c.wife_housework_pct `controls'  if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1, or
margins, at(female_earn_pct=(0(.1)1) wife_housework_pct=(0(.1)1))
marginsplot
margins, at(female_earn_pct=(0(.1)1) wife_housework_pct=(0(.1)1)) saving(college_unpaid, replace)
// use college_unpaid, clear
// twoway contour _margin _at2 _at3, scolor(green) ecolor(red) levels(10)

// Interactions with total earnings
local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth i.interval i.num_children" // have to remove earnings from controls if I do this?

logit dissolve_lag i.dur c.female_earn_pct##c.earnings_1000s `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or
margins, at(earnings_1000s=(0(10)150) female_earn_pct=(0(.1)1)) saving(nocollege_earn, replace)
marginsplot
// use nocollege_earn, clear
// twoway contour _margin _at2 _at3, scolor(green) ecolor(red) levels(10)

logit dissolve_lag i.dur c.female_earn_pct##c.earnings_1000s `controls' if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1, or
margins, at(earnings_1000s=(0(10)150) female_earn_pct=(0(.1)1)) saving(college_earn, replace)
marginsplot
// use college_earn, clear
// twoway contour _margin _at2 _at3, scolor(green) ecolor(red) levels(10)


// Interactions with childcare costs
 local controls "age_mar_wife age_mar_wife_sq age_mar_head age_mar_head_sq i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth i.interval i.num_children knot1 knot2 knot3"
logit dissolve_lag i.dur c.female_earn_pct##c.CHILDCARE_COSTS_ `controls'  if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==0, or
margins, at(CHILDCARE_COSTS_=(0(500)10000) female_earn_pct=(0(.1)1)) saving(nocollege_cc, replace)
marginsplot
// use nocollege_cc, clear
// twoway contour _margin _at2 _at3, scolor(green) ecolor(red) levels(10)

logit dissolve_lag i.dur c.female_earn_pct##c.CHILDCARE_COSTS_ `controls'  if inlist(IN_UNIT,0,1,2) & cohort_v2==1 & couple_educ_gp==1, or
margins, at(CHILDCARE_COSTS_=(0(500)10000) female_earn_pct=(0(.1)1)) saving(college_cc, replace)
marginsplot
// use college_cc, clear
// twoway contour _margin _at2 _at3, scolor(green) ecolor(red) levels(10)

