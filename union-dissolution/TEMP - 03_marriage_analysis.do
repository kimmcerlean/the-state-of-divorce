
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
local controls "age_mar_wife age_mar_head i.race_head i.same_race i.either_enrolled i.region cohab_with_wife cohab_with_other pre_marital_birth"
logit dissolve_lag i.dur i.earn_type_hw knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq  `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0, or
margins, dydx(earn_type_hw)

logit dissolve_lag i.dur i.earn_type_hw knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq  `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==0 [pweight=weight], or
margins, dydx(earn_type_hw)

logit dissolve_lag i.dur i.earn_type_hw knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq  `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==1, or
margins, dydx(earn_type_hw)

logit dissolve_lag i.dur i.earn_type_hw knot1 knot2 knot3 i.interval i.num_children age_mar_head_sq age_mar_wife_sq  `controls' if inlist(IN_UNIT,0,1,2) & cohort==3 & couple_educ_gp==1 [pweight=weight], or
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

*********************************************** Race differences
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
