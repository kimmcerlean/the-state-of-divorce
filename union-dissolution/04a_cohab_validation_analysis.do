********************************************************************************
* Analysis to compare to prior studies
* cohab_validation_analysis.do
* Kim McErlean
********************************************************************************

use "$data_keep\PSID_union_validation_sample.dta", clear
// keep if relationship_type==1


********************************************************************************
* Brines and Joyner 1999
********************************************************************************
keep if rel_start_all >=1976 & rel_start_all <=1985
unique id, by(relationship_type) // slightly more of both, but also needed to be intact across two waves, how to designate that -- have to have at least two rows

bysort id: egen total_dur = max(dur)
browse id survey_yr relationship_type rel_start_all dur total_dur

drop if total_dur==0 | total_dur==1
unique id, by(relationship_type) // okay much closer count of cohab, but still higher marriages - because I also have marriages that lasted until 2019. gah this is the part that is confusing.

browse id survey_yr MARITAL_STATUS_REF_ dissolve

gen divorce_date = rel_end_all if dissolve==1
bysort id (divorce_date): replace divorce_date = divorce_date[1]
sort id survey_yr

gen in_div_sample=0
replace in_div_sample=1 if divorce_date <=1989 | divorce_date==.

unique id if in_div_sample, by(relationship_type) // cohab is okay, but marriage still much higher. is it because should just remove those rows?


preserve
drop if survey_yr > 1989
unique id, by(relationship_type) // okay actually getting closer
// restore

// jasso earnings
egen min_earn=rowmin(earnings_head earnings_wife)
egen max_earn=rowmax(earnings_head earnings_wife)

gen jasso_earn=.
replace jasso_earn= ln(min_earn/max_earn) if ft_wife==1 & ft_head==1
replace jasso_earn=ln(1/2) if jasso_earn==.

browse min_earn max_earn jasso_earn // more negative = greater distance; smaller = less distance

egen min_wage=rowmin(WAGE_RATE_HEAD_ WAGE_RATE_WIFE_)
egen max_wage=rowmax(WAGE_RATE_HEAD_ WAGE_RATE_WIFE_)

gen jasso_wage=.
replace jasso_wage= ln(min_wage/max_wage) if ft_wife==1 & ft_head==1
replace jasso_wage=ln(1/2) if jasso_wage==.

browse min_wage max_wage jasso_wage // more negative = greater distance; smaller = less distance

gen jasso_wage_alt=.
replace jasso_wage_alt= ln(min_wage/max_wage) if employed_ly_head==1 & employed_ly_wife==1
replace jasso_wage_alt=ln(1/2) if jasso_wage_alt==.

gen wife_earns_more=0
replace wife_earns_more=1 if earnings_wife>earnings_head

// qual checks against table1
tab relationship_type wife_earns_more, row // a little higher than theirs, but generally directionally same.

tab relationship_type, sum(jasso_earn)
tab relationship_type, sum(jasso_wage)

gen earn_ratio = earnings_wife / earnings_head
replace earn_ratio=10 if earn_ratio>10 & earn_ratio!=. // top code

tab relationship_type, sum(earn_ratio)
tab relationship_type if employed_ly_head==1, sum(earn_ratio) // this is less close
tab relationship_type if employed_ly_head==1 & employed_ly_wife==1, sum(earn_ratio) // this is very close

// models
logit dissolve couple_earnings if relationship_type==1 // sig neg - B&J sometimes is sig, sometimes isn't. - okay but positive asssociation gah
logit dissolve couple_earnings if relationship_type==2 // sig negative - matches B&J

logit dissolve female_earn_pct if relationship_type==1 // cohab - not sig
logit dissolve female_earn_pct if relationship_type==2 // marriage - sig positive - which makes sense

local controls "dur i.race_head i.same_race i.children i.educ_wife i.educ_head age_mar_head age_mar_wife"
logit dissolve couple_earnings `controls' if relationship_type==1 & in_div_sample==1 // not sig - B&J sometimes is sig, sometimes isn't.
logit dissolve couple_earnings `controls' if relationship_type==2  & in_div_sample==1 // sig negative - matches B&J

logit dissolve female_earn_pct `controls' if relationship_type==1 // cohab - not sig
logit dissolve female_earn_pct `controls' if relationship_type==2 // marriage - sig positive - which makes sense

local controls "dur i.race_head i.same_race i.children i.educ_wife i.educ_head age_mar_head age_mar_wife"
logit dissolve i.wife_earns_more `controls' if relationship_type==1 // not sig
logit dissolve i.wife_earns_more `controls' if relationship_type==2 // sig positive - which is not consistent, but had to be EMPLOYED MALE PARTNER

logit dissolve earn_ratio `controls' if relationship_type==1 // sig pos
logit dissolve earn_ratio `controls' if relationship_type==2 // sig positive

logit dissolve jasso_earn `controls' if relationship_type==1 // sig neg - this aligns
logit dissolve jasso_earn `controls' if relationship_type==2 // not sig - algins with B&J

logit dissolve jasso_wage `controls' if relationship_type==1 // okay not sig but neg
logit dissolve jasso_wage `controls' if relationship_type==2 // not sig but less neg

logit dissolve jasso_wage i.wife_earns_more if relationship_type==1
logit dissolve jasso_wage i.wife_earns_more if relationship_type==2
logit dissolve jasso_wage i.wife_earns_more if relationship_type==1 & in_div_sample==1
logit dissolve jasso_wage i.wife_earns_more if relationship_type==2 & in_div_sample==1

logit dissolve jasso_wage i.wife_earns_more if relationship_type==1 & employed_ly_head==1
logit dissolve jasso_wage i.wife_earns_more if relationship_type==2 & employed_ly_head==1

local controls "dur i.race_head i.same_race i.children i.educ_wife i.educ_head age_mar_head age_mar_wife"
logit dissolve jasso_wage_alt i.wife_earns_more `controls' if relationship_type==1 // nothing sig
logit dissolve jasso_wage_alt i.wife_earns_more `controls' if relationship_type==2 // wife earns more = sig positive

// okay this islike crux of table 4 and I can't get it to match - cohab, nothing sig. for marriage, wife-earns_more is sig, when I add in divorce sample, nothing sig

logit dissolve jasso_wage_alt i.wife_earns_more if relationship_type==1 // here, marginally sig neg, but wife earns more is not sig.
logit dissolve jasso_wage_alt i.wife_earns_more if relationship_type==2 // wife earns more = sig
logit dissolve jasso_wage_alt i.wife_earns_more if relationship_type==1 & in_div_sample==1 // nothing sig
logit dissolve jasso_wage_alt i.wife_earns_more if relationship_type==2 & in_div_sample==1 // nothing sig

gen jasso_wage_lag=.
replace jasso_wage_lag=jasso_wage[_n-1] if id==id[_n-1]

logit dissolve jasso_wage_lag i.wife_earns_more if relationship_type==1
logit dissolve jasso_wage_lag i.wife_earns_more if relationship_type==2 // wife earns more = sig

// how about my variables - dual earning should stabilize cohab, and male BW = married
logit dissolve i.hh_earn_type_bkd if relationship_type==1, or // no diffs male bw / male sole and dual, but female BW = sig higher - aligns with B&J overall conclusion - but I in general, am like - is this real or size.  like 300 cohabs, and 100 dissolutions...
logit dissolve i.hh_earn_type_bkd if relationship_type==2, or // male primary sig less likely to dissolve than dual, female BW = sig higher - so kinda generally aligns with speciailization, but no diff with male sole and dual earning...

logit dissolve female_earn_pct if relationship_type==1 // cohab - not sig, which makes sense, kinda
logit dissolve female_earn_pct if relationship_type==2 // marriage - sig positive - which makes sense if male BW is particularly stabilizing