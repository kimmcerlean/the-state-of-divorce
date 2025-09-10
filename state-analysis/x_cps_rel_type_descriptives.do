********************************************************************************
* Getting CPS data to compare cohabitors in PSID (not representative) to cohabitors
* in CPS (should be representative)
* cps_rel_type_descriptives.do
* Kim McErlean
********************************************************************************

use "$CPS\cps_1990_2019_reltype.dta", clear

********************************************************************************
* First figure out who is partnered and what info is collected about partners /
* When we can identify cohab and cohab info
********************************************************************************

// so I can match spousal characteristics in IPUMs, which i did. I don't *think* this applies to cohab, so let's figure this out
// okay more detailed notes below - but it actually DOES apply if you are a partner of the HH head (but not if not partnered with HH head and not captured in years before partnered split out from roommmate)
browse year serial pernum lineno marst relate famkind pecohab sploc educ educ_sp

tabstat pecohab, by(year) // also check when this added (feels like not there always?) - okay yeah, not until 2007. I think before this you could identify cohab JUST to HH head (which actually is possibly more similar to PSID anyway?) but then after that they added this so you could identify all cohab in a HH (this is also maybe good to cmpare because is another limitation of PSID wrt cohab)
// so do (1) married, (2) cohab - all, (3) cohab - with HH head, (4) cohab - not with HH head?
// also note - I am restricting to similar time frame of PSID, BUT i cannot restrict (that I am aware) on relationship START YEAR - because I don't have that information. so people observed in 1990 could have started their relationship decades ago. age at first marriage went away decades ago in terms of being measured in the CPS

tab relate, m
gen relate_gp = .
replace relate_gp = 1 if relate==101 // hh head
replace relate_gp = 2 if inrange(relate,201,203) // spouse (generic, opposite,same) -  not split out until 2019
replace relate_gp = 3 if inlist(relate,1114,1116,1117) // def unmarried partner (only split into own category starting 1995, then by gender in 2019)
replace relate_gp = 4 if relate== 1113 // until 1994, this was partner / roommate (why is that funny to me...)
replace relate_gp = 5 if inrange(relate,300,1001) | relate==1115 | inrange(relate,1200,1300) // other

label define relate_gp 1 "HH head" 2 "spouse" 3 "partner" 4 "Prob partner" 5 "other"
label values relate_gp relate_gp

tab relate relate_gp
tab year relate_gp // okay so yes, could be listed as cohab
tab relate if pecohab!=0 & pecohab!=. // okay this is confusing because can also be listed as housemate / roommate or other non-relative... (i guess if not HH head?) but sometimes it feels like they are the head's partner (based on age / sex) and still listed as roommate?

browse year serial pernum lineno marst relate_gp sex age ncouples famkind pecohab aspouse educ educ_sp // okay but this is line number, not person number? because often times they are listed as like 2 and 2 as a pernum feels obviously like the child? this is confusing...BUT the real thing is - it does actually seem like the sp info contains cohab? (only once pecohab is added?) ofc I didn't add that OKAY adding now...
tab sex_sp if pecohab!=0 & pecohab!=., m // so actually like 80% filled in? I am just worried there is something wrong?
tab year sex_sp if pecohab!=0 & pecohab!=., m // okay but the missing are not related to survey year, there are always missing
tab relate if sex_sp ==. & pecohab!=0 & pecohab!=., m  // okay, so it seems as though it is NOT filled in when they are NOT cohabiting with HH head?
tab relate sex_sp if pecohab!=0 & pecohab!=., m // yes - so always missing if listed as roommate / other non-relative. never missing if listed as unmarried partner to HH head. when listed as HH head - mostly filled in, but small % missing
tab relate_gp if pecohab!=0 & pecohab!=., m // so about 10% of cohabitors are NOT HH head or their partner - so if I want this info, need to fill in manually. and I think I *do* want this info. also - do I need to attempt to fill in manully prior to 2007 as well? yes probably... okay, so after 2007, I should match on line number. what do I match on before that? or fill in line no manually based on relationship?
tab sex_sp if relate_gp==3 & pecohab==., m // okay, wait so this was already being matched actually - even before pecohab came along? so, we actually always have this info if cohabiting with HH head. the info we are missing is if NOT cohabiting with HH head
tab sex_sp if relate_gp==4 & pecohab==., m // okay DON'T have this info if ONLY probably partner

browse year serial pernum marst relate_gp sex age ncouples famkind pecohab sploc educ educ_sp uhrsworkt uhrsworkt_sp if pecohab!=0 & pecohab!=.

// okay, I want to try to make this file smaller. so, let's create an indicator, first, if they are partnered, then second, what type, then I will drop all non-partnered
// so for HH head, prior to 2007, need to identify based on whether there is a cohabitor in the HH (bc marst is legal)
tab marst if relate_gp==3
tab sploc marst, m // there are a lot of non-zeroes for other marital statuses, are these coahab? I am so confused...
tab pecohab marst, m
tab marst sex_sp , m
tab marst sex_sp if year < 1995 , m // so only after 1995 did they fill in the spouse info for cohabiting partners

gen partner = 0
replace partner = 1 if relate_gp==3 // there shouldn't be able to be more than 1 in HH right?

bysort year serial: egen partner_in_hh = max(partner)
sort year serial pernum
browse year serial pernum marst relate_gp partner_in_hh educ educ_sp
tab relate if partner_in_hh==1
fre relate if partner_in_hh==1

gen partnered=0
replace partnered=1 if marst == 1 // because won't have spouse info if married, spouse absent
replace partnered=1 if partner_in_hh==1 & inlist(relate_gp,1,3) // also need to restrict to them being HH head or partner because if they have children, they will be labeled as partnered DUH
replace partnered=1 if pecohab!=. & pecohab!=0
replace partnered=1 if sploc!=. & sploc!=0 // should be captured above, but jic

tab relate_gp partnered, m
tab pecohab partnered, m
tab marst partnered, m

keep if partnered==1 // just want to make this file smaller
drop if year < 1995 // not going to use the non-clear partner info

gen rel_type = .
replace rel_type = 1 if marst==1
replace rel_type = 2 if rel_type==. & partner_in_hh==1
replace rel_type = 2 if rel_type==. & pecohab!=. & pecohab!=0
tab rel_type, m

label define rel_type 1 "married" 2 "cohab"
label values rel_type rel_type
tab year rel_type, m row

tab rel_type sex_sp, m
tab relate_gp if sex_sp==.

********************************************************************************
* Now let's match info for the cohabitors not with HH head
********************************************************************************
tab year relate_gp, m 
tab sploc rel_type, m // I think sploc is filled in with cohab partner if with HH head
tab sploc rel_type if relate_gp!=5

browse year serial pernum lineno marst relate_gp rel_type partner_in_hh sex age ncouples pecohab sploc educ educ_sp uhrsworkt uhrsworkt_sp

preserve

keep if sex_sp==.
keep if rel_type==2

local spouse_vars "age sex race hispan empstat labforce uhrsworkt ahrsworkt wkstat educ wkswork1 uhrsworkly inctot incwage" // these are the vars to match (and append _sp to)
keep lineno year serial `spouse_vars'
rename lineno pecohab
foreach var in `spouse_vars'{
	rename `var' `var'_sp
}
drop if pecohab==.

save "$temp/cps_cohab_match.dta", replace

restore

merge m:1 serial year pecohab using "$temp/cps_cohab_match.dta", update
drop if _merge==2

tab rel_type _merge // yes, all cohab
tab relate _merge // and no spouses / partners, all "others" and then HH head
drop _merge

tab sex_sp, m // okay, yes got it to very few missings now

tab relate if rel_type==2 & partner_in_hh==1
tab relate if rel_type==2 & partner_in_hh==0

browse year serial pernum lineno marst relate_gp rel_type partner_in_hh relate sploc pecohab if rel_type==2 

gen rel_type_detailed = .
replace rel_type_detailed = 1 if rel_type==1
replace rel_type_detailed = 2 if rel_type==2 & partner_in_hh==1
replace rel_type_detailed = 3 if rel_type==2 & partner_in_hh==0

label define detailed 1 "married" 2 "cohab - hh head" 3 "cohab - non hh head"
label values rel_type_detailed detailed

tab year rel_type_detailed, m // only after 2007 includes non HH heads
tab relate rel_type_detailed, m // okay these make sense

********************************************************************************
* A few variables to create / recode to be similar to PSID
********************************************************************************
// first clean up the data and deduplicate
drop if sex==sex_sp // need different gender couples
drop if sex_sp==.

keep if sex==2 // let's just keep women so a. we have deduplicated and b. then we don't need to create gendered versions. main = women, sp = men

// education
recode educ (1/71=1)(73=2)(81/92=3)(111/125=4), gen(education)
recode educ_sp (1/71=1)(73=2)(81/92=3)(111/125=4), gen(education_sp)

label define education 1 "LTHS" 2 "HS" 3 "Some College" 4 "College"
label values education education_sp education

gen college=0
replace college=1 if education==4
gen college_sp=0
replace college_sp=1 if education_sp==4

tab college college_sp

gen educ_type=.
replace educ_type = 1 if college == 0 & college_sp == 0
replace educ_type = 2 if college == 0 & college_sp == 1
replace educ_type = 3 if college == 1 & college_sp == 0
replace educ_type = 4 if college == 1 & college_sp == 1

label define educ_type 1 "Neither College" 2 "Him College" 3 "Her College" 4 "Both College"
label values educ_type educ_type

// race
gen race_gp = .
replace race_gp = 1 if race == 100
replace race_gp = 2 if race == 200
replace race_gp = 3 if race == 300
replace race_gp = 4 if inrange(race,650,652)
replace race_gp = 5 if race == 700
replace race_gp = 6 if inrange(race,801,830)

gen race_gp_sp = .
replace race_gp_sp = 1 if race_sp == 100
replace race_gp_sp = 2 if race_sp == 200
replace race_gp_sp = 3 if race_sp == 300
replace race_gp_sp = 4 if inrange(race_sp,650,652)
replace race_gp_sp = 5 if race_sp == 700
replace race_gp_sp = 6 if inrange(race_sp,801,830)

label define race_gp 1 "White" 2 "Black" 3 "American Indian" 4 "Asian / Pac Is" 5 "Other Single Race" 6 "Multi-racial"
label values race_gp race_gp_sp race_gp

gen hispanic=.
replace hispanic=0 if hispan==0
replace hispanic=1 if inrange(hispan,100,612)

gen hispanic_sp=.
replace hispanic_sp=0 if hispan_sp==0
replace hispanic_sp=1 if inrange(hispan_sp,100,612)

tab race_gp hispanic, m

// this matches PSID
gen raceth = .
replace raceth = 1 if race_gp==1 & (hispanic==0 | hispanic==.) // NH White
replace raceth = 2 if race_gp==2 // Black
replace raceth = 3 if hispanic==1 & race_gp!=2  // Hispanic
replace raceth = 4 if race_gp==4 & (hispanic==0 | hispanic==.) // NH Asian
replace raceth = 5 if inlist(race_gp,3,5,6) & (hispanic==0 | hispanic==.) // NH Other (including Amer Indian, Other, and Multiracial)

gen raceth_sp = .
replace raceth_sp = 1 if race_gp_sp==1 & (hispanic_sp==0 | hispanic_sp==.) // NH White
replace raceth_sp = 2 if race_gp_sp==2 // Black
replace raceth_sp = 3 if hispanic_sp==1 & race_gp_sp!=2  // Hispanic
replace raceth_sp = 4 if race_gp_sp==4 & (hispanic_sp==0 | hispanic_sp==.) // NH Asian
replace raceth_sp = 5 if inlist(race_gp_sp,3,5,6) & (hispanic_sp==0 | hispanic_sp==.) // NH Other (including Amer Indian, Other, and Multiracial)

label define raceth 1 "NH White" 2 "Black" 3 "Hispanic" 4 "NH Asian" 5 "NH Other"
label values raceth raceth_sp raceth

gen same_raceth = 0
replace same_raceth = 1 if raceth == raceth_sp

// employment status
fre labforce
fre empstat

gen employed = 0
replace employed = 1 if inrange(empstat,10,12)

gen employed_sp = 0
replace employed_sp = 1 if inrange(empstat_sp,10,12)

tab labforce employed, m
gen employed_det = 0 if labforce==0 | labforce==1
replace employed_det = 1 if labforce==2 & employed==0
replace employed_det = 2 if labforce==2 & employed==1

gen employed_det_sp = 0 if labforce_sp==0 | labforce_sp==1
replace employed_det_sp = 1 if labforce_sp==2 & employed_sp==0
replace employed_det_sp = 2 if labforce_sp==2 & employed_sp==1

label define employed_det 0 "not in LF" 1 "unemployed" 2 "employed"
label values employed_det employed_det_sp employed_det

// work hours and relative work hours / DoL.
// Use LY to be equivalent to PSID?
	// uhrsworkt = hours usually worked, all jobs. range = 0-999, 997=hours vary, 999 = NIU (so should be 0?)
	// ahrsworkt = hours worked last week. range = 1-999, 999 = NIU
	// uhrsworkly = usual hours worked per week LY. range = 1-999, 999==NIU. 99 = top coded
sum uhrsworkt ahrsworkt uhrsworkly uhrsworkt_sp ahrsworkt_sp uhrsworkly_sp
browse year serial pernum uhrsworkt ahrsworkt uhrsworkly uhrsworkt_sp ahrsworkt_sp uhrsworkly_sp

gen usual_hours = uhrsworkt
replace usual_hours = 0 if uhrsworkt==999
replace usual_hours = 40 if uhrsworkt==997 // let's just put at 40

gen hours_last_week = ahrsworkt
replace hours_last_week = 0 if ahrsworkt==999

gen hours_last_year = uhrsworkly
replace hours_last_year = 0 if uhrsworkly==999

gen usual_hours_sp = uhrsworkt_sp
replace usual_hours_sp = 0 if uhrsworkt_sp==999
replace usual_hours_sp = 40 if uhrsworkt_sp==997 // let's just put at 40

gen hours_last_week_sp = ahrsworkt_sp
replace hours_last_week_sp = 0 if ahrsworkt_sp==999

gen hours_last_year_sp = uhrsworkly_sp
replace hours_last_year_sp = 0 if uhrsworkly_sp==999

sum usual_hours hours_last_week hours_last_year usual_hours_sp hours_last_week_sp hours_last_year_sp
sum usual_hours hours_last_week hours_last_year if employed==1 
sum usual_hours_sp hours_last_week_sp hours_last_year_sp if employed_sp==1

// now turn this into a DoL variable (can't do combined bc no housework in CPS)
egen couple_hours_t1 = rowtotal(hours_last_year hours_last_year_sp)
gen female_hours_pct_t1 = hours_last_year/couple_hours_t1

gen hh_hours_type_t1=.
replace hh_hours_type_t1=1 if female_hours_pct_t1 >=.4000 & female_hours_pct_t1 <=.6000
replace hh_hours_type_t1=2 if female_hours_pct_t1 <.4000
replace hh_hours_type_t1=3 if female_hours_pct_t1 >.6000 & female_hours_pct_t1!=.
replace hh_hours_type_t1=4 if hours_last_year==0 & hours_last_year_sp==0

label define hh_hours_type 1 "Dual Earner" 2 "Male BW" 3 "Female BW" 4 "No Earners"
label values hh_hours_type_t1 hh_hours_type

tab hh_hours_type_t1, m
tab hh_hours_type_t1 rel_type, m col

// total couple earnings
egen couple_earnings = rowtotal(incwage incwage_sp)
browse year serial pernum relate_gp hhincome ftotval faminc couple_earnings incwage incwage_sp inctot inctot_sp
tabstat couple_earnings ftotval hhincome, by(rel_type_detailed) stats(mean p50)

// home ownership
gen home_owner = 0
replace home_owner = 1 if hhtenure==1

// include age as well, maybe kids in home?
sum age age_sp nchlt5 nchild

// need to drop certain ages to be comparable to PSID
// can't really do other restrictions (like year at marriage or marriage order, bc that doesn't exist...)
keep if (age>=18 & age<=55) &  (age>=18 & age_sp<=55)

********************************************************************************
**# Let's make descriptive table now
********************************************************************************
tab hh_hours_type_t1, gen(hours_type)
tab educ_type, gen(educ_type)
tab raceth, gen(race_wife)
tab raceth_sp, gen(race_husband)

svyset [pweight=asecwt]

putexcel set "$results/CPS_relationship_comparison", replace
putexcel B1 = "Married Couples"
putexcel C1 = "All Cohabitors"
putexcel D1 = "Just HH Head Cohab"
putexcel E1 = "Just Other Cohab"

putexcel A2 = "Women's % of Total Hours"
putexcel A3 = "Dual Earning HH (Hours)"
putexcel A4 = "Male Breadwinner (Hours)"
putexcel A5 = "Female Breadwinner (Hours)"
putexcel A6 = "Neither Partner has Hours"
putexcel A7 = "Neither has college degree"
putexcel A8 = "He has college degree"
putexcel A9 = "She has college degree"
putexcel A10 = "Both have college"
putexcel A11 = "Her Employed"
putexcel A12 = "Him Employed"
putexcel A13 = "Her Weekly Work Hours (LY)"
putexcel A14 = "His Weekly Work Hours (LY)"
putexcel A15 = "Total Couple Earnings"
putexcel A16 = "Wife's Race: NH White"
putexcel A17 = "Wife's Race: Black"
putexcel A18 = "Wife's Race: Hispanic"
putexcel A19 = "Wife's Race: NH Asian"
putexcel A20 = "Wife's Race: NH Other"
putexcel A21 = "Husband's Race: NH White"
putexcel A22 = "Husband's Race: Black"
putexcel A23 = "Husband's Race: Hispanic"
putexcel A24 = "Husband's Race: NH Asian"
putexcel A25 = "Husband's Race: NH Other"
putexcel A26 = "Husband and wife same race"
putexcel A27 = "Couple owns home"
putexcel A28 = "Wife's age"
putexcel A29 = "Husband's age"
putexcel A30 = "Total # of Children"
putexcel A31 = "# of Children under 5"

local meanvars "female_hours_pct_t1 hours_type1 hours_type2 hours_type3 hours_type4 educ_type1 educ_type2 educ_type3 educ_type4 employed employed_sp hours_last_year hours_last_year_sp couple_earnings race_wife1 race_wife2 race_wife3 race_wife4 race_wife5 race_husband1 race_husband2 race_husband3 race_husband4 race_husband5 same_race home_owner age age_sp nchild nchlt5"

// Married
forvalues w=1/30{
	local row=`w'+1
	local var: word `w' of `meanvars'
	svy: mean `var' if rel_type==1
	matrix t`var'= e(b)
	putexcel B`row' = matrix(t`var'), nformat(#.#%)
}

// All Cohab
forvalues w=1/30{
	local row=`w'+1
	local var: word `w' of `meanvars' 
	svy: mean `var' if rel_type==2
	matrix t`var'= e(b)
	putexcel C`row' = matrix(t`var'), nformat(#.#%)
}


// Cohab with HH Head
forvalues w=1/30{
	local row=`w'+1
	local var: word `w' of `meanvars'
	svy: mean `var' if rel_type_detailed==2
	matrix t`var'= e(b)
	putexcel D`row' = matrix(t`var'), nformat(#.#%)
}

// Other Cohab
forvalues w=1/30{
	local row=`w'+1
	local var: word `w' of `meanvars'
	svy: mean `var' if rel_type_detailed==3
	matrix t`var'= e(b)
	putexcel E`row' = matrix(t`var'), nformat(#.#%)
}
