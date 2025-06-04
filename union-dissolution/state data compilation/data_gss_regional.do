set maxvar 10000

use "$GSS/gss7221_r1b.dta", clear
// use "$GSS\ICPSR_36797-V1\ICPSR_36797\DS0001\36797-0001-Data.dta", clear


// run supplemental syntax

rename *, lower

// keep if year > 2009 & year <2021

keep year region womenhrt chldhrt allhrt noonehrt famsuffr twoincs hubbywrk hubbywk1 /// 
twoincs1 menhrt nooneben id childs educ sex tradmod menben womenben chldben /// 
allben ballot kidsuffr fepresch mawrkwrm mapaid paidlvdv

// recode missings and not asked so can aggregate questions by year and region.
foreach var in womenhrt chldhrt allhrt noonehrt famsuffr twoincs hubbywrk hubbywk1 /// 
twoincs1 menhrt nooneben tradmod menben womenben chldben /// 
allben kidsuffr fepresch mawrkwrm mapaid paidlvdv{
	recode `var' (0=.)(8/9=.)
}

recode educ(0/11=1)(12=2)(13/15=3)(16/20=4), gen(education)
gen college=0
replace college=1 if education==4

save "$temp/GSS_region_long.dta", replace

// prepping data to annualize
* recode binary so it's 0/1, not 1/2 (No=0, Yes=1 to whatever the question is)
foreach var in womenhrt chldhrt allhrt noonehrt menhrt nooneben menben womenben chldben allben{
	recode `var' (2=0)
}

* want agree to be HIGHER (right now AGREE=1 and disagree=5)
foreach var in famsuffr twoincs hubbywrk hubbywk1 twoincs1 kidsuffr mawrkwrm mapaid{
	recode `var' (1=5)(2=4)(4=2)(5=1)
}

* Others:
* tradmod: 1=man alone responsible; 2: both responsible
* fepresch: doesn't have 3 as neither - so 3=disagree, 4=strongly disagree. 
recode fepresch (1=5)(2=4)(3=2)(4=1)

* paidlvdv: who should take paid leave. 1= mother, 3=equal, 5=father

collapse (mean) womenhrt chldhrt allhrt noonehrt menhrt nooneben menben womenben chldben allben ///
				famsuffr twoincs hubbywrk hubbywk1 twoincs1 kidsuffr mawrkwrm mapaid fepresch ///
				tradmod paidlvdv, by(year region)
				
// how to fill in previous years: https://www.statalist.org/forums/forum/general-stata-discussion/general/1358759-duplicate-each-row-as-many-times-as-is-given-in-a-variable
// use expand to create new rows? then label?
				
save "$temp/GSS_region.dta", replace

save "$temp/GSS_region_altyears.dta", replace

drop if year==2010
recode year (2012=2011)(2014=2013)(2016=2015)(2018=2017)

append using "$temp/GSS_region.dta"
sort year region

rename region region_gss
recode region_gss (1=11)(2=12)(3=21)(4=22)(5=31)(6=32)(7=33)(8=41)(9=42), gen (region)

foreach var in famsuffr twoincs1 kidsuffr mawrkwrm paidlvdv hubbywk1 hubbywrk{
	bysort region (`var'): replace `var' = `var'[1]
}

gen hubbywk_all = hubbywk1 if year>=2010 & year <=2014 // asked in 2012 so will use for earlier years
replace hubbywk_all = hubbywrk if year >=2015 & year<=2018 // asked in 2018 so will use for later years

save "$created_data/GSS_region.dta", replace

// keepusing(famsuffr twoincs1 kidsuffr mawrkwrm paidlvdv hubbywk_all fepresch)

/*
GSS regions:
Valid   1 new england         |          9      11.11      11.11      11.11
        2 middle atlantic     |          9      11.11      11.11      22.22
        3 east north central  |          9      11.11      11.11      33.33
        4 west north central  |          9      11.11      11.11      44.44
        5 south atlantic      |          9      11.11      11.11      55.56
        6 east south atlantic |          9      11.11      11.11      66.67
        7 west south central  |          9      11.11      11.11      77.78
        8 mountain            |          9      11.11      11.11      88.89
        9 pacific             |          9      11.11      11.11     100.00
		
ACS regions:
Valid   11 New England Division     |         54      11.76      11.76      11.76
        12 Middle Atlantic Division |         27       5.88       5.88      17.65
        21 East North Central Div.  |         45       9.80       9.80      27.45
        22 West North Central Div.  |         63      13.73      13.73      41.18
        31 South Atlantic Division  |         81      17.65      17.65      58.82
        32 East South Central Div.  |         36       7.84       7.84      66.67
        33 West South Central Div.  |         36       7.84       7.84      74.51
        41 Mountain Division        |         72      15.69      15.69      90.20
        42 Pacific Division         |         45       9.80       9.80     100.00
/*

/* Variables
//2012 and 2002
* hubbywk1: Men should earn money women keep house (1=agree 5=disagree)
tab region hubbywk1 if (year==2002 | year ==2012) & inrange(hubbywk1,1,5), row
tab region hubbywk1 if year==2002 & inrange(hubbywk1,1,5), row
tab region hubbywk1 if year==2012 & inrange(hubbywk1,1,5), row

* twoincs1: Both men and women should contribute to income (1=agree 5=disagree)
tab region twoincs1 if (year==2002 | year ==2012) & inrange(twoincs1,1,5), row
tab region twoincs1 if year==2002 & inrange(twoincs1,1,5), row
tab region twoincs1 if year==2012 & inrange(twoincs1,1,5), row

* famsuffr: Family life suffers if mom works f-t (1=agree 5=disagree)
tab region famsuffr if (year==2002 | year ==2012) & inrange(famsuffr,1,5), row
tab region famsuffr if year==2002 & inrange(famsuffr,1,5), row
tab region famsuffr if year==2012 & inrange(famsuffr,1,5), row

//2018 and 2008
* hubbywrk: Husb shld work wife shld look after home
tab region hubbywrk if (year==2008 | year ==2018) & inrange(hubbywrk,1,5), row
tab region hubbywrk if year==2008 & inrange(hubbywrk,1,5), row
tab region hubbywrk if year==2018 & inrange(hubbywrk,1,5), row

// 1988, 1994, 2002, 2012
* kidsuffr: Preschool child suffers with working mother
* mawrkwrm: A working mother can establish just as warm and secure a relationship with her children as a mother who does not work

// 1988 onwards
* fepresch: Preschool child suffers with working mother (1=strongly agree -> 4 = strongly disagree) so as is - higher = more egal?
* I recoded - so higher now is more traditional? I am so confused...

// 1994 and 2002
* mapaid: Should working women have paid maternity leave?

// 2012
* paidlvdv: Who should take paid leave?
*/