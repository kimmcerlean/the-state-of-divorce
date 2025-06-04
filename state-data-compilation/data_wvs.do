********************************************************************************
* Compiling WVS files to get what I need
* wvs_data.do
* Kim McErlean
********************************************************************************

use "$WVS/F00011930-WVS_TimeSeries_1981_2022_Stata_v3_0/WVS_TimeSeries_1981_2022_Stata_v3_0.dta", clear

browse S006 S007 S020 S003 COW_NUM

keep if COW_NUM==2 // just US

/* variables
S006 	ID
S007	ID
S020	year
S003	Country // US = 840
COW_NUM Country // US = 2
X048ISO	Region // lots of missing
X048WVS Region //lots of missing - THIS HAS MORE FILLED IN THOUGH, bc have region for some, not state
D037	Important for succesful marriage: Sharing household chores - 1990
D058	Husband and wife should both contribute to income - 1990, 1995, 1999
D061	Pre-school child suffers with working mother - 1990, 2011, 2017
D066	Problem if women have more income than husband (4 categories) - only 1995
D066_01	Problem if women have more income than husband (5 categories) - only 2017
D066_B	Problem if women have more income than husband (3 categories) - filled in most? - 2011 and 2017, 2017 matches above, but coding seems off
D078	Men make better business executives than women do - 2006, 2011, 2017
*/


// maybe pull WVS GSS and see how similar / different the regions are? Possibly use region and GSS since limited state data in WVS AND questions might not be as good?
// OR apply for GSS state data? might actually be the move...