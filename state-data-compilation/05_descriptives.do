********************************************************************************
* Descriptive table for proposal
* descriptives.do
* Kim McErlean
********************************************************************************

use "$created_data/2010_2018_individ_analysis_LAG.dta", clear // created in 3a

unique year serial, by(college)
tab college

/* Variables
infant_care_pct
cc_percent_served
childcare_gap
x_all_prob
x_all_pt
fepresch
male_bw
dual_earn
college
*/

tab college male_bw, row
tab college dual_earn, row

tabstat infant_care_pct, by(college)
tabstat cc_percent_served, by(college)
tabstat childcare_gap, by(college)
tabstat x_all_prob, by(college)
tabstat x_all_pt, by(college)
tabstat fepresch, by(college)