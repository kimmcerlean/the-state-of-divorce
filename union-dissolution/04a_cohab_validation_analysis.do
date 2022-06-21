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
keep if relationship_start_v2 >=1976 & relationship_start_v2 <=1985
unique id, by(relationship_type) // slightly more of both, but also needed to be intact across two waves, how to designate that