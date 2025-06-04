global homedir "T:" // comment this out if you are not using the PRC Remote Server

* This is the base directory with the setup files.
* It is the directory you should change into before executing any files
global base_code "$homedir/github/SOC384/kmcerlean"
global code "$homedir/github/marriage_predictors/state-data"


* This locations of folders containing the original data files
global SIPP2014 "/data/sipp/2014" // you either need to have the data (currently on Box), in a folder structure that mimics this OR update this macro to the directory where you've stored the data
// global NSFG1517 "/data/NSFG/NSFG15_17"
global ACS "/data/ACS"
global ATUS "/data/ATUS"
global WVS "/data/WVS"
global GSS "/data/GSS"
global CPS "/data/CPS"
global licensing "/data/NSECE/licensing_data"

* Note that these directories will contain all "created" files - including intermediate data, results, and log files.

* created data files
global created_data "$homedir/Research Projects/State data/data_keep"

* results
global results "$homedir/Research Projects/State data/results"

* logdir
global logdir "$homedir/Research Projects/State data/logs"

* temporary data files (they get deleted without a second thought)
global temp "$homedir/Research Projects/State data/data_tmp"

********************************************************************************
/* Create macro for current date
global logdate = string( d(`c(current_date)'), "%dCY.N.D" ) 		// create a macro for the date*/
