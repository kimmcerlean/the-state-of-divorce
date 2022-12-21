global homedir "T:" // comment this out if you are not using the PRC Remote Server

* This is the base directory with the setup files.
* It is the directory you should change into before executing any files
global code "$homedir/github/marriage_predictors/union-dissolution"


* This locations of folders containing the original data files
global PSID "/data/PSID"
global SIPP2014 "/data/sipp/2014"
global ACS "/data/ACS"
global CPS "/data/CPS"


* Note that these directories will contain all "created" files - including intermediate data, results, and log files.

* created data files
global created_data "$homedir/Research Projects/Dissertation - Union Dissolution/data_keep"

* results
global results "$homedir/Research Projects/Dissertation - Union Dissolution/results"

* logdir
global logdir "$homedir/Research Projects/Dissertation - Union Dissolution/logs"

* temporary data files (they get deleted without a second thought)
global temp "$homedir/Research Projects/Dissertation - Union Dissolution/data_tmp"

********************************************************************************
/* Create macro for current date
global logdate = string( d(`c(current_date)'), "%dCY.N.D" ) 		// create a macro for the date*/
