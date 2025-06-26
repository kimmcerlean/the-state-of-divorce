********************************************************************************
** PRC STATS SERVER
********************************************************************************

if `"`c(hostname)'"' == "PPRC-STATS-P01"{
	
	global homedir "T:"

	* This is the base directory with the setup files.
	* It is the directory you should change into before executing any files
	global code "$homedir/github/the-state-of-divorce"

	* This locations of folders containing the original data files
	global PSID "/data/PSID"
	global SIPP2014 "/data/sipp/2014"
	global ACS "/data/ACS"
	global CPS "/data/CPS"
	global ATUS "/data/ATUS"
	global WVS "/data/WVS"
	global GSS "/data/GSS"
	global licensing "/data/NSECE/licensing_data"

	* Note that these directories will contain all "created" files - including intermediate data, results, and log files.

	* created data files
	global created_data "$homedir/Research Projects/Dissertation - Union Dissolution/data_keep"
	global state_data "$homedir/Research Projects/State data/data_keep"
	global structural "$homedir/data/structural support measure"

	* results
	// global results "$homedir/Research Projects/Dissertation - Union Dissolution/results"
	global results "$homedir/Research Projects/State data/results"

	* logdir
	// global logdir "$homedir/Research Projects/Dissertation - Union Dissolution/logs"
	global logdir "$homedir/Research Projects/State data/logs"


	* temporary data files (they get deleted without a second thought)
	// global temp "$homedir/Research Projects/Dissertation - Union Dissolution/data_tmp"
	global temp "$homedir/Research Projects/State data/data_tmp"

}

********************************************************************************
** PERSONAL COMPUTER, EUI ONEDRIVE
********************************************************************************
if `"`c(hostname)'"' == "LAPTOP-TP2VHI6B"{
	global homedir "C:/Users/mcerl/OneDrive - Istituto Universitario Europeo/projects/Divorce and Policy"
	
	global code "G:/Other computers/My Laptop/Documents/GitHub/the-state-of-divorce"
	
	*input data
	global data "$homedir/data"
	global CPS "$homedir/data"
	global ACS "$homedir/data"
	global GSS "$homedir/data"
	
	*outputs
	global created_data "$homedir/created data"
	global results "$homedir/results"
	global logdir "$homedir/logs"
	global temp "$homedir/temp data"

}

********************************************************************************
** Create macro for current date to use when saving files
********************************************************************************
global logdate = string( d(`c(current_date)'), "%dCY.N.D" )
