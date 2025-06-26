********************************************************************************
** PRC STATS SERVER
********************************************************************************
if `"`c(hostname)'"' == "PPRC-STATS-P01"{
	
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
}

********************************************************************************
** PERSONAL COMPUTER, EUI ONEDRIVE
********************************************************************************
if `"`c(hostname)'"' == "LAPTOP-TP2VHI6B"{
	global homedir "C:/Users/mcerl/OneDrive - Istituto Universitario Europeo/projects/Divorce and Policy" // comment this out if you are not using the PRC Remote Server
	
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
* Check for package dependencies 
********************************************************************************
* This checks for packages that the user should install prior to running the project do files.

// fre: https://ideas.repec.org/c/boc/bocode/s456835.html
capture : which fre
if (_rc) {
    display as error in smcl `"Please install package {it:fre} from SSC in order to run these do-files;"' _newline ///
        `"you can do so by clicking this link: {stata "ssc install fre":auto-install fre}"'
    log close
    exit 199
}

capture: which ereplace 
if (_rc) {
    display as error in smcl `"Please install package {it:ereplace} from SSC in order to run these do-files;"' _newline ///
        `"you can do so by clicking this link: {stata "ssc install ereplace":auto-install ereplace}"'
    log close
    exit 199
}

capture: which stcompet 
if (_rc) {
    display as error in smcl `"Please install package {it:stcompet} from SSC in order to run these do-files;"' _newline ///
        `"you can do so by clicking this link: {stata "ssc install stcompet":auto-install stcompet}"'
    log close
    exit 199
}
