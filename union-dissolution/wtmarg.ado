prog def wtmarg
quietly { // define passed arguments from command
	tempfile returner // save tempfile to return if process fails 
	save `returner' // see above 
	args pe1 pe2  // define arguments pased from command
}
disp ""
quietly { // rename variables passed from command 
	estimates restore `pe1' 
	estimates store pe1
	mat pe1check = e(k_predict)
	local pe1check = pe1check[1,1]
	disp "`pe1check'"
	estimates restore `pe2' 
	estimates store pe2
	mat pe2check = e(k_predict)
	local pe2check = pe2check[1,1]
	disp "`pe2check'"
	if `pe1check' == `pe2check' {
		noisily: disp ""
		noisily: disp in green "Wald Chi-Squared Test"
		noisily: disp ""
		local Variable = e(xvars)
		tokenize `Variable', parse( " "".")
		if missing(`"`2'"') {
			noisily: disp as text "dy/dx: "as result" c.`1'"
			local variable_check = 0
		} 
		else {
			noisily: disp as text "dy/dx: "as result" i.`3'"
			local variable_check = 1
		}
		noisily: disp in green "Ho: "as result" (`pe1' - `pe2')  = 0"
		noisily: disp in green "Ha: "as result" (`pe1' - `pe2') != 0"
	}
	else {
		disp in red "Outcomes Not The Same"
		use `returner', clear
		exit
	}
	if `variable_check' == 1 {
		local starts = pe1check[1,1] +1
		local ends = (pe1check[1,1] + pe1check[1,1])
		local outcome_quant = pe1check[1,1]
	}
	if `variable_check' == 0 {
		local starts = 1
		local ends = pe1check[1,1] 
		local outcome_quant = pe1check[1,1]
	}
}
quietly { 
	forvalue i = `starts'/`ends' {
		local outcome = `outcome'+1
		estimates restore `pe1'
		matrix stderror1 = sqrt(abs(e(V)[`i', `i']))
		local outerror_`outcome'_1  = stderror1[1,1]
		matrix point_est1 = e(b)[1,`i']
		local outpoint_`outcome'_1 = point_est1[1,1]
		estimates restore `pe2'
		matrix stderror2 = sqrt(abs(e(V)[`i', `i']))
		local outerror_`outcome'_2 = stderror2[1,1]
		matrix point_est2 = e(b)[1,`i']
		local outpoint_`outcome'_2 = point_est2[1,1]
	}
}
forvalue i = 1/`outcome_quant' {
	local diff`i' = abs(`outpoint_`i'_1' -`outpoint_`i'_2')
	local std_error`i' = sqrt(((`outerror_`i'_1')^2) + ((`outerror_`i'_2')^2))
	local zscore`i' = `diff`i''/`std_error`i''
	local pvalue`i' = (1-(normal(`zscore`i'')))*2
	local diff`i'r = round(`diff`i'',.0001)
	local std_error`i'r = round(`std_error`i'',.0001)
	local zscore`i'r = round(`zscore`i'',.0001)
	local pvalue`i'r = round(`pvalue`i'',.0001)	
	if `pvalue`i'r' <0.0001 {
		local pvalue`i'r = "0.0000"
	}
	display in white "{hline 60}"
	display in white "Outcome: `i'"
	display in white "{hline 60}"
	disp as text  "   DIFF: " as result " `diff`i'r'"
	disp as text  "     SE: " as result " `std_error`i'r'"
	disp as text  "Z-Score: " as result " `zscore`i'r'"
	disp as text  "P-Value: " as result " `pvalue`i'r'"	
	mat wr`i' = (`diff`i'', `std_error`i'', `zscore`i'', `pvalue`i'')
	mat PE`i' = `diff`i''
	matrix colnames PE`i' = Outcome_`i'
	mat SE`i' = `std_error`i''
}

local matlocal = "PE1"
if `outcome_quant' > 1 {
	forvalue i = 2/`outcome_quant' {
		local matlocal = "`matlocal', PE`i'"
	}
}
matrix DIFF = `matlocal'
local conf_level = 95
local comf = -1*(invnormal((1-(`conf_level'/100))/2)) // 
forvalue i = 1/`outcome_quant' {
	local intvl = `diff`i'' + (`std_error`i'' * `comf')
	local intvu = `diff`i'' - (`std_error`i'' * `comf')
	matrix CI`i' = `intvl' \ `intvu'
	matrix colnames CI`i' = Outcome_`i'
}

local matlocal = "CI1"
if `outcome_quant' > 1 {
	forvalue i = 2/`outcome_quant' {
		local matlocal = "`matlocal', CI`i'"
	}
}
matrix CI = `matlocal'
forvalue i = 1/`outcome_quant' {
	matrix wrt`i' = wr`i',`i'
}
local matlocal = "wrt1"
if `outcome_quant' > 1 {
	forvalue i = 2/`outcome_quant' {
		local matlocal = "`matlocal' \ wrt`i'"
	}
}
matrix WTR = `matlocal'
forvalue i = 1/`outcome_quant' {
	matrix drop wrt`i'
	matrix drop wr`i'
	matrix drop PE`i'
	matrix drop SE`i'
}
end 

/*
READ ME WTMARG 
Creator: Max Lisch 
Created: 05/08/2023
Version: v0.1

Documentation for wtmarg.ado program: 

wtmarg is a MAX created program that compares margins, dydx()
outputs between models with mutually exclusive observations.

wtmarg calculates z' scores for the difference of differences 
between predicted probabilities. It can handle logit and 
mlogit models with n discrete outcomes. At the moment it
can only use discrete predictors, but I plan to impliment
code for continuous predictors. 

For discrete predictors, wtmarg calcuates the difference 
in average discrete change btween two models. I may impliment
multiple comparison for >2 models but not yet. 

SET UP: 

1)	Download wtmarg.ado 
2)	Run set up code to load program:

quietly { 
	capture {
		wtmarg
	}
	if _rc == 199{
		quietly {
			do "T:\github\marriage_predictors\union-dissolution/wtmarg.ado"
			noisily: dis "wtmarg installed"
		}
	}
}


SYNTAX: 

wtmarg [modelmarginslist]

where modelmarginslist is a list of two stored margins, dydx()
commands. It calculates z' scores for the difference in the 
predict differences. Significance is assessed using a standard 
Wald Chi-Squared Test with pooled standard errors. Results are
presented as two-tailed p-values.

Outputs from wtmarg can be displayed using the coeffplot 
command. An example is provided. The default is a 95% CI. 


EXAMPLE 

EXP1: Binary Predictor and Outcomes

Setup 
	. webuse auto.dta
	. gen mpg_cat = (mpg<= 21)
	. gen price_cat = (price<= 5000)

Estimate: logistic models and margins, dydx(). First, estimate 
logit models for each mutually exclusive group. Run average 
discrete change margins for each group with "post" argument. 
Then, save ADC for each model using estimates store.

	. logit mpg_cat i.price_cat if foreign==0
	. margins, dydx(price_cat) post
	. estimates store est1
	. logit mpg_cat i.price_cat if foreign==1
	. margins, dydx(price_cat) post
	. estimates store est2

Run wtmarg command: Pass saved ADC results to wtmarg command

	. wtmarg est1 est2

Wald Chi-Squared Test

dy/dx:  i.price_cat
Ho:  (est1 - est2)  = 0
Ha:  (est1 - est2) != 0
------------------------------------------------------------
Outcome: 1
------------------------------------------------------------
   DIFF:  .0922
     SE:  .2079
Z-Score:  .4436
P-Value:  .6573

With a p-value of 0.65, we fail to reject the null that 
there is no difference in average discrete change for 
price_cat between foreign and domestic cars. 



EXP2: Catagorical Predictor and Outcomes

What if we want to use a catagorical predictor with n>2 
catagories? We construct a measure of car trunk size roughly 
coded as small, medium and large sized trunks. 

Setup 
	. webuse auto.dta
	. gen mpg_cat = (mpg<= 21)
	. egen trunk_cats = cut(trunk), at(0,12,16,25) icodes

Estimate: logistic models and margins, dydx(). First, estimate 
logit models for each mutually exclusive group. Run average 
discrete change margins for each group with "post" argument. 
IMPORTANT: You must specify a specific dummy predictor in 
margins command, if not the default is the first non-reference
catagory. Then, save ADC for each model using estimates store.

	. logit mpg_cat i.trunk_cats if foreign==0
	. margins, dydx(2.trunk_cats) post
	. estimates store est1
	. logit mpg_cat i.trunk_cats if foreign==1
	. margins, dydx(2.trunk_cats) post
	. estimates store est2

Run wtmarg command: Pass saved ADC results to wtmarg command

	. wtmarg est1 est2

Wald Chi-Squared Test

dy/dx:  i.trunk_cats
Ho:  (est1 - est2)  = 0
Ha:  (est1 - est2) != 0
------------------------------------------------------------
Outcome: 1
------------------------------------------------------------
   DIFF:  .3632
     SE:  .3879
Z-Score:  .9365
P-Value:  .349

With a p-value of 0.349, we fail to reject the null that 
there is no difference in average discrete change for the 
comparison of small to large trunks between foreign and 
domestic cars. 


INTEGREATION WITH COEFPLOT: Plotting Results 

wtmarg produces matrixes that can be used to produce 
publication quality plots using the coefplot command. 
It stores the following named matrixes: 

	DIFF:	Vector of estimated difference 

	CI: 	Matrix of pooled confidence intervals (95%)

	WTR:	Matrix of all Results 

	
SYNTAX

	. coefplot matrix(DIFF), ci(CI) 


coefplot takes a vector of the predicted differences and a 
matrix contrianing confidence intervals to produce a plot. 
I am currently working to allow a confidence interval 
argument to be specified with the wtmarg command to set
a confidence other than 95%. All the argumenets for the 
coefplot command shoudl work, but have not tested it. 

*/