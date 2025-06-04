import excel "T:\Research Projects\State data\state_info.xlsx", sheet("Sheet1") firstrow clear

save "$created_data\state_info.dta", replace

** https://www.stata.com/features/overview/latent-class-analysis/
** https://www.stata.com/features/latent-class-analysis/
** https://www.stata.com/meeting/uk18/slides/uk18_MacDonald.pdf

// first do factor analysis to confirm?! but these aren't real yet...
/* work-family: child_care equality

*labor force: female_lf_ratio female_earn_ratio women_professionals

*economic necessity: female_college female_poverty married_poverty gini

*culture: religiosity gender_attitudes
*/

gsem (child_care female_lf_ratio female_college married_poverty gini religiosity gender_attitudes <-), lclass(C 4) // DON'T INCLUDE LOGIT KIM


// https://www.stata.com/manuals/semexample52g.pdf: how to test # of classes?
gsem (child_care equality female_lf_ratio female_earn_ratio women_professionals female_college married_poverty gini religiosity gender_attitudes <-), lclass(C 3)
estimates store c3inv
gsem (child_care equality female_lf_ratio female_earn_ratio women_professionals female_college married_poverty gini religiosity gender_attitudes <-), lclass(C 4)
estimates store c4inv
gsem (child_care equality female_lf_ratio female_earn_ratio women_professionals female_college married_poverty gini religiosity gender_attitudes <-), lclass(C 5)
estimates store c5inv
gsem (child_care equality female_lf_ratio female_earn_ratio women_professionals female_college married_poverty gini religiosity gender_attitudes <-), lclass(C 6)
estimates store c6inv

estimates stats c3inv c4inv c5inv c6inv // want low AIC /BIC

gsem (child_care equality female_lf_ratio female_earn_ratio women_professionals female_college married_poverty gini religiosity gender_attitudes <-), lclass(C 6)

estat lcprob  // prob of being in each group
/*

          1  |   .1508147   .0552164      .0708799    .2925151
          2  |   .2197492   .0647105      .1184868    .3711191
          3  |   .2120081   .0592899       .118318    .3504022
          4  |   .2202625   .0610486      .1233784    .3618242
          5  |   .1775577   .0554644      .0930118    .3124772
          6  |   .0196078   .0194147      .0027552    .1264679
*/

estat lcmean // values by group


// predict which class a state is in

predict cpost*, classposteriorpr
egen max = rowmax(cpost*)
generate predclass = 1 if cpost1==max
replace predclass = 2 if cpost2==max
replace predclass = 3 if cpost3==max
replace predclass = 4 if cpost4==max
replace predclass = 5 if cpost5==max
replace predclass = 6 if cpost6==max

tabulate predclass

save "$created_data\state_lca.dta", replace

use "$created_data\state_hh_distribution.dta", clear
rename statefip state_code

merge m:1 state_code using "$created_data\state_lca.dta"

save "$created_data\state_combined.dta", replace

browse

gen male_sole = hh2_ct / (hh2_ct + hh3_ct + hh4_ct + hh5_ct)

regress male_sole i.predclass


/*
. regress male_sole i.predclass

      Source |       SS           df       MS      Number of obs   =       408
-------------+----------------------------------   F(5, 402)       =     28.43
       Model |  .267970737         5  .053594147   Prob > F        =    0.0000
    Residual |  .757797184       402  .001885068   R-squared       =    0.2612
-------------+----------------------------------   Adj R-squared   =    0.2521
       Total |  1.02576792       407  .002520314   Root MSE        =    .04342

------------------------------------------------------------------------------
   male_sole | Coefficient  Std. err.      t    P>|t|     [95% conf. interval]
-------------+----------------------------------------------------------------
   predclass |
          2  |   .0103097   .0071327     1.45   0.149    -.0037124    .0243317
          3  |  -.0504031   .0071327    -7.07   0.000    -.0644252   -.0363811
          4  |   -.046371   .0071327    -6.50   0.000     -.060393   -.0323489
          5  |  -.0418465   .0074589    -5.61   0.000    -.0565099   -.0271831
          6  |  -.0417384   .0162815    -2.56   0.011    -.0737459   -.0097308
             |
       _cons |   .2666995   .0054272    49.14   0.000     .2560303    .2773686
------------------------------------------------------------------------------
*/

margins predclass

/*
   predclass |
          1  |   .2666995   .0054272    49.14   0.000     .2560303    .2773686
          2  |   .2770091   .0046283    59.85   0.000     .2679104    .2861078
          3  |   .2162964   .0046283    46.73   0.000     .2071976    .2253951
          4  |   .2203285   .0046283    47.60   0.000     .2112298    .2294272
          5  |   .2248529   .0051168    43.94   0.000     .2147939    .2349119
          6  |   .2249611   .0153504    14.66   0.000     .1947841    .2551381
*/

regress male_sole i.predclass if couple_educ==0
margins predclass

/*
          1  |   .3062931   .0052224    58.65   0.000     .2959945    .3165917
          2  |   .3133636   .0044536    70.36   0.000      .304581    .3221463
          3  |   .2399785   .0044536    53.88   0.000     .2311958    .2487611
          4  |   .2443297   .0044536    54.86   0.000     .2355471    .2531124
          5  |   .2448406   .0049237    49.73   0.000     .2351311    .2545502
          6  |   .2982142    .014771    20.19   0.000     .2690855     .327343
*/

regress male_sole i.predclass if couple_educ==1 // still significant but not as large associations - but is this maybe not a story?!?! gah
margins predclass

/*
------------------------------------------------------------------------------
             |            Delta-method
             |     Margin   std. err.      t    P>|t|     [95% conf. interval]
-------------+----------------------------------------------------------------
   predclass |
          1  |   .2271058   .0055707    40.77   0.000     .2161204    .2380913
          2  |   .2406546   .0047507    50.66   0.000     .2312862     .250023
          3  |   .1926142   .0047507    40.54   0.000     .1832458    .2019826
          4  |   .1963272   .0047507    41.33   0.000     .1869588    .2056956
          5  |   .2048653   .0052521    39.01   0.000     .1945081    .2152224
          6  |   .1517079   .0157562     9.63   0.000     .1206364    .1827794
------------------------------------------------------------------------------
*/