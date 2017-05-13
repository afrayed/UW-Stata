* Load data.
cd "\\csde-fs2.csde.washington.edu\t-desktops\harveyc\Desktop\Siegel"
use EulerNew.dta, replace
tsset lopnr year

* Generate other variables.
generate lnrf = ln(1 + rf)
generate lnrf_L1 = ln(1 + rf_L1)
generate lnrfsq = lnrf^2
generate lnrfsq_L1 = lnrf_L1^2
generate lncons_A_grsq = ln_consumption_A_gr^2
generate lncons_A_grandrf = ln_consumption_A_gr * lnrf
generate lncons_A_grsq_L1 = ln_consumption_A_gr_L1^2
generate lncons_A_grandrf_L1 = ln_consumption_A_gr_L1 * lnrf_L1

* Clean (arbitary).
sort lopnr year
* Subset here.
drop if sample != 1
egen group = group(lopnr)


**** STEP 1...
* LLE1 OLS.
generate gammaoneols = .
generate constantoneols = .
quietly summarize group
forvalues i = 1/`r(max)' {
	display "THE CURRENT WINDOW IS WINDOW " `i'
	capture regress ln_consumption_A_gr lnrf if (group == `i'), vce(robust)
	capture replace gammaoneols = _b[lnrf] if (group == `i')
	capture replace constantoneols = _b[_cons] if (group == `i')
}
save StepOne1, replace

* LLE1 IV1.
generate gammaoneivone = .
generate constantoneivone = .
quietly summarize group
forvalues i = 1/`r(max)' {
	display "THE CURRENT WINDOW IS WINDOW " `i'
	capture ivregress 2sls ln_consumption_A_gr (lnrf = lnrf_L1) if (group == `i'), vce(robust)
	capture replace gammaoneivone = _b[lnrf] if (group == `i')
	capture replace constantoneivone = _b[_cons] if (group == `i')
}
save StepOne2, replace

* LLE1 IV2.
generate gammaoneivtwo = .
generate constantoneivtwo = .
quietly summarize group
forvalues i = 1/`r(max)' {
	display "THE CURRENT WINDOW IS WINDOW " `i'
	capture ivregress 2sls ln_consumption_A_gr (lnrf = lnrf_L1 disp_income_SCB_pens_L1) if (group == `i'), vce(robust)
	capture replace gammaoneivtwo = _b[lnrf] if (group == `i')
	capture replace constantoneivtwo = _b[_cons] if (group == `i')
}
save StepOne3, replace

* LLE1 IV3.
generate gammaoneivthree = .
generate constantoneivthree = .
quietly summarize group
forvalues i = 1/`r(max)' {
	display "THE CURRENT WINDOW IS WINDOW " `i'
	capture ivregress 2sls ln_consumption_A_gr (lnrf = lnrf_L1 disp_income_SCB_pens_L1 ln_consumption_A_gr_L1) if (group == `i'), vce(robust)
	capture replace gammaoneivthree = _b[lnrf] if (group == `i')
	capture replace constantoneivthree = _b[_cons] if (group == `i')
}
save StepOne4, replace

* LLE2 OLS.
generate gammatwoolsa = .
generate gammatwoolsb = .
generate constanttwools = .
quietly summarize group
forvalues i = 1/`r(max)' {
	display "THE CURRENT WINDOW IS WINDOW " `i'
	capture regress ln_consumption_A_gr lnrf lncons_A_grsq if (group == `i'), vce(robust)
	capture replace gammatwoolsa = _b[lnrf] if (group == `i')
	capture replace gammatwoolsb = _b[lncons_A_grsq] if (group == `i')
	capture replace constanttwools = _b[_cons] if (group == `i')
}
save StepOne5, replace

* LLE2 IV2.
generate gammatwoivtwoa = .
generate gammatwoivtwob = .
generate constanttwoivtwo = .
quietly summarize group
forvalues i = 1/`r(max)' {
	display "THE CURRENT WINDOW IS WINDOW " `i'
	capture ivregress 2sls ln_consumption_A_gr (lnrf lncons_A_grsq = rf_L1 disp_income_SCB_pens_L1) if (group == `i'), vce(robust)
	capture replace gammatwoivtwoa = _b[lnrf] if (group == `i')
	capture replace gammatwoivtwob = _b[lncons_A_grsq] if (group == `i')
	capture replace constanttwoivtwo = _b[_cons] if (group == `i')
}
save StepOne6, replace

*** LLE2 IV3.
generate gammatwoivthreea = .
generate gammatwoivthreeb = .
generate constanttwoivthree = .
quietly summarize group
forvalues i = 1/`r(max)' {
	display "THE CURRENT WINDOW IS WINDOW " `i'
	capture ivregress 2sls ln_consumption_A_gr (lnrf lncons_A_grsq = rf_L1 disp_income_SCB_pens_L1 ln_consumption_A_gr_L1) if (group == `i'), vce(robust)
	capture replace gammatwoivthreea = _b[lnrf] if (group == `i')
	capture replace gammatwoivthreeb = _b[lncons_A_grsq] if (group == `i')
	capture replace constanttwoivthree = _b[_cons] if (group == `i')
}
save StepOne7, replace

* LLE3 OLS.
generate gammathreeolsa = .
generate gammathreeolsb = .
generate gammathreeolsc = .
generate gammathreeolsd = .
generate constantthreeols = .
quietly summarize group
forvalues i = 1/`r(max)' {
	display "THE CURRENT WINDOW IS WINDOW " `i'
	capture regress ln_consumption_A_gr lnrf lncons_A_grsq lnrfsq lncons_A_grandrf if (group == `i'), vce(robust)
	capture replace gammathreeolsa = _b[lnrf] if (group == `i')
	capture replace gammathreeolsb = _b[lncons_A_grsq] if (group == `i')
	capture replace gammathreeolsc = _b[lnrfsq] if (group == `i')
	capture replace gammathreeolsd = _b[lncons_A_grandrf] if (group == `i')
	capture replace constantthreeols = _b[_cons] if (group == `i')
}
save StepOne8, replace

*** Generate requested variances and covariances.
sort group year
egen covlncons_A_grandlnrf = corr(ln_consumption_A_gr lnrf), covariance by(group)
egen varlncons_A_gr = var(ln_consumption_A_gr), by(group)
egen varlnrf = var(lnrf), by(group)
egen meanlncons_A_gr = mean(ln_consumption_A_gr), by(group)

save StepOne, replace

*** Generate requested file...
use StepOne, replace
egen ncgrates = count(ln_consumption_A_gr), by(lopnr)
keep lopnr gammaoneols constantoneols gammaoneivone constantoneivone gammaoneivtwo constantoneivtwo gammaoneivthree constantoneivthree gammatwoolsa gammatwoolsb constanttwools gammatwoivtwoa gammatwoivtwob constanttwoivtwo gammathreeolsa gammathreeolsb gammathreeolsc gammathreeolsd constantthreeols gammatwoivthreea gammatwoivthreeb constanttwoivthree covlncons_A_grandlnrf varlncons_A_gr varlnrf meanlncons_A_gr ncgrates
order lopnr gammaoneols constantoneols gammaoneivone constantoneivone gammaoneivtwo constantoneivtwo gammaoneivthree constantoneivthree gammatwoolsa gammatwoolsb constanttwools gammatwoivtwoa gammatwoivtwob constanttwoivtwo gammatwoivthreea gammatwoivthreeb constanttwoivthree gammathreeolsa gammathreeolsb gammathreeolsc gammathreeolsd constantthreeols covlncons_A_grandlnrf varlncons_A_gr varlnrf meanlncons_A_gr ncgrates
rename gammaoneols LLE1_OLS_rf
rename constantoneols LLE1_OLS_cons
rename gammaoneivone LLE1_IV1_rf
rename constantoneivone LLE1_IV1_cons
rename gammaoneivtwo LLE1_IV2_rf
rename constantoneivtwo LLE1_IV2_cons
rename gammaoneivthree LLE1_IV3_rf
rename constantoneivthree LLE1_IV3_cons
rename gammatwoolsa LLE2_OLS_rf
rename gammatwoolsb LLE2_OLS_cons
rename constanttwools LLE2_OLS_cgsq
rename gammatwoivtwoa LLE2_IV2_rf
rename gammatwoivtwob LLE2_IV2_cgsq
rename constanttwoivtwo LLE2_IV2_cons
rename gammatwoivthreea LLE2_IV3_rf
rename gammatwoivthreeb LLE2_IV3_cgsq
rename constanttwoivthree LLE2_IV3_cons
rename gammathreeolsa LLE3_OLS_rf
rename gammathreeolsb LLE3_OLS_cgsq
rename gammathreeolsc LLE3_OLS_rfsq
rename gammathreeolsd LLE3_OLS_cg_rf
rename constantthreeols LLE3_OLS_cons
rename covlncons_A_grandlnrf cov_cg_rf
rename varlncons_A_gr var_cg
rename varlnrf var_rf
rename meanlncons_A_gr average_cg

collapse LLE1_OLS_rf - ncgrates, by(lopnr)
save StepOnePanel, replace
outsheet using StepOnePanel.csv , comma replace

*** Summary statistics...
log using SummaryStatistics.txt, replace
summarize
summarize, detail
log close

*** Alternative.
use StepOnePanel, replace
foreach i of varlist LLE1_OLS_rf-average_cg {
egen `i'_n = count(`i')
egen `i'_mean = mean(`i')
egen `i'_median = median(`i')
egen `i'_min = min(`i')
egen `i'_max = max(`i')
egen `i'_p1 = pctile(`i'), p(1)
egen `i'_p99 = pctile(`i'), p(99)
egen `i'_stdev = sd(`i')
}
drop if _n != 1
save StepOneSummary, replace
drop lopnr - average_cg
outsheet using StepOneSummary.csv , comma replace

*** Dataset with average consumption growth rates used.
use StepOnePanel, replace
drop LLE1_OLS_rf - var_rf
save StepOneCGRates, replace
outsheet using StepOneCGRates.csv , comma replace
