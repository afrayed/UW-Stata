set more off

*** Set folder...
cd "H:\EFA"

*** First, get the FF factors in order to get the 12 month market return.
*** Need this in order to calculate the excess return we want to use for 
*** ranking later on.
*** FFRF is the Fama-French research factors text file with the top lines 
*** and bottom (annual factors) trimmed.
infile date rmrf smb hml rf using FFRF.txt, clear
sort date
gen year = round(date / 100)
gen month = round(date - (year * 100))
gen timedate = ym(year, month)
drop year month
tsset timedate
format timedate %tm
gen mktret = (rmrf + rf) / 100
gen calcmktret = ln(1 + mktret)
drop smb hml rmrf rf
mvsumm l.calcmktret, generate(calcmktret12) stat(sum) window(12) end
gen mktret12 = exp(calcmktret12) - 1
drop calcmktret calcmktret12
save marketreturn, replace

*** Second, calculation of the 12 month portfolio return.
insheet using return.csv, clear
gen calcportret = ln(1 + portret)
sort newportid date
gen year = round(date / 10000)
gen month = round((date - (year * 10000))/100)
gen timedate = ym(year, month)
drop year month
tsset newportid timedate
format timedate %tm
tsfill
mvsumm l.calcportret, generate(calcportret12) stat(sum) window(12) end
gen portret12 = exp(calcportret12) - 1
drop calcportret calcportret12
save return, replace

*** Merge the portfolio returns and market returns file to get excess returns.
merge m:1 timedate using marketreturn, nogenerate
gen calcexcportret = ln(1 + portret - mktret)
sort newportid timedate
tsset newportid timedate
tsfill
mvsumm l.calcexcportret, generate(calcexcportret12) stat(sum) window(12) end
gen excportret12 = exp(calcexcportret12) - 1
drop date calcexcportret calcexcportret12
save combinedreturn, replace

*** Now get the component return data...
insheet using aug06outputdata.csv, clear
sort newportid date
gen testdate = date(date, "DMY")
format testdate %td
gen year = year(testdate)
gen month = month(testdate)
gen timedate = ym(year, month)
drop year month testdate
tsset newportid timedate
format timedate %tm
save outputdata, replace

*** Merge with the portfolio and market returns file.
merge m:1 newportid timedate using combinedreturn, keep(3) nogenerate
sort newportid timedate

*** Create the rankings.
*** Unnecessary lines commented out for now.
by date, sort: egen mktadj_group = xtile(mktadj), nq(10)
by date, sort: egen excportret12_group = xtile(excportret12), nq(10)
by date, sort: egen portret12_group = xtile(portret12), nq(10)
by date, sort: egen capm_group = xtile(capm_alpha1), nq(10)
* by date, sort: egen ff3_group = xtile(ff3_alpha1), nq(10)
* by date, sort: egen ff4_group = xtile(alpha1), nq(10)
* by date, sort: egen sr_group = xtile(sr1), nq(10)

*** Setting up dummies based on rankings.
forvalues i = 1(1)10{
	forvalues j = 1(1)10{
		* gen adcf`i'`j' = 0
		* replace adcf`i'`j' = 1 if capm_group == `i' & ff3_group == `j'
		gen adcm`i'`j' = 0
		replace adcm`i'`j' = 1 if capm_group == `i' & mktadj_group == `j'
		* gen adff`i'`j' = 0
		* replace adff`i'`j' = 1 if ff3_group == `i' & ff4_group == `j'
		* gen admf`i'`j' = 0
		* replace admf`i'`j' = 1 if mktadj_group == `i' & ff3_group == `j'
		* gen adcs`i'`j' = 0
		* replace adcs`i'`j' = 1 if capm_group == `i' & sr_group == `j' 
		* gen adsm`i'`j' = 0
		* replace adsm`i'`j' = 1 if sr_group == `i' & mktadj_group == `j'
		* gen adsf`i'`j' = 0
		* replace adsf`i'`j' = 1 if sr_group == `i' & ff3_group == `j'
		* gen adef`i'`j' = 0
		* replace adef`i'`j' = 1 if excportret12_group == `i' & ff3_group == `j'
		gen adce`i'`j' = 0
		replace adce`i'`j' = 1 if capm_group == `i' & excportret12_group == `j'
		* gen adpf`i'`j' = 0
		* replace adpf`i'`j' = 1 if portret12_group == `i' & ff3_group == `j'
		
		gen adcp`i'`j' = 0
		replace adcp`i'`j' = 1 if capm_group == `i' & portret12_group == `j'
		gen admp`i'`j' = 0
		replace admp`i'`j' = 1 if mktadj_group == `i' & portret12_group == `j'	
		gen adpe`i'`j' = 0
		replace adpe`i'`j' = 1 if portret12_group == `i' & excportret12_group == `j'
		gen adme`i'`j' = 0
		replace adme`i'`j' = 1 if mktadj_group == `i' & excportret12_group == `j'	
	}
}

*** Calculate the difference between the total return and the factor returns.
*** Also generate it on a 12 month and 1 month basis just in case.
gen portret1 = ((1 + portret12)^(1/12)) - 1
gen excportret1 = ((1 + excportret12)^(1/12)) - 1
gen returndiff1 = portret1 - alpha1 - bf_exrm - bf_smb - bf_hml - bf_mom
gen alpha112 = alpha1 * 12
gen bf_exrm12 = bf_exrm * 12
gen bf_smb12 = bf_smb * 12
gen bf_hml12 = bf_hml * 12
gen bf_mom12 = bf_mom * 12
gen returndiff12 = portret12 - alpha112 - bf_exrm12 - bf_smb12 - bf_hml12 - bf_mom12
gen mktadjportretgroupdiff = mktadj_group - portret12_group
gen mktadjexcportretgroupdiff = mktadj_group - excportret12_group
gen portretexcportretgroupdiff = portret12_group - excportret12_group

tsset newportid timedate
gen std_flowdiff = D.std_flow
gen alpha1diff = D.alpha1
gen bf_exrmdiff = D.bf_exrm
gen bf_smbdiff = D.bf_smb
gen bf_hmldiff = D.bf_hml
gen bf_momdiff = D.bf_mom
gen lglagmtnadiff = D.lglagmtna
gen lgagediff = D.lgage
gen alpha112diff = D.alpha112
gen bf_exrm12diff = D.bf_exrm12
gen bf_smb12diff = D.bf_smb12
gen bf_hml12diff = D.bf_hml12
gen bf_mom12diff = D.bf_mom12
save testdata, replace

use testdata, clear

*** Check differences in ranking...
estpost tabulate mktadjportretgroupdiff
esttab using outrankdiffmp, cells("b pct cumpct") csv replace
estpost tabulate mktadjexcportretgroupdiff
esttab using outrankdiffme, cells("b pct cumpct") csv replace
estpost tabulate portretexcportretgroupdiff
esttab using outrankdiffpe, cells("b pct cumpct") csv replace

*** Check the return difference - 12 month or monthly?
estpost summarize returndiff12 returndiff1 portret12 portret1 excportret12 excportret1 avgret mktadj capm_alpha1 alpha1 alpha112 ff3_alpha1 b1f_exrm bf_exrm bf_exrm12 bf_smb bf_smb12 bf_hml bf_hml12 bf_mom bf_mom12
esttab using outreturndiff, cells("mean(fmt(a3)) sd") csv replace

*** Replicate and extend table 4 with the return difference.
xtset newportid timedate
xtreg std_flow alpha1 bf_exrm bf_smb bf_hml bf_mom lglagmtna lgage i.timedate, fe vce(r)
outreg2 using outtable4a, excel replace
xtreg std_flow alpha1 bf_exrm bf_smb bf_hml bf_mom lglagmtna lgage returndiff1 i.timedate, fe vce(r)
outreg2 using outtable4a, excel append
xtreg std_flow alpha112 bf_exrm12 bf_smb12 bf_hml12 bf_mom12 lglagmtna lgage i.timedate, fe vce(r)
outreg2 using outtable4b, excel append
xtreg std_flow alpha112 bf_exrm12 bf_smb12 bf_hml12 bf_mom12 lglagmtna lgage returndiff12 i.timedate, fe vce(r)
outreg2 using outtable4b, excel append


*** First differences?
xtreg std_flow alpha1diff bf_exrmdiff bf_smbdiff bf_hmldiff bf_momdiff lglagmtnadiff lgagediff i.timedate, fe vce(r)
outreg2 using outtable4c, excel replace
xtreg std_flow alpha112diff bf_exrm12diff bf_smb12diff bf_hml12diff bf_mom12diff lglagmtnadiff lgagediff i.timedate, fe vce(r)
outreg2 using outtable4c, excel append
xtreg std_flowdiff alpha1diff bf_exrmdiff bf_smbdiff bf_hmldiff bf_momdiff lglagmtnadiff lgagediff i.timedate, fe vce(r)
outreg2 using outtable4d, excel replace
xtreg std_flowdiff alpha112diff bf_exrm12diff bf_smb12diff bf_hml12diff bf_mom12diff lglagmtnadiff lgagediff i.timedate, fe vce(r)
outreg2 using outtable4d, excel append

*** Get Ns for the dummies.
* estpost summarize adef1* adef2* adef3* adef4* adef51 adef52 adef53 adef54 adef56 adef57 adef58 adef59 adef510 adef6* adef7* adef8* adef9*
* esttab using outdummystatsef, cells("sum") csv replace
* estpost summarize adce1* adce2* adce3* adce4* adce51 adce52 adce53 adce54 adce56 adce57 adce58 adce59 adce510 adce6* adce7* adce8* adce9*
* esttab using outdummystatsce, cells("sum") csv replace
estpost summarize adcp1* adcp2* adcp3* adcp4* adcp51 adcp52 adcp53 adcp54 adcp56 adcp57 adcp58 adcp59 adcp510 adcp6* adcp7* adcp8* adcp9*
esttab using outtable3statscp, cells("sum") csv replace
estpost summarize admp1* admp2* admp3* admp4* admp51 admp52 admp53 admp54 admp56 admp57 admp58 admp59 admp510 admp6* admp7* admp8* admp9*
esttab using outtable3statsmp, cells("sum") csv replace
estpost summarize adme1* adme2* adme3* adme4* adme51 adme52 adme53 adme54 adme56 adme57 adme58 adme59 adme510 adme6* adme7* adme8* adme9*
esttab using outtable3statsme, cells("sum") csv replace
estpost summarize adpe1* adpe2* adpe3* adpe4* adpe51 adpe52 adpe53 adpe54 adpe56 adpe57 adpe58 adpe59 adpe510 adpe6* adpe7* adpe8* adpe9*
esttab using outtable3statspe, cells("sum") csv replace
estpost summarize adcm1* adcm2* adcm3* adcm4* adcm51 adcm52 adcm53 adcm54 adcm56 adcm57 adcm58 adcm59 adcm510 adcm6* adcm7* adcm8* adcm9*
esttab using outtable3statscm, cells("sum") csv replace
estpost summarize adce1* adce2* adce3* adce4* adce51 adce52 adce53 adce54 adce56 adce57 adce58 adce59 adce510 adce6* adce7* adce8* adce9*
esttab using outtable3statsce, cells("sum") csv replace

*** Now do the regression for table 3.
xtset newportid timedate
* xtreg std_flow adef1* adef2* adef3* adef4* adef51 adef52 adef53 adef54 adef56 adef57 adef58 adef59 adef510 adef6* adef7* adef8* adef9* lglagmtna, fe vce(r)
* outreg2 using outtable3ef, excel replace
* test (adef109 - adef910 = 0) (adef108 - adef810 = 0) (adef107 - adef710 = 0) (adef106 - adef610 = 0) (adef105 - adef510 = 0) (adef104 - adef410 = 0) (adef103 - adef310 = 0) (adef102 - adef210 = 0) (adef101 - adef110 = 0) (adef98 - adef89 = 0) (adef97 - adef79 = 0) (adef96 - adef69 = 0) (adef95 - adef59 = 0) (adef94 - adef49 = 0) (adef93 - adef39 = 0) (adef92 - adef29 = 0) (adef91 - adef19 = 0) (adef87 - adef78 = 0) (adef86 - adef68 = 0) (adef85 - adef58 = 0) (adef84 - adef48 = 0) (adef83 - adef38 = 0) (adef82 - adef28 = 0) (adef81 - adef18 = 0) (adef76 - adef67 = 0) (adef75 - adef57 = 0) (adef74 - adef47 = 0) (adef73 - adef37 = 0) (adef72 - adef27 = 0) (adef71 - adef17 = 0) (adef65 - adef56 = 0) (adef64 - adef46 = 0) (adef63 - adef36 = 0) (adef62 - adef26 = 0) (adef61 - adef16 = 0) (adef54 - adef45 = 0) (adef53 - adef35 = 0) (adef52 - adef25 = 0) (adef51 - adef15 = 0) (adef43 - adef34 = 0) (adef42 - adef24 = 0) (adef41 - adef14 = 0) (adef32 - adef23 = 0) (adef31 - adef13 = 0) (adef21 - adef12 = 0), mtest
* xtreg std_flow adce1* adce2* adce3* adce4* adce51 adce52 adce53 adce54 adce56 adce57 adce58 adce59 adce510 adce6* adce7* adce8* adce9* lglagmtna, fe vce(r)
* outreg2 using outtable3ce, excel replace
* test (adce109 - adce910 = 0) (adce108 - adce810 = 0) (adce107 - adce710 = 0) (adce106 - adce610 = 0) (adce105 - adce510 = 0) (adce104 - adce410 = 0) (adce103 - adce310 = 0) (adce102 - adce210 = 0) (adce101 - adce110 = 0) (adce98 - adce89 = 0) (adce97 - adce79 = 0) (adce96 - adce69 = 0) (adce95 - adce59 = 0) (adce94 - adce49 = 0) (adce93 - adce39 = 0) (adce92 - adce29 = 0) (adce91 - adce19 = 0) (adce87 - adce78 = 0) (adce86 - adce68 = 0) (adce85 - adce58 = 0) (adce84 - adce48 = 0) (adce83 - adce38 = 0) (adce82 - adce28 = 0) (adce81 - adce18 = 0) (adce76 - adce67 = 0) (adce75 - adce57 = 0) (adce74 - adce47 = 0) (adce73 - adce37 = 0) (adce72 - adce27 = 0) (adce71 - adce17 = 0) (adce65 - adce56 = 0) (adce64 - adce46 = 0) (adce63 - adce36 = 0) (adce62 - adce26 = 0) (adce61 - adce16 = 0) (adce54 - adce45 = 0) (adce53 - adce35 = 0) (adce52 - adce25 = 0) (adce51 - adce15 = 0) (adce43 - adce34 = 0) (adce42 - adce24 = 0) (adce41 - adce14 = 0) (adce32 - adce23 = 0) (adce31 - adce13 = 0) (adce21 - adce12 = 0), mtest
xtreg std_flow adcp1* adcp2* adcp3* adcp4* adcp51 adcp52 adcp53 adcp54 adcp56 adcp57 adcp58 adcp59 adcp510 adcp6* adcp7* adcp8* adcp9* lglagmtna i.timedate, fe vce(r)
outreg2 using outtable3cp, excel replace
* test (adcp109 - adcp910 = 0) (adcp108 - adcp810 = 0) (adcp107 - adcp710 = 0) (adcp106 - adcp610 = 0) (adcp105 - adcp510 = 0) (adcp104 - adcp410 = 0) (adcp103 - adcp310 = 0) (adcp102 - adcp210 = 0) (adcp101 - adcp110 = 0) (adcp98 - adcp89 = 0) (adcp97 - adcp79 = 0) (adcp96 - adcp69 = 0) (adcp95 - adcp59 = 0) (adcp94 - adcp49 = 0) (adcp93 - adcp39 = 0) (adcp92 - adcp29 = 0) (adcp91 - adcp19 = 0) (adcp87 - adcp78 = 0) (adcp86 - adcp68 = 0) (adcp85 - adcp58 = 0) (adcp84 - adcp48 = 0) (adcp83 - adcp38 = 0) (adcp82 - adcp28 = 0) (adcp81 - adcp18 = 0) (adcp76 - adcp67 = 0) (adcp75 - adcp57 = 0) (adcp74 - adcp47 = 0) (adcp73 - adcp37 = 0) (adcp72 - adcp27 = 0) (adcp71 - adcp17 = 0) (adcp65 - adcp56 = 0) (adcp64 - adcp46 = 0) (adcp63 - adcp36 = 0) (adcp62 - adcp26 = 0) (adcp61 - adcp16 = 0) (adcp54 - adcp45 = 0) (adcp53 - adcp35 = 0) (adcp52 - adcp25 = 0) (adcp51 - adcp15 = 0) (adcp43 - adcp34 = 0) (adcp42 - adcp24 = 0) (adcp41 - adcp14 = 0) (adcp32 - adcp23 = 0) (adcp31 - adcp13 = 0) (adcp21 - adcp12 = 0), mtest
xtreg std_flow admp1* admp2* admp3* admp4* admp51 admp52 admp53 admp54 admp56 admp57 admp58 admp59 admp510 admp6* admp7* admp8* admp9* lglagmtna i.timedate, fe vce(r)
outreg2 using outtable3mp, excel replace
* test (admp109 - admp910 = 0) (admp108 - admp810 = 0) (admp107 - admp710 = 0) (admp106 - admp610 = 0) (admp105 - admp510 = 0) (admp104 - admp410 = 0) (admp103 - admp310 = 0) (admp102 - admp210 = 0) (admp101 - admp110 = 0) (admp98 - admp89 = 0) (admp97 - admp79 = 0) (admp96 - admp69 = 0) (admp95 - admp59 = 0) (admp94 - admp49 = 0) (admp93 - admp39 = 0) (admp92 - admp29 = 0) (admp91 - admp19 = 0) (admp87 - admp78 = 0) (admp86 - admp68 = 0) (admp85 - admp58 = 0) (admp84 - admp48 = 0) (admp83 - admp38 = 0) (admp82 - admp28 = 0) (admp81 - admp18 = 0) (admp76 - admp67 = 0) (admp75 - admp57 = 0) (admp74 - admp47 = 0) (admp73 - admp37 = 0) (admp72 - admp27 = 0) (admp71 - admp17 = 0) (admp65 - admp56 = 0) (admp64 - admp46 = 0) (admp63 - admp36 = 0) (admp62 - admp26 = 0) (admp61 - admp16 = 0) (admp54 - admp45 = 0) (admp53 - admp35 = 0) (admp52 - admp25 = 0) (admp51 - admp15 = 0) (admp43 - admp34 = 0) (admp42 - admp24 = 0) (admp41 - admp14 = 0) (admp32 - admp23 = 0) (admp31 - admp13 = 0) (admp21 - admp12 = 0), mtest
xtreg std_flow adme1* adme2* adme3* adme4* adme51 adme52 adme53 adme54 adme56 adme57 adme58 adme59 adme510 adme6* adme7* adme8* adme9* lglagmtna i.timedate, fe vce(r)
outreg2 using outtable3me, excel replace
* test (adme109 - adme910 = 0) (adme108 - adme810 = 0) (adme107 - adme710 = 0) (adme106 - adme610 = 0) (adme105 - adme510 = 0) (adme104 - adme410 = 0) (adme103 - adme310 = 0) (adme102 - adme210 = 0) (adme101 - adme110 = 0) (adme98 - adme89 = 0) (adme97 - adme79 = 0) (adme96 - adme69 = 0) (adme95 - adme59 = 0) (adme94 - adme49 = 0) (adme93 - adme39 = 0) (adme92 - adme29 = 0) (adme91 - adme19 = 0) (adme87 - adme78 = 0) (adme86 - adme68 = 0) (adme85 - adme58 = 0) (adme84 - adme48 = 0) (adme83 - adme38 = 0) (adme82 - adme28 = 0) (adme81 - adme18 = 0) (adme76 - adme67 = 0) (adme75 - adme57 = 0) (adme74 - adme47 = 0) (adme73 - adme37 = 0) (adme72 - adme27 = 0) (adme71 - adme17 = 0) (adme65 - adme56 = 0) (adme64 - adme46 = 0) (adme63 - adme36 = 0) (adme62 - adme26 = 0) (adme61 - adme16 = 0) (adme54 - adme45 = 0) (adme53 - adme35 = 0) (adme52 - adme25 = 0) (adme51 - adme15 = 0) (adme43 - adme34 = 0) (adme42 - adme24 = 0) (adme41 - adme14 = 0) (adme32 - adme23 = 0) (adme31 - adme13 = 0) (adme21 - adme12 = 0), mtest
xtreg std_flow adpe1* adpe2* adpe3* adpe4* adpe51 adpe52 adpe53 adpe54 adpe56 adpe57 adpe58 adpe59 adpe510 adpe6* adpe7* adpe8* adpe9* lglagmtna i.timedate, fe vce(r)
outreg2 using outtable3pe, excel replace
* test (adpe109 - adpe910 = 0) (adpe108 - adpe810 = 0) (adpe107 - adpe710 = 0) (adpe106 - adpe610 = 0) (adpe105 - adpe510 = 0) (adpe104 - adpe410 = 0) (adpe103 - adpe310 = 0) (adpe102 - adpe210 = 0) (adpe101 - adpe110 = 0) (adpe98 - adpe89 = 0) (adpe97 - adpe79 = 0) (adpe96 - adpe69 = 0) (adpe95 - adpe59 = 0) (adpe94 - adpe49 = 0) (adpe93 - adpe39 = 0) (adpe92 - adpe29 = 0) (adpe91 - adpe19 = 0) (adpe87 - adpe78 = 0) (adpe86 - adpe68 = 0) (adpe85 - adpe58 = 0) (adpe84 - adpe48 = 0) (adpe83 - adpe38 = 0) (adpe82 - adpe28 = 0) (adpe81 - adpe18 = 0) (adpe76 - adpe67 = 0) (adpe75 - adpe57 = 0) (adpe74 - adpe47 = 0) (adpe73 - adpe37 = 0) (adpe72 - adpe27 = 0) (adpe71 - adpe17 = 0) (adpe65 - adpe56 = 0) (adpe64 - adpe46 = 0) (adpe63 - adpe36 = 0) (adpe62 - adpe26 = 0) (adpe61 - adpe16 = 0) (adpe54 - adpe45 = 0) (adpe53 - adpe35 = 0) (adpe52 - adpe25 = 0) (adpe51 - adpe15 = 0) (adpe43 - adpe34 = 0) (adpe42 - adpe24 = 0) (adpe41 - adpe14 = 0) (adpe32 - adpe23 = 0) (adpe31 - adpe13 = 0) (adpe21 - adpe12 = 0), mtest
xtreg std_flow adcm1* adcm2* adcm3* adcm4* adcm51 adcm52 adcm53 adcm54 adcm56 adcm57 adcm58 adcm59 adcm510 adcm6* adcm7* adcm8* adcm9* lglagmtna i.timedate, fe vce(r)
outreg2 using outtable3cm, excel replace
* test (adcm109 - adcm910 = 0) (adcm108 - adcm810 = 0) (adcm107 - adcm710 = 0) (adcm106 - adcm610 = 0) (adcm105 - adcm510 = 0) (adcm104 - adcm410 = 0) (adcm103 - adcm310 = 0) (adcm102 - adcm210 = 0) (adcm101 - adcm110 = 0) (adcm98 - adcm89 = 0) (adcm97 - adcm79 = 0) (adcm96 - adcm69 = 0) (adcm95 - adcm59 = 0) (adcm94 - adcm49 = 0) (adcm93 - adcm39 = 0) (adcm92 - adcm29 = 0) (adcm91 - adcm19 = 0) (adcm87 - adcm78 = 0) (adcm86 - adcm68 = 0) (adcm85 - adcm58 = 0) (adcm84 - adcm48 = 0) (adcm83 - adcm38 = 0) (adcm82 - adcm28 = 0) (adcm81 - adcm18 = 0) (adcm76 - adcm67 = 0) (adcm75 - adcm57 = 0) (adcm74 - adcm47 = 0) (adcm73 - adcm37 = 0) (adcm72 - adcm27 = 0) (adcm71 - adcm17 = 0) (adcm65 - adcm56 = 0) (adcm64 - adcm46 = 0) (adcm63 - adcm36 = 0) (adcm62 - adcm26 = 0) (adcm61 - adcm16 = 0) (adcm54 - adcm45 = 0) (adcm53 - adcm35 = 0) (adcm52 - adcm25 = 0) (adcm51 - adcm15 = 0) (adcm43 - adcm34 = 0) (adcm42 - adcm24 = 0) (adcm41 - adcm14 = 0) (adcm32 - adcm23 = 0) (adcm31 - adcm13 = 0) (adcm21 - adcm12 = 0), mtest
xtreg std_flow adce1* adce2* adce3* adce4* adce51 adce52 adce53 adce54 adce56 adce57 adce58 adce59 adce510 adce6* adce7* adce8* adce9* lglagmtna i.timedate, fe vce(r)
outreg2 using outtable3ce, excel replace
* test (adce109 - adce910 = 0) (adce108 - adce810 = 0) (adce107 - adce710 = 0) (adce106 - adce610 = 0) (adce105 - adce510 = 0) (adce104 - adce410 = 0) (adce103 - adce310 = 0) (adce102 - adce210 = 0) (adce101 - adce110 = 0) (adce98 - adce89 = 0) (adce97 - adce79 = 0) (adce96 - adce69 = 0) (adce95 - adce59 = 0) (adce94 - adce49 = 0) (adce93 - adce39 = 0) (adce92 - adce29 = 0) (adce91 - adce19 = 0) (adce87 - adce78 = 0) (adce86 - adce68 = 0) (adce85 - adce58 = 0) (adce84 - adce48 = 0) (adce83 - adce38 = 0) (adce82 - adce28 = 0) (adce81 - adce18 = 0) (adce76 - adce67 = 0) (adce75 - adce57 = 0) (adce74 - adce47 = 0) (adce73 - adce37 = 0) (adce72 - adce27 = 0) (adce71 - adce17 = 0) (adce65 - adce56 = 0) (adce64 - adce46 = 0) (adce63 - adce36 = 0) (adce62 - adce26 = 0) (adce61 - adce16 = 0) (adce54 - adce45 = 0) (adce53 - adce35 = 0) (adce52 - adce25 = 0) (adce51 - adce15 = 0) (adce43 - adce34 = 0) (adce42 - adce24 = 0) (adce41 - adce14 = 0) (adce32 - adce23 = 0) (adce31 - adce13 = 0) (adce21 - adce12 = 0), mtest
