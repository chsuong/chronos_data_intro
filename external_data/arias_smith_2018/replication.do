capture log close
clear all

cap cd "D:\Dropbox\AriasSmith\dofiles\replication\"
cap cd "C:\Users\EricArias\Dropbox\AriasSmith\dofiles\replication\"

log using replication.log, replace

version 12

*#delimit ;
set more off

set seed 1234567891


*************************************************************************;
* DATA IN
*************************************************************************;

use ambassadors.dta , clear

*************************************************************************;
* EDITING
*************************************************************************;

g logt = ln(_t)



*************************************************************************;
* TABLE 1
*************************************************************************;

by politicalappointee, sort: stdes
bysort politicalappointee: sum joborder careeryrs if lastjob==1 & (fail==1 | endobs==mdy(12,31,2014))
bysort politicalappointee: sum joborder careeryrs if lastjob==1 & (fail==1 | endobs==mdy(12,31,2014)) ,d
tab politicalappointee if died==1 & fail==1



*************************************************************************;
* FIGURE 1
*************************************************************************;

sort ccode year
by ccode year: gen count = 1 if _n==1
sort year
by year: egen COUNT = sum(count)

#delimit ;
twoway scatter COUNT year if year<2014,
title("Number of Ambassadors Per Year") ytitle("Number of Ambassadors") 
xtitle("Year") scheme(s2mono) graphregion(fcolor(white)) ;
#delimit cr
window manage close graph 



*************************************************************************;
* TABLE 2
*************************************************************************;

eststo clear

*MODEL 1
set more off
#delimit ;
stcox USturnover FOREIGNturnover ambage female careeryrs demaut yr yr2
if died==0 & politicalappointee==0 ,
tvc(ambage) texp(ln(_t)) cluster(ambid)  nohr ;
#delimit cr
sum year if e(sample)
eststo bigsample_tvc0 

*MODEL 2
#delimit ;
stcox USturnover FOREIGNturnover ambage female careeryrs demaut yr yr2
if died==0 & politicalappointee==1 ,
tvc(USturnover careeryrs) texp(ln(_t)) cluster(ambid)  nohr ;
#delimit cr
sum year if e(sample)
eststo bigsample_tvc1 

*MODEL 3
set more off
#delimit ;
stcox USturnover FOREIGNturnover ambage female careeryrs demaut yr yr2
if died==0 & politicalappointee==0 ,
tvc(ambage careeryrs) texp(ln(_t)) cluster(ambid)  str(joborder) nohr ;
#delimit cr
sum year if e(sample)
eststo bigsample_strata0

*MODEL 4 (conditional on years because wouldn't converge)
set more off
#delimit ;
stcox USturnover FOREIGNturnover ambage female careeryrs demaut yr yr2
if died==0 & politicalappointee==1 & year>1808 ,
tvc(USturnover) texp(ln(_t)) cluster(ambid) str(joborder) nohr ;
#delimit cr
sum year if e(sample)
eststo bigsample_strata1

#delimit ;
esttab 
bigsample_tvc0 bigsample_tvc1 bigsample_strata0 bigsample_strata1
using table2.tex, label 
title("\textbf{Cox Proportional Hazards Estimates: U.S. Ambassadors' Tenure (1800-2013)}")
nogaps compress b(%12.3fc) se star(* 0.10 ** 0.05 *** 0.01) 
stats(N N_sub N_fail , fmt(%18.0g %18.0g %18.0g) 
labels(`"Observations"' `"\# of subjects"' `"\# of failures"')) 
replace ;
#delimit cr


*************************************************************************;
* TABLE 3
*************************************************************************;

eststo clear

*MODEL 1
set more off
#delimit ;
stcox USturnover FOREIGNturnover ambage female careeryrs
demaut logpop loggdp trade s3un tau_glob ally 
newmid Xgrowth ds3un dtau_glob bit pta partnerfiles usfiles yr yr2
if died==0 & politicalappointee==0 ,
tvc(loggdp trade s3un newmid ds3un) texp(ln(_t)) cluster(ambid)  nohr ;
#delimit cr
sum year if e(sample)
eststo tvc0 

*MODEL 2
#delimit ;
stcox USturnover FOREIGNturnover ambage female careeryrs
demaut logpop loggdp trade s3un tau_glob ally    
newmid Xgrowth ds3un dtau_glob bit pta partnerfiles usfiles yr yr2
if died==0 & politicalappointee==1 ,
tvc(USturnover careeryrs demaut s3un) texp(ln(_t)) cluster(ambid)  nohr ;
#delimit cr
sum year if e(sample)
eststo tvc1 

*MODEL 3
set more off
#delimit ;
stcox USturnover FOREIGNturnover ambage female careeryrs
demaut logpop loggdp trade s3un tau_glob ally 
performanceMU yr yr2 
if died==0 & politicalappointee==0 ,
tvc(USturnover) texp(ln(_t)) cluster(ambid)  nohr ;
#delimit cr
sum year if e(sample)
eststo std2_0 

*MODEL 4
#delimit ;
stcox USturnover FOREIGNturnover ambage female careeryrs
demaut logpop loggdp trade s3un tau_glob ally 
performanceMU yr yr2
if died==0 & politicalappointee==1 ,
tvc(USturnover careeryrs demaut logpop s3un tau_glob ally) texp(ln(_t))
cluster(ambid)  nohr ;
#delimit cr
sum year if e(sample)
eststo std2_1 

*MODEL 5
set more off
#delimit ;
stcox USturnover FOREIGNturnover ambage female careeryrs
demaut logpop loggdp trade s3un tau_glob ally 
good bad yr yr2
if died==0 & politicalappointee==0 , tvc(USturnover) texp(ln(_t))
cluster(ambid)  nohr ;
#delimit cr
sum year if e(sample)
eststo std3_0

*MODEL 6
#delimit ;
stcox USturnover FOREIGNturnover ambage female careeryrs
demaut logpop loggdp trade s3un tau_glob ally 
good bad yr yr2
if died==0 & politicalappointee==1 ,
tvc(USturnover ambage careeryrs s3un tau_glob ally) texp(ln(_t))
cluster(ambid)  nohr ;
#delimit cr
sum year if e(sample)
eststo std3_1

#delimit ;
esttab 
tvc0 tvc1 std2_0 std2_1 std3_0 std3_1
using table3.tex, label 
title("\textbf{Cox Proportional Hazards Estimates: U.S. Ambassadors' Tenure (1950-2000)}")
nogaps compress b(%12.3fc) se star(* 0.10 ** 0.05 *** 0.01) 
stats(N N_sub N_fail , fmt(%18.0g %18.0g %18.0g) 
labels(`"Observations"' `"\# of subjects"' `"\# of failures"')) 
replace ;
#delimit cr

*************************************************************************;
* FIGURE 2
*************************************************************************;
bysort year: egen politicalshare = mean(politicalappointee)
gen careershare = 1 - politicalshare
label var careershare "Share of Career Appointees"

bysort year: egen meantenure2 = mean(_t)
label var meantenure2 "Average Tenure"

#delimit ;
graph twoway 
(scatter meantenure2 year, msymbol(th) ylabel(0(5)15))
(scatter careershare year, msymbol(Oh) yaxis(2) ylabel(0(.2)1,axis(2)) ) ,
	title(Average Tenure and Proportion of Career Appointees)
	ytitle(Average Tenure) 
	xtitle(Year) scheme(s2mono) graphregion(fcolor(white))
  legend(label(1 Average Tenure) label(2 Share of Career Appointees));
#delimit cr
window manage close graph 


*************************************************************************;
* TABLE 4
*************************************************************************;

tab jobcareer2 anotherjob if politicalappointe==0
tab jobcareer2 anotherjob if politicalappointe==1

*************************************************************************;
* TABLE 5
*************************************************************************;

set more off

eststo clear

#delimit ;
logit anotherjob nationrank ambage careeryrs female demaut  
sumDeltaUN sumDeltaTAUG maxnewmid 
avgDeltaTRADE maxBIT maxPTA sumPartnerfiles sumUSfiles yr yr2
if politicalappointe == 0 & fail == 1 ; 
eststo a1;
#delimit cr
sum year if e(sample)

#delimit ;
logit anotherjob nationrank ambage careeryrs female demaut  
sumDeltaUN sumDeltaTAUG maxnewmid 
avgDeltaTRADE maxBIT maxPTA sumPartnerfiles sumUSfiles yr yr2
if politicalappointe == 1 & fail == 1 ;
eststo a2;
#delimit cr
sum year if e(sample)

#delimit ;
logit anotherjob nationrank ambage careeryrs female demaut  
avgPerformanceMU yr yr2
if politicalappointe == 0 & fail == 1 ;
eststo a3;
#delimit cr
sum year if e(sample)

#delimit ;
logit anotherjob nationrank ambage careeryrs female demaut  
avgPerformanceMU yr yr2
if politicalappointe == 1 & fail == 1 ;
eststo a4;
#delimit cr
sum year if e(sample)

#delimit ;
logit anotherjob nationrank ambage careeryrs female demaut  
avgGood avgBad yr yr2
if politicalappointe == 0 & fail == 1 ;
eststo a5;
#delimit cr
sum year if e(sample)

#delimit ;
logit anotherjob nationrank ambage careeryrs female demaut  
avgGood avgBad yr yr2
if politicalappointe == 1 & fail == 1 ;
eststo a6;
#delimit cr
sum year if e(sample)


#delimit ;
esttab a1 a2 a3 a4 a5 a6 using table5.tex, label 
title("\textbf{Logit Model: Are Ambassadors Appointed to Another Ambassadorial Position? (1950-2011)}")
nogaps compress b(%12.3fc) se star(* 0.10 ** 0.05 *** 0.01) 
stats(N ll , fmt(%18.0g %12.2f) labels(`"Observations"' `"Log-Likelihood"')) 
replace ;
#delimit cr

*************************************************************************;
* TABLE 6
*************************************************************************;

eststo clear

#delimit ;
mlogit jobcareer2 nationrank ambage careeryrs female demaut  
avgPerformanceMU yr yr2
if politicalappointe == 0 & fail == 1 , baseoutcome(-2) ;
eststo m3;
#delimit cr
sum year if e(sample)

#delimit ;
mlogit jobcareer2 nationrank ambage careeryrs female demaut  
avgPerformanceMU yr yr2
if politicalappointe == 1 & fail == 1 , baseoutcome(-2) ;
eststo m4;
#delimit cr
sum year if e(sample)

#delimit ;
mlogit jobcareer2 nationrank ambage careeryrs female demaut  
avgGood avgBad yr yr2
if politicalappointe == 0 & fail == 1 , baseoutcome(-2) ;
eststo m5;
#delimit cr
sum year if e(sample)

#delimit ;
mlogit jobcareer2 nationrank ambage careeryrs female demaut  
avgGood avgBad yr yr2
if politicalappointe == 1 & fail == 1 , baseoutcome(-2) ;
eststo m6;
#delimit cr
sum year if e(sample)


#delimit ;
esttab m3 m4 m5 m6 using table6.tex, label 
title("\textbf{Multinomial Logit: Promotion, Demotion, Similar Job or Retirement (1950-2011)}")
nogaps compress b(%12.3fc) se star(* 0.10 ** 0.05 *** 0.01) 
stats(N ll , fmt(%18.0g %12.2f) labels(`"Observations"' `"Log-Likelihood"')) 
replace ;
#delimit cr


*************************************************************************;
* TABLE A1 summary
*************************************************************************;

*USING userwritten ado "sutex"
#delimit ;
sutex politicalappointee USturnover USPartyurnover FOREIGNturnover
ambage female careeryrs distip
demaut logpop loggdppc trade s3un tau_glob ally 
newmid Xgrowth ds3un dtau_glob bit pta partnerfiles 
usfiles performanceMU good bad 
, minmax title(\textbf{Summary Statistics}) labels ;
#delimit cr



*************************************************************************;
* TABLE A2
*************************************************************************;

set more off
eststo clear


#delimit ;
stcox USturnover FOREIGNturnover distip
ambage female careeryrs demaut logpop loggdp trade s3un ally 
Xgrowth ds3un bit  partnerfiles usfiles
if died==0 & politicalappointee==0 ,
tvc(ambage) texp(ln(_t)) cluster(ambid)  nohr ;
eststo distip_tvc0 ;
#delimit cr
sum year if e(sample)

#delimit ;
stcox USturnover FOREIGNturnover distip
ambage female careeryrs demaut logpop loggdp trade s3un ally 
Xgrowth ds3un bit pta partnerfiles usfiles
if died==0 & politicalappointee==1 ,
tvc(distip Xgrowth ds3un) texp(ln(_t)) cluster(ambid) nohr ;
eststo distip_tvc1 ;
#delimit cr
sum year if e(sample)

set more off
#delimit ;
stcox USturnover FOREIGNturnover distip
ambage female careeryrs demaut logpop loggdp trade s3un ally 
performanceMU
if died==0 & politicalappointee==0 ,
tvc(performanceMU) texp(ln(_t)) cluster(ambid)  nohr ;
eststo distip2_tvc0 ;
#delimit cr
sum year if e(sample)

#delimit ;
stcox USturnover FOREIGNturnover distip
ambage female careeryrs demaut logpop loggdp trade s3un ally 
performanceMU
if died==0 & politicalappointee==1 ,
tvc(USturnover distip ambage careeryrs s3un) texp(ln(_t))
cluster(ambid)  nohr ;
eststo distip2_tvc1 ;
#delimit cr
sum year if e(sample)

#delimit ;
stcox USturnover FOREIGNturnover distip
ambage female careeryrs demaut logpop loggdp trade s3un ally 
good bad
if died==0 & politicalappointee==0 ,
tvc(good bad) texp(ln(_t)) cluster(ambid)  nohr ;
eststo distip3_tvc0 ;
#delimit cr
sum year if e(sample)

#delimit ;
stcox USturnover FOREIGNturnover distip
ambage female careeryrs demaut logpop loggdp trade s3un ally 
good bad
if died==0 & politicalappointee==1 ,
tvc(USturnover distip ambage careeryrs s3un bad) texp(ln(_t))
cluster(ambid)  nohr ;
eststo distip3_tvc1 ;
#delimit cr
sum year if e(sample)

#delimit ;
esttab 
distip_tvc0 distip_tvc1 
distip2_tvc0 distip2_tvc1
distip3_tvc0 distip3_tvc1
using tableA2.tex, label 
title("\textbf{Cox Proportional Hazards Estimates: U.S. Ambassadors' Tenure (1987-2009)}")
nogaps compress b(%12.3fc) se star(* 0.10 ** 0.05 *** 0.01) 
stats(N N_sub N_fail , fmt(%18.0g %18.0g %18.0g) 
labels(`"Observations"' `"\# of subjects"' `"\# of failures"')) replace ;
#delimit cr
eststo clear



*************************************************************************;
* TABLE A3
*************************************************************************;

eststo clear

set more off
#delimit ;
stcox USPartyurnover FOREIGNturnover ambage female careeryrs
demaut logpop loggdp trade s3un tau_glob ally performanceMU yr yr2
if died==0 & politicalappointee==0 ,
tvc(USPartyurnover) texp(ln(_t)) cluster(ambid) nohr ;
eststo party0 ;
#delimit cr
sum year if e(sample)

#delimit ;
stcox USPartyurnover FOREIGNturnover ambage female careeryrs
demaut logpop loggdp trade s3un tau_glob ally performanceMU yr yr2
if died==0 & politicalappointee==1 ,
tvc(USPartyurnover careeryrs demaut logpop trade) texp(ln(_t))
cluster(ambid) nohr ;
eststo party1 ;
#delimit cr
sum year if e(sample)

#delimit ;
esttab party0 party1 using tableA3.tex, label 
title("\textbf{Cox Proportional Hazards Estimates: US Partisan Turnover (1950-2000)}")
nogaps compress b(%12.3fc) se star(* 0.10 ** 0.05 *** 0.01) 
stats(N N_sub N_fail , fmt(%18.0g %18.0g %18.0g) 
labels(`"Observations"' `"\# of subjects"' `"\# of failures"')) 
replace ;
#delimit cr


*************************************************************************;
* TABLE A4
*************************************************************************;

set more off
eststo clear

#delimit ;
stcox USturnover FOREIGNturnover ambage female careeryrs
demaut logpop loggdp trade s3un tau_glob ally performanceMU yr yr2
if died==0 & politicalappointee==0 & g20==0,
tvc(USturnover careeryrs) texp(ln(_t)) cluster(ambid)  nohr ;
eststo g201 ;
#delimit cr
sum year if e(sample)

#delimit ;
stcox USturnover FOREIGNturnover ambage female careeryrs
demaut logpop loggdp trade s3un tau_glob ally performanceMU yr yr2
if died==0 & politicalappointee==1  & g20==0,
tvc(USturnover careeryrs logpop performanceMU) texp(ln(_t))
cluster(ambid)  nohr ; 
eststo g202 ;
#delimit cr
sum year if e(sample)

set more off
#delimit ;
stcox USturnover FOREIGNturnover ambage female careeryrs
demaut logpop loggdp trade s3un tau_glob ally performanceMU yr yr2
if died==0 & politicalappointee==0  & g20==1,
tvc(USturnover) texp(ln(_t)) cluster(ambid)  nohr ;
eststo g203 ;
#delimit cr
sum year if e(sample)

#delimit ;
stcox USturnover FOREIGNturnover ambage female careeryrs
demaut logpop loggdp trade s3un tau_glob ally performanceMU yr yr2
if died==0 & politicalappointee==1  & g20==1,
tvc(careeryrs) texp(ln(_t)) cluster(ambid)  nohr ;
eststo g204 ;
#delimit cr
sum year if e(sample)

#delimit ;
esttab g201 g202 g203 g204 using tableA4.tex, label 
title("\textbf{Cox Proportional Hazards Estimates: Different Sample of Countries (1950-2000)}")
nogaps compress b(%12.3fc) se star(* 0.10 ** 0.05 *** 0.01) 
stats(N N_sub N_fail , fmt(%18.0g %18.0g %18.0g) 
labels(`"Observations"' `"\# of subjects"' `"\# of failures"')) 
replace ;
#delimit cr



*************************************************************************;
* TABLE A5
*************************************************************************;

eststo clear

#delimit ;
stcox USturnover FOREIGNturnover ambage female careeryrs
demaut logpop loggdp trade s3un tau_glob ally performanceMU yr yr2
if died==0 & position!="Charg" & politicalappointee==0 ,
tvc(USturnover loggdp trade s3un newmid ds3un) texp(ln(_t))
cluster(ambid)  nohr ; eststo pos1 ;
#delimit cr
sum year if e(sample)

#delimit ;
stcox USturnover FOREIGNturnover ambage female careeryrs
demaut logpop loggdp trade s3un tau_glob ally performanceMU yr yr2
if died==0 & position!="Charg" & politicalappointee==1 ,
tvc(USturnover careeryrs demaut logpop trade) texp(ln(_t))
cluster(ambid)  nohr ; eststo pos2 ;
#delimit cr
sum year if e(sample)

#delimit ;
stcox USturnover FOREIGNturnover ambage female careeryrs
demaut logpop loggdp trade s3un tau_glob ally performanceMU yr yr2
if died==0 & position=="Ambassador Extraordinary and Plenipotentiary" & politicalappointee==0 ,
tvc(USturnover loggdp trade s3un newmid ds3un) texp(ln(_t))
cluster(ambid)  nohr ; eststo pos3 ;
#delimit cr
sum year if e(sample)

#delimit ;
stcox USturnover FOREIGNturnover ambage female careeryrs
demaut logpop loggdp trade s3un tau_glob ally performanceMU yr yr2
if died==0 & position=="Ambassador Extraordinary and Plenipotentiary" & politicalappointee==1 ,
tvc(USturnover careeryrs demaut logpop trade) texp(ln(_t))
cluster(ambid)  nohr ; eststo pos4 ;
#delimit cr
sum year if e(sample)

#delimit ;
esttab pos1 pos2 pos3 pos4 using tableA5.tex, label 
title("\textbf{Cox Proportional Hazards Estimates: Excluding Positions (1950-2000)}")
nogaps compress b(%12.3fc) se star(* 0.10 ** 0.05 *** 0.01) 
stats(N N_sub N_fail , fmt(%18.0g %18.0g %18.0g) 
labels(`"Observations"' `"\# of subjects"' `"\# of failures"')) 
replace ;
#delimit cr

*************************************************************************;
* TABLE A6
*************************************************************************;

set more off
eststo clear

#delimit ;
stcox USturnover FOREIGNturnover ambage female careeryrs
demaut yr yr2 if died==0 & politicalappointee==0 & year>=1924,
tvc(USturnover) texp(ln(_t)) cluster(ambid)  nohr ; eststo year1 ;
#delimit cr
sum year if e(sample)

#delimit ;
stcox USturnover FOREIGNturnover ambage female careeryrs
demaut yr yr2 if died==0 & politicalappointee==1 & year>=1924 ,
tvc(USturnover careeryrs demaut logpop trade) texp(ln(_t))
cluster(ambid)  nohr ; eststo year2 ;
#delimit cr
sum year if e(sample)

#delimit ;
stcox USturnover FOREIGNturnover ambage female careeryrs
demaut logpop loggdp trade s3un tau_glob ally performanceMU yr yr2
if died==0 & politicalappointee==0 & year>=1980,
tvc(USturnover) texp(ln(_t)) cluster(ambid)  nohr ; eststo year3 ;
#delimit cr
sum year if e(sample)

#delimit ;
stcox USturnover FOREIGNturnover ambage female careeryrs
demaut logpop loggdp trade s3un tau_glob ally performanceMU yr yr2
if died==0 & politicalappointee==1 & year>=1980 ,
tvc(USturnover careeryrs demaut logpop trade) texp(ln(_t))
cluster(ambid)  nohr ; eststo year4 ;
#delimit cr
sum year if e(sample)

#delimit ;
esttab year1 year2 year3 year4 using tableA6.tex, label 
title("\textbf{Cox Proportional Hazards Estimates: Different Time Periods}")
nogaps compress b(%12.3fc) se star(* 0.10 ** 0.05 *** 0.01) 
stats(N N_sub N_fail , 
fmt(%18.0g %18.0g %18.0g) 
labels(`"Observations"' `"\# of subjects"' `"\# of failures"')) 
replace ;
#delimit cr


*************************************************************************;
* TABLE A7
*************************************************************************;
/*TAKES SEVERAL HOURS TO RUN
eststo clear

set more off

#delimit ;
stcox  USturnover FOREIGNturnover ambage female careeryrs
demaut logpop loggdp trade s3un tau_glob ally 
newmid Xgrowth ds3un dtau_glob bit pta partnerfiles usfiles
yr yr2 if died==0 & politicalappointee==0 ,
tvc(trade s3un newmid) texp(ln(_t)) shared(ccode) forceshared nohr ;
eststo smallsample_sharedccode1 ;
#delimit cr
sum year if e(sample)

#delimit ;
stcox  USturnover FOREIGNturnover ambage female careeryrs
demaut logpop loggdp trade s3un tau_glob ally 
newmid Xgrowth ds3un dtau_glob bit pta partnerfiles usfiles
yr yr2 if died==0 & politicalappointee==1 ,
tvc(USturnover tau_glob ally) texp(ln(_t)) shared(ccode) forceshared nohr ;
eststo smallsample_sharedccode2 ;
#delimit cr
sum year if e(sample)

#delimit ;
stcox  USturnover FOREIGNturnover ambage female careeryrs
demaut logpop loggdp trade s3un tau_glob ally 
performanceMU yr yr2 if died==0 & politicalappointee==0 ,
tvc(s3un) texp(ln(_t)) shared(ccode) forceshared  nohr ; 
eststo smallsample_sharedccode3 ;
#delimit cr
sum year if e(sample)

#delimit ;
stcox  USturnover FOREIGNturnover ambage female careeryrs
demaut logpop loggdp trade s3un tau_glob ally 
performanceMU yr yr2 if died==0 & politicalappointee==1 ,
tvc(USturnover tau_glob ally) texp(ln(_t)) shared(ccode) forceshared nohr ; 
eststo smallsample_sharedccode4 ;
#delimit cr
sum year if e(sample)

#delimit ;
stcox  USturnover FOREIGNturnover ambage female careeryrs
demaut logpop loggdp trade s3un tau_glob ally 
good bad yr yr2 if died==0 & politicalappointee==0 ,
tvc(s3un) texp(ln(_t)) shared(ccode) forceshared  nohr ; 
eststo smallsample_sharedccode5 ;
#delimit cr
sum year if e(sample)

#delimit ;
stcox  USturnover FOREIGNturnover ambage female careeryrs
demaut logpop loggdp trade s3un tau_glob ally 
good bad yr yr2 if died==0 & politicalappointee==1 ,
tvc(USturnover tau_glob ally) texp(ln(_t)) shared(ccode) forceshared nohr ; 
eststo smallsample_sharedccode6 ;
#delimit cr
sum year if e(sample)

#delimit ;
esttab 
smallsample_sharedccode1 smallsample_sharedccode2 smallsample_sharedccode3
smallsample_sharedccode4 smallsample_sharedccode5 smallsample_sharedccode6
using tablleA7.tex, label 
title("\textbf{Cox Shared Frailty Hazards Estimates: U.S. Ambassadors' Tenure (1950-2000)}")
nogaps compress b(%12.3fc) se star(* 0.10 ** 0.05 *** 0.01) 
stats(N N_sub N_fail , fmt(%18.0g %18.0g %18.0g) 
labels(`"Observations"' `"\# of subjects"' `"\# of failures"')) replace ;
#delimit cr
*/

*************************************************************************;
* TABLE A8
*************************************************************************;

set more off
eststo clear

#delimit ;
streg USturnover FOREIGNturnover politicalappointee
ambage female careeryrs demaut logpop loggdp trade s3un tau_glob ally 
newmid Xgrowth ds3un dtau_glob bit pta partnerfiles usfiles yr yr2
if died==0 , cluster(ambid) d(wei) anc(politicalappointee) ;eststo ic2;
#delimit cr
estat ic
sum year if e(sample)

#delimit ;
streg USturnover FOREIGNturnover politicalappointee
ambage female careeryrs demaut logpop loggdp trade s3un tau_glob ally 
newmid Xgrowth ds3un dtau_glob bit pta partnerfiles usfiles yr yr2
if died==0 , cluster(ambid) d(lognormal) anc(politicalappointee) ; eststo ic3;
#delimit cr
estat ic
sum year if e(sample)

#delimit ;
streg USturnover FOREIGNturnover politicalappointee
ambage female careeryrs demaut logpop loggdp trade s3un tau_glob ally 
newmid Xgrowth ds3un dtau_glob bit pta partnerfiles usfiles yr yr2
if died==0 , cluster(ambid) d(loglogistic) anc(politicalappointee) ; eststo ic4;
#delimit cr
estat ic
sum year if e(sample)

#delimit ;
streg USturnover FOREIGNturnover politicalappointee
ambage female careeryrs demaut logpop loggdp trade s3un tau_glob ally 
newmid Xgrowth ds3un dtau_glob bit pta partnerfiles usfiles yr yr2
if died==0 , cluster(ambid) d(exponential)  ; eststo ic5;
#delimit cr
estat ic
sum year if e(sample)

#delimit ;
esttab ic2 ic3 ic4 ic5 using tableA8.tex, label 
title("\textbf{Different Survival Estimations: Ambassadorial Tenure (1950-2000)}")
nogaps compress b(%12.3fc) se star(* 0.10 ** 0.05 *** 0.01) 
stats(N N_sub N_fail , 
fmt(%18.0g %18.0g %18.0g) 
labels(`"Observations"' `"\# of subjects"' `"\# of failures"')) 
replace ;
#delimit cr



*************************************************************************;
* TABLE A9
*************************************************************************;

set more off
eststo clear

#delimit ;
streg USturnover FOREIGNturnover
ambage female careeryrs demaut logpop loggdp trade s3un tau_glob ally 
newmid Xgrowth ds3un dtau_glob bit pta partnerfiles usfiles 
yr yr2 if died==0 & politicalappointee==0 , cluster(ambid) d(wei) nohr;eststo w1;
#delimit cr
sum year if e(sample)

#delimit ;
streg USturnover FOREIGNturnover
ambage female careeryrs demaut logpop loggdp trade s3un tau_glob ally 
newmid Xgrowth ds3un dtau_glob bit pta partnerfiles usfiles 
yr yr2 if died==0 & politicalappointee==1 , cluster(ambid) d(wei) nohr;eststo w2;
#delimit cr
sum year if e(sample)

#delimit ;
streg USturnover FOREIGNturnover
ambage female careeryrs demaut logpop loggdp trade s3un tau_glob ally 
performanceMU
yr yr2 if died==0 & politicalappointee==0 ,cluster(ambid) d(wei) nohr;eststo w3;
#delimit cr
sum year if e(sample)

#delimit ;
streg USturnover FOREIGNturnover
ambage female careeryrs demaut logpop loggdp trade s3un tau_glob ally 
performanceMU
yr yr2 if died==0 & politicalappointee==1 ,cluster(ambid) d(wei) nohr;eststo w4;
#delimit cr
sum year if e(sample)

#delimit ;
streg USturnover FOREIGNturnover
ambage female careeryrs demaut logpop loggdp trade s3un tau_glob ally 
good bad yr yr2 if died==0 & politicalappointee==0,cluster(ambid) d(wei) nohr;eststo w5;
#delimit cr
sum year if e(sample)

#delimit ;
streg USturnover FOREIGNturnover
ambage female careeryrs demaut logpop loggdp trade s3un tau_glob ally 
good bad yr yr2 if died==0 & politicalappointee==1,cluster(ambid) d(wei) nohr;eststo w6;
#delimit cr
sum year if e(sample)

#delimit ;
esttab w1 w2 w3 w4 w5 w6 using tableA9.tex, label 
title("\textbf{Weibull Survival Estimates: Ambassadorial Tenure (1950-2000)}")
nogaps compress b(%12.3fc) se star(* 0.10 ** 0.05 *** 0.01) 
stats(N N_sub N_fail , fmt(%18.0g %18.0g %18.0g) 
labels(`"Observations"' `"\# of subjects"' `"\# of failures"')) replace ;
#delimit cr

*************************************************************************;
* TABLE A10
*************************************************************************;

set more off
eststo clear

#delimit ;
areg fail c.USturnover#c.politicalappointee c.FOREIGNturnover#c.politicalappointee
ambage careeryrs demaut logpop loggdp trade s3un tau_glob ally 
newmid Xgrowth ds3un dtau_glob bit pta partnerfiles usfiles
i.year if died==0 , cluster(ambid) a(ambid) ; eststo ols1;
#delimit cr
sum year if e(sample)

#delimit ;
areg fail c.USturnover#c.politicalappointee c.FOREIGNturnover#c.politicalappointee
ambage careeryrs demaut logpop loggdp trade s3un tau_glob ally 
performanceMU c.performanceMU#c.politicalappointee
 i.year if died==0 , cluster(ambid) a(ambid) ; eststo ols2;
#delimit cr
sum year if e(sample)

#delimit ;
areg fail c.USturnover#c.politicalappointee c.FOREIGNturnover#c.politicalappointee
ambage careeryrs demaut logpop loggdp trade s3un tau_glob ally 
good c.good#c.politicalappointee bad c.bad#c.politicalappointee
i.year if died==0 , cluster(ambid) a(ambid) ; eststo ols3;
#delimit cr
sum year if e(sample)

#delimit ;
areg fail c.USturnover#c.politicalappointee c.FOREIGNturnover#c.politicalappointee
ambage careeryrs demaut logpop loggdp trade s3un tau_glob ally 
good c.good#c.politicalappointee bad c.bad#c.politicalappointee
i.year i.ccode if died==0 , cluster(ambid) a(ambid) ; eststo ols4;
#delimit cr
sum year if e(sample)


#delimit ;
esttab ols1 ols2 ols3 ols4 using tableA10.tex, label 
title("\textbf{OLS Estimates - Ambassadorial Tenure (1950-2000)}")
nogaps compress b(%12.3fc) se star(* 0.10 ** 0.05 *** 0.01) 
stats(N N_clust r2 , fmt(%18.0g %18.0g %12.2fc) 
labels(`"Observations"' `"Ambassadors"' `"R-squared"')) replace ;
#delimit cr



*************************************************************************;
* TABLE A11
*************************************************************************;

#delimit ;
qui logit anotherjob nationrank ambage careeryrs female demaut  
avgPerformanceMU yr yr2 if fail == 1 ;
#delimit cr
g sample = 1 if e(sample)

#delimit ;
sutex anotherjob nationrank ambage careeryrs female demaut  
avgPerformanceMU avgGood avgBad
sumDeltaUN sumDeltaTAUG maxnewmid 
avgDeltaTRADE maxBIT maxPTA sumPartnerfiles sumUSfiles
if sample == 1 , 
minmax title(\textbf{Summary Statistics: Career Considerations}) labels ;
#delimit cr



*************************************************************************;
* TABLE A12
*************************************************************************;

set more off
eststo clear

#delimit ;
logit anotherjob nationrank ambage careeryrs female demaut  
avgPerformanceMU yr yr2
if politicalappointe == 0 & fail == 1 ;
eststo a3;
#delimit cr
sum year if e(sample)

#delimit ;
logit anotherjob nationrank ambage careeryrs female demaut  
avgPerformanceMU yr yr2
if politicalappointe == 1 & fail == 1 ;
eststo a4;
#delimit cr
sum year if e(sample)

#delimit ;
logit anotherjob nationrank ambage careeryrs female demaut  
avgPerformanceMU yr yr2
if politicalappointe == 0 & fail == 1 & year>=1980 ;
eststo a5;
#delimit cr
sum year if e(sample)

#delimit ;
logit anotherjob nationrank ambage careeryrs female demaut  
avgPerformanceMU yr yr2
if politicalappointe == 1 & fail == 1 & year>=1980 ;
eststo a6;
#delimit cr
sum year if e(sample)


#delimit ;
esttab a3 a4 a5 a6 using tableA12.tex, label 
title("\textbf{Logit Model: Are Ambassadors Appointed to Another Ambassadorial Position?}")
nogaps compress b(%12.3fc) se star(* 0.10 ** 0.05 *** 0.01) 
stats(N ll , fmt(%18.0g %12.2f) labels(`"Observations"' `"Log-Likelihood"')) 
replace ;
#delimit cr




*************************************************************************;
* TABLES A13-A15
*************************************************************************;

eststo clear


#delimit ;
mlogit jobcareer2 nationrank ambage careeryrs female demaut  
sumDeltaUN sumDeltaTAUG maxnewmid 
avgDeltaTRADE maxBIT maxPTA sumPartnerfiles sumUSfiles yr yr2
if politicalappointe == 0 & fail == 1 , baseoutcome(-2) ; eststo m1;
#delimit cr
sum year if e(sample)

#delimit ;
mlogit jobcareer2 nationrank ambage careeryrs female demaut  
sumDeltaUN sumDeltaTAUG maxnewmid 
avgDeltaTRADE maxBIT maxPTA sumPartnerfiles sumUSfiles yr yr2
if politicalappointe == 1 & fail == 1 , baseoutcome(-2) ; eststo m2;
#delimit cr
sum year if e(sample)

#delimit ;
mlogit jobcareer2 nationrank ambage careeryrs female demaut  
avgPerformanceMU yr yr2
if politicalappointe == 0 & fail == 1 , baseoutcome(-2) ; eststo m3;
#delimit cr
sum year if e(sample)

#delimit ;
mlogit jobcareer2 nationrank ambage careeryrs female demaut  
avgPerformanceMU yr yr2
if politicalappointe == 1 & fail == 1 , baseoutcome(-2) ; eststo m4;
#delimit cr
sum year if e(sample)

#delimit ;
mlogit jobcareer2 nationrank ambage careeryrs female demaut  
avgGood avgBad yr yr2
if politicalappointe == 0 & fail == 1 , baseoutcome(-2) ; eststo m5;
#delimit cr
sum year if e(sample)

#delimit ;
mlogit jobcareer2 nationrank ambage careeryrs female demaut  
avgGood avgBad yr yr2
if politicalappointe == 1 & fail == 1 , baseoutcome(-2) ; eststo m6;
#delimit cr
sum year if e(sample)


#delimit ;
esttab m1 m2 m3 m4 m5 m6 using tableA13-15.tex, label 
title("\textbf{Multinomial Logit: Promotion, Demotion, Similar Job or Retirement (1950-2000)}")
nogaps compress b(%12.3fc) se star(* 0.10 ** 0.05 *** 0.01) 
stats(N ll , fmt(%18.0g %12.2f) labels(`"Observations"' `"Log-Likelihood"')) 
replace ;
#delimit cr

*************************************************************************;
* TABLE A16
*************************************************************************;

* JOB CAREER 3 = quality of post

eststo clear

*not shown
#delimit ;
mlogit jobcareer3 nationrank ambage careeryrs female demaut  
sumDeltaUN sumDeltaTAUG maxnewmid 
avgDeltaTRADE maxBIT maxPTA sumPartnerfiles sumUSfiles yr yr2
if politicalappointe == 0 & fail == 1 , baseoutcome(-2) ; eststo m1;
#delimit cr
sum year if e(sample)

*not shown
#delimit ;
mlogit jobcareer3 nationrank ambage careeryrs female demaut  
sumDeltaUN sumDeltaTAUG maxnewmid 
avgDeltaTRADE maxBIT maxPTA sumPartnerfiles sumUSfiles yr yr2
if politicalappointe == 1 & fail == 1 , baseoutcome(-2) ; eststo m2;
#delimit cr
sum year if e(sample)

#delimit ;
mlogit jobcareer3 nationrank ambage careeryrs female demaut  
avgPerformanceMU yr yr2
if politicalappointe == 0 & fail == 1 , baseoutcome(-2) ; eststo m3;
#delimit cr
sum year if e(sample)

#delimit ;
mlogit jobcareer3 nationrank ambage careeryrs female demaut  
avgPerformanceMU yr yr2
if politicalappointe == 1 & fail == 1 , baseoutcome(-2) ; eststo m4;
#delimit cr
sum year if e(sample)

#delimit ;
mlogit jobcareer3 nationrank ambage careeryrs female demaut  
avgGood avgBad yr yr2
if politicalappointe == 0 & fail == 1 , baseoutcome(-2) ; eststo m5;
#delimit cr
sum year if e(sample)

#delimit ;
mlogit jobcareer3 nationrank ambage careeryrs female demaut  
avgGood avgBad yr yr2
if politicalappointe == 1 & fail == 1 , baseoutcome(-2) ; eststo m6;
#delimit cr
sum year if e(sample)


#delimit ;
esttab m3 m4 m5 m6 using tableA16.tex, label 
title("\textbf{Multinomial Logit: Low, Avg. and High Ranked Posts or Retirement (1950-2000)}")
nogaps compress b(%12.3fc) se star(* 0.10 ** 0.05 *** 0.01) 
stats(N ll , fmt(%18.0g %12.2f) labels(`"Observations"' `"Log-Likelihood"')) 
replace ;
#delimit cr


*************************************************************************;
* TABLE A17
*************************************************************************;

set more off
eststo clear

#delimit ;
mlogit jobcareer2 nationrank ambage careeryrs female demaut  
avgPerformanceMU yr yr2
if politicalappointe == 0 & fail == 1 , baseoutcome(-2) ;
eststo m3;
#delimit cr
sum year if e(sample)

#delimit ;
mlogit jobcareer2 nationrank ambage careeryrs female demaut  
avgPerformanceMU yr yr2
if politicalappointe == 1 & fail == 1 , baseoutcome(-2) ;
eststo m4;
#delimit cr
sum year if e(sample)

#delimit ;
mlogit jobcareer2 nationrank ambage careeryrs female demaut  
avgPerformanceMU yr yr2
if politicalappointe == 0 & fail == 1 & year>=1980 , baseoutcome(-2) ;
eststo m5;
#delimit cr
sum year if e(sample)

#delimit ;
mlogit jobcareer2 nationrank ambage careeryrs female demaut  
avgPerformanceMU yr yr2
if politicalappointe == 1 & fail == 1 & year>=1980 , baseoutcome(-2) ;
eststo m6;
#delimit cr
sum year if e(sample)


#delimit ;
esttab m3 m4 m5 m6 using tableA17.tex, label 
title("\textbf{Multinomial Logit: Promotion, Demotion, Similar Job or Retirement}")
nogaps compress b(%12.3fc) se star(* 0.10 ** 0.05 *** 0.01) 
stats(N ll , fmt(%18.0g %12.2f) labels(`"Observations"' `"Log-Likelihood"')) 
replace ;
#delimit cr


*************************************************************************;
* Closing log
*************************************************************************;
capture log close 
*************************************************************************;
