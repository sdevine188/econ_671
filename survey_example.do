clear
set memory 500m
set matsize 1000
set more off 
set scrollbufsize 300000


/*merge the three files */

 use "C:\Users\prichard\Documents\backupfiles\MEPS\consol2014_0206.dta", clear

 tab year

svyset varpsu [pweight=perwt], strata(varstr)



gen fpl2=0
replace fpl2=1 if nearpoor==1 |lincome ==1



/*label variables*/

label variable usc "usual source of care"
label variable prv "private insurance "
label variable pubcov "public insurance "
label variable unins "uninsurance"
label variable prescins " prescription drug insurance "
label variable nohsdg " No H School"
label variable hs "highschool "
label variable somecoll_ass" SomeCool_ass"
label variable collgr "college Grad"
label variable mastersplus "MastersPl "
label variable yreduc "Years of education" 
label variable emply "Employment status "
label variable unemploy "Unemployed "
label variable selfempl "self_employed"
label variable union "labor union"
label variable daysmissed "Days missed from work "
label variable marry "Married"
label variable div "Divorced"
label variable widow "Widow"
label variable separated "Separated"
label variable nvrmarried "Never married"
label variable famsz "family size"
label variable excleh "excellent health"
label variable vgoodh "very good health"
label variable exclvrgoodh "excellent/very good health"
label variable goodh "good health"
label variable fairh "fair health"
label variable poorh "poor health"
label variable fairp "fair/poor health"
label variable exclehm "excellent mental health"
label variable vgoodhm "good mental health"
label variable exclvrgoodhm "excellent/very good mental h"
label variable goodhm "good mental health"
label variable fairhm "fair mental health"
label variable poorhm "poor health"
label variable fairpm "fair/poor mental h"
label variable exclehsf "current health is excellent" 
label variable vgoodhsf "current health is very good" 
label variable goodhsf "current health is good" 
label variable fairhsf "current health is fair" 
label variable poorhsf "current health is poor" 
label variable fairpsf "current health is fair/poor" 
label variable totexp "Total expenditures"
label variable rxexp "Total pharma expenditures"
label variable opfexp "Outpatient facility expenditures"
label variable opdexp "Outpatient physician expenditures"
label variable erfexp "ER facility expenditures"
label variable erdexp "ER physician expenditures"
label variable ipfexp "Total inpatient expenditures"
label variable female "Female" 
label variable age "Age"
label variable agesq "Age-squared"
label variable black "African-American"
label variable hisp "Hispanic"
label variable asian "Asian"
label variable diabetes "diabetes diagnosis" 
label variable income "family income"
label variable incomesq "family income-squared"
label variable midw "Midwest "
label variable south "South"
label variable west "West"

label variable poor "below poverty "


/*define the different groups */

gen racegroup=0
replace racegroup=1 if white==1
replace racegroup=2 if black==1
replace racegroup=3 if hisp==1
replace racegroup=4 if asian==1 

***********************************

gen selfemplemplpay=0
 replace selfemplemplpay=1 if totslf>0
gen medicaid=0
replace medicaid=1 if totmcd >0

gen medicare=0
replace medicare=1 if totmcr >0
gen totvains=0
replace totvains=1 if totva >0 
gen tricare=0
replace tricare=1 if tottri>0
gen othfed=0
replace othfed=1 if totofd >0

gen otherstate=0
replace  otherstate =1 if totstl >0
gen workcomp=0
replace workcomp=1 if totwcp >0
gen othrpub=0
replace  othrpub=1 if totopu >0
gen othsources=0
replace  othsources=1 if totosr >0


**************************************


replace income=0 if income<=0

gen logincome=log(income+1)

replace mcs42 =. if mcs42 <0

*************************************

gen mdcareinsu=0
replace mdcareinsu=1 if totmcr>0

gen insugroup=.
replace insugroup=1 if priv==1
replace insugroup=2 if pubcov==1
replace insugroup=3 if unins==1
replace insugroup=4 if mdcareinsu==1

*******INCOME GROUP**********************  

gen incgroup=.
replace  incgroup=1 if  poor==1
replace  incgroup=2 if nearpoor==1
replace incgroup=3 if lincome ==1
replace incgroup=4 if  mincome==1
replace incgroup=5 if hincome==1

/*****MODELS FOR DARRELL******/

gen age1834=0
replace age1834=1 if age1824==1 | age2534==1

gen anyexpend=0
replace anyexpend=1 if totexp >0
replace anyexpend=.  if totexp  <0
/*create the different age groups */
 
gen age1839=0
replace age1839=1 if age > 17 & age <40 

gen age4049=0
replace age4049=1 if age >=40 & age <50 

gen age5064=0
replace age5064=1 if age >=50 & age <=64

gen age6585=0
replace age6585=1 if age >=65 & age <=85

gen age85plus=0
replace age85plus=1 if age >85


/*define the sample*/

gen sampleadult=0
replace sampleadult =1 if age>17 
replace sampleadult =.  if age==. 
 
gen mino=1
replace mino=1 if black==1 | hisp==1
 
replace mino=. if white ==1 | asian==1| otherrace==1

gen samplemino=0
replace samplemino =1 if sampleadult ==1 & mino==1
 
/*adjust the expenditures variable for inflation */
replace totexp = . if totexp <0

replace bmi =. if bmi <0 | bmi >82

replace  bmindx53=. if  bmindx53 <0 |  bmindx53 >82

gen bmisq= bmindx53*  bmindx53

gen comorb2sq= comorb2 * comorb2

/* create add var */
gen lessthanhs=0
replace lessthanhs=1 if noschool==1 | elemschool==1 | somehs==1

replace lessthanhs=. if noschool==. | elemschool==. | somehs==. 

gen comorbcat=0
replace comorbcat=0 if comorb2==0
replace comorbcat=1 if comorb2==1
replace comorbcat=2 if comorb2==2 |comorb2==3 | comorb2==4 |comorb2==5 | comorb2==6 | comorb2==7
replace comorbcat=. if comorb2==.

gen nocomorbid=0
replace nocomorbid=1 if comorb2==0
replace nocomorbid=. if comorb2==.

gen comorbid1=0
replace comorbid1=1 if comorb2==1
replace comorbid1=.  if comorb2==.

gen comorb2pl=0
replace comorb2pl=1 if comorb2==2 |comorb2==3 | comorb2==4 |comorb2==5 | comorb2==6 | comorb2==7
replace comorb2pl=.  if comorb2==. 



gen comorbidwo=0
replace comorbidwo=1 if corhrtattack==1| angina==1 | othrhrtdisease==1 | stroke==1

replace comorbidwo=.  if corhrtattack==.| angina==. |mildhrtattack==. |othrhrtdisease  ==. | stroke ==. 

gen hyperten=0
replace hyperten=1 if hibloodpress==1 | multihibloodpress==1 
replace hyperten=. if hibloodpress==.  | multihibloodpress==. 

gen fairpmental=0
replace fairpmental=1 if fairhm==1 | poorhm==1
replace fairpmental=. if fairhm==. | poorhm==.

replace k6sum= . if  k6sum  <0

svy, subpop(sampleadult): mean k6sum




******************************SCREENING**********************

gen paptest=0
replace paptest=1 if papchec3==1 |  papchec1==1 | papchec2==1

gen mamtest =0
replace mamtest=1 if mamochec1==1 | mamochec2==1 |  mamochec3==1

****************************CANCER TYPES******************************

replace cabreast =. if cabreast <0
gen breastc=0
replace breastc=1 if cabreast==1

replace cacervix=. if cacervix <0
gen cervixc=0
replace cervixc=1 if cacervix==1


replace cacolon=. if cacolon <0
gen colonc=0
replace colonc=1 if cacolon==1

replace caprosta=.  if caprosta <0
gen prostac=0
replace prostac=1 if caprosta==1



*********Independent
gen hispender=0
replace hispender=1 if totexp>31130

gen hirxspender=0
replace hirxspender=1 if rxexp>5500

gen asianotherace=0
replace asianotherace=1 if asian==1 | otherrace==1


gen comorbidcond=0
replace comorbidcond=1 if diabetes ==1 |  asthma ==1 |  asthmatcck==1 | hibloodpress==1 |  multihibloodpress==1 | corhrtattack ==1 | angina ==1| mildhrtattack ==1 | othrhrtdisease ==1 |stroke ==1 |emphysema==1 

gen mdicaremcd=0
replace mdicaremcd=1 if medicaid ==1 & medicare

replace breastcancer=0 if breastcancer==.

gen lungcancer=0
replace lungcancer=1 if calung==1
replace lungcancer=. if calung==-8


replace racegroup=. if racegroup==0
gen logbmi=log(bmindx53+1)
replace sampleadult=. if otherrace==1
 **********************
 gen hrtdisease=0
 replace hrtdisease=1 if angina ==1| corhrtattack ==1|mildhrtattack ==1 |othrhrtdisease ==1
 replace hrtdisease=.  if angina ==.| corhrtattack ==.|mildhrtattack ==. |othrhrtdisease ==.
 
***Global

global xvars "male female age1824 age2534 age3544 age4554 age5564 age6574 age75plus white black hisp asian marry div widow separated nvrmarried nohsdg hs somecoll_ass collgr mastersplus  poor nearpoor lincome mincome hincome prv pubcov unins fairp goodh exclvrgoodh fairpm goodhm exclvrgoodhm anylmt hyperten diabetes asthma hrtdisease stroke emphysema jointpain arthritis cancer east midw south west"

****Univariate
  ****Univariate
svy, subpop(sampleadult): mean anyexpend totexp cancer $xvars  

 ****Bivariate
 svy, subpop(sampleadult): mean anyexpend  totexp $xvars, over (cancer)
test [totexp]0 = [totexp]1
test [anyexpend]0 = [anyexpend]1


****two parts models 
global model14 "female age2534 age3544 age4554 age5564 age6574 age75plus  black hisp asian div widow separated nvrmarried hs somecoll_ass collgr mastersplus poor nearpoor lincome mincome pubcov unins fairp fairpm anylmt hyperten diabetes asthma hrtdisease stroke emphysema jointpain arthritis cancer midw south west"

****first part
svy, subpop(sampleadult) : logit anyexpend cancer $model14
estimates store logitmod1
replace cancer =0
predict cancer_p0, p
replace cancer=1
predict  cancer_p1, p

drop cancer
gen cancer=0 if cancerdx==2
replace cancer=1 if cancerdx==1
replace cancer=. if cancerdx <0
***second part
svy, subpop(sampleadult):  glm totexp cancer $model14  if totexp>0 , link(log) family(gamma)
estimates store glmmod1
replace cancer =0
predict cancer_xb0,xb
replace cancer=1
predict  cancer_xb1,xb

drop cancer
gen cancer=0 if cancerdx==2
replace cancer=1 if cancerdx==1

replace cancer=. if cancerdx <0
/*tables */
estout logitmod1 glmmod1, cells(b(star fmt(2)) se(par fmt(2)))  starlevels(* .10 ** 0.05 *** .01) stats(r2 N_sub, label("R2" "N" fmt(%7.2g %7.0f))) varwidth(12) modelwidth(10) margin legend label varlabels(_cons Constant) type

********** INCREMENTAL COST***************************

gen lvlcancermod1_xb0 = exp(cancer_xb0) 
gen lvlcancermod1_xb1 = exp(cancer_xb1)
gen fcancermod1 =cancer_p0*lvlcancermod1_xb0 
gen scancermod1 =cancer_p1*lvlcancermod1_xb1
gen incrcancemod1=scancermod1-fcancermod1


svy, subpop(sampleadult): mean fcancermod1 scancermod1 incrcancemod1
