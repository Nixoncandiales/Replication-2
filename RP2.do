*************************************************************
*	Replication Exercise 2.									*
*	Nixon Alberto Torres Candiales							*
*	nat936													*
*	Cuasal Inference										*
*	ECO 385k												*
*	The University of Texas at Austin						*
*	02/27/2020												*
*************************************************************
*prueba prueba prueba
cd "/Users/Nixon/Desktop/Ma Economics/Spring 20/CI/PS/Assignment2/Rp/Do"
use ../Data/pums80.dta, clear
desc

//1
*Table Means
generate kidcount2 = kidcount
la var kidcount2 "Mean children ever born"
generate workedm2=100*workedm
la var workedm2 "Percent with more than 2 children"
generate morekids2=100*morekids
la var morekids2 "Percent worked last year"


*quie eststo group1: mean kidcount2 morekids2 workedm2 if agem1 >=21 & agem1 <=35 // Women Aged 21-35 information is the same as group 3
//eststo group2: mean kidcount morekids workedm if agem1 >=36 & agem1 <=50
quie eststo group3: mean kidcount2 morekids2 workedm2 if agem1 >=21 & agem1 <=35 & kidcount>=2 // Women Aged 21-35 With more than two kids 
cd ../Tables
esttab group3 using table1.tex, not nostar label title(Fertility And Labor Supply Measures \label{tab1}) mtitles("1980 PUMS") nogap replace
esttab group3, not nostar label title(Table1 Fertility And Labor-Supply Measures \label{tab1}) mtitles("1980 PUMS") nogap replace
eststo clear

//2
gen boyboy=boy1st*boy2nd
la var boyboy "Two boys (=1 if first two children were boys"
gen girlgirl =(1-boy1st)*(1-boy2nd)
la var girlgirl "Two girls (=1 if first two children were girls"
la var kidcount "Children ever born"
la var morekids "More than 2 children (=1 if mother had more than 2 children, =0 otherwise)"
la var boy1st "Boy 1st(s1) (=1 if first child was a boy)"
la var boy2nd "Boy 2nd(s2) (=1 if second child was a boy)"
la var samesex "Same Sex (=1 if first two children were the same sex)"
la var multi2nd "Twins-2 (=1 if second birth was a twin)"
la var workedm "Worked for pay (=1 if worked for pay in year prior to census)"
la var incomem "Labor income (labor earnings in year prior to census, in 1995 dollars)"
la var agem1 "Age"
la var agefstm "Age at first birth (parent's age in year when first child was born)"
la var weeksm1 "Weeks worked (weeks worked in year prior to census)"
la var hourswm "Hours/week (average hours worked per week)"

quie eststo table2: mean kidcount morekids boy1st boy2nd boyboy girlgirl samesex multi2nd agem1 agefstm workedm weeksm1 hourswm incomem
esttab table2 using table2.tex, nostar se label title(Descriptive Statistics, Women Aged 21-35 With 2 or More Children \label{tab2}) mtitles("1980 PUMS / All Women") nogap cells(b(fmt(3)) se(par fmt(3))) note("\textbf{Notes:} The 1980 data are from the 5-percent PUMS. Source: Angrist(XXX)") replace //wrap *optional
esttab table2, nostar se label title(Table2- Descriptive Statistics, Women Aged 21-35 With 2 or More Children \label{tab2}) mtitles("1980 PUMS / All Women") nogap wrap cells(b(fmt(3)) se(par fmt(3))) note("Source: Data") replace
eststo clear

//3
 ** 2sls estimation
 ivregress 2sls incomem agem1 agefstm black hispan othrace boy1st boy2nd (morekids = samesex), first vce(r)
 
 ivregress2 2sls incomem agem1 agefstm black hispan othrace boy1st boy2nd (morekids = samesex), first
     est restore first
    outreg2 using myfile.tex, cttop(first) replace
	 est restore second
    estat firststage
    local fstat `r(mineig)'
    estat endogenous
    local p_durbin `r(p_durbin)'
    outreg2 using myfile.txt, cttop(second) tex adds(IV F-stat, `fstat', Durbin pval, `p_durbin')


 
 


//4
