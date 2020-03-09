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

******************** 1. DESCRIPTIVE STATISTICS ACROSS GROUPS AND SUBSAMPLES
{
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
}

******************** 2. DESCRIPTIVE STATISTICS
{
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
}

******************** 3. Estimations: OLS, CIV, 2SLS, IVREG
{
	** OLS estimation
	{
		qui eststo ols1: regress workedm morekids agem1 agefstm black hispan othrace boy1st boy2nd,r // regular ols estimators
		qui eststo ols2: regress weeksm1 morekids agem1 agefstm black hispan othrace boy1st boy2nd,r // regular ols estimators
		qui eststo ols3: regress hourswm morekids agem1 agefstm black hispan othrace boy1st boy2nd,r // regular ols estimators
		qui eststo ols4: regress incomem morekids agem1 agefstm black hispan othrace boy1st boy2nd,r // regular ols estimators
	}

	** 2SLS CIV covariate adjusted iv estimator  // WALD ESTIMATOR (E[y|z==1]-E[y|z==0])/(E[x|z==1]-E[x|z==0])
	{
		qui regress workedm agem1 agefstm black hispan othrace boy1st boy2nd samesex,r // To get cor(y,z)
		mat a =e(b)
		qui regress morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r // To get cor(x,z)
		mat b =e(b)
		scalar civ1 = a[1,8]/b[1,8] //Point estimate for CIV regression.

		qui regress weeksm1 agem1 agefstm black hispan othrace boy1st boy2nd samesex,r // To get cor(y,z)
		mat a =e(b)
		qui regress morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r // To get cor(x,z)
		mat b =e(b)
		scalar civ2 = a[1,8]/b[1,8] //Point estimate for CIV regression.

		qui regress hourswm agem1 agefstm black hispan othrace boy1st boy2nd samesex,r // To get cor(y,z)
		mat a =e(b)
		qui regress morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r // To get cor(x,z)
		mat b =e(b)
		scalar civ3 = a[1,8]/b[1,8] //Point estimate for CIV regression.

		qui regress incomem agem1 agefstm black hispan othrace boy1st boy2nd samesex,r // To get cor(y,z)
		mat a =e(b)
		qui regress morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r // To get cor(x,z)
		mat b =e(b)
		scalar civ4 = a[1,8]/b[1,8] //Point estimate for CIV regression.
	}

	** 2SLS Manually
	{
		quie regress morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
		quie predict morekids_hat,xb
		qui eststo twosls1: reg workedm morekids_hat agem1 agefstm black hispan othrace boy1st boy2nd ,r
		drop morekids_hat

		quie regress morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
		quie predict morekids_hat,xb
		qui eststo twosls2: reg weeksm1 morekids_hat agem1 agefstm black hispan othrace boy1st boy2nd ,r
		drop morekids_hat

		quie regress morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
		quie predict morekids_hat,xb
		qui eststo twosls3: reg hourswm morekids_hat agem1 agefstm black hispan othrace boy1st boy2nd ,r
		drop morekids_hat

		quie regress morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
		quie predict morekids_hat,xb
		qui eststo twosls4: reg incomem morekids_hat agem1 agefstm black hispan othrace boy1st boy2nd ,r
		drop morekids_hat
	}

	 ** 2SLS ivregress estimation
	 {
		quie eststo ivreg1: ivregress 2sls workedm agem1 agefstm black hispan othrace boy1st boy2nd (morekids = samesex), vce(r)
		quie eststo ivreg2: ivregress 2sls weeksm1 agem1 agefstm black hispan othrace boy1st boy2nd (morekids = samesex), vce(r)
		quie eststo ivreg3: ivregress 2sls hourswm agem1 agefstm black hispan othrace boy1st boy2nd (morekids = samesex), vce(r)
		quie eststo ivreg4: ivregress 2sls incomem agem1 agefstm black hispan othrace boy1st boy2nd (morekids = samesex), vce(r)
	}
}

******************** TABLES
{
	**********************************************************TABLE 3.
	{
		//////////// Dependent Variable WORKEDM
		{
						**** OLS 
						{
							cap n tempvar tempsample
							cap n local specname=`specname'+1
							qui regress workedm morekids agem1 agefstm black hispan othrace boy1st boy2nd,r // regular ols estimators
							cap n estadd ysumm
							cap n estimates store ols_`specname'
						}
						*** CIV
						{
							quie regress morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
							cap n local first = _b[samesex]
							quie regress workedm agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
							cap n local reduce = _b[samesex]
							cap n estadd scalar civ = `reduce'/`first'
							cap n estimates store civ_`specname'
						}
						***** 2SLS Manually
						{
							cap n tempvar tempsample
							quie regress morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
							predict morekids_hat,xb
							cap n local specname=`specname'+1
							cap n local biv = _b[samesex]
							cap n local seiv = _se[samesex]
							cap n unab ivs: samesex
							cap n local xlist: colnames e(b)
							cap n local ivs: list ivs & xlist
							cap n test `ivs'
							cap n local F_iv=r(F)
							cap n local specname=`specname'+1
							quie replace morekids = morekids_hat
							quie reg workedm morekids agem1 agefstm black hispan othrace boy1st boy2nd ,r
							cap n estadd ysumm
							cap n estadd scalar biv  = `biv'
							cap n estadd scalar seiv = `seiv'
							cap n estadd scalar F_iv = `F_iv'
							cap n estimates store mtsls_`specname'
							drop morekids_hat
						}	
						**** 2SLS IV regress
						{
							replace morekids=morekids2/100
							cap n local specname=`specname'+1
							quie regress morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
							cap n local biv = _b[samesex]
							cap n local seiv = _se[samesex]
							cap n unab ivs: samesex
							cap n local xlist: colnames e(b)
							cap n local ivs: list ivs & xlist
							cap n test `ivs'
							cap n local F_iv=r(F)
							cap n local specname=`specname'+1
							cap n quie ivregress2 2sls workedm agem1 agefstm black hispan othrace boy1st boy2nd (morekids = samesex)
							cap n estadd ysumm
							cap n estadd scalar biv  = `biv'
							cap n estadd scalar seiv = `seiv'
							cap n estadd scalar F_iv = `F_iv'
							cap n rivtest
							n return list
							cap n local ar_chi2=r(ar_chi2)
							cap n local ar_p=r(ar_p)
							cap n estadd scalar ar_chi2 = `ar_chi2'
							cap n estadd scalar ar_p = `ar_p'
							cap n estimates store tsls_`specname'
						}
			}
			//////////// Dependent Variable weeksm1
			{
							**** OLS 
							{
								cap n tempvar tempsample
								cap n local specname=`specname'+1
								qui regress weeksm1 morekids agem1 agefstm black hispan othrace boy1st boy2nd,r // regular ols estimators
								cap n estadd ysumm
								cap n estimates store ols_`specname'
							}
							*** CIV
							{
								quie regress morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
								cap n local first = _b[samesex]
								quie regress weeksm1 agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
								cap n local reduce = _b[samesex]
								cap n estadd scalar civ = `reduce'/`first'
								cap n estimates store civ_`specname'
							}
							***** 2SLS Manually
							{
								cap n tempvar tempsample
								quie regress morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
								predict morekids_hat,xb
								cap n local specname=`specname'+1
								cap n local biv = _b[samesex]
								cap n local seiv = _se[samesex]
								cap n unab ivs: samesex
								cap n local xlist: colnames e(b)
								cap n local ivs: list ivs & xlist
								cap n test `ivs'
								cap n local F_iv=r(F)
								cap n local specname=`specname'+1
								quie replace morekids = morekids_hat
								quie reg weeksm1 morekids agem1 agefstm black hispan othrace boy1st boy2nd ,r
								cap n estadd ysumm
								cap n estadd scalar biv  = `biv'
								cap n estadd scalar seiv = `seiv'
								cap n estadd scalar F_iv = `F_iv'
								cap n estimates store mtsls_`specname'
								drop morekids_hat
							}	
							**** 2SLS IV regress
							{
								replace morekids=morekids2/100
								cap n local specname=`specname'+1
								quie regress morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
								cap n local biv = _b[samesex]
								cap n local seiv = _se[samesex]
								cap n unab ivs: samesex
								cap n local xlist: colnames e(b)
								cap n local ivs: list ivs & xlist
								cap n test `ivs'
								cap n local F_iv=r(F)
								cap n local specname=`specname'+1
								cap n quie ivregress2 2sls weeksm1 agem1 agefstm black hispan othrace boy1st boy2nd (morekids = samesex)
								cap n estadd ysumm
								cap n estadd scalar biv  = `biv'
								cap n estadd scalar seiv = `seiv'
								cap n estadd scalar F_iv = `F_iv'
								cap n rivtest
								n return list
								cap n local ar_chi2=r(ar_chi2)
								cap n local ar_p=r(ar_p)
								cap n estadd scalar ar_chi2 = `ar_chi2'
								cap n estadd scalar ar_p = `ar_p'
								cap n estimates store tsls_`specname'
							}
			}
		
	}							
					
	esttab *, stats(biv seiv F_iv ar_p N ymean ysd civ, star(biv) fmt(3 3 3 2 %9.0fc 3 3))
	esttab * using tabla3-1.rtf,cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) stats(biv seiv F_iv ar_p N ymean ysd civ, star(biv) fmt(3 3 3 2 %9.0fc 3 3)) keep(agem1 agefstm black hispan othrace boy1st boy2nd morekids) replace noabbrev starlevels(* 0.10 ** 0.05 *** 0.01)
	estimates clear
	scalar drop _all
					
	***********************************************************TABLE 4.
	{
		//////////// Dependent Variable hourswm
		{
						**** OLS 
						{
							cap n tempvar tempsample
							cap n local specname=`specname'+1
							qui regress hourswm morekids agem1 agefstm black hispan othrace boy1st boy2nd,r // regular ols estimators
							cap n estadd ysumm
							cap n estimates store ols_`specname'
						}
						*** CIV
						{
							quie regress morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
							cap n local first = _b[samesex]
							quie regress hourswm agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
							cap n local reduce = _b[samesex]
							cap n estadd scalar civ = `reduce'/`first'
							cap n estimates store civ_`specname'
						}
						***** 2SLS Manually
						{
							cap n tempvar tempsample
							quie regress morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
							predict morekids_hat,xb
							cap n local specname=`specname'+1
							cap n local biv = _b[samesex]
							cap n local seiv = _se[samesex]
							cap n unab ivs: samesex
							cap n local xlist: colnames e(b)
							cap n local ivs: list ivs & xlist
							cap n test `ivs'
							cap n local F_iv=r(F)
							cap n local specname=`specname'+1
							quie replace morekids = morekids_hat
							quie reg hourswm morekids agem1 agefstm black hispan othrace boy1st boy2nd ,r
							cap n estadd ysumm
							cap n estadd scalar biv  = `biv'
							cap n estadd scalar seiv = `seiv'
							cap n estadd scalar F_iv = `F_iv'
							cap n estimates store mtsls_`specname'
							drop morekids_hat
						}	
						**** 2SLS IV regress
						{
							replace morekids=morekids2/100
							cap n local specname=`specname'+1
							quie regress morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
							cap n local biv = _b[samesex]
							cap n local seiv = _se[samesex]
							cap n unab ivs: samesex
							cap n local xlist: colnames e(b)
							cap n local ivs: list ivs & xlist
							cap n test `ivs'
							cap n local F_iv=r(F)
							cap n local specname=`specname'+1
							cap n quie ivregress2 2sls hourswm agem1 agefstm black hispan othrace boy1st boy2nd (morekids = samesex)
							cap n estadd ysumm
							cap n estadd scalar biv  = `biv'
							cap n estadd scalar seiv = `seiv'
							cap n estadd scalar F_iv = `F_iv'
							cap n rivtest
							n return list
							cap n local ar_chi2=r(ar_chi2)
							cap n local ar_p=r(ar_p)
							cap n estadd scalar ar_chi2 = `ar_chi2'
							cap n estadd scalar ar_p = `ar_p'
							cap n estimates store tsls_`specname'
						}
			}
			//////////// Dependent Variable incomem
			{
							**** OLS 
							{
								cap n tempvar tempsample
								cap n local specname=`specname'+1
								qui regress incomem morekids agem1 agefstm black hispan othrace boy1st boy2nd,r // regular ols estimators
								cap n estadd ysumm
								cap n estimates store ols_`specname'
							}
							*** CIV
							{
								quie regress morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
								cap n local first = _b[samesex]
								quie regress incomem agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
								cap n local reduce = _b[samesex]
								cap n estadd scalar civ = `reduce'/`first'
								cap n estimates store civ_`specname'
							}
							***** 2SLS Manually
							{
								cap n tempvar tempsample
								quie regress morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
								predict morekids_hat,xb
								cap n local specname=`specname'+1
								cap n local biv = _b[samesex]
								cap n local seiv = _se[samesex]
								cap n unab ivs: samesex
								cap n local xlist: colnames e(b)
								cap n local ivs: list ivs & xlist
								cap n test `ivs'
								cap n local F_iv=r(F)
								cap n local specname=`specname'+1
								quie replace morekids = morekids_hat
								quie reg incomem morekids agem1 agefstm black hispan othrace boy1st boy2nd ,r
								cap n estadd ysumm
								cap n estadd scalar biv  = `biv'
								cap n estadd scalar seiv = `seiv'
								cap n estadd scalar F_iv = `F_iv'
								cap n estimates store mtsls_`specname'
								drop morekids_hat
							}	
							**** 2SLS IV regress
							{
								replace morekids=morekids2/100
								cap n local specname=`specname'+1
								quie regress morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
								cap n local biv = _b[samesex]
								cap n local seiv = _se[samesex]
								cap n unab ivs: samesex
								cap n local xlist: colnames e(b)
								cap n local ivs: list ivs & xlist
								cap n test `ivs'
								cap n local F_iv=r(F)
								cap n local specname=`specname'+1
								cap n quie ivregress2 2sls incomem agem1 agefstm black hispan othrace boy1st boy2nd (morekids = samesex)
								cap n estadd ysumm
								cap n estadd scalar biv  = `biv'
								cap n estadd scalar seiv = `seiv'
								cap n estadd scalar F_iv = `F_iv'
								cap n rivtest
								n return list
								cap n local ar_chi2=r(ar_chi2)
								cap n local ar_p=r(ar_p)
								cap n estadd scalar ar_chi2 = `ar_chi2'
								cap n estadd scalar ar_p = `ar_p'
								cap n estimates store tsls_`specname'
							}
			}
		
	}							

	esttab *, stats(biv seiv F_iv ar_p N ymean ysd civ, star(biv) fmt(3 3 3 2 %9.0fc 3 3))
	esttab * using tabla3-2.rtf,cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) stats(biv seiv F_iv ar_p N ymean ysd civ, star(biv) fmt(3 3 3 2 %9.0fc 3 3)) keep(agem1 agefstm black hispan othrace boy1st boy2nd morekids) replace noabbrev starlevels(* 0.10 ** 0.05 *** 0.01)	
	estimates clear
	scalar drop _all
}
