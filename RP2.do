*************************************************************
*	Replication Exercise 2.									*
*	Nixon Alberto Torres Candiales							*
*	nat936													*
*	Cuasal Inference										*
*	ECO 385k												*
*	The University of Texas at Austin						*
*	03/10/2020												*
*************************************************************

cd "/Users/Nixon/Desktop/Ma Economics/Spring 20/CI/Problem Sets/Assignment2/Rp/Do"
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

	cd ../Tables
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
		quie eststo ivreg1: ivregress 2sls workedm agem1 agefstm black hispan othrace boy1st boy2nd (morekids = samesex), first vce(r) 
		quie eststo ivreg2: ivregress 2sls weeksm1 agem1 agefstm black hispan othrace boy1st boy2nd (morekids = samesex), vce(r) first
		quie eststo ivreg3: ivregress 2sls hourswm agem1 agefstm black hispan othrace boy1st boy2nd (morekids = samesex), vce(r) first
		quie eststo ivreg4: ivregress 2sls incomem agem1 agefstm black hispan othrace boy1st boy2nd (morekids = samesex), vce(r) first
	}
}
******************** 4. TABLES
{
cd ../Tables
	**********************************************************
	*						TABLE 3-1						 *
	**********************************************************
	{
	**** Estimation
	{
	estimates clear
	scalar drop _all
		//////////// Models for: workedm
		{
			**** CIV
			{
				quie regress morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
				cap n quie local first = _b[samesex]
				quie regress workedm agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
				cap n quie local reduce = _b[samesex]
				qui reg workedm morekids, nocons
				cap n quie estadd scalar civ = `reduce'/`first'
				cap n quie estimates store civ_`specname'
			}
			**** OLS 
			{
				cap n quie tempvar tempsample
				cap n quie local specname=`specname'+1
				qui regress workedm morekids agem1 agefstm black hispan othrace boy1st boy2nd,r // regular ols estimators
				cap n quie estadd ysumm
				cap n quie estimates store ols_`specname'
			}
			**** 2SLS 
			{
				cap n quie tempvar tempsample
				quie regress morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
				quie predict morekids_hat,xb
				cap n quie local specname=`specname'+1
				cap n quie local biv = _b[samesex]
				cap n quie local seiv = _se[samesex]
				cap n quie unab ivs: samesex
				cap n quie local xlist: colnames e(b)
				cap n quie local ivs: list ivs & xlist
				cap n quie test `ivs'
				cap n quie local F_iv=r(F)
				cap n quie local specname=`specname'+1
				quie replace morekids = morekids_hat
				quie reg workedm morekids agem1 agefstm black hispan othrace boy1st boy2nd ,r
				cap n quie estadd ysumm
				cap n quie estadd scalar biv  = `biv'
				cap n quie estadd scalar seiv = `seiv'
				cap n quie estadd scalar F_iv = `F_iv'
				cap n quie estimates store mtsls_`specname'
				quie drop morekids_hat
			}	
			**** IVreg
			{
				quie replace morekids=morekids2/100
				cap n quie local specname=`specname'+1
				quie regress morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
				cap n quie local biv = _b[samesex]
				cap n quie local seiv = _se[samesex]
				cap n quie unab ivs: samesex
				cap n quie local xlist: colnames e(b)
				cap n quie local ivs: list ivs & xlist
				cap n quie test `ivs'
				cap n quie local F_iv=r(F)
				cap n quie local specname=`specname'+1
				cap n quie ivregress 2sls workedm agem1 agefstm black hispan othrace boy1st boy2nd (morekids = samesex), vce(r)
				cap n quie estadd ysumm
				cap n quie estadd scalar biv  = `biv'
				cap n quie estadd scalar seiv = `seiv'
				cap n quie estadd scalar F_iv = `F_iv'
				cap n quie rivtest
				n quie return list
				cap n quie local ar_chi2=r(ar_chi2)
				cap n quie local ar_p=r(ar_p)
				cap n quie estadd scalar ar_chi2 = `ar_chi2'
				cap n quie estadd scalar ar_p = `ar_p'
				cap n quie estimates store tsls_`specname'
			}
			}
		//////////// Models for: weeksm1
		{
			*** CIV
			{
				quie regress morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
				cap n quie local first = _b[samesex]
				quie regress weeksm1 agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
				cap n quie local reduce = _b[samesex]
				quie reg weeksm1 morekids, nocons
				cap n quie estadd scalar civ = `reduce'/`first'
				cap n quie estimates store civ_`specname'
			}
			**** OLS 
			{
				cap n quie tempvar tempsample
				cap n quie local specname=`specname'+1
				qui regress weeksm1 morekids agem1 agefstm black hispan othrace boy1st boy2nd,r // regular ols estimators
				cap n quie estadd ysumm
				cap n quie estimates store ols_`specname'
			}
			***** 2SLS Manually
			{
				cap n quie tempvar tempsample
				quie regress morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
				quie predict morekids_hat,xb
				cap n quie local specname=`specname'+1
				cap n quie local biv = _b[samesex]
				cap n quie local seiv = _se[samesex]
				cap n quie unab ivs: samesex
				cap n quie local xlist: colnames e(b)
				cap n quie local ivs: list ivs & xlist
				cap n quie test `ivs'
				cap n quie local F_iv=r(F)
				cap n quie local specname=`specname'+1
				quie replace morekids = morekids_hat
				quie reg weeksm1 morekids agem1 agefstm black hispan othrace boy1st boy2nd ,r
				cap n quie estadd ysumm
				cap n quie estadd scalar biv  = `biv'
				cap n quie estadd scalar seiv = `seiv'
				cap n quie estadd scalar F_iv = `F_iv'
				cap n quie estimates store mtsls_`specname'
				quie drop morekids_hat
			}	
			**** 2SLS IV regress
			{
								quie replace morekids=morekids2/100
								cap n quie local specname=`specname'+1
								quie regress morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
								cap n quie local biv = _b[samesex]
								cap n quie local seiv = _se[samesex]
								cap n quie unab ivs: samesex
								cap n quie local xlist: colnames e(b)
								cap n quie local ivs: list ivs & xlist
								cap n quie test `ivs'
								cap n quie local F_iv=r(F)
								cap n quie local specname=`specname'+1
								cap n quie ivregress 2sls weeksm1 agem1 agefstm black hispan othrace boy1st boy2nd (morekids = samesex), vce(r)
								cap n quie estadd ysumm
								cap n quie estadd scalar biv  = `biv'
								cap n quie estadd scalar seiv = `seiv'
								cap n quie estadd scalar F_iv = `F_iv'
								cap n quie rivtest
								n quie return list
								cap n quie local ar_chi2=r(ar_chi2)
								cap n quie local ar_p=r(ar_p)
								cap n quie estadd scalar ar_chi2 = `ar_chi2'
								cap n quie estadd scalar ar_p = `ar_p'
								cap n quie estimates store tsls_`specname'
							}
		}	
	}							
	**** CODE FOR LATEX
	{
			la var morekids "More than 2 children"
			la var agem1 "Age"
			la var agefstm "Age at first birth"
			la var hispan "Hispanic"
			la var black "Black"
			la var othrace "Other race"
			la var boy1st "Boy 1st(s\_1)"
			la var boy2nd "Boy 2nd(s\_2)"
			la var samesex "Same Sex"
			la var workedm "Worked for pay"
			la var incomem "Labor income"
			la var weeksm1 "Weeks worked"
			la var hourswm "Hours per week"			
		#delimit ;
				cap n estout * using table3-1.tex, 
				style(tex) label notype 
				cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) 
				stats(biv seiv F_iv ar_chi2 N ymean ysd civ, star(biv) fmt(3 4 3 2 %9.0fc 3 3) 
				labels("Same sex IV" "Robust std. err." "F-statistic IV" 
				"Anderson-Rubin test" "Observations" "Mean Dep. Var." "SD. Dep. Var." "CIV"))
				keep(agem1 agefstm black hispan othrace boy1st boy2nd morekids) 
				replace noabbrev starlevels(* 0.10 ** 0.05 *** 0.01) wrap ///mtitles("CIV" "OLS" "2SLS" "IVREG" "CIV" "OLS" "2SLS" "IVREG")
				collabels(none) eqlabels(none) mlabels(none) mgroups(none)
				title(OLS and 2SLS Estimates of Labor-Supply Models Using 1980 Census Data)
				prehead("\begin{table}[htbp]\centering" "\begin{center}" "\Rotatebox{90}{" "\begin{threeparttable}" "\scriptsize"
				"\caption{@title}" "\label{table3-1}" "\begin{tabular}{l*{@E}{c}}"
				"\toprule"
				"\multicolumn{1}{l}{\textbf{Dependent variable}}&"
				"\multicolumn{4}{c}{\textbf{Worked for pay}}&"
				"\multicolumn{4}{c}{\textbf{Weeks worked}}\\"
				"\cmidrule(lr){2-5}"
				"\cmidrule(lr){6-9}"
				"\multicolumn{1}{c}{}&"
				"\multicolumn{1}{c}{CIV}&"
				"\multicolumn{1}{c}{OLS}&"
				"\multicolumn{1}{c}{2SLS}&"
				"\multicolumn{1}{c}{IVreg}&"
				"\multicolumn{1}{c}{CIV}&"
				"\multicolumn{1}{c}{OLS}&"
				"\multicolumn{1}{c}{2SLS}&"
				"\multicolumn{1}{c}{IVreg}&")
				posthead("\midrule")
				prefoot("\\" "\midrule" "\multicolumn{1}{c}{First Stage Regression}\\")  
				postfoot("\bottomrule" "\end{tabular}" "\begin{tablenotes}" "\tiny" 
				"\item Standard errors in parenthesis. * p$<$0.10, ** p$<$0.05, *** p$<$0.01" "\end{tablenotes}" "\end{threeparttable}" "}" 
				"\end{center}" "\end{table}")
			;
		#delimit cr
		}			
	}
	**********************************************************
	*						TABLE 3-2						 *
	**********************************************************
	{
	**** Estimation
	{
	estimates clear
	scalar drop _all 
		//////////// Models for: hourswm
		{
			*** CIV
			{
				quie regress morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
				cap n quie local first = _b[samesex]
				quie regress hourswm agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
				cap n quie local reduce = _b[samesex]
				quie reg hourswm morekids, nocons
				cap n quie estadd scalar civ = `reduce'/`first'
				cap n quie estimates store civ_`specname'
			}
			**** OLS 
			{
				cap n quie tempvar tempsample
				cap n quie local specname=`specname'+1
				qui regress hourswm morekids agem1 agefstm black hispan othrace boy1st boy2nd,r // regular ols estimators
				cap n quie estadd ysumm
				cap n quie estimates store ols_`specname'
			}
			***** 2SLS Manually
			{
				cap n quie tempvar tempsample
				quie regress morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
				quie predict morekids_hat,xb
				cap n quie local specname=`specname'+1
				cap n quie local biv = _b[samesex]
				cap n quie local seiv = _se[samesex]
				cap n quie unab ivs: samesex
				cap n quie local xlist: colnames e(b)
				cap n quie local ivs: list ivs & xlist
				cap n quie test `ivs'
				cap n quie local F_iv=r(F)
				cap n quie local specname=`specname'+1
				quie replace morekids = morekids_hat
				quie reg hourswm morekids agem1 agefstm black hispan othrace boy1st boy2nd ,r
				cap n quie estadd ysumm
				cap n quie estadd scalar biv  = `biv'
				cap n quie estadd scalar seiv = `seiv'
				cap n quie estadd scalar F_iv = `F_iv'
				cap n quie estimates store mtsls_`specname'
				quie drop morekids_hat
			}	
			**** 2SLS IV regress
			{
				quie replace morekids=morekids2/100
				cap n quie local specname=`specname'+1
				quie regress morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
				cap n quie local biv = _b[samesex]
				cap n quie local seiv = _se[samesex]
				cap n quie unab ivs: samesex
				cap n quie local xlist: colnames e(b)
				cap n quie local ivs: list ivs & xlist
				cap n quie test `ivs'
				cap n quie local F_iv=r(F)
				cap n quie local specname=`specname'+1
				cap n quie ivregress 2sls hourswm agem1 agefstm black hispan othrace boy1st boy2nd (morekids = samesex), vce(r)
				cap n quie estadd ysumm
				cap n quie estadd scalar biv  = `biv'
				cap n quie estadd scalar seiv = `seiv'
				cap n quie estadd scalar F_iv = `F_iv'
				cap n quie rivtest
				n quie return list
				cap n quie local ar_chi2=r(ar_chi2)
				cap n quie local ar_p=r(ar_p)
				cap n quie estadd scalar ar_chi2 = `ar_chi2'
				cap n quie estadd scalar ar_p = `ar_p'
				cap n quie estimates store tsls_`specname'
			}
			}
		//////////// Models for: incomem
		{
			**** CIV
			{
				quie regress morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
				cap n quie local first = _b[samesex]
				quie regress incomem agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
				cap n quie local reduce = _b[samesex]
				quie reg incomem morekids, nocons
				cap n quie estadd scalar civ = `reduce'/`first'
				cap n quie estimates store civ_`specname'
			}
			**** OLS 
			{
				cap n quie tempvar tempsample
				cap n quie local specname=`specname'+1
				qui regress incomem morekids agem1 agefstm black hispan othrace boy1st boy2nd,r // regular ols estimators
				cap n quie estadd ysumm
				cap n quie estimates store ols_`specname'
			}
			**** 2SLS
			{
				cap n quie tempvar tempsample
				quie regress morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
				quie predict morekids_hat,xb
				cap n quie local specname=`specname'+1
				cap n quie local biv = _b[samesex]
				cap n quie local seiv = _se[samesex]
				cap n quie unab ivs: samesex
				cap n quie local xlist: colnames e(b)
				cap n quie local ivs: list ivs & xlist
				cap n quie test `ivs'
				cap n quie local F_iv=r(F)
				cap n quie local specname=`specname'+1
				quie replace morekids = morekids_hat
				quie reg incomem morekids agem1 agefstm black hispan othrace boy1st boy2nd ,r
				cap n quie estadd ysumm
				cap n quie estadd scalar biv  = `biv'
				cap n quie estadd scalar seiv = `seiv'
				cap n quie estadd scalar F_iv = `F_iv'
				cap n quie estimates store mtsls_`specname'
				quie drop morekids_hat
			}	
			**** IVreg
			{
				quie replace morekids=morekids2/100
				cap n quie local specname=`specname'+1
				quie regress morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
				cap n quie local biv = _b[samesex]
				cap n quie local seiv = _se[samesex]
				cap n quie unab ivs: samesex
				cap n quie local xlist: colnames e(b)
				cap n quie local ivs: list ivs & xlist
				cap n quie test `ivs'
				cap n quie local F_iv=r(F)
				cap n quie local specname=`specname'+1
				cap n quie ivregress 2sls incomem agem1 agefstm black hispan othrace boy1st boy2nd (morekids = samesex), vce(r)
				cap n quie estadd ysumm
				cap n quie estadd scalar biv  = `biv'
				cap n quie estadd scalar seiv = `seiv'
				cap n quie estadd scalar F_iv = `F_iv'
				cap n quie rivtest
				n quie return list
				cap n quie local ar_chi2=r(ar_chi2)
				cap n quie local ar_p=r(ar_p)
				cap n quie estadd scalar ar_chi2 = `ar_chi2'
				cap n quie estadd scalar ar_p = `ar_p'
				cap n quie estimates store tsls_`specname'
			}
			
		
	}
	}
	**** CODE FOR LATEX
	{
				la var morekids "More than 2 children"
				la var agem1 "Age"
				la var agefstm "Age at first birth"
				la var hispan "Hispanic"
				la var black "Black"
				la var othrace "Other race"
				la var boy1st "Boy 1st(s\_1)"
				la var boy2nd "Boy 2nd(s\_2)"
				la var samesex "Same Sex"
				la var workedm "Worked for pay"
				la var incomem "Labor income"
				la var weeksm1 "Weeks worked"
				la var hourswm "Hours per week"			
			#delimit ;
				cap n estout * using table3-2.tex, 
				style(tex) label notype 
				cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) 
				stats(biv seiv F_iv ar_chi2 N ymean ysd civ, star(biv) fmt(3 4 3 2 %9.0fc 3 3) 
				labels("Same sex IV" "Robust std. err." "F-statistic IV" 
				"Anderson-Rubin test" "Observations" "Mean Dep. Var." "SD. Dep. Var." "CIV"))
				keep(agem1 agefstm black hispan othrace boy1st boy2nd morekids) 
				replace noabbrev starlevels(* 0.10 ** 0.05 *** 0.01) wrap ///mtitles("CIV" "OLS" "2SLS" "IVREG" "CIV" "OLS" "2SLS" "IVREG")
				collabels(none) eqlabels(none) mlabels(none) mgroups(none)
				title(OLS and 2SLS Estimates of Labor-Supply Models Using 1980 Census Data)
				prehead("\begin{table}[htbp]\centering" "\begin{center}" "\Rotatebox{90}{" "\begin{threeparttable}" "\scriptsize"
				"\caption{@title}" "\label{table3-2}" "\begin{tabular}{l*{@E}{c}}"
				"\toprule"
				"\multicolumn{1}{l}{\textbf{Dependent variable}}&"
				"\multicolumn{4}{c}{\textbf{Hours per week}}&"
				"\multicolumn{4}{c}{\textbf{Labor income}}\\"
				"\cmidrule(lr){2-5}"
				"\cmidrule(lr){6-9}"
				"\multicolumn{1}{c}{}&"
				"\multicolumn{1}{c}{CIV}&"
				"\multicolumn{1}{c}{OLS}&"
				"\multicolumn{1}{c}{2SLS}&"
				"\multicolumn{1}{c}{IVreg}&"
				"\multicolumn{1}{c}{CIV}&"
				"\multicolumn{1}{c}{OLS}&"
				"\multicolumn{1}{c}{2SLS}&"
				"\multicolumn{1}{c}{IVreg}&")
				posthead("\midrule")
				prefoot("\\" "\midrule" "\multicolumn{1}{c}{First Stage Regression}\\")  
				postfoot("\bottomrule" "\end{tabular}" "\begin{tablenotes}" "\tiny" 
				"\item Standard errors in parenthesis. * p$<$0.10, ** p$<$0.05, *** p$<$0.01" "\end{tablenotes}" "\end{threeparttable}" "}" 
				"\end{center}" "\end{table}")
			;
		#delimit cr
		}	
	}
}	
