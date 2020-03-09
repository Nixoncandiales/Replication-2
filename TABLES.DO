******************** TABLES

{
cd ../Tables
	**********************************************************
	*						TABLE 3							 *
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
				quie regress workedm morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
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
				quie regress workedm morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
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
				quie regress weeksm1 morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
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
								//quie regress morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
								quie regress weeksm1 morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
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
				stats(biv seiv F_iv ar_chi2 N ymean ysd civ, star(biv) fmt(3 3 3 2 %9.0fc 3 3) 
				labels("IV instrument" "Robust std. err." "F-statistic IV" 
				"Anderson-Rubin test" "Observations" "Mean Dep. Var." "Std. Dev. Dep. Var." "CIV"))
				keep(agem1 agefstm black hispan othrace boy1st boy2nd morekids) 
				replace noabbrev starlevels(* 0.10 ** 0.05 *** 0.01) wrap ///mtitles("CIV" "OLS" "2SLS" "IVREG" "CIV" "OLS" "2SLS" "IVREG")
				collabels(none) eqlabels(none) mlabels(none) mgroups(none)
				title(OLS and 2SLS regressions of Log Earnings on Schooling)
				prehead("\begin{table}[htbp]\centering" "\begin{center}" "\Rotatebox{90}{" "\begin{threeparttable}" "\scriptsize"
				"\caption{@title}" "\label{table3-1}" "\begin{tabular}{l*{@E}{c}}"
				"\toprule"
				"\multicolumn{1}{l}{\textbf{Dependent variable}}&"
				"\multicolumn{4}{c}{\textbf{Worked for pay}}&"
				"\multicolumn{4}{c}{\textbf{Weeks worked}}\\"
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
				prefoot("\\" "\midrule" "\multicolumn{1}{c}{First Stage Instrument}\\")  
				postfoot("\bottomrule" "\end{tabular}" "\begin{tablenotes}" "\tiny" 
				"\item Standard errors in parenthesis. * p$<$0.10, ** p$<$0.05, *** p$<$0.01" "\end{tablenotes}" "\end{threeparttable}" "}" 
				"\end{center}" "\end{table}")
			;
		#delimit cr
		}			
	}
	**********************************************************
	*						TABLE 4							 *
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
				quie regress hourswm morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
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
				quie regress hourswm morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
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
				quie regress incomem morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
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
				quie regress incomem morekids agem1 agefstm black hispan othrace boy1st boy2nd samesex,r
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
				stats(biv seiv F_iv ar_chi2 N ymean ysd civ, star(biv) fmt(3 3 3 2 %9.0fc 3 3) 
				labels("IV instrument" "Robust std. err." "F-statistic IV" 
				"Anderson-Rubin test" "Observations" "Mean Dep. Var." "Std. Dev. Dep. Var." "CIV"))
				keep(agem1 agefstm black hispan othrace boy1st boy2nd morekids) 
				replace noabbrev starlevels(* 0.10 ** 0.05 *** 0.01) wrap ///mtitles("CIV" "OLS" "2SLS" "IVREG" "CIV" "OLS" "2SLS" "IVREG")
				collabels(none) eqlabels(none) mlabels(none) mgroups(none)
				title(OLS and 2SLS regressions of Log Earnings on Schooling)
				prehead("\begin{table}[htbp]\centering" "\begin{center}" "\Rotatebox{90}{" "\begin{threeparttable}" "\scriptsize"
				"\caption{@title}" "\label{table3-1}" "\begin{tabular}{l*{@E}{c}}"
				"\toprule"
				"\multicolumn{1}{l}{\textbf{Dependent variable}}&"
				"\multicolumn{4}{c}{\textbf{Hours per week}}&"
				"\multicolumn{4}{c}{\textbf{Labor income}}\\"
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
				prefoot("\\" "\midrule" "\multicolumn{1}{c}{First Stage Instrument}\\")  
				postfoot("\bottomrule" "\end{tabular}" "\begin{tablenotes}" "\tiny" 
				"\item Standard errors in parenthesis. * p$<$0.10, ** p$<$0.05, *** p$<$0.01" "\end{tablenotes}" "\end{threeparttable}" "}" 
				"\end{center}" "\end{table}")
			;
		#delimit cr
		}	
}
