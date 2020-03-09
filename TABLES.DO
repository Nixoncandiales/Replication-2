
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
