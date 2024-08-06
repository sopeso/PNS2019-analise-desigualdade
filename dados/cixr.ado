/* ******************************************************************
Author: Aluisio J D Barros
Email: abarros.epi@gmail.com
Date: 07 June 2012, 17 April 2014
      28 Jan 2016 - corrected graph option; added stable option to sort rank variable

This procedure calculates the concentration index (CIX) and 
can handle grouped data or microdata. For grouped data
it is based on the calculation of the area of the concentration curve.

For microdata, estimation is based on convenient regression and Stata commands
presented in O’Donnell O, van Doorslaer E, Wagstaff A, Lindelow M. Analyzing 
Health Equity Using Household Survey Data. A Guide to Techniques and Their 
Implementation. Washington, DC: The World Bank; 2008. 

Adapted to also calculate the "Corrected" Concentration Index proposed by
Erreygers, G., Correcting the Concentration Index, J Health Econ (2008),
doi:10.1016/j.jhealeco.2008.02.003.
The basic idea is that the CIX is obtained by a regression where
8 * Var(rank) / (Max(H_i) - Min(H_i)) = alpha + beta * rank_i + e_i

USING THE COMMAND
*** 1st var: ranking variable (some wealth indicator)
*** 2nd var: non-negative outcome (usually an indicator of ill-health). 
*** graph option to create the concentration curve
*** Quant(n) option to change de default number of quantiles in the graph
*** weight option to include sampling weights - must be integer
*** if complex sample, a cluster(cvar) option is available to be used along with weights (microdata only)
*** CORRected option to present the "corrected" concentration index proposed by Erreygers

The results are output in Stata scalars r(cix) and r(cix_se) for the CIX and its std. error.
If requested, r(ccix) and r(ccix_se) are the "corrected" CIX and its std. error. 

SYNTAX
cixr rank_var health_var [w=weight_var], CORRected GRaph Quant(number of groups for graph - microdata only) Cluster(cluster_variable)

EXAMPLES
cixr income stunted --> CIX with confidence interval
cixr income stunted, gr --> adds concentration curve
cixr assetindex development [w=sampleweight] --> CIX for weighted sample
cixr assetindex development [w=sampleweight], cluster(psu) --> CIX in complex cluster sample

Version history
17 April 2014 - corrected for the case where CIX was calculated even if the rank variable was missing for all observations
*********************************************************************
*/

program define cixr, rclass
  version 9.0

********** Concentration index (relative) allowing for weights and clusters
********** HOW TO USE THE COMMAND
*** 1st var: ordering (some wealth indicator)
*** 2nd var: non-negative outcome (usually an indicator of ill-health)
*** graph option to create the concentration curve
*** quant option allows to define de desired number of quantiles for the graph
*** plain outputs only the CI value
*** cluster allows for cluster sampling be corrected for
*** example: ci4 income malnutrition, gr

*** it is possible to recover the conc index in e(b) and its variance in e(V)
**********


syntax varlist(max=2 numeric) [if] [in] [fweight/] , [CORRected] [GRaph] [Quant(integer 0)] [plain] [Cluster(varname numeric)]

*** Temporary variables
tempvar _use _rrank _rank _temp _sum _dec _concxw _wt _area _wi _wisum

*** Values to 1, 2 e 3
tokenize `varlist'
preserve
quietly {
  set more off
  
  *** Apply "if" and "in" restrictions and remove missings for concentration variable
  gen byte _use = 1 `if' `in'
  keep if _use==1 
  
  *** Check suitability of rank variable
  inspect `1'
  if r(N) == 0 {
  	di as error "Error! Rank variable [" "`1'" "] is missing for all observations."
  	error 416
  }
  
  *** Check suitability of outcome (non-negative)
  inspect `2' 
  if r(N) == 0 {
  	di as error "Error! Outcome variable [" "`2'" "] is missing for all observations."
  	error 416
  }
  if r(N_neg)>0 {
  	di in red "Outcome must be a non-negative variable."
  	di as error "Outcome variable [" "`2'" "] contains negative values."
  	error 411
  }
  keep if `2' ~=.
  
  *** Decide whether microdata or grouped data 
  inspect `1'
  
  *** GROUPED DATA ***
  if r(N)==r(N_unique) {
        local nq = r(N)										// The data is grouped - one line per group
  	if "`plain'" == "plain" local graph = ""
  	if "`plain'" == "" {
  	  noisily display _newline in green "Relative concentration index for " in yellow "`2'" in green " using "in yellow "`1'" in green " as the sorting variable"
  	  noisily display in green _d(78) "-"
  	}
  	*** Calculate weights, ranks and the concentration index
  	if "`exp'" == "" gen _wt = 1
  	else gen _wt = `exp'
  	summ _wt
  	local wt_sum=r(sum)
  	local q1 = _N+1
  	set obs `q1'
  	replace `1' = 0 in `q1'
  	sort `1'
  	replace _wt=0 in 1
  	replace `2'=0 in 1
  	g _wtrel=_wt/`wt_sum'
  	gen cumwtrel = sum(_wtrel)
  	g x_i=cumwtrel
  	g concxwtrel = `2'*_wtrel
  	summ concxwtrel
  	local conc_mean=r(sum)
  	g concxwtrel_sum = sum(concxwtrel)
  	g y_i=concxwtrel_sum/`conc_mean'
  	gen y_idiag=cumwtrel
  	gen y_im1 = y_i[_n-1]
  	gen x_im1 = x_i[_n-1]
  	gen a_i = (y_i + y_im1)*(x_i - x_im1)/2
  	gen _area = sum(a_i)
  	local cidxw = 1 - 2 * _area[_N]
  	g a=(`2'/`conc_mean')*(2*(cumwtrel[_n-1]+0.5*_wtrel)-1-`cidxw')+2-y_i-y_i[_n-1]
  	g fa2=_wtrel*a^2
  	su fa2
  	local fa2sum=r(sum)
  	local cidxw_se=sqrt(1/(_N-1)*(`fa2sum'-(1+`cidxw')^2))
  	return scalar cix_se = `cidxw_se'
  	return scalar cix = `cidxw'
  	
  	*** Create graph
  	if "`graph'" == "graph" {
  	  sc y_idiag y_i x_i, c(l l) s(. .) xla(0 .2 to 1) yla(0 .2 to 1) xtitle("Cumulative percent ranked by economic status") l2title("Cumulative percent") t2title(" ") legend(off)
  	} /* end if */
  	*** Output results
  	if "`plain'" == "" {
  	  noisily display in green "Concentration index = "  as result %6.4f `cidxw' " (" %6.4f `cidxw_se' ")"
  	  noisily display in green _d(78) "-"
  	}
  	else  di "`cidxw'" 
  } // end routine for grouped data
  
  *** MICRODATA ***
  else {														// Microdata - one line per individual		
  	*** Choose best number of quantiles for the graph
  	foreach bnq in 20 10 5 {
  	  pctile _pct=`1', nq(`bnq')
  		inspect _pct
  	  	if r(N_unique)==`bnq'-1 {
          	  	  local nq = `bnq'
          	  	  continue, break
  	  	} 
  	  	else drop _pct
  	  	}
    
  	/**** Calculate concentration index based on formulas given in 
  	O’Donnell O, van Doorslaer E, Wagstaff A, Lindelow M. Analyzing Health Equity Using Household 
  	Survey Data. A Guide to Techniques and Their Implementation. Washington, DC: The World Bank; 2008. */
  	
  	*** Create rank variable for convenient regression
  	sort `1', stable
  	egen double _rrank=rank(`1'), unique
  	sort _rrank 
  	if "`exp'" == "" gen _wt = 1
  	else gen _wt = `exp'
  	sum _wt
  	gen double _wi=_wt/r(sum)
  	gen double _wisum=sum(_wi)
  	gen double _rank=_wisum[_n-1]+0.5*_wi
  	replace _rank=0.5*_wi in 1
  	sum _rank [fw=_wt]
  	scalar var_rank=r(Var)
  	
  	*** Calculate max and min health var values
  	sum `2'
  	scalar _min=r(min)
  	scalar _max=r(max)
  	
  	*** Calculate concentration index
  	if "`cluster'" == "" reg `2' _rank [pw=_wt]
  	else reg `2' _rank [pw=_wt], vce(cluster `cluster')
  	noisily display _newline "Relative concentration index:"
  	noisily nlcom Conc_index: ((2*var_rank)/(_b[_cons]+0.5*_b[_rank]))*_b[_rank], post noheader cformat(%9.4f)
  	mat def cix=e(b)
  	mat def cixse=e(V)
  	return scalar cix_se = sqrt(cixse[1,1])
  	return scalar cix = cix[1,1]
    return scalar cix_p = 2*normal(-abs(cix[1,1]/cixse[1,1]))
  	
  	*** Calculate "corrected" concentration index
  	if "`corrected'" ~= "" {
  	  if "`cluster'" == "" regr `2' _rank [pw=_wt]
  	  else regr `2' _rank [pw=_wt], vce(cluster `cluster')
  	  noisily display _newline `""Corrected" concentration index (Erreygers, 2008):"'
  	  noisily nlcom Corr_Conc_index: ((8*var_rank)/(_max-_min))*_b[_rank], post noheader cformat(%9.4f)
  	  mat def ccix=e(b)
  	  mat def ccixse=e(V)
  	  return scalar ccix_se = sqrt(ccixse[1,1])
  	  return scalar ccix = ccix[1,1]
      return scalar ccix_p = 2*normal(-abs(ccix[1,1]/ccixse[1,1]))
  	}
  	
  	**** Generate graph
  	if "`graph'" == "graph" {
  	  
  	  *** Criar os pontos de corte para a variável de ordenação
  	  if `quant'==0 local quant=`nq'
  	  xtile _dec = `1', n(`quant')
  	  
  	  *** Colapsar por decil e somar pesos e conc x peso
  	  gen _concxw = `2' * _wt
  	  collapse (sum) _wt _concxw, by(_dec)
  	  drop if _dec == .
  	  
  	  *** Somar para ter as cumulativas
  	  gen cumwt = sum(_wt)
  	  gen cumconc = sum(_concxw)
  	  local q1 = `quant'+1
  	  set obs `q1'
  	  replace cumwt = 0 in `q1'
  	  replace cumconc = 0 in `q1'
  	  replace _dec = 0 in `q1'
  	  sort _dec
  	  
  	  *** Gerar cumulativas em escala para max=1
  	  gen x_i=cumwt/cumwt[_N]
  	  gen y_i=cumconc/cumconc[_N]
  	  gen y_idiag=x_i
  	  
  	  *** gerar gráfico
  	  scatter y_idiag y_i x_i, c(l l) s(i i) xla(0 .2 to 1) yla(0 .2 to 1) xtitle("Cumulative percent ranked by wealth status") ytitle("Cumulative percent") legend(off)
  	}
  }
}

end

