* 变量预处理
use data.dta,clear
encode stock, gen(id)
encode industry, gen(ind)
xtset id year
gen Ltrans = L.trans
destring liquidity, replace force
save "data.dta", replace
* sgmediation3 导入
// cap program drop sgmediation3
// program define sgmediation3, rclass
// /* sobel-goodman mediation tests */
// version 9.0
// syntax varlist(max=1) [if] [in], iv(varlist numeric max=1) ///
//    mv(varlist numeric max=1) [ cv(varlist numeric) quietly ///
//    level(integer 95) prefix(string)]
// marksample touse
// markout `touse' `varlist' `mv' `iv' `cv'
// tempname coef emat
//
// display
// `quietly' {
//  display as text "Model with dv regressed on iv (path c)"
//  `prefix' reghdfe `varlist' `iv' `cv' if `touse', absorb(id year)
//  *`prefix' xtreg `varlist' `iv' `cv' i.time if `touse', fe
//  local ccoef=_b[`iv']
//  local cse =_se[`iv']
//
//  display
//  display "Model with mediator regressed on iv (path a)"
//  `prefix' reghdfe `mv' `iv' `cv' if `touse', absorb(id year)
//  *`prefix' xtreg `mv' `iv' `cv' i.time if `touse', fe
//
//  local acoef=_b[`iv']
//  local ase  =_se[`iv']
//  local avar =_se[`iv']^2
//
//  display
//  display "Model with dv regressed on mediator and iv (paths b and c')"
//  `prefix' reghdfe `varlist' `mv' `iv' `cv' if `touse', absorb(id year)
//  *`prefix' xtreg `varlist' `mv' `iv' `cv' i.time if `touse', fe
// }
//
// local bcoef=_b[`mv']
// local bse  =_se[`mv']
// local bvar =_se[`mv']^2
//
// local sobel =(`acoef'*`bcoef')
// local serr=sqrt((`bcoef')^2*`avar' + (`acoef')^2*`bvar')
// local stest=`sobel'/`serr'
// local g1err=sqrt((`bcoef')^2*`avar' + (`acoef')^2*`bvar' + `avar'*`bvar')
// local good1=`sobel'/`g1err'
// local g2err=sqrt((`bcoef')^2*`avar' + (`acoef')^2*`bvar' - `avar'*`bvar')
// local good2=`sobel'/`g2err'
// local direff = (`ccoef'-(`acoef'*`bcoef'))
// local dse    = _se[`iv']
// local toteff = `sobel'/`ccoef'
// local ratio = `sobel'/`direff'
// local t2d = ((`acoef'*`bcoef')+(`ccoef'-(`acoef'*`bcoef')))/`direff'
//
// display
// display as txt "Sobel-Goodman Mediation Tests"
// display
// display as txt "                     Coef         Std Err     Z           P>|Z|"
// display as txt "Sobel               " as res `sobel' _skip(4) `serr'  %8.4g ///
// `stest', _skip(5) 2*(1-norm(abs(`stest')))
// display as txt "Goodman-1 (Aroian)  " as res `sobel' _skip(4) `g1err' %8.4g ///
// `good1', _skip(5) 2*(1-norm(abs(`good1')))
// display as txt "Goodman-2           " as res `sobel' _skip(4) `g2err' %8.4g ///
// `good2', _skip(5) 2*(1-norm(abs(`good2')))
// display
// display as txt _col(21) "Coef" _col(31) "Std Err" _col(42) "Z" _col(53) "P>|Z|"
// display as txt "a coefficient   = " as res %8.0g `acoef'  "  " %8.0g `ase' "  " %8.0g `acoef'/`ase'  _col(50) %8.0g 2*(1-norm(abs(`acoef'/`ase')))
// display as txt "b coefficient   = " as res %8.0g `bcoef'  "  " %8.0g `bse' "  " %8.0g `bcoef'/`bse'  _col(50) %8.0g 2*(1-norm(abs(`bcoef'/`bse')))
// display as txt "Indirect effect = " as res %8.0g `sobel'  "  " %8.0g `serr' "  " %8.0g `stest'       _col(50) %8.0g 2*(1-norm(abs(`stest')))
// display as txt "  Direct effect = " as res %8.0g `direff' "  " %8.0g `dse' "  " %8.0g `direff'/`dse' _col(50) %8.0g 2*(1-norm(abs(`direff'/`dse')))
// display as txt "   Total effect = " as res %8.0g `ccoef'  "  " %8.0g `cse' "  " %8.0g `ccoef'/`cse'  _col(50) %8.0g 2*(1-norm(abs(`ccoef'/`cse')))
// display
// display as txt "Proportion of total effect that is mediated: ", as res `toteff'
// display as txt "Ratio of indirect to direct effect:          ", as res `ratio'
// display as txt "Ratio of total to direct effect:             ", as res `t2d'
//
// return scalar ind_eff = `sobel'
// return scalar dir_eff = `direff'
// return scalar tot_eff = `ccoef'
// return scalar a_coef  = `acoef'
// return scalar b_coef  = `bcoef'
// return scalar ind2tot = `toteff'
// return scalar ind2dir = `ratio'
// return scalar tot2dir = `t2d'
//
// end
* 基准回归
reghdfe liquidity trans,	///
		absorb(id year) vce(cluster id)
est sto e1
reghdfe liquidity trans age size cashflow roe votality bm audit,	///
		absorb(id year) vce(cluster id)
est sto e2
esttab e1 e2 using 基准回归结果.rtf,compress nogap r2(a4)  star(* 0.1 ** 0.05 *** 0.01) b(a4) t(%6.4f)
* 中介效应检验
sgmediation3 	liquidity,	///
						mv(attention) iv(Ltrans) ///
						cv(age size cashflow roe votality bm audit)
sgmediation3 	liquidity,	///
						mv(innovation_c_cum) iv(Ltrans) ///
						cv(age size cashflow roe votality bm audit)
sgmediation3 	liquidity,	///
						mv(tobinq) iv(Ltrans) ///
						cv(age size cashflow roe votality bm audit)
* 异质性分析
reghdfe 	liquidity trans age size cashflow roe votality bm audit if (dual == 0),	///
				absorb(id year) vce(cluster id)
est sto m1
reghdfe 	liquidity trans age size cashflow roe votality bm audit if (dual == 1),	///
				absorb(id year) vce(cluster id)
est sto m2
esttab m1 m2 using 管理激励异质性.rtf,compress nogap r2(a4)  star(* 0.1 ** 0.05 *** 0.01) b(a4) t(%6.4f)
reghdfe 	liquidity trans age size cashflow roe votality bm audit if (soe == 0),	///
				absorb(id year) vce(cluster id)
est sto m1
reghdfe 	liquidity trans age size cashflow roe votality bm audit if (soe == 1),	///
				absorb(id year) vce(cluster id)
est sto m2
esttab m1 m2 using 股权激励异质性.rtf,compress nogap r2(a4)  star(* 0.1 ** 0.05 *** 0.01) b(a4) t(%6.4f)
