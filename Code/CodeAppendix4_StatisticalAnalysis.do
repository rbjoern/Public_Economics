* CODE APPENDIX 4
* STATISTICAL ANALYSIS


********************************************************************************
*LOAD DATA
********************************************************************************
set more off 				// Useful setting
clear all					// Clears existing data, so code can be re-run
cls 						// Clears the results window. 
cd "C:/Users/rbjoe/Dropbox/Kugejl/8. semester/Public Economics/Public_economics/Code/Stata output" 
							// Assign a working folder
use "C:/Users/rbjoe/Dropbox/Kugejl/8. semester/Public Economics/Public_economics/Data/10. Main Dataset.dta"	
							// Load data
cap log close
log using log.txt, replace text
							
********************************************************************************

*THE CODE APPENDIX HAS THE FOLLOWING SECTION 
* VARIABLE GROUPS
* GENERATE VARIABLES
* SET LABELS 
* DESCRIPTIVE STATISTICS TABLE
* REGRESSION TABLE ONE. 	BASIC REGRESSION
* REGRESSION TABLE ONE. 	BASIC REGRESSION (CLUSTERED STD ERRORS)
* REGRESSION TABLE TWO.		POISSON 
* REGRESSION TABLE THREE. 	INVERSE HYPERBOLIC SINE TRANSFORMATION
* REGRESSION TABLE FIVE. 	TYPES OF FDI INCOME
* REGRESSION TABLE SIX. 	TAX DIFFERENTIAL 
* REGRESSION TABLE 7. 	CFC RULES. 


********************************************************************************
* VARIABLE GROUPS
********************************************************************************
{
local y "ln_FDIInc_Out_Total"
local y_ejlog "FDIInc_Out_Total"
local yy "ln_FDIInc_Out_Dividends ln_FDIInc_Out_Reinvested ln_FDIInc_Out_Debt" //ln_FDIInc_Out_Equity 
local yy_ejlog "FDIInc_Out_Dividends FDIInc_Out_Reinvested FDIInc_Out_Debt" //FDIInc_Out_Equity 
local Gravity_ejlog "GDP GDP_j distw"
local Gravity "ln_GDP ln_GDP_j ln_distw"
local Independent1 "Haven CIT_RATE HavenCIT_RATE"
local Independent2 "Small_Haven Large_Haven CIT_RATE Small_HavenCIT_RATE Large_HavenCIT_RATE"
local Independent3 "Haven CIT_difference HavenCIT_difference"
local Independent4 "CFC Haven CFCHaven CIT_RATE CFCCIT_RATE HavenCIT_RATE CFCHavenCIT_RATE"
local Control1 "contig comlang_off colony comcol" //EXCE
}
********************************************************************************

********************************************************************************
* GENERATE VARIABLES
********************************************************************************
{
*Interaction terms 
*Basic interaction
gen HavenCIT_RATE = Haven*CIT_RATE

*Based on size
gen Small_HavenCIT_RATE = Small_Haven*CIT_RATE

gen Large_HavenCIT_RATE = Large_Haven*CIT_RATE

*Based on the tax differences variable
gen HavenCIT_difference = Haven*CIT_difference

*CFC interactions
gen CFCHaven = Haven*CFC

gen CFCHavenCIT_RATE = Haven*CFC*CIT_RATE

gen CFCCIT_RATE = CFC*CIT_RATE

*Inverse hyperbolic sine transformation
gen IHS_FDI = log(FDIInc_Out_Total +(FDIInc_Out_Total^2+1)^(1/2))

*Create necessary logged variables
foreach var in `y_ejlog' `yy_ejlog' `Gravity_ejlog' {
gen ln_`var' = log(`var')
}
*Encode assigns numerical values to each group in a variable
encode(COU)					, gen(COU_Tal)
encode(COUNTERPART_AREA)	, gen(COUNTERPART_AREA_Tal)
encode(Pair)				, gen(Pair_Tal)
}

********************************************************************************

********************************************************************************
* SET LABELS 
********************************************************************************
{
*Dependent variabkes
label variable FDIInc_Out_Total 		"FDI income - Total"
label variable ln_FDIInc_Out_Total 		"ln FDI income - Total"
label variable IHS_FDI 					"IHS FDI income - Total"
label variable FDIInc_Out_Dividends 	"FDI income - Dividends"
label variable ln_FDIInc_Out_Dividends 	"ln FDI income - Dividends"
label variable FDIInc_Out_Equity 		"FDI income - Income on Equity"
label variable FDIInc_Out_Debt 			"FDI income - Debt income"
label variable ln_FDIInc_Out_Debt 		"ln FDI income - Debt income"
label variable FDIInc_Out_Reinvested 	"FDI income - Reinv. earnings"
label variable ln_FDIInc_Out_Reinvested	"ln FDI income - Reinv. earnings"
label variable FDIInc_Inw_Total 		"FDI income - Total (Inwards)"
label variable FDIInc_Inw_Dividends 	"FDI income - Dividends (Inwards)"
label variable FDIInc_Inw_Equity 		"FDI income - Income on Equity (Inwards)"
label variable FDIInc_Inw_Debt 			"FDI income - Interests from income on debt (Inwards)"
label variable FDIInc_Inw_Reinvested 	"FDI income - Reinvested earnings (Inwards)"
*Gravity
label variable ln_GDP					"$\ln\text{GDP}_{it}$"	
label variable ln_GDP_j					"$\ln\text{GDP}_{jt}$"	
label variable ln_distw					"$\ln\text{Distance (w)}$"
label variable contig 					"Common border"
label variable colony 					"Colony"
label variable comcol 					"Common colony"
label variable comlang_off 				"Common language"
*Independent
label variable CIT_RATE					"$ \text{CIT}_{it}$ $(\gamma_1)$"
label variable Haven 					"Haven $(\gamma_2)$"
label variable HavenCIT_RATE 			"Haven$\times\text{CIT}_{it}$ $ (\gamma_3)$ "
*Size
label variable Small_HavenCIT_RATE 		"Small Haven$\times\text{CIT}_{it}$"
label variable Large_HavenCIT_RATE 		"Large Haven$\times\text{CIT}_{ijt}$"
*Difference
label variable CIT_RATE_KPMG_j 			"$ \text{CIT}_{jt}$"
label variable CIT_difference 			"$ \text{DCIT}_{ijt}$ $(\gamma_1)$"
label variable HavenCIT_difference 		"Haven$\times\text{DCIT}_{ijt}$ $(\gamma_3)$ "
* CFC
label variable CFC 						"$\text{CFC}_i$$ (\iota_1)$"
label variable CFCHaven  				"$\text{CFC}_i\times\text{Haven}_j$$ (\iota_2)$"
label variable CFCCIT_RATE   			"$\text{CFC}_i\times\text{CIT}_{it}$$ (\iota_3)$"
label variable CFCHavenCIT_RATE  		"$\text{CFC}_i\times\text{Haven}_j\times\text{CIT}_{it}$$ (\iota_4)$"

}
********************************************************************************
*format dataset as panel. 
xtset Pair_Tal obsTime, yearly

*********************************************************************************************************
* DESCRIPTIVE STATISTICS TABLE
*********************************************************************************************************

/*
		\vspace{2pt} & \begin{footnotesize}\end{footnotesize} & \begin{footnotesize}\end{footnotesize} & \begin{footnotesize}\end{footnotesize} & \begin{footnotesize}\end{footnotesize} & \begin{footnotesize}\end{footnotesize} & \begin{footnotesize}\end{footnotesize} \\
\multicolumn{7}{l}{\textbf{Dependent variables (transformations)}} \\\hline
*/

quietly outreg2 using summary_gravity, sum(detail) replace label tex(frag pretty) word dec(1) ///
				keep(`y_ejlog' `yy_ejlog'  `y' IHS_FDI `yy' ////
				CIT_RATE  CIT_RATE_KPMG_j CIT_difference ///
				Haven Small_Haven Large_Haven  CFC ///				
				`Gravity_ejlog' `Gravity' `Control1' ///
				) ///
				sortvar(`y_ejlog' `yy_ejlog'  `y' IHS_FDI `yy' ////
				CIT_RATE  CIT_RATE_KPMG_j CIT_difference ///
				Haven Small_Haven Large_Haven  CFC ///				
				`Gravity_ejlog' `Gravity' `Control1' ///
				) /// 
				eqkeep(N mean p50 sd p1  p99 ) 
			
				
				
*********************************************************************************************************
* REGRESSION TABLE ONE. 	BASIC REGRESSION
*********************************************************************************************************
{
regress `y' `Gravity' `Independent1' `Control1', robust
	test CIT_RATE + HavenCIT_RATE=0
		scalar p1 = r(p)
	outreg2 using OLS, replace ctitle("Eq. (\ref{gravity1})") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("Dummies are added as specified above. FE drops $\zeta_i+\eta_j$. Robust standard errors in parantheses.", "*** p<0.01, ** p<0.05, * p<0.1") ///
							 addstat("$\gamma_1+\gamma_3 =0$ (p-value)", p1)  ///
							 dec(3) keep(`Gravity' `Independent1' `Control1')					 					 
						 
							 
regress `y' `Gravity' `Independent1' `Control1' b2015.obsTime, robust
	test CIT_RATE + HavenCIT_RATE=0
			scalar p1 = r(p)
	outreg2 using OLS, append ctitle("$+\delta_t$") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("Dummies are added as specified above. FE drops $\zeta_i+\eta_j$. Robust standard errors in parantheses.", "*** p<0.01, ** p<0.05, * p<0.1") ///
							 addstat("$\gamma_1+\gamma_3 =0$ (p-value)", p1)  ///
							 dec(3) keep(`Gravity' `Independent1' `Control1')	
*Equivalent alternative:
*areg `y' `Gravity' `Independent1' `Control1', absorb(obsTime) vce(robust) noomitted							

*regress `y' `Gravity' `Independent1' `Control1' b2015.obsTime b6.COU_Tal, robust // Reference group: Germany (most observations)									
areg `y' `Gravity' `Independent1' `Control1' b2015.obsTime, absorb(COU_Tal) vce(robust)
	test CIT_RATE + HavenCIT_RATE=0
			scalar p1 = r(p)
	outreg2 using OLS, append ctitle("$+\zeta_i$") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("Dummies are added as specified above. FE drops $\zeta_i+\eta_j$. Robust standard errors in parantheses.", "*** p<0.01, ** p<0.05, * p<0.1") ///
							 addstat("$\gamma_1+\gamma_3 =0$ (p-value)", p1)  ///
							 dec(3) keep(`Gravity' `Independent1' `Control1')				 
*Equivalent alternative:
*regress `y' `Gravity' `Independent1' `Control1' b2015.obsTime b6.COU_Tal  b59.COUNTERPART_AREA_Tal, robust //Reference group: Germany

areg `y' `Gravity' `Independent1' `Control1' b2015.obsTime  b6.COU_Tal, absorb(COUNTERPART_AREA_Ta) vce(robust) // Reference group: Germany (most observations)												 
	test CIT_RATE + HavenCIT_RATE=0
			scalar p1 = r(p)
	outreg2 using OLS, append ctitle("$+\eta_j$") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("Dummies are added as specified above. FE drops $\zeta_i+\eta_j$. Robust standard errors in parantheses.", "*** p<0.01, ** p<0.05, * p<0.1") ///
							 addstat("$\gamma_1+\gamma_3 =0$ (p-value)", p1)  ///
							 dec(3) keep(`Gravity' `Independent1' `Control1')	


areg `y' `Gravity' `Independent1' `Control1' b2015.obsTime, absorb(Pair_Tal) vce(robust) 
	test CIT_RATE + HavenCIT_RATE=0
		scalar p1 = r(p)
	outreg2 using OLS, append ctitle("$+\theta_{ij}$ (FE)") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("Dummies are added as specified above. FE drops $\zeta_i+\eta_j$. Robust standard errors in parantheses.", "*** p<0.01, ** p<0.05, * p<0.1") ///
							 addstat("$\gamma_1+\gamma_3 =0$ (p-value)", p1)  ///
							 dec(3) keep(`Gravity' `Independent1' `Control1')		

*Equivalent alternative: 
*regress `y' `Gravity' `Independent1' `Control1' b2015.obsTime b6.COU_Tal  b59.COUNTERPART_AREA_Tal  b1017.Pair_Tal //Reference group: Germany-France (most observations, large flow)
*quietly eststo eq1: regress `y' `Gravity' `Independent1' `Control1' b2015.obsTime b1017.Pair_Tal, robust //Reference group: Germany-France (most observations, large flow)
*esttab eq1, se keep(`Gravity' `Independent1' `Control1')							 
							 
*Equivalent alternative 
*xtreg `y' `Gravity' `Independent1' `Control1' b2015.obsTime, fe robust	

/*
			Time dummies &&X&X&X&X \\
			Country \textit{i} dummies &&&X&X& \\
			Country \textit{j} dummies &&&&X& \\
			Country pair dummies &&&&&X \\	
*/
}

*********************************************************************************************************
* REGRESSION TABLE ONE. 	BASIC REGRESSION (CLUSTERED STD ERRORS)
*********************************************************************************************************
{
regress `y' `Gravity' `Independent1' `Control1', robust cluster(distw)
	test CIT_RATE + HavenCIT_RATE=0
		scalar p1 = r(p)
	outreg2 using OLSCluster, replace ctitle("Eq. (\ref{gravity1})") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("Dummies are added as specified above. FE drops $\zeta_i+\eta_j$. Robust standard errors in parantheses.", "*** p<0.01, ** p<0.05, * p<0.1") ///
							 addstat("$\gamma_1+\gamma_3 =0$ (p-value)", p1)  ///
							 dec(3) keep(`Gravity' `Independent1' `Control1')		 					 
						 
							 
regress `y' `Gravity' `Independent1' `Control1' b2015.obsTime, robust cluster(distw)
	test CIT_RATE + HavenCIT_RATE=0
			scalar p1 = r(p)
	outreg2 using OLSCluster, append ctitle("$+\delta_t$") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("Dummies are added as specified above. FE drops $\zeta_i+\eta_j$. Robust standard errors in parantheses.", "*** p<0.01, ** p<0.05, * p<0.1") ///
							 addstat("$\gamma_1+\gamma_3 =0$ (p-value)", p1)  ///
							 dec(3) keep(`Gravity' `Independent1' `Control1')
*Equivalent alternative:
*areg `y' `Gravity' `Independent1' `Control1', absorb(obsTime) vce(cluster distw) noomitted							

*regress `y' `Gravity' `Independent1' `Control1' b2015.obsTime b6.COU_Tal, robust // Reference group: Germany (most observations)									
areg `y' `Gravity' `Independent1' `Control1' b2015.obsTime, absorb(COU_Tal) vce(cluster distw) 
	test CIT_RATE + HavenCIT_RATE=0
			scalar p1 = r(p)
	outreg2 using OLSCluster, append ctitle("$+\zeta_i$") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("Dummies are added as specified above. FE drops $\zeta_i+\eta_j$. Robust standard errors in parantheses.", "*** p<0.01, ** p<0.05, * p<0.1") ///
							 addstat("$\gamma_1+\gamma_3 =0$ (p-value)", p1)  ///
							 dec(3) keep(`Gravity' `Independent1' `Control1')		 
*Equivalent alternative:
*regress `y' `Gravity' `Independent1' `Control1' b2015.obsTime b6.COU_Tal  b59.COUNTERPART_AREA_Tal, robust //Reference group: Germany

areg `y' `Gravity' `Independent1' `Control1' b2015.obsTime  b6.COU_Tal, absorb(COUNTERPART_AREA_Ta) vce(cluster distw)  // Reference group: Germany (most observations)												 
	test CIT_RATE + HavenCIT_RATE=0
			scalar p1 = r(p)
	outreg2 using OLSCluster, append ctitle("$+\eta_j$") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("Dummies are added as specified above. FE drops $\zeta_i+\eta_j$. Robust standard errors in parantheses.", "*** p<0.01, ** p<0.05, * p<0.1") ///
							 addstat("$\gamma_1+\gamma_3 =0$ (p-value)", p1)  ///
							 dec(3) keep(`Gravity' `Independent1' `Control1')
*Equivalent alternative: 
*regress `y' `Gravity' `Independent1' `Control1' b2015.obsTime b6.COU_Tal  b59.COUNTERPART_AREA_Tal  b1017.Pair_Tal //Reference group: Germany-France (most observations, large flow)


areg `y' `Gravity' `Independent1' `Control1' b2015.obsTime, absorb(Pair_Tal) vce(cluster distw)  
	test CIT_RATE + HavenCIT_RATE=0
		scalar p1 = r(p)
	outreg2 using OLSCluster, append ctitle("$+\theta_{ij}$ (FE)") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("Dummies are added as specified above. FE drops $\zeta_i+\eta_j$. Robust standard errors in parantheses.", "*** p<0.01, ** p<0.05, * p<0.1") ///
							 addstat("$\gamma_1+\gamma_3 =0$ (p-value)", p1)  ///
							 dec(3) keep(`Gravity' `Independent1' `Control1')

*Equivalent alternative 
*xtreg `y' `Gravity' `Independent1' `Control1' b2015.obsTime, fe robust								 
}	

********************************************************************************************************
* REGRESSION TABLE TWO.		POISSON 
********************************************************************************************************						 
{

*Creates dummies manually (needed for ppml)
quietly ta COU_Tal, gen(COU_Tal_)
quietly ta Pair_Tal, gen(Pair_Tal_)
quietly ta obsTime, gen(obsTime_)

*replace  FDIInc_Out_Total=0 if FDIInc_Out_Total==.

/*
\begin{tabular}{lccc|ccc|ccc} \hline
			& \multicolumn{9}{c}{} \\
			& \multicolumn{3}{c}{OLS} & 
			\multicolumn{3}{c}{Poisson PML, $ FDIInc>0 $} & \multicolumn{3}{c}{Poisson PML, $ FDIInc\geq0 $} \\
		\cline{2-10}	
*/





* OLS
regress `y' `Gravity' `Independent1' `Control1', robust
	test CIT_RATE + HavenCIT_RATE=0
		scalar p1 = r(p)
	outreg2 using Zeroes, replace ctitle("Eq. (\ref{gravity1})") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1, 0.15) symbol(***, **, *, +) ///
							 addnote("All models include $ D_{ij}$ and a constant term. Dummies are added as specified above. Robust standard errors in parantheses.", "*** p<0.01, ** p<0.05, * p<0.1, + p<0.15") ///
							 dec(3) keep(`Gravity' `Independent1') nocon nonotes

areg `y' `Gravity' `Independent1' `Control1' b2015.obsTime, absorb(COU_Tal) vce(robust)
	test CIT_RATE + HavenCIT_RATE=0
			scalar p1 = r(p)
	outreg2 using Zeroes, append ctitle("$+\delta_t+\zeta_i$") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1, 0.15) symbol(***, **, *, +) ///
							 addnote("All models include $ D_{ij}$ and a constant term. Dummies are added as specified above. Robust standard errors in parantheses.", "*** p<0.01, ** p<0.05, * p<0.1, + p<0.15") ///
							 dec(3) keep(`Gravity' `Independent1') nocon nonotes
*Equivalent
*reg `y' `Gravity' `Independent1' `Control1' obsTime_* COU_Tal_*, robust
							 
areg `y' `Gravity' `Independent1' `Control1' b2015.obsTime, absorb(Pair_Tal) vce(robust) 
	test CIT_RATE + HavenCIT_RATE=0
		scalar p1 = r(p)
	outreg2 using Zeroes, append ctitle("FE") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1, 0.15) symbol(***, **, *, +) ///
							 addnote("All models include $ D_{ij}$ and a constant term. Dummies are added as specified above. Robust standard errors in parantheses.", "*** p<0.01, ** p<0.05, * p<0.1, + p<0.15") ///
							 dec(3) keep(`Gravity' `Independent1') nocon nonotes

*Poisson >0 
ppml `y_ejlog' `Gravity' `Independent1' `Control1'  if `y_ejlog'>0
	outreg2 using Zeroes, append ctitle("Eq. (\ref{gravity1})") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1, 0.15) symbol(***, **, *, +) ///
							 addnote("All models include $ D_{ij}$ and a constant term. Dummies are added as specified above. Robust standard errors in parantheses.", "*** p<0.01, ** p<0.05, * p<0.1, + p<0.15") ///
							 dec(3) keep(`Gravity' `Independent1') nocon nonotes
							 
ppml `y_ejlog' `Gravity' `Independent1' `Control1'  obsTime_* COU_Tal_*  if `y_ejlog'>0
	outreg2 using Zeroes, append ctitle("$+\delta_t+\zeta_i$") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1, 0.15) symbol(***, **, *, +) ///
							 addnote("All models include $ D_{ij}$ and a constant term. Dummies are added as specified above. Robust standard errors in parantheses.", "*** p<0.01, ** p<0.05, * p<0.1, + p<0.15") ///
							 dec(3) keep(`Gravity' `Independent1') nocon nonotes
							 
*quietly ppml `y_ejlog' `Gravity' `Independent1' `Control1'  obsTime_* Pair_Tal_* if `y_ejlog'>0
xtpoisson `y_ejlog' `Gravity' `Independent1' `Control1' b2015.obsTime  if `y_ejlog'>0, fe vce(robust) 
	outreg2 using Zeroes, append ctitle("FE") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1, 0.15) symbol(***, **, *, +) ///
							 addnote("All models include $ D_{ij}$ and a constant term. Dummies are added as specified above. Robust standard errors in parantheses.", "*** p<0.01, ** p<0.05, * p<0.1, + p<0.15") ///
							 dec(3) keep(`Gravity' `Independent1')	noni nocon nonotes					
							 							 
							 
*ppml `y' `Gravity' `Independent1' `Control1' obsTime_* Pair_Tal_*  if `y'>=0


*Poisson >=0 
ppml `y_ejlog' `Gravity' `Independent1' `Control1' if `y_ejlog'>=0
	outreg2 using Zeroes, append ctitle("Eq. (\ref{gravity1})") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1, 0.15) symbol(***, **, *, +) ///
							 addnote("All models include $ D_{ij}$ and a constant term. Dummies are added as specified above. Robust standard errors in parantheses.", "*** p<0.01, ** p<0.05, * p<0.1, + p<0.15") ///
							 dec(3) keep(`Gravity' `Independent1') nocon nonotes
							 
ppml `y_ejlog' `Gravity' `Independent1' `Control1'  obsTime_* COU_Tal_* if `y_ejlog'>=0
	outreg2 using Zeroes, append ctitle("$+\delta_t+\zeta_i$") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1, 0.15) symbol(***, **, *, +) ///
							 addnote("All models include $ D_{ij}$ and a constant term. Dummies are added as specified above. Robust standard errors in parantheses.", "*** p<0.01, ** p<0.05, * p<0.1, + p<0.15") ///
							 dec(3) keep(`Gravity' `Independent1') nocon nonotes

*quietly ppml `y_ejlog' `Gravity' `Independent1' `Control1'  obsTime_* Pair_Tal_* if `y_ejlog'>=0
xtpoisson `y_ejlog' `Gravity' `Independent1' `Control1' b2015.obsTime  if `y_ejlog'>=0, fe vce(robust)
	outreg2 using Zeroes, append ctitle("FE") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1, 0.15) symbol(***, **, *, +) ///
							 addnote("All models include $ D_{ij}$ and a constant term. Dummies are added as specified above. Robust standard errors in parantheses.", "*** p<0.01, ** p<0.05, * p<0.1, + p<0.15") ///
							 dec(3) keep(`Gravity' `Independent1')	noni nocon	 nonotes						 
						 
drop COU_Tal_*
drop Pair_Tal_*
drop obsTime_*


}						 


						 
********************************************************************************************************
* REGRESSION TABLE THREE. 	INVERSE HYPERBOLIC SINE TRANSFORMATION
********************************************************************************************************
{
*replace  FDIInc_Out_Total=0 if FDIInc_Out_Total==.
*drop IHS_FDI
*gen IHS_FDI = log(FDIInc_Out_Total +(FDIInc_Out_Total^2+1)^(1/2))


* OLS
regress `y' `Gravity' `Independent1' `Control1', robust
	test CIT_RATE + HavenCIT_RATE=0
		scalar p1 = r(p)
	outreg2 using ZeroesIHS, replace ctitle("Eq. (\ref{gravity1})") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1, 0.15) symbol(***, **, *, +) ///
							 addnote("All models include $ D_{ij}$ and a constant term. Dummies are added as specified above. Robust standard errors in parantheses.", "*** p<0.01, ** p<0.05, * p<0.1, + p<0.15") ///
							 addstat("$\gamma_1+\gamma_3 =0$ (p-value)", p1)  ///
							 dec(3) keep(`Gravity' `Independent1') nocon nonotes

areg `y' `Gravity' `Independent1' `Control1' b2015.obsTime, absorb(COU_Tal) vce(robust)
	test CIT_RATE + HavenCIT_RATE=0
			scalar p1 = r(p)
	outreg2 using ZeroesIHS, append ctitle("$+\delta_t+\zeta_i$") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1, 0.15) symbol(***, **, *, +) ///
							 addnote("All models include $ D_{ij}$ and a constant term. Dummies are added as specified above. Robust standard errors in parantheses.", "*** p<0.01, ** p<0.05, * p<0.1, + p<0.15") ///
							 addstat("$\gamma_1+\gamma_3 =0$ (p-value)", p1)  ///
							 dec(3) keep(`Gravity' `Independent1') nocon nonotes
							 
areg `y' `Gravity' `Independent1' `Control1' b2015.obsTime, absorb(Pair_Tal) vce(robust) 
	test CIT_RATE + HavenCIT_RATE=0
		scalar p1 = r(p)
	outreg2 using ZeroesIHS, append ctitle("FE") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1, 0.15) symbol(***, **, *, +) ///
							 addnote("All models include $ D_{ij}$ and a constant term. Dummies are added as specified above. Robust standard errors in parantheses.", "*** p<0.01, ** p<0.05, * p<0.1, + p<0.15") ///
							 addstat("$\gamma_1+\gamma_3 =0$ (p-value)", p1)  ///
							 dec(3) keep(`Gravity' `Independent1') nocon nonotes
							 
*IHS >0 
regress IHS_FDI `Gravity' `Independent1' `Control1' if FDIInc_Out_Total>0, robust
	test CIT_RATE + HavenCIT_RATE=0
		scalar p1 = r(p)
	outreg2 using ZeroesIHS, append ctitle("Eq. (\ref{gravity1})") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1, 0.15) symbol(***, **, *, +) ///
							 addnote("All models include $ D_{ij}$ and a constant term. Dummies are added as specified above. Robust standard errors in parantheses.", "*** p<0.01, ** p<0.05, * p<0.1, + p<0.15") ///
							 addstat("$\gamma_1+\gamma_3 =0$ (p-value)", p1)  ///
							 dec(3) keep(`Gravity' `Independent1') nocon nonotes

areg IHS_FDI `Gravity' `Independent1' `Control1' b2015.obsTime if FDIInc_Out_Total>0, absorb(COU_Tal) vce(robust)
	test CIT_RATE + HavenCIT_RATE=0
			scalar p1 = r(p)
	outreg2 using ZeroesIHS, append ctitle("$+\delta_t+\zeta_i$") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1, 0.15) symbol(***, **, *, +) ///
							 addnote("All models include $ D_{ij}$ and a constant term. Dummies are added as specified above. Robust standard errors in parantheses.", "*** p<0.01, ** p<0.05, * p<0.1, + p<0.15") ///
							 addstat("$\gamma_1+\gamma_3 =0$ (p-value)", p1)  ///
							 dec(3) keep(`Gravity' `Independent1') nocon nonotes
							 
areg IHS_FDI `Gravity' `Independent1' `Control1' b2015.obsTime if FDIInc_Out_Total>0, absorb(Pair_Tal) vce(robust) 
	test CIT_RATE + HavenCIT_RATE=0
		scalar p1 = r(p)
	outreg2 using ZeroesIHS, append ctitle("FE") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1, 0.15) symbol(***, **, *, +) ///
							 addnote("All models include $ D_{ij}$ and a constant term. Dummies are added as specified above. Robust standard errors in parantheses.", "*** p<0.01, ** p<0.05, * p<0.1, + p<0.15") ///
							 addstat("$\gamma_1+\gamma_3 =0$ (p-value)", p1)  ///
							 dec(3) keep(`Gravity' `Independent1') nocon nonotes					 

*IHS 
regress IHS_FDI `Gravity' `Independent1' `Control1', robust
	test CIT_RATE + HavenCIT_RATE=0
		scalar p1 = r(p)
	outreg2 using ZeroesIHS, append ctitle("Eq. (\ref{gravity1})") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1, 0.15) symbol(***, **, *, +) ///
							 addnote("All models include $ D_{ij}$ and a constant term. Dummies are added as specified above. Robust standard errors in parantheses.", "*** p<0.01, ** p<0.05, * p<0.1, + p<0.15") ///
							 addstat("$\gamma_1+\gamma_3 =0$ (p-value)", p1)  ///
							 dec(3) keep(`Gravity' `Independent1') nocon nonotes

areg IHS_FDI `Gravity' `Independent1' `Control1' b2015.obsTime, absorb(COU_Tal) vce(robust)
	test CIT_RATE + HavenCIT_RATE=0
			scalar p1 = r(p)
	outreg2 using ZeroesIHS, append ctitle("$+\delta_t+\zeta_i$") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1, 0.15) symbol(***, **, *, +) ///
							 addnote("All models include $ D_{ij}$ and a constant term. Dummies are added as specified above. Robust standard errors in parantheses.", "*** p<0.01, ** p<0.05, * p<0.1, + p<0.15") ///
							 addstat("$\gamma_1+\gamma_3 =0$ (p-value)", p1)  ///
							 dec(3) keep(`Gravity' `Independent1') nocon nonotes
							 
eststo check: areg IHS_FDI `Gravity' `Independent1' `Control1' b2015.obsTime, absorb(Pair_Tal) vce(robust) 
	test CIT_RATE + HavenCIT_RATE=0
		scalar p1 = r(p)
	outreg2 using ZeroesIHS, append ctitle("FE") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1, 0.15) symbol(***, **, *, +) ///
							 addnote("All models include $ D_{ij}$ and a constant term. Dummies are added as specified above. Robust standard errors in parantheses.", "*** p<0.01, ** p<0.05, * p<0.1, + p<0.15") ///
							 addstat("$\gamma_1+\gamma_3 =0$ (p-value)", p1)  ///
							 dec(3) keep(`Gravity' `Independent1') nocon nonotes							 
}
							 


********************************************************************************************************
* REGRESSION TABLE FOUR. 	SIZE OF HAVENS
********************************************************************************************************
{
regress `y' `Gravity' `Independent2' `Control1', robust
	test CIT_RATE + Small_HavenCIT_RATE=0
		scalar p1 = r(p)
	test CIT_RATE + Large_HavenCIT_RATE=0
		scalar p2 = r(p)
	outreg2 using OLS_Size, replace ctitle("Eq. (\ref{gravitysize})") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("All models include gravity variables and intercept.", "Robust standard errors in parantheses. *** p<0.01, ** p<0.05, * p<0.1") ///
							 addstat("$\gamma_1+\gamma^S_3 =0$ (p-value)", p1,"$\gamma_1+\gamma^L_3 =0$ (p-value)", p2)  ///
							 dec(3) keep(`Independent2') nocons nor2 noobs 						 					 
						 							 
areg `y' `Gravity' `Independent2' `Control1' b2015.obsTime, absorb(COU_Tal) vce(robust)
	test CIT_RATE + Small_HavenCIT_RATE=0
		scalar p1 = r(p)
	test CIT_RATE + Large_HavenCIT_RATE=0
		scalar p2 = r(p)
	outreg2 using OLS_Size, append ctitle("$+\delta_t+\zeta_i$") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("All models include gravity variables and intercept.", "Robust standard errors in parantheses. *** p<0.01, ** p<0.05, * p<0.1") ///
							 addstat("$\gamma_1+\gamma^S_3 =0$ (p-value)", p1,"$\gamma_1+\gamma^L_3 =0$ (p-value)", p2)  ///
							 dec(3) keep(`Independent2') nocons nor2 noobs 					 

areg `y' `Gravity' `Independent2' `Control1' b2015.obsTime, absorb(Pair_Tal) vce(robust) 
	test CIT_RATE + Small_HavenCIT_RATE=0
		scalar p1 = r(p)
	test CIT_RATE + Large_HavenCIT_RATE=0
		scalar p2 = r(p)
	outreg2 using OLS_Size, append ctitle("$+\theta_{ij}$ (FE)") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("All models include gravity variables and intercept.", "Robust standard errors in parantheses. *** p<0.01, ** p<0.05, * p<0.1") ///
							 addstat("$\gamma_1+\gamma^S_3 =0$ (p-value)", p1,"$\gamma_1+\gamma^L_3 =0$ (p-value)", p2)  ///
							 dec(3) keep(`Independent2') nocons nor2 noobs 		
}
							 
********************************************************************************************************
* REGRESSION TABLE FIVE. 	TYPES OF FDI INCOME 
********************************************************************************************************
{
cap erase OLS_Types.rtf
cap erase OLS_Types.tex
cap erase OLS_Types.txt

/*
				& \multicolumn{9}{c}{} \\
				& \multicolumn{3}{c}{Dividends} & \multicolumn{3}{c}{Reinvested earnings} & \multicolumn{3}{c}{Debt income}  \\
			\cline{2-10}
*/

foreach var in `yy' {

regress `var' `Gravity' `Independent1' `Control1', robust
	test CIT_RATE + HavenCIT_RATE=0
		scalar p1 = r(p)
	outreg2 using OLS_Types, append ctitle("Eq. (\ref{gravity1})") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("All models include $ D_{ij}$ and a constant term. Dummies are added as specified above. FE drops $\zeta_i$", "Robust standard errors in parantheses. *** p<0.01, ** p<0.05, * p<0.1, + p<0.15") ///
							 addstat("$\gamma_1+\gamma_3 =0$ (p)", p1)  ///
							 dec(3) keep(`Gravity' `Independent1') nocon			

							 
areg `var' `Gravity' `Independent1' `Control1' b2015.obsTime, absorb(COU_Tal) vce(robust)
	test CIT_RATE + HavenCIT_RATE=0
			scalar p1 = r(p)
	outreg2 using OLS_Types, append ctitle("$+\delta_t+\zeta_i$") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("All models include $ D_{ij}$ and a constant term. Dummies are added as specified above. FE drops $\zeta_i$", "Robust standard errors in parantheses. *** p<0.01, ** p<0.05, * p<0.1, + p<0.15") ///
							 addstat("$\gamma_1+\gamma_3 =0$ (p)", p1)  ///
							 dec(3) keep(`Gravity' `Independent1') nocon								 

areg `var' `Gravity' `Independent1' `Control1' b2015.obsTime, absorb(Pair_Tal) vce(robust) 
	test CIT_RATE + HavenCIT_RATE=0
		scalar p1 = r(p)
	outreg2 using OLS_Types, append ctitle("$+\theta_{ij}$") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("All models include $ D_{ij}$ and a constant term. Dummies are added as specified above. FE drops $\zeta_i$", "Robust standard errors in parantheses. *** p<0.01, ** p<0.05, * p<0.1, + p<0.15") ///
							 addstat("$\gamma_1+\gamma_3 =0$ (p)", p1)  ///
							 dec(3) keep(`Gravity' `Independent1') nocon									 
}	
}						 
********************************************************************************************************
* REGRESSION TABLE SIX. 	TAX DIFFERENTIAL
********************************************************************************************************
{
regress `y' `Gravity' `Independent3' `Control1', robust
	test CIT_difference + HavenCIT_difference=0
		scalar p1 = r(p)
	outreg2 using OLS_taxdifference, replace ctitle(" ") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("All models include $ D_{ij}$ and intercept. FE drops $\zeta_i$.", "Robust standard errors in parantheses. *** p<0.01, ** p<0.05, * p<0.1") ///
							 addstat("$\gamma_1+\gamma_3 =0$ (p)", p1)  ///
							 dec(3) keep(`Gravity' `Independent3') nocon		

							 
areg `y' `Gravity' `Independent3' `Control1' b2015.obsTime, absorb(COU_Tal) vce(robust)
	test CIT_difference + HavenCIT_difference=0
			scalar p1 = r(p)
	outreg2 using OLS_taxdifference, append ctitle("$+\delta_t+\zeta_i$") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("All models include $ D_{ij}$ and intercept. FE drops $\zeta_i$.", "Robust standard errors in parantheses. *** p<0.01, ** p<0.05, * p<0.1") ///
							 addstat("$\gamma_1+\gamma_3 =0$ (p)", p1)  ///
							 dec(3) keep(`Gravity' `Independent3') nocon									 

areg `y' `Gravity' `Independent3+' `Control1' b2015.obsTime, absorb(Pair_Tal) vce(robust) 
	test CIT_difference + HavenCIT_difference=0
		scalar p1 = r(p)
	outreg2 using OLS_taxdifference, append ctitle("$+\theta_{ij}$") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("All models include $ D_{ij}$ and intercept. FE drops $\zeta_i$.", "Robust standard errors in parantheses. *** p<0.01, ** p<0.05, * p<0.1") ///
							 addstat("$\gamma_1+\gamma_3 =0$ (p)", p1)  ///
							 dec(3) keep(`Gravity' `Independent3') nocon		

}
********************************************************************************************************
* REGRESSION TABLE 7. 	CFC RULES. 
********************************************************************************************************
{
regress `y' `Gravity' `Independent4' `Control1', robust
	test CIT_RATE + HavenCIT_RATE=0
			scalar p1 = r(p)
	test CIT_RATE + HavenCIT_RATE+CFCCIT_RATE+CFCHavenCIT_RATE=0
		scalar p2 = r(p)
	outreg2 using OLS_CFC, replace ctitle("Eq. (\ref{gravityCFC})") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("All models include gravity variables and intercept. FE drops $\zeta_i$.", "Robust standard errors in parantheses. *** p<0.01, ** p<0.05, * p<0.1") ///
							 addstat("$\gamma_1+\gamma_3 =0$ (p)", p1, "$\gamma_1+\gamma_3+\iota_3+\iota_4 =0$ (p)", p2)  ///
							 dec(3) keep(`Independent4') nocons nor2 noobs 					

							 
areg `y' `Gravity' `Independent4' `Control1' b2015.obsTime, absorb(COU_Tal) vce(robust)
	test CIT_RATE + HavenCIT_RATE=0
			scalar p1 = r(p)
	test CIT_RATE + HavenCIT_RATE+CFCCIT_RATE+CFCHavenCIT_RATE=0
		scalar p2 = r(p)
	outreg2 using OLS_CFC, append ctitle("$+\delta_t+\zeta_i$") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("All models include gravity variables and intercept. FE drops $\zeta_i$.", "Robust standard errors in parantheses. *** p<0.01, ** p<0.05, * p<0.1") ///
							 addstat("$\gamma_1+\gamma_3 =0$ (p)", p1, "$\gamma_1+\gamma_3+\iota_3+\iota_4 =0$ (p)", p2)  ///
							 dec(3) keep(`Independent4') nocons nor2 noobs 								
									 

areg `y' `Gravity' `Independent4+' `Control1' b2015.obsTime, absorb(Pair_Tal) vce(robust) 
	test CIT_RATE + HavenCIT_RATE=0
			scalar p1 = r(p)
	test CIT_RATE + HavenCIT_RATE+CFCCIT_RATE+CFCHavenCIT_RATE=0
		scalar p2 = r(p)
	outreg2 using OLS_CFC, append ctitle("$+\theta_{ij}$") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("All models include gravity variables and intercept. FE drops $\zeta_i$.", "Robust standard errors in parantheses. *** p<0.01, ** p<0.05, * p<0.1") ///
							 addstat("$\gamma_1+\gamma_3 =0$ (p)", p1, "$\gamma_1+\gamma_3+\iota_3+\iota_4 =0$ (p)", p2)  ///
							 dec(3) keep(`Independent4') nocons nor2 noobs 							
}
							 
*********************************************************************************************************
							 					 
log close


