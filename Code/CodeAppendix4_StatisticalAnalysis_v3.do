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
* REGRESSION TABLE FOUR. 	TAX DIFFERENTIAL 
* REGRESSION TABLE FIVE. 	SIZE OF HAVENS
* REGRESSION TABLE SIX. 	TYPES OF FDI INCOME
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
local Independent1 "Haven NonHavenCIT_RATE HavenCIT_RATE"
local Independent2 "Small_Haven Large_Haven NonHavenCIT_RATE Small_HavenCIT_RATE Large_HavenCIT_RATE"
local Independent3 "Haven NonHavenCIT_difference HavenCIT_difference"
local Independent4 "CFCNonHaven NonCFCHaven CFCHaven NonHavenNonCFCCIT_RATE NonHavenCFCCIT_RATE HavenNonCFCCIT_RATE CFCHavenCIT_RATE"
local Independent5 "NonCFCHaven CFCHaven NonHavenNonCFCCIT_RATE NonHavenCFCCIT_RATE HavenNonCFCCIT_RATE CFCHavenCIT_RATE"
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

*Interaction for NonHaven
gen NonHaven = 0
replace NonHaven = 1 if Haven ==0

gen NonHavenCIT_RATE = NonHaven*CIT_RATE

*Based on size
gen Small_HavenCIT_RATE = Small_Haven*CIT_RATE

gen Large_HavenCIT_RATE = Large_Haven*CIT_RATE

*Based on the tax differences variable
gen HavenCIT_difference = Haven*CIT_difference
gen NonHavenCIT_difference = NonHaven*CIT_difference

*CFC interactions
gen CFCHaven = Haven*CFC
gen CFCHavenCIT_RATE = Haven*CFC*CIT_RATE

gen NonCFC =0 
replace NonCFC = 1 if CFC ==0

gen NonCFCHaven = NonCFC*Haven
gen CFCNonHaven = NonHaven*CFC

gen NonHavenNonCFCCIT_RATE = NonCFC*NonHaven*CIT_RATE
gen HavenNonCFCCIT_RATE = Haven*NonCFC*CIT_RATE

gen CFCCIT_RATE = CFC*CIT_RATE
gen NonHavenCFCCIT_RATE = NonHaven*CFC*CIT_RATE

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
label variable CIT_RATE					"$ \text{CIT}_{it}$"
label variable Haven 					"$\text{Haven}_j$ $(\gamma_1)$"
label variable HavenCIT_RATE 			"$\text{Haven}_j\times\text{CIT}_{it}$ $ (\gamma_3)$ "
label variable NonHaven					"$\text{Non-haven}_j$ $(\gamma_2)$"
label variable NonHavenCIT_RATE 		"$\text{Non-haven}_j\times\text{CIT}_{it}$ $ (\gamma_2)$ "
*Size
label variable Small_HavenCIT_RATE 		"Small Haven$\times\text{CIT}_{it}$"
label variable Large_HavenCIT_RATE 		"Large Haven$\times\text{CIT}_{ijt}$"
*Difference
label variable CIT_RATE_KPMG_j 			"$ \text{CIT}_{jt}$"
label variable CIT_difference 			"$ \text{DCIT}_{ijt}$ $(\gamma_1)$"
label variable HavenCIT_difference 		"Haven$\times\text{DCIT}_{ijt}$ $(\gamma_3)$ "
label variable NonHavenCIT_difference 	"Non-haven$\times\text{DCIT}_{ijt}$ $(\gamma_3)$ "
* CFC
label variable CFC 						"$\text{CFC}_i$$ (\iota_1)$"
label variable CFCNonHaven				"$\text{CFC}_i\times\text{Non-haven}_j$$ (\iota_1)$"
label variable CFCHaven  				"$\text{CFC}_i\times\text{Haven}_j$$ (\iota_2)$"
label variable NonCFCHaven 				"$\text{Non-CFC}_i\times\text{Haven}_j$$ (\gamma_2)$"
label variable CFCCIT_RATE   			"$\text{CFC}_i\times\text{CIT}_{it}$$ (\iota_3)$"
label variable CFCHavenCIT_RATE  		"$\text{CFC}_i\times\text{Haven}_j\times\text{CIT}_{it}$$ (\iota_4)$"
label variable NonHavenCFCCIT_RATE		"$\text{CFC}_i\times\text{Non-haven}_j\times\text{CIT}_{it}$$ (\iota_3)$"
label variable NonHavenNonCFCCIT_RATE	"$\text{Non-CFC}_i\times\text{Non-haven}_j\times\text{CIT}_{it}$ $ (\gamma_2)$ "
label variable HavenNonCFCCIT_RATE  	"$\text{Non-CFC}_i\times\text{Haven}_j\times\text{CIT}_{it}$ $ (\gamma_3)$ "
 
}
********************************************************************************
*format dataset as panel. 
xtset Pair_Tal obsTime, yearly

*********************************************************************************************************
* DESCRIPTIVE STATISTICS TABLE
*********************************************************************************************************
{
/*
		\vspace{2pt} & \begin{footnotesize}\end{footnotesize} & \begin{footnotesize}\end{footnotesize} & \begin{footnotesize}\end{footnotesize} & \begin{footnotesize}\end{footnotesize} & \begin{footnotesize}\end{footnotesize} & \begin{footnotesize}\end{footnotesize} \\
\multicolumn{7}{l}{\textbf{Dependent variables (transformations)}} \\\hline
*/

quietly outreg2 using 0_summary_gravity, sum(detail) replace label tex(frag pretty) word dec(1) ///
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
			
}				
				
*********************************************************************************************************
* REGRESSION TABLE ONE. 	BASIC REGRESSION
*********************************************************************************************************
{
regress `y' `Gravity' `Independent1' `Control1', robust cluster(distw)
	test NonHavenCIT_RATE = HavenCIT_RATE
		scalar p1 = r(p)
	outreg2 using 1_OLS, replace ctitle("Eq. (\ref{gravity1})") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("Dummies are added as specified above. FE drops $\zeta_i+\eta_j$. Standard errors in parantheses.", "Standard errors are robust and clustered by distance. *** p<0.01, ** p<0.05, * p<0.1") ///
							 addstat("$\gamma_2=\gamma_3 $ (p-value)", p1)  ///
							 dec(3) keep(`Gravity' `Independent1' `Control1')			
				 					 
						 
							 
regress `y' `Gravity' `Independent1' `Control1' b2015.obsTime, robust cluster(distw)
	test NonHavenCIT_RATE = HavenCIT_RATE
		scalar p1 = r(p)
	outreg2 using 1_OLS, append ctitle("$+\delta_t$") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("Dummies are added as specified above. FE drops $\zeta_i+\eta_j$. Standard errors in parantheses.", "Standard errors are robust and clustered by distance. *** p<0.01, ** p<0.05, * p<0.1") ///
							 addstat("$\gamma_2=\gamma_3 $ (p-value)", p1)  ///
							 dec(3) keep(`Gravity' `Independent1' `Control1')			
			
*Equivalent alternative:
*areg `y' `Gravity' `Independent1' `Control1', absorb(obsTime) vce(cluster distw) noomitted							

*regress `y' `Gravity' `Independent1' `Control1' b2015.obsTime b6.COU_Tal, robust cluster(distw) // Reference group: Germany (most observations)									
areg `y' `Gravity' `Independent1' `Control1' b2015.obsTime, absorb(COU_Tal) vce(cluster distw)
	test NonHavenCIT_RATE = HavenCIT_RATE
		scalar p1 = r(p)
	outreg2 using 1_OLS, append ctitle("$+\zeta_i$") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("Dummies are added as specified above. FE drops $\zeta_i+\eta_j$. Standard errors in parantheses.", "Standard errors are robust and clustered by distance. *** p<0.01, ** p<0.05, * p<0.1") ///
							 addstat("$\gamma_2=\gamma_3 $ (p-value)", p1)  ///
							 dec(3) keep(`Gravity' `Independent1' `Control1')			
							 
*Equivalent alternative:
*regress `y' `Gravity' `Independent1' `Control1' b2015.obsTime b6.COU_Tal  b59.COUNTERPART_AREA_Tal, robust cluster(distw) //Reference group: Germany

areg `y' `Gravity' `Independent1' `Control1' b2015.obsTime  b6.COU_Tal, absorb(COUNTERPART_AREA_Ta) vce(cluster distw) // Reference group: Germany (most observations)												 
	test NonHavenCIT_RATE = HavenCIT_RATE
		scalar p1 = r(p)
	outreg2 using 1_OLS, append ctitle("$+\eta_j$") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("Dummies are added as specified above. FE drops $\zeta_i+\eta_j$. Standard errors in parantheses.", "Standard errors are robust and clustered by distance. *** p<0.01, ** p<0.05, * p<0.1") ///
							 addstat("$\gamma_2=\gamma_3 $ (p-value)", p1)  ///
							 dec(3) keep(`Gravity' `Independent1' `Control1')			
	


areg `y' `Gravity' `Independent1' `Control1' b2015.obsTime, absorb(Pair_Tal) vce(cluster distw) 
	test NonHavenCIT_RATE = HavenCIT_RATE
		scalar p1 = r(p)
	outreg2 using 1_OLS, append ctitle("$+\theta_{ij}$ (FE)") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("Dummies are added as specified above. FE drops $\zeta_i+\eta_j$. Standard errors in parantheses.", "Standard errors are robust and clustered by distance. *** p<0.01, ** p<0.05, * p<0.1") ///
							 addstat("$\gamma_2=\gamma_3 $ (p-value)", p1)  ///
							 dec(3) keep(`Gravity' `Independent1' `Control1')			

*Equivalent alternative: 
*regress `y' `Gravity' `Independent1' `Control1' b2015.obsTime b6.COU_Tal  b59.COUNTERPART_AREA_Tal  b1017.Pair_Tal //Reference group: Germany-France (most observations, large flow)
*quietly eststo eq1: regress `y' `Gravity' `Independent1' `Control1' b2015.obsTime b1017.Pair_Tal, robust cluster(distw) //Reference group: Germany-France (most observations, large flow)
*esttab eq1, se keep(`Gravity' `Independent1' `Control1')							 
							 
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
regress `y' `Gravity' `Independent1' `Control1', robust cluster(distw)
	outreg2 using 2_Poisson, replace ctitle("Eq. (\ref{gravity1})") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("All models include $ D_{ij}$ and a constant term. Dummies are added as specified above. Standard errors in parantheses.", "Standard errors are robust and clustered by \textit{distw}. *** p<0.01, ** p<0.05, * p<0.1") ///
							 dec(3) keep(`Gravity' `Independent1') nocon

areg `y' `Gravity' `Independent1' `Control1' b2015.obsTime, absorb(COU_Tal) vce(cluster distw)
	outreg2 using 2_Poisson, append ctitle("$+\delta_t+\zeta_i$") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("All models include $ D_{ij}$ and a constant term. Dummies are added as specified above. Standard errors in parantheses.", "Standard errors are robust and clustered by \textit{distw}. *** p<0.01, ** p<0.05, * p<0.1") ///
							 dec(3) keep(`Gravity' `Independent1') nocon
*Equivalent
*reg `y' `Gravity' `Independent1' `Control1' obsTime_* COU_Tal_*, robust cluster(distw)
							 
areg `y' `Gravity' `Independent1' `Control1' b2015.obsTime, absorb(Pair_Tal) vce(cluster distw) 
	outreg2 using 2_Poisson, append ctitle("FE") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("All models include $ D_{ij}$ and a constant term. Dummies are added as specified above. Standard errors in parantheses.", "Standard errors are robust and clustered by \textit{distw}. *** p<0.01, ** p<0.05, * p<0.1") ///
							 dec(3) keep(`Gravity' `Independent1') nocon

*Poisson >0 
ppml `y_ejlog' `Gravity' `Independent1' `Control1'  if `y_ejlog'>0, cluster(distw)
	outreg2 using 2_Poisson, append ctitle("Eq. (\ref{gravity1})") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("All models include $ D_{ij}$ and a constant term. Dummies are added as specified above. Standard errors in parantheses.", "Standard errors are robust and clustered by \textit{distw}. *** p<0.01, ** p<0.05, * p<0.1") ///
							 dec(3) keep(`Gravity' `Independent1') nocon
							 
ppml `y_ejlog' `Gravity' `Independent1' `Control1'  obsTime_* COU_Tal_*  if `y_ejlog'>0, cluster(distw)
	outreg2 using 2_Poisson, append ctitle("$+\delta_t+\zeta_i$") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("All models include $ D_{ij}$ and a constant term. Dummies are added as specified above. Standard errors in parantheses.", "Standard errors are robust and clustered by \textit{distw}. *** p<0.01, ** p<0.05, * p<0.1") ///
							 dec(3) keep(`Gravity' `Independent1') nocon
							 
*quietly ppml `y_ejlog' `Gravity' `Independent1' `Control1'  obsTime_* Pair_Tal_* if `y_ejlog'>0
xtpoisson `y_ejlog' `Gravity' `Independent1' `Control1' b2015.obsTime  if `y_ejlog'>0, fe vce(robust)
	outreg2 using 2_Poisson, append ctitle("FE") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("All models include $ D_{ij}$ and a constant term. Dummies are added as specified above. Standard errors in parantheses.", "Standard errors are robust and clustered by \textit{distw}. *** p<0.01, ** p<0.05, * p<0.1") ///
							 dec(3) keep(`Gravity' `Independent1') nocon	noni		
							 							 
							 
*ppml `y' `Gravity' `Independent1' `Control1' obsTime_* Pair_Tal_*  if `y'>=0


*Poisson >=0 
ppml `y_ejlog' `Gravity' `Independent1' `Control1' if `y_ejlog'>=0, cluster(distw)
	outreg2 using 2_Poisson, append ctitle("Eq. (\ref{gravity1})") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("All models include $ D_{ij}$ and a constant term. Dummies are added as specified above. Standard errors in parantheses.", "Standard errors are robust and clustered by \textit{distw}. *** p<0.01, ** p<0.05, * p<0.1") ///
							 dec(3) keep(`Gravity' `Independent1') nocon noni
							 
ppml `y_ejlog' `Gravity' `Independent1' `Control1'  obsTime_* COU_Tal_* if `y_ejlog'>=0, cluster(distw)
	outreg2 using 2_Poisson, append ctitle("$+\delta_t+\zeta_i$") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("All models include $ D_{ij}$ and a constant term. Dummies are added as specified above. Standard errors in parantheses.", "Standard errors are robust and clustered by \textit{distw}. *** p<0.01, ** p<0.05, * p<0.1") ///
							 dec(3) keep(`Gravity' `Independent1') nocon

*quietly ppml `y_ejlog' `Gravity' `Independent1' `Control1'  obsTime_* Pair_Tal_* if `y_ejlog'>=0
xtpoisson `y_ejlog' `Gravity' `Independent1' `Control1' b2015.obsTime  if `y_ejlog'>=0, fe vce(robust)
	outreg2 using 2_Poisson, append ctitle("FE") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("All models include $ D_{ij}$ and a constant term. Dummies are added as specified above. Standard errors in parantheses.", "Standard errors are robust and clustered by \textit{distw}. *** p<0.01, ** p<0.05, * p<0.1") ///
							 dec(3) keep(`Gravity' `Independent1') nocon	noni			 
						 
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
regress `y' `Gravity' `Independent1' `Control1', robust cluster(distw)
	test NonHavenCIT_RATE = HavenCIT_RATE
		scalar p1 = r(p)
	outreg2 using 3_IHS, replace ctitle("Eq. (\ref{gravity1})") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							nonotes addnote("Dummies are added as specified above. FE drops $\zeta_i+\eta_j$. Standard errors in parantheses.", "Standard errors are robust and clustered by distance. *** p<0.01, ** p<0.05, * p<0.1") ///
							addstat("$\gamma_2=\gamma_3 $ (p-value)", p1)  ///
							 dec(3) keep(`Gravity' `Independent1') nocon

areg `y' `Gravity' `Independent1' `Control1' b2015.obsTime, absorb(COU_Tal) vce(cluster distw)
	test NonHavenCIT_RATE = HavenCIT_RATE
		scalar p1 = r(p)
	outreg2 using 3_IHS, append ctitle("$+\delta_t+\zeta_i$") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							nonotes addnote("Dummies are added as specified above. FE drops $\zeta_i+\eta_j$. Standard errors in parantheses.", "Standard errors are robust and clustered by distance. *** p<0.01, ** p<0.05, * p<0.1") ///
							addstat("$\gamma_2=\gamma_3 $ (p-value)", p1)  ///
							 dec(3) keep(`Gravity' `Independent1') nocon

							 
areg `y' `Gravity' `Independent1' `Control1' b2015.obsTime, absorb(Pair_Tal) vce(cluster distw) 
	test NonHavenCIT_RATE = HavenCIT_RATE
		scalar p1 = r(p)
	outreg2 using 3_IHS, append ctitle("FE") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							nonotes addnote("Dummies are added as specified above. FE drops $\zeta_i+\eta_j$. Standard errors in parantheses.", "Standard errors are robust and clustered by distance. *** p<0.01, ** p<0.05, * p<0.1") ///
							addstat("$\gamma_2=\gamma_3 $ (p-value)", p1)  ///
							 dec(3) keep(`Gravity' `Independent1') nocon

							 
*IHS >0 
regress IHS_FDI `Gravity' `Independent1' `Control1' if FDIInc_Out_Total>0, robust cluster(distw)
	test NonHavenCIT_RATE = HavenCIT_RATE
		scalar p1 = r(p)
	outreg2 using 3_IHS, append ctitle("Eq. (\ref{gravity1})") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							nonotes addnote("Dummies are added as specified above. FE drops $\zeta_i+\eta_j$. Standard errors in parantheses.", "Standard errors are robust and clustered by distance. *** p<0.01, ** p<0.05, * p<0.1") ///
							addstat("$\gamma_2=\gamma_3 $ (p-value)", p1)  ///
							 dec(3) keep(`Gravity' `Independent1') nocon


areg IHS_FDI `Gravity' `Independent1' `Control1' b2015.obsTime if FDIInc_Out_Total>0, absorb(COU_Tal) vce(cluster distw)
	test NonHavenCIT_RATE = HavenCIT_RATE
		scalar p1 = r(p)
	outreg2 using 3_IHS, append ctitle("$+\delta_t+\zeta_i$") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							nonotes addnote("Dummies are added as specified above. FE drops $\zeta_i+\eta_j$. Standard errors in parantheses.", "Standard errors are robust and clustered by distance. *** p<0.01, ** p<0.05, * p<0.1") ///
							addstat("$\gamma_2=\gamma_3 $ (p-value)", p1)  ///
							 dec(3) keep(`Gravity' `Independent1') nocon

							 
areg IHS_FDI `Gravity' `Independent1' `Control1' b2015.obsTime if FDIInc_Out_Total>0, absorb(Pair_Tal) vce(cluster distw) 
	test NonHavenCIT_RATE = HavenCIT_RATE
		scalar p1 = r(p)
	outreg2 using 3_IHS, append ctitle("FE") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							nonotes addnote("Dummies are added as specified above. FE drops $\zeta_i+\eta_j$. Standard errors in parantheses.", "Standard errors are robust and clustered by distance. *** p<0.01, ** p<0.05, * p<0.1") ///
							addstat("$\gamma_2=\gamma_3 $ (p-value)", p1)  ///
							 dec(3) keep(`Gravity' `Independent1') nocon
			 

*IHS 
regress IHS_FDI `Gravity' `Independent1' `Control1', robust cluster(distw)
	test NonHavenCIT_RATE = HavenCIT_RATE
		scalar p1 = r(p)
	outreg2 using 3_IHS, append ctitle("Eq. (\ref{gravity1})") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							nonotes addnote("Dummies are added as specified above. FE drops $\zeta_i+\eta_j$. Standard errors in parantheses.", "Standard errors are robust and clustered by distance. *** p<0.01, ** p<0.05, * p<0.1") ///
							addstat("$\gamma_2=\gamma_3 $ (p-value)", p1)  ///
							 dec(3) keep(`Gravity' `Independent1') nocon


areg IHS_FDI `Gravity' `Independent1' `Control1' b2015.obsTime, absorb(COU_Tal) vce(cluster distw)
	test NonHavenCIT_RATE = HavenCIT_RATE
		scalar p1 = r(p)
	outreg2 using 3_IHS, append ctitle("$+\delta_t+\zeta_i$") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							nonotes addnote("Dummies are added as specified above. FE drops $\zeta_i+\eta_j$. Standard errors in parantheses.", "Standard errors are robust and clustered by distance. *** p<0.01, ** p<0.05, * p<0.1") ///
							addstat("$\gamma_2=\gamma_3 $ (p-value)", p1)  ///
							 dec(3) keep(`Gravity' `Independent1') nocon

							 
eststo check: areg IHS_FDI `Gravity' `Independent1' `Control1' b2015.obsTime, absorb(Pair_Tal) vce(cluster distw) 
	test NonHavenCIT_RATE = HavenCIT_RATE
		scalar p1 = r(p)
	outreg2 using 3_IHS, append ctitle("FE") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							nonotes addnote("Dummies are added as specified above. FE drops $\zeta_i+\eta_j$. Standard errors in parantheses.", "Standard errors are robust and clustered by distance. *** p<0.01, ** p<0.05, * p<0.1") ///
							addstat("$\gamma_2=\gamma_3 $ (p-value)", p1)  ///
							 dec(3) keep(`Gravity' `Independent1') nocon
					 
}
							 
********************************************************************************************************
* REGRESSION TABLE FOUR. 	TAX DIFFERENTIAL
********************************************************************************************************
{
regress `y' `Gravity' `Independent3' `Control1', robust cluster(distw)
	test NonHavenCIT_difference=HavenCIT_difference
		scalar p1 = r(p)
	outreg2 using 4_DCIT, replace ctitle(" ") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("All models include $ D_{ij}$ and intercept. FE drops $\zeta_i$. S.e. in parantheses.", "S.e. are robust and clustered by distance. *** p<0.01, ** p<0.05, * p<0.1") ///
							 addstat("$\gamma_2=\gamma_3 $ (p-value)", p1)  ///
							 dec(3) keep(`Gravity' `Independent3') nocon		

							 
areg `y' `Gravity' `Independent3' `Control1' b2015.obsTime, absorb(COU_Tal) vce(cluster distw)
	test NonHavenCIT_difference=HavenCIT_difference
		scalar p1 = r(p)
	outreg2 using 4_DCIT, append ctitle("$+\delta_t+\zeta_i$") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("All models include $ D_{ij}$ and intercept. FE drops $\zeta_i$. S.e. in parantheses.", "S.e. are robust and clustered by distance. *** p<0.01, ** p<0.05, * p<0.1") ///
							 addstat("$\gamma_2=\gamma_3 $ (p-value)", p1)  ///
							 dec(3) keep(`Gravity' `Independent3') nocon										 

areg `y' `Gravity' `Independent3+' `Control1' b2015.obsTime, absorb(Pair_Tal) vce(cluster distw) 
	test NonHavenCIT_difference=HavenCIT_difference
		scalar p1 = r(p)
	outreg2 using 4_DCIT, append ctitle("$+\theta_{ij}$") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("All models include $ D_{ij}$ and intercept. FE drops $\zeta_i$. S.e. in parantheses.", "S.e. are robust and clustered by distance. *** p<0.01, ** p<0.05, * p<0.1") ///
							 addstat("$\gamma_2=\gamma_3 $ (p-value)", p1)  ///
							 dec(3) keep(`Gravity' `Independent3') nocon	

}	

********************************************************************************************************
* REGRESSION TABLE FIVE. 	SIZE OF HAVENS
********************************************************************************************************
{
regress `y' `Gravity' `Independent2' `Control1', robust cluster(distw)
	test NonHavenCIT_RATE = Small_HavenCIT_RATE
		scalar p1 = r(p)
	test NonHavenCIT_RATE = Large_HavenCIT_RATE
		scalar p2 = r(p)
	outreg2 using 5_Size, replace ctitle("Eq. (\ref{gravitysize})") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							nonotes addnote("All models include $ D_{ij}$ and intercept. FE drops $\zeta_i$. S.e. in parantheses.", "S.e. are robust and clustered by distance. *** p<0.01, ** p<0.05, * p<0.1") ///
							 addstat("$\gamma_2=\gamma^S_3$ (p-value)", p1,"$\gamma_2=\gamma^L_3 $ (p-value)", p2)  ///
							 dec(3) keep(`Independent2') nocons nor2 noobs 						 					 
						 							 
areg `y' `Gravity' `Independent2' `Control1' b2015.obsTime, absorb(COU_Tal) vce(cluster distw)
	test NonHavenCIT_RATE = Small_HavenCIT_RATE
		scalar p1 = r(p)
	test NonHavenCIT_RATE = Large_HavenCIT_RATE
		scalar p2 = r(p)
	outreg2 using 5_Size, append ctitle("$+\delta_t+\zeta_i$") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							nonotes addnote("All models include $ D_{ij}$ and intercept. FE drops $\zeta_i$. S.e. in parantheses.", "S.e. are robust and clustered by distance. *** p<0.01, ** p<0.05, * p<0.1") ///
							 addstat("$\gamma_2=\gamma^S_3$ (p-value)", p1,"$\gamma_2=\gamma^L_3 $ (p-value)", p2)  ///
							 dec(3) keep(`Independent2') nocons nor2 noobs 						 

areg `y' `Gravity' `Independent2' `Control1' b2015.obsTime, absorb(Pair_Tal) vce(cluster distw) 
	test NonHavenCIT_RATE = Small_HavenCIT_RATE
		scalar p1 = r(p)
	test NonHavenCIT_RATE = Large_HavenCIT_RATE
		scalar p2 = r(p)
	outreg2 using 5_Size, append ctitle("$+\theta_{ij}$ (FE)") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							nonotes addnote("All models include $ D_{ij}$ and intercept. FE drops $\zeta_i$. S.e. in parantheses.", "S.e. are robust and clustered by distance. *** p<0.01, ** p<0.05, * p<0.1") ///
							 addstat("$\gamma_2=\gamma^S_3$ (p-value)", p1,"$\gamma_2=\gamma^L_3 $ (p-value)", p2)  ///
							 dec(3) keep(`Independent2') nocons nor2 noobs 		
}
							 
********************************************************************************************************
* REGRESSION TABLE SIX. 	TYPES OF FDI INCOME 
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

regress `var' `Gravity' `Independent1' `Control1', robust cluster(distw)
	test NonHavenCIT_RATE = HavenCIT_RATE
		scalar p1 = r(p)
	outreg2 using 6_Types, append ctitle("Eq. (\ref{gravity1})") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("All models include $ D_{ij}$ and a constant term. Dummies are added as specified above. FE drops $\zeta_i$. Robust standard errors in parantheses.", "Standard errors are robust and clustered by distance. *** p<0.01, ** p<0.05, * p<0.1, + p<0.15") ///
							 addstat("$\gamma_2=\gamma_3$ (p)", p1)  ///
							 dec(3) keep(`Gravity' `Independent1') nocon			

							 
areg `var' `Gravity' `Independent1' `Control1' b2015.obsTime, absorb(COU_Tal) vce(cluster distw) 
	test NonHavenCIT_RATE = HavenCIT_RATE
		scalar p1 = r(p)
	outreg2 using 6_Types, append ctitle("$+\delta_t+\zeta_i$") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("All models include $ D_{ij}$ and a constant term. Dummies are added as specified above. FE drops $\zeta_i$. Robust standard errors in parantheses.", "Standard errors are robust and clustered by distance. *** p<0.01, ** p<0.05, * p<0.1, + p<0.15") ///
							 addstat("$\gamma_2=\gamma_3$ (p)", p1)  ///
							 dec(3) keep(`Gravity' `Independent1') nocon									 

areg `var' `Gravity' `Independent1' `Control1' b2015.obsTime, absorb(Pair_Tal) vce(cluster distw) 
	test NonHavenCIT_RATE = HavenCIT_RATE
		scalar p1 = r(p)
	outreg2 using 6_Types, append ctitle("$+\theta_{ij}$") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("All models include $ D_{ij}$ and a constant term. Dummies are added as specified above. FE drops $\zeta_i$. Robust standard errors in parantheses.", "Standard errors are robust and clustered by distance. *** p<0.01, ** p<0.05, * p<0.1, + p<0.15") ///
							 addstat("$\gamma_2=\gamma_3$ (p)", p1)  ///
							 dec(3) keep(`Gravity' `Independent1') nocon									 
}	
}						 

********************************************************************************************************
* REGRESSION TABLE SEVEN. 	CFC RULES. 
********************************************************************************************************
{
regress `y' `Gravity' `Independent4' `Control1', robust cluster(distw)
	test NonHavenNonCFCCIT_RATE=HavenNonCFCCIT_RATE
			scalar p1 = r(p)
	test NonHavenCFCCIT_RATE=CFCHavenCIT_RATE
		scalar p2 = r(p)
	outreg2 using 7_CFC, replace ctitle("Eq. (\ref{gravityCFC})") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("All models include gravity variables and intercept. FE drops $\zeta_i$.", "S.e. are robust and clustered by distance. *** p<0.01, ** p<0.05, * p<0.1") ///
							 addstat("$\gamma_2=\gamma_3$ (p)", p1, "$\iota_3=\iota_4$ (p)", p2)  ///
							 dec(3) keep(`Independent4') nocons nor2 noobs 					

							 
areg `y' `Gravity' `Independent5' `Control1' b2015.obsTime, absorb(COU_Tal) vce(cluster distw)
	test NonHavenNonCFCCIT_RATE=HavenNonCFCCIT_RATE
			scalar p1 = r(p)
	test NonHavenCFCCIT_RATE=CFCHavenCIT_RATE
		scalar p2 = r(p)
	outreg2 using 7_CFC, append ctitle("$+\delta_t+\zeta_i$") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("All models include gravity variables and intercept. FE drops $\zeta_i$.", "S.e. are robust and clustered by distance. *** p<0.01, ** p<0.05, * p<0.1") ///
							 addstat("$\gamma_2=\gamma_3$ (p)", p1, "$\iota_3=\iota_4$ (p)", p2)  ///
							 dec(3) keep(`Independent4') nocons nor2 noobs 									
									 

areg `y' `Gravity' `Independent4' `Control1' b2015.obsTime, absorb(Pair_Tal) vce(cluster distw) 
	test NonHavenNonCFCCIT_RATE=HavenNonCFCCIT_RATE
			scalar p1 = r(p)
	test NonHavenCFCCIT_RATE=CFCHavenCIT_RATE
		scalar p2 = r(p)
	outreg2 using 7_CFC, append ctitle("$+\theta_{ij}$") ///
							tex(frag pretty) word label alpha(0.01, 0.05, 0.1) symbol(***, **, *) ///
							 nonotes addnote("All models include gravity variables and intercept. FE drops $\zeta_i$.", "S.e. are robust and clustered by distance. *** p<0.01, ** p<0.05, * p<0.1") ///
							 addstat("$\gamma_2=\gamma_3$ (p)", p1, "$\iota_3=\iota_4$ (p)", p2)  ///
							 dec(3) keep(`Independent4') nocons nor2 noobs 								
}
							 
*********************************************************************************************************
							 					 
log close


