clear
****Homework 6****
import excel "C:\Users\Sreeram S R\OneDrive\Desktop\Spring 2024\ECON 641 Time Series Econometrics\Final Project\Exchange Rate.xlsx", firstrow clear


****Understanding the data****
describe
****Setting Time-Series****
tsset observation_date
****Plotting Time-Series****
tsline DTWEXBGS
 ****Plotting Auto-correlation****
ac DTWEXBGS
****PLotting Partial-Autocorrelation****
pac DTWEXBGS
gen t = _n
tsset t


****Two variables related to the Nominal Broad US Dollar Index is: *Crude Oil Prices*
*Connection of crude oil prices to NBUSDX:*
*If the dollar is strong, it takes more USD to buy things which is priced in USD. Expensive for other countries. Vice-versa when the dollar is weak.*

*CBOE Gold ETF Volatility Index*
*Connection of CBOE Index to NBUSDX:*
*When dollar strengthens, gold become unattractive (expensive) for investors. Similarly vice-versa when the dollar weakens. Increases volatility of gold prices as investors react to dollar movements.*

describe
tsset observation_date
line CRUDEOIL DTWEXBGS GOLDETF observation_date
line CRUDEOIL

tsline CRUDEOIL
ac CRUDEOIL
pac CRUDEOIL

tsline GOLDETF
ac GOLDETF
pac GOLDETF
****All of them looking non-stationary****
tsset t
*DF Test for all*
dfuller DTWEXBGS, drift regress lags(8)
*Lag 6 significant*
dfuller DTWEXBGS, drift regress lags(6)
**DF Test of Crude Oil**
dfuller CRUDEOIL, drift regress lags(8)
*Lag 5 significant*
dfuller CRUDEOIL, drift regress lags(5)
*Fail to reject the null*
**DF test of Gold Votality*
dfuller GOLDETF, drift regress lags(8)
*Lag 3 significant*
dfuller GOLDETF, drift regress lags(3)
*Fail to reject the null*

tsset observation_date
tsline d.DTWEXBGS
ac d.DTWEXBGS

tsline d.CRUDEOIL
ac d.CRUDEOIL

tsline d.GOLDETF
ac d.GOLDETF

tsset t
*DF Test differenced for all*
dfuller d.DTWEXBGS, noconstant regress lags(8)
*Lag 6 significant*
dfuller d.DTWEXBGS, noconstant regress lags(6)
**DF Test of Crude Oil**
dfuller d.CRUDEOIL, noconstant regress lags(8)
*Lag 4 significant*
dfuller d.CRUDEOIL, noconstant regress lags(4)
**DF test of Gold Votality*
dfuller d.GOLDETF, noconstant regress lags(8)
*Lag 3 significant*
dfuller d.GOLDETF, noconstant regress lags(3)
*Fail to reject the null*


*Taking first difference of all the variables to make it stationary* 
line d.CRUDEOIL d.DTWEXBGS d.GOLDETF observation_date
dfuller d.DTWEXBGS, noconstant regress lags(6)
dfuller d.CRUDEOIL, noconstant regress lags(4)
dfuller d.GOLDETF, noconstant regress lags(7)
****Results shows all of them are stationary****

*Checking the connection between DTWEXBGS, CRUDEOIL and GOLDETF*
corr d.CRUDEOIL d.GOLDETF d.DTWEXBGS
*Negative correlation between NBUSDX and CRUDEOIL (17%)*
*Positive correlation between NBUSDX and GOLDETF (14%)*

****Lag Order selection****
varsoc d.CRUDEOIL d.GOLDETF d.DTWEXBGS if t<1045
*We choose the number of lags to be 3 according to the AIC*

****Reduced Form VAR****
var d.CRUDEOIL d.GOLDETF d.DTWEXBGS if t<1045, lags(1/3) lutstats dfk small


*Checking stability*
varstable, graph

drop dfNBUSDX cons_se dfNBUSDX_l dfNBUSDX_u

****Forecast VAR****
* calculate static 1-step ahead forecast
predict ddfNBUSDX if t>1045,  xb equation(#3)
label variable ddfNBUSDX "VAR"

*ARMA(1,1)*
arima d.DTWEXBGS, arima (1,0,1), if t<1045
*Check residuals
predict r, r
twoway (line r observation_date)
wntestq r
display "adj p-valu = " chi2tail(r(df)-2,r(stat))
estat aroots
estat ic
predict r, r
wntestq r
display "adj p-valu = " chi2tail(r(df)-2,r(stat))
estat aroots
matrix absroots = r(Modulus_ar)
mata:
  absroots = st_matrix("absroots")
  highestroot = max(absroots)
  st_numscalar("highestroot", highestroot)
end
display "The highest absolute value of the AR root is: " highestroot

predict f11, y
replace f11 = . if t<1045
line (DTWEXBGS f11 t) if t>1045
gen ARMA = d.f11
line (dDTWEXBGS ddfNBUSDX ARMA t) if t>1045

twoway (dDTWEXBGS ddfNBUSDX df11 t) if t>1045

twoway (tsline dDTWEXBGS if t > 1045)(tsline ddfNBUSDX  if t > 1045, lpattern(dash))(tsline df11 if t > 1045, lpattern(dash))





* Calculate absolute forecast errors and then the mean for VAR(3)
gen abs_errorVR3 = abs(d.DTWEXBGS - ddfNBUSDX) if t>=1045
sum abs_errorVR3, mean
display "MAFE for VAR(3) = " r(mean)

* calculate 95% confidence bounds around forecast
mat SigV = e(Sigma)
mat list SigV
* NBUSDX is 3rd, so the variance of consumption error term is:
gen cons_se = sqrt(SigV[3,3])

* produce 95% confidence intervals for one-step-ahead forecast
gen dfNBUSDX_l = dfNBUSDX-2*cons_se
label variable dfNBUSDX_l "95% lower bound"

gen dfNBUSDX_u = dfNBUSDX+2*cons_se
label variable dfNBUSDX_u "95% upper bound"

* plot forecast along with data
twoway (tsline d.DTWEXBGS if t > 1045)(tsline dfNBUSDX  if t > 1045, lpattern(dash))(tsline dfNBUSDX_l if t > 1045, lpattern(dash))(tsline dfNBUSDX_u if t > 1045, lpattern(dash))

gen d1fNBUSDX = d.DTWEXBGS
****Computing my loss function****
gen D1VAR3 = cond(dfNBUSDX > d.DTWEXBGS, 1, 0)
gen D2VAR3 = cond(d1fNBUSDX[_n+1] > d.DTWEXBGS, 1, 0)
*Calculating the difference*
gen diffVR3 = d1fNBUSDX[_n+1] - d.DTWEXBGS  
gen weighted_diffVR3 = diffVR3 * 1000
gen lossVR3 = D1VAR3 * weighted_diffVR3 + (1 - D1VAR3) * D2VAR3 * weighted_diffVR3
summarize lossVR3, mean
display "The average loss of VAR(3) is " r(mean)


****Checking VAR with other lags****
var d.CRUDEOIL d.GOLDETF d.DTWEXBGS if t<1045, lags(1/4) lutstats dfk small
****Forecast VAR****
* calculate static 1-step ahead forecast
predict dfNBUSDXVR4 if t>1045,  xb equation(#3)
label variable dfNBUSDXVR4 "forecast of Nominal Broad USD"

* Calculate absolute forecast errors and then the mean for VAR(3)
gen abs_errorVR4 = abs(d.DTWEXBGS - dfNBUSDXVR4) if t>=1045
sum abs_errorVR4, mean
display "MAFE for VAR(4) = " r(mean)

****Computing my loss function****
gen D1VAR4 = cond(dfNBUSDXVR4 > d.DTWEXBGS, 1, 0)
gen D2VAR4 = cond(d1fNBUSDX[_n+1] > d.DTWEXBGS, 1, 0)
*Calculating the difference*
gen diffVR4 = d1fNBUSDX[_n+1] - d.DTWEXBGS  
gen weighted_diffVR4 = diffVR4 * 1000
gen lossVR4 = D1VAR4 * weighted_diffVR4 + (1 - D1VAR4) * D2VAR4 * weighted_diffVR4
summarize lossVR4, mean
display "The average loss of VAR(4) is " r(mean)


* calculate 95% confidence bounds around forecast
mat SigV = e(Sigma)
mat list SigV
* NBUSDX is 3rd, so the variance of consumption error term is:
gen cons_se = sqrt(SigV[3,3])

* produce 95% confidence intervals for one-step-ahead forecast
gen dfNBUSDX_l = dfNBUSDX-2*cons_se
label variable dfNBUSDX_l "95% lower bound"

gen dfNBUSDX_u = dfNBUSDX+2*cons_se
label variable dfNBUSDX_u "95% upper bound"

* plot forecast along with data
twoway (tsline d.DTWEXBGS if t > 1045)(tsline dfNBUSDX  if t > 1045, lpattern(dash))(tsline dfNBUSDX_l if t > 1045, lpattern(dash))(tsline dfNBUSDX_u if t > 1045, lpattern(dash))

var d.CRUDEOIL d.GOLDETF d.DTWEXBGS if t<1045, lags(1/5) lutstats dfk small
****Forecast VAR****
* calculate static 1-step ahead forecast
predict dfNBUSDXVR5 if t>1045,  xb equation(#3)
label variable dfNBUSDXVR5 "forecast of Nominal Broad USD"

* Calculate absolute forecast errors and then the mean for VAR(3)
gen abs_errorVR5 = abs(d.DTWEXBGS - dfNBUSDXVR5) if t>=1045
sum abs_errorVR5, mean
display "MAFE for VAR(5) = " r(mean)

****Computing my loss function****
gen D1VAR5 = cond(dfNBUSDXVR5 > d.DTWEXBGS, 1, 0)
gen D2VAR5 = cond(d1fNBUSDX[_n+1] > d.DTWEXBGS, 1, 0)
*Calculating the difference*
gen diffVR5 = d1fNBUSDX[_n+1] - d.DTWEXBGS  
gen weighted_diffVR5 = diffVR5 * 1000
gen lossVR5 = D1VAR5 * weighted_diffVR5 + (1 - D1VAR5) * D2VAR5 * weighted_diffVR5
summarize lossVR5, mean
display "The average loss of VAR(5) is " r(mean)


* estimate dynamic 16 step ahead forecast
fcast compute dfNBUSDX, step(16)
fcast graph dfNBUSDX, observed


*Granger causality test*
var d.CRUDEOIL d.GOLDETF d.DTWEXBGS if t<1045, lags(1/3) lutstats dfk small
vargranger
*We understood that GOLDETF helps in forecasting the DTWEXBGS.*
*Given the interdependence of variables, VAR model is appropriate* 
*Both D.GOLDETF and D.DTWEXBGS seems to be endogenous while D.CRUDEOIL seems exogenous.*


*Estimating out-of-sample forecast for VAR(3) model*
var d.CRUDEOIL d.GOLDETF d.DTWEXBGS if t <= 1306, lags(1/3)

*Forecasting one step ahead*
predict dDTWEXBGS_f, y 

var d.CRUDEOIL d.GOLDETF d.DTWEXBGS if t <= 1306, lags(1/3)
tsappend, add(1)
predict dDTWEXBGS_f if t == 1307
predict stdp_dDTWEXBGS_f, stdp, if t == 1307

gen lower95_dDTWEXBGS_f = dDTWEXBGS_f - 1.96*stdp_dDTWEXBGS_f if t == 1307
gen upper95_dDTWEXBGS_f = dDTWEXBGS_f + 1.96*stdp_dDTWEXBGS_f if t == 1307

list t dDTWEXBGS_f lower95_dDTWEXBGS_f upper95_dDTWEXBGS_f if t >= 1307

**Converting it to original value*
display DTWEXBGS[1306]
gen last_value = DTWEXBGS[1306]
gen DTWEXBGS_f = last_value + dDTWEXBGS_f if t == 1307
gen lower95_DTWEXBGS_f = last_value + lower95_dDTWEXBGS_f if t == 1307
gen upper95_DTWEXBGS_f = last_value + upper95_dDTWEXBGS_f if t == 1307

list t DTWEXBGS_f lower95_DTWEXBGS_f upper95_DTWEXBGS_f if t == 1307
*The prediction says that the Nominal Broad Exchange rate for 16th March 2024 would be 120.7113. 19 out of 20 times it will be between 120.4482 and 120.9743. 


****4. FORECAST****

*ARMA(2,1)*
arima DTWEXBGS, arima (2,0,1), if t<1045
*Check residuals
predict r, r
twoway (line r observation_date)
wntestq r
display "adj p-valu = " chi2tail(r(df)-3,r(stat))
estat aroots
estat ic
predict r, r
wntestq r
display "adj p-valu = " chi2tail(r(df)-3,r(stat))
estat aroots
matrix absroots = r(Modulus_ar)
mata:
  absroots = st_matrix("absroots")
  highestroot = max(absroots)
  st_numscalar("highestroot", highestroot)
end
display "The highest absolute value of the AR root is: " highestroot
drop fitted_values

predict f21, y
replace f21 = . if t<1045
line (DTWEXBGS f21 t) if t>1045
line (dDTWEXBGS df21 t) if t>1045

****Computing my loss function****
gen D1AR21 = cond(f21 > DTWEXBGS, 1, 0)
gen D2AR21 = cond(DTWEXBGS[_n+1] > DTWEXBGS, 1, 0)
*Calculating the difference*
gen diff21 = DTWEXBGS[_n+1] - DTWEXBGS  
gen weighted_diff = diff * 1000
gen loss21 = D1AR21 * weighted_diff + (1 - D1AR21) * D2AR21 * weighted_diff
summarize loss21, mean
display "The average loss of ARMA(2,1) is " r(mean)

*MSFE*
* Calculate squared forecast errors and then the mean for ARMA(2,2)
gen sq_error21 = (DTWEXBGS - f21)^2 if t>=1045
egen msfe_rdtw = mean(sq_error2)
display "MSFE for ARMA(2,1) = " msfe_rdtw
drop r

*Diebold Mariano Test on the other two best models*
*ARMA(1,1) vs ARMA(1,2)
* Calculate the differences in forecast errors
gen diff_errors_11_12 = abs_error11 - abs_error12
ac diff_errors_11_12
regress diff_errors_11_12

*ARMA(1,1) vs ARMA(2,0)
* Calculate the differences in forecast errors
gen diff_errors_11_20 = abs_error11 - abs_error20
ac diff_errors_11_20
regress diff_errors_11_20


****My loss function differenced****
gen D1AR21d = cond(df21 > dDTWEXBGS, 1, 0)
gen D2AR21d = cond(dDTWEXBGS[_n+1] > dDTWEXBGS, 1, 0)
*Calculating the difference*
gen diff21d = dDTWEXBGS[_n+1] - dDTWEXBGS  
gen weighted_diff21d = diff21d * 1000
drop loss21d
gen loss21d = D1AR21d * weighted_diff + (1 - D1AR21d) * D2AR21d * weighted_diff21d
summarize loss21d, mean
display "The average loss of ARMA(2,1) d is " r(mean)

*MSFE differeced*
gen dsq_error21 = (dDTWEXBGS - df21)^2 if t>=1045
egen dmsfe_rdtw21 = mean(sq_error21)
display "MSFE for dARMA(2,1) = " dmsfe_rdtw21
drop r


*ARMA(2,2)*
arima d.DTWEXBGS, arima (2,0,2), if t<1045
*Check residuals
predict r, r
twoway (line r observation_date)
wntestq r
display "adj p-valu = " chi2tail(r(df)-4,r(stat))
estat aroots
estat ic
predict r, r
wntestq r
display "adj p-valu = " chi2tail(r(df)-4,r(stat))
estat aroots
matrix absroots = r(Modulus_ar)
mata:
  absroots = st_matrix("absroots")
  highestroot = max(absroots)
  st_numscalar("highestroot", highestroot)
end
display "The highest absolute value of the AR root is: " highestroot
predict f22, y
replace f22 = . if t<1045
line (DTWEXBGS f22 t) if t>1045
gen df22 = d.f22
line (dDTWEXBGS df22 t) if t>1045

****Computing my loss function****
gen D1AR22 = cond(f22 > DTWEXBGS, 1, 0)
gen D2AR22 = cond(DTWEXBGS[_n+1] > DTWEXBGS, 1, 0)
*Calculating the difference*
gen diff22 = DTWEXBGS[_n+1] - DTWEXBGS  
gen weighted_diff22 = diff22 * 1000
gen loss22 = D1AR22 * weighted_diff22 + (1 - D1AR22) * D2AR22 * weighted_diff22
summarize loss11, mean
display "The average loss of ARMA(1,1) is " r(mean)
*MSFE*

* Calculate squared forecast errors and then the mean for ARMA(2,2)
gen sq_error22 = (DTWEXBGS - f22)^2 if t>=1045
egen msfe_rdtw22 = mean(sq_error22)
display "MSFE for ARMA(2,2) = " msfe_rdtw22
drop r

gen abs_error22 = abs(DTWEXBGS - f22)
gen abs_error20 = abs(DTWEXBGS - f20)
gen abs_error30 = abs(DTWEXBGS - f30)
gen abs_error40 = abs(DTWEXBGS - f40)
gen abs_error21 = abs(DTWEXBGS - f21)
gen abs_error01 = abs(DTWEXBGS - f01)
gen abs_error10 = abs(DTWEXBGS - f10)



****Diebold Mariano-Test for all the models****
*ARMA(1,1) VS ARMA(2,2)
gen diff_error11_22 = abs_error11 - abs_error22
ac diff_error11_22
regress diff_error11_22
*No significant difference between the models*

*ARMA(1,1) VS ARMA(1,0)
gen diff_error11_10 = abs_error11 - abs_error10
ac diff_error11_10
regress diff_error11_10
*No significant difference between the models*

*ARMA(1,1) VS ARMA(2,0)
gen diff_error11_20 = abs_error11 - abs_error20
ac diff_error11_20
regress diff_error11_20
*No significant difference between the models*

*ARMA(1,1) VS ARMA(2,0)
gen diff_error11_20 = abs_error11 - abs_error20
ac diff_error11_20
regress diff_error11_20
*No significant difference between the models*

*ARMA(1,1) VS ARMA(3,0)
gen diff_error11_30 = abs_error11 - abs_error30
ac diff_error11_30
regress diff_error11_30

*ARMA(1,1) VS ARMA(4,0)
gen diff_error11_40 = abs_error11 - abs_error40
ac diff_error11_40
regress diff_error11_40

*ARMA(1,1) VS ARMA(2,1)
gen diff_error11_21 = abs_error11 - abs_error21
ac diff_error11_21
regress diff_error11_21


*ARMA(1,1) VS ARMA(2,1)
gen diff_error11_21 = abs_error11 - abs_error21
ac diff_error11_21
regress diff_error11_21

*ARMA(1,1) VS ARMA(0,1)
gen diff_error11_01 = abs_error11 - abs_error01
ac diff_error11_01
regress diff_error11_01


*ARMA(1,1) VS ARMA(1,2)
gen diff_error11_12 = abs_error11 - abs_error12
ac diff_error11_12
regress diff_error11_12


*ARMA(1,1) VS VAR(4)
gen diff_error11_VR4 = abs_error11 - abs_errorVR4
ac diff_error11_VR4
regress diff_error11_VR3

drop diff_error11_VR3
*ARMA(1,1) VS VAR(3)
gen diff_error11_VR3= abs_error11 - abs_errorVR3
ac diff_error11_VR3
regress diff_error11_VR3

*ARMA(1,1) VS VAR(5)
gen diff_error11_VR5 = abs_error11 - abs_errorVR5
ac diff_error11_VR5
regress diff_error11_VR5

gen abs_errorVR3 = abs(d.DTWEXBGS - ddfNBUSDX) if t>=1045
sum abs_errorVR3, mean
display "MAFE for VAR(3) = " r(mean)



* Calculate absolute forecast errors and then the mean for VAR(3)
gen abs_errorVR4 = abs(d.DTWEXBGS - dfNBUSDXVR4) if t>=1045
sum abs_errorVR4, mean
display "MAFE for VAR(4) = " r(mean)




drop D1AR22d D2AR22d diff22d weighted_diff22d loss22d
****My loss function differenced****
gen D1AR22d = cond(df22 > dDTWEXBGS, 1, 0)
gen D2AR22d = cond(dDTWEXBGS[_n+1] > dDTWEXBGS, 1, 0)
*Calculating the difference*
gen diff22d = dDTWEXBGS[_n+1] - dDTWEXBGS  
gen weighted_diff22d = diff22d * 1000
gen loss22d = D1AR22d * weighted_diff22d + (1 - D1AR22d) * D2AR22d * weighted_diff22d
summarize loss22d, mean
display "The average loss of ARMA(2,2) d is " r(mean)

*MSFE differeced*
gen dsq_error22 = (dDTWEXBGS - df22)^2 if t>=1045
egen dmsfe_rdtw22 = mean(sq_error22)
display "MSFE for dARMA(2,2) = " dmsfe_rdtw22
drop r



*ARMA(1,1)*
arima d.DTWEXBGS, arima (1,0,1), if t<1045
*Check residuals
predict r, r
twoway (line r observation_date)
wntestq r
display "adj p-valu = " chi2tail(r(df)-2,r(stat))
estat aroots
estat ic
predict r, r
wntestq r
display "adj p-valu = " chi2tail(r(df)-2,r(stat))
estat aroots
matrix absroots = r(Modulus_ar)
mata:
  absroots = st_matrix("absroots")
  highestroot = max(absroots)
  st_numscalar("highestroot", highestroot)
end
display "The highest absolute value of the AR root is: " highestroot

predict f11, y
replace f11 = . if t<1045
line (DTWEXBGS f11 t) if t>1045
gen df11 = d.f11
line (dDTWEXBGS df11 t) if t>1045
*MSFE*
* Calculate squared forecast errors and then the mean for ARMA(2,2)
gen sq_error11 = (DTWEXBGS - f11)^2 if t>=1045
egen msfe_rdtw11 = mean(sq_error11)
display "MSFE for ARMA(1,1) = " msfe_rdtw11
drop r
gen abs_error11 = abs(DTWEXBGS - f11)
drop loss11

*Forecasting one step ahead*
predict f11, y 

arima d.DTWEXBGS, arima (1,0,1), if t<1045
tsappend, add(1)
predict ff11 if t == 1307
predict stdp_ff11, stdp, if t == 1307

gen lower95_ff11 = ff11 - 1.96*stdp_ff11 if t == 1307
gen upper95_ff11 = ff11 + 1.96*stdp_ff11 if t == 1307

list t ff11 lower95_ff11 upper95_ff11 if t >= 1307

**Converting it to original value*
display DTWEXBGS[1306]
gen last_value = DTWEXBGS[1306]
gen DTWEXBGSf11 = last_value + ff11 if t == 1307
gen lower95_DTWEXBGSf11 = last_value + lower95_ff11 if t == 1307
gen upper95_DTWEXBGS_f11 = last_value + upper95_ff11 if t == 1307

list t observation_date DTWEXBGSf11 lower95_DTWEXBGSf11 upper95_DTWEXBGS_f11 if t == 1307


*ARMA(0,0)*
arima d.DTWEXBGS, arima (0,0,0), if t<1045
*Check residuals
predict r, r
twoway (line r observation_date)
wntestq r
display "adj p-valu = " chi2tail(r(df)-2,r(stat))
estat aroots
estat ic
predict r, r
wntestq r
display "adj p-valu = " chi2tail(r(df)-2,r(stat))
estat aroots
matrix absroots = r(Modulus_ar)
mata:
  absroots = st_matrix("absroots")
  highestroot = max(absroots)
  st_numscalar("highestroot", highestroot)
end
display "The highest absolute value of the AR root is: " highestroot

predict f00, y
replace f00 = . if t<1045
line (DTWEXBGS f00 t) if t>1045
gen df00 = d.f00
line (dDTWEXBGS df00 t) if t>1045
*MSFE*
* Calculate squared forecast errors and then the mean for ARMA(2,2)
gen sq_error11 = (DTWEXBGS - f11)^2 if t>=1045
egen msfe_rdtw11 = mean(sq_error11)
display "MSFE for ARMA(1,1) = " msfe_rdtw11
drop r
gen abs_error11 = abs(DTWEXBGS - f11)
drop loss11

*Forecasting one step ahead*
predict f11, y 

arima d.DTWEXBGS, arima (0,0,0), if t<1045
tsappend, add(1)
predict ff00 if t == 1307
predict stdp_ff00, stdp, if t == 1307

gen lower95_ff00 = ff00 - 1.96*stdp_ff00 if t == 1307
gen upper95_ff00 = ff00 + 1.96*stdp_ff00 if t == 1307

list t ff00 lower95_ff00 upper95_ff00 if t >= 1307

**Converting it to original value*
display DTWEXBGS[1306]
gen last_value = DTWEXBGS[1306]
gen DTWEXBGSf00 = last_value + ff00 if t == 1307
gen lower95_DTWEXBGSf00 = last_value + lower95_ff00 if t == 1307
gen upper95_DTWEXBGS_f00 = last_value + upper95_ff00 if t == 1307

list t DTWEXBGSf00 lower95_DTWEXBGSf00 upper95_DTWEXBGS_f00 if t == 1307

drop D1AR11 D2AR11 diff11 weighted_diff11 loss11
****Computing my loss function****
gen D1AR11 = cond(f11 > DTWEXBGS, 1, 0)
gen D2AR11 = cond(DTWEXBGS[_n+1] > DTWEXBGS, 1, 0)
*Calculating the difference*
gen diff11 = DTWEXBGS[_n+1] - DTWEXBGS  
gen weighted_diff11 = diff11 * 1000
gen loss11 = D1AR11 * weighted_diff11 + (1 - D1AR11) * D2AR11 * weighted_diff11
summarize loss11, mean
display "The average loss of ARMA(1,1) is " r(mean)
drop r

****My loss function differenced****
gen D1AR11d = cond(df11 > dDTWEXBGS, 1, 0)
gen D2AR11d = cond(dDTWEXBGS[_n+1] > dDTWEXBGS, 1, 0)
*Calculating the difference*
gen diff11d = dDTWEXBGS[_n+1] - dDTWEXBGS  
gen weighted_diff11d = diff11d * 1000
gen loss11d = D1AR11d * weighted_diff11d + (1 - D1AR11d) * D2AR11d * weighted_diff11d
summarize loss11d, mean
display "The average loss of ARMA(1,1) d is " r(mean)

*MSFE differeced*
gen dsq_error11 = (dDTWEXBGS - df11)^2 if t>=1045
egen dmsfe_rdtw11 = mean(sq_error11)
display "MSFE for dARMA(1,1) = " dmsfe_rdtw11
drop r



*ARMA(1,0)*
arima d.DTWEXBGS, arima (1,0,0), if t<1045
*Check residuals
predict r, r
twoway (line r observation_date)
wntestq r
display "adj p-valu = " chi2tail(r(df)-1,r(stat))
estat aroots
estat ic
predict r, r
wntestq r
display "adj p-valu = " chi2tail(r(df)-1,r(stat))
estat aroots
matrix absroots = r(Modulus_ar)
mata:
  absroots = st_matrix("absroots")
  highestroot = max(absroots)
  st_numscalar("highestroot", highestroot)
end
display "The highest absolute value of the AR root is: " highestroot
predict f10, y
replace f10 = . if t<1045
line (DTWEXBGS f10 t) if t>1045
gen df10 = d.f10
line (dDTWEXBGS df10 t) if t>1045

drop loss10
drop D1AR10 D2AR10 diff10 weighted_diff10 loss10
****Computing my loss function****
gen D1AR10 = cond(f10 > DTWEXBGS, 1, 0)
gen D2AR10 = cond(DTWEXBGS[_n+1] > DTWEXBGS, 1, 0)
*Calculating the difference*
gen diff10 = DTWEXBGS[_n+1] - DTWEXBGS  
gen weighted_diff10 = diff10 * 1000
gen loss10 = D1AR10 * weighted_diff10 + (1 - D1AR10) * D2AR10 * weighted_diff10
summarize loss10, mean
display "The average loss of ARMA(1,0) is " r(mean)

*MSFE*
* Calculate squared forecast errors and then the mean for ARMA(2,2)
gen sq_error10 = (DTWEXBGS - f10)^2 if t>=1045
egen msfe_rdtw10 = mean(sq_error10)
display "MSFE for ARMA(1,0) = " msfe_rdtw10
drop r

****My loss function differenced****
gen D1AR10d = cond(df10 > dDTWEXBGS, 1, 0)
gen D2AR10d = cond(dDTWEXBGS[_n+1] > dDTWEXBGS, 1, 0)
*Calculating the difference*
gen diff10d = dDTWEXBGS[_n+1] - dDTWEXBGS  
gen weighted_diff10d = diff10d * 1000
gen loss10d = D1AR10d * weighted_diff10d + (1 - D1AR10d) * D2AR10d * weighted_diff10d
summarize loss10d, mean
display "The average loss of ARMA(1,0) d is " r(mean)

*MSFE differeced*
gen dsq_error10 = (dDTWEXBGS - df10)^2 if t>=1045
egen dmsfe_rdtw10 = mean(sq_error10)
display "MSFE for dARMA(1,0) = " dmsfe_rdtw10
drop r



*ARMA(2,0)*
arima d.DTWEXBGS, arima (2,0,0), if t<1045
*Check residuals
predict r, r
twoway (line r observation_date)
wntestq r
display "adj p-valu = " chi2tail(r(df)-2,r(stat))
estat aroots
estat ic
predict r, r
wntestq r
display "adj p-valu = " chi2tail(r(df)-2,r(stat))
estat aroots
matrix absroots = r(Modulus_ar)
mata:
  absroots = st_matrix("absroots")
  highestroot = max(absroots)
  st_numscalar("highestroot", highestroot)
end
display "The highest absolute value of the AR root is: " highestroot
predict f20, y
replace f20 = . if t<1045
line (DTWEXBGS f20 t) if t>1045
gen df20 = d.f20
line (dDTWEXBGS df20 t) if t>1045

drop loss20 D1AR20 D2AR20 diff20 weighted_diff20 loss20
****Computing my loss function (Original)****
gen D1AR20 = cond(f20 > DTWEXBGS, 1, 0)
gen D2AR20 = cond(DTWEXBGS[_n+1] > DTWEXBGS, 1, 0)
*Calculating the difference*
gen diff20 = DTWEXBGS[_n+1] - DTWEXBGS  
gen weighted_diff20 = diff20 * 1000
gen loss20 = D1AR20 * weighted_diff20 + (1 - D1AR20) * D2AR20 * weighted_diff20
summarize loss20, mean
display "The average loss of ARMA(2,0) is " r(mean)

****My loss function differenced****
gen D1AR20d = cond(df20 > dDTWEXBGS, 1, 0)
gen D2AR20d = cond(dDTWEXBGS[_n+1] > dDTWEXBGS, 1, 0)
*Calculating the difference*
gen diff20 = dDTWEXBGS[_n+1] - dDTWEXBGS  
gen weighted_diff = diff * 1000
gen loss20d = D1AR20d * weighted_diff + (1 - D1AR20d) * D2AR20d * weighted_diff
summarize loss20d, mean
display "The average loss of ARMA(2,0) d is " r(mean)

*MSFE*
* Calculate squared forecast errors and then the mean for ARMA(2,2)
gen sq_error20 = (DTWEXBGS - f20)^2 if t>=1045
egen msfe_rdtw20 = mean(sq_error20)
display "MSFE for ARMA(2,0) = " msfe_rdtw20
drop r

*MSFE differeced*
gen dsq_error20 = (dDTWEXBGS - df20)^2 if t>=1045
egen dmsfe_rdtw20 = mean(sq_error20)
display "MSFE for dARMA(2,0) = " dmsfe_rdtw20
drop r

*ARMA(3,0)*
arima d.DTWEXBGS, arima (3,0,0), if t<1045
*Check residuals
predict r, r
twoway (line r observation_date)
wntestq r
display "adj p-valu = " chi2tail(r(df)-3,r(stat))
estat aroots
estat ic
predict r, r
wntestq r
display "adj p-valu = " chi2tail(r(df)-3,r(stat))
estat aroots
matrix absroots = r(Modulus_ar)
mata:
  absroots = st_matrix("absroots")
  highestroot = max(absroots)
  st_numscalar("highestroot", highestroot)
end
display "The highest absolute value of the AR root is: " highestroot
predict f30, y
replace f30 = . if t<1045
line (DTWEXBGS f30 t) if t>1045
gen df30 = d.f30
line (dDTWEXBGS df30 t) if t>1045

drop D1AR30 D2AR30 diff30 weighted_diff loss30
****Computing my loss function****
gen D1AR30 = cond(f30 > DTWEXBGS, 1, 0)
gen D2AR30 = cond(DTWEXBGS[_n+1] > DTWEXBGS, 1, 0)
*Calculating the difference*
gen diff30 = DTWEXBGS[_n+1] - DTWEXBGS  
gen weighted_diff = diff * 1000
gen loss30 = D1AR30 * weighted_diff + (1 - D1AR30) * D2AR30 * weighted_diff
summarize loss30, mean
display "The average loss of ARMA(3,0) is " r(mean)
****My loss function differenced****
gen D1AR30d = cond(df30 > dDTWEXBGS, 1, 0)
gen D2AR30d = cond(dDTWEXBGS[_n+1] > dDTWEXBGS, 1, 0)
*Calculating the difference*
gen diff30d = dDTWEXBGS[_n+1] - dDTWEXBGS  
gen weighted_diff30d = diff30d * 1000
gen loss30d = D1AR30d * weighted_diff30d + (1 - D1AR30d) * D2AR30d * weighted_diff
summarize loss30d, mean
display "The average loss of ARMA(3,0) d is " r(mean)

*MSFE*
* Calculate squared forecast errors and then the mean for ARMA(2,2)
gen sq_error30 = (DTWEXBGS - f30)^2 if t>=1045
egen msfe_rdtw30 = mean(sq_error30)
display "MSFE for ARMA(3,0) = " msfe_rdtw30

*MSFE differeced*
* Calculate squared forecast errors and then the mean for ARMA(2,2)
gen dsq_error30 = (dDTWEXBGS - df30)^2 if t>=1045
egen dmsfe_rdtw30 = mean(sq_error30)
display "MSFE for dARMA(3,0) = " dmsfe_rdtw30


drop r

*ARMA(4,0)*
arima d.DTWEXBGS, arima (4,0,0), if t<1045
*Check residuals
predict r, r
twoway (line r observation_date)
wntestq r
display "adj p-valu = " chi2tail(r(df)-4,r(stat))
estat aroots
estat ic
predict r, r
wntestq r
display "adj p-valu = " chi2tail(r(df)-4,r(stat))
estat aroots
matrix absroots = r(Modulus_ar)
mata:
  absroots = st_matrix("absroots")
  highestroot = max(absroots)
  st_numscalar("highestroot", highestroot)
end
display "The highest absolute value of the AR root is: " highestroot

predict f40, y
replace f40 = . if t<1045
line (DTWEXBGS f40 t) if t>1045
gen df40 = d.f40
line (dDTWEXBGS df40 t) if t>1045

****Computing my loss function****
gen D1AR40 = cond(f40 > DTWEXBGS, 1, 0)
gen D2AR40 = cond(DTWEXBGS[_n+1] > DTWEXBGS, 1, 0)
*Calculating the difference*
gen diff40 = DTWEXBGS[_n+1] - DTWEXBGS  
gen weighted_diff40 = diff40 * 1000
gen loss40 = D1AR40 * weighted_diff + (1 - D1AR40) * D2AR40 * weighted_diff40
summarize loss40, mean
display "The average loss of ARMA(4,0) is " r(mean)

*MSFE*
* Calculate squared forecast errors and then the mean for ARMA(2,2)
gen sq_error40 = (DTWEXBGS - f40)^2 if t>=1045
egen msfe_rdtw40 = mean(sq_error40)
display "MSFE for ARMA(4,0) = " msfe_rdtw40
drop r

****My loss function differenced****
gen D1AR40d = cond(df40 > dDTWEXBGS, 1, 0)
gen D2AR40d = cond(dDTWEXBGS[_n+1] > dDTWEXBGS, 1, 0)
*Calculating the difference*
gen diff40d = dDTWEXBGS[_n+1] - dDTWEXBGS  
gen weighted_diff40d = diff40d * 1000
gen loss40d = D1AR40d * weighted_diff40d + (1 - D1AR40d) * D2AR40d * weighted_diff
summarize loss40d, mean
display "The average loss of ARMA(4,0) d is " r(mean)

*MSFE differeced*
* Calculate squared forecast errors and then the mean for ARMA(2,2)
gen dsq_error40 = (dDTWEXBGS - df40)^2 if t>=1045
egen dmsfe_rdtw40 = mean(sq_error40)
display "MSFE for dARMA(4,0) = " dmsfe_rdtw40
drop r

*ARMA(0,1)*
arima d.DTWEXBGS, arima (0,0,1), if t<1045
*Check residuals
predict r, r
twoway (line r observation_date)
wntestq r
display "adj p-valu = " chi2tail(r(df)-1,r(stat))
estat aroots
estat ic
predict r, r
wntestq r
display "adj p-valu = " chi2tail(r(df)-1,r(stat))
estat aroots
matrix absroots = r(Modulus_ar)
mata:
  absroots = st_matrix("absroots")
  highestroot = max(absroots)
  st_numscalar("highestroot", highestroot)
end
display "The highest absolute value of the AR root is: " highestroot
predict f01, y
replace f01 = . if t<1045
line (DTWEXBGS f01 t) if t>1045
gen df01 = d.f01
line (dDTWEXBGS df01 t) if t>1045



****Computing my loss function****
gen D1AR01 = cond(f01 > DTWEXBGS, 1, 0)
gen D2AR01 = cond(DTWEXBGS[_n+1] > DTWEXBGS, 1, 0)
*Calculating the difference*
gen diff01 = DTWEXBGS[_n+1] - DTWEXBGS  
gen weighted_diff01 = diff * 1000
gen loss01 = D1AR01 * weighted_diff01 + (1 - D1AR01) * D2AR01 * weighted_diff
summarize loss01, mean
display "The average loss of ARMA(0,1) is " r(mean)

*MSFE*
* Calculate squared forecast errors and then the mean for ARMA(2,2)
gen sq_error01 = (DTWEXBGS - f01)^2 if t>=1045
egen msfe_rdtw01 = mean(sq_error01)
display "MSFE for ARMA(0,1) = " msfe_rdtw01
drop r

****My loss function differenced****
gen D1AR01d = cond(df01 > dDTWEXBGS, 1, 0)
gen D2AR01d = cond(dDTWEXBGS[_n+1] > dDTWEXBGS, 1, 0)
*Calculating the difference*
gen diff01d = dDTWEXBGS[_n+1] - dDTWEXBGS  
gen weighted_diff01d = diff01d * 1000
gen loss01d = D1AR01d * weighted_diff01d + (1 - D1AR01d) * D2AR01d * weighted_diff
summarize loss01d, mean
display "The average loss of ARMA(0,1) d is " r(mean)

*MSFE differeced*
* Calculate squared forecast errors and then the mean for ARMA(2,2)
gen dsq_error01 = (dDTWEXBGS - df01)^2 if t>=1045
egen dmsfe_rdtw01 = mean(sq_error01)
display "MSFE for dARMA(0,1) = " dmsfe_rdtw01
drop r



*ARMA(1,2)*
arima d.DTWEXBGS, arima (1,0,2), if t<1045
*Check residuals
predict r, r
twoway (line r observation_date)
wntestq r
display "adj p-valu = " chi2tail(r(df)-3,r(stat))
estat aroots
estat ic
predict r, r
wntestq r
display "adj p-valu = " chi2tail(r(df)-3,r(stat))
estat aroots
matrix absroots = r(Modulus_ar)
mata:
  absroots = st_matrix("absroots")
  highestroot = max(absroots)
  st_numscalar("highestroot", highestroot)
end
display "The highest absolute value of the AR root is: " highestroot
predict f12, y
replace f12 = . if t<1045
line (DTWEXBGS f12 t) if t>1045
gen df12 = d.f12
line (dDTWEXBGS df12 t) if t>1045


****Computing my loss function****
gen D1AR12 = cond(f12 > DTWEXBGS, 1, 0)
gen D2AR12 = cond(DTWEXBGS[_n+1] > DTWEXBGS, 1, 0)
*Calculating the difference*
gen diff12 = DTWEXBGS[_n+1] - DTWEXBGS  
gen weighted_diff12 = diff12 * 1000
gen loss12 = D1AR12* weighted_diff12 + (1 - D1AR12) * D2AR12 * weighted_diff12
summarize loss12, mean
display "The average loss of ARMA(1,2) is " r(mean)

*MSFE*
* Calculate squared forecast errors and then the mean for ARMA(2,2)
gen sq_error12 = (DTWEXBGS - f12)^2 if t>=1045
egen msfe_rdtw12 = mean(sq_error12)
display "MSFE for ARMA(1,2) = " msfe_rdtw12
drop r

****My loss function differenced****
gen D1AR12d = cond(df12 > dDTWEXBGS, 1, 0)
gen D2AR12d = cond(dDTWEXBGS[_n+1] > dDTWEXBGS, 1, 0)
*Calculating the difference*
gen diff12d = dDTWEXBGS[_n+1] - dDTWEXBGS  
gen weighted_diff12d = diff12d * 1000
gen loss12d = D1AR12d * weighted_diff12d + (1 - D1AR12d) * D2AR12d * weighted_diff
summarize loss12d, mean
display "The average loss of ARMA(1,2) d is " r(mean)

*MSFE differeced*
* Calculate squared forecast errors and then the mean for ARMA(2,2)
gen dsq_error12 = (dDTWEXBGS - df12)^2 if t>=1045
egen dmsfe_rdtw12 = mean(sq_error12)
display "MSFE for dARMA(1,2) = " dmsfe_rdtw12
drop r
gen abs_error12 = abs(dDTWEXBGS - df12)

*ARMA(1,3)*
arima DTWEXBGS, arima (1,0,3), if t<1045
*Check residuals
predict r, r
twoway (line r observation_date)
wntestq r
display "adj p-valu = " chi2tail(r(df)-4,r(stat))
estat aroots
estat ic
predict r, r
wntestq r
display "adj p-valu = " chi2tail(r(df)-4,r(stat))
estat aroots
matrix absroots = r(Modulus_ar)
mata:
  absroots = st_matrix("absroots")
  highestroot = max(absroots)
  st_numscalar("highestroot", highestroot)
end
display "The highest absolute value of the AR root is: " highestroot
predict f11, y
replace f11 = . if t<1045
line (DTWEXBGS f11 t) if t>1045
gen df11 = d.f11
line (dDTWEXBGS df11 t) if t>1045

****Computing my loss function****
gen D1AR21 = cond(f21 > DTWEXBGS, 1, 0)
gen D2AR21 = cond(DTWEXBGS[_n+1] > DTWEXBGS, 1, 0)
*Calculating the difference*
gen diff21 = DTWEXBGS[_n+1] - DTWEXBGS  
gen weighted_diff = diff * 1000
gen loss21 = D1AR21 * weighted_diff + (1 - D1AR21) * D2AR21 * weighted_diff
summarize loss, mean
display "The average loss of ARMA(2,1) is " r(mean)
*MSFE*
* Calculate squared forecast errors and then the mean for ARMA(2,2)
gen sq_error11 = (DTWEXBGS - f11)^2 if t>=1045
egen msfe_rdtw11 = mean(sq_error11)
display "MSFE for ARMA(1,1) = " msfe_rdtw11
drop r

*ARMA(1,4)*
arima DTWEXBGS, arima (1,0,4), if t<1045
*Check residuals
predict r, r
twoway (line r observation_date)
wntestq r
display "adj p-valu = " chi2tail(r(df)-5,r(stat))
estat aroots
estat ic
predict r, r
wntestq r
display "adj p-valu = " chi2tail(r(df)-5,r(stat))
estat aroots
matrix absroots = r(Modulus_ar)
mata:
  absroots = st_matrix("absroots")
  highestroot = max(absroots)
  st_numscalar("highestroot", highestroot)
end
display "The highest absolute value of the AR root is: " highestroot
predict f11, y
replace f11 = . if t<1045
line (DTWEXBGS f11 t) if t>1045
gen df11 = d.f11
line (dDTWEXBGS df11 t) if t>1045

****Computing my loss function****
gen D1AR21 = cond(f21 > DTWEXBGS, 1, 0)
gen D2AR21 = cond(DTWEXBGS[_n+1] > DTWEXBGS, 1, 0)
*Calculating the difference*
gen diff21 = DTWEXBGS[_n+1] - DTWEXBGS  
gen weighted_diff = diff * 1000
gen loss21 = D1AR21 * weighted_diff + (1 - D1AR21) * D2AR21 * weighted_diff
summarize loss, mean
display "The average loss of ARMA(2,1) is " r(mean)
*MSFE*
* Calculate squared forecast errors and then the mean for ARMA(2,2)
gen sq_error11 = (DTWEXBGS - f11)^2 if t>=1045
egen msfe_rdtw11 = mean(sq_error11)
display "MSFE for ARMA(1,1) = " msfe_rdtw11
drop r



