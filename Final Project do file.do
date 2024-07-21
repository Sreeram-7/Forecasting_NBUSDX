clear
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


****Converting it to stationary****
****Taking Seasonal difference to transform the data into stationary****
gen diff_DTWEXBGS_7d = DTWEXBGS - L7.DTWEXBGS
****Plotting seasonally differenced series****
tsline diff_DTWEXBGS_7d
****Plotting the Autocorrelation Function (ACF) for the differenced series****
ac diff_DTWEXBGS_7d
****Plotting the Partial Autocorrelation Function (PACF) for the differenced series****
pac diff_DTWEXBGS_7d

count
gen start_date = date("15mar2019", "DMY")

****Forecasting for the next day****
arima diff_DTWEXBGS_7d, arima(1,0,4)
predict pdiff_DTWEXBGS_7d, t0(start_date)
label variable pdiff_DTWEXBGS_7d "Forecast of Nominal Exchange Rate"
predict se, stdp

***Generating 95% confidence intervals****
gen pdiff_DTWEXBGS_7d95l = pdiff_DTWEXBGS_7d-2*se
label variable pdiff_DTWEXBGS_7d95l "Lower Bound of Confidence Interval"

***Generating 95% confidence intervals****
gen pdiff_DTWEXBGS_7d95u = pdiff_DTWEXBGS_7d+2*se
label variable pdiff_DTWEXBGS_7d95u "Upper Bound of Confidence Interval"

****Plotting****
twoway (tsline diff_DTWEXBGS_7d if observation_date > start_date) ///
       (tsline pdiff_DTWEXBGS_7d if observation_date > start_date, lpattern(solid)) ///
       (tsline pdiff_DTWEXBGS_7d95l if observation_date > start_date, lpattern(dash) lcolor(green)) ///
       (tsline pdiff_DTWEXBGS_7d95u if observation_date > start_date, lpattern(dash) lcolor(green)) ///
       , ytitle("Percent")

describe

****Question 5****
* Estimate the ARMA(1,4) model using the full sample
*Our data goes till 15th March 2024. Now we predicting forecast of 16th March 2024*
arima diff_DTWEXBGS_7d, arima(1,0,4)
tsappend, add(1)
gen se = e(sigma)

predict f16_March_2024, y

gen lower_9540 = f16_March_2024 - (1.96 * se)
gen upper_9540 = f16_March_2024 + (1.96 * se)

display f16_March_2024
display lower_95






