# Forecasting the Nominal Broad US Dollar Index Using Time-Series Models

## Objective
The primary goal of this project is to forecast the Nominal Broad US Dollar Index (NBUSDX) for March 16, 2024, using time-series models. The forecast will inform investment decisions, specifically determining whether to purchase 1000 CHF worth of shares in international stocks, with a focus on Swiss stocks, based on predictions of the NBUSDX's movement.

## Actions
1. **Data Collection:**
   - Daily data was collected for the Nominal Broad US Dollar Index, Crude Oil Prices, and Gold ETF Volatility Index from March 15, 2019, to March 15, 2024.

2. **Data Analysis & Transformation:**
   - Performed stationarity tests (e.g., Augmented Dickey-Fuller test) on all variables, confirming non-stationarity.
   - Transformed the series into stationary by taking the first difference.

3. **Model Selection:**
   - **Univariate Models:** Compared 9 ARMA models based on criteria like residual white noise and BIC values.
   - **Multivariate Models:** Evaluated a VAR model including Crude Oil Prices and Gold ETF Index.
   - ARMA(11) was selected as the best model for forecasting the NBUSDX due to its superior performance metrics.

4. **Forecasting & Investment Strategy:**
   - Forecasted the NBUSDX for March 16, 2024, using ARMA(11).
   - Based on the forecast, decided to purchase 1000 CHF worth of shares on March 15, 2024.

## Results
- **ARMA(11) Model Performance:**
  - The forecasted NBUSDX for March 16, 2024, is between 120.7636 and 120.821, with the actual rate recorded at 120.9572.
  - The model accurately predicted an increase in the NBUSDX, validating the decision to purchase shares.

- **Multivariate Model Insights:**
  - The VAR model with 3 lags was tested but was not as predictive as the ARMA model.
  - Granger causa

