
#### Data Pre-Processing
We utilized the Berkeley Earth dataset (available on Kaggle). The analysis includes:

Global Land Temperature by City: 8,599,212 rows, 6 columns, with missing values from 1743 to 1849, filtered from 1850 to 2013.
Global Land Temperature by Country (India): Filtered data from 1850 onwards, imputed missing values using Predictive Mean Matching, and converted to a time series dataset.
#### Data Analysis
**Temperature Change over the Globe**
- Plotted a world map showing temperature changes, with significant changes observed in Western countries, while India showed less drastic changes.
**Temperature Change in India**
- Trend & Seasonality: Decomposed the time series into seasonal, trend, and irregular components, confirming the presence of trend and seasonality in India's temperature data.
#### Methodology
**Trend Estimation**
- Used Mann-Kendall test for trend analysis.
-Differenced the data to remove trends, ensuring stationarity.
**Stationarity Estimation**
- Applied the Augmented Dickey-Fuller (ADF) test, confirming stationarity after differencing.
**Randomness Estimation**
- Used Turning Point test, indicating non-randomness in the data.
#### Forecasting
Model1: Yearly Average Temperature
- Fitted an ARIMA(3,1,2) model, selected based on AIC/BIC values and log-likelihood.
Model2: Monthly Average Temperature
- Fitted an ARIMA(2,1,0) model, validated through PACF cut-offs.
#### Validation
Employed Box-Ljung test to validate the goodness-of-fit for the models.
**Forecasting Results**
- Forecasted temperature trends for the next ten years (2013-2023), showing a continued rise in temperatures.
#### Conclusion
The analysis revealed clear trends and seasonality in temperature data, with a gradual rise in temperatures over the years, confirming global warming's impact. Urgent measures are needed to mitigate climate change for future sustainability.
