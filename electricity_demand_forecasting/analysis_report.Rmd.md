The time series study of daily electricity demand for six big US energy companies from January 2020 to December 2024 is summed up in this report.
The goal was utilizing a dataset containing 10,961 daily records spanning a five-year period from January 1, 2020, to December 31, 2024 that is segmented across six distinct US power system operators/utilities:
1. Duke Energy Progress East (CPLE)
2. Duke Energy Carolinas (DUK)
3. New York Independent System Operator (NYIS)
4. PJM Interconnection, LLC (PJM)
5. Southern Company Services, Inc. Trans (SOCO)
6. Tennessee Valley Authority (TVA)
and generate short-term forecasts for each operator's future electricity demand. Exploratory Data Analysis (EDA) confirmed a strong weekly seasonal pattern, with demand consistently dropping on weekends. PJM Interconnection, LLC exhibits the highest average demand, with different overall magnitudes among
operators.
In order to predict future demand, firstly I used the Seasonal Autoregressive Integrated Moving Average (SARIMA) modeling approach, classifying all series as stationary. Although this manual model was successful in capturing the weekly seasonality and observable trend, the resulting projections showed poor
predictive accuracy.
Then I used ARIMA, ETS(Error, Trend, Seasonality) and SARIMA(auto.arima()) methods and then predicted the peak values.
