# Comparing the Accuracy and Robustness of Various Automated Time Series Forecasting Packages

To address the use case of multivariate, high-frequency time series data, several packages have been created for Python and R. These packages use methods including ARIMA, exponential smoothing, seasonal decomposition, and even recurrent neural networks. They automate the fitting of model parameters and have built-in forecasting methods. Some of these packages also ensemble multiple single models together, or forecast prediction intervals in addition to point forecasts.

Some work has been done to make an objective evaluation of the accuracy of these packages compared to each other, but the work is minimal, and in particular has often disregarded time series at frequencies higher than daily. Additionally, the package ecosystem is evolving and requires frequent updates. At present, the ecosystem of such packages is more complete in R than in Python, so this study will be done in R, but that choice does not reflect a belief that one _language_ is inherently better for time series forecasting than another.

We will consider the following packages:
* auto.arima (R)
* forecastHybrid (R)
* prophet/fbprophet (R/Python)

As for the data, we will use some of the standard data sources used for forecasting competitions...
We will also include hourly data from ... (stock ticks, twitter, Google search?)
