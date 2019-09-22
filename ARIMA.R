library(forecast)
library(tseries)
### 1. Ensure that the time series is stationary ###

# plot the time series to assess the stationarity

plot(Nile)

# Difference the time series to check for the trend

ndiffs(Nile)

# the series is differenced once (lag = 1 is default) and saved as dNile

dNile <- diff(Nile)

# plot the differenced time series

plot(dNile)

# Apply ADF test to check for stationarity.

adf.test(dNile)

# ADF test proves that time series is stationary, therefore, proceed further.

### 2. Identifying one or more reasonable models

# possible models are selected based on ACF and PACF plots

par(mfrow = c(2,1))

Acf(dNile)
Pacf(dNile)

par(mfrow = c(1,1))

### 3. Fitting the Models ###

fit <- arima(Nile, order = c(0,1,1))
fit

accuracy(fit)

### 4. Evaluating Model Fit ###

qqnorm(fit$residuals)
qqline(fit$residuals)

Box.test(fit$residuals, type = "Ljung-Box")

### 5. Making Forecast ###

forecast(fit, 3)

plot(forecast(fit, 3), xlab = "Year", ylab = "Annual Flow")

####### Automated ARIMA Forecasting ########
fit1 <- auto.arima(Nile)
fit1

forecast(fit1, 3)

accuracy(fit1)

plot(forecast(fit1, 3))
     
rm(list = ls())

##################Automated ARIMA on Amtrack Data#######

Amtrack <- read.csv("Amtrak data.csv", header = TRUE, stringsAsFactors = TRUE)

head(Amtrack)

ride_ts <- ts(Amtrack$Ridership, start = c(1991,1), end = c(2004, 3), freq = 12)

ride_ts

plot(ride_ts, xlab = "Time", ylab = "Ridership", ylim = c(1300, 2300))


fit1 <- auto.arima(ride_ts)
fit1

forecast(fit1, 3)

accuracy(fit1)

plot(forecast(fit1, 3))

rm(list = ls())



