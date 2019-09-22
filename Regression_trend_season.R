setwd("D:/Shivani 2017/LBS classes/Data Science/Forecasting/Regression_Forecasting")

ride <- read.csv("Amtrak data.csv", header = TRUE, stringsAsFactors = TRUE)

head(ride)

ride_ts <- ts(ride$Ridership, start = c(1991,1), end = c(2004, 3), freq = 12)

ride_ts

plot(ride_ts, xlab = "Time", ylab = "Ridership", ylim = c(1300, 2300))

library(forecast)

#### LINEAR TREND & MULTIPLICATIVE SEASONALITY ###

#no multiplciative seasonality its mentioned wrongly.
fit_tren_sea <- tslm(ride_ts ~ season)
pred <- forecast(fit_tren_sea, h=12)
pred

summary(fit_tren_sea)

accuracy(fit_tren_sea)

plot(pred, xlab = "Time", ylab = "Sales ($)", main = "Actual and forecasted values (Linear Trend with Season)")

##########

par(mfrow = c(2,1))
plot(ride_ts, xlim = c(1990, 2006))
lines(fit_tren_sea$fitted, col = "red", lty = 2)
points(pred$mean, col = "blue", pch = 16)

plot(fit_tren_sea$residuals)

par(mfrow = c(1,1))

####### QUADRATIC TREND & MULTIPLICATIVE SEASONALITY ##########

fit_quad_sea <- tslm(ride_ts ~ trend + I(trend^2) + season)
pred <- forecast(fit_quad_sea, h=12)
pred

summary(fit_quad_sea)

accuracy(fit_quad_sea)

plot(pred, xlab = "Time", ylab = "Sales ($)", main = "Actual and forecasted values (Quadratic Trend with Season)")

##########

par(mfrow = c(2,1))
plot(ride_ts, xlim = c(1990, 2006))
lines(fit_quad_sea$fitted, col = "red", lty = 2)
points(pred$mean, col = "blue", pch = 16)

plot(fit_quad_sea$residuals)

par(mfrow = c(1,1))

rm(list = ls())
