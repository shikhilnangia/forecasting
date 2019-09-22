#setwd("D:/Shivani 2017/LBS classes/Data Science 1/Data Science_2017/Forecasting/Soft drink sales")

sales <- read.csv("Soft Drink Sales.csv", header = TRUE, stringsAsFactors = TRUE)

head(sales)

sales_ts <- ts(sales$Sales, start = c(1997, 1), frequency = 4)

sales_ts

plot(sales_ts)

library(forecast)

############ Linear Trend (Additive) ############

#fit linear additive trend
fit_lin_trend <- tslm(sales_ts ~ trend)
pred <- forecast(fit_lin_trend, h=4)
pred
summary(fit_lin_trend)
accuracy(fit_lin_trend)

plot(pred, xlab = "Time", ylab = "Sales ($)", main = "Actual and forecasted values (Linear Trend)")

##########

par(mfrow = c(2,1))
plot(sales_ts)
lines(fit_lin_trend$fitted, col = "red", lty = 2)
points(pred$mean, col = "blue", pch = 16)

plot(fit_lin_trend$residuals)

par(mfrow = c(1,1))

####### Exponential Trend (Multiplicative)  ########

fit_exp_trend <- tslm(sales_ts ~ trend, lambda = 0)
#lambda=0 means conversion of y (sales) into log 
pred <- forecast(fit_exp_trend, h=4)
pred

accuracy(fit_exp_trend)

plot(pred, xlab = "Time", ylab = "Sales ($)", main = "Actual and forecasted values")

##########

par(mfrow = c(2,1))
plot(sales_ts)
lines(fit_exp_trend$fitted, col = "red", lty = 2)
points(pred$mean, col = "blue", pch = 16)

plot(fit_exp_trend$residuals)

par(mfrow = c(1,1))

############# Quadratic Trend (Amtrak Data) #############

#setwd("D:/Shivani 2017/LBS classes/Data Science 1/Data Science_2017/Forecasting/Regression_Forecasting")

ride <- read.csv("Amtrak data.csv", header = TRUE, stringsAsFactors = TRUE)

head(ride)

ride_ts <- ts(ride$Ridership, start = c(1991,1), end = c(2004, 3), freq = 12)

ride_ts

plot(ride_ts, xlab = "Time", ylab = "Ridership", ylim = c(1300, 2300))

fit_quad_trend <- tslm(ride_ts ~ trend + I(trend^2))

pred <- forecast(fit_quad_trend, h=12)
pred

accuracy(fit_quad_trend)

plot(pred, xlab = "Time", ylab = "Sales ($)", main = "Actual and forecasted values")

##########
#Also known as POLYNOMIAL TREND
par(mfrow = c(2,1))
plot(ride_ts)
lines(fit_quad_trend$fitted, col = "red", lty = 2)
points(pred$mean, col = "blue", pch = 16)

plot(fit_quad_trend$residuals)

par(mfrow = c(1,1))
##################################

rm(list = ls())

##########################
##Model with seasonality ###

fit_season <- tslm(ride_ts ~ season)

pred <- forecast(fit_season, h=12)
pred

accuracy(fit_season)

plot(pred, xlab = "Time", ylab = "Sales ($)", main = "Actual and forecasted values")

#######################################

par(mfrow = c(2,1))
plot(ride_ts)
lines(fit_season$fitted, col = "red", lty = 2)
points(pred$mean, col = "blue", pch = 16)

plot(fit_season$residuals)

par(mfrow = c(1,1))
##################################
## Model with trend and seasonality ####

fit_season_trend <- tslm(ride_ts ~ trend + I(trend^2) + season)

pred <- forecast(fit_season_trend, h=12)
pred

accuracy(fit_season_trend)

plot(pred, xlab = "Time", ylab = "Sales ($)", main = "Actual and forecasted values")

#######################################

par(mfrow = c(2,1))
plot(ride_ts)
lines(fit_season_trend$fitted, col = "red", lty = 2)
points(pred$mean, col = "blue", pch = 16)

plot(fit_season_trend$residuals)

par(mfrow = c(1,1))

rm(list = ls())



