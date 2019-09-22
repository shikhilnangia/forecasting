setwd("......")

sales <- read.csv("Soft Drink Sales.csv", header = TRUE, stringsAsFactors = TRUE)

head(sales)

sales_ts <- ts(sales$Sales, start = c(1997, 1), frequency = 4)

sales_ts

library(forecast)

#par(mfrow = c(1,1))

fit <- stl(sales_ts, s.window = "period")
plot(fit)

######### SIMPLE EXPONENTIAL SMOOTHING ###########

fit <- ets(sales_ts, model = "ANN")

fit

pred <- forecast(fit, 4)

pred

plot(pred, xlab = "Year", ylab = "Temperature (F)", main = "New Heaven Annual Mean Temperature")

accuracy(fit)

##########  Visualizing Fit  #################

plot(sales_ts)
lines(fit$fitted, col = "red", lty = 2)
points(pred$mean, col = "blue", pch = 16)

######### HOLT's EXPONENTIAL SMOOTHING ###########

fit_H <- ets(sales_ts, model = "AAN")

fit_H

pred <- forecast(fit_H, 4)

pred

plot(pred, xlab = "Year", ylab = "Temperature (F)", main = "New Heaven Annual Mean Temperature")

accuracy(fit_H)

##########  Visualizing Fit (HOLT's) #################

plot(sales_ts)
lines(fit_H$fitted, col = "red", lty = 2)
points(pred$mean, col = "blue", pch = 16)

######### WINTER EXPONENTIAL SMOOTHING ###########

fit_W <- ets(sales_ts, model = "AAA")

fit_W

pred <- forecast(fit_W, 4)

pred

plot(pred, xlab = "Year", ylab = "Temperature (F)", main = "New Heaven Annual Mean Temperature")

accuracy(fit_W)

##########  Visualizing Fit  #################

plot(sales_ts)
lines(fit_W$fitted, col = "red", lty = 2)
points(pred$mean, col = "blue", pch = 16)

########################################
par(mfrow = c(2,2))

plot(sales_ts)

plot(sales_ts)
lines(fit$fitted, col = "red", lty = 2)
points(pred$mean, col = "blue", pch = 16)

plot(sales_ts)
lines(fit_H$fitted, col = "red", lty = 2)
points(pred$mean, col = "blue", pch = 16)

plot(sales_ts)
lines(fit_W$fitted, col = "red", lty = 2)
points(pred$mean, col = "blue", pch = 16)

rm(list = ls())
