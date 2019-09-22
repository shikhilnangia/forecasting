library(forecast)

######### SIMPLE EXPONENTIAL SMOOTHING ###########

plot(nhtemp)

fit <- ets(nhtemp, model = "ANN")

fit

pred <- forecast(fit, 1)

pred

plot(pred, xlab = "Year", ylab = "Temperature (F)", main = "New Heaven Annual Mean Temperature")

accuracy(fit)

#### HOLT & HOLT WINTERS EXPONENTIAL SMOOTHING ######

# Exponential Smoothing with level, slope and seasonal components

fit1 <- ets(log(AirPassengers), model = "AAA")

fit1

accuracy(fit1)

pred1 <- forecast(fit1, 5)

pred1

plot(pred1, xlab = "Time", ylab = "Log(Airpassengers)", main = "Forecast for air travel")

pred1$mean <- exp(pred1$mean)
pred1$lower <- exp(pred1$lower)
pred1$upper <- exp(pred1$upper)
p <- cbind(pred1$mean, pred1$lower, pred1$upper)

dimnames(p) [[2]] <- c("mean", "Lo 80", "Lo 95", "Hi 80", "Hi 95")

p

ets(AirPassengers)

######### Automatic exponential forecasting with ets () ######

fit2 <- ets(JohnsonJohnson)

fit2 

plot(forecast(fit2), main = "Johnson & Johnson", xlab = "Time", ylab = "Quarterly Earnings (Dollars)", flty = 2)


rm(list = ls())

