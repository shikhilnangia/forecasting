install.packages("forecast")

library(forecast)

##### Simple Moving Average ########

plot(Nile, main = "Raw time series")

plot(ma(Nile, 3), main = "Simple Moving Averages (k = 3)")

plot(ma(Nile, 7), main = "Simple Moving Averages (k = 7)")

plot(ma(Nile, 15), main = "Simple Moving Averages (k = 15)")

##### Simple Moving Average for better visualization ########

lim <- c(min(Nile), max(Nile))



par(mfrow = c(2,2))

plot(Nile, main = "Raw time series")

plot(ma(Nile, 3), main = "Simple Moving Averages (k = 3)", ylim = lim)

plot(ma(Nile, 7), main = "Simple Moving Averages (k = 7)", ylim = lim)

plot(ma(Nile, 15), main = "Simple Moving Averages (k = 15)", ylim = lim)


par(mfrow = c(1,1))

####### Seasonal Decomposition using stl() #########

par(mfrow = c(2,1))

plot(AirPassengers)

lAirPassengers <- log(AirPassengers)

plot(lAirPassengers, ylab = "log(AirPassengers)")

fit <- stl(lAirPassengers, s.window = "period")
plot(fit)

fit$time.series

exp(fit$time.series)

# Additional plot help to visualize a seasonal decomposition.

par(mfrow = c(2,1))

monthplot(AirPassengers, xlab="", ylab = "")

seasonplot(AirPassengers, year.labels = "TRUE", main = "")

rm(list = ls())
