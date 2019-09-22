install.packages("forecast")

library(forecast)

####Simple mooving avaerages

# ?Nile
plot(Nile, main="Raw time series")

plot(ma(Nile,3), main="Simple moving Averages(k=3)")

plot(ma(Nile,7), main="Simple moving Averages(k=7)")

plot(ma(Nile,15), main="Simple moving Averages(k=15)")

lim <- c(min(Nile),max(Nile))

par(mfrow = c(2,2))

plot(Nile, main="Raw Time series",ylim=lim)
plot(ma(Nile,3), main="Simple moving Averages(k=3)",ylim=lim)

plot(ma(Nile,7), main="Simple moving Averages(k=7)",ylim=lim)

plot(ma(Nile,15), main="Simple moving Averages(k=15)",ylim=lim)

par(mfrow = c(1,1))

#Seasonal Decomposition by Loess Smoothing
#Restriction of Seasonal Decomposition is it can be only applied to Additive Trend & Seasonality


par(mfrow = c(2,1))

plot(AirPassengers)   #Seasonality is multiplicative
lAirPassengers <- log(AirPassengers)
plot(lAirPassengers,ylab="Log AirPassengers") 
#logAirPassenger Additive Trend and Seasonality

?stl
fit <- stl(lAirPassengers,s.window = "period")
plot(fit)

fit$time.series

#head(lAirPassengers)
#lAirPassengers[0]=fit$timeseries[0]

#Taking Antilog
exp(fit$time.series)

monthplot(AirPassengers,xlab="",ylab="")
seasonplot(AirPassengers,year.labels = TRUE , main="")




