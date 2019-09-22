#Simple exponential smoothing

plot(nhtemp)
fit <- ets(nhtemp,model='ANN')

fit

pred<-forecast(fit,1)

pred

plot(pred,xlab='Year',ylab='Temp')

accuracy(fit)

#Exponential smoothing with level,slope, and seasonal components

fit1 <- ets(log(AirPassengers),model = 'AAA')

accuracy(fit1)

pred1 <- forecast(fit1,5)

pred1

plot(pred1,xlab="Time",ylab="log(Airpassengers)",main='Forecast for Air Travel')

pred1$mean <- exp(pred1$mean)



