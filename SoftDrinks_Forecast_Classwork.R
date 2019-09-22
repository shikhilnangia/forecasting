dataset<-read.csv("Soft Drink Sales.csv",header = TRUE  )

tsoftdrinks <- ts(dataset$Sales,start=c(1997,1),frequency = 4)
par(mfrow = c(1,1))
plot(tsoftdrinks,xlab="Time",ylab="Quarterly Sales",main='Sales(1997-2012)')

tsoftdrinks

fit.ann <- ets(tsoftdrinks,model='ANN')

fit.ann


fit.aan <- ets(tsoftdrinks,model='AAN')

fit.aan


fit.aaa <- ets(tsoftdrinks,model='AAA')

fit.aaa

plot(fit.aaa)
plot(fit.aan)
plot(fit.ann)



pred<-forecast(fit,1)

pred

plot(pred,xlab='Time',ylab='Quarterly Sales')

accuracy(fit)

fit_decomposition <- stl(tsoftdrinks,s.window = "period")
plot(fit_decomposition)