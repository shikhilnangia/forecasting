ride <- read.csv("Amtrak data.csv", header = TRUE, stringsAsFactors = TRUE)

head(ride)

ride_ts <- ts(ride$Ridership, start = c(1991,1), end = c(2004, 3), freq = 12)

ride_ts

plot(ride_ts, xlab = "Time", ylab = "Ridership", ylim = c(1300, 2300))

#### Partition data into training and validadtion ######
time = time(ride_ts)
stepsAhead <- 36    #VALIDATION FOR 36 months
nTrain <- length(ride_ts) - stepsAhead
train.ts <- window(ride_ts, start = time[1], end = time[nTrain])
valid.ts <- window(ride_ts, start = time[nTrain + 1], end = time[nTrain + stepsAhead])

##### Forecast using Linear Trend ############

train.lm <- tslm(train.ts ~ trend)
train.lm.pred <- forecast(train.lm, h = stepsAhead)
accuracy(train.lm.pred, valid.ts) #Accracy of Training & Validation Set

#----------------------------------------#

# Figure 6.2
plot(train.lm.pred, xlab = "Time", ylab = "Ridership", ylim = c(1300, 2300), bty = "l")
lines(train.lm$fitted, lwd = 2)

# Table 6.1
summary(train.lm)

# Figure 3-2
plot(train.lm.pred, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time", bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "", flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1))) 
lines(train.lm$fitted, lwd = 2, col = "red", lty = 2)
lines(stepsAhead)
lines(c(2004.25 - 3, 2004.25 - 3), c(0, 3500)) 

lines(c(2004.25, 2004.25), c(0, 3500))
text(1996.25, 2500, "Training")
text(2002.75, 2500, "Validation")
text(2005.25, 2500, "Future")
arrows(2004 - 3, 2450, 1991.25, 2450, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5 - 3, 2450, 2004, 2450, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5, 2450, 2006, 2450, code = 3, length = 0.1, lwd = 1, angle = 30)


lines(valid.ts)

rm(list = ls())