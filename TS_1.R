# Creating a Time Series Object

sales <- c(18, 33, 41, 7, 34, 35, 24, 25, 24, 21, 25, 20, 22, 31, 40, 29, 25, 21, 22, 54, 31, 25, 26, 35)

tsales <- ts(sales, start = c(2003, 1), frequency = 12)
tsales

plot(tsales)

plot(tsales, xlab = "Time", ylab = "Monthly sales", main = "Sales(2003-2004)")

start(tsales)

end(tsales)

frequency(tsales)

tsales.subset <- window(tsales, start = c(2003, 5), end = c(2004, 6))

tsales.subset
 
plot(tsales.subset)

rm(list = ls())
