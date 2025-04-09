#install.packages(c("xts", "zoo", "TTR", "timeDate", "timeSeries", "quantmod", "lubridate","forecast","tseries"))
#library(xts)
#library(zoo)
#library(TTR)
#library(timeDate)
#library(timeSeries)
#library(quantmod)
#library(lubridate)
#library(forecast)
#library(tseries)

getSymbols('AAPL', from = '2024-06-01', to = '2025-03-01')
View(AAPL)
# Reduce margins
par(mar = c(4, 4, 2, 2))  # Default is c(5, 4, 4, 2)

# Retry the chartSeries command
chartSeries(AAPL, subset = index(AAPL) >= tail(index(AAPL), 1) - months(12), 
            type = 'auto')
addBBands()
# Assigning columns of dataset
Open_prices = AAPL[,1]
High_prices = AAPL[,2]
Low_prices = AAPL[,3]
Close_prices = AAPL[,4]
Volume_prices = AAPL[,5]
Adjusted_prices = AAPL[,6]

par(mfrow = c(1, 1), mar = c(4, 4, 2, 1))  

plot(Open_prices, main = 'Opening price of stock (Over a given period)', xlab = '', xaxt = 'n')
plot(High_prices, main = 'Highest price of stock (Over a given period)', xlab = '', xaxt = 'n')
plot(Low_prices, main = 'Lowest price of stock (Over a given period)', xlab = '', xaxt = 'n')
plot(Close_prices, main = 'Closing price of stock (Over a given period)', xlab = '', xaxt = 'n')
plot(Volume_prices, main = 'Volume price of stock (Over a given period)', xlab = '', xaxt = 'n')
plot(Adjusted_prices, main = 'Adjusted price of stock (Over a given period)', xlab = '', xaxt = 'n')


par(mfrow = c(1,2)) #par is used to set a parameter for the intended graphic
Acf(Close_prices, main = 'ACF for differenced series')
Pacf(Close_prices, main = 'PACF for differenced series ', col = '#cc0000')
print(adf.test(Close_prices))
auto.arima(Close_prices, seasonal = FALSE)
fitA = auto.arima(Close_prices, seasonal = FALSE) #auto Arima 1, 2, 2 (1- first-order autoregresive term; 2- two differences, 2- moving avergae term of order 2) all stem from auto.arima function utilizing the close_prices
tsdisplay(residuals(fitA), lag.max = 30, main='(1,2,2) Model Residuals')
auto.arima(Close_prices, seasonal = FALSE)

fitB = arima(Close_prices, order=c(1,2,4))  #custom arima of 1(autoregressive order of 1), 2(two differences), 4(moving avg order of 4)
tsdisplay(residuals(fitB), lag.max = 30, main='(1,2,4) Model Residuals')


fitC = arima(Close_prices, order = c(6,1,4))
tsdisplay(residuals(fitC), lag.max=30, main='(6,1,4) Model Residuals') #tried using 5 instead of 6 but it was non-stationary


fitD = arima(Close_prices, order = c(1,1,1))
tsdisplay(residuals(fitD), lag.max=30, main='(1,1,1) Model Residuals')
par(mfrow=c(2,2))

# auto arima (2,0,2)
var<-150 #variable for how many days of projection
fcast1<-forecast(fitA, h=var)
plot(fcast1)
# custom arima (3,0,3)
fcast2 <- forecast(fitB, h=var)
plot(fcast2)
fcast3 <- forecast(fitC, h=var)
plot(fcast3)
fcast4 <- forecast(fitD, h=var)
plot(fcast4)

accuracy(fcast1)
accuracy(fcast2)
accuracy(fcast3)
accuracy(fcast4)









