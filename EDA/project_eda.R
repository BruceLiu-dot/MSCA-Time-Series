getwd()
setwd(dir = '/Users/nihaalz/Desktop/UChicago/2021-2022/Q3_Spring2022/Time Series/Project/')

library(readxl)
library(xts)
library(zoo)
library(tseries)
library(forecast)
library(Metrics)
library(car)

TB = read.csv('Data files/TB3MS.csv')
BY = read.csv('Data files/IRLTLT01USM156N.csv')
SP500 = read.csv('Data files/SP500.csv')


TB$DATE = as.Date(TB$DATE)
BY$DATE = as.Date(BY$DATE)
SP500$Date = as.Date(SP500$Date)

TB = xts(TB[,-1],order.by=TB[,1],)
BY = xts(BY[,-1],order.by=BY[,1],)
SP500 = xts(SP500[,-1],order.by=SP500[,1],)

colnames(TB) = '3 Month Treasury Rate'
colnames(BY) = 'Long Term Bond Yields'
colnames(SP500) = 'S&P 500 PE Ratio'

TB = TB['1974-01-01/2021-10-01']
BY = BY['1974-01-01/2021-10-01']
SP500 = SP500['1974-01-01/2021-10-01']

plot(TB,main='3-Month Treasury Bill Secondary Market Rate')
plot(BY,main='Long Term Government Bond Yields')
plot(SP500,main='S&P 500 PE Ratio')


acf(TB,main='3 Month Treasury Bill ACF')
kpss.test(TB,null='Trend')
adf.test(TB)
arima_TB = auto.arima(TB)
acf(arima_TB$residuals,main='Treasury Bill ARIMA ACF')
pacf(arima_TB$residuals,main='Treasury Bill ARIMA PACF')
plot(arima_TB$residuals,main='Treasury Bill ARIMA Residuals')



acf(BY,main='Bond Yields ACF')
kpss.test(BY,null='Trend')
adf.test(BY)
arima_BY = auto.arima(BY)
acf(arima_BY$residuals,main='Bond Yields ARIMA ACF')
pacf(arima_BY$residuals,main='Bond Yields ARIMA PACF')
plot(arima_BY$residuals,main='Bond Yields ARIMA Residuals')




acf(SP500,main='S&P 500 PE Ratio ACF')
kpss.test(SP500,null='Trend')
adf.test(SP500)
arima_SP = auto.arima(SP500)
acf(arima_SP$residuals,main='SP 500 ARIMA ACF')
pacf(arima_SP$residuals,main='SP 500 ARIMA PACF')
plot(arima_SP$residuals,main='AP 500 ARIMA Residuals')



write.zoo(TB, file="3MTBSMR.csv", sep=" ",col.names=c('Date','3-Month Treasury Bill Secondary Market Rate'))
write.zoo(BY, file="LTGBY.csv", sep=" ",col.names=c('Date','Long Term Government Bond Yields'))
write.zoo(SP500, file="SP500PER.csv", sep=" ",col.names=c('Date','S&P 500 PE Ratio'))
