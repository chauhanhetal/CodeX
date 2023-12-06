#Moving Average

trend = seq(from = 10, to = 30,length = 100)
seasonal = sin(pi * trend)
irregular = rnorm(100, sd = 0.1)
X = trend + seasonal + irregular

TS = ts(X, frequency = 12, start = c(2000, 1))

TS

plot(TS)

library(forecast)
a = ma(TS, order = 12)
lines(a, col = "red")


library(zoo)
b = rollmean(TS, k = 12)
lines(b, col = "blue")


##################
#Simple Exponential Smoothing

x=20 + rnorm(50, mean = 0, sd = 1)
TS=ts(x, start = c(2000,1), frequency = 12)
TS
plot(TS)

SES_1 = HoltWinters(TS, alpha = 0.2, beta = FALSE, gamma = FALSE)
plot(SES_1)

SES_2 = HoltWinters(TS, alpha = 0.8, beta = FALSE, gamma = FALSE)
plot(SES_2)

SES_1$SSE
SES_2$SSE

SES_3 = HoltWinters(TS, alpha = NULL, beta = FALSE, gamma = FALSE)
plot(SES_3)
SES_3$alpha

SES_3$SSE



#Simple Exponential Smoothing Forecasting

library(forecast)

Forecast_SES = forecast:::forecast.HoltWinters(SES_3, h = 5)
Forecast_SES
plot(Forecast_SES)


#######################
##practise quiz
library(readxl)
Sales_data <- read_excel("D:/ISI/Time Series/Sales_data.xlsx")
#View(Sales_data)
rows(Sales_data)
plot(Sales_data)

TS = ts(Sales_data )


library(forecast)
a = ma(Sales_data, order = 6)

a

##########################
library(readxl)
number_of_acres_burned_in_forest <- read_excel("D:/ISI/Time Series/number_of_acres_burned_in_forest.xlsx")
head(number_of_acres_burned_in_forest)
plot(number_of_acres_burned_in_forest)
tail(number_of_acres_burned_in_forest)
X=number_of_acres_burned_in_forest$`Number of acres burned in forest fires in Canada`
Y=number_of_acres_burned_in_forest$Year
TSForest = ts(X, start=1918, end=1988)
SES_HW = HoltWinters(TSForest, alpha = NULL, beta = FALSE, gamma = FALSE)
SES_HW$alpha
plot(SES_HW)

Forecast_SES = forecast:::forecast.HoltWinters(SES_HW, h = 5)
Forecast_SES
plot(Forecast_SES)


##########################
#Double Exponential Smoothing

#install.packages(fpp2)

library(fpp2)

ausair

TS = window(ausair, start = 1990)

plot(TS)

library(forecast)

DES = holt(TS, h = 5)

summary(DES)

accuracy(DES)

plot(DES)

############################

library(readxl)
sales_of_shampoo <- read_excel("D:/ISI/Time Series/sales_of_shampoo.xlsx")
sales_of_shampoo

TSShampoo = ts(sales_of_shampoo, start=1980, end=2015)
plot(TSShampoo)
library(forecast)

DES = holt(TSShampoo, h = 10)

summary(DES)

accuracy(DES)

plot(DES)

#########################
#Holt-Winters Exponential Smoothing

trend = 1:100
season = sin(trend)
random = rnorm(100, mean = 0, sd = .5)
x = 10 + trend + trend * season * 0.3 + random

TS=ts(x, start = c(2000,1), frequency = 12)

TS

plot(TS)

HW = hw(TS, seasonal = "multiplicative", h = 12)

summary(HW)

plot(HW)

HW1 = hw(TS, seasonal = "additive", h = 12)

accuracy(HW)
accuracy(HW1)



######################
library(readxl)
airline_passengers <- read_excel("D:/ISI/Time Series/airline_passengers.xlsx")
airline_passengers

TsPass = ts(airline_passengers$`Number of Passengers`, frequency=12, start=c(1949,1))
plot(TsPass)

HWPass = hw(TsPass, seasonal = "multiplicative", h = 24)

summary(HWPass)

plot(HWPass)

HW1Pass = hw(TsPass, seasonal = "additive", h = )

accuracy(HWPass)
accuracy(HW1Pass)
