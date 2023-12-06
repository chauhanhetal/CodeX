rm(list=ls())
set.seed(125)

#b = c(14,32,70,121,190,270,375, 511,674, 866,1082, 1328,1602, 1906,2232)

b = data.frame(x = 1:15, y = c(14,32,70,121,190,270,375, 511,674, 866,1082, 1328,1602, 1906,2232))
tm = ts(b$y)
print(tm)

plot(tm)

#check stationarity - if p<0.05 then null is rejected and alternate is accepted
acf(b$y) 
pacf(b$y)


library(tseries)
library(forecast)
adf.test(tm, k = 1) 
adf.test(tm, k = 4)
pp.test(tm)

lm1 = lm(b$y~b$x)
fit2 = lm(y~poly(x, degree = 2,raw = TRUE), data=b)
fit3 = lm(y~poly(x, degree = 3,raw = TRUE), data=b)
fit4 = lm(y~poly(x, degree = 4,raw = TRUE), data=b)
fit5 = lm(y~poly(x, degree = 5,raw = TRUE), data=b)
plot(b$x, b$y, pch=19, xlab='Time', ylab='y')

X = 1:15
lines(X, predict(lm1), col = 'red')
lines(X, predict(fit2), col = 'blue')
lines(X, predict(fit3), col = 'purple')
lines(X, predict(fit4), col = 'orange')
lines(X, predict(fit5), col = 'green')

summary(lm1)$adj.r.squared
summary(fit2)$adj.r.squared
summary(fit3)$adj.r.squared
summary(fit4)$adj.r.squared
summary(fit5)$adj.r.squared
plot(b$x, b$y, pch = 19, xlab = 'Time', ylab = 'y')
lines(X, predict(fit4), col = 'orange')

summary(lm1)
summary(fit2)
summary(fit3)

#lm
xgrid= seq(16,20,1)
ygrid=lm1$coefficients[1]+lm1$coefficients[2]*xgrid
ygrid1=fit2$coefficients[1]+fit2$coefficients[2]*xgrid+fit2$coefficients[3]*xgrid^2
ygrid2=fit3$coefficients[1]+fit3$coefficients[2]*xgrid+fit3$coefficients[3]*xgrid^2+fit3$coefficients[4]*xgrid^3
ygrid
ygrid1
ygrid2

#observed values : 2592, 2897, 3411, 3852, 4320


adf.test(lm1$residuals, k=1)
plot(lm1$residuals)
adf.test(lm1$residuals, k = 1) 
adf.test(lm1$residuals, k = 4)
pp.test(lm1$residuals)



library(forecast)
arima_model <- auto.arima(lm1$residuals)
summary(arima_model)
forecast_auto <- forecast(arima_model, h = 5)
plot(forecast_auto)
forecast_auto

#observed values : 2592, 2897, 3411, 3852, 4320



model <- Arima(b$y, order = c(1,1,1), method="ML") #ARIMA(1,1,1)
model
forecast_111 <- forecast(model, h = 5)
sum(forecast_111$residuals^2)

DES = holt(tm, h = 5)
sum(DES$residuals^2)
SES = HoltWinters(tm, alpha = 0.2, beta = FALSE, gamma = FALSE)

