library(forecast)
trend = seq(10, 30, length = 100)

X = 10 + trend + sin(10 * trend)

tsX= ts(data=X)
start(tsX)

end(tsX)

summary(tsX)
plot(tsX)

#R CODES ON DECOMPOSITION OF TIME SERIES COMPONENTS

library(forecast)

data("AirPassengers")

class(AirPassengers)

print(AirPassengers)

start(AirPassengers)

end(AirPassengers)

summary(AirPassengers)

plot(AirPassengers)

tsdata <- ts(AirPassengers, frequency = 12) 

ddata <- decompose(tsdata, "multiplicative")

plot(ddata)

plot(ddata$trend)

plot(ddata$seasonal)

plot(ddata$random)
