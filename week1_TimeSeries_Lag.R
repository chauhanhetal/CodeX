X <- c(23.32, 32.33, 32.88, 28.98, 33.16, 26.33, 29.88, 32.69, 18.98, 21.23, 26.66, 29.89)
library(tseries)
acf(X, pl=FALSE)

tm1 = ts(X, frequency = 1)
tm1_lag1=lag(tm1,-2)
plot(tm1_lag1,tm1,xy.lines = FALSE)
lag.plot(tm1,set.lags = c(1:6),do.lines=FALSE)