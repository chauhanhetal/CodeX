library(alr4)
#data()
data("wblake")
head(wblake)

plot(wblake, cex=.4)


wblake1=lm(Age ~ Length+Scale, data = wblake)
summary(wblake1)




xgrid= seq(7,12, .1)
ygrid = lmgirls2$coefficients[1]+lmgirls2$coefficients[2]*xgrid +lmgirls2$coefficients[3]*xgrid^2
lines(xgrid, ygrid, col="red")


lmgirls1 = lm(Height~poly(Age,2,raw=T), data = girlgrowth)
summary(lmgirls1)

xgrid= seq(7,12, .1)
ygrid = lmgirls1$coefficients[1]+lmgirls1$coefficients[2]*xgrid +lmgirls1$coefficients[3]*xgrid^2
lines(xgrid, ygrid, col="blue")



summary(lmgirls1)
abline(lmgirls1)

