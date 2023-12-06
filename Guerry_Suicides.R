library(HistData)
data(Guerry)
head(Guerry)
help(Guerry)



cities = as.data.frame(binaries(Guerry$MainCity))
colnames(cities)=c("Medium", "Small", "Large")
lmsuicides1 <- lm(Suicides ~ Wealth + cities$Medium+cities$Large, data=Guerry)
summary(lmsuicides1)

lmsuicides2 <- lm(Suicides ~ Wealth, data=Guerry)
summary(lmsuicides2)


help(hanova)
hanova(lmsuicides1,lmsuicides2)


lmsuicides <- lm(Suicides ~ Wealth+factor(MainCity), data=Guerry)
summary(lmsuicides)
