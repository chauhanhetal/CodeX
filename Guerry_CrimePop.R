library(HistData)
data(Guerry)
head(Guerry)
help(Guerry)


Guerry <-na.omit(Guerry) 
lmsuicides <- lm(Crime_prop ~ factor()+Wealth, data=na.omit(Guerry) )
summary(lmsuicides)

cities = as.data.frame(binaries(Guerry$Region))
colnames(cities)=c("Medium", "Small", "Large")
#lmsuicides1 <- lm(Suicides ~ Wealth + cities$Medium+cities$Large, data=Guerry)
#summary(lmsuicides1)

lmsuicides2 <- lm(Crime_prop ~ Wealth, data=na.omit(Guerry) )
summary(lmsuicides2)

hanova(lmsuicides,lmsuicides2)


crime2=lm(Crime_prop~factor(Region)+Wealth,data=na.omit(Guerry))

crime1=lm(Crime_prop~Wealth,data=na.omit(Guerry))

hanova(crime1,crime2)
