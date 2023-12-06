# M3L1V4 How conclusions depend on the model
# Pre- and post-treatment leprosy scores of patients 
# receiving different treatments
library(lmreg)
data(leprosy)
head(leprosy)
help(leprosy)
Treat = binaries(leprosy$treatment)
Treat = as.data.frame(Treat) 
head(Treat)
colnames(Treat) = c("A","D","F")

summary(lm(post ~ Treat$A + Treat$D, data = leprosy))$coeff

summary(lm(post ~ Treat$A + Treat$D + pre, data = leprosy))$coeff
