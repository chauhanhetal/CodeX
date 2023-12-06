library("lmreg")
data(girlgrowth)

head(girlgrowth)
y=girlgrowth$Height
x1=girlgrowth$Age
x2=girlgrowth$Age^2

lm1 = lm(y~x1)
lm2= lm(y~x2)
lm3 = lm(y~x1+x2)

sighat <- summary(lm3)$sigma

criteria <- function(lmobj,sig) {
  M <- summary(lmobj)
  rsq <- M$r.sq
  arsq <- M$adj.r.squared
  cp <- M$df[2]*(M$sigma / sig)^2 - M$df[2] + M$df[1]
  return(c(rsq,arsq,cp))
}

crit <- cbind(criteria(lm1,sighat),
              criteria(lm2,sighat),
              criteria(lm3,sighat))
crit <- as.data.frame(crit, row.names = c("R square", "Adjusted R square","Cp"))
colnames(crit) <- c("x1 only","x2 only","x1 and x2")
crit


criter <- function(lmobj) {
  y <- lmobj$model[[1]]
  n <- dim(lmobj$model)[[1]]
  p <- dim(lmobj$model)[[2]]
  resids <- lmobj$res
  leverage <- hatvalues(lmobj)
  press <- sum((resids / (1 - leverage))^2)
  yvar <- sum((y - mean(y))^2) 
  predictedrsq <- 1 - press / yvar
  aic <- n * log (mean(resids^2) * 2*pi) + 2 * p + n
  bic <- n * log (mean(resids^2) * 2*pi) + log(n) * p + n
  return(c(press,predictedrsq,aic,bic))
}

# values of the criteria for the three models
crit <- cbind(criter(lm1), criter(lm2), criter(lm3))
crit <- as.data.frame(crit, row.names = 
                        c("PRESS", "Predicted R square", "AIC","BIC"))
colnames(crit) <- c("y~x01","y~x2","y~x1+x2")
crit

