tpaired <- function(x, y) {
  # your code here
  m <- length(x)
  n <- length(y)
  if (m==n)
  {
    a <- x - y  
    abar <- mean(a)
    sp <- sqrt(sum((a - abar)^2)/(n-1))
   
    T  <- abar / (sp /sqrt(n))
    degFreedom <- (n-1)
    pval <- 2 * pt(abs(T), degFreedom, lower.tail = FALSE)
    result <- list(
      statistic = T,
      degFreedom = n-1,               
      p.value = pval
    )
  }
  else
  {
    result <- list(
      statistic = "cannot be found",
      degFreedom = "lists are not of same size",               
      p.value = 0
    )
  }
  class(result) <- "tpaired"
  result
}



if (tpaired(rnorm(10), rnorm(10))$df != 9) cat("ERROR: wrong degrees of freedom")
tpaired(rnorm(10), rnorm(10))

x <- c(10.25, 10.06, 10.0, 10.78, 10.56, 10.08, 10.72, 10.56, 10.66)
y <- c(10.93, 10.73, 10.2, 10.72, 10.68, 10.86, 10.32, 10.18, 10.77)
t.test(x, y, paired = TRUE)
tpaired(x,y)
x


