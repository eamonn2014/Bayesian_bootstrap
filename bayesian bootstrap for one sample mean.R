
# one sample mean bootstrap
#http://rsnippets.blogspot.ie/2012/11/possible-error-bayesian-bootstrap.html
require(Hmisc)
dboot <- function(data.set) {
  u <- c(0, sort(runif(length(data.set) - 1)), 1)
  g <- diff(u)
  #return( wtd.quantile(data.set, weights=g, probs=.5, normwt=T))
  return( wtd.mean(data.set, weights=g))
}

#http://rsnippets.blogspot.ie/2012/11/simple-bayesian-bootstrap.html
library(gtools)

# Bayesian bootstrap
mean.bb <- function(x, n) {
  apply(rdirichlet(n, rep(1, length(x))), 1, weighted.mean, x = x)
}

# standard bootstrap
mean.fb <- function(x, n) {
  replicate(n, mean(sample(x, length(x), TRUE)))
}

#set.seed(2131)
reps <- 100000
x <- rnorm(100, mean=0, sd=1)
fbq<-bbq<-bb2<-NULL

system.time(A <- mean.fb(x, reps))
system.time(B <- (mean.bb(x, reps)))
system.time(C <- replicate(reps, dboot(x)))

fbq <- quantile(A, c(0.025, 0.975)) 
bbq <- quantile(B, c(0.025, 0.975))
bb2 <- quantile(C, c(0.025, 0.975))

cat("Frequentist 95% CI from "   , fbq[1], " , ", fbq[2], "\n")
cat("Bayesian 95% CI from "      , bbq[1], " , ", bbq[2], "\n")
cat("EOB's Bayesian 95% CI from ", bb2[1], " , ", bb2[2], "\n")


t.test(x)