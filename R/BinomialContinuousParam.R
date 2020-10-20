##Estimate posterior probability of a parameter p for a binomial distribution, given "r" successes out of "n" trials


##No prior
contBinom <- function(r, n, theta){
B = gamma(n)*gamma(r)/gamma(n+r)
return((1/B)*(theta^(r))*(1-theta)^(n-r))
}
##Obtain estimates
contBinom(2,3, .6667)

##Verify results
dbeta(.6667, 3,2)

##So verified

## Beta prior

a = 3 
b =4
contBinomPrior <- function(r, n, theta, prior=c(0,0)){
  a = prior[1]
  b = prior[2]
  B = gamma(a+r)*gamma(b+n-r)/gamma(a+b+n)
  return((1/B)*(theta^(a+r-1))*(1-theta)^(b+n-r -1))
}

