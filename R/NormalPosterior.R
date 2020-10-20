## Compute posterior probability that "data" comes from a choice of several normal distributions
##Parameter 1 = mean, parameter 2 = sd
##Priors are a vector of probabilities subjectively adopted by the researcher

normalPosterior <- function(data, parameter1, parameter2, priors){
if (length(parameter1) == length(parameter2) & length(parameter1) == length(priors)){
data <- as.vector(data)
likelihood <- c()

for (i in 1:length(parameter1)){
likelihood[i] <- 1
for (j in 1:length(data)){
likelihood[i] <- likelihood[i]*dnorm(data[j], parameter1[i], parameter2[i])  
}
}

posterior <- c()

for (i in 1:length(parameter1)){
posterior[i] <- (likelihood[i]*priors[i])/(sum(likelihood*priors))
}
return(posterior)
}
else {
print("Oops! You entered incompatible arguments. Your parameter vectors and your priors vector should be the same length")
}
}


## We observe a data draw of 76. We want to compute the posterior probabiltiy that it is distributed N(74, 15), N(68, 15), N(81, 15). Based on prior information
##We think with 50% confidence the true distribution is N(81, 15).
normalPosterior(c(76), parameter1=c(74,68, 81), parameter2=c(15,15,15), priors=c(.25,.25,.5))

