#### Matt's VCP Utilitles Package

library(dplyr)
library(fdrtool) # for half normal distribution


# INPUT: A vector of distance values, and a vector of equally spaced breakpoints
# RETURN: delta for Loess, and the Loess object for use with prediction
# Try to only run this once, so that we're not repeatedly doing the loess regression.
VCP.defineLoess <- function(distances, breakpoints){
  hist.info <- hist(distances, breaks=breakpoints, freq=FALSE)
  bin.w <- breakpoints[2]-breakpoints[1]
  hist.df <- data.frame(x=hist.info$mids, y=hist.info$density*bin.w) # density should now sum to 1
  hist.lo <- loess(y~x, data=hist.df, span=0.2) # span is hard coded for my particular application
  
  # find delta
  th.x <- seq(min(breakpoints), max(breakpoints),bin.w)
  th.y <- predict(hist.lo,th.x)
  max.y <- which.max(th.y)
  delta.RS <- 1/th.y[max.y]
  
  params <- list(delta= delta.RS, dist.lo = hist.lo)
  
  return(params)
}


# INPUT: a vector of distance values; a params object with a scaling factor named "delta" and a loess regression object named "dist.lo". Unpredictable results if distances and the loess object are on different scales.
# RETURN: a vector of yes/no if the point was detected at that distance.
VCP.loessProb <- function(distances, params ){
  pi.raw <- predict(params$dist.lo, distances)*params$delta
  
  # using the Empirical data, distances beyond .69 will have a probability of "NA". Need to set these to 0.
  idx <- which(pi.raw %in% NA)
  for(i in 1:length(idx)){
    pi.raw[idx[i]] <- 0
  }
  
  detected <- VCP.detected(pi.raw)
  return(detected)  
}


# INPUT: a vector of distance values; 
#   a params object with a scaling factor named "delta" and a parameter "theta" that is the parameter to a half-normal distribution
# RETURN: a vector of yes/no if the point was detected at that distance.
VCP.hnormProb <- function(distances, params){
  pi.raw <- dhalfnorm(distances, params$theta)*params$delta
  detected <- VCP.detected(pi.raw)
  return(detected)  
}

VCP.dhnorm <- function(r, params){
  return(dhalfnorm(r, params$theta)*params$delta)
}

# returns the parameters for a half normal probability
## INPUT:
#  w: the radius of detectability, beyond which detectability is essentially 0
## OUTPUT:
#  q: a scaling parameter so g(0)= 1
#	theta: the parameter for the half-normal distribution.
VCP.defineHalfnorm <- function(right.bound, g.zero=1){
  # calculate the approximate "standard deviation" so that detection probability at w is essentially 0
  # 	this was an estimate, and dividing w by 3.5 may not be teh optimal choice.
  sigma <- right.bound/3.5  
  #calculate the parameter for the half normal (dhalfnorm documentation)
  theta <- sqrt(pi/2)/sigma 
  # q is a scale parameter so g(0) =1
  delta<-g.zero/dhalfnorm(0,theta)
  # return the parameters
  params <- list(delta, theta)
  names(params) <- c("delta", "theta")
  return(params)
}


## Detected: generate a list of 0/1 values for if a nest is detected.
## INPUT
#  pi.list: a vetcor of probabilities. list will be truncated (NOT scaled) to be between 0 & 1
## OUTPUT
#	detected: a vector of 0/1 values: 1 if object detected, 0 if not
## Discussion: The probability is used in a binomial random variable to generate the 0/1 value.
#	this means that even an object with 0.99 probability could still return a 0, and a nest with 
#	0.02 probability might return a 1, which is a possibility for this type of study.
## This code was taken from work done for Ecological Sampling project
VCP.detected <- function(pi.list){
  #empty set
  detected <- rep(0, length(pi.list))
  # for each value in the list, truncate to between 0 & 1 (inclusive) 
  for(i in 1:length(pi.list)){
    p <- pi.list[i]
    if(p>1)
    {p <-1}
    else if(p <0)
    {  p<-0}
    
    # generate 0/1 based on random binomial. aka, the reason this is in a for loop
    detected[i] <- rbinom(1,1,p)
  }
  return(detected)
}