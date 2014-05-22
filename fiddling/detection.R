### 5/13 - Detection functions

library(dplyr)
library(fdrtool) # for halfnormal which you need for project_functions

## test data
x.y <- get.Coords(20)
w <- .500 # how will the function get this?

stations <- VCP.structuredLayout()
i<-1

### Candidate POints
# INPUT: need data frame, w, 


# for each point (transect)

# get next x, y 
curr.x <- stations[i,"x"]
curr.y <- stations[i,"y"]
# consider objects within x+/- w and y+/- w
#   (not considering "noise" for this sim)

xmin = curr.x-w
xmax = curr.x+w
ymin = curr.y-w
ymax = curr.y+w

candidate.xy <- filter(x.y, (xmin <= x & x <= xmax) & (ymin <= y & y <= ymax))

# calculate straight-line distance from observer to object, R.j

candidate.xy <- mutate(candidate.xy, x.dist = x-curr.x, y.dist=x-curr.y, R.j = sqrt(x.dist^2+y.dist^2))
# feed R.j into detection function


# store list of detected R.j in data frame, with station # and transect # if applicable


### Detection Function g(r)

# for each point, using specified g(r)

# get probability of detection

# get binary detected

# return list of 0/1 detected.

# For half normal, need theta and the scaling factor

# for exponential, need --- how did I get exponential into my head?


th=seq(0,1, length=100)
plot(th, dexp(th,14.09))
plot(th, exp(1-2.0408*th))

dexp(0.4, 14.09)
dexp(0,14.09)

plot(th, pexp(th,14.09))

paras <- VCP.defineHalfnorm(0.5)

plot(th, dhalfnorm(th, paras$theta))
dhalfnorm(0, paras$theta)
## not sure how I'm going ot scale the exponential.



## Emperical

# get coki.82 from DataExploration

dist.82 <- coki.82$Distance
names(dist.82) <- "R.j"
my_breaks<- c(seq(0,100,10),700)
hist.info<- hist(coki.82$Distance, breaks=my_breaks)

hist.info$counts

nrow(filter(coki.82, Distance <100 & Distance >= 90))
nrow(filter(coki.82, Distance <=100 & Distance > 90)) ## This is teh scheme hist uses.

# this was my original thought, but it shoots that last spike up really high
my_breaks <- c(seq(0,100,10),seq(120,200,20), seq(250,700,50))
hist.info<- hist(coki.82$Distance, breaks=my_breaks)

# I like how this is looking, but may be aggregating too much
my_breaks <- c(seq(0,100,20),seq(120,200,20), seq(250,700,50))
h

hist.info2<- hist(coki.82$Distance, breaks=seq(0,700,5), freq=FALSE)
by5 <- data.frame(x=hist.info2$mids, y=hist.info2$density*5)
by5.lo <- loess(y~x, data=by5, span=0.2)
predict(by5.lo,100)
th.x <- seq(0,700,5)
th.y <- predict(by5.lo,th.x)
th.y[1] <- th.y[141] <- 0


ggplot()+geom_histogram(data=by5, aes(x, y), stat="identity")+geom_smooth(data=by5, aes(x, y), method="loess", span=0.2, size=3)+geom_line(inherit.aes=FALSE, aes(x=th.x, y=th.y), color="red", linetype=2, size=2)+theme_bw(18)+xlab("Detection Distance in Meters")+ylab('Detection Density')

which.max(th.y) # 19
th.y[19] # 0.02464387

th.x[19]
sum(th.y, na.rm=T) #make sure this goes to 1, it does.

delta.RS = 1/th.y[19]

m.500 <- predict(by5.lo, 500)

delta.RS*m.500

by5.lo$fitted[20:22]
hist.info2$breaks[20:22]
hist.info2$density[20:22]

distances <- coki.82$Distance
breakpoints <- seq(0,700,5)

# test new function
new.lo <- VCP.defineLoess(distances, breakpoints)

predict(new.lo$dist.lo, 100)*new.lo$delta # 96.8%
predict(new.lo$dist.lo, 90)*new.lo$delta # 100%

th.x2 <- seq(0,500,5)
th.y2 <- predict(new.lo$dist.lo, th.x2) # sums to 0.992455, pretty darn close to 1.

sum(th.y2, na.rm=T) # 0.992455


# test with simulation-appropriate values
distances <- distances/1000
breakpoints <- breakpoints/1000
new.lo <- VCP.defineLoess(distances, breakpoints)

predict(new.lo$dist.lo, .1)*new.lo$delta # 96.8%
predict(new.lo$dist.lo, .09)*new.lo$delta # 100%

th.x2 <- seq(0,.500,.005)
th.y2 <- predict(new.lo$dist.lo, th.x2) # sums to 0.992455, pretty darn close to 1.

sum(th.y2, na.rm=T) # 0.992455


### ------------------- FUNCTIONS -------------------------
# Already in VCP Functions

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
  
  # the Loess prediction returns NA for observations beyond the scope of the original data
  # turn them into 0 probability, so the detected function doesn't choke.
  
  na.idx <- which(pi.raw %in% NA)
  for(i in 1:length(na.idx)){
    pi.raw[na.idx[i]] <- 0
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

# returns the parameters for a half normal probability
## INPUT:
#  w: the radius of detectability, beyond which detectability is essentially 0
## OUTPUT:
#	q: a scaling parameter so g(0)= 1
#	theta: the parameter for the half-normal distribution.
VCP.defineHalfnorm <- function(w){
  # calculate the approximate "standard deviation" so that detection probability at w is essentially 0
  # 	this was an estimate, and dividing w by 3.5 may not be teh optimal choice.
  sigma <- w/3.5  
  #calculate the parameter for the half normal (dhalfnorm documentation)
  theta <- sqrt(pi/2)/sigma 
  # q is a scale parameter so g(0) =1
  delta<-1/dhalfnorm(0,theta)
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