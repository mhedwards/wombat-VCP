## Utility
library(dplyr)

get.Coords <- function(dens){
  # hard coding for 36km^2, with a 0.5 buffer on either side, then trimming.
  #  km2 <- ha_to_km2(ha)
  #  cmax <- sqrt(km2)
  km2 <- 49
  cmax <- 6.5
  cmin <- -0.5
  X <- runif(km2*dens, min=cmin, max=cmax)
  Y <- runif(km2*dens, min=cmin, max=cmax)
  ret <- data.frame(x=X,y=Y)
  # ret2 <- filter(ret, !((X < 0 | X > 6) | (Y < 0 | Y > 6)))
  return (ret)
}


ha_to_km2 <- function(ha){
  return (ha/100)
}

km2_to_ha <- function(km2) {
  return (km2*100)
}

# total study area in Palie was 941 ha, which is 9.41 km^2. For ease of simulation, I'm going to say 9km2, which gives me an even 3 on each side.
n.toSim <- function(ha, dens) {
  km2 <- ha_to_km2(ha)
  return (km2*dens)  # if we have 9 square kilometers, we want approx "density" # of birds per km2
}


is.in.bounds <- function(x,y, xmin=0.5, xmax=5.5, ymin=0.5, ymax=5.5) {
  ok <- TRUE
  if(x < xmin | x > xmax | y < ymin | y > ymax){ ok<-FALSE}
  return(ok)
  
}


detected <- function(pi.list) {
  len <- length(pi.list)
  det <- rep(0, len)
  for(i in 1:len){
    p <- pi.list[i]
    det[i] <- rbinom(1,1,p)
  }
  return (det)
}



## Kernel method Density estimate for set of VCP
## INPUT
#  m: a vector of counts of observed objects per VCP
#	R.j: a vector of distances at which nests were observed
#		in the case of overlapping VCPs, a tree may be represented here more than once.
## OUTPUT
#	D.hat <- density estimate using Kernel method for VCP with normal kernel
D.kernel.norm.VCP <- function(m, R.j) {
  #VCP density from Quang, 1993
  m.bar <- mean(m) # mean # of objects per plot.
  n.hat <- sum(m) #sum of objects surveyed
  h <- sd(R.j)*n.hat^(-1/5)
  t <- length(m)
  D.hat.2 <- (1/(sqrt(2*pi)*pi*(h^3)*t)) * sum(R.j*exp(-((R.j)^2)/(2*h^2)    ))
  # Quang provides two methods, the previous line should return same value as following sequence.
  K <- sqrt(2 * pi)^(-1) * (exp((-0.5) * (R.j/h)^2) * ((-0.5) * (2 * (R.j/h))))
  B.hat = (-2/(n.hat*h^2))*sum(K)
  D.hat = (m.bar*B.hat)/(2*pi)
  return(D.hat)  
}