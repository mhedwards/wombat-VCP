## ST 573 Ecological Sampling
## Red Tree Vole project Simulation Code
## by: Matt Edwards; edwarmat@onid.orst.edu


library(fdrtool) # for halfnormal which you need for project_functions

## Returns an RTV.fullmap object
## INPUT:
#     r: radius beyond which detection is 0
#     t.dens: Tree/Stand density
#     n.dens: nest/tree density (between 0 & 1)
## OUTPUT: An RTV.fullmap object, containing:
#     W: 6*r*1.10  width of stand
#     L: 20*r*1.10 Length of stand
#     r: the input Radius
#     e.r: Effective Radius: 1.10*r
#     trans.X: W/2
#     X.min, X.max:  trans.X+/-e.r  min and max x values for which nest will be seen
#     trees - a data.frame with columns
#  	X: x coordinate of tree [0:W]
#		Y: y coordinate of tree [0:L]
#		has_nest: 1 if tree has a nest, 0 otherwise
#		this is only trees between X.min and X.max
rtv.fullmap <- function(r, t.dens, n.dens ){
  e.r = r+r*0.10 # Effective Radius (Radius + 10%)
  W = 6*e.r  # Stand width (transect +/- 3 e.r)
  L = 8*2*e.r+4*e.r   # allows for 5 VCP separated by a distance of 1/2 e.r
  # this Length calculation might be causing the issue with the VCP > LT conversion
  
  
  N.tree <- W*L*t.dens # The number of trees within the "stand" 
  
  # randomly pick x and y within the stand.
  X <- runif(N.tree)*W
  Y <- runif(N.tree)*L
  
  #create the data frame
  stand<-data.frame(X=X, Y=Y)
  # yes/no if tree has nest, based on density prob.
  stand$has_nest <- rbinom(N.tree, 1, n.dens) 
  
  # get the transect coordinates and the min and max x values at which we'll "see" things.
  trans.X <- W/2
  min.X <- trans.X - e.r
  max.X <- trans.X + e.r
  
  #we don't want to carry around ALL trees, so reduce data frame to only those trees
  # in the observable distance.
  stand$subset <- ifelse(min.X<=stand$X & stand$X<=max.X, 1, 0)
  trees <- stand[stand$subset==1,1:3]
  
  # create and return the "full map" object.
  full.map <- list(W,L, trees, r, e.r, trans.X, min.X, max.X)
  names(full.map) <- c("W", "L", "trees", "r", "e.r", "trans.X", "X.min", "X.max")
  return(full.map)
}



## Returns an RTV.redmap object
## INPUT:
#		RTV.fullmap - an object returned by the rtv.fullmap() function
#		noise.sd: the standard deviation of a Gaussian function to add "noise" to the detectability probability
#		 - default value is 0.04
## OUTPUT:An RTV.redmap object, containing:
#     W: 6*r*1.10  width of stand
#     L: 20*r*1.10 Length of stand
#     e.r: Effective Radius: 1.10*r
#     trans.X: W/2
#     X.min, X.max:  trans.X+/-e.r  min and max x values for which nest will be seen
#     nests - a data.frame with columns
#		X: x coordinate of tree [0:W]
#		Y: y coordinate of tree [0:L]
#		has_nest: 1 if tree has a nest, 0 otherwise
#		x.dist: horizontal distance from trans.X to nest
#		noise: a value of the detectability "noise" for this particular nest
#		tree.id: a unique identifier for this tree.
#		this is only trees between X.min and X.max that have nests. all other trees trimmed out.
#		
rtv.reducemap <- function(RTV.fullmap, noise.sd=0.04){
  all.trees <- RTV.fullmap$trees
  
  # reduce to only nests in the survey ara
  a.nests <- all.trees[all.trees$has_nest==1,]
  
  #sort by Y to make sequential VCP faster
  nests <- a.nests[order(a.nests$Y),]
  #add columns for distance from transect, noise, and a unique ID.
  nests$x.dist <- abs(nests$X - RTV.fullmap$trans.X)
  nests$noise <- rnorm(nrow(nests), 0, noise.sd)
  nests$tree.id <- seq(1:nrow(nests))
  
  #compile and return the object
  nests.map <- list(RTV.fullmap$W,RTV.fullmap$L, nests, 
                    RTV.fullmap$e.r, RTV.fullmap$trans.X, RTV.fullmap$X.min, RTV.fullmap$X.max)
  names(nests.map) <- c("W", "L", "nests", "e.r", "trans.X", "X.min", "X.max")
  return(nests.map)
  
}



# returns the parameters for a half normal probability
## INPUT:
#	r: the radius of detectability, beyond which detectability is essentially0
## OUTPUT:
#	w: a scaling parameter so g(0)= 1
#	theta: the parameter for the half-normal distribution.
rtv.hnorm <- function(r){
  # calculate the approximate "standard deviation" so that detection probability at r is essentially 0
  # 	this was an estimate, and dividing r by 3.5 may not be teh optimal choice.
  sigma <- r/3.5  
  #calculate the parameter for the half normal (dhalfnorm documentation)
  theta <- sqrt(pi/2)/sigma 
  # w is a scale parameter so g(0) =1
  w<-1/dhalfnorm(0,theta)
  # return the parameters
  params <- list(w, theta)
  names(params) <- c("w", "theta")
  return(params)
}



## Density for "very narrow" strip method
## 	calculate the density based on the trees w/in 1 unit of the centerline
## INPUT:
#		RTV.fullmap object
#		m: the effective half width for the very narrow strip
## OUTPUT:
#		Density: (# nest trees in strip)/(# of trees in strip)
D.vnarrow <- function(RTV.fullmap, m=1){
  #bounds of the strip
  strip.min.x <- RTV.fullmap$trans.X - m
  strip.max.x <- RTV.fullmap$trans.X + m
  #only the trees in the strip
  narrow.strip <- RTV.fullmap$trees[RTV.fullmap$trees$X >= strip.min.x & RTV.fullmap$trees$X <= strip.max.x,]
  #calculate density: (# nest trees in strip)/(# of trees in strip)
  Dens <- sum(narrow.strip$has_nest)/nrow(narrow.strip)
  return(Dens)
}

## Density using Line Transect
## Calculate the density based on a standard line transect (perpendicular distance to transect)
## INPUT
#	RTV.redmap object
#	w: scaling parameter for halfnormal with given theta so g(0)=1
#	theta: parameter for half-normal distribution
## OUTPUT
#	Density estimate using the kernel method and the distance to the nest perpendicular to the transect
D.linetrans <- function(RTV.redmap, w, theta){
  # for every nest, find the theoretical probability of detection 
  #		based on halfnormal + Gaussian noise (see rtv.redmap documentation)
  pi.raw <- w*dhalfnorm(RTV.redmap$nests$x.dist, theta)+RTV.redmap$nests$noise
  
  # randomly determine based on the probability if the nest is seen or not. 
  ltrans.detected <- rtv.detected(pi.raw)
  
  # determine & return the density estimate using the Kernel method with a normal kernel
  D.ltrans <- D.kernel.norm(RTV.redmap, ltrans.detected)
  return(D.ltrans)  
}



## Density for VCP (and convert to line Transect)
## INPUT:
#	RTV.redmap object from the rtv.redmap() function
#	w: scaling parameter for halfnormal with parameter theta so g(0)=1
#	theta: parameter for half-normal distribution
#	change.rate.Y: how far along the transect the next observer position should be, in terms of the # of effective Radii
#		e.g: change.rate.Y=1 means that the y value for the center of the next VCP is 1 e.r down the transect
#		change.rate.Y=3 means the y value for the center of the next VCP is 3 e.r down the transect
#	line.transect=FALSE: if the nests detected in the VCPs should be converted to a Line Transect, default False
## OUTPUT
#	D.VCP[1]: density of the set of VCPs generated, using Kernel method with a normal KErnel
#	D.VCP[2]: (if line.transect==TRUE) density calculated on the unique nests found by VCP using Line Transect Kernel method.
#		Set of all nests seen is stripped to unique nests, the perpendicular distance to line transect 
#		is calculated, and those values are used to generate LT density estimate. Using the initial observed
#		value from observer to tree from VCP probably better, but if nest seen multiple times, how do you
#		pick the one that is "correct"? 
D.VCP <- function(RTV.redmap, w, theta, change.rate.Y, line.transect=FALSE){
  ### for series of VCP
  ## Initilise starting values for first VCP
  Y.min <- 0
  Y.max <- 2*RTV.redmap$e.r
  Observer <- data.frame(X = RTV.redmap$trans.X, Y=RTV.redmap$e.r)
  Y.change <- change.rate.Y*RTV.redmap$e.r    ## change factor submitted by function
  
  ## First pass ##
  # get only nests within the Y range
  nests.tmp <- RTV.redmap$nests[RTV.redmap$nests$Y >= Y.min & RTV.redmap$nests$Y <= Y.max, ]
  # use Pythagorean theorem to get the direct distance (on ground) from observer to nest (C)
  Y.dist.2 <- (nests.tmp$Y-Observer$Y)^2
  C <- sqrt(nests.tmp$x.dist^2+Y.dist.2)
  # get the raw detection probability from halfnormal + noise (see rtv.redmap() for noise)
  pi.raw <- w*dhalfnorm(C, theta)+ nests.tmp$noise
  # get the 0/1 detected flag for each nest.
  vcp.detected <- rtv.detected(pi.raw)
  
  ## m & R.j & detected list ##
  # initialize the variables for m and R.j
  m <- sum(vcp.detected) #for each VCP, the # of objects seen
  R.j <- C[vcp.detected==1] # for each nest, the distance from the Observer.
  d.list <- nests.tmp[vcp.detected==1, "tree.id"] # a list of ID's of detected nests.
  
  ## Remaining Passes ##
  #update for next round, move Y.min, Y.max and Observer down the transect to the next VCP
  Y.min <- Y.min + Y.change
  Y.max <- Y.max + Y.change
  Observer$Y <- Observer$Y + Y.change
  
  #only while we're still in range for our transect.
  while(Y.max < RTV.redmap$L) {  
    # as above, only for nests within our current Y-range
    nests.tmp <- red.map$nests[red.map$nests$Y >= Y.min & red.map$nests$Y <= Y.max, ]
    Y.dist.2 <- (nests.tmp$Y-Observer$Y)^2
    C <- sqrt(nests.tmp$x.dist^2+Y.dist.2)
    pi.raw <- w*dhalfnorm(C, theta)+ nests.tmp$noise
    vcp.detected <- rtv.detected(pi.raw)
    
    # update the trackign variables
    m <- c(m, sum(vcp.detected))
    R.j <- c(R.j, C[vcp.detected==1])
    d.list <- c(d.list, nests.tmp[vcp.detected==1, "tree.id"])
    
    # update Y-variables for next round.
    Y.min <- Y.min + Y.change
    Y.max <- Y.max + Y.change
    Observer$Y <- Observer$Y + Y.change
    
  }
  
  # if the "line transect" option was selected
  if(line.transect){
    # Get the Density estimate from VCPs
    D.v <- D.kernel.norm.VCP(m, R.j)
    
    # get a list of unique identifiers for detected nests, generate teh 0/1 'detected' vector
    d.list.unique <- unique(d.list)
    detected <- rep(0,nrow(RTV.redmap$nests))
    for(k in 1:length(d.list.unique)){
      detected[d.list.unique[k]] <- 1
    }
    #get the Density estimate for the LT data
    D.l <- D.kernel.norm(RTV.redmap, detected)
    D.VCP <- c(D.v, D.l)
  }
  else 
  {
    #otherwise, just get the VCP estimate.
    D.VCP <- D.kernel.norm.VCP(m, R.j)
  }
  
  return(D.VCP)  
}


## Kernel method Density estimate for set of VCP
## INPUT
#	m: a vector of counts of observed objects per VCP
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
  #D.hat.2 <- (1/(sqrt(2*pi)*pi*(h^3)*t)) * sum(R.j*exp(-((R.j)^2)/(2*h^2)    ))
  # Quang provides two methods, the previous line should return same value as following sequence.
  K <- sqrt(2 * pi)^(-1) * (exp((-0.5) * (R.j/h)^2) * ((-0.5) * (2 * (R.j/h))))
  B.hat = (-2/(n.hat*h^2))*sum(K)
  D.hat = (m.bar*B.hat)/(2*pi)
  return(D.hat)  
}

## Kernel method Density estimate for Line Transect
## INPUT
#	RTV.redmap a reduced object from the rtv.redmap() function
#	d.list: a vector of values, of length nrow(rtv.redmap$nests) with 1 if it was detected, 0 if not
## OUTPUT
#	D.km: density estimate using Kernel method for LT with normal kernel.
D.kernel.norm <-function(RTV.redmap, d.list){
  #trim to detected nests
  d.nests <- RTV.redmap$nests[d.list==1,]
  d.sits <- d.nests$x.dist
  y=nrow(d.nests)
  # variables for the Kernel Method, Thompson pg 238
  a = min( sd(d.nests$x.dist), median(d.nests$x.dist)/1.34 )
  h = 0.9*a*y^(-1/5)
  # density estimate
  K <- sqrt(2*pi)^(-1)*exp( (-0.5) * (d.nests$x.dist/h)^2)
  f.0.km <- (2/(y*h))* sum(K)
  D.km <- y*f.0.km/(2*RTV.redmap$L)  
  return(D.km)
}

## Detected: generate a list of 0/1 values for if a nest is detected.
## INPUT
#	pi.list: a vetcor of probabilities. list will be truncated (NOT scaled) to be between 0 & 1
## OUTPUT
#	detected: a vector of 0/1 values: 1 if nest detected, 0 if not
## Discussion: The probability is used in a binomial random variable to generate the 0/1 value.
#	this means that even a tree with 0.99 probability could still return a 0, and a nest with 
#	0.02 probability might return a 1, which is a possibility for this type of study.
rtv.detected <- function(pi.list){
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





