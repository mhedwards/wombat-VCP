### test simulation
library(dplyr)
library(fdrtool) # for halfnormal which you need for project_functions

# set up environment
D.known <- 20
max.r <- 0.5
nsim <- 50

# half normal parameters for g(r)
hn.params <- VCP.defineHalfnorm(max.r)

# for emperical g(r), pull original distances, scale to our "1=1 km" scale
raw.data <- tbl_df(read.csv("data/COKI.csv", stringsAsFactors=FALSE))
coki.82 <- filter(raw.data, Year==82)
distances <- coki.82$Distance/1000 
breakpoints <- seq(0,.7,.005)
emp.params <- VCP.defineLoess(distances, breakpoints)

# VCP points
xy.structured <- VCP.structuredLayout() # won't change


D.hat.df <- data.frame()

## IN loop =======================================
for(i in 1:nsim){
  # generate map
  x.y = get.Coords(D.known)
  
  # get random VCP coords
  xy.random <- VCP.randomLayout()
  xy.transect <- VCP.randSystematicLayout()
  
  # for each VCP layout
  #   for each detection function
  #     get the density estimate.
  
  D.pass <- c() # group variables
  D.hat.df <- rbind(D.hat.df, D.pass) # add to data frame
}


## g.type can be "hnorm" or "emp"; it defaults to "hnorm" if an unrecognized value entered

VCP.dHat <- function(xy.objects, xy.vcp, params, g.type="hnorm", transects=FALSE){
  n.vcp <- nrow(xy.vcp)
  m <- rep(0, n.vcp)
  R.j <- data.frame()
  
  # for each point (transect)
  for(i in 1:n.vcp){
    # get next x, y 
    curr.x <- xy.vcp[i,"x"]
    curr.y <- xy.vcp[i,"y"]
    # consider objects within x+/- w and y+/- w
    #   (not considering "noise" for this sim)
    
    xmin = curr.x-w
    xmax = curr.x+w
    ymin = curr.y-w
    ymax = curr.y+w
    
    candidate.xy <- filter(xy.objects, (xmin <= x & x <= xmax) & (ymin <= y & y <= ymax))
    
    # calculate straight-line distance from observer to object, R.j
    
    candidate.xy <- mutate(candidate.xy, x.dist = x-curr.x, y.dist=x-curr.y, R.j = sqrt(x.dist^2+y.dist^2))
    
    # feed R.j into detection function
    detected=NULL
    if(tolower(g.type)=="emp"){
      detected <- VCP.loessProb(candidate.xy$R.j, params)
    } else {
      detected <- VCP.hnormProb(candidate.xy$R.j, params)
    }
    
    # store list of detected R.j in data frame, with station # and transect # if applicable
    m[i] <- sum(detected)
    R.j <- rbind(R.j, filter(candidate.xy, detected==1)$R.j)
  }
  
}


## needs a vector m, that is the length of the # of VCPS, and a vector R.j that is all the detected distances.


VCP.Dhat.hnormKernel <- function(m, R.j) {
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