#### Density Estimate functions
#     Originally in the SimTesting file



VCP.Dhat.hnormKernel <- function(m, R.i) {
  #VCP density from Quang, 1993
  m.bar <- mean(m) # mean # of objects per plot.
  n.hat <- sum(m) #sum of objects surveyed
  h <- sd(R.i)*n.hat^(-1/5)
  t <- length(m)
  #D.hat.2 <- (1/(sqrt(2*pi)*pi*(h^3)*t)) * sum(R.i*exp(-((R.i)^2)/(2*h^2)    ))
  # Quang provides two methods, the previous line should return same value as following sequence.
  K.prime <- sqrt(2 * pi)^(-1) * (exp((-0.5) * (R.i/h)^2) * ((-0.5) * (2 * (R.i/h))))
  B.hat = (-2/(n.hat*h^2))*sum(K.prime)
  D.hat = (m.bar*B.hat)/(2*pi)
  return(D.hat)  
}



## g.type can be "hnorm" or "emp"; it defaults to "hnorm" if an unrecognized value entered

VCP.dHat <- function(xy.objects, xy.vcp, params, w=0.5, g.type="hnorm", transects=FALSE, true.D){
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
    
    candidate.xy <- mutate(candidate.xy, x.dist = x-curr.x, y.dist=y-curr.y, R.j = sqrt(x.dist^2+y.dist^2))
    
    # feed R.j into detection function
    detected=NULL
    if(tolower(g.type)=="emp"){
      detected <- VCP.loessProb(candidate.xy$R.j, params)
      #print("emp")
    } else {
      detected <- VCP.hnormProb(candidate.xy$R.j, params)
      #print("hnorm")
    }
    
    
    
    # store list of detected R.j in data frame, with station # and transect # if applicable
    m[i] <- sum(detected)
    R.j <- rbind(R.j, candidate.xy %.% filter(detected==1) %.% select(x, y, R.j) %.% mutate(s_id = i, t_id=xy.vcp$t_id[i]))
  }
  
  dhat <- VCP.Dhat.hnormKernel(m, R.j$R.j)
  return(dhat)
}