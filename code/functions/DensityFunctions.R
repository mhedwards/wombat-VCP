#### Density Estimate functions
#     Originally in the SimTesting file



VCP.Dhat.hnormKernel <- function(m, R.i, true.D=NULL) {
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
  
  ret <- NULL
  if(is.null(true.D))
    ret <- D.hat
  else{
    inCI <- VCP.inCI(D.hat, h, true.D, t)
    ret <- list(Dhat = D.hat, inCI = inCI)
  }
  
  return(ret)  
}


#xy.objects <- get.Coords(20)
#xy.vcp <- VCP.randomLayout()
#params <- hn.params
#w=0.5
#g.type="hnorm"
#transects=F
#true.D = 20

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
    
    candidate.xy <- mutate(candidate.xy, x.dist = x-curr.x, y.dist=y-curr.y, Rj = sqrt(x.dist^2+y.dist^2))
    
    # feed R.j into detection function
    detected=NULL
    if(tolower(g.type)=="emp"){
      detected <- VCP.loessProb(candidate.xy$Rj, params)
      #print("emp")
    } else {
      detected <- VCP.hnormProb(candidate.xy$Rj, params)
      #print("hnorm")
    }
    
    
    
    # store list of detected R.j in data frame, with station # and transect # if applicable
    m[i] <- sum(detected)
    if(sum(detected) > 0){
      R.j <- rbind(R.j, candidate.xy %.% filter(detected==1) %.% select(x, y, Rj) %.% mutate(s_id = i, t_id=xy.vcp$t_id[i]))
    }
      ## had to add if statement on 5/31 because when running it on Mac, was getting instances were no objects were detected, even if several were in the .1-.2 range, and it was throwing errors.  PC had higher max observation
  }
  
  dhat <- VCP.Dhat.hnormKernel(m, R.j$Rj, true.D)
  return(dhat)
}


VCP.dhat.movement <- function (xy.objects, xy.vcp, params, w=0.5, g.type="hnorm", m.type="temp", true.D=20){
  n.vcp <- nrow(xy.vcp)
  m <- rep(0, n.vcp)
  observed <- data.frame()
  
  # for each VCP
  for(i in 1:n.vcp){
    
    # filter points to current
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
    
    
    
    if (m.type=="perm"){
      # get new values
      update.xy <- VCP.newXY(candidate.xy)
      detected <- VCP.hnormProb(update.xy$R.j2, params)
      
      observed <- rbind(observed, update.xy %.% filter(detected==1) %.% 
                          select(x=new.x, y=new.y, o_id, Rj=R.j2) %.% mutate( s_id = i, t_id=xy.vcp$t_id[i]))
      
      # update original database
      for(k in 1:nrow(update.xy)){
        idx <- update.xy$o_id[k]
        xy.objects[idx,] <- c(update.xy$new.x[k], update.xy$new.y[k], update.xy$o_id[k])
      }
      
    } else {
      # initiate movement
      adj.Rj <- VCP.movement(candidate.xy$R.j)
      candidate.xy$new.Rj <- adj.Rj + candidate.xy$R.j
      
      # get observed based on new positions
      detected <- VCP.hnormProb(candidate.xy$new.Rj, params)
      observed <- rbind(observed, candidate.xy %.% filter(detected==1) %.% 
                          select(x, y, o_id, Rj=new.Rj) %.% mutate( s_id = i, t_id=xy.vcp$t_id[i]))
    }
    
    m[i] <- sum(detected)
  }
  
  # once done w/ all VCP, get D.hat based on movement positions.
  dhat <- VCP.Dhat.hnormKernel(m, observed$Rj, true.D)
  return(dhat)
}