### Movement functions

# VCP.movement - temporary
# VCP.newXY - compounded



VCP.movement <- function(distances, threshold=0.1){
  hnorm.p <- VCP.defineHalfnorm(threshold)
  # determine probability of movement
  pi.raw <- hnorm.p$delta*dhalfnorm(distances, hnorm.p$theta)
  #determine if it moved
  moved <- VCP.detected(pi.raw)
  
  # This is arbitrary. Assuming an average movement of 50 m (.05) at 0 distance, 
  #   reducing linearly to 0 movement at 110 m (.110). I felt there would be more movement
  #   closer to teh station, and less further away.
  m<- -.05/.110 # slope downwards (less movement, further away)
  b <- .05  # 50 meters of movement at 0
  mvmt.base <- m*distances+b
  
  # add a little noise to the movement
  mvmt.noise <- rnorm(length(pi.raw), sd=(threshold/10))
  
  mvmt <- moved*(mvmt.base+mvmt.noise)
  return(mvmt)
}



## assumes candidate.xy contains fields "x.dist" which is the change in X, and R.j, which is the hypotenuse
## origin.xy is a dataframe with at least variables x and y - NOT NEEDED
VCP.newXY <- function(candidate.xy){
  thetas <- acos(candidate.xy$x.dist/candidate.xy$R.j)*sign(candidate.xy$y.dist)
  movement <- VCP.movement(candidate.xy$R.j)
  
  new.Rj <- candidate.xy$R.j + movement
  
  #these are changes in xy, not
  x.chng <- movement*cos(thetas)
  y.chng <- movement*sin(thetas)
  
  update.xy <- candidate.xy %.% mutate( new.x = x+x.chng, new.y=y+y.chng, R.j2=new.Rj)
  return(update.xy)
}