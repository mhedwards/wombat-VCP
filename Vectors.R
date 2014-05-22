## mainly notes to me about vector.

From C and x change can get angle.

From angle and new C can get x and y changes.

Only necessary for the "compounded" version


need to keep Original version somewhere & "current" x.y


chance to move would be dnorm() based on original R.j
then randomly pick distance. Weighted?

for the "compounded" version, base next change off the updated x.y
so, will need to have a "movement" variable: no, temp, compounded


## Temp Move

# filter to candidate x.y, calculate R.j
# update C to new C before passing to detected. need to record that updated R.j as the detection distance.

## !! Candidate.xy is a filter of the (orginal.xy), so we can update it's R.j with impunity, it will only effect that VCP

## perm Move
# as above, only update the x.y not the MASTER but the list passed for that pass.


if(move.type="temp"){
  new.Rj = VCP.movement(R.j)
  
}

distances <- runif(20, 0, .2)

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


my.df <- data.frame(moved, pi.raw, distances)
my.df[order(distances), ]

origin.xy <- xy.structured[12,]

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
## now, just have to integrate those changes back into the original dataset. WHEE. probably should imlement an object id field to make that easier. 


new.Rj <- candidate.xy$R.j + .02
candidate.xy$new.x <- new.x
candidate.xy$new.y <- new.y
tbl_df(candidate.xy)

update.xy <- tbl_df(candidate.xy %.% mutate( new.x = x+x.chng, new.y=y+y.chng, R.j2=new.Rj))
head(update.xy)
library(ggplot2)
ggplot()+geom_point(data=update.xy, aes(x=x,y=y), colour="blue")+geom_point(data=update.xy, aes(new.x, new.y), colour="red")

plot(5.5, 1.5, pch=16)
points(5.758238, 1.739867)
points(6.031129, 1.9933447)
xy.structured[12,]

X <- 5.5
Y <- 1.5
theta = pi/4
h=.25
x1 <- h*cos(theta) + X
y1 <- h*sin(theta) + Y

plot(X,Y, pch=16)
points(x1, y1)

n.theta <- acos( (x1-X)/h) ## this equals theta



(y1-Y)/(x1-X)
