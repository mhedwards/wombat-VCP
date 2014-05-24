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

### ------ Compare Movement Detection Distances with Lack of Movement -----------------

nsim <- 50
hnorm.p <- VCP.defineHalfnorm(.5)

# get VCP points
xy.S <- VCP.structuredLayout()
#xy.R <- VCP.randomLayout()
#xy.T <- VCP.transectLayout()

Rj.still <- data.frame()
Rj.move <- data.frame()
xy.vcp <- xy.S
w <- 0.5 # max detection distance.

# cycle through points, do the regular detection thing
for(i in 1:nsim){
  xy.objects <- get.Coords(20)
  
  for(j in 1:36){
    # get next x, y 
    curr.x <- xy.vcp[j,"x"]
    curr.y <- xy.vcp[j,"y"]
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

    detected.still <- VCP.hnormProb(candidate.xy$R.j, hnorm.p)
  
    # add movement, see if detected (using h-norm detection)
    movement <- VCP.movement(candidate.xy$R.j)
    detected.move <- VCP.hnormProb(candidate.xy$R.j + movement, hnorm.p)
    
    candidate.xy <- candidate.xy %.% mutate(new.RJ = R.j + movement)
    
    # store list of detected R.j in data frame, with station # and transect # if applicable
    Rj.still <- rbind(Rj.still, candidate.xy %.% filter(detected.still==1) %.% mutate(Rj = R.j))
    Rj.move  <- rbind(Rj.move, candidate.xy %.% filter(detected.move==1) %.% mutate(Rj=new.RJ))
  }
  
  

}


ddist.mov <- Rj.move$Rj
ddist.still <- Rj.still$Rj
brks <- seq(0,.62, by=.02)
hist(ddist.mov, breaks=brks)
hist(ddist.still, breaks=brks)

# the hell? the "still" distances also have a drop close to the point

xy.vcp <- VCP.transectLayout()
raw.Rj <- data.frame()
xy.objects <- get.Coords(20)

for(j in 1:36){
  # get next x, y 
  curr.x <- xy.vcp[j,"x"]
  curr.y <- xy.vcp[j,"y"]
  # consider objects within x+/- w and y+/- w
  #   (not considering "noise" for this sim)
  
  xmin = curr.x-w
  xmax = curr.x+w
  ymin = curr.y-w
  ymax = curr.y+w
  
  candidate.xy <- filter(xy.objects, (xmin <= x & x <= xmax) & (ymin <= y & y <= ymax))
  
  # calculate straight-line distance from observer to object, R.j
  
  candidate.xy <- mutate(candidate.xy, x.dist = x-curr.x, y.dist=y-curr.y, R.j = sqrt(x.dist^2+y.dist^2))
  
  raw.Rj <- rbind(raw.Rj, candidate.xy)
}

dist.raw <- raw.Rj$R.j
hist(dist.raw)

ggplot()+geom_histogram(aes(x=dist.raw), fill="white", colour="black")+theme_bw(18)+xlab("Distance from Observer")+ylab("Number of Birds")

ggsave("images/simulated_bird_distances.pdf", width=8, height=8)

# so, at least with this map, we're dealing with most of the points are not within .1 to .2 of the thing
# both with structured
dist.raw1 <- dist.raw
dist.raw2 <- dist.raw

#We see the same patterns with random layout & transect layout. 


pi*.1^2
#Area: 0.0314 km2
20*pi*.1^2
# # of birds: .628

pi*.2^2
#Area: 0.1256
20*pi*.2^2
# # of birds: 2.513

pi*.5^2 
#area: .7853982
20*pi*.5^2 
#birds: 15 birds


pi*.1^2
#Area: 0.0314 km2
20*pi*.1^2
# # of birds: .628

pi*.5^2  - pi*.4^2
# area: .283 km2
20*(pi*.5^2  - pi*.4^2)
# birds: 5.6548


## so, we are, at distances beyond .3, seeing the effect of the deteciton curve in teh drop off of detection.
# # the movement is dropping the frequency closer to 0 and bumping it up closer to .1

