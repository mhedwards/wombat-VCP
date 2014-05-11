library(ggplot2)
library(dplyr)

x.y <- get.Coords( 20) # simulating the area surveyed in Palie

#https://groups.google.com/forum/#!topic/ggplot2/f0I4tWWOhbs
angle <- seq(-pi, pi, length=50)
df <- data.frame(a = sin(angle)*0.2, b = cos(angle)*0.2)


ggplot(x.y, aes(X, Y))+geom_point()+ geom_path(aes(a, b), data = df, inherit.aes = F)


## 30 equispaced points, centers of VCPs
stat.X <- rep(seq(0.5, 5.5,1),6)
stat.Y <- sort(rep(seq(0.5, 5.5,1),6))

angle <- seq(-pi, pi, length=50)
ell <- data.frame()
for(i in 1:36) {
  df <- data.frame(gp=i, a = sin(angle)*0.2+stat.X[i], b = cos(angle)*0.2+stat.Y[i])
  ell <- rbind(ell, df)
}

#Bad old plot, DNU
#ggplot()+geom_point(data=x.y, aes(X, Y))+geom_point(aes(x=stat.X, y=stat.Y, colour="#0072B2"), inherit.aes=FALSE) #+geom_path(data=ell, aes(a,b,group=group, colour="#0072B2"))


## Plot of structured layout.
ggplot()+geom_point(data=x.y, aes(X,Y))+geom_point(aes(x=stat.X, y=stat.Y), color="#D55E00", inherit.aes=FALSE)+geom_path(data=ell, aes(a,b,group=gp), colour="#D55E00", size=.75)+coord_fixed(xlim=c(-0.1,6.1), ylim=c(-0.1,6.1))

ggsave("images/layout_structured.pdf", width=8, height=8)

## Random VCP layout
stat.X <- runif(36, 0.5, 5.5)
stat.Y <- runif(36, 0.5, 5.5)

angle <- seq(-pi, pi, length=50)
ell <- data.frame()
for(i in 1:36) {
  df <- data.frame(gp=i, a = sin(angle)*0.2+stat.X[i], b = cos(angle)*0.2+stat.Y[i])
  ell <- rbind(ell, df)
}

ggplot()+geom_point(data=x.y, aes(X,Y))+geom_point(aes(x=stat.X, y=stat.Y), color="#D55E00", inherit.aes=FALSE)+geom_path(data=ell, aes(a,b,group=gp), colour="#D55E00", size=.75)+coord_fixed(xlim=c(-0.1,6.1), ylim=c(-0.1,6.1))

ggsave("images/layout_random.pdf", width=8, height=8)

ggplot()+geom_point(data=x.y, aes(X,Y))+geom_point(aes(x=stat.X, y=stat.Y), color="#D55E00", inherit.aes=FALSE)+geom_path(data=ell, aes(a,b,group=gp), colour="#D55E00", size=.75)+coord_fixed(xlim=c(-0.1,6.1), ylim=c(-0.1,6.1))

## transect

# pick random location on border:
#   random 0.5, or 5.5 for one axis, and a random (0.5,5.5) for the other.
#   randomly pick which is x and which is y
#   randomly pick an angle. If we cannot get 6 points before moving outside of the box, in either direction, start over.
#   place points until you get to 18, or until you hit box boundary.
#   move 2km up or down. (decision?) (Left or right?)

#     if outside bounds, go other way
transect.shift <- 2 # number of kilometers between transects
start.x <- NULL
start.y <- NULL
theta <- NULL
case.wall <- NULL  ## 1 = horizontal, 0 = vertical
shift.down <- TRUE

a <- if(rbinom(1,1,0.5)) 0.5 else 5.5
b <- runif(1,0.5, 5.5)
if(rbinom(1,1,0.5)) {
  start.x <- a
  start.y <- b
  case.wall <- TRUE
  # case x is left or right wall
  if(start.x == 0.5){
    # case: left wall
    theta <- runif(1,-pi/2, pi/2)
    shift.down <- FALSE
  } else {
    # case: right wall
    theta <- runif(1, pi/2, 3*pi/2)
    shift.down <- TRUE
  }
} else {
  start.x <- b
  start.y <- a
  case.wall = FALSE
  # Case, y is floor or ceiling
  if(start.y == 0.5){
    # case floor
    theta <- runif(1,0,pi)
    shift.down <- FALSE
  } else {
    # case ceiling
    theta <- runif(1,pi,2*pi)
    shift.down <- TRUE
  }
}

# check to see if outside boundaries
test.x <- start.x + 6*0.15*cos(theta)
test.y <- start.y + 6*0.15*sin(theta)

start.ok <- TRUE

# if either text.x or test.y is outside the 0.5, 5.5 boundaries, error.
if(test.x < 0.5 | text.x > 5.5 | test.y < 0.5 | text.y > 5.5) { start.ok <- 0}

if(!is.in.bounds(test.x, test.y)) { start.ok <- FALSE}

# probably a while loop or something. once we get to this point:

stat.df <- data.frame(stat.X=start.x, stat.Y=start.y)

last.x <- start.x
last.y <- start.y


curr.xy.in.bounds=TRUE
count <- 1
while(curr.xy.in.bounds & (count < 18)){
  next.x <- last.x+0.15*cos(theta)
  next.y <- last.y+0.15*sin(theta)
  curr.xy.in.bounds <- is.in.bounds(next.x, next.y)
  if(curr.xy.in.bounds){
    stat.df <- rbind(stat.df, c(next.x, next.y))
    last.x <- next.x
    last.y <- next.y
    count= count + 1
  }

}

transect.df <- data.frame(t_id = 1, m=count)
# this will let me do a while(sum(m) <36) type loop
transect.count <- 2


# next transect.
# if floor/ceiling, move x left/right
# if wall, move y up/down
temp.x <- temp.y <- 0

## @TODO - If time, make the perpindicular distance between transects 2km, but right now, it's going to be 2km strictrly vertical or strictly horizontal
d.change <- 2

case.wall <- F  ## 1 = horizontal, 0 = vertical
shift.down <- T

# Case.wall = TRUE means x is 0.5 or 5.5. FALSE means Y is 0.5 or 5.5
# shift.down = TRUE means x or y is 5.5. (can only move to smaller numbers) 
#            = FALSE means x or y is 0.5 (can only move to bigger numbers)

if(case.wall & shift.down){
  temp.x <- start.x-d.change
} else if(case.wall & !shift.down) {
  temp.x <- start.x+d.change
} else if(!case.wall & shift.down) {
  temp.y <- start.y-d.change
} else if(!case.wall & !shift.down){
  temp.y <- start.y+d.change
} else { print("boogers")}

## this will run into issues where we're starting very close to the edge and can only fit 6 in, so we have 6 rows of 6.

# are we in bounds? 
# is the point a duplicate?
# can we get 6 stations in before we go out of bounds?

# if yes, do another transect
# stop if we hit 18 for this transect
# OR stop if we hit 36 total.

# if no, move the other direction.
# i think we start out moving in a negative (left/down) direction, then bounce back to positive and don't go back to negative.

# are we < 36 stations?
# No: stop.
# Yes: # are we > 30 stations? 
#     Yes: Stop.
#     No: Make another transect.


## ctrl-zed.

#generate theta
theta <- runif(1,0,pi)
# generate 1 transect with 18 stations
start.x <- 0
start.y <- 0
trans_id <- 1
stat.temp <- data.frame(x=start.x, y=start.y, t_id=trans_id)

last.x <- start.x
last.y <- start.y
for(i in 2:18){
  next.x <- last.x+0.15*cos(theta)
  next.y <- last.y+0.15*sin(theta)
  stat.temp <- rbind(stat.temp, c(next.x, next.y, trans_id))
  last.x <- next.x
  last.y <- next.y
}

# get vertical distance that will put 2k between parallel lines
delta <-0
if(theta < (pi/2)){
  delta <- pi/2 + theta
} else if( theta > (pi/2)){
  delta <- theta - pi/2
} else { delta <- -1 }  # if theta is exactly pi/2 (not likely, but possible) need to know

if(delta > 0 ){
  #v.dist = 2/sin(delta)
  last.x <- start.x + 2*cos(delta)
  last.y <- start.y + 2*sin(delta)
} else {
  last.x <- start.x + 2
  last.y <- start.y
}

# generate 2nd transect w/ 18 stations
trans_id <- 2
stat.temp <- rbind(stat.temp, c(last.x, last.y, trans_id))

for(i in 2:18){
  next.x <- last.x+0.15*cos(theta)
  next.y <- last.y+0.15*sin(theta)
  stat.temp <- rbind(stat.temp, c(next.x, next.y, trans_id))
  last.x <- next.x
  last.y <- next.y
}

## @TODO, modify code to generate variable #'s of transects/stations

# if there was an angle in quadrant II, shit all x values to be > 0

if(min(stat.temp$x) < 0 ){
  stat.temp$x <- stat.temp[,1]+abs(min(stat.temp$x))
}

# get x,y dimensions
x.range <- max(stat.temp[,1]) - min(stat.temp[,1])
y.range <- max(stat.temp[,2]) - min(stat.temp[,2])



# randomly place transect w/in 0.5,5.5grid.

x.rng.max <- 5.5 - x.range
y.rng.max <- 5.5 - y.range

x.shift <- runif(1, 0.5, x.rng.max)
y.shift <- runif(1, 0.5, y.rng.max)

## adjust data frame
x.tem = stat.temp$x + x.shift
y.tem = stat.temp$y + y.shift

stat.df <- data.frame(
  stat.X = stat.temp$x + x.shift,
  stat.Y = stat.temp$y + y.shift,
  t_id = stat.temp$t_id
  )







angle <- seq(-pi, pi, length=50)
ell <- data.frame()
for(i in 1:nrow(stat.df)) {
  df <- data.frame(gp=i, a = sin(angle)*0.2+stat.df$stat.X[i], b = cos(angle)*0.2+stat.df$stat.Y[i])
  ell <- rbind(ell, df)
}


ggplot()+geom_point(data=x.y, aes(X,Y))+geom_point(data=stat.df, aes(x=stat.X, y=stat.Y), color="#D55E00", inherit.aes=FALSE)+geom_path(data=ell, aes(a,b,group=gp), colour="#D55E00", size=.75)+coord_fixed(xlim=c(-0.1,6.1), ylim=c(-0.1,6.1))





theta <- runif(1,-pi,pi)

## start point:
# x 0.5, y 1.44652

## left wall
(test.x <- start.x + 6*0.15*cos(pi/6))
(text.y <- start.y +6*0.15*sin(pi/6))
## [1] 1.279
## [1] 1.896

(test.x <- start.x + 6*0.15*cos(-pi/6))
(text.y <- start.y +6*0.15*sin(-pi/6))
#[1] 1.279
# [1] .9965


## Right wall
start.x <- 5.5
start.y <- 3
theta <- runif(1,pi/2, 3*pi/2)
(test.x <- start.x + 6*0.15*cos(5*pi/6))
(text.y <- start.y +6*0.15*sin(5*pi/6))
# 4.72, 3.45   x left, y up
(test.x <- start.x + 6*0.15*cos(7*pi/6))
(text.y <- start.y +6*0.15*sin(7*pi/6))
# 4.72, 2.55: x left, y down.


## floor
start.x <- 3
start.y <- 0.5
#theta <- runif(1,pi/2, 3*pi/2)
(test.x <- start.x + 6*0.15*cos(5*pi/6))
(text.y <- start.y +6*0.15*sin(5*pi/6))
# 4.72, 3.45   x left, y up
(test.x <- start.x + 6*0.15*cos(7*pi/6))
(text.y <- start.y +6*0.15*sin(7*pi/6))
# 4.72, 2.55: x left, y down.


#first, we need distance as a density
dens <- rep(0,70)
n <- nrow(coki.82)
dist <- coki.82$Distance
for(i in 1:70){
  upr <- i*10
  lwr <- upr-10
  dens[i] <- sum(lwr< dist & dist <= upr)/n
  
}

sum(dens[51:70])
# there is a 0.9% chance of seeing anything beyond 500 meters. That's not nothing, but it's pretty low.
sum(dens[56:70]) # 0.7% beyond 550 m.



###### =======TESTING=============
nsim <- 1000
d.list <- rep(0,nsim)

for(i in 1:nsim){
  paras <- rtv.hnorm(.5)
  x.y <- get.Coords(3600, 20)
  birds.seen <- data.frame()
  
  for(j in 1:6){
    for(k in 1:6){
      # set the min/max x&y values at which we can see a bird (truncating at 500 m)
      xmin <- stat.X[j]-0.5
      xmax <- stat.X[j]+0.5
      ymin <- stat.Y[k]-0.5
      ymax <- stat.Y[k]+0.5
      # filter down to those points.
      points <- filter(x.y, X>=xmin, X<=xmax, Y>=ymin, Y<=ymax)
      
      # calculate the straight-line distance to each bird, and teh probability for seeing it.
      points <- mutate(points, a=(X-stat.X[j])^2, b=(Y-stat.Y[k])^2, c=sqrt(a+b), idx=floor(c*100), prob=paras$w*dhalfnorm(c,paras$theta))
      
      #pull out the probability list, determine which birds are seen
      probs <- points$prob
      det <- detected(probs) # in VCP_Utilities.R
      
      # get index for seen birds, 
      idx <- which( det %in% 1)
      # get the direct line distance to that bird.
      obs.dist <- points$c[idx]
      
      #add to the list of birds that were observed.
      if(length(obs.dist)>0) {
        newbirds <- cbind(R.j=obs.dist, stat=(j*10+k))
        birds.seen <- rbind(birds.seen, newbirds)
      }
    }
  }
  
  # need m 
  birds.sum <- birds.seen %.% group_by(stat) %.% summarise(m=n())
  m <- c(birds.sum$m, rep(0, 36-length(birds.sum$m)))
  # need R.j, list of distances
  R.j <- birds.seen$R.j
  d.list[i] <- D.kernel.norm.VCP(m, R.j)

}

hist(d.list)

mean(d.list)
sd(d.list)
ggplot(dens)+geom_line()


## lets come at this from a different way. 



