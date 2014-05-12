# Example Graphs

# requires dplyr
# requires get.Coords from VCP_Utilities.R

library(dplyr)

# get coords, using a density of 20 birds/km2
x.y <- get.Coords( 20)

## -------------- Structured VCP layout -------------- 
# get the equi-spaced, independent VCP locations.
stat.X <- rep(seq(0.5, 5.5,1),6)
stat.Y <- sort(rep(seq(0.5, 5.5,1),6))

angle <- seq(-pi, pi, length=50)
ell <- data.frame()
for(i in 1:36) {
  df <- data.frame(gp=i, a = sin(angle)*0.2+stat.X[i], b = cos(angle)*0.2+stat.Y[i])
  ell <- rbind(ell, df)
}

# generate and save plot
# you will get warnings when running these, because it is trimming out points to get to the 0,6 bounds
ggplot()+geom_point(data=x.y, aes(X,Y))+geom_point(aes(x=stat.X, y=stat.Y), color="#D55E00", inherit.aes=FALSE)+geom_path(data=ell, aes(a,b,group=gp), colour="#D55E00", size=.75)+coord_fixed(xlim=c(-0.1,6.1), ylim=c(-0.1,6.1))+theme_bw(18)

ggsave("images/layout_structured.pdf", width=8, height=8)


## -------------- Random VCP layout -------------- 
# get random x,y for VCP's
stat.X <- runif(36, 0.5, 5.5)
stat.Y <- runif(36, 0.5, 5.5)

# get circle boundaries
angle <- seq(-pi, pi, length=50)
ell <- data.frame()
for(i in 1:36) {
  df <- data.frame(gp=i, a = sin(angle)*0.2+stat.X[i], b = cos(angle)*0.2+stat.Y[i])
  ell <- rbind(ell, df)
}

# generate and save plot
ggplot()+geom_point(data=x.y, aes(X,Y))+geom_point(aes(x=stat.X, y=stat.Y), color="#D55E00", inherit.aes=FALSE)+geom_path(data=ell, aes(a,b,group=gp), colour="#D55E00", size=.75)+coord_fixed(xlim=c(-0.1,6.1), ylim=c(-0.1,6.1))+theme_bw(18)

ggsave("images/layout_random.pdf", width=8, height=8)



## -------------- Random-Systematic VCP layout -------------- 
# Degrees & unit circle reference
# http://www.glogster.com/egroblero/the-unit-circle-mrs-mooring-pre-calc-/g-6n6upldvjsvpf9c6ebpk5a0

# sin, cos, tangent refresher:
# http://www.mathsisfun.com/sine-cosine-tangent.html


#generate random angle theta from 0 to pi (Quadrants I, & II)
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

# get delta = angle perpendicular to line on angle theta
delta <-0
if(theta < (pi/2)){ 
  # if we're in quadrant I, add 90 degrees
  delta <- pi/2 + theta
} else if( theta > (pi/2)){ 
  # if we're in quadrant II, subtract 90 degrees
  delta <- theta - pi/2
} else { delta <- -1 }  # if theta is exactly pi/2 (not likely, but possible) need to know

if(delta > 0 ){
  last.x <- start.x + 2*cos(delta)
  last.y <- start.y + 2*sin(delta)
} else { # if the line is vertical, shift right 2 km.
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


ggsave("images/layout_rand-sys-6.pdf", width=8, height=8)
