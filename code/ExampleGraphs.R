# Example Graphs
OSUOrange <- rgb(243/255,115/255,33/255) #http://oregonstate.edu/brand/color-palette
OSURed <- rgb(192/255,49/225,26/255)
OSUBlue1 <- rgb(93/255, 135/255, 161/255)
OSUBlue2 <- rgb(156/255, 197/255, 202/255)
OSUMedBrn <- rgb(176/255,96/255,16/255)
OSUDkGray <- rgb(171/255,175/255,166/255)
# requires dplyr
# requires get.Coords from VCP_Utilities.R

library(dplyr)
library(ggplot2)

# get coords, using a density of 20 birds/km2
x.y <- get.Coords(20)

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
ggplot()+geom_point(data=x.y, aes(x,y), color=OSUDkGray)+geom_point(aes(x=stat.X, y=stat.Y), color=OSUOrange, inherit.aes=FALSE)+geom_path(data=ell, aes(a,b,group=gp), colour=OSUOrange, size=.75)+coord_fixed(xlim=c(-0.1,6.1), ylim=c(-0.1,6.1))+theme_bw(18)

ggsave("images/layout_structured.pdf", width=8, height=8)


ggplot()+geom_point(data=x.y, aes(x,y), color=OSUDkGray)+geom_point(aes(x=stat.X, y=stat.Y), color=OSUMedBrn, inherit.aes=FALSE)+geom_path(data=ell, aes(a,b,group=gp), colour=OSUMedBrn, size=.75)+coord_fixed(xlim=c(-0.1,6.1), ylim=c(-0.1,6.1))+theme_bw(18)
ggsave("images/slides-layoutS.pdf", width=8, height=8)


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

ggplot()+geom_point(data=x.y, aes(x,y), color=OSUDkGray)+geom_point(aes(x=stat.X, y=stat.Y), color=OSUMedBrn, inherit.aes=FALSE)+geom_path(data=ell, aes(a,b,group=gp), colour=OSUMedBrn, size=.75)+coord_fixed(xlim=c(-0.1,6.1), ylim=c(-0.1,6.1))+theme_bw(18)

ggsave("images/slides-layoutR.pdf", width=8, height=8)

# generate and save plot
ggplot()+geom_point(data=x.y, aes(x,y))+geom_point(aes(x=stat.X, y=stat.Y), color=OSUOrange, inherit.aes=FALSE)+geom_path(data=ell, aes(a,b,group=gp), colour=OSUOrange, size=.75)+coord_fixed(xlim=c(-0.1,6.1), ylim=c(-0.1,6.1))+theme_bw(18)

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

stat.df <- VCP.transectLayout()

angle <- seq(-pi, pi, length=50)
ell <- data.frame()
for(i in 1:nrow(stat.df)) {
  df <- data.frame(gp=i, a = sin(angle)*0.2+stat.df$x[i], b = cos(angle)*0.2+stat.df$y[i])
  ell <- rbind(ell, df)
}


ggplot()+geom_point(data=x.y, aes(x,y), color=OSUDkGray)+geom_point(data=stat.df, aes(x=x, y=y), color=OSUMedBrn, inherit.aes=FALSE)+geom_path(data=ell, aes(a,b,group=gp), colour=OSUMedBrn, size=.75)+coord_fixed(xlim=c(-0.1,6.1), ylim=c(-0.1,6.1))+theme_bw(18)


ggsave("images/slides-layoutT1.pdf", width=8, height=8)
ggsave("images/slides-layoutT2.pdf", width=8, height=8)


ggplot()+geom_point(data=x.y, aes(x,y))+geom_point(data=stat.df, aes(x=x, y=y), color=OSUOrange, inherit.aes=FALSE)+geom_path(data=ell, aes(a,b,group=gp), colour=OSUOrange, size=.75)+coord_fixed(xlim=c(-0.1,6.1), ylim=c(-0.1,6.1))+theme_bw(18)


ggsave("images/layout_rand-sys-4.pdf", width=8, height=8)


## ------------------------  LOess REgression ----------------------------------

hist.info2<- hist(coki.82$Distance, breaks=seq(0,700,5), freq=TRUE)
by5 <- data.frame(x=hist.info2$mids, y=hist.info2$density*5)
by5.lo <- loess(y~x, data=by5, span=0.2)
predict(by5.lo,100)
th.x <- seq(0,700,5)
th.y <- predict(by5.lo,th.x)
th.y[1] <- th.y[141] <- 0


ggplot()+geom_histogram(data=by5, aes(x, y), stat="identity", fill=OSUOrange)+geom_smooth(data=by5, aes(x, y), method="loess", span=0.2, size=3, color=OSURed)+geom_line(inherit.aes=FALSE, aes(x=th.x, y=th.y), color=OSUBlue2, linetype=2, size=2)+theme_bw(18)+xlab("Detection Distance in Meters")+ylab('Detection Density')

ggsave("images/loess.pdf", width=8, height=4)

which.max(th.y) # 19
th.y[19] # 0.02464387

th.x[19]
sum(th.y, na.rm=T) #make sure this goes to 1, it does.

delta.RS = 1/th.y[19]

m.500 <- predict(by5.lo, 500)

delta.RS*m.500


hnorm.p <- VCP.defineHalfnorm(.5)
th.x=seq(0,.6,length=100)
th.y=dhalfnorm(th.x, hnorm.p$theta)*hnorm.p$delta
plot(x=th.x, y=th.y, type="l")


### -----------------------------------------------------------------
ggplot()+geom_point(data=x.y, aes(x,y), color=OSUDkGray)+coord_fixed()+theme_bw(18)+geom_rect(mapping=aes(xmin=0, xmax=6, ymin=0, ymax=6), alpha=0, color=OSUMedBrn, linetype="longdash", size=1.5)+geom_rect(mapping=aes(xmin=0.5, xmax=5.5, ymin=0.5, ymax=5.5), alpha=0, color=OSUOrange, linetype="longdash", size=1.5)

ggsave("images/slides-pointGeneration.pdf", width=8, height=8)



#+geom_point(data=stat.df, aes(x=x, y=y), color=OSUMedBrn, inherit.aes=FALSE)+geom_path(data=ell, aes(a,b,group=gp), colour=OSUMedBrn, size=.75)+coord_fixed(xlim=c(-0.1,6.1), ylim=c(-0.1,6.1))+theme_bw(18)
