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