library(ggplot2)
library(dplyr)

x.y <- get.Coords(3600, 20) # simulating the area surveyed in Palie

#https://groups.google.com/forum/#!topic/ggplot2/f0I4tWWOhbs
angle <- seq(-pi, pi, length=50)
df <- data.frame(a = sin(angle)*0.2, b = cos(angle)*0.2)


ggplot(x.y, aes(X, Y))+geom_point()+ geom_path(aes(a, b), data = df, inherit.aes = F)


## 30 random points, centers of VCPs
stat.X <- rep(seq(0.5, 5.5,1),6)
stat.Y <- sort(rep(seq(0.5, 5.5,1),6))

angle <- seq(-pi, pi, length=50)
ell <- data.frame()
for(i in 1:36) {
  df <- data.frame(group=i, a = sin(angle)*0.2+stat.X[i], b = cos(angle)*0.2+stat.Y[i])
  ell <- rbind(ell, df)
}

ggplot()+geom_point(data=x.y, aes(X, Y))+geom_point(aes(x=stat.X, y=stat.Y, colour="red"), inherit.aes=FALSE)+geom_path(data=ell, aes(a,b,group=group, colour='red'))


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



