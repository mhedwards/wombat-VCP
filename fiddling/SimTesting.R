### test simulation
library(dplyr)
library(fdrtool) # for halfnormal which you need for project_functions

# set up environment
D.known <- 20
max.r <- 0.5
nsim <- 1000

# half normal parameters for g(r)
hn.params <- VCP.defineHalfnorm(max.r)

# for emperical g(r), pull original distances, scale to our "1=1 km" scale
raw.data <- tbl_df(read.csv("data/COKI.csv", stringsAsFactors=FALSE))
coki.82 <- filter(raw.data, Year==82)
distances <- coki.82$Distance/1000 
breakpoints <- seq(0,.7,.005)
emp.params <- VCP.defineLoess(distances, breakpoints)

# VCP points
xy.structured <- VCP.structuredLayout() # won't change


D.hat.df <- data.frame()

# http://ryouready.wordpress.com/2009/03/16/r-monitor-function-progress-with-a-progress-bar/
pb <- winProgressBar(title="progress bar", min=0, max=nsim, width=300)

## IN loop =======================================
for(i in 1:nsim){
  # generate map
  x.y = get.Coords(D.known)
  
  # get random VCP coords
  xy.random <- VCP.randomLayout()
  xy.transect <- VCP.randSystematicLayout()
  
  # for each VCP layout
  #   for each detection function
  #     get the density estimate.
  
  D.hnorm.S <- VCP.dHat(x.y, xy.structured, hn.params, w=max.r, g.type="hnorm")
  D.hnorm.R <- VCP.dHat(x.y, xy.random, hn.params, w=max.r, g.type="hnorm")
  D.hnorm.T <- VCP.dHat(x.y, xy.transect, hn.params, w=max.r, g.type="hnorm")
  
  #D.pass <- cbind(D.hnorm.S, D.hnorm.R, D.hnorm.T) # group variables
  
  D.emp.S <- VCP.dHat(x.y, xy.structured, emp.params, w=max.r, g.type="emp")
  D.emp.R <- VCP.dHat(x.y, xy.random, emp.params, w=max.r, g.type="emp")
  D.emp.T <- VCP.dHat(x.y, xy.transect, emp.params, w=max.r, g.type="emp")
  
  D.pass <- cbind(D.hnorm.S, D.hnorm.R, D.hnorm.T, D.emp.S, D.emp.R, D.emp.T)
  #D.pass <- cbind(D.emp.S)
  
  D.hat.df <- rbind(D.hat.df, D.pass) # add to data frame
  
  setWinProgressBar(pb, i, title=paste(round(i/nsim*100,0),"% done"))
}


write.csv(D.hat.df, "data/Sim1_05-18.csv", row.names=FALSE)

close(pb)

names(D.hat.df) <- c("hnorm.S")
hist(D.hat.df$D.hnorm.S)
hist(D.hat.df$D.hnorm.R)
hist(D.hat.df$D.hnorm.T)

mean(D.hat.df$D.hnorm.S)
mean(D.hat.df$D.hnorm.R)
mean(D.hat.df$D.hnorm.T)


mean(D.hat.df$D.hnorm.S)+c(-1,1)*1.96*sd(D.hat.df$D.hnorm.S)
mean(D.hat.df$D.hnorm.R)+c(-1,1)*1.96*sd(D.hat.df$D.hnorm.R)
mean(D.hat.df$D.hnorm.T)+c(-1,1)*1.96*sd(D.hat.df$D.hnorm.T)

median(D.hat.df$D.hnorm.S)
median(D.hat.df$D.hnorm.R)
median(D.hat.df$D.hnorm.T)
## it's biased low, which I also observed in the Vole simulations.




hist(D.hat.df$D.emp.S)
hist(D.hat.df$D.emp.R)
hist(D.hat.df$D.emp.T)

mean(D.hat.df$D.emp.S)
mean(D.hat.df$D.emp.R)
mean(D.hat.df$D.emp.T)


mean(D.hat.df$D.emp.S)+c(-1,1)*1.96*sd(D.hat.df$D.emp.S)
mean(D.hat.df$D.emp.R)+c(-1,1)*1.96*sd(D.hat.df$D.emp.R)
mean(D.hat.df$D.emp.T)+c(-1,1)*1.96*sd(D.hat.df$D.emp.T)

median(D.hat.df$D.emp.S)
median(D.hat.df$D.emp.R)
median(D.hat.df$D.emp.T)


hist(D.hat.df$D.emp.S)

### set up for manual looping----
xy.objects <- x.y
xy.vcp <- xy.structured
params <- emp.params
g.type= "emp"
i <- 1
w <- max.r
####

## g.type can be "hnorm" or "emp"; it defaults to "hnorm" if an unrecognized value entered

VCP.dHat <- function(xy.objects, xy.vcp, params, w=0.5, g.type="hnorm", transects=FALSE){
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

## loop testing ---
D.hat.df <- rbind(D.hat.df, dhat)
i <- i+1
##

### issue w/ empircal, getting: Error in if (p > 1) { : missing value where TRUE/FALSE needed
dist <- candidate.xy$R.j
pi.raw <- predict(emp.params$dist.lo, dist)*emp.params$delta
dist[26]
## okay, so what's happening, is that the empirical data only goes out to .700, so anything beyond that is returning an NA in terms of probability, instead of 0. so what needs to happen is to truncate everything 


### examining plot
stat.X <- xy.vcp$x
stat.Y <- xy.vcp$y
angle <- seq(-pi, pi, length=50)
ell <- data.frame()
for(i in 1:36) {
  df <- data.frame(gp=i, a = sin(angle)*0.2+stat.X[i], b = cos(angle)*0.2+stat.Y[i])
  ell <- rbind(ell, df)
}
ggplot()+geom_point(data=xy.objects, aes(x,y))+geom_point(aes(x=stat.X, y=stat.Y), color="#D55E00", inherit.aes=FALSE)+geom_path(data=ell, aes(a,b,group=gp), colour="#D55E00", size=.75)+coord_fixed(xlim=c(-0.1,6.1), ylim=c(-0.1,6.1))+theme_bw(18)


## needs a vector m, that is the length of the # of VCPS, and a vector R.j that is all the detected distances.


VCP.Dhat.hnormKernel <- function(m, R.j) {
  #VCP density from Quang, 1993
  m.bar <- mean(m) # mean # of objects per plot.
  n.hat <- sum(m) #sum of objects surveyed
  h <- sd(R.j)*n.hat^(-1/5)
  t <- length(m)
  #D.hat.2 <- (1/(sqrt(2*pi)*pi*(h^3)*t)) * sum(R.j*exp(-((R.j)^2)/(2*h^2)    ))
  # Quang provides two methods, the previous line should return same value as following sequence.
  K <- sqrt(2 * pi)^(-1) * (exp((-0.5) * (R.j/h)^2) * ((-0.5) * (2 * (R.j/h))))
  B.hat = (-2/(n.hat*h^2))*sum(K)
  D.hat = (m.bar*B.hat)/(2*pi)
  return(D.hat)  
}