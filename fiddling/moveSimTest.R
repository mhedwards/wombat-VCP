
## ============ Needed Libraries ============ 
library(dplyr)
library(fdrtool) # for halfnormal which you need for project_functions
library(ggplot2)

# ============ set up environment ============ 
D.known <- 20
max.r <- 0.5
nsim <- 1000
OSUOrange <- rgb(243/255,115/255,33/255) #http://oregonstate.edu/brand/color-palette
OSURed <- rgb(192/255,49/225,26/255)
OSUBlue1 <- rgb(93/255, 135/255, 161/255)
OSUBlue2 <- rgb(156/255, 197/255, 202/255)

# === half normal parameters for g(r) ===
hn.params <- VCP.defineHalfnorm(max.r)

# not doing emperical for this
# for emperical g(r), pull original distances, scale to our "1=1 km" scale
#raw.data <- tbl_df(read.csv("data/COKI.csv", stringsAsFactors=FALSE))
#coki.82 <- filter(raw.data, Year==82)
#distances <- coki.82$Distance/1000 
#breakpoints <- seq(0,.7,.005)
#emp.params <- VCP.defineLoess(distances, breakpoints)

# === Structured VCP points (Doesn't Change) ===
xy.structured <- VCP.structuredLayout() 


## ====================================================================================
#   Simulation for Half-normal Vs. Emperical, no movement
## ====================================================================================

# === Empty Data Frame for Results ===
D.hat.df <- data.frame()

# http://ryouready.wordpress.com/2009/03/16/r-monitor-function-progress-with-a-progress-bar/
pb <- winProgressBar(title="progress bar", min=0, max=nsim, width=300)

## IN loop -----------------------------------------------------------------
for(i in 1:nsim){
  # generate map
  x.y = get.Coords(D.known)
  
  # get random VCP coords
  xy.random <- VCP.randomLayout()
  xy.transect <- VCP.transectLayout()
  
  # for each VCP layout
  #   for each detection function
  #     get the density estimate.
  
  # no movement
  
  D.S.nomove <- c(VCP.dHat(x.y, xy.structured, hn.params, w=max.r, g.type="hnorm",
                           true.D=D.known),
                  Layout="structured", MoveType="NoMove")
  D.R.nomove <- c(VCP.dHat(x.y, xy.random, hn.params, w=max.r, g.type="hnorm",
                           true.D=D.known),
                  Layout="random", MoveType="NoMove")
  D.T.nomove <- c(VCP.dHat(x.y, xy.transect, hn.params, w=max.r, g.type="hnorm",
                           true.D=D.known),
                  Layout="transect", MoveType="NoMove")
  
  # temp movement
  
  D.S.temp <- c(VCP.dhat.movement(x.y, xy.structured, hn.params, w=max.r, g.type="hnorm",
                                  m.type="temp", true.D=D.known), 
                Layout="structured", MoveType="TempMove")
  D.R.temp <- c(VCP.dhat.movement(x.y, xy.random, hn.params, w=max.r, g.type="hnorm",
                                  m.type="temp", true.D=D.known), 
                Layout="random", MoveType="TempMove")
  D.T.temp <- c(VCP.dhat.movement(x.y, xy.transect, hn.params, w=max.r, g.type="hnorm",
                                  m.type="temp", true.D=D.known), 
                Layout="transect", MoveType="TempMove")
  
  ## compounded movement
  
  D.S.perm <- c(VCP.dhat.movement(x.y, xy.structured, hn.params, w=max.r, g.type="hnorm",
                                  m.type="perm", true.D=D.known), 
                Layout="structured", MoveType="PermMove")
  D.R.perm <- c(VCP.dhat.movement(x.y, xy.random, hn.params, w=max.r, g.type="hnorm",
                                  m.type="perm", true.D=D.known), 
                Layout="random", MoveType="PermMove")
  D.T.perm <- c(VCP.dhat.movement(x.y, xy.transect, hn.params, w=max.r, g.type="hnorm",
                                  m.type="perm", true.D=D.known), 
                Layout="transect", MoveType="PermMove")
  
  #D.pass <- rbind(D.S.nomove, D.R.nomove, D.T.nomove,
  #                D.S.temp, D.R.temp, D.T.temp,
  #                D.S.perm, D.R.perm, D.T.perm)
  #D.pass <- cbind(D.emp.S)
  
  #D.hat.df <- rbind(D.hat.df, D.pass) # add to data frame
  
  D.hat.df <- rbind(D.hat.df,
                    D.S.nomove, D.R.nomove, D.T.nomove,
                  D.S.temp, D.R.temp, D.T.temp,
                  D.S.perm, D.R.perm, D.T.perm)
  
  setWinProgressBar(pb, i, title=paste(round(i/nsim*100,0),"% done"))
}
## END Loop -----------------------------------------------------------------

close(pb)

names(D.hat.df) <- c("D.hat", "ContainsTrue", "Layout", "MoveType")
row.names(D.hat.df) <- NULL

write.csv(D.hat.df, "data/moveSim_05-25.csv", row.names=F)

ggplot(D.hat.df, aes(x=Dhat))+geom_histogram(aes(y=..density..), binwidth=1, colour="white", fill=OSUOrange)+facet_grid(Layout~MoveType)+theme_bw(18)+geom_vline(xintercept=20, colour=OSURed, linetype="longdash", size=1)+xlab("Density Estimates")+ylab("Frequency")

#hmm. not really the drastic differences I was expecting.
#after tweaking movement, it looks better

D.hat.df %.% group_by(Layout, MoveType) %.% summarise(avg=mean(Dhat), n.In=sum(inCI))

# the lack of difference between PermMove and TempMove makes sense. The majority of the movement (if that's what it was) was within the first 100 m. The stations are 150 m apart. It's not going to affect those quite as much, unless you have REALLY closely spaces VCP.


## ------ Set up for by-hand Method test
xy.vcp <- VCP.structuredLayout()
xy.objects <- get.Coords(20)
params <- VCP.defineHalfnorm(.5)
w <- 0.5
g.type <- "hnorm"
m.type <- "temp"
true.D <- 20
## ------ End set up

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