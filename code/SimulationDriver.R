## Simulation Code

## ============ Needed Libraries ============ 
library(dplyr)
library(fdrtool) # for halfnormal which you need for project_functions

# ============ set up environment ============ 
D.known <- 20
max.r <- 0.5
nsim <- 1000

# === half normal parameters for g(r) ===
hn.params <- VCP.defineHalfnorm(max.r)

# for emperical g(r), pull original distances, scale to our "1=1 km" scale
raw.data <- tbl_df(read.csv("data/COKI.csv", stringsAsFactors=FALSE))
coki.82 <- filter(raw.data, Year==82)
distances <- coki.82$Distance/1000 
breakpoints <- seq(0,.7,.005)
emp.params <- VCP.defineLoess(distances, breakpoints)

# === Structured VCP points (Doesn't Change) ===
xy.structured <- VCP.structuredLayout() 


## ====================================================================================
#   Simulation for Half-normal Vs. Emperical, no movement
## ====================================================================================

# === Empty Data Frame for Results ===
D.hat.df <- data.frame()

# http://ryouready.wordpress.com/2009/03/16/r-monitor-function-progress-with-a-progress-bar/
#pb <- winProgressBar(title="progress bar", min=0, max=nsim, width=300)
# didn't work on Mac for somereason >_>

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
  
  ## Half normal g(r)------------------
  #D.hnorm.S <- VCP.dHat(x.y, xy.structured, hn.params, w=max.r, g.type="hnorm")
  #D.hnorm.R <- VCP.dHat(x.y, xy.random, hn.params, w=max.r, g.type="hnorm")
  #D.hnorm.T <- VCP.dHat(x.y, xy.transect, hn.params, w=max.r, g.type="hnorm")
  
  D.S.hnorm <- c(VCP.dHat(x.y, xy.structured, hn.params, w=max.r, g.type="hnorm",
                           true.D=D.known),
                  Layout="structured", GType="HalfNorm")
  D.R.hnorm <- c(VCP.dHat(x.y, xy.random, hn.params, w=max.r, g.type="hnorm",
                           true.D=D.known),
                  Layout="random", GType="HalfNorm")
  D.T.hnorm <- c(VCP.dHat(x.y, xy.transect, hn.params, w=max.r, g.type="hnorm",
                           true.D=D.known),
                  Layout="transect", GType="HalfNorm")
  ## Emperical g(r)------------------
  #D.emp.S <- VCP.dHat(x.y, xy.structured, emp.params, w=max.r, g.type="emp")
  #D.emp.R <- VCP.dHat(x.y, xy.random, emp.params, w=max.r, g.type="emp")
  #D.emp.T <- VCP.dHat(x.y, xy.transect, emp.params, w=max.r, g.type="emp")
  D.S.emp <- c(VCP.dHat(x.y, xy.structured, emp.params, w=max.r, g.type="emp",
                          true.D=D.known),
                 Layout="structured", GType="Emperical")
  D.R.emp <- c(VCP.dHat(x.y, xy.random, emp.params, w=max.r, g.type="emp",
                          true.D=D.known),
                 Layout="random", GType="Emperical")
  D.T.emp <- c(VCP.dHat(x.y, xy.transect, emp.params, w=max.r, g.type="emp",
                          true.D=D.known),
                 Layout="transect", GType="Emperical")
  
  ## Add to data frame --------------
  #D.pass <- cbind(D.hnorm.S, D.hnorm.R, D.hnorm.T, D.emp.S, D.emp.R, D.emp.T)
  #D.hat.df <- rbind(D.hat.df, D.pass) # add to data frame
  
  D.hat.df <- rbind(D.hat.df,
                    D.S.hnorm, D.R.hnorm, D.T.hnorm,
                    D.S.emp, D.R.emp, D.T.emp)
  
  #setWinProgressBar(pb, i, title=paste(round(i/nsim*100,0),"% done"))
}
## END Loop -----------------------------------------------------------------

write.csv(D.hat.df, "data/Sim1_05-31.csv", row.names=FALSE)
#   already done, don't want to overwrite original simulation data.
