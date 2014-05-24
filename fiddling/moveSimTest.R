
xy.vcp <- VCP.structuredLayout()
xy.objects <- get.Coords(20)
params <- VCP.defineHalfnorm(.5)
w <- 0.5
g.type <- "hnorm"
m.type <- "temp"
true.D <- 20

VCP.dhat.movement <- function ((xy.objects, xy.vcp, params, w=0.5, g.type="hnorm", m.type="temp", true.D=20)){
  n.vcp <- nrow(xy.vcp)
  m <- rep(0, n.vcp)
  observed <- data.frame()
  
  # for each VCP
  
  # filter points to current
  
  # initiate movement
  
  # get observed based on new positions
  
  # if mtype="perm"
    # update original database.
  
  # once done w/ all VCP, get D.hat based on movement positions.
  
  
}