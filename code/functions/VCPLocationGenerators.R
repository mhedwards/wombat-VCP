## Station Generation Code


get.Coords <- function(dens){
  # hard coding for 36km^2, with a 0.5 buffer on either side, then trimming.
  km2 <- 49
  cmax <- 6.5
  cmin <- -0.5
  X <- runif(km2*dens, min=cmin, max=cmax)
  Y <- runif(km2*dens, min=cmin, max=cmax)
  idx <- seq(1:length(X))
  ret <- data.frame(x=X,y=Y,o_id=idx)
  # ret2 <- filter(ret, !((X < 0 | X > 6) | (Y < 0 | Y > 6)))
  return (ret)
}

## Generates a structured layout
#   hardcoded (yes, bad) for my partiular set up.
#   returns a data frame with columns x and y
VCP.structuredLayout <- function(n=36) {
  stat.X <- rep(seq(0.5, 5.5,1),6)
  stat.Y <- sort(rep(seq(0.5, 5.5,1),6))
  s_id <- seq(1,n)
  t_id <- rep(1,n)
  stations <- data.frame(x=stat.X, y=stat.Y, s_id=s_id, t_id=t_id)
  return(stations)
}


## n = quantity to generate, min= minimum x and y value, max=max x and y value
#   assumes square layout.
#   returns a data frame with columns x and y
VCP.randomLayout <- function(n=36, min=0.5, max=5.5){
  stat.X <- runif(n, min, max)
  stat.Y <- runif(n, min, max)
  s_id <- seq(1,n)
  t_id <- rep(1,n)
  stations <- data.frame(x=stat.X, y=stat.Y, s_id=s_id, t_id=t_id)
  return(stations)
}


## Generates a random-systematic layout with 2 transects and a 0.15 (150 m) distance between stations, and 2km distance between transects
#   returns a data frame with columns x, y and t_id for transect ID.
#   hardcoded (still bad) for my partiular set up.
VCP.transectLayout <- function(n=36) {
  
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
  
  s_id = seq(1,n)
  stat.df <- data.frame(
    x = stat.temp$x + x.shift,
    y = stat.temp$y + y.shift,
    s_id = s_id,
    t_id = stat.temp$t_id
  )
  
  return (stat.df)
}