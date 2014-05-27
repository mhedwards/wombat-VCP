## Transect Utility Functions

# These are the functions needed to generate two randomly positioned transects, within the sample space.

# Right now, it is (mostly) hard coded to make 2 transects of 18 stations at a 0.15 seperation
# within a 0< x,y < 6 square with a 0.5 buffer


make.transects <- function(n=2, n.stations=18, dist=0.15){
  
  # Degrees & unit circle reference
  # http://www.glogster.com/egroblero/the-unit-circle-mrs-mooring-pre-calc-/g-6n6upldvjsvpf9c6ebpk5a0
  
  # sin, cos, tangent refresher:
  # http://www.mathsisfun.com/sine-cosine-tangent.html
  
  
  #generate random angle theta from 0 to pi (Quadrants I, & II)
  theta <- runif(1,0,pi)
  start.x <- 0
  start.y <- 0
  
  # -- generate 1 transect with 18 stations --
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
  } else { 
    # if the line is vertical, shift right 2 km.
    last.x <- start.x + 2
    last.y <- start.y
  }
  
  # -- generate 2nd transect w/ 18 stations --
  trans_id <- 2
  stat.temp <- rbind(stat.temp, c(last.x, last.y, trans_id))
  
  for(i in 2:18){
    next.x <- last.x+0.15*cos(theta)
    next.y <- last.y+0.15*sin(theta)
    stat.temp <- rbind(stat.temp, c(next.x, next.y, trans_id))
    last.x <- next.x
    last.y <- next.y
  }
  
  # -- Adjust points to fit in our grid --
  
  # if there was an angle in quadrant II, shit all x values to be > 0
  # by default, we only have positive y values, so adjustment not needed.
  if(min(stat.temp$x) < 0 ){
    stat.temp$x <- stat.temp[,1]+abs(min(stat.temp$x))
  }
  
  # get x,y dimensions
  x.range <- max(stat.temp[,1]) - min(stat.temp[,1])
  y.range <- max(stat.temp[,2]) - min(stat.temp[,2])
  
  
  # -- randomly place transect w/in 0.5,5.5grid. --
  # our points stretch over a vertical and horizontal range that define a square that 
  # our transects lie within. if that box is placed so the upper right corner is at (5.5, 5.5),
  # the lower left corner is at some x,y. 
  # if we place the lower left corner randomly in the rectangle bounded on lower left by
  # (0.5,0.5) and upper right by that x,y, then the transects will fit within our buffered 
  # search area. 
  
  # get the value of x,y as described above
  x.rng.max <- 5.5 - x.range
  y.rng.max <- 5.5 - y.range
  
  # randomly generate the amount to shift our values by:
  x.shift <- runif(1, 0.5, x.rng.max)
  y.shift <- runif(1, 0.5, y.rng.max)
  
  ## compile & return final data frame with shifted values.
  stat.df <- data.frame(
    stat.X = stat.temp$x + x.shift,
    stat.Y = stat.temp$y + y.shift,
    t_id = stat.temp$t_id
  )
  
  return(stat.df)
}