## Confidence Interval


# need passed: h (bandwidth) and t(# of vcp), d.hat

VCP.inCI <- function(d.hat, h, true.D, t=36, alpha=0.05){
  # from Quang 1993, eq. 3.6
  z <- qnorm((1-alpha/2))
  u = z/(2*sqrt(2*t)*pi*h)
  
  lb <- (sqrt(u^2+d.hat)-u)^2
  ub <- (sqrt(u^2+d.hat)+u)^2
  
  isIn <- FALSE
  
  if( lb <= true.D & true.D <= ub) { isIn <- TRUE}
  
  return(isIn)
}



