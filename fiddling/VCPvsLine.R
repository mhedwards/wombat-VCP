
# E(n) = D*Area*Probability of observing something in that area. 

# I suspect that with the line transect, we'll see the nice shoulder curve that we are told to expect, but that with the circular plot, we'll see something that's lower near 0, simply due to the nature of the area of circles


D <- 20
hn.paras <- VCP.defineHalfnorm(.5)
L <- 36*.15 # length of single transect of 36 VCP separated by .15 km
r <- seq(0,.5, by=0.01)

pi*(.5^2) #- area of circle with radius= 0.5 = 0.7853982
L <- (pi*(.5^2))/(2*.5)
#L <- 1
A.lt <- 2*r*L
A.vcp <- pi*(r^2)

#N.lt <- D*(2*r*L)*VCP.dhnorm(r)
#N.vcp <- D*(pi*(r^2))*VCP.dhnorm(r)



A.lt.diff <- c(0,diff(A.lt))
A.vcp.diff <- c(0,diff(A.vcp))

hist(A.lt.diff)
hist(A.vcp.diff)

plot(r, A.lt)
plot(r, A.vcp)

library(ggplot2)
ggplot()+geom_line(aes(x=r, y=A.lt), size=2)+geom_line(aes(x=r, y=A.vcp), linetype="dashed", colour="red", size=2)+theme_bw(18)+xlab("Distance from transect")+ylab("Area covered")

ggsave("images/rect-circ-area.pdf", width=6, height=6)

## does this need a legend? probably. This is for a square and a circle with the same area.


# for each "band" further from the observer, in 0.01 km increments, get the expected # by multiplying the Density*Area*Probability of being seen there.

#For Probability in band, will use halfway point in interval to estimate. 

d.pts <- seq(0.005, .5, by=0.01)
p.dpts <- VCP.dhnorm(d.pts, hn.paras)


# Density * Area in Band * probability in band 

LT <- D * A.lt.diff[2:51]*p.dpts
VCP <- D * A.vcp.diff[2:51] * p.dpts

ggplot()+geom_line(aes(x=r[2:51], y=LT), size=2)+geom_line(aes(x=r[2:51], y=VCP), linetype="dashed", colour="red", size=2)+theme_bw(18)+xlab("Distance from transect")+ylab("E[N]")

ggsave("images/rect-circ-detection.pdf", width=6, height=6)


# what this says to me is that even with the best case scenario, original observation distance, accurate, detection exactly follows the half-normal curve , you will not see teh "shoulder" in the emperical 

# because of the way the area of a circle works, if we did see a shoulder in emperical data from VCP