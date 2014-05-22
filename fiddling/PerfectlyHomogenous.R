library(ggplot2)
library(fdrtool)

xvals <- seq(.1,6, by =.2)
yvals <-seq(.25/4, 6, by=.25)

30*24
6*6*20


xy.perfect <- data.frame()
ny <- length(yvals)
for(i in 1:length(xvals)){
 x <- rep(xvals[i],ny)
 new <- cbind(x, y=yvals)
 
 xy.perfect <- rbind(xy.perfect, new)
  
}



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
ggplot()+geom_point(data=xy.perfect, aes(x,y))+geom_point(aes(x=stat.X, y=stat.Y), color="#D55E00", inherit.aes=FALSE)+geom_path(data=ell, aes(a,b,group=gp), colour="#D55E00", size=.75)+coord_fixed(xlim=c(-0.1,6.1), ylim=c(-0.1,6.1))+theme_bw(18)

#ggsave("images/layout_structured.pdf", width=8, height=8)

xy.structured <- VCP.structuredLayout()
xy.random <- VCP.randomLayout()
xy.transect <- VCP.randSystematicLayout()
hn.params <- VCP.defineHalfnorm(.5)

my.data <- data.frame()
for(i in 1:50){
  xy.random <- VCP.randomLayout()
  xy.transect <- VCP.randSystematicLayout()
  
  d.s <- VCP.dHat(xy.perfect, xy.structured, hn.params, w=0.5, g.type="hnorm")
  d.r <- VCP.dHat(xy.perfect, xy.random, hn.params, w=0.5, g.type="hnorm")
  d.t <- VCP.dHat(xy.perfect, xy.transect, hn.params, w=0.5, g.type="hnorm")
  new <- data.frame(D.hat = c(d.s, d.r, d.t), Layout=c("S", "R", "T"))
  my.data <- rbind(my.data, new)
}


ggplot(my.data, aes(x=D.hat))+geom_histogram(aes(y=..density..), colour="black", fill="white")+facet_grid(Layout~.)+theme_bw(18)+geom_vline(xintercept=20, colour="red", linetype="longdash")+xlab("Density Estimates")+ylab("Frequency")
## error about Position_stack requires constant width - answer: using CBIND was making them both characters which were read as factors in the data frame.

"Structured" is biased VERY high, around 35. Random is biased low, and more spread out.
Transect is still biased low, but less spread out, and kind of lumped right under 20, which I think is a very interesting observation.
