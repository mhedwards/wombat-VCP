library(fdrtool) #half normal
library(ggplot2)

OSUOrange <- rgb(243/255,115/255,33/255) #http://oregonstate.edu/brand/color-palette
OSURed <- rgb(192/255,49/225,26/255)
OSUBlue1 <- rgb(93/255, 135/255, 161/255)
OSUBlue2 <- rgb(156/255, 197/255, 202/255)

hn <- VCP.defineHalfnorm(500)

x <- seq(0, 500, length=100)
y <- dhalfnorm(x, hn$theta)*hn$delta





ggplot()+geom_line(aes(x=x, y=y), size=2, color=OSUOrange)+theme_bw(18)+xlab("Distance from transect: r")+ylab("g(r)")

ggsave("images/detectionCurve.pdf", width=8, height=6)



hn.5 <- VCP.defineHalfnorm(.5)

x <- seq(0, .5, length=100)
y <- dhalfnorm(x, hn.5$theta)*hn.5$delta

ggplot()+geom_line(aes(x=x, y=y), size=2, color=OSUOrange)+theme_bw(18)+xlab("Distance from transect: r")+ylab("g(r)")

ggsave("images/detectionCurveScaled.pdf", width=8, height=6)



#### circles vs squares
OSUOrange <- rgb(243/255,115/255,33/255) #http://oregonstate.edu/brand/color-palette
OSURed <- rgb(192/255,49/225,26/255)
OSUBlue1 <- rgb(93/255, 135/255, 161/255)
OSUBlue2 <- rgb(156/255, 197/255, 202/255)
OSUMedBrn <- rgb(176/255,96/255,16/255)
OSUDkGray <- rgb(171/255,175/255,166/255)
# requires dplyr
# requires get.Coords from VCP_Utilities.R

library(dplyr)
library(ggplot2)

# get coords, using a density of 20 birds/km2
x.y <- get.Coords(20)



ggplot()+geom_point(data=x.y, aes(x,y), color=OSUDkGray)+coord_fixed(xlim=c(-0.1,6.1), ylim=c(-0.1,6.1))+theme_bw(18)+
  geom_rect(mapping=aes(xmin=1, xmax=5, ymin=0, ymax=6), alpha=.75, fill=OSUOrange)+
  geom_rect(mapping=aes(xmin=2, xmax=4, ymin=0, ymax=6), alpha=.75, fill=OSUMedBrn)+
  geom_vline(xintercept=c(3), color=OSUBlue2, size=2, linetype="dashed")

ggsave("images/slides-LTr.pdf", width=8, height=8)


stat.df <- data.frame(x=3,y=3)
angle <- seq(-pi, pi, length=50)
r2 <- data.frame()
for(i in 1:nrow(stat.df)) {
  df <- data.frame(gp=i, a = sin(angle)*2+stat.df$x[i], b = cos(angle)*2+stat.df$y[i])
  r2 <- rbind(r2, df)
}

r1 <- data.frame()
for(i in 1:nrow(stat.df)) {
  df <- data.frame(gp=i, a = sin(angle)*1+stat.df$x[i], b = cos(angle)*1+stat.df$y[i])
  r1 <- rbind(r1, df)
}


ggplot()+geom_point(data=x.y, aes(x,y), color=OSUDkGray)+coord_fixed(xlim=c(-0.1,6.1), ylim=c(-0.1,6.1))+theme_bw(18)+
  geom_polygon(data=r2, aes(x=a, y=b), alpha=.75, fill=OSUOrange)+
  geom_polygon(data=r1, aes(x=a, y=b), alpha=.75, fill=OSUMedBrn)+
  geom_point(aes(x=3,y=3), color=OSUBlue2, size=5)

ggsave("images/slides-VCPr.pdf", width=8, height=8)
