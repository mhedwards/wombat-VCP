library(ggplot2)
library(dplyr)

OSUOrange <- rgb(243/255,115/255,33/255) #http://oregonstate.edu/brand/color-palette
OSURed <- rgb(192/255,49/225,26/255)
OSUBlue1 <- rgb(93/255, 135/255, 161/255)
OSUBlue2 <- rgb(156/255, 197/255, 202/255)

#### Original Sim 1 ------------------------------------------------------------------------
raw.data <- read.csv("data/Sim1_05-18.csv", stringsAsFactors=F, header=T)

d.hats <- c(raw.data$D.hnorm.S, raw.data$D.hnorm.R, raw.data$D.hnorm.T,
            raw.data$D.emp.S, raw.data$D.emp.R, raw.data$D.emp.T)

g.X <- c(rep("Half-normal", 3000), rep("Empirical",3000))
layout <- c(
    rep("Structured",1000),
    rep("Random", 1000),
    rep("Transect", 1000),
    rep("Structured",1000),
    rep("Random", 1000),
    rep("Transect", 1000)
  )


dhat.df <- data.frame(d.hats, g.X, layout)


ggplot(dhat.df, aes(x=d.hats))+geom_histogram(aes(y=..density..), binwidth=1, colour="black", fill="white")+facet_grid(layout~g.X)+theme_bw(18)+geom_vline(xintercept=20, colour="red", linetype="longdash")+xlab("Density Estimates")+ylab("Frequency")


ggsave("images/Emp_Vs_Hnorm.pdf", width=6, height=6)


dhat.df %.% group_by(g.X, layout) %.% 
  summarise(avg=mean(d.hats), sigma=sd(d.hats), med=median(d.hats),
            lb.95 = avg-1.96*sigma, ub.95=avg+1.96*sigma,
            lb.iqr = quantile(d.hats,0.25), ub.iqr=quantile(d.hats,0.75)
            )


#####  Mac Sim 1.2 ----------------------------------------------------------------------------
mac.sim <- tbl_df(read.csv("data/Sim1_05-31_MAC.csv", stringsAsFactors=F, header=T))

names(mac.sim) <- c("D.hat", "ContainsTrue", "Layout", "GType")


ggplot(mac.sim, aes(x=D.hat))+geom_histogram(aes(y=..density..), binwidth=1, colour="white", fill=OSUOrange)+facet_grid(Layout~GType)+theme_bw(18)+geom_vline(xintercept=20, colour=OSURed, linetype="longdash", size=1)+xlab("Density Estimates")+ylab("Frequency")+ggtitle("Mac Sim")

ggsave("images/Emp_Vs_Hnorm.pdf", width=6, height=6)

#hmm. not really the drastic differences I was expecting.
#after tweaking movement, it looks better

move.summ <- D.hat.df %.% group_by(Layout, MoveType) %.% summarise(avg=mean(Dhat), n.In=sum(inCI))


#####  PC Sim 1.2 ----------------------------------------------------------------------------
PC.sim <- tbl_df(read.csv("data/Sim1_05-31_PC.csv", stringsAsFactors=F, header=T))

names(PC.sim) <- c("D.hat", "ContainsTrue", "Layout", "GType")


ggplot(PC.sim, aes(x=D.hat))+geom_histogram(aes(y=..density..), binwidth=1, colour="white", fill=OSUOrange)+facet_grid(Layout~GType)+theme_bw(18)+geom_vline(xintercept=20, colour=OSURed, linetype="longdash", size=1)+xlab("Density Estimates")+ylab("Frequency")+xlim(0,38)#+ggtitle("PC Sim")

ggsave("images/Emp_Vs_Hnorm_1-2.pdf", width=8, height=6)

max(PC.sim$D.hat)
max(mac.sim$D.hat)
quantile(PC.sim$D.hat)
quantile(mac.sim$D.hat)

# > quantile(PC.sim$D.hat)
# 0%       25%       50%       75%      100% 
# 3.953212 13.074927 15.810915 18.782879 42.031921 
# 
# > quantile(mac.sim$D.hat)
# 0%       25%       50%       75%      100% 
# 3.717636 13.309291 15.925499 19.027584 35.544663 

PC.sim %.% group_by(Layout, GType) %.% summarise(avg=mean(D.hat), n=n(), n.In=sum(ContainsTrue), pct.in=n.In/n)


PC.sim %.% group_by(GType, Layout) %.%  summarise(n=n(), avg=mean(D.hat), std=sd(D.hat), 
                                                  LB= avg-1.96*std/sqrt(n), UB= avg-1.96*std/sqrt(n),
                                                  q25=quantile(D.hat, .25), med=median(D.hat), q75=quantile(D.hat, .75))

##### movement sim---------------

move.sim <- tbl_df(read.csv("data/moveSim_05-25.csv", header=T, stringsAsFactors=F))
move.sim

names(move.sim) <- c("D.hat", "ContainsTrue", "Layout", "MoveType")

move.sim %.% group_by(Layout, MoveType) %.% summarise(avg=mean(D.hat), n=n(), n.In=sum(ContainsTrue), pct.in=n.In/n)

move.sim %.% group_by(MoveType, Layout) %.%  summarise(n=n(), avg=mean(D.hat), std=sd(D.hat), 
                                                  q25=quantile(D.hat, .25), med=median(D.hat), q75=quantile(D.hat, .75))