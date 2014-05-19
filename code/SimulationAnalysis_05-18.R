library(ggplot2)

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


