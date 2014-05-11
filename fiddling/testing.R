## testing summarise for mean obs per observer per station

Obs <- c(rep("J", 9), rep("M",6))
Stat <- c(rep(c(1,2,3), 5))

t.data <- data.frame(Obs, Stat)

t.dat.G <- group_by(t.data, Stat, Obs) # summarizes Observer within Station

summarise(group_by(summarise(t.dat.G, n=n()), Stat), mean(n))
