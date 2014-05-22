# Quang's Half Normal

# this was used in Quang's simulations
r=seq(0,2, length=100)
tau=0.6
y <- exp(-(r^2)/(2*tau^2))
# that is just the kernel of hte halfnormal distribution. It defaults to 1 at 0
# he's using Tau to control the spread, but I think my method gives me more control.
plot(r, y)

sqrt(0.5)

hn.params


y2 <- hn.params$delta*dhalfnorm(r, hn.params$theta)
plot(r, y2)
