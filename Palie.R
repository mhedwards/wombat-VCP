### Density estimation with Rota:Palie data only.


Palie.82 <- tbl_df(read.csv("Palie_82.csv", stringsAsFactors=FALSE))

Palie.JE <- filter(Palie.82, Observer=="JE")

# 4 observers, JE, PA, PP, TA

## Frmo Buckland page 67, using a half-normal "key" for the detection functioN:
n <- nrow(Palie.JE) # total # of observations
k <- 17 # number of point transects. Assuming total.  T5 = 17 (page 32, Micronesian)
R.i <- Palie.JE$Distance/100 # converted to KM
D.hat = (n^2)/(pi*k*sum(R.i^2)) ##.6407




Palie.PA <- filter(Palie.82, Observer=="PA")
# 52 observations, only transect 11
n <- nrow(Palie.PA) # total # of observations
k <- 16 # number of point transects. Assuming total.  T5 = 17, T11=16 (page 32, Micronesian)
R.i <- Palie.PA$Distance/100 # converted to KM
D.hat = (n^2)/(pi*k*sum(R.i^2)) ## .5125



Palie.PP <- filter(Palie.82, Observer=="PP")
# 52 observations, only transect 11
n <- nrow(Palie.PP) # total # of observations
k <- 16 # number of point transects. Assuming total.  T5 = 17, T11=16 (page 32, Micronesian)
R.i <- Palie.PP$Distance/100 # converted to KM
D.hat = (n^2)/(pi*k*sum(R.i^2)) ## .3559

hist(Palie.PP$Distance)
hist(Palie.PA$Distance)


Palie.PP.Trunc <- filter(Palie.82, Observer=="PP", Distance <=195)
# 52 observations, only transect 11
n <- nrow(Palie.PP.Trunc) # total # of observations
k <- 16 # number of point transects. Assuming total.  T5 = 17, T11=16 (page 32, Micronesian)
R.i <- Palie.PP.Trunc$Distance/100 # converted to KM
D.hat = (n^2)/(pi*k*sum(R.i^2)) ## .5202


Palie.TA <- filter(Palie.82, Observer=="TA")
# 47 observations, only transect 5
n <- nrow(Palie.TA) # total # of observations
k <- 17 # number of point transects. Assuming total.  T5 = 17, T11=16 (page 32, Micronesian)
R.i <- Palie.TA$Distance/100 # converted to KM
D.hat = (n^2)/(pi*k*sum(R.i^2)) ## 1.032
hist(Palie.TA$Distance)

## Quang Method
h = sd(R.i)*n^(-1/5)
t = k
D.hat.q = (1/( sqrt(2*pi)*pi*t*h^3  )) * sum(R.i * exp(-(R.i^2)/(2*h^2)))
(1/(sqrt(2*pi)*pi*(h^3)*t)) * sum(R.i*exp(-((R.i)^2)/(2*h^2)    ))
# this is 10 times smaller than the Buckland estimate.


# Using the UNTRUNCATED PP data, we get:
Buckland: 0.3559
Quang: 0.2647

# Untruncated PA:
Buckland: 0.5125
Quang: 0.5317

# Untruncated JE: 
Buckland: 0.6407
Quang: 0.6088

# Untruncated TA:
Buckland: 0.9714
Quang: 0.4655  # h was .18 here, while it was .27 - .38 for the others. 


### Ramesy/Scott 1979, using 4.9 on pg 167
P.JE.t <- filter(Palie.82, Observer=="JE", Distance <= 195)
n.t <- nrow(P.JE.t)
r <- .195 # km
D.ram <- n/(pi*(r^2))

rho.hat <- .195*(n.t/nrow(Palie.JE) )^(-1/2)
D.ram <- (n.t)/(pi*(rho.hat^2))


#### Ramsey/Scotte 1981
# n = birds detected 
# m = all the birds detected in Basal Region R (perfect detection region)
All.R.j <- Palie.82$Distance ## 193
R.j.195 <- filter(Palie.82, Distance <=195) ##169

n<-193
m<- 169
Area.R = pi*195^2
EAS.hat = (n/m)*Area.R

## Effective area surveyd  = representeditive of the observers total survey effort.
## EAS is NOT the basal region


R.j.195 <- R.j.195$Distance

## Target for effective area surveyed is 172 HA or 1.72 km^2 or 1,720,000 m^2
sum(pi*(All.R.j)^2) ## 11003070 m^2

sum(pi*(R.j.195)^2) ##6264213 m^2


Songsong <- filter(raw.data.2, Year==82, Transect==9, Island==2)
R.j.ss <- Songsong$Distance
sum(pi*(R.j.ss)^2) # gives 525 HA, looking for 79

#Only one transect in Song Song, though 2 observers

# per station:
#    total obs/# less than 195

SS.g <- group_by(Songsong, Observer, Stat_ID)
SS.g.s <- summarise(SS.g, n=n(), m=sum(Distance<=195), EAS = (n/max(m,1))*(pi*195^2))
sum(SS.g.s$EAS) #gives 2.43 HA (i didn't group by observer.)

SS.g.2 <- group_by(SS.g.s, Stat_ID)
summarise(SS.g.2, EAS.hat = mean(EAS))

PP.ss <- filter(Songsong, Observer=="PP")
# 52 observations, only transect 11
n <- nrow(Palie.PP) # total # of observations
k <- 16 # number of point transects. Assuming total.  T5 = 17, T11=16 (page 32, Micronesian)
R.i <- Palie.PP$Distance/100 # converted to KM
D.hat = (n^2)/(pi*k*sum(R.i^2)) ## .3559