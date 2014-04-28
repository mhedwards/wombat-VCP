### 4/20/14

## Reminder:
#Island 2 = Rota
#Island 4 = Tinian

library(dplyr)
library(ggplot2)
library(lubridate) # date/time manipulation

raw.data <- tbl_df(read.csv("COKI.csv", stringsAsFactors=FALSE))

## ===== CODING ====

Labels.Type <- c("Heard", "Seen", "", "H&S")
islands = c("Rota", "Tinian")

## want to add unique station ID
## need to get date into a date form



raw.data.2 <- mutate(raw.data, 
                     Date = mdy(paste(Mo, Day, 1900+Year, sep="/")),
                     Time = hm(paste(Hour%/%100, Hour%%100, sep=":")),
                     Stat_ID = (Transect*100)+Station,
                     Type_lbl = Labels.Type[Type],
                     Island_Name = islands[Island/2]
                     )

coki.82 <- filter(raw.data.2, Year==82)

Palie.82 <- filter(raw.data.2, Island==2, Year==82, Transect %in% c(5,11))
write.csv(Palie.82, file="Palie_82.csv", row.names=FALSE)

## ====== Graphs =======

ggplot(coki.82, aes(x=Distance, fill=factor(Type)))+geom_dotplot(stackgroups=TRUE, binwidth=1, method="histodot")+ylim(0, 0.4)+xlim(0,200)
# too hard to read


## === Distance by Observation Type ===
ggplot(coki.82, aes(factor(Type_lbl), Distance))+geom_boxplot()+coord_flip()
# Heard were further away, only seen up close


## === Distance by observer ===
ggplot(coki.82, aes(factor(Observer), Distance))+geom_boxplot()+coord_flip()
# TA & PP are comparable, PA's median is slightly lower w/ smaller spread than previous two. JE is higher, with  much wider spread than the other three.


## ==== Station Plot ===
# this is not meant to be an actual map, but just to observe the # observed per station
# will need count per observer per station, then summarise by station.
by.Station.2 <- coki.82 %.%
    filter(Island==2) %.%
    group_by(Stat_ID, Observer)%.%
    summarise(
      Seen=n(), 
      Tr = first(Transect), 
      St=first(Station)
      ) %.%
    group_by(Stat_ID) %.%
    summarise(
      Avg=mean(Seen),
      Tr = first(Tr), 
      St=first(St)
      )

ggplot(by.Station.2, aes(St, Tr, colour=Avg))+geom_point(aes(size = Avg)) +scale_size_area()+ggtitle("Mean Observations per Station: Island 2")+ylab("Transect #")+xlab("Station #")

# I'm not sure this graph  has statistical merit, but what it does tell me is that the higher observations are not clustered in any particular area, but they are spread out. It would probably be beneficial to map this to an actual map but that may be more work than I care to do at the moment. 

# --- I may not have broken that up by island. Fixed. There is maybe some clustering




by.Date.2 <- coki.82 %.%
  filter(Island==2) %.%
  group_by(Date, Stat_ID, Observer)%.%
  summarise(
    Seen=n()
  ) %.%
  group_by(Date, Stat_ID) %.%
  summarise(
    Avg=mean(Seen)
  ) 

ggplot(by.Date.2, aes(Date, Avg))+geom_point()+ylim(0,7)
# this is not quite the graph I was shooting for, but I think it's informative. It shows that the average # of birds observed per station (averaging between the two observers) was pretty consistent. I think I'd like to summarise further by the value of Avg to see how many fell in which range.

by.Date.2.avg <- by.Date.2 %.%
  group_by(Date, Avg) %.%
  summarise( count=n() )

ggplot(by.Date.2.avg, aes(Date, Avg, colour=count))+geom_point(aes(size = count)) +ylim(0,7)
## i think this is a bit more informative, showing that we saw around 2 per station, on average, and that it did not seem to change much throughout time. The days with lower averages may just be fewer station visits.


ggplot(by.Date.2, aes(Date, Stat_ID))+geom_point()
# hmm. This is looking like some of the adjacent stations were observed on the same day

coki.82 %.%
  group_by(Island, Date, Stat_ID) %.%
  summarise(n(), first(Hour))

## Yeah, it's looking like they stepped sequentially through the stations, by date. Will need to look at full list, but that could affect analysis. They're broken up over time, so it wasn't completely sequential, and I do understand that for convenience they'd need to do this, but not great. 


filter(by.Date.2, Date== as.POSIXct("1982-03-29", tz="UTC"))
