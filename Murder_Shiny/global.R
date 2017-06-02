library(dplyr)

latlng <- read.csv("./data/LatLng.csv")
murderTotals <- read.csv("./data/MurderTotals.csv")

#byYear <- aggregate(Victim.Count ~ Year,murderTotals,sum)
#byCity <- aggregate(Victim.Count ~ City+State,murderTotals,sum)

#murderLocation <- merge(byCity,latlng,by=c("City","State"),all.x = TRUE)

#byYear <- aggregate(Victim.Count ~ Year,murderTotals,sum)
#byCity <- aggregate(Victim.Count ~ City+State,murderTotals,sum)
#write.csv(murderTotals,file="./data/MurderTotals.csv",row.names =  FALSE, quote = TRUE)
#write.csv(latlng,file="./data/LatLng.csv",row.names =  FALSE, quote = TRUE)
#murderLocation <- merge(byCity,latlng,by=c("City","State"),all.x = TRUE)
#murderLocTotals <- aggregate(Victim.Count ~ City+State+lat+lng,murderLocation,sum)
#plotdata <- tail(murderLocTotals[with(murderLocTotals, order(Victim.Count)),],500)
#plotdata$title <- paste(plotdata$City,'(',plotdata$State,') - ',plotdata$Victim.Count)
#murderLocation[is.na(murderLocation$lat),]
