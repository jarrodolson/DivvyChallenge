##Testing out GIS functions
library(rgeos)
library(plyr)
library(ggplot2)
bLic <- read.csv("Business_Licenses_-_Current_Active_-_Map.csv",header=TRUE,stringsAsFactors=FALSE)
bLicKey <- read.csv("BusinessLicenseKey.csv",header=TRUE,stringsAsFactors=FALSE)[c("LICENSE.DESCRIPTION","LICENSE.TYPE")]
bLic <- merge(bLic,bLicKey,by="LICENSE.DESCRIPTION",all.x=TRUE)
bLic$LATITUDE <- round(bLic$LATITUDE,digits=2)
bLic$LONGITUDE <- round(bLic$LONGITUDE,digits=2)
nBLic <- count(bLic,.(LATITUDE,LONGITUDE,LICENSE.TYPE))
nBLic <- rename(nBLic, c("freq"="licFreq"))
nBLic <- reshape(nBLic,idvar=c("LATITUDE","LONGITUDE"),timevar="LICENSE.TYPE",direction="wide")
stations <- read.csv("Divvy_Stations_2013.csv",header=TRUE,stringsAsFactors=FALSE)
stations$LATITUDE <- round(stations$latitude,digits=2)
stations$LONGITUDE <- round(stations$longitude,digits=2)
nStations <- count(stations,.(LATITUDE,LONGITUDE))
nStations <- rename(nStations,c("freq"="nStations"))
crimes <- read.csv("Crimes_-_2013.csv",header=TRUE,stringsAsFactors=FALSE)
colnames(crimes)
crimes$LONGITUDE <- round(crimes$Longitude,digits=2)
crimes$LATITUDE <- round(crimes$Latitude,digits=2)
nCrimes <- count(crimes,.(LATITUDE,LONGITUDE))
nCrimes <- rename(nCrimes,c("freq"="nCrimes"))

data <- merge(nStations,nCrimes,by=c("LATITUDE","LONGITUDE"),all=TRUE)
data <- merge(data,nBLic,by=c("LATITUDE","LONGITUDE"),all=TRUE)
data$nStations[is.na(data$nStations)]<-0
data$nCrimes[is.na(data$nCrimes)]<-0
data[is.na(data)] <- 0
data$licFreq[is.na(data$licFreq)] <- 0

library(corrgram)
png("Corrgram.png",height=20,width=20,res=300,units="in")
corrgram(data,lower.panel=panel.pts,upper.panel=panel.pie)
dev.off()
