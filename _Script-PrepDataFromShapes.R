##Preps dataset by pulling data from GiS data

prepDataFromShapes <- function(){
  library(plyr)
  ##Data in
  bLic <- read.csv("Business_Licenses_-_Current_Active_-_Map.csv",header=TRUE,stringsAsFactors=FALSE)
  bLicKey <- read.csv("BusinessLicenseKey.csv",header=TRUE,stringsAsFactors=FALSE)[c("LICENSE.DESCRIPTION","LICENSE.TYPE")]
  trips <- read.csv("C:\\Users\\jarrodanderin\\Documents\\_RWork\\_Datasets\\Divvy_Stations_Trips_2013\\Divvy_Stations_Trips_2013\\Divvy_Trips_2013.csv",header=TRUE,stringsAsFactors=FALSE)
  stations <- read.csv("Divvy_Stations_2013.csv",header=TRUE,stringsAsFactors=TRUE)
  crimes <- read.csv("Crimes_-_2013.csv",header=TRUE,stringsAsFactors=FALSE)
  
  ##License info
  bLic <- merge(bLic,bLicKey,by="LICENSE.DESCRIPTION",all.x=TRUE)
  bLic$LATITUDE <- round(bLic$LATITUDE,digits=2)
  bLic$LONGITUDE <- round(bLic$LONGITUDE,digits=2)
  nBLic <- count(bLic,.(LATITUDE,LONGITUDE,LICENSE.TYPE))
  nBLic <- rename(nBLic, c("freq"="licFreq"))
  nBLic <- reshape(nBLic,idvar=c("LATITUDE","LONGITUDE"),timevar="LICENSE.TYPE",direction="wide")
  
  ##Stations data
  stations$LATITUDE <- round(stations$latitude,digits=2)
  stations$LONGITUDE <- round(stations$longitude,digits=2)
  nStations <- count(stations,.(LATITUDE,LONGITUDE))
  nStations <- rename(nStations,c("freq"="nStations"))
  
  ##NTrips
  nTrips_in <- count(trips,.(to_station_id))
  nTrips_in <- merge(nTrips_in,stations,by.x="to_station_id",by.y="id",all.x=TRUE)
  nTrips_in <- nTrips_in[c("LATITUDE","LONGITUDE","freq")]
  colnames(nTrips_in) <- c("LATITUDE","LONGITUDE","nTripsIn")
  nTrips_out <- count(trips,.(from_station_id))
  nTrips_out <- merge(nTrips_out,stations,by.x="from_station_id",by.y="id",all.x=TRUE)
  nTrips_out <- nTrips_out[c("LATITUDE","LONGITUDE","freq")]
  colnames(nTrips_out) <- c("LATITUDE","LONGITUDE","nTripsOut")
  
  ##NCrimes
  crimes$LONGITUDE <- round(crimes$Longitude,digits=2)
  crimes$LATITUDE <- round(crimes$Latitude,digits=2)
  nCrimes <- count(crimes,.(LATITUDE,LONGITUDE))
  nCrimes <- rename(nCrimes,c("freq"="nCrimes"))
  
  ##Put data together
  data <- merge(nStations,nCrimes,by=c("LATITUDE","LONGITUDE"),all=TRUE)
  data <- merge(data,nTrips_in,by=c("LATITUDE","LONGITUDE"),all=TRUE)
  data <- merge(data,nTrips_out,by=c("LATITUDE","LONGITUDE"),all=TRUE)
  data$nTotalTrips <- data$nTripsIn+data$nTripsOut
  data <- merge(data,nBLic,by=c("LATITUDE","LONGITUDE"),all=TRUE)
  write.csv(data,"ShapeFileData.csv",row.names=FALSE)
  return(data)
}
