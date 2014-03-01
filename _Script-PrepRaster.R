##Raster File Read-In
readAndMerge <- function(){
  library(raster)
  library(plyr)
  getLatLong <- function(df,name){
    df$LONGITUDE <- round(df$x,digits=2)
    df$LATITUDE <-  round(df$y,digits=2)
    df <- df[c("LATITUDE","LONGITUDE",name)]
    return(df)
  }
  
  ##First read raster
  ba <- raster("demographics\\m1602ba00.asc")
  ##Then extract data with an x and a y column
  ba <- as.data.frame(ba,xy=TRUE)
  ##Then round it
  ba <- getLatLong(ba,"m1602ba00")
  ##Then take mean
  baMean <- ddply(ba,.(LATITUDE,LONGITUDE),summarize,meanBA=mean(m1602ba00,na.rm=TRUE))
  rm(ba)
  
  fb <- raster("demographics\\m1602fb00.asc")
  fb <- as.data.frame(fb,xy=TRUE)
  fb <- getLatLong(fb,"m1602fb00")
  fbMean <- ddply(fb,.(LATITUDE,LONGITUDE),summarize,meanFB=mean(m1602fb00,na.rm=TRUE))
  rm(fb)
  
  hh3 <- raster("demographics\\m1602HH300.asc")
  hh3 <- as.data.frame(hh3,xy=TRUE)
  hh3 <- getLatLong(hh3,"m1602HH300")
  hh3Mean <- ddply(hh3,.(LATITUDE,LONGITUDE),summarize,meanHH3=mean(m1602HH300,na.rm=TRUE))
  rm(hh3)
  
  
  hs <- raster("demographics\\m1602hs00.asc")
  hs <- as.data.frame(hs,xy=TRUE)
  hs <- getLatLong(hs,"m1602hs00")
  hsMean <- ddply(hs,.(LATITUDE,LONGITUDE),summarize,meanHS=mean(m1602hs00,na.rm=TRUE))
  rm(hs)
  
  
  hu3 <- raster("demographics\\m1602hu300.asc")
  hu3 <- as.data.frame(hu3,xy=TRUE)
  hu3 <- getLatLong(hu3,"m1602hu300")
  hu3Mean <- ddply(hu3,.(LATITUDE,LONGITUDE),summarize,meanHU3=mean(m1602hu300,na.rm=TRUE))
  rm(hu3)
  
  
  hunv <- raster("demographics\\m1602hunv00.asc")
  hunv<- as.data.frame(hunv,xy=TRUE)
  hunv <- getLatLong(hunv,"m1602hunv00")
  hunvMean <- ddply(hunv,.(LATITUDE,LONGITUDE),summarize,meanHUNV=mean(m1602hunv00,na.rm=TRUE))
  rm(hunv)
  
  
  p25 <- raster("demographics\\m1602p2500.asc")
  p25 <- as.data.frame(p25,xy=TRUE)
  p25 <- getLatLong(p25,"m1602p2500")
  meanP25 <- ddply(p25,.(LATITUDE,LONGITUDE),summarize,meanp25=mean(m1602p2500,na.rm=TRUE))
  rm(p25)
  
  pop3 <- raster("demographics\\m1602pop300.asc")
  pop3 <- as.data.frame(pop3,xy=TRUE)
  pop3 <- getLatLong(pop3,"m1602pop300")
  pop3Mean <- ddply(pop3,.(LATITUDE,LONGITUDE),summarize,meanPop3=mean(m1602pop300,na.rm=TRUE))
  rm(pop3)
  
  pov <- raster("demographics\\m1602pov00.asc")
  pov <- as.data.frame(pov,xy=TRUE)
  pov <- getLatLong(pov,"m1602pov00")
  povMean <- ddply(pov,.(LATITUDE,LONGITUDE),summarize,meanPOV=mean(m1602pov00,na.rm=TRUE))
  rm(pov)
  
  sevp <- raster("demographics\\m1602sevp00.asc")
  sevp <- as.data.frame(sevp,xy=TRUE)
  sevp <- getLatLong(sevp,"m1602sevp00")
  sevpMean <- ddply(sevp,.(LATITUDE,LONGITUDE),summarize,meanSEVP=mean(m1602sevp00,na.rm=TRUE))
  rm(sevp)
  
  demo <- merge(baMean,fbMean,by=c("LATITUDE","LONGITUDE"),all=TRUE)
  demo <- merge(demo,hh3Mean,by=c("LATITUDE","LONGITUDE"),all=TRUE)
  demo <- merge(demo,hsMean,by=c("LATITUDE","LONGITUDE"),all=TRUE)
  demo <- merge(demo,hu3Mean,by=c("LATITUDE","LONGITUDE"),all=TRUE)
  demo <- merge(demo,hunvMean,by=c("LATITUDE","LONGITUDE"),all=TRUE)
  demo <- merge(demo,meanP25,by=c("LATITUDE","LONGITUDE"),all=TRUE)
  demo <- merge(demo,pop3Mean,by=c("LATITUDE","LONGITUDE"),all=TRUE)
  demo <- merge(demo,povMean,by=c("LATITUDE","LONGITUDE"),all=TRUE)
  demo <- merge(demo,sevpMean,by=c("LATITUDE","LONGITUDE"),all=TRUE)
  
  write.csv(demo,"Demographics.csv",row.names=FALSE)
  return(demo)
}
##demo <- readAndMerge()