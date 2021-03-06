##For context sized map
makeLatLong <- function(dir,fi){
  require(rgdal)
  require(sp)
  layer <- readOGR(dir, layer=fi)
  layer <- spTransform(layer,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  return(layer)
}

library(maptools)
library(RColorBrewer)
city <- makeLatLong("City_Boundary","City_Boundary")
stationMap <- makeLatLong("Divvy_Stations_2013.shp","Divvy_Stations_2013")
wards <- makeLatLong("Boundaries-Wards","Wards")
bikeRt <- makeLatLong("Bike_Routes","Bikeroutes3")
cops <- makeLatLong("Boundaries - Police Beats (current)","geo_aerh-rz74-1")
businessDist <- makeLatLong("Boundaries_-_Central_Business_District","Central_Business_District")
majorSt <- makeLatLong("Major_Streets","Major_Streets")
neighborhoods <- makeLatLong("Neighborhoods_2012","Neighborhoods_2012b")
water <- makeLatLong("Waterways","Hydro")

png("MapOfChicagoAndBikes.png", heigh=20,width=20,res=300,units="in")
plot(city)
neighbCols <- brewer.pal(7,"Set3")
data <- rep(rbind(neighbCols),14)
plot(neighborhoods, add=TRUE, col=data)
##plot(businessDist, add=TRUE, col="red")
plot(water,add=TRUE,col="Blue",border="blue")
plot(majorSt, add=TRUE)
plot(bikeRt,add=TRUE,col="blue")
temp <- as.data.frame(coordinates(neighborhoods))
temp$names <- neighborhoods$PRI_NEIGH
text(temp[,1],y=temp[,2],labels=temp$names,cex=.5)
plot(stationMap, add=TRUE, pch=20,col="red")
points(refPoints$longitude,y=refPoints$latitude,pch=20,cex=1,col="red")
text(refPoints$longitude+.01,y=refPoints$latitude+.005,labels=refPoints$Location,cex=.5)
dev.off()

png("MapOfChicagoAndUnmetNeed.png", heigh=20,width=20,res=300,units="in")
plot(city)
neighbCols <- brewer.pal(7,"Set3")
data <- rep(rbind(neighbCols),14)
plot(neighborhoods, add=TRUE, col=data)
plot(water,add=TRUE,col="Blue",border="blue")
plot(majorSt, add=TRUE)
plot(bikeRt,add=TRUE,col="blue")
temp <- as.data.frame(coordinates(neighborhoods))
temp$names <- neighborhoods$PRI_NEIGH
text(temp[,1],y=temp[,2],labels=temp$names,cex=.5)
points(result$LONGITUDE,result$LATITUDE,pch=20,cex=((result$unmetNeed/max(result$unmetNeed))*10))
plot(stationMap, add=TRUE, pch=20,col="red")
dev.off()

