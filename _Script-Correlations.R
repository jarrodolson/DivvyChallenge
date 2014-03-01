##Testing out GIS functions
library(plyr)
library(ggplot2)
library(corrgram)
library(MASS)

##One degree is ~68 miles... 2 sig digits is .68 miles
##http://sedac.ciesin.columbia.edu/data/set/usgrid-summary-file3-2000-msa/data-download

##demo <- read.csv("Demographics.csv",header=TRUE,stringsAsFactors=FALSE)
##demo <- demo[demo$meanBA!=0 & demo$meanFB!=0 & demo$meanHH3!=0 & demo$meanHS!=0 &
               demo$meanHU3!=0 & demo$meanHUNV!=0 & demo$meanp25!=0 & demo$meanPop3!=0 &
               demo$meanPOV!=0 & demo$meanSEVP!=0,]

data <- read.csv("ShapeFileData.csv",header=TRUE,stringsAsFactors=FALSE)
##data <- merge(data,demo,by=c("LATITUDE","LONGITUDE"),all=TRUE)
data[is.na(data)] <- 0

hist(data$nTotalTrips)
##count data... heavily over-dispersed to 0
##Model needs be run on data in the area where there are a lot of divvy bikes
##xlim=c(-87.70786,-87.58071),ylim=c(41.78875,41.97835)
data_m <- data[data$LONGITUDE>=-87.70786 & data$LONGITUDE<=-87.58071,]
data_m <- data_m[data_m$LATITUDE>=41.78875 & data_m$LATITUDE<=41.97835,]
mod <- glm.nb(nTotalTrips~nStations, data=data_m)
mod <- glm.nb(nTotalTrips~licFreq.Retail.Food+licFreq.Entertainment+licFreq.FoodBev+
                meanBA+meanHS+meanHUNV+meanp25+meanPop3+
                meanSEVP,data=data_m)
summary(mod)

##Looking at suitability
r <- predict(mod,newdata=data,se.fit=TRUE)
result <- cbind(data,r)
result$prediction <- exp(result$fit)
result$unmetNeed <- result$prediction-result$nTotalTrips##If negative, overserved
result$unmetNeedNorm <- result$unmetNeed/max(result$unmetNeed)

# group1 <- data[,1:18]
# group2 <- data[,c(1:6,19:ncol(data))]
# png("Corrgram_1.png",height=20,width=20,res=300,units="in")
# corrgram(group1,lower.panel=panel.pts,upper.panel=panel.pie)
# dev.off()
# 
# png("Corrgram_2.png",height=20,width=20,res=300,units="in")
# corrgram(group2,lower.panel=panel.pts,upper.panel=panel.pie)
# dev.off()

