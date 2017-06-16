# Sono Buoy study data processing
library(RSQLite)
library(ggplot2)
library(dplyr)
library(data.table)
library(manipulate)
library(geosphere)
library(swfscMisc)
library(lubridate)
setwd('~/R Projects/SWFSC/SonoBuoy')
source('~/R Projects/SWFSC/SonoBuoy/SonoBuoyFunctions.R')
###########################################################################
###########################################################################
# FIX MEEE
###########################################################################
###########################################################################

# Connecting to sqlite
con <- dbConnect(drv=SQLite(), dbname='PAST_20160607_POST_PB_Edited.sqlite3')


# BuoyHeading column in vesselcal first non-NA numbers for each channel. Should be the offset calibration. Only for station 1.
# Just add it. 
# dbListTables(con)
# dbListFields(con, 'DIFAR_Localisation')

difarData <- dbReadTable(con, 'DIFAR_Localisation')
difarData$posixDate <- ymd_hms(difarData$UTC)
# difarData$posixDate <- as.POSIXct(sapply(difarData$PCLocalTime, function(x) gsub('\\.\\S*', '', x)))
difarData <- data.table(difarData)
gpsData <- dbReadTable(con, 'gpsData')
gpsData$posixDate <- ymd_hms(gpsData$UTC)
# gpsData$posixDate <- as.POSIXct(sapply(gpsData$PCLocalTime, function(x) gsub('\\.\\S*', '', x)))

dbDisconnect(con)

con <- dbConnect(drv=SQLite(), dbname='PAST_20160607_POST_VesselCalOnly.sqlite3') 
buoyCal <- dbReadTable(con, 'DIFAR_Localisation')
dbDisconnect(con)
buoyCal <- data.table(buoyCal)
buoyCal <- buoyCal[grep('S1\\s', buoyCal$TrackedGroup),]
calValues <- distinct(buoyCal, Channel, BuoyHeading)[,Channel, BuoyHeading]

##################################################################################
calValues$BuoyHeading <- c(5.8, 5, 7.3, 12.9) # From mean of medians of Real - DIFAR
#                          NW   SW  SE   NE  
calValues$BuoyHeading <- c(10.9, 3.4, 6.7, 12.7) # Median of Real - DIFAR
#                          NW     SW   SE   NE
calValues$BuoyHeading <- c(9.5, -4.5, 3.5, 6.5) # Jenn new 5/10
##################################################################################

buoyGps <- read.csv('spot_messages_RUST_JLK.csv') %>%
      mutate(posixDate = mdy_hm(datetime, tz='America/Los_Angeles'),
             posixDate = with_tz(posixDate, tzone='UTC'))
buoyGps$Channel <- sapply(buoyGps$PlotPoints, function(x) buoyId(x))
buoyGps <- data.table(buoyGps)

# gpsData is boat, buoyGps is accurate buoy locations, difarData is DIFAR

boatData <- select(gpsData, Latitude, Longitude, posixDate)
# boatData <- select(gpsData, Latitude, Longitude, posixDate)
# use select boatData <- boatData[Latitude, Longitude, posixDate]
# rm(tempBoat, gpsData)
boatData <- data.table(boatData)
names(boatData) <- c('BoatLat', 'BoatLong', 'posixDate')

# 2-518R 0-709U 3-609T 1-52S5
# format='%m/%d/%Y %H:%M'

# For each time in DIFAR set get the real Long/Lat for the buoy. Use mapply, can loop over channels instead. 
# Write this separately and compare the speeds.
for(i in 1:nrow(difarData)){
      thisChan <- difarData[i, Channel]
      thisGps <- gpsInterp(buoyGps[Channel==thisChan, posixDate],
                           buoyGps[Channel==thisChan, Longitude],
                           buoyGps[Channel==thisChan, Latitude],
                           difarData[i, posixDate])
      difarData$BuoyLatitude[i] <- thisGps$Latitude
      difarData$BuoyLongitude[i] <- thisGps$Longitude
}


###########################################################################
###########################################################################
###########################################################################
################# DATA TABLE TIME JOIN NEEDED #############################
setkey(boatData, posixDate)
setkey(difarData, posixDate)
finalDifar <- boatData[difarData, roll='nearest']
###########################################################################
###########################################################################
###########################################################################
###########################################################################

## Calculating angle use Eric's SWFSC misc bearing angle package. 
finalDifar$RealBearing <- mapply(bearing, finalDifar$BuoyLatitude, finalDifar$BuoyLongitude, finalDifar$BoatLat, finalDifar$BoatLong)[1,]
finalDifar$Distance <- mapply(distance, finalDifar$BuoyLatitude, finalDifar$BuoyLongitude, finalDifar$BoatLat, finalDifar$BoatLong,
                              MoreArgs=list(units='km', method='vincenty'))
finalDifar <- merge(finalDifar, calValues, by='Channel')
finalDifar$DifarFixed <- finalDifar$DIFARBearing + finalDifar$BuoyHeading.y
# finalDifar$Channel <- as.factor(finalDifar$Channel)
# finalDifar$Channel <- factor(finalDifar$Channel,levels=c(0,1,2,3), labels=c('NE', 'SW', 'SE', 'NW'))
finalDifar$Species <- sapply(finalDifar$Species, function(x) gsub(' ','',x))
finalDifar$TrackedGroup <- sapply(finalDifar$TrackedGroup, function(x) gsub(' ','', x))
finalDifar$TriggerName <- sapply(finalDifar$TriggerName, function(x) gsub(' ', '', x))
finalDifar$MatchedAngles <- sapply(finalDifar$MatchedAngles, function(x) gsub(' ', '', x))
finalDifar$CallType <- sapply(finalDifar$Species, function(x) gsub('[0-9]', '', x))
finalDifar$Intensity <- as.numeric(sapply(finalDifar$Species, function(x) gsub('[a-zA-Z]', '' ,x)))
finalDifar$Intensity <- as.factor(finalDifar$Intensity)
finalDifar <- filter(finalDifar, !(Species %in% c('bulb1','bulb2','bulb3', 'buzz')))
# Need to get boat GPS for each difar ping to calculate angle. Boat GPS is every 2s.
# Can create copy of DF with posixdate -1 then combine then sort by posix date. Then can join the DFs on that.

# DIFAR bearing is relative to original buoy location. Variability not increasing with distance because
# we have already filtered by 'can I get a signal from this.' If we can get a signal, it will be accurate.
# If we tried to measure an angle from the other ones we would get a lot of noise and gibberish.
# Calculate dist between buoys

# Probability of detection for different call types? Need to figure out how many calls for each type are missing.
# How many repetitions did we do at each station? x call types. 

# Building model essentially for each call type. 

# No buzz!

# Possibly create new empty dataset with 4x spots for each sound type. Probably just have to count for how many times each
# thing runs at each station. Can I join on two conditions? Or can I fill chronologically? 

# Might need to try and classify each call burst by numdate. Further than 5s apart or something. Then can match within
# each playback.

smallDifar <- select(finalDifar,c(Id,
                            Channel, UTC, UTCMilliseconds, ClipLength, DIFARBearing, DifarFrequency, SignalAmplitude, DifarGain, Species, TrackedGroup, posixDate,
                            BuoyLatitude, BuoyLongitude, BoatLat, BoatLong, RealBearing, Distance, BuoyHeading.y, DifarFixed,
                            CallType, Intensity, MatchedAngles))

smallDifar <- arrange(smallDifar, posixDate) 
smallDifar$nextTime <- smallDifar$posixDate
smallDifar$nextTime[2:nrow(smallDifar)] <- smallDifar$posixDate[1:(nrow(smallDifar)-1)]

smallDifar$nextGroup <- (as.numeric(seconds((smallDifar$posixDate - smallDifar$nextTime))) > 20)


PlaybackNumber <- 1
for(i in 1:nrow(smallDifar)){
      PlaybackNumber <- PlaybackNumber + smallDifar$nextGroup[i]
      smallDifar$PlaybackNumber[i] <- PlaybackNumber
}

smallDifar$PlaybackNumber <- as.factor(smallDifar$PlaybackNumber)
smallDifar <- select(smallDifar, -c(nextTime, nextGroup))
# I think upX8 Id == 735
smallDifar <- data.table(smallDifar)
smallDifar[Id==735,]$Species <- 'upY5'

# write.csv(smallDifar, 'DIFAR_single_buoy.csv')
# smallDifar <- read.csv('DIFAR_single_buoy.csv')

# Make empty data set so we can get missed calls
stations <- unlist(lapply(1:30, function(x) rep(x, 60)))
species <- unlist(lapply(unique(smallDifar$Species), function(x) rep(x, 4)))
singleDifar <- data.frame(Channel=rep(c(0,1,2,3), 15*30))
singleDifar$PlaybackNumber <- stations
singleDifar$Species <- rep(species, 30)
# Station 4 only had dn1 dn5 calls
singleDifar <- filter(singleDifar, PlaybackNumber != 4 | 
                            Species %in% c('dn1', 'dn5'))


smallDifar$AngleError <- ((smallDifar$RealBearing - smallDifar$DIFARBearing) %% 360)
smallDifar$AngleError <-sapply(smallDifar$AngleError, function(x) {
      if(is.na(x)) {x}
      else if(x < abs(x-360)){x}
      else {x-360}
})

finalDifar$AngleError <- ((finalDifar$RealBearing - finalDifar$DIFARBearing) %% 360)
finalDifar$AngleError <-sapply(finalDifar$AngleError, function(x) {
      if(is.na(x)) {x}
      else if(x < abs(x-360)){x}
      else {x-360}
})

# smallDifar$Channel <- as.numeric(smallDifar$Channel)
singleDifar <- merge(singleDifar, smallDifar, by.x=c('Channel', 'PlaybackNumber', 'Species'), by.y=c('Channel', 'PlaybackNumber', 'Species'), all.x=TRUE)
singleDifar$PlaybackNumber <- as.factor(singleDifar$PlaybackNumber)
singleDifar <- data.table(singleDifar)

# Making new lats/longs. Also making distances and real angles and splitting call type up
tempEmpty <- within(singleDifar[is.na(singleDifar$BoatLat),], rm(BuoyLatitude, BuoyLongitude, BoatLat, BoatLong,
                                                                 TrackedGroup, CallType, Intensity, Distance, RealBearing))
meanSubs <- summarise(group_by(singleDifar, Channel, PlaybackNumber), BuoyLatitude=mean(BuoyLatitude, na.rm=TRUE), BuoyLongitude=mean(BuoyLongitude, na.rm=TRUE),
                      BoatLat=mean(BoatLat, na.rm=TRUE), BoatLong=mean(BoatLong, na.rm=TRUE))

meanSubs <- group_by(singleDifar, PlaybackNumber) %>% mutate(BoatLat=mean(BoatLat, na.rm=TRUE), BoatLong=mean(BoatLong, na.rm=TRUE)) %>%
      group_by(PlaybackNumber, Channel) %>% mutate(BuoyLatitude=mean(BuoyLatitude, na.rm=TRUE), BuoyLongitude=mean(BuoyLongitude, na.rm=TRUE)) %>% ungroup %>%
      select(BoatLat, BoatLong, BuoyLatitude, BuoyLongitude, Channel, Species, PlaybackNumber)

# meanSubs <- meanSubs[!(is.na(meanSubs$BuoyLat)),]
tgNum <- distinct(smallDifar[, .(PlaybackNumber, TrackedGroup)])
tempEmpty$PlaybackNumber <- as.factor(tempEmpty$PlaybackNumber)
test <- merge(tempEmpty, tgNum, by=c('PlaybackNumber'))
test <- merge(test, meanSubs, by=c('Channel', 'PlaybackNumber', 'Species'))

test$CallType <- sapply(test$Species, function(x) gsub('[0-9]', '', x))
test$Intensity <- as.numeric(sapply(test$Species, function(x) gsub('[a-zA-Z]', '' ,x)))
test$Distance <- mapply(distance, test$BuoyLatitude, test$BuoyLongitude, test$BoatLat, test$BoatLong,
                        MoreArgs=list(units='km', method='haversine'))
test$RealBearing <- mapply(bearing, test$BuoyLatitude, test$BuoyLongitude, test$BoatLat, test$BoatLong)[1,]

tempers <- rbind(smallDifar, test)

singleDifar <- tempers
singleDifar$Channel <- factor(singleDifar$Channel,levels=c(0,1,2,3), labels=c('NE', 'SW', 'SE', 'NW'))
singleDifar$Intensity <- as.factor(singleDifar$Intensity)
setnames(singleDifar, 'Channel', 'Buoy')

singleDifar$Detected <- !(is.na(singleDifar$DIFARBearing))
singleDifar <- data.table(singleDifar)
singleDifar$Time <- as.numeric(difftime(singleDifar$posixDate, min(singleDifar[Detected == TRUE, posixDate]), units='secs'))
singleDifar <- singleDifar %>%
      mutate(DifarAdj = (DIFARBearing + 11.7) %% 360,
             AdjError = (RealBearing - DifarAdj) %% 360,
             AdjError = sapply(AdjError, function(x) {
                   if(is.na(x)) {x}
                   else if(abs(x) <= abs(x-360)) {x}
                   else {x-360}
             }))
             
# write.csv(singleDifar, 'DIFAR_single_buoy_308.csv')


# Make empty for paired data
stations <- unlist(lapply(1:30, function(x) rep(x, 90)))
species <- unlist(lapply(unique(smallDifar$Species), function(x) rep(x, 6)))
pairedDifar <- data.frame(Channel1=rep(c(0,0,0,1,1,2), 15*30), Channel2=rep(c(1,2,3,2,3,3), 15*30), PairNum=rep(c(1,2,3,4,5,6), 15*30))
pairedDifar$PlaybackNumber <- stations
pairedDifar$Species <- rep(species, 30)
pairedDifar$Channel1 <- factor(pairedDifar$Channel1, levels=c(0,1,2,3), labels=c('NE', 'SW', 'SE', 'NW'))
pairedDifar$Channel2 <- factor(pairedDifar$Channel2, levels=c(0,1,2,3), labels=c('NE', 'SW', 'SE', 'NW'))
pairedDifar$PlaybackNumber <- as.factor(pairedDifar$PlaybackNumber)
pairedDifar <- filter(pairedDifar, PlaybackNumber != 4 |
                            Species %in% c('dn1', 'dn5'))

# slim <- smallDifar[, .(numDate, Channel, Species, TrackedGroup, PlaybackNumber, NewBuoyLat, NewBuoyLong,DIFARBearing,
#                        DifarFixed, RealBearing, Distance, BoatLat, BoatLong, CallType, Intensity)]

pairedDifar <- merge(pairedDifar, singleDifar, by.x=c('Channel1', 'PlaybackNumber', 'Species'), by.y=c('Buoy', 'PlaybackNumber', 'Species'), all.x=TRUE)
pairedDifar <- merge(pairedDifar, singleDifar, by.x=c('Channel2', 'PlaybackNumber', 'Species'), by.y=c('Buoy', 'PlaybackNumber', 'Species'), all.x=TRUE)
colnames(pairedDifar) <- gsub('\\.x', '1',colnames(pairedDifar))
colnames(pairedDifar) <- gsub('\\.y', '2', colnames(pairedDifar))

pairedDifar$PairBearing <- mapply(bearing, pairedDifar$BuoyLat1, pairedDifar$BuoyLong1,
                                  pairedDifar$BuoyLat2, pairedDifar$BuoyLong2)[1,]
pairedDifar$PairDistance <- mapply(distance, pairedDifar$BuoyLat1, pairedDifar$BuoyLong1,
                                   pairedDifar$BuoyLat2, pairedDifar$BuoyLong2,
                                   MoreArgs=list(units='km', method='haversine'))

pairedDifar$DoesIntersect <- with(pairedDifar, mapply(doesIntersect, RealBearing1, RealBearing2, PairBearing))
pairedDifar$MidLat <- (pairedDifar$BuoyLat1 + pairedDifar$BuoyLat2)/2
pairedDifar$MidLong <- (pairedDifar$BuoyLong1 + pairedDifar$BuoyLong2)/2

pairedDifar$RealMidDist <- with(pairedDifar, mapply(distance, MidLat, MidLong, BoatLat1, BoatLong1,
                                                    MoreArgs=list(units='km', method='haversine')))

# Why doesn't this work? Need to fix
intPoints <- with(pairedDifar, mapply(intersectPoint, DifarAdj1, DifarAdj2, PairBearing,
                                      BuoyLat1, BuoyLong1, BuoyLat2, BuoyLong2))

intPoints <- with(pairedDifar, mapply(intersectPoint, RealBearing1, RealBearing2, PairBearing,
                                      BuoyLat1, BuoyLong1, BuoyLat2, BuoyLong2))

intTemp <- matrix(intPoints,2,2622)
intTemp <- t(intTemp)

intPoints <- as.data.frame(intTemp)
names(intPoints) <- c('IntLat', 'IntLong')
pairedDifar$IntLat <- intPoints$IntLat
pairedDifar$IntLong <- intPoints$IntLong
pairedDifar$IntLat <- as.numeric(pairedDifar$IntLat)
pairedDifar$IntLong <- as.numeric(pairedDifar$IntLong)

# PB 2,4,12. We just don't have them? WTF?

pairedDifar$distError <- with(pairedDifar, mapply(distance, IntLat, IntLong, BoatLat1, BoatLong1,
                                                  MoreArgs=list(units='km', method='haversine')))
# pairedDifar <- data.table(pairedDifar)

# write.csv(pairedDifar,'DIFAR_paired_buoys_308.csv')
