# Testing calibration locations

# caltest <- read.csv('C:/Users/Taiki/Desktop/sonobuoys/data/SB Playback Experiment/DIFAR_Localisation_VesselCalibration.csv')

### NEXT STEP ###
# Go through CALCURCEAS data, use angle between buoys 0 and 1 during deployment
# as 'real' angle. See how that shit looks. GPS data to come later if it looks too shitty
#################
library(tidyverse)
library(lubridate)
library(stringr)
con <- dbConnect(drv=SQLite(), dbname='C:/Users/Taiki/Desktop/sonobuoys/data/SB Playback Experiment/PAST_20160607_POST_VesselTime_Test_5-10.sqlite3')

myQuery <- dbSendQuery(con, 'SELECT * FROM DIFAR_Localisation')

caltest <- dbFetch(myQuery, n=-1)

caltest$posixDate <- as.POSIXct(caltest$PCLocalTime)
caltest$numDate <- as.numeric(caltest$posixDate)
caltest$numDate1 <- as.integer(caltest$numDate)
calt <- merge(cbind(round(caltest$posixDate, 'secs'), caltest),
              cbind(round(boatData$boatPosix, 'secs'), boatData),
              by=1)[-1]
calt <- data.table(calt)

for(i in 1:nrow(calt)){
      thisChan <- calt[i, Channel]
      thisGps <- gpsInterp(buoyGps[Channel==thisChan, posixDate],
                           buoyGps[Channel==thisChan, Longitude],
                           buoyGps[Channel==thisChan, Latitude],
                           calt[i, posixDate])
      calt$NewBuoyLat[i] <- thisGps$Latitude
      calt$NewBuoyLong[i] <- thisGps$Longitude
}

calt$RealBearing <- mapply(bearing, calt$NewBuoyLat, calt$NewBuoyLong, calt$BoatLat, calt$BoatLong)[1,]
calt$Distance <- mapply(distance, calt$NewBuoyLat, calt$NewBuoyLong, calt$BoatLat, calt$BoatLong, units='km', method='haversine')
calt$Channel <- factor(calt$Channel,levels=c(0,1,2,3), labels=c('NE', 'SW', 'SE', 'NW'))
calt$AngleError <- calt$RealBearing - calt$DIFARBearing
calt$AngleError <- sapply(calt$AngleError, function(x){
      if(abs(x) <= abs(x-360)) {x}
      else {x-360}
})

calplots <- qplot(data=calt[1:48,], x=RealBearing, y=AngleError, colour=Channel) +
      geom_point(data=summarise(group_by(calt[1:48,], Channel), RealMedian=median(RealBearing), ErrMedian=median(RealBearing-DIFARBearing)),
                 aes(x=RealMedian, y=ErrMedian, colour=Channel), size=6, alpha=.5) + xlim(0, 360) + ylim(-25, 50)
ggsave('Calibration Direction - Old.jpg', calplots, width=8, height=6, units='in', dpi=200)


calmap <- ggplot(data=calt[1:48,]) + geom_point(aes(x=NewBuoyLong, y=NewBuoyLat, colour='Channel')) + geom_point(aes(x=BoatLong, y=BoatLat, colour='Boat'))
ggsave('Calibration map.jpg', calmap, width=8, height=6, units='in', dpi=200)
      

# ggplot(data=calt[Channel=='SE',][1:20,]) + geom_point(aes(x=NewBuoyLong, y=NewBuoyLat, colour='Channel')) + geom_point(aes(x=BoatLong, y=BoatLat, colour='Boat')) 

### LOAD CALCURCEAS R WORKSPACE DATA ###
realData <- difar %>%
      mutate(UTC = ymd_hms(UTC),
             Species = str_trim(Species)) %>%
      filter(Species=='Vessel') %>%
      select(Id, UTC, UTCMilliseconds, Channel, BuoyLatitude, BuoyLongitude, DIFARBearing, DifarFrequency, SignalAmplitude, DifarGain, Species,
             stationAll, station, station.Part) %>%
      arrange(UTC)
fitLat <- c(31, 31, 32, 35, 35, 40, 40, 40, 45, 45)
fitLong <- c(-119.5, -124.5, -117.5, -125, -127.5, -127.5, -130, -125, -125, -127.5)
fitDec <- c(11.9, 12.5, 11.7, 13.5, 13.7, 14.9, 15.1, 14.7, 15.9, 16.2)
decdf <- data.frame(Lat=fitLat, Long=fitLong, Dec = fitDec)
declm <- lm(Dec ~ Lat + Long, data=decdf)

realData$Declination <- predict(declm, newdata=data.frame(Lat=realData$BuoyLatitude, Long=realData$BuoyLongitude))

realPairs <- do.call('rbind', 
                     by(realData, realData$station, function(df) {
                           tmp <- arrange(df, UTC)
                           this <- tmp$Channel[1]
                           that <- tmp[tmp$Channel != this,][1,]
                           result <- filter(tmp, Channel==this) %>%
                                 mutate(RealBearing = mapply(bearing, BuoyLatitude, BuoyLongitude, that$BuoyLatitude, that$BuoyLongitude)[1,],
                                        TimeDiff = that$UTC - UTC,
                                        TimeDiff = as.numeric(TimeDiff),
                                        Order = (1:n()/n()),
                                        AngleError = (RealBearing - (DIFARBearing + Declination)) %% 360,
                                        AngleError = sapply(AngleError, function(x) {
                                              if(abs(x) <= abs(x-360)) {x}
                                                 else {x-360}
                                              }))
                           result
                     }))


# Approximate declination at lat/long using linear model. Works good enough for target area. 
#####
# Time difference look at clip length
#####

lastCallPlot <- ggplot(data=realPairs[Order==1,]) + geom_point(aes(x=RealBearing, y=(AngleError-Declination), colour=as.factor(Channel))) + ylim(-25,25) +
      geom_hline(yintercept=0, size=2, colour='darkgreen', alpha=.5) + labs(title='Angle Error using only last received signal') +
      annotate('text', 0, 14, label='11.7', colour='darkgreen', fontface='bold')
lastCallPlot

lastCallPlotDate <- ggplot(data=realPairs[Order==1,]) + geom_point(aes(x=as.numeric(UTC), y=AngleError)) + ylim(-40,40) +
      geom_hline(yintercept = 15) + geom_hline(yintercept = 11) + labs(title='Holder')
lastCallPlotDate

ggsave('CalCurCEAS calibration test - last call only.jpg', lastCallPlot, width=8, height=6, units='in', dpi=200)

allCallPlot <- ggplot(data=realPairs) + geom_point(aes(x=RealBearing, y=(AngleError-Declination), colour=Order), size=2) +ylim(-25, 25) +
      labs(title='Angle Error using all signals')# +
      scale_colour_gradient2(low='red', high='darkgreen', mid='white', midpoint=.5)
allCallPlot

ggsave('CalCurCEAS calibration test - all calls, dec fixed, colored by order.jpg', allCallPlot, width=8, height=6, units='in', dpi=200)




