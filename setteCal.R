library(PAMsbuoy)
library(dplyr)
library(ggplot2)
library(geosphere)
source('loadGpsDifar.R')
source('SonoBuoyFunctions.R')

dbFile <- './Data/HICEAS_2017/Sette/Database/1706_pg11511_sb_35_20170914.sqlite3'
pamDb <- loadDB(dbFile)
pamData <- formatStation(pamDb)
myData <- loadGpsDifar(dbFile) %>% 
      mutate(RealBearing = bearing(cbind(BuoyLongitude, BuoyLatitude), cbind(BoatLongitude, BoatLatitude)) %% 360,
      AngleError = (RealBearing - DIFARBearing) %% 360,
      AngleError = sapply(AngleError, function(x) {
            if(is.na(x)) {x}
            else if(abs(x) <= abs(x-360)) {x}
            else {x-360}
      })) %>% 
      select(-Latitude, -Longitude, -snr, -RMS, -ZeroPeak, -PeakPeak, -SEL, -TrueBearing, -TriggerName,
             -PCLocalTime, -PCTime, -TrackedGroup, -MatchedAngles) %>% 
      filter(DifarFrequency < 900)

myData %>% filter(Channel==1) %>% 
      mutate(AngleError = mapply(errorTransform, DIFARBearing + 180, RealBearing)) %>%
      ggplot(aes(x=RealBearing, y=AngleError)) + geom_point(aes(color=as.factor(Channel))) +
      xlim(0,360)


