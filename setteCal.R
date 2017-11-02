library(PAMsbuoy)
library(dplyr)
library(ggplot2)
library(geosphere)
library(viridisLite)
source('loadGpsDifar.R')
source('SonoBuoyFunctions.R')

dbFile <- './Data/HICEAS_2017/Sette/Database/1706_pg11511_sb_35_20170914.sqlite3'
dbFile <- './Data/HICEAS_2017/Sette/Database/1706_pg11511_sb_45_20171008.sqlite3'

pamDb <- loadDB(dbFile)
pamData <- formatStation(pamDb)
myData <- loadGpsDifar(dbFile) %>% 
      mutate(RealBearing = bearing(cbind(BuoyLongitude, BuoyLatitude), cbind(BoatLongitude, BoatLatitude)) %% 360,
      AngleError = (RealBearing - DIFARBearing)) %>% #,
      # AngleError = sapply(AngleError, function(x) {
      #       if(is.na(x)) {x}
      #       else if(abs(x) <= abs(x-360)) {x}
      #       else {x-360}
      # })) %>% 
      select(-Latitude, -Longitude, -snr, -RMS, -ZeroPeak, -PeakPeak, -SEL, -TrueBearing, -TriggerName,
             -PCLocalTime, -PCTime, -TrackedGroup, -MatchedAngles) %>% 
      filter(DifarFrequency < 900)

myData %>% 
      ggplot(aes(x=RealBearing, y=AngleError %% 360)) + geom_point(aes(color=DifarFrequency < 500)) +
      facet_wrap(~Channel) + scale_color_gradientn(colors=viridis(256), limits=c(50, 62))

# moving average of calibration



