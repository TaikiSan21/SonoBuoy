library(PAMsbuoy)
library(dplyr)
library(ggplot2)
library(geosphere)
library(swfscMisc)

source('loadGpsDifar.R')
source('SonoBuoyFunctions.R')
source('../PAMsbuoy/devel/drawBearing.R')

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



myData %>% filter(Channel==1) %>% 
      mutate(AngleError = mapply(errorTransform, DIFARBearing + 180, RealBearing)) %>%
      ggplot(aes(x=RealBearing, y=AngleError)) + geom_point(aes(color=as.factor(Channel))) +
      xlim(0,360)



##########################################################
# plotting stuff for shannon - station map

# Id 39329 is start of buoys, Id 42249 is end
gps <- pamDb$gpsData %>% 
      filter(Id %in% 39000:43000)

g <- ggplot() + geom_point(data=myData, aes(x=BuoyLongitude, y=BuoyLatitude, color='Buoy'), size=2) + 
      geom_path(data=gps, aes(x=Longitude, y=Latitude, color='Boat'))
# g
distance <- .8
alpha <- .3

shannonBearing <- function(df, distance, alpha) {
      df %>% 
            mutate(endLat = t(mapply(destination, .$Latitude, .$Longitude, .$DIFARBearing, distance, units='km'))[,1],
                   endLong = t(mapply(destination, .$Latitude, .$Longitude, .$DIFARBearing, distance, units='km'))[,2]) %>% 
            ggplot() + geom_point(aes(x=BuoyLongitude, y=BuoyLatitude, color=as.factor(Channel)), size=4) + 
            geom_path(data=gps, aes(x=Longitude, y=Latitude, color='Boat')) +
            geom_segment(aes(x=Longitude, y=Latitude, xend=endLong, yend=endLat, color=as.factor(Channel)),
                         arrow=arrow(type='closed', angle=15, length=unit(.35, 'cm')), alpha=alpha,
                         show.legend=FALSE) +
            scale_color_manual(values=c('red', 'blue', 'black'),
                               labels=c('Buoy 0', 'Buoy 1', 'Boat'),
                               guide = guide_legend(override.aes = list(
                                     color=c('red', 'blue', 'black'),
                                     linetype=c('blank', 'blank', 'solid'),
                                     shape=c(16, 16, NA)))) +
            theme(panel.background = element_rect(fill='lightblue', linetype='blank'),
                  panel.grid = element_blank(),
                  plot.title = element_text(hjust=.5)) +
            labs(x='Longitude', y='Latitude', color='Legend', title='Calibration Angles')
}

# from boat
myData %>% mutate(Latitude=BoatLatitude, Longitude = BoatLongitude, DIFARBearing = ifelse(Channel==0, DIFARBearing + 180, DIFARBearing)) %>% 
      shannonBearing(distance, alpha)

# from buoy
myData %>% mutate(Latitude = BuoyLatitude, Longitude = BuoyLongitude, DIFARBearing = ifelse(Channel==0, DIFARBearing, DIFARBearing+180)) %>% 
      shannonBearing(distance, alpha)
#########################################################################





