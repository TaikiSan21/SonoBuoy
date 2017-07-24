# 620 Trial
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(RSQLite)
library(viridisLite)
library(swfscMisc)
library(manipulate)
library(ggjoy)
source('loadGpsDifar.R')
source('SonoBuoyFunctions.R')
source('../PAMsbuoy/devel/drawBearing.R')
buoy <- read.csv('./Data/PAST_20170620/Data/spot_messages.csv') %>%
      mutate(datetime=mdy_hm(datetime))
#####
# FOR NOISE CAL STUFF ASK FOR BLUE WHALE ANTARCTIC DATA TO COMPARE LOW FREQUENCY
#####
# Break stations up?

#### 
# START TUESDAY AT FILE 21000
####
### 19:43 losing highest gain per user input. Then Station 12 is last reliable.
# Deploy coords from streamer table
deploy <- data.frame(Latitude=c(32.60034, 32.5999, 32.5995, 32.5992), 
                     Longitude=c(-117.357, -117.3565, -117.35612, -117.3558))

buoy %>% filter(hour(datetime) > 7 & hour(datetime) < 11, Longitude>-117.360) %>% ggplot() + 
      geom_point(aes(x=Longitude, y=Latitude, color=PlotPoints)) +
      geom_point(data=deploy, aes(x=Longitude, y=Latitude), size=2)

difar <- difarSixTwenty(db='./Data/PAST_20170620/PAST20Jun2017_pg11511_sbExperiment DIFAR - Playback.sqlite3',
                      buoylocs = './Data/PAST_20170620/Data/spot_messages.csv',
                      buoyfunc = sixTwentyId)
# Look at SA between length - seems same.
stations <- 6:10
difar %>% filter(grepl('tone', Species), Station %in% stations) %>% 
      ggplot(aes(x=ClipLength, y=SignalAmplitude, color=Species)) + geom_point() + facet_wrap(Station~Channel)
difar %>% filter(grepl('tone', Species), Station %in% stations) %>% 
      ggplot(aes(x=ClipLength, y=AdjError, color=Species)) + geom_point() + facet_wrap(Station~Channel)
# Look at diff length ups/downs. Seems same error. Also SA seems to be same.
difar %>% filter(!grepl('tone', Species), Station %in% stations) %>% 
      ggplot(aes(x=ClipLength, y=AdjError, color=grepl('up', Species))) + geom_point() + facet_wrap(Station~Channel) + ylim(-20,20)

difar %>% filter(!grepl('tone', Species), Station %in% stations) %>% 
      ggplot(aes(x=ClipLength, y=SignalAmplitude, color=grepl('up', Species))) + geom_point() + facet_wrap(Station~Channel)

# Look at diff between tone/not. They seem to sometimes have very different patterns in SAvsError.
difar %>% filter(Station %in% 8:12) %>% 
      ggplot(aes(x=SignalAmplitude, y=AdjError, color=grepl('tone', Species))) + geom_point() + facet_wrap(Channel~Station) + ylim(-20,20)
####
difar %>% filter(Station %in% stations) %>% 
      ggplot(aes(x=ClipLength, y=AdjError, color=grepl('tone', Species))) + geom_point() + facet_wrap(Station~Channel) + ylim(-20,20)

difar %>% filter(Station %in% stations) %>% 
      ggplot(aes(x=ClipLength, y=SignalAmplitude, color=grepl('tone', Species))) + geom_point() + facet_wrap(Station~Channel)
#### OKAY, SO SA SEEMS TO BE SAME ACROSS LENGTH. ERROR SEEMS SAME ACROSS LENGTH. BOTH SEEM SAME BETWEEN UP/DN.
### Channels 2 and 3 sometimes have very sloped weird SA vs Error pattern.
# SA vs Error. Unexpected pattern.
difar %>% filter(Station %in% stations) %>% 
      ggplot(aes(x=log(SignalAmplitude), y=AdjError, color=Distance)) + geom_point() + facet_wrap(Channel~Station) + ylim(-20,20)
# Station 15 is when we paused for battery swap it seems

########### LETS LOOK AT STATION 9:12 TO CHECK OUT WEIRD LINEAR PATTERN
weirds <- filter(difar, Station %in% 9:12)
weirds %>% ggplot(aes(x=SignalAmplitude, y=AdjError, color=Order)) + geom_point() + facet_wrap(Station~Channel) + ylim(-20,20)
# The high error (likely just noise) values are at relatively high SA???
# 9/2 and 12/3 seem nice and weird.
looky <- filter(weirds, (Station==9 & Channel==2))
manipulate({ggplot(looky, aes_string(x='SignalAmplitude', y='AdjError', color=colpick)) + geom_point()},
           colpick=picker('ClipLength', 'Order', 'Species'))


#### WE STILL GET SINS? ##
difar %>% filter(Distance > 1000) %>% ggplot(aes(x=RealBearing, y=AdjError, color=Distance)) + 
      geom_point() + facet_wrap(~Channel) + ylim(-20,20) + scale_colour_gradientn(colours=viridis(256))
# Distribution of errors across stations
difar %>% filter(Distance > 1000) %>% ggplot(aes(x=AdjError, y=as.factor(Station))) + geom_joy() +
      facet_wrap(~Channel) + xlim(-20,20)

##### MEDIANS ####
meds <- difar %>% group_by(Channel, Station, Species) %>% 
      mutate(Median = median(DIFARBearing, na.rm=TRUE),
             MedBearing = median(RealBearing, na.rm=TRUE),
             MedError = MedBearing - Median) %>%
      ungroup %>% data.frame %>% 
      mutate(AdjMedian = (MedError - 11.7) %% 360,
             AdjMedian = sapply(AdjMedian, function(x) {
                   if(is.na(x)) {x}
                   else if(x > 180) {x - 360}
                   else {x}}))

meds %>% filter(Distance > 1000) %>% select(MedBearing, AdjMedian, Channel, Species, Station) %>%
      distinct() %>% ggplot(aes(x=MedBearing, y=AdjMedian)) + geom_point() +
      facet_wrap(grepl('tone', Species)~Channel, nrow=2) + ylim(-20,20)                                                               
######### Look at paths #######
g <- ggplot(data=difar) + geom_point(aes(x=Longitude, y=Latitude, color='Boat')) + 
      geom_point(aes(x=BuoyLongitude, y=BuoyLatitude, color=as.factor(Channel)))
difar %>% mutate(DIFARBearing=(DifarAdj+180) %% 360) %>% filter(Channel==2) %>%
      drawBearings(g, distance = .3, alpha=.3)
difar  %>% filter(Channel==0) %>% ggplot(aes(x=UTC, y=AdjError, color=Distance)) + geom_point()

ggplot(difar, aes(x=RealBearing, y=AdjError, color=log(SignalAmplitude))) + geom_point() + facet_wrap(~Channel) + 
      ylim(c(-25,25)) + xlim(c(0,360))

con <- dbConnect(drv=SQLite(), './Data/PAST_20170620/PAST20Jun2017_pg11511_sbExperiment DIFAR.sqlite3')
gps <- dbReadTable(con, 'gpsData') %>% mutate(UTC=ymd_hms(UTC))
dbDisconnect(con)

ggplot() + geom_point(data=buoy, aes(x=Longitude, y=Latitude)) + 
      geom_path(data=gps, aes(x=Longitude, y=Latitude, color=as.numeric(UTC))) +
      coord_cartesian(xlim=c(-117.392,-117.324), ylim=c(32.55, 32.627)) +
      scale_colour_gradientn(colours=viridis(256))
###############################


# 498-AB 760-AC 334-AE 891-AG