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
source('noiseMatcher.R')
source('SonoBuoyFunctions.R')
source('../PAMsbuoy/devel/drawBearing.R')
buoy <- read.csv('./Data/PAST_20170620/Data/spot_messages.csv') %>%
      mutate(datetime=mdy_hm(datetime))
#### 8/4 ###
# Start grabbing noise again at 2000 - its right at the end of a toneZ, BE READY this is #18
##### 8/3 ###
# Fixing 620 loading function. Want to create gain orders backwards from 123 based just on order received
# it isn't really the right gains sometimes, but it should be close enough to look at shit better
####### 7/25 Notes #####
# Next plan : Get calibration data for channels 1 and 3. Not sure why calibration ones are different.
# Then gonna have to go and get noise...ugh. 
# Channel 1 seems to have a bunch of garbage noise in the first stretch. Missed a bunch because it turned to 180
# After station 7 gets really quiet
# 2029 ended

#####
# FOR NOISE CAL STUFF ASK FOR BLUE WHALE ANTARCTIC DATA TO COMPARE LOW FREQUENCY
#####
# Break stations up?

### 19:43 losing highest gain per user input. Then Station 12 is last reliable.
# Deploy coords from streamer table
deploy <- data.frame(Latitude=c(32.60034, 32.5999, 32.5995, 32.5992), 
                     Longitude=c(-117.357, -117.3565, -117.35612, -117.3558))

buoy %>% filter(hour(datetime) > 7 & hour(datetime) < 11, Longitude>-117.360) %>% ggplot() + 
      geom_point(aes(x=Longitude, y=Latitude, color=PlotPoints)) +
      geom_point(data=deploy, aes(x=Longitude, y=Latitude), size=2)

dict <- data.frame(Species = c('upA', 'upB', 'upC', 'dnA', 'dnB', 'dnC', 'toneX', 'toneY', 'toneZ'),
                   Noise = c('noiseSweep', 'noiseSweep', 'noiseSweep', 'noiseSweep', 'noiseSweep', 'noiseSweep',
                             'noiseX', 'noiseY', 'noiseZ'))

difar <- difarSixTwenty(db='./Data/PAST_20170620/PAST20Jun2017_pg11511_sbExperiment DIFAR - Playback.sqlite3',
                      buoylocs = './Data/PAST_20170620/Data/spot_messages.csv',
                      buoyfunc = sixTwentyId,
                      noiseDict = dict)
# Ids 369:409 seem to be that werid spike in nonsense
cal <- difarSixTwenty(db='./Data/PAST_20170620/PAST20Jun2017_pg11511_sbExperiment DIFAR.sqlite3',
                      buoylocs = './Data/PAST_20170620/Data/spot_messages.csv',
                      buoyfunc = sixTwentyId) %>% filter(!(Id %in% 369:409))


ntest <- difar %>% noiseMatcher(dict)

ntest %>% filter(Station==1, Channel==0, grepl('noise', Species))
#####
# Matching up calls to gain. Looks like there is always a pattern (esp. in tones) as gain changes

## Calibration data looking at how error goes across the path
cal %>% filter(Distance < 1400, abs(AdjError) < 50) %>% ggplot() + geom_path(aes(x=Longitude, y=Latitude, color=as.numeric(UTC)), size=2) + 
      scale_color_gradientn(colors=viridis(256)) + geom_point(aes(x=BuoyLongitude, y=BuoyLatitude)) + facet_wrap(~Channel)
# Data for aaron
aaron <- difar %>% select(-Noise, -MatchedAngles, -ClipLength, -TimeDiff, -Next, -Order,
                          -DifarAdj, -AdjError, -Gain, -UTCMilliseconds) %>% 
      mutate(BoatLatitude = Latitude, BoatLongitude = Longitude) %>% select(-Longitude, -Latitude) %>% 
      select(UTC:BuoyLongitude, BoatLatitude, BoatLongitude, DIFARBearing:Length, Channel:Species, NoiseBearing:NoiseAmplitude,
             SNR:Station)

df <- loadGpsDifar('./Data/PAST_20160607_POST_PB_Edited.sqlite3',
                   buoylocs = './Data/spot_messages_RUST_JLK.csv', buoyfunc = firstTrialId)
aaron2 <- select(df, -PCLocalTime, -PCTime, -ClipLength, -TriggerName, -BuoyHeading, -TrueBearing,
                 -MatchedAngles, -snr, -RMS, -ZeroPeak, -SEL, -PeakPeak, -DifarAdj, -AdjError) %>%
      mutate(BoatLatitude = Latitude, BoatLongitude = Longitude, Station=TrackedGroup) %>% 
      select(-Latitude, -Longitude, -TrackedGroup, -UTCMilliseconds) %>%
      filter(!(Species %in% c('bulb1', 'bulb2', 'bulb3', 'buzz')))
# I want polar plots
station <- 2
difar %>% filter(Station %in% station) %>%
      ggplot(aes(y=30-SNR, shape=grepl('tone', Species))) + geom_point(aes(x=DIFARBearing + 11.7, color='Difar')) +
      geom_point(aes(x=RealBearing, color='Real')) +
      geom_point(aes(x=NoiseBearing, color='Noise')) +
      coord_polar() + facet_wrap(~Channel, nrow=2)
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
             MedError = mapply(errorTransform, MedBearing, Median + 11.7)) %>%
      ungroup %>% data.frame

meds %>% filter(Distance > 1000) %>% select(MedBearing, MedError, Channel, Species, Station) %>%
      distinct() %>% ggplot(aes(x=MedBearing, y=MedError)) + geom_point() +
      # geom_hline(yintercept=0, size=3, alpha=.5, color='green') +
      facet_wrap(grepl('tone', Species)~Channel, nrow=2) + ylim(-20,20)
      

### JUST TRYING FIRST CAL KINE STUFF
## Id 338 ends first round channel 0 365 ends 2nd
## Id 57 ends first round of channel 2, next isnt for an hour
first <- cal %>% filter(Channel==2, Distance > 700, Id <= 57)
ggplot(first, aes(x=RealBearing, y=AdjError, color=Id)) + geom_point() + scale_color_gradientn(colors=viridis(256)) + xlim(0,360)
ggplot(first, aes(x=RealBearing, y=AdjError, color = Id <= 338)) + geom_point()

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