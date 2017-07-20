# 620 Trial
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(RSQLite)
library(viridisLite)
library(swfscMisc)
source('loadGpsDifar.R')
source('SonoBuoyFunctions.R')
source('../PAMsbuoy/devel/drawBearing.R')
buoy <- read.csv('./Data/PAST_20170620/Data/spot_messages.csv') %>%
      mutate(datetime=mdy_hm(datetime))
#####
# FOR NOISE CAL STUFF ASK FOR BLUE WHALE ANTARCTIC DATA TO COMPARE LOW FREQUENCY
#####


#### 
# START TUESDAY AT FILE 21000
####

# Deploy coords from streamer table
deploy <- data.frame(Latitude=c(32.60034, 32.5999, 32.5995, 32.5992), 
                     Longitude=c(-117.357, -117.3565, -117.35612, -117.3558))

buoy %>% filter(hour(datetime) > 7 & hour(datetime) < 11, Longitude>-117.360) %>% ggplot() + geom_point(aes(x=Longitude, y=Latitude, color=PlotPoints)) +
      geom_point(data=deploy, aes(x=Longitude, y=Latitude), size=2)

################
difarSixTwenty <- function(...) {
      difar <- loadGpsDifar(...) %>%
            mutate(Length=sapply(ClipLength, function(x) {
                  if(x > 3.5) 10
                  else if(x > 1.5) 3
                  else 1}))
      difar <- do.call(rbind, lapply(split(difar, difar$Channel), function(x) {
            df <- arrange(x, UTC)
            timeId <- c(2:nrow(df), nrow(df))
            df$TimeDiff <- difftime(df[timeId,]$UTC, df$UTC, units='secs')
            df$Station <- sapply(1:nrow(df), function(x) sum(df$TimeDiff[1:(x-1)] > 200))
            df
      }))
      difar      
}
#################
difar <- difarSixTwenty(db='./Data/PAST_20170620/PAST20Jun2017_pg11511_sbExperiment DIFAR - Playback.sqlite3',
                      buoylocs = './Data/PAST_20170620/Data/spot_messages.csv',
                      buoyfunc = sixTwentyId)
# Look at SA between length - seems same.
difar %>% filter(grepl('tone', Species), Station %in% 6:10) %>% 
      ggplot(aes(x=ClipLength, y=SignalAmplitude, color=Species)) + geom_point() + facet_wrap(Station~Channel)
# Lets fix some shit #
lengthClass <- function(x) {
      if(x > 3.5) 10
      else if(x > 1.5) 3
      else 1
}
# Time diff

difart <- do.call(rbind, lapply(split(difar, difar$Channel), function(x) {
      df <- arrange(x, UTC)
      timeId <- c(2:nrow(df), nrow(df))
      df$TimeDiff <- difftime(df[timeId,]$UTC, df$UTC, units='secs')
      df$Station <- sapply(1:nrow(df), function(x) sum(df$TimeDiff[1:(x-1)] > 200))
      df
}))
# Station 15 is when we paused for battery swap it seems
timeId <- c(seq(2, nrow(difar)), nrow(difar))
difar$TimeDiff <- difftime(difar[timeId,]$UTC, difar$UTC, units='secs')

difar <- mutate(difar, Length = sapply(ClipLength, function(x) lengthClass(x)))
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