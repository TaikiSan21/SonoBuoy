# Functions for SWFSC sono buoy project

# ######################################################################## 
# gpsInterp is used to create interpolated GPS readings from a set of accurate 
# GPS readings. Used to find actual sono buoy location for the DIFAR readings we
# have (outputs from DIFAR are just starting location). 
# ######################################################################## 
# accTime: Time (POSIXct) of the accurate GPS reading for a device 
# accLong/accLat: Accurate GPS readings for a device newTime: The times
# (POSIXct) we want to know the GPS for 
# ########################################################################

gpsInterp <- function(accTime, accLong, accLat, newTime) {
      if(!inherits(accTime, 'POSIXct') | !inherits(newTime, 'POSIXct')){
            stop('Please convert times to POSIXct')
      }
      accNumeric <- as.numeric(accTime)
      newNumeric <- as.numeric(newTime)
      
      if((max(newNumeric) > max(accNumeric)) | (min(newNumeric) < min(accNumeric)) ){
            print('Warning: New times are out of bounds. Using nearest value.')
      }
      newLong <- approx(accNumeric, accLong, xout=newNumeric, rule=2)$y
      newLat <- approx(accNumeric, accLat, xout=newNumeric, rule=2)$y
      list(Latitude = newLat, Longitude = newLong)
}

# Map buoy id codes from spot (PlotPoints) to matching Channel in DIFAR
# 709(U): 0, 525(S): 1, 518(R): 2, 609(T): 3 
firstTrialId <- function(id) {
      if(id=='0-2571525') 1
      else if(id=='0-2571709') 0
      else if(id=='0-2571609') 3
      else if(id=='0-2571518') 2
}

aprilTrialId <- function(id) {
      if(id=='0-2573760') 0
      else if(id=='0-2572498') 3
      else if(id=='0-2573891') 2
      else if(id=='0-2574334') 1
}

sixTwentyId <- function(id) {
      if(id=='0-2573891') 0
      else if(id=='0-2574334') 3
      else if(id=='0-2573760') 2
      else if(id=='0-2572498') 1
}

sixTwentyEight <- function(id) {
      if(id=='0-2573891') 3
      else if(id=='0-2574334') 2
      else if(id=='0-2573760') 1
      else if(id=='0-2572498') 0
}

# Get slope

bearingToSlope <- function(angle) {
      angle <- (angle * pi) / (180)
      # if(0<=angle & angle<(pi/2)) tan((pi/2)-angle)
      # else if((pi/2)<=angle & angle<pi) tan((pi/2)-angle)
      # else if(pi<=angle & angle<(3*pi/2)) tan((3*pi/2)-angle)
      # else if((3*pi/2)<=angle & angle<(2*pi)) tan(angle-(3*pi/2))
      tan((pi/2)-angle)
}

doesIntersect <- function(angle1, angle2, angle12) {
      if(is.na(angle12) | is.na(angle1) | is.na(angle2)) FALSE
      else{
      rotAngle1 <- (angle1 + 90 - angle12) %% 360
      rotAngle2 <- (angle2 + 90 - angle12) %% 360
      if(0<=rotAngle1 & rotAngle1<90) ((0<rotAngle2 & rotAngle2<rotAngle1) | (rotAngle2>270))
      else if(90<=rotAngle1 & rotAngle1<270) (rotAngle1<rotAngle2 & rotAngle2<270)
      else if(270<=rotAngle1 & rotAngle1<360) (270<rotAngle2 & rotAngle2<rotAngle1)
      }
}

intersectPoint <- function(angle1, angle2, angle12, lat1, long1, lat2, long2) {
      doesInt <- doesIntersect(angle1, angle2, angle12)
      if(is.na(doesInt)) list(lat=NA, long=NA)
      else if(doesInt==FALSE) list(lat=NA, long=NA)
      else{
      # slope1 <- bearingToSlope(angle1)
      # slope2 <-bearingToSlope(angle2)
      # x <- (slope2*long2 - slope1*long1 + lat1 - lat2)/(slope2 - slope1)
      # y <- slope1*(x - long1)+lat1
      # list(lat=y, long=x)
      points <- gcIntersectBearing(c(long1, lat1), angle1, c(long2, lat2), angle2)
      list(lat=points[1,2], long=points[1,1])
      }
}

toDirection <- function(angle) {
      if(angle < 22.5) 'N'
      else if(angle < 67.5) 'NE'
      else if(angle < 112.5) 'E'
      else if(angle < 157.5) 'SE'
      else if(angle < 202.5) 'S'
      else if(angle < 247.5) 'SW'
      else if(angle < 292.5) 'W'
      else if(angle < 336.5) 'NW'
      else 'N'
}

# Don't know why this isn't working. Gain is somehow ending up
# FUCK. It's the length counter that I'm using.
difarSixTwenty <- function(noiseDict=FALSE, ...) {
      difar <- loadGpsDifar(...) %>%
            mutate(Length=sapply(ClipLength, function(x) {
                  if(x > 3.5) 10
                  else if(x > 1.35) 3
                  else 1}))
      suppressWarnings(if(noiseDict != FALSE) difar <- difar %>% noiseMatcher(noiseDict))
      difar <- do.call(rbind, lapply(split(difar, difar$Channel), function(x) {
            # Within Channel
            df <- arrange(x, UTC)
            timeId <- c(2:nrow(df), nrow(df))
            df$TimeDiff <- difftime(df[timeId,]$UTC, df$UTC, units='secs')
            df$Next <- sapply(c(2,2:nrow(df)), function(x) {
                  df$Species[x-1]=='toneZ' & df$Species[x] != 'toneZ'
            })
            df$Station <- sapply(1:nrow(df), function(x) sum(df$Next[1:x])+1)
            do.call(rbind, lapply(split(df, df$Station), function(y) {
                  # Within Station
                  tmp <- arrange(y, UTC)
                  do.call(rbind, lapply(split(tmp, list(tmp$Species, tmp$Length)), function(z) {
                        # Within Species & Length
                        arrange(z, UTC) %>% mutate(Gain = rev(seq(from=3, by=-1, length.out=n())))
                  })) %>% 
                        arrange(UTC) %>% mutate(Order = 1:n())
            }))
      }))
      select(difar, -c(snr, RMS, PeakPeak, ZeroPeak, SEL, PCLocalTime, PCTime, 
                       TriggerName, TrackedGroup, TrueBearing, BuoyHeading))      
}

errorTransform <- function(bearing1, bearing2) {
      error <- (bearing1 - bearing2) %% 360
      if(error > 180) {error-360}
      else {error}
}
