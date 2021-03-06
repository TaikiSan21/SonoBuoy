# Read db and match to gps data. Also match buoy data if you want.
library(RSQLite)
library(dplyr)
library(lubridate)
library(stringr)
library(data.table)
library(geosphere)
source('../SonoBuoy/SonoBuoyFunctions.R')

loadGpsDifar <- function(db, buoylocs = FALSE, buoytz='America/Los_Angeles', buoyfunc = NULL) {
      
      if(buoylocs==FALSE) print('No buoy data provided. Will not calculate real distance/bearing.')
      con <- dbConnect(drv=SQLite(), db)
      difar <- dbReadTable(con, 'DIFAR_Localisation') %>%
            mutate(UTC = ymd_hms(UTC),
                   TriggerName = str_trim(TriggerName),
                   Species = str_trim(Species),
                   MatchedAngles = str_trim(MatchedAngles),
                   TrackedGroup = str_trim(TrackedGroup))
      gps <- dbReadTable(con, 'gpsData') %>%
            mutate(UTC = ymd_hms(UTC)) %>%
            select(UTC, Latitude, Longitude)
      names(gps) <- c('UTC', 'BoatLatitude', 'BoatLongitude')
      dbDisconnect(con)
      
      # Using data tables to join boat gps to call times by nearest time
      difar <- data.table(difar)
      gps <- data.table(gps)
      setkey(difar, UTC)
      setkey(gps, UTC)
      difar <- gps[difar, roll='nearest']
      
      # If buoy gps data is available, linearly get location @ times by interpolating between
      # Interpolating because guoy gps wasn't updated as frequently as boat gps
      if(buoylocs!=FALSE) {
            if(is.null(buoyfunc)) stop('Need buoyfunc')
            buoy <- read.csv(buoylocs) %>%
                  mutate(UTC = mdy_hm(datetime, tz=buoytz),
                         UTC = with_tz(UTC, tzone='UTC'),
                         Channel = sapply(PlotPoints, function(x) buoyfunc(x)))
            buoy <- data.table(buoy)
            difar <- do.call(rbind, lapply(unique(difar$Channel), function(x) {
                df <- filter(difar, Channel == x)
                buoygood <- filter(buoy, Channel == x)
                locs <- gpsInterp(buoygood$UTC, buoygood$Longitude, buoygood$Latitude, df$UTC)
                mutate(df, BuoyLatitude = locs$'Latitude', BuoyLongitude = locs$'Longitude')
            })) %>% mutate(Distance = distGeo(cbind(BuoyLongitude, BuoyLatitude), cbind(BoatLongitude, BoatLatitude)),
                           RealBearing = geosphere::bearing(cbind(BuoyLongitude, BuoyLatitude), cbind(BoatLongitude, BoatLatitude)) %% 360,
                           AngleError = (RealBearing - DIFARBearing) %% 360,
                           AngleError = sapply(AngleError, function(x) {
                               if(is.na(x)) {x}
                               else if(abs(x) <= abs(x-360)) {x}
                               else {x-360}
                           }),
                           DifarAdj = (DIFARBearing + 11.7) %% 360,
                           AdjError = (RealBearing - DifarAdj) %% 360,
                           AdjError = sapply(AdjError, function(x) {
                               if(is.na(x)) {x}
                               else if(abs(x) <= abs(x-360)) {x}
                               else {x-360}
                           })) %>%
                  select(-Latitude, -Longitude) %>%
                  rename(Latitude=BoatLatitude, Longitude=BoatLongitude)
            # for(i in 1:nrow(difar)){
            #       thisChan <- difar[i, Channel]
            #       thisGps <- gpsInterp(buoy[Channel==thisChan, posixDate],
            #                            buoy[Channel==thisChan, Longitude],
            #                            buoy[Channel==thisChan, Latitude],
            #                            difar[i, posixDate])
            #       difar$BuoyLatitude[i] <- thisGps$Latitude
            #       difar$BuoyLongitude[i] <- thisGps$Longitude
            # }
            # # With buoy & boat locations we can get real distance/angles
            # difar <- difar %>% mutate(Distance = distGeo(cbind(BuoyLongitude, BuoyLatitude), cbind(BoatLong, BoatLat)),
            #                           RealBearing = geosphere::bearing(cbind(BuoyLongitude, BuoyLatitude), cbind(BoatLong, BoatLat)) %% 360,
            #                           AngleError = (RealBearing - DIFARBearing) %% 360,
            #                           AngleError = sapply(AngleError, function(x) {
            #                                 if(is.na(x)) {x}
            #                                 else if(abs(x) <= abs(x-360)) {x}
            #                                 else {x-360}
            #                           }),
            #                           DifarAdj = (DIFARBearing + 11.7) %% 360,
            #                           AdjError = (RealBearing - DifarAdj) %% 360,
            #                           AdjError = sapply(AdjError, function(x) {
            #                                 if(is.na(x)) {x}
            #                                 else if(abs(x) <= abs(x-360)) {x}
            #                                 else {x-360}
            #                           }))
                                      
      }
      difar
}


# difarData <- rbind(difarData, difarData)
# Rprof(NULL)
# Rprof(tmp <- tempfile())
# 
# 
# for(i in 1:nrow(difarData)){
#     thisChan <- difarData[i, Channel]
#     thisGps <- gpsInterp(buoyGps[Channel==thisChan, posixDate],
#                          buoyGps[Channel==thisChan, Longitude],
#                          buoyGps[Channel==thisChan, Latitude],
#                          difarData[i, posixDate])
#     difarData$BuoyLatitude[i] <- thisGps$Latitude
#     difarData$BuoyLongitude[i] <- thisGps$Longitude
# }
# Rprof()
# summaryRprof(tmp)
# 
# Rprof(NULL)
# Rprof(tmp <- tempfile())
# temp <- do.call(rbind, lapply(unique(difarData$Channel), function(x) {
#     df <- filter(difarData, Channel == x)
#     buoy <- filter(buoyGps, Channel == x)
#     locs <- gpsInterp(buoy$posixDate, buoy$Longitude, buoy$Latitude, df$posixDate)
#     mutate(df, BuoyLatitude = locs$'Latitude', BuoyLongitude = locs$'Longitude')
# })) %>% arrange(posixDate)
# Rprof()
# summaryRprof(tmp)

    
      