loadGpsDifarApril <- function(db, buoylocs = FALSE, buoytz='America/Los_Angeles', adj=0) {
    
    if(buoylocs==FALSE) print('No buoy data provided. Will not calculate real distance/bearing.')
    con <- dbConnect(drv=SQLite(), db)
    difar <- dbReadTable(con, 'DIFAR_Localisation') %>%
        mutate(UTC = ymd_hms(UTC)-adj,
               TriggerName = str_trim(TriggerName),
               Species = str_trim(Species),
               MatchedAngles = str_trim(MatchedAngles),
               TrackedGroup = str_trim(TrackedGroup))
    names(gps) <- c('BoatLong', 'BoatLat', 'UTC')
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
        buoy <- read.csv(buoylocs) %>%
            mutate(UTC = mdy_hm(datetime, tz=buoytz),
                   UTC = with_tz(UTC, tzone='UTC'),
                   Channel = sapply(PlotPoints, function(x) buoyIdApril(x)))
        buoy <- data.table(buoy)[33:44,]
        ch1fix <- data.frame('4/27/2017 20:54:32', '0-2573891', 'UNLIMITED-TRACK', 
                             32.69347, -117.471, mdy_hms('4/27/2017 20:54:32'), 1)
        names(ch1fix) <- names(buoy)
        buoy <- rbind(buoy, ch1fix)
        difar <- do.call(rbind, lapply(unique(difar$Channel), function(x) {
            df <- filter(difar, Channel == x)
            buoygood <- filter(buoy, Channel == x)
            locs <- gpsInterp(buoygood$UTC, buoygood$Longitude, buoygood$Latitude, df$UTC)
            mutate(df, BuoyLatitude = locs$'Latitude', BuoyLongitude = locs$'Longitude')
        })) %>% mutate(Distance = distGeo(cbind(BuoyLongitude, BuoyLatitude), cbind(BoatLong, BoatLat)),
                       RealBearing = geosphere::bearing(cbind(BuoyLongitude, BuoyLatitude), cbind(BoatLong, BoatLat)) %% 360,
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
            rename(Latitude=BoatLat, Longitude=BoatLong)
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
        # 
    }
    difar
}

buoyIdApril <- function(id) {
    if(id=='0-2573760') 0
    else if(id=='0-2572498') 3
    else if(id=='0-2573891') 2
    else if(id=='0-2574334') 1
}
