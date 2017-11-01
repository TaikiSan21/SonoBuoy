# HICEAS Test'
library(swfscMisc)
library(geosphere)
source('SonoBuoyFunctions.R')
source('loadGpsDifar.R')
df <- loadGpsDifar('./Data/HICEAS Test/1705_pg11511_sb_opp_20170818.sqlite3') %>%
      select(-c(TrueBearing, snr, RMS, ZeroPeak, PeakPeak, SEL))
df$Channel[1:20] <- 3

df <- df %>% mutate(Distance = distGeo(cbind(BuoyLongitude, BuoyLatitude), cbind(BoatLong, BoatLat)),
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
                    }))

df %>% ggplot() + geom_point(aes(x=BoatLong, y=BoatLat, color='Boat')) +
      geom_point(aes(x=BuoyLongitude, y=BuoyLatitude, color='Buoy')) + 
      facet_wrap(~Channel, scales='free')
