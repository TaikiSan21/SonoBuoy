library(dplyr)
library(stringr)
library(lubridate)
source('loadGpsDifar.R')
path <- 'C:/Users/taiki.sakai/Desktop/Transfer/Transfer/sonobuoys/data/1647_sb/difar/'

calcurceas <- do.call(rbind, lapply(dir(path), function(x) {
      read.csv(paste0(path, x)) %>%
            mutate(Station=gsub('\\.csv', '', x),
                   UTC=mdy_hms(UTC),
                   Species=str_trim(Species),
                   TriggerName=str_trim(TriggerName),
                   MatchedAngles=str_trim(MatchedAngles),
                   TrackedGroup=str_trim(TrackedGroup))
}
))

path <- 'E:/CalCurCEAS_Sonobuoy/SQLite/'

cal <- do.call(rbind, lapply(list.files(path, pattern='\\.sqlite3'), function(x) {
      loadGpsDifar(paste0(path, x)) %>%
            mutate(Station=gsub('\\.sqlite3', '', x))
}))

load('calFromSql.RData')

fitLat <- c(31, 31, 32, 35, 35, 40, 40, 40, 45, 45)
fitLong <- c(-119.5, -124.5, -117.5, -125, -127.5, -127.5, -130, -125, -125, -127.5)
fitDec <- c(11.9, 12.5, 11.7, 13.5, 13.7, 14.9, 15.1, 14.7, 15.9, 16.2)
decdf <- data.frame(Lat=fitLat, Long=fitLong, Dec = fitDec)
declm <- lm(Dec ~ Lat + Long, data=decdf)
calFromSql$Declination <- predict(declm, newdata=data.frame(Lat=calFromSql$BuoyLatitude, Long=calFromSql$BuoyLongitude))

calFromSql <- calFromSql %>% filter(BuoyLatitude > 30) %>%
      mutate(RealBearing = geosphere::bearing(cbind(BuoyLongitude, BuoyLatitude), cbind(BoatLong, BoatLat)) %% 360,
             DifarAdj = DIFARBearing + Declination,
             AdjError = (RealBearing - DifarAdj) %% 360,
             AdjError = sapply(AdjError, function(x) {
                   if(x > 180) {x-360}
                   else {x}}),
             Distance = geosphere::distGeo(cbind(BuoyLongitude, BuoyLatitude), cbind(BoatLong, BoatLat)))

# Graphin da errs
calFromSql %>% filter(Species=='Vessel') %>% ggplot(aes(x=AdjError, fill=Distance > 600)) + 
      geom_histogram(binwidth=1) + geom_vline(xintercept=0, size=2, color='orange') + xlim(-20,20)
# Check calibration directions vs reported calibration

buoyHeads <- calFromSql %>% filter(Species != 'Vessel') %>% distinct(Station, BuoyHeading, Channel)
test <- calFromSql %>% select(-BuoyHeading) %>% merge(buoyHeads)

