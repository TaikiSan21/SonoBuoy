library(dplyr)
library(stringr)
library(lubridate)
source('loadGpsDifar.R')
path <- 'C:/Users/taiki.sakai/Desktop/Transfer/Transfer/sonobuoys/data/1647_sb/difar/'

############################################ THOUGHTS ###############################################
# Each individual error isn't normally distributed on a normal 0,7 but in aggregate they seem to be #
# Random effects model - each mean is distributed 0,7 then distributed somehow around that.         #
# Actual distribution of errors isn't quite centered on 0 - its more like -1 or -2. Could be        # 
# from the fact that the buoy locations aren't really accurate - there is a delay between dropping  #
# the buoy and hitting the deploy button. If we go in a straight line after this doesn't affect the #
# errors of our calibraton, but if we are turning a lot after then it does. Can check the change in #
# RealBearing after deployment and see what's going on there.                                       #
#                                                                                                   #
# This simplified version of calibration may be better than the sin version if we can't find a      #
# consistent / reliable way of getting the sin data. Or if the sin thing doesn't actually seem      #
# to be real or if we don't have the data to conclude either way.                                   #
##################################################################################################### 

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

# Really only interested in calibration data for this
calFromSql <- calFromSql %>% filter(BuoyLatitude > 30, Species=='Vessel') %>%
      mutate(RealBearing = geosphere::bearing(cbind(BuoyLongitude, BuoyLatitude), cbind(BoatLong, BoatLat)) %% 360,
             DifarAdj = DIFARBearing + Declination,
             AdjError = (RealBearing - DifarAdj) %% 360,
             AdjError = sapply(AdjError, function(x) {
                   if(x > 180) {x-360}
                   else {x}}),
             Distance = geosphere::distGeo(cbind(BuoyLongitude, BuoyLatitude), cbind(BoatLong, BoatLat)),
             StationBuoy = paste(Station, Channel, sep='_')) %>%
      group_by(StationBuoy) %>% mutate(Median = median(AdjError, na.rm=TRUE),
                                       Mean = mean(AdjError, na.rm=TRUE)) %>%
      ungroup %>% data.frame %>% mutate(MedianError = (RealBearing - DifarAdj - Median) %% 360,
                                        MedianError = sapply(MedianError, function(x) {
                                              if(x > 180) {x-360}
                                              else {x}}))
# Look at distribution of median or mean errors at each station calibration
# ILL WANT TO CHECK AGAINST SOME NORMAL DISTRIBUTIONS AGAIN
calFromSql %>% distinct(StationBuoy, Median, Mean) %>% ggplot() + geom_histogram(aes(x=Mean), binwidth=1)

# This is a hist of error in calibration values after centering by the median error. So this is basically
# what we would want to do in a naive calibration instead of whatever the fuck pamguard is doing. The peak
# at 0 is really artificial because every single stationbuoy will have a 0 because we subtracted one of the values
# from itself unless there were an even number of dakine in which case it is averaged between middle 2.
ggplot(calFromSql, aes(x=MedianError)) + geom_histogram(binwidth=1)

# Graphin da errs
calFromSql %>% ggplot(aes(x=AdjError, fill=Distance > 600)) + 
      geom_histogram(binwidth=1) + geom_vline(xintercept=0, size=2, color='orange') + xlim(-20,20)

# Check against 0,10 normal. Pretty damn close to a 0,7 normal.
norms <- rnorm(1266,0,7)
test <- calFromSql %>% mutate(norms=norms)
ggplot(test) + geom_histogram(aes(x=AdjError, fill= Distance > 600), binwidth=1) + 
      geom_histogram(aes(x=norms), binwidth=1, alpha=.3) + xlim(-25,25)

smallnorms <- rnorm(148,0,7)
smalltest <- filter(calFromSql, Distance > 600) %>% mutate(norms=smallnorms)
ggplot(data=smalltest) + geom_histogram(aes(x=AdjError), binwidth=1) + 
      geom_histogram(aes(x=norms), binwidth=1, color='red', alpha=.2) + xlim(-25,25)

# Lets look by stations
ns <- calFromSql %>% filter(AdjError >= -25, AdjError <=25) %>% group_by(StationBuoy) %>% summarise(n()) %>% data.frame
stations <- ns$StationBuoy[which(ns$n.. >= 3)]
calFromSql %>% filter(StationBuoy %in% stations) %>%
      ggplot(aes(x=AdjError, y=StationBuoy)) + geom_joy() + geom_vline(xintercept=0, color='darkgreen') + 
      xlim(-25,25) + facet_wrap(~(Station %in% unique(calFromSql$Station[1:83])))

# Check calibration directions vs reported calibration
############# THIS DOESNT WORK ANYMORE I REMOVED NOT VESSEL SPECIES ###############3
buoyHeads <- calFromSql %>% filter(Species != 'Vessel') %>% distinct(Station, BuoyHeading, Channel)
test <- calFromSql %>% select(-BuoyHeading) %>% merge(buoyHeads)

