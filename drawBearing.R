library(ggplot2)
library(geosphere)
library(dplyr)
library(swfscMisc)
# Add bearings as arrows to an existing plot

# df is a dataframe, but must have columns named lat, long, bearings
# distance is how far to draw, but is in units of decimal degrees not any meaningful distance measure

drawBearings <- function(df, plot, distance=4000, alpha=.5) {
    if(sum(c('long', 'lat', 'bearings') %in% colnames(df)) != 3){
        stop('Columns named long, lat, and bearings must be supplied.')
    }
    data <- df
    endcoord <- destPoint(cbind(df$long, df$lat), df$bearings, distance)
    data$endlat <- endcoord[,2]
    data$endlong <- endcoord[,1]
    plot + geom_segment(data=data, aes(x=long, y=lat, xend=endlong, yend=endlat),
                     arrow=arrow(type='closed', angle=15), alpha=alpha)
}

drawBearingst <- function(df, plot, distance=4, ...) {
    if(sum(c('Longitude', 'Latitude', 'DIFARBearing') %in% colnames(df)) != 3){
        stop('Columns named Longitude, Latitude, and DIFARBearing must be supplied.')
    }
    data <- df
    endcoord <- t(mapply(destination, data$Latitude, data$Longitude, data$DIFARBearing, distance, units='km'))
    data$endlat <- endcoord[,1]
    data$endlong <- endcoord[,2]
    plot + geom_segment(data=data, aes(x=Longitude, y=Latitude, xend=endlong, yend=endlat),
                        arrow=arrow(type='closed', angle=15), ...)
}

# From here is testing 
####
load('C:/Users/taiki.sakai/Documents/R Projects/SWFSC/AprilTrial/difarApril.RData')
difarApril3 <- difarApril3 %>% select(-c(Latitude, Longitude)) %>% 
    rename(Latitude=BuoyLatitude, Longitude=BuoyLongitude)

g <- ggplot(data=difarApril3 %>% filter(Channel==0) %>% slice(300:500)) + geom_point(aes(x=BoatLong, y=BoatLat, color='Boat')) + 
    geom_point(aes(x=Longitude, y=Latitude, color='Buoy')) + 
    coord_cartesian(xlim=c(-117.48, -117.465), ylim=c(32.685, 32.695))
difarApril3 %>% filter(Channel==0) %>% slice(300:500) %>%
    drawBearingst(g, distance=1, alpha=.1)
####
g <- ggplot() + geom_point(data=boat, aes(x=long, y=lat), size=3) + coord_cartesian(xlim=c(-117.43, -117.4), ylim=c(32.64, 32.67))
drawBearings(g, list(long=boat$long, lat=boat$lat), bearings=boat$difar, distance=.02)

t <- ggplot(data=oldcal) + geom_point(aes(x=BoatLong, y=BoatLat, color='Boat')) + 
    geom_point(aes(x=BuoyLongitude, y=BuoyLatitude, color='Buoy'))
oldcal %>% mutate(lat=BoatLat, long=BoatLong, bearings=180+DIFARBearing) %>% drawBearings(t)

difarApril3 <- loadGpsDifarApril('./PAST_SBtesting_20170417-3.sqlite3', buoylocs=buoys, adj=-50)
ggplot(data=difarApril3, aes(x=RealBearing, y=AdjError, color=Distance)) + geom_point() + facet_wrap(~Channel, nrow=2)

difarApril3 %>% filter(Channel==0) %>% slice(300:500) %>%
    mutate(lat=BoatLat, long=BoatLong, bearings=180+DifarAdj) %>% drawBearingst(g, alpha=.2, distance=1200)