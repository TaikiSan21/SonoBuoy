# Sea trial circular data
library(rgdal)
library(RSQLite)
library(dplyr)
library(lubridate)
library(stringr)
library(data.table)
library(geosphere)
library(ggplot2)
library(plotly)
library(pracma)
source('SonoBuoyFunctions.R')
source('aprilFunctions.R')
source('~/R Projects/SWFSC/SonoBuoy/diagnosticGraphs.R')

# LAST LOCATION FOR CHANNEL 1&2 ARE LIKELY WRONG, 0&3 SHOULD BE FINE
# ONLY ~10-15% WRONG

# FIX STARTING AND ENDING LOCATIONS isn't interpolating out

# TRY SHORTER TIMES IN PAMGUARD wut

# Trying to adjust difar times by -30s
gpx <- readOGR('./Data/PAST_20170427/Data/Vesseltrack 27Apr2017 Current.gpx', layer='track_points')
time <- gpx@data$time
gps <- data.frame(gpx@coords)
colnames(gps) <- c('long', 'lat')
gps$time <- ymd_hms(time)
gps <- gps[7:3179,]


gpxToGps <- function(file) {
    gpx <- readOGR(file, layer='track_points')
    time <- gpx@data$time
    gps <- data.frame(gpx@coords)
    colnames(gps) <- c('long', 'lat')
    gps$time <- ymd_hms(time)
    gps
}

gps1 <- gpxToGps('./Data/PAST_20170427/Data/Vesseltrack 27Apr2017 Current.gpx')

# dogtag GPS
dogfiles <- grep('detailed\\.gpx', dir('./DogTracks'), value=TRUE)
doggpx <- do.call(rbind, lapply(dogfiles, function(x) {
    gps <- gpxToGps(paste0('./DogTracks/', x))
    gps$name <- gsub('([A-Za-z]*)_.*', '\\1', x)
    gps
}
)
)

# writeKML(data=gps, outputfile='AprilTrack.kml')
# qplot(data=gps, x=long, y=lat)
buoys <- './Data/PAST_20170427/Data/spot_messages (13).csv'
# r33 to 44 should be the good points

buoy <- read.csv(buoys) %>%
    mutate(posixDate = mdy_hm(datetime, tz='America/Los_Angeles'),
           posixDate = with_tz(posixDate, tzone='UTC'),
           Channel = sapply(PlotPoints, function(x) buoyIdApril(x)))
buoygood <- data.table(buoy)[33:44,]
buoystart <- arrange(buoygood, posixDate) %>% head(4) %>% rename(UTC=posixDate) %>% 
    select(Latitude, Longitude, Channel, UTC)

difarApril3 <- loadGpsDifarApril('./Data/PAST_20170427/PAST_SBtesting_20170417.sqlite3', buoylocs=buoys)

##############################
## TESTING LOOP DRAW
############################
tadj <- 0
difarApril3 <- loadGpsDifarApril('./Data/PAST_SBtesting_20170417-3.sqlite3', buoylocs=buoys, adj=tadj)
difarApril3 <- unique(select(difarApril3, -Id, -PCTime)) %>% arrange(UTC)
difarApril3$Species[2020:2099] <- 'Noise'
qplot(data=filter(difarApril3, Distance>70), x=RealBearing, y=AngleError, color=Distance) + facet_wrap(~Channel, nrow=2)

qplot(data=difarApril3, x=RealBearing, y=AngleError, color=Species, shape=Species) + facet_wrap(~Channel, nrow=2)

surfplots <- vector('list', length(unique(difarApril3$Channel)))
i <- 1
for(c in unique(difarApril3$Channel)) {
    filtStart <- filter(buoystart, Channel==c)
    filtBoat <- filter(difarApril3, Channel==c, UTC > filtStart$UTC, Distance>7) %>%
        mutate(DIFARBearing=DIFARBearing)
    # print(qplot(data=filtBoat, x=RealBearing, y=AngleError, color=Distance))
    surfplots[i] <- diagnosticGraphs(filtBoat, filtStart, outpath = './Diagnostic Graphs/First Batch/',
                     name=paste0('Ch', c, ' Difar ', tadj))
    i <- i+1
}
grad <- gradient(-log(mat))
gx <- grad$X
gy <- grad$Y
dim(gx) <- NULL; dim(gy) <- NULL
gradx <- data.frame(value=gx, rate= rep((1:100)/(100/3), 360), phi=unlist(lapply(1:360, function(x) rep(x, 100))))
grady <- data.frame(value=gy, rate= rep((1:100)/(100/3), 360), phi=unlist(lapply(1:360, function(x) rep(x, 100))))

##############################
difarApril3 %>% filter(Distance > 750, Species=='Vessel') %>% 
    ggplot(aes(x=RealBearing, y=AdjError, color=Distance)) +geom_point() +
    facet_wrap(~Channel, nrow=2) + scale_colour_gradient(low='black', high='green') +
    #ylim(-35,0) + 
    geom_vline(xintercept=c(0, 90, 180, 270)+11.7, color='red')


qplot(data=difarApril, x=BuoyLongitude, y=BuoyLatitude, color=as.factor(Channel))
# all boat gps with buoy
ggplot() + geom_point(data=difarApril3, aes(x=BuoyLongitude, y=BuoyLatitude, color='Buoy')) + 
    geom_point(data=gps[1750:2400,], aes(x=long, y=lat, color='Boat')) +
    xlim(-117.475,-117.4675) + ylim(32.692, 32.6975) +
    #adding in dog tag stuff
    geom_point(data=filter(doggpx, name %in% c('Dog')), aes(x=long, y=lat, color='Dog')) +
    geom_point(data=buoy, aes(x=Longitude, y=Latitude, color=PlotPoints), size=4)

ggplot() + geom_point(data=difarApril3, aes(x=BuoyLongitude, y=BuoyLatitude, color='Buoy')) + 
    geom_path(data=gps, aes(x=long, y=lat, color='Boat')) +
    geom_point(data=buoy, aes(x=Longitude, y=Latitude, color='Buoy', shape=PlotPoints))

# make plotly to look at times. Need one dataframe with all shit in one
combined <- rbind(gps %>% mutate(name='Boat'), 
                  doggpx %>% filter(name %in% c('Dog'))) %>%
    filter(hour(time) < 23)

plot_ly(data=combined, x = ~long, y = ~lat,
        color = ~name, text = ~paste('Time', time))


