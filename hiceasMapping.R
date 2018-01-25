# Map of all stations
library(PAMsbuoy)
library(dplyr)
library(ggplot2)
library(ggmap)
library(data.table)

setteDir <- './Data/HICEAS_2017/Sette/Database'
laskerDir <- './Data/HICEAS_2017/Lasker/Database'

setteStations <- loadStations(setteDir)
laskerStations <- loadStations(laskerDir)

setteBuoyPos <- rbindlist(lapply(setteStations, function(s) {
      tmp <- rbindlist(lapply(s$buoys, function(b) {
            b$position[1,]
      }))
      # print(s$station)
      tmp$Station <- gsub('(.*)\\..*', '\\1',attr(s, 'station'))
      tmp
}))

setteBuoyPos1 <- rbindlist(lapply(setteStations, function(s) {
      s$buoys$`0`$position[1,] %>% 
            mutate(Station = gsub('(.*)\\..*', '\\1', attr(s, 'station')),
                   Ship = 'Sette')
}))
laskerBuoyPos1 <- rbindlist(lapply(laskerStations, function(s) {
      # cat(attr(s, 'station'), '\n')
      s$buoys$`1`$position[1,] %>% 
            mutate(Station = gsub('(.*)\\..*', '\\1', attr(s, 'station')),
                   Ship = 'Lasker')
}))

buoyPos <- rbind(setteBuoyPos1, laskerBuoyPos1)

buoyPos <- rbind(setteBuoyPos1[1:7,], setteBuoyPos1[8:15,] %>% mutate(Ship = 'Lasker'))
head(buoyPos1)

# 22.718889, -168.572593 hawaii, est.

hiMap <- get_map(location = c(lon=-168.573, lat = 22.719),
                 zoom = 4)

# Down triangle - legend not right yet
ggmap(hiMap) + geom_point(data=buoyPos, aes(x=(Longitude %% 360)-360, y=Latitude, color=Ship, fill=Ship), size=3, shape=25) +
      scale_color_manual(values=c('navy', 'darkorchid3')) + 
      scale_fill_manual(values=c('navy', 'darkorchid3')) +
      labs(x='Longitude', y='Latitude', title='Sonobuoy Stations on HICEAS', color='Stations') +
      theme(plot.title = element_text(hjust=.5))
# Circles close scale
hiMap <- get_map(location = c(lon=-168.573, lat = 22.719),
                 zoom = 5)
ggmap(hiMap) + geom_point(data=buoyPos, aes(x=(Longitude %% 360)-360, y=Latitude, color=Ship), size=3.5, shape=19) +
      scale_color_manual(values=c('navy', 'darkorchid3')) + 
      labs(x='Longitude', y='Latitude', title='Sonobuoy Stations on HICEAS', color='Stations') +
      theme(plot.title = element_text(hjust=.5))
# Circles far scale
hiMap <- get_map(location = c(lon=-168.573, lat = 22.719),
                 zoom = 5)
ggmap(hiMap) + geom_point(data=buoyPos[3:75,], aes(x=(Longitude %% 360)-360, y=Latitude, color=Ship), shape=19, size=3) +
      scale_color_manual(values=c('navy', 'darkorchid3')) +
      labs(x='Longitude', y='Latitude', title='Sonobuoy Stations on HICEAS', color='Stations') +
      theme(plot.title = element_text(hjust=.5),
            panel.background = element_rect(linetype='blank', fill='#A3CCFF'),
            panel.grid = element_blank()) +
      xlim(-185, -150) + ylim(15.6, 32)
      
#####################
# Lets try to automate it
#####################
buoyPos$Longitude <- (buoyPos$Longitude %% 360) - 360
midLat <- median(buoyPos$Latitude)
midLong <- median(buoyPos$Longitude)
boundLat <- range(buoyPos$Latitude)
boundLong <- range(buoyPos$Longitude)
testMap <- get_map(location = c(boundLong[1], boundLat[1], boundLong[2], boundLat[2]))
testMap <- get_map(location = c(lon=mean(boundLong), lat=mean(boundLat)), zoom=5) 

zoom <- 5

makeMap <- function(buoys, zoom=8, ...) {
      boundLong <- range(buoys$Longitude)
      boundLat <- range(buoys$Latitude)
      if(zoom==0) {
            stop('Zoom is 0. Check coordinates for errors.')
      }
      suppressMessages(map <- get_map(location = c(lon=mean(boundLong), lat=mean(boundLat)), zoom=zoom))
      g <- ggmap(map) + geom_point(data=buoys, aes(x=Longitude, y=Latitude, color='Station'), size=3, ...) +
            labs(x='Longitude', y='Latitude', title='Sonobuoy Stations', color='') +
            scale_color_manual(values='black') +
            theme(plot.title = element_text(hjust=.5))
      tryCatch({
            print(g)
            cat('Zoom level', zoom, 'being used.')
      }, warning = function(w) {
            cat('Zoom level', zoom, 'is too close. Trying', zoom -1, '. \n')
            makeMap(buoys, zoom -1)
      }
      )
      g
}

makeMap <- function(buoys, zoom=8, ...) {
      boundLong <- range(buoys$Longitude)
      boundLat <- range(buoys$Latitude)
      if(zoom==0) {
            stop('Zoom is 0. Check coordinates for errors.')
      }
      suppressMessages(map <- get_map(location = c(lon=mean(boundLong), lat=mean(boundLat)), zoom=zoom))
      mapRange <- attr(map, 'bb')
      # Checking if all points are within map range. If not, zoom out 1.
      if(boundLong[1] < mapRange[2] |
         boundLong[2] > mapRange[4] |
         boundLat[1] < mapRange[1] |
         boundLat[2] > mapRange[3]) {
            cat('Zoom level', zoom, 'is too close. Trying', zoom-1,'. \n')
            return(makeMap(buoys, zoom-1))
      }
      cat('Zoom level', zoom, 'being used.')
      g <- ggmap(map) + geom_point(data=buoys, aes(x=Longitude, y=Latitude, color='Station'), size=3, ...) +
            labs(x='Longitude', y='Latitude', title='Sonobuoy Stations', color='') +
            scale_color_manual(values='black') +
            theme(plot.title = element_text(hjust=.5))
      g
}
m <- makeMap(buoyPos[1:8,],8)
m 
z <- 6


