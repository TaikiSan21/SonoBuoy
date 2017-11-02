# Map of all stations
library(PAMsbuoy)
library(dplyr)
library(ggplot2)
library(ggmap)
library(data.table)

setteDir <- 'E:/Hiceas/Database'
laskerDir <- ''

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
      s$buoys$`0`$position[1,] %>% 
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
ggmap(hiMap) + geom_point(data=buoyPos, aes(x=(Longitude %% 360)-360, y=Latitude, color=Ship), size=3, shape=19) +
      scale_color_manual(values=c('navy', 'darkorchid3')) + 
      labs(x='Longitude', y='Latitude', title='Sonobuoy Stations on HICEAS', color='Stations') +
      theme(plot.title = element_text(hjust=.5),
            panel.background = element_rect(linetype='blank', fill='#A3CCFF'),
            panel.grid = element_blank()) +
      xlim(-185, -153) + ylim(16.9, 32.5)
      
