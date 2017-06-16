setwd('C:/Users/Taiki/Desktop/Pamguard Data Collection/')

edata <- loadGpsDifar('RTsonobuoy_1_15_09.sqlite3', 'spot_messages_RUST_JLK.csv')

# Compare "Calibrations"
# 0-8.5, 1-6.5, 2-5.5, 3-8.5
edata %>% filter(Species=='Vessel') %>% group_by(Channel) %>% summarise(median(RealBearing-DIFARBearing))

oldcal <- loadGpsDifar('C:/Users/Taiki/Desktop/sonobuoys/data/SB Playback Experiment/PAST_20160607_POST_VesselTime_Test_5-10.sqlite3',
                       'spot_messages_RUST_JLK.csv')

oldcal %>% group_by(Channel) %>% summarise(median(RealBearing-DIFARBearing))

ggplot() + geom_point(data=filter(edata, Species=='Vessel'), aes(x=posixDate, y=DIFARBearing)) + 
      geom_point(data=oldcal, aes(x=posixDate, y=DIFARBearing), color='red') + 
      geom_point(data=oldcal, aes(x=posixDate, y=RealBearing), color='orange')

ggplot(data=oldcal, aes(x=BoatLong, y=BoatLat, color=posixDate)) + geom_point()

boatNoise <- loadGpsDifar('C:/Users/Taiki/Documents/R Projects/SWFSC/SonoBuoy/DIFAR Testing/BoatNoiseTest.sqlite3',
                          'C:/Users/Taiki/Documents/R Projects/SWFSC/SonoBuoy/spot_messages_RUST_JLK.csv')

boatNoiseAll <- loadGpsDifar('C:/Users/Taiki/Documents/R Projects/SWFSC/SonoBuoy/DIFAR Testing/BoatNoiseTest2.sqlite3',
                          'C:/Users/Taiki/Documents/R Projects/SWFSC/SonoBuoy/spot_messages_RUST_JLK.csv')

ggplot(boatNoiseAll, aes(color=as.numeric(posixDate))) + geom_point(aes(x=BuoyLongitude, y=BuoyLatitude, shape=as.factor(Channel)), size=3) + 
      geom_point(aes(x=BoatLong, y=BoatLat, shape='Boat')) + scale_colour_gradient(low='black', high='green')

ggplot(boatNoiseAll) + geom_point(aes(x=RealBearing, y=AdjError)) + facet_wrap(~Channel, nrow=2) + ylim(-25,50) + facet_wrap(~Channel, nrow=2)

ggplot() + geom_point(data=filter(boatNoiseAll, Channel==1), aes(x=RealBearing, y=AdjError, color='Second')) + 
      geom_point(data=filter(boatNoise, Channel==1), aes(x=RealBearing, y=AdjError, color='First'))

ggplot(data=boatNoiseAll, aes(x=RealBearing, y=AdjError, color=Distance)) +
      geom_point() + scale_colour_gradient2(low='black', high='lightblue', mid='green', midpoint=1500) + coord_polar() + facet_wrap(~Channel, nrow=2)
