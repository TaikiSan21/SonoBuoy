# for shnnaon 2/9

# calls df - might have problem that times for non-received calls are not accurate
# looks like I filled in blanks with some averages? should only matter if we had
# a call that was received by nothing


## WEIRDNESS ##
# PlaybackNumber 4 is only downs 1 & 5? Gotta fix that in my shit.
# Should be at 17:32?
# Also multiple occurences where tone1&5 made it, but 8 didn't. Cant be right.
# ^ PB# 6, 8, 14 ^ appear normal in timestamps.
# Look @ UTC 18:23 ish for PB 14. Found spot. All three tones should be there.
# Possible they just didn't get boxed? Ask Jennifer about viewer mode.
# Also don't forget to check the PB#4 in pamguard. 

saplotall <- ggplot(data=singleDifar, aes(x=Distance, y=SignalAmplitude, color=Intensity)) + geom_point() + geom_smooth()
saplotbuoy <- ggplot(data=singleDifar, aes(x=Distance, y=SignalAmplitude, color=Buoy)) + geom_point() + geom_smooth()
saplotbuoy
saplotall

library(gridExtra)
grid.arrange(saplotbuoy, saplotall, ncol=2)

ggsave('Signal Amplitude - All Buoys.jpg', plot=saplotall, width=8, height=6, units='in', dpi=200)

saplotintens <- ggplot(data=singleDifar, aes(x=Distance, y=SignalAmplitude, color=Buoy)) + geom_point() + geom_smooth() + facet_wrap(facets=~Intensity)
saplotintens

ggsave('Signal Amplitude - Buoys Separated.jpg', plot=saplotintens, width=8, height=6, units='in', dpi=200)

singleDifar %>%
      select(Species, posixDate, BoatLat, BoatLong, BuoyLat, BuoyLong, Buoy, Detected, PlaybackNumber) %>%
      group_by(PlaybackNumber, Species) %>%
      summarise(Nums=sum(Detected)) %>%
      filter(Nums==0) %>%
      as.data.frame()

singleDifar %>% filter(as.numeric(PlaybackNumber) %in% 1:8) %>%
      ggplot(aes(x=posixDate, y=BoatLat, color=CallType)) + geom_point()
      
ggplot(difarData, aes(x=posixDate, y=NewBuoyLat)) + geom_point()

# Normal one: upz5 -3s- upz8 -2s- tone1 -3s- tone5 -3s- tone8 - next call
singleDifar %>% 
      filter(PlaybackNumber %in% c(14)) %>%
      select(posixDate, Species, PlaybackNumber, Buoy, DIFARBearing, RealBearing)