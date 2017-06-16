# Check dat noise - this is my big dataset
library(dplyr)
library(ggplot2)
library(manipulate)
source('noiseMatcher.R')
source('loadGpsDifar.R')
source('./DIFAR Testing/callGrouper.R')
buoypath <- 'spot_messages_RUST_JLK.csv'

noisebig <- loadGpsDifar('./DIFAR Testing/BigBoxTest2.sqlite3', buoypath) %>% mutate(
      Buoy = factor(Channel, levels=c(0,1,2,3), labels=c('NE', 'SW', 'SE', 'NW')),
      RealRound = round(RealBearing/.5)*.5) %>%
      select(-c(Latitude, Longitude, MatchedAngles, snr, RMS, ZeroPeak, PeakPeak, SEL))
noisebig <- noiseMatcher(noisebig)
noisebig <- callGrouper(noisebig)
noisebig <- noisebig %>% group_by(callId) %>% mutate(MedianDifar=median(DIFARBearing, na.rm=TRUE), 
                                                     Time=median(posixDate, na.rm=TRUE),
                                                     MedianSA=median(SignalAmplitude, na.rm=TRUE),
                                                     MaxSA=max(SignalAmplitude, na.rm=TRUE)) %>% data.frame

noisebig <- arrange(noisebig, posixDate) 
noisebig$nextTime <- noisebig$posixDate
noisebig$nextTime[2:nrow(noisebig)] <- noisebig$posixDate[1:(nrow(noisebig)-1)]

noisebig$nextGroup <- (as.numeric(seconds((noisebig$posixDate - noisebig$nextTime))) > 20)

PlaybackNumber <- 1
for(i in 1:nrow(noisebig)){
      PlaybackNumber <- PlaybackNumber + noisebig$nextGroup[i]
      noisebig$PlaybackNumber[i] <- PlaybackNumber
}

noisebig$PlaybackNumber <- as.factor(noisebig$PlaybackNumber)
noisebig <- select(noisebig, -c(nextTime, nextGroup))

# SSE and other stuff for medians vs. longest clip length, which would be the normal way to do it
sse <- group_by(noisebig, callId) %>% filter(ClipLength==max(ClipLength), !(i.Species %in% c('upY1', 'upY5', 'upY8'))) %>% 
      select(RealBearing, DIFARBearing, MedianDifar, Buoy, PlaybackNumber, Species) %>% mutate(diff=MedianDifar-DIFARBearing, 
                                                                mederr=RealBearing-MedianDifar-11.7, 
                                                                difarerror=RealBearing-DIFARBearing-11.7, 
                                                                difference=abs(difarerror)-abs(mederr)) %>% 
      data.frame %>% group_by(PlaybackNumber, Buoy) %>%
      summarise(sum(mederr^2)/n(), sum(difarerror^2)/n(), sd(mederr), sd(difarerror), mean(mederr),mean(difarerror)) %>% data.frame

group_by(noisebig, callId) %>% filter(ClipLength==max(ClipLength), !(i.Species %in% c('upY1', 'upY5', 'upY8'))) %>%
      select(RealBearing, DIFARBearing, MedianDifar, Buoy, PlaybackNumber) %>% group_by(PlaybackNumber, Buoy) %>% 
      summarise(mean(MedianDifar), mean(DIFARBearing), sd(MedianDifar), sd(DIFARBearing))

# Graph new vsold
ggplot(data=rbind(select(singleDifar, posixDate, RealBearing, DIFARBearing, Buoy, PlaybackNumber) %>% mutate(id = 'Old'),
                  select(noisebig, posixDate, RealBearing, DIFARBearing, Buoy, PlaybackNumber) %>% mutate(id = 'new')) %>%
             filter(PlaybackNumber==11),
       aes(x=posixDate))+ geom_point(aes(y=DIFARBearing+11.7, color=id, shape=Buoy)) + geom_point(aes(y=RealBearing, color='R'), alpha=.3)

# Graph SNR vs error, normal method
noisebig %>% group_by(callId) %>% filter(ClipLength==max(ClipLength)) %>% data.frame %>%  
      ggplot(aes(x=SNR, y=AdjError, color=abs(NoiseBearing-RealBearing))) + geom_point() +
      facet_wrap(~Buoy, nrow=2) + scale_colour_gradient2(low='black', mid='red',  high='red', midpoint=90)
# Using medians and maxSA. They are pretty similar, notable difference is fewer crappy ones on bottom left
noisebig %>% ggplot(aes(x=MaxSA-NoiseAmplitude, y=RealBearing-MedianDifar-11.7, color=abs(NoiseBearing-RealBearing))) + geom_point() +
      facet_wrap(~Buoy, nrow=2) + scale_colour_gradient2(low='black', mid='red',  high='red', midpoint=90)