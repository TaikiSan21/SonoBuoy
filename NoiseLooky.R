# Check dat noise
library(dplyr)
library(ggplot2)
library(manipulate)
setwd('~/R Projects/SWFSC/SonoBuoy')
source('noiseMatcher.R')
source('loadGpsDifar.R')
source('./DIFAR Testing/callGrouper.R')
buoypath <- 'C:/Users/Taiki/Documents/R Projects/SWFSC/Sonobuoy/spot_messages_RUST_JLK.csv'

noise <- loadGpsDifar('./DIFAR Testing/NoiseDifferentBox9.sqlite3', buoypath) %>% mutate(
      Buoy = factor(Channel, levels=c(0,1,2,3), labels=c('NE', 'SW', 'SE', 'NW')),
      RealRound = round(RealBearing/.5)*.5) %>%
      select(-c(Latitude, Longitude, MatchedAngles, snr, RMS, ZeroPeak, PeakPeak, SEL))

noise <- noiseMatcher(noise)

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
# Polar plot shows angle drifting to noise
ggplot(data=filter(noisebig, PlaybackNumber==9), aes(y=30-SNR, shape=Species)) +  geom_point(aes(x=MedianDifar+11.7, color='D')) +
      geom_point(aes(x=RealBearing, color='R')) +
      geom_point(aes(x=NoiseBearing, color='N')) + 
      geom_hline(yintercept=20) + xlim(0,360) + coord_polar() + facet_wrap(~Channel, nrow=2)


ggplot(data=noise, aes(x=SNR, y=AdjError)) + geom_point()

ggplot(data=noise) + geom_histogram(aes(x=DIFARBearing + 11.7, color='DIFAR')) + 
      geom_histogram(aes(x=NoiseBearing, color='Noise')) +
      geom_histogram(aes(x=RealBearing, color='Real')) + coord_polar() + facet_wrap(~Channel, nrow=2)

ggplot(data=noise, aes(x=posixDate)) + geom_point(aes(y=DIFARBearing, color='D')) + 
      geom_point(aes(y=RealBearing, color='R')) +
      geom_point(aes(y=NoiseBearing, color='N')) + facet_wrap(~Channel, nrow=2)



ggplot(data=noise, aes(y=Distance, x=DIFARBearing)) + geom_bar(stat='identity')

#SNR exponential behavior graph
noise %>% ggplot(aes(x=SNR, y=DIFARBearing+11.7)) + 
      geom_point(aes(color='D')) + 
      geom_point(aes(y=RealBearing, color='R')) + 
      geom_point(aes(y=NoiseBearing, color='N')) +
      facet_wrap(~Channel, nrow=2, scales='free_y')

manipulate({
      noise %>% ggplot(aes_string(x=xpicker, y='DIFARBearing+11.7')) + 
            geom_point(aes(color=log(DifarGain), shape='D')) + 
            geom_point(aes(y=RealBearing, shape='R')) + 
            geom_point(aes(y=NoiseBearing, shape='N')) +
            facet_wrap(~Channel, nrow=2, scales='free_y') +
            scale_colour_gradient(low='blue', high='orange')},
      xpicker=picker('SNR', 'log(DifarGain)'))


# Is gain a proxy for SNR?
noise %>% ggplot(aes(x=log(DifarGain), y=AdjError)) + geom_point(aes(color=as.factor(Channel)))
noise %>% ggplot(aes(x=log(DifarGain), y=SNR, color=as.factor(Channel))) + geom_point()



########## ONLY GOT TO 17:40 ###############