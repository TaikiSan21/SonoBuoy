# Check dat noise
library(dplyr)
library(ggplot2)
library(manipulate)
source('noiseError.R')
source('noiseMatcher.R')
source('loadGpsDifar.R')
source('./DIFAR Testing/callGrouper.R')


### NOTES 7/27 ####
# It's possible that the conversion between magnetic - real is getting messed up somehwere along
# the way, seems like the potential errors could be off by ~ 11.7. Or maybe they are just bad.
# Uncertain if this will actually be useful. Think about how the noise physically happens and is 
# measured. Doesnt work too well with just SNR, tried SNR + 5 and was doing better. Why?
# Lets (ugh) get noise data for our new shit, then we have 2 different data sets to try it on.
###################
# Shit. Adding to the SNR just means that the values are more skewed towards the real bearing. 

# changed shit to be able to use median or max SA. Play with results more now to compare to first way.

# Does it matter that there is a base level of electrical noise? ie. does absolute value of SA have an effect

##### 7/28 #####
# Sum of sines formula - when trying to work backward from SNR & DIFAR to Real, should end up as 
# Acos(phi) + sin(phi)
buoypath <- './Data/spot_messages_RUST_JLK.csv'

noise <- loadGpsDifar('./DIFAR Testing/NoiseDifferentBox9.sqlite3', buoypath) %>% mutate(
      Buoy = factor(Channel, levels=c(0,1,2,3), labels=c('NE', 'SW', 'SE', 'NW')),
      RealRound = round(RealBearing/.5)*.5) %>%
      select(-c(Latitude, Longitude, MatchedAngles, snr, RMS, ZeroPeak, PeakPeak, SEL))

noise <- noiseMatcher(noise)

noisebig <- loadGpsDifar('./DIFAR Testing/NoiseDifferentBox10.sqlite3', buoypath, buoyfunc=firstTrialId) %>% mutate(
      Buoy = factor(Channel, levels=c(0,1,2,3), labels=c('NE', 'SW', 'SE', 'NW')),
      RealRound = round(RealBearing/.5)*.5) %>%
      select(-c(Latitude, Longitude, MatchedAngles, snr, RMS, ZeroPeak, PeakPeak, SEL))
noiseDict <- data.frame(Species = c('dn1', 'DN5', 'dn8', 'upX1', 'upX5', 'upX8',
                                    'upY1', 'upY5', 'upY8', 'upZ1', 'upZ5', 'upZ8',
                                    'tone1', 'tone5', 'tone8'),
                        Noise = c(rep('ambientbig', 12), rep('ambientsmall', 3)))
noisebig <- callGrouper(noisebig) %>% noiseMatcher(noiseDict, noisename = 'ambient') %>%
      mutate(NoiseDifar = noiseError(SNR, NoiseBearing, RealBearing - 11.7),
             NoiseError = mapply(errorTransform, RealBearing, NoiseDifar + 11.7),
             ErrorDiff = mapply(errorTransform, AdjError, NoiseError),
             PotentialBearing = DIFARBearing + 11.7 + NoiseError,
             PotentialError = mapply(errorTransform, RealBearing, PotentialBearing)
      )
## WTF
noise <- filter(noisebig, grepl('ambient', Species)) %>%
      select(matchDate=UTC, Species, NoiseBearing=DIFARBearing, NoiseAmplitude=SignalAmplitude, Channel) %>%
      merge(noiseDict, by.x = 'Species', by.y = 'Species', all.x = TRUE, sort = FALSE)
##
noisebig <- arrange(noisebig, UTC) 

noisebig$nextTime <- noisebig$UTC
noisebig$nextTime[2:nrow(noisebig)] <- noisebig$UTC[1:(nrow(noisebig)-1)]

noisebig$nextGroup <- (as.numeric(seconds((noisebig$UTC - noisebig$nextTime))) > 20)


PlaybackNumber <- 1
for(i in 1:nrow(noisebig)){
      PlaybackNumber <- PlaybackNumber + noisebig$nextGroup[i]
      noisebig$PlaybackNumber[i] <- PlaybackNumber
}
noisebig <- select(noisebig, -c(nextTime, nextGroup, TriggerName, BuoyHeading, PCTime, PCLocalTime, UTCMilliseconds))
# noisebig <- callGrouper(noisebig) %>% mutate(NoiseDifar = noiseError(SNR, NoiseBearing, RealBearing - 11.7),
#                                              NoiseError = mapply(errorTransform, RealBearing, NoiseDifar + 11.7),
#                                              ErrorDiff = mapply(errorTransform, AdjError, NoiseError),
#                                              PotentialBearing = DIFARBearing + 11.7 + NoiseError,
#                                              PotentialError = mapply(errorTransform, RealBearing, PotentialBearing)
#                                              )
# # noisebig <- noisebig %>% group_by(callId) %>% mutate(MedianDifar=median(DIFARBearing, na.rm=TRUE),
# #                                                      Time=median(UTC, na.rm=TRUE),
# #                                                      MedianSA=median(SignalAmplitude, na.rm=TRUE),
# #                                                      MaxSA=max(SignalAmplitude, na.rm=TRUE)) %>% data.frame
# # 

# Check only tones - should have more reliable SNR measures. Removing +5 from SNR
noisebig %>% filter(grepl('tone', Species), Distance > 1000, SNR > 10) %>%
      ggplot(aes(x=abs(AdjError), y=abs(PotentialError), color=abs(PotentialError))) + geom_point(size=2) + #facet_wrap(~Channel) +
      scale_color_gradientn(colors=viridis(256)) + geom_abline(slope=1) + xlim(0, 30) + ylim(0,30)

# What could we get if we could do this
noisebig %>% filter(Distance > 1000) %>%
      ggplot(aes(x=abs(PotentialError), y=abs(AdjError), color=abs(NoiseError))) + geom_point(size=2) + geom_abline(slope=1) +
      xlim(0,120) + ylim(0,150) + geom_vline(xintercept=10, size=2, color='darkgreen', alpha=.5) + facet_wrap(~Channel) +
      geom_hline(yintercept=10, size=2, color='orange', alpha=.5) + scale_color_gradientn(colors=viridis(256, option='D', direction=-1))

sum(abs(noisebig$AdjError) < 10)
sum(abs(noisebig$PotentialError) < 10)
# Why is it getting some so wrong?
noisebig %>% filter(Distance > 1000, SNR < 10) %>%
      ggplot(aes(x=NoiseError, y=PotentialError, color=SNR)) + 
      geom_point() + scale_color_gradientn(colors=viridis(256, direction=-1)) +
      geom_abline(slope=1) + geom_hline(yintercept=c(-10,10)) + ylim(-50,50)

# Check what noise error should be
noisebig %>% filter(abs(AdjError) < 40, Distance > 1000) %>% 
      ggplot(aes(x=AdjError, y=NoiseError, color=SNR)) + geom_point() +
      facet_wrap(~Channel) + scale_color_gradientn(colors=viridis(256)) + geom_abline(slope=1, size=2, alpha=.5) +
      ylim(-30, 30) + xlim(-30,30)

# Compare what our error was to what should be
noisebig %>% filter(Distance > 1000, abs(ErrorDiff) < 35) %>% 
      ggplot() +geom_point(aes(x=RealBearing, y=NoiseError, color=abs(ErrorDiff))) + facet_wrap(~Channel) +
      scale_color_gradientn(colors=viridis(256))

# Noise bearings vs real bearings
noisebig %>% 
      ggplot(aes(x=RealBearing, y=NoiseError)) + geom_point() + facet_wrap(~Channel)

# What if we could correct difar by this expected noise error
noisebig %>% filter(Distance > 1000, abs(AngleError) < 45) %>% 
      ggplot(aes(x=RealBearing, y=DIFARBearing + 11.7 + NoiseError, color= abs(AngleError)))+ 
      geom_point() + geom_abline(slope=1) + scale_color_gradientn(colors=viridis(256))

# Polar plot shows angle drifting to noise
ggplot(data=filter(noisebig, PlaybackNumber==1, !grepl('tone', Species)), aes(y=30-SNR)) +  
      geom_point(aes(x=DIFARBearing+11.7, color='DIFAR + Magnetic Deviation')) +
      geom_point(aes(x=RealBearing, color='Real Bearing (True)')) +
      geom_point(aes(x=NoiseBearing+11.7, color='Noise Bearing + Magnetic Deviation')) + 
      geom_hline(aes(yintercept=20, color='10dB SNR'), size=1) + 
      scale_y_continuous(labels=c(20, 10, 0), breaks=c(10, 20, 30), limits=c(0,30)) +
      scale_x_continuous(labels=c(360,90,180,270), breaks=c(0,90,180,270), limits=c(0,360)) +
      coord_polar() + facet_wrap(~Channel, nrow=2) + 
      labs(title='Swept Calls Only', x='DIFAR Bearing + Magnetic Deviation', y='SNR') +
      theme(plot.title = element_text(hjust=.5)) +
      scale_color_manual(values=c('black','#F8766D','#00BA38','#619CFF'),
                         guide = guide_legend(override.aes = list(
                               linetype = c('solid', rep('blank', 3)),
                               shape = c(NA, 16, 16, 16))))
      

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