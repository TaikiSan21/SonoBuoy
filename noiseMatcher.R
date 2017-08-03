# Noise matching
# Need a noise dict to match. Species - Noise
noiseMatcher <- function(df, dict, noisename = 'noise') {
      noise <- filter(df, grepl(noisename, Species)) %>%
            select(matchDate=UTC, Noise = Species, NoiseBearing=DIFARBearing, NoiseAmplitude=SignalAmplitude, Channel) %>%
            merge(dict, by.x = 'Noise', by.y = 'Noise', all.x = TRUE, sort = FALSE)
      noise <- data.table(noise, key=c('Channel', 'Species', 'matchDate'))
      data <- filter(df, !(grepl(noisename, Species))) %>%
            mutate(matchDate =  UTC, Noise=Species)
      data <- data.table(data, key= c('Channel', 'Species', 'matchDate'))
      noise[data, roll='nearest'] %>% select(-c(matchDate, i.Noise)) %>%
            {if('callId' %in% names(.)) {group_by(., callId) %>% mutate(MedianDifar=median(DIFARBearing, na.rm=TRUE), 
                                        Time=median(UTC, na.rm=TRUE),
                                        MedianSA=median(SignalAmplitude, na.rm=TRUE),
                                        MaxSA=max(SignalAmplitude, na.rm=TRUE)) %>% data.frame %>%
            mutate(MaxSNR = MaxSA - NoiseAmplitude,
                   MedianSNR = MedianSA - NoiseAmplitude)} else . } %>% 
            mutate(SNR = SignalAmplitude - NoiseAmplitude)
      
}
