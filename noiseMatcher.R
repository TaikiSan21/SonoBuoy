# Noise matching

noiseMatcher <- function(df) {
      noise <- filter(df, Species %in% c('ambientbig', 'ambientsmall')) %>%
            select(matchDate=UTC, Species, NoiseBearing=DIFARBearing, NoiseAmplitude=SignalAmplitude, Buoy) %>%
            mutate(tone = grepl('small', Species))
      noise <- data.table(noise, key=c('Buoy', 'tone', 'matchDate'))
      data <- filter(df, !(Species %in% c('ambientbig', 'ambientsmall'))) %>%
            mutate(tone = grepl('tone', Species),
                   matchDate =  UTC)
      data <- data.table(data, key= c('Buoy', 'tone', 'matchDate'))
      noise[data, roll='nearest'] %>% select(-c(matchDate, tone)) %>%
            group_by(callId) %>% mutate(MedianDifar=median(DIFARBearing, na.rm=TRUE), 
                                        Time=median(UTC, na.rm=TRUE),
                                        MedianSA=median(SignalAmplitude, na.rm=TRUE),
                                        MaxSA=max(SignalAmplitude, na.rm=TRUE)) %>% data.frame %>%
            mutate(SNR = SignalAmplitude - NoiseAmplitude,
                   MaxSNR = MaxSA - NoiseAmplitude,
                   MedianSNR = MedianSA - NoiseAmplitude)
      
}
