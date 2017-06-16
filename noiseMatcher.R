# Noise matching

noiseMatcher <- function(df) {
      noise <- filter(df, Species %in% c('ambientbig', 'ambientsmall')) %>%
            select(matchDate=posixDate, Species, NoiseBearing=DIFARBearing, NoiseAmplitude=SignalAmplitude, Buoy) %>%
            mutate(tone = grepl('small', Species))
      noise <- data.table(noise, key=c('Buoy', 'tone', 'matchDate'))
      data <- filter(df, !(Species %in% c('ambientbig', 'ambientsmall'))) %>%
            mutate(tone = grepl('tone', Species),
                   matchDate =  posixDate)
      data <- data.table(data, key= c('Buoy', 'tone', 'matchDate'))
      noise[data, roll='nearest'] %>% select(-c(matchDate, tone)) %>%
            mutate(SNR=SignalAmplitude-NoiseAmplitude)
}
            