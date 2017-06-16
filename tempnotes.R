library(manipulate)
# How do these things relate to each other for each call. Are SA/Gain reliably related to Distance? They should be...
singleDifar %>% filter(Species == 'dn8', PlaybackNumber == 6) %>% select(Buoy, DifarGain, SignalAmplitude, Distance)

# colors along theoretical gain imbalance lines
# Gain imbalance probably can't be real? Rotating sonobuoy, not sure.
# ^ According to a paper they are measured against magnetic N/S, E/W so should still be real. Don't know how it works.
ggplot(filter(singleDifar, Distance <1), aes(x=Distance, y=AngleError-11.7, color=RealBearing)) + geom_point(size=2) + 
      scale_color_gradientn(colours=c('white', 'red', 'white', 'red', 'white', 'red', 'white', 'red', 'white'),
                            breaks=c(0, 45, 90, 135, 180, 225, 270, 315, 360))

# Gain and SA have to be useful somehow. 
manipulate({
      ggplot(filter(singleDifar, Distance>1, abs(AngleError-11.7)<25), aes_string(x=xpick, y='AngleError-11.7', color=colpick)) + 
            geom_point(size=2) + scale_color_gradient(low='white', high='red') + geom_smooth()},
      xpick=picker('log(DifarGain)', 'log(SignalAmplitude)'),
      colpick=picker('Distance', 'Buoy'))



# SA Distance by Buoy. It's different across them? Possible some read hotter, possible some are noisier so adds
# to the overall SA level read. Possibly can compare a ration of gain/SA, but probably needs to be difference in
# SA over background noise level. Depends on what that gain number means - something to ask BM? I know it's the
# difargram value, but where does that come from/what does it represent physically. Maybe same question for SA -
# it's NaN for some large windows of noise. Below certain thresh it doesn't register at all?

singleDifar %>% filter(Distance < 12) %>% 
      ggplot(aes(x=log(Distance), y=log(SignalAmplitude), color=Buoy)) + 
      geom_point(size=2) + geom_smooth() + facet_grid(.~Intensity)