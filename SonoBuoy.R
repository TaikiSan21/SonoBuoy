# Sono Buoy study for SWFSC
library(RSQLite)
library(ggplot2)
library(dplyr)
library(data.table)
library(manipulate)
library(geosphere)
library(caret)
library(lubridate)
library(plotly)

setwd('~/R Projects/SWFSC/SonoBuoy')
source('SonoBuoyFunctions.R')
source('~/R Projects/swfscMisc/R/bearing.R')
source('~/R Projects/swfscMisc/R/convert.angle.R')
source('~/R Projects/swfscMisc/R/convert.distance.R')
source('~/R Projects/swfscMisc/R/distance.R')
source('~/R Projects/swfscMisc/R/datum.R')
# pairedDifar <- data.table(read.csv('DIFAR_paired_buoys.csv'))
# finalDifar <- data.table(read.csv('DIFAR_single_buoy.csv')) # this is with my median error. BuoyHeading.y has the offset used

finalDifar <- data.table(read.csv('DIFAR_single_buoy_308.csv')) %>% # using jenns calibration, kept more columns (gain, amplitude freq, milliseconds)
      mutate(UTC=ymd_hms(UTC),
             Intensity=as.factor(Intensity),
             PlaybackNumber=as.factor(PlaybackNumber))

pairedDifar <- data.table(read.csv('DIFAR_paired_buoys_308.csv')) # same as above
      
# paired <- read.csv('DIFAR_paired_buoys.csv')

# str(pairedDifar)
# head(pairedDifar,15)
#### DATA FOR ERIC / P(DETECT)
# Buoy, Angle, Call Type, Intensity, Distance
detectData <- finalDifar[, .(Distance, DifarFixed, RealBearing, Channel, CallType, Intensity)]
colnames(detectData) <- c('Distance', 'FixedBearing', 'RealBearing', 'Buoy', 'CallType', 'Intensity')
write.csv(detectData, 'DetectionData.csv')
#### Making plot to export. Density plot of distance error colored by channels. Used different cal values.
manipulate({ggplot(pairedDifar, aes_string(colour=xpicker, x='distError')) + geom_density() + xlim(0,5)}, 
           xpicker=picker( 'factor(PairNum)', 'Channel1'))
testdat <- pairedDifar[,.(distError, PairNum)]
jennError <- ggplot(pairedDifar, aes(colour=factor(PairNum), x=distError)) + geom_density() + xlim(0,5)

ggsave('medianError.jpg',plot=jennError, width=8, height=6, units='in', dpi=200)
######

# intersectPoint <- function(angle1, angle2, angle12, lat1, long1, lat2, long2)
arrange(summarise(group_by(finalDifar, Species), mean=mean(RealBearing-DifarFixed), std=sd(RealBearing-DifarFixed)), desc(std))

intersectYes <- mapply( doesIntersect,pairedDifar$DifarFixed1, pairedDifar$DifarFixed2, pairedDifar$PairBearing)
# write.csv(finalDifar, 'DIFAR_actual_locations.csv')
finalDifar$Channel <- factor(finalDifar$Channel,levels=c(0,1,2,3), labels=c('NE', 'SW', 'SE', 'NW'))
########### SignalAmplitude Model #########################
ampModelFull <- lm(SignalAmplitude ~ Distance + Intensity + CallType + Distance*Intensity + Distance*CallType + 
                      Intensity*CallType + Distance*Intensity*CallType, data=finalDifar)
ampModel2Way <- lm(SignalAmplitude ~ Distance + Intensity + CallType + Distance*Intensity + Distance*CallType + 
                         Intensity*CallType, data=finalDifar)
ampModelNoInter <- lm(SignalAmplitude ~ Distance + Intensity + CallType, data=finalDifar)

anova(ampModelNoInter, ampModelFull)
anova(ampModel2Way, ampModelFull)
anova(ampModelNoInter, ampModel2Way)
qplot(x=finalDifar$Distance, y=ampModel2Way$residuals)
qplot(x=finalDifar$SignalAmplitude, y=ampModel2Way$residuals)
qplot(x=ampModel2Way$fitted.values, y=ampModel2Way$residuals)
qplot(x=finalDifar$Distance, y=finalDifar$SignalAmplitude)
qplot(x=finalDifar$SignalAmplitude, y=(finalDifar$RealBearing - finalDifar$DIFARBearing))
qplot(x=finalDifar$Distance, y=finalDifar$SignalAmplitude)

modelData <- finalDifar[, .(SignalAmplitude, Distance, Intensity, CallType)]
modelData$logSA <- log(modelData$SignalAmplitude)
modelData$logDistance <- log(modelData$Distance)
qplot(x=modelData$logDistance, y=modelData$logSA)
qplot(x=modelData$Distance, y=modelData$logSA)
qplot(x=modelData$Distance, y=modelData$SignalAmplitude)
########################################################
# MODELING ANGLE ERROR
########################################################
angleData <- errorsOnly[, .(AngleError, Distance, Channel, RealBearing)]
angleData$FactorBearing <- as.factor(sapply(angleData$RealBearing, function(x) toDirection(x)))
angleData$CosineBearing <- sapply(angleData$RealBearing, function(x) cos(x*pi/180))
angleFactor <- lm(AngleError ~ Distance*Channel*FactorBearing, data=angleData)
angleCos <- lm(AngleError ~ Distance*Channel*CosineBearing, data=angleData)
############################################
# Get DIFAR error for each station to compare to Jennifer's calibrations
errorsOnly <- finalDifar[, .(TrackedGroup, Channel, Species, Distance, DIFARBearing, RealBearing, DifarFixed, SignalAmplitude)]
errorsOnly$Channel <- factor(errorsOnly$Channel, levels=c(0,1,2,3), labels=c('NE', 'SW', 'SE', 'NW'))
errorsOnly$AngleError <- ((errorsOnly$RealBearing - errorsOnly$DIFARBearing) %% 360)
errorsOnly$AngleErrorFixed <- ((errorsOnly$RealBearing - errorsOnly$DifarFixed) %% 360)
errorsOnly$AngleErrorMagFixed <- ((errorsOnly$RealBearing - errorsOnly$DIFARBearing - 11.7) %% 360)
# Angle errors like 359 -> -1
errorsOnly$AngleError <-sapply(errorsOnly$AngleError, function(x) {
      if(x < abs(x-360)){x}
      else {x-360}
})
errorsOnly$AngleErrorFixed <-sapply(errorsOnly$AngleErrorFixed, function(x) {
      if(x < abs(x-360)){x}
      else {x-360}
})
errorsOnly$AngleErrorMagFixed <-sapply(errorsOnly$AngleErrorMagFixed, function(x) {
      if(x < abs(x-360)){x}
      else {x-360}
})
# Error by amplitude. Gradient should be removed to look by channel.
manipulate({ggplot(errorsOnly, aes_string(x='SignalAmplitude', y=errpick, colour=colpick)) + geom_point() +
            scale_colour_gradient(low='red', high='green') + ylim(-50,50)},
           errpick=picker('AngleError', 'AngleErrorFixed', 'AngleErrorMagFixed'), colpick=picker('Distance', 'Channel' ))
# Error by bearing
manipulate({ggplot(errorsOnly[Distance > 1,], aes_string(x='RealBearing', y=errpick, colour=colpick)) + geom_point(aes(shape=Channel)) +
            scale_colour_gradient(low='red', high='green') + ylim(-50,50)},
           errpick=picker('AngleErrorMagFixed', 'AngleError', 'AngleErrorFixed'), colpick=picker('Distance', 'as.numeric(Channel)' ))
# Abs(Error) by bearing
manipulate({ggplot(errorsOnly[Distance > 1,], aes_string(x='RealBearing', y=errpick, colour=colpick)) + geom_point(aes(shape=Channel)) +
            scale_colour_gradient(low='red', high='green') + ylim(0,50)},
           errpick=picker('abs(AngleErrorMagFixed)', 'abs(AngleError)', 'abs(AngleErrorFixed)'), colpick=picker('Distance', 'Channel' ))




errorsChannel <- summarise(group_by(errorsOnly, Channel),Error = median(AngleError), Bearing= mean(RealBearing), Distance=mean(Distance))
errorsStation <- summarise(group_by(errorsOnly, TrackedGroup), Error = median(AngleError), Bearing = mean(RealBearing), Distance=mean(Distance))
errorsStationChannel <- summarise(group_by(errorsOnly, TrackedGroup, Channel),Error = median(AngleError), Bearing = mean(RealBearing), Distance=mean(Distance))
# Does error vary with direction?
directionPlot <- ggplot(errorsStationChannel, aes(x=Bearing, y=Error, colour=Channel)) + geom_point(aes(size=2)) + 
      labs(x='Real Bearing', title='DIFAR error vs True Bearing', y='Median DIFAR error at each station')
ggsave('DirectionError.jpg', plot=directionPlot, width=8, height=6, units='in', dpi=300)
# Distance vary with direction?
directionDistance <- ggplot(errorsStationChannel, aes(x=Bearing, y=Distance, colour=Channel)) + geom_point(aes(size=2)) + 
      labs(x='Real Bearing', title='Distance vs True Bearing', y='Mean distance from each station') 
ggsave('DirectionDistance.jpg', plot=directionDistance, width=8, height=6, units='in', dpi=300)
# All errors not just median per station
directionAllErrorChannel <- ggplot(errorsOnly, aes(x=RealBearing, y=AngleError, colour=Channel)) + geom_point() + 
      labs(x='Real Bearing', title='DIFAR error vs True Bearing colored by buoy', y='GPS Bearing - DIFAR') + ylim(-50,50) 
ggsave('ErrorAllPointsByBuoy.jpg', plot=directionAllErrorChannel, width=8, height=6, units='in', dpi=300)
directionAllErrorDistance <- ggplot(errorsOnly, aes(x=RealBearing, y=AngleError, colour=Distance)) + geom_point() + 
      labs(x='Real Bearing', title='DIFAR error vs True Bearing colored by distance', y='GPS Bearing - DIFAR') +
      scale_colour_gradient(low='red', high='green') + ylim(-50,50)
ggsave('ErrorAllPointsByDistance.jpg', plot=directionAllErrorDistance, width=8, height=6, units='in', dpi=300)
directionAllErrorDistanceTrunc <- ggplot(errorsOnly[errorsOnly$Distance>1,], aes(x=RealBearing, y=AngleError, colour=Channel)) + geom_point() + 
      labs(x='Real Bearing', title='DIFAR error vs True Bearing colored by distance (Distance > 1km)', y='GPS Bearing - DIFAR') + ylim(-50,50)
      #scale_colour_gradient(low='red', high='green') + ylim(-50,50)

directionAllErrorDistance <- ggplot(errorsOnly, aes(x=RealBearing, y=AngleError, colour=Channel)) + geom_point() + 
      labs(x='Real Bearing', title='DIFAR error vs True Bearing colored by distance (Distance > 1km)', y='GPS Bearing - DIFAR') + ylim(-50,50)

ggsave('ErrorAllPointsByDistanceTrunc.jpg', plot=directionAllErrorDistanceTrunc, width=8, height=6, units='in', dpi=300)
summarise(group_by(calCheck, Channel), mean(AngleError))
# ###########################################
ggplot(errorsOnly, aes(x=AngleError, colour=Channel)) + geom_density() + xlim(-20,30)
ggplot(errorsOnly, aes(x=RealBearing, y=AngleErrorFixed, colour=Distance)) + geom_point(aes(shape=Channel))

calCheck <- summarise(group_by(errorsOnly, TrackedGroup, Channel), AngleError=median(AngleError), Distance=mean(Distance))
ggplot(calCheck, aes(x=Distance, y=AngleError, colour=factor(Channel))) + geom_point() + geom_hline(y=12.7)

ggplot(calCheck, aes(x=AngleError, colour=factor(Channel))) + geom_density() + xlim(-20,30)

# Testing intersection point I think?
index <- 95
test <- with(pairedDifar, intersectPoint(RealBearing1[index], RealBearing2[index], PairBearing[index], NewBuoyLat1[index],
                                         NewBuoyLong1[index], NewBuoyLat2[index], NewBuoyLong2[index]))
xyangle <- with(pairedDifar, atan((BoatLat1-NewBuoyLat1)/(BoatLong1-NewBuoyLong1))*180/pi)
t0 <- (xyangle %% 360)
t1 <- ((90-pairedDifar$RealBearing1) %% 360)

diffs <- (xyangle %% 360)-((90-pairedDifar$RealBearing1) %% 360)
# Intensity vs signal amplitude
amplitudes <- select(finalDifar, c(Distance, Species, SignalAmplitude))
amplitudes$intensity <- sapply(amplitudes$Species, function(x) gsub('[a-zA-Z]','',x))
amplitudes$Species <- sapply(amplitudes$Species, function(x) gsub('[0-9]', '', x))
amplitudes$Species <- as.factor(amplitudes$Species)
amplitudes$intensity <- as.numeric(amplitudes$intensity)

ggplot(amplitudes, aes(x=SignalAmplitude, colour=factor(intensity))) + geom_histogram()
# Amplitude vs distance with intensity or species
manipulate({ggplot(amplitudes, aes_string(x='Distance', y='SignalAmplitude', colour=colpick)) + 
            geom_point() + geom_smooth(method='loess')},
           colpick = picker('factor(intensity)', 'Species'))


ggplot(data=errorsOnly, aes(x=AngleError)) + geom_histogram(binwidth=2) + xlim(-50,50)
ggplot(data=errorsOnly[errorsOnly$Distance>1,], aes(x=AngleError, colour=Channel)) + geom_density() + xlim(-50,50)
# Checking how difference between intersected point 
# write.csv(pairedDifar,'DIFAR_paired_buoys.csv')
############
# GRAPHS
############

# Pick a station, show guesses for each buoy pair
manipulate({
      ggplot(pairedDifar[pairedDifar$TrackedGroup1==sliderin,]) + geom_point(aes(x=BoatLong1, y=BoatLat1, color='Actual'), color='black',size=3) + 
            geom_point(aes(x=IntLong, y=IntLat, colour=factor(PairNum), alpha=.02), size=2) + ylim(32.79,32.84) + xlim(-117.615, -117.55)},
      sliderin=slider(1,12)
)

# Pick a station, pick a buoy pair. Plots buoy, boat, and guess locations
manipulate({
      ggplot(pairedDifar[pairedDifar$PairNum==pairin,][pairedDifar$TrackedGroup1==sliderin,]) + geom_point(aes(x=BoatLong1, y=BoatLat1, color='Actual'), color='black',size=3) + 
            geom_point(aes(x=IntLong, y=IntLat, colour=factor(PairNum), alpha=.2, shape=factor(Intensity)),size=4) + ylim(32.79,32.84) + xlim(-117.615, -117.55) +
            geom_point(aes(x=NewBuoyLong1, y=NewBuoyLat1, colour=Channel1), size=4) +
            geom_point(aes(x=NewBuoyLong2, y=NewBuoyLat2, colour=Channel2), size=4)},
      sliderin=slider(1,12), pairin=slider(1,6)
)

index <- 1580
intercept <- pairedDifar$BoatLat1[index] - pairedDifar$BoatLong1[index]*bearingToSlope(pairedDifar$RealBearing1[index])
intersectGraph <- ggplot(pairedDifar[index,]) + geom_point(aes(x=BoatLong1, y=BoatLat1, color='Boat')) +
      geom_point(aes(x=NewBuoyLong1, y=NewBuoyLat1, color='Signal')) +
      geom_abline(slope=bearingToSlope(pairedDifar$RealBearing1[index]), intercept=intercept)
intersectGraph
### LABEL CHANNELS NW:2 NE:0 SW:1 SE:3
# Boat and buoy location graph

mapGraph <- ggplot(data=smallDifar[1:20,]) + geom_point(aes(x=BoatLong, y=BoatLat, color = 'Boat')) + 
      geom_point(aes(x=BuoyLong, y=BuoyLat, colour=as.factor(Channel))) +
      labs(color='Legend')
mapGraph

# Pre-adjustment plot
manipulate({
      ggplot(data=finalDifar, aes_string(x=xpick, y=('RealBearing-DIFARBearing'), color=colpick)) + geom_point() + ylim(-100,100)},# + 
      # abline(h=0, col='red') + abline(h=20, col='black') + abline(h=-20,col='black')},
      xpick=picker('Distance', 'numDate', 'Id'), colpick=picker('Species', 'Channel', 'Distance', 'RealBearing'))

ggplot(data=finalDifar, aes(x=Distance, y=(RealBearing-DIFARBearing), color=RealBearing)) + geom_point() + ylim(-100,100) +
      scale_color_gradient2(low='red', mid='green', high='red', midpoint=180)

# Adjusted DIFAR
manipulate({
      ggplot(data=finalDifar, aes_string(x=xpick, y=('RealBearing-DifarFixed'), color=colpick)) + geom_point()},# + ylim(-100,100)},# + 
      # abline(h=0, col='red') + abline(h=20, col='black') + abline(h=-20,col='black')},
      xpick=picker('numDate', 'Distance'), colpick=picker('Species', 'Channel', 'Distance')
)
# Gain-8 only
eights <- finalDifar[grep('8', finalDifar$Species),]
manipulate({
      ggplot(data=eights, aes_string(x=xpick, y=('RealBearing-DIFARBearing'), color=colpick)) + geom_point() + ylim(-100,100)},# + 
      # abline(h=0, col='red') + abline(h=20, col='black') + abline(h=-20,col='black')},
      xpick=picker('numDate','Distance'), colpick=picker('Species', 'Channel', 'Distance')
)
# Dist > 1.5
fars <- finalDifar[Distance > 1.5,]
manipulate({
      ggplot(data=fars, aes_string(x=xpick, y=('RealBearing-DifarFixed'), color=colpick)) + geom_point()},# + ylim(-100,100)},# + 
      # abline(h=0, col='red') + abline(h=20, col='black') + abline(h=-20,col='black')},
      xpick=picker('numDate', 'Distance'), colpick=picker('Species', 'Channel', 'Distance')
)
# Plot broken by station
manipulate({
      ggplot(data=smallDifar[grep(paste('S',numpick,'\\s',sep=''), smallDifar$TrackedGroup),], aes_string(x=xpick, y=('RealBearing-DifarFixed'), color=colpick)) +
            geom_point() + ylim(-100,100)},
      xpick=picker('numDate','Distance'), colpick=picker('Channel', 'Distance', 'Species', 'PlaybackNumber'), numpick=slider(1,12))
# Plot to check PlaybackNumber works right
ggplot(data=smallDifar, aes(x=posixDate, y=RealBearing-DifarFixed, color=PlaybackNumber)) + geom_point()

# Distance or angle between buoys over time
ggplot(data=pairedDifar, aes(x=numDate1, y=PairBearing, color=Channel1)) + geom_point()

# Looking at bearings
ggplot(data=pairedDifar[1371,]) + geom_point(aes(x=NewBuoyLong1, y=NewBuoyLat1)) + geom_point(aes(x=NewBuoyLong2, y=NewBuoyLat2)) +
      geom_point(aes(x=BoatLong2, y=BoatLat2, col='green'))


####### FOR SHANNON ##########
# Plotly?
# All calls centered around one spot
p <- plot_ly(
      df, r = ~Distance, t = ~RealBearing, color = ~Detected, alpha = .05,
      type = 'scatter', colors=c('red', 'green'))
layout(p, title = 'Relative call locations', orientation = -90,
       margin = list(t=100, r=100))

# Data collection track
buoys <- data.table(summarise(group_by(filter(finalDifar, TrackedGroup == 'S1'), Buoy), Lat=mean(BuoyLat), Long=mean(BuoyLong)))
stations <- data.table(summarise(group_by(filter(finalDifar, Buoy == 'NE'), TrackedGroup), Distance = mean(Distance), Bearing = mean(RealBearing)))
stations$Long <- destPoint(c(buoys[1,Long], buoys[1,Lat]), b=stations$Bearing, d=1000*stations$Distance)[,1]
stations$Lat <- destPoint(c(buoys[1,Long], buoys[1,Lat]), b=stations$Bearing, d=1000*stations$Distance)[,2]
stations$TrackedGroup <- sapply(stations$TrackedGroup, function(x) gsub('S', '', x))
g <- ggplot() + geom_point(data=buoys, aes(x=Long, y=Lat, color=Buoy), size=4) +
      geom_point(data=stations, aes(x=Long, y=Lat), size=2) +
      geom_text(data=stations, aes(x=Long, y=Lat, label=TrackedGroup), hjust=-.4, vjust=0) +
      xlim(-117.617, -117.562) +
      theme(panel.background = element_rect(fill='lightblue'),
            panel.grid = element_blank())
g

# Location estimates prettified
manipulate({
      ggplot(pairedDifar[pairedDifar$PairNum==pairin,][pairedDifar$TrackedGroup1==sliderin,]) + geom_point(aes(x=BoatLong1, y=BoatLat1, color='Actual Location'),size=3) + 
            geom_point(aes(x=IntLong, y=IntLat, color='Estimated Location'), alpha=.2,size=4) + ylim(32.79,32.84) + xlim(-117.615, -117.55) +
            geom_point(aes(x=NewBuoyLong1, y=NewBuoyLat1, color='Buoy'), size=4) +
            geom_point(aes(x=NewBuoyLong2, y=NewBuoyLat2, color='Buoy'), size=4) +
            theme(legend.title=element_blank(),
                  panel.background = element_rect(fill='lightblue'),
                  panel.grid = element_blank()) +
            scale_color_manual(name='', values=c('Buoy'='black', 'Actual Location'='limegreen', 'Estimated Location'='red')) +
            labs(x='Longitude', y='Latitude', title='Estimated Signal Location')},
      sliderin=slider(1,12), pairin=slider(1,6)
)
