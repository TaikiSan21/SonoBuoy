library(geosphere)
library(ggplot2)
source('SonoBuoyFunctions.R')
################################
######### TO DO LIST ###########
################################

# 1: Use Von Mises distribution instead of normal - details in greene.etal paper on your desktop folder
# 2: Re-do the times for the original data set (this is already done?) then re-try the hyperbolic shiz
#     - Hyperbolic has some error, axis isn't rotating properly. Use bearings not slopes?
# 3: If that works, how do we estimate error? Probably can try monte carlo, time is random in some way, 
# how does that randomness translate. 
# ^^^ I don't think we actually draw a hyperbola. We have time difference. We know what the difference should
# be at every spot. Say times are normally distributed, you get a density. ITS SO GOOD. But our times are fucked
# Don't know if PAM is accurate enough - something bad is happening to the times. Fixed my mistake, still way
# off on some of them. Probably have to use cross corr to find the time diffs, somehow a way with just our data?
# 4: Re-adjust grid building to be for arbitrary number of buoys. Can't hard code the 2 in. Probably also
# pass out density calculations to a separate function. That's what we will do in the future, and makes it
# easy to edit shit later. 
# 5: Possible option for graph : make it a manipulate, be able to select which density you want to look at

###########
# We can convert units from dist, angle to lat, long and get prob distributions. It isn't straight forward,
# but go to kaharris.org/teaching/425/Lectures/lec30.pdf
# Distance needs to not be p(det) though. Real density. I think we can turn
# the logit probability into a density. We just need to integrate D - 0->inf, then divide by that?
# Then it integrates to 1, and the density should just be proportional to that probability right?

# Why does clip length come in the same cutoffs? . ... 8333.....? 

# Questions

# Second model:

# From Baye's rule the equation for what we want is:
# P(Lat, Long | Difar1, Difar2, Det1, Det2) ~ P(Difar1, Difar2, Det1, Det2 | Lat, Long) * P(Lat, Long)

# Where P(Difar | Lat, Long) = P(Difar | TrueAngle) ~ Normal(TrueAngle + Declination, 10) 
# P(Det | Lat, Long) ~ Our model
# P(Lat, Long) ~ Uniform prior: angle ~ U(0-360), Dist ~ U(0-4)

# How do I translate this to a jags model? 

# logit model not related to graphing. Using to get pr(dt)
df$cosangle <- cos(convert.angle(df$RealBearing))
df$Intensity <- as.factor(df$Intensity)
logdet <- glm(Detected ~ Distance + cosangle + Distance * cosangle, data=df[df$Intensity != '1',], family='binomial')
logsimp <- glm(Detected ~ Distance, data=df[df$Intensity != '1' & df$Detected == TRUE,], family='binomial')
library(glmnet)
xval <- model.matrix(as.formula('~ Buoy + Distance + CallType + Intensity + cosangle'), df[,.(Buoy, Distance, CallType, Intensity, cosangle)])
yval <- df$detected
logcv <- cv.glmnet(xval, yval, family='binomial', standardize=TRUE, nfolds=10, alpha=1, type.measure = 'class')

# Getting model data from DetectionVisualization.R
# Coefficientsfor models - logit and mean/std for angle distribution
detCoeff <- summarise(group_by(detdf, Buoy, CallType, Intensity), DistCoeff=median(DistCoeff), AngCoeff=median(AngCoeff), InterCoeff=median(InterCoeff))
angleCoeff <- summarise(group_by(angledf, Buoy), Mean=median(Offset), Stdev=median(Stdev))

grid.build <- function(buoy1, buoy2, difar1, difar2, grid.width=8000, grid.steps=100) {
      endPoint <- destPoint(rev(buoy1), 0, d=(100*grid.width/2)) #geosphere uses long, lat
      step.size <- (endPoint[2] - buoy1[1])/grid.steps/100
      angle12 <- geosphere::bearing(rev(buoy1), rev(buoy2))
      # int.point <- intersectPoint(difar1, difar2, angle12, buoy1[1], buoy1[2], buoy2[1], buoy2[2])
      int.point <- list(lat=mean(c(buoy1[1], buoy2[1])), long=mean(c(buoy1[2], buoy2[2])))
      lats <- numeric(grid.steps*2 +1)
      longs <- numeric(grid.steps*2 +1)
      for(i in 1:length(lats)){
            lats[i] <- int.point$lat+(-grid.steps+i-1)*step.size
            longs[i] <- int.point$long+(-grid.steps+i-1)*step.size
      }
      #list(lats=lats, longs=longs)
      dflats <- unlist(lapply(lats, function(x) rep(x, length(lats))))
      dflongs <- rep(longs, length(longs))
      df <- data.frame(lats=dflats, longs=dflongs)
      df <- grid.positions(df, buoy1, buoy2)
      df <- grid.density(df, difar1, difar2)
      max <- df[df$density==max(df$density),]
      plot <- ggplot() + geom_point(data=df, aes(x=longs, y=lats, colour=density), size=3) + 
                  scale_colour_gradient2(low='lightblue', high='red', mid='lightblue', midpoint=max$density*.5) +
                  geom_label(aes(x=buoy1[2], y=buoy1[1], label='B1', fontface='bold'), colour='orange') +
                  geom_label(aes(x=buoy2[2], y=buoy2[1], label='B2', fontface='bold'), colour='darkgreen') +
                  # geom_point(aes(x=int.point$long, y=int.point$lat), colour='purple', size=2) +
                  geom_point(aes(x=max$long, y=max$lat), colour='green', size=5, shape=10) +
                  geom_contour(data=df, aes(x=longs, y=lats, z=density), breaks=max$density*c(.9, .95)) +
                  # geom_point(aes(x=lat1, y=long1), size=5,  colour='orange') + 
                  # geom_point(aes(x=lat2, y=long2), size=5, colour='darkgreen') +
                  #scale_color_manual(breaks=c('orange', 'darkgreen'), values=c('orange', 'darkgreen'), labels=c('Buoy1', 'Buoy2')) +
                  coord_fixed()
      list(df=df, plot=plot)
}
# df is lat, long. geosphere does long, lat
grid.positions <- function(df, buoy1, buoy2) {
      angle1 <- geosphere::bearing(rev(buoy1), as.matrix(rev(df)))
      angle2 <- geosphere::bearing(rev(buoy2), as.matrix(rev(df)))
      dist1 <- distGeo(rev(buoy1), as.matrix(rev(df)))
      dist2 <- distGeo(rev(buoy2), as.matrix(rev(df)))
      df$angle1 <- (angle1 %% 360); df$angle2 <- (angle2 %% 360)
      df$dist1 <- dist1; df$dist2 <- dist2
      df
}

grid.density <- function(df, difar1, difar2) {
      df$ang.dens1 <- sapply(df$angle1, function(x) max(dnorm(x, difar1, 20), dnorm(x-360, difar1, 20), dnorm(x+360, difar1, 20)))
      df$ang.dens2 <- sapply(df$angle2, function(x) max(dnorm(x, difar2, 20), dnorm(x-360, difar2,20), dnorm(x+360, difar2, 20)))
      logit.fit <- function(d) exp(3.37-.76*d)
      dist.dens <- function(e) {logit.fit(e)/(1+logit.fit(e))}
      dist.normalize <- log(1+exp(3.37))/(-1/-.76) # Go from probability to density by normalizing
      df$dist.dens1 <- sapply(df$dist1, function(x) dist.dens(x/1000)) / dist.normalize
      df$dist.dens2 <- sapply(df$dist2, function(x) dist.dens(x/1000)) / dist.normalize
      df$diff.dens <- 1
      # df$diff.dens <- dnorm((df$dist1-df$dist2)/1500, tdiff, .4)
      # df$diff.dens <- dunif((df$dist1-df$dist2)/1500, tdiff-.5, tdiff+.5)
      df$density <- df$ang.dens1 * df$ang.dens2 * df$dist.dens1 * df$dist.dens2 * df$diff.dens
      #df$density <- df$ang.dens1 * df$ang.dens2
      #df$density <- dnorm((df$dist1-df$dist2)/1500, tdiff, .4)
      #print((df$dist1-df$dist2)/1500)
      #print(tdiff)
      df
}

pairedDifar <- data.table(read.csv('./Data/DIFAR_paired_buoys_308.csv'))
intersonly <- filter(pairedDifar,!(is.na(UTC1)) & !(is.na(UTC2)))

usethis <- intersonly[987,]

# usethis <- filter(intersonly, PlaybackNumber=='25', !is.na(UTC1), !is.na(UTC2))
# usethis <- filter(usethis, distError > 10)[1,]
tdiff <- usethis$Time1 - usethis$Time2
# tdiff <- (usethis$Distance1 - usethis$Distance2)/1.5
realone <- with(usethis, grid.build(c(BuoyLat1, BuoyLong1), c(BuoyLat2, BuoyLong2), DifarAdj1,DifarAdj2))
realone$plot + geom_point(aes(x=usethis$BoatLong1, y=usethis$BoatLat1), color='yellow', size=2) + 
      geom_point(aes(x=usethis$IntLong, y=usethis$IntLat), color='purple', size=2)
# Expected dist error ### This doesn't work because the distance isn't a density,
# They are all probabilites. They don't sum to one (much higher)
# ^ Okay I maybe turned it into a density, but this all seems way too small. Sum(density) = .05
# Should be way higher than that? No. Sum isn't really the same as integral.

n <- realone$df
# n$density <- n$density / sum(n$density)
n$exp1 <- n$density * (n$dist1 - usethis$Distance1*1000)
n$exp2 <- n$density * (n$dist2 - usethis$Distance2*1000)
sum(n$exp1)
sum(n$exp2)

usethis$Time1 - usethis$Time2
(usethis$Distance1 - usethis$Distance2)/1.5

densityview <- function(x) {
      df <- intersonly[x,]
      dec <- with(df, grid.build(c(BuoyLat1, BuoyLong1), c(BuoyLat2, BuoyLong2), DifarAdj1, DifarAdj2))
      difar <- with(df, grid.build(c(BuoyLat1, BuoyLong1), c(BuoyLat2, BuoyLong2), DIFARBearing1, DIFARBearing2))
      decplot <- dec$plot + geom_point(aes(x=df$IntLong, y=df$IntLat), colour='purple', size=2)
      difarplot <- difar$plot + geom_point(aes(x=df$IntLong, y=df$IntLat), colour='purple', size=2)
      list(Dec=decplot, Difar=difarplot)
}

Rprof()
d <- densityview(543)
Rprof(NULL)
summaryRprof()
grid.arrange(d$Dec, d$Difar, ncol=2)

n <- realone$df
n[n$density==max(n$density),]
dens <- grid.build(c(32.81801, -117.5680), c(32.80052,-117.5686), 243+11.7, 276+11.7, grid.width=4000, grid.steps=70)
# head(d2)
dens$plot
n <- dens$df
head(n)
n[n$density==max(n$density),]

ggplot() + geom_point(data=d2, aes(x=longs, y=lats, colour=density), size=3) + 
      scale_colour_gradient(low='white', high='red') +
      geom_label(aes(x=long1, y=lat1, label='B1', fontface='bold'), colour='orange') +
      geom_label(aes(x=long2, y=lat2, label='B2', fontface='bold'), colour='darkgreen') +
      # geom_point(aes(x=lat1, y=long1), size=5,  colour='orange') + 
      # geom_point(aes(x=lat2, y=long2), size=5, colour='darkgreen') +
      #scale_color_manual(breaks=c('orange', 'darkgreen'), values=c('orange', 'darkgreen'), labels=c('Buoy1', 'Buoy2')) +
      coord_fixed()

ggplot() + geom_point(data=d2, aes(x=longs, y=lats, colour=density), size=3) + 
      scale_colour_gradient(low='white', high='red') +
      geom_label(aes(x=long1, y=lat1, label='B1', fontface='bold'), colour='orange') +
      geom_label(aes(x=long2, y=lat2, label='B2', fontface='bold'), colour='darkgreen') +
      #geom_density2d(data=d2, aes(x=longs, y=lats)) +
      geom_contour(data=d2, aes(x=longs, y=lats, z=density), breaks=c(0,.00005, .0001, .0004, .0008, .00012)) +
      # geom_point(aes(x=lat1, y=long1), size=5,  colour='orange') + 
      # geom_point(aes(x=lat2, y=long2), size=5, colour='darkgreen') +
      #scale_color_manual(breaks=c('orange', 'darkgreen'), values=c('orange', 'darkgreen'), labels=c('Buoy1', 'Buoy2')) +
      coord_fixed()

# Hyperbola line rotation

complex.mult <- function(ab, cd) {
      real <- ab[1]*cd[1] - ab[2]*cd[2]
      complex <- ab[2]*cd[1] + ab[1]*cd[2]
      c(real, complex)
}

slope <- complex.mult(c(4,1), c(-4,1))


## TEST THIS. Need to update pairedDifar with time information

## Doesnt work because time isn't accurate enough. Sometimes the time difference * sound speed
## is greater than the distance between the buoys, which is impossible. Next step: can get a range
## of possible angles for some uncertain amount of time. Broken. The fuck?
# Its not. Time was fucked. Rounded to nearest second, then added millis back. Re-do time, should be fine.
# Could get angles, then use geosphere functions to get actual lat/long points. Might probs work better?
# Could get the lat long, then use that to just get the slope I want to the midpoint. 
hyperbola.lines <- function(buoy1, buoy2, time1, time2, speed.sound = 1500) { #prob feed in distance, no need to calculate again
      distance <- distGeo(rev(buoy1), rev(buoy2))
      time.diff <- time1-time2
      # time.diff <- 1.6
      denom <- sqrt((distance/time.diff/speed.sound)^2 - 1)
      slope.asy <- (denom^-1)#*111/94 #Need to adjust units from meters -> lat/long. Approx conversion?
      slope.intermediate <- complex.mult(c(buoy2[2]-buoy1[2], buoy2[1]-buoy1[1]), c(1, slope.asy))
      print(slope.asy)
      slope.adjust <- slope.intermediate[2]/slope.intermediate[1]
      print(slope.adjust)
      mid <- (buoy1+buoy2)/2
      list(slope1=slope.adjust, slope2= -slope.adjust, intercept1=mid[1]-slope.adjust*mid[2], intercept2=mid[1]+slope.adjust*mid[2])
}
# pairedDifar <- pairedDifar %>% mutate(TimeDiff = abs(Time2 - Time1),
#                        MaxDiff = abs((Distance2 - Distance1) / 1.5))

# ggplot(data=pairedDifar) + geom_point(aes(x=MaxDiff, y=TimeDiff)) + geom_abline(intercept=0, slope=1)
pairedDifar <- data.table(pairedDifar)
htest <- pairedDifar[!(is.na(DIFARBearing1)) & !(is.na(DIFARBearing2)),][RealMidDist>1.5,][701,]
htest$RealMidDist
line <- hyperbola.lines(c(htest$BuoyLat1, htest$BuoyLong1), c(htest$BuoyLat2, htest$BuoyLong2), htest$Time1, htest$Time2)
line <- hyperbola.lines(c(htest$BuoyLat1, htest$BuoyLong1), c(htest$BuoyLat2, htest$BuoyLong2), htest$Distance1/1.5, htest$Distance2/1.5)


ggplot(data=htest) + geom_point(aes(x=BuoyLong1, y=BuoyLat1, colour='1')) + geom_point(aes(x=BuoyLong2, y=BuoyLat2, colour='2')) + 
      geom_abline(slope=line$slope1, intercept=line$intercept1) + geom_abline(slope=line$slope2, intercept=line$intercept2) +
      geom_point(aes(x=IntLong, y=IntLat, colour='Int'))



