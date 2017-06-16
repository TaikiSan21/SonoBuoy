difference <- 'DIFARBearing'
summary(finalDifar2[[difference]] - singleDifar[[difference]])
sum(finalDifar2[[difference]] != singleDifar[[difference]], na.rm=TRUE)
which(finalDifar2[[difference]] != singleDifar[[difference]])

finalDifar2$Species <- as.factor(finalDifar2$Species)
singleDifar$Species <- as.factor(singleDifar$Species)
merged <- merge(finalDifar2, singleDifar, by=c('PlaybackNumber', 'Species', 'Buoy'))

ggplot(data=filter(merged, is.na(DIFARBearing.y)), aes(color=PlaybackNumber)) + 
      geom_point(aes(x=BoatLong.x, y=BoatLat.x), shape=1, size=3) + 
      geom_point(aes(x=BoatLong.y, y=BoatLat.y), shape=3, size=3)

summary(with(filter(merged, !is.na(DIFARBearing.y)), Distance.x - Distance.y))

ggplot(data=merged, aes(x=BoatLong.y, y=BoatLat.y, color=Detected.x)) + geom_point()