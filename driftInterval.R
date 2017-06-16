# Fix time intervals

expectedBearingUpdate <- function(boat, last, drift, delta=list(rate=0, phi=0, UTC=Inf)) {
    turn.distance <- drift$rate*difftime(delta$UTC, last$UTC, units='secs')/3600
    turn.loc <- t(mapply(destination, last$Latitude, last$Longitude, drift$phi, turn.distance, MoreArgs = list(units='km')))
    drift.distance <- (drift$rate + delta$rate) * difftime(boat$UTC, delta$UTC, units='secs')/3600
    buoy.loc <- t(mapply(destination, turn.loc[,1], turn.loc[,2], drift$phi + delta$phi, drift.distance, MoreArgs = list(units='km')))
    mapply(bearing, buoy.loc[,1], buoy.loc[,2], boat$Latitude, boat$Longitude)[1,]
}

negloglUpdateInterval <- function(boat, last, drift, time, params) {
    #print(params[3])
    delta <- list(rate=params[1], phi=params[2], UTC=last$UTC + time)
    expected <- expectedBearingUpdate(boat, last, drift, delta)
    error <- sapply((boat$DIFARBearing - expected) %% 360, function(x) {
        if(x<abs(x-360)) {x}
        else {x-360}
    })
    sum((error)^2)
}

drift <- list(rate=try$par[1], phi=try$par[2])
leg1 <- testdata2[1:20,]
leg2 <- testdata2[21:40,]
last <- leg1[20,]
lastloc <- destination(start$Latitude, start$Longitude, drift$phi, drift$rate * difftime(leg1$UTC[20], start$UTC, units='secs') / 3600, units='km')
last$Latitude <- lastloc[1]
last$Longitude <- lastloc[2]

for(t in c(100,200,300,400,500,600,700,800,900,1000)) {
    tmp <- optim(par=c(0,0), negloglUpdateInterval, boat=leg2, last=last, drift=drift, time=t,
              control=list(maxit=10000, parscale=c(1,5)), hessian=TRUE, method='L-BFGS-B',
              lower=c(-1, -180), upper=c(1,180))
    print(t)
    print(tmp$value)
    print(tmp$par)
}

try2$par
updates <- try2$par
uturn <- destination(start$Latitude, start$Longitude, drift$phi, 
                     drift$rate * difftime(leg1$UTC[nrow(leg1)]+updates[3], start$UTC, units='secs') / 3600, units='km')
uend <- destination(uturn[1], uturn[2], drift$phi + updates[2], 
                    (drift$rate + updates[1]) * difftime(leg2$UTC[nrow(leg2)], leg1$UTC[nrow(leg1)]+updates[3], units='secs') / 3600, units='km')

p <- ggplot() + geom_point(data=leg1, aes(x=Longitude, y=Latitude, color='Boat')) +
    geom_point(data=leg1, aes(x=buoylong, y=buoylat, color='Buoy')) +
    geom_point(data=leg2, aes(x=Longitude, y=Latitude, color='Boat')) +
    geom_point(data=leg2, aes(x=buoylong, y=buoylat, color='Buoy')) +
    geom_segment(aes(x=start$Longitude, y=start$Latitude, xend=uturn[2], yend=uturn[1]), size=2) +
    geom_segment(aes(x=uturn[2], y=uturn[1], xend=uend[2], yend=uend[1]), size=2) +
    coord_cartesian(xlim=c(-117.415, -117.395), ylim=c(32.639, 32.65))
drawBearings(mutate(testdata2, DIFARBearing=DIFARBearing-180), p, distance=2, alpha=.6)