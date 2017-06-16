# Drift Update
# Assuming we know first rate, direction, start time, last time

# last is destination from start to last time 
expectedBearingUpdate <- function(boat, last, drift, delta=list(rate=0, phi=0, time=Inf)) {
      turn.distance <- drift$rate*difftime(delta$UTC, last$UTC, units='secs')/3600
      turn.loc <- t(mapply(destination, last$Latitude, last$Longitude, drift$phi, turn.distance, MoreArgs = list(units='km')))
      drift.distance <- (drift$rate + delta$rate) * difftime(boat$UTC, delta$UTC, units='secs')/3600
      buoy.loc <- t(mapply(destination, turn.loc[,1], turn.loc[,2], drift$phi + delta$phi, drift.distance, MoreArgs = list(units='km')))
      mapply(bearing, buoy.loc[,1], buoy.loc[,2], boat$Latitude, boat$Longitude)[1,]
}

expectedBearingDelta <- function(boat, start, drift.rate, drift.phi, delta=list(rate=0, phi=0, time=Inf)) {
      id <- which(boat$time < delta$time)
      bearings <- rep(0, nrow(boat))
      bearings[id] <- {
            drift.distance <- drift.rate*difftime(boat[id,]$time, start$time, units='secs')*1000/3600 # need m/s
            buoyLoc <- destPoint(c(start$long, start$lat), drift.phi, drift.distance)
            geosphere::bearing(buoyLoc, cbind(boat[id,]$long, boat[id,]$lat))
      }
      if(delta$time < Inf) {
            bearings[-id] <- {
                  first.distance <- drift.rate*difftime(delta$time, start$time, units='secs')*1000/3600
                  firstLoc <- destPoint(c(start$long, start$lat), drift.phi, first.distance)
                  second.distance <- (drift.rate+delta$rate)*(difftime(boat[-id,]$time, delta$time, units='secs')*1000/3600)
                  buoyLoc <- destPoint(firstLoc, drift.phi+delta$phi, second.distance)
                  geosphere::bearing(buoyLoc, cbind(boat[-id,]$long, boat[-id,]$lat))
            }
      }
      bearings
}

expectedBearingUpdate(leg2, last, drift, list(rate=.5, phi=100, UTC=last$UTC + 600))
leg2$DIFARBearing

negloglUpdate <- function(boat, last, drift, params) {
    #print(params[3])
      delta <- list(rate=params[1], phi=params[2], UTC=last$UTC + params[3])
      expected <- expectedBearingUpdate(boat, last, drift, delta)
      error <- sapply((boat$DIFARBearing - expected) %% 360, function(x) {
            if(x<abs(x-360)) {x}
            else {x-360}
      })
      sum((error)^2)
}

negloglUpdate(leg2, last, drift, c(-.5, 61, 0))

drift <- list(rate=try$par[1], phi=try$par[2])
leg1 <- testdata2[1:20,]
leg2 <- testdata2[21:40,]
last <- leg1[20,]
lastloc <- destination(start$Latitude, start$Longitude, drift$phi, drift$rate * difftime(leg1$UTC[20], start$UTC, units='secs') / 3600, units='km')
last$Latitude <- lastloc[1]
last$Longitude <- lastloc[2]


try2 <- optim(par=c(0,0,difftime(leg2$UTC[1], leg1$UTC[20], units='secs')/2), negloglUpdate, boat=leg2, last=last, drift=drift,
             control=list(maxit=10000, parscale=c(1,1,5)), hessian=TRUE, method='L-BFGS-B',
             lower=c(-3, -180, 0), upper=c(3,180,Inf))
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
try$par; try2$par

      
