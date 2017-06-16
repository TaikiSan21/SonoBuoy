library(geosphere)
library(dplyr)
library(lubridate)
library(ggplot2)
library(swfscMisc)
# source('~/R Projects/SWFSC/PAMsbuoy/devel/drawBearing.R')
source('../PAMsbuoy/devel/drawBearing.R')
# Sonobuoy drift adapted from MATLAB code
# We are already matching difar times to boat GPS in difar loading
# We just need to feed in df with boat data, and grab buoy deployment info
# Eric should be organizing buoy info stuff on his end, can just leave as
# start for now. We'll apply this function to all buoys using Vessel data
# (or whatever sounds we end up using to calibrate shit
# I'll assume that we will be making a 'calibration df' so that
# thi boat input will only be a df with appropriate data for this

# boat is Df with lat, long, time of signals we are using to fit
# start is initial lat, long, time of buoy deployment. This will be from
# 'deploy' in pamguard, this will need to be standardized.
# drift.rate in km/hr, drift.phi is direction

expectedBearing <- function(boat, start, drift.rate, drift.phi) {
    drift.distance <- drift.rate*difftime(boat$UTC, start$UTC, units='secs')*1000/3600 # need m/s
    buoyLoc <- destPoint(c(start$Longitude, start$Latitude), drift.phi, drift.distance)
    geosphere::bearing(buoyLoc, cbind(boat$Longitude, boat$Latitude))
}

expectedBearingDelta <- function(boat, start, drift.rate, drift.phi, delta=list(rate=0, phi=0, UTC=Inf)) {
    id <- which(boat$UTC < delta$UTC)
    bearings <- rep(0, nrow(boat))
    bearings[id] <- {
        drift.distance <- drift.rate*difftime(boat[id,]$UTC, start$UTC, units='secs')*1000/3600 # need m/s
        buoyLoc <- destPoint(c(start$Longitude, start$Latitude), drift.phi, drift.distance)
        geosphere::bearing(buoyLoc, cbind(boat[id,]$Longitude, boat[id,]$Latitude))
    }
    if(delta$UTC < Inf) {
        bearings[-id] <- {
            first.distance <- drift.rate*difftime(delta$UTC, start$UTC, units='secs')*1000/3600
            firstLoc <- destPoint(c(start$Longitude, start$Latitude), drift.phi, first.distance)
            second.distance <- (drift.rate+delta$rate)*(difftime(boat[-id,]$UTC, delta$UTC, units='secs')*1000/3600)
            buoyLoc <- destPoint(firstLoc, drift.phi+delta$phi, second.distance)
            geosphere::bearing(buoyLoc, cbind(boat[-id,]$Longitude, boat[-id,]$Latitude))
        }
    }
    bearings
}

negloglDelta <- function(boat, start, params) {
    delta <- list(UTC=Inf)
    if(length(params) == 5) {
        delta <- list(rate=params[3], phi=params[4], UTC=start$UTC + params[5])
    }
    expected <- expectedBearingDelta(boat, start, params[1], params[2], delta)
    error <- sapply((boat$DIFARBearing - expected) %% 360, function(x) {
        if(x<abs(x-360)) {x}
        else {x-360}
    })
    sum((error)^2)
}

expectedBearingDelta(boat, start, drift.rate=(22.7)*3600/1000, drift.phi=0, delta=list(rate=1, phi=30, time=ymd_hms('2017-06-07 08:21:40')))
expectedBearing(boat, start, drift.rate=(22.7)*3.6, drift.phi=0)
# drift needs to be vector for optim, 
# [1] is rate in km/h, [2] is direction
# This is the negative log likelihood for normals with equal variance
neglogl <- function(boat, start, drift) {
    expected <- expectedBearing(boat, start, drift[1], drift[2])
    error <- sapply((boat$DIFARBearing - expected) %% 360, function(x) {
        if(x < abs(x-360)) {x}
        else {x-360}
    }
    )
    (nrow(boat)/2)*log(2*pi*(4^2)) + (1/2/(4^2))*sum((error)^2)
}

# fits, par is initials. 
ans <- optim(par=c(60, 10), negloglDelta, boat=copy(boat,2), start=start, hessian=TRUE)
sqrt(solve(ans$hessian)) # approx SE of estimates

drift.rate=2; drift.phi=90

copy <- function(df, n) {
    if(n==1) {df}
    else{
    copy(rbind(df,df), n-1)
    }
}
boat <- data.frame(lat=c(32.65, 32.66), long=c(-117.41, -117.42),                   
                   time=ymd_hms(c('2017/6/7 8:21:35', '2017/6/7 8:21:59')),
                   difar=c(69.87, 96.7)) # approx what difar would be @ 0 deg 22.7 m/s

start <- list(lat=32.64, long=-117.4, time=ymd_hms('2017/6/7 8:20:31'))
# Making data 300s after this start time
fakeData <- function(start=list(Latitude=32.64, Longitude=-117.4, UTC=ymd_hms('2017/6/7 8:20:31')),
                     times=c(300, 150, 150, 150, 150), rate1=2.5, phi1=0, 
                     delta=list(rate=-.3, phi=-90, brate=0, bphi=0), n=20, sd=5, bias=0,
                     boat=list(Longitude=-117.405, Latitude=32.64, rate=7, phi=50)) {
    first <- destPoint(c(start$Longitude, start$Latitude),phi1, rate1/3.6*times[1])
    second <- destPoint(first, phi1, rate1/3.6*times[2])
    boat2 <- destPoint(c(boat$Longitude, boat$Latitude), boat$phi, boat$rate/3.6*times[2])
    third <- destPoint(second, phi1, rate1/3.6*times[3])
    boat3 <- destPoint(boat2, boat$phi, boat$rate/3.*times[3])
    fourth <- destPoint(third, phi1+delta$phi, (rate1+delta$rate)/3.6*times[4])
    boat4 <- destPoint(boat3, boat$phi+delta$bphi, (boat$rate+delta$brate)/3.6*(times[3]+times[4]))
    fifth <- destPoint(fourth, phi1+delta$phi, (rate1+delta$rate)/3.6*times[5])
    boat5 <- destPoint(boat4, boat$phi+delta$bphi, (boat$rate+delta$brate)/3.6*times[5])
    leg1 <- data.frame(long=seq(first[1], second[1], length.out=n),
                       lat=seq(first[2], second[2], length.out=n),
                       boatlong=seq(boat$Longitude, boat2[1], length.out=n),
                       boatlat=seq(boat$Latitude, boat2[2], length.out=n),
                       seconds=seq(times[1], times[1]+times[2], length.out=n))
    leg1$time <- leg1$seconds+start$UTC
    leg2 <- data.frame(long=seq(fourth[1], fifth[1], length.out=n),
                       lat=seq(fourth[2], fifth[2], length.out=n),
                       boatlong=seq(boat4[1], boat5[1], length.out=n),
                       boatlat=seq(boat4[2], boat5[2], length.out=n),
                       seconds=seq(sum(times[1:4]), sum(times[1:5]), length.out=n))
    leg2$time <- leg2$seconds+start$UTC
    rbind(leg1, leg2) %>% rename(buoylong=long, buoylat=lat,
                                 Longitude=boatlong, Latitude=boatlat, UTC=time) %>%
        mutate(real=geosphere::bearing(cbind(buoylong, buoylat), cbind(Longitude, Latitude)),
               DIFARBearing=real+rnorm(2*n,bias,sd))
}
# Testing multiple leg nonsense. It fits...sometimes?
start=list(Latitude=32.64, Longitude=-117.4, UTC=ymd_hms('2017/6/7 8:20:31'))
times=c(400, 300, 400, 400, 300); rate1=1.5; phi1=-40
delta=list(rate=0, phi=0, brate=-4, bphi=-140); n=20; sd=2; bias=10
boat=list(Longitude=-117.405, Latitude=32.64, rate=7, phi=30)

testdata2 <- fakeData(start=start, times=times, rate1=rate1, phi1=phi1,
                     delta=delta, n=n, sd=sd, bias=bias, boat=boat)

try <- optim(par=c(1,0,0,0,(sum(times[1:2])+sum(times[1:4]))/2), negloglDelta, boat=testdata2, start=start, 
             control=list(maxit=10000, parscale=c(1,1,1,1,1)), hessian=TRUE, method='L-BFGS-B',
             lower=c(0,-180,-3, -180, sum(times[1:2])), upper=c(3,180,3,180,Inf))
try$par
# sqrt(diag(solve(try$hessian)))
g <- ggplot(data=testdata2) + geom_point(aes(x=Longitude, y=Latitude, color='boat')) + geom_point(aes(x=buoylong, y=buoylat, color='buoy'))
# g
drifts <- try$par
turn <- destPoint(c(start$Longitude, start$Latitude), drifts[2], drifts[1]*drifts[5]/3.6)
end <- destPoint(turn, drifts[2]+drifts[4], (drifts[1]+drifts[3])*(sum(times)-drifts[5]))
drawBearings(select(testdata2, buoylong, buoylat, DIFARBearing) %>%
                   rename(Longitude=buoylong, Latitude=buoylat), g, distance=1, alpha=.2) + 
      geom_segment(aes(x=turn[1], y=turn[2], xend=end[1], yend=end[2]), size=2) + 
      geom_segment(aes(x=start$Longitude, y=start$Latitude, xend=turn[1], yend=turn[2]), size=2) +
      coord_cartesian(xlim=c(-117.415, -117.395), ylim=c(32.639, 32.65))

g 



expectedBearingDelta(alls, start2, drift.rate=(185/50)*3.6, drift.phi=0)


expectedBearingDelta(boat, start, drift.rate=(22.7)*3600/1000, drift.phi=0)

ggplot() + geom_point(data=boat, aes(x=long, y=lat, color=as.factor(time))) + 
    geom_point(aes(x=start$long, y=start$lat, color=as.factor(start$time)))


