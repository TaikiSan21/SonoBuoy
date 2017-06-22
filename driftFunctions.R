library(geosphere)
library(dplyr)
library(lubridate)
library(ggplot2)
library(swfscMisc)
# source('~/R Projects/SWFSC/PAMsbuoy/devel/drawBearing.R')
source('../PAMsbuoy/devel/drawBearing.R')

# Functions for drift nonsense so I can just source it
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

negLoglDelta <- function(boat, start, params) {
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

negLogl <- function(boat, start, drift) {
    expected <- expectedBearing(boat, start, drift[1], drift[2])
    error <- sapply((boat$DIFARBearing - expected) %% 360, function(x) {
        if(x < abs(x-360)) {x}
        else {x-360}
    }
    )
    (nrow(boat)/2)*log(2*pi*(4^2)) + (1/2/(4^2))*sum((error)^2)
}

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

gridOptim <- function(values=list(rate=seq(0,2, length.out=75), phi=seq(0,360,2)), fun, boat, start) {
    # df <- data.frame(rate = unlist(lapply(values$rate, function(x) rep(x, length(values$phi)))),
    #                  phi = rep(values$phi, length(values$rate)))
    # df$value <- mapply(fun, df$rate, df$phi, MoreArgs=list(boat=boat, start=start))
    # df
    do.call(rbind, lapply(values$rate, function(x) {
        value <- sapply(values$phi, function(y) {
            fun(boat, start, c(x, y))
        })
        data.frame(rate=x, phi=values$phi, value=value)
    }))
}