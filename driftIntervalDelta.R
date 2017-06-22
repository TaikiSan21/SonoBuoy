source('loadGpsDifar.R')
source('../PAMsbuoy/devel/drawBearing.R')
source('diagnosticGraphs.R')
library(rgdal)
library(RSQLite)
library(dplyr)
library(lubridate)
library(stringr)
library(data.table)
library(geosphere)
library(ggplot2)
library(plotly)
library(swfscMisc)
library(optimx)
library(webshot)
library(RSelenium)
library(viridisLite)

expectedBearingDeltaInterval <- function(boat, start, drift.rate, drift.phi, delta=list(rate=0, phi=0, UTC=Inf)) {
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

negloglDeltaInterval <- function(boat, start, time, params) {
    delta <- list(UTC=Inf)
    if(length(params) == 4) {
        delta <- list(rate=params[3], phi=params[4], UTC=start$UTC + time)
    }
    expected <- expectedBearingDeltaInterval(boat, start, params[1], params[2], delta)
    error <- sapply((boat$DIFARBearing - expected) %% 360, function(x) {
        if(x<abs(x-360)) {x}
        else {x-360}
    })
    sum((error)^2)
}

buoypath <- data.frame(
    turnlat=rep(0,10),
    turnlong=rep(0,10),
    endlat=rep(0,10),
    endlong=rep(0,10),
    time=(6:15)*100,
    score=rep(0,10))

i=1
for(t in seq(from=sum(times[1:2], 100), to=sum(times[1:4]), length.out=10)) {
    tmp <- optim(par=c(1,0,0,0), negloglDeltaInterval, boat=testdata2, start=start, time=t,
                 control=list(maxit=100000, parscale=c(1,1,1,1)), hessian=TRUE, method='L-BFGS-B',
                 lower=c(0.5, -180, -1, -180), upper=c(3, 180, 1,180))
    print(t)
    print(tmp$value)
    print(tmp$par)
    drifts <- tmp$par
    turn <- destPoint(c(start$Longitude, start$Latitude), drifts[2], drifts[1]*t/3.6)
    end <- destPoint(turn, drifts[2]+drifts[4], (drifts[1]+drifts[3])*(sum(times)-t)/3.6)
    buoypath$turnlat[i] <- turn[2]; buoypath$turnlong[i] <- turn[1]
    buoypath$endlat[i] <- end[2]; buoypath$endlong[i] <- end[1]
    buoypath$score[i] <- tmp$value
    i <- i+1
}
buoypath$score <- buoypath$score / min(buoypath$score)
p <- ggplot() + geom_point(data=testdata2,aes(x=Longitude, y=Latitude, fill='boat')) + 
    geom_point(data=testdata2,aes(x=buoylong, y=buoylat, fill='buoy'))
p +
    geom_segment(data=buoypath, aes(x=turnlong, y=turnlat, xend=endlong, yend=endlat, alpha=score^-1),color='darkgreen', size=2) + 
    geom_segment(data=buoypath, aes(x=start$Longitude, y=start$Latitude, xend=turnlong, yend=turnlat, alpha=score^-1),color='darkgreen', size=2) +
    coord_cartesian(xlim=c(-117.415, -117.395), ylim=c(32.639, 32.65)) +
    scale_alpha_continuous(range=c(.1,.6))

# Trying on first trial data. Using boatnoiseall
useme <- loadGpsDifar('./DIFAR Testing/BoatNoiseTest2.sqlite3',
                      './Data/spot_messages_RUST_JLK.csv') %>%
    mutate(UTC=ymd_hms(UTC)) %>% filter(Channel==0, Distance > 70) %>% arrange(UTC) %>% select(-Longitude, -Latitude) %>%
    rename(Longitude=BoatLong, Latitude=BoatLat) %>% mutate(DIFARBearing=DifarAdj)

start <- select(useme[1,], UTC, Longitude=BuoyLongitude, Latitude=BuoyLatitude)

# ggplot(data=slice(useme, 2:47)) + geom_point(aes(x=Longitude, y=Latitude, color=Distance))
testdrift <- optim(par=c(0,0), negLogl, boat=useme, start=start, control=list(maxit=10000))
# testdrift <- optimx(par=c(.5,0), neglogl, boat=slice(useme, 2:n()), start=start,
#       control=list(maxit=100000, parscale=c(1,1)), method=c('CG'), hessian=TRUE)# ,
      # lower=c(0, 0), upper=c(3,360))
testdrift
    
drifts <- testdrift$par
end <- destPoint(c(start$Longitude, start$Latitude), drifts[2], drifts[1]*(difftime(useme$UTC[nrow(useme)], start$UTC, units='secs'))/3.6)

a <- ggplot(data=useme) + geom_point(aes(x=Longitude, y=Latitude, color=as.numeric(UTC))) + 
     geom_point(aes(x=BuoyLongitude, y=BuoyLatitude), color='orange', size=4, alpha=.3) +
    geom_segment(aes(x=start$Longitude, y=start$Latitude, xend=end[1], yend=end[2]), size=2, color='darkgreen') 

drawBearings(useme %>% mutate(DIFARBearing=(DIFARBearing-180)%% 360), a, distance=.7,alpha=.3)

# Grid search
negLoglGrid <- function(drift.rate, drift.phi, boat, start) {
    expected <- expectedBearing(boat, start, drift.rate, drift.phi)
    error <- sapply((boat$DIFARBearing - expected) %% 360, function(x) {
        if(x < abs(x-360)) {x}
        else {x-360}
    }
    )
    sum((error)^2)
}


gridOptim <- function(values=list(rate=seq(0,3, length.out=100), phi=seq(1,360,1)), fun, boat, start) {
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
########################################################################
#### IMAGE TESTING ONE AT A TIME
###############################################################
# useme <- testdata2 %>% rename(BuoyLatitude=buoylat, BuoyLongitude=buoylong)
# Trying on first trial data. Using boatnoiseall
useme <- loadGpsDifar('./DIFAR Testing/BoatNoiseTest2.sqlite3',
                      './Data/spot_messages_RUST_JLK.csv') %>%
    mutate(UTC=ymd_hms(UTC)) %>% filter(Channel==3, Distance > 7) %>% arrange(UTC) %>% select(-Longitude, -Latitude) %>%
    rename(Longitude=BoatLong, Latitude=BoatLat) %>% mutate(DIFARBearing=RealBearing)

start <- select(useme[1,], UTC, Longitude=BuoyLongitude, Latitude=BuoyLatitude)

c <- 3
# filtStart <- filter(buoystart, Channel==c)
# filtBoat <- filter(difarApril3, Channel==c, UTC > filtStart$UTC, Distance>7) %>% 
#     mutate(DIFARBearing = RealBearing)
# useme <- filtBoat
# start <- filtStart
# Rprof(NULL)
# Rprof(tmp <- tempfile())
filter(useme, !(Id %in% c(512,513,514))) %>% diagnosticGraphs(start, outpath = './Drift Diagnostic/Simulated Error/',
                 contours=c(1.1,1.2,1.3,1.4), name=paste0('Ch', c,' RealFirst'))

d <- gridOptim(fun=negLogl,boat=useme, start=start, values=list(rate=seq(0,2,length.out=75), phi=seq(0,360,2)))
# Rprof()
# summaryRprof(tmp)
d <- d %>% mutate(like = exp(-value),
                  like = like/sum(like))

min <- min(d$value)

# ggplot(data=d, aes(x=value)) + geom_histogram(binwidth=50) + xlim(0,5e5)+ ylim(0, 150)
### GRID PLOT ###
ggplot(data=d, aes(x=phi, y=rate, color=log(value))) + geom_point(size=3, shape=15) + 
    scale_colour_gradientn(colours=viridis(256, option = "inferno", direction=-1), limits=c(5,10), oob=scales::squish) +
    geom_contour(aes( z=value), breaks=min*seq(.9,2,.2), alpha=.5)
# ggsave('~/R Projects/SWFSC/SonoBuoy/Drift Diagnostic/Ch0 Difar Far Grid.png')
### PATH PLOT ###
pars <- d[which.min(d$value),]
drifts <- c(pars$rate, pars$phi)
end <- destPoint(c(start$Longitude, start$Latitude), drifts[2], drifts[1]*(difftime(useme$UTC[nrow(useme)], start$UTC, units='secs'))/3.6)

a <- ggplot(data=useme) + geom_point(aes(x=Longitude, y=Latitude, color=as.numeric(UTC))) + 
    geom_point(aes(x=BuoyLongitude, y=BuoyLatitude), color='orange', size=4, alpha=.3) +
    geom_segment(aes(x=start$Longitude, y=start$Latitude, xend=end[1], yend=end[2]), size=2, color='darkgreen') 

drawBearings(useme %>% mutate(DIFARBearing=(DIFARBearing-180)%% 360), a, distance=.3,alpha=.1)
ggsave('./Drift Diagnostic/Ch0 Difar Far Path.png')
### 3D PLOT ###
mat <- t(matrix(d$value, 181, 75))

# This works at home, tnsakai google login
plotly_IMAGE(plot_ly(z=-log(mat)) %>% add_surface(), format='png', out_file = 'test.png')

#######################################################
#### TESTING GRADIENT STUFF MAYBE NOT USEFUL ##########
grad <- gradient(-log(mat))
gx <- grad$X
gy <- grad$Y
dim(gx) <- NULL; dim(gy) <- NULL
gradx <- data.frame(value=gx, rate= rep(seq(0, 3, length.out=100), 360), phi=unlist(lapply(1:360, function(x) rep(x, 100))))
grady <- data.frame(value=gy, rate= rep(seq(0, 3, length.out=100), 360), phi=unlist(lapply(1:360, function(x) rep(x, 100))))
ggplot(data=gradx, aes(x=phi, y=rate, color=value)) + geom_point(size=4) + 
    scale_colour_gradient2(low='blue', mid ='white', high='red', midpoint=0)
ggplot(data=grady, aes(x=phi, y=rate, color=value)) + geom_point(size=4) + 
    scale_colour_gradient2(low='blue', mid ='white', high='red', midpoint=0)
############################################
#### LOOP THROUGH DIFFERENT ERROR TYPES ####

# Either x + err or Asin(x+B) + C
errFunction <- function(pars) {
    if(length(pars)==1) {
        function(x) x + pars
    }
    else if(length(pars)==3) {
        function(x) x + pars[1]*sin((x+pars[2])*pi/180)+pars[3]
    }
}

errFuncs <- list(0, 20,
                 c(20, -11.7, -10))

c <- 3
useme <- loadGpsDifar('./DIFAR Testing/BoatNoiseTest2.sqlite3',
                      './Data/spot_messages_RUST_JLK.csv') %>%
    mutate(UTC=ymd_hms(UTC)) %>% filter(Channel==c) %>% arrange(UTC) %>% select(-Longitude, -Latitude) %>%
    rename(Longitude=BoatLong, Latitude=BoatLat)
start <- select(useme[1,], UTC, Longitude=BuoyLongitude, Latitude=BuoyLatitude)

i <- 1
surfplots <- vector('list', length=length(errFuncs))
for(e in errFuncs) {
    tmpdf <- useme %>% mutate(DIFARBearing=errFunction(e)(RealBearing) + 30*exp(-Distance/300))
    surfplots[[i]] <- diagnosticGraphs(tmpdf, start, outpath = './Drift Diagnostic/Simulated Error/',
                                     contours=c(1.1,1.2,1.3,1.4), name=paste0('Ch', c, ' Function ', paste0(e,collapse='_')))
    i <- i+1
}

x <- 1:1000
y <- 30*exp(-x/280)
qplot(x=x, y=y) + geom_hline(yintercept=c(10,7,5))