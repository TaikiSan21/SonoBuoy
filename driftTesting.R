library(geosphere)
library(dplyr)
library(lubridate)
library(ggplot2)
library(swfscMisc)
library(plotly)
library(viridisLite)
library(R.utils)
library(tidyr)
# source('~/R Projects/SWFSC/PAMsbuoy/devel/drawBearing.R')
source('../PAMsbuoy/devel/drawBearing.R')
sourceDirectory('../PAMsbuoy/R/')
source('loadGpsDifar.R')
source('driftFunctions.R')

# Test using package
pamDb <- loadDB('../PAMsbuoy/devel/final db formatting/FinalFormat_Station1.sqlite3')
save(pamDb, file='pamDb.Rdata')
pamDifar <- formatStation(pamDb)

# Might need to do drit in the calculateOffset function. This is where we get true.bearing, so it has the 
# lat long from gps. Or we just have that keep the lat long so we can use it later.
# The shit I want is in buoy$number$calibration
buoyToDo <- pamDifar$buoys$`0`
calculateDrift(start=buoyToDo$position, difar=buoyToDo$calibration)
# drift testing area

difar <- loadGpsDifar('./Data/PAST_20160607_POST_PB_Edited.sqlite3', buoyfunc=firstTrialId, buoylocs = './Data/spot_messages_RUST_JLK.csv')
cals <- loadGpsDifar('./Data/PAST_20160607_POST_VesselCalOnly.sqlite3', buoyfunc=firstTrialId, buoylocs= './Data/spot_messages_RUST_JLK.csv')
difar <- rbind(cals, difar)
start <- do.call(rbind, by(difar, difar$Channel, function(x) {
      arrange(x, UTC) %>% head(1)
})) %>% select(Latitude=BuoyLatitude, Longitude=BuoyLongitude, UTC, DIFARBearing, Channel)

calculateDrift <- function(start, difar, niter = 10000, initial = c(1, 0), graph=NULL) {
      if(is.null(graph)) {
            drift <- optim(par=initial, neglogl, boat=difar, start=start, hessian=TRUE,
                           method='L-BFGS-B', lower=c(-3, -360), upper=c(3, 360))
      }
      drift
}

driftGrid <- function(values=list(rate=seq(0,2, length.out=75), phi=seq(0,360,2)), fun, boat, start) {
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

Chan <- 3
filtStart <- filter(start, Channel==Chan)
filtDifar <- filter(difar, Channel==Chan)
filtDifar <- filter(difar, Channel==Chan, Species=='Vessel') %>% mutate(DIFARBearing = RealBearing)# %>% head(12)

grid <- driftGrid(fun=negLogl, boat=filtDifar, start=filtStart)
ggplot(grid, aes(x=phi, y=rate, color=value)) + geom_point() + scale_color_gradientn(colors=viridis(256))

mat <- t(matrix(grid$value, length(unique(grid$phi)), length(unique(grid$rate))))
plot_ly(z=-log(mat)) %>% add_surface()

drift <- calculateDrift(filtStart, 
                        filtDifar %>% filter(Species=='Vessel'))
sqrt(solve(drift$hessian))

endPoint <- destPoint(c(filtStart$Longitude, filtStart$Latitude), drift$par[2], 
                      drift$par[1] * difftime(slice(filter(difar, Channel==Chan), 20:22)$UTC, filtStart$UTC, units='secs') * 1000 / 3600)
                      # drift$par[1] * difftime(slice(filter(difar, Channel==Chan), n())$UTC, filtStart$UTC, units='secs') * 1000 / 3600)

difar %>% ggplot() + geom_point(aes(x=Longitude, y=Latitude, color='Boat')) + 
      geom_point(aes(x=BuoyLongitude, y=BuoyLatitude, color=as.factor(Channel))) +
      geom_segment(aes(x=filtStart$Longitude, y=filtStart$Latitude, xend=endPoint[1], yend=endPoint[2]))
