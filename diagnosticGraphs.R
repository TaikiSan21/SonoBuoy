source('loadGpsDifar.R')
source('../PAMsbuoy/devel/drawBearing.R')
source('driftFunctions.R')
library(dplyr)
library(ggplot2)
library(plotly)
library(geosphere)
library(viridisLite)
library(cowplot)
# Making my diagnostic likelihood graphs

diagnosticGraphs <- function(boat, start, fun=neglogl, contours=c(1.1, 1.3), outpath='./', distance=700, name, ...) {
    if(sum(colnames(boat) %in% c('UTC', 'Latitude', 'Longitude', 'DIFARBearing', 'Distance')) != 5) {
        stop('Column names UTC, Latitude, Longitude, DIFARBearing, Distance needed in boat data.')
    }
    if(sum(names(start) %in% c('UTC', 'Longitude', 'Latitude')) != 3) {
        stop('Names UTC, Longitude, Latitude needed in buoy data.')
    }
    boat <- arrange(boat, UTC)
    plots <- list(surf1 = 'tmp', surf2 = 'tmp')
    for(i in 1:2) {
        grid <- gridOptim(fun=fun, boat=boat, start=start, ...) %>% 
            mutate(like = exp(-value),
                   like = like/sum(like))
        min <- min(grid$value)
        g <- ggplot(data=grid, aes(x=phi, y=rate, color=log(value))) + geom_point(size=3, shape=15) +
            scale_colour_gradientn(colours=viridis(256, option='inferno', direction=-1), limits=c(5,10), oob=scales::squish) +
            geom_contour(data=grid, aes(z=value), breaks=min*contours, alpha=.5) +
            labs(title=name)
        # ggsave(filename=paste0(name, ' Grid.png'),
        #         plot=g, path=outpath)
        
        drift <- grid[which.min(grid$value), c('rate', 'phi')]
        end <- destPoint(c(start$Longitude, start$Latitude), drift$phi, 
                         drift$rate*difftime(boat$UTC[nrow(boat)], start$UTC, units='secs')/3.6)
        p <- ggplot(boat) + geom_point(aes(x=Longitude, y=Latitude, color=as.numeric(UTC))) +
            geom_segment(x=start$Longitude, y=start$Latitude, xend=end[1], yend=end[2], color='darkgreen', size=2)
        p <- drawBearings(boat %>% mutate(DIFARBearing= (DIFARBearing-180) %% 360), p, distance=.4, alpha=.1)
        if(TRUE) {
            p <- p + geom_line(data=boat, aes(x=BuoyLongitude, y=BuoyLatitude), size=1, color='orange', alpha=.5)
        }
        # ggsave(filename=paste0(name, ' Path.png'), plot=p, path=outpath)
        ggsave(filename=paste0(name, '.png'), plot=plot_grid(g, p, align='h'), path=outpath, width=14, height=7, units='in')
        mat <- t(matrix(grid$value, length(unique(grid$phi)), length(unique(grid$rate))))
        plots[[i]] <- plot_ly(z=-log(mat)) %>% add_surface()
        print(paste0('Call me ', name, ' 3D.png'))
        
        boat <- filter(boat, Distance > distance)
        name <- paste0(' Far ', name)
        
    }
    plots
}


