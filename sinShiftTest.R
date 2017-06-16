# Multi path phase shift
library(ggplot2)
library(manipulate)
# sin(freq*t)
freq <- 1000
time <- seq(0, 6*pi/freq, length.out=2000)

siny <- sin(freq*time)

sindf <- data.frame(time=time, siny = mysins(0))
# sin(freq*t + shift)
mysins <- function(shift) {
      mysin <- rep(0,2000)
      for(f in c(1000)) {
            mysin <- mysin + sin(f*(time + shift))
      }
      mysin
}


manipulate({ggplot(data=sindf, aes_string(x='time', y = 'siny + mysins(shiftpick)')) + geom_point() + ylim(-8,8)},
       shiftpick=slider(0,2, step=.05))

ggplot(data=sindf, aes_string(x='time', y = 'siny ')) + geom_point()