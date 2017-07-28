# Try modeling

modelData <- singleDifar %>% filter(Buoy=='NW', Distance > .7, Detected==TRUE, Intensity != 1, !(CallType %in% c('upY')))

modelData %>% group_by(PlaybackNumber) %>% 
    mutate(range=max(DIFARBearing)-min(DIFARBearing)) %>% select(PlaybackNumber, range) %>% unique %>% data.frame

modelData <- difarApril3 %>% filter(Channel==1, Distance > 750)
model <- nls(AngleError ~ a * sin((2*pi/360)*(RealBearing +b)) + c, data=modelData,
             start=list(a=-10, b=-11.7, c=0))

summary(model)

ggplot() + geom_point(data=modelData, aes(x=RealBearing, y=AngleError, color='Data')) + 
    geom_line(aes(x=seq(0,360,length.out=100), 
                  y=predict(model, newdata=list(RealBearing=seq(0,360,length.out=100))), color='Model')) + 
    xlim(0,360) + ylim(-30,30)

# New gain idea testing ?

theta <- seq(0, 360, length.out=150)
x  <-  cos(theta*pi/180) -.2
y <- sin(theta*pi/180)

hat <- function(theta, xg=0, yg=0, dec=0, amp=1) {
    sapply(theta-dec, function(t) {
        x <- amp*cos(t*pi/180) + xg
        y <- amp*sin(t*pi/180) + yg
        angle <- atan(y/x)*180/pi
        if(x >= 0) angle %% 360
        else if(x < 0) (angle+180) %% 360 
    })
}
###
db <- 10
angle <- 270 * pi / 180
xg <- cos(angle) / (1 - 10^(db/20))
yg <- sin(angle) / (1 - 10^(db/20))
hats <- hat(theta=theta, xg= xg, yg=yg, dec=0, amp=6)

error <- sapply((theta-hats) %% 360, function(x) {
      if(x <= 180) x
      else if(x >= 180) x-360
})

ggplot() + geom_line(aes(x=theta, y=error, color='Wrong')) + ylim(-30,30) +
      geom_vline(xintercept = angle * 180 / pi, size=2, alpha=.3) + geom_hline(yintercept=0, size=2, alpha=.3)
max(error)


# Lets draw some fuckin sins, does solving for R actually do anything???
# GIVEN this received Difar with this noise, whats the actual R.
# If D and N are farther apart, a low SNR should not be possible. 
# They would have to be close.

N <- 100 
D <- 80
SNR <- 8
Delta <- 1 / (10^(SNR/20) - 1)
theta <- seq(0,360, length.out=100)
Ns <- -1 * Delta * sin((N * pi / 180) - (D * pi / 180))
Rs <- sapply(theta * pi / 180,function(x) sin(x - (D * pi / 180)))

ggplot(data.frame(theta=theta, Rs=Rs), aes(x=theta, y=Rs)) + geom_line() + geom_hline(yintercept=Ns, color='orange') +
      geom_vline(xintercept=N, color='red') + geom_vline(xintercept=D, color='darkgreen')
