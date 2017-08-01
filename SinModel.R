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
xg <- -.2
yg <- 0
hats <- hat(theta=theta, xg= xg, yg=yg, dec=0, amp=1)

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
noisebig %>% filter(grepl('5', Species), grepl('tone', Species), PlaybackNumber==6) %>% 
      select(RealBearing, DIFARBearing, NoiseBearing, SNR, AdjError, Species) %>%
      slice(30:40)

ntest %>% filter(Station==1) %>%
      select(RealBearing, DIFARBearing, NoiseBearing, SNR, AdjError, Species) %>%
      slice(41:60)

# This model may not work. It is an exact situation, assuming nothing else happens.
N <- 3 + 11.7
D <- 227 + 11.7
SNR <-9
Delta <- 1 / (10^(SNR/20) - 1)
theta <- seq(0,360, length.out=100)
Ns <- -1 * Delta * sin((N * pi / 180) - (D * pi / 180))
Rs <- sapply(theta * pi / 180,function(x) sin(x - (D * pi / 180)))

g <- ggplot(data.frame(theta=theta, Rs=Rs), aes(x=theta, y=Rs)) + geom_line() + geom_hline(yintercept=Ns, color='orange') +
      geom_vline(xintercept=N, color='red') + geom_vline(xintercept=D, color='darkgreen')
ggplotly(g)

# Real model where we can't just add that shit together.
# Accounts for the fact that we are calculating the SNR from our data by subtracting noise SA from
# total SA. The total SA isn't simply signal + noise because they come from different directions.
# Ends up being a clusterfuck. The individual legs are added together (directional), but the reported
# SA is (I assume) from the omni. This seems to be better in that it has a different effect at different 
# angles which is the kind of behavior we observe (and would be expected when adding noise - if we add
# noise from the same direction as the signal it should have no effect on our received signal no matter
# what the SNR value is. 
####
# It appears that this is not really what's happening, or at least this theoretical
# effect is way stronger than what appears in practice. Possible cause - model is wrong, that isn't how 
# DIFAR is being calculated. Way of measuring SA in Pamguard isn't really matching up to what it should
# be in dB. 
delta <- function(SNR, diff) {
      # N <- N * pi / 180; R <- R * pi / 180
      diff <- diff * pi / 180
      B <- 10^(SNR/20)
      sqrt(B^2 - sin(diff)^2) - cos(diff)
}

manipulate({ggplot(data=data.frame(diff=seq(0,360,length.out=300), 
                                   deltaRight=sapply(seq(0,360, length.out=300), function(x) 1/delta(SNR, x)),
                                   deltaWrong=sapply(seq(0,360, length.out=300), function(y) 1/delta(SNR, 0))),
                   aes(x=diff)) + geom_line(aes(y=deltaRight, color='Right')) + 
            geom_line(aes(y=deltaWrong, color='Wrong')) + ylim(0,10)},
           SNR=slider(1, 20, step=1))

# This is the fixed version. Added the potential gain imbalance thing to see if anything happens that looks good.
yg <- 1
xg <- 1
difarNoise <- function(SNR, R, N) {
      N <- N * pi / 180; R <- R * pi / 180
      delta <- delta(SNR, R-N)
      numerator <- sin(R) + sin(N)/delta
      denom <- (cos(R) + cos(N)/delta ) * xg
      if(denom < 0) {( atan(numerator/denom) * 180 / pi + 180) %% 360}
      else {(atan(numerator/denom) * 180 / pi) %% 360}
}

theta <- seq(0, 360, length.out=180)
difars <- sapply(theta, function(x) difarNoise(SNR, x, N))
R <- 244
D <- 227 + 11.7
manipulate({ggplot(data=data.frame(theta=theta, difar = sapply(theta, function(x) difarNoise(SNR, x, N + 11.7)))) +
            geom_line(aes(x=theta, y=theta-difar, color='Difar')) + ylim(-40,40) + xlim(0, 360) +
            geom_vline(xintercept=R, color='green') + geom_hline(yintercept=D+11.7) + 
            geom_hline(yintercept=(N+11.7) %% 360, color='red') + geom_hline(yintercept=(N+11.7) %% 360 +360, color='red') +
            geom_abline(slope=1)},
           SNR = slider(1,30, step=1, initial=30), N=slider(1,360, step=2), D=slider(1,360, step=2), R=slider(1,360, step=2))
