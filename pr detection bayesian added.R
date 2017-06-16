rm(list = ls())
library(rjags)
library(swfscMisc)
set.seed(1)

# read and format data
df <- read.csv("./Data/DIFAR_single_buoy_308.csv")
df$Intensity <- as.factor(df$Intensity)

# FALSE if the DIFAR reported is NA, ie. we didn't get the call   

# set mcmc parameters
n.chains <- 1
n.adapt <- 500
n.burnin <- 1000
n.samples <- 10000
thin <- 2

# create data list
data <- list(
  b0.min = -5, b0.max = 5,
  b1.min = -5, b1.max = 5,
  b2.min = -5, b2.max = 5,
  b12.min = -5, b12.max = 5,
  n = nrow(df),
  detected = df$Detected,
  dist = df$Distance,
  angle = cos(convert.angle(df$RealBearing)), # Need to use real here becaues thats all we have for undetected
  buoy = as.numeric(df$Buoy),
  num.buoy = nlevels(df$Buoy),
  call = as.numeric(df$CallType),
  num.call = nlevels(df$CallType),
  intensity = as.numeric(df$Intensity),
  num.intensity = nlevels(df$Intensity),
  m = sum(df$Detected),
  angle.delta = df[df$Detected==TRUE, 'AngleError'],
  angle.buoy = as.numeric(df[df$Detected==TRUE, 'Buoy'])
)

# create initial prior values
num.cells <- data$num.buoy * data$num.call * data$num.intensity
inits <- lapply(1:n.chains, function(i) {
  ran.init <- list(
    b0 = runif(1, data$b0.min, data$b0.max),
    b1 = array(
      runif(num.cells, data$b1.min, data$b1.max),
      dim = c(data$num.buoy, data$num.call, data$num.intensity)
    ),
    b2 = array(
      runif(num.cells, data$b2.min, data$b2.max),
      dim = c(data$num.buoy, data$num.call, data$num.intensity)
    ),
    b12 = array(
          runif(num.cells, data$b12.min, data$b12.max),
          dim = c(data$num.buoy, data$num.call, data$num.intensity)
    ),
    offset = array(
          11.7, # Should start with declination
          dim = data$num.buoy
          ),
    tau = array(
          1/100, # SD of 10 per manufacturer
          dim = data$num.buoy
    ))
  #lapply(ran.init, function(i) ifelse(i < 1e-99, 1e-99, i))
  ran.init
})

# write model
write("model{
  # logistic likelihood
  for(i in 1:n) {
    detected[i] ~ dbern(p[i])
    logit(p[i]) <- b0 + b1[buoy[i], call[i], intensity[i]] * dist[i] + b2[buoy[i], call[i], intensity[i]] * angle[i] + b12[buoy[i], call[i], intensity[i]] * dist[i] * angle[i]
                        
  }
  # angle model
  for(j in 1:m) {
    angle.delta[j] ~ dnorm(mu[j], tau[angle.buoy[j]])
    mu[j] <- offset[angle.buoy[j]]
  }

  # priors
  b0 ~ dnorm(0, .0001)
  for(b in 1:num.buoy) {
    for(c in 1:num.call) {
      for(i in 1:num.intensity) {
        b1[b, c, i] ~ dnorm(0, .0001)
        b2[b, c, i] ~ dnorm(0, .0001)
        b12[b, c, i] ~ dnorm(0, .0001)
      }
    }
  }
  for(b in 1:num.buoy) {
    offset[b] ~ dnorm(11.7, .0001)
    tau[b] ~ dgamma(.001, .001)
  }
}", file = "pr.detect.model.txt")

# load and check model
logistic.jags <- jags.model(
  file = "pr.detect.model.txt",
  data = data,
  inits = inits,
  n.chains = length(inits),
  n.adapt = n.adapt
)

# burnin
update(logistic.jags, n.burnin)

# run mcmc and collect samples
logistic.post <- jags.samples(
  model = logistic.jags,
  variable.names = c("b0", "b1", "b2", "b12", "offset", "tau"),
  n.iter = n.samples,
  thin = thin
)

save(logistic.post, df, file = "logistic posterior.rdata")

test.post <- jags.samples(
      model=logistic.jags,
      variable.names= c('detected','dist'),
      n.iter=n.samples,
      thin=thin
)
