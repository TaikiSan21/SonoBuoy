rm(list = ls())
library(rjags)
library(swfscMisc)
set.seed(1)

# read and format data
df <- read.csv("DetectionData.csv")
df$Intensity <- factor(df$Intensity)
# FOR TESTING: random indication of detected calls
df$detected <- sample(0:1, nrow(df), replace = TRUE)

# set mcmc parameters
n.chains <- 1
n.adapt <- 500
n.burnin <- 1000
n.samples <- 2000
thin <- 10

# create data list
data <- list(
  b0.min = -5, b0.max = 5,
  b1.min = -5, b1.max = 5,
  b2.min = -5, b2.max = 5,
  n = nrow(df),
  detected = df$detected,
  dist = df$Distance,
  angle = cos(convert.angle(df$FixedBearing)),
  buoy = as.numeric(df$Buoy),
  num.buoy = nlevels(df$Buoy),
  call = as.numeric(df$CallType),
  num.call = nlevels(df$CallType)
)

# create initial prior values
num.cells <- data$num.buoy * data$num.call
inits <- lapply(1:n.chains, function(i) {
  ran.init <- list(
    b0 = runif(1, data$b0.min, data$b0.max),
    b1 = array(
      runif(num.cells, data$b1.min, data$b1.max),
      dim = c(data$num.buoy, data$num.call)
    ),
    b2 = array(
      runif(num.cells, data$b2.min, data$b2.max),
      dim = c(data$num.buoy, data$num.call)
    )
  )
  #lapply(ran.init, function(i) ifelse(i < 1e-99, 1e-99, i))
  ran.init
})

# write model
write("model{
  # logistic likelihood
  for(i in 1:n) {
    detected[i] ~ dbern(p[i])
    logit(p[i]) <- b0 + b1[buoy[i], call[i]] * dist[i] + b2[buoy[i], call[i]] * angle[i]
  }

  # priors
  b0 ~ dunif(b0.min, b0.max)
  for(b in 1:num.buoy) {
    for(c in 1:num.call) {
      b1[b, c] ~ dunif(b1.min, b1.max)
      b2[b, c] ~ dunif(b2.min, b2.max)
    }
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
  variable.names = c("b0", "b1", "b2"),
  n.iter = n.samples,
  thin = thin
)

save(logistic.post, df, file = "logistic posterior.rdata")

