
# Chapter 16 --------------------------------------------------------------
library(arm)
library(rjags)
library(coda)

# set up the data ---------------------------------------------------------
srrs2 <- read.table("srrs2.dat",header=T,sep=",")      # read in the data, folder: data -> radon
mn <- srrs2$state=="MN"                                # subset for Minnesota
radon <- srrs2$activity[mn]                            # subset radon levels for MN
y <- log(ifelse(radon==0,.1,radon))                    # our response will be log radon because we are not assuming multiplactive effects, round 0s to 0.1 before taking the log
n <- length(radon)                                     # sample size
x <- srrs2$floor[mn]                                   # our dependent variable will be floor, 0 for basement, 1 for first floor
# srrs2.fips <- srrs2$stfips*1000 + srrs2$cntyfips
county.name <- as.vector(srrs2$county[mn])             # subset county for MN
uniq.name <- unique(county.name)                       # make a list of only unique names (length=85)
J <- length(uniq.name)                                 # number of groups J
county <- rep(NA, J)                                   # empty vector
for (i in 1:J){
  county[county.name==uniq.name[i]] <- i               # fill in empty vector to correspond to county.name, and code counties 1:85  
}

# classic regression  -----------------------------------------------------
# complete pooling
lm.pooled <- lm(y~x)
display(lm.pooled)

# classic regression - no pooling
lm.unpooled.0 <- lm(y~x+factor(county))
display(lm.unpooled.0)
lm.unpooled <- lm(y~x+factor(county)-1)                # removes constant term, each county has own intercept
display(lm.unpooled)

# multilevel in lmer
library(lme4)
mult.lm <- lmer(y~x+(1|factor(county)))
display(mult.lm)

# multilevel regression with JAGS  ----------------------------------------

# 1. Define model in JAGS
cat("model{                                                 
  for(i in 1:n){
    y.hat[i] <- a[county[i]] + b*x[i]                  # data model (e.g. 16.1a)
    y[i] ~ dnorm(y.hat[i], tau.y)                      # y[i] distributed normal with mean y.hat and precision tau.y (precision = 1/variance)
  }                                                    # loop ends here that defines likelihood
  b ~ dnorm(0,0.0001)                                  # prior for b
  tau.y <- pow(sigma.y, -2)                            # tau.y is equal to 1/sigma.y squared
  sigma.y ~ dunif(0,100)                               # prior for sigma.y
  for(j in 1:J){
    a[j] ~ dnorm(mu.a, tau.a)                          # group model (e.g. 16.1b)
  }
  mu.a ~ dnorm(0, 0.0001)                              # prior for mu.a
  tau.a <- pow(sigma.a, -2)                            # tau.a is equal to 1 over sigma.a squared
  sigma.a ~ dunif(0,100)                               # prior for sigma.a
}", file="jags_multilevel")

# call JAGS from R
# 2. Compile the model, check for undefined parameters, and get ready to collect posterior samples
radon.data <- list(y=y,x=x,n=n,county=county,J=J)      # define data in a list "knowns"
# set up list of initial values to start the sampler, number of lists should be the number chains
radon.inits <- list(list(a=rnorm(J),b=rnorm(1)
                         ,mu.a=rnorm(1), sigma.y=runif(1),  
                         sigma.a=runif(1)),
                    list(a=rnorm(J),b=rnorm(1)
                         ,mu.a=rnorm(1), sigma.y=runif(1),  
                         sigma.a=runif(1)),
                    list(a=rnorm(J),b=rnorm(1)
                         ,mu.a=rnorm(1), sigma.y=runif(1),  
                         sigma.a=runif(1)))

jags.mod <- jags.model(file="jags_multilevel",         # construct the jags model object in R
                       data=radon.data,
                       inits=radon.inits,              # can comment this line if want random initials
                       n.chains=length(radon.inits),   # 3 MCMC chains
                       n.adapt=100)                    # number of runs to toss for JAGS to figure out tuning parameter for jump size

# 3. Initialize the model
load.module("dic")                                     # load the DIC module to monitor deviance
params <- c("deviance","b","mu.a","sigma.y","sigma.a") # make a list of paramters for JAGS to monitor
samps <- coda.samples(jags.mod,params
                      ,n.iter=2000)                    # run 2000 iterations
plot(samps)                                            # trace plots to assess mixing

st <- start(samps)                                     # save start and end points for later
en <- end(samps)

# 4. Burn in
burn.in <- 1000
update(jags.mod, n.iter=burn.in)                       # update to the model to start after burn in
samps <- coda.samples(jags.mod,params,n.iter=2000)      # run samples again
start(samps)
end(samps)

# 5. Monitor
plot(samps)
gelman.diag(samps)                                     # Gelman-Rubin convergence diagnostics (want r < 1.2)
autocorr.plot(samps)
## plot nears zero at 10 for sigma so we should thin every 10 steps

samps <- coda.samples(jags.mod,params,n.iter=2000,thin=10)      # run samples again, or you could thin the existing chain
autocorr.plot(samps) # check again

# 6. Results
summary(samps)                                         # Interence for model parameters
dic.star <- summary(samps)[[1]][2,1] + 0.5*(summary(samps)[[1]][2,2]^2) # estimate of expected predictive error (lower is better)
dic.star #deviance information criterion (save for later - model selection)

a <- coda.samples(jags.mod,c("a"),n.iter=2000,thin=10) # save results by county for plotting

# plots to compare the complete, no, and pooled results -------------------

# complete pooling
plot(y~x)
abline(lm.pooled,lwd=2)

# classic regression - no pooling
par(mfrow=c(4,2),mar=c(3,3,2,1),oma=c(1,1,0,0))
for(i in 1:8){
  plot(y[county==i]~jitter(x[county==i],factor=0.1),ylim=c(-1.1,3.1),xlab="",ylab="",main=uniq.name[i])
  curve(coef(lm.pooled)[1]+coef(lm.pooled)[2]*x,col="black",add=T,lty=2)
  curve(coef(lm.unpooled)[i+1]+coef(lm.unpooled)[1]*x,col="grey10",add=T)
}
mtext("log radon",side=2,outer=T)
mtext("floor",side=1,outer=T)


# jags results
a.multilevel <- rep(NA,J)
for(j in 1:J){
  a.multilevel[j] <- median(a[[1]][,j])
}
b.multilevel <- median(samps[[1]][,1])

par(mfrow=c(4,2),mar=c(3,3,2,1),oma=c(1,1,0,0))
for(i in 1:8){
  plot(y[county==i]~jitter(x[county==i],factor=0.1),ylim=c(-1.1,3.1),xlab="",ylab="",main=uniq.name[i])
  curve(coef(lm.pooled)[1]+coef(lm.pooled)[2]*x,col="black",add=T,lty=2)
  curve(coef(lm.unpooled)[i+1]+coef(lm.unpooled)[1]*x,col="grey10",add=T)
  curve(a.multilevel[i] + b.multilevel*x, lwd=1, col="blue", add=T)
}
mtext("log radon",side=2,outer=T)
mtext("floor",side=1,outer=T)
