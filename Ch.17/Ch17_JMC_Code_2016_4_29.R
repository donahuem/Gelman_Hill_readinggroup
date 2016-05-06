# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/radon

# The R codes & data files should be saved in the same directory for
# the source command to work

rm(list=ls()) #remove previous variable assignments
library(arm)
library(rjags)
library(coda)

setwd("C:/Users/Jamie/IDriveSync/MultilevelModels_ReadingGroup/Book_Codes/Ch.17/") # set working directory


##################### 17.1 Varying intercept, varying slope models ############################
## set up the data 
srrs2 <- read.table("srrs2.dat", header=T, sep=",")  # read in the data, copy from chapter 12
radon <- srrs2$activity                            # subset radon levels 

logRadon <- log(ifelse(radon==0,.1,radon))                # our response will be log radon because we are not assuming multiplactive effects, round 0s to 0.1 before taking the log
numSamps <- length(radon)                          # sample size
floor <- srrs2$floor                                   # our dependent variable will be floor, 0 for basement, 1 for first floor

county.name <- as.vector(srrs2$county)             # subset counties
uniq.name <- unique(county.name)                   # make a list of only unique names (length=85)
numCounties <- length(uniq.name)                      # number of groups J
countyID <- rep(NA, numCounties)                        # empty vector
for (i in 1:numCounties){
  countyID[county.name==uniq.name[i]] <- i           # fill in empty vector to correspond to county.name, and code counties 1:85  
}

## set up parameters to use later
numChains = 3
numUpdate=4000
numIter=2000
numAdapt=1000
numThin=300
burn.in = 1000

cat("model {
  for (i in 1:numSamps){
    logRadon[i] ~ dnorm (logRadon.hat[i], tau.y)
    logRadon.hat[i] <- a[countyID[i]] + b[countyID[i]]*floor[i]
  }
  tau.y <- pow(sigma.y, -2)
  sigma.y ~ dunif (0, 100)
  
  for (j in 1:numCounties){
    a[j] <- B[j,1]
    b[j] <- B[j,2]
    B[j,1:2] ~ dmnorm (B.hat[j,], Tau.B[,])
    B.hat[j,1] <- mu.a
    B.hat[j,2] <- mu.b
  }
  mu.a ~ dnorm (0, .0001)
  mu.b ~ dnorm (0, .0001)
  
  Tau.B[1:2,1:2] <- inverse(Sigma.B[,])
  Sigma.B[1,1] <- pow(sigma.a, 2)
  sigma.a ~ dunif (0, 100)
  Sigma.B[2,2] <- pow(sigma.b, 2)
  sigma.b ~ dunif (0, 100)
  Sigma.B[1,2] <- rho*sigma.a*sigma.b
  Sigma.B[2,1] <- Sigma.B[1,2]
  rho ~ dunif (-1, 1)
}", file = "jags_correlation_inter_slope")

radon.data <- list ("numSamps"=numSamps, "numCounties"=numCounties, "floor"=floor, "logRadon"=logRadon, "countyID"=countyID)#, "u"=u) # define data in a list "knowns"

radon.inits <- function (){ # function for creating initial conditions, copied from book
  list (B=array (rnorm (2*numCounties), c(numCounties,2)), g.a.0=rnorm(1), g.a.1=rnorm(1),
        g.b.0=rnorm(1), g.b.1=rnorm(1),
        sigma.y=runif(1), sigma.a=runif(1), sigma.b=runif(1), rho=runif(1))
}

radon.jags.4a <- jags.model(file="jags_correlation_inter_slope", data=radon.data, 
                            inits=radon.inits, n.chains=numChains, n.adapt=numAdapt)

# initialize the model
load.module("dic")  # load the DIC module to monitor deviance
params <- c("deviance","b","mu.a","sigma.y","sigma.a", "rho") # make a list of paramters for JAGS to monitor
codaSamps <- coda.samples(radon.jags.4a, params, n.iter=numIter) # run iterations
plot(codaSamps) # make trace plots to assess mixing

# Adapt and burn in
update(radon.jags.4a, n.iter=burn.in) # update to the model to start after burn in
codaSamps2 <- coda.samples(radon.jags.4a, params, n.iter=numIter, thin=numThin)

# Monitor
plot(codaSamps2)
gelman.diag(codaSamps2) # Gelman-Rubin convergence diagnostics (want r < 1.2)
autocorr.plot(codaSamps2)

# 6. Results
summary(codaSamps2) # Interence for model parameter

## Set up and call jags for the scaled-inverse Wishart distribution

library("MCMCpack") # need to install MCMCpack from packages first

cat("model {
  for (i in 1:numSamps){
    logRadon[i] ~ dnorm (logRadon.hat[i], tau.y)
    logRadon.hat[i] <- a[countyID[i]] + b[countyID[i]]*floor[i]
    }
    
  tau.y <- pow(sigma.y, -2)
  sigma.y ~ dunif (0, 100)
  
  for (j in 1:numCounties){
    a[j] <- xi.a*B.raw[j,1]
    b[j] <- xi.b*B.raw[j,2]
    B.raw[j,1:2] ~ dmnorm (B.raw.hat[j,], Tau.B.raw[,])
    B.raw.hat[j,1] <- mu.a.raw
    B.raw.hat[j,2] <- mu.b.raw
    }
  
  mu.a <- xi.a*mu.a.raw
  mu.b <- xi.b*mu.b.raw
  mu.a.raw ~ dnorm (0, .0001)
  mu.b.raw ~ dnorm (0, .0001)
  xi.a ~ dunif (0, 100)
  xi.b ~ dunif (0, 100)
  Tau.B.raw[1:2,1:2] ~ dwish (W[,], df)
  df <- 3
  Sigma.B.raw[1:2,1:2] <- inverse(Tau.B.raw[,])
  sigma.a <- xi.a*sqrt(Sigma.B.raw[1,1])
  sigma.b <- xi.b*sqrt(Sigma.B.raw[2,2])
  rho <- Sigma.B.raw[1,2]/sqrt(Sigma.B.raw[1,1]*Sigma.B.raw[2,2])
}", file = "Scaled_inverse_Wishart")

W <- diag (2)
radon.data2 <- list ("numSamps"=numSamps, "numCounties"=numCounties, "floor"=floor, "logRadon"=logRadon, "countyID"=countyID, "W"=W)

radon.inits2 <- function (){
  list (B.raw=array(rnorm(2*numCounties), c(numCounties,2)), mu.a.raw=rnorm(1), mu.b.raw=rnorm(1),
    sigma.y=runif(1), Tau.B.raw=rwish(3,diag(2)), xi.a=runif(1), 
    xi.b=runif(1))
}

M1 <- jags.model(file="Scaled_inverse_Wishart",
                 data=radon.data2, 
                 inits=radon.inits2,
                 n.chains=numChains, 
                 n.adapt=numAdapt)

radon.parameters <- c ("a"=a, "b"=b, "mu.a"=mu.a, "mu.b", "sigma.y", "sigma.a"=sigma.a, "sigma.b"=sigma.b, "rho"=rho)

## Set up and call jags for the multiple varying coefficients model

library("MCMCpack")

# new variables
X <- cbind (1,floor)
K <- ncol (X)
W <- diag (2) # diagnol of n x K matrix of predictors

cat("model {
  for (i in 1:numSamps){
    logRadon[i] ~ dnorm (logRadon.hat[i], tau.y)
    logRadon.hat[i] <- inprod(B[countyID[i],],X[i,]) # inprod computes a product of inner products
  }
  tau.y <- pow(sigma.y, -2)
  sigma.y ~ dunif (0, 100)
  
  for (j in 1:numCounties){
    for (k in 1:K){
      B[j,k] <- xi[k]*B.raw[j,k]
    }
    B.raw[j,1:K] ~ dmnorm (mu.raw[], Tau.B.raw[,])
  }
  
  for (k in 1:K){
    mu[k] <- xi[k]*mu.raw[k]
    mu.raw[k] ~ dnorm (0, .0001)
    xi[k] ~ dunif (0, 100)
  }

  Tau.B.raw[1:K,1:K] ~ dwish (W[,], df)
  df <- K+1
  Sigma.B.raw[1:K,1:K] <- inverse(Tau.B.raw[,])
  for (k in 1:K){
    for (k.prime in 1:K){
      rho.B[k,k.prime] <- Sigma.B.raw[k,k.prime]/
      sqrt(Sigma.B.raw[k,k]*Sigma.B.raw[k.prime,k.prime])
  }
  sigma.B[k] <- abs(xi[k])*sqrt(Sigma.B.raw[k,k])
  }
}", file="multi_vary_coeff")

radon.data3 <- list ("numSamps"=numSamps, "numCounties"=numCounties, "K"=K, "X"=X, "logRadon"=logRadon, "countyID"=countyID, "W"=W)

radon.inits3 <- function (){
  list (B.raw=array(rnorm(numCounties*K), c(numCounties,K)), mu.raw=rnorm(K), sigma.y=runif(1),
       Tau.B.raw=rwish(K+1,diag(K)), xi=runif(K))
}

M2 <- jags.model(file="multi_vary_coeff",
                 data=radon.data3, 
                 inits=radon.inits3,
                 n.chains=numChains, 
                 n.adapt=numAdapt)

##################### Chapter 17.2 adding group level predictors ##############################
## Multiple varying coefficients with multiple group-level predictors
cat("model {
for (i in 1:numSamps){
    logRadon[i] ~ dnorm (logRadon.hat[i], tau.y)
    logRadon.hat[i] <- inprod(B[countyID[i],],X[i,])
    }
    tau.y <- pow(sigma.y, -2)
    sigma.y ~ dunif (0, 100)
    for (k in 1:K){
    for (j in 1:numCounties){
    B[j,k] <- xi[k]*B.raw[j,k]
    }
    xi[k] ~ dunif (0, 100)
    }
    for (j in 1:numCounties){
    B.raw[j,1:K] ~ dmnorm (B.raw.hat[j,], Tau.B.raw[,])
    for (k in 1:K){
    B.raw.hat[j,k] <- inprod(G.raw[k,],U[j,])
    }
    }
    for (k in 1:K){
    for (l in 1:L){ # L?
    G[k,l] <- xi[k]*G.raw[k,l]
    G.raw[k,l] ~ dnorm (0, .0001)
    }
    }
    Tau.B.raw[1:K,1:K] ~ dwish (W[,], df)
    df <- K+1
    Sigma.B.raw[1:K,1:K] <- inverse(Tau.B.raw[,])
    for (k in 1:K){
    for (k.prime in 1:K){
    rho.B[k,k.prime] <- Sigma.B.raw[k,k.prime]/
    sqrt(Sigma.B.raw[k,k]*Sigma.B.raw[k.prime,k.prime])
    }
    sigma.B[k] <- abs(xi[k])*sqrt(Sigma.B.raw[k,k])
    }
}", file="vary_coeff_multi_group_level_pred")

W <- diag (2)
radon.data4 <- list ("numSamps"=numSamps, "numCounties"=numCounties, "X"=X, "logRadon"=logRadon, "countyID"=countyID, "W"=W, "K"=K)

radon.inits4 <- function (){
  list (B.raw=array(rnorm(2*numCounties), c(numCounties,2)), mu.a.raw=rnorm(1), mu.b.raw=rnorm(1),
        sigma.y=runif(1), Tau.B.raw=rwish(3,diag(2)), xi.a=runif(1), 
        xi.b=runif(1))
}

M2 <- jags.model(file="vary_coeff_multi_group_level_pred",
                 data=radon.data4, 
                 inits=radon.inits4,
                 n.chains=3, 
                 n.adapt=100)

##################### Chapter 17.3 non-nested models ##############################
source("C:/Users/Jamie/IDriveSync/MultilevelModels_ReadingGroup/Book_Codes/Ch.13/13.5_Non-nested models.R") # where data was cleaned

cat("model {
for (i in 1:n){
    y[i] ~ dnorm (y.hat[i], tau.y)
    y.hat[i] <- mu + gamma[treatment[i]] + delta[airport[i]]
    }
    mu ~ dnorm (0, .0001)
    tau.y <- pow(sigma.y, -2)
    sigma.y ~ dunif (0, 100)
    for (j in 1:n.treatment){
    gamma[j] ~ dnorm (0, tau.gamma)
    }
    tau.gamma <- pow(sigma.gamma, -2)
    sigma.gamma ~ dunif (0, 100)
    for (k in 1:n.airport){
    delta[k] ~ dnorm (0, tau.delta)
    }
    tau.delta <- pow(sigma.delta, -2)
    sigma.delta ~ dunif (0, 100)
  }", file="nonNested_model")

## Define variables
y <- successes/(successes+failures)
treatment <- group.id
airport <- scenario.id

## Fit the 2-model using jags

n.treatment <- max(treatment)
n.airport <- max(airport)
n <- length(y)

data <- list ("y"=y, "treatment"=treatment, "airport"=airport, "n"=n, "n.treatment"=n.treatment, "n.airport"=n.airport)

inits <- function (){
  list (mu=rnorm(1), sigma.delta=runif(1), sigma.gamma=runif(1), sigma.y=runif(1))
}

pilots <- jags.model(file="nonNested_model", 
                     data=data, inits=inits, 
                     n.chains=numChains, 
                     n.adapt=numAdapt)


##################### Chapter 17.4 multilevel logistic regression ##############################


## Read the data & define variables
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/election88
library ("arm")
polls.subset <- read.table ("polls.subset.dat")
attach (polls.subset)

# Load in data for region indicators
# Use "state", an R data file (type ?state from the R command window for info)
#
# Regions:  1=northeast, 2=south, 3=north central, 4=west, 5=d.c.
# We have to insert d.c. (it is the 9th "state" in alphabetical order)

data (state)                  # "state" is an R data file
state.abbr <- c (state.abb[1:8], "DC", state.abb[9:50])
dc <- 9
not.dc <- c(1:8,10:51)
region <- c(3,4,4,3,4,4,1,1,5,3,3,4,4,2,2,2,2,3,3,1,1,1,2,2,3,2,4,2,4,1,1,4,1,3,2,2,3,4,1,1,3,2,3,3,4,1,3,4,1,2,4)

# define other data summaries

y <- bush                  # 1 if support bush, 0 if support dukakis
n <- length(y)             # of survey respondents
n.age <- max(age)          # of age categories
n.edu <- max(edu)          # of education categories
n.state <- max(state)      # of states
n.region <- max(region)    # of regions

# also include a measure of previous vote as a state-level predictor

library (foreign)
presvote <- read.dta ("presvote.dta")
attach (presvote)
v.prev <- presvote$g76_84pr
age.edu <- n.edu*(age-1) + edu

## model
cat("model {
for (i in 1:n){
    y[i] ~ dbin (p.bound[i], 1) # distributed binomially (y[i]=1 with probability p[i] and 0 otherwise)
    p.bound[i] <- max(0, min(1, p[i])) # p.boud restricts the probability between 0 and 1
    logit(p[i]) <- Xbeta[i]
    Xbeta[i] <- b.0 + b.female*female[i] + b.black*black[i] +
    b.female.black*female[i]*black[i] + b.age[age[i]] + b.edu[edu[i]] +
    b.age.edu[age[i],edu[i]] + b.state[state[i]]
    }
    b.0 ~ dnorm (0, .0001)
    b.female ~ dnorm (0, .0001)
    b.black ~ dnorm (0, .0001)
    b.female.black ~ dnorm (0, .0001)
    for (j in 1:n.age) {b.age[j] ~ dnorm (0, tau.age)}
    for (j in 1:n.edu) {b.edu[j] ~ dnorm (0, tau.edu)}
    for (j in 1:n.age) {for (k in 1:n.edu){
    b.age.edu[j,k] ~ dnorm (0, tau.age.edu)}}
    for (j in 1:n.state) {
    b.state[j] ~ dnorm (b.state.hat[j], tau.state)
    b.state.hat[j] <- b.region[region[j]] + b.v.prev*v.prev[j]}
    b.v.prev ~ dnorm (0, .0001)
    for (j in 1:n.region) {b.region[j] ~ dnorm (0, tau.region)}
    tau.age <- pow(sigma.age, -2)
    tau.edu <- pow(sigma.edu, -2)
    tau.age.edu <- pow(sigma.age.edu, -2)
    tau.state <- pow(sigma.state, -2)
    tau.region <- pow(sigma.region, -2)
    sigma.age ~ dunif (0, 100)
    sigma.edu ~ dunif (0, 100)
    sigma.age.edu ~ dunif (0, 100)
    sigma.state ~ dunif (0, 100)
    sigma.region ~ dunif (0, 100)
}", file="multilevel_logistic")

## Fit the model in jags

data2 <- list ("n"=n, "n.age"=n.age, "n.edu"=n.edu, "n.state"=n.state, "n.region"=n.region,
              "y"=y, "female"=female, "black"=black, "age"=age, "edu"=edu, "state"=state, "region"=region, "v.prev"=v.prev)

election.inits <- function (){
  list(b.0=rnorm(1), b.female=rnorm(1), b.black=rnorm(1), b.female.black=rnorm(1),
       b.age=rnorm(n.age), b.edu=rnorm(n.edu), b.age.edu=array(rnorm(n.age*n.edu), 
                                                               c(n.age,n.edu)), b.state=rnorm(n.state), b.v.prev=rnorm(1), 
       b.region=rnorm(n.region), sigma.age=runif(1), sigma.edu=runif(1), 
       sigma.age.edu=runif(1), sigma.state=runif(1), sigma.region=runif(1))
}

election.ch17 <- jags.model(file="multilevel_logistic",
                            data=data2, 
                            inits=election.inits,
                            n.chains=3, 
                            n.adapt=100)

##################### Chapter 17.5 multilevel poisson regression ##############################
cat("model {
for (i in 1:n){
    stops[i] ~ dpois (lambda[i])
    log(lambda[i]) <- offset[i] + mu +
    b.eth[eth[i]] + b.precinct[precinct[i]] + epsilon[i]
    epsilon[i] ~ dnorm (0, tau.epsilon)
    }
    mu ~ dnorm (0, .0001)
    mu.adj <- mu + mean(b.eth[]) + mean(b.precinct[])
    tau.epsilon <- pow(sigma.epsilon, -2)
    sigma.epsilon ~ dunif (0, 100)
    for (j in 1:n.eth){
    b.eth[j] ~ dnorm (0, tau.eth)
    b.eth.adj[j] <- b.eth[j] - mean(b.eth[])
    }
    tau.eth <- pow(sigma.eth, -2)
    sigma.eth ~ dunif (0, 100)
    for (j in 1:n.precinct){
    b.precinct[j] ~ dnorm (0, tau.precinct)
    b.precinct.adj[j] <- b.precinct[j] - mean(b.precinct[])
    }
    tau.precinct <- pow(sigma.precinct, -2)
    sigma.precinct ~ dunif (0, 100)
    }", file="multilevel_poisson")

##################### Chapter 17.6 multilevel ordered categorical regression ##############################
cat("model {
for (i in 1:n){
y[i] ~ dcat(P[i,])
P[i,1] <- 1 - Q[i,1]
for (i.cut in 2:n.cut){
P[i,i.cut] <- Q[i,i.cut-1] - Q[i,i.cut]
}
P[i,n.cut+1] <- Q[i,n.cut]
for (i.cut in 1:n.cut){
logit(Q[i,i.cut]) <- z[i,i.cut]
Z[i,i.cut] <- (x[i] - C[player[i],i.cut])/s[player[i]]
}
}
for (i.player in 1:n.player){
C[i.player,1] ~ dnorm (mu.c[1], tau.c[1])I(0,C[i.player,2])
C[i.player,2] ~ dnorm (mu.c[2], tau.c[2])I(C[i.player,1],100)
s[i.player] ~ dlnorm (mu.log.s, tau.log.s)I(1,100)
}
for (i.cut in 1:n.cut){
mu.c[i.cut] ~ dnorm (0, 1.E-6)
tau.c[i.cut] <- pow(sigma.c[i.cut], -2)
sigma.c[i.cut] ~ dunif (0, 1000)
}
mu.log.s ~ dnorm (0, .0001)
tau.log.s <- pow(sigma.log.s, -2)
sigma.log.s ~ dunif (0, 1000)
}", file="multilvel_ordered_categorical")