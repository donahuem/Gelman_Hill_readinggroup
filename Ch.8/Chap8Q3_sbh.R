# 3. Using simulation to check the fit of a time-series model: 
#   find time-series data and fit a first-order autoregression model to it. 
# Then use predictive simulation to check the fit of this model as in Section 8.4.

# Using mauna loa co2 time series data (co2dat.csv in the same directory)
myts <- read.csv("co2dat.csv", header= T)
y <- myts[,2]
n <- length(y)
y.lag <- c(NA, y[1:(n-1)])
lm.lag <- lm(y ~ y.lag)
display(lm.lag)

# OUTPUT
#lm(formula = y ~ y.lag)
# coef.est coef.se
# (Intercept) 5.97     4.63   
# y.lag       0.98     0.01   
# ---
#   n = 191, k = 2
# residual sd = 1.15, R-Squared = 0.96

# Generate fake data using parameter estimates( b.hat and s.hat) from lm
b.hat <- coef (lm.lag) 
s.hat <- sigma.hat (lm.lag)
n.sims <- 100
y.rep <- array (NA, c(n.sims, n)) 

for (s in 1:n.sims){
  y.rep[s,1] <- y[1] 
  for (t in 2:n){
    prediction <- c (1, y.rep[s,t-1]) %*% b.hat
    y.rep[s,t] <- rnorm (1, prediction, s.hat) }
  }

# Simulation used to obtain the parameter estimates then create fake data
lm.lag.sim <- sim (lm.lag, n.sims) # simulations of beta and sigma 

for (s in 1:n.sims){
  y.rep[s,1] <- y[1] 
  for (t in 2:n){
    prediction <- c (1, y.rep[s,t-1]) %*% coef(lm.lag.sim)[s,]
    y.rep[s,t] <- rnorm (1, prediction, sigma.hat(lm.lag.sim)[s]) 
    }
}


# Function testing 1st order AR model (how many switches does data have?)
Test <- function (y){
  n <- length (y)
  y.lag <- c (NA, y[1:(n-1)])
  y.lag2 <- c (NA, NA, y[1:(n-2)])
  sum (sign(y-y.lag) != sign(y.lag-y.lag2), na.rm=TRUE)
}

print (Test(y))
test.rep <- rep (NA, n.sims) 
for (s in 1:n.sims){
  test.rep[s] <- Test (y.rep[s,]) 
}


sum(test.rep>Test(y))  
# 100% of replicates show more switches thus the 1st order autoregressive model clearly does not
# fit the data

# checking the result graphically
par (mfrow=c(3,3))
plot(lm.lag$residuals, type = "l")
for(j in 2:9) plot(y.rep[,j], type = "l")
