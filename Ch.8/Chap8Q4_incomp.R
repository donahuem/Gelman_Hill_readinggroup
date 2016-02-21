# 4. Model checking for count data: 
# the folder risky.behavior contains data from a study of 
# behavior of couples at risk for HIV; see Exercise 6.1.
# (a) Fit a Poisson regression model predicting number of 
# unprotected sex acts from baseline HIV status. 

# (b) Perform predictive simulation to generate 1000 datasets 
# and record both the percent of observations 
# that are equal to 0 and the percent that are greater 
# than 10 (the third quartile in the observed data) for each. 
# Compare these values to the observed value in the original data.
 
# (c) Repeat (b), also including ethnicity and baseline number 
of unprotected sex acts as input variables.


library(foreign)
library(arm)
risky <- read.dta("data/risky.behaviors/risky_behaviors.dta")
names(risky)
# > names(risky)
# [1] "sex"         "couples"     "women_alone" "bs_hiv"      "bupacts"     "fupacts"    

attach(risky)

# (a) Fit a Poisson regression model predicting number of 
# unprotected sex acts from baseline HIV status. 
# Perform predictive simulation to generate 1000 datasets 
# and record both the percent of observations 
# that are equal to 0 and the percent that are greater 
# than 10 (the third quartile in the observed data) for each. 
# Compare these values to the observed value in the original data.

# I don't know the variables! :(
# Fit a Posson regression model
# bupacts ~ HIV status

glm.a <- glm(bupacts ~ bs_hiv, family = poisson)
n <- length (bupacts)
x <- cbind(rep(1,n), bs_hiv)

# E(y|x) = exp(xB)  possion regression model
y.hat <-  exp (x %*% coef (glm.a)) 
y.rep <- rpois (n, y.hat)
print (mean (bupacts==0)) 
print (mean (y.rep==0))

n.sims <- 1000
y.rep <- array (NA, c(n.sims, n)) 
sim.a <- sim (glm.a, n.sims)

for (s in 1:n.sims){
  y.hat <- exp(x %*% coef(sim.a)[s,])
  y.rep[s,] <- rpois (n, y.hat) 
  }

# returns number of counts == 0
Test <- function (y) mean (y==0)

# testing the simulated data
# how proportion of each data have 0?
test.rep <- rep (NA, n.sims) 
for (s in 1:n.sims) test.rep[s] <- Test (y.rep[s,]) 

sum(test.rep!=0)/n.sims





# (b) Repeat (a) using an overdispersed Poisson regression model
glm.a <- glm(bupacts ~ bs_hiv, family = quasipoisson)
n <- length (bupacts)
x <- cbind(rep(1,n), bs_hiv)

# E(y|x) = exp(xB)  possion regression model
y.hat <-  exp (x %*% coef (glm.a)) 
y.rep <- rpois (n, y.hat)
print (mean (bupacts==0)) 
print (mean (y.rep==0))

n.sims <- 1000
y.rep <- array (NA, c(n.sims, n)) 
sim.a <- sim (glm.a, n.sims)

for (s in 1:n.sims){
  y.hat <- exp(x %*% coef(sim.a)[s,])
  y.rep[s,] <- rpois (n, y.hat) 
}

# returns number of counts == 0
Test <- function (y) mean (y==0)

# testing the simulated data
# how proportion of each data have 0?
test.rep <- rep (NA, n.sims) 
for (s in 1:n.sims) test.rep[s] <- Test (y.rep[s,]) 

sum(test.rep!=0)/n.sims

detach(risky)

 
