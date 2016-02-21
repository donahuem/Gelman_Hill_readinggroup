library(arm)
# 1. Fitting the wrong model: suppose you have 100 data points that arose 
# from the following model: y = 3 + 0.1x1 + 0.5x2 + error, with errors 
# having a t distribution with mean 0, scale 5, and 4 degrees of freedom.
# We shall explore the implications of fitting a standard linear regression to these data.

# (a) Simulate data from this model. For simplicity, 
# suppose the values of x1 are simply the integers from 1 to 100, 
# and that the values of x2 are random and equally likely to be 0 or 1.3 
# Fit a linear regression (with normal errors) to these data and 
# see if the 68% confidence intervals for the regression coefficients 
# (for each, the estimates ±1 standard error) cover the true values.
# (b)Put the above step in a loop and repeat 1000 times. 
# Calculate the confidence coverage for the 68% intervals 
# for each of the three coefficients in the model. 
#(c) Repeat this simulation, but instead fit the model using t errors (see Exercise 6.6).

# (a) Simulate data from this model. For simplicity, 
# suppose the values of x1 are simply the integers from 1 to 100, 
# and that the values of x2 are random and equally likely to be 0 or 1.3 
# Fit a linear regression (with normal errors) to these data and 
# see if the 68% confidence intervals for the regression coefficients 
# (for each, the estimates ±1 standard error) cover the true values.

# No of data
nfake <- 100

# x1 :  integers from 1 to 100
x1 <- c(1:nfake)

# x2 : 0 or 1.3 equally at random 
x2 <- sample(c(0, 1.3), nfake, replace =TRUE)

# "true" parameters
b1 = 0.1; b2 = 0.5

# y : 3 + 0.1x1 + 0.5x2 + error where error ~ t(scale = 5, mean =0, df = 4)
y = 3 + 0.1*x1 + 0.5 * x2 + rt(nfake, df=4)*5

# fitting linear regression
lm.a <- lm(y~x1+x2)
b1.hat <- coef(lm.a)[2] 
b2.hat <- coef(lm.a)[3]

b1.se <- se.coef(lm.a)[2]  
b2.se <- se.coef(lm.a)[3]
# OUTPUT
# > b1.se
# 0.01963182 
# > b2.se
# 0.8734098 

b1_cover.68 <- abs(b1-b1.hat) <b1.se  
b2_cover.68 <- abs(b2-b2.hat) <b2.se  
# OUTPUT
# true
# false




# (b)Put the above step in a loop and repeat 1000 times. 
# Calculate the confidence coverage for the 68% intervals 

b <- 1000
x1 <- c(1:nfake)
x2 <- sample(c(0, 1.3), nfake, replace =TRUE)

b1_cover.68 <- rep(NA, b)
b2_cover.68 <- rep(NA, b)
for(i in 1:b){
  y = 3 + 0.1*x1 + 0.5 * x2 + rt(nfake, df=4)*5
  lm.a <- lm(y~x1+x2)
  b1.hat <- coef(lm.a)[2] 
  b2.hat <- coef(lm.a)[3]
  b1.se <- se.coef(lm.a)[2]  
  b2.se <- se.coef(lm.a)[3]

  b1_cover.68[i] <- abs(b1-b1.hat) <b1.se  
  b2_cover.68[i] <- abs(b2-b2.hat) <b2.se  
}

#Proportion that fits within 68% CI
sum(b1_cover.68)/b   
sum(b2_cover.68)/b     



#(c) Repeat this simulation, but instead fit the model using t errors (see Exercise 6.6).
# More info on tlm() and hett, https://cran.r-project.org/web/packages/hett/hett.pdf
install.packages('hett')
library(hett)
b <- 1000
b1_cover.68 <- rep(NA, b)
b2_cover.68 <- rep(NA, b)
t.68 <- qt (.84, 4)  # 68% with df = 4

for(i in 1:b){
  y = 3 + 0.1*x1 + 0.5 * x2 + rt(nfake, df=4)*5
  tlm.c <- tlm(y ~ x1+x2, start = list(dof = 4))
  tcoef <- tsum(tlm.c$loc.fit, dispersion =1)$coefficients
  b1.hat <- tcoef[2,1] 
  b2.hat <- tcoef[3,1]
  b1.se <- tcoef[2,1]  
  b2.se <- tcoef[3,1]
  
  #standard error interval of t dist
#  b1_cover.68[i] <- abs(b1-b1.hat) <b1.se*t.68  
#  b2_cover.68[i] <- abs(b2-b2.hat) <b2.se*t.68  
  
  #standard error interval of normal
  b1_cover.68[i] <- abs(b1-b1.hat) <b1.se
  b2_cover.68[i] <- abs(b2-b2.hat) <b2.se
}

sum(b1_cover.68)/b   
sum(b2_cover.68)/b   

# x1 is within 68% interval
# x2 is not 

