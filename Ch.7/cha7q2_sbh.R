#7.2
# Continuous probability simulation: the logarithms of weights (in pounds) 
# of men in the United States are approximately normally distributed 
# with mean 5.13 and standard deviation 0.17; women with mean 4.96 
# and standard deviation 0.20. Suppose 10 adults selected at 
# random step on an elevator with a capacity of 1750 pounds. 
# What is the probability that the elevator cable breaks?

n.sims <- 1000
sum.wt <- rep(NA, n.sims)

for(s in 1:n.sims) {
    sex<- rbinom(10,1,.5)
    # sex 0 = man, sex 1 = woman
    tot_wt <- sum(exp(rnorm(sum(sex==0), 5.13, .17)),exp(rnorm(sum(sex==1), 4.96, .2)))
    sum.wt[s] <- ifelse((1750 < tot_wt), 1, 0) 
  }

p<- sum(sum.wt==1)/1000
# probably that the evelator cable breaks is 0.062
