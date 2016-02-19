# Chap 7
# 1. Discrete probability simulation: suppose that a basketball player 
# has a 60% chance of making a shot, and he keeps taking shots until 
# he misses two in a row. Also assume his shots are independent 
# (so that each shot has 60% probability of success, no matter what happened before).

# (a) Write an R function to simulate this process.
# (b) Put the R function in a loop to simulate the process 1000 times. Use the simulation to estimate the mean, standard deviation, and distribution of the total number of shots that the player will take.
# (c) Using your simulations, make a scatterplot of the number of shots the player will take and the proportion of shots that are successes.

#rbinom(n, size, prob)
# n: no of observations
# size: no of trials
# prob: pro of success

n.sims <- 1000
n.balls <- rep(NA, 1000)
n.succ <- rep(NA, 1000)
for (s in 1:n.sims) {
    i=2
    shot <- NA
    shot[1] <- rbinom(1,1,.6)
    while(i>1) {
      shot[i] <- rbinom (1, 1, .6)
      if(shot[i]==0 & shot[i-1]==0) break
      i=1+i
    }
    
    n.balls[s] <- i; n.succ[s] <- sum(shot==1)
}

hist (n.balls, main="Simulated no of shots until 2 fails (p=.6)")
mean(n.balls)
#8.865
sd(n.balls)
#7.65
plot(n.balls, n.succ)
