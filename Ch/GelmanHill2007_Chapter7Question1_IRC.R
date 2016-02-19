#Gelman & Hill - Hierarchical Models
#Chapter 7 Questions

#Question 1
# Discrete probability simulation: suppose that a basketball player has a 60%
# chance of making a shot, and he keeps taking shots until he misses two in a
# row. Also assume his shots are independent (so that each shot has 60% probability
#                                             of success, no matter what happened before).
# (a) Write an R function to simulate this process.

basketballShots <- function(probOfSuccess) {
  firstTwoShots = rbinom(2, 1, probOfSuccess)
  allShots = firstTwoShots
  numShots = length(allShots)
  sumTwoLastShots = allShots[numShots] + allShots[numShots-1] 
  while(sumTwoLastShots > 0) {
    newShot <- rbinom(1, 1, probOfSuccess)
    allShots <- append(allShots, newShot)
    numShots = length(allShots)
    sumTwoLastShots = allShots[numShots] + allShots[numShots-1]
  }
  return(allShots)
}

# (b) Put the R function in a loop to simulate the process 1000 times. Use the
# simulation to estimate the mean, standard deviation, and distribution of the
# total number of shots that the player will take.
numShotsVect = c()
numSuccesses = c()
for (i in 1:1000) {
  newShots = basketballShots(0.6)
  numShotsVect = append(numShotsVect, length(newShots))
  numSuccesses = append(numSuccesses, sum(newShots))
}
meanShots = mean(numShotsVect)
stdevShots = sd(numShotsVect)
hist(numShotsVect) #has a skewed distribution towards the low end.

# (c) Using your simulations, make a scatterplot of the number of shots the player
# will take and the proportion of shots that are successes.
propSuccesses = numSuccesses/numShotsVect
plot(numShotsVect, propSuccesses)

