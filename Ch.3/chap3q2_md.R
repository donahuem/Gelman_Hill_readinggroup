## Chapter 3
## Question 2

# Mary Donovan
# 1/29/16

#Suppose that, for a certain population, we can predict log earnings from log height as follows: A person who is 66 inches tall is predicted to have earnings of $30,000. Every increase of 1% in height corresponds to a predicted increase of 0.8% in earnings.The earnings of approximately 95% of people fall within a factor of 1.1 of predicted values.

# a) Give the equation of the regression line and the residual standard deviation of the regression.
slope <- (0.008/0.01)
intercept <- log(30000) - slope*log(66)

# log(earnings) = 6.96 + 0.8*log(x)

# 1.1 is equal to 1.96 standard deviations so, one standard deviation is:
(sdev <- 1.1/(1.96))

# b) Suppose the standard deviation of log heights is 5% in this population. What, then, is the R2 of the regression model described here?
(R2 <- 1-((0.05^2)/(sdev^2)))          # equation from pg. 41
# not sure this is right...
