# Chapter 4, Question 3 from Gelman & Hill 2007

#For this question, I think we are drawing these graphs
#without fitting the actual data, but just based on the output.

#(a) On a graph of weights vs age, draw the fitted regression from the first model
# the first model 
#lm(formula = weight ~ age10) R output
#              coef.est   coef.se
# (Intercept)    161.0      7.3
# age10            2.6      1.6
# n = 2009, k = 2
# residual sd = 119.7, R-Squared = 0.00

a <- seq(18,85,1)
plot(a, 161 + 2.6/10*a, type="l",xlim=c(18,80),ylim=c(150,200),ylab="Weight (lbs)", xlab="Age (yr)", main="Ch 4, Q3")

#(b) On the same graph, draw the fitted regression line from the second model.
# lm(formula = weight ~ age10 + age10.sq)
#             coef.est coef.se
# (Intercept)    96.2    19.3
# age10         33.6      8.7
# age10.sq      -3.2      0.9
# n = 2009, k = 3
# residual sd = 119.3, R-Squared = 0.01

lines(a, 96.2 + 33.6/10*a -3.2/100*a^2, xlim=c(18,80),type="l",ylab="Weight (lbs)", xlab="Age (yr)", main="Ch 4, Q3a", col="blue")

# (c) On another graph with the same axes and scale, draw the fitted regression
# line from the third model. (It will be discontinuous.)
# lm(formula = weight ~ age30.44 + age45.64 + age65up)
#               coef.est coef.se
# (Intercept)    157.2    5.4
# age30.44TRUE    19.1    7.0
# age45.64TRUE    27.2    7.6
# age65upTRUE      8.5    8.7
# n = 2009, k = 4
# residual sd = 119.4, R-Squared = 0.01

seg1 <- rep(157.2,30-17)
seg2 <- rep(157.2+19.1, 45-30)
seg3 <- rep(157.2+27.2, 65-45)
seg4 <- rep(157.2+8.5, 85-65)
w3 <- c(seg1,seg2,seg3,seg4)
lines(a,w3,col="red")
