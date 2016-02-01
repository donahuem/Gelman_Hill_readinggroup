## Chapter 4
## Question 4

# Mary Donovan
# 1/29/16

library(arm)
library(foreign)

# 4. Logarithmic transformations: the folder pollution contains mortality rates and various environmental factors from 60 U.S. metropolitan areas (see McDonald and Schwing, 1973). For this exercise we shall model mortality rate given nitric oxides, sulfur dioxide, and hydrocarbons as inputs. This model is an extreme oversimplification as it combines all sources of mortality and does not adjust for crucial factors such as age and smoking. We use it to illustrate log transformations in regression.
pollution <- read.dta("pollution.dta")
str(pollution)

# (a) Create a scatterplot of mortality rate versus level of nitric oxides. Do you think linear regression will fit these data well? Fit the regression and evaluate a residual plot from the regression.
plot(pollution$mort,pollution$nox)  ## terrible fit
nox.lm <- lm(pollution$nox~pollution$mort)
summary(nox.lm); display(nox.lm)
plot(residuals(nox.lm)~fitted(nox.lm)) # not pretty

# (b) Find an appropriate transformation that will result in data more appropriate for linear regression. Fit a regression to the transformed data and evaluate the new residual plot.
hist(pollution$nox)
hist(log(pollution$nox)) # log looks good
lognox.lm <- lm(log(pollution$nox)~pollution$mort)
plot(residuals(lognox.lm)~fitted(lognox.lm))

# (c) Interpret the slope coefficient from the model you chose in (b).
display(lognox.lm)
# as nitric oxide increases, mortality increases by 1%

# (d) Now fit a model predicting mortality rate using levels of nitric oxides, sulfur dioxide, and hydrocarbons as inputs. Use appropriate transformations when helpful. Plot the fitted regression model and interpret the coefficients.
mort.lm <- lm(log(nox)~mort+so2+log(hc),data=pollution)
plot(residuals(mort.lm)~fitted(mort.lm))
display(mort.lm)
curve(coef(mort.lm)[1]+coef(mort.lm)[2]*x+coef(mort.lm)[3]*x+coef(mort.lm)[4]*x,ylab="log prediction")

# (e) Cross-validate: fit the model you chose above to the first half of the data and then predict for the second half. (You used all the data to construct the model in (d), so this is not really cross-validation, but it gives a sense of how the steps of cross-validation can be implemented.)
mort.lm2 <- lm(log(nox)~mort+so2+log(hc),data=pollution[1:30,])
mort.lm2.pred <- predict(mort.lm2, newdata=pollution[31:60,])

plot(pollution$mort[1:30],mort.lm2$fitted.values,pch=19)
points(pollution$mort[31:60],mort.lm2.pred,pch=19,col="red")
legend("topright",legend=c("fitted","predicted"),pch=19,col=c("black","red"))
