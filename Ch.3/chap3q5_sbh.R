#Gelman Reading Group
#Chapter 3- Question 5- Courtney

rm(list=ls()) #cleared workspace

setwd("Ch.3/")
data<-read.csv("../data/beauty/ProfEvaltnsBeautyPublic.csv", header=T)
attach(data)

head(data)

#rename predictor and outcome variables
beaut<-btystdave
eval<-courseevaluation


#plot
plot(eval~beaut)

#Does beauty predict evaluation score when controling for tenure status (0=not tenured, 1= tenured)
lm.1<-lm(eval~beaut + tenured)
summary(lm.1) # I couldn't get the package arm loaded to use the display function

#Plot model with different colors for tenure status and add regression lines
colors <- ifelse (tenured==1, "black", "red") #black=tenured, red=not tenured
plot (eval~beaut, xlab="Beauty", ylab="Evaluation Score",
      col=colors, pch=20)
curve (cbind (1, 1, x) %*% coef(lm.1), add=TRUE, col="black",lwd=2)
curve (cbind (1, 0, x) %*% coef(lm.1), add=TRUE, col="red",lwd=2)

#Coeffient of the intercept is relatively meaningless because it means that if we expect a 4.02 point increase in scores if beauty and tenure status are both 0.
#beauty coeffient =  for the people that are not tenured, an increase of 1 point of beauty leads to a 0.12 point increase in score.
#Tenure coefficent = the difference in scores between tenured and non tenured teachers if beauty is held contstant at 0
##The residual SE = 0.55, meaning that our model can predict evaluation scores to an accuracy of 0.55 (or each score is on average 0.55 from the model prediction)

##Plotting residuals against fitted values
x.new2 <- data.frame (tenured=tenured, beaut=beaut,Fitted=fitted(lm.1))
res.1 <-resid(lm.1)
plot(res.1~x.new$Fitted)


#Does age predict evaluation score when controling for gender (1=female, 0=male) & the interaction of age and gender
lm.2<-lm(eval~age + female) #eval= outcome variable, age=predictor, female=input variable
summary(lm.2) 

#Coeffient of the intercept is relatively meaningless because it means that if we expect a 4.36 point increase in scores if age and gender are both 0.
#age coeffient =  for males, for each additional year of age, you will see a decrease in -.005 point in score.
#gender coefficent = the difference in scores between female and male teachers if age is held contstant at 0- this is meaningless because age can't = 0
##The residual SE = 0.55, meaning that our model can predict evaluation scores to an accuracy of 0.55 (or each score is on average 0.55 from the model prediction)

##I would like to go over how to interpret coefficients of interaction terms.

lm.3<-lm(eval~age* female)
summary(lm.3) 

#Plot model with different colors for tenure status and add regression lines
colors <- ifelse (female==1, "black", "red") #black=female, red=male
plot (eval~age, xlab="Age", ylab="Evaluation Score",
      col=colors, pch=20)
curve (cbind (1, 1, x) %*% coef(lm.2), add=TRUE, col="black",lwd=2)
curve (cbind (1, 0, x) %*% coef(lm.2), add=TRUE, col="red",lwd=2)

