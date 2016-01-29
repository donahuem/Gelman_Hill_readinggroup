#Gelman Chapter 3, Question 4

#Script by C.W.W. Counsell 
#Written on 28 January 2016

#clear the workspace---------------------------------------
rm(list=ls())

#load necessary packages
library(foreign) #needed for data

setwd(file.path('C:/Users/Chelsie/Documents/RClass/Donahue')) #connect to correct working directory

#________load data files_______________

iq.data <-read.dta("~/RClass/Donahue/child.iq/child.iq.dta")

#4a___fit a regression of child test scores on mother's age, display data and fitted model; check assumptions and interpret the slope coefficient
#at what age do you recommend mothers give birth, what assumptions are you making with these recommendations

fit.4a <- lm(ppvt~momage, data=iq.data) #fit a regression of child test scores on mom's age
plot(iq.data$momage, iq.data$ppvt, xlab="Mother's Age", ylab="Child Test Score at Age 3") #display data
curve(coef(fit.4a)[1]+coef(fit.4a)[2]*x, add=TRUE) #display fitted model
summary (fit.4a)
#the effect of age seems relatively small, 0.84 points per year of age, also there is a lot of variation in the data
#one thing to note is the intercept is a bit nonsensical - at mother's age 0, child test scores at 67.8

#4b___Now include mother's education and interpret both slope coefficients in this model; have your conclusions changed?

fit.4b <- lm(ppvt~momage+educ_cat, data=iq.data) #fit a regression of child test scores on mom's age and educ_cat
summary (fit.4b)
#this is a bit tricky - educ cat has discrete values 1 to 4, in this form it is being treated as continuous
#when the moms age is held constant, a change of 1 in the mom's education category corresponds to a 4.7 increase in the child's test score
#when the moms education category is held constant, a change of 1 in the mom's age corresponds to a 0.34 increase in the child's test scores
#It still seems like age isn't that important, but education class is

#4c___Now create an indicator variable for whether the mother completed high school or not

iq.data$momHS <- ifelse(test=iq.data$educ_cat>1,yes=1,no=0)  #unclear what the educational categories are - did not find this definition, just used 1 is non highschool and highernumbers are different levels from highschool completed and more
#This organization should make sense as in fit.4b there was a positive slope with educ_cat and kids score supporting the idea that higher education has higher educ_cat values
#now in momHS 1 corresponds to educ_cat 2,3,or4, while 0 corresponds to educ_cat 1 

#consider interactions between high school completion and age
fit.4c<-lm(ppvt~momage+momHS+momage:momHS,data=iq.data)
summary(fit.4c) #now mom age and high school completion have a negative effect estimate, the interaction has a postitive effect estimate

#plot with separate regression lines for each high school completion group
colors<-ifelse (iq.data$momHS==1, "black","gray")
plot(iq.data$momage, iq.data$ppvt, xlab="Mother's Age", ylab="Child Test Score at Age 3",col=colors,pch=20) #display data
curve(cbind(1,x,1,1*x) %*% coef(fit.4c), add=TRUE, col="black") #display fitted model
#above is matrix version of: curve(coef(fit.4c)[1]+coef(fit.4c)[2]*x+coef(fit.4c)[3]*1+coef(fit.4c)[4]*1*x, add=TRUE, col="black") 
curve(cbind(1,x,0,0*x) %*% coef(fit.4c), add=TRUE, col="gray") #display fitted model

#it appears that if you completed high school, being older is better, and if didn't complete high school being older is worse for the kid's test score

#4d___fit regression of child test scores on mother's age and education level for first 200 children 

#split data into first 200 and second 200 children
first<-iq.data[1:200,]
test<-iq.data[201:400,]

#fit regression of child test scores on mother's age and education level for first 200
fit.4d <- lm(ppvt~momage+educ_cat, data=first) #fit a regression of child test scores on mom's age and educ_cat

#use model to predict test scores for next 200
p.scores<-predict(fit.4d, test,level=0.95) #use model to predict for values in test dataframe 

#graphically display comparisons of predicted and actual scores for the other 200 children
plot(p.scores, test$ppvt, xlab="Predicted Scores",ylab="Actual Scores")
abline(0,1)

#as noted in the beginning, there is a lot of variation. the model predicts as well as expected given the variation
