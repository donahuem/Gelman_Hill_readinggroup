#Gelman Chapter 5, Question 1

#Script by C.W.W. Counsell 
#Written on 4 February 2016

#clear the workspace---------------------------------------
rm(list=ls())

#load necessary packages
library(foreign) #needed for read.dta

setwd(file.path('C:/Users/Chelsie/Documents/RClass/Donahue')) #connect to correct working directory

#________load data files_______________

nes <-read.dta("~/RClass/Donahue/nes/nes5200_processed_voters_realideo.dta")

#subset the data for just year==1992
nes92<-subset(nes,year==1992)

#1a___fit a logistic regression predicting support for Bush given all input listed below 
#income, sex, ethnicity, education, party id, political ideology
#conside how to include these as regression predictors and consider possible interactions

#first pass at model leaving inputs in their original form
#note - would be better to work with these as many are currently categorical
fit1<-glm(presvote~income+gender+race+educ1+partyid3_b+real_ideo, data=nes92,family=binomial(link="logit"))

#1b___evaluate and compare the different models you have fit
#consider coefficient estimates and standard errors, residual plots, and deviances

#I have only run one model following the  provided data's setup
summmary(fit1)

#1c___for your chosen model - discuss and compare the importance of each input variable in the prediction
#it seems like party id and ideology are quite important predictors
#race and income have some importance as well
