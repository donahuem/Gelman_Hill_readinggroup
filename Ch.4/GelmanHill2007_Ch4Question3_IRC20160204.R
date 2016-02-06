#Gelman & Hill (2007) Chapter 4, Question 3

rm(list = ls()) #remove all past worksheet variables

#First, create a mock dataset with ages from 18 to 100
age <- c(18:100) #create a vector of ages

#Now create the new variables (age10 & age10.sq) 
age10 <- age/10
age10.sq <- age10^2

#Put those variables into a dataframe
weightVsAge <- data.frame(age, age10, age10.sq)

#Now recode the variables to create the 
# indicators (age18.29, age30.44, age45.64 and age65up)
weightVsAge$age18.29 <- NA #first create a new column
weightVsAge$age18.29[weightVsAge$age %in% 18:29] <- 1 #recode all values within 18-29 as 1
weightVsAge$age18.29[!weightVsAge$age %in% 18:29] <- 0 #recode all other values as 0

#Do the same for the rest of the indicators
weightVsAge$age30.44 <- NA #first create a new column
weightVsAge$age30.44[weightVsAge$age %in% 30:44] <- 1 #recode all values within 18-29 as 1
weightVsAge$age30.44[!weightVsAge$age %in% 30:44] <- 0 #recode all other values as 0

weightVsAge$age45.64 <- NA #first create a new column
weightVsAge$age45.64[weightVsAge$age %in% 45:64] <- 1 #recode all values within 18-29 as 1
weightVsAge$age45.64[!weightVsAge$age %in% 45:64] <- 0 #recode all other values as 0

weightVsAge$age65up <- NA #first create a new column
weightVsAge$age65up[weightVsAge$age >= 65] <- 1 #recode all values within 18-29 as 1
weightVsAge$age65up[weightVsAge$age < 65] <- 0 #recode all other values as 0

#Now get values of y for each of the models
weightVsAge$weight.lm1 = 161 + (2.6*weightVsAge$age10)
weightVsAge$weight.lm2 = 96.2 + (33.6*weightVsAge$age10) + (-3.2*weightVsAge$age10.sq)
weightVsAge$weight.lm3 = 157.2 + (19.1*weightVsAge$age30.44) + (27.2*weightVsAge$age45.64) + (8.5*weightVsAge$age65up)

#Find out which of the models returns the highest weight for plotting
maxWeightLm1 = max(weightVsAge$weight.lm1) #187
maxWeightLm2 = max(weightVsAge$weight.lm2) #184.392
maxWeightLm3 = max(weightVsAge$weight.lm3) #184.4

#Set up a blank plotting area for 2 plots
par(mfrow=c(1,2))

#Graph the first regression
# -Since the first model has the highest weight I will plot with that one
plot(x = weightVsAge$age, y = weightVsAge$weight.lm1, xlab = "Age (in years)", ylab = "Weight (in pounds)", type = "n")
colors = rainbow(3) #get 3 colors for lines
lines(x = weightVsAge$age, y = weightVsAge$weight.lm1, col = colors[1])

#Graph the second regression on the same graph
lines(x = weightVsAge$age, y = weightVsAge$weight.lm2, col = colors[2])

#Graph the third regression on a new graph with the same axes
plot(x = weightVsAge$age, y = weightVsAge$weight.lm1, xlab = "Age (in years)", ylab = "Weight (in pounds)", type = "n")
lines(x = weightVsAge$age, y = weightVsAge$weight.lm3, col = colors[3])
