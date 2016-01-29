#Gelman Chapter 4, Question 6

#Script by C.W.W. Counsell 
#Written on 28 January 2016

#question does not require coding.

#6___regression examining relations between average price, P, and quantity purchased, Q
#assume the following functional form, log Q = a + B log P; suppose the estimate for B is 0.3
#interpret this coefficient

#the coefficient, B 0.3, can be interpreted as the expected proportional change 
#in quantity purchased per proportional change in average price
#for a 1% difference in price, the model predicts a 0.3% difference in quantity purchased.

#if the model was instead, log Q = a + B * P then:
#B 0.3 would have implied that a difference of 1 in average price corresponded to an expected postive difference 
#of 0.3 in log quantity purchased.
#This would have meant for a difference of 1 in the average price, the difference in quanity purchased was exp(0.3)=1.35
#so an increase in quantity purchase of 35%.
