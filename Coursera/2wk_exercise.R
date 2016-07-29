#Q1
qnorm(c(0.025,0.975),mean=10, sd=sqrt(5))

#Q2
qbeta(c(0.05,0.95),shape1 =2, shape2=5)

#Q3
qgamma(c(0.005,0.995),shape=4, rate=8)

#Q4
qbeta(c(0.025,0.975),2586+1,1+2414)
#[1] 0.5033405 0.5310327
#Answer # 2

#Q5
#2. The probability that the true proportion of females lies in this interval is 0.95.

#Q6.
qbeta(c(0.025,0.975),shape1=500+2586,shape2 = 500+2414)
#[1] 0.5016840 0.5269736
#Answer #4

#Q7.
qbeta(0.5,shape1=5,shape2 = 200)
#[1] 0.022859
#Answer 1

#Q8
qbeta(c(0.025,0.975),shape1=5+2586,shape2 = 200+2414)
#[1] 0.4842100 0.5113728
#Answer 4

#Q9
#uniform prior: Beta(1,1); update with binomial
qbeta(c(0.05,0.95), shape1 = 1+table(brfss$exercise)[1],shape2 = 1 + table(brfss$exercise)[2])
#[1] 0.7636949 0.7831620
#Answer 2

#Q10
obs <- c(2,3,4,5,4)
a = 4 + sum(obs)
b = 1 + length(obs)

#Q11
x <- seq(0,20,0.001)
gam_a1_b5 <- dgamma(x,shape=1,rate=5)
gam_a5_b1 <- dgamma(x,shape=5,rate=1)
gam_a100_b500 <- dgamma(x,shape=100,rate=500)
gam_a500_b100 <- dgamma(x,shape=500,rate=100)
plot(x,gam_a1_b5,col="black",type="l",pch="none")
lines(x,gam_a5_b1,col="blue")
lines(x, gam_a100_b500,col="green")
lines(x,gam_a500_b100,col="red")
#Answer is 2. a=5, b=1;  mean of gamma is a/b

#Q12
sumx = sum(table(brfss$fruit_per_day)*seq(0,9,1))  #total number of observations
n = sum(table(brfss$fruit_per_day))
a = 5+sumx
b = 1 + n
c(a,b)
#[1] 8119 5001


#Q13
qgamma(c(0.05,0.95),a,b)
#[1] 1.593953 1.653225

#Q14
x = seq(0,10,0.01)
plot(x,dgamma(x,a,b),xlim = c(0,10),type="l")
#No.

#Q15
sumx = sum(table(brfss$vege_per_day)*seq(0,7,1))  #total number of observations
n = sum(table(brfss$vege_per_day))
a = 5+sumx
b = 1 + n
c(a,b)
x = seq(0,10,0.01)
plot(x,dgamma(x,a,b),xlim = c(0,10),type="l")
