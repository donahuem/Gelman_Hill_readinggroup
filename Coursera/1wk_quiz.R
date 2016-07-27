#Question 1

Pr_Y = .85
Pr_G = .15
P_ry_Y = 2/3
P_rg_Y = 1/3
P_ry_G = 1/3
P_rg_G = 2/3

#data
n=5  #5 witnesses
k=4  #4 said green cab

#likelihoods
Lik_data_G = dbinom(k,n,P_rg_G)
Lik_data_Y = dbinom(k,n,P_rg_Y)

#Posterior prob of green cab
Post_G <- Pr_G*Lik_data_G/(Pr_G*Lik_data_G + Pr_Y*Lik_data_Y)

#Question 2
#Likelihood is the probability of the data given the model

#Question 3

lambda = c(1,2,4)  #models
priors = c(1/3,1/3,1/3)  #prior probability of all three models is 1/3
x = 3  #data

Lik_data_lambda <- dpois(x,lambda)

Post_lambda_data <- priors * Lik_data_lambda/(sum(priors*Lik_data_lambda))

#lambda>2 has highest posterior (but note that only model of lambda>2 was lambda=4)

#Question 4:  2 identical expts vs 1 big expt - same posterior?
#Make example to illustrate
# Expt 1:  5 coin flips with 4 heads; Expt 2:  5 coin flips with 4 heads
#1 big expt : 10 coin flips with 8 heads
#do these generate the same posterior prob for p?  YES

p = seq(0,1,.01)       #models for the value of p
priors = rep(.01,101)  #uninformative prior on p

like_p_data <- dbinom(4,5,p)
denom1 = sum(like_p_data*priors)

post_p_data1 <- priors*like_p_data/denom1

denom2 <- sum(like_p_data*post_p_data1)
post_p_data2 <- post_p_data1*like_p_data/denom2

like_p_data3 <- dbinom(8,10,p)
denom3 <- sum(like_p_data3*priors)
post_p_data3 <- priors*like_p_data3/denom3

plot(p,priors,col="black",type="l",ylim=c(0,.05))
lines(p,post_p_data1,col = "lightblue")
lines(p,post_p_data2,col = "blue")
points(p,post_p_data3,col = "red")

#Question 5
#If conditions identical to tomorrow occurred an infinite number of times

#Question 6
#By making a stopping rule...

#Question 7
#<1% regular are tropical
# alpha = 0.1
# data:  5/300 regular are tropical
#H0: p<0.01; H: p>0.01
#What is the probability of getting 5/300 or more tropical skittles if p<=0.01?

#pbinom(4,300,.01) = .816 --> P(>4 events out of 300 trials given p=0.01)
#dbinom(5,300,.01) = 0.101 --> P(exactly 5 events of 300 trials given p=0.01)
#sum(dbinom,5:300,300,0.01) --> P(>=5 events occur in 300 trials given p=1%)
#This is supposed to be a frequentist approach

sum(dbinom(5:300,size=300,p=0.01))


#Question 8
#N possible lottery combinations: multiple of 100m and <1b
#413,271,201 people entered and 3 won
#binomial distribution with: N choose k with probablity p
#n is 413271201, k is 3, p is 1/N
# What is the posterior prob that <600,000,000 combinations

N <-seq(100,900,100)*1000000
p <- 1/N
n <- rep(413271201,length(p))
k <-3
prior <- rep(1/length(p),length(p))

like <- dbinom(k,n,p)
post <- prior*like/sum(prior*like)
plot(N,prior,ylim=c(0,0.5),col="red")
points(N,like,col="blue")
points(N,post,col="purple")
sum(post[1:5])


#Question 9
#David: 1 typo/pg, Amy: 2 typos/pg, Joe: 3 typos/pg
#data: 1 page with 10 typos
# find posterior prob that TA was Joe

lambda <- c(1,2,3)
prior <- rep(1/3,3)
like <- dpois(10,lambda)
post <- prior*like/sum(prior*like)
plot(lambda,prior,ylim=c(0,1),col="red")
points(lambda,like,col="blue")
points(lambda,post,col="purple")
post[3]
