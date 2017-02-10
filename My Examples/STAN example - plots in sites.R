#STAN example - plots in sites

#something is wrong with the pooled variance example, but otherwise working. it is something basic
#like the way that I am (or not) transforming between logn and n

library(rstan)
library(shinystan)

nreps = 20  #replicate observations per plot
nplots = 4  #plots per site
S = 10     #total number of sites
J = nplots * S  #total number of plots (4 per site)
N = nreps*nplots*10 

x <- rgamma(N,5,2)

plotnum <- rep(c(1:J),each=nreps)   #list of plot numbers for each observation (length N)
sitenum <- rep(c(1:10),each=nplots)  #list of site numbers for each plot (length J)

#mu_a and sig_a are the mean and variance of the intercept across sites
mu_a <- 5
sig_a <- 2
#mu_b and sig_b are the mean and variance of the slope across sites
mu_b <- 4
sig_b <- 1
#draw S=10 site means for slope and intercept
a_site <- rnorm(S,mu_a,sig_a)
b_site <- rnorm(S,mu_b,sig_b)

#UNPOOLED WITHIN-SITE VARIANCE separate sds for slope and intercept (variability of plots within sites)
#here, give specific values for each site within-site variance
# sig_a_site <- seq(1:5, by=1/S)
# sig_b_site <- seq(0.1:2, by=1/S)

#POOLED WITHIN-SITE VARIANCE: within site variance drawn from same distribution 
  #within site variances of intercepts are drawn from a shared Gamma(10,10)
mu_sig_a_site <-1 #gam1_sig_a_site <- 10
sig_sig_a_site <- 0.1 #gam2_sig_a_site <- 10
sig_a_site <- rlnorm(S, mu_sig_a_site, sig_sig_b_site) #rgamma(S,gam1_sig_a_site,gam2_sig_a_site)
  #within site variances of intercepts are drawn from a shared Gamma(5,10)
mu_sig_b_site <- 0.5  #gam1_sig_b_site <- 5
sig_sig_b_site <- 0.1 #gam2_sig_b_site <- 10
sig_b_site <- rlnorm(S, mu_sig_b_site, sig_sig_b_site) # rgamma(S,gam1_sig_b_site,gam2_sig_b_site)

# #SINGLE VALUE: all sites have the same within-site variance for intercept and for slope
# sig_a_site <- rgamma(1,gam1_sig_a_site,gam2_sig_a_site)
# sig_b_site <- rgamma(1,gam1_sig_b_site,gam2_sig_b_site)


#for each plot, draw the slope and intercept from the appropriate site mean and sd
a_plot <- rep(0,J)
b_plot <- rep(0,J)

#different variances for each site (EITHER POOLED or UNPOOLED case)
for (j in 1:J){
  a_plot[j] <- rnorm(1,a_site[sitenum[j]], sig_a_site[sitenum[j]]);
  b_plot[j] <- rnorm(1,b_site[sitenum[j]], sig_b_site[sitenum[j]]);
}

# #Alternatively, assume same within-site variance for all sites (SINGLE VALUE case)
# for (j in 1:J){
#   a_plot[j] <- rnorm(1,a_site[sitenum[j]], sig_a_site);
#   b_plot[j] <- rnorm(1,b_site[sitenum[j]], sig_b_site);
# }


#draw three observations from each plot with sd = sig_y
sig_y <- 1
y <- rep(0,N)
for (n in 1:N){
  yhat[n] <- a_plot[plotnum[n]] + b_plot[plotnum[n]]*x[n]
  y[n] <- a_plot[plotnum[n]] + b_plot[plotnum[n]]*x[n] + rnorm(1,0,sig_y)
}

#plot data
allsites <- rep(c(1:10),each=12) #for plotting, list of site numbers for each obs
plot(y~x,col=allsites,type='n')
text(x,y,plotnum, cex=0.5, col=allsites)

#call stan model
dat <- list(N=N,S=S,J=J,plotnum=plotnum, sitenum=sitenum,y=y,x=x) 
fitme <- stan("My Examples/threelevel_plotsinsites.stan", data=c("N","J","S","plotnum", "sitenum","y","x"), iter=4000, chains=3, control=list(adapt_delta=.9))

mu_a
mu_b
sig_a
sig_b
sig_y
sig_a_site
sig_b_site
mean_sig_a_site <- exp(mu_sig_a_site + 0.5*sig_sig_a_site^2)
sd_sig_a_site <- (exp(sig_sig_a_site^2)-1)*exp(2*mu_sig_a_site+sig_sig_a_site^2)
mean_sig_b_site <- exp(mu_sig_b_site + 0.5*sig_sig_b_site^2)
sd_sig_b_site <- (exp(sig_sig_b_site^2)-1)*exp(2*mu_sig_b_site+sig_sig_b_site^2)
a_plot
b_plot
a_site
b_site


#launch_shinystan(fitme)
