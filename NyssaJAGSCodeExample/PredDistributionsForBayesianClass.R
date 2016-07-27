model{
  
  #priors
  #non-varying coefs
  a.Tide2~ dnorm(0, 1E-6)
  a.Prey~ dnorm(0, 1E-6)
  a.pred~ dnorm(0, 1E-6)
  
  b.exposure~dnorm(0,1E-6)
  a.exposure~dnorm(0,1E-6)
  
  #time
  sigma.b0.time~dunif(0,100)
  tau.b0.time<-1/sigma.b0.time^2
  
  sigma.a0.time~dunif(0,100)
  tau.a0.time<-1/sigma.a0.time^2
  
  #site
  sigma.b0.site~dunif(0,100)
  tau.b0.site<-1/sigma.b0.site^2
  sigma.a0.site~dunif(0,100)
  tau.a0.site<-1/sigma.a0.site^2
 
  #region 
  tau.a0.region<-1/sigma.a0.region^2
  sigma.a0.region~dunif(0,100)
  
  #overdispersion parameters
  tau.epsilon<-pow(sigma.epsilon,-2)
  sigma.epsilon~dunif(0,100)
  mu.over~dnorm(0,1E-6)
  
  #set-up means for multivariate distribution
  for (i in 1:4){
    xi[i]~dunif(0,100)
    mu_raw[i]~dnorm(0,1E-6)
    mu[i]<-xi[i]*mu_raw[i]
  }
  #name all the mus for easier interpretation
  mu.Prey<-mu[1]
  mu.Pred<-mu[2]
  mu.b0.region<-mu[3]
  mu.Tide2<-mu[4]
  
  #correlated errors for varying slopes using inverse Wishart
  Tau_B_raw[1:4,1:4]~dwish(W[,],5)
  Sigma_B_raw[1:4,1:4]<-inverse(Tau_B_raw[,])
  
  

  #Likelihood-----------------------------------------------
  for (i in 1:length(y)){
    #ecological process 
    log(lambda[i])<-
      b.Tide2[Region[i]]*((EmTi[i]-Tide_bar)/(2*Tide_sd))^2+ #tide squared
      b.Prey[Region[i]]*((Prey[i]-Prey_bar)/(2*Prey_sd))+ #prey
      b.pred[Region[i]]*((Crabs[i]-Crabs_bar)/(2*Crabs_std))+ #predators
         #varying intercepts
      b0.time[Time[i]]   +epsilon[i]  + b0.site[Sites[i]] #+ b0.region[Region[i]]
    
     #MIGHT CONSIDER SPLITTING TIME BY YEAR AND MONTH INSTEAD OF EACH INDIVIDUAL FOR A VARIANCE COMPONENT... DIFFERENT BETWEEN SEASONS vs years?
   #error prior
     epsilon[i]~dnorm(0,tau.epsilon)
   
     
      #occupancy proccess (the less south (the higher the aspect #) the better the habitat)
       psi[i]<-ilogit(#a.Tide*(EmTi[i]-Tide_bar)/(2*Tide_sd)+
        a.Tide2*((EmTi[i]-Tide_bar)/(2*Tide_sd))^2+
        a.Prey*((Prey[i]-Prey_bar)/(2*Prey_sd))+
        a.pred*((Crabs[i]-Crabs_bar)/(2*Crabs_std))+
        a0.time[Time[i]]  + a0.site[Sites[i]] )# + a0.region[Region[i]] 
    
    #zero-inflated part
    psi1[i]  <- min(0.999999,max(.000001,psi[i])) #workaround to not get an error
    z[i]~dbern(psi1[i])
    z.new[i]~dbern(psi1[i])
    
    #multiply my lambda by zero-inflated part
    mu.obs[i] <- lambda[i]*(z[i]+0.0001)
    
    #estimate of y
    y[i] ~ dpois(mu.obs[i])
     
    #simulate new data for posterior predicitve check
    y.new[i] ~ dpois(mu.obs[i])
    
    #accumlate test statistics for posterior predictive check
    sq[i] <- (y[i]-mu.obs[i])^2
    sq.rep[i] <-(y.new[i] - mu.obs[i])^2
  }
  
  
  #Varying intercept coefficients (Time, Site, Region) 
 #regions
  for (r in 1:4){
    a0.region[r]~dnorm(0, tau.a0.region) # is mu here suppose to be zero or varying??
    #a0.region[r]~dnorm(mu.a0.region[r], tau.a0.region) # if mu should vary use this
    
  }
  
  #Sites
  for (s in 1:n_Sites){ #sites within regions with wave exposure as a variable
    b0.site[s]~dnorm(mu.b0.site[s], tau.b0.site)
    mu.b0.site[s]<- b.exposure*(Exposure[s]-Exposure_bar)/(2*Exposure_sd) + b0.region[Region[s]]
    
    
    #a0.site[s]~dnorm(a0.region[Region[s]], tau.a0.site)
    a0.site[s]~dnorm(mu.a0.site[s], tau.a0.site) #I AM HAVING A HARD TIME PREDICTING SIGMA.A0.SITE....
    mu.a0.site[s]<- a.exposure*(Exposure[s]-Exposure_bar)/(2*Exposure_sd) + a0.region[Region[s]]
  }
  
  for (t in 1:4){ 
    # b0.time[t]~dnorm(mu.time[t], tau.b0.time) # should mu be zero?
    # a0.time[t]~dnorm(mu.time.a[t], tau.a0.time) 
    b0.time[t]~dnorm(0, tau.b0.time) # should mu be zero or varying?
    a0.time[t]~dnorm(0, tau.a0.time) 
    
    #site time--- another possibility since site and time are correlated and there is some data for both
    
    # for (s in 1:n_Sites){
    #  b0.sitetime[s,t]~dnorm(mu.b.sitetime[s,t], tau.b0.sitetime)
    #  mu.b.sitetime[s,t]<-b.SST*SST[s,t]+eta0
    
    # a0.sitetime[s,t]~dnorm(mu.a.sitetime[s,t], tau.a0.sitetime)
    #  mu.a.sitetime[s,t]<-a.SST*SST[s,t]+a.eta0
    #}
  }
  
  
  #could add transect level error here.....it is nested within site and region
  
  #Multivariate distribution for varying coefs
  for (r in 1:n_regions){ #region level
    
    
    #multivariate normal distribution 
   #put all the mus in a matrix
    B.hat[r,1]<-mu_raw[1]
    B.hat[r,2]<-mu_raw[2]
    B.hat[r,3]<-mu_raw[3]
    B.hat[r,4]<-mu_raw[4]
    
    #pull from a multivariate normal 
    B[r,1:4]~dmnorm(B.hat[r,],Tau_B_raw[,])
    
    #multiply by scaling factor for inverse wisshart
    b.Prey[r]<-xi[1]*B[r,1]
    b.pred[r]<-xi[2]*B[r,2]
    b0.region[r]<-xi[3]*B[r,3]
    b.Tide2[r]<-xi[4]*B[r,4]
  }
  
  
  #correlated errors using a Scaled inverse-Wisshart Model
  for (i in 1:4){
    sigma[i]<-xi[i]*sqrt(Sigma_B_raw[i,i])
  }
  # sigma.Tide<-sigma[1]
  sigma.Prey<-sigma[1]
  sigma.Pred<-sigma[2]
  sigma.b0.region<-sigma[3]
  sigma.Tide2<-sigma[4]
  
  #correlation coefs
  for (i in 1:4){
    for (j in 1:4){
      rho[i,j]<-Sigma_B_raw[i,j]/sqrt(Sigma_B_raw[i,i]*Sigma_B_raw[j,j])
    }
  }
  
  #name the coefs for easy interpretation
  rho.Prey_Pred<-rho[1,2]
  rho.Prey_b0<-rho[1,3]
  rho.Prey_Tide2<-rho[1,4]
  rho.Pred_b0<-rho[2,3]
  rho.Pred_Tide2<-rho[2,4]
  rho.b0_Tide2<-rho[3,4]
  
  #posterior predictive checks-- calculate Bayesian p-value
  #pvalue CV
 # cv.y <- sd(y[])/mean(y[])
  #  cv.y.rep <- sd(y.new[])/mean(y.new[])
  #  pvalue.cv <- step(cv.y.rep-cv.y) # find Bayesian P
  #value--the mean of many 0's and 1's returned by
  #the step function, one for each step in the chain
  
  #pvalue mean
  mean.y <-mean(y[])
  mean.y.rep <-mean(y.new[])
  # sigma.y<-mean(sigma.model[])
  pvalue.mean <-step(mean.y.rep - mean.y)
  
  #pvalue fit---this is what we report
  fit <- sum(sq[])
  fit.new <- sum(sq.rep[])
  pvalue.fit <- step(fit.new-fit)# Test whether new data set more extreme
  #pvalue.fit<-mean(test) #Bayesian p-value
  
  
  
  
  #predictions--------------------------This is probably all wrong right now
  
  #average over sites.....??
  #b0.site.all<- b.exposure*mean(Exposure) + mean(b0.region[Region])
  
  for (i in 1:length(x.hat)){
    
    for (r in 1:4){
      
      y.hat[i,r]<-#b.Tide[Region[i]]*(EmTi[i]-Tide_bar)/(2*Tide_sd)+
        exp(b.Tide2[Region[i]]*((x.hat[i]+Tide_bar)*(2*Tide_sd))^2+
              mean(b.Prey[Region[i]]*((Prey[i]+Prey_bar)*(2*Prey_sd)))+
              mean(b.pred[Region[i]]*((Crabs[i]+Crabs_bar)*(2*Crabs_std)))+
              mean(b0.time[Time[i]])  + b0.region[Region[i]] +epsilon[i] + mu.over)
    }
  }
  # End of model
  
}