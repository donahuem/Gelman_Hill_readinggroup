model {
  
  for(i in 1:N){ 
    y[i] ~ dnorm(yhat[i],tau.y)
    yhat[i] <- a[block[i]] + b_sp*x[i,1] + b_nut*x[i,2]
  }
  b_sp ~ dnorm(0,.0001)
  b_nut ~ dnorm(0,.0001)
  tau.y <- pow(sigma.y,-2)
  sigma.y ~ dunif(0,100)
  
  for (j in 1:J){
    a[j] ~ dnorm(mu.a,tau.a)
  }
  mu.a ~ dnorm(0,0.0001)
  tau.a <- pow(sigma.a,-2)
  sigma.a ~ dunif(0,100)
  
}