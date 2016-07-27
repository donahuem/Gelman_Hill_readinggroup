cat("model{
    # Define non-varying priors:
    tau.y <- pow(sig.y, -2)
    sig.y ~ dunif(0,100)

    # Define means of alphas (pres/absence) and betas (density) for multivar distribution
    mu.a0 <- xi.a0*mu.a0.raw
    mu.a1 <- xi.a1*mu.a1.raw
    mu.a2 <- xi.a2*mu.a2.raw
    mu.a3 <- xi.a3*mu.a3.raw
    mu.a4 <- xi.a4*mu.a4.raw
    mu.a5 <- xi.a5*mu.a5.raw
    mu.a6 <- xi.a6*mu.a6.raw
    mu.a7 <- xi.a7*mu.a7.raw
    mu.a8 <- xi.a8*mu.a8.raw
    mu.a9 <- xi.a9*mu.a9.raw
    
    mu.a0.raw ~ dnorm(0, 0.001)
    mu.a1.raw ~ dnorm(0, 0.001)
    mu.a2.raw ~ dnorm(0, 0.001)
    mu.a3.raw ~ dnorm(0, 0.001)
    mu.a4.raw ~ dnorm(0, 0.001)
    mu.a5.raw ~ dnorm(0, 0.001)
    mu.a6.raw ~ dnorm(0, 0.001)
    mu.a7.raw ~ dnorm(0, 0.001)
    mu.a8.raw ~ dnorm(0, 0.001)
    mu.a9.raw ~ dnorm(0, 0.001)
    
    xi.a0 ~ dunif(0,100)
    xi.a1 ~ dunif(0,100)
    xi.a2 ~ dunif(0,100)
    xi.a3 ~ dunif(0,100)
    xi.a4 ~ dunif(0,100)
    xi.a5 ~ dunif(0,100)
    xi.a6 ~ dunif(0,100)
    xi.a7 ~ dunif(0,100)
    xi.a8 ~ dunif(0,100)
    xi.a9 ~ dunif(0,100)

    mu.b0 <- xi.b0*mu.b0.raw
    mu.b1 <- xi.b1*mu.b1.raw
    mu.b2 <- xi.b2*mu.b2.raw
    mu.b3 <- xi.b3*mu.b3.raw
    mu.b4 <- xi.b4*mu.b4.raw
    mu.b5 <- xi.b5*mu.b5.raw
    mu.b6 <- xi.b6*mu.b6.raw
    mu.b7 <- xi.b7*mu.b7.raw
    mu.b8 <- xi.b8*mu.b8.raw
    mu.b9 <- xi.b9*mu.b9.raw
    
    mu.b0.raw ~ dnorm(0, 0.001)
    mu.b1.raw ~ dnorm(0, 0.001)
    mu.b2.raw ~ dnorm(0, 0.001)
    mu.b3.raw ~ dnorm(0, 0.001)
    mu.b4.raw ~ dnorm(0, 0.001)
    mu.b5.raw ~ dnorm(0, 0.001)
    mu.b6.raw ~ dnorm(0, 0.001)
    mu.b7.raw ~ dnorm(0, 0.001)
    mu.b8.raw ~ dnorm(0, 0.001)
    mu.b9.raw ~ dnorm(0, 0.001)
    
    xi.b0 ~ dunif(0,100)
    xi.b1 ~ dunif(0,100)
    xi.b2 ~ dunif(0,100)
    xi.b3 ~ dunif(0,100)
    xi.b4 ~ dunif(0,100)
    xi.b5 ~ dunif(0,100)
    xi.b6 ~ dunif(0,100)
    xi.b7 ~ dunif(0,100)
    xi.b8 ~ dunif(0,100)
    xi.b9 ~ dunif(0,100)

    # set up covariance matrix using inverse-Wishart model, with df = number of coefficients (including intercept) + 1
    Tau.A.raw[1:10,1:10,1] ~ dwish(W.A[,], A.df)
    A.df <- nalpha+1+1
    Sigma.A.raw[1:10,1:10] <- inverse(Tau.A.raw[,,])

    Tau.B.raw[1:10,1:10,1] ~ dwish(W.B[,], B.df)
    B.df <- nbeta+1+1
    Sigma.B.raw[1:10,1:10] <- inverse(Tau.B.raw[,,])

    #define model
    for (i in 1:N) {
    #define lognormal model for the mean given z=1; mean of lnorm is log(mu)
    mu[i] <- b0[IS[i]] + b1[IS[i]]*Xb[i,1] + b2[IS[i]]*Xb[i,2] + b3[IS[i]]*Xb[i,3] + b4[IS[i]]*Xb[i,4] + b5[IS[i]]*Xb[i,5] + b6[IS[i]]*Xb[i,6] + b7[IS[i]]*Xb[i,7] + b8[IS[i]]*Xb[i,8] + b9[IS[i]]*Xb[i,9]
    phi[i] <- ilogit(a0[IS[i]] + a1[IS[i]]*Xa[i,1] + a2[IS[i]]*Xa[i,2] + a3[IS[i]]*Xa[i,3] + a4[IS[i]]*Xa[i,4] + a5[IS[i]]*Xa[i,5] + a6[IS[i]]*Xa[i,6] + a7[IS[i]]*Xa[i,7] + a8[IS[i]]*Xa[i,8] + a9[IS[i]]*Xa[i,9])
    z[i] ~ dbern(phi[i])
    y[i] ~ dnorm(z[i]*mu[i],tau.y)
    
    #Simulations:
    #First, simulation of altered human and other drivers:
    mu.humansim[i] <- b0[IS[i]] + b1[IS[i]]*Xb.humansim[i,1] + b2[IS[i]]*Xb.humansim[i,2] + b3[IS[i]]*Xb.humansim[i,3] + b4[IS[i]]*Xb.humansim[i,4] + b5[IS[i]]*Xb.humansim[i,5] + b6[IS[i]]*Xb.humansim[i,6] + b7[IS[i]]*Xb.humansim[i,7] + b8[IS[i]]*Xb.humansim[i,8] + b9[IS[i]]*Xb.humansim[i,9]
    phi.humansim[i] <- ilogit(a0[IS[i]] + a1[IS[i]]*Xa.humansim[i,1] + a2[IS[i]]*Xa.humansim[i,2] + a3[IS[i]]*Xa.humansim[i,3] + a4[IS[i]]*Xa.humansim[i,4] + a5[IS[i]]*Xa.humansim[i,5] + a6[IS[i]]*Xa.humansim[i,6] + a7[IS[i]]*Xa.humansim[i,7] + a8[IS[i]]*Xa.humansim[i,8] + a9[IS[i]]*Xa.humansim[i,9])
    z.humansim[i] ~ dbern(phi.humansim[i])
    y.humansim[i] ~ dnorm(z.humansim[i]*mu.humansim[i],tau.y)
    
    #Then, simulate yreplicate data to: (1) compare with humansim and (2) evaluate full model fit
    z.rep[i] ~ dbern(phi[i])
    y.rep[i] ~ dnorm(z.rep[i]*mu[i],tau.y)
    
    #Finally, simulate yreplicate data (y.pos) to evaluate just the positive-half (z=1) of the model
    y.pos[i] ~ dnorm(z[i]*mu[i],tau.y)
    }

    # Multi-variate distribution for varying coefficients
    for(n in 1:nisland){
    # matrix of mu
    A.raw.hat[n,1] <- mu.a0.raw
    A.raw.hat[n,2] <- mu.a1.raw
    A.raw.hat[n,3] <- mu.a2.raw
    A.raw.hat[n,4] <- mu.a3.raw
    A.raw.hat[n,5] <- mu.a4.raw
    A.raw.hat[n,6] <- mu.a5.raw
    A.raw.hat[n,7] <- mu.a6.raw
    A.raw.hat[n,8] <- mu.a7.raw
    A.raw.hat[n,9] <- mu.a8.raw
    A.raw.hat[n,10] <- mu.a9.raw
    B.raw.hat[n,1] <- mu.b0.raw
    B.raw.hat[n,2] <- mu.b1.raw
    B.raw.hat[n,3] <- mu.b2.raw
    B.raw.hat[n,4] <- mu.b3.raw
    B.raw.hat[n,5] <- mu.b4.raw
    B.raw.hat[n,6] <- mu.b5.raw
    B.raw.hat[n,7] <- mu.b6.raw
    B.raw.hat[n,8] <- mu.b7.raw
    B.raw.hat[n,9] <- mu.b8.raw
    B.raw.hat[n,10] <- mu.b9.raw
    
    A.raw[n,1:10] ~ dmnorm(A.raw.hat[n,], Tau.A.raw[,,])
    B.raw[n,1:10] ~ dmnorm(B.raw.hat[n,], Tau.B.raw[,,])

    # multiply coefficients by scaling factor (for scaled inverse-Wishart model)
    a0[n] <- xi.a0*A.raw[n,1]
    a1[n] <- xi.a1*A.raw[n,2]
    a2[n] <- xi.a2*A.raw[n,3]
    a3[n] <- xi.a3*A.raw[n,4]
    a4[n] <- xi.a4*A.raw[n,5]
    a5[n] <- xi.a5*A.raw[n,6]
    a6[n] <- xi.a6*A.raw[n,7]
    a7[n] <- xi.a7*A.raw[n,8]
    a8[n] <- xi.a8*A.raw[n,9]
    a9[n] <- xi.a9*A.raw[n,10]
    b0[n] <- xi.b0*B.raw[n,1]
    b1[n] <- xi.b1*B.raw[n,2]
    b2[n] <- xi.b2*B.raw[n,3]
    b3[n] <- xi.b3*B.raw[n,4]
    b4[n] <- xi.b4*B.raw[n,5]
    b5[n] <- xi.b5*B.raw[n,6]
    b6[n] <- xi.b6*B.raw[n,7]
    b7[n] <- xi.b7*B.raw[n,8]
    b8[n] <- xi.b8*B.raw[n,9]
    b9[n] <- xi.b9*B.raw[n,10]
    }

    # Calculate correlation (rho) between varying intercepts and slopes
    sigma.a0 <- xi.a0*sqrt(Sigma.A.raw[1,1])
    sigma.a1 <- xi.a1*sqrt(Sigma.A.raw[2,2])
    sigma.a2 <- xi.a2*sqrt(Sigma.A.raw[3,3])
    sigma.a3 <- xi.a3*sqrt(Sigma.A.raw[4,4])
    sigma.a4 <- xi.a4*sqrt(Sigma.A.raw[5,5])
    sigma.a5 <- xi.a5*sqrt(Sigma.A.raw[6,6])
    sigma.a6 <- xi.a6*sqrt(Sigma.A.raw[7,7])
    sigma.a7 <- xi.a7*sqrt(Sigma.A.raw[8,8])
    sigma.a8 <- xi.a8*sqrt(Sigma.A.raw[9,9])
    sigma.a9 <- xi.a9*sqrt(Sigma.A.raw[10,10])
    rho.A01  <- Sigma.A.raw[1,2]/sqrt(Sigma.A.raw[1,1]*Sigma.A.raw[2,2])
    rho.A02  <- Sigma.A.raw[1,3]/sqrt(Sigma.A.raw[1,1]*Sigma.A.raw[3,3])
    rho.A03  <- Sigma.A.raw[1,4]/sqrt(Sigma.A.raw[1,1]*Sigma.A.raw[4,4])
    rho.A04  <- Sigma.A.raw[1,5]/sqrt(Sigma.A.raw[1,1]*Sigma.A.raw[5,5])
    rho.A05  <- Sigma.A.raw[1,6]/sqrt(Sigma.A.raw[1,1]*Sigma.A.raw[6,6])
    rho.A06  <- Sigma.A.raw[1,7]/sqrt(Sigma.A.raw[1,1]*Sigma.A.raw[7,7])
    rho.A07  <- Sigma.A.raw[1,8]/sqrt(Sigma.A.raw[1,1]*Sigma.A.raw[8,8])
    rho.A08  <- Sigma.A.raw[1,9]/sqrt(Sigma.A.raw[1,1]*Sigma.A.raw[9,9])
    rho.A09  <- Sigma.A.raw[1,10]/sqrt(Sigma.A.raw[1,1]*Sigma.A.raw[10,10])
    rho.A12  <- Sigma.A.raw[2,3]/sqrt(Sigma.A.raw[2,2]*Sigma.A.raw[3,3])
    rho.A13  <- Sigma.A.raw[2,4]/sqrt(Sigma.A.raw[2,2]*Sigma.A.raw[4,4])
    rho.A14  <- Sigma.A.raw[2,5]/sqrt(Sigma.A.raw[2,2]*Sigma.A.raw[5,5])
    rho.A15  <- Sigma.A.raw[2,6]/sqrt(Sigma.A.raw[2,2]*Sigma.A.raw[6,6])
    rho.A16  <- Sigma.A.raw[2,7]/sqrt(Sigma.A.raw[2,2]*Sigma.A.raw[7,7])
    rho.A17  <- Sigma.A.raw[2,8]/sqrt(Sigma.A.raw[2,2]*Sigma.A.raw[8,8])
    rho.A18  <- Sigma.A.raw[2,9]/sqrt(Sigma.A.raw[2,2]*Sigma.A.raw[9,9])
    rho.A19  <- Sigma.A.raw[2,10]/sqrt(Sigma.A.raw[2,2]*Sigma.A.raw[10,10])
    rho.A23  <- Sigma.A.raw[3,4]/sqrt(Sigma.A.raw[3,3]*Sigma.A.raw[4,4])
    rho.A24  <- Sigma.A.raw[3,5]/sqrt(Sigma.A.raw[3,3]*Sigma.A.raw[5,5])
    rho.A25  <- Sigma.A.raw[3,6]/sqrt(Sigma.A.raw[3,3]*Sigma.A.raw[6,6])
    rho.A26  <- Sigma.A.raw[3,7]/sqrt(Sigma.A.raw[3,3]*Sigma.A.raw[7,7])
    rho.A27  <- Sigma.A.raw[3,8]/sqrt(Sigma.A.raw[3,3]*Sigma.A.raw[8,8])
    rho.A28  <- Sigma.A.raw[3,9]/sqrt(Sigma.A.raw[3,3]*Sigma.A.raw[9,9])
    rho.A29  <- Sigma.A.raw[3,10]/sqrt(Sigma.A.raw[3,3]*Sigma.A.raw[10,10])
    rho.A34  <- Sigma.A.raw[4,5]/sqrt(Sigma.A.raw[4,4]*Sigma.A.raw[5,5])
    rho.A35  <- Sigma.A.raw[4,6]/sqrt(Sigma.A.raw[4,4]*Sigma.A.raw[6,6])
    rho.A36  <- Sigma.A.raw[4,7]/sqrt(Sigma.A.raw[4,4]*Sigma.A.raw[7,7])
    rho.A37  <- Sigma.A.raw[4,8]/sqrt(Sigma.A.raw[4,4]*Sigma.A.raw[8,8])
    rho.A38  <- Sigma.A.raw[4,9]/sqrt(Sigma.A.raw[4,4]*Sigma.A.raw[9,9])
    rho.A39  <- Sigma.A.raw[4,10]/sqrt(Sigma.A.raw[4,4]*Sigma.A.raw[10,10])
    rho.A45  <- Sigma.A.raw[5,6]/sqrt(Sigma.A.raw[5,5]*Sigma.A.raw[6,6])
    rho.A46  <- Sigma.A.raw[5,7]/sqrt(Sigma.A.raw[5,5]*Sigma.A.raw[7,7])
    rho.A47  <- Sigma.A.raw[5,8]/sqrt(Sigma.A.raw[5,5]*Sigma.A.raw[8,8])
    rho.A48  <- Sigma.A.raw[5,9]/sqrt(Sigma.A.raw[5,5]*Sigma.A.raw[9,9])
    rho.A49  <- Sigma.A.raw[5,10]/sqrt(Sigma.A.raw[5,5]*Sigma.A.raw[10,10])
    rho.A56  <- Sigma.A.raw[6,7]/sqrt(Sigma.A.raw[6,6]*Sigma.A.raw[7,7])
    rho.A57  <- Sigma.A.raw[6,8]/sqrt(Sigma.A.raw[6,6]*Sigma.A.raw[8,8])
    rho.A58  <- Sigma.A.raw[6,9]/sqrt(Sigma.A.raw[6,6]*Sigma.A.raw[9,9])
    rho.A59  <- Sigma.A.raw[6,10]/sqrt(Sigma.A.raw[6,6]*Sigma.A.raw[10,10])
    rho.A67  <- Sigma.A.raw[7,8]/sqrt(Sigma.A.raw[7,7]*Sigma.A.raw[8,8])
    rho.A68  <- Sigma.A.raw[7,9]/sqrt(Sigma.A.raw[7,7]*Sigma.A.raw[9,9])
    rho.A69  <- Sigma.A.raw[7,10]/sqrt(Sigma.A.raw[7,7]*Sigma.A.raw[10,10])
    rho.A78  <- Sigma.A.raw[8,9]/sqrt(Sigma.A.raw[8,8]*Sigma.A.raw[9,9])
    rho.A79  <- Sigma.A.raw[8,10]/sqrt(Sigma.A.raw[8,8]*Sigma.A.raw[10,10])
    rho.A89  <- Sigma.A.raw[9,10]/sqrt(Sigma.A.raw[9,9]*Sigma.A.raw[10,10])
    
    sigma.b0 <- xi.b0*sqrt(Sigma.B.raw[1,1])
    sigma.b1 <- xi.b1*sqrt(Sigma.B.raw[2,2])
    sigma.b2 <- xi.b2*sqrt(Sigma.B.raw[3,3])
    sigma.b3 <- xi.b3*sqrt(Sigma.B.raw[4,4])
    sigma.b4 <- xi.b4*sqrt(Sigma.B.raw[5,5])
    sigma.b5 <- xi.b5*sqrt(Sigma.B.raw[6,6])
    sigma.b6 <- xi.b6*sqrt(Sigma.B.raw[7,7])
    sigma.b7 <- xi.b7*sqrt(Sigma.B.raw[8,8])
    sigma.b8 <- xi.b8*sqrt(Sigma.B.raw[9,9])
    sigma.b9 <- xi.b9*sqrt(Sigma.B.raw[10,10])
    rho.B01  <- Sigma.B.raw[1,2]/sqrt(Sigma.B.raw[1,1]*Sigma.B.raw[2,2])
    rho.B02  <- Sigma.B.raw[1,3]/sqrt(Sigma.B.raw[1,1]*Sigma.B.raw[3,3])
    rho.B03  <- Sigma.B.raw[1,4]/sqrt(Sigma.B.raw[1,1]*Sigma.B.raw[4,4])
    rho.B04  <- Sigma.B.raw[1,5]/sqrt(Sigma.B.raw[1,1]*Sigma.B.raw[5,5])
    rho.B05  <- Sigma.B.raw[1,6]/sqrt(Sigma.B.raw[1,1]*Sigma.B.raw[6,6])
    rho.B06  <- Sigma.B.raw[1,7]/sqrt(Sigma.B.raw[1,1]*Sigma.B.raw[7,7])
    rho.B07  <- Sigma.B.raw[1,8]/sqrt(Sigma.B.raw[1,1]*Sigma.B.raw[8,8])
    rho.B08  <- Sigma.B.raw[1,9]/sqrt(Sigma.B.raw[1,1]*Sigma.B.raw[9,9])
    rho.B09  <- Sigma.B.raw[1,10]/sqrt(Sigma.B.raw[1,1]*Sigma.B.raw[10,10])
    rho.B12  <- Sigma.B.raw[2,3]/sqrt(Sigma.B.raw[2,2]*Sigma.B.raw[3,3])
    rho.B13  <- Sigma.B.raw[2,4]/sqrt(Sigma.B.raw[2,2]*Sigma.B.raw[4,4])
    rho.B14  <- Sigma.B.raw[2,5]/sqrt(Sigma.B.raw[2,2]*Sigma.B.raw[5,5])
    rho.B15  <- Sigma.B.raw[2,6]/sqrt(Sigma.B.raw[2,2]*Sigma.B.raw[6,6])
    rho.B16  <- Sigma.B.raw[2,7]/sqrt(Sigma.B.raw[2,2]*Sigma.B.raw[7,7])
    rho.B17  <- Sigma.B.raw[2,8]/sqrt(Sigma.B.raw[2,2]*Sigma.B.raw[8,8])
    rho.B18  <- Sigma.B.raw[2,9]/sqrt(Sigma.B.raw[2,2]*Sigma.B.raw[9,9])
    rho.B19  <- Sigma.B.raw[2,10]/sqrt(Sigma.B.raw[2,2]*Sigma.B.raw[10,10])
    rho.B23  <- Sigma.B.raw[3,4]/sqrt(Sigma.B.raw[3,3]*Sigma.B.raw[4,4])
    rho.B24  <- Sigma.B.raw[3,5]/sqrt(Sigma.B.raw[3,3]*Sigma.B.raw[5,5])
    rho.B25  <- Sigma.B.raw[3,6]/sqrt(Sigma.B.raw[3,3]*Sigma.B.raw[6,6])
    rho.B26  <- Sigma.B.raw[3,7]/sqrt(Sigma.B.raw[3,3]*Sigma.B.raw[7,7])
    rho.B27  <- Sigma.B.raw[3,8]/sqrt(Sigma.B.raw[3,3]*Sigma.B.raw[8,8])
    rho.B28  <- Sigma.B.raw[3,9]/sqrt(Sigma.B.raw[3,3]*Sigma.B.raw[9,9])
    rho.B29  <- Sigma.B.raw[3,10]/sqrt(Sigma.B.raw[3,3]*Sigma.B.raw[10,10])
    rho.B34  <- Sigma.B.raw[4,5]/sqrt(Sigma.B.raw[4,4]*Sigma.B.raw[5,5])
    rho.B35  <- Sigma.B.raw[4,6]/sqrt(Sigma.B.raw[4,4]*Sigma.B.raw[6,6])
    rho.B36  <- Sigma.B.raw[4,7]/sqrt(Sigma.B.raw[4,4]*Sigma.B.raw[7,7])
    rho.B37  <- Sigma.B.raw[4,8]/sqrt(Sigma.B.raw[4,4]*Sigma.B.raw[8,8])
    rho.B38  <- Sigma.B.raw[4,9]/sqrt(Sigma.B.raw[4,4]*Sigma.B.raw[9,9])
    rho.B39  <- Sigma.B.raw[4,10]/sqrt(Sigma.B.raw[4,4]*Sigma.B.raw[10,10])
    rho.B45  <- Sigma.B.raw[5,6]/sqrt(Sigma.B.raw[5,5]*Sigma.B.raw[6,6])
    rho.B46  <- Sigma.B.raw[5,7]/sqrt(Sigma.B.raw[5,5]*Sigma.B.raw[7,7])
    rho.B47  <- Sigma.B.raw[5,8]/sqrt(Sigma.B.raw[5,5]*Sigma.B.raw[8,8])
    rho.B48  <- Sigma.B.raw[5,9]/sqrt(Sigma.B.raw[5,5]*Sigma.B.raw[9,9])
    rho.B49  <- Sigma.B.raw[5,10]/sqrt(Sigma.B.raw[5,5]*Sigma.B.raw[10,10])
    rho.B56  <- Sigma.B.raw[6,7]/sqrt(Sigma.B.raw[6,6]*Sigma.B.raw[7,7])
    rho.B57  <- Sigma.B.raw[6,8]/sqrt(Sigma.B.raw[6,6]*Sigma.B.raw[8,8])
    rho.B58  <- Sigma.B.raw[6,9]/sqrt(Sigma.B.raw[6,6]*Sigma.B.raw[9,9])
    rho.B59  <- Sigma.B.raw[6,10]/sqrt(Sigma.B.raw[6,6]*Sigma.B.raw[10,10])
    rho.B67  <- Sigma.B.raw[7,8]/sqrt(Sigma.B.raw[7,7]*Sigma.B.raw[8,8])
    rho.B68  <- Sigma.B.raw[7,9]/sqrt(Sigma.B.raw[7,7]*Sigma.B.raw[9,9])
    rho.B69  <- Sigma.B.raw[7,10]/sqrt(Sigma.B.raw[7,7]*Sigma.B.raw[10,10])
    rho.B78  <- Sigma.B.raw[8,9]/sqrt(Sigma.B.raw[8,8]*Sigma.B.raw[9,9])
    rho.B79  <- Sigma.B.raw[8,10]/sqrt(Sigma.B.raw[8,8]*Sigma.B.raw[10,10])
    rho.B89  <- Sigma.B.raw[9,10]/sqrt(Sigma.B.raw[9,9]*Sigma.B.raw[10,10])
    
    }",
    file="model3a.txt" ) 
