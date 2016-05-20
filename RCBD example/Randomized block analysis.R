#Examples for Bayesian

#Simplest Randomized Block Design for Nutrients x Species with RE of Tank

setwd(paste(getwd(),"/RCBD example",sep=""))
load("../RCBD example/RCBDdata.Rdata")

library(lme4)
Coral_BW_lme <- lmer(pcDeltaBW ~ Nuts + Species + (1|Tank), data=CoralSet)
Coral_BW_lme
summary(Coral_BW_lme)

#Model of organic matter with orthogonal random effects
Coral_org_lme <- lmer(pcAFDW ~ Nuts + Species + (1|Tank) +(Species|Clone), data=CoralNubb)
Coral_org_lme
summary(Coral_org_lme)

source("RCBD_plotting.R")

#Fit 2-factor RCBD in JAGS
library(rjags)
library(xtable)
data <- list(y = CoralSet$pcDeltaBW, 
             N=length(CoralSet$pcDeltaBW), 
             J=length(unique(CoralSet$Nuts)),
             block=as.numeric(CoralSet$Tank),
             x = cbind(as.numeric(CoralSet$Species)-1,as.numeric(CoralSet$Nuts)-1))

inits = list(
  list(sigma.a = 0.5, sigma.y = 0.5, mu.a =0.01, b_sp = 1, b_nut = 1),
  list(sigma.a = 0.1, sigma.y = 0.1, mu.a = 1.0, b_sp = 10, b_nut = 1),
  list(sigma.a = 1.0, sigma.y = 1.0, mu.a = 0.1, b_sp = 0.1, b_nut = 0.1))

n.adapt=1000
n.iter=5000

#### Set-up the MCMC procedure by specifying the file that contains
#### the model code (be sure to specify the name of YOUR model file),
#### indicate the data, indicate the initials, indicate the number of 
#### chains (i.e., the number of lists in the list of lists of starting
#### values), specify the length of the adapting phase.
jm=jags.model("RCBD_JAGS.R",
              data=data,
              inits=inits,
              n.chains=length(inits),
              n.adapt=n.adapt)
