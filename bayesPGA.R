######################################################################################
######################## Porites GA disease model  ###################################
######################################################################################
rm(list=ls()) #remove previous variable assignments
library(plyr) # load packages
library(rjags)

setwd("//Users//jam1e112//IDriveSync//Documents//UHM//Thesis//Chapter1")
setwd("C://Users//Jamie//IDriveSync//Documents//UHM//Thesis//Chapter1//")

porSub <- read.csv("bayesPoritesSubset.csv", head=T, stringsAsFactors=F)
porSubSurveys <- ddply(porSub, .(survey, sector, island),
                       summarise,
                       numColonies = length(survey),
                       numGAcolonies = sum(GA == 1),
                       PGAprev = numGAcolonies/length(Genus == "Porites"),
                       depth = mean(Scaled_depth),
                       PoritesDensity = mean(ScaledPorDensity),
                       TotColDensity = mean(ScaledColDensity),
                       humanPop = mean(ScaledPopulation))

Por_Sub_numSurveys <- length(porSubSurveys$survey)
Por_Sub_numSectors <- length(unique(porSubSurveys$sector))
PGA_SubSize_W <- diag(5) 
numAdapt <- 10000
numUpdate <- 10000
numIter <- 10000


PGA_SubSize_model_string <- "model{
    for(i in 1:length(healthState)) {
      healthState[i] ~ dbern(psi[i]) 
      healthStatePredictions[i] <- ilogit(beta.0.survey[surveyNum[i]] + beta.colony.size * colonySize[i])  # predicted values
      psi[i] <- ilogit(beta.0.survey[surveyNum[i]] + beta.colony.size * colonySize[i]) # bounds continuous data between 0 and 1     
    }

    #prior
    beta.colony.size ~ dnorm(0,1E-6)

      for (j in 1:numSurveys){
        beta.0.survey[j] ~ dnorm(mu.survey[j], tau.survey) #tau.survey[sectorNum[j]]
        mu.survey[j] <- beta.0.sector[sectorNum[j]] 
        + beta.hostDensity[sectorNum[j]] * hostDensity[j]  
        + beta.Depth[sectorNum[j]] * depth[j]
        + beta.HumPop[sectorNum[j]] * humPop[j]
        + beta.ColDensity[sectorNum[j]] * colDensity[j]
      }          

    # priors for survey to survey variance within sectors
    tau.survey <- pow(sigma.survey, -2) #[sectorNum[k]] in for loop if want to vary within sectors
    sigma.survey ~ dunif(0,100)

        for (k in 1:numSectors){
          beta.0.sector[k] <- scaleFactor[1] * B.raw[k,1]
          beta.hostDensity[k] <- scaleFactor[2] * B.raw[k,2]
          beta.Depth[k] <- scaleFactor[3] * B.raw[k,3]
          beta.HumPop[k] <- scaleFactor[4] * B.raw[k,4]
          beta.ColDensity[k] <- scaleFactor[5] * B.raw[k,5]
          B.raw[k,1:5] ~ dmnorm(B.raw.hat[k,], Tau.B.raw[,]) #1:n = number of variables
          B.raw.hat[k,1] <- mu.beta.0.sector.raw # island level prior [islandNum[k]]
          B.raw.hat[k,2] <- mu.beta.hostDensity.raw 
          B.raw.hat[k,3] <- mu.beta.Depth.raw
          B.raw.hat[k,4] <- mu.beta.HumPop.raw
          B.raw.hat[k,5] <- mu.beta.ColDensity.raw
        }

    #priors - Add for loop when adding island/region
    mu.beta.0.sector <- scaleFactor[1] * mu.beta.0.sector.raw #mu.a
    mu.beta.hostDensity <- scaleFactor[2] * mu.beta.hostDensity.raw #mu.b
    mu.beta.Depth <- scaleFactor[3] * mu.beta.Depth.raw
    mu.beta.HumPop <- scaleFactor[4] * mu.beta.HumPop.raw
    mu.beta.ColDensity <- scaleFactor[5] * mu.beta.ColDensity.raw
    mu.beta.0.sector.raw ~ dnorm(0, 0.0001)
    mu.beta.hostDensity.raw ~ dnorm(0, 0.0001)
    mu.beta.Depth.raw ~ dnorm(0, 0.0001)
    mu.beta.HumPop.raw ~ dnorm(0, 0.0001)
    mu.beta.ColDensity.raw ~ dnorm(0, 0.0001)

          for (l in 1:5){
            scaleFactor[l] ~ dunif(0,100)
          }

    Tau.B.raw[1:5,1:5] ~ dwish(W[,], df) 
    df <- 6  #degrees of freedom: number of variables + 1
    Sigma.B.raw[1:5,1:5] <- inverse(Tau.B.raw[,])

            for (l in 1:5){
              for (m in 1:5){
                rho[l,m]<-Sigma.B.raw[l,m]/sqrt(Sigma.B.raw[l,l]*Sigma.B.raw[m,m])
              }
            }

    rho.b0_hostDens <- rho[1,2]
    rho.b0_Depth <- rho[1,3]
    rho.b0_HumPop <- rho[1,4]
    rho.b0_ColDen <- rho[1,5]
    rho.hostDens_Depth <- rho[2,3]
    rho.hostDens_HumPop <- rho[2,4]
    rho.hostDens_ColDen <- rho[2,5]
    rho.Depth_HumPop <- rho[3,4]
    rho.Depth_ColDen <- rho[3,5]
    rho.HumPop_ColDen <- rho[4,5]
}"

PGA_SubSize_dataList = list('healthState' = porSub$GA, 
                            'surveyNum' = porSub$survey, 
                            'colonySize' = porSub$Scaled_colSize, 
                            'numSurveys' = Por_Sub_numSurveys,
                            'sectorNum' = porSubSurveys$sector,
                            'hostDensity'= porSubSurveys$PoritesDensity,
                            'numSectors' = Por_Sub_numSectors,
                            'depth' = porSubSurveys$depth,
                            'humPop' = porSubSurveys$humanPop,
                            'colDensity' = porSubSurveys$TotColDensity,
                            'W' = PGA_SubSize_W)

PGA_SubSize_variableNames = c("healthStatePredictions",
                              "beta.0.survey", 
                              "beta.colony.size",
                              "mu.survey",
                              "tau.survey",
                              "sigma.survey",
                              "beta.hostDensity",
                              "beta.Depth",
                              "beta.HumPop",
                              "beta.ColDensity",
                              "beta.0.sector",
                              "rho.b0_hostDens",
                              "rho.b0_Depth",
                              "rho.b0_HumPop",
                              "rho.b0_ColDen",
                              "rho.hostDens_Depth",
                              "rho.hostDens_HumPop",
                              "rho.hostDens_ColDen",
                              "rho.Depth_HumPop",
                              "rho.Depth_ColDen",
                              "rho.HumPop_ColDen")

PGA_SubSize_model <- jags.model(textConnection(PGA_SubSize_model_string), data = PGA_SubSize_dataList, n.chains = 3, n.adapt=numAdapt)
update(PGA_SubSize_model, numUpdate); # Burnin for 10000+ samples (keep updating until converged, then start from that point in the next step)
PGA_SubSize_mcmc_samples <- coda.samples(PGA_SubSize_model, variable.names = PGA_SubSize_variableNames, n.iter=numIter) #20000; put step size in to avoid autocorrelation
