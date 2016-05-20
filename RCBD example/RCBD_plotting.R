#Plotting for RCBD examples

#plot results of RCBD model for growth
pred.nuts.M <- c(coef(summary(Coral_BW_lme))[1,1],
                 coef(summary(Coral_BW_lme))[1,1]+coef(summary(Coral_BW_lme))[3,1],
                 coef(summary(Coral_BW_lme))[1,1]+coef(summary(Coral_BW_lme))[2,1])
pred.nuts.P <- pred.nuts.M + coef(summary(Coral_BW_lme))[4,1]

sd.nuts.M <- c(coef(summary(Coral_BW_lme))[1,2],
               coef(summary(Coral_BW_lme))[2,2],
               coef(summary(Coral_BW_lme))[3,2])
sd.nuts.P <- sqrt((sd.nuts.M)^2 + coef(summary(Coral_BW_lme))[4,2]^2)
#plot main effects of nutrients and species on growth
plot(c(1,2,3),pred.nuts.M, ylim=c(0.01,0.07),pch = 16,cex=1.5,ylab="Change in Buoyant Wt",
     xlab="Nutrient Level",main=deparse(Coral_BW_lme@call$formula))
points(c(1,2,3)+.05,pred.nuts.P, ylim=c(0.01,0.05),pch = 1,cex=1.5)

#error bars
arrows(c(1,2,3), pred.nuts.M-sd.nuts.M,c(1,2,3),pred.nuts.M + sd.nuts.M, length=0.05, angle=90, code=3)
arrows(c(1,2,3)+.05, pred.nuts.P-sd.nuts.P,c(1,2,3)+.05,pred.nuts.P + sd.nuts.P, length=0.05, angle=90, code=3)

#plot growth x nutrients for each species
lines(c(1,2,3),pred.nuts.M,lwd=2,col="black")
lines(c(1,2,3)+.05,pred.nuts.P,lwd=2,lty=2)

# Randomly varying intercept (Tank) around each species x nutrient main effect
lines(c(1,2,3),pred.nuts.M + ranef(Coral_BW_lme)[[1]]$"(Intercept)"[1], col="red")
lines(c(1,2,3),pred.nuts.M + ranef(Coral_BW_lme)[[1]]$"(Intercept)"[2], col="green")
lines(c(1,2,3),pred.nuts.M + ranef(Coral_BW_lme)[[1]]$"(Intercept)"[3], col="blue")

lines(c(1,2,3)+.05,pred.nuts.P + ranef(Coral_BW_lme)[[1]]$"(Intercept)"[1], col="red",lty=2)
lines(c(1,2,3)+.05,pred.nuts.P + ranef(Coral_BW_lme)[[1]]$"(Intercept)"[2], col="green",lty=2)
lines(c(1,2,3)+.05,pred.nuts.P + ranef(Coral_BW_lme)[[1]]$"(Intercept)"[3], col="blue",lty=2)

legend(x="topright",legend = c("Far","Mid","Near","Montipora","Porites"),col=c("red","green","blue","black","black"),
       lty=c(1,1,1,1,2),pch=c("","","",16,1))

#plot results of orthgonal RE model for organic matter
predO.nuts.M <- c(coef(summary(Coral_org_lme))[1,1],
                  coef(summary(Coral_org_lme))[1,1]+coef(summary(Coral_org_lme))[3,1],
                  coef(summary(Coral_org_lme))[1,1]+coef(summary(Coral_org_lme))[2,1])
predO.nuts.P <- predO.nuts.M + coef(summary(Coral_org_lme))[4,1]

sdO.nuts.M <- c(coef(summary(Coral_org_lme))[1,2],
                coef(summary(Coral_org_lme))[2,2],
                coef(summary(Coral_org_lme))[3,2])
sdO.nuts.P <- sqrt((sdO.nuts.M)^2 + coef(summary(Coral_org_lme))[4,2]^2)

#plot main effects of nutrients and species on growth
plot(c(1,2,3),predO.nuts.M, ylim=c(0.045,0.065),pch = 16,cex=1.5,
     ylab="Organic Fraction of Dry Wt",xlab="Nutrient Level",main=deparse(Coral_org_lme@call$formula))
points(c(1,2,3)+.05,predO.nuts.P, ylim=c(0.01,0.05),pch = 1,cex=1.5)

#error bars
arrows(c(1,2,3), predO.nuts.M-sdO.nuts.M,c(1,2,3),predO.nuts.M + sdO.nuts.M, length=0.05, angle=90, code=3)
arrows(c(1,2,3)+.05, predO.nuts.P-sdO.nuts.P,c(1,2,3)+.05,predO.nuts.P + sdO.nuts.P, length=0.05, angle=90, code=3)

#plot growth x nutrients for each species
lines(c(1,2,3),predO.nuts.M,lwd=2,col="black")
lines(c(1,2,3)+.05,predO.nuts.P,lwd=2,lty=2)

# Randomly varying intercept (Tank) around each species x nutrient main effect
lines(c(1,2,3),predO.nuts.M + ranef(Coral_org_lme)[[1]]$"(Intercept)"[1], col="red")
lines(c(1,2,3),predO.nuts.M + ranef(Coral_org_lme)[[1]]$"(Intercept)"[2], col="green")
lines(c(1,2,3),predO.nuts.M + ranef(Coral_org_lme)[[1]]$"(Intercept)"[3], col="blue")
points(c(1,2,3),predO.nuts.M + ranef(Coral_org_lme)[[1]]$"(Intercept)"[1], col="red",pch=15)
points(c(1,2,3),predO.nuts.M + ranef(Coral_org_lme)[[1]]$"(Intercept)"[2], col="green",pch=15)
points(c(1,2,3),predO.nuts.M + ranef(Coral_org_lme)[[1]]$"(Intercept)"[3], col="blue",pch=15)

lines(c(1,2,3)+.05,predO.nuts.P + ranef(Coral_org_lme)[[1]]$"(Intercept)"[1], col="red",lty=2)
lines(c(1,2,3)+.05,predO.nuts.P + ranef(Coral_org_lme)[[1]]$"(Intercept)"[2], col="green",lty=2)
lines(c(1,2,3)+.05,predO.nuts.P + ranef(Coral_org_lme)[[1]]$"(Intercept)"[3], col="blue",lty=2)
points(c(1,2,3)+.05,predO.nuts.P + ranef(Coral_org_lme)[[1]]$"(Intercept)"[1], col="red",pch=22)
points(c(1,2,3)+.05,predO.nuts.P + ranef(Coral_org_lme)[[1]]$"(Intercept)"[2], col="green",pch=22)
points(c(1,2,3)+.05,predO.nuts.P + ranef(Coral_org_lme)[[1]]$"(Intercept)"[3], col="blue",pch=22)

legend(x="bottomright",legend = c("Far","Mid","Near","Montipora","Porites"),col=c("red","green","blue","black","black"),
       lty=c(1,1,1,1,2),pch=c(0,0,0,16,1))
########################################################
#Plot Random effects of Tank and Colony vs Species
predO.sp <- c(coef(summary(Coral_org_lme))[1,1],coef(summary(Coral_org_lme))[1,1]+coef(summary(Coral_org_lme))[4,1])
sdO.sp <- c(coef(summary(Coral_org_lme))[1,2],coef(summary(Coral_org_lme))[4,2])

#plot main effects of species on pcOrg
plot(c(0,1),predO.sp, pch = 16,ylim=c(0.05,0.065),xlim=c(0,1.07),cex=1.5,xlab = "Species",
     ylab="Organic Fraction of Dry Wt",main=deparse(Coral_org_lme@call$formula))
lines(c(0,1),predO.sp,lwd=2,col="black")
#error bars
arrows(c(0,1), predO.sp-sdO.sp,c(0,1),predO.sp + sdO.sp, length=0.05, angle=90, code=3)
#tank effects
lines(c(0,1),predO.sp + ranef(Coral_org_lme)[[1]]$"(Intercept)"[1], col="gray")
lines(c(0,1),predO.sp + ranef(Coral_org_lme)[[1]]$"(Intercept)"[2], col="gray")
lines(c(0,1),predO.sp + ranef(Coral_org_lme)[[1]]$"(Intercept)"[3], col="gray")
#colony in species effects
points(c(0,0,0)+.02,predO.sp[1] + ranef(Coral_org_lme)[[2]][1]$"(Intercept)",col="orange",pch = 1)
points(c(1,1,1)+.02,predO.sp[2] + ranef(Coral_org_lme)[[2]][2]$"SpeciesPorites",col="purple",pch = 1)

legend("topright",legend=c("RE Tank","RE Colony in Montipora", "RE Colony in Porites"),pch=c("-","o","o"),col=c("gray","orange","purple"))
