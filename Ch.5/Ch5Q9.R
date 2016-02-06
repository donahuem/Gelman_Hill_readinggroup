## Question number 9 from chapter 5 of Gelman and Hill
#Code by Nyssa Silbiger
# 2/5/2006
#########################################################

#All data is from the arsenic folder on the Gelman and Hill Website

#The following code is copied  from Gelman and Hill

#load libraries-----------
library (foreign)
library(arm)

#load functions-----------------------------
attach.all <- function (x, overwrite = NA, name = "attach.all")  {
  rem <- names(x) %in% ls(.GlobalEnv)
  if (!any(rem)) overwrite <- FALSE
  rem <- names(x)[rem]
  if (is.na(overwrite)) {
    question <- paste("The following objects in .GlobalEnv will mask\nobjects in the attached database:\n", paste(rem, collapse = ", "), "\nRemove these objects from .GlobalEnv?", sep = "")
    if (interactive()) {
      if (.Platform$OS.type == "windows")  overwrite <- "YES" == winDialog(type = "yesno",  question)
      else overwrite <- 1 == menu(c("YES", "NO"), graphics = FALSE, title = question)
    }
    else overwrite <- FALSE
  }
  if (overwrite) remove(list = rem, envir = .GlobalEnv)
  attach(x, name = name)
}

#a function to "jitter" the data
jitter.binary<- function(a, jitt=.05){
  ifelse(a==0, runif(length(a),0,jitt), runif(length(a),1-jitt,1))
}

#binned residuals
binned.resids <- function (x, y, nclass=sqrt(length(x))){
  breaks.index <- floor(length(x)*(1:(nclass-1))/nclass)
  breaks <- c (-Inf, sort(x)[breaks.index], Inf)
  output <- NULL
  xbreaks <- NULL
  x.binned <- as.numeric (cut (x, breaks))
  for (i in 1:nclass){
    items <- (1:length(x))[x.binned==i]
    x.range <- range(x[items])
    xbar <- mean(x[items])
    ybar <- mean(y[items])
    n <- length(items)
    sdev <- sd(y[items])
    output <- rbind (output, c(xbar, ybar, n, x.range, 2*sdev/sqrt(n)))
  }
  colnames (output) <- c ("xbar", "ybar", "n", "x.lo", "x.hi", "2se")
  return (list (binned=output, xbreaks=xbreaks))
}
# Set up cleaned dataset for Bangladesh well-switching------------

# Read in the data
all <- read.dta ("Data/arsenic/all.dta", convert.factors=F)

# For simplicity, pull out all wells with missing data in the variables that we
# will be using in our analysis

missing <- is.na (all[,"func"] + all[,"as"] + all[,"distnearest"] + all[,"assn"] + all[,"ed"])
table (missing)

# Include only the wells that are functioning (func==1) and "unsafe" (as>50)

keep <- all[,"func"]==1 & all[,"as"]>50
attach.all (all[!missing & keep,])

# Give convenient names to the variables

switch <- switch
arsenic <- as/100
dist <- distnearest
assoc <- ifelse (assn>0,1,0)
educ <- ed

wells.data <- cbind (switch, arsenic, dist, assoc, educ)
write.table (wells.data, "wells.dat")

## Start code from Nyssa----------------------------------------
#Question 9a
#Fit a logistic regression for the probability of switching using log (distance to nearest safe
#well) as a predictor

fit.1<-glm(switch~dist, family=binomial(link="logit"))
#display the results
display(fit.1)

#9b Make a graph similar to 5.9 displaying Pr(switch) as a function of distance to nearest safe well, along with the data
#first "jitter the data so that it is easier to read
switch.jitter<-jitter.binary(switch)
plot(dist,switch.jitter, xlab='distance', ylab='Pr(Switching)')
x<-min(dist):.1:max(dist) #create a vector of x's for plotting
y.new<-curve(invlogit(coef(fit.1)[1]+coef(fit.1)[2]*x), add=TRUE)

#9c make a residuals plot and a binned residual plot as in figure 5.13
#residuals
plot(fit.1$fitted.values,fit.1$y-fit.1$fitted.values, xlab="Estimated Pr(Switching)",
     ylab="observed-estimated", ylim=c(-1,1))
abline(h =0)


#binned residuals plot for 40 bins
br.8 <- binned.resids (fit.1$fitted.values, switch-fit.1$fitted.values, nclass=40)$binned
plot(range(br.8[,1]), range(br.8[,2],br.8[,6],-br.8[,6]), xlab="Estimated  Pr (switching)", ylab="Average residual", type="n", main="Binned residual plot", mgp=c(2,.5,0))
abline (0,0, col="gray", lwd=.5)
lines (br.8[,1], br.8[,6], col="gray", lwd=.5)
lines (br.8[,1], -br.8[,6], col="gray", lwd=.5)
points (br.8[,1], br.8[,2], pch=20, cex=.5)

#9d compute the error rate and compare it to the null model
error.rate<-mean((fit.1$fitted.values>0.5 & fit.1$y==0) | (fit.1$fitted.values<0.5 & fit.1$y==1))
#how gelman coded it... same answer
error.rate <- mean(round(abs(switch-fit.1$fitted)))
#null error
error.rate.null <- mean(round(abs(switch-mean(fit.1$fitted))))

#9e create indicator variables for dist <100, = 100, >100 and <200 and >200
#93
dist100<-dist/100