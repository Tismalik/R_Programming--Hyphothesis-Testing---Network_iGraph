library("readxl")

Lab5data <- read_excel("MATPMD1LabSession5Data.xlsx",sheet="BP Prediction Data")
attach (Lab5data)

#sample size
n<-length(Systol)

# Find predicted values using model developed using training data
sbpPred<-54.2960 - 0.6256*Years + 1.3125*Weight

# plot predicted versus actual observations
plot(Systol,sbpPred,xlim=c(100,160),ylim=c(100,160))
abline(0,1,lty=2)

# find SStot
(SStot <-sum((Systol)^2)-(sum(Systol))^2/n)

# and SSres - note this is from the difference between the predictions
# and the actual
sbpPredDiff<-sbpPred-Systol
(SSres <-sum(sbpPredDiff^2)-(sum(sbpPredDiff))^2/n)

(R2<-1-SSres/SStot)

detach(Lab5data)
