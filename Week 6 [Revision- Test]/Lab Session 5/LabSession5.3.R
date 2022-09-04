library("readxl")

lab5data <- read_excel("MATPMD1LabSession5Data.xlsx",sheet="BP Training Data")
attach (lab5data)

#sample size
n<-length(Diastol)

pairs(lab5data)

# set up linear regression model using all the covariates
dbp.lm <- lm(Diastol~Age+Years+Weight+Height+Chin+Forearm+Calf+Pulse)
summary(dbp.lm)

# R2 is 0.37 check how variables correlate with Diastol. Note
# regression is not considered significant
corMatrix<-cor(lab5data)
corMatrix["Diastol",]

# Try using step() and AIC criteria to reduce the number of explanatory
# variables in the model
step(dbp.lm,direction="backward")

# and generate the new model, but note how some of the beta values are
# not significant
dbp.lm2 <- lm(Diastol ~ Age + Height + Calf + Pulse)
summary(dbp.lm2)

# do we get the same result if we use the drop1 F-test as our method
# of deciding whether to drop and explanatory variable?
drop1(dbp.lm,test="F")

# drop Years
dbp.lm3<-lm(Diastol~Age+Weight+Height+Chin+Forearm+Calf+Pulse)
drop1(dbp.lm3,test="F")

# drop Weight
dbp.lm4<-lm(Diastol~Age+Height+Chin+Forearm+Calf+Pulse)
drop1(dbp.lm4,test="F")

# drop Chin
dbp.lm5<-lm(Diastol~Age+Height+Forearm+Calf+Pulse)
summary(dbp.lm5)
drop1(dbp.lm5,test="F")

# drop Forearm
dbp.lm6<-lm(Diastol~Age+Height+Calf+Pulse)
summary(dbp.lm6)
drop1(dbp.lm6,test="F")

# drop Height
dbp.lm7<-lm(Diastol~Age+Calf+Pulse)
summary(dbp.lm7)
drop1(dbp.lm7,test="F")

# drop Age
dbp.lm8<-lm(Diastol~Calf+Pulse)
summary(dbp.lm8)
drop1(dbp.lm8,test="F")

# drop Pulse
dbp.lm9<-lm(Diastol~Calf)
summary(dbp.lm9)

# So we have found a significant regression with just one explanatory
# variable. How valuable this model is, is debatable. Note how the R^2 
# value for the model we generated using the AIC criteria is higher. 
# This is not surprising as it has more explanatory variables. However, 
# even its adjusted R^2 value is higher. More research is definitely 
# required.

detach(lab5data)
