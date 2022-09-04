library("readxl")
library("car")
library("nortest")

Lab5Data <- read_excel("MATPMD1LabSession5Data.xlsx", sheet = "Timber")
attach(Lab5Data)

grades<-factor(Grade)
locations<-factor(Location)

aov1<-aov(Volume~grades*locations)
summary(aov1)

lm5<-lm(Volume~Diameter+Height+grades+locations)
summary(lm5)

plot(grades,Volume)
plot(locations,Volume)

interaction.plot(grades,locations,Volume)
interaction.plot(locations,grades,Volume)

# our assumptions don't look like they are valid. Any inference we
# make is going to be suspect
leveneTest(aov1)
# there isn't equal variance between groups, but residuals look like
# they are normally distributed
qqnorm(aov1$residuals)
ad.test(aov1$residuals)

detach(lab5data)

detach(Lab5Data)
