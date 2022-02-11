library(Matrix) 
library(foreign)
library(lme4) #main
library(lmerTest) 
library(psych) 
library(ggplot2) 
library(dplyr)
library(car) 
library(misty) 
library(corrplot)  
library(Hmisc) 
library(nlme) 
library(r2mlm) 
library(lmtest) 
library(moments) 
library(robustlmm)
library(optimx)

gc()
options(scipen=100)
options(digits=4)
getwd()
currentwd <-"XXXXXXX" 
setwd(currentwd)

projdat <- read.delim("final.dat", header = TRUE)
projdat$ï..ID <- NULL
str(projdat)

#reshape the data
datlong <- reshape(projdat, 
                   idvar = "SubjID",
                   varying = list(c(1:10)),
                   v.names = c("Autism"),
                   direction = "long")


datlong <- datlong[order(datlong$SubjID),]
rownames(datlong)= c()
str(datlong)
datlong$time <- as.numeric(datlong$time)

#create all subscale variables (EFFECTE CODED)
datlong$SS1Eff <- recode(datlong$time, "1=1; 2=1; 3=0; 4=0; 5=0; 6=0; 7=0; 8=0; 9=-1; 10=-1")
datlong$SS2Eff <- recode(datlong$time, "1=0; 2=0; 3=1; 4=1; 5=0; 6=0; 7=0; 8=0; 9=-1; 10=-1")
datlong$SS3Eff <- recode(datlong$time, "1=0; 2=0; 3=0; 4=0; 5=1; 6=1; 7=0; 8=0; 9=-1; 10=-1")
datlong$SS4Eff <- recode(datlong$time, "1=0; 2=0; 3=0; 4=0; 5=0; 6=0; 7=1; 8=1; 9=-1; 10=-1")

#create the dummy-coded version for mean and SDs (NOT used for analysis)
datlong$SS1dum <- recode(datlong$time, "1=1; 2=1; 3=0; 4=0; 5=0; 6=0; 7=0; 8=0; 9=0; 10=0")
datlong$SS2dum <- recode(datlong$time, "1=0; 2=0; 3=1; 4=1; 5=0; 6=0; 7=0; 8=0; 9=0; 10=0")
datlong$SS3dum <- recode(datlong$time, "1=0; 2=0; 3=0; 4=0; 5=1; 6=1; 7=0; 8=0; 9=0; 10=0")
datlong$SS4dum <- recode(datlong$time, "1=0; 2=0; 3=0; 4=0; 5=0; 6=0; 7=1; 8=1; 9=0; 10=0")

datlong$FemDum <- recode(datlong$FemEFF, "1=1; -1=0")

##############
#####
#MODEL
#Model 1: Empty
datlong_M1a <- glmer(Autism ~  
                       (1|SubjID), data = datlong, family = binomial)
summary(datlong_M1a) 
#obtain ICC
var(predict(datlong_M1a))
0.918/(0.918+3.29)
#OR 0.918/(0.918+((3.142^2)/3))
#RSquare
0.5477/(0.5477+3.29+0.91)

#####
#Model 2: With predictors
datlong_M2 <- glmer(Autism ~
                      SS1Eff +
                      SS2Eff +
                      SS3Eff +
                      SS4Eff +
                      FemEFF +
                      (1|SubjID), data = datlong, family = binomial)
summary(datlong_M2)
#Rsquare
var(predict(datlong_M2))
1.06/(1.06+3.29+1.06)

#####
#Model 3: With interactions
datlong_M3a <- glmer(Autism ~
                       SS1Eff +
                       SS2Eff +
                       SS3Eff +
                       SS4Eff +
                       FemEFF +
                       FemEFF*SS1Eff +
                       FemEFF*SS2Eff +
                       FemEFF*SS3Eff +
                       FemEFF*SS4Eff +
                       (1|SubjID), data = datlong, family = binomial, control = glmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
summary(datlong_M3a)
#RSquare
var(predict(datlong_M3a))
1.072/(1.072+3.29+1.06)

#Compare models
lrtest(datlong_M1a, datlong_M2)
lrtest(datlong_M2, datlong_M3a)

#interactions and converting likelihood into probability
#female ss1
(-0.4347) + 0.5655*(1) + (-0.2981)*(1) + 0.1839*(1)*(1)
1/(1 + exp((-1)*(0.0166)))
#male ss1
(-0.4347) + 0.5655*(1) + (-0.2981)*(-1) + 0.1839*(1)*(-1)
1/(1 + exp((-1)*(0.245)))
#female ss2
(-0.4347) + (-0.0492)*(1) + (-0.2981)*(1) + (-0.1429)*(1)*(1)
1/(1 + exp((-1)*(-0.9249)))
#male ss2
(-0.4347) + (-0.0492)*(1) + (-0.2981)*(-1) + (-0.1429)*(1)*(-1)
1/(1 + exp((-1)*(-0.0429)))
#female ss3
(-0.4347) + (-0.7451)*(1) + (-0.2981)*(1) + (-0.0315)*(1)*(1)
1/(1 + exp((-1)*(-1.509)))
#male ss3
(-0.4347) + (-0.7451)*(1) + (-0.2981)*(-1) + (-0.0315)*(1)*(-1)
1/(1 + exp((-1)*(-0.8502)))
#female ss4
(-0.4347) + (0.7242)*(1) + (-0.2981)*(1) + (-0.0306)*(1)*(1)
1/(1 + exp((-1)*(-0.0392)))
#male ss4
(-0.4347) + (0.7242)*(1) + (-0.2981)*(-1) + (-0.0306)*(1)*(-1)
1/(1 + exp((-1)*(0.6182)))
#female SS5 - I know things will cancel out here and there but it is good to type out for my own reference
(-0.4347) + (0.5655)*(-1) + (-0.0492)*(-1) + (-0.7451)*(-1) + (0.7242)*(-1) + (-0.2981)*(1) + (0.1830)*(-1)*(1) + (-0.1429)*(-1)*(1) + (-0.0315)*(-1)*(1) + (-0.0306)*(-1)*(1)
1/(1 + exp((-1)*(-1.206)))
#male ss5
(-0.4347) + (0.5655)*(-1) + (-0.0492)*(-1) + (-0.7451)*(-1) + (0.7242)*(-1) + (-0.2981)*(-1) + (0.1830)*(-1)*(-1) + (-0.1429)*(-1)*(-1) + (-0.0315)*(-1)*(-1) + (-0.0306)*(-1)*(-1)
1/(1 + exp((-1)*(-0.654)))

#Standard deviations using Dummy-coded binary variables
#get some sd
sd(datlong$SS1dum, na.rm=TRUE)
sd(datlong$SS2dum, na.rm=TRUE)
sd(datlong$SS3dum, na.rm=TRUE)
sd(datlong$SS4dum, na.rm=TRUE)
sd(datlong$FemDum, na.rm=TRUE)

#mean values using Dummy-coded binary variables
mean(datlong$SS1dum, na.rm=TRUE)
mean(datlong$SS2dum, na.rm=TRUE)
mean(datlong$SS3dum, na.rm=TRUE)
mean(datlong$SS4dum, na.rm=TRUE)
mean(datlong$FemDum, na.rm=TRUE)

#correlation
cor(datlong, method = "pearson")

#testing correlation significance
cor.test(datlong$Autism, datlong$SS1Eff)
cor.test(datlong$Autism, datlong$SS2Eff)
cor.test(datlong$Autism, datlong$SS3Eff)
cor.test(datlong$Autism, datlong$SS4Eff)
cor.test(datlong$Autism, datlong$ZAgeGRM)
cor.test(datlong$Autism, datlong$FemEFF)

