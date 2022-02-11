install.packages(caTools)
library(lavaan)
library(psych)
library(EFA.dimensions) #parallel analysis
library(GPArotation) #rotate factors


library(psych)
library(lavaan)
library(semPlot)
library(ggplot2)
library(MASS) #3D kernel density estimation
library(semTools) #mardia's kurtosis
library(plotly)

#This is the data that GIVES the negative loading on item 1. Even if I believe my reverse coding is correct.
dat <-read.csv("XXXXXX")
head(dat)


eigen(cor(dat))$values
sum(eigen(cor(dat))$values>1) 

scree(dat)

#now i can see that there are 4 factors with eigenvalues more than 1
#BUT PCA gives me 3 factors instead

RAWPAR(dat, factormodel='PCA', Ndatasets=100, percentile=95,corkind='pearson', verbose=TRUE)

efa2 <- fa(dat, nfactors=3, rotate="varimax", fm="ml") ##exploratory factor analysis using the 'psych' package 
print.psych(efa2, digits=3, cut=.3)

#five-factor model #item1 has a weird variable name due to some data coding process
mod1 <- 'f1 =~ NA*ï..Item1.new + Item2.new
          f2 =~ NA*Item3.new + Item4.new
          f3 =~ NA*Item5.new + Item6.new
          f4 =~ NA*Item7.new + Item8.new
          f5 =~ NA*Item9.new + Item10.new

         f1 ~~1*f1
          f2 ~~1*f2
          f3 ~~1*f3
          f4 ~~1*f4
          f5 ~~1*f5'

run1.3 <- cfa(mod1, data=dat, estimator="MLR")
run1.3

#unidimensional model #item1 has a weird variable name due to some data coding process
mod2 <- 'f1 =~ NA*ï..Item1.1.new + Item2.new + Item3.new + Item4.new + Item5.new + Item6.new + Item7.new + Item8.new + Item9.new + Item10.new

         f1 ~~1*f1'

run1.4 <- cfa(mod2, data=dat1, estimator="WLS")
summary(run1.4, rsquare=TRUE, fit.measures=TRUE, standardized=TRUE)

ggplot(dat, aes(x = Item8.new)) + geom_density(color="darkblue", fill="lightblue")+theme_bw()
ggplot(dat2, aes(x = X2)) + geom_density(color="darkblue", fill="lightblue")+theme_bw()

ggplot(dat, aes(x = Item3.new)) + geom_density(color="darkblue", fill="lightblue")+theme_bw()
ggplot(dat, aes(x = Item4.new)) + geom_density(color="darkblue", fill="lightblue")+theme_bw()
ggplot(dat, aes(x= Item3.new, y= Item4.new)) + geom_point(colour="red", alpha=.4)+theme_bw()

#now I am going to use other DATA files that I have split in SPSS
#This is the data set that DOES NOT give the negative loading (I reverse question one back..)
dathalf1 <-read.csv("XXXXXX")
dathalf2 <- read.csv("XXXXXX")
head(dathalf1)

#FIRSTHALF
eigen(cor(dathalf1))$values
sum(eigen(cor(dathalf1))$values>1) 

scree(dathalf1)


RAWPAR(dathalf1, factormodel='PCA', Ndatasets=100, percentile=95,corkind='pearson', verbose=TRUE)

efa3 <- fa(dathalf2, nfactors=3, rotate="oblimin", fm="ml") ##exploratory factor analysis using the 'psych' package 
print.psych(efa3, digits=3, cut=.3)


#SECONDHALF
eigen(cor(dathalf2))$values
sum(eigen(cor(dathalf2))$values>1) 

scree(dathalf2)


RAWPAR(dathalf2, factormodel='PCA', Ndatasets=100, percentile=95,corkind='pearson', verbose=TRUE)

efa4 <- fa(dathalf2, nfactors=3, rotate="oblimin", fm="ml") ##exploratory factor analysis using the 'psych' package 
print.psych(efa4, digits=3, cut=.3)

#This is the total sample, does NOT give the negative loading.
dat1 <-read.csv("C:\\Users\\palmy\\Documents\\cambridge\\gradschool\\UW-Spring21\\SEM\\aq10_experiment.csv")
eigen(cor(dat1))$values
sum(eigen(cor(dat1))$values>1) 

scree(dat1)


RAWPAR(dat1, factormodel='PCA', Ndatasets=100, percentile=95,corkind='pearson', verbose=TRUE)

efa5 <- fa(dat1, nfactors=3, rotate="oblimin", fm="ml") ##exploratory factor analysis using the 'psych' package 
print.psych(efa5, digits=3, cut=.3)


#three-factor model
#After looking at the EFA, results I will come up with a model that has 3 factors
mod3 <- 'f1 =~ NA*Item7.new + Item10.new
          f2 =~ NA*Item3.new + Item4.new
          f3 =~ NA*Item5.new + Item6.new + Item9.new + ï..Item1.1.new
       

         f1 ~~1*f1
          f2 ~~1*f2
          f3 ~~1*f3

   f1~~ f2
	    f1~~ f3
          f2~~ f3'
run1.5 <- cfa(mod3, data=dathalf1, estimator="MLR")
run1.5
summary(run1.5, rsquare=TRUE, fit.measures=TRUE, standardized=TRUE)
lavResiduals(run1.5)$cov ##check residual correlations for clue of where the 
##misfit might be present

sort_residuals(lavResiduals(run1.5)$cov)
modindices(run1.5, sort = TRUE, maximum.number = 10) 

  
  mod4 <- 'f1 =~ NA*ï..Item1.1.new + Item2.new
          f2 =~ NA*Item3.new + Item4.new
          f3 =~ NA*Item5.new + Item6.new
          f4 =~ NA*Item7.new + Item8.new
          f5 =~ NA*Item9.new + Item10.new

         f1 ~~1*f1
          f2 ~~1*f2
          f3 ~~1*f3
          f4 ~~1*f4
          f5 ~~1*f5'
  
  	 

run1.6 <- cfa(mod4, data=dat1, estimator="MLR")
run1.6
summary(run1.6, rsquare=TRUE, fit.measures=TRUE, standardized=TRUE)

mod5 <- 'f1 =~ NA*Item5.new + Item6.new + Item7.new + Item9.new + Item10.new
        f2 =~ NA*Item3.new + Item4.new
       
       f1 ~~1*f1
        f2 ~~1*f2'


run1.7 <- cfa(mod5, data=dathalf1, estimator="MLR")
run1.7
summary(run1.7, rsquare=TRUE, fit.measures=TRUE, standardized=TRUE)
lavResiduals(run1.7)$cov ##check residual correlations for clue of where the 
##misfit might be present
