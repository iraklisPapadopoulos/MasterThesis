## Calculate the modified GEE variance estimator proposed by Fay and Graubard (2001)
# The folder of the project
W = "C:/works/diploma"
WR = paste(W,"results",sep = "/")
WP = paste(W,"programs",sep = "/")
WG = paste(W,"graphs",sep = "/")
WD = paste(W,"data",sep = "/")
WRD = paste(W,"raw_data",sep = "/")
library(haven)
library(gdata)
library(tidyverse)
library(dplyr)
library(lubridate)
library(transplantr)

setwd(WD)
total=read_dta("total.dta")
library(geepack)
library(gee)

################
### Formulas ###
################

## Univariate adjusted only for age 

f0<- mm~factor(CalendarYear)+factor(catagew)
f1 <- mm~ factor(catagew)
f2 <- mm~ factor(catagew) + factor(gender)
f3 <- mm~ factor(catagew) + factor(race)
f4 <- mm~ factor(catagew) + factor(mode)
f5 <- mm~ factor(catagew) + factor(region_group)
f6 <- mm~ factor(catagew) + factor(smoke)
f7 <- mm~ factor(catagew) + factor(BMI)
f8 <- mm~ factor(catagew) + factor(aids)
f9 <- mm~ factor(catagew) + factor(edulvl)

## Full models

f10 <- mm~ factor(catagew)+ factor(mode)+ factor(gender)+ factor(race)+ factor(smoke)+ factor(aids)
f11<-mm~ factor(CalendarYear)+factor(catagew)+factor(race)+factor(mode)


#total$race[which(is.na(total$race))] <- 1

#################
### apply GEE ###
#################

gf0=gee(f0,id=PATIENT, data=total, family=poisson, corstr="independence",na.action = na.omit)
gf1=gee(f1,id=PATIENT, data=total, family=poisson, corstr="independence",na.action = na.omit)
#gf1=geeglm(f1,id=PATIENT, data=total, family=poisson, corstr="independence")
#total$gender=relevel(total$gender,ref = "Male")
gf2=geeglm(f2,id=PATIENT, data=total, family=poisson, corstr="independence",na.action = na.omit)
gf3=gee(f3,id=PATIENT, data=total, family=poisson, corstr="independence",na.action = na.omit)
gf4=gee(f4,id=PATIENT, data=total, family=poisson, corstr="independence",na.action = na.omit)
gf5=gee(f5,id=PATIENT, data=total, family=poisson, corstr="independence",na.action = na.omit)
gf6=gee(f6,id=PATIENT, data=total, family=poisson, corstr="independence",na.action = na.omit)
gf7=gee(f7,id=PATIENT, data=total, family=poisson, corstr="independence",na.action = na.omit)
gf8=gee(f8,id=PATIENT, data=total, family=poisson, corstr="independence",na.action = na.omit)
gf9=gee(f9,id=PATIENT, data=total, family=poisson, corstr="independence",na.action = na.omit)


gf5=geeglm(f5,id=PATIENT, data=total, family=binomial("log"), corstr="ind")
gf6=geeglm(f6,id=PATIENT, data=total, family=poisson, corstr="ind")
gf7=geeglm(f7,id=PATIENT, data=total, family=binomial("log"), corstr="ind")
gf8=geeglm(f8,id=PATIENT, data=total, family=binomial("log"), corstr="ind")
gf9=geeglm(f9,id=PATIENT, data=total, family=binomial("log"), corstr="ind")


gf10=gee(f10,id=PATIENT, data=total, family=poisson, corstr="exchangeable",na.action = na.omit)


gf11=gee(f11,id=PATIENT, data=total, family=poisson, corstr="exchangeable")

#################
### summaries ###
#################

summary(gf0)
summary(gf1)
summary(gf2)
summary(gf3)
summary(gf4)
summary(gf5)
summary(gf6)
summary(gf7)
summary(gf8)
summary(gf9)
summary(gf10)


#################
### exp(coef) ###
#################

exp(gf0$coefficients)
exp(gf1$coefficients)
exp(gf2$coefficients)
exp(gf3$coefficients)
exp(gf4$coefficients)

exp(gf5$coefficients)
exp(gf6$coefficients)
exp(gf7$coefficients)
exp(gf8$coefficients)
exp(gf9$coefficients)
exp(gf10$coefficients)

###################
### extract CIs ###
###################



####################################################
c1 <- coef(summary(gf1))
citab1 <- with(as.data.frame(c1),
              cbind(lwr=Estimate-1.96*Std.err,
                    upr=Estimate+1.96*Std.err))
rownames(citab1) <- rownames(c1)
####################################################

cc <- coef(summary(gf10))
citab <- with(as.data.frame(cc),
              cbind(lwr=Estimate-1.96*Std.err,
                    upr=Estimate+1.96*Std.err))
rownames(citab) <- rownames(cc)




d1 <- dia~ factor(catagew)+ factor(mode)+ factor(gender)+ factor(race)+ factor(smoke)+ factor(aids) + LastCD4
gd1=gee(f10,id=PATIENT, data=total, family=poisson, corstr="exchangeable",na.action = na.omit)

exp(gd1$coefficients)



library(tidyverse)    # all things tidy
library(pander)       # nice looking genderal tabulations
library(furniture)    # nice table1() descriptives
library(texreg)       # Convert Regression Output to LaTeX or HTML Tables
library(psych)        # contains some useful functions, like headTail

library(interactions)
library(performance)
library(lme4)         # Linear, generalized linear, & nonlinear mixed models

library(corrplot)     # Vizualize correlation matrix
library(gee)          # Genderalized Estimation Equation Solver
library(geepack)      # Genderalized Estimation Equation Package 
library(MuMIn)  
library(texreghelpr)  # extract functions for exponentiating parameters form generalized regression models within a texreg table of model parameters.


texreg::knitreg(list(extract_gee_exp(gf)),
                custom.model.names = c(  "Exchangeable"
                                       ),
                single.row = T,
                digits = 3,
                ci.test = 1,
                caption = "GEE - Estimates on Count Scale (RR)")

texreg::htmlreg(list(extract_gee_exp(gf2)),
                custom.model.names = c(  "model2 "
                ),
                single.row = T,
                digits = 3,
                ci.test = 1,
                caption = "GEE - Estimates on Count Scale (RR)")
