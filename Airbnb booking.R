library(tidyverse)
library(ggplot2)
library(data.table)
library (ggthemes)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(readxl)
library(esquisse)
library(car)
library(estimatr)
library(caret)

#load data

cancer <- Cancer_cleaned_features

reg_all <- lm(TARGET_deathRate ~ . , cancer)
summary(reg_all)
#lets take the States and counties out. 

reg_most <- lm(TARGET_deathRate ~ avgAnnCount + avgDeathsPerYear + incidenceRate + medIncome + popEst2015 + povertyPercent 
              + AvgHouseholdSize + PercentMarried + PctHS18_24 + PctSomeCol18_24 + PctBachDeg18_24+ PctHS25_Over
              + PctBachDeg25_Over + PctEmployed16_Over + PctUnemployed16_Over + PctPrivateCoverage + PctPrivateCoverageAlone 
              + PctEmpPrivCoverage + PctPublicCoverage + PctPublicCoverageAlone + PctWhite + PctBlack + PctAsian + PctOtherRace
              + PctMarriedHouseholds + BirthRate + Non.Good.Day.Ratio.CountyLevelImputed, cancer)
summary(reg_most
      )
# GOing to remove State variable for now. 
# PctNoHS18_24 removed since it along with PctHS18_24 + PctSomeCol18_24 + PctBachDeg18_24 = 100%


reg_1 <- lm(TARGET_deathRate ~ avgAnnCount + avgDeathsPerYear + incidenceRate + popEst2015 
              + PercentMarried + PctHS18_24 + PctSomeCol18_24 +PctBachDeg18_24+ PctHS25_Over + MedianAgeMale + MedianAgeFemale
              + PctBachDeg25_Over + PctEmployed16_Over + PctUnemployed16_Over + PctPrivateCoverage + PctPrivateCoverageAlone 
              + PctEmpPrivCoverage + PctPublicCoverage + PctPublicCoverageAlone + PctWhite + PctBlack + PctAsian + PctOtherRace
              + PctMarriedHouseholds + BirthRate + Non.Good.Day.Ratio.CountyLevelImputed , cancer)

summary(reg_1)
plot(reg_1)
#
# Should do an f-test on PctNoHS18_24, PctHS18_24, PctSomeCol18_24, PctBachDeg18_24
education_hyp <- linearHypothesis(reg_1, c("PctSomeCol18_24 = 0", "PctBachDeg18_24 = 0", "PctHS18_24 = 0")) 
education_hyp  

education_hyp2 <- linearHypothesis(reg_1, c(" PctHS25_Over = 0", " PctBachDeg25_Over = 0"))
education_hyp2

#P-value of of 3.022e-07 and 2.2e-16 respectivly shows that education jointly between 18_24 is a factor. 
#Linear hypothesis test

#Hypothesis:
#  PctSomeCol18_24 = 0
#PctBachDeg18_24 = 0
#PctHS18_24 = 0

#Model 1: restricted model
#Model 2: TARGET_deathRate ~ avgAnnCount + avgDeathsPerYear + incidenceRate + 
#  popEst2015 + povertyPercent + PercentMarried + PctHS18_24 + 
#  PctSomeCol18_24 + PctBachDeg18_24 + PctHS25_Over + PctBachDeg25_Over + 
#  PctEmployed16_Over + PctUnemployed16_Over + PctPrivateCoverage + 
#  PctPrivateCoverageAlone + PctEmpPrivCoverage + PctPublicCoverage + 
#  PctPublicCoverageAlone + PctWhite + PctBlack + PctAsian + 
#  PctOtherRace + PctMarriedHouseholds + BirthRate + Non.Good.Day.Ratio.CountyLevelImputed

#Res.Df     RSS Df Sum of Sq      F    Pr(>F)    
#1   3024 1132257                                  
#2   3021 1119910  3     12348 11.103 3.022e-07 ***

#Now lets look at insurace coverage. 
summary(cancer$PctPrivateCoverage)
summary(cancer$PctPrivateCoverageAlone)
summary(cancer$PctPublicCoverage)
summary(cancer$PctPublicCoverageAlone)

#public_2_private <- cancer$PctPublicCoverageAlone/cancer$PctPrivateCoverageAlone
#PctPublicCoverage + PctPrivateCoverage = 1
#remove either PCtPubliccoverage or PctPrivateCoverage

reg_2 <- lm(TARGET_deathRate ~ avgAnnCount + avgDeathsPerYear + incidenceRate + popEst2015  
            + PercentMarried + PctHS18_24 + PctSomeCol18_24 + PctBachDeg18_24 + PctHS25_Over + MedianAgeMale + MedianAgeFemale
            + PctBachDeg25_Over + PctEmployed16_Over + PctUnemployed16_Over + PctPrivateCoverage + PctPrivateCoverageAlone 
            + PctEmpPrivCoverage + PctPublicCoverageAlone + PctWhite + PctBlack + PctAsian + PctOtherRace
            + PctMarriedHouseholds + BirthRate + Non.Good.Day.Ratio.CountyLevelImputed , cancer)

summary(reg_2)

#should Public coverage alone be dropped?

reg_3 <- lm(TARGET_deathRate ~ avgAnnCount + avgDeathsPerYear + incidenceRate + popEst2015  
            + PercentMarried + PctHS18_24 + PctSomeCol18_24 + PctBachDeg18_24 + PctHS25_Over
            + PctBachDeg25_Over + PctEmployed16_Over + PctUnemployed16_Over + PctPrivateCoverage + PctPrivateCoverageAlone 
            + PctEmpPrivCoverage + PctWhite + PctBlack + PctAsian + PctOtherRace + MedianAgeMale + MedianAgeFemale
            + PctMarriedHouseholds + BirthRate + Non.Good.Day.Ratio.CountyLevelImputed , cancer)
summary(reg_3)
#Public Coverage alone dropped

#lets take a look at Unemployment.
reg_4 <- lm(TARGET_deathRate ~ avgAnnCount + avgDeathsPerYear + incidenceRate + popEst2015 
            + PercentMarried + PctHS18_24 + PctSomeCol18_24 + PctBachDeg18_24 + PctHS25_Over
            + PctBachDeg25_Over + PctEmployed16_Over + PctPrivateCoverage + PctPrivateCoverageAlone 
            + PctEmpPrivCoverage + PctWhite + PctBlack + PctAsian + PctOtherRace + MedianAgeMale + MedianAgeFemale
            + PctMarriedHouseholds + BirthRate + Non.Good.Day.Ratio.CountyLevelImputed , cancer)
summary(reg_4)
#unemployment can be dropped because the r-squared when it was removed only decreased by 0.0001

#like some of the other variables the sume of the race % is one. I will dropp PctWhite to compensate for this. 

reg_5 <- lm(TARGET_deathRate ~ avgAnnCount + avgDeathsPerYear + incidenceRate + popEst2015 
            + PercentMarried + PctHS18_24 + PctSomeCol18_24 + PctBachDeg18_24 + PctHS25_Over
            + PctBachDeg25_Over + PctEmployed16_Over + PctPrivateCoverage + PctPrivateCoverageAlone 
            + PctEmpPrivCoverage + PctBlack + PctWhite + PctOtherRace + MedianAgeMale + MedianAgeFemale
            + PctMarriedHouseholds + BirthRate + Non.Good.Day.Ratio.CountyLevelImputed , cancer)
summary(reg_5)
par(mfrow = c(2, 2))
plot(reg_5)

#there are 3 obs that can be removed as outliers or leverage points.

cancer_2 <- cancer[-c(172, 919, 1408, 2788),]
length(cancer$Obs)
length(cancer_2$Obs)

reg_6 <- lm(TARGET_deathRate ~ avgAnnCount + avgDeathsPerYear + incidenceRate + popEst2015  
            + PercentMarried + PctHS18_24 + PctSomeCol18_24 + PctBachDeg18_24 + PctHS25_Over
            + PctBachDeg25_Over + PctEmployed16_Over + PctPrivateCoverage  
            + PctEmpPrivCoverage + PctWhite + PctOtherRace + MedianAgeMale + MedianAgeFemale
            + PctMarriedHouseholds + BirthRate + Non.Good.Day.Ratio.CountyLevelImputed , cancer_2)

summary(reg_6)

plot(density(resid(reg_6)))
par(mfrow = c(2, 2))
plot(reg_6)
ncvTest(reg_6)

#Non-constant Variance Score Test 
#Variance formula: ~ fitted.values 
#Chisquare = 8.696224, Df = 1, p = 0.0031887
#lets add the states back in
reg_7 <- lm(TARGET_deathRate ~ avgAnnCount + avgDeathsPerYear + incidenceRate + popEst2015 
            + PercentMarried + PctHS18_24 + PctSomeCol18_24 + PctBachDeg18_24 + PctHS25_Over
            + PctBachDeg25_Over + PctEmployed16_Over + PctPrivateCoverage + PctPrivateCoverageAlone 
            + PctEmpPrivCoverage + PctBlack + PctOtherRace + PctWhite + State + MedianAgeMale + MedianAgeFemale
            + PctMarriedHouseholds + BirthRate + Non.Good.Day.Ratio.CountyLevelImputed , cancer_2)
summary(reg_7)


ggplot(cancer_2, aes(x = medIncome, y = TARGET_deathRate)) + geom_point()
cancer_2$predicted <- predict(reg_7)
cancer_2$residuals <- residuals(reg_7)





