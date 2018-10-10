#    Follow-up course Data exploration, regression, GLM & GAM course
#    Highland Statistics Ltd.
#    www.highstat.com

#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  


# Eagles solution file


#Data file: HumanEagleV3.txt
#Only first encounter data 
#NAs in Flush were deleted
#FVis smaller or equal than 500


setwd("/Users/Highstat/applicat/HighlandStatistics/Courses/FilesOnlineFollowUpRegressionGLMGAM/FinalExercises")
Eagles <- read.table(file="HumanEagleV3.txt", header = TRUE, dec = ".")

str(Eagles)
names(Eagles)


library(lattice)  #Needed for multi-panel graphs
library(mgcv)     #Needed for smoothing curves in scatterplots
source(file="/Users/Highstat/applicat/HighlandStatistics/Courses/FilesOnlineFollowUpRegressionGLMGAM/FinalExercises/HighstatLibV4.R")  


#####################################################
#Start of data exploration

#Exclude all NAs (make sure we keep enough data)
#But I already removed all non-essential 
#covariates in Excel
Eagles2 <- na.exclude(Eagles)
dim(Eagles2)
dim(Eagles)

colSums(is.na(Eagles))
#All ok now

#Make a dotchart of the continuous covariates
MyVar <- c("Flush", "Year", "Month", "DayInYear", "NumEagles",
           "Time", "DisRiv", "Height", "Fvis", "Temp", "Cloud")

Mydotplot(Eagles2[,MyVar])
#1. DisRiv looks a bit ugly
#   It woud benefit from a transformation
#2. Flush has a few large values, but it is the
#   response variable
#3. Height has probably two coding errors.
#   Remove these two observations


#Now look at the categorical variables
table(Eagles$Year)
table(Eagles$Month)
table(Eagles$Age)
table(Eagles$Activity)
table(Eagles$Breeding)
table(Eagles$fWind)
table(Eagles$fRain)
table(Eagles$fLocation)

#Age has some classes with too few observations
# It would be nice if we could recode it
#Ignore Activity
#Breeding is unbalanced
#Rain is unbalanced
#One location has only 12 observations



sort(Eagles2$Height)
#Oeps...999 was used as NA.
Eagles3 <- Eagles2[Eagles2$Height < 900,]
dim(Eagles2)
dim(Eagles3)


############################################
#Apply the transformations that we discussed 
Eagles3$LogDisRiv <- log(Eagles3$DisRiv + 1)
 


####################################################
#Collinearity covariates
#
#First we will make a pairplot of the 
#continuous covariates, and then we will
#calculate VIFs

MyVar <- c("Year", "Month", "DayInYear", "NumEagles",
           "Time", "LogDisRiv", "Height", "Fvis", "Temp", "Cloud")

pairs(Eagles3[,MyVar], lower.panel = panel.cor)

#Month and Day of year are trouble.
#In fact, year and Month are factors (only 3 years)

#Let's calculate VIF values between the continuous covariates

MyVar <- c("Year", "DayInYear", "NumEagles",
           "Time", "LogDisRiv", "Height", "Fvis", "Temp", "Cloud")

corvif(Eagles3[,MyVar])
#All ok


#Let's make some boxplots of continuous covariates 
#conditional on the factors

MyVar <- c("Year", "DayInYear", "NumEagles",
           "Time", "LogDisRiv", "Height", "Fvis", "Temp", "Cloud")
Mybwplot(Eagles3, MyVar, "Age")
# Patterns

Mybwplot(Eagles3, MyVar, "Year")
Mybwplot(Eagles3, MyVar, "Month")
 #Temperature and month are collinear
 #Use Temperature and drop month 
Mybwplot(Eagles3, MyVar, "Breeding")
Mybwplot(Eagles3, MyVar, "fWind")
Mybwplot(Eagles3, MyVar, "fRain")
 #Rain and temperature collinear?
Mybwplot(Eagles3, MyVar, "fLocation")
 #Height effect? 
 #Keep in mind that location W has less samples!



##########################################
#Relationships   
MyVar <- c("Year", "DayInYear", "NumEagles",
           "Time", "LogDisRiv", "Height", "Fvis", "Temp", "Cloud")

Myxyplot(Eagles3, MyVar, "Flush")
#Hmm...that doesn't make me happy!


#Now relationships with Time
with(Eagles3,
     plot(x = DayInYear, y = Flush))

with(Eagles3,
     plot(x = Time, y = Flush))
#No clear patterns at all



#Finally, relationships with categorical covariates
boxplot(Flush ~ Year, data = Eagles3, varwidth = TRUE)
boxplot(Flush ~ Month, data = Eagles3, varwidth = TRUE)
boxplot(Flush ~ Age, data = Eagles3, varwidth = TRUE)
boxplot(Flush ~ Breeding, data = Eagles3, varwidth = TRUE)
boxplot(Flush ~ fWind, data = Eagles3, varwidth = TRUE)
boxplot(Flush ~ fRain, data = Eagles3, varwidth = TRUE)
boxplot(Flush ~ fLocation, data = Eagles3, varwidth = TRUE)

#I think we need to drop one of the locations
Eagles4 <- Eagles3[Eagles3$fLocation != "W",]
dim(Eagles3)
dim(Eagles4)
#################################################


#Conclusions.....wow....difficult data
#For sure it would be better to aggregate some of
#the age class levels.

#Below we see a list of 13 models, all determined
#by a priori formulated biological hyptheses

M1 <- lm(Flush ~ Fvis, data = Eagles4)
M2 <- lm(Flush ~ (fWind + fRain + Temp + Cloud)^2, data = Eagles4)
M3 <- lm(Flush ~ Height + Fvis + Height : Fvis, data = Eagles4)
M4 <- lm(Flush ~ Height + Fvis + fLocation + 
                 Height : Fvis + 
                 Height : fLocation + 
                 Fvis : fLocation, data = Eagles4)

M5 <- lm(Flush ~ Age, data = Eagles4)
M6 <- lm(Flush ~ Age + Fvis + Age : Fvis, data = Eagles4)
M7 <- lm(Flush ~ fLocation * Fvis, data = Eagles4)
M8 <- lm(Flush ~ NumEagles, data = Eagles4)
M9 <- lm(Flush ~ LogDisRiv * Fvis, data = Eagles4)
M10 <- lm(Flush ~ Fvis * fLocation +
                (Temp + Cloud + fWind + fRain)^2,
          data = Eagles4)
M11 <- lm(Flush ~ Fvis + Age + fLocation + fWind, data = Eagles4)
M12 <- lm(Flush ~ factor(Year) + Time + NumEagles + 
                  LogDisRiv + Height + Fvis + Age + 
                  Breeding + fLocation + fWind + 
                  fRain + Temp + Cloud, 
          data = Eagles4)
M13 <- lm(Flush ~ 1, data = Eagles4)


###################################################
#The code to extract AIC and calculate 
#Akaike weights is as follows
AICs <- AIC(M1, M2, M3, M4, M5, M6, 
            M7, M8, M9, M10, M11, M12, M13)

MyDf <- AICs[,1]
AICsNum <- AICs[,2]
minAW <- min(AICsNum)
Delta <- AICsNum-minAW
RL <- exp(-0.5 * Delta)
wi <- RL / sum(RL)
Z <- data.frame(MyDf, AICsNum, Delta, wi)
Z <- round(Z, digits = 3)
colnames(Z)<- c("Df", "AIC", "AIC differences", "Akaike weights")
Z

##############################################
#We now need to present each model 
#with reasonable high weights

# 
#    Df      AIC AIC differences Akaike weights
# 1   3 6992.792          32.431          0.000
# 2  16 7008.438          48.077          0.000
# 3   5 6991.589          31.228          0.000
# 4  11 6966.186           5.825          0.038
# 5  12 6998.188          37.827          0.000
# 6  23 6991.535          31.173          0.000
# 7   7 6973.603          13.242          0.001
# 8   3 7008.728          48.366          0.000
# 9   5 6994.267          33.906          0.000
# 10 21 6967.364           7.003          0.021
# 11 17 6962.454           2.093          0.244 <---
# 12 27 6960.361           0.000          0.696 <---
# 13  2 7013.870          53.509          0.000


#That means we need to concentrate on models
#12 and 11. But model 12 has a lot (!!) of parameters

#Model 12 first
print(drop1(M12, test = "F"),
      signif.stars = FALSE,
      digits = 2)


# Single term deletions
# 
# Model:
#   Flush ~ factor(Year) + Time + NumEagles + LogDisRiv + Height + 
#   Fvis + Age + Breeding + fLocation + fWind + fRain + Temp + 
#   Cloud
# Df Sum of Sq     RSS  AIC F value Pr(>F)
# <none>                    2539597 5202               
# factor(Year)  2      5250 2544847 5199     0.6  0.542
# Time          1     15724 2555322 5204     3.7  0.056
# NumEagles     1      7360 2546957 5202     1.7  0.190
# LogDisRiv     1         4 2539601 5200     0.0  0.977
# Height        1     12546 2552143 5203     2.9  0.088
# Fvis          1     72524 2612121 5217    16.9  4e-05
# Age          10    154239 2693836 5218     3.6  1e-04
# Breeding      1     10315 2549913 5202     2.4  0.121
# fLocation     2     51098 2590695 5210     6.0  0.003
# fWind         2     27156 2566754 5204     3.2  0.043
# fRain         1      9925 2549523 5202     2.3  0.128
# Temp          1      6389 2545986 5201     1.5  0.222
# Cloud         1       830 2540428 5200     0.2  0.660

#An Fvis, age effect and a location effect...and that's it. 
#Weak height and time effect.
#Write your paper around this

#Model validation for M12
par(mfrow = c(2,2))
plot(M12)

#1 or 2 negative fitted values
#Seems we have some sort of heterogeneity?
#Non-normality

#Plot the residuals versus each covariate in the model

#Plot residuals versus each covariate in the model, and each
#covariate not in the model. We don't want to see any patterns

Eagles4$E12 <- rstandard(M12)
MyVar <- c("Year", "DayInYear", "NumEagles",
           "Time", "LogDisRiv", "Height", "Fvis", "Temp", "Cloud")
Myxyplot(Eagles4, MyVar, "E12")
#Ha.....heterogeneity by Fvis
#This means that if Fvis is small (the distance 
#that the eagles see the scientist for the first
#time), then they all behave similar...small variation
#But if Fvis is large, meaning the eagles see them
#for the first time at a large distance...then
#there is more variation in the residuals (and therefore
#in Flush)

#We can easily extend the model and include a heterogeneity
#structure on the residuals:
#This is how you do it:
 
#  eps ~ N(0, exp(delta * Fvis) * sigma^2)

#It can be done with the function gls, see Chapter 4
#in Zuur et al (2009)
#But we did not hypothesize this!!!!
#If we do it now then the IT folks will accuse us of
#data snooping! We should have thought about this
#before doing the analysis!
  
#We also need to plot the residuals versus each 
#categorical covariate

MyVar <- c("E12")
Mybwplot(Eagles4, MyVar, "Age") #Sample size issue
Mybwplot(Eagles4, MyVar, "Year") #ok
Mybwplot(Eagles4, MyVar, "Month") #Month 6 trouble!
Mybwplot(Eagles4, MyVar, "Breeding")
Mybwplot(Eagles4, MyVar, "fWind")
Mybwplot(Eagles4, MyVar, "fRain") #More variation with no rain?
Mybwplot(Eagles4, MyVar, "fLocation")
#Some minor trouble? Would be nice to extend
#this function and get sample size info per level on 
#it as well
  
#Further info on M12
print(summary(M12),
      digits = 2,
      signif.stars = FALSE)
#The R2 is only 15%!!!
#We will discuss this later.

#And you need to explain what it all means. The best way 
#is to skecth fitted values. We have no interactions,
#so the code is not very difficult

#if you want to present this...try to talk your way out of the
#negative fitted values, and the heterogeneity. Alternatively,
#consider applying a GLM with Gamma distribution. This will work
#here because Flush is always bigger than 0.



#You now need to do exactly the same for M11
#Model validation for M12
par(mfrow = c(2,2))
plot(M11)
#Same problems


Eagles4$E11 <- rstandard(M11)
MyVar <- c("Year", "DayInYear", "NumEagles",
           "Time", "LogDisRiv", "Height", "Fvis", "Temp", "Cloud")
Myxyplot(Eagles4, MyVar, "E11")
#Same problem

MyVar <- c("E11")
Mybwplot(Eagles4, MyVar, "Age") #Sample size issue
Mybwplot(Eagles4, MyVar, "Year") #ok
Mybwplot(Eagles4, MyVar, "Month") #Month 6 trouble!
Mybwplot(Eagles4, MyVar, "Breeding")
Mybwplot(Eagles4, MyVar, "fWind")
Mybwplot(Eagles4, MyVar, "fRain") #More variation with no rain?
Mybwplot(Eagles4, MyVar, "fLocation")
#Did I see some patterns in here? Is there a rain effect?
MTest <- lm(E11 ~ fRain, data = Eagles4)
anova(MTest)
#Nahh.....that is a very small rain effect in the residuals
#The R2 is very small.....no need to worry

print(summary(M12),
      digits = 2,
      signif.stars = FALSE)

#Tja..try to explain that!
#Here is a way to show the partial
#linear regression parameters
par(mfrow = c(2,2), mar = c(5,5,2,2))
termplot(M11, se = TRUE,
         partial.resid = FALSE,
         rug = TRUE)

#Or:

library(memisc) #need to install
Termplot(M11)

#What is in a termplot?
#Plots regression terms against their predictors, 
#optionally with standard errors and partial residuals added

#You can also use the predict function
#and show lines on the scale of the real data



#############################################
#What about the low R2?
#Here is a simulation study.
#I simulate 10000 times a data of 600 observations..
#the covariate is pure noise...what is the R2 of these
#10,000 simulations?
NBOOT <- 10000
AllR2 <- vector(length = NBOOT)
for (i in 1:NBOOT) {
  y <- rnorm(600)
  x <- rnorm(600)
  tmp <- lm(y ~ x)
  AllR2[i] <-summary(tmp)$r.squared
  print(i)
}

plot(1:NBOOT, AllR2,
     xlab = "Index",
     ylab = "R2")

hist(AllR2)
hist(AllR2, breaks = 25)
#so an R of 2% or less could have been obtained by 
#just adding covariates with pure noise 

#For smaller data sets the R2 level is higher! Instead of 600
#take 100...and then it is around 5%
################################################


#Conclusions of the data:
#1. I hope the referee would agree with the
#   selected models for IT
#2. We did not think good enough as there is
#   structured heterogeneity
#3. Low R2.
#4. Talk your way out of these 1 or 2 negative
#   fitted values
#5. And here is a potential serious problem:
#   Some of these categorical variables are
#   highly unbalanced. A referee may reject
#   it and ask you to regroup some of these
#   age class levels. Especially because
#   all models seem to indicate that Age class
#   is important
