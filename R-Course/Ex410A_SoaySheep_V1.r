#    Highland Statistics Ltd.
#    www.highstat.com

#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

#############################################################################

#Set the working directory and load the data
setwd("/Users/Highstat/applicat/HighlandStatistics/Courses/Data/")
SA <- read.table("SheepAnitbodiesV2.txt", 
                 header = TRUE,
                 dec = ".")
str(SA)
names(SA)

# [1] "IDx"    "Year"   "IgATot" "IgAKLH" "IgMKLH" "IgATc"  "IgGTc"  "ANAnc"
# [9] "Weight" "FEC"    "Age"    "SURV"


# These data are taken from:
# Nussey DH, Watt KA, Clark A, Pilkington JG, Pemberton JM, 
# Graham AL, McNeilly TN (2014) Multivariate immune defences and 
# fitness in the wild: complex but ecologically important associations 
# among plasma antibodies, health and survival. Proceedings of 
# the Royal Society B 281(1779): 20132931.

# Nussey et al., 2015 looked at the relationships between plasma antibodies
# and health status in Soay sheep (Ovis aries).

# From the paper:
# Antibodies circulating in the blood of an individual at a given point
# in time will provide indication for protection against infections.
# There is increasing evidence that strong associations exist between
# measures of antibody-mediated immunity and fitness-related traits
# in natural populations. The bulk of this evidence comes from studies
# of survival in juvenile animals although there is also some support
# from immunologically mature and experienced adults.

#Health status in their paper is quantified as:
#   Survival,
#   The weight of the animal taken in August
#   August faceal egg counts of parasited (FEC)

# So the looked at 3 respons evariables versus the
# antonodies. In this exercise Here we analyse the survival 
# data (Over-winter survival probability).



#RESPONSES
#"Weight" "FEC""SURV"
# HERE WE WILL USE THE RESPONSE SURV (BINARY RESPONSE)


#COVARIATES (all antibodies)
# "IgATot" "IgAKLH" "IgMKLH" "IgATc"  "IgGTc"  "ANAnc

# Others
# "Year", "Age"
########################################################



######################################################################
#Load packages and library files
library(lattice)
source("HighstatLibV9.R")
######################################################################



#Housekeeping
SA$fYear    <- factor(SA$Year)



#########################################################
#DATA EXPLORATION

# Missing values?
colSums(is.na(SA)) 
# IDx   Year IgATot IgAKLH IgMKLH  IgATc  IgGTc  ANAnc Weight    FEC    Age
#    0      0      0      0      0      0      0      0      2     10      0
# SURV
#    0

#No missing values for the response SURV


#How many years?
table(SA$fYear)
#1998 2001 2004
#  64   57   69


#How many zeros and ones?
table(SA$SURV)
#0   1
#72 118
#Binary data well represented


#Outliers in the covariates?
#The covariates are: IgATot IgAKLH IgMKLH  IgATc  IgGTc  ANAnc

Myvar <- c("IgATot","IgAKLH","IgMKLH",
            "IgATc","IgGTc","ANAnc","Age")
Mydotplot(SA[,Myvar])

# Some of them may have smamm outliers, e.g.  IgAKLH
# Let's wait doing a transformation.



#############
#Colinearity
Mypairs(SA[,Myvar])
corvif(SA[,Myvar])
# Some stuff going on!
# Let's dump IgATot and IgAKLH

Myvar2 <- c("IgATc","IgMKLH","IgGTc","ANAnc","Age")
Mypairs(SA[,Myvar2])
#Looks ok


#Now let's see if there is collinearity with factor Year
par(mfrow = c(2,3))
boxplot(IgATc ~ factor(Year), data = SA) #ok
boxplot(IgMKLH ~ factor(Year), data = SA) #ok
boxplot(IgGTc ~ factor(Year), data = SA) # Not good
boxplot(ANAnc ~ factor(Year), data = SA) # So so
boxplot(Age ~ factor(Year), data = SA) #ok

#I think it is better not to use Year as the target antibody is IgGTc!
###########################################################################




###########################################################################
## START of the Analysis
M1 <- glm(SURV ~IgATc + IgMKLH + ANAnc + IgGTc + Age,
          data = SA,
          family = binomial)

summary(M1)
#Coefficients:
#            Estimate Std. Error z value Pr(>|z|)
#(Intercept) -0.93994    0.89471  -1.051  0.29347
#IgATc        0.78632    0.46462   1.692  0.09057 .
#IgMKLH      -0.70099    0.51071  -1.373  0.16988
#ANAnc        2.95394    1.09264   2.703  0.00686 **
#IgGTc        1.63990    0.59766   2.744  0.00607 **
#Age         -0.21009    0.06795  -3.092  0.00199 **
---

#(Dispersion parameter for binomial family taken to be 1)

#    Null deviance: 252.15  on 189  degrees of freedom
#Residual deviance: 218.95  on 184  degrees of freedom
#AIC: 230.95


#What is the explained variation?



###################################################
#Model validation for 0-1 data is an art





###################################################
#Model interpretation

#Task: Write down the estimated model.


#Sketch the fitted values

#1. Plot the fitted values versus IgGtc for average 'all other' covariates. 
range(SA$IgGTc)
#[1] 0.3041 1.9497

MyData <- expand.grid(IgATc  = mean(SA$IgATc),
                      IgMKLH = mean (SA$IgMKLH),
                      ANAnc  = mean(SA$ANAnc),
                      IgGTc  = seq(0.30, 1.95, length = 25),
                      Age    = mean(SA$Age))

P1          <- predict(M1, newdata = MyData, se = TRUE, type = "link")
MyData$Pi   <- exp(P1$fit) / (1 + exp(P1$fit))
MyData$SeUp <- exp(P1$fit + 1.96*P1$se.fit) / (1 + exp(P1$fit + 1.96*P1$se.fit))
MyData$SeLo <- exp(P1$fit - 1.96*P1$se.fit) / (1 + exp(P1$fit - 1.96*P1$se.fit))
MyData


library(ggplot2)

p <- ggplot()
p <- p + geom_point(data = SA,
                    aes(y = SURV, x =IgGTc),
                    shape = 1,
                    size = 1)
p <- p + xlab("IgG T. circumcincta") + ylab("Over-winter survival probbility")
p <- p + theme(text = element_text(size=15)) + theme_bw()
p <- p + geom_line(data = MyData,
                   aes(x =IgGTc, y = Pi),
                   colour = "black")

p <- p + geom_ribbon(data = MyData,
                     aes(x = IgGTc,
                         ymax = SeUp,
                         ymin = SeLo ),
                     alpha = 0.2)

p



###################################################################
###And if we used Age as the variable along the x-axis we get
range(SA$Age)
# 2 11

MyData <- expand.grid(IgATc  = mean(SA$IgATc),
                      IgMKLH = mean (SA$IgMKLH),
                      ANAnc  = mean(SA$ANAnc),
                      IgGTc  = mean(SA$IgGTc),
                      Age    = seq(2, 11, length = 25))



P1          <- predict(M1, newdata = MyData, se = TRUE, type = "link")
MyData$Pi   <- exp(P1$fit) / (1 + exp(P1$fit))
MyData$SeUp <- exp(P1$fit + 1.96*P1$se.fit) / (1 + exp(P1$fit + 1.96*P1$se.fit))
MyData$SeLo <- exp(P1$fit - 1.96*P1$se.fit) / (1 + exp(P1$fit - 1.96*P1$se.fit))
MyData


p <- ggplot()
p <- p + geom_point(data = SA,
                    aes(y = SURV, x =Age),
                    shape = 1,
                    size = 1)
p <- p + xlab("Age") + ylab("Over-winter survival probbility")
p <- p + theme(text = element_text(size=15)) + theme_bw()
p <- p + geom_line(data = MyData,
                   aes(x =Age, y = Pi),
                   colour = "black")

p <- p + geom_ribbon(data = MyData,
                     aes(x = Age,
                         ymax = SeUp,
                         ymin = SeLo ),
                     alpha = 0.2)

p
#Do some jittering
#####################################################



#And as a 3-d picture:
MyData <- expand.grid(IgATc  = mean(SA$IgATc),
                      IgMKLH = mean (SA$IgMKLH),
                      ANAnc  = mean(SA$ANAnc),
                      IgGTc  = seq(0.30, 1.95, length = 25),
                      Age    = seq(2, 11, length = 25))


P1          <- predict(M1, newdata = MyData, se = TRUE, type = "link")
MyData$Pi   <- exp(P1$fit) / (1 + exp(P1$fit))
MyData$SeUp <- exp(P1$fit + 1.96*P1$se.fit) / (1 + exp(P1$fit + 1.96*P1$se.fit))
MyData$SeLo <- exp(P1$fit - 1.96*P1$se.fit) / (1 + exp(P1$fit - 1.96*P1$se.fit))
MyData


library(rgl)

#These are the two variables that we use
#to calculate our grid for:
X1 <- seq(0.30, 1.95, length = 25) #IgGTc
Y1 <- seq(2, 11, length = 25)      #Age

#And we convert the vector with expected values into
#a 25 by 25 matrix
Pi.2d <- matrix(MyData$Pi, nrow = length(X1), ncol = length(Y1))


plot3d(x = SA$IgGTc,
       y = SA$Age,
       z = SA$SURV,
       type = "p",
       size = 2,
       lit = FALSE,
       xlab = "IgGTc",
       ylab = "Age",
       zlab = "SURV")#,
#       col = MyColour)

#Add the surface for the fitted Poisson values
surface3d(X1, Y1, Pi.2d, 
          alpha = 0.6, 
          front = "lines", 
          back = "lines", 
          color = "black")


#Task: 1. Add the surfaces for the lower and uppr 95% CI as well.
#         Use a different colour for this.
#      2. Add some jittering.

###############################################################################