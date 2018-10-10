#    Data exploration, regression, GLM & GAM course
#    Highland Statistics Ltd.
#    www.highstat.com

#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.



#######################################################################
#set the working directory & read the data
#On a Windows computer: adjust blah blah blah
#setwd("c:/blah blah blah")

#For a Mac:
setwd("/Users/Highstat/applicat/HighlandStatistics/Courses/Data")


#Import the data from a tab delimited ascii file
IrishPh <- read.table(file = "IrishPh.txt", 
                      header = TRUE,
                      dec = ".")
#dec = '.'   means that the point is used for decimals. 
#Change to   dec = ","   if required.
########################################################################



########################################################################
#House keeping
#Load packages from R and support functions that we wrote

library(lattice)  #For fancy multipanel graphs

#Ensure that the file HighstatLibV9.R is in your working directory
#Ensure it is a .R file and not a .R.txt file!!!!!! 

source("HighstatLibV9.R")
########################################################################




########################################################################
#Inspect the file
names(IrishPh)
str(IrishPh)  #Make sure you have num and not factors for the numerical variables!
# 'data.frame':	210 obs. of  7 variables:
 # $ ID      : int  27 28 29 30 31 32 33 34 35 36 ...
 # $ Easting : int  117294 114989 115931 120904 147715 159527 157561 157351 158982 167071 ...
 # $ Northing: int  161990 165877 182021 181242 184192 189332 176617 172758 165623 170498 ...
 # $ Altitude: int  45 73 75 72 89 60 68 48 49 65 ...
 # $ Forested: int  2 1 2 2 2 2 1 2 2 2 ...
 # $ pH      : num  7.24 6.45 6.92 7.09 6.98 7.36 7.24 7.37 7.34 7.65 ...
 # $ SDI     : num  48.8 73.5 67.1 53.2 57.9 ...
########################################################################




############################################################
#DATA CODING
IrishPh$fForested <- factor(IrishPh$Forested,
                        levels = c(1, 2),
                        labels = c("Forested", "NonForested"))
############################################################




############################################################
#DATA EXPLORATION
#Spatial positions
xyplot(Northing ~ Easting,
       aspect = "iso",
       col = 1, 
       pch = 16,
       data = IrishPh)

#Outliers
MyVar <- c("pH","Altitude","SDI")
Mydotplot(IrishPh[,MyVar])


#Should we transform Altitude?
#Perhaps yes..but let's make a scatterplot of
#Altitude and pH first before doing anything
plot(x = IrishPh$Altitude, 
     y = IrishPh$pH)

#Yes...let's transform altitude
IrishPh$LOGAltitude <- log10(IrishPh$Altitude)


#Collinearity
cor(IrishPh$LOGAltitude, IrishPh$SDI)
plot(x = IrishPh$LOGAltitude, 
     y = IrishPh$SDI)

Z <- cbind(IrishPh$LOGAltitude, IrishPh$SDI)
colnames(Z) <- c("Log Altitude","SDI")
pairs(Z)

MySel <- c("pH","Altitude","SDI")
Mypairs(IrishPh[,MySel])



#Relationships
MySel <- c("pH","Altitude","SDI")
Mypairs(IrishPh[,MySel])
 
boxplot(pH ~ fForested, data = IrishPh)

#Interactions
coplot(pH ~ LOGAltitude | fForested,
       data = IrishPh,
        panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })


coplot(pH ~ SDI | fForested,
         data = IrishPh,
        panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })


coplot(pH ~ LOGAltitude | SDI * fForested,  number = 4 ,
        data = IrishPh,
        panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })
#############################################################



#############################################################
#Start analysis
#Apply linear regression model
M1 <- lm(pH ~ LOGAltitude + SDI + fForested +
              LOGAltitude : SDI  +
              LOGAltitude : fForested +
              SDI : fForested +
              LOGAltitude : SDI : fForested,
              data = IrishPh )

# A + B + C + A:B + A:C + B:C + A:B:C

summary(M1)
#############################################################
#Everything significant....no  -->  MODEL SELECTION

step(M1)


#Optimal model as judged by AIC

M2 <- lm(pH ~ LOGAltitude + SDI + fForested +
               LOGAltitude:fForested,
               data = IrishPh)
 
summary(M2)

 
#Coefficients:
#                                   Estimate Std. Error t value Pr(>|t|)
#(Intercept)                        9.397836   0.379420  24.769  < 2e-16 ***
#LOGAltitude                       -0.476696   0.175927  -2.710  0.00731 **
#SDI                               -0.024612   0.001694 -14.530  < 2e-16 ***
#fForested_NonForested             -1.294020   0.457941  -2.826  0.00518 **
#LOGAltitude:fForested_NonForested  0.659620   0.217136   3.038  0.00269 **


#Equations:
# Ph = alpha + beta_1 * LogALT + beta_2 * SDI
#      beta_3 * F
#      beta_4 * F * LogAlt
#
#
# fForested = "Forested"
# Ph = 9.39  -0.47 * LogALT -0.02 * SDI
#
# fForested = "NonForested"
# Ph = 9.39 -1.29 + (-0.47+0.65) * LogALT -0.02 * SDI
#    = 8.19  + 0.18 * LogALT -0.02 * SDI
#


#Model validation
#Homogeneity
E2 <- rstandard(M2)
F2 <- fitted(M2)
plot(x = F2, y = E2)
abline(h = 0, v = 0)

#Normality
hist(E2)

#Independence due to model misfit
plot(x = IrishPh$LOGAltitude, 
     y = E2)
abline(h = 0, v = 0)

plot(x = IrishPh$SDI, 
     y = E2)
abline(h = 0, v = 0)

boxplot(E2 ~ fForested, data = IrishPh)
abline(h = 0)

#Spatial patterns in the residuals?
MyCex <- 3 * abs(E2) / max(E2)
xyplot(Northing ~ Easting,
       data = IrishPh,
       cex = MyCex,
       col = 1)
       
#Or make a variogram
###########################################################################




###########################################################################
#Model interpretation
summary(M2)
#
#                                  Estimate Std. Error t value Pr(>|t|)
#(Intercept)                       9.397836   0.379420  24.769  < 2e-16 ***
#LOGAltitude                      -0.476696   0.175927  -2.710  0.00731 **
#SDI                              -0.024612   0.001694 -14.530  < 2e-16 ***
#fForestedNonForested             -1.294020   0.457941  -2.826  0.00518 **
#LOGAltitude:fForestedNonForested  0.659620   0.217136   3.038  0.00269 **
#-
#

#We have already seen the equations:
#fForested = "Forested"
#pH = 9.397836 -0.476696 * LogAltitude -0.024612 * SDI

#fForested = "NonForested"
#pH = 9.397836 -1.294020 + (-0.476696+0.659620) * LogAltitude -0.024612 * SDI
#   = 8.10 + 0.18 * LogAltitude -0.024612 * SDI


#Skecth curves
M2 <- lm(pH ~ LOGAltitude + SDI + fForested +
              LOGAltitude:fForested,
         data = IrishPh)
summary(M2)


#Plot the model
plot(x = IrishPh$LOGAltitude, 
     y = IrishPh$pH, 
     cex = 0.8,
     pch = 16, 
     col=as.numeric(IrishPh$fForested))

MyData1 <- data.frame(LOGAltitude =
                       seq(from = min(IrishPh$LOGAltitude),
                           to = max(IrishPh$LOGAltitude),
                           length=10),
                      SDI = mean(IrishPh$SDI),
                      fForested = "Forested")
                      
MyData2 <- data.frame(LOGAltitude =
                       seq(from = min(IrishPh$LOGAltitude),
                           to = max(IrishPh$LOGAltitude),
                           length=10),
                      SDI = mean(IrishPh$SDI),
                      fForested = "NonForested")

P1 <- predict(M2, newdata = MyData1)
P2 <- predict(M2, newdata = MyData2)

lines(MyData1$LOGAltitude, P1, col = 1, lwd = 2)
lines(MyData2$LOGAltitude, P2, col = 2, lwd = 2)
legend("bottomleft", legend = c("For for average SDI", "NFor for average SDI"),
       col = c(1,2), lty = c(1,1))


