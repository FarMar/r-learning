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
Fish <- read.table(file = "Baileyetal2008.txt",
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
names(Fish)
str(Fish)  #Make sure you have num and not factors for the numerical variables!
# 'data.frame':	148 obs. of  9 variables:
 # $ Site     : int  1 2 3 4 5 6 7 8 9 10 ...
 # $ TotAbund : int  76 161 39 410 177 695 352 674 624 736 ...
 # $ Dens     : num  0.00207 0.00352 0.000981 0.008039 0.005933 ...
 # $ MeanDepth: int  804 808 809 848 853 960 977 982 985 986 ...
 # $ Year     : int  1978 2001 2001 1979 2002 1980 1981 1979 1982 1980 ...
 # $ Period   : int  1 2 2 1 2 1 1 1 1 1 ...
 # $ Xkm      : num  98.8 76.8 103.8 91.5 107.1 ...
 # $ Ykm      : num  -57.5 178.6 -50.1 146.4 -37.1 ...
 # $ SweptArea: num  36710 45741 39775 51000 29831 ...
########################################################################




########################################################
# Task: Get familiar with the coding for bivariate 
# linear regression. Model Density as a function 
# of Meandepth
########################################################



########################################################
#Data exploration: See exercise 2.
#Remove missing value and spatial outlier 
Fish2 <- na.exclude(Fish) ### EXCLUDING 
Fish3 <- Fish2[c(-135), ]
########################################################





###################################################
#Start analysis
#Apply the linear regression model
M1 <- lm(Dens ~ MeanDepth,
         data = Fish3)
 
#Everything signficant?
summary(M1)

###################################################
#Model validation
#Look at homogeneity: plot fitted values vs residuals
#Look at influential values: Cook
#Look at independence: 
#      plot residuals vs each covariate in the model
#      plot residuals vs each covariate not in the model
#      Common sense (smelly dependence?)
#Look at normality: histogram


E1 <- resid(M1)
F1 <- fitted(M1)
par(mfrow = c(1, 1))
plot(x = F1, 
     y = E1, 
     xlab = "Fitted values",
     ylab = "Residuals")
abline(v = 0, lwd = 2, col = 2)
abline(h = 0, lty = 2, col = 1)
#Ouch...negative fitted values!!!!

par(mfrow = c(1, 1))
hist(E1, breaks = 10)


#Independence
plot(x = Fish3$MeanDepth, 
     y = E1, 
     main = "Independence")
abline(h = 0, v = 0, lty = 2)
#Ouch.....we have heterogeneity and non-linear patterns

#Also plot residuals vs each covariate not in the model
boxplot(E1 ~ Fish3$Period)

#Spatial independence?
Fish3$MyCex <- abs(E1) / max(abs(E1))
Fish3$MyCol <- E1
Fish3$MyCol[E1 >= 0 ] <- 1
Fish3$MyCol[E1 < 0 ]  <- 2

xyplot(Ykm ~ Xkm,
       cex = 4 * sqrt(Fish3$MyCex),  #This is trial and error
       pch = 16,
       col = Fish3$MyCol,
       data = Fish3,
       main = "Spatial plot of residuals")



##########################################################
#Model interpretation (though we are wasting our time as
#we should be doing something else)
summary(M1)

#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  1.090e-02  8.114e-04  13.434  < 2e-16 ***
#MeanDepth   -2.567e-06  3.004e-07  -8.544  1.7e-14 ***


#Equation:
#E(Dens_i) = 0.0109 - 0.00000256 * MeanDepth_i 

#Sketch fitted values
range(Fish3$MeanDepth)
MyData1 <- data.frame(MeanDepth = seq(from = 800, 
                                      to = 4865, 
                                      length = 25))
                      
P1 <- predict(M1, newdata = MyData1)

plot(x = Fish3$MeanDepth, 
     y = Fish3$Dens, 
     col = Fish3$Period,
     xlab = "Mean Depth",
     ylab = "Density", 
     main = "But it is all rubbish!",
     pch = 16)

lines(x = MyData1$MeanDepth, 
      y = P1, 
      col = 1, 
      lwd = 5)


