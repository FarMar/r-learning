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
#Housekeeping
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
# Question: Has the Density - Depth relationship changed
# over time?
# Model Dens as a function of Meandepth, Year (or: Period)
# Take account of Xkm, Ykm
########################################################


########################################################
#Data exploration: See exercise 2.
#Remove missing value and spatial outlier 
Fish2 <- na.exclude(Fish)
Fish3 <- Fish2[c(-135), ]
########################################################





########################################################
#Start analysis
#Apply the linear regression model
M1 <- lm(Dens ~ MeanDepth + factor(Period) + 
                MeanDepth : factor(Period),
         data = Fish3)
 
#Same model, different coding:
#M1 <- lm(Dens ~ MeanDepth * factor(Period),
#         data = Fish3)

#Everything signficant?
summary(M1)
drop1(M1, test = "F")  #Same info as there are no factors with > 2 levels
##########################################################





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

#Normality
par(mfrow = c(1, 1))
hist(E1, breaks = 10)


#Independence
plot(x = Fish3$MeanDepth, 
     y = E1, 
     main = "Independence")
abline(h = 0,  lty = 2)
#Ouch.....we have heterogeneity and non-linear patterns
boxplot(E1 ~ Fish3$Period)

#Spatial independence?
Fish3$MyCex <- abs(E1) / max(abs(E1))
Fish3$MyCol <- E1
Fish3$MyCol[E1 >= 0 ] <- 1
Fish3$MyCol[E1 < 0 ]  <- 2

library(lattice)
xyplot(Ykm ~ Xkm,
       cex = 4 * sqrt(Fish3$MyCex),
       pch = 16,
       col = Fish3$MyCol,
       data = Fish3,
       main = "Spatial plot of residuals")
##########################################################





##########################################################
#Model interpretation (though we are wasting our time as
#we should be doing something else)
summary(M1)


#(Intercept)                1.280e-02  9.638e-04  13.279  < 2e-16 ***
#MeanDepth                 -3.042e-06  3.587e-07  -8.481 2.63e-14 ***
#factor(Period)2           -5.549e-03  1.640e-03  -3.383 0.000927 ***
#MeanDepth:factor(Period)2  1.378e-06  6.046e-07   2.279 0.024180 *  

        
#Equations:
#For period 1:
# Dens = 0.0128 - 0.00000304 * MeanDepth 
#
#For period 2:
# Dens = 0.0128 - 0.00000304 * MeanDepth +
#        -0.005549 +
#        0.00000137 * MeanDepth 
#       = (0.0128 -0.005549) + (- 0.00000304 + 0.00000137) * MeanDepth


##########################################################
#Sketch fitted values

range(Fish3$MeanDepth[Fish3$Period == 1])
range(Fish3$MeanDepth[Fish3$Period == 2])
tapply(Fish3$MeanDepth, INDEX = Fish3$Period, FUN = range)
MyData1 <- data.frame(MeanDepth = seq(from = 800 , to = 4865 , 
                                       length = 25),
                      Period = 1)
                      
MyData2 <- data.frame(MeanDepth = seq(from = 800 , to = 4840 , length = 25),
                      Period = 2)
                      
P1 <- predict(M1, newdata = MyData1)
P2 <- predict(M1, newdata = MyData2)

plot(x = Fish3$MeanDepth, 
     y = Fish3$Dens, 
     col = Fish3$Period,
     xlab = "Mean Depth",
     ylab = "Density", 
     main = "But it is all rubbish!",
     pch = 16)

lines(x = MyData1$MeanDepth, y = P1, col = 1, lwd = 5)
lines(x = MyData2$MeanDepth, y = P2, col = 2, lwd = 5)

legend("topright",
        legend = c("Period 1", "Period 2"),
        col = c(1,2),
        lty = c(1,1),
        lwd = c(5,5))
#########################################################





#########################################################
#And some ggplot2 code


library(plyr)
library(ggplot2)

MyData <- ddply(Fish3, .(Period), summarize,
                MeanDepth = seq(min(MeanDepth), max(MeanDepth),
                                length = 10))
MyData

P <- predict(M1, newdata = MyData, se = TRUE)

#Add fitted values and confidence bands
MyData$mu    <- P$fit  #Fitted values
MyData$selow <- P$fit - 2 * P$se.fit  #lower bound
MyData$seup  <- P$fit + 2 * P$se.fit  #upper bound
head(MyData)

p <- ggplot()
p <- p + geom_point(data = Fish3, 
                    aes(y = Dens, x = MeanDepth),
                    shape = 16, 
                    size = 3)
p <- p + xlab("Mean depth (m)") + ylab("Density in ..")
p <- p + theme(text = element_text(size=15)) + theme_bw()
p <- p + geom_line(data = MyData, 
                   aes(x = MeanDepth, y = mu), 
                   colour = "black")

p <- p + geom_ribbon(data = MyData, 
                     aes(x = MeanDepth, 
                         ymax = seup, 
                         ymin = selow ),
                     alpha = 0.5)

p <- p + facet_grid(. ~ Period, scales = "fixed")
p  

