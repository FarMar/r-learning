#    Data exploration, regression, GLM & GAM course
#    Highland Statistics Ltd.
#    Alain Zuur & Elena Ieno
#    www.highstat.com
#
#    This program is distributed in the hope that it will be useful,
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
library(lattice)  
library(mgcv) #Load the mgcv package
library(mgcv)
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




############################################################
# Task: Model TotABund as a function of Meandepth and period
############################################################



########################################################
#Data exploration: See exercise 2.
#Remove missing value and spatial outlier 
Fish2 <- na.exclude(Fish)
Fish3 <- Fish2[c(-135), ]
########################################################





###################################################
#Start analysis

#Needed for offset
Fish3$LSA <- log(Fish3$SweptArea)


M1 <- gam(TotAbund ~ s(MeanDepth) + factor(Period) +
                     offset(LSA),
          family=poisson,
          data=Fish3)

M2 <- gam(TotAbund ~ s(MeanDepth, by = factor(Period)) +
                     factor(Period) +
                     offset(LSA),
          family=poisson,
          data=Fish3)

#Check for overdispersion
E2 <- resid(M2, type = "pearson")
Overdispersion <- sum(E2^2) / M2$df.res
Overdispersion
#Oeps....

#Apply model validation to figure out why there is overdispersion
#See GLM exercise for this data set


#One option to solve overdispersion is NB
M3 <- gam(TotAbund ~ s(MeanDepth) + factor(Period) +
                     offset(LSA),
          family=negbin(c(1,10),link = log),
          data=Fish3)
#The negbin(c(1, 10)) specifies a grid for the k parameter
#This is essentially an optimisaton routine for k on top of the gam function
#The optimisation functon will try to find the optimal k between 1 and 10
#You may need to adjust this interval for your data  

#Also apply the model with the MeanDepth x Period interaction for ths smoother
M4 <- gam(TotAbund ~ s(MeanDepth, by = factor(Period)) +
                     factor(Period) +
                     offset(LSA),
          family=negbin(c(1,10),link = log),
          data=Fish3)

E4 <- resid(M4, type = "pearson")
Overdispersion <- sum(E4^2) / M4$df.res
Overdispersion
#Perfect

#Compare models
AIC(M3, M4)
#Model with interaction slightly better
#################################################






#################################################
#Model validation
plot(x = Fish3$MeanDepth, 
     y = E4)
abline(h = 0, lty = 2)

boxplot(E4 ~ Fish3$Period)

F4 <- fitted(M4)
plot(x = F4, y = E4)
################################################




################################################
#Model interpretation
summary(M4)
par(mfrow = c(1, 2))
plot(M4)


#The model is as follows.
#TotABund_i ~ NB(mu_i, 2.27)
#
#E(TotAbund_i) = mu_i
#var(Totbund_i) = mu_i + mu_i^2 / 2.27
#
#Period 1:
#         -5.75732 + f_1(MeanDepth) + LSA_i
#mu_i = e
#
#Period 2:
#         -5.75732 -0.48107 + f_2(MeanDepth) + LSA_i
#mu_i = e
#
################################################




################################################
#Sketch fitted values
range(Fish3$MeanDepth)
MyData1 <- data.frame(MeanDepth = seq(from = 800, 
                                      to = 4865, length = 100),
                      Period = 1,
                      LSA = mean(Fish3$LSA))

MyData2 <- data.frame(MeanDepth = seq(from = 800, 
                                      to = 4865, length = 100),
                      Period = 2,
                      LSA = mean(Fish3$LSA))

P1 <- predict(M4, newdata = MyData1, se = TRUE, link = "predictor")
P2 <- predict(M4, newdata = MyData2, se = TRUE, link = "predictor")


par(mfrow = c(1, 1))
with(Fish3, plot(x = MeanDepth, y = TotAbund))
lines(MyData1$MeanDepth, exp(P1$fit), lwd = 3)
lines(MyData2$MeanDepth, exp(P2$fit), lwd = 3)


#Add the CI;
with(Fish3, plot(x = MeanDepth, y = TotAbund))

lines(MyData1$MeanDepth, exp(P1$fit), lwd = 3)
lines(MyData1$MeanDepth, exp(P1$fit + 2* P1$se.fit), lwd = 1)
lines(MyData1$MeanDepth, exp(P1$fit - 2* P1$se.fit), lwd = 1)

lines(MyData2$MeanDepth, exp(P2$fit), lwd = 3)
lines(MyData2$MeanDepth, exp(P2$fit + 2* P2$se.fit), lwd = 1)
lines(MyData2$MeanDepth, exp(P2$fit - 2* P2$se.fit), lwd = 1)


#That is not really nice...add polygons
with(Fish3, plot(x = MeanDepth, y = TotAbund))

x    <- MyData1$MeanDepth
yup  <- exp(P1$fit + 2* P1$se.fit)
ylow <- exp(P1$fit - 2* P1$se.fit)
polygon(c(x, rev(x)),
        c(yup, rev(ylow)),
        col =1,border=NULL,
        density =50   )
        
x    <- MyData2$MeanDepth
yup  <- exp(P2$fit + 2* P2$se.fit)
ylow <- exp(P2$fit - 2* P2$se.fit)
polygon(c(x, rev(x)),
        c(ylow, rev(yup)),
        col =2,border=NULL,
        density =50   )
################################################










