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




###########################################################
# Task: Model Density as a function of Meandepth and period
###########################################################



########################################################
#Data exploration: See exercise 2.
#Remove missing value and spatial outlier 
Fish2 <- na.exclude(Fish)
Fish3 <- Fish2[c(-135), ]
########################################################





###################################################
#Start analysis



#########################################################
#Why are we going to do GAM?
#Recall that the lm indicated a non-linear MeanDepth effect
M0 <- lm(Dens ~ MeanDepth * factor(Period), data = Fish3)

E0 <- resid(M0)
plot(x = Fish3$MeanDepth, y = E0,
     xlab = "Mean depth",
     ylab = "Residuals",
     main = "Lack of fit")
abline(0,0, lty=2)

#NOTE: the additive model will be able to cope
#with the non-linear patterns....but most likely 
#not with the heterogeneity! And neither with the
#negative fitted values. We will extend it later.
      

#########################################
#Start the GAM

#Fit a model of the form:
#Dens_i = alpha + s(MeanDepth) + Period_i + eps_i
#R code for this model:
M1 <- gam(Dens ~ s(MeanDepth) + factor(Period), 
          data = Fish3)

#Standard gam tools:
plot(M1)
summary(M1)


#Now the question is: 
#Do we need one smoother for both periods, or
#is the MeanDepth effect different per period?
#So...our two compteting models are:
# M1: Dens_i = alpha + s(MeanDepth) + Period_i + eps_i
# M2: Dens_i = alpha + s_j(MeanDepth) + Period_i + eps_i
# Note the index j to the smoother....we have one smoother
# for each period; j = 1,2

#R code for M2:
M2 <- gam(Dens ~ s(MeanDepth, by = factor(Period)) + 
                 factor(Period), 
          data = Fish3)

#Note the  by =  option. Its argument must be a factor.
#Also note that the syntax is: s(x, by = z), where x is
#MeanDepth and z is factor(Period)). The by argument is 
#inside the s() function.

#Standard tools after the GAM.
#There are 2 smoothers...one per period
par(mfrow = c(1, 2))  
plot(M2)

#Sometimes it helps to give each panel its own y-scale:
par(mfrow = c(1, 2))  
plot(M2, scale = FALSE)
#Note the range along the y-axes..they are now different.



#There is one more model that we could apply:
# M3: Dens_i = alpha + s(MeanDepth) + eps_i
# Because smoothers are centred around 0, it
# may be better not to do this model with the 
# by function. Therefore we will not fit
# M4: Dens_i = alpha + s_j(MeanDepth) + eps_i
# though technically you can do it

M3 <- gam(Dens ~ s(MeanDepth), data = Fish3)

#Standard output:
par(mfrow = c(1,1))
plot(M3)
summary(M3)


####################################################
#Model selection
#Ok...we have fitted 3 GAMs, which model is the best?

AIC(M1, M2, M3)

# df       AIC
# M1 8.227847 -1173.051  <---
# M2 8.360754 -1173.676  <---
# M3 7.410655 -1166.280

#The anova model will give you numbers as well:
#anova(M1, M2, test = "F") 
#But the problem is that M1 and M2 are strictly speaking
#not nested. So care is needed with the anova function
#to compare models with different smoothers.

#The AIC of models M1 and M2 are the smallest. Because the
#difference is AIC between M1 and M2 is less than 1,
#go for the simplest one...which is M1. 


##########################################################
#Model validation

#We will first show the numerical output of model M1
#For the sake of explaining GAMs with interactions, we 
#will also discuss M2

#All numerical output
summary(M1)
 #Scale est = variance
 #Deviance explained = R2
 #R-sq(adj) is adjusted R2
anova(M1) #This is condensed output, and applies an F test (non-sequential testing)

#Extract residuals and extract them versus residuals
E1 <- resid(M1)   
F1 <- fitted(M1)

#Do we have homogeneity?
plot(x=F1, y = E1)
#No

#Do we have normality
hist(E1, breaks = 15)
#So so..

#Do we have "independence" alias model misfit
plot(x = Fish3$MeanDepth, 
     y = E1, 
     main = "Independence")
abline(0, 0, lty = 2)
#Ah...the GAM has done its job; the non-linear pattern is gone!

#And plot the residuals also vs Period
boxplot(E1 ~ Fish3$Period)
#Hmm....not sure...

#And we need to check the "smelly" dependence
#Plot residuals versus the spatial coordinates
#hope that there is no clear patter. 
Fish3$MyCex <- abs(E1) / max(abs(E1)) 
Fish3$MyCol <- E1
Fish3$MyCol[E1 >= 0 ] <- 1
Fish3$MyCol[E1 < 0 ] <- 2

library(lattice)
xyplot(Ykm ~ Xkm,
       cex = 3 * sqrt(Fish3$MyCex),
       pch = 16,
       col = Fish3$MyCol,
       data = Fish3,
       main = "Spatial plot of residuals")
#The sqrt for the cex option is this one way of 
#getting a nice graph
#Is there spatial correlation? Well...it is not clear.
#It may be better to make a variogram, but that is outside
#the scope of this course.

#A model validation on M2 would be identical.


#######################################################
#Model interpretation
#Now we need to explain what it all means

#First we discuss M1

#Recall that the model is:
#Dens_i = intercept + f(MeanDepth) + Period + eps_i
#eps_i ~ N(0, sigma^2)

print(summary(M1), signif.stars = FALSE)
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)      0.0054951  0.0004332  12.685  < 2e-16
# factor(Period)2 -0.0021923  0.0007474  -2.933  0.00392
# 
# Approximate significance of smooth terms:
#   edf Ref.df     F  p-value
# s(MeanDepth) 5.228  6.294 15.32 7.95e-14
# 
# R-sq.(adj) =  0.414   Deviance explained = 43.9%
# GCV score = 1.8763e-05  Scale est. = 1.7834e-05  n = 146


#Let us extract the relevant bits
#The model for period 1 is:
#mu_i = 0.0054 + f(MeanDepth)

#The model for period 2 is:
#mu_i = 0.0054 - 0.0021 + f(MeanDepth)

#The f(MeanDepth) is the smoother in the graph
#It is this graph:
plot(M1) 
 
#There is a nice function vis.gam that can visualise
#simple gam models. But it only runs if factors are defined
#as factors:

Fish3$fPeriod <- factor(Fish3$Period)      
M1.also <- gam(Dens ~ s(MeanDepth) + fPeriod, 
               data = Fish3)
vis.gam(M1.also)

#And you can make a nice movie of this!
#This does not work in R-studio
Angles <- seq(1, 360, length = 100)
for (theta in Angles){
   #vis.gam(TestM1, theta = theta, se = 2)
   vis.gam(M1.also, theta = theta)
}


#We are now going to sketch the fitted values
#We will draw a line for each period
#But...what is actually the sampled depths in each period?
range(Fish3$MeanDepth[Fish3$Period == 1])
range(Fish3$MeanDepth[Fish3$Period == 2])
#So...in period 1 sampling took place from 804 to 4865
#and in period 2 between 808 and 4840 meters.

#Make 2 data frames.
MyData1 <- data.frame(MeanDepth = seq(from = 800 , to = 4865 , length = 25),
                      Period = 1)
                      
MyData2 <- data.frame(MeanDepth = seq(from = 808 , to = 4840 , length = 25),
                      Period = 2)
#NOTE: If Period is a factor, then use Period = "1" and Period = "2"

MyData1$P1 <- predict(M1, newdata = MyData1)
MyData2$P2 <- predict(M1, newdata = MyData2)

#So P1 and P2 contain the predicted values
head(MyData1)
head(MyData2)


#The rest is elementary plotting:
#Plot the raw data
#Add the smoother for period 1
#Add the smoother for period 2

plot(x = Fish3$MeanDepth, y = Fish3$Dens, col = Fish3$Period,
    xlab = "Mean Depth",
    ylab = "Density", 
    main = "But it is all rubbish!",
    pch = 16)

lines(MyData1$MeanDepth, MyData1$P1, col = 1, lwd = 5)
lines(MyData2$MeanDepth, MyData2$P2, col = 2, lwd = 5)

legend("topright",
        legend=c("Period 1","Period 2"),
        col = c(1,2),
        lty = c(1,1),
        lwd = c(5,5))



#What do the do if the referee asks for confidence bands?

#Make the two data frames again
MyData1 <- data.frame(MeanDepth = seq(from = 800 , to = 4865 , length = 25),
                      Period = 1)

MyData2 <- data.frame(MeanDepth = seq(from = 808 , to = 4840 , length = 25),
                      Period = 2)

#Now predict, but the se = TRUE option creates a 
#P1 that contains a $fit and a $se.fit  (and this does not fit inside MyData1)
P1 <- predict(M1, newdata = MyData1, se = TRUE)
P2 <- predict(M1, newdata = MyData2, se = TRUE)

#The rest is elementary plotting
 #Plot the raw data
 #Add the smoother for period 1..this is in P1$fit
 #Add the smoother for period 2..this is in P1$fit
 #Add 2 * SE to the fit...........this is P1$fit + 2 * P1$se.fit
 #Subtract 2 * SE from the fit....this is P1$fit - 2 * P1$se.fit

plot(x = Fish3$MeanDepth, y = Fish3$Dens, 
     col = Fish3$Period,
     xlab = "Mean Depth",
     ylab = "Density", main = "But it is all rubbish!",
     pch = 16)


#Add the lines for period 1
lines(MyData1$MeanDepth, P1$fit)
lines(MyData1$MeanDepth, P1$fit + 2 * P1$se.fit, lty=2)
lines(MyData1$MeanDepth, P1$fit - 2 * P1$se.fit, lty=2)

#Add the lines for period 2
lines(MyData2$MeanDepth, P2$fit, col =2)
lines(MyData2$MeanDepth, P2$fit + 2 * P2$se.fit, lty=2, col =2)
lines(MyData2$MeanDepth, P2$fit - 2 * P2$se.fit, lty=2, col =2)


#If you don't like the graph above, then use polygons.
#But the syntax for polygons is difficult.
#We won't explain the polygon syntax..that is something
#for a rainy Sunday afternoon.

plot(x = Fish3$MeanDepth, y = Fish3$Dens, 
     col = Fish3$Period,
     xlab = "Mean Depth",
     ylab = "Density", 
     main = "But it is all rubbish!",
     pch = 16)

#Fitted values 
lines(MyData1$MeanDepth, P1$fit)
lines(MyData2$MeanDepth, P2$fit)

#Upper and lower limits first period
SeUp1  <- P1$fit + 2 * P1$se.fit
SeLow1 <- P1$fit - 2 * P1$se.fit

#Upper and lower limits second period
SeUp2  <- P2$fit + 2 * P2$se.fit
SeLow2 <- P2$fit - 2 * P2$se.fit


#Ugly stuff for a rainy Sunday afternoon
x1 <- c(MyData1$MeanDepth, MyData1$MeanDepth)
y1 <- c(SeUp1,SeUp1)
polygon(c(MyData1$MeanDepth, rev(MyData1$MeanDepth)),
        c(SeLow1, rev(SeUp1)),
        col = 1, border=NULL,
        density =50)
        
#Second period
x2 <- c(MyData2$MeanDepth, MyData2$MeanDepth)
y2 <- c(SeUp2, SeUp2)
polygon(c(MyData2$MeanDepth, rev(MyData2$MeanDepth)),
        c(SeLow2, rev(SeUp2)),
        col = 2, border=NULL,
        density =50)
        
#You can also use col = grey(0.7)   where the 0.7 determines
#the amount of grey-ness
    