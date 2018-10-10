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
# Question: Has the Density - Depth relationship changed
# over time?
# Model TotAbund as a function of Meandepth, Year (or: Period)
# Take account of Xkm, Ykm
########################################################



########################################################
#Data exploration: See exercise 2.
#Remove missing value and spatial outlier 
Fish2 <- na.exclude(Fish)
Fish3 <- Fish2[c(-135), ]
########################################################



########################################################
#Some extra data exploration graphs

#Offset variable
dotchart(Fish3$SweptArea,
         xlab = "Swept area",
         ylab = "Order of the data")

dotchart(Fish3$TotAbund,
         xlab = "TotAbund",
         ylab = "Order of the data")


plot(x = Fish3$MeanDepth,
     y = Fish3$SweptArea,
     xlab = "Mean Depth",
     ylab = "Swept area")

#For the offset variable:
Fish3$LSA <- log(Fish3$SweptArea)
# Make sure there are no zeros in SweptArea ELSE WILL CRASH

plot(x = Fish3$MeanDepth,
     y = Fish3$TotAbund)

#Percentage of zeros
sum(Fish3$TotAbund == 0) / nrow(Fish3)
############################################################





############################################################
#Apply a Poisson GLM
# TotAbund_i ~ Poisson(mu_i)
# E(TotAbund_i) = mu_i   and   var(TotAbund_i) = mu_i

# log(mu_i) = Intercept + beta_1 * Period01_i +
#                         beta_2 * MeanDepth +
#                         beta_3 * Period01_i * MeanDepth_i +
#                         1 * log(SweptArea_i)
#

M1 <- glm(TotAbund ~ MeanDepth * factor(Period) + 
                     offset(LSA),
          data = Fish3, 
          family = poisson)
summary(M1)

#Check for overdispersion
E1 <- resid(M1, type = "pearson")
Overdispersion <- sum(E1^2) / M1$df.res #df.res = N-p
Overdispersion
#Oeps...
#....WHY???

#Why do you have overdispersion
 #A. Outliers                  => Remove them..but subjective
 #B. Missing covariates        => Add them
 #C. Missing interactions      => Add them (coplot)
 #D. Zero inflation            => ZIP/ZINB
 #E. Dependency                => GLMM
 #F. Non-linear relationships  => GAM
 #G. Wrong link function       => Change it
 #H. Large variation           => NB GLM


#Plot E1 vs all covariates
#Look for outliers
#Check for temporal or spatial correlation
#Check for zero inflation
#If nothing can be pinpointed....go to NB GLM


#MAKE PICTURES!!!!!
plot(x=Fish3$MeanDepth, y= E1)
#Homework: do all other steps

#For spatial correlation:
#Y_i ~ Poisson(mu_i)
#        alpha + MeanDepth * factor(Period) + eps_i 
#mu_i = e
#eps_i = spatially correlated....CAR correlation
# INLA for spatial correlation


#Apply a NB GLM
library(MASS)
M2 <- glm.nb(TotAbund ~ MeanDepth * factor(Period) +
                        offset(LSA), 
             link = "log",
             data = Fish3)
summary(M2)
#Check overdispersion
E2 <- resid(M2, type = "pearson")
Overdispersion <- sum(E2^2) / M2$df.res
Overdispersion
#If still overdispersed...identify why
# Outliers?
# Missing covariates or interactions?
# Non-linear patterns?
# Correlation


#The model that we fitted:
#TotAbund_i ~ NB(mu_i, k)
#E(TotAbund_i) = mu_i
#var(TotAbund_i) = mu_i + mu_i^2 / k
#log(mu_i) = alpha + Period + MeanDepth + 
#            Period x MeanDepth + offset(LSA)

print(summary(M2), digits = 2, signif.stars=FALSE)

#Model selection
drop1(M2, test = "Chi")  #Hypothesis testing
#Or:
step(M2)  #AIC


#Drop the interaction
M3 <- glm.nb(TotAbund ~ MeanDepth + factor(Period) +
             offset(LSA), data = Fish3)

print(summary(M3), digits = 2, signif.stars=FALSE)
drop1(M3, test = "Chi")



#summary output
# Coefficients:
#                 Estimate Std. Error z value Pr(>|z|)
# (Intercept)     -3.3e+00    1.4e-01   -24.2   <2e-16
# MeanDepth       -1.0e-03    4.9e-05   -20.8   <2e-16
# factor(Period)2 -4.3e-01    1.3e-01    -3.4    7e-04
#......
#(Dispersion parameter for Negative Binomial(1.9438) family taken to be 1)
###############################################




###############################################
#NEXT: 
#    Model validation
#    Model interpretation
###############################################


#Model validation
par(mfrow = c(2, 2))
plot(M3)

#Or:
E3 <- resid(M3, type = "pearson")
F3 <- fitted(M3)
plot(x = F3, 
     y = E3,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, v = 0, lty = 2)


#Influential observations
plot(M3, which = c(4))

par(mfrow=c(1,1))
plot(cooks.distance(M3),
     type = "h",
     ylim = c(0,1))
abline(h=1)

#Normality
hist(E3, breaks = 15)

#Independence:
plot(x = Fish3$MeanDepth, 
     y = E3,
     xlab = "Mean depth",
     ylab = "Residuals")
abline(h = 0, v = 0, lty=2)

#Per period:
xyplot(E3 ~ MeanDepth | factor(Period), 
       data = Fish3,
       panel = function(x,y){
         panel.points(x,y, col = 1, pch = 16, cex = 0.7)
         panel.loess(x,y, col = 1, lwd = 2)
         panel.abline(h=0)
       })
#Oeps....perhaps we should apply GAM

boxplot(E3 ~ Period, data = Fish3)

#Spatial independence?
Fish3$MyCex <- abs(E3) / max(abs(E3))
Fish3$MyCol <- E3
Fish3$MyCol[E3 >= 0 ] <- 1
Fish3$MyCol[E3 < 0 ] <- 2

xyplot(Ykm ~ Xkm,
       aspect = "iso",
       cex = 3 * Fish3$MyCex,
       pch = 16,
       col = Fish3$MyCol,
       data = Fish3,
       main = "Spatial plot of residuals")
       
       
Fish3$E3 <- E3    
library(ggplot2)   
p <- ggplot(data = Fish3, 
                    aes(y = E3, x = MeanDepth))
p <- p + geom_point()

p <- p + geom_smooth(colour = 1)                    
p <- p + xlab("Mean depth (m)") + ylab("Residuals")
p <- p + theme(text = element_text(size=15)) + theme_bw()
p <- p + geom_hline(yintercept=0)

p <- p + facet_grid(. ~ Period, scales = "fixed")
p  

       
       
##############################################





##############################################
#Model interpretation:
# Model:
# Abund_i ~ NB(mu_i, 1.94)
# E(Abund_i) = mu_i
# var(Abund_i) = mu_i + mu_i^2 / 1.94
# 
# #Period 1:
# log(mu_i) = -3.3 - 0.001 * MeanDepth_i + LSA_i
# 
# #Period 2:
# log(mu_i) = -3.3 - 0.43 - 0.001 * MeanDepth_i + LSA_i
#           = -3.73 - 0.001 * MeanDepth_i + LSA_i

#Sketch lines
mean(Fish3$LSA)
tapply(Fish3$LSA, FUN = mean,  INDEX = Fish3$Period)
MyData1 <- data.frame(MeanDepth=seq(from = 804,
                                    to = 4865,
                                    length = 10),
                      Period = 1,
                      LSA = log(mean(Fish3$SweptArea)))

MyData2 <- data.frame(MeanDepth=seq(from = 804,
                                    to = 4865,
                                    length = 10),
                      Period = 2,
                      LSA = log(mean(Fish3$SweptArea)))

P1 <- predict(M3, newdata = MyData1, type = "response")
P2 <- predict(M3, newdata = MyData2, type = "response")

NicePch <- Fish3$Period
NicePch[Fish3$Period==1] <- 1
NicePch[Fish3$Period==2] <- 16

plot(x = Fish3$MeanDepth, y = Fish3$TotAbund,
     pch=NicePch,
     col = Fish3$Period)
lines(MyData1$MeanDepth, P1, lty = 1,col = 1, lwd = 3)
lines(MyData2$MeanDepth, P2, lty = 1,col = 2, lwd = 3)

legend("topright",legend=c("Period 1", "Period 2"),
       col = c(1,2), lty=c(1,1),
       lwd=c(3,3),
       cex=0.5)


#With CI
P1 <- predict(M3, newdata = MyData1, type="link", se = TRUE)
P2 <- predict(M3, newdata = MyData2, type="link", se = TRUE) 
#This has a $fit   and a $se.fit

plot(x = Fish3$MeanDepth, 
     y = Fish3$TotAbund,
     pch = NicePch,
     col = Fish3$Period,
     type = "n")
lines(MyData1$MeanDepth, exp(P1$fit), lty=1,col=1, lwd=3)
lines(MyData1$MeanDepth, exp(P1$fit + 2 * P1$se.fit), lty=1,col=1, lwd=3)
lines(MyData1$MeanDepth, exp(P1$fit - 2 * P1$se.fit), lty=1,col=1, lwd=3)


lines(MyData2$MeanDepth, exp(P2$fit), lty=1,col=2, lwd=3)
lines(MyData2$MeanDepth, exp(P2$fit + 2 * P2$se.fit), lty=1,col=2, lwd=3)
lines(MyData2$MeanDepth, exp(P2$fit - 2 * P2$se.fit), lty=1,col=2, lwd=3)

legend("topright",
       legend = c("Period 1", "Period 2"),
       col = c(1, 2), 
       lty = c(1, 1),
       lwd = c(3, 3),
       cex = 0.5)


#####################################################

#Does it make sense to use the offset?

#Drop the interaction
M4 <- glm.nb(TotAbund ~ MeanDepth + factor(Period) + LSA, data = Fish3)
summary(M4)
#Is the estimated regression parameter for LSA close to 1?
#If so, then the offset was a good thing to do. Otherwise
#use LSA (or Swept Area) as a covariate.


##################################################

 
  
#So..we should do this:
Fish3$fPeriod <- factor(Fish3$Period)
M5 <- glm.nb(TotAbund ~ MeanDepth + fPeriod + LSA, data = Fish3)

range(Fish3$MeanDepth)
range(Fish3$LSA)

MyData <- expand.grid(MeanDepth = seq(804, 4865, length = 25),
                      LSA       = seq(8.9, 12.3, length = 25),
                      fPeriod    = levels(Fish3$fPeriod))
MyData                      
                      
P1 <- predict(M5, newdata = MyData, type="link", se = TRUE)



MyData$mu <- exp(P1$fit)

library(rgl)

plot3d(x = Fish3$MeanDepth ,
       y = Fish3$LSA,
       z = Fish3$TotAbund,
       type = "p",
       size = 5,
       lit = FALSE,
       xlab = "MeanDepth",
       ylab = "LSA",
       zlab = "TotAbund",
       col = Fish3$Period)

#Add the surface for the fitted Poisson values
#For this we need to have the 25 X1 and 25 X2 values
#that we used to create the grid. Because we simulated
#them from a uniform distribution, they are nicely spread
#between 0 and 1. Later on, when we use real covariates, quite
#often multiple observations have the same covariate values. Then
#it may help the visualisation process to add some random noise.
X1.25 = seq(804, 4865, length = 25)
X2.25 = seq(8.9, 12.3, length = 25)



#And we convert the vector with expected values, ExpY, into
#a 25 by 25 matrix
MyDataP1 <- MyData[MyData$fPeriod == "1",]
MyDataP2 <- MyData[MyData$fPeriod == "2",]

ExpY1.2d <- matrix(MyDataP1$mu, nrow = length(X1.25), ncol = length(X1.25))
ExpY2.2d <- matrix(MyDataP2$mu, nrow = length(X1.25), ncol = length(X1.25))

#And we are ready to plot the expected values
surface3d(X1.25, X2.25, ExpY1.2d, 
          alpha = 0.6, 
          front = "lines", 
          back = "lines", 
          color = "black")

surface3d(X1.25, X2.25, ExpY2.2d, 
          alpha = 0.6, 
          front = "lines", 
          back = "lines", 
          color = "red")
  
  




