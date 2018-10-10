#    Highland Statistics Ltd.
#    Alain Zuur & Elena Ieno
#    www.highstat.com

#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.




###################################################################
#Set the working directory and import the data
setwd("C:/Courses/Palermo")
#setwd("/Users/Highstat/applicat/HighlandStatistics/Courses/FilesRegressionGLMGAMCourse/CD/Data/")
Fish <- read.table(file = "Baileyetal2008.txt",
                   header = TRUE)

str(Fish)
names(Fish)

# > str(Fish)
# 'data.frame':	148 obs. of  10 variables:
 # $ Site        : int  1 2 3 4 5 6 7 8 9 10 ...
 # $ TotAbund    : int  76 161 39 410 177 695 352 674 624 736 ...
 # $ Dens        : num  0.00207 0.00352 0.000981 0.008039 0.005933 ...
 # $ MeanDepth   : int  804 808 809 848 853 960 977 982 985 986 ...
 # $ Year        : int  1978 2001 2001 1979 2002 1980 1981 1979 1982 1980 ...
 # $ Period      : int  1 2 2 1 2 1 1 1 1 1 ...
 # $ Xkm         : num  98.8 76.8 103.8 91.5 107.1 ...
 # $ Ykm         : num  -57.5 178.6 -50.1 146.4 -37.1 ...
 # $ SweptArea   : num  36710 45741 39775 51000 29831 ...
 # $ LogSweptArea: num  10.5 10.7 10.6 10.8 10.3 ...
# > names(Fish)
 # [1] "Site"         "TotAbund"     "Dens"         "MeanDepth"    "Year"         "Period"      
 # [7] "Xkm"          "Ykm"          "SweptArea"    "LogSweptArea"
###################################################################




###################################################################
#Load packages and source our library file Highstatlib.R
library(lattice)

#You need to adjust the path!
#Windows OS:
#source("Z:/Users/Highstat/applicat/HighlandStatistics/Courses/FilesRegressionGLMGAMCourse/RSolutions/HighstatLib.R")
#Mac OS:
source("HighstatLibV9.R")
###################################################################




###################################################################
# Question: Has the Density - Depth relationship changed
# over time?
# Model Dens as a function of Meandepth, Year (or: Period)
# Take into account of Xkm, Ykm
###################################################################



###################################################################
#See preparation code for data exploration
###################################################################



###################################################################
#Remove NAs and spatial outlier
Fish2 <- na.exclude(Fish)
Fish3 <- Fish2[c(-135), ]
Fish  <- Fish3           #I'm lazy
###################################################################


###################################################################
#House keeping
#Express depth in km

Fish$MeanDepth.km <- Fish$MeanDepth / 1000
###################################################################



###################################################################
#Data exploration graph

xyplot(TotAbund ~ MeanDepth.km | factor(Period), 
       xlab = list("Mean depth (km)", cex = 1.5),
       ylab = list("Number of fish", cex = 1.5),
       data = Fish, 
       layout = c(2,1),
       type = "p", 
       col = 1, 
       pch = 16,
       strip = strip.custom(bg = 'white',
                            par.strip.text = list(cex = 1.2)),
       scales = list(alternating = T,
                     x = list(relation = "same"),
                     y = list(relation = "same")))

###################################################################




###################################################################
#Apply linear regression model
M0 <- lm(TotAbund ~ MeanDepth.km, 
          data = Fish)
summary(M0)


#Model validation
# 1. Homogeneity
# 2. Independence
# 3. Normality
# 4. Influential observations

# Tools:
# 1. Plot residuals versus fitted values
# 2. Plot residuals versus each covariate in the model 
#    and not in the model
# 3. Histogram
# 4. Cook's distance

#Let's do 1, 2 and 3
E0 <- resid(M0)
F0 <- fitted(M0)

par(mfrow = c(2,2), mar = c(5,5,3,2))
plot(x = F0, 
     y = E0,
     xlab = "Fitted values",
     ylab = "Residuals",
     cex.lab = 1.5)
abline(h = 0, lty = 2)

plot(x = Fish$MeanDepth.km, 
     y = E0,
     xlab = "Mean Depth (km)",
     ylab = "Residuals",
     cex.lab = 1.5)
abline(h = 0, lty = 2)

boxplot(E0 ~ Period, 
        data = Fish, 
        cex.lab = 1.5, 
        xlab = "Period",
        ylab = "Residuals")

#Normality
hist(E0, 
     main = "", 
     breaks = 20, 
     cex.lab = 1.5, 
     xlab = "Residuals")

#More trouble:
par(mfrow = c(1, 1), mar = c(5,5,3,2))
plot(x = F0, 
     y = E0,
     xlab = "Fitted values",
     ylab = "Residuals",
     cex.lab = 1.5)
abline(v = 0, lty = 2, col = 2, lwd = 2)
abline(h = 0, lty = 2, col = 1)


#Sketch fitted values of the model
par(mfrow = c(1,1), mar = c(5,5,3,2))
plot(x = Fish$MeanDepth.km, 
     y = Fish$TotAbund,
     xlab = "Mean Depth (km)",
     ylab = "Total abundance",
     cex.lab = 1.5,
     pch = 1,
     ylim = c(-300, 1200))
abline(M0, lwd = 5)
abline(h=0, lty = 2)


#And visualize what the linear regression model assumes in
#terms of normality
par(mfrow = c(1,1), mar = c(5,5,3,2))
plot(x = Fish$MeanDepth.km, 
     y = Fish$TotAbund,
     xlab = "Mean Depth (km)",
     ylab = "Total abundance",
     cex.lab = 1.5,
     pch = 1,
     ylim = c(-300, 1200))
abline(M0, lwd = 5)
abline(h=0, lty = 2)

range(Fish$MeanDepth.km)
md <- seq(0.804, 4.865, length = 10)

Beta <- coef(M0)
for (i in 1:10){
	mu <- Beta[1] + Beta[2] * md[i]
	yi <- rnorm(100, mean = mu, sd = summary(M0)$sigma)
	points(x = jitter(rep(md[i], 100)), 
	       y = jitter(yi), 
	       col = grey(0.5), 
	       pch = 16, 
	       cex = 1)
}
###################################################################
#Return to Powerpoint



###################################################################
#Section 1.2.5

#Fit a Poisson GLM
M1 <- glm(TotAbund ~ MeanDepth.km, 
          data = Fish, 
          family = poisson(link = "log"))

#No need to specify the link. This works as well:
#M1 <- glm(TotAbund ~ MeanDepth.km, 
#          data = Fish, 
#          family = poisson)

#Present the numerical output
summary(M1)

#Estimated parameters:
#Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   6.64334    0.01273  521.70   <2e-16 ***
#MeanDepth.km -0.62870    0.00670  -93.84   <2e-16 ***

#This means:
#TotAbund_i ~ Poisson(mu_i)
#log(mu_i) = eta_i
#eta_i = 6.64 - 0.62 * MeanDepth.km_i

#AIC stuff:
AIC(M1)
#Or do it yourself
#AIC = - 2 * logL + 2 * number of parameters
-2 * logLik(M1) + 2 * 2

###################################################################
#Return to Powerpoint



###################################################################
#Deviance stuff

#Calculate the LogLik yourself
beta1 <- coef(M1)[1]
beta2 <- coef(M1)[2]
eta   <- beta1 + beta2 * Fish$MeanDepth.km
mu    <- exp(eta)
y     <- Fish$TotAbun
LogL <- sum(y * eta-mu - lgamma(y + 1))
LogL

#Compare with:
logLik(M1)

#And the residual deviance:
2 * sum(y*log(y/mu) - (y-mu))
###################################################################
#Return to Powerpoint



###################################################################
#R^2 stuff
summary(M1)
(27779 - 15770) / 27779
#The explained deviance is 43%
#Other definitions exists

M1A <- glm(TotAbund ~ 1,  
           data = Fish, 
           family = poisson(link = "log"))
1 - logLik(M1) / logLik(M1A)

###################################################################
#Return to Powerpoint


###################################################################
#Show model fit

par(mar = c(5,5,2,2))
MyData <- data.frame(MeanDepth.km = seq(0.804, 4.865, length = 25))
P1     <- predict(M1, 
                  newdata = MyData, 
                  type = "response")

plot(x = Fish$MeanDepth.km,
     y = Fish$TotAbund,
     ylim = c(0,1300),
     xlab = "Mean depth (km)",
     ylab = "Total abundance values", 
     cex.lab = 1.5)
     
lines(x = MyData$MeanDepth.km, 
      y = P1, 
      lwd = 3)


#And what is the Poisson distribution doing?
MyData <- data.frame(MeanDepth.km = seq(0.804, 4.865, length = 25))
P1     <- predict(M1, 
                  newdata = MyData, 
                  type = "response")

plot(x = Fish$MeanDepth.km,
     y = Fish$TotAbund,
     ylim = c(0,1300),
     xlab = "Mean depth (km)",
     ylab = "Total abundance values", 
     cex.lab = 1.5)
     
lines(x = MyData$MeanDepth.km, 
      y = P1, 
      lwd = 3)


HL <- seq(.804, 4.865, length = 25)
Beta <- coef(M1)
for (i in 1:25){
	mu <- exp(Beta[1] + Beta[2] * HL[i])
	yi <- rpois(150, lambda= mu)
	points(jitter(rep(HL[i], 150)), 
	       jitter(yi), col = grey(0.5), 
	       pch = 16, cex = 0.5)
}
#Didn't say it is a good model!

###################################################################
#Return to Powerpoint

###################################################################
#Model validation
par(mfrow = c(2,2))
plot(M1)


E1  <- resid(M1, type = "pearson")
F1  <- fitted(M1)
eta <- predict(M1, type = "link")


par(mfrow = c(2,2), mar = c(5,5,2,2))
plot(x = F1, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Pearson residuals",
     cex.lab = 1.5)
abline(h = 0, v = 0, lty = 2)

plot(x = eta, 
     y = E1,
     xlab = "Eta",
     ylab = "Pearson residuals",
     cex.lab = 1.5)
abline(h = 0, v = 0, lty = 2)


plot(x = Fish$MeanDepth.km, 
     y = E1,
     xlab = "Mean Depth (km)",
     ylab = "Pearson residuals",
     cex.lab = 1.5,
     pch = 16)
abline(h = 0, v = 0, lty = 2)

boxplot(E1 ~ Period, 
        ylab = "Pearson residuals",
        data = Fish,
        cex.lab = 1.5, 
        xlab = "Period")
abline(h = 0, v = 0, lty = 2)

#Compare to:
par(mfrow = c(2,2))
plot(M1)

###################################################################
#Return to Powerpoint


###################################################################
#Section 1.2.6

#Checking for overdispersion
N <- nrow(Fish)
p <- length(coef(M1))
Dispersion <- sum(E1^2) / (N - p)
Dispersion



#Why do we have overdispersion?

#Outliers?
par(mar = c(5,5,2,2))
plot(cooks.distance(M1), 
     type = "h", 
     ylim = c(0,20), 
     cex.lab = 1.5)
abline(h = 0, lty = 2, lwd = 2)
I <- 1:nrow(Fish)
I[cooks.distance(M1) > 1]

#Missing covariates, zero inflation, correlation?
#Will show a detailed model validation in the next section

###################################################################
#Return to Powerpoint

###################################################################
#Section 1.2.7 Adding covariates

Fish$fPeriod <- factor(Fish$Period)
M2 <- glm(TotAbund ~ MeanDepth.km * fPeriod, 
          data = Fish, 
          family = poisson)
summary(M2)

#Coefficients:
#                       Estimate Std. Error z value Pr(>|z|)    
#(Intercept)            6.832036   0.014837 460.473  < 2e-16 ***
#MeanDepth.km          -0.658858   0.007935 -83.031  < 2e-16 ***
#fPeriod2              -0.674857   0.029189 -23.120  < 2e-16 ***
#MeanDepth.km:fPeriod2  0.115712   0.014908   7.762 8.39e-15 ***

#Assuming the Poisson distribution is correct, this means:
#TotAbund_i ~ Poisson(mu_i)
#log(mu_i) = eta_i
#Period 1: eta_i = 6.64 - 0.65 * MeanDepth.km_i
#Period 2: eta_i = 6.64 - 0.67 + (0.11 - 0.65) * MeanDepth.km_i


#Check for overdispersion
E2 <- resid(M2, type = "pearson")
N  <- nrow(Fish)
p  <- length(coef(M2))
Dispersion <- sum(E2^2) / (N - p)
Dispersion

###################################################################
#Return to Powerpoint


###################################################################
#Section 1.3 Negative binomial GLM

library(MASS)
M4 <- glm.nb(TotAbund ~ MeanDepth.km * fPeriod,
             data = Fish)

summary(M4)

#Check for overdispersion
E4 <- resid(M4, type = "pearson")
N  <- nrow(Fish)
p  <- length(coef(M4)) + 1  #the +1 is due to k
Dispersion <- sum(E4^2) / (N - p)
Dispersion

###################################################################
#Return to Powerpoint

###################################################################
#Model selection
drop1(M4, test = "Chi")

#Fit new model
M5 <- glm.nb(TotAbund ~ MeanDepth.km + fPeriod ,
            data = Fish)

drop1(M5, test = "Chi")
summary(M5)


#Model validation
par(mfrow = c(2,2))
plot(M5)

#Or:
E5 <- resid(M5, type = "pearson")
F5 <- fitted(M5, type = "response")

par(mfrow = c(2, 2), mar = c(5,5,2,2))
plot(x = F5, 
     y = E5,
     xlab = "Fitted values",
     ylab = "Residuals",
     cex.lab = 1.5)
abline(0,0, lty = 2)

#Influential observations
plot(cooks.distance(M5),
     type = "h",
     ylim = c(0,1),
     cex.lab = 1.5)
abline(h=1)

#Independence:
plot(x = Fish$MeanDepth, 
     y = E5,
     xlab = "Mean depth (km)",
     ylab = "Pearson residuals", 
     cex.lab = 1.5)
abline(0,0, lty=2)

boxplot(E5 ~ Period, 
        data = Fish, 
        xlab = "Period",
        ylab = "Pearson residuals",
        cex.lab = 1.5)

par(mfrow = c(1, 1), mar = c(5,5,2,2))
#Normality
hist(E5, 
     breaks = 15,
     cex.lab = 1.5,
     ylab ="",
     xlab ="Pearson residuals")
#Pearson residuals from NB GLM tend to be skewed


#Spatial independence?
Fish3$MyCex <- abs(E5) / max(abs(E5))
Fish3$MyCol <- E5
Fish3$MyCol[E5 >= 0 ] <- 1
Fish3$MyCol[E5 < 0 ] <- 2

library(lattice)
xyplot(Ykm ~ Xkm,
       aspect = "iso",
       cex = 3 * Fish3$MyCex,
       pch = 16,
       col = Fish3$MyCol,
       data = Fish3,
       main = "Spatial plot of residuals")

coplot(E5 ~ MeanDepth.km | factor(Period),
       data = Fish)

coplot(E5 ~ MeanDepth.km | factor(Period),
       data = Fish, 
       panel = function(x, y, ...)
         panel.smooth(x, y,  lwd = 2, span = .5, ...))
#Is there a non-linear pattern?

#Same type of graph using lattice
xyplot(E5 ~ MeanDepth.km | factor(Period), 
       data = Fish,
       xlab = list(label = "Mean depth (km)", cex = 1.5),
       ylab = list(label = "Pearson residuals", cex = 1.5),
       panel = function(x,y){
         panel.points(x,y, col = 1, pch = 16, cex = 0.7)
         panel.loess(x,y, col = 1, lwd = 2)
         panel.abline(h=0)
       })
###################################################################
#Return to Powerpoint


###################################################################
#Model interpretation

#This was our model:
M5 <- glm.nb(TotAbund ~ MeanDepth.km + fPeriod,
          data = Fish)

#Visualize the model fit
#First create artifical data
MyData1 <- data.frame(MeanDepth.km = seq(from = 0.804,
                                         to = 4.865,
                                         length = 25),
                      fPeriod = "1")

MyData2 <- data.frame(MeanDepth.km = seq(from = 0.804,
                                         to = 4.865,
                                         length = 25),
                      fPeriod = "2")
#Predict for these data
P1 <- predict(M5, newdata = MyData1, type = "link", se = TRUE)
P2 <- predict(M5, newdata = MyData2, type = "link", se = TRUE)
 
#Create a vector for the point characters
NicePch <- Fish$Period
NicePch[Fish$Period==1] <- 1
NicePch[Fish$Period==2] <- 16

#Plot the whole thing
par(mar = c(5,5,2,2))
plot(x = Fish$MeanDepth.km,
     y = Fish$TotAbund,
     pch = NicePch, 
     cex.lab = 1.5, 
     xlab = "Mean depth (km)",
     ylab = "Total abundance")


polygon(c(MyData1$MeanDepth.km, rev(MyData1$MeanDepth.km)),
        c(exp(P1$fit - 2 * P1$se), rev(exp(P1$fit + 2 * P1$se))),
        col = gray(0.5), 
        border = NULL,
        density = 50)
        
polygon(c(MyData2$MeanDepth, rev(MyData2$MeanDepth)),
        c(exp(P2$fit - 2 * P2$se), rev(exp(P2$fit + 2 * P2$se))),
        col = 1, 
        border = NULL,
        density = 50)

###################################################################
#Return to Powerpoint

####################################################################
#Add the offset variable

#This was our model:

Fish$LogSA <- log(Fish$SweptArea)
M6 <- glm.nb(TotAbund ~ MeanDepth.km + fPeriod + offset(LogSA),
          data = Fish)

#Visualize the model fit
#First create artifical data
MyData1 <- data.frame(MeanDepth.km = seq(from = 0.804,
                                         to = 4.865,
                                         length = 25),
                      fPeriod = "1",
                      LogSA = log(mean(Fish$SweptArea)))

MyData2 <- data.frame(MeanDepth.km = seq(from = 0.804,
                                         to = 4.865,
                                         length = 25),
                      fPeriod = "2",
                      LogSA = log(mean(Fish$SweptArea)))
#Predict for these data
P1 <- predict(M5, newdata = MyData1, type = "link", se = TRUE)
P2 <- predict(M5, newdata = MyData2, type = "link", se = TRUE)

#Create a vector for the point characters
NicePch <- Fish$Period
NicePch[Fish$Period==1] <- 1
NicePch[Fish$Period==2] <- 16

#Plot the whole thing
par(mar = c(5,5,2,2))
plot(x = Fish$MeanDepth.km,
     y = Fish$TotAbund,
     pch = NicePch,
     cex.lab = 1.5,
     xlab = "Mean depth (km)",
     ylab = "Total abundance")


polygon(c(MyData1$MeanDepth.km, rev(MyData1$MeanDepth.km)),
        c(exp(P1$fit - 2 * P1$se), rev(exp(P1$fit + 2 * P1$se))),
        col = gray(0.5),
        border = NULL,
        density = 50)

polygon(c(MyData2$MeanDepth, rev(MyData2$MeanDepth)),
        c(exp(P2$fit - 2 * P2$se), rev(exp(P2$fit + 2 * P2$se))),
        col = 1,
        border = NULL,
        density = 50)

######################################################################