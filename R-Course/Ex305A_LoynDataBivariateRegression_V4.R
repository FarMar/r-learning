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
setwd("/Users/far218/Dropbox/R Course")


#Import the data from a tab delimited ascii file
Birds <- read.table(file = "loyn.txt",
                    header = TRUE,
                    dec = ".")
#dec = '.'   means that the point is used for decimals. 
#Change to   dec = ","   if required.

names(Birds)
str(Birds)  #Check that ABUND and AREA are numerical (num)!
########################################################################





########################################################################
#Housekeeping
#Load packages from R and support functions that we wrote
library(lattice)  #For fancy multipanel graphs
source("HighstatLibV9.R")
########################################################################




########################################################################
#Data exploration: See code exercise 1

#Conclusion Data exploration:
dotchart(Birds$AREA)
Birds$LOGAREA  <- log10(Birds$AREA)
Birds$LOGDIST  <- log10(Birds$DIST)
Birds$LOGLDIST <- log10(Birds$LDIST)
########################################################################



########################################################################
#Start analysis
#Fit the model: ABUND_i = Intercept + beta * LOGAREA_i + eps_i
#               eps_i ~ N(0, sigma^2)

# ABUND_i ~ N(mu_i, sigma^2)
# E(ABUND_i)   = mu_i
# var(ABUND_i) = sigma^2
# mu_i = Intercept + beta * LOGAREA_i

M1 <- lm(ABUND ~  LOGAREA, data = Birds)
            
#What is the model that we are fitting?
# ABUND_i = alpha + beta * LOGAREA_i + eps_i
# eps_i ~ N(0, sigma^2)
   
# where i = 1,...,56
# ABUND_i is the abundance at site i
summary(M1) 
 

# What is the fitted model
# E(ABUND_i) = mu_i = 10.40 + 9.77 * LOGAREA_i 
# eps ~ N(0,  7.28^2)


            
#Is everything significant?
summary(M1)

#ABUND = alpha + beta * LOGAREA + eps

#Interpretation of F and t values 
 #H0: beta = 0
 #H1: beta <> 0

 #F_1,54 = 65.38 (p<0.001)
 #Or:
 #t_n-2    t_54 = 8.08 (p < 0.001)
 #Text in paper: A t-value indicated a significant effect (t = 8.08; df = 54, p < 0.001)
 #t-value is t(N-p-1) in: y = β0 + β1 * x1 + β2 * x2 + . . . βp * xp + ε
 #                        p is the number of covariates

################################################
#Model validation
#1. Homogeneity
#2. Independence
#3. Influential observations
#4. Normality
#5. Does it all make sense?


#Standard graphical output lm:
par(mfrow = c(2, 2))
plot(M1)


#More specific output:
#Homogeneity
#E1 <- resid(M1)   #or better: 
E1 <- rstandard(M1)
F1 <- fitted(M1)

plot(x = F1, 
     y = E1, 
     xlab = "Fitted values",
     ylab = "Residuals", 
     main = "Homogeneity?")
abline(h = 0, v = 0, lty = 2)
     
#Normality
hist(E1, main = "Normality", breaks=10)
#Or qq-plot
#qqnorm(E1)
#qqline(E1)

#Dependence due to model misfit
#Plot residuals versus covariates
plot(x = Birds$LOGAREA, 
     y = E1)
abline(0,0,lty=2)


#But plot residuals also versus covariates NOT in the model!!!!!!!
boxplot(E1 ~ factor(Birds$GRAZE), varwidth = TRUE)
abline(h = 0, lty = 2)

plot(y = E1, 
     x = Birds$YR.ISOL, 
     pch =16)
abline(h = 0, lty = 2)

#and also for all the other covariates!!!
#...
#...

#Or use the Myxyplot function to plot E1 vs each continous (!!) covariate
Birds$E1 <- E1   #Put E inside the Birds object (which will give trouble if there are NAs)
MySel <- c("LOGAREA","LOGDIST","LOGLDIST", "YR.ISOL","ALT")

Myxyplot(Birds, MySel, "E1", MyYlab = "Residuals")
#It would be useful to omit the smoother
######################################



################################################
#Look at influential observations
par(mfrow = c(1, 1))
plot(cooks.distance(M1), type = "h", ylim = c(0, 1))
abline(h = 1, col = 2,lwd = 3)

#Or:
plot(M1, which = c(4))

#Or (if you are familiar with advanced R coding):
#C1 <- cooks.distance(M1)
#I1 <- seq(1:56)
#plot(x = I1, y = C1, type = "h")
#text(I1[C1 > 0.1], C1[C1 > 0.1] + 0.01, I1[C1>0.1], cex =0.5)
#identify(x=I1, y = C1)  #Now press escape
        
  
        
################################################
#What does the numerical output mean?
#Option 1: Look at the numbers:
summary(M1)


#Option 2: Plot what the model is doing
#What is the equation?

#mu_i = 10.40 + 9.78 * LOGAREA_i

#Clumsy way of plotting 
 range(Birds$LOGAREA)
 LA <- seq(from   = -1, 
           to     = 3.25, 
           length = 25)
 
 FittedAbundance = 10.40 + 9.78 * LA 
 plot(x = LA, 
      y = FittedAbundance, 
      type = "l")
 points(x = Birds$LOGAREA,
        y = Birds$ABUND)
        
 
#More advanced way of plotting the model results

range(Birds$LOGAREA)
#-1.000000  3.248219
#Create a data frame that contains x numbers of LOGAREA values
MyData <- data.frame(LOGAREA = seq(from= -1,
                                   to = 3.25,
                                   length = 25))

P1 <- predict(M1, newdata = MyData)
plot(x = Birds$LOGAREA, 
     y = Birds$ABUND,
     xlab = "Log transformed area",
     ylab = "Bird abundance")

lines(x = MyData$LOGAREA, 
      y = P1, 
      lwd = 3, 
      lty = 1,
      col = 1)


###################################################################
#Below is some advanced stuff to add confidence bands.
#Most likely we will not discuss this during the course.
P1  <- predict(M1, newdata = MyData, se = TRUE, interval = "prediction")
PP1 <- predict(M1, newdata = MyData, se = TRUE, interval = "confidence")

#Get the upper and lower bounds of the prediction interval.
#This is the measure of uncertainty that an individudal observation
#will be in.
SePred.Up  <- P1$fit[,3]
SePred.Low <- P1$fit[,2]

#Get the upper and lower bounds of the confidence interval
#If you were to repeat this experiment a large number
#of times then in 95% if the cases the real line/mean will be
#within these bands:
SeConf.Up  <- PP1$fit[,3]
SeConf.Low <- PP1$fit[,2]


plot(x = Birds$LOGAREA, 
     y = Birds$ABUND,
     xlab = "Log transformed area",
     ylab = "Bird abundance")

lines(x = MyData$LOGAREA, 
      y = P1$fit[,1], 
      lwd = 3, 
      lty = 1,
      col = 1)

polygon(c(MyData$LOGAREA, rev(MyData$LOGAREA)),
        c(SePred.Low, rev(SePred.Up)),
        col = NA, border = NULL,
        density = 50)

polygon(c(MyData$LOGAREA, rev(MyData$LOGAREA)),
        c(SeConf.Low, rev(SeConf.Up)),
        col = 2, border = NULL,
        density = 50)


#Useful for later...when we have multiple lines
legend("topleft",
       legend=c("Line 1"),
       col = c(1),
       lty = c(1),
       lwd = c(1))
########################################################


########################################################
#Same graph...but now with ggplot2

#If you are online, then use this:
Install <- TRUE #Change to FALSE if you don't want packages installed
toInstall <- c("ggplot2")
if(Install){
	install.packages(toInstall, 
	                 dependencies = TRUE, 
	                 repos = "http://cran.us.r-project.org")
	        }
library(ggplot2)

MyData$selow <- SeConf.Low  #lower bound
MyData$seup  <- SeConf.Up   #lower bound
MyData$mu    <- P1$fit[,1]  #Fitted values
head(MyData)

p <- ggplot()
p <- p + geom_point(data = Birds, 
                    aes(y = ABUND, x = LOGAREA),
                    shape = 16, 
                    size = 3)
p <- p + xlab("Log area") + ylab("Abundance")
p <- p + theme(text = element_text(size=15)) + theme_bw()
p <- p + geom_line(data = MyData, 
                   aes(x = LOGAREA, y = mu), 
                   colour = "black")

p <- p + geom_ribbon(data = MyData, 
                     aes(x = LOGAREA, 
                         ymax = seup, 
                         ymin = selow ),
                     alpha = 0.5)
p
##############################################




#########################################################
#Class exercise:
M2 <- lm(ABUND ~ factor(GRAZE), data=Birds)
summary(M2)


