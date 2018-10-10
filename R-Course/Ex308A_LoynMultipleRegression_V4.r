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



#####################################################
#Apply model
M1 <- lm(ABUND ~ LOGAREA + LOGDIST + LOGLDIST +
                 YR.ISOL + ALT + factor(GRAZE),
         data = Birds)


#
#Is everything significant?
summary(M1)
drop1(M1, test = "F")

#
##################################################################
#1. Keep the model as it is
#2. Adopt IT approach (but should have done this from the start)
#3. Backward selection 
    #3A. AIC
    #3B. Hypothesis testing using t-test or F-test
##################################################################




#Option 1 is not good here....because of collinearity 
#between YR.ISOL and GRAZE:
boxplot(YR.ISOL ~ factor(GRAZE), data = Birds)

#See the results effects of collinearity of SEs and p-values
M1A <- lm(ABUND ~ LOGAREA + LOGDIST + LOGLDIST +
                  ALT + factor(GRAZE),
         data = Birds)

M1B <- lm(ABUND ~ LOGAREA + LOGDIST + LOGLDIST +
                  ALT + YR.ISOL,
         data = Birds)

drop1(M1, test = "F")
drop1(M1A, test = "F")
drop1(M1B, test = "F")


###########################################################
#Option 2. IT approach
#We will discuss this in another exercise


###########################################################
#Option 3. Model selection
# We have already shown option 3B in the Methane exercise
# We used drop1 to decide whether a term was significant or not.
# Now we discuss 3A: Classical model selection using AIC

step(M1)

#Optimal model as judged by the AIC
M2 <- lm(ABUND ~ LOGAREA + factor(GRAZE), data = Birds)

#Is everything significant?
summary(M2)
drop1(M2, test= "F")
#Yes..everything is significant
#############################################



#############################################
#Model validation
#1. Homogeneity
#2. Independence
#3. Influential observations
#4. Normality
#5. Does it all make sense?

#Homogeneity
E2 <- resid(M2)
F2 <- fitted(M2)
plot(x = F2, 
     y = E2,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, v = 0, lty = 2)


#Influential observations
par(mfrow = c(1, 1))
plot(cooks.distance(M2), type = "h", ylim = c(0, 1))
abline(h = 1)

#Normality
E2 <- resid(M2)
hist(E2, breaks = 15)


#"Independence"
#Plot residuals vs each covariate in the model
#Plot residuals vs each covariate not in the model
plot(x = Birds$LOGAREA, 
     y = E2, 
     main = "No lack of fit")
abline(h = 0, lty = 2)


#Plot residuals vs each covariate not in the model
plot(y = E2, x = Birds$ALT)
abline(0,0)
###Do for other covariates as well
plot(y = E2, x = Birds$LOGLDIST)
abline(0,0)

#...
#...

##Or use the Myxyplot function to plot E2 vs each continous (!!) covariate
Birds$E2 <- E2   #Put E2 inside the Birds object (which will give trouble if there are NAs)
MySel <- c("LOGAREA","LOGDIST","LOGLDIST", "YR.ISOL","ALT")

Myxyplot(Birds, MySel, "E2", MyYlab = "Residuals")
#########################################################


#plot fitted values vs observed values
plot(x = F2, 
     y = Birds$ABUND,
     xlab = "Fitted values",
     ylab = "Observed data")
abline(coef = c(0, 1), lty = 2)
#########################################################


     
     
#########################################################
#Model interpretation
summary(M2)

#Plot what the model is doing
#What is the equation?

#Coefficients:
#               Estimate Std. Error t value Pr(>|t|)
#(Intercept)     15.7164     2.7674   5.679 6.87e-07 ***
#LOGAREA          7.2472     1.2551   5.774 4.90e-07 ***
#factor(GRAZE)2   0.3826     2.9123   0.131 0.895993
#factor(GRAZE)3  -0.1893     2.5498  -0.074 0.941119
#factor(GRAZE)4  -1.5916     2.9762  -0.535 0.595182
#factor(GRAZE)5 -11.8938     2.9311  -4.058 0.000174 ***
#
# ABUNDANCE is a function of GRAZING and AREA
#
# 

# ABUND_i = 15.71 +
#           0      * GRAZE1_i +
#           0.38   * GRAZE2_i +
#          -0.18  *  GRAZE3_i +
#          -1.59  *  GRAZE4_i +
#          -11.89 *  GRAZE5_i +
#           7.24   * LOGAREA_i
 
#-------------------------------- 
#Equation for GRAZE = 1
# ABUND = 15.71 +
#         0      * 1 +
#         0.38   * 0 +
#         -0.18  * 0 +
#         -1.59  * 0 +
#         -11.89 * 0 +
#         7.24   * LOGAREA
         
#ABUND = 15.71 + 7.24 * LOGAREA
  
#--------------------------------   
#Equation for GRAZE = 2         
# ABUND = 15.71 +
#         0      * 0 +
#         0.38   * 1 +
#         -0.18  * 0 +
#         -1.59  * 0 +
#         -11.89 * 0 +
#         7.24   * LOGAREA

#ABUND = 15.71 + 0.38 + 7.24 * LOGAREA
#      = 16.09 + 7.24 * LOGAREA
      
#Do this for each level



###################################################
#Plot results
range(Birds$LOGAREA)
#-1.000000  3.248219
tapply(Birds$LOGAREA, INDEX = Birds$GRAZE, FUN = range )
MyData1 <- data.frame(LOGAREA = seq(from = -1,
                                    to = 3.24, 
                                    length = 25),
                      GRAZE = 1)
                     
MyData2 <- data.frame(LOGAREA = seq(from = -1,
                                    to = 3.24, 
                                    length = 25),
                     GRAZE = 2)

MyData3 <- data.frame(LOGAREA = seq(from = -1, 
                                    to = 3.24, 
                                    length = 25),
                     GRAZE = 3)

MyData4 <- data.frame(LOGAREA = seq(from = -1,
                                    to = 3.24, 
                                    length  =25),
                     GRAZE = 4)

MyData5 <- data.frame(LOGAREA = seq(from = -1,
                                    to = 3.24, 
                                    length = 25),
                     GRAZE = 5)

P1 <- predict(M2, newdata = MyData1)
P2 <- predict(M2, newdata = MyData2)
P3 <- predict(M2, newdata = MyData3)
P4 <- predict(M2, newdata = MyData4)
P5 <- predict(M2, newdata = MyData5)

plot(x = Birds$LOGAREA, 
     y = Birds$ABUND,
     col = Birds$GRAZE, 
     pch = 16)

lines(MyData1$LOGAREA, P1, lty = 1,col = 1)
lines(MyData2$LOGAREA, P2, lty = 1,col = 2)
lines(MyData3$LOGAREA, P3, lty = 1,col = 3)
lines(MyData4$LOGAREA, P4, lty = 1,col = 4)
lines(MyData5$LOGAREA, P5, lty = 1,col = 5)

#Add a legend
legend("topleft",
        legend=c("G1","G2","G3","G4","G5"),
        col = c(1,2,3,4,5),
        lty = c(1,1,1,1,1),
        lwd = c(1,1,1,1,3))


#To control the range of the lines:
#Birds$LOGAREA[Birds$GRAZE == 5]
#range(Birds$LOGAREA[Birds$GRAZE == 5])
#range(Birds$LOGAREA[Birds$GRAZE==1], na.rm = TRUE)
#

#Advanced coding....do it in a loop:
plot(x = Birds$LOGAREA, 
     y = Birds$ABUND,
     col = Birds$GRAZE, 
     pch = 16)

for (i in 1:5){
   MinMaxi <- range(Birds$LOGAREA[Birds$GRAZE==i])
   MyDatai <- data.frame(LOGAREA=
                  seq(from = MinMaxi[1],
                      to   = MinMaxi[2],
                      length=25),
                  GRAZE = i)
   Pi <- predict(M2, newdata = MyDatai)
   lines(MyDatai$LOGAREA, Pi, lty=1,col=i,lwd=2)
}

legend("topleft",
        legend=c("G1","G2","G3","G4","G5"),
        col = c(1,2,3,4,5),
        lty = c(1,1,1,1,1),
        lwd = c(2,2,2,2,2))
######################################################

#Or very advanced plotting....but very (!) nice for a paper
#Get predicted values and store them in a vector

#Run each time from here
Pred    <- NULL
SE      <- NULL
ID      <- NULL
LogArea <- NULL
for (i in 1:5){
   MinMaxi <- range(Birds$LOGAREA[Birds$GRAZE==i])
   MyDatai <- data.frame(LOGAREA=
                  seq(from = MinMaxi[1],
                      to   = MinMaxi[2],
                      length=25),
                  GRAZE = i)
   Pi <- predict(M2, newdata = MyDatai, se = TRUE)
   Pred <- c(Pred, Pi$fit)
   SE   <- c(SE, Pi$se.fit)
   ID   <- c(ID, rep(i, nrow(MyDatai)))
   LogArea <- c(LogArea, MyDatai$LOGAREA)
}

cbind(LogArea, Pred, SE, ID)


xyplot(ABUND ~ LOGAREA | factor(GRAZE),
       data = Birds,
       xlab = list("Log area", cex = 1.5),
       ylab = list("Abundance", cex = 1.5),
       layout = c(5,1),   #Modify
       strip = function(bg = 'white', ...)
       strip.default(bg = 'white', ...),
       scales = list(alternating = T,
                     x = list(relation = "same"),
                     y = list(relation = "same")),
       panel=function(x, y, subscripts){
             WhichPanel <- Birds$GRAZE[subscripts][1]
             xi  <- LogArea[ID == WhichPanel]
             yi  <- Pred[ID == WhichPanel]
             sei <- SE[ID == WhichPanel]
             panel.grid(h=-1, v= 2)
             #panel.lines(xi, yi + 1.96 * sei, col = 1, lty = 2)
             #panel.lines(xi, yi - 1.96 * sei, col = 1, lty = 2)        
             panel.polygon(c(xi, rev(xi)),
                           c(yi - 1.96 * sei, rev(yi + 1.96 * sei)),
                           col = grey(0.5), border = NULL,
                           density = 50)

             panel.lines(xi, yi, col = 1)
             panel.points(x, y, col = 1, pch = 16)
             })
#Nice graph...but not easy to make!       



#########################################
#And some ggplot2 code

#Here is some fancy code to make a better MyData
library(plyr)
MyData <- ddply(Birds, .(GRAZE), summarize,
                LOGAREA = seq(min(LOGAREA), max(LOGAREA),length = 10))
# ddply allows ggplot2 to draw lines without extrapolation
MyData

#And here is some ggplot2 code
P <- predict(M2, newdata = MyData, se = TRUE)


#Add fitted values and confidence bands
MyData$mu    <- P$fit  #Fitted values
MyData$selow <- P$fit - 2 * P$se.fit  #lower bound
MyData$seup  <- P$fit + 2 * P$se.fit   #lower bound
head(MyData)

library(ggplot2)
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

p <- p + facet_grid(. ~ GRAZE, scales = "fixed")
p  




