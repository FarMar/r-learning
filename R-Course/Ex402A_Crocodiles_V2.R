
#    Highland Statistics Ltd.
#    www.highstat.com

#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  


######################################################################
#These data are taken from:

# Dead or Alive? Factors Affecting the Survival of Victims during 
# Attacks by Saltwater Crocodiles Crocodylus porosus in Australia
# Authors: Yusuke Fukuda, Charlie Manolis, Keith Saalfeld, Alain Zuur

# The data are kindly supplied by the NT Government, Australia
######################################################################


#Set the working directory and import the data
setwd("/Users/Highstat/applicat/HighlandStatistics/Courses/Data/")
Crocs <- read.table("Crocodiles.txt", 
                    header = TRUE,
                    dec = ".")
str(Crocs)
names(Crocs)

#This data set is a subset of the data analyzed in Fukuda et al.
#We dropped some of the covariates.
#In the paper an information theoretic approach is presented in
#which 15-ish models are compared. Here we only focus on the best one
#
#Task: Model survival (of victim) as a function of:
#      -DeltaWeight (difference in body weight between crocidile and victim) 
#      -Position (land, on water, in water)

##########################################################


######################################
#Load packages and support files
library(lattice)
library(ggplot2)
source("HighstatLibV9.R")
######################################


##################################################################
#Data exploration
# Outliers
MyVar <- c("Survived01", "DeltaWeight")
Mydotplot(Crocs[, MyVar])


#Number of zeros/ones
table(Crocs$Survived)
#Ok  

#Collinearity
par(cex.lab = 1.5, mar = c(5,5,2,2), mfrow = c(1,1))
boxplot(DeltaWeight ~ Position, 
        data = Crocs,
        xlab = "Position",
        ylab = "Weight difference")
#No

#Balanced data?
table(Crocs$Position)
#Hmmm....
#############################################






#############################################
#Frequentist analysis
M1 <- glm(Survived01 ~ DeltaWeight + Position, 
          data = Crocs, 
          family = binomial(link = "logit"))
drop1(M1, test = "Chi")
#Everything is indeed significant
summary(M1)

#Explained variance:
(107.77 - 45.66) / 107.77



#Task: Write down the model that we are fitting.
#Survived ~ Bernoulli(Pi_i)
#Pi_i = exp(eta_i) / (1 + exp(eta_i))

#In water
#eta_i = 2.19 -0.018 * DeltaWeight

#On land
#eta_i = 2.19 + 5.02 - 0.018 * DeltaWeight

#On water
#eta_i = 2.19 + 2.19 - 0.018 * DeltaWeight

###################################################




###################################################
#Model validation for 0-1 data is an art
E1 <- resid(M1, type = "pearson")
#(Survival - E(Survival)) / sqrt(var(Survival))
F1 <- fitted(M1)
plot(x = F1,
     y = E1)
abline(h=0, lty = 2)     


###################################################
#Model interpretation
range(Crocs$DeltaWeight)

MyData <- expand.grid(DeltaWeight = seq(from = -85.97, 
                                        to = 510.06, 
                                        length = 25),
                      Position = levels(Crocs$Position))
#NEXT SECTION FORCES LINE AND ERROR BANDS TO STAY WITHIN 'REAL' DATA
#WE ALSO DID THIS IN THE POISSON CLASS - LOOK AT CODE THERE TOO
#BASICALLY, THIS IS GENERATING TEH DATA WHICH GGPLOT2 CAN USE TO DRAW LINES ON THE FIGURE
P1          <- predict(M1, newdata = MyData, se = TRUE, type = "link")
MyData$Pi   <- exp(P1$fit) / (1 + exp(P1$fit))
MyData$SeUp <- exp(P1$fit + 1.96*P1$se.fit) / (1 + exp(P1$fit + 1.96*P1$se.fit))
MyData$SeLo <- exp(P1$fit - 1.96*P1$se.fit) / (1 + exp(P1$fit - 1.96*P1$se.fit))
MyData




p <- ggplot()
p <- p + geom_point(data = Crocs, 
                    aes(y = Survived01, x = DeltaWeight),
                    shape = 1, 
                    size = 1)
p <- p + xlab("DeltaWeight") + ylab("Probability of survival")
p <- p + theme(text = element_text(size=15)) + theme_bw()
p <- p + geom_line(data = MyData, 
                   aes(x = DeltaWeight, 
                       y = Pi), 
                   colour = "black")

p <- p + geom_ribbon(data = MyData, 
                     aes(x = DeltaWeight, 
                         ymax = SeUp, 
                         ymin = SeLo ),
                     alpha = 0.2)
p <- p + facet_grid(. ~ Position, scales = "fixed") #EVERY BOX HAS SAME RANGE
p


#And now multiple lines in the same panel
p1 <- ggplot()
p1 <- p1 + geom_point(data = Crocs, ##THIS SECTION THE DOTS
                    aes(y = Survived01, ##"AES" IS IMPORTANT - READ UP ON IT
                        x = DeltaWeight, 
                        group = Position),
                    shape = 1, 
                    size = 1)
p1 <- p1 + xlab("DeltaWeight") + ylab("Probability of survival") 
p1 <- p1 + theme(text = element_text(size=15)) 
p1 <- p1 + geom_line(data = MyData, ##THIS SECTION THE LINES
                   aes(x = DeltaWeight, 
                       y = Pi, 
                       group = Position, 
                       colour = Position))

p1 <- p1 + geom_ribbon(data = MyData, ##THIS SECTION THE ERROR RIBBONS
                     aes(x = DeltaWeight, 
                         ymax = SeUp, 
                         ymin = SeLo,
                         group = Position,
                         colour = Position,
                         fill = Position),
                     alpha = 0.2)
p1

#What is a sensible thing to do?
#Task: Do it

