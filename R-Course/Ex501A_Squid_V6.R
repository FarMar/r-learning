#    Highland Statistics Ltd.
#    www.highstat.com

#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  


#Beginner's Guide to GAM (2012). Alain F Zuur

#Chapter 5 Additive modelling applied on stable isotope 
#ratios of ocean squid

#Alain F Zuur, Sonia Mendes, Jason Newton, Gabi Stowasser, 
#Karsten Zumholz, Elena N Ieno, Graham J Pierce 



#Isotopes are variants of a chemical element that 
#differ in atomic mass due to possessing different 
#numbers of neutrons.

#The underlying ecological question is whether 
#latitude, capture depth, and body size can explain 
#intra-population isotopic variation in G. fabricii 
#and whether those parameters have implications for 
#interpretation of predator isotope values.  


######################################################
#Load the data
setwd("/Users/Highstat/applicat/HighlandStatistics/Books/BGS/GAM/Data/Sonia")
Squid <- read.table(file = "SquidNorway.txt",
                    header = TRUE,
                    dec = ".")

names(Squid) ## ML = body length
str(Squid)



######################################################
#Load packages and functions
library(lattice)
library(mgcv)
source("HighstatLibV9.R")


######################################################
#Data exploration
# Outliers
# Collinearity
# Relationships

MyVar <- c("Lat", "Depth", "ML", "d15N")
Mydotplot(Squid[,MyVar])
#No clear outliers
        
##############################################
#Collinearity & relationships
MyVar <- c("d15N" , "Lat", "Depth", "ML")
pairs(Squid[,MyVar], 
      lower.panel = panel.cor)
#0.7 is on the high side!

#If you are familiar with VIFs:
MyVar <- c("Lat", "Depth", "ML")
corvif(Squid[,MyVar])
#Looks ok!


#Relationships
MyVar <- c("Lat", "Depth", "ML")
Myxyplot(Squid,MyVar,"d15N")
#Strong ML effect


#Conclusions data exploration
# Latitude and depth have limited unique values
# No outliers
# Small collinearity between Lat and Depth
#   Let's use only ML and Lat for the moment
#   The 0.7 worries me a little
# Non-linear ML effect
############################



#############################################
#Start frequentist analysis

#Starting point of the analysis:
#Option 1: d15N ~ ML + Lat    and check residuals
#Option 2: d15N ~ s(ML) + Lat   (Lat has limited unique values)

#Let's go for the first option
M1 <- lm(d15N ~ ML + Lat, 
          data = Squid)
summary(M1)


#Model validation
E1 <- rstandard(M1)
F1 <- fitted(M1)

par(mfrow = c(2, 2), 
    mar = c(5, 5, 3, 3))
plot(x = F1, 
     y = E1, 
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, lty = 2)

plot(x = Squid$ML, 
     y = E1, 
     xlab = "Mantel length",
     ylab = "Residuals")
abline(h = 0, lty = 2)

plot(x = Squid$Lat, 
     y = E1, 
     xlab = "Latitude",
     ylab = "Residuals")
abline(h = 0, lty = 2)

plot(x = Squid$Depth, 
     y = E1, 
     xlab = "Depth",
     ylab = "Residuals")
abline(h = 0, lty = 2)

#Is there a non-linear ML effect?
#Check this with a GAM
T1 <- gam(E1 ~ s(ML), ## "s" = SMOOTHER
          data = Squid)

summary(T1)
# 15% of variation in the residuals is 
# explained by the non-linear ML effect

par(mar = c(5, 5, 3, 3))
plot(T1, ylab = "Residuals", ylim = c(-3,2))
abline(0, 0, lty = 2)
points(Squid$ML, E1)
#Trouble..there is a significant ML effect 
#in the residuals!




################################################
#Go for option 2...and apply a GAM
M2 <- gam(d15N ~ Lat + s(ML), 
          data = Squid)
summary(M2)
plot(M2)

# Parametric coefficients:
            # Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  8.81246    1.45034   6.076 2.22e-08 ***
# Lat          0.05409    0.02086   2.593   0.0109 *  

# Approximate significance of smooth terms:
        # edf Ref.df     F p-value    
# s(ML) 2.577  3.246 43.71  <2e-16 *** ##2.577 DF - SIGNIFICANT, BUT NPOT HUGELY - 4 IS CONSIDERED THE SWEET-SPOT,
                                                                                  #1 WOULD BE LINEAR

#ON THE FIGURE, Y-AXIS IS THE INFLUENCE OF THE SMOOTHING

#Fitted model:
# d15N_i ~ N(mu_i, sigma^2)
# E(d15N_i)   = mu_i
# var(d15N_i) = sigma^2
# mu_i = 8.81 + 0.054 * Lat_i + s(ML_i)

#Which model is better, M1 or M2?
AIC(M1, M2)
#The GAM is better!



#Model validation
E2 <- resid(M2)
F2 <- fitted(M2)
par(mfrow = c(2,3), 
    mar = c(5,5,3,3))
plot(x = F2, 
     y = E2, 
     cex.lab = 1.5,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, lty = 2)

plot(x = Squid$ML, 
     y = E2, 
     cex.lab = 1.5,
     xlab = "ML",
     ylab = "Residuals")
abline(h = 0, lty = 2)

plot(x = Squid$Lat, 
     y = E2, 
     cex.lab = 1.5,
     xlab = "Latitude",
     ylab = "Residuals")
abline(h = 0, lty = 2)

plot(x = Squid$Depth, 
     y = E2, 
     cex.lab = 1.5,
     xlab = "Depth",
     ylab = "Residuals")
abline(h = 0, lty = 2)

hist(E2, 
     xlab = "", 
     ylab = "", 
     breaks = 10)


##################################
#Model interpretation

plot(M2, 
     resid = TRUE, 
     pch = 16, 
     cex = 0.5)
abline(h = 0, lty = 2)


# Do some prediction in order to visualize 
# the model fit
M2 <- gam(d15N ~ Lat + s(ML), 
          data = Squid)
                            
range(Squid$ML)
MyData <- data.frame(
            Lat = mean(Squid$Lat),
            ML  = seq(52, 292, length = 100))

P1 <- predict(M2, 
              newdata = MyData, 
              se = TRUE)

plot(x = Squid$ML,
     y = Squid$d15N, 
     type = "n")
lines(x = MyData$ML,
      y = P1$fit)
      
lines(x = MyData$ML,
      y = P1$fit + 2 * P1$se.fit,
      lty = 2)
           
lines(x = MyData$ML,
      y = P1$fit - 2 * P1$se.fit,
      lty = 2)


# MyData <- data.frame(
            # Lat = 70,
            # ML  = 50)

# # P1 <- predict(M2, 
              # newdata = MyData, 
              # se = TRUE)


###################################################
# In later exercises we will dive deeper into
# all the gam options


#For the moment:
# How can you criticise the analysis?
######################################



############################################
#Visualise the modelling results.

# #Calculate the fitted values
set.seed(12345)
Squid$Bd.jitter   <- jitter(Squid$Lat,amount = 0.1)
Squid$Prc.jitter <- jitter(Squid$ML, amount = 0.1)




                     
range(Squid$Lat) 
MyData <- expand.grid(
            Lat = seq(62, 75, length = 25),
            ML  = seq(52, 292, length = 25))
                     
mu   <- predict(M2, newdata = MyData)
ExpY <- mu  #Expected values Y. For a ZIP these will change. 

MyData2 <- cbind(MyData, ExpY)


#So..these are the two variables that we use
#to calculate our grid for:
BD.25   <- seq(62, 75, length = 25)
Prc.25  <- seq(52, 292, length = 25)

#And we convert the vector with expected values into
#a 25 by 25 matrix

ExpY.2d <- matrix(ExpY, nrow = length(Prc.25), ncol = length(BD.25))


#Now plot the jittered data again
library(rgl)

plot3d(x = Squid$Bd.jitter ,
       y = Squid$Prc.jitter,
       z = Squid$d15N,
       type = "p",
       size = 10,
       lit = FALSE,
       xlab = "Lat",
       ylab = "ML",
       zlab = "d15N"#,
       #zlim = c(0,10)
       #col = MyColour
       )

#Add the surface for the fitted Poisson values
surface3d(BD.25, Prc.25, ExpY.2d, 
          alpha = 0.6, 
          front = "lines", 
          back = "lines", 
          color = "black"#,
          #zlim = c(0,15)
          )

#And show vertical lines for the distance
#between the observed and fitted values.
interleave <- function(v1, v2) as.vector(rbind(v1, v2))
segments3d(interleave(Squid$Bd.jitter, Squid$Bd.jitter),
           interleave(Squid$Prc.jitter, Squid$Prc.jitter),
           interleave(Squid$d15N, fitted(M2)),
           alpha = 0.4,
           col = "black")





