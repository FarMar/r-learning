#    Beginner's Guide to GLM and GLMM with R
#    Alain Zuur, Joseph M Hilbe, and Elena N Ieno

#    www.highstat.com

#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.



##############################################################
#Set the working directory (on a Mac) and load the data
setwd("/Users/Highstat/applicat/HighlandStatistics/Books/BGS/GLM/Data/Seedlings")
Seedlings <- read.table(file = "Seedling.txt",
                        header = TRUE)

names(Seedlings)

##################################################################
# Rename some of the variables
# The seedling diameters are often oval, rather than round-
# so we measure diameter twice and take the mean of diam.1 and diam.2

Seedlings$Biomass  <- (Seedlings$stem.biomass + Seedlings$leafy.biomass)
Seedlings$Diameter <- (Seedlings$diam.1 + Seedlings$diam.2)/2
Seedlings$Height   <- Seedlings$crown.mm
Seedlings$Leaves   <- Seedlings$leaves
##################################################################

# Philipson et al. (2012) investigated how tree species
# respond to environmental light. They estimated growth 
# rates of 21 tropical lowland rainforest tree species 
# from Malaysian Borneo grown in shade houses for 2 years 
# under three light regimes

# Accurate estimates of initial size are important to
# the fit of the non-linear models. To estimate the 
# initial aboveground biomass of each seedling and 
# the aboveground biomass on subsequent monitoring dates, 
# Philipson et al. (2012) established regression relationships 
# for each of the species.

# Summarising...the data used in this analysis are used to
# estimate initial plant biomass values...which will then
# be used in subsequent analyses. 


#AIM: Model biomass of plants as a function of diameter, 
#     height and number of leaves

# The code below was taken from Chapter 6 in 
# 'Beginner's Guide to GLM and GLMM with R.'
# Zuur, Hilbe, Ieno.
# The chapter is a general introduction to gamma GLM
# (and GLMM). The code below contains more things 
# than needed for the exercise, but perhaps it
# contains some useful things.




###################################################################
#Load packages and library files
library(lattice)  #Needed for data exploration
library(mgcv)     #Needed for data exploration
source("HighstatLibV9.R")  
##################################################################



##################################################################
#Data exploration
#Outliers
MyVar <- c("Biomass", "Diameter", "Height", "Leaves")
Mydotplot(Seedlings[,MyVar])


#And for the book chapter we used:
AllY  <- as.vector(as.matrix(Seedlings[,MyVar]))
AllX  <- rep(1:nrow(Seedlings), length(MyVar))
AllID <- rep(MyVar, each = nrow(Seedlings))

xyplot(AllX ~ AllY | factor(AllID),
         strip = strip.custom(bg = 'white',
                               par.strip.text = list(cex = 1.2)),
         scales = list(x = list(relation = "free", draw = TRUE),
                       y = list(relation = "same", draw = FALSE)),
          col = 1, cex  = 0.75, pch = 16,
          xlab = list(label = "Value of the variable", cex = 1.5),
          ylab = list(label = "Order of the data from text file", cex = 1.5))





#Number of observation per species
table(Seedlings$Species)

#Collinearity
MyVar <- c("Diameter", "Height", "Leaves")
Mypairs(Seedlings[, MyVar])

MyPairs2 <- function(Z) {
  MyVarx <- colnames(Z)
  pairs(Z, labels = MyVarx,
      cex.labels =  2,
      lower.panel = function(x, y, digits=2, prefix="", cex.cor = 7) {
        panel.cor(x, y, digits, prefix, cex.cor)}, 
      upper.panel =  function(x, y) points(x, y, 
                                           pch = 16, cex = 1.2, 
                                           col = gray(0.1)))
 #print(P)
}
MyPairs2(Seedlings[, MyVar])



#Relationships
Myxyplot(Seedlings, MyVar, "Biomass")
Mybwplot(Seedlings,c("Biomass"),"Species")



#Create a new categorical variable, Gen_sp
Seedlings$Gen_Sp<-factor(paste(substr(Seedlings$Genus,1,1),
                         Seedlings$Species, sep=". "))


xyplot(Biomass ~ Diameter | Gen_Sp,
       data = Seedlings,
       strip = function(bg='white', ...) strip.default(bg='white', ...),
              scales = list(alternating = T,
                            x = list(relation = "same"),
                            y = list(relation = "same")),
              xlab = list(label = "Diameter", cex = 1.5),
              ylab = list(label = "Biomass", cex = 1.5),
              panel=function(x,y){
                panel.grid(h=-1, v= 2)
                panel.points(x,y,col=1,pch =1)
                tmp<-lm(y~x)
                MyData <- data.frame(x = seq(min(x), max(x), length = 25))
                p1 <- predict(tmp, newdata = MyData, type ="response")
                panel.lines(MyData$x,p1, col = 1, lwd = 2)
              })
##################################################################




##################################################################
#Section 6.4
M1 <- lm(Biomass  ~ Species * Diameter + Height + Leaves,
         data = Seedlings)

summary(M1)
drop1(M1, test = "F")

#Model validation
E1 <- rstandard(M1)
F1 <- fitted(M1)
plot(x = F1,
     y = E1,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, v = 0, lty = 2)
#Trouble!!



########################################################
#Section 6.5

# The chapter goes on by giving a simple introduction
# to a gamma distribution.
# So..what is a GLM with a gamma distribution?

# The gamma GLM is given by:
#   Y[i]      ~ Gamma(mu[i], r)
#   E(Y[i])   = mu[i]
#   var(Y[i]) = mu[i]^2 / r
#   mu[i]     = exp(eta[i])
#   eta[i]    = Covariates

# Very similar to a negative binomial 
# GLM! The r is like the k from NB
# The only limitation is that a gamma
# is for strictly positive data: Y > 0

# Instead of a log link you can also use 
# the identity link


# So...how does the gamma GLM look like?
# COPY AND RUN THE CODE FROM HERE.....
#Figure 6.7
par(mfrow = c(2,2), mar = c(5,5,2,2))
x <- seq(0,1, length = 25)
mu <- exp(1 + 2 * x)

plot(x,mu, type = "l", cex.lab = 1.5,
     xlab = "Covariate",
     ylab = "Biomass")
x1 = seq(0,25, length = 200)

Shape <- 15
Scale <- 5 / Shape
plot(x1, dgamma(x = x1, shape = Shape, scale = Scale), type = "l",
     xlab = "Possible Biomass values",
     ylab = "Probability",
     xlim = c(0,25),
     cex.lab = 1.5)

Shape <- 10
Scale <- 5 / Shape
lines(x1, dgamma(x = x1, shape = Shape, scale = Scale), type = "l")

Shape <- 5
Scale <- 5 / Shape
lines(x1, dgamma(x = x1, shape = Shape, scale = Scale), type = "l")

Shape <- 3
Scale <- 5 / Shape
lines(x1, dgamma(x = x1, shape = Shape, scale = Scale), type = "l")

Shape <- 2
Scale <- 5 / Shape
lines(x1, dgamma(x = x1, shape = Shape, scale = Scale), type = "l")

Shape <- 1
Scale <- 5 / Shape
lines(x1, dgamma(x = x1, shape = Shape, scale = Scale), type = "l")


###########
Shape <- 15
Scale <- 10 / Shape
plot(x1, dgamma(x = x1, shape = Shape, scale = Scale), type = "l",
     xlab = "Possible Biomass values",
     ylab = "Probability",
     xlim = c(0,25),
     cex.lab = 1.5)

Shape <- 10
Scale <- 10 / Shape
lines(x1, dgamma(x = x1, shape = Shape, scale = Scale), type = "l")

Shape <- 5
Scale <- 10 / Shape
lines(x1, dgamma(x = x1, shape = Shape, scale = Scale), type = "l")

Shape <- 3
Scale <- 10 / Shape
lines(x1, dgamma(x = x1, shape = Shape, scale = Scale), type = "l")

Shape <- 2
Scale <- 10 / Shape
lines(x1, dgamma(x = x1, shape = Shape, scale = Scale), type = "l")

Shape <- 1
Scale <- 10 / Shape
lines(x1, dgamma(x = x1, shape = Shape, scale = Scale), type = "l")



###############
Shape <- 15
Scale <- 15 / Shape
plot(x1, dgamma(x = x1, shape = Shape, scale = Scale), type = "l",
     xlab = "Possible Biomass values",
     ylab = "Probability",
     xlim = c(0,25),
     cex.lab = 1.5)

Shape <- 10
Scale <- 15 / Shape
lines(x1, dgamma(x = x1, shape = Shape, scale = Scale), type = "l")

Shape <- 5
Scale <- 15 / Shape
lines(x1, dgamma(x = x1, shape = Shape, scale = Scale), type = "l")

Shape <- 3
Scale <- 15 / Shape
lines(x1, dgamma(x = x1, shape = Shape, scale = Scale), type = "l")

Shape <- 2
Scale <- 15 / Shape
lines(x1, dgamma(x = x1, shape = Shape, scale = Scale), type = "l")

Shape <- 1
Scale <- 15 / Shape
lines(x1, dgamma(x = x1, shape = Shape, scale = Scale), type = "l")


# ALL THE WAY UP TO HERE....
# These figures show gamma distributions
# Some look like the normal distribution, others
# look completely different.

# Different software routines have different ways of modelling
# a Gamma GLM. For example in the glm function we have:

# Movement_ij ~ Gamma(scale_ij, shape)
# E(Movement_ij)   = shape * scale_ij
# var(Movement_ij) = shape * scale_ij^2
# 
# This uses the scale and the shape. 
# And it is quite confusing to figure out what is what.
# The glm function models the scale_ij as a function 
# of the covariates.





# Here is another picture what the gamma GLM
# is doing.
# COPY FROM HERE...
#Figure 6.8
par(mfrow = c(1,1))
z1 <- seq(0, 1, length = 50)
Bio <- exp(1 + 2 * z1)
Shape <- 15
Z<-matrix(nrow=50,ncol=50)
for (i in 1:50){
	Scale <- Bio[i] / Shape 
	Z[,i]<-dgamma(x=Bio, shape=Shape, scale = Scale )
}

persp(x = z1, y = Bio, z = Z,
                 scale = TRUE,
                 theta = 130, phi = 20, expand = 1,
                 ltheta = 120, shade = 0.5, 
                 ticktype = "detailed",
                 xlab = "Covariate", 
                 ylab = "Biomass", 
                 zlab = "Probability",
                 main = "")  -> res
round(res,3)
lines (trans3d(z1, y = Bio, 
               z = rep(0,50), 
               pmat = res), col = grey(0.5), lwd=5)

lines (trans3d(z1[45:50], y = Bio[45:50], 
               z = rep(0,6), 
               pmat = res), col = 1, lwd=5)

# TO HERE.....
################################################



################################################
#Section 6.5.5
# This is how you do the gamma GLM with the glm
# function:
M2 <- glm(Biomass  ~ Diameter + Height + Leaves,
          data = Seedlings,
          family = Gamma(link ="log") )          
summary(M2)


#Calculate Pearson residuals manually
X <- model.matrix(~ Diameter + Height + Leaves,
                    data = Seedlings)

eta <- X %*% coef(M2)
mu  <- exp(eta)
EPearson  <- (Seedlings$Biomass - mu)/(mu)


# A gamma GLM cannot be overdispersed.
# So..there is no need for this type of stuff
N <- nrow(Seedlings)
p <- length(coef(M2))
dispersion <- sum(EPearson^2)/( N - p)
dispersion


#Calculation of the SCALE parameter
Scale <- fitted(M2,type = response) / 13.46856


#Figure 6.9
#This is the model fit.
#To get an impression what the gamma distribution
#is doing we have added some random gamma data. 

#COPY AND RUN FROM HERE ....
par(mar = c(5,5,2,2))
plot(x=Seedlings$Diameter,
     y = Seedlings$Biomass,pch = 1, 
     col = grey(0.1), cex = 1,
     cex.lab = 1.5,
     xlab = "Diameter",
     ylab = "Biomass",
     ylim = c(0,40))

MyData <- data.frame(Diameter = seq(min(Seedlings$Diameter),
                                     max(Seedlings$Diameter),
                                     length =100),
                     Height = mean(Seedlings$Height),
                     Leaves = mean(Seedlings$Leaves)                 
                                     )
P3 <- predict(M2, newdata = MyData, type = "response")
lines(MyData$Diameter, P3, lwd = 3)

MyData <- data.frame(Diameter = seq(min(Seedlings$Diameter),
                                     max(Seedlings$Diameter),
                                     length =25),
                     Height = mean(Seedlings$Height),
                     Leaves = mean(Seedlings$Leaves)                 
                                     )
P3 <- predict(M2, newdata = MyData, type = "response")

for (i in 1:nrow(MyData)){
  Scale <- P3[i] / 13.46856
  yi <- rgamma(50, shape = 13.46856, scale = Scale)
  points(rep(MyData$Diameter[i], 50), 
         yi, cex = 0.75, col = grey(0.3), pch = 16)
}

#TO HERE....
#####################################################




###########################################################
#Section 6.5.7
#So...what happens if you do the identity link?

#COPY AND RUN FROM HERE....
M2B <- glm(Biomass  ~ Diameter + Height + Leaves,
          data = Seedlings,
          family = Gamma(link ="identity"),
          start = c(0, 0.5, 0, 0) )
          


MyData <- data.frame(Diameter = seq(min(Seedlings$Diameter),
                                     max(Seedlings$Diameter),
                                     length =100),
                     Height = mean(Seedlings$Height),
                     Leaves = mean(Seedlings$Leaves)                 
                                     )
P2B <- predict(M2B, newdata = MyData, type = "response")
par(mar=c(5,5,2,2))
plot(x=Seedlings$Diameter,
     y = Seedlings$Biomass,pch = 16, 
     cex = 0.9,
     cex.lab = 1.5,
     xlab = "Diameter",
     ylab = "Biomass")
lines(MyData$Diameter, P2B, lwd = 3)

#TO HERE....
#Why do you get warning messages?
#GAMMA DISTRIBUTION CANNOT COPE WITH -VE VALUES - BETTER TO USE LOG-LINK IN THIS CASE RATHER THAN "IDENTITY"

############################################################


          
          
          
          
######################################          
#And some code for doing a gamma GAM
#See GAM chapter what exactly a gamma GLM is
M2 <- glm(Biomass  ~ Diameter + Height + Leaves, 
          data = Seedlings,
          family = Gamma(link ="log") )
summary(M2)
drop1(M2, test = "F")


E2 <- resid(M2, type = "pearson")
F2 <- fitted(M2, type = "response")
plot(x=F2, y=E2)
abline(h=0, v=0, lty = 2)



#Plot residuals vs each covariate
plot(x = Seedlings$Height,
     y = E2)
abline(h = 0, lty = 2)

plot(x = Seedlings$Diameter,
     y = E2)
abline(h = 0, lty = 2)

plot(x = Seedlings$Leaves,
     y = E2)
abline(h = 0, lty = 2)

# Are there any patterns in these residuals???
# Check with a GAM     
#
T1 <- gam(E2 ~ s(Height), data = Seedlings)
summary(T1)
plot(T1)
abline(h = 0, lty = 2)
#Nah...that is OK-ISH


T1 <- gam(E2 ~ s(Diameter), data = Seedlings)
summary(T1)
plot(T1)
abline(h = 0, lty = 2)
#That is more serious!!!


T1 <- gam(E2 ~ s(Leaves), data = Seedlings)
summary(T1)
plot(T1)
abline(h = 0, lty = 2)
#Close to OK-ISH

#So......there is a strong non-linear Diameter effect in the residuals
#Should we fit a GAM with a gamma distribution
#Or should we do a gamma GLM with identity link??




#Based on the results let's only use
#Diameter as a smoother
M3 <- gam(Biomass  ~ s(Diameter) + Height + Leaves, 
          data = Seedlings,
          family = Gamma(link ="log") )
summary(M3)
#The scale estimater is the v in the variance function
plot(M3)

#A gamma GLM/GAM cannot be overdispersed.
#gamma GAM in MCMC is discussed in the book chapter.







