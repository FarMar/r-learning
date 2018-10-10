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
RW <- read.table(file = "ragweed.txt", 
                 header = TRUE,
                 dec = ".")
#dec = '.'   means that the point is used for decimals. 
#Change to   dec = ","   if required.
########################################################################


#Description
#The variable of interest is ragweed pollen. To avoid allergic 
#reactions in humans due to pollen, it is important to have a 
#model that can predict daily pollen levels. And it is also 
#important to know which environmental variables can cause high 
#pollen levels. The data were sampled in Michigan, US in 1991-1994.

# The variables are given below. The data are in the file 
# ragweed.txt. Each row in the text file represents a day.

#Variables in the file ragweed.txt.

#Ragweed       Ragweed level for that day (grains / m3)
#Temperature   Temperature
#Rain          Indicator for rainfall (1 is > 3 hours of steady or 
#              brief rain; 0 is otherwise)
#Windspeed	   Windspeed (knots)	
#DayinSeason   Day number in year	
#Year	       



########################################################################
#House keeping
#Load packages from R and support functions that we wrote
library(lattice)  
source("HighstatLibV9.R")
########################################################################



########################################################################
#Inspect the file
names(RW)
str(RW)
########################################################################






###########################################################
#Data exploration

#Look at outliers
#outliers in the response?
dotchart(RW$Ragweed, group = factor(RW$Year))  #no outliers
sum(RW$Ragweed==0) / nrow(RW)

#outliers in the x? 
par(mfrow=c(2,3), mar=c(4,4,2,2))
dotchart(RW$Year, main = "Year")
dotchart(RW$DayinSeason, main = "DayinSeason")
dotchart(RW$Temperature, main = "Temperature")
dotchart(RW$Rain, main = "Rain")
dotchart(RW$WindSpeed, main = "WindSpeed")
#Everything ok?
tapply(RW$DayinSeason, FUN = range, INDEX = RW$Year)


#Or all in one graph
MyVar <- c("Ragweed", "Year", "DayinSeason", 
           "Temperature", "Rain", "WindSpeed")

Mydotplot(RW[,MyVar])




##################################################
#Collinearity
MyVar <- c("Year", "DayinSeason", 
           "Temperature", "Rain", "WindSpeed")

#Collinearity covariates
pairs(RW[, MyVar], 
      lower.panel = panel.cor)


#####################################
#Relationships

#Make a multi-panel scatterplot
MyX  <- c("DayinSeason", "Temperature", "WindSpeed")
Myxyplot(RW, MyX, "Ragweed", MyYlab = "rag weed pollen")


#And plot Ragweed pollen versus Rain using a boxplot
par(mfrow = c(1,1))
boxplot(Ragweed ~ Rain, 
        data = RW,
        varwidth = TRUE)




###########################################################
#Analysis
#Due to a certain degree of collinearity we decided to drop
#DayinSeason for the moment
#We use all main term and the year-temperature interaction

#We start with a Gaussian model...but this is essentially wasting time
#as we should start with a Poisson or NB GLM. But it is nice to see
#how to reckognize that lm is wrong. 
M1 <- lm(Ragweed ~ factor(Year) + factor(Rain) + 
                   Temperature + WindSpeed +
                   Temperature : factor(Year), data = RW)
         
summary(M1)
drop1(M1, test = "F")
step(M1)

M1A <- lm(Ragweed ~ factor(Year) + factor(Rain) + Temperature + WindSpeed, 
          data = RW)
drop1(M1A, test = "F")

#Overrule AIC
M1B <- lm(Ragweed ~ factor(Year) + factor(Rain) + Temperature, 
          data = RW)
drop1(M1B, test = "F")


#Model validation
par(mfrow = c(2,2))
plot(M1B)


#Homogeneity
E1 <- resid(M1B)
F1 <- fitted(M1B)
par(mfrow = c(1,1))
plot(x = F1, y = E1, xlab = "fitted", ylab = "Residuals")
abline(h = 0, lty = 2)
abline(v = 0, lty = 2, col = 2)



#Trouble...apply a log transformation
RW$LOGRagweed <- log(RW$Ragweed + 1)
M2 <- lm(LOGRagweed ~ factor(Year)+ factor(Rain) + Temperature + WindSpeed +
                     Temperature : factor(Year), data = RW)
         
summary(M2)
drop1(M2, test = "F")
par(mfrow = c(2,2))
plot(M2)
#It would barely pass....there is some evidence of heterogeneity


#Homogeneity
E2 <- resid(M2)
F2 <- fitted(M2)
par(mfrow = c(1, 1))
plot(x = F2, 
     y = E2, 
     xlab = "fitted", 
     ylab = "Residuals")
abline(h = 0, lty = 2)


#Influential observations
plot(M2,which = c(4))


#Independence/patterns
par(mfrow = c(1,1))
#Plot residuals vs Temperature
with(RW, plot(y = E2, x = Temperature))
abline(0,0)
#Ok

#Plot residuals vs WindSpeed
with(RW, plot(y = E2, x = WindSpeed))
abline(0,0)
#Nice

#Plot residuals vs year
boxplot(E2 ~ Year, data = RW)
abline(0,0)
#Nice

#Plot residuals vs Rain
boxplot(E2 ~ Rain, data = RW)
abline(0,0)
#Nice

#Plot residuals versus day in season
with(RW, plot(y = E2, x = DayinSeason))
abline(0,0)
#Ough....

xyplot(E2 ~ DayinSeason | factor(Year), data = RW, col = 1)
#Each year has residual patterns. Does the seasonal 
#effect change per year








#Problems with this analysis:
#1. Negative fitted values
#2. We needed a log to deal with heterogeneity
#3. Seasonal patterns within the residuals!
#########################################################################











##########################################################################
#Apply a GLM because we essentially have a count!
M3 <- glm(Ragweed ~ factor(Year)+ factor(Rain) + Temperature + WindSpeed +
                    Temperature : factor(Year), 
          data = RW, family = poisson)
E3 <- resid(M3, type = "pearson")
Overdispersion <- sum(E3^2) / M3$df.res
Overdispersion

#Why do we have this?
#A. Outliers?              => Remove
#B. Non-linear patterns    => GAM
#C. Zero inflation?        => ZI GLM
#D. Missing covariates or missing interactions ==> Add them
#E. Dependency             => GLMM 
#F. Variation is large!    => NB GLM

F3 <- fitted(M3)
plot(x = F3, y = E3)

#plot Cook values
plot(M3,which = c(4))
#Lots of observations with high Cook

#Zero inflation:
sum(RW$Ragweed == 0) / nrow(RW)

#B: Non-linear patterns:
#Plot residuals versus day in season
with(RW, plot(y = E3, x = DayinSeason))
abline(0,0)
#Ough....the model really needs DayinSeason..and the effect seems non-linear
#And that means a Poisson GAM!




###########################################################################
library(mgcv)
#Let's be brave...
M4 <- gam(Ragweed ~ factor(Year)+ factor(Rain) +
                     s(Temperature) + s(WindSpeed) + 
                     s(DayinSeason), data = RW,
          family = poisson)
summary(M4)
par(mfrow = c(2,2))
plot(M4)
#...but be stupid too..!
#The fact that smoothers are used for both Temperature and DayinSeason
#makes me rather nervous. I would prefer to use Temperature as a linear term

M5 <- gam(Ragweed ~ factor(Year)+ factor(Rain) +
                     Temperature + WindSpeed + 
                     s(DayinSeason), data = RW,
          family = poisson)

E5 <- resid(M5, type = "pearson")
Overdispersion <- sum(E5^2) / M5$df.res
Overdispersion

#Option 1: quasipoisson
#Option 2: NB GAM
#Option 3: Observation level random intercept + GAM (different course)

#quasipoisson
M6A <- gam(Ragweed ~ factor(Year)+ factor(Rain) +
                     Temperature + WindSpeed + 
                     s(DayinSeason), data = RW,
          family = quasipoisson)

E6A <- resid(M6A, type = "pearson")
summary(M6A)
anova(M6A)
plot(M6A)

#NB GAM
M6B <- gam(Ragweed ~ factor(Year)+ factor(Rain) +
                     Temperature + WindSpeed +
                     s(DayinSeason),
          family=negbin(c(1,10),link = log),
          data = RW)

M6B.future <- gam(Ragweed ~ factor(Year)+ factor(Rain) +
                     Temperature + WindSpeed +
                     s(DayinSeason),
          family=nb(),
          data = RW)


#Ragweed_i ~ NB(mu_i, k)
#E(Ragweed_i) = mu_i
#var(Ragweed_i) = mu_i + mu_i^2 / k
#log(mu_i) = alpha + Temp_i + Wind_i + Year_i + s(DayinSeason_i) + Rain_i

plot(M6B.future)
summary(M6B.future)
anova(M6B.future)  #GAM equivalent of drop1
E6B <- resid(M6B.future, type = "pearson")
Overdispersion <- sum(E6B^2) / M6B.future$df.res
Overdispersion
#Perfect

E6B <- resid(M6B.future, type = 'pearson')
par(mfrow = c(3,1))
plot(x=RW$Temperature, y = E6B)
plot(x=RW$WindSpeed, y = E6B)
plot(x=RW$DayinSeason, y = E6B)


# Ragweed_i ~ NB(mu_i, 1.85)
# E(Ragweed_i) = mu_i
# var(Ragweed_i) = mu_i + mu_i^2 / 1.85
# mu_i = exp(eta_i)

# eta_i = Covariate stuff
      # = Intercept + Year_i + Rain_i + Temp_i + Wind_i + f(DayinSeason_i)
    
#1991 and no rain:
#mu_i = exp(-1.51 + 0.04 * Temperature + 0.10 * WindSpeed + s(DayInSeason) )

#1992 and no rain
#mu_i = exp(-1.51 -0.027 0.04 * Temperature + 0.10 * WindSpeed + s(DayInSeason) )

#1994 and rain
#mu_i = exp(-1.51 -0.645 + 0.654 + 0.04 * Temperature + 0.10 * WindSpeed + s(DayInSeason) )


par(mfrow = c(1,1))
plot(M6B)

#Now the golden question....is the seasonal effect the same in each year?
#NB GAM

M7 <- gam(Ragweed ~ factor(Rain) + factor(Year) +
                     Temperature + WindSpeed +
                     s(DayinSeason, by = factor(Year)),
          family=negbin(c(1,10),link = log),
          data = RW)
#The smooths from s(..by = ..) are centred around 0...so you do 
#need the factor(Year) term
E7 <- resid(M7, type = "pearson")
Overdispersion <- sum(E7^2) / M7$df.res
Overdispersion
#Perfect


AIC(M6B, M7)

#          df      AIC
#M6B 14.43330 2398.201
#M7  37.05021 2260.184

#So..the second model is much (!) better.
#Let's plot it'
par(mfrow = c(2,2))
plot(M7, scale = FALSE)

par(mfrow = c(2,2))
plot(M7, scale = FALSE, select = c(1))
plot(M7, scale = FALSE, select = c(2), xlim = c(0, 70), ylim = c(-5,5))
plot(M7, scale = FALSE, select = c(3))
plot(M7, scale = FALSE, select = c(4), xlim = c(0, 70), ylim = c(-5,5))


#Model validation of M7
F7 <- fitted(M7)
par(mfrow = c(1,1))
plot(x = F7, y = RW$Ragweed)



#Independence/patterns
par(mfrow = c(1,1))

#Plot residuals vs Temperature
with(RW, plot(y=E7, x = Temperature))
abline(0,0)
#Good

#Plot residuals vs WindSpeed
with(RW, plot(y=E7, x = WindSpeed))
abline(0,0)
#Nice

#Plot residuals vs year
boxplot(E7 ~ Year, data = RW)
abline(0,0)
#Nice

#Plot residuals vs Rain
boxplot(E7 ~ Rain, data = RW)
abline(0,0)
#Nice

#Plot residuals versus day in season
with(RW, plot(y=E7, x = DayinSeason))
abline(0,0)
#Nice
###############################################




###############################################
#Write down the model

#Ragweed_i ~ NB(mu_i, 3.814)
#E(Ragweed_i) = mu_i
#var(Ragweed_i) = mu_i + mu_i^2 / 3.814

#Rain = 0 and Year = 1991
#log(mu_i) = -1.79 + 0.048 * Temp_i +
#             0.09 * WindSpeed +
#             f_j(DayInSeason)
     
     
     
###########################################     
#You can do prediction with GAM:             
MyData1 <- data.frame(
             Rain = 0,
             Year = 1991,
             Temperature = 75,
             WindSpeed   = 15,
             DayinSeason = seq(1,70)
               )    
P1 <- predict(M7, 
              newdata = MyData1, 
              type = "link",
              se = TRUE )

#Day in Season effect for 1991, for wind speed = 15 and temp = 75
plot(MyData1$DayinSeason, exp(P1$fit), type = "l", ylim = c(0, 350))
lines(MyData1$DayinSeason, exp(P1$fit + 2 * P1$se.fit), type = "l", lty = 2)
lines(MyData1$DayinSeason, exp(P1$fit - 2 * P1$se.fit), type = "l", lty = 2)







#######################################################
#And this is code for a binomial GAM
RW$Ragweed01 <- RW$Ragweed
RW$Ragweed01[RW$Ragweed > 0] <- 1
RW$Ragweed01


M8 <- gam(Ragweed01 ~ factor(Rain) + factor(Year) +
                     Temperature + WindSpeed +
                     s(DayinSeason, by = factor(Year)),
          family=binomial,
          data = RW)
summary(M8)


M9 <- gam(Ragweed01 ~ factor(Rain) + factor(Year) +
                     Temperature + WindSpeed +
                     s(DayinSeason),
          family=binomial,
          data = RW)
summary(M9)

plot(M9)
anova(M9)
AIC(M9)

