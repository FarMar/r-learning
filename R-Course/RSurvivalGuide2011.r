#R survival guide
#Highland Statistics Ltd.
#
#
#This file contains R code that will be used in the
#regression, GLM and GAM course provided by Highland Statistics Ltd.
#The underlying statistical methods are explained in:
#Zuur, AF, Ieno, EN, Smith, GM. (2007). Analysing Ecological Data. Springer.
#
#The code is also useful as preparation for the mixed modelling course
#provided by Highland Statistics Ltd.
#
#The code is fully explained in our: 
#Beginner's Guide to R (2009)  By: Zuur, Ieno, Meesters. Springer



##############################################################################
#Part A: READING AND PREPARING THE DATA
#We assume that your data are in Excel. Export the data  from Excel
#to tab-delimited ascii file and close Excel.
#
#The following commands import the RIKZ data into R. This is a
#marine benthic data set used in Zuur et al (2007). We will model
#species richness as a function of variables explanatory variables.
#
#

setwd("C:/StatsCourse")
#Note how it has been spelled: StatsCourse
#On a Mac use: setwd("/Users/...../StatsCourse/")



RIKZ <- read.table(file = "RIKZRichness.txt", header = TRUE, dec = ".")


#These two command assumes that the data are in the directory C:/Course. 
#If you stored the data somehwere else, change the argument in the setwd 
#command.

#The data are in the format: 12345.67. The . is used for decimal seperator. 
#For some systems (e.g. Portugal, The netherlands), you have to use:
#
#RIKZ  <- read.table(file = "RIKZRichness.txt", header = TRUE, dec = ",")

#We have put a # in front of the command to ensure that you do not run both
#read.table commands! Change the path and the file name for
#your own data.

#Check the import process by:
str(RIKZ)

#'data.frame':   45 obs. of  15 variables:
# $ Sample       : int  1 2 3 4 5 6 7 8 9 10 ...
# $ Richness     : int  11 10 13 11 10 8 9 8 19 17 ...
# $ week         : int  1 1 1 1 1 1 1 1 1 1 ...
# $ angle1       : int  32 62 65 55 23 129 126 52 26 143 ...
# $ angle2       : int  96 96 96 96 96 89 89 89 89 89 ...
# $ exposure     : int  10 10 10 10 10 8 8 8 8 8 ...
# $ salinity     : num  29.4 29.4 29.4 29.4 29.4 29.6 29.6 29.6 29.6 29.6 ...
# $ temperature  : num  17.5 17.5 17.5 17.5 17.5 20.8 20.8 20.8 20.8 20.8 ...
# $ NAP          : num  0.045 -1.036 -1.336 0.616 -0.684 ...
# $ penetrability: num  254 227 237 249 252 ...
# $ grainsize    : num  222 200 194 221 202 ...
# $ humus        : num  0.05 0.3 0.1 0.15 0.05 0.1 0.1 0.1 0.15 0 ...
# $ chalk        : num  2.05 2.5 3.45 1.6 2.45 2.5 1.85 1.7 2.3 2.6 ...
# $ sorting1     : num  69.8 59 59.2 67.8 57.8 ...
# $ Beach        : int  1 1 1 1 1 2 2 2 2 2 ...

#If it does not say num and int, but factor, then you used the wrong dec argument




##############################################################################
#Part B: ACCESSING THE DATA
#To see what is in the object RIKZ, type:

names(RIKZ)

#It gives:
#
# [1] "ID"            "Richness"      "week"          "angle1"
# [5] "angle2"        "exposure"      "salinity"      "temperature"
# [9] "NAP"           "penetrability" "grainsize"     "humus"
#[13] "chalk"         "sorting1"      "Beach"
#
#ID is the observation number, Richness is the response variable, and
#everything else are explanatory variables. See Zuur et al. (2007) for an
#explanation
#
#If you type:
#
Richness

#nothing will happen. There are multiple ways to access the variables in the
#data frame RIKZ...easy options, difficult options and dangerous options.
#The dangerous option is attach, and we will not use it.


#Type:
RIKZ$Richness

#and

RIKZ$temperature

#Note that this is all capital sensitive!


RIKZ$Temperature
#gives an error message, and so does:
RIKZ$temprature

#You just have to learn to spell things correct.








##############################################################################
#Part C: DATA EXPLORATION: 

#C.1: OUTLIERS
#The first step of any analysis should be a data exploration. Look for outliers,
#collinearity in the explanatory variables, and relationships between response and
#explanatory variables.
#
#Good tools for outliers are boxplots and Cleveland dotplots. See Zuur et al (2007)
#for examples and an explanation how to use them.
#

boxplot(RIKZ$Richness)
boxplot(Richness ~ factor(week), data = RIKZ  )
dotchart(RIKZ$Richness)
dotchart(RIKZ$Richness, groups = factor(RIKZ$week), col = RIKZ$week)

#Some functions can use the data = RIKZ arguments, others not.

#Be careful with col = RIKZ$week.
#It has the values 0, 1, 2 and 3 (instead of 1, 2, 3 and 4). The
#colour 0 is white!
#
#It is also possible to get these graphs in one figure:
#
par(mfrow=c(2,2))
boxplot(RIKZ$Richness)
boxplot(Richness ~ factor(week), data = RIKZ)
dotchart(RIKZ$Richness)
dotchart(RIKZ$Richness, groups = factor(RIKZ$week))

#See

?par

#for further options in the par command, e.g. the mar option to reduce the
#white space around the individual panels.
#
#To transform variables use:
#
RIKZ$Loggrainsize  <- log10(RIKZ$grainsize)

#or

RIKZ$grainsize2    <- log10(RIKZ$grainsize)

#or

RIKZ$SQRTgrainsize <- sqrt(RIKZ$grainsize)

#You can use any name of the left side of the <-    but avoid names with: %$^&*().


#It is also possible to make multipanel dotplots:
library(lattice)
MyVar <- c("grainsize","NAP","temperature","salinity")

dotplot(as.matrix(RIKZ[,MyVar]), groups=FALSE,
        strip = strip.custom(bg = 'white',
        par.strip.text = list(cex = 0.8)),
        scales = list(x = list(relation = "free"),
                      y = list(relation = "free"),
                      draw = FALSE),
        col=1, cex  =0.5, pch = 16,
        xlab = "Value of the variable",
        ylab = "Order of the data from text file")

#You only need to change the arguments in the MyVar character string
#and the RIKZ object on the first line
#







#C.1: COLLINEARITY X
#Here, we can use pair plots and correlation coefficients. To make a pairplot,
#use:
#

MyVar <- c("salinity","temperature","NAP","grainsize")
pairs(RIKZ[,MyVar])



#
#It is also nice to add smoothers or correlation coefficients to the pairplot.
#See the pairs help file (?pairs) or some of the examples in Zuur et al. (2007).
#You can do this with:
#

#NOTE: THIS GIVES AN ERROR MESSAGE, SEE THE TEXT BELOW
pairs(RIKZ[,MyVar], lower.panel = panel.smooth,
         upper.panel = panel.cor, 
         diag.panel = panel.hist)


#This gives an error message, because R does not know (yet)
#what panel.cor and panel.hist is. We took these files from the pairs help file
#and changed them a little bit. In order to tell R what it is, copy and paste
#the rather impressive looking code in the file: HighstatLibV6.R into R. The 
#alternative is to type:
#
source(file="HighstatLibV6.R")

#This command will look in your working directory for the file HighstatLibV6.R. So,
#in our case, this is the directory: C:/Course". If you stored it somewhere else,
#use:
#

source(file="C:/......your directory..../HighstatLibV6.R")


#Don't even try to understand what the code in this file does....just copy and 
#paste it all (or source it). You have to do this process each time you start a 
#new R session. Once copied and pasted, this will work:
#

pairs(RIKZ[,MyVar], lower.panel = panel.smooth2,
         upper.panel = panel.cor,
         diag.panel = panel.hist)


#Or if you want to have lines instead of smoothers, use:

pairs(RIKZ[,MyVar], lower.panel = panel.lines2,
         upper.panel = panel.cor,
         diag.panel = panel.hist)


#You can also remove some of the options:

pairs(RIKZ[,MyVar], lower.panel = panel.cor)

#If you want to change the labels in the diagonal panels, see ?pairs. There is
#as label option. Use something along the lines of: pairs(...bla bla,labels=c("a","b",etc),
#Talking about correlation coefficients, you can get these also by:
#
cor(RIKZ[,MyVar], use = "pairwise.complete.obs")

#Ensure that MyVar does not contain any factors!
#The use="pairwise.complete.obs" is handy for missing values (which should
#bne labelled as NA in Excel by the way)! Chapter 26 in Zuur et al. (2007)
#uses variance inflation factors (VIF). We wrote a routine to calculate these.
#If you have not copied and pasted the code in the file HighstatLib.R into R yet, then
#now is the time to do so, else this will not work:
#

corvif(RIKZ[,MyVar])

#This routine calculates VIF values.







#C.3:  RELATIONSHIPS BETWEEN RESPONSE AND EXPLANATOY VARIABLES
#The simplest option is to use the plot command:

plot(y = RIKZ$Richness, x = RIKZ$NAP, xlab = "NAP", 
     ylab = "Richness",
     main = "Scatterplot")

#or

plot(y = RIKZ$Richness,
     x = RIKZ$NAP,
     xlab = "NAP",
     ylab = "Richness",
     main = "Scatterplot",
     cex = RIKZ$week/4)

#or use:

boxplot(Richness ~ factor(week), data = RIKZ)


#This is useful for identifying individual observations:
plot(y = RIKZ$Richness, x = RIKZ$NAP, xlab = "NAP", 
     ylab = "Richness",
     main = "Scatterplot")
identify(y = RIKZ$Richness, x = RIKZ$NAP)

#Click close to a point 
#Press escape to quit
#



#More advanced graphs that focus on interaction are coplots. These
#can be very simple....and very advanced. Below we give a series of
#coplots that each do something slightly different. Don't use them
#all...just the ones you like.
#

coplot(Richness ~ NAP | factor(week), data = RIKZ)

#If you want to add smoothers in each panel, use:


coplot(Richness ~ NAP | factor(week), data = RIKZ,
       panel = panel.smooth)


#To make these graphs for your own data, change Richness, NAP and week. In the
#code above, week is a factor. If you have a continuous variable, use:

coplot(Richness ~ NAP | temperature, data = RIKZ, 
       panel = panel.smooth)


#Smoothers with a different span width are obtained by:
coplot(Richness~NAP|factor(week), data = RIKZ,
        panel = function(x, y, ...)
         panel.smooth(x, y, span = .8, ...))

#Do not change the x and y bit! To have points in the panels, use:
coplot(Richness ~ NAP | factor(week), data = RIKZ,
        panel = function(x, y, ...)
         points(x, y))

#Perhaps adding a regression line to the points is the best
#coplot.
#
coplot(Richness ~ NAP | factor(week), data = RIKZ,
        panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

#note that the l in lm is an l (from Lima), not a 1 (from 1 2 3)



#Similar graphs can be made with the xyplot plot command. The difference between
#a coplot and a xyplot from the lattice library is that the conditioning variable in
#the xyplotn has to be a nominal variable.
#

library(lattice)
xyplot(Richness ~ NAP | factor(week), data = RIKZ)

xyplot(Richness ~ NAP | factor(week), data = RIKZ,
 panel=function(x, y, subscripts, ...){
     I1 <- order(x)
     llines(x[I1], y[I1])})

#Don't change the x and y bit.
xyplot(Richness ~ NAP | factor(week), data = RIKZ,
 panel=function(x, y, subscripts, ...){
     I1 <- order(x)
     tmp <- lm(y~x)
     llines(x[I1], tmp$fit[I1], col = 1)
     panel.points(x, y, col = 1)})


#Here is an advanced graph. We will plot each covariate
#versus the response variable Richness in a xyplot
# 
XNames <- c("week",     "angle1",        "angle2",
            "exposure", "salinity",      "temperature",
            "NAP",      "penetrability", "grainsize",
            "humus",    "chalk",         "sorting1",
            "Beach")
            
Var4YAxes <- RIKZ$Richness
Z         <- RIKZ[,XNames]

#For your own data the rest of the code can stay as it is
AllY      <- rep(Var4YAxes, length(XNames))
AllX      <- as.vector(as.matrix(Z))
ID        <- rep(XNames, each = nrow(RIKZ))

xyplot(AllY ~ AllX | factor(ID),
   xlab = "Covariates",
   ylab = "Response variable",
   strip = function(bg = 'white', ...) 
   strip.default(bg = 'white', ...),
   scales = list(alternating = T, 
                x = list(relation = "free"),
                y = list(relation = "same")),
   panel=function(x,y){
    panel.grid(h=-1, v= 2)
    panel.points(x,y,col=1)
    panel.loess(x,y,col=1,lwd=2)
    })
#Warnings may be due to covariates with limited unique values








###########################################################################
#D:  LINEAR REGRESSION
#Let us now start with linear regression. Richness is modelled as a function
#of NAP, exposure (nominal), salinity and grainsize.
M1 <- lm(Richness ~ NAP + factor(exposure) + salinity + grainsize,
          data = RIKZ)

#Numerical output is obtained from:

summary(M1)
drop1(M1, test = "F")
anova(M1)

#The last one is doing sequential testing! The interpretation of the output
#is given in Chapter 5 of Zuur et al. (2007). To compare nested models,
#use:
#

M2  <- lm(Richness ~ NAP + factor(exposure) + salinity + grainsize, data = RIKZ)
M2A <- lm(Richness ~ NAP + salinity + grainsize, data = RIKZ)
anova(M2, M2A, test = "F")

#To add interaction, use:

M3 <- lm(Richness ~ NAP + factor(exposure) + NAP:factor(exposure), data = RIKZ)

#You can also fit the same model with:

M4 <- lm(Richness ~ NAP * factor(exposure), data = RIKZ)

#A backwards selection is carried out with:

step(M1)

#A forward selection is carried out with:

M5 <- lm(Richness ~ 1, data = RIKZ)
step(M5, 
    scope = list(lower = ~1, 
                 upper =~ NAP + factor(exposure) + salinity + grainsize))

#Suppose that the optimal model is given by:

M6 <- lm(Richness ~ NAP + factor(exposure), data = RIKZ)

#Exposure is a nominal variable with the values 8, 10 and 11. We now give the
#R code to fit the three paralel lines.

D1 = data.frame(NAP = RIKZ$NAP, exposure = 8)
P1 = predict(M6, newdata = D1)

D2 = data.frame(NAP = RIKZ$NAP, exposure = 10)
P2 = predict(M6, newdata = D2)

D3 = data.frame(NAP = RIKZ$NAP, exposure = 11)
P3 = predict(M6, newdata = D3)

#Just type

D1 

#to see what it does

plot(x = RIKZ$NAP, y = RIKZ$Richness, type = "p",
     xlab = "NAP",
     ylab = "Richness")
lines(D1$NAP, P1)
lines(D2$NAP, P2)
lines(D3$NAP, P3)

#This looks complicated, but all it does is create a data frame D1 with new data.
#It may be an option to use: NAP=seq(from=-1.3,to=1.2,by=0.1)
#These are the NAP values observed if exposure is 8. The predict command
#predict the new values for the specified explanatory variables. The rest is a
#matter of plotting the data and drawing the lines.
#

#Here is the same graph but with better values for NAP

R1 <- range(RIKZ$NAP[RIKZ$exposure == 8])
R2 <- range(RIKZ$NAP[RIKZ$exposure == 10])
R3 <- range(RIKZ$NAP[RIKZ$exposure == 11])

D1 = data.frame(NAP = seq(from = R1[1],
                          to   = R1[2], length = 25), 
                exposure = 8)

D2 = data.frame(NAP = seq(from = R2[1],
                          to   = R2[2], length = 25), 
                exposure = 10)

D3 = data.frame(NAP = seq(from = R3[1],
                          to   = R3[2], length = 25), 
                exposure = 11)

P1 = predict(M6, newdata = D1)
P2 = predict(M6, newdata = D2)
P3 = predict(M6, newdata = D3)

plot(x = RIKZ$NAP, y = RIKZ$Richness, type = "p",
     xlab = "NAP",
     ylab = "Richness")
lines(D1$NAP, P1)
lines(D2$NAP, P2)
lines(D3$NAP, P3)

#Something you can use for a publiction!




#GRAPHICAL MODEL VALIDATION FOR LINEAR REGRESSION
#Let us now concentrate on the graphical model validation. The standard graphical
#output is obtained by:
#
#Model validation
par(mfrow = c(2, 2))
plot(M6)

#Extract residuals
E6 <- resid(M6)      #Or:
E6 <- rstandard(M6)
F6 <- fitted(M6)

#Standardised residuals are better. The following command
#plots the (standardised) residuals versus fitted values (usful for checking
#homogeneity).
#
par(mfrow = c(1,1))
plot(F6, E6, xlab = "Fitted values", ylab = "Residuals")
abline(0,0)

#To verify the independence assumption, plot residuals versus each
#explanatory variable in the model (and also those not in the model!)
#

plot(x = RIKZ$NAP, y = E6, xlab = "NAP", ylab = "Residuals", type = "p")
abline(0,0)

#To aid visual interpretation, you can add a smoother. 
#
#If you do not have missing values, use the following code:

plot(x = RIKZ$NAP, y = E6, xlab = "NAP", ylab = "Residuals", type = "p")
abline(0,0)
tmp <- loess(E6~RIKZ$NAP,span=0.75)
tmp2 <- predict(tmp,se=T)
I1 <- order(RIKZ$NAP)
lines(RIKZ$NAP[I1], tmp2$fit[I1], lty=1)
lines(RIKZ$NAP[I1], tmp2$fit[I1] + 2*tmp2$se.fit[I1], lty = 2)
lines(RIKZ$NAP[I1], tmp2$fit[I1] - 2*tmp2$se.fit[I1], lty = 2)

#If you do have missing values, remove them using:
RIKZ2 <- na.exclude(RIKZ)

#Note: this removes any row where there is a missing value
#Perhaps you first want to remove a couple of variables that have a 
#large number of missing values before doing the na.exclude thing
#Now run all code discussed above with RIKZ2
#


#Repeat the same for all other continuous explanatory variables. For nominal
#explanatory variables, use:
#
boxplot(E6 ~ factor(RIKZ$exposure), ylab = "Residuals")

#The spread should be the same everywhere. For influential observations, you can
#use the Cook distances. These are given by the plot(tmp7) command, but can
#also be obtained by:
#


plot(cooks.distance(M6), type = "h")

#The type="h" plots the values as vertical lines.


#9 ADVANCED GRAPHICAL MODEL VALIDATION FOR LINEAR REGRESSION
#Plot the residuals versus each covariate in the model, and not in the model
#The code below is copy-paste from above, except that we use E6 instead 
#of Richness
#
library(lattice)
XNames <- c("week",     "angle1",        "angle2",
            "exposure", "salinity",      "temperature",
            "NAP",      "penetrability", "grainsize",
            "humus",    "chalk",         "sorting1",
            "Beach")
            
Var4YAxes <- E6
Z         <- RIKZ[,XNames]

#For your own data the rest of the code can stay as it is
AllY      <- rep(Var4YAxes, length(XNames))
AllX      <- as.vector(as.matrix(Z))
ID        <- rep(XNames, each = nrow(RIKZ))

xyplot(AllY ~ AllX | factor(ID),
   xlab = "Covariates",
   ylab = "Residuals",
   strip = function(bg = 'white', ...) 
   strip.default(bg = 'white', ...),
   scales = list(alternating = T, 
                x = list(relation = "free"),
                y = list(relation = "same")),
   panel=function(x,y){
    panel.grid(h=-1, v= 2)
    panel.points(x,y,col=1)
    panel.loess(x,y,col=1,lwd=2)
    })






###########################################################################
#E: GLM for count data
#The response variable Richness is a count, hence we could argue that
#we should use a Poisson distribution instead of the Gaussian distribution.
#
M7 <- glm(Richness ~ NAP + factor(exposure) + salinity + grainsize,
          family = poisson,
          data = RIKZ)
          
par(mfrow = c(2, 2))
plot(M7)
summary(M7)
step(M7)
drop1(M7, test = "Chi")
E7A <- resid(M7, type = "deviance")
E7B <- resid(M7, type = "pearson")

#All the fancy graphs that we made for linear regression can also be made for
#GLM models, see Zuur et al (2007) for examples. Note that for the predict function
#you have to use:
#
predict(M7, type = "response")


#The quasipoisson is fitted using:

M8 <- glm(Richness ~ NAP + factor(exposure) + salinity + grainsize,
          family = quasipoisson,
          data = RIKZ)
summary(M8)
drop1(M8,test = "F")

#Note that the AIC is not defined for quasipoisson, hence no step.
#You can also use nested models and analysis of deviance:
#
M8A <- glm(Richness ~ NAP + salinity + grainsize, 
           family = quasipoisson,
           data = RIKZ)
anova(M8, M8A, test = "F")



#The negative binomial GLM is applied with:
library(MASS)
M9 <- glm.nb(Richness ~ NAP + factor(exposure) + salinity + grainsize,
             data = RIKZ)
summary(M9)
step(M9)
#The same graphical model validation graphs must be made




#11 GLM for 0-1 binomial data
#For 0-1 binomial, use the glm command with the option: family=binomial. Note
#that you should not apply the quasibinomial! 

ParasiteCod <- read.table("ParasiteCod.txt", header=TRUE)

ParasiteCod$fArea <- factor(ParasiteCod$Area)
ParasiteCod$fYear <- factor(ParasiteCod$Year)
B1 <- glm(Prevalence ~ fArea * fYear + Length,
               family = binomial,
               data = ParasiteCod)
               
summary(B1)


#For proportional data, use the option
#family=binomial or family=quasibinomial. There are two options to fit the model
#
#Option 1: Use as response variable: The proportion between 0 and 1. Use also as
#option: weights=n  where n is number of trials
#
#Option 2:
#
#glm(cbind(Positive,Negatives)~ x + z,family=binomial)
#
#where n = Positives+Negatives


TB <- read.table("Tbdeer.txt", header = TRUE)

TB$DeerNegCervi <- TB$DeerSampledCervi -  TB$DeerPosCervi
B2 <- glm(cbind(DeerPosCervi,DeerNegCervi) ~ OpenLand + ScrubLand,
          family = binomial,
          data = TB)
summary(B2)


#Compare with:

TB$PosProp <- TB$DeerPosCervi / TB$DeerSampledCervi
B3 <- glm(PosProp ~ OpenLand + ScrubLand,
          family = binomial,
          weights = DeerSampledCervi,
          data = TB)
summary(B3)





###########################################################################
#E: Generalised additive modelling
#A GAM in which NAP is fitted as a smoother, exposure as a factor and salinity
#as a continuous explanatory variable, is fitted with the commands:
#
library(mgcv)
M10 <- gam(Richness ~ s(NAP) + factor(exposure) + salinity,
           family = gaussian, 
           data = RIKZ)

#This command applies cross-validation on the NAP smoother. Numerical output
#is obtained by:
#
anova(M10)
summary(M10)

#And the smoothers are obtained by:

plot(M10)
plot(M10, scale = FALSE)



#To fit a model with 4 degrees of freedom, use

M11 <- gam(Richness ~ s(NAP, fx = T, k = 5) + factor(exposure) + salinity,
           family = gaussian,
           data = RIKZ)
AIC(M11)

#The following command works as well:

anova(M10, M11, test = "F")

#And here is some stuff to get AICs:
M12 <- gam(Richness ~ s(NAP) + factor(exposure) + salinity,
           family = gaussian, 
           data = RIKZ)

M12A <- update(M11, . ~ . - s(NAP))
M12B <- update(M11, . ~ . - factor(exposure))
M12C <- update(M11, . ~ . - salinity)

AIC(M12, M12A, M12B, M12C)





###########################################################################
#G: And here is some code for an Information theoretic approach
#Suppose that based on your underlying biological knowledge, you want
#to comapre the following 10 models.

IT1  <- lm(Richness ~ NAP + factor(exposure) + salinity + grainsize, data = RIKZ)
IT2  <- lm(Richness ~ NAP + factor(exposure)                       , data = RIKZ)
IT3  <- lm(Richness ~ NAP + factor(exposure)            + grainsize, data = RIKZ)
IT4  <- lm(Richness ~ NAP * factor(exposure)                       , data = RIKZ)
IT5  <- lm(Richness ~ NAP                                          , data = RIKZ)
IT6  <- lm(Richness ~       factor(exposure) + salinity + grainsize, data = RIKZ)
IT7  <- lm(Richness ~                          salinity + grainsize, data = RIKZ)
IT8  <- lm(Richness ~                          salinity * grainsize, data = RIKZ)
IT9  <- lm(Richness ~ NAP + factor(exposure)            + grainsize, data = RIKZ)
IT10 <- lm(Richness ~       factor(exposure) * grainsize, data = RIKZ)
 
#The code to extract AIC and calculate Akaike weights is as follows
#
AICs <- AIC(IT1, IT2, IT3, IT4, IT5, IT6, IT7, IT8, IT9, IT10)

MyDf <- AICs[,1]
AICsNum <- AICs[,2]
minAW <- min(AICsNum)
Delta <- AICsNum-minAW
RL <- exp(-0.5 * Delta)
wi <- RL / sum(RL)
Z <- data.frame(MyDf, AICsNum, Delta, wi)
Z <- round(Z, digits = 3)
colnames(Z)<- c("Df", "AIC", "AIC differences", "Akaike weights")
Z







