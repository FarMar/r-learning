#    Data exploration, regression, GLM & GAM course
#    Highland Statistics Ltd.
#    www.highstat.com

#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#######################################################################


#These data were taken from:
#Methane fluxes show consistent temperature dependence across 
#microbial to ecosystem scales

#Nature: doi:10.1038/nature13164
#Gabriel Yvon-Durocher, Andrew P. Allen, David Bastviken, Ralf Conrad, 
#Cristian Gudasz,Annick St-Pierre, Nguyen Thanh-Duc & Paul A. del Giorgio

#Methane (CH4) is an important greenhouse gas. 
#Recent calculations suggest that atmospheric CH4 
#emissions have been responsible for approximately 
#20% of Earth’s warming since pre-industrial times	

#Yvon-Duroche et al	(2014) modelled methane flux as a function
#of standardized temperature. Samples were taken from a large (100-150) 
#number of sites. Multiple observations per site were taken.
#To avoid independency problems due to multiple observations
#per site, we take averages per site. To analyse the original data
#mixed effects models are requires.

#Task 1: 
#Model Flux as a function of Temperature and Ecosystem
#Ecosystem has three classes
	
#Task 2 (if interactions have already been explained):
#Model Flux as a function of Temperature, Ecosystem and the interaction
#Ecosystem has three classes
	

#######################################################################
#set the working directory & read the data
#On a Windows computer: adjust blah blah blah
#setwd("c:/blah blah blah")

#For a Mac:
setwd("/Users/far218")


#Import the data from a tab delimited ascii file
Methane <- read.table(file = "Methane.txt",
                   header = TRUE,
                   dec = ".")

#dec = '.'   means that the point is used for decimals. 
#Change to   dec = ","   if required.
########################################################################


########################################################################
#Housekeeping
#Load packages from R and support functions that we wrote

library(lattice)  #For fancy multipanel graphs

#Ensure that the file HighstatLibV9.R is in your working directory
#Ensure it is a .R file and not a .R.txt file!!!!!! 

source("HighstatLibV9.R")
########################################################################




########################################################################
#Inspect the file
names(Methane)
str(Methane)  
# 'data.frame':	127 obs. of  6 variables:
 # $ Area     : Factor w/ 127 levels "Ahvensalo","BASFSpain",..: 1 2 3 4 5 6 7 8 9 10 ...
 # $ Flux     : num  -1.746 1.112 -0.596 1.598 -0.126 ...
 # $ Temp     : num  -2.017 1.082 -0.637 1.865 -0.204 ...
 # $ Ecosystem: Factor w/ 3 levels "Aquatic","RicePaddy",..: 3 2 3 2 3 2 3 1 3 3 ...
 # $ Longitude: num  30.53 -4.71 -84.01 99.97 -93.28 ...
 # $ Latitude : num  65.5 37.7 42.3 15 47.3 ...
########################################################################



########################################################################
#Data exploration

#Outliers
MyVar <- c("Flux", "Temp", "Longitude", "Latitude")
Mydotplot(Methane[, MyVar])
#Some of the plots are still close to each other. Can we argue
#that there is no inherent correlation? I hope so...else see
#you back on a mixed modelling course


#Collinearity
boxplot(Temp ~ Ecosystem, 
        varwidth = TRUE, 
        data = Methane,
        ylab = "Temperature",
        xlab = "Ecosytem",
        cex.lab = 1.5)
#Minor collinearity

#Relationships
plot(y = Methane$Flux,
     x = Methane$Temp,
     xlab = "Standardized temperature",
     ylab = ' Methane flux')

plot(y = Methane$Latitude,
     x = Methane$Longitude,
     xlab = "Longitude",
     ylab = 'Latitude')

boxplot(Flux ~ Ecosystem, data = Methane)     
     
#Interactions     
xyplot(Flux ~ Temp | factor(Ecosystem),
       data = Methane,
       col = 1)     
#Looks like the relationship is similar...whatever the 
#type of ecosystem?

table(Methane$Ecosystem)


#Spatial position of sites
plot(x = Methane$Longitude,
     y = Methane$Latitude)

xyplot(Latitude ~ Longitude, 
       aspect = "iso",
       data = Methane,
       col = 1,
       pch = 16,
       cex = 2)


#If you are online, then use this:
Install <- FALSE #Change to FALSE if you don't want packages installed
toInstall <- c("ggplot2", "ggmap")
if(Install){
	install.packages(toInstall, 
	                 dependencies = TRUE, 
	                 repos = "http://cran.us.r-project.org")
	        }
library(ggplot2)
library(ggmap)

glgmap   <- get_map(location = c(-160, -50, 200, 70),
                     maptype= "terrain",
                     col = "bw")    
p <- ggmap(glgmap)
p <- p + geom_point(aes(Longitude, Latitude),
                    data = Methane) 
p <- p + xlab("Longitude") + ylab("Latitude")  
p
#End of code that requires online access


#You can also do something like this with xyplot
library(maps)
xyplot(Latitude ~ Longitude,
       data = Methane,
       layout = c(1,1),
       aspect = "iso",
       #ylim = c(-40, 40),
       xlab = list(label = "Longitude", cex = 1.5),
       ylab = list(label = "Latitude", cex = 1.5),
        panel = function(x,y) {
          panel.points(x,y, pch = 16, col = 1, cex = 1.5)
          mp <- map(database = "world", plot = FALSE)
          panel.lines(mp$x, mp$y, col = grey(0.5))
        })


xyplot(Latitude ~ Longitude | Ecosystem,
       data = Methane,
       layout = c(1,3),
       aspect = "iso",
       xlab = list(label = "Longitude", cex = 1.5),
       ylab = list(label = "Latitude", cex = 1.5),
        panel = function(x,y) {
          panel.points(x,y, pch = 16, col = 1, cex = 1.5)
          mp <- map(database = "world", plot = FALSE)
          panel.lines(mp$x, mp$y, col = grey(0.5))
        })


#######################################################





########################################################################
#Analysis
#Based on the underlying question:

M0 <- lm(Flux ~ Temp + Ecosystem, data = Methane)
summary(M0)

#Task: Write down the model that you will fit in
#the code below

# Flux_i = alpha + beta_1 * Temp_i + Ecosystem_i + eps_i
# eps_i ~ N(0, sigma^2)

# Flux_i ~ N(mu_i, sigma^2)
# E(Flux_i) = mu_i
# mu_i = alpha + beta_1 * Temp_i + Ecosystem_i 


#Software does this:

#mu_i = alpha + beta_1 * Temp_i + 
#       0      * Ecosystem_A +
#       beta_3 * Ecosystem_R +
#       beta_4 * Ecosystem_W 

#                     Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         0.007839   0.034124   0.230  0.81868    
# Temp                0.882194   0.031088  28.377  < 2e-16 ***
# EcosystemRicePaddy  0.252059   0.077548   3.250  0.00149 ** 
# EcosystemWetland   -0.054635   0.049625  -1.101  0.27306    


#Aquatic
#mu_i = 0.007839 + 0.882194 * Temp_i + 
#       0        * 1 +
#       0.252059 * 0 +
#      -0.054635 * 0 




#RicePaddy
#mu_i = 0.0078 + 0.88 * Temp_i + 
#       0     * 0 +
#       0.25  * 1 +
#      -0.054 * 0 
      = 0.0078 + 0.88 * Temp_i  + 0.25
      = 0.2578 + 0.88 * Temp_i

#Site.....ecosystem is aquatic
#mu_i = 0.0078 + 0.882 * Temp_i + 
#       0    * 1 +
#       0.25 * 0 +
#      -0.54 * 0 

#mu_i = 0.0078 + 0.882 * Temp_i + 0 
  
    
    
#To summarise the whole thing:
#Aquatic:  
#Flux_i = 0.0078 + 0.88 * Temp_i

#RicePaddy
#Flux_i = 0.0078 + 0.88 * Temp_i + 0.252
#        = 0.259 + 0.88 * Temp_i

#Wetland:
#FLux_i = 0.0078 + 0.88 * Temp_i - 0.054
#        =-0.0462 + 0.88 * Temp_i
#################################################




#################################################
#Get one p-value for the Ecosystem variable
drop1(M0, test = "F")

#F_2,123 = 7.56 (p < 0.05)

#Task: What is drop1 doing?
#Convince yourself that the drop1 is 
#doing this:
M0 <- lm(Flux ~ Temp + Ecosystem, data = Methane)
M1 <- lm(Flux ~ Ecosystem, data = Methane)
M2 <- lm(Flux ~ Temp, data = Methane)
anova(M0, M1)  #p-value for Temp
anova(M0, M2)  #p-value for Ecosystem

#Both terms highly significant!
##############################################################




##############################################################
#Model validation

#Homogeneity
par(mfrow = c(1,1))
E0 <- resid(M0)   #better: E0 <- rstandard(M0)
F0 <- fitted(M0)
plot(x = F0,
     y = E0,
     xlab = "Fitted values",
     ylab = "Residuals",
     cex.lab = 1.5,
     col = as.numeric(Methane$Ecosystem))
abline(h = 0, lty = 2)
#Ok-ish

boxplot(E0 ~ Ecosystem, data = Methane)
#Hmmm

#Is the ratio of the largest and smallest variance > 4
tapply(E0, INDEX = Methane$Ecosystem, FUN = var)
#Yes...nearly 10!
#That is trouble. 


#Dependence due to model misfit
plot(x = Methane$Temp,
     y = E0,
     xlab = "Standardized temperature",
     ylab = "Residuals")
abline(h = 0, lty = 2)
#It is not that we have clusters of residuals above 
#and below 0!

#Inherent dependence:
#Plot the spatial position of the sites.
#Use pointsizes proportional to the value 
#of the residuals. Use colours for positive and
#negative residuals. This picture should be 'sky at night'

#Spatial independence?
MyCex <- abs(E0) / max(abs(E0))
MyCol <- E0
MyCol[E0 >= 0 ] <- 1
MyCol[E0 < 0 ]  <- 2

xyplot(Latitude ~ Longitude,
       cex = 10 * sqrt(MyCex),  #This is trial and error
       pch = 1,
       col = MyCol,
       data = Methane,
       main = "Sky at night?")
#Hmmm...despite taking averages per site, it seems 
#that we still have a lot of sites close to each other.
#Maybe these sites follow an altitude gradient.
#And at some places all sites have the same color! 
#So....that may be inherent correlation!
#You could also make a variogram. - SEE MYCORHIZA.TXT EXAMPLE


#Options:
# 1. We are violating the 
#    homogeneity assumption (due to ecosystem) and we have 
#    inherent correlation. Focus on model M0, conclude that
#    both terms are highly significant and write in the paper:
#    Both terms are highly significant but care is needed with the 
#    interpretation as there is evidence of heterogeneity and dependency.
#    The question that a referee may ask is: How severe is the effect
#    of the dependency?

# 2. Make a variogram of the residuals (see Zuur et al. 2009). And 
#    if you are lucky then the variogram shows that there is only 
#    correlation up to a distance of X km. Drop sites from the data 
#    that are seperated less than X km (keep one)..and refit the models.  - SEE MYCORHIZA.TXT EXAMPLE
#    
# 3. Dump the linear regression model. Conclude that a mixed model 
#    or a gls should be fitted. But then you better use the original 
#    data and not the averages per site. See you in the 
#    future on the mixed modelling course. REGRESSION WITH SPATIAL DEPENDENCE 
#
# 4. Try to find a missing covariate that explains the local differences.
########################################################################


# In case you want to go for option 1, below is code that shows the model
# fit.


#Three useful functions:
range(Methane$Temp)
levels(Methane$Ecosystem)
as.numeric(Methane$Ecosystem)

#Create a grid of covariate values
MyData <- expand.grid(Temp = seq(-2.01, 1.98, length = 5),
                      Ecosystem = levels(Methane$Ecosystem))
       
#Predict FLux values from the model on this grid       
P <- predict(M0, newdata = MyData)

#Plot the results
plot(x = Methane$Temp, 
     y = Methane$Flux,
     col = as.numeric(Methane$Ecosystem), 
     pch = 16,
     main = "Little problem",
     xlab = "Standardized Temperature",
     ylab = "Flux")

lines(x = MyData$Temp[MyData$Ecosystem == "Aquatic"], 
      y = P[MyData$Ecosystem == "Aquatic"], 
      lty = 1,
      col = 1)

lines(x = MyData$Temp[MyData$Ecosystem == "RicePaddy"], 
      y = P[MyData$Ecosystem == "RicePaddy"], 
      lty = 1,
      col = 2)

lines(x = MyData$Temp[MyData$Ecosystem == "Wetland"], 
      y = P[MyData$Ecosystem == "Wetland"], 
      lty = 1,
      col = 3)

MyVar.l <- levels(Methane$Ecosystem)
#Add a legend
legend("topleft",
        legend = MyVar.l,
        col = c(1,2,3),
        lty = c(1,1,1),
        lwd = c(1,1,1))

###########################################################
#Or very advanced plotting....but very (!) nice for a paper
#Don't worry if the R code is too difficult.
#DONT NEED THIS - USE GGPLOT.2 BELOW


#Get predicted values and store them in a vector
#Run each time from here
Pred    <- NULL
SE      <- NULL
ID      <- NULL
Temp    <- NULL
for (i in levels(Methane$Ecosystem)){
   MinMaxi <- range(Methane$Temp[Methane$Ecosystem==i])
   MyDatai <- data.frame(Temp=
                  seq(from   = MinMaxi[1],
                      to     = MinMaxi[2],
                      length = 25),
                  Ecosystem = i)
   Pi <- predict(M0, newdata = MyDatai, se = TRUE)
   Pred <- c(Pred, Pi$fit)
   SE   <- c(SE, Pi$se.fit)
   ID   <- c(ID, rep(i, nrow(MyDatai)))
   Temp <- c(Temp, MyDatai$Temp)
}

cbind(Temp, Pred, SE, ID)


xyplot(Flux ~ Temp | Ecosystem,
       data = Methane,
       xlab = list("Standardized temperature", cex = 1.5),
       ylab = list("Methance flux", cex = 1.5),
       layout = c(3,1),   #Modify
       strip = function(bg = 'white', ...)
       strip.default(bg = 'white', ...),
       scales = list(alternating = T,
                     x = list(relation = "same"),
                     y = list(relation = "same")),
       panel=function(x, y, subscripts){
             WhichPanel <- Methane$Ecosystem[subscripts][1]
             xi  <- Temp[ID == WhichPanel]
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

###############################################################





########################################################
#Same graph...but now with ggplot2

library(ggplot2)


P <- predict(M0, newdata = MyData, se = TRUE)

MyData$mu    <- P$fit  #Fitted values

MyData$selow <- P$fit - 2 * P$se.fit  #lower bound
MyData$seup  <- P$fit + 2 * P$se.fit   #lower bound
head(MyData)

p <- ggplot()
p <- p + geom_point(data = Methane, 
                    aes(y = Flux, x = Temp),
                    shape = 16, 
                    size = 3)
p <- p + xlab("Temperature") + ylab("Flux")
p <- p + theme(text = element_text(size=15)) + theme_bw()
p <- p + geom_line(data = MyData, 
                   aes(x = Temp, y = mu), 
                   colour = "black")

p <- p + geom_ribbon(data = MyData, 
                     aes(x = Temp, 
                         ymax = seup, 
                         ymin = selow ),
                     alpha = 0.5)

p <- p + facet_grid(. ~ Ecosystem, scales = "free")
p


##############################################








#Task 2: Interaction (but stop here if interactions have not been)
#        discussed yet!
##############################################################

M4 <- lm(Flux ~ Temp * Ecosystem, 
         data = Methane)

#Or:
#M4 <- lm(Flux ~ Temp + Ecosystem + Temp : Ecosystem, 
#         data = Methane)

# Flux_i = alpha + 
         # beta_1 * Temp +
              # 0 * DummyAquatic +
         # beta_3 * DummyRP +
         # beta_4 * DummyWl +
              # 0 * DummyAquatic * Temp +
         # beta_5 * DummyRP * Temp +
         # beta_6 * DummyWl * Temp




summary(M4)
model.matrix(M4)
#Task: Write down the fitted model. This is a set of
#      three equations; one for each ecosystem

#Aquatic
#E(Flux) = -0.0019 + 1.085 * Temp 

#RicePaddy
#E(Flux) = -0.0019 + 1.085 * Temp + 0.05 -0.031 * Temp
#        = -0.0019 + 0.05 + (1.085 - 0.031) * Temp
#        =  0.0481 + 1.054 * Temp

#Wetland
#E(Flux) = -0.0019 + 1.085 * Temp -0.044 -0.244 * Temp
#        = -something + 0.8something * Temp


#Is the interaction significant?
drop1(M4, test = "F") ### IF LINEAR REGRESSION THEN 'test = "F"'
#What is the drop1 doing?


# Methane$Ecosystem2 <- factor(Methane$Ecosystem,
                            # levels = c("RicePaddy", 
                                       # "Wetland", 
                                       # "Aquatic"))                            
# Methane$Ecosystem
# Methane$Ecosystem2


#So...now what? We have a p-value of 0.015 for the interaction
#If we can assume independence, homogeneity and normality,
#then present the model with the interaction in your paper.
#If independence and homogeneity are questionable...drop the 
#interaction and write your story around M0.


###############################################
#Apply a model validation on the M2 results.
E4 <- resid(M4)
F5 <- fitted(M4)
plot(x = F5,
     y = E4)
abline(h = 0, lty = 2)
#Not much has changed.
#So homogeneity in questionable!

#Homework: Verify that all other graphs look similar as before



#Task: Figure out what we changed in the code below
#Get predicted values and store them in a vector
#Run each time from here
Pred    <- NULL
SE      <- NULL
ID      <- NULL
Temp    <- NULL
for (i in levels(Methane$Ecosystem)){
   MinMaxi <- range(Methane$Temp[Methane$Ecosystem==i])
   MyDatai <- data.frame(Temp=
                  seq(from   = MinMaxi[1],
                      to     = MinMaxi[2],
                      length = 25),
                  Ecosystem = i)
   Pi <- predict(M4, newdata = MyDatai, se = TRUE)
   Pred <- c(Pred, Pi$fit)
   SE   <- c(SE, Pi$se.fit)
   ID   <- c(ID, rep(i, nrow(MyDatai)))
   Temp <- c(Temp, MyDatai$Temp)
}

cbind(Temp, Pred, SE, ID)


xyplot(Flux ~ Temp | Ecosystem,
       data = Methane,
       xlab = list("Standardized temperature", cex = 1.5),
       ylab = list("Methance flux", cex = 1.5),
       layout = c(3,1),   #Modify
       strip = function(bg = 'white', ...)
       strip.default(bg = 'white', ...),
       scales = list(alternating = T,
                     x = list(relation = "same"),
                     y = list(relation = "same")),
       panel=function(x, y, subscripts){
             WhichPanel <- Methane$Ecosystem[subscripts][1]
             xi  <- Temp[ID == WhichPanel]
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






#Here is some fancy code to make a better MyData
library(plyr)
MyData <- ddply(Methane, .(Ecosystem), summarize,
                Temp = seq(min(Temp), max(Temp),length = 10))
MyData



#And here is some ggplot2 code
P <- predict(M4, newdata = MyData, se = TRUE)


#Add fitted values and confidence bands
MyData$mu    <- P$fit  #Fitted values
MyData$selow <- P$fit - 2 * P$se.fit  #lower bound
MyData$seup  <- P$fit + 2 * P$se.fit   #lower bound
head(MyData)

p <- ggplot()
p <- p + geom_point(data = Methane, 
                    aes(y = Flux, x = Temp),
                    shape = 16, 
                    size = 3)
p <- p + xlab("Temperature") + ylab("Flux")
p <- p + theme(text = element_text(size=15)) + theme_bw()
p <- p + geom_line(data = MyData, 
                   aes(x = Temp, y = mu), 
                   colour = "black")

p <- p + geom_ribbon(data = MyData, 
                     aes(x = Temp, 
                         ymax = seup, 
                         ymin = selow ),
                     alpha = 0.5)

p <- p + facet_grid(. ~ Ecosystem, scales = "fixed")
p  




