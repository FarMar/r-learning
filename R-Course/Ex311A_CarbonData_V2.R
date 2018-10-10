#    Data exploration, regression, GLM & GAM course
#    Highland Statistics Ltd.
#    www.highstat.com
   
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.




#######################################################################
#set the working directory & read the data
#On a Windows computer: adjust blah blah blah and remove the #
#setwd("c:/blah blah blah/Data/")
#setwd("c:\\blah blah blah\\Data")


#For a Mac:
setwd("/Users/Highstat/applicat/HighlandStatistics/Courses/Data")

#Import the data from a tab delimited ascii file
CD <- read.table("MycorrhizaV1.txt", 
                 header = TRUE,
                 dec = ".")
#dec = '.'   means that the point is used for decimals. 
#Change to   dec = ","   if required.


names(CD)
str(CD)
###########################################################################



###########################################################################
#Underlying question and task
#The data for this exercise is taken from:
#Mycorrhiza-mediated competition between plants and decomposers drives soil carbon storage
#Colin Averill, Benjamin L. Turner & Adrien C. Finzi
#23 JANUARY 2014 | VOL 505 | NATURE | 543

#Abstract of the paper:
# Soil contains more carbon than the atmosphere and vegetation combined. 
# Understanding the mechanisms controlling the accumulation and stability 
# of soil carbon is critical to predicting the Earth’s future climate. 
# Recent studies suggest that decomposition of soil organic matter is 
# often limited by nitrogen availability to microbes and that plants, 
# via their fungal symbionts, compete directly with free-living decomposers 
# for nitrogen. Ectomycorrhizal and ericoid mycorrhizal (EEM) fungi 
# produce nitrogen-degrading enzymes, allowing them greater access to 
# organic nitrogen sources than arbuscular mycorrhizal (AM) fungi. 
# This leads to the theoretical prediction that soil carbon storage 
# is greater in ecosystems dominated by EEM fungi than in those 
# dominated by AM fungi. Using global data sets, we show that soil 
# in ecosystems dominated by EEM-associated plants contains 70% more 
# carbon per unit nitrogen than soil in ecosystems dominated by 
# AM-associated plants. The effect of mycorrhizal type on soil carbon 
# is independent of, and of far larger consequence than, the effects 
# of net primary production, temperature, precipitation and 
# soil clay content. Hence the effect of mycorrhizal type on soil 
# carbon content holds at the global scale. This finding links the 
# func-tional traits of mycorrhizal fungi to carbon storage at 
# ecosystem- to-global scales, suggesting that plant–decomposer 
# competition for nutrients exerts a fundamental control over the 
# terrestrial carbon cycle.

#Analysis in paper:
#"We used a mixed effects model to test the hypothesis that 
#ecosystems dominated by EEM fungi (EEM ecosystems) store 
#significantly more soil C than do ecosystems dominated by 
#AM fungi (AM ecosystems) after accounting for variation 
#in soil N and other drivers of soil C storage.

#Here: Linear regression (will explain later why)

# The data:
# 227 observations
# Response variable: Carbin storage at 1 m depth
# Covariates and model: Nitrogen1m * EM (yes or no) + Clay + NetPrimProduction + 
#                       Temperature + Precipitation + Biome

#In case you are not familiar with the word Biome:
# Wikipedea: Biomes are climatically and geographically defined 
# as contiguous areas with similar climatic conditions on the 
# Earth, such as communities of plants, animals, and soil 
# organisms, and are often referred to as ecosystems.
###########################################################################





###########################################################################
#Load support files and packages
source(file = "/Users/Highstat/applicat/HighlandStatistics/Courses/Data/HighstatLibV9.R")
library(lme4)
library(lattice)
###########################################################################



###########################################################################
###########################################################################
###########################################################################
#Data exploration

###########################################################################
#Outliers

MyVar<- c("Latitude", "Longitude" ,  "Carbon1m",         
          "Nitrogen1m", "Clay",  "EM", "NetPrimProduction",
          "Temperature", "Precipitation")                       
Mydotplot(CD[,MyVar])
#Nothing jumps out


#Collinearity 
MyVar<- c("Nitrogen1m", "Clay",  "EM", "NetPrimProduction",
          "Temperature", "Precipitation")

Mypairs(CD[,MyVar])
corvif(CD[,MyVar])
#Dump temperature?

par(mfrow = c(2,3))
boxplot(Nitrogen1m ~ EM, data = CD, main = "Nitrogen1m")
boxplot(Clay ~ EM, data = CD, main = "Clay")
boxplot(NetPrimProduction ~ EM, data = CD, main = "NetPrimProduction")
boxplot(Temperature ~ EM, data = CD, main = "Temperature")
boxplot(Precipitation ~ EM, data = CD, main = "Precipitation")
#But VIFS were < 5

par(mfrow = c(2,3))
boxplot(Nitrogen1m ~ Biome, data = CD, main = "Nitrogen1m")
boxplot(Clay ~ Biome, data = CD, main = "Clay")
boxplot(NetPrimProduction ~ Biome, data = CD, main = "NetPrimProduction")
boxplot(Temperature ~ Biome, data = CD, main = "Temperature")
boxplot(Precipitation ~ Biome, data = CD, main = "Precipitation")
table(CD$Biome) #So..we mainly have the last two classes 

#Biome is collinear with nearly eveything, especially if you look at the last two groups.
#Dump Biome



#Where in the world are the sites?
library(lattice)
library(maps)

xyplot(Latitude ~ Longitude | EM,
       data = CD,
       layout = c(1,2),
       aspect = "iso",
       xlab = list(label = "Longitude", cex = 1.5),
       ylab = list(label = "Latitude", cex = 1.5),
        panel = function(x,y) {
          panel.points(x,y, pch = 16, col = 1, cex = 1.5)
          mp <- map(database = "world", plot = FALSE)
          panel.lines(mp$x, mp$y, col = grey(0.5))
        })
#So..a EM effect could be a East-US effect         
        
        
#Relationships        
MyVar<- c("Latitude", "Longitude",         
          "Nitrogen1m", "Clay",  "NetPrimProduction",
          "Temperature", "Precipitation")
        
Myxyplot(CD, MyVar, "Carbon1m")
      
#And visualize the interaction we are after
xyplot(Carbon1m ~ Nitrogen1m | factor(EM),
       data = CD,
       panel = function(x,y){
       	panel.points(x,y, col = 1, pch = 16)
       	Mi <- lm(y ~ x)
       	panel.abline(Mi, lwd = 3, col = 1)
       })
#Ranges differ. But it seems that the slope also differs


#Carbon strictly positive?
sort(CD$Carbon1m) #Yes
###############################################################






###############################################################
#Analysis
#Start with a linear regression model
CD$fEM <- factor(CD$EM)
M1 <- lm(Carbon1m ~  fEM * Nitrogen1m  + Clay + NetPrimProduction + Temperature + Precipitation,
         data = CD)
summary(M1)

# Coefficients:
                    # Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        4.3553742  3.0840600   1.412   0.1593    
# fEM1              -4.0998136  2.8398948  -1.444   0.1503    
# Nitrogen1m        13.8675891  1.7689128   7.840 1.95e-13 ***
# Clay              -0.1300262  0.0533075  -2.439   0.0155 *  
# NetPrimProduction -0.0024015  0.0029880  -0.804   0.4224    
# Temperature       -0.0578449  0.1570044  -0.368   0.7129    
# Precipitation      0.0010403  0.0008135   1.279   0.2023    
# fEM1:Nitrogen1m    9.1346247  2.0082124   4.549 8.94e-06 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 11.01 on 219 degrees of freedom
# Multiple R-squared:  0.7791,	Adjusted R-squared:  0.7721 
# F-statistic: 110.4 on 7 and 219 DF,  p-value: < 2.2e-16

#Conclusion: Significant interaction and a weakly significant clay effect


#Do we want to do model selection?
#step(M1)   

#Model validation
E1 <- rstandard(M1)
F1 <- fitted(M1)
plot(x = F1, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, v = 0, lty = 2)     

#Negative fitted values?
sort(F1) #Bugger

#Heterogeneity and patterns??
plot(x = CD$Nitrogen1m,
     y = E1)
abline(h = 0)
#Ok-ish?

#If you are familiar with smoothing
#library(mgcv)
#t1 <- gam(E1 ~ s(Nitrogen1m), data = CD)
#summary(t1)
#plot(t1)


plot(x = CD$Clay,
     y = E1)
abline(h = 0)   
#Heterogeneity?

plot(x = CD$Temperature,
     y = E1)
abline(h = 0)     
######################################################



#Check spatial independence 
# The 'Check spatial independence' part is outside the scope of 
# the course and will not be discussed.
# Google variogram if you want to know what this is.

library(gstat)
library(sp)
library(rgdal)

#Transfer Longitude and Latitude to UTM coordinates
CD$x<-CD$Longitude
CD$y<-CD$Latitude
coordinates(CD) <- c("x", "y")
proj4string(CD) <- CRS("+proj=longlat +datum=WGS84")
latlong<-as.data.frame(spTransform(CD, CRS("+init=epsg:28992"))) 

#Datum = gps coordinate system you are using
#epsg = xy coordinate system you want your xy values to be in 
           # Rijksdriehoek new (Amersfoort new)= 28992
           # WGS 84 = 4326

#Convert from meters to km
latlong$xkm <- latlong$x / 1000
latlong$ykm <- latlong$y / 1000

#And finally make the variogram
mydata <- data.frame(E1, latlong$xkm, latlong$ykm)
coordinates(mydata) <- c("latlong.xkm", "latlong.ykm")
V1 <- variogram(E1 ~ 1, mydata, cutoff = 50)
plot(V1)
# If this is a horizontal band of points then there is no correlation
# In this case there is no correlation within 50 km.
# But change the 50 to 200 km...then there is clearly correlation


#End of 'Check spatial independence' part.
####################################################################





#Informal way to check spatial independence (will be discussed in
#the course)
plot(x = jitter(CD$Longitude),
     y = jitter(CD$Latitude),
     pch = 1,
     cex = 4 * sqrt(abs(E1) /max(abs(E1))),
     col = as.numeric(E1>1) + 1) #<---red for positive residuals, black for negative residuals
#This should be sky at night..no pattenrs
#I think there is spatial correlation
      
      
      
#####################################################################
#Model interpretation
#Here are the results:
summary(M1)

# Carbon1m_i ~ N(mu_i , sigma^2)
# E(Carbon1m_i) = mu_i

#For EM = 0: 
# mu_i = 4.35 + 13.86 * Nitrogen1m_i - 0.13 * Clay_i + 
#        -0.0024 * NetPrimProduction_i + more rubbish

#For EM = 1: 
# mu_i = 4.35 -4.09 + (13.86 + 9.13) * Nitrogen1m_i - 0.13 * Clay_i + 
#        -0.0024 * NetPrimProduction_i + more rubbish

#Let's sketch the results.
#Plot fitted values versus Nitrogen1m for EM = 0 and EM = 1.
#Use average CLay, NetPrimProduction, Temperature and Precipitation values

#What is the range of Nitrogen1m per EM level?
tapply(CD$Nitrogen1m, INDEX = CD$fEM, FUN = range)
# $`0`
# [1] 0.291240 3.112524

# $`1`
# [1] 0.145359 6.947740


MyData0 <- data.frame(Nitrogen1m        = seq(0.29, 3.11, length = 25),
                      fEM               = "0",
                      Clay              = mean(CD$Clay),
                      NetPrimProduction = mean(CD$NetPrimProduction),
                      Temperature       = mean(CD$Temperature),
                      Precipitation     = mean(CD$Precipitation)
                      )
P0 <- predict(M1, newdata = MyData0, se = TRUE)

MyData1 <- data.frame(Nitrogen1m        = seq(0.14, 6.95, length = 25),
                      fEM               = "1",
                      Clay              = mean(CD$Clay),
                      NetPrimProduction = mean(CD$NetPrimProduction),
                      Temperature       = mean(CD$Temperature),
                      Precipitation     = mean(CD$Precipitation)
                      )
P1 <- predict(M1, newdata = MyData1, se = TRUE)

#And plot the two lines
plot(x = CD$Nitrogen1m,
     y = CD$Carbon1m,
     col = CD$EM + 1,
     pch = 16,
     cex = 0.5)
     
lines(x = MyData0$Nitrogen1m,
      y = P0$fit, 
      col = 1,
      lwd = 3)    
       
xi <- MyData0$Nitrogen1m       
yi <- P0$fit       
sei <- P0$se.fit
polygon(c(xi, rev(xi)),
        c(yi - 1.96 * sei, rev(yi + 1.96 * sei)),
        col = grey(0.5), 
        border = NULL,
        density = 50)

lines(x = MyData1$Nitrogen1m,
      y = P1$fit, 
      col = 2,
      lwd = 3)     

xi <- MyData1$Nitrogen1m       
yi <- P1$fit       
sei <- P1$se.fit
polygon(c(xi, rev(xi)),
        c(yi - 1.96 * sei, rev(yi + 1.96 * sei)),
        col = "red", 
        border = NULL,
        density = 50)
       
#Claim paper:
#"We found that EEM ecosystems store 1.7 times more C 
#per unit of soil N than do AM ecosystems"

#So..if EM is 1, then 1.7 times more Carbon??        
#No idea where this is coming from.

#####################################################################








#####################################################################
#Conclusions
#1. We have negative fitted values. Ignore or fix?
#   We could apply a gamma GLM
#2. There may be spatial correlation. Is this due to
#   a missing covariate or inherent correlation?
Loc  <- cbind(CD$Longitude, CD$Latitude)
Distances <- dist(Loc)  #Euclidean distances      
hist(as.numeric(Distances))
#These are distances between points
hist(as.numeric(Distances), breaks = 50)
      
#Solution: 1. apply a GLS
#          2. Define a variable 'Location' with multiple observations
#             per Location. Use this as a random intercept in a mixed model
#             But that is a different course

#3. We have small heterogeneity

#It is a Nature paper!






#######################################################################
#For later this week:
#And this is how you fit a gamma GLM
M2 <- glm(Carbon1m ~  fEM * Nitrogen1m  + Clay + NetPrimProduction + 
                      Temperature + Precipitation,
         data = CD,
         family = Gamma(link = "log"))
summary(M2)
E2 <- resid(M2, type = "pearson")
F2 <- fitted(M2)
plot(x = F2,
     y = E2)         

plot(x = F2,
     y = CD$Carbon1m,
     xlim = c(0, 500),
     ylim = c(0, 500))
#Hmmm...that doesn;t seem the correct approach neither.

#Probably a sqrt or log transformation on Carbon1m, 
#followed by a linear regression will be better? 

#Or apply a Tobit model using JAGS.

     
      