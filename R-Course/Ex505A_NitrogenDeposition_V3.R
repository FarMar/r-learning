#    Data exploration, regression, GLM & GAM course
#    Highland Statistics Ltd.
#    www.highstat.com

#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#######################################################################




#######################################################################
#These data were taken from:
# Nitrogen deposition is negatively related to species richness and 
# species composition of vascular plants and bryophytes in Swiss 
# mountain grassland
# Tobias Roth, Lukas Kohli, Beat Rihm, Beat Achermann
# Agriculture, Ecosystems and Environment 178 (2013) 121–126


#First part of the abstract:
#Nitrogen (N) deposition is a major threat to biodiversity of many 
#habitats in the lowlands. In mountain habitats, however, the effect 
#of N deposition on biodiversity is not well understood. Here, data 
#from the biodiversity monitoring of Switzerland were used to investigate 
#whether high N deposition is negatively related to species richness and c
#ommunity uniqueness of vascular plants and bryophytes in mountain grassland. 
#The total species diversity, as well as the diversity of three subsets 
#of species (i.e. oligotrophic species, eutrophic species and targeted 
#grassland species according to conservation objectives of the Swiss authorities) 
#were analyzed.
#######################################################################








#######################################################################
#Task1: Analysis of continuous data
# Model Z12_Mo (moss diversity index) as a function of:
# NitrogenDep	
# Altitude	
# Inclination	
# Precipitation	
# CACO3	
# Humidity	
# Light	
# Exposition

#Interpretation of the index:
# A value close to one would indicate a plot with a high proportion of 
# unique species (i.e. a high proportion of rare species);
# a value close to zero would indicate a high proportion of common species.

#Do we expect a non-linear N effect? Possibly


#Task 2: Analysis of count data
# Analyse Z9 (total plant species richness)
# Same covariates

#Task 3: Analysis of count data
#Analyse Oligo (plant species richness of oligotrophic species)
# Same covariates


#Task 1 will be regression/additive model
#Tasks 2 & 3 will be GLM (Poisson, NB) or GAM
########################################################################







#######################################################################
#set the working directory & read the data
#On a Windows computer: adjust blah blah blah
#setwd("c:/blah blah blah")

#For a Mac:
setwd("/Users/Highstat/applicat/HighlandStatistics/Courses/Data")

#Import the data from a tab delimited ascii file
SMD <- read.table(file = "NitrogenDeposition.txt",
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
source("/Users/Highstat/applicat/HighlandStatistics/Courses/Data/HighstatLibV9.R")
########################################################################







########################################################################
#Inspect the file
names(SMD)
str(SMD)  
# 'data.frame':	122 obs. of  19 variables:
 # $ SiteID       : int  821178 809186 761154 629118 629114 785154 
 # $ NitrogenDep  : num  6.95 7.01 7.01 7.72 8.6 ...
 # $ Altitude     : int  1786 2166 2246 1641 1419 1796 1482 1736 1422 
 # $ Inclination  : num  13 19.7 11.4 34.7 23.1 ...
 # $ Precipitation: num  771 977 1220 906 681 ...
 # $ CACO3        : int  3 2 4 1 3 1 4 2 1 4 ...
 # $ Humidity     : num  2.87 2.73 3.23 2.58 2.63 ...
 # $ Light        : num  3.69 3.94 4.16 3.66 3.72 ...
 # $ Exposition   : int  257 167 330 229 290 107 357 42 249 164 ...
 # $ Z9           : int  36 38 59 47 51 55 58 56 58 47 ...
 # $ Oligo        : int  5 21 38 13 18 22 20 20 13 21 ...
 # $ Eutro        : int  8 3 4 12 11 12 16 13 18 6 ...
 # $ UZL          : int  10 15 15 20 22 25 21 22 19 21 ...
 # $ Z9_Mo        : int  5 7 24 9 22 3 7 12 9 15 ...
 # $ Oligo_Mo     : int  0 2 9 1 3 0 1 3 1 2 ...
 # $ Eutro_Mo     : int  1 0 0 0 2 0 0 1 3 0 ...
 # $ UZL_Mo       : int  1 1 0 0 1 0 0 1 1 2 ...
 # $ Z12          : num  0.631 0.732 0.837 0.64 0.657 ...
 # $ Z12_Mo       : num  0.939 0.922 0.865 0.933 0.749 ...
 

########################################################################

#Dump the rubbish and keep the relevant variables
MyVar <- c("NitrogenDep",   "Altitude",      "Inclination",     
           "Precipitation", "CACO3",         "Humidity",
           "Light",         "Exposition",   "Z12_Mo")      
SMD2 <- SMD[, MyVar]
dim(SMD)
dim(SMD2)



########################################################################
#Data exploration
# Missing values
# Outliers
# Collinearity
# Relationships


#Missing values?
is.na(SMD2)
colSums(is.na(SMD2))

#Option 1: Dump the rows with NA in the response!
#Option 2: Dump  "Exposition" due to 6 NAs

#We will adopt option 2
MyVar <- c("NitrogenDep",   "Altitude",      "Inclination",     
           "Precipitation", "CACO3",         "Humidity",
           "Light",         "Z12_Mo")      
SMD3 <- SMD2[, MyVar]
dim(SMD2)
dim(SMD3)

#Still 3 NA in the respponse
SMD4 <- na.exclude(SMD3)
dim(SMD4)
#119 observations


#Outliers
MyVar <- c("NitrogenDep",   "Altitude",      "Inclination",     
           "Precipitation", "CACO3",         "Humidity",
           "Light",         "Z12_Mo")      

Mydotplot(SMD4[, MyVar])
#OK


#Collinearity
MyVar <- c("NitrogenDep",   "Altitude",      "Inclination",     
           "Precipitation", "CACO3",         "Humidity",
           "Light") 

Mypairs(SMD4[, MyVar])
corvif(SMD4[, MyVar])

#Oeps..... dump altitude??
MyVar <- c("NitrogenDep",                    "Inclination",     
           "Precipitation", "CACO3",         "Humidity",
           "Light")      
corvif(SMD4[, MyVar])
#All ok now




#Relationships
MyVar <- c("NitrogenDep",   "Altitude",      "Inclination",     
           "Precipitation", "CACO3",         "Humidity",
           "Light")     

Myxyplot(SMD4, MyVar, "Z12_Mo")
#######################################################


#There is one observation with a large N value.
#I'm going to delete it.
SMD5 <- SMD4[SMD4$NitrogenDep < 35, ]
dim(SMD4)
dim(SMD5)

#######################################################
#Analysis.
#Continuous response ==> Multiple linear regression?
#                         between 0-1: beta regression?

#Would you expect a non-linear N effect?
#If so..start with a GAM?

#Hence, these are the two competing models
M1 <- lm(Z12_Mo ~ NitrogenDep + Inclination +
                  Precipitation + CACO3 + Humidity + Light, 
         data = SMD5)

M2 <- gam(Z12_Mo  ~ s(NitrogenDep) + Inclination +
                    Precipitation + CACO3 + Humidity + Light, 
           data = SMD5)

AIC(M1, M2)
#The GAM is better.



#Model validation
E2 <- resid(M2)
F2 <- fitted(M2)

#Plot resudals vs fitted values
plot(x = F2,
     y = E2)
abline(h = 0)     
#Ok..and no fitted values outside 0-1 range
#Homogeneity looks ok


#Plot residuals vs each covariate in the model
SMD5$E2 <- resid(M2)
Myxyplot(SMD5, MyVar, "E2")
#Looks ok.



#Model interpretation
summary(M2)
#It may be an option to do some further
#model selection
 
#Plot the smoother
par(mfrow = c(1,1))
plot(M2)



#But the main problem with the whole analysis is this:
#Use altitude
M4 <- gam(Z12_Mo  ~ s(Altitude) + Inclination +
                    Precipitation + CACO3 + Humidity + Light, 
           data = SMD5)
summary(M4)
AIC(M1, M2, M3, M4)

par(mfrow = c(1,1))
plot(M4)




#######################################################
#Alternative analysis
#These smoothers allow for 0 df...which means that
#you can remove all smoothers with 0 df at once.
M5 <- gam(Z12_Mo  ~ s(NitrogenDep, bs = "cs") + 
                    s(Inclination, bs = "cs") +
                    s(Precipitation, bs = "cs") + 
                    CACO3 + 
                    s(Humidity, bs = "cs") + 
                    s(Light, bs = "cs"), 
           data = SMD5)
summary(M5)
#Remove all the rubbish


M5B <- gam(Z12_Mo  ~ s(NitrogenDep, bs = "cs") + 
                     CACO3 + 
                     s(Humidity, bs = "cs"), 
           data = SMD5)                  
summary(M5B)
par(mfrow = c(1,2))
plot(M5B)

M5C <- gam(Z12_Mo  ~ s(NitrogenDep, bs = "cs") + 
                     s(Humidity, bs = "cs"), 
           data = SMD5)                  
summary(M5C)
par(mfrow = c(1,2))
plot(M5C)
#I would stick to only a NitrogenDep smoother


# END OF FIRST PART
#################################################################
#################################################################
#################################################################









#################################################################
#################################################################
#################################################################
#Task two: Analyse Z9, the total plant species richness
#Remove all variables
rm(list = ls())

#Set the working directory
setwd("/Users/Highstat/applicat/HighlandStatistics/Courses/NewDataSets/Roth")

#Import the data from a tab delimited ascii file
SMD <- read.table(file = "NitrogenDeposition.txt",
                   header = TRUE,
                   dec = ".")

#dec = '.'   means that the point is used for decimals. 
#Change to   dec = ","   if required.
########################################################################


########################################################################
#Housekeeping
#Load packages from R and support functions that we wrote

library(lattice)  #For fancy multipanel graphs

#Ensure that the file HighstatLibV6.R is in your working directory
#Ensure it is a .R file and not a .R.txt file!!!!!! 

source("/Users/Highstat/applicat/HighlandStatistics/Courses/Data/HighstatLibV9.R")
########################################################################




########################################################################
#Inspect the file
names(SMD)
str(SMD)  
########################################################################

#Dump the rubbish and keep the relevant variables
MyVar <- c("NitrogenDep",   "Altitude",      "Inclination",     
           "Precipitation", "CACO3",         "Humidity",
           "Light",         "Exposition",   "Z9")      
SMD2 <- SMD[, MyVar]
dim(SMD)
dim(SMD2)

########################################################################
#Data exploration
# Missing values
# Outliers
# Collinearity
# Relationships


#Missing values?
is.na(SMD2)
colSums(is.na(SMD2))
#Dump the rows with NA in the response!
#Dump    "Exposition" due to 6 NAs

MyVar <- c("NitrogenDep",   "Altitude",      "Inclination",     
           "Precipitation", "CACO3",         "Humidity",
           "Light",         "Z9")      
SMD3 <- SMD2[, MyVar]
dim(SMD2)
dim(SMD3)

#There are no NA's elsewhere....
SMD4 <- na.exclude(SMD3)
dim(SMD4)




#Outliers
MyVar <- c("NitrogenDep",   "Altitude",      "Inclination",     
           "Precipitation", "CACO3",         "Humidity",
           "Light",         "Z9")      

Mydotplot(SMD4[, MyVar])
#OK


#Collinearity
MyVar <- c("NitrogenDep",   "Altitude",      "Inclination",     
           "Precipitation", "CACO3",         "Humidity",
           "Light")  #,   "Z12_Mo")      

Mypairs(SMD4[, MyVar])
corvif(SMD4[, MyVar])

#Oeps..... dump altitude??
MyVar <- c("NitrogenDep",                    "Inclination",     
           "Precipitation", "CACO3",         "Humidity",
           "Light")      
corvif(SMD4[, MyVar])
#All ok now




#Relationships
MyVar <- c("NitrogenDep",   "Altitude",      "Inclination",     
           "Precipitation", "CACO3",         "Humidity",
           "Light")     

Myxyplot(SMD4, MyVar, "Z9")
#######################################################


#Dump the observation with the laste Nitrogen value
SMD5 <- SMD4[SMD4$NitrogenDep < 35, ]
dim(SMD4)
dim(SMD5)



#Apply Poisson GLM
M1 <- glm(Z9 ~ NitrogenDep + Inclination +
                  Precipitation + CACO3 + Humidity + Light, 
         data = SMD5,
         family = poisson)
summary(M1)

#Check for overdispersion
E1 <- resid(M1, type = "pearson")
N <- nrow(SMD5)
p <- length(coef(M1))
sum(E1^2) / (N - p)
#1.85


#Why do we have overdispersion?
F1 <- fitted(M1)
plot(x = F1,
     y = E1)
abline(h = 0)     
#Small cone effect....could mean that 
#we need a NB GLM


#Plot residuals vs each covariate
SMD5$E1 <- E1
Myxyplot(SMD5, MyVar, "E1")
#Looks ok!



#But based on biology this could be a starting point as well
M3 <- gam(Z9  ~ s(NitrogenDep) + Inclination +
                    Precipitation + CACO3 + Humidity + Light, 
           data = SMD5,
           family = poisson)
summary(M3)
#This is a GLM!

#Overdispersion
E3 <- resid(M3, type = "pearson")
sum(E3^2) / M3$df.res 
#M3$df.res is equal to N - p
#


#Fit a NB GLM 
library(MASS)
M4 <- glm.nb(Z9 ~ NitrogenDep + Inclination +
                  Precipitation + CACO3 + Humidity + Light, 
         data = SMD5)
summary(M4)

#Check for overdispersion
E4 <- resid(M4, type = "pearson")
N <- nrow(SMD5)
p <- length(coef(M4))
sum(E4^2) / (N - p)
#Perfect
summary(M4)
step(M4)
#################################################################
#################################################################
#################################################################







#################################################################
#################################################################
#################################################################
#Task three: Analyse Oligo, the total plant species richness
#Remove all variables
rm(list = ls())

setwd("/Users/Highstat/applicat/HighlandStatistics/Courses/NewDataSets/Roth")

#Import the data from a tab delimited ascii file
SMD <- read.table(file = "NitrogenDeposition.txt",
                   header = TRUE,
                   dec = ".")

#dec = '.'   means that the point is used for decimals. 
#Change to   dec = ","   if required.
########################################################################


########################################################################
#Housekeeping
#Load packages from R and support functions that we wrote

library(lattice)  #For fancy multipanel graphs

#Ensure that the file HighstatLibV6.R is in your working directory
#Ensure it is a .R file and not a .R.txt file!!!!!! 

source("/Users/Highstat/applicat/HighlandStatistics/Courses/Data/HighstatLibV9.R")
########################################################################





########################################################################
#Inspect the file
names(SMD)
str(SMD)  
########################################################################

#Dump the rubbish and keep the relevant variables
MyVar <- c("NitrogenDep",   "Altitude",      "Inclination",     
           "Precipitation", "CACO3",         "Humidity",
           "Light",         "Exposition",   "Oligo")      
SMD2 <- SMD[, MyVar]
dim(SMD)
dim(SMD2)

########################################################################
#Data exploration
# Missing values
# Outliers
# Collinearity
# Relationships


#Missing values?
is.na(SMD2)
colSums(is.na(SMD2))
#Dump the rows with NA in the response!
#Dump    "Exposition" due to 6 NAs

MyVar <- c("NitrogenDep",   "Altitude",      "Inclination",     
           "Precipitation", "CACO3",         "Humidity",
           "Light",         "Oligo")      
SMD3 <- SMD2[, MyVar]
dim(SMD2)
dim(SMD3)

SMD4 <- na.exclude(SMD3)
dim(SMD4)




#Outliers
MyVar <- c("NitrogenDep",   "Altitude",      "Inclination",     
           "Precipitation", "CACO3",         "Humidity",
           "Light",         "Oligo")      

Mydotplot(SMD4[, MyVar])
#OK


#Collinearity
MyVar <- c("NitrogenDep",   "Altitude",      "Inclination",     
           "Precipitation", "CACO3",         "Humidity",
           "Light")        

Mypairs(SMD4[, MyVar])
corvif(SMD4[, MyVar])

#Oeps..... dump altitude??
MyVar <- c("NitrogenDep",                    "Inclination",     
           "Precipitation", "CACO3",         "Humidity",
           "Light")      
corvif(SMD4[, MyVar])
#All ok now




#Relationships
MyVar <- c("NitrogenDep",   "Altitude",      "Inclination",     
           "Precipitation", "CACO3",         "Humidity",
           "Light")     

Myxyplot(SMD4, MyVar, "Oligo")
#######################################################


#Dump the observation with the laste Nitrogen value
SMD5 <- SMD4[SMD4$NitrogenDep < 35, ]
dim(SMD4)
dim(SMD5)



#Apply Poisson GLM
M1 <- glm(Oligo ~ NitrogenDep + Inclination +
                  Precipitation + CACO3 + Humidity + Light, 
         data = SMD5,
         family = poisson)
summary(M1)

#Check for overdispersion
E1 <- resid(M1, type = "pearson")
N <- nrow(SMD5)
p <- length(coef(M1))
sum(E1^2) / (N - p)
#4.14


#Why do we have overdispersion?
F1 <- fitted(M1)
plot(x = F1,
     y = E1)
abline(h = 0)     
#OK


#Plot residuals vs each covariate
SMD5$E1 <- E1
Myxyplot(SMD5, MyVar, "E1")
#Not sure



#Based on biology this could be a starting point as well
M3 <- gam(Oligo  ~ s(NitrogenDep) + Inclination +
                    Precipitation + CACO3 + Humidity + Light, 
           data = SMD5,
           family = poisson)
summary(M3)

#Overdispersion
E3 <- resid(M3, type = "pearson")
sum(E3^2) / M3$df.res 
#M3$df.res is equal to N - p
#Overdispersion

plot(M3)
#

#NB GAM
M4 <- gam(Oligo  ~ s(NitrogenDep) + Inclination +
                    Precipitation + CACO3 + Humidity + Light, 
           data = SMD5,
           family = negbin(c(2,5),link = log))
summary(M4)
plot(M4)
#This is a GAM!

#Overdispersion
E4 <- resid(M4, type = "pearson")
sum(E4^2) / M4$df.res 
#That is ok


#1. Now write down the fitted model. 
#2. Maybe you want to further simplify the model?
#3. Investigate what the altitude effect is.
#4. Which model is better...with NitrogenDep or with altitude?



