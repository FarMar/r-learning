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
setwd("c:/blah blah blah")

#For a Mac:
setwd("/Users/far218/Dropbox/R Course/")


#Import the data from a tab delimited ascii file
Fish <- read.table(file = "Baileyetal2008.txt",
                   header = TRUE,
                   dec = ".")

#dec = '.'   means that the point is used for decimals. 
#Change to   dec = ","   if required.
########################################################################


########################################################################
#Housekeeping
#Load packages from R and support functions that we wrote

library(lattice)  #For fancy multipanel graphs
source("HighstatLibV9.R")

#Ensure that the file HighstatLibV9.R is in your working directory
#Ensure it is a .R file and not a .R.txt file!!!!!! 
########################################################################




########################################################################
#Inspect the file
names(Fish)
str(Fish)  #Make sure you have num and not factors for the numerical variables!
# 'data.frame':	148 obs. of  9 variables:
 # $ Site     : int  1 2 3 4 5 6 7 8 9 10 ...
 # $ TotAbund : int  76 161 39 410 177 695 352 674 624 736 ...
 # $ Dens     : num  0.00207 0.00352 0.000981 0.008039 0.005933 ...
 # $ MeanDepth: int  804 808 809 848 853 960 977 982 985 986 ...
 # $ Year     : int  1978 2001 2001 1979 2002 1980 1981 1979 1982 1980 ...
 # $ Period   : int  1 2 2 1 2 1 1 1 1 1 ...
 # $ Xkm      : num  98.8 76.8 103.8 91.5 107.1 ...
 # $ Ykm      : num  -57.5 178.6 -50.1 146.4 -37.1 ...
 # $ SweptArea: num  36710 45741 39775 51000 29831 ...
########################################################################




#######################################################################
# Underlying question: Has the Density - Depth relationship changed
# over time?
# Model Dens as a function of Meandepth, Year (or: Period)
# Take into account of Xkm, Ykm
#######################################################################





########################################################################
#Data exploration
#A Outliers in Y / Outliers in X
#B Collinearity X
#C Relationships Y vs X
#D Spatial/temporal aspects of sampling design (not relevant here)
#E Interactions (is the quality of the data good enough to include them?)
#F Zero inflation Y
#G Are categorical covariates balanced?


##############################################
#A Outliers

MyVar <- c("Dens", "MeanDepth", "Year", "Xkm", "Ykm")
Mydotplot(Fish[, MyVar])

#Why we don't like boxplots....
par(mfrow = c(1, 2))
boxplot(Fish$Dens)
dotchart(Fish$Dens)


#Is there an outlier in the spatial sampling positions?
xyplot(Ykm ~ Xkm, 
       aspect = "iso", 
       data = Fish,
       col = 1,
       pch = 16)

xyplot(Ykm ~ Xkm, 
       aspect = "iso", 
       data = Fish,
       col = Fish$Period, #1 = black, 2 = red
       pch = 16)
#This is also a little bit point E...


#Identify the outlier
#Copy and paste from here...
par(mfrow = c(1, 1))
plot(x = Fish$Xkm, 
     y = Fish$Ykm)
identify(x = Fish$Xkm, 
         y = Fish$Ykm)
#..to here. Click close to a point
#Press ESCAPE to quit


#Are the missing values?
sum(is.na(Fish$Xkm))
colSums(is.na(Fish))


#Remove missing value:
#Option 1:
Fish2 <- na.exclude(Fish) #Be careful! Every row where there is somewhere an NA will be removed!

#Option 2:
I1 <- is.na(Fish$Xkm) | is.na(Fish$Ykm)  
# |   or   
# &   and   
# ==  equals
# !=  not equal

I1
Fish2 <- Fish[!I1, ]
dim(Fish)
dim(Fish2)

#Just double checking the ID of the outlier
plot(x = Fish2$Xkm, 
     y = Fish2$Ykm)
identify(x = Fish2$Xkm, 
         y = Fish2$Ykm)
#Oeps..

#Remove the spatial outlier
Fish3 <- Fish2[c(-135), ]
dim(Fish3)
##############################################




##############################################
#B. Collinearity

pairs(Fish3[,MyVar], 
      lower.panel = panel.cor)

boxplot(MeanDepth ~ Period, 
        data = Fish3)
##############################################




##############################################
#C. Relationships between Density and MeanDepth
plot(x = Fish3$MeanDepth, 
     y = Fish3$Dens,
     col = Fish3$Period,
     pch = 16)

boxplot(Dens ~ Period, data = Fish3)
##############################################
         


##############################################
#D. Spatial/temporal aspects of sampling

#We have already done part of this:
xyplot(Ykm ~ Xkm, 
       aspect = "iso", 
       data = Fish3,
       col = Fish3$Period, #1 = black, 2 = red
       pch = 16)

xyplot(Ykm ~ Xkm | factor(Period), 
       aspect = "iso", 
       data = Fish3,
       col = 1,
       pch = 16)

##############################################



##############################################
#E Interactions
coplot(Dens ~ MeanDepth | factor(Period), data = Fish3)
#Or add a straight line

coplot(Dens ~ MeanDepth | factor(Period), data = Fish3,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

#Or if you would like more control
xyplot(Dens ~ MeanDepth | factor(Period),
   data = Fish3, 
   ylab = "Density",
   xlab = "Mean depth",
   strip = function(bg = 'white', ...) 
   strip.default(bg = 'white', ...),
   scales = list(alternating = T, 
                x = list(relation = "free"),
                y = list(relation = "same")),
   panel=function(x,y){
    panel.grid(h=-1, v= 2)
    panel.points(x, y, col = 1)
    #panel.loess(x,y,col=1,lwd=2) #Add smoother
    #panel.abline(lm(y~x))        #Add regression line
    })
##############################################




##############################################
#F. Zero inflation
sum(Fish3$Dens == 0)
sum(Fish3$Dens == 0) / nrow(Fish3)
plot(table(Fish3$Dens)) #More useful for count data
##############################################



##############################################
#G. Are categorical covariates balanced?
table(Fish3$Period)
##############################################



##############################################
#Why we don't like transformations
#It can remove interactions!

#Code below visualizes the interaction between MeanDepth and Period for
#Density and also for log transformed density

Fish3$LogDens <- log(Fish3$Dens)
                  
coplot(Dens ~ MeanDepth | factor(Period), data = Fish3,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })
           
#win.graph()   #Use quartz() for a MAC
quartz()
coplot(LogDens ~ MeanDepth | factor(Period), data = Fish3,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })
##############################################
         
         
         







