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
setwd("c:/blah blah blah/Data/")
setwd("c:\\blah blah blah\\Data")


#For a Mac:
setwd("/Users/far218/Dropbox/R Course/")


#Import the data from a tab delimited ascii file
Birds <- read.table(file = "loyn.txt",
                    header = TRUE,
                    dec = ".")
#dec = '.'   means that the point is used for decimals. 
#Change to   dec = ","   if required.
########################################################################



########################################################################
# Underlying question and task
# The variable ABUND is the density of birds in 56 forest patches. 
# The explanatory variables are size of the forest patches (AREA), 
# distance to the nearest forest patch (DIST), distance to the 
# nearest larger forest patch (LDIST), year of isolation of the 
# patch (YR.ISOL), agricultural grazing intensity at each patch (GRAZE) 
# and altitude (ALT). The underlying aim of the research is to find 
# a relationship between bird densities and the explanatory variables. 

# The task for the moment is to apply a data exploration. 
########################################################################






########################################################################
#Housekeeping
#Load packages from R and support functions that we wrote

library(lattice)  #For fancy multipanel graphs

#Copy the file HighstatLibV9.R into your working directory
#Ensure it is a .R file and not a .R.txt file!!!!!! 

source("HighstatLibV9.R")
#Now we can make fancy graphs


#Alternative approach: Add path to the source function 
#Windows OS:
#source("Z:/Users/Highstat/applicat/HighlandStatistics/Courses/FilesRegressionGLMGAMCourse/RSolutions/HighstatLibV9.R")
#Mac OS:
#source("/Users/Highstat/applicat/HighlandStatistics/Courses/FilesRegressionGLMGAMCourse/RSolutions/HighstatLibV9.R")
########################################################################




########################################################################
#Inspect the file
#What do we have?
names(Birds)

#[1] "Site"    "ABUND"   "AREA"    "DIST"
#    "LDIST"   "YR.ISOL" "GRAZE"
#[8] "ALT"

str(Birds)  #Make sure that ABUND and AREA are num  and not factors!!!!!!
            #If they are factors then change the dec argument!
#'data.frame':   56 obs. of  8 variables:
# $ Site   : int  1 2 3 4 5 6 7 8 9 10 ...
# $ ABUND  : num  5.3 2 1.5 17.1 13.8 14.1 3.8 2.2 3.3 3 ...
# $ AREA   : num  0.1 0.5 0.5 1 1 1 1 1 1 1 ...
# $ DIST   : int  39 234 104 66 246 234 467 284 156 311 ...
# $ LDIST  : int  39 234 311 66 246 285 467 1829 156 571 ...
# $ YR.ISOL: int  1968 1920 1900 1966 1918 1965 1955 1920 1965 1900 ...
# $ GRAZE  : int  2 5 5 3 5 3 5 5 4 5 ...
# $ ALT    : int  160 60 140 160 140 130 90 60 130 130 ...
########################################################################



########################################################################
#Data exploration
#A Outliers in Y / Outliers in X
#B Collinearity X
#C Relationships Y vs X
#D Spatial/temporal aspects of sampling design (not relevant here)
#E Interactions (is the quality of the data good enough to include them?)
#F Zero inflation Y
#G Are categorical covariates balanced?



###################################################
#First some elementary R commands.
#1 How do you acces variables in an object like Birds?
Birds            #All data
head(Birds)      #First 6 rows
head(Birds, 10)  #First 10 rows
Birds$ABUND      #ABUND variable
Birds[ ,1]         #First column
Birds[,2]        #Second coloumn
Birds[,"ABUND"]  #ABUND variable 
Birds[1,"ABUND"] #First row of ABUND variable
Birds[1:10,"ABUND"]  #First 10 rows of ABUND
c("ABUND", "AREA")   #Two characters concatenated
Birds[ , c("ABUND", "AREA")] #ABUND and AREA variables

MyVar <- c("ABUND",  "AREA")  #Same as last two steps
Birds[, MyVar]
##################################################




##############################################
#A Outliers
#Copy from here...
par(mfrow = c(1, 2))
boxplot(Birds$ABUND, 
        main = "Abundance")
dotchart(Birds$ABUND, 
	     xlab = "Range of data", 
	     ylab = "Order of the data")
#...to here and paste into R


#B Outliers in the X
#Copy from here...
par(mfrow = c(2, 3), mar = c(4, 3, 3, 2))
dotchart(Birds$AREA, main = "Area")
dotchart(Birds$DIST, main = "Distance")
dotchart(Birds$LDIST, main = "Distance to larger patch")
dotchart(Birds$YR.ISOL, main = "Year of isloation")
dotchart(Birds$ALT, main = "Altitude")
dotchart(Birds$GRAZE, main = "Grazing levels")
#to here and paste into R


#Or if you want to make a multi-panel dotplot use
#the following code.
#First make a vector of names
MyVar <- c("AREA", "DIST", "LDIST", "YR.ISOL", 
           "ALT", "GRAZE")
MyVar


#We wrote a wrapper around the dotplot
#function. It is in HighstatlibV6.R
#Make sure that you typed library(lattice) and sourced 
#the HighstatLibV6.R file 
Mydotplot(Birds[,MyVar])
 

#Code below is nice, but always results in confusion.
#Hence we hashed it out.
# #Identify the outlier in area.
# #Let us first make the dotchart ourselves
# plot(x = Birds$AREA, 
     # y = 1:nrow(Birds),
     # xlab = "Value of Area",
     # ylab = "Order of the data from text file")

# #Which site is the outlier?
# #Use the identify function

# #Copy from here
# plot(x = Birds$AREA, 
     # y = 1:nrow(Birds),
     # xlab = "Value of Area",
     # ylab = "Order of the data from text file")

# identify(x = Birds$AREA, y = 1:nrow(Birds))
# #to here and paste. Click on a point and PRESS ESCAPE to stop 
# #the identify process



#Apply transformations
Birds$LOGAREA  <- log10(Birds$AREA)
Birds$LOGDIST  <- log10(Birds$DIST)
Birds$LOGLDIST <- log10(Birds$LDIST)
##############################################



##############################################
#B Collinearity X

MyVar <- c("LOGAREA","LOGDIST","LOGLDIST",
               "YR.ISOL","ALT","GRAZE")
pairs(Birds[, MyVar])



#You need HighstatlibV6.R for this. No need to source
#it again though.
pairs(Birds[,c("LOGAREA","LOGDIST","LOGLDIST",
               "YR.ISOL","ALT","GRAZE")],
      lower.panel = panel.cor)


#This will work as well. It is a wrapper around the pairs
#function. 
MyVar <- c("LOGAREA","LOGDIST","LOGLDIST",
               "YR.ISOL","ALT","GRAZE")
Mypairs(Birds[,MyVar])


#Later in the week we will use VIF values. 
#These are available in the HighstatLibV6.R file
#corvif(Birds[,c("LOGAREA","LOGDIST","LOGLDIST",
#               "YR.ISOL","ALT")])
               

#How do you detect collinearity between a continuous covariate
#and a categorical? Make a conditional boxplot
boxplot(YR.ISOL ~ factor(GRAZE), 
        data = Birds,
        ylab = "Year of isolation",
        xlab = "Grazing intensity")
        
boxplot(LOGAREA ~ factor(GRAZE), 
        data = Birds)
##############################################



##############################################
#C. Relationships Y vs X
MyVar <- c("ABUND","LOGAREA","LOGDIST","LOGLDIST",
               "YR.ISOL","ALT","GRAZE")
pairs(Birds[, MyVar],
      lower.panel = panel.cor)


boxplot(ABUND ~ factor(GRAZE), 
        data = Birds,
        varwidth = TRUE,
        ylab = "Bird abundance",
        xlab = "Grazing levels",
        main = "")
        


#Plot every covariate versus Y
MyX  <- c("LOGAREA", "LOGDIST", "LOGLDIST", "YR.ISOL", "ALT")
Myxyplot(Birds, MyX, "ABUND", MyYlab = "Abundance")

#The code for this yourself is rather ugly!
#See 'Beginner's Guide to R' (2009). Zuur, Ieno, Meesters
##############################################


##############################################
#D. Spatial/temporal aspects of sampling
#Not relevant here
#We will see examples later
##############################################



##############################################
#E Interactions
coplot(ABUND ~ LOGAREA | factor(GRAZE),
      data = Birds)
      
coplot(ABUND ~ LOGAREA | factor(GRAZE),
      data = Birds,
        panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })


#In case you would like to have more control...take a deep breath
#It looks difficult.....but...once you have seen it a couple of times
#it is not that difficult!
xyplot(ABUND ~ LOGAREA | factor(GRAZE),
   data = Birds, 
   layout = c(3,2),
   xlab = "Log area",
   ylab = "Abundance",
   strip = function(bg = 'white', ...) 
   strip.default(bg = 'white', ...),
   scales = list(alternating = TRUE, 
                x = list(relation = "free"),
                y = list(relation = "same")),
   panel=function(x,y){
    panel.grid(h=-1, v= 2)
    panel.points(x, y, col = 1)
    #panel.loess(x,y,col=1,lwd=2) #Add smoother
    panel.abline(lm(y~x))        #Add regression line
    })
##############################################




##############################################
#F. Zero inflation
sum(Birds$ABUND == 0)
100 * sum(Birds$ABUND == 0) / nrow(Birds)
##############################################



##############################################
#G. Are categorical covariates balanced?
table(Birds$GRAZE)
#No levels with 0, 1, 2, 3, 4 observations
##############################################



##############################################
#DON'T MAKE HISTOGRAMS OF COVARIATES!!!!!!!!!!!!!!
#DON'T MAKE HISTOGRAMS OF THE RESPONSE VARIABLE!!!!!!!!!!!!!!
##############################################




