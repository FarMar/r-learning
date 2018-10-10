#    Data exploration, regression, GLM & GAM course
#    Highland Statistics Ltd.
#    www.highstat.com

#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.



#######################################################
#set the working directory & read the data
setwd("/Users/Highstat/applicat/HighlandStatistics/Courses/FilesRegressionGLMGAMCourse/CD/Data")

Veg <- read.table(file = "VegetationX.txt", 
                  header = TRUE,
                  dec = ".")
########################################################################






########################################################################
#To see what is in the object Veg, type:
names(Veg)
str(Veg)
########################################################################




########################################################################
#House keeping
#Load packages from R and support functions that we wrote
source("HighstatLibV6.R")
########################################################################




########################################################################
#Aim of this code: show the use of VIF


#Make a long vector "MyVar" with everything except "PCTCLAY" 

MyVar <- c("ROCK", "LITTER", "ML", "BARESOIL", "FallPrec", "SprPrec", "SumPrec",
           "WinPrec","FallTmax","SprTmax", "SumTmax","WinTmax","FallTmin","SprTmin",
           "SumTmin","WinTmin","PCTSAND","PCTSILT","PCTOrgC","ELEV")
 
corvif(Veg[,MyVar])

# Variance inflation factors
# 
#                  GVIF
# #ROCK         2.783989
# #LITTER       3.578094
# #ML           1.600409
# #BARESOIL     1.719606
# #FallPrec     6.099953
# #SprPrec      2.672436
# #SumPrec      4.788593
# #WinPrec      3.706036
# #FallTmax    37.892704
# #SprTmax     34.797116
# #SumTmax     23.766834
# #WinTmax     46.769026
# #FallTmin   120.941888
# #SprTmin     77.283758
# #SumTmin     87.839398
# #WinTmin     55.271090
# #PCTSAND   5233.133204
# #PCTSILT  18977.737557
# #PCTOrgC  36934.353011  <---- remove 
# #ELEV        96.119556




MyVar <- c("ROCK", "LITTER", "ML", "BARESOIL", "FallPrec", "SprPrec", "SumPrec",
           "WinPrec","FallTmax","SprTmax", "SumTmax","WinTmax","FallTmin","SprTmin",
           "SumTmin","WinTmin","PCTSAND","PCTSILT","ELEV")

corvif(Veg[,MyVar])

# Variance inflation factors
#                 GVIF
# #ROCK        2.760342
# #LITTER      3.398391
# #ML          1.437680
# #BARESOIL    1.698548
# #FallPrec    5.354282
# #SprPrec     2.642068
# #SumPrec     4.785572
# #WinPrec     3.698740
# #FallTmax   34.281808
# #SprTmax    34.703164
# #SumTmax    23.308730
# #WinTmax    46.689137
# #FallTmin  117.862334
# #SprTmin    75.235366
# #SumTmin    78.817980
# #WinTmin    55.156569
# #PCTSAND  1532.567490
# #PCTSILT  1583.844595  <------
# #ELEV       60.746388

  




MyVar <- c("ROCK", "LITTER", "ML", "BARESOIL", "FallPrec", "SprPrec", "SumPrec",
           "WinPrec","FallTmax","SprTmax", "SumTmax","WinTmax","FallTmin","SprTmin",
           "SumTmin","WinTmin","PCTSAND","ELEV")

corvif(Veg[,MyVar])

#Variance inflation factors
#               GVIF
#ROCK       2.572442
#LITTER     3.292423
#ML         1.404018
#BARESOIL   1.698168
#FallPrec   5.287438
#SprPrec    2.626543
#SumPrec    4.772835
#WinPrec    3.490800
#FallTmax  34.279520
#SprTmax   34.352395
#SumTmax   23.152498
#WinTmax   45.290655
#FallTmin 115.791631   #<------
#SprTmin   75.164253
#SumTmin   78.126660
#WinTmin   53.425090
#PCTSAND    1.575174
#ELEV      44.541530
 
 
 
MyVar <- c("ROCK", "LITTER", "ML", "BARESOIL", "FallPrec", "SprPrec", "SumPrec",
           "WinPrec","FallTmax","SprTmax", "SumTmax","WinTmax","SprTmin",
           "SumTmin","WinTmin","PCTSAND","ELEV")

corvif(Veg[,MyVar])
 
# Variance inflation factors
# 
#               GVIF
# #ROCK      2.572275
# #LITTER    3.139389
# #ML        1.385509
# #BARESOIL  1.670371
# #FallPrec  3.149124
# #SprPrec   2.009915
# #SumPrec   3.633010
# #WinPrec   2.410799
# #FallTmax  8.804022
# #SprTmax  27.528761
# #SumTmax  22.292883
# #WinTmax  29.212165
# #SprTmin  55.254486 #remove
# #SumTmin  54.701756
# #WinTmin  44.171264
# #PCTSAND   1.515968
# #ELEV     43.562152

#and you go on untill all the vifs value are less than 3







MyVar <- c("ROCK", "LITTER", "ML", "BARESOIL", "FallPrec", "SprPrec", "SumPrec",
           "WinPrec","SprTmax","PCTSAND")

corvif(Veg[,MyVar])

#Variance inflation factors
#             GVIF
#ROCK     2.206729
#LITTER   2.424254
#ML       1.256119
#BARESOIL 1.282613
#FallPrec 1.530193
#SprPrec  1.674138
#SumPrec  1.475390
#WinPrec  1.772835
#SprTmax  2.202798
#PCTSAND  1.315688




