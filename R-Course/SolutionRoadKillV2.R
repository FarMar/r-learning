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
setwd("/Users/Highstat/applicat/HighlandStatistics/Courses/Data")


#Import the data from a tab delimited ascii file
RK <- read.table(file = "Roadkills.txt", 
                 header = TRUE,
                 dec = ".")
                 
#dec = '.'   means that the point is used for decimals. 
#Change to   dec = ","   if required.
########################################################################



########################################################################
#Check variables and names
str(RK)
names(RK)
############################################


########################################################################
#Load all packages and functions
source("HighstatLibV6.R")
library(lattice)
########################################################################



########################################################################
#Data exploration
#Outliers in the Y and X

#Copy-paste from the names(RK) results:
MyVar <- c( "X1", "Y1", "TOT.N", "OPEN.L",  
            "OLIVE", "MONT.S", "MONT",  "POLIC", "SHRUB" ,    
            "URBAN" , "WAT.RES",  "L.WAT.C", "L.D.ROAD",   "L.P.ROAD",  
            "D.WAT.RES","D.WAT.COUR", "D.PARK", "N.PATCH",    "P.EDGE"  ,  
            "L.SDI")

Mydotplot(as.matrix(RK[,MyVar]))

#We decided to square root transform 
#the following covariates: 
#POLIC, WAT.RES, URBAN, OLIVE, 
#L.P.ROAD, SHRUB, D.WAT.COUR. 
#Do you agree with this?

RK$SQ.POLIC    <- sqrt(RK$POLIC)
RK$SQ.WATRES   <- sqrt(RK$WAT.RES)
RK$SQ.URBAN    <- sqrt(RK$URBAN)
RK$SQ.OLIVE    <- sqrt(RK$OLIVE)
RK$SQ.LPROAD   <- sqrt(RK$L.P.ROAD)
RK$SQ.SHRUB    <- sqrt(RK$SHRUB)
RK$SQ.DWATCOUR <- sqrt(RK$D.WAT.COUR)

#####################################
#Collinearity
#  pairs
#  VIF
         #Remove: "X1", "Y1", "TOT.N",
         #Use SQ variables
MyVar <- c( "OPEN.L",  
            "SQ.OLIVE", 
            "MONT.S", "MONT",  
            "SQ.POLIC", 
            "SQ.SHRUB" ,    
            "SQ.URBAN" , 
            "SQ.WATRES",  
            "L.WAT.C", "L.D.ROAD",   
            "SQ.LPROAD",  
            "D.WAT.RES",
            "SQ.DWATCOUR", 
            "D.PARK", "N.PATCH",    "P.EDGE"  ,  
            "L.SDI")

pairs(RK[,MyVar], 
      lower.panel = panel.cor)
corvif(RK[,MyVar])

#                   GVIF
# OPEN.L      161.011940
# SQ.OLIVE     34.449442
# MONT.S        3.968517
# MONT        213.635153  <------
# SQ.POLIC      3.897250
# SQ.SHRUB      3.322271
# SQ.URBAN     14.035447
# SQ.WATRES     1.985143
# L.WAT.C       3.645053
# L.D.ROAD      4.414907
# SQ.LPROAD     3.386142
# D.WAT.RES     2.118247
# SQ.DWATCOUR   2.558128
# D.PARK        2.915299
# N.PATCH      24.307098
# P.EDGE       19.364710
# L.SDI        10.027313



MyVar2 <- c( "OPEN.L",  
            "SQ.OLIVE", 
            "MONT.S", 
            "SQ.POLIC", 
            "SQ.SHRUB" ,    
            "SQ.URBAN" , 
            "SQ.WATRES",  
            "L.WAT.C", "L.D.ROAD",   
            "SQ.LPROAD",  
            "D.WAT.RES",
            "SQ.DWATCOUR", 
            "D.PARK", "N.PATCH",    "P.EDGE"  ,  
            "L.SDI")

corvif(RK[,MyVar2])
#                  GVIF
# OPEN.L       1.553334
# SQ.OLIVE     7.980747
# MONT.S       1.585527
# SQ.POLIC     2.680075
# SQ.SHRUB     3.146558
# SQ.URBAN     4.036965
# SQ.WATRES    1.575170
# L.WAT.C      3.340893
# L.D.ROAD     4.387305
# SQ.LPROAD    3.236589
# D.WAT.RES    2.111337
# SQ.DWATCOUR  2.439228
# D.PARK       2.886749
# N.PATCH     12.341053
# P.EDGE      15.024742  <-----
# L.SDI        7.333497


MyVar3 <- c( "OPEN.L",  
             "SQ.OLIVE", 
             "MONT.S", 
             "SQ.POLIC", 
             "SQ.SHRUB" ,    
             "SQ.URBAN" , 
             "SQ.WATRES",  
             "L.WAT.C", "L.D.ROAD",   
             "SQ.LPROAD",  
             "D.WAT.RES",
             "SQ.DWATCOUR", 
             "D.PARK", "N.PATCH",    
             "L.SDI")

corvif(RK[,MyVar3])


#                  GVIF
# OPEN.L       1.553310
# SQ.OLIVE     7.687101
# MONT.S       1.559003
# SQ.POLIC     2.673122
# SQ.SHRUB     3.123827
# SQ.URBAN     4.013779
# SQ.WATRES    1.570619
# L.WAT.C      2.603512
# L.D.ROAD     2.286635
# SQ.LPROAD    2.411745
# D.WAT.RES    2.109753
# SQ.DWATCOUR  2.387500
# D.PARK       2.750898
# N.PATCH     11.037524  <----
# L.SDI        7.222316



MyVar4 <- c( "OPEN.L",  
             "SQ.OLIVE", 
             "MONT.S", 
             "SQ.POLIC", 
             "SQ.SHRUB" ,    
             "SQ.URBAN" , 
             "SQ.WATRES",  
             "L.WAT.C", "L.D.ROAD",   
             "SQ.LPROAD",  
             "D.WAT.RES",
             "SQ.DWATCOUR", 
             "D.PARK",    
             "L.SDI")

corvif(RK[,MyVar4])

#                 GVIF
# OPEN.L      1.533354
# SQ.OLIVE    3.883120
# MONT.S      1.553256
# SQ.POLIC    2.316072
# SQ.SHRUB    2.491582
# SQ.URBAN    3.674728
# SQ.WATRES   1.568785
# L.WAT.C     2.421625
# L.D.ROAD    1.574901
# SQ.LPROAD   1.834711
# D.WAT.RES   2.100651
# SQ.DWATCOUR 2.113186
# D.PARK      2.642756
# L.SDI       6.438174  <------
# 



MyVar5 <- c( "OPEN.L",  
             "SQ.OLIVE", 
             "MONT.S", 
             "SQ.POLIC", 
             "SQ.SHRUB" ,    
             "SQ.URBAN" , 
             "SQ.WATRES",  
             "L.WAT.C", "L.D.ROAD",   
             "SQ.LPROAD",  
             "D.WAT.RES",
             "SQ.DWATCOUR", 
             "D.PARK")

corvif(RK[,MyVar5])
#                 GVIF
# OPEN.L      1.527207
# SQ.OLIVE    2.925562
# MONT.S      1.208718
# SQ.POLIC    2.294887
# SQ.SHRUB    1.754087
# SQ.URBAN    3.483754  <------
# SQ.WATRES   1.444741
# L.WAT.C     2.044351
# L.D.ROAD    1.524871
# SQ.LPROAD   1.621724
# D.WAT.RES   1.967797
# SQ.DWATCOUR 2.051042
# D.PARK      2.522630





MyVar6 <- c( "OPEN.L",  
             "SQ.OLIVE", 
             "MONT.S", 
             "SQ.POLIC", 
             "SQ.SHRUB" ,    
             "SQ.WATRES",  
             "L.WAT.C", "L.D.ROAD",   
             "SQ.LPROAD",  
             "D.WAT.RES",
             "SQ.DWATCOUR", 
             "D.PARK")

corvif(RK[,MyVar6])
#                 GVIF
# OPEN.L      1.426982
# SQ.OLIVE    1.596145
# MONT.S      1.168054
# SQ.POLIC    2.201264
# SQ.SHRUB    1.751432
# SQ.WATRES   1.403705
# L.WAT.C     2.029626
# L.D.ROAD    1.517820
# SQ.LPROAD   1.374345
# D.WAT.RES   1.953729
# SQ.DWATCOUR 2.037860
# D.PARK      2.443277



# Subset based on VIF:
#   MyVar6 <- c( "OPEN.L",  
#                "SQ.OLIVE", 
#                "MONT.S", 
#                "SQ.POLIC", 
#                "SQ.SHRUB" ,    
#                "SQ.WATRES",  
#                "L.WAT.C", "L.D.ROAD",   
#                "SQ.LPROAD",  
#                "D.WAT.RES",
#                "SQ.DWATCOUR", 
#                "D.PARK")




#The variable D_PARK represents the spatial position. 
#Plot each covariate against D_PARK using a multi-panel graph. 
#What do you notice?
MyVar6 <- c( "OPEN.L",  
             "SQ.OLIVE", 
             "MONT.S", 
             "SQ.POLIC", 
             "SQ.SHRUB" ,    
             "SQ.WATRES",  
             "L.WAT.C", 
             "L.D.ROAD",   
             "SQ.LPROAD",  
             "D.WAT.RES",
             "SQ.DWATCOUR", 
             "D.PARK")

MyY   <- MyVar6
Y     <- RK[,MyY]
AllY  <- as.vector(as.matrix(Y))
AllX  <- rep(RK$D.PARK,  length(MyVar6))   
ID    <- rep(MyVar6, each = nrow(RK))

xyplot(AllY ~ AllX | factor(ID),
       col = 1,
       xlab = "Explanatory variables",
       ylab = "Response variable",
       strip = function(bg='white', ...)
         strip.default(bg='white', ...),
       scales = list(alternating = T,
                     x = list(relation = "free"),
                     y = list(relation = "free")),
       panel=function(x, y){
         panel.grid(h=-1, v= 2)
         panel.abline(0,0)
         panel.points(x, y, col = 1)
         panel.loess(x, y, col = 1, lwd = 2)
       })

#This is what Myxyplot is doing!
#Due to spatial 'correlation', remove:
#SQ.OLIVE
#D.WAT.RES
#L.D.ROAD                    



#Other steps: 
#    check for zeros inflation TOT.N
#    Plot TOT.N vs each covariate
#################################################





#################################################
#Now start the glm with the following covariates:
#OPEN.L, MONT.S, SQ.POLIC, SQ.SHRUB,
#       SQ.WATRES, L.WAT.C, SQ.LPROAD,
#       SQ.DWATCOUR, D.PARK
#     

M1 <- glm(TOT.N ~ OPEN.L + MONT.S + SQ.POLIC +
                  SQ.SHRUB + SQ.WATRES + L.WAT.C + SQ.LPROAD +
                  SQ.DWATCOUR + D.PARK, data = RK,
          family = poisson)

#Check for overdispersion
E1 <- resid(M1, type = "pearson") 
N <- nrow(RK)
p <- length(coef(M1))
Overdispersion1 <- sum(E1^2) / (N-p)
Overdispersion1
#[1] 5.928003
#Overdispersion!


#Option 1: quasiPoisson
M2 <- glm(TOT.N ~ OPEN.L + MONT.S + SQ.POLIC +
                  SQ.SHRUB + SQ.WATRES + L.WAT.C + SQ.LPROAD +
                  SQ.DWATCOUR + D.PARK, data = RK,
        family=quasipoisson)
summary(M2)
drop1(M2, test = "F")

#.....drop non-significant cocariates...one at a time and refit the glm/ drop1



M3 <- glm(TOT.N ~ OPEN.L + MONT.S + SQ.POLIC +
                  SQ.SHRUB + SQ.WATRES + L.WAT.C + SQ.LPROAD +
                   D.PARK, data = RK,
          family=quasipoisson)


drop1(M3, test = "F")
step(M3)  #<---- Doesn't work. You could use quasi-AIC






#Option 2: Negative binomial
library(MASS)
M6 <- glm.nb(TOT.N ~ OPEN.L + MONT.S + SQ.POLIC +
                     SQ.SHRUB + SQ.WATRES + L.WAT.C +
                     SQ.LPROAD + SQ.DWATCOUR + D.PARK,
             link = "log", data = RK)
drop1(M6, test = "Chi")

#Check for overdispersion
E6 <- resid(M6, type = "pearson") 
Overdispersion6 <- sum(E6^2) /  M6$df.resid
#M6$df.resid is the same as N - p
Overdispersion6
#0.9789879


#Is everything significant?
drop1(M6, test = "Chi")
#No....time for IT or model selection

step(M6)


M7 <- glm.nb(TOT.N ~ OPEN.L + L.WAT.C + SQ.LPROAD + D.PARK,
             link = "log", data = RK)
drop1(M7, test = "Chi")


#Overrule the AIC
M8 <- glm.nb(TOT.N ~ OPEN.L + L.WAT.C + D.PARK,
             link = "log", data = RK)
drop1(M8, test = "Chi")
summary(M8)
#################################################




#################################################
#Model validation
par(mfrow = c(2, 2))
plot(M8)

par(mfrow = c(1, 1))
E8 <- resid(M8, type="pearson")
F8 <- fitted(M8)
plot(x = F8, y = E8)
abline(h = 0)


#Plot residuals vs each covariate
RK$E8 <- E8

MyVar <- c( "OPEN.L",  
            "SQ.OLIVE", 
            "MONT.S", "MONT",  
            "SQ.POLIC", 
            "SQ.SHRUB" ,    
            "SQ.URBAN" , 
            "SQ.WATRES",  
            "L.WAT.C", "L.D.ROAD",   
            "SQ.LPROAD",  
            "D.WAT.RES",
            "SQ.DWATCOUR", 
            "D.PARK", "N.PATCH",    "P.EDGE"  ,  
            "L.SDI")


Myxyplot(RK, MyVar, "E8", MyYlab = "Pearson residuals")
######################################

#Plot residuals versus Distance to park
plot(x = RK$D.PARK, y = E8)
abline(h = 0, v = 0, lty = 2)



#Spatial independence?
RK$MyCex <- abs(E8) / max(abs(E8))
RK$MyCol <- E8
RK$MyCol[E8 >= 0 ] <- 1
RK$MyCol[E8 < 0 ] <- 2

xyplot(Y1 ~ X1,
       aspect = "iso",
       cex = 3 * RK$MyCex,
       pch = 16,
       col = RK$MyCol,
       data = RK,
       main = "Spatial plot of residuals")
##############################################################





##############################################################
#Model interpretation
summary(M8)

#TOT.N ~ NB(mu_i, 4.73)
#E(TOT.N) = mu_i
#var(TOT.N) = mu_i + mu_i^2 / 4.73
#log(mu_i) = 4.46 -0.01 * OPEN.L + 0.18 * L.WAT.C - 0.00012 * D.PARK






