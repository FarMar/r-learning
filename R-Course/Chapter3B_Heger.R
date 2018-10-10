#Load the data
setwd("/Users/Highstat/applicat/HighlandStatistics/Books/BGS/GAM/Data/Aimy")
BL <- read.table(file = "HegerPierce.txt", 
                 header = TRUE)

str(BL)
names(BL)



#Plot the data
library(lattice)
xyplot(Sources ~ Depth | factor(Station),
       data = BL,
       col = 1,
       pch = 16,
       type = "p",
       subset = (Station!= "56"))


plot(x = BL$Depth, 
     y = BL$Sources,
     xlab = "Depth",
     ylab = "Sources")


#Rescale depth (Because we are programming smoothers ourselves)
BL$DepthOriginal <- BL$Depth
BL$Depth         <- BL$Depth / max(BL$Depth)

plot(x = BL$Depth, 
     y = BL$Sources,
     xlab = "Scaled depth",
     ylab = "Sources", 
     cex = 0.7, 
     pch = 16,
     col = grey(0.5))

#Return to Powerpoint
#######################################################




#Fit linear regression
M1 <- lm(Sources ~ Depth, 
         data = BL)

plot(x = BL$Depth, 
     y = BL$Sources,
     xlab = "Scaled depth",
     ylab = "Sources", 
     cex = 0.7, 
     pch = 16,
     col = grey(0.5))

abline(M1, lwd = 5)

#Return to Powerpoint
#######################################################



#Polynomial regression
M2 <- lm(Sources ~ Depth + 
                   I(Depth^2) + 
                   I(Depth^3), 
         data = BL)

#Show model fit
MD <- data.frame(Depth = seq(0.1, 1, length =100))
plot(x = BL$Depth, 
     y = BL$Sources,
     xlab = "Scaled depth",
     ylab ="Sources", 
     cex = 0.7, 
     pch = 16,
     col = grey(0.5))

P2 <- predict(M2, newdata = MD)
lines(x = MD$Depth, 
      y = P2, 
      lwd = 5)

#Return to powerpoint
###################################################





#Function for (Depthi – 0.2)+ 
rhs <- function(x, TH) ifelse(x >= TH, x-TH,0)

#Is the same as
#rhs <- function(x, TH) {
#	if (x >= TH) { x <- x-TH }
#	if (x <  TH) { x <- 0    }
#	x #Take this back
#	}


#This states:
# if Depth < 0.2  then x = 0
# if Depth >= 0.2 then x = Depth - 0.2
#To test type:
cbind(BL$Depth, rhs(BL$Depth, 0.2))

#Linear spline with 1 knot at 0.2

M3 <- lm(Sources ~ Depth  + 
                   rhs(Depth, 0.2),
         data = BL)
         
head(model.matrix(M3), 10)
summary(M3)

#Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
#(Intercept)      100.581      2.659   37.82   <2e-16 ***
#Depth           -396.454     14.536  -27.27   <2e-16 ***
#rhs(Depth, 0.2)  365.316     15.235   23.98   <2e-16 ***

#Fitted model:
#Dens = 100.581 - 396.45 * Depth + 365.31 * rhs(Depth,0.2)

#Fitted model for Depth < 0.2
#Dens = 100.581 - 396.45 * Depth + 365.31 * 0

#Fitted model for Depth >= 0.2
#Dens = 100.581 - 396.45 * Depth + 365.31 * (Depth - 0.2)
#     = 27.51 - 31.14 * Depth

#At 0.2 both lines are equal.




#Show model fit
P3 <- predict(M3, newdata = MD)
plot(x = BL$Depth, 
     y = BL$Sources,
     xlab = "Scaled depth",
     ylab ="Sources", 
     cex = 0.7, 
     pch = 16,
     col = grey(0.5))

lines(x = MD$Depth, 
      y = P3, 
      lwd = 5)

#Compare, lm, polynomial model and linear spline
AIC(M1, M2, M3)
#Return to powerpoint
#################################################




#Select 11 quartiles and use these as knots
probs <- seq(0, 1, length = 11)
probs
QD <- quantile(BL$Depth, probs)
QD  #Quartiles
#Return to Powerpoint
##################################################



M4 <- lm(Sources ~ Depth  +
                   rhs(Depth, 0.159) + 
                   rhs(Depth, 0.220) +
                   rhs(Depth, 0.281) +
                   rhs(Depth, 0.344) +
                   rhs(Depth, 0.410) +
                   rhs(Depth, 0.490) +
                   rhs(Depth, 0.567) +
                   rhs(Depth, 0.664) +
                   rhs(Depth, 0.787),
         data = BL)

summary(M4)
#What is in model.matrix(M4)
head(model.matrix(M4))


#Show modelfit
P4 <- predict(M4, newdata = MD)
plot(x = BL$Depth, 
     y = BL$Sources,
     xlab = "Scaled depth",
     ylab ="Sources", 
     cex = 0.7, 
     pch = 16, 
     col = grey(0.5))

lines(x= MD$Depth, 
      y = P4, 
      lwd = 5)
#Return to Powerpoint
####################################################



#Quadratic spline
rhs2 <- function(x, TH) ifelse(x >= TH, (x-TH)^2,0)

#Is the same as
#rhs2 <- function(x, TH) {
#	if (x >= TH) { x <- (x-TH)^2 }
#	if (x <  TH) { x <- 0    }
#	x #Take this back
#	}



M5 <- lm(Sources ~ Depth  + I(Depth^2) +
                   rhs2(Depth, 0.159) + 
                   rhs2(Depth, 0.220) +
                   rhs2(Depth, 0.281) +
                   rhs2(Depth, 0.344) +
                   rhs2(Depth, 0.410) +
                   rhs2(Depth, 0.490) +
                   rhs2(Depth, 0.567) +
                   rhs2(Depth, 0.664) +
                   rhs2(Depth, 0.787),
         data = BL)

#Show model fit
P5 <- predict(M5, newdata = MD)
plot(x = BL$Depth, 
     y = BL$Sources,
     xlab = "Scaled depth",
     ylab ="Sources", 
     cex = 0.7, 
     pch = 16,
     col = grey(0.5))
lines(x = MD$Depth, 
      y = P5, 
      lwd = 5)

#Have a look at the output
summary(M5)
#Collinearity may be causing non-significance of these terms
#mgcv has special tools for this

#Return to powerpoint
###########################################################


#####################################################
#Section 3.6: Cubic regression splines
#####################################################

probs <- seq(0, 1, length = 11)
QD    <- quantile(BL$Depth, probs)
QD


#Define function for R values
rk <- function(x, z){
  ((z - 0.5)^2 - 1/12)*((x - 0.5)^2 - 1/12)/4 - ((abs(x - z) - 0.5)^4 - 0.5*(abs(x - z) - 0.5)^2 + 7/240)/24
}

#Test it for 1 knot
rk(BL$Depth, 0.159)


#Make a function that calculates the X matrix
#for all knots
spl.X <- function(x, xk) {
 q <- length(xk) + 2  #Number of knots
 n <- length(x)       #Number of observations
 X <- matrix(1, n, q) #X matrix; first column = 1
 X[, 2] <- x          #Second column = Depth
 X[, 3:q] <- outer(x, xk, FUN = rk)  #R values
 X
}

#Define X
X  <- spl.X(BL$Depth, QD[2:10])
print(head(X), digits=2)

#Return to Powerpoint
##################################################


#First column of X contains the intercept
M6 <- lm(Sources ~ X - 1, 
         data = BL)
summary(M6)


plot(x = BL$Depth, 
     y = BL$Sources,
     xlab = "Scaled depth",
     ylab ="Sources", 
     cex = 0.7, 
     pch = 16, 
     col = grey(0.5))

for (i in 1:11){ abline(v = QD[i], lty = 2) }
 

MD <- data.frame(Depth = seq(QD[1], QD[11], length =100))
Xp <- spl.X(MD$Depth, QD[2:10])
lines(x = MD$Depth, 
      y = Xp %*% coef(M6), 
      lwd = 5)
#End of Figure 3.8
#Return to powerpoint
##########################################



#Show a Cubic smoothing spling in action

#Calculate X matrix

X  <- spl.X(BL$Depth, QD[2:10])

#Penalty matrix
#Code from Wood (2006) to calculate D
spl.S <- function(xk){
	q <- length(xk) + 2
	S <- matrix(0, nrow = q, ncol = q)
	S[3:q, 3:q] <- outer(xk, xk, FUN = rk)
	S
}

D <- spl.S(QD[2:10]) 


#Cross-validation
K <- 25
PredError <- vector(length = K)
Lambda    <- seq(0, 0.001, length = K)     
N         <- nrow(BL)

for (j in 1:K){
  lambda <- Lambda[j] 
  print(j)
  EP <- vector(length = N) 
  for (i in 1:N){
  	Xi    <- X[-i,]
  	Betai <- solve(t(Xi) %*% Xi + lambda * D) %*%t(Xi) %*% BL$Sources[-i]
  	Yi    <- X[i,] %*% Betai
    EP[i] <- BL$Sources[i] - Yi
    }
  PredError[j] <- sum(EP^2)/ N
}

par(mfrow = c(1,1), mar = c(5,5,2,2))
plot(Lambda, PredError, 
     type = "l",
     xlab = "lambda",
     ylab = "OCV", cex.lab = 1.5)
     
#cbind(Lambda,PredError,PredError-min(PredError))


#Get the optimal lambda and calculate smoother
#Show smoother
lambda <- 2.083333e-04
Beta <- solve(t(X) %*% X + lambda * D) %*%t(X) %*% BL$Sources


#Sketch fitted values
MD <- data.frame(Depth = seq(QD[1], QD[11], length =100))
Xp <- spl.X(MD$Depth, QD[2:10])

fhat <- Xp %*% Beta
yfit <- fhat
plot(x = BL$Depth, 
     y = BL$Sources,
     xlab = "Scaled depth",
     ylab ="Sources", 
     cex = 0.7, 
     pch = 16,
     col = grey(0.5))
lines(x = MD$Depth, 
      y = fhat, 
      lwd = 5)


#Do it more efficient
#K <- 25
#V0 <- vector(length = K)
#Lambda <- seq(0, 0.001, length = K)     
#N <- nrow(BL)

#for (j in 1:K){
#  lambda <- Lambda[j] 
#  print(j) 
#  S <- X %*% solve(t(X) %*% X + lambda * D) %*%t(X)
#  Sii <- diag(S)
#  #Beta <- solve(t(X) %*% X + lambda * D) %*%t(X) %*% BL$Sources
#  Yfit <- S %*% BL$Sources
#  E <- BL$Sources - Yfit
#  V0[j] <- (1/N) * sum(( E/(1-Sii))^2 ) 
#}

#par(mfrow = c(1,1), mar = c(5,5,2,2))
#plot(Lambda, V0, 
#     type = "l",
#     xlab = "lambda",
#     ylab = "OCV", cex.lab = 1.5)
################ END cubic smoothing spline code

#to go from lambda to degrees of freedom, take the trace of S
#Return to Powerpoint





########################################################
#Section 3.15
############ GAM analysis ##################################
library(mgcv)
G1 <- gam(Sources ~ s(Depth), data = BL)
summary(G1)

#pf(250.9,df1=4,df2=45-3-4-1,lower.tail=FALSE)



plot(G1, 
     shift = 16.7455, 
     ylim = c(-10, 100), 
     lwd = 3, 
     rug = FALSE)
points(x = BL$Depth, 
       y = BL$Sources, 
       cex = 0.5, 
       pch = 16, 
       col = grey(0.5))


#Or doing it manually
X <- predict(G1, type = "lpmatrix")
eta <- X %*% coef(G1)
eta - fitted(G1)

#Get predicited values
P1 <- predict(G1, newdata = MD, se = TRUE)



#Penalized cubic regression spline
G1 <- gam(Sources ~ s(Depth, bs = "cr"), data = BL)
summary(G1)

G1A <- gam(Sources ~ s(Depth, bs = "cr", k =20), data = BL)
summary(G1A)
plot(G1A)

G1B <- gam(Sources ~ s(Depth, bs = "cr", k =20), gamma = 1.4, data = BL)
summary(G1B)
plot(G1B)


coefficients(G1)
G1$edf

#Model validation graphs
E1 <- resid(G1)
F1 <- fitted(G1)
plot(x = F1, 
     y = E1, 
     cex = 0.7, 
     pch = 16,
     xlab = "Fitted values",
     ylab = "Residuals")
     
abline(h = 0, lty = 2)


boxplot(E1 ~ Station, 
        data = BL,     
        xlab = "Station",
        ylab = "Residuals")
abline(h = 0, lty = 2)

xyplot(E1 ~ Depth | factor(Station),
       data = BL)

xyplot(E1 ~ Depth | factor(Station), 
       data = BL,
       xlab = list(label = "Depth", cex = 1.5),
       ylab = list(label = "Residuals", cex = 1.5),
         strip = strip.custom(bg = 'white',
            par.strip.text = list(cex = 1.2)),
       panel = function(x,y){
       	panel.points(x,y,pch = 16, cex = 0.7, col =1)
       	panel.abline(h = 0, lty = 2)
       }     
       )
       
       
       

xyplot(Sources ~ Depth | factor(Eddy),
       groups = factor(Station), 
       type = "l", 
       col = 1, 
       pch = 16,
       data = BL,
       xlab = list(label = "Depth", cex = 1.5),
       ylab = list(label = "Sources", cex = 1.5),
         strip = strip.custom(bg = 'white',
            par.strip.text = list(cex = 1.2)))
       

G2 <- gam(Sources ~ s(Depth) + factor(Station), 
          data = BL)
   
G3 <- gam(Sources ~ s(Depth, by = factor(Eddy)) + factor(Station), 
          data = BL)
AIC(G2, G3)
par(mfrow = c(1,2)) 
plot(G3)
     





###################################################################
#Model with two smoothers


par(mfrow = c(1, 2))
plot(x = BL$Depth, 
     y = BL$flcugl,
     xlab = "Depth",
     ylab = "flcugl")
     
plot(x = BL$flcugl, 
     y = BL$Sources,
     xlab = "flcugl",
     ylab = "Sources")



BL2 <- BL[BL$flcugl < 0.03, ]
BL2$fStation <- factor(BL2$Station)
G5 <- gam(Sources ~ s(Depth, bs = "cr") + 
                    s(flcugl, bs = "cr") + 
                    fStation, 
          data = BL2)

par(mfrow = c(1,2))
plot(G5, cex.par = 1.5)

summary(G5)





G6 <- gam(Sources ~ s(Depth, bs = "cr"), data = BL2)
G7 <- gam(Sources ~ factor(Station) + s(Depth, by = factor(Eddy), bs = "cr") + s(flcugl, bs = "cr"), data = BL2)
G8 <- gam(Sources ~ factor(Station) + s(Depth, by = factor(Eddy)) + s(flcugl, by = factor(Eddy), bs = "cr"), data = BL2)
G9 <- gam(Sources ~ factor(Station) + s(Depth, by = factor(Eddy)) + flcugl, data = BL2)
G10 <- gam(Sources ~ factor(Station) + s(Depth, by = factor(Eddy)) + flcugl * factor(Eddy), data = BL2)
summary(G6)

G11 <- gam(Sources ~ factor(Station) + te(Depth, flcugl), data = BL2)
G12 <- gam(Sources ~ factor(Station) + te(Depth, flcugl, by = Eddy), data = BL2)

AIC(G5, G6, G7, G8, G9, G10, G11, G12)

par(mfrow = c(2,2))
plot(G8, scale = FALSE)

anova(G3)
summary(G4)
summary(G2)


#End of code
###########################################







X <- predict(G1, type ="lpmatrix")
mu <- X %*% coef(G1)
dim(X)
lambda <- G1$sp
lambda
sum(G1$hat)




