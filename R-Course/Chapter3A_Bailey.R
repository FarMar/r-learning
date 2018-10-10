#Code for presentation Chapter 2

#Set the working directory (computer specific)
setwd("/Users/Highstat/applicat/HighlandStatistics/Books/BGS/GAM/Data/Baileyetal2008")

#Load the data and subselect the required data
DF1 <- read.table("BaileyDensity.txt", 
                  header = TRUE)
DF2 <- DF1[DF1$MeanDepth > 800,]
DF  <- na.exclude(DF2)

#Check import results
names(DF)
str(DF)


#Scatterplot
par(mfrow = c(1, 1), 
    mar   = c(5, 5, 2, 3))

#Define x and y labels    
MyXLab <- "Mean sampling depth (m)"
MyYLab <- expression(paste("Fish density (",m^{-2}, ")"))

#Define point characters
DF$MyPch <- DF$Year
DF$MyPch[DF$Year<1990] <- 1
DF$MyPch[DF$Year>1990] <- 16


plot(x = DF$MeanDepth, 
     y = DF$Dens,
     xlab = MyXLab,
     ylab = MyYLab,
     col = DF$Period,
     pch = DF$MyPch,
     cex.lab = 1.5)



#Close your eyes and apply lm     
M1 <- lm(Dens ~ MeanDepth, 
         data = DF)
         
print(summary(M1), 
      digits = 2, 
      signif.stars = FALSE)


#Model validation 
#Plot residuals vs fitted values and vs depth
E1 <- rstandard(M1)
F1 <- fitted(M1)

par(mfrow = c(1, 2), 
    mar = c(5, 5, 2, 2))

plot(x = F1, 
     y = E1, 
     xlab = "Fitted values", 
     ylab = "Residuals", 
     cex.lab = 1.5)
abline(0, 0)

plot(x = DF$MeanDepth, 
     y = E1, 
     xlab = "Mean depth", 
     ylab = "Residuals", 
     cex.lab = 1.5)
abline(0, 0)


#Skecth model fit
par(mfrow = c(1, 1),  
    mar = c(5, 5, 2, 2))
plot(x = DF$MeanDepth, 
     y = DF$Dens,
     cex.lab = 1.5,
     xlab = MyXLab,
     ylab = MyYLab)

abline(M1, lwd = 5)
#Return to Powerpoint
########################################################





#Apply a GAM with 4 degrees of freedom
library(mgcv)
M4 <- gam(Dens ~ 1 + s(MeanDepth, fx = TRUE, k = 5), 
          data = DF)

#Functions to obtain the results 
summary(M4)
anova(M4)
plot(M4)

#Dens_i = alpha + f(MeanDepth_i) + eps_i
#eps_i ~ N(0, sigma^2)

#Fitted model:
#Dens_i = 0.0047137 + f(MeanDepth_i)
#plot(M4, shift = 0.0047137)

#Plot the smoother with the data
range(DF$MeanDepth)
MyData4 <- data.frame(MeanDepth = seq(from = 804,
                                      to = 4865,
                                      length = 100))
P4 <- predict(M4, 
              newdata = MyData4, 
              se = TRUE)

plot(x = DF$MeanDepth, 
     y = DF$Dens,
     xlab = MyXLab,
     ylab = MyYLab)

lines(x = MyData4$MeanDepth, 
      y = P4$fit, 
      lwd = 3)
lines(x = MyData4$MeanDepth, 
      y = P4$fit + 2 * P4$se.fit, 
      lwd = 3, 
      lty = 2)
lines(x = MyData4$MeanDepth, 
      y = P4$fit - 2 * P4$se.fit, 
      lwd = 3, 
      lty = 2)
#Return to Powerpoint



#######################################
#Cross-validation
M5 <- gam(Dens ~ s(MeanDepth), data=DF)

summary(M5)
plot(M5)

#Start simple:
#1. Choose df
#2. Leave out observation 1
#3. Apply GAM on remaining data
#4. Predict density for observation 1
#5. Calculate prediction residual

#1 - 3
i <- 1
M5i <- gam(Dens ~ s(MeanDepth, fx = TRUE, k = 5), 
           data=DF, 
           subset = -i)

plot(x = DF$MeanDepth, 
     y = DF$Dens)

points(x = DF$MeanDepth[i], 
       y = DF$Dens[i],
       col = 2,
       pch = 16,
       cex = 3)
       
MDi <- data.frame(MeanDepth = seq(804, 4865, 
                                  length = 100))
P5  <- predict(M5i, 
               newdata = MDi)
lines(x = MDi$MeanDepth, 
      y = P5, 
      lwd = 2)

#4
MDi <- data.frame(MeanDepth = DF$MeanDepth[i])
P5  <- predict(M5i, newdata = MDi)
EP  <- DF$Dens[i] - P5

#Now do this for every observation

EP <- vector(length = 147) 
for (i in 1:147) {
  M5i <- gam(Dens ~ s(MeanDepth, fx = TRUE, k = 5), 
             data=DF, 
             subset = -i)

  plot(x = DF$MeanDepth, 
       y = DF$Dens)

  points(x = DF$MeanDepth[i], 
         y = DF$Dens[i],
         col = 2,
         pch = 16,
         cex = 3)
       
  MDi <- data.frame(MeanDepth = seq(804, 4865, 
                                  length = 100))
  P5  <- predict(M5i, 
                 newdata = MDi)
  lines(x = MDi$MeanDepth, 
        y = P5, 
       lwd = 2)

  #4
  MDi <- data.frame(MeanDepth = DF$MeanDepth[i])
  P5  <- predict(M5i, newdata = MDi)
  EP[i]  <- DF$Dens[i] - P5
}

plot(1:147,EP)    
 
#And now try 1, 2, 3, 4, 5, 6, df




#Fancy coding...just run it     
PredError <- vector(length = 10-4+1)
j <- 1
     
for (k in 4:10){
  Myk <- k
  print(k)
  EP <- vector(length = 147) 
  for (i in 1:147){
    M5i <- gam(Dens ~ s(MeanDepth, fx = TRUE, k = Myk), data=DF, subset = -i)
    MDi <- data.frame(MeanDepth = DF$MeanDepth[i])
    P5 <- predict(M5i, newdata = MDi)
    EP[i] <- DF$Dens[i] - P5
    }
  PredError[j] <- sum(EP^2)/ 147
  j <- j+1
}

par(mfrow = c(1,1), mar = c(5,5,2,2))
plot(3:9, PredError, 
     type = "l",
     xlab = "Equivalent degrees of freedom",
     ylab = "OCV", cex.lab = 1.5)

#Return to Powerpoint
     
     
Nonsense <- gam(Dens ~ s(Year), data = DF)     
plot(Nonsense)

     
###############################################     
#Model validation 

M5 <- gam(Dens ~ s(MeanDepth), data=DF)

#Get residuals and fitted values
E5 <- resid(M5)
F5 <- fitted(M5)

hist(E5, 
     breaks = 25, 
     main = "", 
     xlab = "Residuals")

plot(x = F5, 
     y = E5, 
     xlab = "Fitted values", 
     ylab = "Residuals")
abline(0, 0)

#Independence
plot(x = DF$MeanDepth, 
     y = E5, 
     xlab = "Depth", 
     ylab = "Residuals")
abline(0, 0)

boxplot(E5 ~ factor(DF$Period), 
        xlab = "Period", 
        ylab = "Residuals")
abline(0, 0)


MyCex <- abs(E5)/max(abs(E5))

MyCol <- vector(length = length(E5))
MyCol[E5 > 0] <- gray(0.5)
MyCol[E5 <= 0] <- gray(0.2)

library(lattice)
xyplot(Ykm ~ Xkm,  
       data = DF,  
       main = list(label = "Residuals", cex = 1.5),
       xlab = list(label = "X-coordinates", cex = 1.5),
       ylab = list(label = "Y-coordinates", cex = 1.5),
       aspect = "iso",
       pch = 16,
       col = MyCol,
       cex = 3 * (MyCex)^(1/6))

#Or make a variogram
#Return to Powerpoint
##############################################




#GAM wth depth smoother and factor Period

DF$fPeriod <- factor(DF$Period)
M6 <- gam(Dens ~ s(MeanDepth) + fPeriod, 
          data=DF)
summary(M6)
#Period 1: Dens_i = 0.0054 + f(MeanDepth)
#Period 2: Dens_i = 0.0054 - 0.0021 + f(MeanDepth)

anova(M6)

par(mfrow = c(1, 1))
plot(M6)



#Model interpretation
par(mar = c(2, 2, 2, 2))
vis.gam(M6, theta = 120, color = "heat")

#Sketch fitted values
MD1 <- data.frame(
            MeanDepth = seq(from = 804, to = 4865, length = 100),
            fPeriod = "1")

MD2 <- data.frame(
            MeanDepth = seq(from = 804, to = 4865, length = 100),
            fPeriod = "2")

P1 <- predict(M6, newdata = MD1)
P2 <- predict(M6, newdata = MD2)

MyPch <- vector(length=length(DF$Dens))
MyPch[DF$Period == 1] <- 1
MyPch[DF$Period == 2] <-16

par(mar = c(5,5,3,3))
plot(x = DF$MeanDepth, 
     y = DF$Dens,
     xlab = MyXLab,
     ylab = MyYLab,
     pch = MyPch)
lines(x = MD1$MeanDepth, 
      y = P1, 
      lwd = 3)
lines(x = MD2$MeanDepth, 
      y = P2, 
      lwd = 3)
      
#Return to Powerpoint
################################################




#GAM with interactions
M7 <- gam(Dens ~ s(MeanDepth, by = fPeriod) +
                 fPeriod,
                 data = DF)
summary(M7)
anova(M7)
                  
# anova(M6, M7, test = "F") #Models are not nested
AIC(M6, M7)
     


#
par(mfrow = c(2, 2))
plot(M7, select = 1, main = "Period 1")
plot(M7, select = 2, main = "Period 2")

#And plot the predicted values


MD7.1 <- data.frame(
             MeanDepth = seq(from = min(DF$MeanDepth),
                             to   = max(DF$MeanDepth), 
                             length = 100),
             fPeriod = "1")
MD7.2 <- data.frame(
             MeanDepth = seq(from = min(DF$MeanDepth),
                             to = max(DF$MeanDepth), 
                             length = 100),
             fPeriod = "2")

P7.1 <- predict(M7, newdata = MD7.1)
P7.2 <- predict(M7, newdata = MD7.2)

#par(mfrow=c(1,2), mar = c(5,6,3,2))
plot(x = DF$MeanDepth[DF$Period == 1], 
     y = DF$Dens[DF$Period == 1],
     xlab = MyXLab, 
     ylab = MyYLab,
     ylim = c(0, 3.092395e-02), 
     main = "Period 1")
lines(x = MD7.1$MeanDepth, 
      y = P7.1, 
      lwd = 3)

plot(x = DF$MeanDepth[DF$Period == 2], 
     y = DF$Dens[DF$Period == 2],
     xlab = MyXLab, 
     ylab = MyYLab,
     ylim = c(0, 3.092395e-02), 
     main = "Period 2")
lines(x = MD7.2$MeanDepth, 
      y = P7.2, 
      lwd = 3)





