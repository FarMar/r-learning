#    Highland Statistics Ltd.
#    www.highstat.com
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 


#######################################################
#set the working directory & read the data
setwd("/Users/Highstat/applicat/HighlandStatistics/Courses/Data")
Mites <- read.table(file = "DrugsMites.txt", 
                    header = TRUE, 
                    dec = ".")


#To see what is in the object Mites, type:
names(Mites)
str(Mites)

################################################
#Aim: Model Proportion (= Dead_mites / Total) as 
#     a function of:
#        -Concentration
#        -Toxic (=factor) 
#        -interaction between Concentration and Toxic  


###############################################
#DATA EXPLORATION: 
#Outliers: Y and X
#Collinearity X
#Relationships  Y vs X    but also interactions
#Zero inflation: Make a frequency plot


#Are there any outliers?
#Outliers in the response variable?
#Outliers in the explanatory variables?
#Make dotplots:

#Y
boxplot(Mites$Proportion)
dotchart(Mites$Proportion,
         ylab = "Order of the data",
         xlab = "Values of Proportion")

#X
dotchart(Mites$Concentration,
         ylab = "Order of the data",
         xlab = "Values of Proportion")



#########################################
#Collinearity:
boxplot(Concentration ~ Toxic,
        data = Mites,
        xlab = "Toxic",
        ylab = "Concentration")


#########################################
#Relationships
boxplot(Proportion ~ Toxic, data = Mites)
plot(x = Mites$Concentration, 
     y = Mites$Proportion,
     xlab = "Concentration",
     ylab = "Proportion")

#Interaction
coplot(Proportion ~ Concentration | factor(Toxic), 
       data = Mites)



####################################################
#Start frequentist analysis
#Binomial GLM 

Mites$LivingMites <- Mites$Total - Mites$Dead_mites
M1 <- glm(cbind(Dead_mites, LivingMites) ~
             Concentration  * factor(Toxic),
             family = binomial, 
             data = Mites)

#Check for overdispersion
E1 <- resid(M1, type = "pearson")
sum(E1^2) / (M1$df.res)

#Numerical output
summary(M1)
drop1(M1, test = "Chi")


######################################
# Fit the model
# What is the model that we are fitting?

# Dead_mites_i      ~ Bin(pi_i, N_i)
# E(Dead_mites_i)   = N_i * pi_i   
# var(Dead_mites_i) = N_i * pi_i * (1 - p_i)
#
# logit(pi_i) = Intercept +  Concentration_i +
#               Toxic_i +
#               Toxic x Concentration_i   

#Toxic 1
#logit(Pi) = -1.43 + 3.80 * Concentration

#Toxic 2
#logit(Pi) = -1.43 -0.008 + (3.80 + 6.17 ) * Concentration

#Etc.


############################################
# Model validation
# Plot residuals vs fitted values
# Influential observations
# Plot residuals vs each covariate (in the model, 
# and not in the model)

F1 <- fitted(M1)
plot(x = F1, 
     y = E1, 
     main = "Residuals versus fitted values")
abline(h = 0, lty = 2)


#Independence:
plot(x = Mites$Concentration, 
     y = E1)
abline(h = 0, lty = 2)


#Influential observations: 
#Cook distance
par(mfrow = c(1, 1))
plot(M1, which = 4)


###############################################
#Model interpretation

#                             Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                  -1.43483    0.41664  -3.444 0.000574 ***
#Concentration                 3.80101    0.87479   4.345 1.39e-05 ***
#factor(Toxic)2               -0.00860    0.57713  -0.015 0.988111    
#factor(Toxic)3               -0.01419    0.58714  -0.024 0.980717    
#factor(Toxic)4               -0.27387    0.58485  -0.468 0.639594    
#Concentration:factor(Toxic)2  6.17647    2.28823   2.699 0.006950 ** 
#Concentration:factor(Toxic)3  1.89983    1.36204   1.395 0.163062    
#Concentration:factor(Toxic)4 -1.48275    0.96299  -1.540 0.123626   


# Equation for toxic 1:
# eta = -1.43 + 3.80 * Concentration
# pi = exp(eta) / (1 + exp(eta))
# 
# Equation for toxic 2:
# eta = -1.43 -0.00860 + (3.80 + 6.17647) * Concentration
# pi = exp(eta) / (1 + exp(eta))
# 
# #Etc.....
                 


#Sketch model fit using clumsy coding
MyData1 <- data.frame(Concentration = seq(0, 2.16, length=50),
                      Toxic = 1)
MyData2 <- data.frame(Concentration = seq(0, 2.16, length=50),
                      Toxic = 2)
MyData3 <- data.frame(Concentration = seq(0, 2.16, length=50),
                      Toxic = 3)
MyData4 <- data.frame(Concentration = seq(0, 2.16, length=50),
                      Toxic = 4)

P1 <- predict(M1, newdata=MyData1, type="response")
P2 <- predict(M1, newdata=MyData2, type="response")
P3 <- predict(M1, newdata=MyData3, type="response")
P4 <- predict(M1, newdata=MyData4, type="response")


plot(x = Mites$Concentration, 
     y = Mites$Proportion, 
     pch=16, 
     col = Mites$Toxic)
lines(MyData1$Concentration, P1, col=1, lty=1)
lines(MyData2$Concentration, P2, col=2, lty=2)
lines(MyData3$Concentration, P3, col=3, lty=3)
lines(MyData4$Concentration, P4, col=4, lty=4)

legend("bottomright", 
       legend = c("1", "2", "3", "4"),
       lty = c(1,2,3,4), 
       col = c(1,2,3,4))
#Task: Explain the results




###################################################
#Sketch model fit with 95% confidence bands..clumsy code
MyData1 <- data.frame(Concentration = seq(0, 2.16, length=50),
                      Toxic = 1)
MyData2 <- data.frame(Concentration = seq(0, 2.16, length=50),
                      Toxic = 2)
MyData3 <- data.frame(Concentration = seq(0, 2.16, length=50),
                      Toxic = 3)
MyData4 <- data.frame(Concentration = seq(0, 2.16, length=50),
                      Toxic = 4)

P1 <- predict(M1, newdata=MyData1, type="link",se=TRUE)
P2 <- predict(M1, newdata=MyData2, type="link",se=TRUE)
P3 <- predict(M1, newdata=MyData3, type="link",se=TRUE)
P4 <- predict(M1, newdata=MyData4, type="link",se=TRUE)


plot(x=Mites$Concentration, y=Mites$Proportion, pch=16, col=Mites$Toxic)

G1 <- exp(P1$fit) / (1 + exp(P1$fit))
G1.UP <- exp(P1$fit + 1.96 * P1$se.fit ) /
          (1 + exp(P1$fit + 1.96 * P1$se.fit))

G1.LO <- exp(P1$fit - 1.96 * P1$se.fit) /
          (1 + exp(P1$fit - 1.96 * P1$se.fit))

lines(MyData1$Concentration, G1, col=1, lty=1)
lines(MyData1$Concentration, G1.UP, col=1, lty=2)
lines(MyData1$Concentration, G1.LO, col=1, lty=2)
#Repeat for the other 3 lines



#Fancy graph for paper:
library(lattice)
xyplot(Proportion ~ Concentration | factor(Toxic),
       data = Mites,
       panel = function(x,y, subscripts,...){
         panel.points(x,y, pch = 16, col =1)
         print(Mites$Toxic[subscripts][1])
         ID <- Mites$Toxic[subscripts][1]
         if (ID == 1) {
            G1 <- exp(P1$fit) / (1 + exp(P1$fit))
            G1.UP <- exp(P1$fit + 1.96 * P1$se.fit ) / (1 + exp(P1$fit + 1.96 * P1$se.fit))
            G1.LO <- exp(P1$fit - 1.96 * P1$se.fit) / (1 + exp(P1$fit - 1.96 * P1$se.fit))
            panel.lines(MyData1$Concentration, G1, col=1, lty=1)
            panel.lines(MyData1$Concentration, G1.UP, col=1, lty=2)
            panel.lines(MyData1$Concentration, G1.LO, col=1, lty=2)
         }   
         if (ID == 2) {
           G2 <- exp(P2$fit) / (1 + exp(P2$fit))
           G2.UP <- exp(P2$fit + 1.96 * P2$se.fit ) / (1 + exp(P2$fit + 1.96 * P2$se.fit))
           G2.LO <- exp(P2$fit - 1.96 * P2$se.fit) / (1 + exp(P2$fit - 1.96 * P2$se.fit))
           panel.lines(MyData2$Concentration, G2, col=1, lty=1)
           panel.lines(MyData2$Concentration, G2.UP, col=1, lty=2)
           panel.lines(MyData2$Concentration, G2.LO, col=1, lty=2)
         }   
         if (ID == 3) {
           G3 <- exp(P3$fit) / (1 + exp(P3$fit))
           G3.UP <- exp(P3$fit + 1.96 * P3$se.fit ) / (1 + exp(P3$fit + 1.96 * P3$se.fit))
           G3.LO <- exp(P3$fit - 1.96 * P3$se.fit) / (1 + exp(P3$fit - 1.96 * P3$se.fit))
           panel.lines(MyData3$Concentration, G3, col=1, lty=1)
           panel.lines(MyData3$Concentration, G3.UP, col=1, lty=2)
           panel.lines(MyData3$Concentration, G3.LO, col=1, lty=2)
         }   
         if (ID == 4) {
           G4 <- exp(P4$fit) / (1 + exp(P4$fit))
           G4.UP <- exp(P4$fit + 1.96 * P4$se.fit ) / (1 + exp(P4$fit + 1.96 * P4$se.fit))
           G4.LO <- exp(P4$fit - 1.96 * P4$se.fit) / (1 + exp(P4$fit - 1.96 * P4$se.fit))
           panel.lines(MyData4$Concentration, G4, col=1, lty=1)
           panel.lines(MyData4$Concentration, G4.UP, col=1, lty=2)
           panel.lines(MyData4$Concentration, G4.LO, col=1, lty=2)
         }   
         
         })
#####################################  




###################################
#And make the same graph in ggplot2



Mites$fToxic <- factor(Mites$Toxic)
M1 <- glm(Proportion ~ Concentration  * fToxic,
          family = binomial,
          weights = Total,
          data = Mites)

library(plyr)
MyData <- ddply(Mites, .(fToxic), summarize,
                Concentration = seq(min(Concentration), 
                                    max(Concentration), 
                                    length = 25))
MyData

P1          <- predict(M1, newdata = MyData, se = TRUE, type = "link")
MyData$Pi   <- exp(P1$fit) / (1 + exp(P1$fit))
MyData$up <- exp(P1$fit + 1.96*P1$se.fit) / (1 + exp(P1$fit + 1.96*P1$se.fit))
MyData$lo <- exp(P1$fit - 1.96*P1$se.fit) / (1 + exp(P1$fit - 1.96*P1$se.fit))
MyData

#Add some random noise around the concentration values
#so that we can distinguish points that would otherwise be
#plotted on top of each other.
Mites$Concentration.jitter  <- jitter(Mites$Concentration,amount = 0.1)
 
library(ggplot2)
p <- ggplot()
p <- p + geom_point(data = Mites, 
                    aes(y = Proportion, x = Concentration.jitter),
                    shape = 1, 
                    size = 1)
p <- p + xlab("Concentration") + ylab("Probability of succes")
p <- p + theme(text = element_text(size=15)) 
p <- p + geom_line(data = MyData, 
                   aes(x = Concentration, y = Pi), 
                   colour = "black")

p <- p + geom_ribbon(data = MyData, 
                     aes(x = Concentration, 
                         ymax = up, 
                         ymin = lo ),
                     alpha = 0.2)
p <- p + facet_grid(fToxic ~ ., scales = "fixed")
p




