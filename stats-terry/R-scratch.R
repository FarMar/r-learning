## Tutorial with Terry from ANU

library(ggplot2)
library(emmeans)

##load and plot wheat data
setwd(data)
wheat<-read.csv("data/wheat yield.csv")
str(wheat)
View(wheat)
wheat$Variety<-relevel(wheat$Variety, ref="Standard")
ggplot(wheat, aes(Variety, Yield, colour-Variety)) + geom_point() + facet_wrap(~Variation)

##subset data
wheat_H<-subset(wheat,Variation=="High")
wheat_L<-subset(wheat,Variation=="Low")

##cheeky t-test
t.test(Yield~Variety, data=wheat_H, var.equal=TRUE)

##Fit linear models
lm1<-lm(Yield ~ Variety, data = wheat_L)
lm1a<-lm(Yield ~ Variety, data = wheat_H)

##Test linear models
anova(lm1)
anova(lm1a)

#Summaries
summary(lm1)
summary(lm1a)

#EM Means
emmeans(lm1, ~Variety)
emmeans(lm1a, ~Variety)

#residuals plots
plot(lm1, which=1)


###########################################################################

#New variety
setwd(data)
wheatP<-read.csv("data/wheat yield PLUS.csv")
str(wheatP)
View(wheatP)
wheatP$Variety<-relevel(wheatP$Variety, ref="Standard")
ggplot(wheatP, aes(Variety, Yield, colour-Variety)) + geom_point() 
lm2<-lm(Yield ~ Variety, data = wheatP)
aov(lm2)
anova(lm2)
summary(lm2)
emmeans(lm2, pairwise~Variety)
plot(lm2, which=c(1,2))

###########################################################################
## The pathway to understanding data

## 1) Import / organise data

setwd("~/DATASCHOOL/stats-terry/data")
data3<- read.csv("Prac 3 mock LWR.csv")

## 2) Visualise data

ggplot(data3, aes(GeneB,LWR,colour=GeneA)) + 
  geom_boxplot() + geom_point(position = position_dodge(width = .75)) 

ggplot(data3, aes(GeneB,LWR)) + 
  geom_boxplot() + geom_point(position = position_dodge(width = .75)) 

ggplot(data3, aes(GeneA,LWR)) + 
  geom_boxplot() + geom_point(position = position_dodge(width = .75)) 

## 3) Model your data
library(emmeans)
lm3<-lm(LWR~GeneA*GeneB, data=data3) #Interaction is denoted by the '*'
anova(lm3)
summary(lm3)
emmeans(lm3, pairwise~GeneA|GeneB)

## 4) Assess model assumptions
plot(lm2, which=1:2)

########################################################################
##Cabbages
cabbage<-read.csv("Prac 3 cabbage data.csv")
str(cabbage)
ggplot(cabbage,aes(Date,VitC,colour=Cult))+geom_boxplot()+ geom_point(position = position_dodge(width = .75))
ggplot(cabbage,aes(Cult,VitC,colour=Date))+geom_boxplot()+ geom_point(position = position_dodge(width = .75)) 

# Full factorial
lm4<-lm(VitC~Cult*Date, data=cabbage)
anova(lm4)

#additive as no interaction
lm5<-lm(VitC~Cult+Date, data=cabbage)
anova(lm5)
emmeans(lm5,pairwise~Cult)

#########################################################################
##Barley
library(tidyverse)
barley<-read_csv("Prac 3 barley yield.csv")

##Restructure to be tidy
barley2<-gather(barley, key="year", value="yield", 4:5)
View(barley2)
str(barley2)

##Visualise
ggplot(barley2,aes(Var,yield,colour=year))+
  geom_boxplot()+
  geom_point(position = position_dodge(width = .75))
ggplot(barley2,aes(Var,yield,colour=year))+
  geom_boxplot()+
  geom_point(position = position_dodge(width = .75))+
  facet_wrap(vars(Loc))
ggplot(barley2,aes(Var,yield,colour=Loc))+
  geom_boxplot()+
  geom_point(position = position_dodge(width = .75))+
  facet_wrap(vars(year))

#Model data
lm6<-lm(yield~Var+Loc, data=barley2)
anova(lm6)

lm7<-lm(yield~Var+Loc+year, data=barley2)
anova(lm7)

lm6a<-lm(yield~Var, data=barley2)
anova(lm6a)

#Rule of thumb on inclusion of interaction - number of parameters 
#in model should not exceed sqrt total df

emmeans(lm7,pairwise~Var)
help("emmeans-package")

############################################################################
##Forests
forest<-read.csv("Prac 3 forest.csv")
str(forest)
View(forest)

ggplot(forest,aes(QuadDiam, Density,colour=StandType)) + geom_point() + geom_smooth(method = lm, se=FALSE)

lm8<-lm(Density~QuadDiam*StandType, data = forest)
anova(lm8)
summary(lm8)
emmeans(lm8,pairwise~StandType)

lm9<-lm(Density~QuadDiam+StandType, data = forest)
anova(lm9)
summary(lm9)
emmeans(lm9,pairwise~StandType)


plot(lm8, which=1:2)
plot(lm9, which=1:2)

#############################################################################
##Ryan's data
ryan<-read_csv("herbicide_raw_data.csv")
ryan2<-transform(ryan, Block = as.factor(Block))
ryan3<-transform(ryan2, ara = as.numeric(ara))
withtotN <- mutate(ryan3, bgN + shootN)
str(withtotN)
names(withtotN)[16] <- "totN"

ggplot(withtotN,aes(Species,totN,colour=Herbicide))+
  geom_boxplot()+
  geom_point(position = position_dodge(width = .75))+
  facet_wrap(vars(Rhizobia))

ggplot(withtotN,aes(Species,totN,colour=Rhizobia))+
  geom_point(position = position_dodge(width = .75))+
  facet_wrap(vars(Herbicide))

ggplot(withtotN,aes(Species,ara,colour=Herbicide))+
  geom_boxplot()+
  geom_point(position = position_dodge(width = .75))+
  facet_wrap(vars(Rhizobia))

ggplot(withtotN,aes(Species,ara,colour=Rhizobia))+
  geom_point(position = position_dodge(width = .75))+
  facet_wrap(vars(Herbicide))

ggplot(withtotN,aes(Species,log(totN),colour=Herbicide))+
  geom_boxplot()+
  geom_point(position = position_dodge(width = .75))+
  facet_wrap(vars(Rhizobia))

lm_ryan<-lm(log(totN)~Species*Herbicide*Rhizobia + Block, data = withtotN)
anova(lm_ryan)
summary(lm_ryan)

ggplot(withtotN,aes(Rhizobia,log(totN),colour=Herbicide))+geom_boxplot()+
  geom_point(position = position_dodge(width = .75))

plot(lm_ryan, which = 1:2)

emmeans(lm_ryan,~Species*Herbicide*Rhizobia, type="response")
emmeans(lm_ryan,pairwise~Herbicide|Species*Rhizobia, type="response")

######################################################################
#Wrap-up hints on data analysis
#0) Sort data in form sample#/Block etc, Factors of interest, responses
#1) Import data, check structure, fix up inconsistencies
#2) Visualise data, explore relationships visually, try to understand important patterns to test
#3) Fit a model, then analyse with ANOVA, summary, emmeans, std errors
#4) Check model assumptions [plot(model, which = 1:2)]
#5) Finalise figures, error bars, etc. Add annotations (*,**,***; a,b,c)