### Chapter 3 of Terry's notes: Mean structure and data modelling

#install.packages("tidyverse")
#install.packages("emmeans")

library("tidyverse")
library("emmeans")

setwd("~/DATASCHOOL/r-learning/stats-terry")

## Example 1 - can sorghum drought tolerance be improved through genetic modification?
# A two-factor ANOVA problem

data2<- read_csv("data/working/Prac 3 mock LWR.csv")

ggplot(data2, aes(GeneB,LWR,colour=GeneA)) + 
  geom_boxplot() + 
  geom_point(position=position_dodge(0.75))

lm1<-lm(LWR~GeneA*GeneB, data = data2)
anova(lm1)
summary(lm1)
emmeans(lm1, pairwise~GeneA|GeneB)
emmeans(lm1, pairwise~GeneB|GeneA)

plot(lm1, which=1:2)

## Example 2 - more explicit model choices
# Which cabbage cultivar has the higher vitamin C content on average?

cabbage<-read_csv("data/working/Prac 3 cabbage data.csv")
str(cabbage)

ggplot(cabbage,aes(Date,VitC,colour=Cult))+
  geom_boxplot() +
  geom_point(position=position_dodge(0.75))

#First model is fully factorial, assuming there is an interaction between planting day and cultivar

lm2<-lm(VitC~Cult*Date, data = cabbage)
anova(lm2)

#As it can be seen that there isn't an interaction, a more appropriate model is an additive model. 
#This is a better test of the question: "Does cultivar affect vitamin C levels?"

lm3<-lm(VitC~Cult+Date, data = cabbage)
anova(lm3)
emmeans(lm3, pairwise~Cult)

plot(lm3, which=1) 
plot(lm3, which=2)