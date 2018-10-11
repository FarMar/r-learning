## Chapter 4 - How to incorporate components of a statistical model into the model statement

library("tidyverse")
library("emmeans")
library("lmerTest")

setwd("~/DATASCHOOL/r-learning/stats-terry/")
photosynthesis<-read_csv("data/working/Prac 4 photosynthesis.csv")
str(photosynthesis)
photosynthesis$Position<-factor(photosynthesis$Position)
photosynthesis$Temp<-factor(photosynthesis$Temp)


ggplot(photosynthesis,aes(Temp,PhotoRate,colour=Position))+
  geom_point()

model1=lm(PhotoRate~Temp, data=photosynthesis)
anova(model1)  

# no sig impact of temp on psn - not particularly what we expected. Let's include the block:

model2=lmer(PhotoRate~Temp+(1|Position), data=photosynthesis)
anova(model2)
