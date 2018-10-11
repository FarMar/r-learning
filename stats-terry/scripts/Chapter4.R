## Chapter 4 - How to incorporate components of a statistical model into the model statement

library("tidyverse")
library("emmeans")
library("lmerTest")

setwd("~/DATASCHOOL/r-learning/stats-terry/")

# Example 1 - impact of temperature on photsynthesis

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
summary(model2)
emmeans(model2,~Temp)
plot(model2)

# Example 2 - Can a gene KO Arabidopsis modulate leaf temperature during drought?

drought<-read_csv("data/working/Prac 4 drought data.csv")
str(drought)
drought$plant<-factor(drought$plant)

drought$Genotype <- factor(drought$Genotype, levels = c("WT", "mutant"))
drought$WaterCondition <- factor(drought$WaterCondition, levels = c("Normal", "Drought"))

drought$cond<-with(drought,interaction(WaterCondition, Genotype))
ggplot(drought, aes(cond, Temperature,colour=plant))+geom_point()

lm.drought<-lm(Temperature~Genotype*WaterCondition, data=drought)
anova(lm.drought)

lmer.drought<-lmer(Temperature~Genotype*WaterCondition+(1|plant), data=drought)
anova(lmer.drought)
