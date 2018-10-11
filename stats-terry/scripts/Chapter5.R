## Chapter 5 - Understanding Error Structure II

library("tidyverse")
library("emmeans")
library("lmerTest")

setwd("~/DATASCHOOL/r-learning/stats-terry/")
respire<-read_csv("data/working/Prac 4 dark respiration.csv")
respire$Plant_ID<-factor(respire$Plant_ID)
respire$Leaf_stage<-factor(respire$Leaf_stage)
respire$Leaf_section<-factor(respire$Leaf_section)

respire$Species<-factor(respire$Species, labels=c("PM","UP","ZM"))

ggplot(respire, aes(Species, Dry_mass_resp,colour=Leaf_section))+geom_boxplot()
ggplot(respire, aes(Species, Dry_mass_resp,colour=Leaf_stage))+geom_boxplot()

model4<-lmer(Dry_mass_resp~Species*Leaf_stage*Leaf_section+(1|Plant_ID) + (1|Leaf_stage:Plant_ID),data=respire) 
anova(model4)
