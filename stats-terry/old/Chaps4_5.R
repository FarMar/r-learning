setwd("DATASCHOOL/stats-terry/data")
library(tidyverse)
library(emmeans)
photo<-read.csv("Prac 4 photosynthesis.csv")
str(photo)
photo$Position<-factor(photo$Position)
photo$Temp<-factor(photo$Temp)
ggplot(photo,aes(Temp,PhotoRate,colour=Position))+
  geom_point()+
  labs(x="Temperature")

##lmerTest https://cran.r-project.org/web/packages/lmerTest/index.html
##Mixed linear effects model

install.packages("lmerTest")
library(lmerTest)
lmer1<-lmer(PhotoRate~Temp+(1|Position), data=photo)
anova(lmer1)
summary(lmer1)
emmeans(lmer1,~Temp)
plot(lmer1)
