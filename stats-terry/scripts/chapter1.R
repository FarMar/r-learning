### Chapter 1 of Terry's notes: t-tests, basic lm, basic data vis

#install.packages("tidyverse")
#install.packages("emmeans")

library("tidyverse")
library("emmeans")

## Exercise 1 - t-test

# Read in data, check structure etc
setwd("~/DATASCHOOL/r-learning/stats-terry")
wheat <- read_csv("data/working/wheat yield.csv")
str(wheat)
View(wheat)

# Fix up factors

as_factor(wheat$Variety)

# Set reference level to force plotting to show control as first item on x-axis

wheat$Variety <- factor(wheat$Variety, levels = c("Standard", "New"))

# Take a look at the data using ggplot

ggplot(wheat, aes(Variety, Yield, colour=Variety)) + 
  geom_point() + 
  facet_wrap(~Variation)

# Split the data 

wheat_H <- filter(wheat, Variation == "High")
wheat_L <- filter(wheat, Variation == "Low")

# T-tests
# Compare Varieties in “High Variation” subset

t.test(Yield~Variety, data = wheat_H, var.equal=TRUE)
t.test(Yield~Variety, data = wheat_L, var.equal=TRUE)
t.test(Yield~Variety, data = wheat_L, var.equal=FALSE)

# Simple linear models

lm1 <- lm(Yield ~ Variety, data = wheat_L)
anova(lm1)

# Review model parameters

summary(lm1)
emmeans(lm1, ~Variety)

# Check model assumptions

plot(lm1, which = 1)

# Report results from the experiment

lm1.results<-summary(emmeans(lm1,~Variety))

ggplot(lm1.results, aes(Variety, emmean, fill=Variety)) +
  geom_bar(stat="identity", width=.4) +
  geom_errorbar(aes(ymin = emmean-SE, ymax = emmean+SE), width=.2) + 
                  ylim(0,5) +
                  labs(y = 'Mean Yield (t/hectare)')
                       

## Second exercise - introducing three levels for Variety

wheat2 <- read_csv("data/working/wheat yield PLUS.csv")
str(wheat2)
View(wheat2)

wheat2$Variety <- factor(wheat2$Variety, levels = c("Standard", "New", "NewPlus"))

ggplot(wheat2, aes(Variety, Yield, colour=Variety)) + 
  geom_point()

# Anova, Tukey - note this is where emmeans gets very useful
lm2 <- lm(Yield ~ Variety, data = wheat2)
anova(lm2)
summary(lm2)
emmeans(lm2, pairwise~Variety)

# Check assumptions
plot(lm2,which=1)
plot(lm2,which=2)

# Final plot
lm2.results<-summary(emmeans(lm2,~Variety))

ggplot(lm2.results, aes(Variety, emmean, fill=Variety)) +
  geom_bar(stat="identity", width=.4) +
  geom_errorbar(aes(ymin = emmean-SE, ymax = emmean+SE), width=.2) + 
  ylim(0,4) +
  labs(y = 'Mean Yield (t/hectare)')
