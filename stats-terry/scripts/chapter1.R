### Chapter 1 of Terry's notes: t-tests, basic lm, basic data vis

install.packages("tidyverse")
install.packages("emmeans")

library("tidyverse")
library("emmeans")

## Exercise 1 - t-test

wheat <- read_csv("data/working/wheat yield.csv")
