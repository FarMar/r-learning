
#####################################################################################################

#### Mark Farrell's trials and tribulations with Coding Club's Modelling tutorials ##################

#### mark.farrell@csiro.au        +61 8 8303 8664         29/05/2020 ################################

#### Their webpage is here: https://ourcodingclub.github.io/tutorials/model-design/index.html########

#####################################################################################################

### Set working directory



### Install packages as needed
install.packages(c("lme4", "sjPlot", "ggeffects", "MCMCglmm", "MCMCvis", "brms", "stargazer"))


### Load packages

library(dplyr)
library(ggplot2)
library(lme4)
library(sjPlot)
library(ggeffects)
library(MCMCglmm)
library(MCMCvis)
library(brms)
library(stargazer)


## Import data