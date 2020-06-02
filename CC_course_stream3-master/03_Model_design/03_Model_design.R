
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
toolik_plants <- read.csv("toolik_plants.csv")


## Inspect data
head(toolik_plants)
str(toolik_plants)

# Make plot a character and factor
toolik_plants$Plot <- as.factor(as.character(toolik_plants$Plot))

# Get unique site names
unique(toolik_plants$Site)
length(unique(toolik_plants$Site))

# Within each site, there are different numbers of blocks: some sites have three sample blocks, others have 
# four or five
toolik_plants %>% group_by(Block) %>% 
  summarise(plot.n = length(unique(Plot)))
# Within each block, there are eight smaller plots.

unique(toolik_plants$Year)
# There are four years of data

# How many species are represented in this data set?
length(unique(toolik_plants$Species))

# Species list
unique(toolik_plants$Species)

## Data cleaning
# Plenty of things that don't appear to be species in there. Not pretty. Easiest way is to filter them out, but no 
# avoiding a lot of typing

toolik_plants <- toolik_plants %>% 
  filter(!Species %in% c("Woody cover", "Tube",
                         "Hole", "Vole trail",
                         "removed", "vole turds",
                         "Mushrooms", "Water",
                         "Caribou poop", "Rocks",
                         "mushroom", "caribou poop",
                         "animal litter", "vole poop",
                         "Vole poop", "Unk?"))
length(unique(toolik_plants$Species))

# Calculate species richness
toolik_plants <- toolik_plants %>%           # Points what we're about to do back to the original df
  group_by(Year, Site, Block, Plot) %>%      # Groups by the factors
  mutate(Richness = length(unique(Species)))  # Adds new column containing richness calcualted for each factor

# Quick histogram to visualise data shape
(hist <- ggplot(toolik_plants, aes(x = Richness)) +
    geom_histogram() +
    theme_classic()
  )

(hist2 <- ggplot(toolik_plants, aes(x = Relative.Cover)) +
    geom_histogram() +
    theme_classic()
  )

# Models - all the usual stuff applies here that you've been doing for the past decade in SPSS

plant_m <- lm(Richness ~ I(Year-2007), data = toolik_plants)
summary(plant_m)

# check the residual versus predicted plot for our linear model. By using the ‘plot()’ function, we can plot the 
# residuals versus fitted values, a Q-Q plot of standardised residuals, a scale-location plot (square roots of 
# standardiaed residuals versus fitted values) and a plot of residuals versus leverage that adds bands corresponding 
# to Cook’s distances of 0.5 and 1. Looking at these plots can help you identify any outliers that have huge 
# leverage and confirm that your model has indeed run e.g. you want the data points on the Q-Q plot to follow the 
# one-to-one line.

plot(plant_m)
