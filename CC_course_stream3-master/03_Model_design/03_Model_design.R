
#####################################################################################################

#### Mark Farrell's trials and tribulations with Coding Club's Modelling tutorials ##################

#### mark.farrell@csiro.au        +61 8 8303 8664         29/05/2020 ################################

#### Their webpage is here: https://ourcodingclub.github.io/tutorials/model-design/index.html########

#####################################################################################################

### Set working directory



### Install packages as needed
install.packages(c("lme4", "sjPlot", "ggeffects", "MCMCglmm", "MCMCvis", "brms", "stargazer", "glmmTMB"))

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
library(glmmTMB)


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

### Models - all the usual stuff applies here that you've been doing for the past decade in SPSS
## Simple model - `I(Year-2007)` is turning the Year variable into a numerical series starting at 1 rather than 2008.
# The I(x) function is what allows the mathematical operator to be conducted on the variable. I assume this is one way
# of log-transform etc on the fly.

# Notice how we have transformed the Year column - I(Year - 2007) means that the year 2008 will become Year 1 - then 
# your model is estimating richness across the first, second, etc., year from your survey period. Otherwise, if we 
# had kept the years just as 2008, 2009,…, the model would have estimated richness really far back into the past, 
# starting from Year 1, Year 2… Year 1550 up until 2012. This would make the magnitude of the estimates we get wrong. 
# You can experiment to see what happens if we just add in Year - suddenly the slope of species change goes in the 
# hundreds!

plant_m <- lm(Richness ~ I(Year-2007), data = toolik_plants) 
summary(plant_m)

# check the residual versus predicted plot for our linear model. By using the ‘plot()’ function, we can plot the 
# residuals versus fitted values, a Q-Q plot of standardised residuals, a scale-location plot (square roots of 
# standardiaed residuals versus fitted values) and a plot of residuals versus leverage that adds bands corresponding 
# to Cook’s distances of 0.5 and 1. Looking at these plots can help you identify any outliers that have huge 
# leverage and confirm that your model has indeed run e.g. you want the data points on the Q-Q plot to follow the 
# one-to-one line.

plot(plant_m)

## Hierarchical models
# First, let’s model with only site as a random effect. This model does not incorporate the temporal replication in 
# the data or the fact that there are plots within blocks within those sites.

plant_m_plot <- lmer(Richness ~ I(Year-2007) + (1|Site), data = toolik_plants)
summary(plant_m_plot)
plot(plant_m_plot)

# Add in block
plant_m_plot2 <- lmer(Richness ~ I(Year-2007) + (1|Site/Block), data = toolik_plants)
summary(plant_m_plot2)
plot(plant_m_plot2)

# Add in plot - This final model answers our question about how plant species richness has changed over time, whilst 
# also accounting for the hierarchical structure of the data
plant_m_plot3 <- lmer(Richness ~ I(Year-2007) + (1|Site/Block/Plot), data = toolik_plants)
summary(plant_m_plot3)
plot(plant_m_plot3)

## Use `sjplot` to plot the model
# Set a 'clean' theme
set_theme(base = theme_bw() +
            theme(panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.major.y = element_blank(),
                  plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"))
            )

# Visualise ramdom effects:
(re.effects <- plot_model(plant_m_plot3, type = "re", show.values = TRUE))
# save_plot(filename = "model_re.png", height = 11, width = 9)  # Save the graph if you wish
# Note how when we visualise our random effects, three different plots come up. The first two show the interaction 
# effects. Here, we are only interested in the plot that shows us the random effects of site

# Visualise fixed effects:
(fe.effects <- plot_model(plant_m_plot3, show.values = TRUE))

## Including temperature
plant_m_temp <- lmer(Richness ~ Mean.Temp + (1|Site/Block/Plot) + (1|Year),
                     data = toolik_plants)
summary(plant_m_temp)
(temp.re.effects <- plot_model(plant_m_temp, type = "re", show.values = TRUE))
(temp.fe.effects <- plot_model(plant_m_temp, show.values = TRUE))

## Random slopes vs random intercepts
# We can now think about having random slopes and random intercepts. For our question, how does temperature 
# influence species richness, we can allow each plot to have it’s own relationship with temperature.

plant_m_rs <- lmer(Richness ~ Mean.Temp + (Mean.Temp|Site/Block/Plot) + (1|Year), data = toolik_plants)
summary(plant_m_rs)
# This model is not converging and we shouldn’t trust its outputs: the model structure is too complicated for the
# underlying data, so now we can simplify it.

plant_m_rs <- lmer(Richness ~ Mean.Temp + (Mean.Temp|Plot) + (1|Year),
                   data = toolik_plants)
summary(plant_m_rs)
# This one is not converging either! Let’s try with just a Plot random intercept and with random slopes to illustrate 
# what a random slope model looks like.

plant_m_rs <- lmer(Richness ~ Mean.Temp + (Mean.Temp|Plot),
                   data = toolik_plants)
summary(plant_m_rs)
(plant.re.effects <- plot_model(plant_m_rs, type = "re", show.values = TRUE))
(plant.fe.effects <- plot_model(plant_m_rs, show.values = TRUE))

# Something funny going on here as not getting plots as shown in tutorial

# We will use the ggeffects package to calculate model predictions and plot them. First, we calculate the 
# overall predictions for the relationship between species richness and temperature. Then, we calculate the 
# predictions for each plot, thus visualising the among-plot variation. Note that the second graph has both 
# freely varying slopes and intercepts (i.e., they’re different for each plot).

ggpredict(plant_m_rs, terms = c("Mean.Temp")) %>% plot()
ggpredict(plant_m_rs, terms = c("Mean.Temp", "Plot"), type = "re") %>% plot()

# take note of the y axis: it doesn’t actually start at zero
