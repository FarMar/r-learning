#####################################################################################################

#### Mark Farrell's trials and tribulations with Coding Club's Data Viz part 2 ######################

#### mark.farrell@csiro.au        +61 8 8303 8664         25/04/2020 ################################

#### Their webpage is here: https://ourcodingclub.github.io/tutorials/datavis/ ######################

#####################################################################################################


### Set working directory
setwd("~/DATASCHOOL/r-learning/CC_course_stream2/03_Data_vis_2/")


### Install packages as needed



### Load packages

library(dplyr)
library(ggplot2)


## Import data
magic_veg <- read.csv("magic_veg.csv")
str(magic_veg)


#### STARTING WITH HISTOGRAMS

species_counts <- magic_veg %>% 
  group_by(land, plot) %>% 
  summarise(Species_number = length(unique(species)))

(hist <- ggplot(species_counts, aes(x = plot)) +
                  geom_histogram()
  )

# well, that's pretty crappy
# This is the common way of making a histogram, when you have one observation per row and the histogram 
# tallies them for you. But you can immediately see that it doesn’t look right, because we are working 
# with summarised data. You therefore need to tell R that you already know how many species are in each plot. 
# You do that by specifying the stat argument

(hist <- ggplot(species_counts, aes(x = plot, y = Species_number)) +
    geom_histogram(stat = "identity")
)

# `geom_col` would do the same thing:

