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
(hist_col <- ggplot(species_counts, aes(x = plot, y = Species_number)) +
    geom_col()
  )

# That looks a bit better, but it still seems to have far too many species. That’s because plots from 
# each fictitious land are being grouped together. We can separate them by introducing a colour code, 
# and make a stacked bar plot like this:

(hist_stack1 <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity")
)

# Still  pretty useless graph... Use `dodge` to put them where they belong

(hist_side1 <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge")
)

# Now to make it a bit prettier
# Note how our figure only shows plot numbers 2, 4, and 6. If you want the axis to display every plot number, 
# 1 - 6, you can run the following code using breaks = c(1,2,3,4,5,6) or using breaks = 1:6. We can also specify 
# the limits of the plot axes - running the code below, you’ll be able to see that the limit of the y axis now 
# extends to the value of 50! This helps us keep all our data within the axis labels that we have, in terms of 
# the visualisation!

(hist_side2 <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") +
    scale_x_continuous(breaks = c(1,2,3,4,5,6)) +
    scale_y_continuous(limits = c(0, 50))
)

# Time for labels and such

(hist_side2 <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") +
    scale_x_continuous(breaks = c(1,2,3,4,5,6)) +
    scale_y_continuous(limits = c(0, 50)) +
    labs(title = "Species richness by plot",
         subtitle = "In the magical lands",
         caption = "Data from the Ministry of Magic",
         x = "\n Plot number",
         y = "Number of species\n")
)

# Moar control with "themes"
(hist_side2 <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") +
    scale_x_continuous(breaks = c(1,2,3,4,5,6)) +
    scale_y_continuous(limits = c(0, 50)) +
    labs(title = "Species richness by plot",
         subtitle = "In the magical lands",
         caption = "Data from the Ministry of Magic",
         x = "\n Plot number",
         y = "Number of species\n") +
    theme(axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12, face = "italic"), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"))
)

# Fix the horrid default background
(hist_side2 <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") +
    scale_x_continuous(breaks = c(1,2,3,4,5,6)) +
    scale_y_continuous(limits = c(0, 50)) +
    labs(title = "Species richness by plot",
         subtitle = "In the magical lands",
         caption = "Data from the Ministry of Magic",
         x = "\n Plot number",
         y = "Number of species\n") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12, face = "italic"), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"))
)

# Legend and colours
(hist_side2 <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") +
    scale_x_continuous(breaks = c(1,2,3,4,5,6)) +
    scale_y_continuous(limits = c(0, 50)) +
    scale_fill_manual(values = c("rosybrown1", "#deebf7"),     # specifying the colours
                      name = "Land of Magic") +                # specifying title of legend
    labs(title = "Species richness by plot",
         x = "\n Plot number",
         y = "Number of species\n") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12, face = "italic"), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.title = element_text(face = "bold"),
          legend.position = "bottom", 
          legend.box.background = element_rect(color = "grey", size = 0.3))
)

# Better labels - note you need to match the order with how the data are actually presented
(hist_side2 <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") +
    scale_x_continuous(breaks = c(1,2,3,4,5,6)) +
    scale_y_continuous(limits = c(0, 50)) +
    scale_fill_manual(values = c("rosybrown1", "#deebf7"),     # specifying the colours
                      labels = c("HOGSMEADE", "NARNIA"),       # changing the site labels
                      name = "Land of Magic") +                # specifying title of legend
    labs(title = "Species richness by plot",
         x = "\n Plot number",
         y = "Number of species\n") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12, face = "italic"), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.title = element_text(face = "bold"),
          legend.position = "bottom", 
          legend.box.background = element_rect(color = "grey", size = 0.3))
)
