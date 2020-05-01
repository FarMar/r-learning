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

ggsave( "images/magical-sp-rich-hist.png", plot = hist_side2, width = 7, height = 5, dpi = 300)


#### COLOUR PALETTES
# A more advanced use of colour palettes is to create one linked to your factor levels. This is great when you 
# work on a project that will have multiple figures, and you want the colour-coding to be consistent across the 
# board. Linking colours specifically to factor levels ensures that if a factor is dropped from a data frame, 
# the corresponding colour will be dropped from the resulting plot, too, instead of being reassigned to the next 
# available factor level.

# Here with only two magical lands, you could easily keep track of the colours, but imagine if you had 10 different 
# lands! Let’s create a fake dataframe of values for more magical lands, and see the power of this approach.

# I suspect this will be the starting point for many a script dealing with categoricals...
# For continuous we know about `scale_colour_gradient()`...

## Make some more magical lands by forming two vectors. NB must be in order if doing this for real
land <- factor(c("Narnia", "Hogsmeade", "Westeros", "The Shire", "Mordor", "Forbidden Forest", "Oz"))
counts <- as.numeric(c(55, 48, 37, 62, 11, 39, 51))

## Grab the vectors into a dataframe
more_magic <- data.frame(land, counts)

## Check how many factors there are - we'll need as many colours as factors
length(levels(more_magic$land))
# That'll be 7!

## CREATE THE COLOUR PALETTE

magic.palette <- c("#698B69", "#5D478B", "#5C5C5C", "#CD6090", "#EEC900", "#5F9EA0", "#6CA6CD") #Defining colours
names(magic.palette) <- levels(more_magic$land) #linking factor names to colours - must be in correct order

## Draw a bar chart
(hist <- ggplot(more_magic, aes(x = land, y = counts, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") + 
    scale_y_continuous(limits = c(0, 65)) +
    scale_fill_manual(values = magic.palette,                        # using our palette here
                      name = "Land of Magic") +                      # Title of legend
    labs(title = "Species richness in magical lands", 
         x = "", y = "Number of species \n") + 
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.text.x = element_text(angle = 45, hjust = 1), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.title = element_text(face = "bold"),
          legend.position = "bottom", 
          legend.box.background = element_rect(color = "grey", size = 0.3)))

## Drop some factors using %in% (neat trick!!) to show consistency of colours
(hist <- ggplot(filter(more_magic, land %in% c("Hogsmeade", "Oz", "The Shire")), 
                aes(x = land, y = counts, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") + 
    scale_y_continuous(limits = c(0, 65)) +
    scale_fill_manual(values = magic.palette,  # using our palette ensures that colours with no corresponding factors are dropped
                      name = "Land of Magic") +                
    labs(title = "Species richness in magical lands", 
         x = "", y = "Number of species \n") + 
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.text.x = element_text(angle = 45, hjust = 1), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.title = element_text(face = "bold"),
          legend.position = "bottom", 
          legend.box.background = element_rect(color = "grey", size = 0.3)))

### Boxplot time
# To make the boxplots, we will slightly reshape the dataset to take account of year as well.

yearly_counts <- magic_veg %>% 
  group_by(land, plot, year) %>%  # year is added here to create the third factor to group by in the plot
  summarise(Species_number = length(unique(species))) %>% 
  ungroup() %>% 
  mutate(plot = as.factor(plot))
