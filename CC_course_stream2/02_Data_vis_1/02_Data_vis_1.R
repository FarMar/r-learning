#####################################################################################################

#### Mark Farrell's trials and tribulations with Coding Club's Data Viz part 1 ######################

#### mark.farrell@csiro.au        +61 8 8303 8664         09/04/2020 ################################

#### Their webpage is here: https://ourcodingclub.github.io/tutorials/datavis/ ######################

#####################################################################################################


### Set working directory
setwd("~/DATASCHOOL/r-learning/CC_course_stream2/02_Data_vis_1/")


### Install packages as needed
install.packages(c("gridExtra", "readr")) #Use `c()` to make the list of packages a vector to install more than one


### Load packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(gridExtra)

## Import data
LPI <- read.csv("LPIdata_CC.csv")

## Convert data to long format
# By adding `9:53` we are selecting all columns from column 9 to column 53. These contain the monitoring data for each
# combination covered by columns 1-8

LPI2 <- gather(LPI, "year", "abundance", 9:53)

# We now have a very long dataset where year and abundance have been added as columns 9 & 10,
# and abundances that were previously in 9-53 are now in 10 with the year for each column in 9

# However, we're not there yet as we have that "X" in front of every year, so we need to use 
LPI2$year <- parse_number(LPI2$year)
str(LPI2)

# Abundance is still a character, let's fix that
LPI2$abundance <- as.numeric(LPI2$abundance)
str(LPI2)

# Given the size of the dataset, we're first going to identify a species to subset to and play with.
unique(LPI2$Common.Name) #lists all individual unique names, though console gets bored after 1000

# We'll stick with the example in the exercise and go with vultures
vulture <- filter(LPI2, Common.Name == "Griffon vulture / Eurasian griffon")
head(vulture)
vulture <- na.omit(vulture) #cull the NAs


####################################################################################################################

### Graphics time

## Base
base_hist <- hist(vulture$abundance)

## ggplot
vulture_hist <- ggplot(vulture, aes(x = abundance)) +
  geom_histogram()
vulture_hist

## gg better
(vulture_hist <- ggplot(vulture, aes(x = abundance)) +
    geom_histogram(binwidth = 250, colour = "brown", fill = "orange") +
    geom_vline(aes(xintercept = mean(abundance)),
               colour = "red", linetype = "dashed", size = 1) +
    theme_bw() +
    ylab("Count\n") + # `\n` adds a line below the label to give more space
    xlab("\nGriffon vulture abundance") +
    theme(axis.text = element_text(size = 12),
          axis.title.x = element_text(size = 14, face = "plain"), # face="plain" is the default, you can change it to italic, bold, etc.
          panel.grid = element_blank(),
          plot.margin = unit(c(1,1,1,1), units = , "cm"))  # Putting a 1 cm margin around the plot
    )

## Scatter plot to show how abundance changes in Italy and Croatia over time

vultureITCR <- filter(vulture, Country.list %in% c("Croatia", "Italy"))
plot(vultureITCR$year, vultureITCR$abundance, col=c("red", "green"))
(vulture_scatter <- ggplot(vultureITCR, aes(x = year, y = abundance, colour = Country.list)) +
                             geom_point(size = 2) +
    geom_smooth(method = "lm", aes(fill = Country.list)) +
    theme_bw() +
    scale_fill_manual(values = c("#EE7600", "#00868B")) +
    scale_colour_manual(values = c("#EE7600", "#00868B"),
                        labels = c("Croatia", "Italy")) +
    ylab("Griffon vulture abundance\n") +
    xlab("\nYear") +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),
          panel.grid = element_blank(),
          plot.margin = unit(c(1,1,1,1), units = , "cm"),
          legend.text = element_text(size = 12, face = "italic"),
          legend.title = element_blank(),
          legend.position = c(0.9, 0.9))
  )

## Boxplot to examine differences in abundance between the two countries

(vulture_boxplot <- ggplot(vultureITCR, aes(Country.list, abundance))
  + geom_boxplot()
  )

# Pretty...

(vulture_boxplot <- ggplot(vultureITCR, aes(Country.list, abundance)) +
    geom_boxplot(aes(fill = Country.list)) +
    theme_bw() +
    scale_fill_manual(values = c("#EE7600", "#00868B")) +
    scale_colour_manual(values = c("#EE7600", "#00868B")) +
    ylab("Griffon vulture abundance\n") +
    xlab("\nCountry") +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                     
          panel.grid = element_blank(),                                               
          plot.margin = unit(c(1,1,1,1), units = , "cm"),               
          legend.position = "none")
    )

## Bar plot to examine several countries. NB we don't like barplots, but this is appropriate as we're presenting
## calculated species richness data

# This chunk takes the LPI2 data, selects five countries, and eventually adds a column based upon how many unique 
# names (species in this case) are present in each country. It has however retained all the other info into the 
# new tibble

richness <- LPI2 %>% filter(Country.list %in% c("United Kingdom", "Germany", "France", "Netherlands", "Italy")) %>% 
  group_by(Country.list) %>% 
  mutate(richness = length(unique(Common.Name)))

(richness_barplot <- ggplot(richness, aes(Country.list, richness)) +
    geom_bar(position = position_dodge(), stat = "identity", colour = "black", fill = "#00868B")
  )

### Facet time

## First to show what a mess when not splitting between panels

(vulture_scatter_all <- ggplot(vulture, aes(x = year, y = abundance, colour = Country.list)) +
    geom_point(size = 2) +
    geom_smooth(method = "lm", aes(fill = Country.list)) +
    theme_bw() +
    ylab("Griffon vulture abundance\n") +
    xlab("\nYear") +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),     # making the years at a bit of an angle
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                        
          panel.grid = element_blank(),                                   # Removing the background grid lines               
          plot.margin = unit(c(1,1,1,1), units = , "cm"),                 # Adding a 1 cm margin around the plot
          legend.text = element_text(size = 12, face = "italic"),         # Setting the font for the legend text
          legend.title = element_blank(),                                 # Removing the legend title
          legend.position = "right")
  ) 

## With facets

(vulture_scatter_facet <- ggplot(vulture, aes(x = year, y = abundance, colour = Country.list)) +
    geom_point(size = 2) +
    geom_smooth(method = "lm", aes(fill = Country.list)) +
    facet_wrap(~ Country.list, scales = "free_y") +
    theme_bw() +
    ylab("Griffon vulture abundance\n") +
    xlab("\nYear") +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),     # making the years at a bit of an angle
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                        
          panel.grid = element_blank(),                                   # Removing the background grid lines               
          plot.margin = unit(c(1,1,1,1), units = , "cm"),                 # Adding a 1 cm margin around the plot
          legend.text = element_text(size = 12, face = "italic"),         # Setting the font for the legend text
          legend.title = element_blank(),                                 # Removing the legend title
          legend.position = "right")
) 

# Some useful arguments to include in facet_wrap()are nrow = or 
# ncol = , specifying the number of rows or columns, respectively. 
# You can also see that we used scales = "free_y", to allow different y axis values 
# because of the wide range of abundance values in the data. 
# You can use “fixed” when you want to constrain all axis values.

## Not quite facets, but graphs from different analyses can be arranged for a single figure
## using `grid.arrange()` from the package gridExtra.

grid.arrange(vulture_hist, vulture_scatter, vulture_boxplot, ncol = 1)

# clearly it's not quite so simple
# adding ylab etc. again overrides the parameters of the original figures

(panel <- grid.arrange(
  vulture_hist + ggtitle("(a)") + ylab("Count") + xlab("Abundance") +
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")),
          vulture_boxplot + ggtitle("(b)") + ylab("Abundance") + xlab("Country") +
            theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")),
                  vulture_scatter + ggtitle("(c)") + ylab("Abundance") + xlab("Year") +
                    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")) +
    theme(legend.text = element_text(size = 12, face = "italic"),
    legend.title = element_blank(),
    legend.position = c(0.85, 0.85)),
#  widths = c(1, 1, 1), # can only use when ncol =/= 1
  heights = c(1, 1, 2),
  ncol = 1
))

# Still not quite right in the renderer, but can be fixed for export easily enough
# using `ggsave`. Units are in inches by default, but can change by adding `units = "px"`
# or `units = "cm"` at the end:

ggsave(panel, file = "vulture_panel2.png", width = 5, height = 12)

#### The Challenge
### 1) Choose TWO species from the LPI data and display their population trends over time, 
###    using a scatterplot and a linear model fit?
###
### 2) Using the same two species, filter the data to include only records from FIVE 
###    countries of your choice, and make a boxplot to compare how the abundance of those 
###    two species varies between the five countries?


## Challenge 1
# Pick two most abundant species

top2 <- LPI2 %>% group_by(Common.Name) %>% 
                            summarise(abundance = sum(abundance, na.rm = TRUE)) %>% 
  top_n(2)


