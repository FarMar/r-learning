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

## Scatter plot to show how aundance changes in Italy and Croatia over time

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
