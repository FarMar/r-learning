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