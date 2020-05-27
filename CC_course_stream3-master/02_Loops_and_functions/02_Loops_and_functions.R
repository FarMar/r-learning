#####################################################################################################

#### Mark Farrell's trials and tribulations with Coding Club's Modelling tutorials ##################

#### mark.farrell@csiro.au        +61 8 8303 8664         27/05/2020 ################################

#### Their webpage is here: https://ourcodingclub.github.io/tutorials/funandloops/index.html ########

#####################################################################################################

### Set working directory



### Install packages as needed



### Load packages

library(dplyr)
library(ggplot2)


## Import data
trees_bicuar <- read.csv("trees_bicuar.csv")
trees_mlunguya <- read.csv("trees_mlunguya.csv")

# The data contains information on tree stems surveyed in four 1 Ha plots at fieldsites around southern Africa. 
# trees_bicuar contains data for trees in Bicuar National Park in southwest Angola, and trees_mlunguya contains 
# data for trees in southern Mozambique. Each tree stem >5 cm trunk diameter was measured for tree height and 
# trunk diameter, and identified to species.


