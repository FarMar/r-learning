#####################################################################################################

#### Mark Farrell's trials and tribulations with Coding Club's Spatial Data  ########################

#### mark.farrell@csiro.au        +61 8 8303 8664         25/05/2020 ################################

#### Their webpage is here: https://ourcodingclub.github.io/tutorials/spatial/ ######################

#####################################################################################################


### Set working directory
setwd("~/DATASCHOOL/r-learning/CC_course_stream2/04_Spatial_analysis/")


### Install packages as needed
install.packages(c("sp", "rgdal", "raster", "viridis", "rasterVis"))


### Load packages
library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(viridis)
library(rasterVis)


## Import data
tay <- raster("taycrop.tif")

## Get properties for tay
tay

# We can create individual raster layers for each of the spectral bands in the raster tay.



