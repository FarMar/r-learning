#####################################################################################################

#### Mark Farrell's trials and tribulations with Coding Club's Spatial Data  ########################

#### mark.farrell@csiro.au        +61 8 8303 8664         25/05/2020 ################################

#### Their webpage is here: https://ourcodingclub.github.io/tutorials/spatial/ ######################

## NB: probably not my intention to get good at this, but a "good enough" state would be the ########
## application of these skills to map the FCNSW flooding extent. Another would be to do the #########
## definitive rainfall map for the SA transect. #####################################################

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
b1 <- raster("taycrop.tif", band = 1)
b2 <- raster("taycrop.tif", band = 2)
b3 <- raster("taycrop.tif", band = 3)
b4 <- raster("taycrop.tif", band = 4)
b5 <- raster("taycrop.tif", band = 5)
b6 <- raster("taycrop.tif", band = 6)
b7 <- raster("taycrop.tif", band = 7)
b8 <- raster("taycrop.tif", band = 8)
b9 <- raster("taycrop.tif", band = 9)
b10 <- raster("taycrop.tif", band = 10)
b11 <- raster("taycrop.tif", band = 11)
b12 <- raster("taycrop.tif", band = 12)

# compare two bands to see if they have the same extent, number of rows and column, projection, resolution and origin
compareRaster(b2, b3)

# View a raster
plot(b8)
image(b8)

# zoom in on a raster
plot(b8)
zoom(b8)

plot(tay)
e <- drawExtent()
cropped_tay <- crop(b7, e)
plot(cropped_tay)

# Visulaise spectral bands using `viridis` and save
#Save
png("tayplot.png", width = 4, height = 4, units = "in", res = 300)
image(b8, col = viridis_pal(option = "D") (10), main = "Sentinel 2 image of Loch Tay")
dev.off()

#Visualise
image(b8, col = viridis_pal(option = "D") (10), main = "Sentinel 2 image of Loch Tay")




