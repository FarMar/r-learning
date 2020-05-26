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

# A useful way to visualise the satellite data is to plot a red-green-blue plot of a multi-layered object 
# for a more realistic rendition. The layers or bands represent different bandwidth in the visible electromagnetic 
# spectrum (corresponding to red, blue and green) and combined, create a naturalistic colour rendition of the 
# earth surface.

# First, create a raster stack, a multi-layered raster object, of the red(b4), green(b3) and blue(b2) bands.

tayRGB <- stack(list(b4, b3, b2))

# Plot the raster stack

plotRGB(tayRGB, axes = TRUE, stretch = "lin", main = "Sentinel RGB colour composite image")

### False colour composites
# Another popular way to visualise remote sensing data is using a false colour composite (FCC), where the 
# red, green, and blue bands have been replaced in order to accentuate vegetation. In a FCC, the red bands is 
# replaced by the near infrared band (band 8 in Sentinel 2), the green band by red and the blue band by green. 
# This creates an image where the vegetation stands out in red. Check `(help(plotRGB))`` for more information 
# and other arguments for the function.

# The package rasterVis provides a number of ways to enhance the visualisation and analysis of raster data, 
# as can be seen on the package’s website. The function levelplot allows level and contour plots to be 
# made of raster objects with elevation data, such as LIDAR and plot3D allows 3D mapping. We do not have elevation 
# data from Sentinel 2, but the package’s gplot function allows us to plot a uni or multivariate raster object 
# using ggplot2 like syntax.

(gplot(b8) +
  geom_raster(aes(x = x, y = y, fill = value)) + 
  scale_fill_viridis_c() +
  coord_quickmap() +
  ggtitle("West of Loch Tay, raster plot") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size=20),
        axis.text.x = element_text(angle = 90, hjust = 1))
)

# To visualise all the bands together, we can use facet_wrap in gplot. First, we will create a stack of all the 
# bands, so just putting them all on top of each other, like layers in a cake.

t <- stack(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12)

# Now we are ready to make out facetted plots

(gplot(t) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    scale_fill_viridis_c() +
    facet_wrap(~variable) +
    coord_quickmap() +
    ggtitle("Sentinel 2 Loch tay, raster plots") +
    xlab("Longitude") +
    ylab("Latitude") +
    theme_classic() +
    theme(text = element_text(size=20),
          axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(plot.title = element_text(hjust = 0.5))
  )

# Alternatively, for a quick visualisation, the original file can be loaded as a raster brick and 
# plotted using ‘plot’.

s_tay <- brick("taycrop.tif")
plot(s_tay)

# Different earth surfaces reflect the solar radiation differently and each raster layer represents how much 
# incident solar radiation is reflected at a particular wavelength bandwidth. Bands 6 to 9 are in the Near 
# Infrared Range (NIR). Vegetation reflects more NIR than other wavelengths but water absorbs NIR, therefore the 
# lighter areas with high reflectance values are likely to be vegetation and the dark blue, low reflectance value 
# areas, likely to be water. Also note that the Sentinel 2 bands have 3 levels of spatial resolution, 10 m, 20 m, 
# and 60 m (see summary below).
# 10 m resolution band 2, band 3, band 4 and band 8
# 20 m resolution band 5, band 6, band 7, band 11 and band 12
# 60 m resolution band 1, band 9 and band 10

### NDVI and other useful indices

# The NDVI ratio is calculated using (NIR - Red) / (NIR + Red). For example, a pixel with an NDVI of less than 0.2 
# is not likely to be dominated by vegetation, and an NDVI of 0.6 and above is likely to be dense vegetation. In R,
# we can calculate the NDVI by creating a function and using raster math operations where NIR = band 8 and 
# Red = band 4 in Sentinel 2 images. We will first use the raster brick we created earlier from the original file.

## Create a Vegetation Index (VI) function
VI <- function(img, k, i) { # info in brackets provides help on exected format
  bk <- img [[k]]
  bi <- img [[i]]
  vi <- (bk - bi) / (bk + bi)
  return(vi)
  }

# For Sentinel 2, the relevant bands to use are:
# NIR = 8, red = 4

ndvi <- VI(s_tay, 8, 4) # following the function's inputs of image name, NIR band, red band
plot(ndvi, col = rev(terrain.colors(10)), main = "Sentinel 2, Loch Tay NDVI")

