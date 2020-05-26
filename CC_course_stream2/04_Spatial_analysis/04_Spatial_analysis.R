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

# We can now analyse the calculated rastor file to show the distribution of NDVI in the file. This would be
# really useful for the forestry project, particularly if coupled with moisture data from sentinel. I could then
# establish driest and wettest times in the forest to relate to soil properties measured and plant health / 
# productivity. This definitely needs exploring further, particularly extracting numeric data from the raster
# file for statistical analysis. First task will be to work out how to apply a shape file to crop the raster.

hist(ndvi,
     main = "Distribution of NDVI values",
     xlab = "NDVI",
     ylab = "Frequency",
     col = "red",
     xlim = c(-0.5, 1),
     breaks = 30,
     xaxt = "n")

# Or, to add x-axis ticks and output as a .png:

png('ndvihist.png', width = 4, height = 4, units = "in", res = 300)
hist(ndvi,
     main = "Distribution of NDVI values",
     xlab = "NDVI",
     ylab = "Frequency",
     col = "red",
     xlim = c(-0.5, 1),
     breaks = 30,
     xaxt = "n")
axis(side = 1, at = seq(-0.5, 1, 0.05), labels = seq(-0.5, 1, 0.05))
dev.off()

# Mask cells that have NDVI of less than 0.4 (less likely to be vegetation)
# Reclassifying object and making all values between negative infinity and 0.4 be NAs
veg <- reclassify(ndvi, cbind(-Inf, 0.4, NA))
plot(veg, main = "Vegetation cover")

## Writing the processed raster
writeRaster(x = ndvi,
            filename = "tay_ndvi_2018.tif",
            format = "GTiff",
            datatype = "INT2S"
            )

tayNDVI <- raster("tay_ndvi_2018.tif")
tayNDVI
plot(tayNDVI)

# hmm, very lossy

writeRaster(x = ndvi,
            filename = "tay_ndvi_2018_float.tif",
            format = "GTiff",
)

tayNDVIfloat <- raster("tay_ndvi_2018_float.tif")
tayNDVIfloat
plot(tayNDVIfloat)

# Still lossy, need to read up on this going forward

### Unsupervised classification
# Raster operations also allow us to perform an unsupervised classification, or a clustering of the pixels, 
# in the satellite image. In this context, unsupervised means that we are not using training data for the 
# clustering. This type of classification can be useful when not a lot is known about an area. In the example 
# below, we are going to use the kmeans algorithm. The algorithm groups pixels that have similar spectral 
# properties in the same cluster. We are going to create 10 clusters using the NDVI raster we have just created 
# above, but first, we need to convert the raster into an array, which is the object format required for the 
# classification.

# This might be useful to see how the 40 sites we have in the NSW forest fall regarding moisture and NDVI. Needs
# more reading...

# convert the raster to vector/matrix ('getValues' converts the RasterLAyer to array)
nr <- getValues(ndvi)
str(nr)

# important to set the seed generator because `kmeans` initiates the centres in random locations the seed 
# generator just generates random numbers
set.seed(99)

# create 10 clusters, allow 500 iterations, start with 5 random sets using 'Lloyd' method
kmncluster <- kmeans(na.omit(nr), 
                     centers = 10, 
                     iter.max = 500,
                     nstart = 5,
                     algorithm = "Lloyd"
                     )
str(kmncluster)

# Kmeans returns an object with 9 elements. The length of the cluster element within kmncluster is 429936 which 
# is the same as the length of nr created from the ndvi object. The cell values of kmncluster$cluster range between 
# 1 to 10 corresponding to the input number of clusters we provided in the kmeans() function. kmncluster$cluster 
# indicates the cluster label for the corresponding pixel. Our classification is now complete, and to visualise 
# the results, we need to convert the kmncluster$cluster array back to a RasterLayer of the same dimension as the 
# ndvi object.

# First create a copy of the ndvi layer
knr <- ndvi

# Now replace raster cell values with kmncluster$cluster array
knr[] <- kmncluster$cluster

# Alternative way to achieve the same result
values(knr) <- kmncluster$cluster
knr

# We can see that knr is a RasterLayer with 429,936 cells, but we do not know which cluster (1-10) belongs what 
# land cover or vegetation type. One way of attributing a class to a land cover type is by plotting the cluster 
# side-by-side with a reference layer of land cover and using unique colours for each cluster. As we don’t have one 
# for our example area, we can use the NDVI map we created earlier or the RGB plot.

par(mfrow = c(1, 2))
plot(ndvi, col = rev(terrain.colors(10)), main = "NDVI")
plot(knr, col = viridis_pal(option = "D")(10), main = "Kmeans")
