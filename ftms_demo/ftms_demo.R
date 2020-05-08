## Mark Farrell's play with the `ftmsRanalysis` package

# Find libraries
install.packages("devtools")
devtools::install_github("EMSL-Computing/ftmsRanalysis")

# Attach packages

library(ftmsRanalysis)

##### Here goes...

### Info on data types
## e_data (Expression Data) - df with one row per peak and one column per sample
## It must have one column that has a unique id (e.g. mass)

data("ftms12T_edata")
str(ftms12T_edata)

## f_data (Sample Data) - df with data on the experimental parameters
## It must have a column that matches the sample column names in `e_data`

data("ftms12T_fdata")
str(ftms12T_fdata)

## e_meta (Molecular Identification Data) - The emeta object is a data frame with one row per peak 
## and columns containing other meta data. Either a column giving the molecular formula or elemental 
## count columns (currently the elements C, H, O, S, N, and P are supported) are required. It must have an 
## ID column corresponding to the ID column in edata. If information about isotopic peaks is available and 
## specified, these peaks are currently filtered from the data upon peakData object creation.

data("ftms12T_emeta")
str(ftms12T_emeta)
