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

## Constructing a peakData object.
## This is how the package ties the three aspects of the data together

peakObj <- as.peakData(ftms12T_edata, ftms12T_fdata, ftms12T_emeta, # The three data frames, in this order
                       edata_cname = "Mass",                        # Column name for edata - must match name in df
                       fdata_cname = "SampleID",                    # Column name for fdata
                       mass_cname = "Mass",                         # Column name for emeta Mass
                       c_cname = "C",
                       h_cname = "H",
                       o_cname = "O",
                       n_cname = "N",
                       s_cname = "S",
                       p_cname = "P",
                       isotopic_cname = "C13",
                       isotopic_notation = "1"
                       )

peakObj
names(peakObj)

# During construction the empirical formula is calculated
tail(peakObj$e_meta)

# Useful summary
summary(peakObj)

# Basic plot
plot(peakObj)

# Normalising
# When dealing with ’omics data quantitatively, we often log-transform to stabilize variances and reduce skew 
# for downstream data processing. Alternatively, it’s common to treat FT-MS data as presence/absence data. We 
# can use the edata_transform function to transform the data scale for either of these options.

peakObj <- edata_transform(peakObj, data_scale = "log2")

# for presence/absence transformation:
# `edata_transform(peakObj, data_scale="pres")`

peakObj
plot(peakObj)

## Calculating meta-data





