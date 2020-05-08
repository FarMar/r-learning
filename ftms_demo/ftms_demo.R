## Mark Farrell's play with the `ftmsRanalysis` package

# Find libraries
install.packages("devtools")
devtools::install_github("EMSL-Computing/ftmsRanalysis")
devtools::install_github("delta-rho/datadr")

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
# It is frequently useful for biological analysis and interpretation to calculate values related to chemical 
# properties of each peak, such as the nominal oxidation state of Carbon (NOSC), aromaticity, and elemental 
# ratios. This can be done via the compound_calcs function. By default, this function calculates all available 
# meta-data fields, specific fields can be chosen with the calc_fns parameter.

peakObj <- compound_calcs(peakObj)
peakObj

# Classification of compounds based on their elemental composition is often desirable. The 
# `assign_elemental_composition` function accomplishes this task.

peakObj <- assign_elemental_composition(peakObj)
table(peakObj$e_meta[, getElCompColName(peakObj)])

# Further, each compound formula can also be assigned to biochemical compound classes (e.g. lipids, lignins, 
# etc.) based on their chemical properities (e.g. O:C, H:C ratios), and the `assign_class` function performs this 
# assignment.
# There are three sets of class boundary definitions that may be used (for the boundary_set parameter) 
# corresponding to the following publications:
# bs1 - Kim, S., et al (2003). Analytical Chemistry.
# bs2 - Bailey, V. et al (2017). Soil Biology and Biochemistry.
# bs3 - Rivas-Ubach, A., et al (2018). Analytical chemistry.

peakObj <- assign_class(peakObj, boundary_set = "bs1")
table(peakObj$e_meta[, getBS1ColName(peakObj)])

## Filtering
# There are multiple types of filtering algorithms provided in ftmsRanalysis:
# Molecule filter: filter rows of e_data to exclude rows observed in too few samples
# Mass filter: filter rows based on mass, e.g. to reflect observational sensitivity of the instrument
# Formula filter: filter rows based on whether they have a molecular formula
# Emeta filter: filter rows of e_data based on a quantity/column in e_meta

filter_obj <- mass_filter(peakObj)
plot(filter_obj, min_mass = 200, max_mass = 1000)

summary(peakObj)

# We then apply the filter to peakObj
peakObj <- applyFilt(filter_obj, peakObj, min_mass = 200, max_mass = 900)
summary(peakObj)

# Other filtering options include number of molecule observations, formula presence or absence, 
# or emeta columns.

peakObj <- applyFilt(molecule_filter(peakObj), peakObj, min_num = 2) # remove peaks in fewer than min_num samples
peakObj <- applyFilt(formula_filter(peakObj), peakObj) # filter peaks without a formula assigned
peakObj <- applyFilt(emeta_filter(peakObj, "NOSC"), peakObj, min_val = 0.5) #filter peaks outside specified range
                                                                            #NOSC = nominal oxidation state of Carbon

summary(peakObj)

## Visualisations of one sample
one_sample <- subset(peakObj, samples = "EM0011_sample")
summary(one_sample)

head(one_sample$e_data)

vanKrevelenPlot(one_sample, title = "EM0011_sample")
vanKrevelenPlot(one_sample, colorCName = "NtoC_ratio",
                title = "Colour by N:C ratio", legendTitle = "N:C ratio")

kendrickPlot(one_sample, title = "Kendrick plot for EM0011_sample")

densityPlot(one_sample, variable = "NOSC", plot_curve = TRUE, plot_hist = TRUE,
            title = "NOSC distribution for EM0011_sample")

densityPlot(one_sample, variable = "kmass.CH2", 
            title = "Kendrick mass for EM0011_sample",
            plot_hist = TRUE, plot_curve = FALSE, yaxis = "count")

## Comparison of experimental groups
#The goal of this experiment was to identify differences in soil organic matter between sample locations 
# and crop types. In order to do that we need to compare experimental treatments (groups).
# The `group_designation`` method defines treatment groups based on the variable(s) specified as main effects. 
# Here we define groups based on the crop/flora type.

peakObj <- group_designation(peakObj, main_effects = c("Crop.Flora"))
getGroupDF(peakObj)

# The summarizeGroups function calculates group-level summaries per peak, such as the number or proportion 
# of samples in which peak is observed. The resulting object’s e_data element contains one column per group, 
# per summary function.

group_summary <- summarizeGroups(peakObj, summary_functions = 
                                   c("n_present", "prop_present"))

head(group_summary$e_data)

densityPlot(peakObj, samples = FALSE, groups = c("S", "C"), variable = "NOSC",
            title = "Comparison of NOSC between crop types")

# We might also want to look at which peaks occur only in one group or another, versus those that appear 
# in both groups. The number or proportion of samples for which a peak must be observed can be specified 
# to determine if a peak was present for a group. Similarly, a threshold based on the number or proportion 
# of samples can be specified to determine when a peak is absent from a group. Alternatively, a statistical 
# test called the G-Test can be used. This is a likelihood ratio test which tests the hypothesis that the 
# presence/absence of a peak across samples is independent of group membership.

# The first step is to create peakData objects that each contain two groups to facilitate group comparisons

byGroup <- divideByGroupComparisons(peakObj,
                                    comparisons = "all") [[1]]$value

crop_unique <- summarizeGroupComparisons(byGroup,
                                         summary_functions = "uniqueness_gtest",
                                         summary_function_params = list(
                                           uniqueness_gtest = list(pres_fn = "nsamps",
                                                                   pres_thresh = 2,
                                                                   pvalue_thresh = 0.05)
                                         ))
tail(crop_unique$e_data)
vanKrevelenPlot(crop_unique, colorCName = "uniqueness_gtest")

