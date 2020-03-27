# EFFICIENT DATA MANIPULATION - USE PIPES TO STREAMLINE YOUR CODE
# Course URL: https://ourcodingclub.github.io/tutorials/data-manip-efficient/

# Libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Load data
trees <- read.csv(file = "trees.csv", header = TRUE)
head(trees)


### Count the number of trees by species
## stepwise first:
# create an internal grouping structure, so that the next function acts on groups (here, species) separately. 
trees.grouped <- group_by(trees, CommonName) 

# here we use length to count the number of rows (trees) for each group (species). We could have used any row name.
trees.summary <- summarise(trees.grouped, count = length(CommonName))

## Alternatively, dplyr has a tally function that does the counts for you!
trees.summary <- tally(trees.grouped)

## Count the number of trees for each species, with a pipe!

trees.summary <- trees %>% 
  group_by(CommonName) %>% 
  tally()

## Summarising - quick way to generate summary dataframe for all variables
sum.all <- summarise_all(trees, mean)
# Not much use for this dataset as only two columns (Easting, Northing) are numeric

## Reclassifying values or factors
vector <- c(4, 13, 15, 6) # just creating a vector to evaluate
ifelse(vector <10, "A", "B") # follows target, condition, if true, if false

# The super useful case_when() is a generalisation of ifelse() that lets you assign more than two outcomes. 
# All logical operators are available, and you assign the new value with a tilde ~. For instance:

vector2 <- c("What am I?", "A", "B", "C", "D")
case_when(vector2 == "What am I?" ~ "I am the walrus",
          vector2 %in% c("A", "B") ~ "goo",
          vector2 == "C" ~ "ga",
          vector2 == "D" ~ "joob")

## Changing factor levels or creating categorical variables
# This code is using `grepl` to pull the Genus from the latin names and create a new column
# Now would be a good time to revisit RegEx implementations in R....

trees.genus <- trees %>% mutate(Genus = case_when(
    grepl("Acer", LatinName) ~ "Acer",
    grepl("Fraxinus", LatinName) ~ "Fraxinus",
    grepl("Sorbus", LatinName) ~ "Sorbus",
    grepl("Betula", LatinName) ~ "Betula",
    grepl("Populus", LatinName) ~ "Populus",
    grepl("Laburnum", LatinName) ~ "Laburnum",
    grepl("Aesculus", LatinName) ~ "Aesculus", 
    grepl("Fagus", LatinName) ~ "Fagus",
    grepl("Prunus", LatinName) ~ "Prunus",
    grepl("Pinus", LatinName) ~ "Pinus",
    grepl("Sambucus", LatinName) ~ "Sambucus",
    grepl("Crataegus", LatinName) ~ "Crataegus",
    grepl("Ilex", LatinName) ~ "Ilex",
    grepl("Quercus", LatinName) ~ "Quercus",
    grepl("Larix", LatinName) ~ "Larix",
    grepl("Salix", LatinName) ~ "Salix",
    grepl("Alnus", LatinName) ~ "Alnus"))

# Yes, lots of typing, but try it in Excel...

# Now a magic way to do it even quicker:
# We read in the "trees" data frame, pipe it to `separate` and pull out the LatinName column
# We then create a vector with two columns, "Genus" and "Species", populate it with LatinName, separated by the 
# space in the binomial. The "FALSE" flag means that the original column isn't removed from the piped dataset.
# `select(-Species)` then unselects the species column in the vector for inclusion in the new data frame` 

trees.genus.2 <- trees %>% 
  tidyr::separate(LatinName, c("Genus", "Species"), sep = " ", remove = FALSE) %>% 
  dplyr::select(-Species)

## The next task is to condense the Height factor from five levels to three

trees.genus.2 <- trees.genus.2 %>% #overwriting existing data frame
  mutate(Height.cat = #creating new column
           case_when(Height %in% c("Up to 5 meters", "5 to 10 meters") ~ "Short",
                     Height %in% c("10 to 15 meters", "15 to 20 meters") ~ "Medium",
                     Height == "20 to 25 meters" ~ "Tall")
         )

## Reordering factor levels
# R will always default to alphabetical, which isn't usually that useful
# Reordering them to a more logical order is very useful for figure legends etc.

levels(trees.genus.2$Height.cat)
trees.genus.2$Height.cat <- factor(trees.genus.2$Height.cat,
                                   levels = c("Short", "Medium", "Tall"),
                                   labels = c("SHORT", "MEDIUM", "TALL")
                                   )
levels(trees.genus.2$Height.cat)
