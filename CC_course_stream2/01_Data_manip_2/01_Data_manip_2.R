# EFFICIENT DATA MANIPULATION - USE PIPES TO STREAMLINE YOUR CODE
# Course URL: https://ourcodingclub.github.io/tutorials/data-manip-efficient/

# Libraries
library(dplyr)
library(ggplot2)

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
