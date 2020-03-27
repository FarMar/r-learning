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

## Summarising - quick way to generate summary dataframe for all variables
sum.all <- summarise_all(trees, mean)
# Not much use for this dataset as only two columns (Easting, Northing) are numeric

## Reclassifying values or factors
vector <- c(4, 13, 15, 6) # just creating a vector to evaluate
ifelse(vector <10, "A", "B") # follows target, condition, if true, if false

#The super useful case_when() is a generalisation of ifelse() that lets you assign more than two outcomes. 
#All logical operators are available, and you assign the new value with a tilde ~. For instance:

vector2 <- c("What am I?", "A", "B", "C", "D")
case_when(vector2 == "What am I?" ~ "I am the walrus",
          vector2 %in% c("A", "B") ~ "goo",
          vector2 == "C" ~ "ga",
          vector2 == "D" ~ "joob")
