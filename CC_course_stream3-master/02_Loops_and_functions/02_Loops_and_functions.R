#####################################################################################################

#### Mark Farrell's trials and tribulations with Coding Club's Modelling tutorials ##################

#### mark.farrell@csiro.au        +61 8 8303 8664         27/05/2020 ################################

#### Their webpage is here: https://ourcodingclub.github.io/tutorials/funandloops/index.html ########

#####################################################################################################

### Set working directory



### Install packages as needed



### Load packages

library(dplyr)
library(ggplot2)


## Import data
trees_bicuar <- read.csv("trees_bicuar.csv")
trees_mlunguya <- read.csv("trees_mlunguya.csv")

# The data contains information on tree stems surveyed in four 1 Ha plots at fieldsites around southern Africa. 
# trees_bicuar contains data for trees in Bicuar National Park in southwest Angola, and trees_mlunguya contains 
# data for trees in southern Mozambique. Each tree stem >5 cm trunk diameter was measured for tree height and 
# trunk diameter, and identified to species.

head(trees_bicuar)
str(trees_bicuar)

#### FUNCTIONS

# The basic syntax for creating a function looks like this:
example.fn <- function(x, y) {
  # perform an action using x and y
  x + y
}

# The function() command is used to tell R that we are creating a function, and we are assigning the function 
# to an object called example.fn. x and y are “arguments” for the function, i.e. things that the user provides 
# when running the function, then in the curly brackets are the actions performed by the function, using the 
# parameters defined by the user earlier in the function call, and any other objects in the working environment 
# in this case adding x and y together.

example.fn(x = 1, y = 2)

# You should get an output of 3, because the function example.fn() was provided with the values of x = 1 and 
# y = 2, which were then passed to the function, which performed the operation x + y. Note that the convention 
# is to name a function using . rather than _ which is normally used to define data objects. This isn’t a rule, 
# but it’s best to stick to the conventions used by other programmers to keep things consistent.

# we can also define a function that calculates the basal area of each stem in m^2 from the diameter, which is in 
# cm. The basal area is the cross-sectional area of the tree trunk if it was cut parallel to the ground.

basal.area <- function(x) {
  (pi*(x)^2)/40000
}

basal.area(x = trees_bicuar$diam)

# Function arguments don’t need to be called x and y, they can be any character string, for example, the function 
# below works identically to the one above, only x is now referred to as dbh:

basal.area <- function(dbh) {
  (pi*(dbh)^2)/40000
}

basal.area(dbh = trees_bicuar$diam)

# Additionally, you can add a indeterminate number of extra arguments using the `...` operator. Imagine that we 
# want to extend our `basal.area()` function so that it can compute the combined basal area of multiple vectors 
# of diameter measurements, e.g. from multiple sites:

basal.area <- function(...) {
  (pi*c(...)^2)/40000
}

basal.area(trees_bicuar$diam, trees_mlunguya$diam)

# Just like a normal function, the output of basal.area() can be assigned to a new object, for example, as a new 
# column in trees_bicuar:

trees_bicuar$ba <- basal.area(trees_bicuar$diam)

# Functions are essentially small programs. There's nothing different here between writing out the formula numerous
# times for each dataset. In my soils world this might be quite useful e.g. for calculating bulk density, applying
# moisture correction, turning concentractions into stocks. It could also be useful in isotope land for bulk-processing
# data, be it raw (LIMS replacement) or for uptake calculations. Not really any different to dragging a formula
# row around in Excel at this basic level, but good to build on.

#### Loops
# Loops come in two main variants in R, for() loops and while() loops. In this workshop we will focus on for() loops, 
# which are generally easier to read than while() loops, and can be used to perform the same sorts of actions. 
# while() loops are used mainly when the user wants to perform an action a set number of times, whereas a for() 
# loop is generally used when the user wants to perform an action on a named set of objects.
#
# A for() loop iterates through a number of items, most commonly stored as a list, and performs some action equally 
# on each item. It can drastically reduce the amount of copying and pasting.
# 
# The basic syntax for creating a for() loop looks like this:

for(i in list){
  #PERFORM SOME ACTION
}

# Imagine you have multiple field sites, each with four 1 Ha plots with the tree stem measurements described earlier. 
# The data for each fieldsite is contained in a different dataframe, e.g. trees_bicuar and trees_mlunguya. If we 
# wanted to calculate the basal area for all stems at both sites, we could run:

trees_bicuar$ba <- basal.area(trees_bicuar$diam)
trees_mlunguya$ba <- basal.area(trees_mlunguya$diam)

## Loops through dataframes

# The above seems fine for now, but what if we had 100 field sites instead of just two? In that case, you can use a 
# for() loop. First, we have to create a list of dataframes to perform the loop on. There are many ways of doing 
# this, but the simplest way is:

trees <- list("trees_bicuar" = trees_bicuar, "trees_mlunguya" = trees_mlunguya)

# This makes a list called trees, where each element in the list is a dataframe. List items within a list can be 
# accessed using double square brackets, e.g. trees[[1]] selects the first list item, the dataframe for trees_bicuar. 
# We can take advantage of this method of list indexing using square brackets when we construct our for() loop:

for( i in 1:length(trees) ){
  trees[[i]]$ba <- basal.area(trees[[i]]$diam)
}

# The first line sets up the loop, similar to how the function() definition worked earlier. 1:length(trees) creates 
# a sequence of integers from 1 to the length of the list (trees), so in this case the sequence will be 1, 2 as 
# there are two list items. i will take each value of 1:length(trees) in turn, then run the actions in the curly 
# brackets once. For example, the first time the loop runs, i will have a value of 1, and the second time i will 
# have a value of 2. Once the loop has run for the second time, the loop will end, as there are no further values 
# in 1:length(trees).
# The body of the loop creates a new column in each dataframe in the list, then runs the function basal.area() 
# using the diam column from the same dataframe as the input. So, the first time the loop runs, it will create a new 
# column called ba in the first list item in trees, trees[[1]].

## Loops within dataframes




