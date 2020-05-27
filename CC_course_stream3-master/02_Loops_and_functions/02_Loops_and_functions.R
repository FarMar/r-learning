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



