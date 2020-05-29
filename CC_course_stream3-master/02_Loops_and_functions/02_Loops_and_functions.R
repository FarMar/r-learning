#####################################################################################################

#### Mark Farrell's trials and tribulations with Coding Club's Modelling tutorials ##################

#### mark.farrell@csiro.au        +61 8 8303 8664         27/05/2020 ################################

#### Their webpage is here: https://ourcodingclub.github.io/tutorials/funandloops/index.html ########

#####################################################################################################

### Set working directory



### Install packages as needed
install.packages("gridExtra")


### Load packages

library(dplyr)
library(ggplot2)
library(gridExtra)
library(stringr)


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
# The above example illustrates how loops work, but often, data are not separated into multiple dataframes from the 
# beginning, instead they are often in a single dataframe with a column to group the different datasets.
# Returning to the trees_mlunguya dataset, you can see that there is a column called year, which denotes when each 
# stem measurement was taken. Imagine we want to perform the basal area calculation on each year in the dataset, then 
# find out whether the mean basal area of stems in the plots has changed over the years. We can do this using a 
# for() loop.
# First, separate trees_mlunguya into a list of dataframes, each based on the contents of the year column:

trees_mlunguya_list <- split(trees_mlunguya, trees_mlunguya$year) # Check tidyverse syntax...

# Then, run a for() loop to fill an empty list with the mean basal area of each year:

mean_ba_list <- list()    # The empty list

for( i in 1:length(trees_mlunguya_list) ){
  ba <- basal.area(trees_mlunguya_list[[i]]$diam)
  mean_ba <- mean(ba)
  year <- mean(trees_mlunguya_list[[i]]$year)
  dat <- data.frame(year, mean_ba)
  mean_ba_list[[i]] <- dat
}

# During each iteration, this loop creates a number of intermediate data objects (ba, mean_ba, year), and 
# eventually returns a dataframe (dat) with a single row and two columns, one for year and one for mean basal 
# area. Each of these dataframes are then stored as a list item in the new list mean_ba_list.
#
# Of course, this intermediate calculation could be stored in it’s own custom function:

ba.mean.year <- function(dbh, year){
  data.frame(
    mean_ba = mean(basal.area(dbh)),
    year = mean(year)
  )
}

ba.mean.year(trees_mlunguya_list[[1]]$diam, trees_mlunguya_list[[1]]$year)

# We can now use this new function in a loop:

for( i in 1:length(trees_mlunguya_list) ){
  mean_ba_list[[i]] <- ba.mean.year(
    trees_mlunguya_list[[i]]$diam,
    trees_mlunguya_list[[i]]$year)
}

# That's nice, but we'd then want to merge the data frames within the list for it to be any use. For this example
# it would probably make more sense to find a tidyverse way. But the principle of this tutorial is to show how 
# functions can be nested in loops

#### Functions with `lapply()`
# lapply() runs operations on lists of items, similar to the for() loops above. To replicate the previous for() 
# loop, where we calculated the mean basal area per year in trees_mlunguya, you can run:

lapply(trees_mlunguya_list,
       function(x){
         ba.mean.year(dbh = x$diam,
                      year = x$year)
       }
       )

# The first argument of lapply() gives the list object to be iterated over. The second argument defines an 
# unnamed function, where x will be replaced with each list item as lapply() iterates over them. The code inside 
# the curly brackets is the unnamed function, which itself contains our custom function ba.mean.year().

# For another example to illustrate another way lapply() can be used, imagine we wanted to find the mean height 
# of trees in trees_bicuar for each taxonomic family.

# First, create a list of vectors of height (rather than dataframes) where each list is a different family of species.

bicuar_height_list <- split(trees_bicuar$height, trees_bicuar$family)

# Then run `lapply`

lapply(bicuar_height_list, mean, na.rm = TRUE)

# sapply() simplifies the output of lapply() to a vector, with elements in the vector named according to the name 
# of the items in the original list:

sapply(bicuar_height_list, mean, na.rm = TRUE)

# OK, now we're talking! I can easily use this to build vectors of means and SEMs then `mutate` to a new tibble. That
# would be a pretty basic use, but potentailly useful if the output from the `sapply` function is simple. Would need
# to think carefully about how this could be applied to things like running lots of ANOVA models or curve fitting 
# for 14C data, for example. `sapply` can only handle simple outputs (note here it's just made a list with attributes). 
# In fact, i've modified this slightly to build a df using the same code

test<-as.data.frame(sapply(bicuar_height_list, mean, na.rm = TRUE))

#### Conditional statements
# In the trees_bicuar data there is a column which refers to the method by which trees_bicuar$height was measured, 
# called trees_bicuar$height_method. One set of field assistants measured tree height with a long stick, while 
# the others had access to a laser range finder, affecting the accuracy with which measurements were taken. 
# Measurements taken with a stick were generally about 1 m short of the actual tree height, while measurements 
# with the laser scanner is only certified accurate to +/- 0.1 m. So a simple correction would be to add 1 m to 
# every measurement done with a stick, and round every measurement done with the laser to the nearest 0.1 m.

# A common forestry metric to assess growth of a forest plot over time is “Lorey’s Mean Height”. Lorey’s mean height 
# is calculated by multiplying tree height by the basal area of the tree, then dividing the sum of this calculation 
# by the total plot basal area. We can construct a function which measures Lorey’s mean height for each plot, but we 
# want to adjust the height estimates depending on which method was used. For this, we can use an ifelse() statement.

# An ifelse() statement tests for some logical TRUE/FALSE condition in the data, then performs one of two 
# actions depending on the outcome of the test. E.g. “if the value of x is greater than 2, multiply it by 2, else 
# if not, divide by 2”. The code below constructs a function with an ifelse() statement to calculate Lorey’s mean 
# height for the Bicuar plots.

stick.adj.lorey <- function(height, method, ba){
  height_adj <- ifelse(method == "stick", height +1, round(height, digits = 1))
  lorey_height <- sum(height_adj * ba, na.rm = TRUE) / sum(ba, na.rm = TRUE)
  return(lorey_height)
}

# We can then test this function on each plot using `lapply`, which returns a list of 4 numbers, 1 for each plot

trees_bicuar_list <- split(trees_bicuar, trees_bicuar$plotcode)
lapply(trees_bicuar_list, function(x){
  stick.adj.lorey(
    height = x$height,
    method = x$height_method,
    ba = x$ba
    )})

# ifelse() statements can also be used in conjunction with logical TRUE/FALSE function arguments to determine 
# whether certain actions are taken. For example, we can write a function that calculates summary statistics on 
# the trunk diameter measurements for a given fieldsite, and we can use TRUE/FALSE arguments to let the user decide 
# whether certain statistics are calculated:

diam.sum <- function(dbh, mean = TRUE, median = TRUE, ba = TRUE){
  mean_dbh <- ifelse(mean == TRUE,
                     mean(dbh),
                     NA)
  median_dbh <- ifelse(median == TRUE,
                       median(dbh),
                       NA)
  mean_ba <- ifelse(ba == TRUE,
                    mean(basal.area(dbh)),
                    NA)
  
  return(as.data.frame(na.omit(t(data.frame(mean_dbh, median_dbh, mean_ba)))))
}

# This is the starting point for what could be a very good function if I can tame outputs from ANOVA models etc. I
# should be able to get it to pull all that I need to from outputs into columns and build the DF all in one go. Need
# to think carefully about order of operations and input format. The `return` line is the bit, and this is where I
# presume I would go fishing for the bits of output. The main feature of the above was of course the `ifelse` which 
# allows conditionality. Just playing about in the example, but would be very convenient for getting it to, for
# example, draw a plot of exponential growth from 14C mineralisation if r2 < x. I suspect I'll be using `grep` a fair
# bit to parse the outputs for the bits I need to add to the df output.

diam.sum(dbh = trees_bicuar$diam, mean = TRUE, median = FALSE)
diam.sum(dbh = trees_bicuar$diam)


#### Write a loop to plot multiple graphs
LPI <- read.csv("LPI_data_loops.csv")

vulture <- filter(LPI, Common.Name == "Griffon vulture / Eurasian griffon")
vultureITCR <- filter(vulture, Country.list == c("Croatia", "Italy"))

(vulture_scatter <- ggplot(vultureITCR, aes(x = year, y = abundance, colour = Country.list)) +
    geom_point(size = 2) +                                              # Changing point size
    geom_smooth(method = lm, aes(fill = Country.list)) +                # Adding a linear model fit and colour-coding by country
    scale_fill_manual(values = c("#EE7600", "#00868B")) +               # Adding custom colours
    scale_colour_manual(values = c("#EE7600", "#00868B"),               # Adding custom colours
                        labels = c("Croatia", "Italy")) +               # Adding labels for the legend
    ylab("Griffon vulture abundance\n") +                             
    xlab("\nYear")  +
    theme_bw() +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),       # making the years at a bit of an angle
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14, face = "plain"),             
          axis.title.y = element_text(size = 14, face = "plain"),             
          panel.grid.major.x = element_blank(),                                # Removing the background grid lines                
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),           # Adding a 0.5cm margin around the plot
          legend.text = element_text(size = 12, face = "italic"),              # Setting the font for the legend text
          legend.title = element_blank(),                                      # Removing the legend title
          legend.position = c(0.9, 0.9)))       

vulture_scatter

# Long, far too much guff. Let's make a formatting function:

theme.my.own <- function(){
  theme_bw()+
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14, face = "plain"),             
          axis.title.y = element_text(size = 14, face = "plain"),             
          panel.grid.major.x = element_blank(),                                          
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
          legend.text = element_text(size = 12, face = "italic"),          
          legend.title = element_blank(),                              
          legend.position = c(0.9, 0.9))
}

(vulture_scatter <- ggplot(vultureITCR, aes (x = year, y = abundance, colour = Country.list)) +
    geom_point(size = 2) +                                                
    geom_smooth(method = lm, aes(fill = Country.list)) +                    
    theme.my.own() +                                                    # Adding our new theme!
    scale_fill_manual(values = c("#EE7600", "#00868B")) +               
    scale_colour_manual(values = c("#EE7600", "#00868B"),               
                        labels = c("Croatia", "Italy")) +                 
    ylab("Griffon vulture abundance\n") +                             
    xlab("\nYear"))

# So that's a refresher of what we started before, not lets go further.
# Let's filter 4 UK species

LPI.UK <- filter(LPI, Country.list == "United Kingdom")
house.sparrow <- filter(LPI.UK, Common.Name == "House sparrow")
great.tit <- filter(LPI.UK, Common.Name == "Great tit")
corn.bunting <- filter(LPI.UK, Common.Name == "Corn bunting")
reed.bunting <- filter(LPI.UK, Common.Name == "Reed bunting")
meadow.pipit <- filter(LPI.UK, Common.Name == "Meadow pipit")

(house.sparrow_scatter <- ggplot(house.sparrow, aes (x = year, y = abundance)) +
    geom_point(size = 2, colour = "#00868B") +                                                
    geom_smooth(method = lm, colour = "#00868B", fill = "#00868B") +          
    theme.my.own() +
    labs(y = "Abundance\n", x = "", title = "House sparrow"))

(great.tit_scatter <- ggplot(great.tit, aes (x = year, y = abundance)) +
    geom_point(size = 2, colour = "#00868B") +                                                
    geom_smooth(method = lm, colour = "#00868B", fill = "#00868B") +          
    theme.my.own() +
    labs(y = "Abundance\n", x = "", title = "Great tit"))

(corn.bunting_scatter <- ggplot(corn.bunting, aes (x = year, y = abundance)) +
    geom_point(size = 2, colour = "#00868B") +                                                
    geom_smooth(method = lm, colour = "#00868B", fill = "#00868B") +          
    theme.my.own() +
    labs(y = "Abundance\n", x = "", title = "Corn bunting"))

(meadow.pipit_scatter <- ggplot(meadow.pipit, aes (x = year, y = abundance)) +
    geom_point(size = 2, colour = "#00868B") +                                                
    geom_smooth(method = lm, colour = "#00868B", fill = "#00868B") +          
    theme.my.own() +
    labs(y = "Abundance\n", x = "", title = "Meadow pipit"))

# Using `gridExtra`, we can plot them all at once

panel <- grid.arrange(house.sparrow_scatter, great.tit_scatter, corn.bunting_scatter, meadow.pipit_scatter, ncol = 2)
ggsave(panel, file = "Pop_trend_panel.png", width = 10, height = 8)
dev.off()

# That's not so bad, but still lots of typing to do, and even copy / paste that will get boring quickly

Sp_list <- list(house.sparrow, great.tit, corn.bunting, meadow.pipit)

for (i in 1:length(Sp_list)) {
  data <- as.data.frame(Sp_list[i])   # Create a dataframe for each species
  sp.name <- unique(data$Common.Name) # Create an object that holds the species name, so that we can title each graph
  plot <- ggplot(data, aes (x = year, y = abundance)) +               # Make the plots and add our customised theme
    geom_point(size = 2, colour = "#00868B") +                                                
    geom_smooth(method = lm, colour = "#00868B", fill = "#00868B") +          
    theme.my.own() +
    labs(y = "Abundance\n", x = "", title = sp.name)
  
  ggsave(plot, file = paste(sp.name, ".pdf", sep = ''), scale = 2)
  
  print(plot) 
} 

# So heres where i'm going to try to be too clever and do it for all species in LPI.UK. First bit of code culls
# the "/" and turns them into "-" so that filenames are OK. Needs to be run twice as some have two "/". I should 
# work on fixing this...

LPI.UK.clean <- LPI.UK %>%
  mutate_at(vars(contains("Common.Name")), funs(str_replace(., "/", "-")))
LPI.UK.clean <- LPI.UK.clean %>%
  mutate_at(vars(contains("Common.Name")), funs(str_replace(., "/", "-")))
Sp_list_long_df <- LPI.UK.clean %>% group_split(Common.Name) # `group_split` builds the table instead of `list`

for (i in 1:length(Sp_list_long_df)) {
  data <- as.data.frame(Sp_list_long_df[i])   # Create a dataframe for each species
  sp.name <- unique(data$Common.Name) # Create an object that holds the species name, so that we can title each graph
  plot <- ggplot(data, aes (x = year, y = abundance)) +               # Make the plots and add our customised theme
    geom_point(size = 2, colour = "#00868B") +                                                
    geom_smooth(method = lm, colour = "#00868B", fill = "#00868B") +          
    
    theme.my.own() +
    labs(y = "Abundance\n", x = "", title = sp.name)
  
  ggsave(plot, file = paste(sp.name, ".pdf", sep = ''), scale = 2)
  
  print(plot) 
} 

# It's not fast or parallelised, but it gets there


