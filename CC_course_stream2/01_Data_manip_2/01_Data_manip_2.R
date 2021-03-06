# EFFICIENT DATA MANIPULATION - USE PIPES TO STREAMLINE YOUR CODE
# Course URL: https://ourcodingclub.github.io/tutorials/data-manip-efficient/

# Libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Set working directory
setwd("~/DATASCHOOL/r-learning/CC_course_stream2/01_data_manip_2")

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

## Advanced piping
# Here we're going to draw a map of the trees. First, the data needs to be simplified:

trees.five <- trees.genus.2 %>% 
  filter(Genus %in% c("Acer", "Fraxinus", "Salix", "Aesculus", "Pinus"))

# we're then going to draw a very simple map in ggplot
# x/y are lat/long, size = height category, genus = colour

(map.all <- ggplot(trees.five) +
    geom_point(aes(x = Easting, y = Northing, size = Height.cat, colour = Genus), alpha = 0.5) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12))
  )

## Now we're going to use dplyr to pull out each genus of the five "chosen ones" and then generate individual plots
## Could do this with facets, but this option is more flexible if happy patching up figures in Illustrator later
## `do()` is the function to explore here

tree.plots <-
  trees.five %>%        # the data frame
  group_by(Genus) %>%   # grouping by genus
  do(plots =            # the plotting call within the `do()` function
      ggplot(data = .) +
       geom_point(aes(x = Easting, y = Northing, size = Height.cat), alpha = 0.5) +
       labs(title = paste("Map of", .$Genus, "at Craigmillar Castle", sep = " ")) + # title autofil magic from the . 
       theme_bw() +
       theme(panel.grid = element_blank(),
             axis.text = element_text(size = 14),
             legend.text = element_text(size = 12),
             plot.title = element_text(hjust = 0.5),
             legend.position = "bottom")
     )

# view the plots in viewer
tree.plots$plots

# save the autonamed plots to an autonamed directory
tree.plots %>% 
  do(.,
     ggsave(.$plots, filename = paste(getwd(), "/", "map-", .$Genus, ".png", sep = ""), 
            device = "png", height = 12, width =16, units = "cm"))

### We would like a summary of the species in each of four quadrants of the park (NE, NW, SE, SW)
### Outputs required for each quadrant are: species richness, abundance of Acer, bar plots of Acer trees

## Calculate the quadrants
# Find the center coordinates that will divide the data 
# (adding half of the range in longitude and latitude to the smallest value)

lon <- (max(trees.genus.2$Easting) - min(trees.genus.2$Easting))/2 + min(trees.genus.2$Easting)
lat <- (max(trees.genus.2$Northing) - min(trees.genus.2$Northing))/2 + min(trees.genus.2$Northing)

# Then create the Quadrant column

trees.genus.2 <- trees.genus.2 %>% 
  mutate(Quadrant = case_when(
    Easting < lon & Northing > lat ~ 'NW',
    Easting < lon & Northing < lat ~ 'SW',
    Easting > lon & Northing > lat ~ 'NE',
    Easting > lon & Northing < lat ~ 'SE')
  )

# Check if it worked
ggplot(trees.genus.2) +
  geom_point(aes(x = Easting, y = Northing, colour = Quadrant)) +
  theme_bw()

# Note the NA, indicating that a point wasn't binned to a quadrant correctly. This is likely because the lat/lon
# splits found at least one point that fell in neither due to it being exactly on the boundary. 
#
# A trap for new players.
#
# Fix is simple enough - use <= and decide where you want points that fall on the cracks to end up
# This would be a very similar situation to the 0-10 cm, 10-20 cm problem of soil cores...

trees.genus.2 <- trees.genus.2 %>% 
  mutate(Quadrant = case_when(
    Easting <= lon & Northing > lat ~ 'NW',
    Easting <= lon & Northing < lat ~ 'SW',
    Easting > lon & Northing > lat ~ 'NE',
    Easting > lon & Northing < lat ~ 'SE')
  )

## Species richness by quadrant
# Reads in data, groups by Quadrant, uses `summarise` to make new tibble with headings Quadrant and richness
# and displays number of unique species in each quadrant. NB "richness" in final line could be any name you 
# wanted to be header for the column in the new tibble

sp.richness <- trees.genus.2 %>% 
   group_by(Quadrant) %>% 
   summarise(richness = length(unique(LatinName)))
sp.richness

## Now to calculate the proportion of Acer trees

acer.percent <- trees.genus.2 %>% 
  group_by(Quadrant, Genus) %>%     # comma lets you have sub-group, in this case Quadrant x Genus
  tally() %>%                       # counts the number in each subgroup
  group_by(Quadrant) %>%            # re-groups just by quadrant
  mutate(total = sum(n)) %>%        # sums the number of trees into a new column "n"
  filter(Genus == 'Acer') %>%       # filters by genus, to keep only Acer. Note `==` is a strict match
  mutate(percent = n/total)         # calculates the proportion

# Draw the figure
ggplot(acer.percent) +
  geom_col(aes(x = Quadrant, y = percent)) +
  labs(x = 'Quadrant', y = 'Proportion of Acer') +
  theme_bw()

## Finally, bar plots of age distribution of Acers in each quadrant
# Create an Acer-only dataframe

acer <- trees.genus.2 %>% 
  filter(Genus == 'Acer')

# Rename and reorder age factor

acer$AgeGroup <- factor(acer$AgeGroup,
                        levels = c('Juvenile', 'Semi-mature', 'Middle Aged', 'Mature'),
                        labels = c('Young', 'Young', 'Middle Aged', 'Mature'))

# Plot the graphs.... magic `do()` in here 
acer.plots <- acer %>% 
  group_by(Quadrant) %>% 
  do(plots = 
       ggplot(data = .) +
       geom_bar(aes(x = AgeGroup)) +
       labs(title = paste('Age distribution of Acer in ', .$Quadrant, ' corner', sep = ''),
            x = 'Age Group', y = 'Number of trees') +
       theme_bw() +
       theme(panel.grid = element_blank(),
             axis.title = element_text(size = 14),
             axis.text = element_text(size = 14),
             plot.title = element_text(hjust = 0.5))
     )

acer.plots$plots  
  
  
  
  
  
