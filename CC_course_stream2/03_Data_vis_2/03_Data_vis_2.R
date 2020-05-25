#####################################################################################################

#### Mark Farrell's trials and tribulations with Coding Club's Data Viz part 2 ######################

#### mark.farrell@csiro.au        +61 8 8303 8664         25/04/2020 ################################

#### Their webpage is here: https://ourcodingclub.github.io/tutorials/datavis/ ######################

#####################################################################################################


### Set working directory
setwd("~/DATASCHOOL/r-learning/CC_course_stream2/03_Data_vis_2/")


### Install packages as needed



### Load packages

library(dplyr)
library(ggplot2)


## Import data
magic_veg <- read.csv("magic_veg.csv")
str(magic_veg)


#### STARTING WITH HISTOGRAMS

species_counts <- magic_veg %>% 
  group_by(land, plot) %>% 
  summarise(Species_number = length(unique(species)))

(hist <- ggplot(species_counts, aes(x = plot)) +
                  geom_histogram()
  )

# well, that's pretty crappy
# This is the common way of making a histogram, when you have one observation per row and the histogram 
# tallies them for you. But you can immediately see that it doesn’t look right, because we are working 
# with summarised data. You therefore need to tell R that you already know how many species are in each plot. 
# You do that by specifying the stat argument

(hist <- ggplot(species_counts, aes(x = plot, y = Species_number)) +
    geom_histogram(stat = "identity")
)

# `geom_col` would do the same thing:
(hist_col <- ggplot(species_counts, aes(x = plot, y = Species_number)) +
    geom_col()
  )

# That looks a bit better, but it still seems to have far too many species. That’s because plots from 
# each fictitious land are being grouped together. We can separate them by introducing a colour code, 
# and make a stacked bar plot like this:

(hist_stack1 <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity")
)

# Still  pretty useless graph... Use `dodge` to put them where they belong

(hist_side1 <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge")
)

# Now to make it a bit prettier
# Note how our figure only shows plot numbers 2, 4, and 6. If you want the axis to display every plot number, 
# 1 - 6, you can run the following code using breaks = c(1,2,3,4,5,6) or using breaks = 1:6. We can also specify 
# the limits of the plot axes - running the code below, you’ll be able to see that the limit of the y axis now 
# extends to the value of 50! This helps us keep all our data within the axis labels that we have, in terms of 
# the visualisation!

(hist_side2 <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") +
    scale_x_continuous(breaks = c(1,2,3,4,5,6)) +
    scale_y_continuous(limits = c(0, 50))
)

# Time for labels and such

(hist_side2 <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") +
    scale_x_continuous(breaks = c(1,2,3,4,5,6)) +
    scale_y_continuous(limits = c(0, 50)) +
    labs(title = "Species richness by plot",
         subtitle = "In the magical lands",
         caption = "Data from the Ministry of Magic",
         x = "\n Plot number",
         y = "Number of species\n")
)

# Moar control with "themes"
(hist_side2 <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") +
    scale_x_continuous(breaks = c(1,2,3,4,5,6)) +
    scale_y_continuous(limits = c(0, 50)) +
    labs(title = "Species richness by plot",
         subtitle = "In the magical lands",
         caption = "Data from the Ministry of Magic",
         x = "\n Plot number",
         y = "Number of species\n") +
    theme(axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12, face = "italic"), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"))
)

# Fix the horrid default background
(hist_side2 <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") +
    scale_x_continuous(breaks = c(1,2,3,4,5,6)) +
    scale_y_continuous(limits = c(0, 50)) +
    labs(title = "Species richness by plot",
         subtitle = "In the magical lands",
         caption = "Data from the Ministry of Magic",
         x = "\n Plot number",
         y = "Number of species\n") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12, face = "italic"), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"))
)

# Legend and colours
(hist_side2 <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") +
    scale_x_continuous(breaks = c(1,2,3,4,5,6)) +
    scale_y_continuous(limits = c(0, 50)) +
    scale_fill_manual(values = c("rosybrown1", "#deebf7"),     # specifying the colours
                      name = "Land of Magic") +                # specifying title of legend
    labs(title = "Species richness by plot",
         x = "\n Plot number",
         y = "Number of species\n") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12, face = "italic"), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.title = element_text(face = "bold"),
          legend.position = "bottom", 
          legend.box.background = element_rect(color = "grey", size = 0.3))
)

# Better labels - note you need to match the order with how the data are actually presented
(hist_side2 <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") +
    scale_x_continuous(breaks = c(1,2,3,4,5,6)) +
    scale_y_continuous(limits = c(0, 50)) +
    scale_fill_manual(values = c("rosybrown1", "#deebf7"),     # specifying the colours
                      labels = c("HOGSMEADE", "NARNIA"),       # changing the site labels
                      name = "Land of Magic") +                # specifying title of legend
    labs(title = "Species richness by plot",
         x = "\n Plot number",
         y = "Number of species\n") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12, face = "italic"), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.title = element_text(face = "bold"),
          legend.position = "bottom", 
          legend.box.background = element_rect(color = "grey", size = 0.3))
)

ggsave( "images/magical-sp-rich-hist.png", plot = hist_side2, width = 7, height = 5, dpi = 300)


#### COLOUR PALETTES
# A more advanced use of colour palettes is to create one linked to your factor levels. This is great when you 
# work on a project that will have multiple figures, and you want the colour-coding to be consistent across the 
# board. Linking colours specifically to factor levels ensures that if a factor is dropped from a data frame, 
# the corresponding colour will be dropped from the resulting plot, too, instead of being reassigned to the next 
# available factor level.

# Here with only two magical lands, you could easily keep track of the colours, but imagine if you had 10 different 
# lands! Let’s create a fake dataframe of values for more magical lands, and see the power of this approach.

# I suspect this will be the starting point for many a script dealing with categoricals...
# For continuous we know about `scale_colour_gradient()`...

## Make some more magical lands by forming two vectors. NB must be in order if doing this for real
land <- factor(c("Narnia", "Hogsmeade", "Westeros", "The Shire", "Mordor", "Forbidden Forest", "Oz"))
counts <- as.numeric(c(55, 48, 37, 62, 11, 39, 51))

## Grab the vectors into a dataframe
more_magic <- data.frame(land, counts)

## Check how many factors there are - we'll need as many colours as factors
length(levels(more_magic$land))
# That'll be 7!

## CREATE THE COLOUR PALETTE

magic.palette <- c("#698B69", "#5D478B", "#5C5C5C", "#CD6090", "#EEC900", "#5F9EA0", "#6CA6CD") #Defining colours
names(magic.palette) <- levels(more_magic$land) #linking factor names to colours - must be in correct order

## Draw a bar chart
(hist <- ggplot(more_magic, aes(x = land, y = counts, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") + 
    scale_y_continuous(limits = c(0, 65)) +
    scale_fill_manual(values = magic.palette,                        # using our palette here
                      name = "Land of Magic") +                      # Title of legend
    labs(title = "Species richness in magical lands", 
         x = "", y = "Number of species \n") + 
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.text.x = element_text(angle = 45, hjust = 1), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.title = element_text(face = "bold"),
          legend.position = "bottom", 
          legend.box.background = element_rect(color = "grey", size = 0.3)))

## Drop some factors using %in% (neat trick!!) to show consistency of colours
(hist <- ggplot(filter(more_magic, land %in% c("Hogsmeade", "Oz", "The Shire")), 
                aes(x = land, y = counts, fill = land)) +
    geom_histogram(stat = "identity", position = "dodge") + 
    scale_y_continuous(limits = c(0, 65)) +
    scale_fill_manual(values = magic.palette,  # using our palette ensures that colours with no corresponding factors are dropped
                      name = "Land of Magic") +                
    labs(title = "Species richness in magical lands", 
         x = "", y = "Number of species \n") + 
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.text.x = element_text(angle = 45, hjust = 1), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.title = element_text(face = "bold"),
          legend.position = "bottom", 
          legend.box.background = element_rect(color = "grey", size = 0.3)))

### Boxplot time
# To make the boxplots, we will slightly reshape the dataset to take account of year as well.
# 
yearly_counts <- magic_veg  %>% # brings in the data
  group_by(land, plot, year) %>%  # year is added here to create the third factor to group by in the plot
  summarise(Species_number = length(unique(species))) %>% # creates `Species_number` column on the basis of the three factors
  ungroup() %>% # drops thr grouping 
  mutate(plot = as.factor(plot)) # converts `plot` into a factor for the boxplots

(boxplot <-ggplot(yearly_counts, aes(plot, Species_number, fill = land)) +
    geom_boxplot()
  )


# Pretty
(boxplot <- ggplot(yearly_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_boxplot() +
    scale_x_discrete(breaks = 1:6) + # Limits numbers/ticks shown on the axis, can be concatenated combinations
    scale_fill_manual(values = c("rosybrown1", "#deebf7"),
                      breaks = c("Hogsmeade","Narnia"),
                      name="Land of magic",
                      labels=c("Hogsmeade", "Narnia")) +
    labs(title = "Species richness by plot", 
         x = "\n Plot number", y = "Number of species \n") + 
    theme_bw() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "bottom", 
          legend.box.background = element_rect(color = "grey", size = 0.3)))

## Dot plots, note error bars 

# Step 1 - create summarised data

summary <- species_counts %>% 
  group_by(land) %>% 
  summarise(mean = mean(Species_number),
            sd = sd(Species_number))

# Step 2 - draw the plot

(dot <- ggplot(summary, aes(x = land, y = mean, colour = land)) +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
    geom_point(size = 3) +
    scale_y_continuous(limits = c(0, 50)) +
    scale_colour_manual(values = c('#CD5C5C', '#6CA6CD'), 
                        labels = c('HOGSMEADE', 'NARNIA'), 
                        name = 'Land of Magic') +                   
    labs(title = 'Average species richness', 
         x = '', y = 'Number of species \n') + 
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = 'bold'), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , 'cm'), 
          legend.title = element_text(face = 'bold'),
          legend.position = 'bottom', 
          legend.box.background = element_rect(color = 'grey', size = 0.3))
    )

# Reminder, data needs reordering in the dataframe to change order on the figure

yearly_counts$land <- factor(yearly_counts$land, 
                             levels = c("Narnia", "Hogsmeade"),
                             labels = c("Narnia", "Hogsmeade"))

# Now Narnia will always be before Hogsmeade

(boxplot <- ggplot(yearly_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_boxplot() +
    scale_x_discrete(breaks = 1:6) +
    scale_fill_manual(values = c("#deebf7", "rosybrown1"),
                      breaks = c("Narnia","Hogsmeade"),
                      name = "Land of magic",
                      labels = c("Narnia", "Hogsmeade")) +
    labs(title = "Species richness by plot", 
         x = "\n Plot number", y = "Number of species \n") + 
    theme_bw() +
    theme() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "bottom", 
          legend.box.background = element_rect(color = "grey", size = 0.3))
    )

# We can also reorder the sequence of plot numbers in the same way

yearly_counts$plot <- factor(yearly_counts$plot,
                             levels = c("6", "1", "2", "3", "4", "5"),
                             labels = c("6", "1", "2", "3", "4", "5"))

# Easy :)

(boxplot2 <- ggplot(yearly_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_boxplot() +
    scale_x_discrete(breaks = 1:6) +
    scale_fill_manual(values = c("#deebf7", "rosybrown1"),
                      breaks = c("Narnia","Hogsmeade"),
                      name = "Land of magic",
                      labels = c("Narnia", "Hogsmeade")) +
    labs(title = "Species richness by plot", 
         x = "\n Plot number", y = "Number of species \n") + 
    theme_bw() + 
    theme() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 12), 
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
          plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
          legend.position = "bottom", 
          legend.box.background = element_rect(color = "grey", size = 0.3)))

## Creating customised themes
# You can include as many elements in your theme as you want, and when you apply your theme to a graph, 
# only the relevant elements will be considered - e.g. for our histograms we won’t need to use legend.position, 
# but it’s fine to keep it in the theme in case any future graphs we apply it to do have the need for legends.

theme_coding <- function(){               # creates a new theme as a function
  theme_bw() +                            # uses this predefined theme as a starting point
    theme(axis.text.x = element_text(size = 12, angle = 45, 
                                     vjust = 1, hjust = 1), # customising font size, angle, justification
          axis.text.y = element_text(size = 12),            # customising font size
          axis.title = element_text(size = 14),             # customising font size
          panel.grid = element_blank(),                     # Blank panels
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5),         # nice plot margins
                             units = , "cm"), 
          plot.title = element_text(size = 20, vjust = 1,   # customising font size, angle, justification
                                    hjust = 0.5),
          legend.text = element_text(size = 12,             # customising font size, format
                                     face = "italic"),
          legend.title = element_blank(),                   # Blank font panel
          legend.position = c(0.9, 0.9))                    # Legend position
}

# This is something to work though in great detail as it will save a lot of effort to have something uniform.
# I can see having multiple themes may be useful when producing figures for a dataset. One that covers the basics
# such as the above (font formats, backgrounds, positions) which may be pretty universal for every figure / dataset,
# and one that handles colours, shapes and alpha for consistency within a dataset. The first would need some 
# altering for my purposes (italic labels are yuck, for example), but it would automate a lot of fixes I usually
# spend ages doing in SigmaPlot. 

# A flow of work for datavis may well follow:
# 1) Import / tidy data
# 2) Sort out summarising as required
# 3) Sort out orders using `factor` as required
# 4) Code (or paste) general function for layout, size, etc.
# 5) Code (or paste) specific function for colours, size, alpha, shape and other attributes
# 6) Do any plot-specific tidying

# Continuing the tutorial, the next two code chunks produce the same output, but the second saves a lot of typing:

# Hard-coded

(boxplot <- ggplot(yearly_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_boxplot() +
    scale_x_discrete(breaks = 1:6) +
    scale_fill_manual(values = c("#deebf7", "rosybrown1"),
                      breaks = c("Narnia","Hogsmeade"),
                      name = "Land of magic",
                      labels = c("Narnia", "Hogsmeade")) +
    labs(title = "Species richness by plot", 
         x = "\n Plot number", y = "Number of species \n") + 
    theme_bw()+                          # using a predefined theme as a base
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),       # customising lots of things
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14),
          panel.grid = element_blank(),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
          legend.text = element_text(size = 12, face = "italic"),
          legend.title = element_blank(),
          legend.position = c(0.9, 0.9))
)

# Using the `theme_coding` function:

(boxplot <- ggplot(yearly_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_boxplot() +
    scale_x_discrete(breaks = 1:6) +
    scale_fill_manual(values = c("#deebf7", "rosybrown1"),
                      breaks = c("Narnia","Hogsmeade"),
                      name = "Land of magic",
                      labels = c("Narnia", "Hogsmeade")) +
    labs(title = "Species richness by plot", 
         x = "\n Plot number", y = "Number of species \n") + 
    theme_coding()                      # short and sweeeeet!
)

# And if you need to change some elements (like the legend that encroaches on the graph here), 
# you can simply overwrite:

(boxplot <- ggplot(yearly_counts, aes(x = plot, y = Species_number, fill = land)) +
    geom_boxplot() +
    scale_x_discrete(breaks = 1:6) +
    scale_fill_manual(values = c("#deebf7", "rosybrown1"),
                      breaks = c("Narnia","Hogsmeade"),
                      name = "Land of magic",
                      labels = c("Narnia", "Hogsmeade")) +
    labs(title = "Species richness by plot", 
         x = "\n Plot number", y = "Number of species \n") + 
    theme_coding() +                      # this contains legend.position = c(0.9, 0.9)
    theme(legend.position = "right")      # this overwrites the previous legend position setting
)

#
