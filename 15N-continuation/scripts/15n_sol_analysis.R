## This script takes the wrangled data from 15n_sol_proc.R and ../data/raw/15n_soil_sol.csv
## It examines the final processed data ../data/working/ratios.csv

library(tidyverse)
library(lmerTest)
library(emmeans)
setwd("~/DATASCHOOL/r-learning/15N-continuation/")
all <- read_csv("data/working/ratios.csv")
all

## Now let's look how the variables change over time

## this page was helpful for mu and super/subscripts: 
## https://stackoverflow.com/questions/34892262/subscripts-and-superscripts-or-with-ggplot2-axis-labels-ionic-chemical

## also here: https://rstudio-pubs-static.s3.amazonaws.com/136237_170402e5f0b54561bf7605bdea98267a.html

## Below is a rather extensive set of code for xy plots with/without trends and CI ribbons

##Where we got to with the previous data exploration exercises using ggplot, colours, sizes etc

## ggplot of TDN
## tdn nitrate - x = time, y = TDN (mg L-1), colour = treatment (green = liquid, brown = solid, black = litter control, blue = zero control)
## size = NO3- concentration (mg L-1)

ggplot(data = all, aes(days, tdn, colour = trt, size = no3)) + # x, y, colour by treatment
  scale_colour_manual(values=c("#CCFF00", # all the colours in hexadecimal code in order of "colour = x" argument above
                               "#99FF00",
                               "#66FF00",
                               "#00CC00",
                               "#336600",
                               "#FFCC33",
                               "#FF9933",
                               "#CC9933",
                               "#996600",
                               "#993300",
                               "#333333",
                               "#0099FF"
  )) +
  #geom_point(size = 6, alpha = 0.6) + # Plot as a point graph, size, transparency
  geom_jitter(alpha = 0.7, width = 0.5) + # Plot as a jittered point graph
  stat_smooth(method = "loess", se = FALSE, size = 1) + # Adds trend line only
 # stat_smooth(method = "loess", # Adds ribbon for SE / CI and allows transparency to be adjusted
              #              colour = "red", 
  #            geom = "ribbon", 
   #           alpha = 0.05, #sets ribbon transparency
    #          size = 0.0001, #sets ribbon outline thickness. Need to work out how to make transparent
     #         fullrange = TRUE) + # Forces ribbon to end of data
  # xlim(0, 48) + ylim(0, 300) + # can be used to set hard limits to scale
  coord_cartesian(xlim = c(0, 48), ylim = c(0, 520)) + #sets soft limits to scale
  labs(
    x = "Time (days)", # x axis label
    y = TDN ~mg~L^{-1}, #y axis label, note code and weblink above for special characters and script
    #  title = "Nitrogen", # Graph title
    colour = "Treatment", # Legend Title
    size = ~NO[3]^{textstyle("-")}-N ~mg~L^{-1}
  ) +
  theme(
    panel.grid = element_blank(), # blank grid
    # axis.text.y = element_text(angle = 90), # rotates labels
    axis.text=element_text(size=14), # axis text size
    axis.title = element_text(color="black", face="bold", size=18), # axis label text size
    panel.background = element_rect(fill = "white", # panel background
                                    colour = "black") # panel outline
  )

## This is a good starting point, but it would be better if size was % of TDN as NO3

no3percent <- mutate(all, percentno3 = (100 / tdn) * no3 )

## Thanks to the usual errors around DON/TDN numbers, we now have some that are >100. Lets fix that here

no3fix <- mutate(no3percent, if_else(percentno3 >100, 100, percentno3))

## Rename
no3fix <- rename(no3fix, percentno3f = "if_else(percentno3 > 100, 100, percentno3)")

## Now let's do the graph we actually want:

## size = %NO3

ggplot(data = no3fix, aes(days, tdn, colour = trt, size = percentno3f)) + # x, y, colour by treatment
  scale_colour_manual(values=c("#CCFF00", # all the colours in hexadecimal code in order of "colour = x" argument above
                               "#99FF00",
                               "#66FF00",
                               "#00CC00",
                               "#336600",
                               "#FFCC33",
                               "#FF9933",
                               "#CC9933",
                               "#996600",
                               "#993300",
                               "#333333",
                               "#0099FF"
  )) +
  geom_jitter(alpha = 0.7, width = 0.5) + # Plot as a jittered point graph
  stat_smooth(method = "loess", se = FALSE, size = 1) + # Adds trend line only
  coord_cartesian(xlim = c(0, 48), ylim = c(0, 520)) + #sets soft limits to scale
  xlab(bquote('Time ('*d*')')) +
  ylab(bquote('TDN ('*'mg N'%.% L^-1*')')) +
  labs(
    title = "Total dissolved N in soil solution", # Graph title
    colour = "Treatment", # Legend Title
    size = "%" ~NO[3]^{textstyle("-")}
  ) +
  theme(
    plot.title = element_text(size=22),
    panel.grid = element_blank(), # blank grid
    axis.text=element_text(size=14), # axis text size
    axis.title = element_text(color="black", face="bold", size=18), # axis label text size
    panel.background = element_rect(fill = "white", # panel background
                                    colour = "black") # panel outline
  )

# Split the data 
t14 <- filter(no3fix, days == 14)
t48 <- filter(no3fix, days == 48)

#Re-order so controls are first
t14$trt <- factor(t14$trt, levels = c("L", "K", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J"))
t48$trt <- factor(t48$trt, levels = c("L", "K", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J"))

#Boxplots
ggplot(data = t14, aes(trt, tdn, colour = trt)) +
  scale_colour_manual(values=c("#0099FF",
                               "#333333",
                               "#CCFF00", # all the colours in hexadecimal code in order of "colour = x" argument above
                               "#99FF00",
                               "#66FF00",
                               "#00CC00",
                               "#336600",
                               "#FFCC33",
                               "#FF9933",
                               "#CC9933",
                               "#996600",
                               "#993300"
                              
  )) +
  geom_boxplot()

ggplot(data = t48, aes(trt, don_din, colour = trt)) +
  scale_colour_manual(values=c("#0099FF",
                               "#333333",
                               "#CCFF00", # all the colours in hexadecimal code in order of "colour = x" argument above
                               "#99FF00",
                               "#66FF00",
                               "#00CC00",
                               "#336600",
                               "#FFCC33",
                               "#FF9933",
                               "#CC9933",
                               "#996600",
                               "#993300"
                               
  )) +
  geom_boxplot()

lmer_tdn14 = lmer(tdn~trt+(1|block), data = t14)
anova(lmer_tdn14)
emmeans(lmer_tdn14, pairwise~trt)

