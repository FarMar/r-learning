## This script takes the wrangled data from 15n_sol_proc.R and ../data/raw/15n_soil_sol.csv
## It examines the final processed data ../data/working/ratios.csv

library(tidyverse)
all <- read_csv("data/processed/ratios.csv")
all

## Now let's look how the variables change over time

## this page was helpful for mu and super/subscripts: 
## https://stackoverflow.com/questions/34892262/subscripts-and-superscripts-or-with-ggplot2-axis-labels-ionic-chemical

## also here: https://rstudio-pubs-static.s3.amazonaws.com/136237_170402e5f0b54561bf7605bdea98267a.html

## Below is a rather extensive set of code for xy plots with/without trends and CI ribbons

## Nitrate

ggplot(data = all, aes(days, no3, colour = trt)) + # x, y, colour by treatment
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
  geom_jitter(size = 4, alpha = 0.7, width = 0.5) + # Plot as a jittered point graph
  stat_smooth(method = "loess", se = FALSE, size = 1) + # Adds trend line only
  stat_smooth(method = "loess", # Adds ribbon for SE / CI and allows transparency to be adjusted
  #              colour = "red", 
                geom = "ribbon", 
                alpha = 0.05, #sets ribbon transparency
                size = 0.0001, #sets ribbon outline thickness. Need to work out how to make transparent
                fullrange = TRUE) + # Forces ribbon to end of data
 # xlim(0, 48) + ylim(0, 300) + # can be used to set hard limits to scale
  coord_cartesian(xlim = c(0, 48), ylim = c(0, 450)) + #sets soft limits to scale
  labs(
    x = "Time (days)", # x axis label
    y = ~NO[3]^{textstyle("-")}-N ~mg~L^{-1}, #y axis label, note cpde and weblink above for special characters and script
    #  title = "Nitrogen", # Graph title
    colour = "Treatment" # Legend Title
  ) +
  theme(
    panel.grid = element_blank(), # blank grid
    # axis.text.y = element_text(angle = 90), # rotates labels
    axis.text=element_text(size=14), # axis text size
    axis.title = element_text(color="black", face="bold", size=18), # axis label text size
    panel.background = element_rect(fill = "white", # panel background
                                    colour = "black") # panel outline
  )

## Ammonium

ggplot(data = all, aes(days, nh4, colour = trt)) + # x, y, colour by treatment
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
  geom_jitter(size = 4, alpha = 0.7, width = 0.5) + # Plot as a jittered point graph
  stat_smooth(method = "loess", se = FALSE, size = 1) + # Adds trend line only
  stat_smooth(method = "loess", # Adds ribbon for SE / CI and allows transparency to be adjusted
              #              colour = "red", 
              geom = "ribbon", 
              alpha = 0.05, #sets ribbon transparency
              size = 0.0001, #sets ribbon outline thickness. Need to work out how to make transparent
              fullrange = TRUE) + # Forces ribbon to end of data
  # xlim(0, 48) + ylim(0, 300) + # can be used to set hard limits to scale
 # coord_cartesian(xlim = c(0, 48), ylim = c(0, 450)) + #sets soft limits to scale
  labs(
    x = "Time (days)", # x axis label
    y = ~NH[4]^{textstyle("+")}-N ~mg~L^{-1}, #y axis label, note cpde and weblink above for special characters and script
    #  title = "Nitrogen", # Graph title
    colour = "Treatment" # Legend Title
  ) +
  theme(
    panel.grid = element_blank(), # blank grid
    # axis.text.y = element_text(angle = 90), # rotates labels
    axis.text=element_text(size=14), # axis text size
    axis.title = element_text(color="black", face="bold", size=18), # axis label text size
    panel.background = element_rect(fill = "white", # panel background
                                    colour = "black") # panel outline
  )

## FAA

ggplot(data = all, aes(days, faa, colour = trt)) + # x, y, colour by treatment
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
  geom_jitter(size = 4, alpha = 0.7, width = 0.5) + # Plot as a jittered point graph
  stat_smooth(method = "loess", se = FALSE, size = 1) + # Adds trend line only
  stat_smooth(method = "loess", # Adds ribbon for SE / CI and allows transparency to be adjusted
              #              colour = "red", 
              geom = "ribbon", 
              alpha = 0.05, #sets ribbon transparency
              size = 0.0001, #sets ribbon outline thickness. Need to work out how to make transparent
              fullrange = TRUE) + # Forces ribbon to end of data
  # xlim(0, 48) + ylim(0, 300) + # can be used to set hard limits to scale
  # coord_cartesian(xlim = c(0, 48), ylim = c(0, 450)) + #sets soft limits to scale
  labs(
    x = "Time (days)", # x axis label
    y = FAA-N ~mg~L^{-1}, #y axis label, note cpde and weblink above for special characters and script
    #  title = "Nitrogen", # Graph title
    colour = "Treatment" # Legend Title
  ) +
  theme(
    panel.grid = element_blank(), # blank grid
    # axis.text.y = element_text(angle = 90), # rotates labels
    axis.text=element_text(size=14), # axis text size
    axis.title = element_text(color="black", face="bold", size=18), # axis label text size
    panel.background = element_rect(fill = "white", # panel background
                                    colour = "black") # panel outline
  )

## TDN

ggplot(data = all, aes(days, tdn, colour = trt)) + # x, y, colour by treatment
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
  geom_jitter(size = 4, alpha = 0.7, width = 0.5) + # Plot as a jittered point graph
  stat_smooth(method = "loess", se = FALSE, size = 1) + # Adds trend line only
  stat_smooth(method = "loess", # Adds ribbon for SE / CI and allows transparency to be adjusted
              #              colour = "red", 
              geom = "ribbon", 
              alpha = 0.05, #sets ribbon transparency
              size = 0.0001, #sets ribbon outline thickness. Need to work out how to make transparent
              fullrange = TRUE) + # Forces ribbon to end of data
  # xlim(0, 48) + ylim(0, 300) + # can be used to set hard limits to scale
  # coord_cartesian(xlim = c(0, 48), ylim = c(0, 450)) + #sets soft limits to scale
  labs(
    x = "Time (days)", # x axis label
    y = TDN ~mg~L^{-1}, #y axis label, note cpde and weblink above for special characters and script
    #  title = "Nitrogen", # Graph title
    colour = "Treatment" # Legend Title
  ) +
  theme(
    panel.grid = element_blank(), # blank grid
    # axis.text.y = element_text(angle = 90), # rotates labels
    axis.text=element_text(size=14), # axis text size
    axis.title = element_text(color="black", face="bold", size=18), # axis label text size
    panel.background = element_rect(fill = "white", # panel background
                                    colour = "black") # panel outline
  )

## DOC

ggplot(data = all, aes(days, doc, colour = trt)) + # x, y, colour by treatment
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
  geom_jitter(size = 4, alpha = 0.7, width = 0.5) + # Plot as a jittered point graph
  stat_smooth(method = "loess", se = FALSE, size = 1) + # Adds trend line only
  stat_smooth(method = "loess", # Adds ribbon for SE / CI and allows transparency to be adjusted
              #              colour = "red", 
              geom = "ribbon", 
              alpha = 0.05, #sets ribbon transparency
              size = 0.0001, #sets ribbon outline thickness. Need to work out how to make transparent
              fullrange = TRUE) + # Forces ribbon to end of data
  # xlim(0, 48) + ylim(0, 300) + # can be used to set hard limits to scale
  # coord_cartesian(xlim = c(0, 48), ylim = c(0, 450)) + #sets soft limits to scale
  labs(
    x = "Time (days)", # x axis label
    y = DOC ~mg~L^{-1}, #y axis label, note cpde and weblink above for special characters and script
    #  title = "Nitrogen", # Graph title
    colour = "Treatment" # Legend Title
  ) +
  theme(
    panel.grid = element_blank(), # blank grid
    # axis.text.y = element_text(angle = 90), # rotates labels
    axis.text=element_text(size=14), # axis text size
    axis.title = element_text(color="black", face="bold", size=18), # axis label text size
    panel.background = element_rect(fill = "white", # panel background
                                    colour = "black") # panel outline
  )

## DON

ggplot(data = all, aes(days, don, colour = trt)) + # x, y, colour by treatment
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
  geom_jitter(size = 4, alpha = 0.7, width = 0.5) + # Plot as a jittered point graph
  stat_smooth(method = "loess", se = FALSE, size = 1) + # Adds trend line only
  stat_smooth(method = "loess", # Adds ribbon for SE / CI and allows transparency to be adjusted
              #              colour = "red", 
              geom = "ribbon", 
              alpha = 0.05, #sets ribbon transparency
              size = 0.0001, #sets ribbon outline thickness. Need to work out how to make transparent
              fullrange = TRUE) + # Forces ribbon to end of data
  # xlim(0, 48) + ylim(0, 300) + # can be used to set hard limits to scale
  # coord_cartesian(xlim = c(0, 48), ylim = c(0, 450)) + #sets soft limits to scale
  labs(
    x = "Time (days)", # x axis label
    y = DON ~mg~L^{-1}, #y axis label, note cpde and weblink above for special characters and script
    #  title = "Nitrogen", # Graph title
    colour = "Treatment" # Legend Title
  ) +
  theme(
    panel.grid = element_blank(), # blank grid
    # axis.text.y = element_text(angle = 90), # rotates labels
    axis.text=element_text(size=14), # axis text size
    axis.title = element_text(color="black", face="bold", size=18), # axis label text size
    panel.background = element_rect(fill = "white", # panel background
                                    colour = "black") # panel outline
  )


## DOC:DON

ggplot(data = all, aes(days, doc_don, colour = trt)) + # x, y, colour by treatment
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
  geom_jitter(size = 4, alpha = 0.7, width = 0.5) + # Plot as a jittered point graph
  stat_smooth(method = "loess", se = FALSE, size = 1) + # Adds trend line only
  stat_smooth(method = "loess", # Adds ribbon for SE / CI and allows transparency to be adjusted
              #              colour = "red", 
              geom = "ribbon", 
              alpha = 0.05, #sets ribbon transparency
              size = 0.0001, #sets ribbon outline thickness. Need to work out how to make transparent
              fullrange = TRUE) + # Forces ribbon to end of data
  # xlim(0, 48) + ylim(0, 300) + # can be used to set hard limits to scale
  # coord_cartesian(xlim = c(0, 48), ylim = c(0, 450)) + #sets soft limits to scale
  labs(
    x = "Time (days)", # x axis label
    y = "DOC:DON ratio", #y axis label, note cpde and weblink above for special characters and script
    #  title = "Nitrogen", # Graph title
    colour = "Treatment" # Legend Title
  ) +
  theme(
    panel.grid = element_blank(), # blank grid
    # axis.text.y = element_text(angle = 90), # rotates labels
    axis.text=element_text(size=14), # axis text size
    axis.title = element_text(color="black", face="bold", size=18), # axis label text size
    panel.background = element_rect(fill = "white", # panel background
                                    colour = "black") # panel outline
  )

## DON:DIN

ggplot(data = all, aes(days, don_din, colour = trt)) + # x, y, colour by treatment
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
  geom_jitter(size = 4, alpha = 0.7, width = 0.5) + # Plot as a jittered point graph
  stat_smooth(method = "loess", se = FALSE, size = 1) + # Adds trend line only
  stat_smooth(method = "loess", # Adds ribbon for SE / CI and allows transparency to be adjusted
              #              colour = "red", 
              geom = "ribbon", 
              alpha = 0.05, #sets ribbon transparency
              size = 0.0001, #sets ribbon outline thickness. Need to work out how to make transparent
              fullrange = TRUE) + # Forces ribbon to end of data
  # xlim(0, 48) + ylim(0, 300) + # can be used to set hard limits to scale
  # coord_cartesian(xlim = c(0, 48), ylim = c(0, 450)) + #sets soft limits to scale
  labs(
    x = "Time (days)", # x axis label
    y = "DON:DIN ratio", #y axis label, note cpde and weblink above for special characters and script
    #  title = "Nitrogen", # Graph title
    colour = "Treatment" # Legend Title
  ) +
  theme(
    panel.grid = element_blank(), # blank grid
    # axis.text.y = element_text(angle = 90), # rotates labels
    axis.text=element_text(size=14), # axis text size
    axis.title = element_text(color="black", face="bold", size=18), # axis label text size
    panel.background = element_rect(fill = "white", # panel background
                                    colour = "black") # panel outline
  )

#############################################################################################

## NOW LET'S LOOK AT SOME VARIABLES AGAINST ONEANOTHER WITH TIME AS SPOT SIZE##

############################################################################################

## NO3 vs NH4
ggplot(data = all, aes(no3, nh4, colour = trt, size = days)) + # x, y, colour by treatment
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
  geom_point(alpha = 0.6) + # Plot as a point graph, size, transparency
  #geom_jitter(size = 4, alpha = 0.7, width = 0.5) + # Plot as a jittered point graph
  #stat_smooth(method = "loess", se = FALSE, size = 1) + # Adds trend line only
  #stat_smooth(method = "loess", # Adds ribbon for SE / CI and allows transparency to be adjusted
              #              colour = "red", 
  #            geom = "ribbon", 
  #            alpha = 0.05, #sets ribbon transparency
  #            size = 0.0001, #sets ribbon outline thickness. Need to work out how to make transparent
  #            fullrange = TRUE) + # Forces ribbon to end of data
  # xlim(0, 48) + ylim(0, 300) + # can be used to set hard limits to scale
  #coord_cartesian(xlim = c(0, 48), ylim = c(0, 450)) + #sets soft limits to scale
  scale_x_log10() +
  scale_y_log10() +
  labs(
    x = ~NO[3]^{textstyle("-")}-N ~mg~L^{-1}, # x axis label
    y = ~NH[4]^{textstyle("-")}-N ~mg~L^{-1}, #y axis label, note cpde and weblink above for special characters and script
    #  title = "Nitrogen", # Graph title
    colour = "Treatment" # Legend Title
  ) +
  theme(
    panel.grid = element_blank(), # blank grid
    # axis.text.y = element_text(angle = 90), # rotates labels
    axis.text=element_text(size=14), # axis text size
    axis.title = element_text(color="black", face="bold", size=18), # axis label text size
    panel.background = element_rect(fill = "white", # panel background
                                    colour = "black") # panel outline
  )

## DIN vs DON

ggplot(data = all, aes(din, don, colour = trt, size = days)) + # x, y, colour by treatment
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
  geom_point(alpha = 0.6) + # Plot as a point graph, size, transparency
  #geom_jitter(size = 4, alpha = 0.7, width = 0.5) + # Plot as a jittered point graph
  #stat_smooth(method = "loess", se = FALSE, size = 1) + # Adds trend line only
  #stat_smooth(method = "loess", # Adds ribbon for SE / CI and allows transparency to be adjusted
  #              colour = "red", 
  #            geom = "ribbon", 
  #            alpha = 0.05, #sets ribbon transparency
  #            size = 0.0001, #sets ribbon outline thickness. Need to work out how to make transparent
  #            fullrange = TRUE) + # Forces ribbon to end of data
  # xlim(0, 48) + ylim(0, 300) + # can be used to set hard limits to scale
  #coord_cartesian(xlim = c(0, 48), ylim = c(0, 450)) + #sets soft limits to scale
  #scale_x_log10() +
  #scale_y_log10() +
  labs(
    x = DIN ~mg~L^{-1}, # x axis label
    y = DON ~mg~L^{-1}, #y axis label, note cpde and weblink above for special characters and script
    #  title = "Nitrogen", # Graph title
    colour = "Treatment" # Legend Title
  ) +
  theme(
    panel.grid = element_blank(), # blank grid
    # axis.text.y = element_text(angle = 90), # rotates labels
    axis.text=element_text(size=14), # axis text size
    axis.title = element_text(color="black", face="bold", size=18), # axis label text size
    panel.background = element_rect(fill = "white", # panel background
                                    colour = "black") # panel outline
  )

## DOC vs DON

ggplot(data = all, aes(doc, don, colour = trt, size = days)) + # x, y, colour by treatment
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
  geom_point(alpha = 0.6) + # Plot as a point graph, size, transparency
  #geom_jitter(size = 4, alpha = 0.7, width = 0.5) + # Plot as a jittered point graph
  #stat_smooth(method = "loess", se = FALSE, size = 1) + # Adds trend line only
  #stat_smooth(method = "loess", # Adds ribbon for SE / CI and allows transparency to be adjusted
  #              colour = "red", 
  #            geom = "ribbon", 
  #            alpha = 0.05, #sets ribbon transparency
  #            size = 0.0001, #sets ribbon outline thickness. Need to work out how to make transparent
  #            fullrange = TRUE) + # Forces ribbon to end of data
  # xlim(0, 48) + ylim(0, 300) + # can be used to set hard limits to scale
  #coord_cartesian(xlim = c(0, 48), ylim = c(0, 450)) + #sets soft limits to scale
  #scale_x_log10() +
  #scale_y_log10() +
  labs(
    x = DOC ~mg~L^{-1}, # x axis label
    y = DON ~mg~L^{-1}, #y axis label, note cpde and weblink above for special characters and script
    #  title = "Nitrogen", # Graph title
    colour = "Treatment" # Legend Title
  ) +
  theme(
    panel.grid = element_blank(), # blank grid
    # axis.text.y = element_text(angle = 90), # rotates labels
    axis.text=element_text(size=14), # axis text size
    axis.title = element_text(color="black", face="bold", size=18), # axis label text size
    panel.background = element_rect(fill = "white", # panel background
                                    colour = "black") # panel outline
  )

## DOC vs TDN

ggplot(data = all, aes(doc, tdn, colour = trt, size = days)) + # x, y, colour by treatment
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
  geom_point(alpha = 0.6) + # Plot as a point graph, size, transparency
  #geom_jitter(size = 4, alpha = 0.7, width = 0.5) + # Plot as a jittered point graph
  #stat_smooth(method = "loess", se = FALSE, size = 1) + # Adds trend line only
  #stat_smooth(method = "loess", # Adds ribbon for SE / CI and allows transparency to be adjusted
  #              colour = "red", 
  #            geom = "ribbon", 
  #            alpha = 0.05, #sets ribbon transparency
  #            size = 0.0001, #sets ribbon outline thickness. Need to work out how to make transparent
  #            fullrange = TRUE) + # Forces ribbon to end of data
  # xlim(0, 48) + ylim(0, 300) + # can be used to set hard limits to scale
  #coord_cartesian(xlim = c(0, 48), ylim = c(0, 450)) + #sets soft limits to scale
  #scale_x_log10() +
#scale_y_log10() +
labs(
  x = DOC ~mg~L^{-1}, # x axis label
  y = TDN ~mg~L^{-1}, #y axis label, note cpde and weblink above for special characters and script
  #  title = "Nitrogen", # Graph title
  colour = "Treatment" # Legend Title
) +
  theme(
    panel.grid = element_blank(), # blank grid
    # axis.text.y = element_text(angle = 90), # rotates labels
    axis.text=element_text(size=14), # axis text size
    axis.title = element_text(color="black", face="bold", size=18), # axis label text size
    panel.background = element_rect(fill = "white", # panel background
                                    colour = "black") # panel outline
  )

######################################################################################

## BACK TO VS TIME

######################################################################################

## doc nitrate

ggplot(data = all, aes(days, doc, colour = trt, size = no3)) + # x, y, colour by treatment
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
  stat_smooth(method = "loess", # Adds ribbon for SE / CI and allows transparency to be adjusted
              #              colour = "red", 
              geom = "ribbon", 
              alpha = 0.05, #sets ribbon transparency
              size = 0.0001, #sets ribbon outline thickness. Need to work out how to make transparent
              fullrange = TRUE) + # Forces ribbon to end of data
  # xlim(0, 48) + ylim(0, 300) + # can be used to set hard limits to scale
  coord_cartesian(xlim = c(0, 48), ylim = c(0, 450)) + #sets soft limits to scale
  labs(
    x = "Time (days)", # x axis label
    y = DOC ~mg~L^{-1}, #y axis label, note cpde and weblink above for special characters and script
    #  title = "Nitrogen", # Graph title
    colour = "Treatment" # Legend Title
  ) +
  theme(
    panel.grid = element_blank(), # blank grid
    # axis.text.y = element_text(angle = 90), # rotates labels
    axis.text=element_text(size=14), # axis text size
    axis.title = element_text(color="black", face="bold", size=18), # axis label text size
    panel.background = element_rect(fill = "white", # panel background
                                    colour = "black") # panel outline
  )

## tdn doc

ggplot(data = all, aes(days, tdn, colour = trt, size = don)) + # x, y, colour by treatment
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
    y = TDN ~mg~L^{-1}, #y axis label, note cpde and weblink above for special characters and script
    #  title = "Nitrogen", # Graph title
    colour = "Treatment", # Legend Title
    size = "DON"
  ) +
  theme(
    panel.grid = element_blank(), # blank grid
    # axis.text.y = element_text(angle = 90), # rotates labels
    axis.text=element_text(size=14), # axis text size
    axis.title = element_text(color="black", face="bold", size=18), # axis label text size
    panel.background = element_rect(fill = "white", # panel background
                                    colour = "black") # panel outline
  )
