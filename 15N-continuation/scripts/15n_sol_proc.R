## This is the first Data School exercise
## We are looking at tidying and graphically presenting data

## My chosen dataset is the soil solution nitrogen data from the GRDC Biological Inputs 15N pot experiment

# first, load tidyverse

library(tidyverse)
raw_data <- read_csv("results/raw/15n_soil_sol.csv")
raw_data

## First we need to create a don variable by subtracting "no3" and "nh4" from "tdn"

# make a 'din' variable

with_din <- mutate(raw_data, nh4 + no3)
with_din

# rename using base R "names function. Note the "[11]" is the column number. 
# But... use this with care as if columns are mis-ordered it can result in wrong data

names(with_din)[11] <- "din"
with_din

# now let's subtract din from tdn to make don
# here we're first making a straight subtraction of din from tdn, but this always brings negatives
# the case_when function makes a second new column don_nz that returns a 0 when the value is < 1

with_don <- with_din %>% 
  mutate(don = tdn - din,
         don_nz = case_when(
           don >= 0 ~ don,
           TRUE ~ 0
         ))


##do we have any negatives?
count(with_don, don_nz < 0)

##tidying...
don_fixed <- select(with_don, -don)
names(don_fixed)[12] <- "don"

##make some ratios
ratios <- mutate(don_fixed, doc_don = doc / don )
ratios <- mutate(ratios, don_din = don / din )
ratios

##Now we have some Inf. these need to become NA
# solution from https://stackoverflow.com/questions/12188509/cleaning-inf-values-from-an-r-dataframe

is.na(ratios) <- do.call(cbind,lapply(ratios, is.infinite))
ratios

##so, with clean data, all that's left to do is write the final .csv, then knitR
write_csv(ratios, "results/processed/ratios.csv") # DON'T FORGET THE QUOTATIONS ON THE PATH!!!!


