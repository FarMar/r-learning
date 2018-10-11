# r-learning - an ongoing saga
Multiple folders of random R code, exercises, and learnings. This repository is the product of my R learnings whilst attending AFDS, particularly stats tuition by [Dr Terry Neeman (ANU)](https://services.anu.edu.au/business-units/statistical-consulting-unit/dr-terry-neeman), and also collates previous knowledge gained from Dr Alain Zuur and associates at [Highland Statistics](http://www.highstat.com/)*. I have also taken a look at the [WSU R course notes](https://www.westernsydney.edu.au/hie/events/data_analysis_r) provided by Remko Duursma & Jeff Powell.

It will hopefully grow over time, and I'll add a summary up here and more in-depth text under headings for each new concept below. Given that we're using R here, most of the analysis etc will be reported using RMarkdown outputs from knitR.


*Note: code and tutorials from the HighStat course are protected by copyright and cannot be shared. The only shared content here is code for my own data following the principles learned in the HighStat course.

_________

## AFDS course material from Terry Neeman

###Chapter 0 

Covered background to statistical modelling and pitfalls of poor design.

###Chapter 1 

Starts on t-tests then simple linear models for the wheat dataset from agridat. 

A useful data-wrangling trick is given here:

```
wheat$Variety <- relevel(wheat$Variety, ref = "Standard")
```

This makes the "Standard" variety of wheat (in the "Variety" variable appear as the reference, and means that it will be plotted first as opposed to teh default of alphanumeric order.

**HOWEVER**, the above only works in base R. When working with the tidyverse, this command does the same trick:

```
fct_relevel(wheat$Variety, "Standard")
```
This is part of the [forcats package](https://forcats.tidyverse.org/reference/fct_relevel.html), with examples given.

**BUT...** that didn't work either when it came to running ggplot. Thankfully, the internet is a wonderful place and forcing levels within the factor command, as shown [here](https://sebastiansauer.github.io/ordering-bars/) works a treat. As ever with the R journey, `steps forward ≈ (steps backwards + 0.1)`

```
wheat$Variety <- factor(wheat$Variety, levels = c("Standard", "New"))
```
And this is supposed to be the easy part!

At least subsetting (filtering in the tidyverse) is pretty easy. Lots more [here](https://dplyr.tidyverse.org/reference/filter.html), but this was an easy translation from Terry's base R code to tidyverse code:

```
wheat_H <- filter(wheat, Variation == "High")
wheat_L <- filter(wheat, Variation == "Low")
```

Now data is all sorted, the initial t-tests and 1-way ANOVA run simply. The `summary(lm1)` command gives info on the residuals and coefficients, goodness of fit, etc. `emmeans(lm1, ~Variety)` provides output of estimated marginal means of model.

The final task for this sub-section is to plot the output of the model. 
Check out `Chapter_1.Rmd` in the outputs folder for mopre in-depth info.

Further info for subsequent chapters is in the R Markdown file.

### Chapter 2
This covered an introdution to experimental design

### Chapter 3
This provides an introduction to mean variance and statistical modelling. Details are again in the appropriate Rmd file. 

All went smoothly. One useful tip picked up from [Stack Overflow](https://stackoverflow.com/questions/37115276/control-alignment-of-two-side-by-side-plots-in-knitr)  was how to get the two diagnostics plot on the same line using knitR:

```
{r,echo=TRUE, out.width='.49\\linewidth', fig.width=3, fig.height=3,fig.show='hold',fig.align='center'}
```

This can obviously be fiddled with to work for whatever. Not sure if it works for ggplot outputs though.... Perhaps something to read up further on if knitRing becomes a more serious exercise for me.

### Chapter 4
Now we're looking at more complex models that take into consideration blocking etc. `lmerTest` is used here to allow a mixed model to be chosen that includes block as a random factor, e.g.:

```
model2=lmer(PhotoRate~Temp+(1|Position), data=photosynthesis)
anova(model2)
```
