# How to get significance listed in `emmeans` multiple comparisons tables

```
emmeans_2_contrast_R_sig <- emmeans_2_contrast_R %>%
    mutate(Significance = case_when(
    p.value < 0.001  ~ "***",
    p.value < 0.01   ~ "**",
    p.value < 0.05   ~ "*",
    TRUE             ~ "Not Significant"))
```
need to make emmeans into dataframe first

```
#emmGrid with 2 bits $emmeans and $contrasts
emmeans_2_values <- summary(emmeans_2)$emmeans
emmeans_2_contrasts <- summary(emmeans_2)$contrasts
```

Bring image into R markdown and resize

```
![](C:\Users\ouz001\DATASCHOOL\stats\nz_vine\SquireD_samples.png){ width=50% }
```