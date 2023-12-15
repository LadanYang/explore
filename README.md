
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Explore

The goal of explore is to automatically generate visualizations given variables 
in a dataset across the disciplines statistics, geography, and neuroscience. It 
generates interactive graphs such scatterplots and boxplots for statistical analysis; 
maps for geographic information (GIS) data; and raster plots for the firing of a neuron.

## Installation

You can install the development version of explore from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("LadanYang/explore")
```

## Example

This is a basic example which shows you how to create a raster and
perievent histogram for neurons spiking:

``` r
library(explore)
##create raster plot
##convert matlab file first
cell_data<-single_cell("https://github.com/LadanYang/explore/raw/main/data-raw/xydata.mat")
#create the raster perievent hist, only single_cell is required, all others have default value
explore(cell_data,
        xaxis="Time",
        yaxis="Trials",
        stim="Stimulus",
        shade_on=120,
        shade_off=150,
        shade_color="pink")
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/raster_PSTH_example.jpg" width="100%"/>

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.

\##Explore Mapping Functions

Explore mapping functions allow users to input point or
polygon/multipolygon data.
