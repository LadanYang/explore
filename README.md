
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Explore

The goal of explore is to automatically generate visualizations given
variables in a dataset across the disciplines statistics, geography, and
neuroscience. It generates interactive graphs such scatterplots and
boxplots for statistical analysis; maps for geographic information (GIS)
data; and raster plots for the firing of a neuron.

## Installation

You can install the development version of explore from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("LadanYang/explore")
```

## Examples

``` r
library(explore)
```

### Raster plot

This is a basic example which shows you how to create a raster and
perievent histogram for neurons spiking:

``` r
##convert matlab file first
cell_data<-single_cell("https://github.com/LadanYang/explore/raw/main/data-raw/xydata.mat")
# Create the raster perievent hist, only single_cell is required, all others have default value
explore(cell_data,
        xaxis="Time",
        yaxis="Trials",
        stim="Stimulus",
        shade_on=120,
        shade_off=150,
        shade_color="pink")
```

<img src="man/figures/raster_PSTH_example.jpg" width="100%"/>

### Statistical Analysis

The following examples illustrate ways to visualize the distribution of
variables in your dataframe

*One numeric variable*

``` r
num_1(iris, Sepal.Length)
```

*Two numeric variables*

``` r
 num_2(iris, Sepal.Length, Sepal.Width)
```

*One categorical variable*

``` r
cat_1(iris, Species)
```

*One numeric variable and one categorical variable*

``` r
num1_cat1(iris, Sepal.Length, Species)
```

*Two numeric variables and one categorical variable*

``` r
num2_cat1(iris, Sepal.Length, Sepal.Width, Species)
```

*One numeric variable and two categorical variables*

``` r
# Creating new categorical column in the iris dataset
iris <- iris
random <- c("a", "b", "c")
iris$cat <- sample(random, size = nrow(data), replace = TRUE)

num1_cat2(iris, cat, Sepal.Length, Species)
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/raster_PSTH_example.jpg" width="100%"/>

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
