
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

### Spatial Data

Load Example Data

``` r
library(explore)
```

    ## Loading required package: leaflet

    ## Warning: replacing previous import 'ggplot2::last_plot' by 'plotly::last_plot'
    ## when loading 'explore'

    ## The legacy packages maptools, rgdal, and rgeos, underpinning this package
    ## will retire shortly. Please refer to R-spatial evolution reports on
    ## https://r-spatial.org/r/2023/05/15/evolution4.html for details.
    ## This package is now running under evolution status 0

``` r
library(sf)
```

    ## Linking to GEOS 3.11.0, GDAL 3.5.3, PROJ 9.1.0; sf_use_s2() is TRUE

``` r
#load spatial data for explore_points and explore_polygon functions
point_data <- st_sample(st_read(system.file("shape/nc.shp", package="sf")), 100)
```

    ## Reading layer `nc' from data source 
    ##   `/Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/library/sf/shape/nc.shp' 
    ##   using driver `ESRI Shapefile'
    ## Simple feature collection with 100 features and 14 fields
    ## Geometry type: MULTIPOLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
    ## Geodetic CRS:  NAD27

``` r
point_data2 <- st_as_sf(point_data)

polygon_data <- st_read(system.file("shape/nc.shp", package="sf"))
```

    ## Reading layer `nc' from data source 
    ##   `/Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/library/sf/shape/nc.shp' 
    ##   using driver `ESRI Shapefile'
    ## Simple feature collection with 100 features and 14 fields
    ## Geometry type: MULTIPOLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
    ## Geodetic CRS:  NAD27

Apply Functions

``` r
library(explore)
library(sf)
#points -- this produces two plots that are accessible through the back and forward arrows in the viewer
#explore::explore_points(point_data2)

#other usage for extracting single plots
#xplore_polygons()$plot_basic(polygon_data)

#explore_polygons(var_column = "FIPS")$plot_scaled(polygon_data)

#explore_polygons(var_column = "AREA")$plot_scaled(polygon_data)
```

<img src="man/figures/scaled.png" width="100%"/>

<img src="man/figures/Rplot" width="100%"/>
