library(sf)
library(leaflet)
library(viridis)
library(leaflet.extras)
library(devtools)
library(ggplot2)
library(tmap)
library(dplyr)



#' Plot spatial data frames with class "POINT"
#' Given a spatial data frame will produce a list of different types of visualizations.
#'
#' @param dataframe with geometry of "point" class
#'
#'
#' @return A list of visualizations interactive visualizations accessible through exp. 'list$map'
#'
#' @examples
#' point_data <- st_sample(st_read(system.file("shape/nc.shp", package="sf")), 100)
#' point_data2 <- st_as_sf(point_data)
#' makeSpatialViz(point_data2)
#'
#'
#' @importFrom sf
#' @importFrom leaflet
#' @importFrom leaflet.extras
#' @importFrom tmap
#' @importFrom dplyr
#'
#' @export

makeSpatialViz <- function(data, variable = NULL) {

  if (!inherits(data, "sf")) {
    stop("Input data should be of class 'sf'")
  }

  # Check if the geometry is point or polygon
  is_point <- inherits(st_geometry(data)[[1]], "POINT")

  # Set base map
  basemap <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
    addProviderTiles("OpenStreetMap")

  #set color palette
  pal <- colorBin("YlOrRd", domain = variable)

  # Point Data
  if (is_point) {
    point_map <- basemap %>%
      addCircleMarkers(data = data) %>%
      addLegend(position = "bottomright", colors = "blue", labels = "Point Map")

    heat_map <- basemap %>%
      addHeatmap(data = data, radius = 10) %>%
      addLegend(position = "bottomright", colors = "viridis", labels = "Heat Map")

    return(list(point_map = point_map, heat_map = heat_map))
  }

}

#' Plot spatial data frames with class "POLYGON" or "MULTIPOLYGON"
#' Given a spatial data frame will produce a list of different types of visualizations.
#'
#' @param dataframe with geometry of "POLYGON" or "MULTIPOLYGON" class
#'
#' @param optional--variable variable to plot a scaled and/or gradient map
#'
#' @return A list of visualizations interactive visualizations accessible through exp. 'list$map'
#'
#' @examples
#' point_data <- st_sample(st_read(system.file("shape/nc.shp", package="sf")), 100)
#' point_data <- st_as_sf(point_data)
#'
#' @importFrom sf
#' @importFrom leaflet
#' @importFrom leaflet.extras
#' @importFrom tmap
#' @importFrom dplyr
#'
#' @export


plot_polygon_factory <- function(base_map_type = "OpenStreetMap", var_column = NULL) {

  tmap_mode("view")

  # Function to plot basic polygons
  plot_basic_polygons <- function(polygons) {
    tm_shape(polygons) +
      tm_borders() +
      tm_layout(title = "Basic Plot of Polygons")
  }

  # Function to plot scaled map of a user input variable
  plot_scaled_map <- function(polygons) {
    if (!is.null(var_column)) {
      tm_shape(polygons) +
        tm_borders() +
        tm_fill(col = var_column) +
        tm_layout(title = paste("Scaled Map of", var_column))
    } else {
      stop("Variable column name is missing.")
    }
  }

  # Function to plot gradient map of a user input variable
  plot_gradient_map <- function(polygons) {
    if (!is.null(var_column)) {
      tm_shape(polygons) +
        tm_borders() +
        tm_fill(col = var_column, style = "cont", title = var_column) +
        tm_layout(title = paste("Gradient Map of", var_column))
    } else {
      stop("Variable column name is missing.")
    }
  }

  # Function to generate the selected type of plot
  generate_plot <- function(polygons, plot_type = c("basic", "scaled", "gradient")) {
    if (plot_type %in% c("basic", "scaled", "gradient")) {
      switch(
        plot_type,
        "basic" = plot_basic_polygons(polygons),
        "scaled" = plot_scaled_map(polygons),
        "gradient" = plot_gradient_map(polygons)
      )
    } else {
      stop("Invalid plot type. Choose from 'basic', 'scaled', or 'gradient'.")
    }
  }

  # Function to create a base map
  create_base_map <- function() {
    tm_basemap(base.map = base_map_type) +
      tm_layout(title = "Base Map")
  }

  # Function to generate the final plot
  plot_final <- function(polygons, plot_type = "basic") {
    create_base_map() + generate_plot(polygons, plot_type)
  }

  return(list(
    plot_basic = plot_basic_polygons,
    plot_scaled = plot_scaled_map,
    plot_gradient = plot_gradient_map,
    plot_final = plot_final
  ))
}



####EXAMPLE_USAGE####


# inj_res <- st_read("~/Downloads/MCW/Research/Projects/Data/injury_and_residence.shp")
# inj_res <- st_as_sf(inj_res)
# inj_res_subset <- subset(inj_res, City_x == c("MILWAUKEE", "Milwaukee") & Gender == "Female")
# inj_res_subset <- head(inj_res_subset, 100)
# #tests
#
# WI_census_tracts <- st_read("~/Downloads/MCW/Research/Projects/Data/Wisconsin_CensusTL_Tract/WI_CensusTL_Tracts_2021.shp")
# WI_census_tracts <- st_as_sf(WI_census_tracts)
#
# point_data <- st_sample(st_read(system.file("shape/nc.shp", package="sf")), 100)
# point_data2 <- st_as_sf(point_data)
#
# polygon_data <- st_read(system.file("shape/nc.shp", package="sf"))
# polygon_maps <- makeSpatialViz(polygon_data, variable = "BIR74")
#
#
# makeSpatialViz(polygon_data, variable = "CNTY_ID")
# makeSpatialViz(WI_census_tracts, variable = "COUNTYFP")
# makeSpatialViz(point_data2)
# makeSpatialViz(inj_res_subset)
#
#
# plot_polygon_factory(var_column = "COUNTYFP")$plot_scaled(WI_census_tracts)
#
# plot_polygon_factory(var_column = "ALAND")$plot_gradient(WI_census_tracts)
#
# plot_polygon_factory()$plot_basic(polygon_data)
#
# plot_polygon_factory(var_column = "FIPS")$plot_scaled(polygon_data)
#
# plot_polygon_factory(var_column = "AREA")$plot_scaled(polygon_data)
#
#
