
#' Generic function for creating graphs for data
#' @param data The dataset to be visualize
#' @param ... All the other inputs depending on the type of graphes
#' @export
explore <- function(data,...) {
  UseMethod("explore")
}
#Clara's Functions
#' create peri-event histogram and raster plot for single_cell data
#' @importFrom gridExtra grid.arrange
#' @import ggplot2
#' @param data a [`single_cell`] object that has first column the spiking data and second column labeling the laps, trials or neurons
#' @param xaxis the x axis label of the graph, default value as "Time"
#' @param yaxis the y axis label of the raster plot, default value as "Trials"
#' @param stim The name of the highlighted region, default value as "Stimulus"
#' @param shade_on The starting x axis value for highlight/shading, default 0
#' @param shade_off The ending x axis value for highlight/shading, default 0
#' @param shade_color The coloring of the shading, default value as "pink"
#' @param ... Ignore
#' @return a plot with raster plot of neurons firing patterns and histogram of counts of the firing
#' @exportS3Method
explore.single_cell <- function(data,
                                xaxis="Time",
                                yaxis="Trials",
                                stim="Stimulus",
                                shade_on=0,
                                shade_off=0,
                                shade_color="pink",
                                ...
                                ) {
  # Plot

  # Raster Plot with adjusted line length using geom_linerange

  gg_raster <- ggplot(data) +
    geom_rect(aes(fill = stim),
              xmin = shade_on, xmax = shade_off,
              ymin = 0,
              ymax = Inf,
              alpha = 0.5
    ) +
    geom_linerange(aes(x = V1, y = V2, ymin = V2 - 0.3, ymax =  V2 + 0.3),
                   color = "black"
                   ) +
    scale_fill_manual(values = shade_color, guide = "none") +
    xlim(c(min(data$V1), max(data$V1))) +
    ylab("Laps") +
    xlab(xaxis)+
    ggtitle("Neuronal Spike Times")

  gg_psth <- ggplot(data) +
    geom_rect(aes(fill = stim),
              xmin = shade_on, xmax = shade_off,
              ymax = Inf,
              ymin = 0,
              alpha = 0.5
    ) +
    geom_histogram(aes(x = V1, y = ..count..), binwidth = 1, fill = "black", color = "black") +
    scale_fill_manual(values = shade_color) +
    xlim(c(min(data$V1), max(data$V1))) +
    ylab("Count") +
    xlab(xaxis)+
    ggtitle("Peri-Stimulus Time Histogram (PSTH)") +
    theme(legend.title = element_blank(),
          legend.position = c(.8, .8)
    )

  # Combine both plots
  grid.arrange(gg_raster, gg_psth, ncol = 1, heights = c(2, 1))

}



#Ari's Functions
#' Plot spatial data frames with class "POINT"
#' Given a spatial data frame will produce a list of different types of visualizations.
#'
#' @param data with geometry of "point" class
#' @param variable option to place a variable as user input
#'
#'
#' @return A list of visualizations interactive visualizations accessible through exp. 'list$map'
#'
#'
#'
#'
#' @import sf
#' @import leaflet
#' @import leaflet.extras
#' @import tmap
#' @import dplyr
#'
#' @export
explore_points <- function(data, variable = NULL) {

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
#' @param base_map_type default basemap is set to OpenStreetMap
#'
#' @param var_column optional variable to plot a scaled and/or gradient map
#'
#'
#' @return A list of visualizations interactive visualizations accessible through exp. 'list$map'
#'
#'
#' @import  sf
#' @import leaflet
#' @import leaflet.extras
#' @import tmap
#' @import dplyr
#'
#' @export
explore_polygons <- function(base_map_type = "OpenStreetMap", var_column = NULL) {

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
