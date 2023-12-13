

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



