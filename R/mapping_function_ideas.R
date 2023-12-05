
# Load necessary libraries
install.packages("sf")
install.packages("leaflet")
install.packages("viridis")
install.packages("leaflet.extras")

library(sf)
library(leaflet)
library(viridis)
library(leaflet.extras)

# Define your function
makeSM2 <- function(data, variable = NULL) {
  if (!inherits(data, "sf")) {
    stop("Input data should be of class 'sf'")
  }

  # Check if the geometry is point or polygon
  is_point <- length(unique(st_geometry_type(data))) == 1

  # Reproject data to WGS84
  data <- st_transform(data, crs = st_crs("+proj=longlat +datum=WGS84"))

  # Set base map
  basemap <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
    addProviderTiles("OpenStreetMap")

  # Set color palette
  pal <- colorBin("YlOrRd", domain = variable)

  # Point Data
  if (is_point) {
    point_map <- basemap %>%
      addCircleMarkers(data = data) %>%
      addLegend(position = "bottomright", colors = "blue", labels = "Point Map")

    heat_map <- basemap %>%
      addHeatmap(data = data) %>%
      addLegend(position = "bottomright", colors = "viridis", labels = "Heat Map")

    return(list(point_map = point_map, heat_map = heat_map))
  }

  # Polygon Data
  if (!is.null(variable)) {
    outline_map <- basemap %>%
      addPolygons(data = data, color = "black", fillOpacity = 0, weight = 2) %>%
      addLegend(position = "bottomright", colors = "black", labels = "Outline Map")

    scaled_map <- basemap %>%
      addPolygons(data = data, fillColor = ~pal(density), fillOpacity = 0.5, weight = 2,
                  highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                  label = ~paste(variable)) %>%
      addLegend(pal = pal, values = ~density, opacity = 0.7,
                position = "bottomright", labels = "Scaled Map")

    return(list(outline_map = outline_map, scaled_map = scaled_map))
  } else {
    stop("For polygon data, specify a variable to plot.")
  }
}

# Example with point data
point_data <- st_sample(st_read(system.file("shape/nc.shp", package="sf")), 100)
point_data <- st_as_sf(point_data)
point_maps <- makeSM2(point_data)

# Display the point map
point_maps$point_map

# Display the heat map
point_maps$heat_map

#test with other data sets. This one comes from and injury registry at Froetdert Hospital in Milwaukee, WI
inj_res <- st_read("~/Downloads/MCW/Research/Projects/Data/injury_and_residence.shp")
inj_res <- st_as_sf(inj_res)
inj_res_point_maps <- makeSM2(inj_res)

inj_res_point_maps$point_map
inj_res_point_maps$heat_map

# Example with polygon data and a variable to plot
polygon_data <- st_read(system.file("shape/nc.shp", package="sf"))
polygon_maps <- makeSM2(polygon_data, variable = "BIR74")

# Display the outline map
polygon_maps$outline_map

# Display the scaled map
polygon_maps$scaled_map

#tests
inherits(st_geometry('Geometry type' == 'POINT'))

#fix aethetics
#test on other df
