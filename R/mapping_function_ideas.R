library(sf)
library(leaflet)
library(viridis)
library(leaflet.extras)


otherrendition <- function(data, variable = NULL) {

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
      addHeatmap(data = data) %>%
      addLegend(position = "bottomright", colors = "viridis", labels = "Heat Map")

    return(list(point_map = point_map, heat_map = heat_map))
  }

  # Check if the geometry is point or polygon
  is_polygon <- inherits(st_geometry(data)[[1]], c("POLYGON", "MULTIPOLYGON"))

  if (is_polygon) {
    # Polygon Data
    if (is.null(variable)) {
      stop("For polygon data, specify a variable to plot.")
    }

    outline_map <- basemap %>%
      addPolygons(data = data, color = "black", fillOpacity = 0, weight = 2) %>%
      addLegend(position = "bottomright", colors = "black", labels = "Outline Map")

    scaled_map <- basemap %>%
      addPolygons(data = data, fillColor = "blue", fillOpacity = 0.5, weight = 2,
                  highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                  label = ~paste(variable)) %>%
      addLegend(position = "bottomright", colors = "blue", labels = "Scaled Map")

    return(list(outline_map = outline_map, scaled_map = scaled_map))
  }

}

inj_res_subset <- subset(inj_res, City_x == c("MILWAUKEE", "Milwaukee") & Gender == "Female")
otherrendition(polygon_data, variable = "FIPS")
otherrendition(WI_census_tracts, variable = "COUNTYFP")
otherrendition(point_data2)
otherrendition(inj_res_subset)


