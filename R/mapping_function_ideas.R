library(sf)
library(leaflet)
library(viridis)
library(leaflet.extras)
library(devtools)



#' Plot spatial data frames bases on data type exp. "MULTIPOLYGON/POLYGON", and "POINT"
#' Given a spatial data frame of either polygon, multipolygon, or point geometry class, will produce a list of different types of visualizations.
#'
#' @param dataframe with geometry of either polygon/multipolygon or point class
#'
#' @param optional variable to plot a scaled and/or gradient map
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
#' @importFrom viridis
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

  # Check if the geometry is point or polygon
  is_polygon <- inherits(st_geometry(data)[[1]], c("POLYGON", "MULTIPOLYGON"))

  qpal <- colorQuantile("RdYlBu", countries$gdp_md_est, n = 5)
  map

  if (is_polygon) {
    # Polygon Data
    if (is.null(variable)) {
      stop("For polygon data, specify a variable to plot.")
    }

    outline_map <- basemap %>%
      addPolygons(data = data, color = "black", fillOpacity = 0, weight = 2) %>%
      addLegend(position = "bottomright", colors = "black", labels = "Outline Map")

    #scaled_map <- basemap %>%
      #addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
                  #color = ~pal(gdp_md_est)) %>%
      #addLegend("bottomright", pal = pal, values = ~variable,
                #title = "Est. GDP (2010)",
                #labFormat = labelFormat(prefix = "$"),
                #opacity = 1)
    scaled_map <- basemap %>%
      addPolygons(
        data = data,
        fillColor = ~pal(variable),
        fillOpacity = 0.5,
        weight = 2,
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
        label = ~paste(variable)
      ) %>%
      addLegend(position = "bottomright", colors = "blue", labels = "Scaled Map")

    gradient_map <- basemap %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
                  color = ~pal(unique(variable))
      ) %>%
      addLegend(pal = qpal, values = ~variable, opacity = 1)

    return(list(outline_map = outline_map, scaled_map = scaled_map, gradient_map = gradient_map))
  }

}

#fillColor = pal(data$variable), fillOpacity = 0.5,weight = 2,highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),

#addLegend("bottomright", pal = pal, values = ~variable,
#title = "Est. GDP (2010)",
#labFormat = labelFormat(prefix = "$"),
#opacity = 1


#scaled map
#addPolygons(data = data,
#label = ~paste(variable),
#stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
#color = ~qpal(variable)) %>%

  #addLegend(position = "bottomright", colors = "blue", labels = "Scaled Map")
inj_res <- st_read("~/Downloads/MCW/Research/Projects/Data/injury_and_residence.shp")
inj_res <- st_as_sf(inj_res)
inj_res_subset <- subset(inj_res, City_x == c("MILWAUKEE", "Milwaukee") & Gender == "Female")
inj_res_subset <- head(inj_res_subset, 100)
#tests

WI_census_tracts <- st_read("~/Downloads/MCW/Research/Projects/Data/Wisconsin_CensusTL_Tract/WI_CensusTL_Tracts_2021.shp")
WI_census_tracts <- st_as_sf(WI_census_tracts)

point_data <- st_sample(st_read(system.file("shape/nc.shp", package="sf")), 100)
point_data2 <- st_as_sf(point_data)

polygon_data <- st_read(system.file("shape/nc.shp", package="sf"))
polygon_maps <- otherrendition(polygon_data, variable = "BIR74")


makeSpatialViz(polygon_data, variable = 'FIPS')
makeSpatialViz(WI_census_tracts, variable = "COUNTYFP")
makeSpatialViz(point_data2)
makeSpatialViz(inj_res_subset)



