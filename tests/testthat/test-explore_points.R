library(testthat)
library(sf)
library(leaflet)


#test for explore_points function
test_that("explore_points works as expected", {
  #create test data
  points <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

  #transform the data to WGS84 datum
  points <- st_transform(test_data, crs = st_crs("+proj=longlat +datum=WGS84"))

  #test if function returns a list
  result <- explore_points(points)
  expect_true(is.list(result))

  #test if the result list contains point_map and heat_map
  expect_true("point_map" %in% names(result))
  expect_true("heat_map" %in% names(result))

})
