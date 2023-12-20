library(testthat)
library(sf)
library(leaflet)

test_that("explore_points handles sf data correctly", {
  # Create a simple sf data frame for testing
  test_data <- st_as_sf(data.frame(lon = c(1, 2), lat = c(3, 4), variable = c(5, 6)),
                        coords = c("lon", "lat"))

  # Test if the function returns a list
  result <- explore_points(test_data)
  expect_true(is.list(result), "Result should be a list")

  # Test if the result list contains point_map and heat_map
  expect_true("point_map" %in% names(result), "Result should contain 'point_map'")
  expect_true("heat_map" %in% names(result), "Result should contain 'heat_map'")
})

test_that("explore_points throws an error for non-sf data", {
  # Create a data frame (non-sf) for testing
  test_data <- data.frame(lon = c(1, 2), lat = c(3, 4), variable = c(5, 6))

  # Test if the function throws an error for non-sf data
  expect_error(explore_points(test_data), "Input data should be of class 'sf'")
})



