library(testthat)
library(tmap)


#test for explore_polygons function
test_that("explore_polygons works as expected", {
  #create test data
  polygons <- st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)

  #test if the function returns a list
  result <- explore_polygons(polygons)

  #test if the result list contains plot_basic, plot_scaled, plot_gradient, and plot_final
  expect_true("plot_basic" %in% names(result))
  expect_true("plot_scaled" %in% names(result))
  expect_true("plot_gradient" %in% names(result))
  expect_true("plot_final" %in% names(result))

})
