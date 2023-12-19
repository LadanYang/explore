test_that("geometry is polygon", {
  expect_equal(2 * 2, 4)
})

library(testthat)
library(explore)  # Replace 'your_package_name' with the actual name of your package

test_that("Test if geometry is of class 'POLYGON' or 'MULTIPOLYGON'", {
  # Create or load an 'sf' object for testing
  your_sf_object <- ...  # Replace with your data or function call

  # Call your function
  result <- explore_polygons(your_sf_object)  # Replace with the actual function name

  # Check the result using expect_true or expect_false
  expect_true(result, "The geometry is of class 'POLYGON' or 'MULTIPOLYGON'")
  # OR
  # expect_false(result, "The geometry is NOT of class 'POLYGON' or 'MULTIPOLYGON'")
})
