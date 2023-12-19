test_that("Test if geometry is of class 'POINT'", {
  # Create or load an 'sf' object for testing
  your_sf_object <- ...  # Replace with your data or function call

  # Call your function
  result <- explore_points(your_sf_object)  # Replace with the actual function name

  # Check the result using expect_true or expect_false
  expect_true(result, "The geometry is of class 'POINT'")
  # OR
  # expect_false(result, "The geometry is NOT of class 'POINT'")
})

