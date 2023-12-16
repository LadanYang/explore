library(vdiffr)

test_that("plotting the graphs given 1 numeric and 2 categorical variables  works", {
  expect_doppelganger(
    title = "1 numeric 2 cat var",
    num1_cat2(iris, cat, Sepal.Length, Species)
  )
})
