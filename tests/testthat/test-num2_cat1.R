library(vdiffr)

test_that("plotting the graphs given 2 numeric and 1 categorical variable  works", {
  expect_doppelganger(
    title = "2 numeric 1 cat var",
    num2_cat1(iris, Sepal.Length, Sepal.Width, Species)
  )
})
