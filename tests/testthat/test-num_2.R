library(vdiffr)

test_that("plotting the graphs given 2 numeric variables works", {
  expect_doppelganger(
    title = "2 numeric var",
    fig = num_2(iris, Sepal.Length, Sepal.Width)
  )
})
