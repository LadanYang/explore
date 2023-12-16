library(vdiffr)

test_that("plotting the graphs given 1 numeric variable works", {
  expect_doppelganger(
    title = "1 numeric var",
    fig = num_1(iris, Sepal.Length)
  )
})

