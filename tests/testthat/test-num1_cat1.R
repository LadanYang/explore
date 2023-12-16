library(vdiffr)

test_that("plotting the graphs given 1 numeric variable and 1 categorical variable works", {
  expect_doppelganger(
    title = "1 numeric 1 cat var",
    cat_1(iris, Species)
  )
})
