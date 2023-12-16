library(vdiffr)

test_that("plotting the graphs given 1 categorical variable works", {
  expect_doppelganger(
    title = "1 categorical var",
    cat_1(iris, Species)
  )
})


