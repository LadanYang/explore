library(vdiffr)

x <- cat_1(iris, Species)

test_that("plotting the graphs given 1 categorical variable works",
  suppressWarnings({
  expect_doppelganger(
    title = "1 categorical var",
    x
  )
  })
)


