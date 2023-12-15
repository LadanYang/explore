library(vdiffr)

test_that("plotting the graphs given 1 numeric and 1 categorical variable works", {
  expect_doppelganger(
    title = "xkcd 1",
    fig = plot(xkcd(1))
  )
})
