library(vdiffr)

x <- num1_cat1(iris, Sepal.Length, Species)

test_that("plotting the graphs given 1 numeric variable and 1 categorical variable works",
          suppressWarnings({
            expect_doppelganger(
              title = "1 numeric 1 cat var",
              x
              )
            })
          )
