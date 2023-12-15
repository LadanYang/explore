test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

cell_data<-single_cell("https://github.com/LadanYang/explore/raw/main/data-raw/xydata.mat")
example_matrix<-matrix(cell_data)
#one_column<-

test_that("input not df or single_cell",{
  expect_error(explore.single_cell(example_matrix))
})

# test_that("missing columns", {
#   expect_error()
# })

# test_that("wrong column names",{
#   expect_equal(cell)
# })
