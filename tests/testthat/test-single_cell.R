test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

cell_data<-single_cell("https://github.com/LadanYang/explore/raw/main/data-raw/xydata.mat")
test_that("single_cell object",){
  expect_equal(class(cell_data),"single_cell")
}
