
cell_data<-single_cell("https://github.com/LadanYang/explore/raw/main/data-raw/xydata.mat")
test_that("single_cell object", {
  expect_equal(class(cell_data),"single_cell")
}
)


test_that("missing columns", {
  expect_error(single_cell("https://github.com/LadanYang/explore/raw/main/data-raw/spikes.mat"))
})

