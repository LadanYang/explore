# combined functions
num_1 <- function(data, x){
  boxplot <- box(data, {{x}})
  histogram <- hist(data, {{x}})

  plotly::subplot(boxplot, histogram, nrows = 2, shareX = TRUE)
}

num_1(iris, Sepal.Length)

numeric_2 <- function(data, x, y){
  scatterplot <- scatter(data, {{x}}, {{y}})
  linegraph <- line(data, {{x}}, {{y}})

  plotly::subplot(scatterplot, linegraph, nrows = 2, shareX = TRUE, shareY = TRUE)
}

numeric_2(iris, Sepal.Length, Sepal.Width)

cat_1 <- function(data, x,...) {
  barchart <- bar(data, {{x}})
  piechart <- pie(data, {{x}}, )

  suppressWarnings(plotly::subplot(barchart, piechart, nrows = 2))
}

cat_1(iris, Species)

num1_cat1 <- function(data, x, categorize_by, ...){
  errorbar <- error_bar(data, {{x}}, {{categorize_by}}, legend = "none")
  stackbar <- stack_bar(data, {{x}}, {{categorize_by}}, legend = "none")
  parbar <- par_bar(data, {{x}}, {{categorize_by}}, legend = "none")
  colorbox <- color_box(data, {{x}}, {{categorize_by}}, legend = F)
  overlaphist <- overlap_hist(data, {{x}}, {{categorize_by}})

  cat <- rlang::enquo(categorize_by)
  numberedx <- plotly::subplot(parbar, stackbar, overlaphist,
                               shareX = TRUE, nrows = 3)
  catx <- plotly::subplot(errorbar, colorbox, nrows = 2)

  plotly::subplot(numberedx, catx)
}

num1_cat1(iris, Sepal.Length, Species)

num2_cat1 <- function(data, x, y, categorize_by,...) {
  colorscatter <- color_scatter(data, {{x}}, {{y}}, {{categorize_by}})
  colorline <- color_line(data, {{x}}, {{y}}, {{categorize_by}})
  facetbar <- facet_bar(data, {{x}}, {{y}}, {{categorize_by}})

  plotly::subplot(colorscatter, colorline, facetbar, nrows = 3)
}

num2_cat1(iris, Sepal.Length, Sepal.Width, Species)

num1_cat2 <- function(data, x_cat, y, group,...) {
  groupbox <- group_box(data, {{x_cat}}, {{y}}, {{group}})

  suppressWarnings(plotly::subplot(groupbox, nrows = 1))
}

num1_cat2(data, new, Sepal.Length, Species)





