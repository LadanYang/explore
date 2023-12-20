#' Generates visualizations for the distribution of a numeric variable.
#' Given a dataframe and the name of a numeric variable in that dataframe, this
#' function generates interactive plots showing the distribution of that variable.
#'
#' @param data A data frame
#' @param x A numeric variable from the data frame
#'
#' @return A plot containing several subplots
#' * A boxplot with quartiles computed through the linear method
#' * A histogram
#' Note: both plots contain hover information
#'
#' @import ggplot2
#' @import dplyr
#' @import viridis
#' @importFrom tidyr spread
#' @importFrom plotly plot_ly
#' @importFrom plotly ggplotly
#' @importFrom plotly subplot
#' @importFrom plotly layout
#' @importFrom plotly add_histogram

#' @export
num_1 <- function(data, x){
  boxplot <- box(data, {{x}})
  histogram <- hist(data, {{x}})

  plotly::subplot(boxplot, histogram, nrows = 2, shareX = TRUE)
}

# num_1(iris, Sepal.Length)

#' Generates visualizations for the distribution of two numeric variables.
#' Given a dataframe and the names of two numeric variable in that dataframe, this
#' function generates interactive plots showing the distribution of one variable against the other.
#'
#' @param data A data frame
#' @param x A numeric variable from the data frame that is plotted on the x-axis
#' @param y A numeric variable from the data frame that is plotted on the y-axis
#'
#' @return A plot containing several subplots
#' * A scatterplot
#' * A linegraph
#' Note: both plots contain hover information

#' @export
num_2 <- function(data, x, y){
  scatterplot <- scatter(data, {{x}}, {{y}})
  linegraph <- line(data, {{x}}, {{y}})

  plotly::subplot(scatterplot, linegraph, nrows = 2, shareX = TRUE, shareY = TRUE)
}

# num_2(iris, Sepal.Length, Sepal.Width)

#' Generates visualizations for the distribution of one categorical variable.
#' Given a dataframe and the name of one categorical variable in that dataframe, this
#' function generates interactive plots showing the distribution of that variable.
#'
#' @param data A data frame
#' @param x A categorical variable from the data frame
#'
#' @return A plot containing several subplots
#' * A bar chart
#' * A pie chart
#' Note: both plots contain hover information

#' @export
cat_1 <- function(data, x) {
  barchart <- bar(data, {{x}})
  piechart <- pie(data, {{x}})

  suppressWarnings(print(plotly::subplot(barchart, piechart, nrows = 2)))
}

#cat_1(iris, Species)

#' Generates visualizations for the distribution of one numeric variable split by
#' one categorical variable and vice versa.
#' Given a dataframe and the names of one numeric variable and one categorical variable in that dataframe, this
#' function generates interactive plots showing the distribution of one variable against the other.
#'
#' @param data A data frame
#' @param x A numeric variable from the data frame
#' @param categorize_by A categorical variable from the data frame
#' @param ... Additional arguments for ggplot2::geom_bar() and ggplot2::geom_errorbar
#'
#' @return A plot containing several subplots
#' * A barchart with error bars
#' * A stacked barchart
#' * A side-by-side barchart
#' * A colored boxplot
#' * An overlapping histogram
#' Note: all plots contain hover information

#' @export
num1_cat1 <- function(data, x, categorize_by, ...){
  errorbar <- error_bar(data, {{x}}, {{categorize_by}}, legend = "none")
  stackbar <- stack_bar(data, {{x}}, {{categorize_by}}, legend = "none")
  parbar <- par_bar(data, {{x}}, {{categorize_by}}, legend = "none")
  colorbox <- color_box(data, {{x}}, {{categorize_by}}, legend = F)
  overlaphist <- overlap_hist(data, {{x}}, {{categorize_by}})

  numberedx <- suppressWarnings(print(plotly::subplot(parbar, stackbar, overlaphist,
                               shareX = TRUE, nrows = 3)))

  catx <- suppressWarnings(print(plotly::subplot(errorbar, colorbox, nrows = 2)))

  suppressWarnings(print(plotly::subplot(numberedx, catx)))
}

# num1_cat1(iris, Sepal.Length, Species)

#' Generates visualizations for the distribution of two numeric variables split by
#' one categorical variable.
#' Given a dataframe and the names of two numeric variables and one categorical variable, this
#' function generates interactive plots showing the distribution of these variables against each other.
#'
#' @param data A data frame
#' @param x A numeric variable from the data frame that is plotted on the x-axis
#' @param y A numeric variable from the data frame that is plotted on the y-axis
#' @param categorize_by A categorical variable from the data frame
#'
#' @return A plot containing several subplots
#' * A scatterplot with colored points corresponding to levels of the categorical variable
#' * A colored linegraph
#' * A faceted histogram

#' Note: all plots contain hover information

#' @export
num2_cat1 <- function(data, x, y, categorize_by) {
  colorscatter <- color_scatter(data, {{x}}, {{y}}, {{categorize_by}})
  colorline <- color_line(data, {{x}}, {{y}}, {{categorize_by}})
  facetbar <- facet_bar(data, {{x}}, {{y}}, {{categorize_by}})

  plotly::subplot(colorscatter, colorline, facetbar, nrows = 3)
}

# num2_cat1(iris, Sepal.Length, Sepal.Width, Species)

#' Generates a visualization for the distribution of two categorical variables based on
#' one numeric variable.
#' Given a dataframe and the names of two categorical variables and one numeric variable, this
#' function generates an interactive plot showing the distribution of these variables against each other.
#'
#' @param data A data frame
#' @param x_cat A categorical variable from the data frame that is plotted on the x-axis
#' @param y A numeric variable from the data frame that is plotted on the y-axis
#' @param group A categorical variable from the data frame that is represented through color
#'
#'
#' @return An interactive colored boxplot
#' Note: this plot contains hover information

#' @export
num1_cat2 <- function(data, x_cat, y, group) {
  groupbox <- group_box(data, {{x_cat}}, {{y}}, {{group}})

  suppressWarnings(print(plotly::subplot(groupbox, nrows = 1)))
}

# num1_cat2(iris, cat, Sepal.Length, Species)





