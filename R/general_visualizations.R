library(ggplot2)
# Two numerical variables

# scatterplot
scatterplot <- function(data, x, y,...) {
  ggplot(data = data,
         mapping = aes(x = {{x}}, y = {{y}})) +
    geom_point(...)
}

# test: scatterplot(iris, Sepal.Length, Sepal.Width, color = "red", size = 3)

# If user wants to further explore options for scatterplots
# Option 1: jittered scatterplot
jitterplot <- function(data, x, y,...) {
  ggplot(data = data,
         mapping = aes(x = {{x}}, y = {{y}})) +
    geom_jitter(...)
}

# test: jitterplot(iris, Sepal.Length, Sepal.Width, color = "red", size = 3)

# linegraphs
linegraph <- function(data, x, y,...) {
  ggplot(data = data,
         mapping = aes(x = {{x}}, y = {{y}})) +
    geom_line(...)
}

# One numerical variable
histogram <- function(data, x,...) {
  ggplot(data = data,
         mapping = aes(x = {{x}})) +
    geom_histogram(...)
}

boxplot <- function(data, x,...){
  ggplot(data = data,
         mapping = aes(x = {{x}})) +
    geom_boxplot(...)
}

# One categorical variable

barplot <- function(data, x,...) {
  ggplot(data = data,
         mapping = aes(x = {{x}})) +
    geom_bar(...)
}

# Multivariate graphs
# At least one categorical variable
# Heatmap
# Facetted histograms
facet_hist <- function(data, x, categorize_by, ...) {
  ggplot(data = data,
         mapping = aes(x = {{x}})) +
    geom_histogram(...) +
    facet_wrap(vars({{categorize_by}}))
}


# facet_hist(data = iris, x = Sepal.Length, categorize_by = Species)

par_box <- function(data, y, categorize_by, ...) {
  ggplot(data = data,
         mapping = aes(x = as.character({{categorize_by}}), y = {{y}})) +
           geom_boxplot(...)
}

# par_box(data = iris, x = Sepal.Length, categorize_by = Petal.Width)

stack_bar <- function(data, x, categorize_by, ...) {
  ggplot(data = {{data}},
        mapping = aes(x = {{x}}, fill = {{categorize_by}})) +
           geom_bar(...)
}

# stack_bar(data = iris, x = Sepal.Length, categorize_by = Species)

par_bar <- function(data, x, categorize_by, ...) {
  ggplot(data = {{data}},
         mapping = aes(x = {{x}}, fill = {{categorize_by}})) +
    geom_bar(position = position_dodge(preserve = "single", ...))
}

# par_bar(data = iris, x = Sepal.Length, categorize_by = Species)

color_scatter <- function(data, x, y, categorize_by,...) {
  ggplot(data = {{data}},
         mapping = aes(x = {{x}}, y = {{y}}, color = {{categorize_by}})) +
    geom_point(...)
}

# color_scatter(data = iris, x = Sepal.Length, y = Sepal.Width, categorize_by = Species)

color_line <- function(data, x, y, categorize_by, ...) {
  ggplot(data = {{data}},
         mapping = aes(x = {{x}}, y = {{y}}, color = {{categorize_by}})) +
    geom_line(...)
}

color_line(data = iris, x = Sepal.Length, y = Sepal.Width, categorize_by = Species)

# One categorical variable
explore_pair1 <- function(data, x, categorize_by) {

  top_left <- stack_bar({{data}}, {{x}}, {{categorize_by}})

  bottom_left <- par_bar({{data}}, {{x}}, {{categorize_by}})

  top_right <- par_box({{data}}, {{x}}, {{categorize_by}})


  gridExtra::grid.arrange(
    top_left,
    top_right,
    bottom_left
  )
}

# Test
explore_pair1(data = iris, x = Sepal.Length, categorize_by = Species)

# Multivariate
library(gridExtra)
explore_pair2 <- function(data, x, y, categorize_by,...) {

  top_left <- color_scatter({{data}}, {{x}}, {{y}}, {{categorize_by}})

  bottom_left <- color_line({{data}}, {{x}}, {{y}}, {{categorize_by}})

  gridExtra::grid.arrange(
    top_left,
    bottom_left
  )
}

explore_pair2(data = iris, x = Sepal.Length, y = Sepal.Width, categorize_by = Species)
