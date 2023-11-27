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
         mapping = aes(x = {{x}})) +
    geom_line(...)
}

# One numerical variable
histogram <- function(data, x,...) {
  ggplot(data = data,
         mapping = aes(x = {{x}})) +
    geom_histogram(...)
}

# Multivariate graphs
# At least one categorical variable
# Heatmap
# Facetted histograms
facet_hist <- function(data, x, wrap_by, ...) {
  ggplot(data = data,
         mapping = aes(x = {{x}})) +
    geom_histogram(...) +
    facet_wrap(~{{wrap_by}})
}

ggplot(data = iris, mapping = aes(x = Sepal.Length)) +
  geom_histogram() +
  facet_wrap(~ Species)

faccet_hist(data = iris, x = Sepal.Length, wrap_by = Species)



