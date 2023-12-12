

# Two numerical variables

# scatterplots
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

#boxplot(iris, Sepal.Width)

# One categorical variable

barplot <- function(data, x) {
  ggplot2::ggplot(data = data,
         mapping = aes(x = {{x}}, fill = {{x}})) +
    geom_bar()
}

# barplot(iris, Species)

pie_chart_interactive <- function(data, x){
  count <- dplyr::count(x = data, {{x}})
  pie <- rAmCharts4::amPieChart(
    data = count,
    category = names(count)[1],
    value =  names(count)[2],
    threeD = TRUE
  )
  suppressWarnings(print(pie))
}

# pie_chart_interactive(iris, Species)

pie_chart <- function(data, x, ...){
  count <- dplyr::count(x = data, {{x}})
  choices <- length(unique(count[[as.character(enquo(x))[-1] ]]))
  plotly::plot_ly(labels = count[[as.character(enquo(x))[-1] ]],
                  values= count[["n"]],
                  type = "pie",
                  marker = list(colors = viridis::viridis_pal(option = "D")(choices))
                  )
}

#pie_chart(iris, Species)


barchart <- function(data, x,...) {
  count <- dplyr::count(x = data, {{x}})
  # number of categories
  choices <- length(unique(count[[as.character(enquo(x))[-1] ]]))
  plotly::plot_ly(x = count[[as.character(enquo(x))[-1] ]],
                  y = count[["n"]],
                  showlegend = TRUE,
                  type = "bar",
                  color = count[[as.character(enquo(x))[-1] ]],
                  colors = viridis::viridis_pal(option = "D")(choices))
}

#barchart(iris, Species)

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
         mapping = aes(x = as.character({{categorize_by}}), y = {{y}}, fill = as.character({{categorize_by}}))) +
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

#color_line(data = iris, x = Sepal.Length, y = Sepal.Width, categorize_by = Species)

# One quantitative variable, grouped by one categorical variable
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

#explore_pair1(data = iris, x = Sepal.Length, categorize_by = Species)




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

#explore_pair2(data = iris, x = Sepal.Length, y = Sepal.Width, categorize_by = Species)

# One categorical variable
explore_pair3 <- function(data, x) {

  top_left <- barplot({{data}}, {{x}})

  bottom_left <- pie_chart({{data}}, {{x}})

  gridExtra::grid.arrange(arrangeGrob(
    top_left,
    bottom_left
  ))
}

#explore_pair3(data = iris, x = Species)

one_cat <- function(data, x) {
  barchart <- barchart({{data}}, {{x}},
                       domain = list(x = c(0, 0.5), y = c(0, 0.5)))

  piechart <-  pie_chart({{data}}, {{x}},
                         domain = list(x = c(0, 0.5), y = c(10.6, 11.1)))

  plotly::subplot(barchart, style(piechart, showlegend = F), nrows = 2)
}

#one_cat(iris, Species)

