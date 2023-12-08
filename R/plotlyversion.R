library(plotly) # plotting graphs side by side
library(viridis)
library(ggplot2)
library(dplyr)
library(tidyr)


# scatterplots
# (2 numeric variables)
scatter <- function(data, x, y,...) {
  x_val <- rlang::enquo(x)
  y_val <- rlang::enquo(y)
  plotly::plot_ly(data, x = x_val, y = y_val,
                  type = "scatter", mode = "markers")
}

scatter(iris, Sepal.Length, Sepal.Width)

# (2 numeric, split by 1 cat)

color_scatter <- function(data, x, y, categorize_by,...) {
  x_val <- rlang::enquo(x)
  y_val <- rlang::enquo(y)
  cat <- rlang::enquo(categorize_by)
  choices <- dplyr::distinct(data, {{categorize_by}}) |>
    nrow()
  plotly::plot_ly(data, x = x_val, y = y_val,
                  type = "scatter", mode = "markers",
                  color = cat,
                  colors = viridis::viridis_pal(option = "D")(choices))
}
color_scatter(iris, Sepal.Length, Sepal.Width, Species)

# linegraphs
# (2 numeric variables)
line <- function(data, x, y,...) {
  x_val <- rlang::enquo(x)
  y_val <- rlang::enquo(y)
  plotly::plot_ly(data, x = x_val, y = y_val,
                  type = "scatter", mode = "lines")
}

line(iris, Sepal.Length, Sepal.Width)

# (2 numeric, split by 1 cat)

color_line <- function(data, x, y, categorize_by,...) {
  x_val <- rlang::enquo(x)
  y_val <- rlang::enquo(y)
  cat <- rlang::enquo(categorize_by)
  choices <- dplyr::distinct(data, {{categorize_by}}) |>
    nrow()
  plotly::plot_ly(data, x = x_val, y = y_val,
                  type = "scatter", mode = "lines",
                  color = cat,
                  colors = viridis::viridis_pal(option = "D")(choices))
}
color_line(iris, Sepal.Length, Sepal.Width, Species)

# barcharts
# (1 categorical variable)

barchart <- function(data, x,...) {
  plot <- ggplot2::ggplot(data = data,
                          mapping = ggplot2::aes(x = {{x}}, fill = {{x}})) +
    ggplot2::geom_bar(...) +
    viridis::scale_fill_viridis(discrete = TRUE)
  plotly::ggplotly(plot)
}

barchart(iris, Species)

# also works
barchart2 <- function(data, x,...) {
  count <- dplyr::count(x = data, {{x}})
  # number of categories
  choices <- length(unique(count[[as.character(rlang::enquo(x))[-1] ]]))
  plotly::plot_ly(x = count[[as.character(rlang::enquo(x))[-1] ]],
                  y = count[["n"]],
                  showlegend = TRUE,
                  type = "bar",
                  color = count[[as.character(rlang::enquo(x))[-1] ]],
                  colors = viridis::viridis_pal(option = "D")(choices))
}

barchart2(iris, Species)

# (1 numeric and 1 categorical)
# a
error_bar <- function(data, x, categorize_by, ...) {
  x_val <- rlang::enquo(x)
  cat <- rlang::enquo(categorize_by)
  summary <- data |>
    dplyr::group_by({{categorize_by}}) |>
    dplyr::summarize("Mean {{x}}" := mean({{x}}), Min = min({{x}}), Max = max({{x}}))
  plot <- ggplot2::ggplot(data = summary,
                          mapping = ggplot2::aes(x = {{categorize_by}},
                                        y =  summary[[2]], ymin = Min, ymax = Max,
                                        fill = {{categorize_by}},
                                        text= paste("Species: ", Species, "<br>",
                                                    "Mean: ", summary[[2]], "<br>",
                                                    "Min: ", Min, "<br>",
                                                    "Max: ", Max, sep = "")
                                        )) +
    ggplot2::labs(y = names(summary)[[2]]) +
    ggplot2::geom_bar(stat = "identity",...) +
    ggplot2::geom_errorbar() +
    viridis::scale_fill_viridis(discrete = TRUE)
  suppressWarnings(plotly::ggplotly(plot, tooltip = c("text")
  ))
}

error_bar(iris, Sepal.Length, Species)

# b
stack_bar <- function(data, x, categorize_by, ...) {
   plot <- ggplot2::ggplot(data = {{data}},
                           mapping = ggplot2::aes(x = {{x}}, fill = {{categorize_by}})) +
     ggplot2::geom_bar(...) +
     viridis::scale_fill_viridis(discrete = TRUE)
   plotly::ggplotly(plot)
}

stack_bar(data = iris, x = Sepal.Length, categorize_by = Species)

# c
par_bar <- function(data, x, categorize_by, ...) {
  plot <- ggplot2::ggplot(data = {{data}},
                          mapping = ggplot2::aes(x = {{x}}, fill = {{categorize_by}})) +
    ggplot2::geom_bar(position = ggplot2::position_dodge(preserve = "single", ...)) +
    viridis::scale_fill_viridis(discrete = TRUE)
  plotly::ggplotly(plot)
}

par_bar(data = iris, x = Sepal.Length, categorize_by = Species)

# (2 numeric, split by 1 cat)

facet_bar <- function(data, x, y, categorize_by, ...) {
  x_val <- rlang::enquo(x)
  y_val <- rlang::enquo(y)
  cat <- rlang::enquo(categorize_by)
  choices <- dplyr::distinct(data, {{categorize_by}}) |>
    nrow()
  data |>
    dplyr::group_by({{categorize_by}}) |>
    dplyr::group_map(~ plotly::plot_ly(data=., x = x_val, y = y_val, color = cat,
                                       colors = viridis::viridis_pal(option = "D")(choices),
                                       type = "bar"), .keep=TRUE) |>
  plotly::subplot(nrows = 1, shareX = TRUE, shareY=TRUE)
}

facet_bar(iris, Sepal.Length, Sepal.Width, Species)

# boxplots
# (1 numeric variable)
box <- function(data, x,...) {
  x_val <- rlang::enquo(x)
  plotly::plot_ly(data, x = x_val,
                  hoverinfo = 'x',
                  name = " ",
                  type = "box",
                  fillcolor = "skyblue")
}

box(iris, Sepal.Length)

# (1 numeric and 1 categorical)

color_box <- box <- function(data, x, categorize_by,...) {
  x_val <- rlang::enquo(x)
  cat <- rlang::enquo(categorize_by)
  choices <- dplyr::distinct(data, {{categorize_by}}) |>
    nrow()
  plotly::plot_ly(data, y = x_val, color = cat,
                  hoverinfo = "y",
                  type = "box",
                  colors = viridis::viridis_pal(option = "D")(choices))
}

color_box(iris, Sepal.Length, Species)

# (2 categorical, 1 numeric)

group_box <- function(data, x_cat, y, group,...) {
  x_val <- rlang::enquo(x_cat)
  y_val <- rlang::enquo(y)
  cat <- rlang::enquo(group)
  choices <- dplyr::distinct(data, {{group}}) |>
    nrow()

  p <- plotly::plot_ly(data, x = x_val, y = y_val, color = cat,
                       type = "box",
                       colors = viridis::viridis_pal(option = "D")(choices)) |>
    plotly::layout(boxmode = "group")
  suppressWarnings(print(p))

}

data <- iris
data$new <- sample(3, size = nrow(data), replace = TRUE)
data$new <- as.factor(data$new)

group_box(data, new, Sepal.Length, Species)

# histograms
# (1 numeric)

hist <- function(data, x,...) {
  x_val <- rlang::enquo(x)
  plotly::plot_ly(data, x = x_val,
                  type = "histogram",
                  marker = list(color = "skyblue", line = list(color = "white", width = 1.5))
                  )
}

hist(iris, Sepal.Length)

# (1 numeric, split by 1 cat)
overlap_hist <- function(data, x, categorize_by,...) {
  x_val <- rlang::enquo(x)
  x_name <- deparse(substitute(x))
  choices <- dplyr::distinct(data, {{categorize_by}}) |>
    nrow()

# making each categorical group into an individual column
  wide <- data |>
    dplyr::select({{categorize_by}}, {{x}}) |>
    dplyr::group_by({{categorize_by}}) |> # starts new count for each category
    dplyr::mutate(ID = 1:dplyr::n()) |>
    dplyr::ungroup() |>
    tidyr::spread(key = {{categorize_by}}, value = {{x}}) |>
    dplyr::select(-ID)

  blank_fig <- plotly::plot_ly(alpha = 0.6, bingroup=1)

  add_hist <- function(fig, x, name) {
    name <- deparse(substitute(x))
    fig |> plotly::add_histogram(~x,
                                 type = "histogram",
                                 name = name,
                                 marker = list(line = list(color = "white", width = 1.5)))
  }
 # iterating over function to produce a histogram for each level of categorical variable
  final_fig <- purrr::reduce(wide, add_hist, .init = blank_fig) |>
    plotly::layout(colorway = viridis::viridis_pal(option = "D")(choices),
                   barmode = "overlay",
                   xaxis = list(title = x_name)
                   )
  final_fig
}
overlap_hist(iris, Sepal.Length, Species)

# combined functions
numeric_1 <- function(data, x){
  boxplot <- box({{data}}, {{x}})
  histogram <- hist({{data}}, {{x}})

  plotly::subplot(boxplot, histogram, nrows = 2, shareX = TRUE)
}

numeric_1(iris, Sepal.Length)









pie_chart <- function(data, x, ...){
  count <- dplyr::count(x = data, {{x}})
  choices <- length(unique(count[[as.character(dplyr::enquo(x))[-1] ]]))
  plotly::plot_ly(labels = count[[as.character(dplyr::enquo(x))[-1] ]],
                  values= count[["n"]],
                  type = "pie",
                  marker = list(colors = viridis::viridis_pal(option = "D")(choices))
  )
}
