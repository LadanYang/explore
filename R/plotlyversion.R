#' Creates an interactive scatterplot.
#' @import viridis
#' @import ggplot2
#' @import dplyr
#' @importFrom plotly plot_ly
#' @importFrom plotly ggplotly
#' @importFrom plotly subplot
#' @importFrom plotly layout
#' @importFrom plotly add_histogram
#' @param data A data frame
#' @param x A numeric variable from the data frame that is plotted on the x-axis
#' @param y A numeric variable from the data frame that is plotted on the y-axis
#'
#' @return An interactive scatterplot

scatter <- function(data, x, y) {
  x_val <- rlang::enquo(x)
  y_val <- rlang::enquo(y)
  y_name <- deparse(substitute(y))
  plotly::plot_ly(data, x = x_val, y = y_val,
                  type = "scatter", mode = "markers", marker = list(color = "skyblue")
                  )

}

# scatter(iris, Sepal.Length, Sepal.Width)

#' Creates an interactive scatterplot with different color points for each level of the categorical variable.
#'
#' @param data A data frame
#' @param x A numeric variable from the data frame that is plotted on the x-axis
#' @param y A numeric variable from the data frame that is plotted on the y-axis
#' @param categorize_by A categorical variable from the data frame
#'
#' @return An colored interactive scatterplot

color_scatter <- function(data, x, y, categorize_by) {
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
# color_scatter(iris, Sepal.Length, Sepal.Width, Species)

#' Creates an interactive linegraph.
#'
#' @param data A data frame
#' @param x A numeric variable from the data frame that is plotted on the x-axis
#' @param y A numeric variable from the data frame that is plotted on the y-axis
#'
#' @return An interactive linegraph

line <- function(data, x, y) {
  x_val <- rlang::enquo(x)
  y_val <- rlang::enquo(y)
  plotly::plot_ly(data, x = x_val, y = y_val,
                  type = "scatter", mode = "lines + markers",
                  line = list(color = "skyblue")
  )
}

# line(iris, Sepal.Length, Sepal.Width)

#' Creates an interactive linegraph with different color lines for each level of the categorical variable.
#'
#' @param data A data frame
#' @param x A numeric variable from the data frame that is plotted on the x-axis
#' @param y A numeric variable from the data frame that is plotted on the y-axis
#' @param categorize_by A categorical variable from the data frame
#'
#' @return An colored interactive linegraph

color_line <- function(data, x, y, categorize_by) {
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
#color_line(iris, Sepal.Length, Sepal.Width, Species)


#' Creates an interactive barchart.
#'
#' @param data A data frame
#' @param x A categorical variable from the data frame
#'
#' @return An interactive barchart

bar2 <- function(data, x,...) {
  plot <- ggplot2::ggplot(data = data,
                          mapping = ggplot2::aes(x = {{x}}, fill = {{x}})) +
    ggplot2::geom_bar(...) +
    viridis::scale_fill_viridis(discrete = TRUE)
  plotly::ggplotly(plot)
}

# bar2(iris, Species)

#' Creates an interactive barchart.
#'
#' @param data A data frame
#' @param x A categorical variable from the data frame
#'
#' @return An interactive barchart

bar <- function(data, x) {
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

# bar(iris, Species)

#' Creates an interactive pie chart.
#'
#' @param data A data frame
#' @param x A categorical variable from the data frame
#'
#' @return An interactive pie chart

pie <- function(data, x, ...){
  count <- dplyr::count(x = data, {{x}})
  choices <- length(unique(count[[as.character(dplyr::enquo(x))[-1] ]]))
  plotly::plot_ly(labels = count[[as.character(dplyr::enquo(x))[-1] ]],
                  values= count[["n"]],
                  type = "pie",
                  marker = list(colors = viridis::viridis_pal(option = "D")(choices)),
                  domain = list(x = c(0.5, 0.5), # centered on x axis
                                y = c(0.0, 0.45)),
                  showlegend = FALSE)
}

# pie(iris, Species)

#' Creates an interactive barchart with error bars.
#'
#' @param data A data frame
#' @param x A numeric variable from the data frame
#' @param categorize_by A categorical variable from the data frame
#'
#' @return An interactive barchart with error bars

error_bar <- function(data, x, categorize_by, legend = "right", ...) {
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
    ggplot2::geom_bar(stat = "identity", ...) +
    ggplot2::geom_errorbar(...) +
    viridis::scale_fill_viridis(discrete = TRUE, option = "D") +
    ggplot2::theme(legend.position = {{legend}})
  suppressWarnings(plotly::ggplotly(plot, tooltip = c("text"),
                                    legendgroup= cat))
}

# error_bar(iris, Sepal.Length, Species)

#' Creates an interactive stacked barchart.
#'
#' @param data A data frame
#' @param x A numeric variable from the data frame
#' @param categorize_by A categorical variable from the data frame
#'
#' @return An interactive stacked barchart

stack_bar <- function(data, x, categorize_by, legend = "right",...) {
   plot <- ggplot2::ggplot(data = {{data}},
                           mapping = ggplot2::aes(x = {{x}}, fill = {{categorize_by}})) +
     ggplot2::geom_bar(...) +
     viridis::scale_fill_viridis(discrete = TRUE, option = "D") +
     ggplot2::theme(legend.position = {{legend}})
   plotly::ggplotly(plot,
                    legendgroup= cat)
}

# stack_bar(data = iris, x = Sepal.Length, categorize_by = Species)

#' Creates an interactive side-by-side barchart.
#'
#' @param data A data frame
#' @param x A numeric variable from the data frame
#' @param categorize_by A categorical variable from the data frame
#'
#' @return An interactive side-by-side barchart

par_bar <- function(data, x, categorize_by, legend = "right", ...) {
  plot <- ggplot2::ggplot(data = {{data}},
                          mapping = ggplot2::aes(x = {{x}}, fill = {{categorize_by}})) +
    ggplot2::geom_bar(position = ggplot2::position_dodge(preserve = "single", ...)) +
    viridis::scale_fill_viridis(discrete = TRUE) +
    ggplot2::theme(legend.position = {{legend}})
  plotly::ggplotly(plot,
                   legendgroup= cat)
}

# par_bar(data = iris, x = Sepal.Length, categorize_by = Species, legend = "none")

#' Creates an interactive barchart faceted by a categorical variable.
#'
#' @param data A data frame
#' @param x A numeric variable from the data frame that is plotted on the x-axis
#' @param y A numeric variable from the data frame that is plotted on the y-axis
#' @param categorize_by A categorical variable from the data frame
#'
#' @return An interactive barchart faceted by a categorical variable

facet_bar <- function(data, x, y, categorize_by) {
  x_val <- rlang::enquo(x)
  y_val <- rlang::enquo(y)
  cat <- rlang::enquo(categorize_by)
  choices <- dplyr::distinct(data, {{categorize_by}}) |>
    nrow()
  data |>
    dplyr::group_by({{categorize_by}}) |>
    dplyr::group_map(~ plotly::plot_ly(data=., x = x_val, y = y_val, color = cat,
                                       colors = viridis::viridis_pal(option = "D")(choices),
                                       type = "bar"), .keep=TRUE,
                     legendgroup= cat) |>
  plotly::subplot(nrows = 1, shareX = TRUE, shareY=TRUE)
}

# facet_bar(iris, Sepal.Length, Sepal.Width, Species)

#' Creates an interactive boxplot.
#'
#' @param data A data frame
#' @param x A numeric variable from the data frame
#'
#' @return An interactive boxplot

box <- function(data, x) {
  x_val <- rlang::enquo(x)
  plotly::plot_ly(data, x = x_val,
                  hoverinfo = 'x',
                  name = " ",
                  type = "box",
                  fillcolor = "skyblue")
}

# box(iris, Sepal.Length)

#' Creates interactive boxplots placed side-by-side with different colors representing different levels
#' of the categorical variable.
#'
#' @param data A data frame
#' @param x A numeric variable from the data frame
#' @param categorize_by A categorical variable from the data frame
#'
#' @return An interactive side-by-side colored boxplot

color_box <- function(data, x, categorize_by, legend = T) {
  x_val <- rlang::enquo(x)
  cat <- rlang::enquo(categorize_by)
  choices <- dplyr::distinct(data, {{categorize_by}}) |>
    nrow()
  plotly::plot_ly(data, y = x_val, color = cat,
                  hoverinfo = "y",
                  type = "box",
                  colors = viridis::viridis_pal(option = "D")(choices),
                  legendgroup= cat,
                  showlegend = {{legend}})
}

# color_box(iris, Sepal.Length, Species, legend = F)

#' Creates interactive boxplots placed side-by-side with different colors representing different levels
#' of one categorical variable, split by the levels of another categorical variable on the x-axis.
#'
#' @param data A data frame
#' @param x_cat A categorical variable from the data frame that is plotted on the x-axis
#' @param y A numeric variable from the data frame that is plotted on the y-axis
#' @param group A categorical variable from the data frame that is represented through color
#'
#' @return An interactive grouped boxplot

group_box <- function(data, x_cat, y, group) {
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

# iris <- iris
# random <- c("a", "b", "c")
# iris$cat <- sample(random, size = nrow(data), replace = TRUE)

# group_box(data, cat, Sepal.Length, Species)


#' Creates an interactive histogram.
#'
#' @param data A data frame
#' @param x A categorical variable from the data frame
#'
#' @return An interactive histogram

hist <- function(data, x) {
  x_val <- rlang::enquo(x)
  plotly::plot_ly(data, x = x_val,
                  type = "histogram",
                  marker = list(color = "skyblue", line = list(color = "white", width = 1.5))
                  )
}

# hist(iris, Sepal.Length)

#' Creates an overlapping histogram.
#'
#' @param data A data frame
#' @param x A numeric variable from the data frame
#' @param categorize_by A categorical variable from the data frame
#'
#' @return An interactive overlapping histogram

overlap_hist <- function(data, x, categorize_by) {
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
    fig |> plotly::add_histogram(~x,
                                 type = "histogram",
                                 name = name,
                                 marker = list(line = list(color = "white", width = 1.5)))
  }

  # iterating over function to produce a histogram for each level of categorical variable
  final_fig <- purrr::reduce2(wide, colnames(wide), add_hist, .init = blank_fig) |>
    plotly::layout(colorway = viridis::viridis_pal(option = "D")(choices),
                   barmode = "overlay",
                   xaxis = list(title = x_name)
                   )
  final_fig
}
# overlap_hist(iris, Sepal.Length, Species)






