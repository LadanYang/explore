#' @export
explore <- function(data,type,x, ...) {
  UseMethod("explore")
}

#' create peri-event histogram and raster plot for single_cell data
#' @importFrom gridExtra grid.arrange
#' @import ggplot2
#' @param single_cell a [`single_cell`] object that has first column the spiking data and second column labeling the laps, trials or neurons
#' @param xaxis the x axis label of the graph, default value as "Time"
#' @param yaxis the y axis label of the raster plot, default value as "Trials"
#' @param stim The name of the highlighted region, default value as "Stimulus"
#' @param shade_on The starting x axis value for highlight/shading, default 0
#' @param shade_off The ending x axis value for highlight/shading, default 0
#' @param shade_color The coloring of the shading, default value as "pink"
#' @return a plot with raster plot of neurons firing patterns and histogram of counts of the firing
#' @exportS3Method
explore.single_cell <- function(single_cell,
                                xaxis="Time",
                                yaxis="Trials",
                                stim="Stimulus",
                                shade_on=0,
                                shade_off=0,
                                shade_color="pink"
                                ) {
  # Plot

  # Raster Plot with adjusted line length using geom_linerange

  gg_raster <- ggplot(single_cell) +
    geom_rect(aes(fill = stim),
              xmin = shade_on, xmax = shade_off,
              ymin = 0,
              ymax = Inf,
              alpha = 0.5
    ) +
    geom_linerange(aes(x = V1, y = V2, ymin = V2 - 0.3, ymax =  V2 + 0.3),
                   color = "black"
                   ) +
    scale_fill_manual(values = shade_color, guide = "none") +
    xlim(c(min(single_cell$V1), max(single_cell$V1))) +
    ylab("Laps") +
    xlab(xaxis)+
    ggtitle("Neuronal Spike Times")

  gg_psth <- ggplot(single_cell) +
    geom_rect(aes(fill = stim),
              xmin = shade_on, xmax = shade_off,
              ymax = Inf,
              ymin = 0,
              alpha = 0.5
    ) +
    geom_histogram(aes(x = V1, y = ..count..), binwidth = 1, fill = "black", color = "black") +
    scale_fill_manual(values = shade_color) +
    xlim(c(min(single_cell$V1), max(single_cell$V1))) +
    ylab("Count") +
    xlab(xaxis)+
    ggtitle("Peri-Stimulus Time Histogram (PSTH)") +
    theme(legend.title = element_blank(),
          legend.position = c(.8, .8)
    )

  # Combine both plots
  print(grid.arrange(gg_raster, gg_psth, ncol = 1, heights = c(2, 1)))

}
