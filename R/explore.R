#' @export
explore <- function(data,type,x, ...) {
  UseMethod("explore")
}
#' @import ggplot2
#' @exportS3Method
explore.data.frame <- function(data,type,x,y, ...){
  #library(ggplot2)
  if (type == "univariate"){
    par(mfrow=c(2,2))
    hist(data[[x]], main="Histogram")
    barplot(data[[x]], main="Bar plot")
    boxplot(data[[x]], main="Box plot")
    plot(data[[x]], main="Scatterplot")
  }
  if (type== "bivariate"){
    library(ggplot2)
    library(patchwork)
    scatter<- ggplot(
      data,aes(x=data[[x]],y=data[[y]]))+geom_point()+xlab(x)+ylab(y)
    line<-ggplot(data,mapping=aes(x=data[[x]],y=data[[y]]))+geom_line()+xlab(x)+ylab(y)
    box<-ggplot(data,mapping=aes(x=data[[x]],y=data[[y]]))+geom_boxplot()+xlab(x)+ylab(y)

    print(scatter + line + box +
       plot_layout(ncol = 2))

  }
  if (type== "multivariate"){

  }

}
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
  #library(ggplot2)
  #' @import ggplot2
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
    #annotate("rect", xmin = 120, xmax = 150, ymin = 0, ymax = 10,
    #alpha = .1,fill = "red")+
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
  library(gridExtra)
  print(grid.arrange(gg_raster, gg_psth, ncol = 1, heights = c(2, 1)))

}
