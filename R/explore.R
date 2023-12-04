#' @export
explore <- function(data,type,x, ...) {
  UseMethod("explore")
}
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
explore.single_cell <- function(single_cell,xaxis="Time",yaxis="Trials") {
  par(mfrow=c(1,1))
  # if (xaxis=="Time"){
  #   x <- single_cell$Time
  #   y <- single_cell$Lap
  # }
  # if(xaxis=="Location"){
  #   x <- single_cell$Loc
  #   y <- single_cell$Lap
  #
  # }

  x <- single_cell$V1
  y <- single_cell$V2

  # Plotting vertical lines with unit length 1
  plot(x,y, type = "n", xlab = xaxis, ylab = yaxis, xlim = c(min(x), max(x) + 1), ylim = c(0, max(y) + 1))

  segments(x, y - 0.5, x, y + 0.5, col = "blue")
  if (yaxis=="Neurons"){
    title(paste("Raster of neurons firing across", xaxis, sep = " "))
  }
  else{
    title(paste("Raster of single neuron firing across", xaxis, sep = " "))
  }


  # how to make the raster plot

}
