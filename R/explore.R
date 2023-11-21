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
