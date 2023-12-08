raster <- function(spikes,eventstart,varargin) {

}

new_single_cell <- function(x, xaxis) {

  stopifnot(is.data.frame(x))
  stopifnot(is.character(xaxis))

  structure(x,
            "class" = "single_cell"
  )
}

validate_single_cell <- function() {

}

#' @importFrom R.matlab readMat
single_cell <- function(matdata, xaxis){

  dfdata<-as.data.frame(readMat(matdata))
  colnames(dfdata) <-c("V1","V2")

  obj<-new_single_cell(dfdata,xaxis

                  )
  #obj<-validate_single_cell(obj)
  return(obj)
}

#' @exportS3Method
fortify.single_cell <- function(model, data, ...) {
  class(model) <- "data.frame"
  model
}

# my_single_cell_object <- single_cell("~/path/to/matlab/file/")
# explotr(my_single_cell_object)



