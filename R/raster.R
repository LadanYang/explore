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

single_cell <- function(matdata, xaxis){
  library(R.matlab)
  dfdata<-as.data.frame(readMat(matdata))
  colnames(dfdata) <-c("V1","V2")

  obj<-new_single_cell(dfdata,xaxis

                  )
  #obj<-validate_single_cell(obj)
  return(obj)
}



# my_single_cell_object <- single_cell("~/path/to/matlab/file/")
# explotr(my_single_cell_object)



