
#'Given a matlab data set, this function creates a dataframe
#'@param matdat the matlab data with the first column neurons firing time/location, eg. the file path to a matlab file
#'@export
new_single_cell <- function(x) {

  stopifnot(is.data.frame(x))
  #stopifnot(is.character(xaxis))

  structure(x,
            "class" = "single_cell"
  )
}

validate_single_cell <- function() {

}


#' Create a single_cell object from a .mat file
#'
#' @param matdat the matlab data with the first column neurons firing time/location, eg. the file path to a matlab file
#' @export
#' @importFrom R.matlab readMat
single_cell <- function(matdata){

  dfdata<-as.data.frame(readMat(matdata))
  colnames(dfdata) <-c("V1","V2")

  obj<-new_single_cell(dfdata

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



