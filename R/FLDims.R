#' Returns the dimensions of the object
#'
#' @param object FLMatrix, FLVector or FLTable object
#' @return R vector giving dimensions of input object
#' @export
dim<-function(object){
	UseMethod("dim",object)
}

#' @export
dim.default <- base::dim
#' @export
dim.FLIndexedValues <- function(object)
{
	 return(object@dims)
}

# Returns the dimensions of the object
#' @export
dim.FLMatrixBind <- function(object)
{
	 return(c(nrow(object),ncol(object)))
}

#' @export
dim.FLTable <- function(object)
{
	 return(c(nrow(object),ncol(object)))
}



#' @export
dimnames.FLMatrix <- function(object)
  return(object@Dimnames)
  #' @export
dimnames.FLVector <- function(object)
  return(object@Dimnames)
#' @export
dimnames.FLTable <- function(object)
  return(object@Dimnames)
#' @export
dimnames.FLTableMD <- function(object)
  return(object@Dimnames)
#' @export
dimnames.FLAbstractTable <- function(object)
  return(object@Dimnames)
